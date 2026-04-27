module AotRewrite

open System
open System.IO
open System.Reflection
open System.Runtime.Loader
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Mono.Cecil
open Mono.Cecil.Cil

/// Detection: a method is a "shader function" iff its IL contains a call/callvirt
/// to a member declared on a type implementing FShade.IShaderBuilder.
module Detect =

    let private implementsShaderBuilder (td : TypeDefinition) =
        if isNull td then false
        else
            let rec walk (t : TypeDefinition) =
                if isNull t then false
                elif t.Interfaces |> Seq.exists (fun i -> i.InterfaceType.FullName = "FShade.IShaderBuilder") then true
                elif isNull t.BaseType then false
                else
                    try walk (t.BaseType.Resolve())
                    with _ -> false
            try walk td
            with _ -> false

    /// True if this method body uses a shader builder (vertex/fragment/compute/...).
    /// F# compiles CE blocks to quotation literals via FSharpExpr.Deserialize40 — the
    /// methods on the builder don't appear as IL calls. What DOES appear is a call
    /// to a module getter (FShade.ShaderBuilders.get_fragment etc) whose return type
    /// IS the builder. So we check both the declaring type and the return type.
    let isShaderFunction (md : MethodDefinition) : bool =
        if isNull md.Body then false
        else
            let isBuilderType (tr : TypeReference) =
                if isNull tr then false
                else
                    try
                        let td = tr.Resolve()
                        implementsShaderBuilder td
                    with _ -> false
            md.Body.Instructions
            |> Seq.exists (fun i ->
                if i.OpCode = OpCodes.Call || i.OpCode = OpCodes.Callvirt then
                    match i.Operand with
                    | :? MethodReference as m ->
                        isBuilderType m.DeclaringType || isBuilderType m.ReturnType
                    | _ -> false
                else false
            )

    /// Diagnostic helper: returns the set of declaring-type FullNames called by this method.
    let calledDeclaringTypes (md : MethodDefinition) : seq<string> =
        if isNull md.Body then Seq.empty
        else
            md.Body.Instructions
            |> Seq.choose (fun i ->
                if i.OpCode = OpCodes.Call || i.OpCode = OpCodes.Callvirt then
                    match i.Operand with
                    | :? MethodReference as m ->
                        try Some (m.DeclaringType.FullName + " :: " + m.Name)
                        with _ -> None
                    | _ -> None
                else None)
            |> Seq.distinct


/// Build-time invocation: load the assembly, reflectively invoke the shader function
/// with placeholder values for each formal parameter, get back the Expr, normalize
/// parameter-bound ValueWithName nodes to Vars, and hash via FShade.Aot.hashBody.
module Hashing =

    /// Allocate a placeholder value for a parameter type. For value types, default(T);
    /// for reference types, an uninitialized (all-zero) instance via GetUninitializedObject —
    /// this avoids NRE when shader bodies do work *outside* the CE block, e.g.
    /// `let c = v.col * inv in fragment { ... }`. The placeholder is only used so the
    /// quotation can be constructed; its actual values are normalized to `Var`s before hashing.
    let placeholder (t : Type) : obj =
        if t.IsValueType then
            try Activator.CreateInstance(t)
            with _ -> null
        elif t = typeof<string> then ""
        elif t.IsAbstract || t.IsInterface then null
        else
            try System.Runtime.Serialization.FormatterServices.GetUninitializedObject(t)
            with _ -> null

    /// Walk the Expr; replace any ValueWithName(_, t, name) where name matches one
    /// of the known carrier names (and the type matches) with a fresh Var.
    let normalize (carriers : Map<string, Var>) (e : Expr) : Expr =
        let rec go (e : Expr) =
            match e with
            | ValueWithName(_, t, n) ->
                match Map.tryFind n carriers with
                | Some v when v.Type = t -> Expr.Var v
                | _ -> e
            | ExprShape.ShapeVar _ -> e
            | ExprShape.ShapeLambda(v, b) -> Expr.Lambda(v, go b)
            | ExprShape.ShapeCombination(o, args) ->
                ExprShape.RebuildShapeCombination(o, args |> List.map go)
        go e

    /// Carriers as `(name, type)`. For static methods these are just formal params;
    /// for instance closures these are closure fields followed by formal params.
    type Carrier = { Name : string; Type : Type; IsField : bool }

    /// Cross-ALC: we can't cast to fshadeaot's own Expr type. Instead the caller passes
    /// `normalizeAndHash : (string * Type)[] -> Type -> obj -> string` which lives in
    /// the runtime ALC and does the walk/hash there.
    /// `closureFields` carries (name, runtime type) for the closure's instance fields
    /// when invoking an instance shader-function; empty array for static methods.
    let tryHashShaderFunction
        (normalizeAndHash : (string * Type)[] -> Type -> obj -> string)
        (mi : MethodInfo)
        (closureFields : (string * Type)[])
        (inputType : Type) : Result<Carrier[] * string, string> =
        let ps = mi.GetParameters()
        let paramPlaceholders = ps |> Array.map (fun p -> placeholder p.ParameterType)
        try
            let target =
                if mi.IsStatic then null
                else
                    // Instance method: instantiate the declaring (closure) type with placeholder
                    // values for each captured field. F# closure ctors take fields by name in
                    // the same order as `closureFields`.
                    let fieldPlaceholders = closureFields |> Array.map (fun (_, t) -> placeholder t)
                    let dt = mi.DeclaringType
                    let ctor =
                        dt.GetConstructors(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
                        |> Array.tryFind (fun c -> c.GetParameters().Length = closureFields.Length)
                    match ctor with
                    | Some c ->
                        try c.Invoke(fieldPlaceholders)
                        with
                        | :? TargetInvocationException as tie when not (isNull tie.InnerException) ->
                            raise tie.InnerException
                    | None -> failwithf "no matching ctor on %s" dt.FullName
            let r =
                try mi.Invoke(target, paramPlaceholders)
                with
                | :? TargetInvocationException as tie when not (isNull tie.InnerException) ->
                    raise tie.InnerException
                | _ -> reraise()
            if isNull r then Error "shader function returned null"
            else
                // Build carrier list: closure fields first (so name lookup matches them),
                // then formal params. ValueWithName names match in either case.
                let allCarriers =
                    [|
                        for (n, t) in closureFields do
                            yield { Name = n; Type = t; IsField = true }
                        for p in ps do
                            yield { Name = p.Name; Type = p.ParameterType; IsField = false }
                    |]
                let carrierInfo = allCarriers |> Array.map (fun c -> c.Name, c.Type)
                let hash = normalizeAndHash carrierInfo inputType r
                Ok (allCarriers, hash)
        with e ->
            Error (sprintf "invocation failed: %s\n  at %s" e.Message (if isNull e.StackTrace then "<no stack>" else e.StackTrace.Split('\n').[0]))


/// IL rewriting helpers.
module Rewrite =

    /// Returns true if the method shape is one we can rewrite.
    /// Either:
    ///   - static, non-generic, declaring type non-generic, returns Expr<_>/Expr,
    ///     has ≥1 formal parameter (the shader input).
    /// or:
    ///   - instance method on a sealed F#-generated closure type (FSharpFunc subclass)
    ///     whose Invoke returns Expr<_>/Expr; declaring type non-generic; closure has
    ///     a single ctor that takes one arg per public instance field (the captured set).
    let canRewrite (md : MethodDefinition) : bool =
        let basicShape =
            not md.IsAbstract
            && not (isNull md.Body)
            && md.GenericParameters.Count = 0
            && md.DeclaringType.GenericParameters.Count = 0
            && md.Parameters.Count >= 1
            && (
                let rt = md.ReturnType.FullName
                rt = "Microsoft.FSharp.Quotations.FSharpExpr"
                || rt.StartsWith "Microsoft.FSharp.Quotations.FSharpExpr`1"
            )
        if not basicShape then false
        elif md.IsStatic then true
        else
            // Instance method: must be on a sealed closure type with a single ctor
            // whose param count matches the type's public instance fields.
            let dt = md.DeclaringType
            if not dt.IsSealed then false
            else
                let fields =
                    dt.Fields
                    |> Seq.filter (fun f -> not f.IsStatic && f.IsPublic)
                    |> Seq.toArray
                let ctors =
                    dt.Methods
                    |> Seq.filter (fun m -> m.IsConstructor && not m.IsStatic)
                    |> Seq.toArray
                ctors.Length = 1
                && ctors.[0].Parameters.Count = fields.Length

    /// Closure-fields-and-params model. For static shader functions, `closureFields` is empty.
    type CarrierModel =
        {
            ClosureFields  : FieldDefinition[]   // empty if static
            FormalParams   : ParameterDefinition[]   // includes the shader input as the LAST entry
        }

    let carrierModel (md : MethodDefinition) : CarrierModel =
        if md.IsStatic then
            { ClosureFields = [||]; FormalParams = md.Parameters |> Seq.toArray }
        else
            let dt = md.DeclaringType
            let fields =
                dt.Fields
                |> Seq.filter (fun f -> not f.IsStatic && f.IsPublic)
                |> Seq.toArray
            // Match field order to ctor parameter order (ctor param names match field names in F#).
            let ctor = dt.Methods |> Seq.find (fun m -> m.IsConstructor && not m.IsStatic)
            let orderedFields =
                ctor.Parameters
                |> Seq.map (fun p ->
                    fields |> Array.tryFind (fun f -> f.Name = p.Name)
                    |> Option.defaultWith (fun _ ->
                        // Fall back to position match if name doesn't align.
                        fields.[p.Index]))
                |> Seq.toArray
            { ClosureFields = orderedFields; FormalParams = md.Parameters |> Seq.toArray }

    /// Type-resolution helpers — find essential type/method references in
    /// FShade.Core / FSharp.Core / mscorlib via Cecil.
    type Refs =
        {
            Module             : ModuleDefinition
            Marker             : MethodReference   // FShade.Aot.marker (deferred path)
            MarkerPrecomputed  : MethodReference   // FShade.Aot.markerPrecomputed (cold-fast path)
            GetTypeFromHandle  : MethodReference   // System.Type.GetTypeFromHandle
            ObjectType         : TypeReference
            FuncType           : TypeReference     // System.Func<Expr>
            FuncCtor           : MethodReference
            LazyExprType       : TypeReference     // Lazy<Expr>
            LazyExprCtor       : MethodReference
            ExprType           : TypeReference
        }

    /// Caller-supplied resolver: assembly name → AssemblyDefinition.
    type Resolver = string -> AssemblyDefinition

    let private resolveType (resolver : Resolver) (mod_ : ModuleDefinition) (asmName : string) (typeName : string) : TypeReference =
        let asm = resolver asmName
        if isNull asm then failwithf "could not resolve assembly %s" asmName
        let td = asm.MainModule.GetType(typeName)
        if isNull td then failwithf "type %s not found in %s" typeName asmName
        mod_.ImportReference(td :> TypeReference)

    let private resolveTypeDef (resolver : Resolver) (asmName : string) (typeName : string) : TypeDefinition =
        let asm = resolver asmName
        if isNull asm then failwithf "could not resolve assembly %s" asmName
        let td = asm.MainModule.GetType(typeName)
        if isNull td then failwithf "type %s not found in %s" typeName asmName
        td

    let private resolveMethod (resolver : Resolver) (mod_ : ModuleDefinition) (asmName : string) (typeName : string) (methodName : string) : MethodReference =
        let td = resolveTypeDef resolver asmName typeName
        let m = td.Methods |> Seq.tryFind (fun m -> m.Name = methodName)
        match m with
        | Some m -> mod_.ImportReference m
        | None -> failwithf "method %s not found on %s in %s" methodName typeName asmName

    let resolveRefs (resolver : Resolver) (mod_ : ModuleDefinition) : Refs =
        // For System types, use Cecil's reflection-based ImportReference. It picks the
        // right assembly scope for the target module (handles netstandard / coreclr /
        // mscorlib differences automatically). For FShade types, we resolve manually
        // via our Cecil resolver so we don't pull fshadeaot's own ALC into the picture.
        let marker = resolveMethod resolver mod_ "FShade.Core" "FShade.Aot" "marker"
        let markerPrecomputed = resolveMethod resolver mod_ "FShade.Core" "FShade.Aot" "markerPrecomputed"
        let exprType = resolveType resolver mod_ "FSharp.Core" "Microsoft.FSharp.Quotations.FSharpExpr"
        let gtfh =
            let mi = typeof<System.Type>.GetMethod("GetTypeFromHandle")
            mod_.ImportReference mi
        let objType = mod_.ImportReference(typeof<obj>)
        let funcOpenT = mod_.ImportReference(typedefof<System.Func<_>>)
        let lazyOpenT = mod_.ImportReference(typedefof<System.Lazy<_>>)
        let funcOfExpr =
            let g = GenericInstanceType(funcOpenT)
            g.GenericArguments.Add(exprType)
            g :> TypeReference
        let lazyExprType =
            let g = GenericInstanceType(lazyOpenT)
            g.GenericArguments.Add(exprType)
            g :> TypeReference
        let funcCtor =
            // System.Func`1<T>.ctor(object, IntPtr)
            let openCtor =
                typedefof<System.Func<_>>.GetConstructors()
                |> Array.find (fun c ->
                    let ps = c.GetParameters()
                    ps.Length = 2 && ps.[0].ParameterType = typeof<obj> && ps.[1].ParameterType = typeof<nativeint>)
            let imported = mod_.ImportReference openCtor
            // Bind generic arg
            let bound = MethodReference(imported.Name, imported.ReturnType, funcOfExpr)
            bound.HasThis <- imported.HasThis
            bound.ExplicitThis <- imported.ExplicitThis
            bound.CallingConvention <- imported.CallingConvention
            for p in imported.Parameters do
                bound.Parameters.Add(ParameterDefinition(p.Name, p.Attributes, p.ParameterType))
            bound :> MethodReference
        let lazyCtor =
            let openCtor =
                typedefof<System.Lazy<_>>.GetConstructors()
                |> Array.find (fun c ->
                    let ps = c.GetParameters()
                    ps.Length = 1
                    && ps.[0].ParameterType.IsGenericType
                    && ps.[0].ParameterType.GetGenericTypeDefinition() = typedefof<System.Func<_>>)
            let imported = mod_.ImportReference openCtor
            // Bind generic arg via MethodReference targeting the closed lazyExprType.
            // Keep parameter types as-is from the open method (they reference Lazy<T>'s
            // generic parameter T, which the runtime substitutes via the declaring type).
            let bound = MethodReference(imported.Name, imported.ReturnType, lazyExprType)
            bound.HasThis <- imported.HasThis
            bound.ExplicitThis <- imported.ExplicitThis
            bound.CallingConvention <- imported.CallingConvention
            for p in imported.Parameters do
                bound.Parameters.Add(ParameterDefinition(p.Name, p.Attributes, p.ParameterType))
            bound
        {
            Module = mod_
            Marker = marker
            MarkerPrecomputed = markerPrecomputed
            GetTypeFromHandle = gtfh
            ObjectType = objType
            FuncType = funcOfExpr
            FuncCtor = funcCtor
            LazyExprType = lazyExprType
            LazyExprCtor = lazyCtor
            ExprType = exprType
        }


    /// Clones the entire body of `src` into a freshly-created static private method on `dt`.
    /// Returns the new MethodDefinition. Mirrors the source method's static/instance kind
    /// so closure-field accesses (`ldarg.0; ldfld`) keep resolving against `this` correctly.
    let cloneBody (dt : TypeDefinition) (newName : string) (src : MethodDefinition) : MethodDefinition =
        let attrs =
            let baseAttrs = MethodAttributes.Private ||| MethodAttributes.HideBySig
            if src.IsStatic then baseAttrs ||| MethodAttributes.Static
            else baseAttrs
        let dst = MethodDefinition(newName, attrs, src.ReturnType)
        dst.HasThis <- src.HasThis
        dst.ExplicitThis <- src.ExplicitThis
        dst.CallingConvention <- src.CallingConvention
        for p in src.Parameters do
            dst.Parameters.Add(ParameterDefinition(p.Name, p.Attributes, p.ParameterType))
        let dstBody = dst.Body
        dstBody.InitLocals <- src.Body.InitLocals
        dstBody.MaxStackSize <- src.Body.MaxStackSize
        let varMap = System.Collections.Generic.Dictionary<VariableDefinition, VariableDefinition>()
        for v in src.Body.Variables do
            let nv = VariableDefinition(v.VariableType)
            varMap.[v] <- nv
            dstBody.Variables.Add(nv)
        // First pass: clone instructions (without resolving branch targets yet)
        let il = dstBody.GetILProcessor()
        let instMap = System.Collections.Generic.Dictionary<Instruction, Instruction>()
        let cloneInstr (i : Instruction) : Instruction =
            match i.Operand with
            | null -> il.Create(i.OpCode)
            | :? string as s -> il.Create(i.OpCode, s)
            | :? sbyte as v -> il.Create(i.OpCode, v)
            | :? byte as v -> il.Create(i.OpCode, v)
            | :? int as v -> il.Create(i.OpCode, v)
            | :? int64 as v -> il.Create(i.OpCode, v)
            | :? single as v -> il.Create(i.OpCode, v)
            | :? double as v -> il.Create(i.OpCode, v)
            | :? VariableDefinition as v -> il.Create(i.OpCode, varMap.[v])
            | :? ParameterDefinition as p ->
                // map by index (params line up)
                il.Create(i.OpCode, dst.Parameters.[p.Index])
            | :? FieldReference as f -> il.Create(i.OpCode, f)
            | :? MethodReference as m -> il.Create(i.OpCode, m)
            | :? TypeReference as t -> il.Create(i.OpCode, t)
            | :? CallSite as c -> il.Create(i.OpCode, c)
            | :? Instruction as _ ->
                // branch target — placeholder; will be patched in second pass
                il.Create(i.OpCode, Instruction.Create(OpCodes.Nop))
            | :? array<Instruction> as _ ->
                il.Create(i.OpCode, [||])
            | other ->
                failwithf "unhandled instruction operand type: %s" (other.GetType().FullName)
        for i in src.Body.Instructions do
            let ni = cloneInstr i
            instMap.[i] <- ni
            il.Append ni
        // Second pass: patch branch targets and instruction-array operands
        for KeyValue(srcI, dstI) in instMap do
            match srcI.Operand with
            | :? Instruction as t ->
                dstI.Operand <- instMap.[t]
            | :? array<Instruction> as arr ->
                dstI.Operand <- arr |> Array.map (fun t -> instMap.[t])
            | _ -> ()
        // Exception handlers
        for eh in src.Body.ExceptionHandlers do
            let nh = ExceptionHandler(eh.HandlerType)
            nh.CatchType <- eh.CatchType
            nh.TryStart <- if isNull eh.TryStart then null else instMap.[eh.TryStart]
            nh.TryEnd <- if isNull eh.TryEnd then null else instMap.[eh.TryEnd]
            nh.HandlerStart <- if isNull eh.HandlerStart then null else instMap.[eh.HandlerStart]
            nh.HandlerEnd <- if isNull eh.HandlerEnd then null else instMap.[eh.HandlerEnd]
            nh.FilterStart <- if isNull eh.FilterStart then null else instMap.[eh.FilterStart]
            dstBody.ExceptionHandlers.Add nh
        dt.Methods.Add dst
        dst

    let private thunkCounter = ref 0

    /// Builds a thunk closure type that, when its parameterless Invoke() runs, calls the
    /// cloned synth method with the right arguments. For a static synth, the thunk's fields
    /// are just the formal-param values; ctor takes them and Invoke calls synth(field0, ...).
    /// For an instance synth (closure case), the thunk additionally holds a reference to the
    /// original closure instance (`this`) and Invoke calls synth on it: this.synth(field0, ...).
    let buildThunkClosure
        (resolver : Resolver)
        (refs : Refs)
        (md : MethodDefinition)
        (synth : MethodDefinition) : TypeDefinition * MethodReference * MethodReference =
        let dt = md.DeclaringType
        let mod_ = dt.Module
        let n = System.Threading.Interlocked.Increment(thunkCounter)
        let closure =
            TypeDefinition(
                "",
                sprintf "<%s>AotThunk_%d" md.Name n,
                TypeAttributes.NestedPrivate ||| TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit,
                refs.ObjectType
            )
        dt.NestedTypes.Add closure

        // For instance src methods, thunk needs to hold `this` to call the synth (instance).
        let thisField : FieldDefinition option =
            if md.IsStatic then None
            else
                let f = FieldDefinition("$this", FieldAttributes.Public, dt :> TypeReference)
                closure.Fields.Add f
                Some f

        // Capture all formal params as fields too.
        let paramFields =
            md.Parameters
            |> Seq.map (fun p ->
                let f = FieldDefinition(p.Name, FieldAttributes.Public, p.ParameterType)
                closure.Fields.Add f
                f)
            |> Seq.toArray

        // ctor(maybeThis, params...) — order: $this (if instance), then formal params.
        let ctor =
            MethodDefinition(
                ".ctor",
                MethodAttributes.Public ||| MethodAttributes.HideBySig
                ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName,
                mod_.TypeSystem.Void)
        match thisField with
        | Some _ -> ctor.Parameters.Add(ParameterDefinition("$this", ParameterAttributes.None, dt :> TypeReference))
        | None -> ()
        for p in md.Parameters do
            ctor.Parameters.Add(ParameterDefinition(p.Name, p.Attributes, p.ParameterType))

        let cil = ctor.Body.GetILProcessor()
        let objCtor =
            let ci = typeof<obj>.GetConstructor([||])
            mod_.ImportReference ci
        cil.Append(Instruction.Create(OpCodes.Ldarg_0))
        cil.Append(Instruction.Create(OpCodes.Call, objCtor))
        // Store $this (if any) then param fields, in the same order as ctor params.
        let mutable ctorParamIdx = 0
        match thisField with
        | Some tf ->
            cil.Append(Instruction.Create(OpCodes.Ldarg_0))
            cil.Append(Instruction.Create(OpCodes.Ldarg, ctor.Parameters.[ctorParamIdx]))
            cil.Append(Instruction.Create(OpCodes.Stfld, tf :> FieldReference))
            ctorParamIdx <- ctorParamIdx + 1
        | None -> ()
        for i in 0 .. paramFields.Length - 1 do
            cil.Append(Instruction.Create(OpCodes.Ldarg_0))
            cil.Append(Instruction.Create(OpCodes.Ldarg, ctor.Parameters.[ctorParamIdx]))
            cil.Append(Instruction.Create(OpCodes.Stfld, paramFields.[i] :> FieldReference))
            ctorParamIdx <- ctorParamIdx + 1
        cil.Append(Instruction.Create(OpCodes.Ret))
        closure.Methods.Add ctor

        // Invoke() : Expr — virtual + final + sealed so ldvirtftn is valid.
        let invoke =
            MethodDefinition("Invoke",
                MethodAttributes.Public ||| MethodAttributes.HideBySig
                ||| MethodAttributes.Virtual ||| MethodAttributes.NewSlot
                ||| MethodAttributes.Final,
                refs.ExprType)
        let iil = invoke.Body.GetILProcessor()
        // For instance synth: load $this first (it's the receiver of the call).
        match thisField with
        | Some tf ->
            iil.Append(Instruction.Create(OpCodes.Ldarg_0))
            iil.Append(Instruction.Create(OpCodes.Ldfld, tf :> FieldReference))
        | None -> ()
        // Then the captured formal params.
        for i in 0 .. paramFields.Length - 1 do
            iil.Append(Instruction.Create(OpCodes.Ldarg_0))
            iil.Append(Instruction.Create(OpCodes.Ldfld, paramFields.[i] :> FieldReference))
        iil.Append(Instruction.Create(OpCodes.Call, synth :> MethodReference))
        iil.Append(Instruction.Create(OpCodes.Ret))
        closure.Methods.Add invoke

        closure, (ctor :> MethodReference), (invoke :> MethodReference)

    /// Rewrite for ZERO-arg shaders: replace body with a `markerPrecomputed` call that
    /// loads the precomputed Effect from an embedded assembly resource. The body's
    /// Map<ShaderStage, Shader> was serialized at build time and embedded as `resourceName`.
    let rewriteShaderFunctionPrecomputed (refs : Refs) (md : MethodDefinition) (id : string) (resourceName : string) (binary : byte[]) =
        let mod_ = md.Module
        let assemblyName = mod_.Assembly.FullName
        // Embed the binary as a public manifest resource.
        if not (mod_.Resources |> Seq.exists (fun r -> r.Name = resourceName)) then
            mod_.Resources.Add(EmbeddedResource(resourceName, ManifestResourceAttributes.Public, binary))

        let body = md.Body
        body.Instructions.Clear()
        body.Variables.Clear()
        body.ExceptionHandlers.Clear()
        body.InitLocals <- false
        let il = body.GetILProcessor()

        let returnType = md.ReturnType
        let elemReturnType =
            match returnType with
            | :? GenericInstanceType as g when g.ElementType.FullName = "Microsoft.FSharp.Quotations.FSharpExpr`1" ->
                g.GenericArguments.[0]
            | _ -> refs.ObjectType

        // markerPrecomputed(returnType, id, assemblyName, resourceName)
        il.Append(Instruction.Create(OpCodes.Ldtoken, elemReturnType))
        il.Append(Instruction.Create(OpCodes.Call,    refs.GetTypeFromHandle))
        il.Append(Instruction.Create(OpCodes.Ldstr,   id))
        il.Append(Instruction.Create(OpCodes.Ldstr,   assemblyName))
        il.Append(Instruction.Create(OpCodes.Ldstr,   resourceName))
        il.Append(Instruction.Create(OpCodes.Call,    refs.MarkerPrecomputed))

        // marker* return Expr (untyped); cast to Expr<'r> via Expr.Cast<'r>.
        match returnType with
        | :? GenericInstanceType as g when g.ElementType.FullName = "Microsoft.FSharp.Quotations.FSharpExpr`1" ->
            let castOpen =
                let exprT = typeof<Microsoft.FSharp.Quotations.Expr>
                exprT.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                |> Array.find (fun m -> m.Name = "Cast" && m.IsGenericMethodDefinition)
            let importedCast = mod_.ImportReference castOpen
            let gim = GenericInstanceMethod(importedCast)
            gim.GenericArguments.Add(g.GenericArguments.[0])
            il.Append(Instruction.Create(OpCodes.Call, gim :> MethodReference))
        | _ -> ()
        il.Append(Instruction.Create(OpCodes.Ret))


    /// Full-stack rewrite: clone body, build thunk closure, replace original body
    /// with the marker-calling sequence.
    let rewriteShaderFunction (resolver : Resolver) (refs : Refs) (md : MethodDefinition) (bodyHash : string) =
        let dt = md.DeclaringType
        // 1. Clone body to synth
        let synth = cloneBody dt (sprintf "%s$AotBody" md.Name) md
        // 2. Build thunk closure
        let closure, ctorRef, invokeRef = buildThunkClosure resolver refs md synth
        // 3. Replace md body with marker call
        let body = md.Body
        body.Instructions.Clear()
        body.Variables.Clear()
        body.ExceptionHandlers.Clear()
        body.InitLocals <- false
        let il = body.GetILProcessor()

        let returnType = md.ReturnType
        let elemReturnType =
            match returnType with
            | :? GenericInstanceType as g when g.ElementType.FullName = "Microsoft.FSharp.Quotations.FSharpExpr`1" ->
                g.GenericArguments.[0]
            | _ -> refs.ObjectType

        let inputParam = md.Parameters.[md.Parameters.Count - 1]
        let inputType = inputParam.ParameterType

        // Push inputType
        il.Append(Instruction.Create(OpCodes.Ldtoken, inputType))
        il.Append(Instruction.Create(OpCodes.Call, refs.GetTypeFromHandle))
        // Push returnType (element of Expr<'r>)
        il.Append(Instruction.Create(OpCodes.Ldtoken, elemReturnType))
        il.Append(Instruction.Create(OpCodes.Call, refs.GetTypeFromHandle))
        // Push bodyHash string
        il.Append(Instruction.Create(OpCodes.Ldstr, bodyHash))
        // Build args[]: object[]  — closure-captured fields (if any) THEN formal params,
        // excluding the LAST formal param (the shader input which is symbolic).
        let model = carrierModel md
        let total = md.Parameters.Count
        let runtimeArgsCount = model.ClosureFields.Length + (total - 1)
        il.Append(Instruction.Create(OpCodes.Ldc_I4, runtimeArgsCount))
        il.Append(Instruction.Create(OpCodes.Newarr, refs.ObjectType))
        let mutable slot = 0
        // 1) closure fields (this.fieldN)
        for f in model.ClosureFields do
            il.Append(Instruction.Create(OpCodes.Dup))
            il.Append(Instruction.Create(OpCodes.Ldc_I4, slot))
            il.Append(Instruction.Create(OpCodes.Ldarg_0))
            il.Append(Instruction.Create(OpCodes.Ldfld, f :> FieldReference))
            if f.FieldType.IsValueType then
                il.Append(Instruction.Create(OpCodes.Box, f.FieldType))
            il.Append(Instruction.Create(OpCodes.Stelem_Ref))
            slot <- slot + 1
        // 2) formal params (excluding the shader input which is the LAST one)
        for i in 0 .. total - 2 do
            let p = md.Parameters.[i]
            il.Append(Instruction.Create(OpCodes.Dup))
            il.Append(Instruction.Create(OpCodes.Ldc_I4, slot))
            il.Append(Instruction.Create(OpCodes.Ldarg, p))
            if p.ParameterType.IsValueType then
                il.Append(Instruction.Create(OpCodes.Box, p.ParameterType))
            il.Append(Instruction.Create(OpCodes.Stelem_Ref))
            slot <- slot + 1
        // Build Lazy<Expr>:
        //   newobj Closure( $this(if instance), formalParam0, ..., formalParamN )
        //   ldvirtftn Closure.Invoke
        //   newobj Func<Expr>(object, IntPtr)
        //   newobj Lazy<Expr>(Func<Expr>)
        if not md.IsStatic then
            il.Append(Instruction.Create(OpCodes.Ldarg_0))
        for i in 0 .. total - 1 do
            il.Append(Instruction.Create(OpCodes.Ldarg, md.Parameters.[i]))
        il.Append(Instruction.Create(OpCodes.Newobj, ctorRef))
        il.Append(Instruction.Create(OpCodes.Dup))
        il.Append(Instruction.Create(OpCodes.Ldvirtftn, invokeRef))
        il.Append(Instruction.Create(OpCodes.Newobj, refs.FuncCtor))
        il.Append(Instruction.Create(OpCodes.Newobj, refs.LazyExprCtor))

        // call Aot.marker → returns FSharpExpr (untyped)
        il.Append(Instruction.Create(OpCodes.Call, refs.Marker))
        // If the method's declared return type is FSharpExpr<'r>, wrap via Expr.Cast<'r>.
        // Expr.Cast is a static method on FSharpExpr taking FSharpExpr and returning FSharpExpr<'a>.
        match returnType with
        | :? GenericInstanceType as g when g.ElementType.FullName = "Microsoft.FSharp.Quotations.FSharpExpr`1" ->
            // Find Expr.Cast<'a>: static, generic, takes FSharpExpr, returns FSharpExpr<'a>.
            let castOpen =
                let exprT = typeof<Microsoft.FSharp.Quotations.Expr>
                exprT.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                |> Array.find (fun m -> m.Name = "Cast" && m.IsGenericMethodDefinition)
            let importedCast = refs.Module.ImportReference castOpen
            let gim = GenericInstanceMethod(importedCast)
            gim.GenericArguments.Add(g.GenericArguments.[0])
            il.Append(Instruction.Create(OpCodes.Call, gim :> MethodReference))
        | _ -> ()
        il.Append(Instruction.Create(OpCodes.Ret))
        ()
