module DoubleChecker

open System
open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
open Mono.Cecil
open Mono.Cecil.Cil

[<AutoOpen>]
module private Internals =

    type TypeReference with
        member this.LongName =
            if this.DeclaringType = null then
                if String.IsNullOrEmpty this.Namespace then this.Name
                else $"{this.Namespace}.{this.Name}"
            else
                $"{this.DeclaringType.LongName}.{this.Name}"

        member this.IsShaderBuilder =
            match this.LongName with
            | "FShade.ShaderBuilders.VertexBuilder"
            | "FShade.ShaderBuilders.FragmentBuilder"
            | "FShade.ShaderBuilders.GeometryBuilder"
            | "FShade.ShaderBuilders.TessBuilder"
            | "FShade.ShaderBuilders.ComputeBuilder"
            | "FShade.ShaderBuilders.RayGenerationBuilder"
            | "FShade.ShaderBuilders.RayMissBuilder"
            | "FShade.ShaderBuilders.RayAnyHitBuilder"
            | "FShade.ShaderBuilders.RayClosestHitBuilder"
            | "FShade.ShaderBuilders.RayCallableBuilder"
            | "FShade.ShaderBuilders.RayIntersectionBuilder" -> true
            | _ -> false

    type MethodReference with
        member inline this.LongName =
            $"{this.DeclaringType.LongName}.{this.Name}"

    type Type with
        member inline this.LongName =
            if String.IsNullOrEmpty this.Namespace then this.Name
            else $"{this.Namespace}.{this.Name}"

    [<AbstractClass; Sealed; Extension>]
    type DictionaryExtensions() =

        [<Extension>]
        static member AddOrAppend<'K, 'V>(dict: Dictionary<'K, List<'V>>, key: 'K, value: 'V) =
            match dict.TryGetValue key with
            | true, values -> values.Add value
            | _ -> dict.[key] <- List [|value|]

        [<Extension>]
        static member AddOrInsert<'K, 'V>(dict: Dictionary<'K, HashSet<'V>>, key: 'K, value: 'V) =
            match dict.TryGetValue key with
            | true, values -> values.Add value |> ignore
            | _ -> dict.[key] <- HashSet [|value|]

    let private uniformScopeExtensionRx = Regex(@"^UniformScope\.get_(?<name>.+)$", RegexOptions.Compiled)

    [<return: Struct>]
    let (|UniformScopeExtension|_|) (method: MethodDefinition) =
        if method.IsStatic && method.Parameters.Count = 1 && method.Parameters.[0].ParameterType.LongName = "FShade.UniformScope" then
            let m = uniformScopeExtensionRx.Match method.Name
            if m.Success then
                let name = m.Groups.["name"].Value
                ValueSome ($"{method.DeclaringType.FullName}", name, method.ReturnType)
            else
                ValueNone
        else
            ValueNone

    [<return: Struct>]
    let (|ShaderExpression|_|) (method: MethodDefinition) =
        if method.HasBody && method.ReturnType.LongName = "Microsoft.FSharp.Quotations.FSharpExpr`1" then
            let isShaderExpr = method.Body.Variables |> Seq.exists (_.VariableType.IsShaderBuilder)
            if isShaderExpr then
                let returnType =
                    match method.ReturnType with
                    | :? GenericInstanceType as expr -> expr.GenericArguments.[0]
                    | _ -> null

                ValueSome returnType
            else
                ValueNone
        else
            ValueNone

    [<return: Struct>]
    let (|Call|_|) (op: OpCode) =
        if op = OpCodes.Call then ValueSome ()
        else ValueNone

    [<return: Struct>]
    let (|NewObj|_|) (op: OpCode) =
        if op = OpCodes.Newobj then ValueSome ()
        else ValueNone

    [<return: Struct>]
    let (|Ldsfld|_|) (op: OpCode) =
        if op = OpCodes.Ldsfld then ValueSome ()
        else ValueNone

    let fsharpFuncRx = Regex(@"^Microsoft\.FSharp\.Core\.FSharpFunc`[1-9]+$")

    [<return: Struct>]
    let (|GenericFSharpFunc|_|) (typ: TypeReference) =
        match typ with
        | :? GenericInstanceType as typ ->
            let def = typ.Resolve()
            match def.BaseType with
            | :? GenericInstanceType as baseType when fsharpFuncRx.IsMatch baseType.LongName ->
                match def.Methods |> Seq.tryFind (fun m -> m.Name = "Invoke") with
                | Some invoke when invoke.ReturnType.LongName = "Microsoft.FSharp.Quotations.FSharpExpr`1" ->
                    let args = typ.GenericArguments |> Seq.toArray
                    ValueSome (invoke, args)
                | _ ->
                    ValueNone
            | _ ->
                ValueNone
        | _ -> ValueNone

    [<return: Struct>]
    let (|DoubleTensor|_|) (typ: TypeReference) =
        match typ.LongName with
        | "System.Double"
        | "Aardvark.Base.V2d"
        | "Aardvark.Base.V3d"
        | "Aardvark.Base.V4d"
        | "Aardvark.Base.M22d"
        | "Aardvark.Base.M23d"
        | "Aardvark.Base.M34d"
        | "Aardvark.Base.M44d" -> ValueSome ()
        | _ -> ValueNone

    [<return: Struct>]
    let (|Ref|_|) (typ: TypeReference) =
        match typ with
        | :? GenericInstanceType as typ when typ.LongName.StartsWith "Microsoft.FSharp.Core.FSharpRef`1" ->
            let content = typ.GenericArguments.[0]
            ValueSome(content)
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|ArrayOf|_|) (typ: TypeReference) =
        if typ.IsArray then ValueSome(typ.GetElementType())
        else ValueNone

    [<return: Struct>]
    let (|ArrOf|_|) (typ: TypeReference) =
        match typ with
        | :? GenericInstanceType as typ when typ.LongName.StartsWith "Aardvark.Base.Arrays.Arr`2" ->
            let targs = typ.GenericArguments
            ValueSome targs.[1]
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|TypeWithFields|_|) (typ: TypeReference) =
        if typ.IsArray || typ.IsPrimitive then ValueNone
        else
            let def = typ.Resolve()
            if def = null then ValueNone
            else
                let fieldTypes =
                    def.Fields.ToArray()
                    |> Array.choose (fun f ->
                        if not f.IsStatic then
                            match typ, f.FieldType with
                            | (:? GenericInstanceType as typ), (:? GenericParameter as fieldType) -> Some typ.GenericArguments.[fieldType.Position]
                            | _ -> Some f.FieldType
                        else
                            None
                    )

                if fieldTypes.Length > 0 then
                    ValueSome fieldTypes
                else
                    ValueNone

    let private doubleTypes = Dictionary<TypeReference, bool>()

    let rec private isDouble (visited: HashSet<TypeReference>) (typ: TypeReference) =
        if visited.Add typ then
            match doubleTypes.TryGetValue typ with
            | true, result -> result
            | _ ->
                let result =
                    match typ with
                    | DoubleTensor
                    | Ref DoubleTensor
                    | ArrayOf DoubleTensor
                    | ArrOf DoubleTensor -> true
                    | TypeWithFields fields -> fields |> Array.exists (isDouble visited)
                    | _ -> false

                doubleTypes.[typ] <- result
                result
        else
            false

    [<return: Struct>]
    let (|Double|_|) (typ: TypeReference) =
        if isDouble (HashSet()) typ then ValueSome ()
        else ValueNone

let run (entryAssembly: string) =
    let dirs = [ Path.GetDirectoryName entryAssembly ]
    let readerParams = Cecil.readerParams dirs
    let entry = Cecil.readAssembly (Some readerParams) entryAssembly

    let assemblyDefinitions = Dictionary()
    assemblyDefinitions.[AssemblyNameReference(entry.Name.Name, entry.Name.Version).ToString()] <- Some entry

    let rec load (name: AssemblyNameReference) =
        let strName = name.FullName
        if not (assemblyDefinitions.ContainsKey strName) then
            let res = Cecil.resolveAssembly dirs (Some readerParams) name
            assemblyDefinitions.[strName] <- res |> Option.map snd

            match res with
            | Some (_, res) ->
                assemblyDefinitions.[strName] <- Some res
                let refs = res.Modules |> Seq.collect (_.AssemblyReferences)
                for r in refs do load r

            | None ->
                assemblyDefinitions.[strName] <- None

    Log.debug "Loading assemblies"

    for m in entry.Modules do
        for ref in m.AssemblyReferences do
            load ref

    let allAssemblies =
        assemblyDefinitions.Values
        |> Seq.choose id
        |> Seq.toArray
        |> Array.filter (fun ass ->
            ass.Name.Name <> "FShade.GLSL" &&
            ass.Modules |> Seq.exists (fun m -> m.AssemblyReferences |> Seq.exists (fun r -> r.Name = "FShade.Core"))
        )

    if allAssemblies.Length > 0 then
        Log.debug $"Found {allAssemblies.Length} assemblies that reference FShade:"
        for ass in allAssemblies do Log.debug $" {ass.Name}"
    else
        Log.debug "Did not find any assemblies referencing FShade"

    // Find all shader expressions and uniforms that use double-based types
    // Remember generic shader expressions for the next step.
    let genericShaderMethods = Dictionary<MethodReference, List<GenericParameter>>()
    let genericMethodCalledBy = Dictionary<MethodReference, List<MethodReference * TypeReference[]>>()

    Log.debug "Searching for shader expressions and uniforms"

    let withinScope =
        let allModuleNames = HashSet<string>()
        let allAssemblyNames = HashSet<string>()

        for asm in allAssemblies do
            allAssemblyNames.Add asm.FullName |> ignore
            for mod_ in asm.Modules do allModuleNames.Add mod_.Name |> ignore

        fun (scope: IMetadataScope) ->
            match scope with
            | :? ModuleDefinition as def -> allModuleNames.Contains def.Name
            | :? ModuleReference as ref -> allModuleNames.Contains ref.Name
            | :? AssemblyNameReference as name -> allAssemblyNames.Contains name.FullName
            | _ -> false

    for asm in allAssemblies do
        for mod_ in asm.Modules do
            for typ in mod_.GetTypes() do
                for meth in typ.Methods do
                    match meth with
                    | UniformScopeExtension (decl, name, (Double as typ)) ->
                        Log.warn $"{decl} defines uniform {name} with double-based type: {typ}"

                    | ShaderExpression returnType ->
                        let doubleTypes = HashSet()
                        let body = Seq.toArray meth.Body.Instructions

                        for i = 0 to body.Length - 3 do
                            if body.[i].OpCode = OpCodes.Ldtoken &&
                               body.[i + 1].OpCode = OpCodes.Call &&
                               body.[i + 2].OpCode = OpCodes.Stelem_Any then

                                match body.[i].Operand, body.[i + 2].Operand with
                                | :? TypeReference as usedType, (:? TypeReference as elemType) when elemType.LongName = "System.Type" ->
                                    if usedType.IsGenericParameter then
                                        genericShaderMethods.AddOrAppend(meth, usedType :?> GenericParameter)
                                    else
                                        match usedType with
                                        | Double -> doubleTypes.Add usedType.LongName |> ignore
                                        | _ -> ()

                                | _ -> ()

                        match returnType with
                        | Double -> doubleTypes.Add returnType.LongName |> ignore
                        | _ -> ()

                        if doubleTypes.Count > 0 then
                            let output = doubleTypes |> String.concat ", "
                            Log.warn $"{meth.LongName} uses double-based types: {output}"

                    | _ when meth.HasBody ->
                        for inst in meth.Body.Instructions do
                            match inst.OpCode, inst.Operand with
                            | Call, (:? GenericInstanceMethod as callee) when withinScope callee.DeclaringType.Scope ->
                                let args = callee.GenericArguments |> Seq.toArray
                                genericMethodCalledBy.AddOrAppend(callee.Resolve(), (meth, args))

                            | Ldsfld, (:? MemberReference as mem)
                            | NewObj, (:? MemberReference as mem) when withinScope mem.DeclaringType.Scope ->
                                match mem.DeclaringType with
                                | GenericFSharpFunc (invoke, args) -> genericMethodCalledBy.AddOrAppend(invoke, (meth, args))
                                | _ -> ()

                            | _ -> ()
                    | _ ->
                        ()

    // For all the generic shader expressions we find any instances using double-based generic arguments
    Log.debug "Detecting usage of generic shader expressions"

    let rec findGenericDoubleArguments (result: Dictionary<MethodReference, HashSet<TypeReference>>) (param: GenericParameter) (meth: MethodReference) =
        match genericMethodCalledBy.TryGetValue meth with
        | true, calledBy ->
            for caller, args in calledBy do
                match args.[param.Position] with
                | :? GenericParameter as param -> findGenericDoubleArguments result param caller
                | Double as arg -> result.AddOrInsert(caller, arg)
                | _ -> ()

        | _ -> ()

    for KeyValue(meth, parameters) in genericShaderMethods do
        let result = Dictionary<MethodReference, HashSet<TypeReference>>()

        for param in parameters do
            findGenericDoubleArguments result param meth

        for KeyValue(caller, args) in result do
            let output = args |> Seq.map _.LongName |> String.concat ", "
            Log.warn $"{caller.LongName} uses generic {meth.LongName} with double-based argument types: {output}"