namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection
open Aardvark.Base
open FShade.Imperative

#nowarn "8989"

[<AutoOpen>]
module ExprHashExtensions =

    module private Reflection = 
        open System.Reflection.Emit

        type private WithDebugDel = delegate of Expr * list<Expr> -> Expr
        type private NewVarDel = delegate of string * Type * Option<bool> * int64 -> Var
        type private GetStampDel = delegate of Var -> int64

        let withAttributes =
            let fi = typeof<Expr>.GetField("term", BindingFlags.NonPublic ||| BindingFlags.Instance)
            let ctor = typeof<Expr>.GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.CreateInstance ||| BindingFlags.Instance, Type.DefaultBinder, CallingConventions.Any, [| fi.FieldType; typeof<list<Expr>> |], null)

            let m = DynamicMethod("withoutDebug", MethodAttributes.Static ||| MethodAttributes.Public, CallingConventions.Standard, typeof<Expr>, [| typeof<Expr>; typeof<list<Expr>> |], typeof<Expr>, true)
            let il = m.GetILGenerator()

            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, fi)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Newobj, ctor)
            il.Emit(OpCodes.Ret)

        
            let withDebug = m.CreateDelegate(typeof<WithDebugDel>) |> unbox<WithDebugDel>

            fun (e : Expr) (debug : list<Expr>) ->
                withDebug.Invoke(e, debug)

        let newVar =
            
            let fi = typeof<Var>.GetField("stamp", BindingFlags.NonPublic ||| BindingFlags.Instance)
            let ctor = typeof<Var>.GetConstructor(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.CreateInstance ||| BindingFlags.Instance, Type.DefaultBinder, CallingConventions.Any, [| typeof<string>; typeof<Type>; typeof<Option<bool>> |], null)
            
            let m = DynamicMethod("newVar", MethodAttributes.Static ||| MethodAttributes.Public, CallingConventions.Standard, typeof<Var>, [| typeof<string>; typeof<Type>; typeof<Option<bool>>; typeof<int64> |], typeof<Var>, true)
            let il = m.GetILGenerator()


            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Newobj, ctor)
            il.Emit(OpCodes.Dup)
            il.Emit(OpCodes.Ldarg_3)
            il.Emit(OpCodes.Stfld, fi)

            il.Emit(OpCodes.Ret)

        
            let newVar = m.CreateDelegate(typeof<NewVarDel>) |> unbox<NewVarDel>

            fun (name : string, typ : Type, isMutable : bool, stamp : int64) ->
                newVar.Invoke(name, typ, Some isMutable, stamp)

        let getStamp =
            
            let fi = typeof<Var>.GetField("stamp", BindingFlags.NonPublic ||| BindingFlags.Instance)

            let m = DynamicMethod("newVar", MethodAttributes.Static ||| MethodAttributes.Public, CallingConventions.Standard, typeof<int64>, [| typeof<Var> |], typeof<Var>, true)
            let il = m.GetILGenerator()

            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, fi)
            il.Emit(OpCodes.Ret)

            let getStamp = m.CreateDelegate(typeof<GetStampDel>) |> unbox<GetStampDel>

            fun (v : Var) -> getStamp.Invoke(v)

    type Var with
        member x.Stamp = Reflection.getStamp x

        static member New(name, t, isMutable, stamp) = Reflection.newVar(name, t, isMutable, stamp)

    module Pickler =
        open MBrace.FsPickler
        open MBrace.FsPickler.Combinators
        open MBrace.FsPickler.Hashing
        open Microsoft.FSharp.Reflection
        open System.Reflection.Emit


        let private (|SimpleValue|_|) (e : Expr) =
            try
                match e with
                    | FieldGet(None, f) -> Some (f.Name, f.GetValue(null))
                    | PropertyGet(None, pi, []) -> 
                        System.Runtime.CompilerServices.RuntimeHelpers.RunClassConstructor(pi.DeclaringType.TypeHandle)

                        Some (pi.Name, pi.GetValue(null))
                    | Call(None, mi, []) -> Some (mi.Name, mi.Invoke(null, [||]))
                    | NewObject(ctor, []) -> Some (ctor.DeclaringType.Name, ctor.Invoke([||]))
                    | _ -> None
            with _ ->
                None
        let private (|ReflectedCall|_|) (e : Expr) =
            match e with
                | Call(t,mi,args) ->
                    let isInline = mi.GetCustomAttributes<InlineAttribute>() |> Seq.isEmpty |> not
                    match ExprWorkardound.TryGetReflectedDefinition mi with
                        | Some def -> 
                            let args = 
                                match t with
                                    | Some t -> t :: args
                                    | None -> args
                            Some (isInline, def, args)
                        | None ->
                            None
                | _ ->
                    None

        let private (|IntrinsicCall|_|) (e : Expr) =
            match e with
                | Call(t,mi,args) ->
                    let att = mi.GetCustomAttributes(typeof<IntrinsicAttribute>, true) |> Seq.map unbox<IntrinsicAttribute> |> Seq.toList
                    match att with
                        | [] ->
                            None
                        | i -> 
                            let args = 
                                match t with
                                    | Some t -> t :: args
                                    | None -> args
                            let intr = i |> List.map (fun i -> i.Intrinsic)

                            Some (intr, args)
                | _ ->
                    None

        type ExprPicklerFunctions private() =
            static let mutable patterns : list<Expr -> Option<obj>> = []
     
            static member AddPattern (f : Expr -> Option<obj>) =
                patterns <- f :: patterns

            static member VarPickler (r : IPicklerResolver) : Pickler<Var> =
                let makeVar (name : string) (t : Type) (isMutable : bool) (stamp : int64) = 
                    Var.New(name, t, isMutable, stamp)
                    
                let pType = r.Resolve<Type>()

                let read (rs : ReadState) =
                    let name = Pickler.string.Read rs "Name"
                    let typ = pType.Read rs "Type"
                    let isMutable = Pickler.bool.Read rs "IsMutable"
                    let stamp = Pickler.int64.Read rs "Stamp"

                    Var.New(name, typ, isMutable, stamp)

                let write (ws : WriteState) (value : Var) =
                    if ws.IsHashComputation then
                        if value.Type <> typeof<unit> then
                            Pickler.string.Write ws "Name" value.Name

                        Pickler.string.Write ws "Type" value.Type.FullName
                        Pickler.bool.Write ws "IsMutable" value.IsMutable
                    else
                        Pickler.string.Write ws "Name" value.Name
                        pType.Write ws "Type" value.Type
                        Pickler.bool.Write ws "IsMutable" value.IsMutable
                        Pickler.int64.Write ws "Stamp" value.Stamp
                        

                Pickler.FromPrimitives(read, write, useWithSubtypes = true)

            static member ExprPickler(r : IPicklerResolver) : Pickler<Expr> =
                let varPickler = r.Resolve<Var>()
                let infoPickler = r.Resolve<obj>()
                let intrinsicPickler = r.Resolve<list<CIntrinsic>>()
                
                let rec (|ExprValue|_|) (e : Expr) =
                    match e with
                        | Coerce(ExprValue v, _) -> Some v
                        | Value((:? Expr as v),_) -> Some v
                        | _ -> None
                Pickler.fix (fun self ->
                    let selfList = Pickler.list self

                    let rec writer (ws : WriteState) (e : Expr) =
                        match Reflection.withAttributes e [] with    

                            | Call(None, mi, [ExprValue v]) when mi.Name = "op_Splice" || mi.Name = "op_SpliceUntyped" ->
                                if v.Type = e.Type then
                                    Pickler.string.Write ws "Kind" "Splice"
                                    self.Write ws "Expr" v
                                else
                                    Pickler.string.Write ws "Kind" "Splice"
                                    self.Write ws "Expr" (Expr.Coerce(v, e.Type))

                            | IntrinsicCall(atts, args) when ws.IsHashComputation ->
                                Pickler.string.Write ws "Kind" "IntrinsicCall"
                                intrinsicPickler.Write ws "Attributes" atts
                                selfList.Write ws "Args" args
                                
                            | ReflectedCall(isInline, def, args) when ws.IsHashComputation ->
                                Pickler.string.Write ws "Kind" "ReflectedCall"
                                self.Write ws "Def" def
                                selfList.Write ws "Args" args
                                Pickler.bool.Write ws "IsInline" isInline

                            | SimpleValue(value) when ws.IsHashComputation ->
                                Pickler.string.Write ws "Kind" "SimpleValue"
                                infoPickler.Write ws "Value" value
                            | e -> 
                                
                                match patterns |> List.tryPick (fun p -> p e) with
                                    | Some res ->
                                        Pickler.string.Write ws "Kind" "Special"
                                        Pickler.obj.Write ws "Content" res
                                    | None -> 
                                        match e with
                                            | ShapeVar v -> 
                                                Pickler.string.Write ws "Kind" "Var"
                                                varPickler.Write ws "Var" v

                                            | ShapeLambda(v, b) ->
                                                Pickler.string.Write ws "Kind" "Lambda"
                                                varPickler.Write ws "Var" v
                                                self.Write ws "Body" b

                                            | ShapeCombination(o, args) ->
                                                Pickler.string.Write ws "Kind" "Comb"
                                                infoPickler.Write ws "Info" o
                                                selfList.Write ws "Children" args

                    let reader (rs : ReadState) : Expr =
                        let kind = Pickler.string.Read rs "Kind"
                        match kind with
                            | "Var" ->
                                let v = varPickler.Read rs "Var"
                                Expr.Var v

                            | "Lambda" ->
                                let v = varPickler.Read rs "Var"
                                let b = self.Read rs "Body"
                                Expr.Lambda(v,b)

                            | "Comb" ->
                                let o = infoPickler.Read rs "Info"
                                let c = selfList.Read rs "Children"
                                RebuildShapeCombination(o, c)
                            
                            | _ ->
                                failwithf "invalid expression kind: %A" kind

                    Pickler.FromPrimitives(reader, writer, useWithSubtypes = true)
                )

        let tryUnifyTypes (decl : Type) (real : Type) =
            let assignment = System.Collections.Generic.Dictionary<Type, Type>()

            let rec recurse (decl : Type) (real : Type) =
                if decl = real then
                    true

                elif decl.IsGenericParameter then
                    match assignment.TryGetValue decl with
                        | (true, old) ->
                            if old.IsAssignableFrom real then 
                                true

                            elif real.IsAssignableFrom old then
                                assignment.[decl] <- real
                                true

                            else 
                                false
                        | _ ->
                            assignment.[decl] <- real
                            true
            
                elif decl.IsArray then
                    if real.IsArray then
                        let de = decl.GetElementType()
                        let re = real.GetElementType()
                        recurse de re
                    else
                        false

                elif decl.ContainsGenericParameters then
                    let dgen = decl.GetGenericTypeDefinition()
                    let rgen = 
                        if real.IsGenericType then real.GetGenericTypeDefinition()
                        else real

                    if dgen = rgen then
                        let dargs = decl.GetGenericArguments()
                        let rargs = real.GetGenericArguments()
                        Array.forall2 recurse dargs rargs

                    elif dgen.IsInterface then
                        let rface = real.GetInterface(dgen.FullName)
                        if isNull rface then
                            false
                        else
                            recurse decl rface

                    elif not (isNull real.BaseType) then
                        recurse decl real.BaseType

                    else
                        false

                elif decl.IsAssignableFrom real then
                    true

                else
                    false


            if recurse decl real then
                Some (assignment |> Dictionary.toSeq |> HMap.ofSeq)
            else
                None

        type CoercePickler<'a, 'b> private(p : Pickler<'a>) =
            let coercePickler = Pickler.FromPrimitives((fun rs -> p.Read rs "Upcast" |> unbox<'b>), (fun ws v -> p.Write ws "Upcast" (unbox<'a> v)))

            member x.Pickler : Pickler<'b> = coercePickler


        type PicklerRegistry(types : list<Type>) =

            let picklerGen = typedefof<Pickler<_>>
            let allMeths = types |> List.collect (fun t -> t.GetMethods(BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic) |> Array.toList) //Introspection.GetAllMethodsWithAttribute<MyCrazyAttribute>() |> Seq.map (fun m -> m.E0) |> Seq.toArray

            let upcastToPicker (mi : MethodInfo) =
                let meth = 
                    DynamicMethod(
                        sprintf "upcasted.%s" mi.Name,
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        CallingConventions.Standard,
                        typeof<Pickler>,
                        [| typeof<IPicklerResolver> |],
                        typeof<obj>,
                        true
                    )
                let il = meth.GetILGenerator()

                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Tailcall)
                il.EmitCall(OpCodes.Call, mi, null)
                il.Emit(OpCodes.Ret)
                let func = 
                    meth.CreateDelegate(typeof<Func<IPicklerResolver, Pickler>>) 
                        |> unbox<Func<IPicklerResolver, Pickler>>        
                fun (r : IPicklerResolver) -> func.Invoke(r)

            let genericThings = 
                allMeths
                    |> List.filter (fun mi -> mi.GetGenericArguments().Length > 0)
                    |> List.choose (fun mi ->
                        let ret = mi.ReturnType
                        if ret.IsGenericType && ret.GetGenericTypeDefinition() = picklerGen && mi.GetParameters().Length = 1 then
                            let pickledType = ret.GetGenericArguments().[0]

                            let tryInstantiate (t : Type) =
                                match tryUnifyTypes pickledType t with
                                    | Some ass ->
                                        let targs = mi.GetGenericArguments() |> Array.map (fun a -> ass.[a])
                                        let mi = mi.MakeGenericMethod targs
                                        Some (upcastToPicker mi)
                                            
                                    | None ->
                                        None
                                        

                            Some tryInstantiate
                        else
                            None
                    )

            let nonGenericThings = 
                allMeths
                    |> List.filter (fun mi -> mi.GetGenericArguments().Length = 0)
                    |> List.choose (fun mi ->
                        let ret = mi.ReturnType
                        if ret.IsGenericType && ret.GetGenericTypeDefinition() = picklerGen && mi.GetParameters().Length = 1 then
                            let pickledType = ret.GetGenericArguments().[0]

                            let create = upcastToPicker mi
                            Some (pickledType, create)

                        else
                            None
                    )
                    |> Dictionary.ofList



                    
            member x.GetRegistration(t : Type) : CustomPicklerRegistration =
                if t.IsGenericType then
                    match genericThings |> List.tryPick (fun a -> a t) with
                        | Some r -> 
                            CustomPicklerRegistration.CustomPickler r
                        | None ->
                            match nonGenericThings.TryGetValue t with   
                                | (true, r) -> CustomPicklerRegistration.CustomPickler r
                                | _ -> CustomPicklerRegistration.UnRegistered
                else
                    match nonGenericThings.TryGetValue t with   
                        | (true, r) -> CustomPicklerRegistration.CustomPickler r
                        | _ -> CustomPicklerRegistration.UnRegistered
//                            let pickler = 
//                                nonGenericThings |> Seq.tryPick (fun (KeyValue(tdecl, pickler)) ->
//                                    if tdecl.IsAssignableFrom t then
//                                        let tc = typedefof<CoercePickler<_,_>>.MakeGenericType [| tdecl; t |]
//                                        let resolve (r : IPicklerResolver) =
//                                            let res = Activator.CreateInstance(tc, [| pickler r :> obj|])
//                                            let prop = tc.GetProperty("Pickler", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
//                                            let real = prop.GetValue(res) |> unbox<Pickler>
//                                            real
//
//                                        Some resolve
//                                    else
//                                        None
//                                )
//                            match pickler with
//                                | Some p -> CustomPicklerRegistration.CustomPickler p
//                                | _ -> CustomPicklerRegistration.UnRegistered

            interface ICustomPicklerRegistry with
                /// Look up pickler registration for particular type
                member x.GetRegistration(t : Type) : CustomPicklerRegistration = x.GetRegistration t

        let registry = PicklerRegistry [ typeof<ExprPicklerFunctions> ]


        type TypeConv() =
            interface ITypeNameConverter with
                member x.OfSerializedType(t : TypeInfo) = 
                    t
                member x.ToDeserializedType(t : TypeInfo) = 
                    t


        let cache = PicklerCache.FromCustomPicklerRegistry registry
        let pickler = FsPickler.CreateBinarySerializer(typeConverter = TypeConv(), picklerResolver = cache)

    let computeHash (value : 'a) = Pickler.pickler.ComputeHash(value).Hash |> System.Convert.ToBase64String

    type Expr with
    

        member x.WithAttributes(attributes : list<Expr>) =
            match attributes, x.CustomAttributes with
                | [], [] -> x
                | _ -> Reflection.withAttributes x attributes

        static member Pickle (e : Expr) =
            Pickler.pickler.Pickle(e)

        static member UnPickle (data : byte[]) : Expr =
            Pickler.pickler.UnPickle(data)
            
                
        static member ComputeHash(e : Expr) =
            let e = Reflection.withAttributes e []
            Pickler.pickler.ComputeHash(e).Hash |> Convert.ToBase64String

        member x.ComputeHash() =
            Expr.ComputeHash x
