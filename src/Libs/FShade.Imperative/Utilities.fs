namespace FShade.Imperative

open System
open System.Reflection

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open Aardvark.Base


[<AutoOpen>]
module private Helpers = 
    open System.Text.RegularExpressions
    open Aardvark.Base.TypeInfo
    open Aardvark.Base.TypeInfo.Patterns

    let rx = Regex @"(?<name>.*)`[0-9]+"


    let private typeNameCache = System.Collections.Concurrent.ConcurrentDictionary<Type, string>()
    let private methodNameCache = System.Collections.Concurrent.ConcurrentDictionary<MethodBase, string>()

    let private builtIn =
        [
            typeof<unit>, "bool"
            typeof<System.Void>, "void"
            typeof<bool>, "bool"

            typeof<int8>, "int8"
            typeof<int16>, "int16"
            typeof<int32>, "int32"
            typeof<int64>, "int64"
            typeof<uint8>, "uint8"
            typeof<uint16>, "uint16"
            typeof<uint32>, "uint32"
            typeof<uint64>, "uint64"
            typeof<float16>, "float16"
            typeof<float32>, "float32"
            typeof<float>, "double"

        ]

    do for (t,n) in builtIn do
        typeNameCache.[t] <- n


    let rec typeName (t : Type) =
        typeNameCache.GetOrAdd(t, fun t ->
            if FSharpType.IsTuple(t) then
                let elements = FSharpType.GetTupleElements t |> Seq.map typeName |> String.concat "_"
                "tup_" + elements

            else
                let selfName = 
                    if t.IsGenericType then
                        let m = rx.Match t.Name
                        let targs = t.GetGenericArguments()
                        let targstr = targs |> Seq.map typeName |> String.concat "_"
                        m.Groups.["name"].Value + string targs.Length + "_" + targstr
                    else
                        t.Name

                if t.IsNested then 
                    (typeName t.DeclaringType) + "_" + selfName
                else 
                    if isNull t.Namespace then selfName
                    else t.Namespace.Replace('.', '_') + "_" + selfName
        )

    let methodName (mi : MethodBase) =
        methodNameCache.GetOrAdd(mi, fun mi -> 
            match mi with
                | :? MethodInfo as mi -> 
                    let selfName =
                        if mi.IsGenericMethod then
                            let m = rx.Match mi.Name
                            let targs = mi.GetGenericArguments() |> Seq.map typeName |> String.concat "_"
                            m.Groups.["name"].Value + "_" + targs
                        else
                            mi.Name
                    (typeName mi.DeclaringType) + "_" + selfName

                | :? ConstructorInfo as ci ->
                    
                    let args = ci.GetParameters() |> Array.map (fun p -> typeName p.ParameterType) |> String.concat "_"
                    "new_" + (typeName mi.DeclaringType) + "_" + args

                | _ ->
                    failwithf "[FShade] cannot get method name for unknown method-type %A" mi
                    
        )

    let (|ArrOf|_|) (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Arr<_,_>> then
            let targs = t.GetGenericArguments()
            let len = targs.[0] |> getSize
            let content = targs.[1]
            Some(len, content)
        else
            None


    module Peano = 
        let private peanoTypes =
            let s = typedefof<S<_>>
            Seq.initInfinite id 
                |> Seq.scan (fun last _ -> s.MakeGenericType [|last|]) typeof<Z>
                |> Seq.cache

        let getPeanoType (i : int) =
            Seq.item i peanoTypes

        let getArrayType (i : int) (content : Type) =
            typedefof<Arr<_,_>>.MakeGenericType [| getPeanoType i; content |]
           
    type Expr with
        static member NewFixedArray(t : Type, values : list<Expr>) =
            let len = values |> List.length
            let arrType = Peano.getArrayType len t
            let seqType = typedefof<seq<_>>.MakeGenericType t
            let ctor = arrType.GetConstructor [| seqType |]
            Expr.NewObject(ctor, [Expr.Coerce(Expr.NewArray(t, values), seqType)])

    let (|NewFixedArray|_|) (e : Expr) =
        match e with
            | NewObject(ctor, [Coerce(NewArray(et, args),_)]) ->
                if ctor.DeclaringType.IsGenericType && ctor.DeclaringType.GetGenericTypeDefinition() = typedefof<Arr<_,_>> then
                    let len = Peano.getSize (ctor.DeclaringType.GetGenericArguments().[0])
                    Some(len, et, args)
                else 
                    None
            | _ ->
                None

    let rec tryGetMethodInfo (e : Expr) =
        match e with
            | Patterns.Call(_,mi,_) -> 
                if mi.IsGenericMethod then mi.GetGenericMethodDefinition() |> Some
                else mi |> Some
            | ExprShape.ShapeCombination(_, args) -> 
                args |> List.tryPick tryGetMethodInfo
            | ExprShape.ShapeLambda(_,b) ->
                tryGetMethodInfo b
            | _ -> None

    let getMethodInfo (e : Expr) =
        match tryGetMethodInfo e with
            | Some mi -> mi
            | None -> failwithf "[FShade] could not find a method-call in expression %A" e


    let private conversionMethods =
        HashSet.ofList [
            getMethodInfo <@ int8 @> 
            getMethodInfo <@ uint8 @> 
            getMethodInfo <@ int16 @> 
            getMethodInfo <@ uint16 @>  
            getMethodInfo <@ int @> 
            getMethodInfo <@ int32 @> 
            getMethodInfo <@ uint32 @> 
            getMethodInfo <@ int64 @> 
            getMethodInfo <@ uint64 @> 
            getMethodInfo <@ nativeint @> 
            getMethodInfo <@ unativeint @> 
            getMethodInfo <@ float @> 
            getMethodInfo <@ float32 @> 
        ]

    let (|ConversionMethod|_|) (mi : MethodInfo) =
        let meth = 
            if mi.IsGenericMethod then mi.GetGenericMethodDefinition()
            else mi
        if conversionMethods.Contains meth then
            let input = mi.GetParameters().[0]
            let output = mi.ReturnType
            Some(input.ParameterType, output)
        else
            None

    let (|EnumerableOf|_|) (t : Type) =
        if t.IsArray then
            Some (t.GetElementType())

        elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<seq<_>> then 
            Some (t.GetGenericArguments().[0])

        else
            let iface = t.GetInterface(typedefof<seq<_>>.Name)
            if isNull iface then
                None
            else
                Some (iface.GetGenericArguments().[0])

    let rec private deconstructTuples x =
        match x with
            | NewTuple(args) -> args |> List.collect deconstructTuples
            | _ -> [x]

    let rec inlineUselessAbstractions(e : Expr) =
        match e with
            | Application(Lambda(v,b),a) ->
                let b = b.Substitute(fun vi -> if vi = v then Some a else None)
                inlineUselessAbstractions b

            | Application(Let(f,l0, b), a) ->
                let b = b.Substitute(fun vi -> if vi = f then Some l0 else None)
                inlineUselessAbstractions (Expr.Application(b,a))
            | _ ->
                    
                e

    /// detects pipe-expressions like (a |> sin, a |> clamp 0 1, sin <| a + b, etc.)
    let (|Pipe|_|) (e : Expr) =
        match e with
            | Call(None, Method("op_PipeLeft", _), [Lambda(v,l);r]) ->
                let e = l.Substitute(fun vi -> if vi = v then Some r else None)
                Pipe(e) |> Some

            | Call(None, Method("op_PipeRight", _), [l;Lambda(v, r)]) ->
                let e = r.Substitute(fun vi -> if vi = v then Some l else None)
                Pipe(e) |> Some

            | Call(None, Method("op_PipeLeft", _), [PropertyGet(t,p, []);r]) ->
                let r = deconstructTuples r
                match t with
                    | None -> 
                        let mi = p.DeclaringType.GetMethod(p.Name, r |> Seq.map (fun ai -> ai.Type) |> Seq.toArray)
                        if mi <> null then(Expr.Call(mi, r)) |> Some
                        else failwith "function is not a method"
                    | Some t -> 
                        failwith "function is not a method"

            | Call(None, Method("op_PipeRight", _), [l;PropertyGet(t,p, [])]) ->
                let l = deconstructTuples l
                match t with
                    | None -> 
                        let mi = p.DeclaringType.GetMethod(p.Name, l |> Seq.map (fun ai -> ai.Type) |> Seq.toArray)
                        if mi <> null then(Expr.Call(mi, l)) |> Some
                        else failwith "function is not a method"
                    | Some t -> 
                        failwith "function is not a method"

            | Call(None, Method("op_PipeLeft", _), [f;r]) ->

                let e = Expr.Application(f, r) |> inlineUselessAbstractions
                Pipe(e) |> Some

            | Call(None, Method("op_PipeRight", _), [l;f]) ->
                let e = Expr.Application(f, l) |> inlineUselessAbstractions
                Pipe(e) |> Some


            | _ -> None

    /// F# creates mutable copies for structs when accessing properties/methods
    /// since mutating the original is not possible (and not desired).
    /// since everything is mutable in C we don't care for those copies making the
    /// code less readable and more complicated
    let (|LetCopyOfStruct|_|) (e : Expr) =
        match e with
            | Let(v, e, b) when v.IsMutable && v.Type.IsValueType && v.Name = "copyOfStruct" ->
                // TODO: find a better way for detecting this
                let mutable count = 0
                let newBody = b.Substitute(fun vi -> if v = vi then count <- count + 1; Some e else None) 
                if count = 0 then Some b
                elif count = 1 then Some newBody
                else None
            | _ ->
                None

    let (|FSharpTypeProperty|_|) (pi : PropertyInfo) =
        if FSharpType.IsRecord(pi.DeclaringType, true) then 
            if FSharpType.GetRecordFields(pi.DeclaringType, true) |> Array.exists (fun p -> p = pi) then
                Some ()
            else
                None

        elif pi.DeclaringType.BaseType <> null && FSharpType.IsUnion(pi.DeclaringType.BaseType, true) then
            let isUnionField =
                FSharpType.GetUnionCases(pi.DeclaringType.BaseType, true)
                    |> Seq.collect (fun c -> c.GetFields())
                    |> Seq.exists (fun p -> p = pi)

            if isUnionField then Some ()
            else None

        else
            None

    let rec (|Trivial|_|) (e : Expr) =
        match e with
            | Var _ 
            | Value _
            | FieldGet(None, _)
            | PropertyGet(None, _, [])
            | TupleGet(Trivial, _)
            | PropertyGet(Some Trivial, FSharpTypeProperty, [])
            | FieldGet(Some Trivial, _) ->
                Some ()
            | _ ->
                None

    /// detects direct applications of lambdas and replaces them with let
    /// bindings or inlines the expression if either the argument is trivial or it occurs
    /// only once in the lambda-body
    let (|LambdaApp|_|) (e : Expr) =
        match e with
            | Application(Lambda(v,b), arg) ->
                let mutable cnt = 0
                let nb = b.Substitute(fun vi -> if vi = v then cnt <- cnt + 1; Some arg else None)
                
                match arg with
                    | Trivial | _ when cnt <= 1 -> nb |> Some
                    | _ ->  Expr.Let(v, e, b) |> Some

            | _ ->
                None


    let (|ReducibleExpression|_|) (e : Expr) =
        match e with
            | LetCopyOfStruct e     -> Some e
            | Pipe e                -> Some e
            | LambdaApp e           -> Some e

            | _                     -> None

    let (|VectorValue|_|) (v : obj) =
        match v with
            | :? V2d as v -> Some (typeof<float>, [| v.X :> obj; v.Y :> obj|])
            | :? V3d as v -> Some (typeof<float>, [| v.X :> obj; v.Y :> obj; v.Z :> obj|])
            | :? V4d as v -> Some (typeof<float>, [| v.X :> obj; v.Y :> obj; v.Z :> obj; v.W :> obj|])
            | :? V2f as v -> Some (typeof<float32>, [| v.X :> obj; v.Y :> obj|])
            | :? V3f as v -> Some (typeof<float32>, [| v.X :> obj; v.Y :> obj; v.Z :> obj|])
            | :? V4f as v -> Some (typeof<float32>, [| v.X :> obj; v.Y :> obj; v.Z :> obj; v.W :> obj|])
            | :? V2i as v -> Some (typeof<int>, [| v.X :> obj; v.Y :> obj|])
            | :? V3i as v -> Some (typeof<int>, [| v.X :> obj; v.Y :> obj; v.Z :> obj|])
            | :? V4i as v -> Some (typeof<int>, [| v.X :> obj; v.Y :> obj; v.Z :> obj; v.W :> obj|])
            | :? V2l as v -> Some (typeof<int64>, [| v.X :> obj; v.Y :> obj|])
            | :? V3l as v -> Some (typeof<int64>, [| v.X :> obj; v.Y :> obj; v.Z :> obj|])
            | :? V4l as v -> Some (typeof<int64>, [| v.X :> obj; v.Y :> obj; v.Z :> obj; v.W :> obj|])

            | :? C3b as v -> Some (typeof<uint8>, [| v.R :> obj; v.G :> obj; v.B :> obj|])
            | :? C4b as v -> Some (typeof<uint8>, [| v.R :> obj; v.G :> obj; v.B :> obj; v.A :> obj|])
            | :? C3us as v -> Some (typeof<uint16>, [| v.R :> obj; v.G :> obj; v.B :> obj|])
            | :? C4us as v -> Some (typeof<uint16>, [| v.R :> obj; v.G :> obj; v.B :> obj; v.A :> obj|])
            | :? C3ui as v -> Some (typeof<uint32>, [| v.R :> obj; v.G :> obj; v.B :> obj|])
            | :? C4ui as v -> Some (typeof<uint32>, [| v.R :> obj; v.G :> obj; v.B :> obj; v.A :> obj|])
            | :? C3f as v -> Some (typeof<float32>, [| v.R :> obj; v.G :> obj; v.B :> obj|])
            | :? C4f as v -> Some (typeof<float32>, [| v.R :> obj; v.G :> obj; v.B :> obj; v.A :> obj|])
            | :? C3d as v -> Some (typeof<float>, [| v.R :> obj; v.G :> obj; v.B :> obj|])
            | :? C4d as v -> Some (typeof<float>, [| v.R :> obj; v.G :> obj; v.B :> obj; v.A :> obj|])

            | _ -> None
