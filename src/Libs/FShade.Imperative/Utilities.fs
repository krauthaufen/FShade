﻿namespace FShade

open System
open System.Threading
open System.Reflection

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Aardvark.Base

exception FShadeOnlyInShaderCodeException of string

[<AutoOpen>]
module DynamicHostInvocation =
    open System.Collections.Generic

    [<return: Struct>]
    let rec (|ShaderOnlyExn|_|) (e : Exception) =
        match e with
        | FShadeOnlyInShaderCodeException n ->
            ValueSome n

        | :? TargetInvocationException as e ->
            match e.InnerException with
            | ShaderOnlyExn n -> ValueSome n
            | _ -> ValueNone

        | :? AggregateException as a ->
            match Seq.toList a.InnerExceptions with
            | [ ShaderOnlyExn n ] -> ValueSome n
            | _ -> ValueNone

        | _ ->
            ValueNone

    let private invalidHostInvocation = new ThreadLocal<_>(fun _ -> Stack<bool>())

    /// <summary>
    /// Returns a default value and signals that it must not be used by the optimizer.
    /// </summary>
    /// <exception cref="FShadeOnlyInShaderCodeException">Thrown when invoked directly.</exception>
    let onlyInShaderCode<'T> (name : string) : 'T =
        let invalid = invalidHostInvocation.Value

        if invalid.Count = 0 then
            raise <| FShadeOnlyInShaderCodeException name
        else
            // Replace the flag to indicate to the optimizer that the result is invalid.
            // We use a stack rather than a simple flag to avoid potential issues with APC.
            // See: https://stackoverflow.com/questions/8431221/why-did-entering-a-lock-on-a-ui-thread-trigger-an-onpaint-event
            invalid.Pop() |> ignore
            invalid.Push true
            Unchecked.defaultof<'T>

    let inline private tryInvoke (f : unit -> 'T) =
        let invalid = invalidHostInvocation.Value

        try
            invalid.Push false
            let result = f()

            if invalid.Count > 1 then
                Log.warn "[FShade] Stack for invalid host invocations has %d values (should be 1)" invalid.Count

            if invalid.Pop() then None
            else Some result

        with
        | ShaderOnlyExn _ ->
            invalid.Pop() |> ignore
            None

        | _ ->
            invalid.Pop() |> ignore
            reraise()

    type MethodInfo with
        member x.TryInvoke(obj : obj, parameters : obj[]) =
            tryInvoke (fun _ -> x.Invoke(obj, parameters))

        member inline x.TryInvoke(obj : obj, parameters : obj list) =
            x.TryInvoke(obj, Array.ofList parameters)

    type FieldInfo with
        member x.TryGetValue(obj : obj) =
            tryInvoke (fun _ -> x.GetValue(obj))

    type PropertyInfo with
        member x.TryGetValue(obj : obj) =
            tryInvoke (fun _ -> x.GetValue(obj))

        member x.TryGetValue(obj : obj, index : obj[]) =
            tryInvoke (fun _ -> x.GetValue(obj, index))

        member inline x.TryGetValue(obj : obj, index : obj list) =
            x.TryGetValue(obj, Array.ofList index)


module ExprWorkardound = 
    let lockObj = obj()

    let TryGetReflectedDefinition (mb : MethodBase) =
        lock lockObj (fun _ -> 
            try Expr.TryGetReflectedDefinition mb
            with e -> None
        )

module Peano = 
    let getPeanoType (i : int) =
        Aardvark.Base.Peano.getPeanoType i

    let getArrayType (i : int) (content : Type) =
        typedefof<Arr<_,_>>.MakeGenericType [| getPeanoType i; content |]
           
    let getSize (t : Type) =
        Aardvark.Base.Peano.getSize t


    let getItem (e : Expr) (index : Expr) =
        let prop = e.Type.GetProperty("Item")
        Expr.PropertyGet(e, prop, [index])

    let setItem (e : Expr) (index : Expr) (value : Expr) =
        let prop = e.Type.GetProperty("Item")
        Expr.PropertySet(e, prop, value, [index])

[<AutoOpen>]
module ReflectionPatterns =
    open Aardvark.Base.TypeInfo.Patterns

    type Type with
        member x.IsArr = x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<Arr<_,_>>
        member x.IsRef = x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<ref<_>>

     //extracts the (optional) top-most method call from an expression
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

    /// extracts the top-most method-call from an expression.
    /// When no method-call is found the method will raise an exception
    let getMethodInfo (e : Expr) =
        match tryGetMethodInfo e with
            | Some mi -> mi
            | None -> failwithf "[FShade] could not find a method-call in expression %A" e

    let inline private getMethodDefinition (mi : MethodInfo) =
        if mi.IsGenericMethod then mi.GetGenericMethodDefinition()
        else mi

    let private conversionMethods =
        HashSet.ofList [
            getMethodInfo <@ int8 @> 
            getMethodInfo <@ uint8 @> 
            getMethodInfo <@ int16 @> 
            getMethodInfo <@ uint16 @>  
            getMethodInfo <@ int @> 
            getMethodInfo <@ int32 @> 
            getMethodInfo <@ uint @> 
            getMethodInfo <@ uint32 @> 
            getMethodInfo <@ int64 @> 
            getMethodInfo <@ uint64 @> 
            getMethodInfo <@ nativeint @> 
            getMethodInfo <@ unativeint @> 
            getMethodInfo <@ float @> 
            getMethodInfo <@ float32 @> 
        ]

    [<return: Struct>]
    let (|Ref|_|) (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<ref<_>> then
            let content = t.GetGenericArguments().[0]
            ValueSome(content)
        else
            ValueNone

    [<return: Struct>]
    let (|ArrOf|_|) (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Arr<_,_>> then
            let targs = t.GetGenericArguments()
            let len = targs.[0] |> Peano.getSize
            let content = targs.[1]
            ValueSome(len, content)
        else
            ValueNone

    [<return: Struct>]
    let (|ArrayOf|_|) (t : Type) =
        if t.IsArray then ValueSome(t.GetElementType())
        else ValueNone

    [<return: Struct>]
    let (|ConversionMethod|_|) (mi : MethodInfo) =
        let meth = getMethodDefinition mi

        if conversionMethods.Contains meth then
            let input = mi.GetParameters().[0]
            let output = mi.ReturnType
            ValueSome(input.ParameterType, output)
        else
            ValueNone

    [<return: Struct>]
    let (|EnumerableOf|_|) (t : Type) =
        if t.IsArray then
            ValueSome (t.GetElementType())

        elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<seq<_>> then 
            ValueSome (t.GetGenericArguments().[0])

        else
            let iface = t.GetInterface(typedefof<seq<_>>.Name)
            if isNull iface then
                ValueNone
            else
                ValueSome (iface.GetGenericArguments().[0])

    [<return: Struct>]
    let (|FSharpTypeProperty|_|) (pi : PropertyInfo) =
        if FSharpType.IsRecord(pi.DeclaringType, true) then 
            if FSharpType.GetRecordFields(pi.DeclaringType, true) |> Array.exists (fun p -> p = pi) then
                ValueSome ()
            else
                ValueNone

        elif pi.DeclaringType.BaseType <> null && FSharpType.IsUnion(pi.DeclaringType.BaseType, true) then
            let isUnionField =
                FSharpType.GetUnionCases(pi.DeclaringType.BaseType, true)
                    |> Seq.collect (fun c -> c.GetFields())
                    |> Seq.exists (fun p -> p = pi)

            if isUnionField then ValueSome ()
            else ValueNone

        else
            ValueNone

    [<return: Struct>]
    let (|ArrayLengthProperty|_|) (pi : PropertyInfo) =
        if pi.Name = "Length" then
            match pi.DeclaringType with
                | ArrOf _ | ArrayOf _ -> ValueSome ()
                | _ -> ValueNone
        else
            ValueNone

    [<return: Struct>]
    let (|VectorValue|_|) (v : obj) =
        match v with
        | :? V2d as v -> ValueSome (typeof<float>, [| v.X :> obj; v.Y :> obj|])
        | :? V3d as v -> ValueSome (typeof<float>, [| v.X :> obj; v.Y :> obj; v.Z :> obj|])
        | :? V4d as v -> ValueSome (typeof<float>, [| v.X :> obj; v.Y :> obj; v.Z :> obj; v.W :> obj|])
        | :? V2f as v -> ValueSome (typeof<float32>, [| v.X :> obj; v.Y :> obj|])
        | :? V3f as v -> ValueSome (typeof<float32>, [| v.X :> obj; v.Y :> obj; v.Z :> obj|])
        | :? V4f as v -> ValueSome (typeof<float32>, [| v.X :> obj; v.Y :> obj; v.Z :> obj; v.W :> obj|])
        | :? V2i as v -> ValueSome (typeof<int>, [| v.X :> obj; v.Y :> obj|])
        | :? V3i as v -> ValueSome (typeof<int>, [| v.X :> obj; v.Y :> obj; v.Z :> obj|])
        | :? V4i as v -> ValueSome (typeof<int>, [| v.X :> obj; v.Y :> obj; v.Z :> obj; v.W :> obj|])
        | :? V2ui as v -> ValueSome (typeof<uint>, [| v.X :> obj; v.Y :> obj|])
        | :? V3ui as v -> ValueSome (typeof<uint>, [| v.X :> obj; v.Y :> obj; v.Z :> obj|])
        | :? V4ui as v -> ValueSome (typeof<uint>, [| v.X :> obj; v.Y :> obj; v.Z :> obj; v.W :> obj|])
        | :? V2l as v -> ValueSome (typeof<int64>, [| v.X :> obj; v.Y :> obj|])
        | :? V3l as v -> ValueSome (typeof<int64>, [| v.X :> obj; v.Y :> obj; v.Z :> obj|])
        | :? V4l as v -> ValueSome (typeof<int64>, [| v.X :> obj; v.Y :> obj; v.Z :> obj; v.W :> obj|])

        | :? C3b as v -> ValueSome (typeof<uint8>, [| v.R :> obj; v.G :> obj; v.B :> obj|])
        | :? C4b as v -> ValueSome (typeof<uint8>, [| v.R :> obj; v.G :> obj; v.B :> obj; v.A :> obj|])
        | :? C3us as v -> ValueSome (typeof<uint16>, [| v.R :> obj; v.G :> obj; v.B :> obj|])
        | :? C4us as v -> ValueSome (typeof<uint16>, [| v.R :> obj; v.G :> obj; v.B :> obj; v.A :> obj|])
        | :? C3ui as v -> ValueSome (typeof<uint32>, [| v.R :> obj; v.G :> obj; v.B :> obj|])
        | :? C4ui as v -> ValueSome (typeof<uint32>, [| v.R :> obj; v.G :> obj; v.B :> obj; v.A :> obj|])
        | :? C3f as v -> ValueSome (typeof<float32>, [| v.R :> obj; v.G :> obj; v.B :> obj|])
        | :? C4f as v -> ValueSome (typeof<float32>, [| v.R :> obj; v.G :> obj; v.B :> obj; v.A :> obj|])
        | :? C3d as v -> ValueSome (typeof<float>, [| v.R :> obj; v.G :> obj; v.B :> obj|])
        | :? C4d as v -> ValueSome (typeof<float>, [| v.R :> obj; v.G :> obj; v.B :> obj; v.A :> obj|])

        | _ -> ValueNone

    [<return: Struct>]
    let (|MatrixValue|_|) (v : obj) =
        match v with
        | :? IMatrix as m ->
            let values =
                [|
                    for r in 0 .. int m.Dim.Y - 1 do
                        for c in 0 .. int m.Dim.X - 1 do
                            yield m.GetValue(int64 c, int64 r)
                |]
            let bt = values.[0].GetType()
            ValueSome (bt, values)
        | _ -> 
            ValueNone

    [<return: Struct>]
    let (|SwitchableType|_|) (t : Type) =
        match t with
        | Integral 
        | Enum -> ValueSome ()
        | _ -> ValueNone

    [<return: Struct>]
    let (|VecMethod|_|) (mi : MethodInfo) =
        match mi with
        | Method(name, args) when mi.DeclaringType = typeof<Vec> -> ValueSome (name, args)
        | _ -> ValueNone

    [<return: Struct>]
    let (|MatMethod|_|) (mi : MethodInfo) =
        match mi with
        | Method(name, args) when mi.DeclaringType = typeof<Mat> -> ValueSome (name, args)
        | _ -> ValueNone

    // Invoking these operators for enum types will
    // throw a TargetInvocationException, even with a witness.
    // Handle those explicitly instead.
    let private bitwiseOps : MethodInfo -> Option<Type -> obj -> obj -> obj> =
        let make op = (fun (x : obj) (y : obj) -> box (op (unbox x) (unbox y)))

        let bitwiseOr =
            LookupTable.lookup [
                typeof<int8>,   make ((|||) : int8 -> _ -> _)
                typeof<uint8>,  make ((|||) : uint8 -> _ -> _)
                typeof<int16>,  make ((|||) : int16 -> _ -> _)
                typeof<uint16>, make ((|||) : uint16 -> _ -> _)
                typeof<int32>,  make ((|||) : int32 -> _ -> _)
                typeof<uint32>, make ((|||) : uint32 -> _ -> _)
                typeof<int64>,  make ((|||) : int64 -> _ -> _)
                typeof<uint64>, make ((|||) : uint64 -> _ -> _)
            ]

        let bitwiseAnd =
            LookupTable.lookup [
                typeof<int8>,   make ((&&&) : int8 -> _ -> _)
                typeof<uint8>,  make ((&&&) : uint8 -> _ -> _)
                typeof<int16>,  make ((&&&) : int16 -> _ -> _)
                typeof<uint16>, make ((&&&) : uint16 -> _ -> _)
                typeof<int32>,  make ((&&&) : int32 -> _ -> _)
                typeof<uint32>, make ((&&&) : uint32 -> _ -> _)
                typeof<int64>,  make ((&&&) : int64 -> _ -> _)
                typeof<uint64>, make ((&&&) : uint64 -> _ -> _)
            ]

        let bitwiseXor =
            LookupTable.lookup [
                typeof<int8>,   make ((^^^) : int8 -> _ -> _)
                typeof<uint8>,  make ((^^^) : uint8 -> _ -> _)
                typeof<int16>,  make ((^^^) : int16 -> _ -> _)
                typeof<uint16>, make ((^^^) : uint16 -> _ -> _)
                typeof<int32>,  make ((^^^) : int32 -> _ -> _)
                typeof<uint32>, make ((^^^) : uint32 -> _ -> _)
                typeof<int64>,  make ((^^^) : int64 -> _ -> _)
                typeof<uint64>, make ((^^^) : uint64 -> _ -> _)
            ]

        LookupTable.tryLookup [
            getMethodInfo <@ (|||) : int -> int -> int @>, bitwiseOr
            getMethodInfo <@ (&&&) : int -> int -> int @>, bitwiseAnd
            getMethodInfo <@ (^^^) : int -> int -> int @>, bitwiseXor
        ]

    let private shiftOps : MethodInfo -> Option<Type -> obj -> int -> obj> =
        let make op = (fun (x : obj) (y : obj) -> box (op (unbox x) (unbox y)))

        let bitwiseLsh =
            LookupTable.lookup [
                typeof<int8>,   make ((<<<) : int8 -> _ -> _)
                typeof<uint8>,  make ((<<<) : uint8 -> _ -> _)
                typeof<int16>,  make ((<<<) : int16 -> _ -> _)
                typeof<uint16>, make ((<<<) : uint16 -> _ -> _)
                typeof<int32>,  make ((<<<) : int32 -> _ -> _)
                typeof<uint32>, make ((<<<) : uint32 -> _ -> _)
                typeof<int64>,  make ((<<<) : int64 -> _ -> _)
                typeof<uint64>, make ((<<<) : uint64 -> _ -> _)
            ]

        let bitwiseRsh =
            LookupTable.lookup [
                typeof<int8>,   make ((>>>) : int8 -> _ -> _)
                typeof<uint8>,  make ((>>>) : uint8 -> _ -> _)
                typeof<int16>,  make ((>>>) : int16 -> _ -> _)
                typeof<uint16>, make ((>>>) : uint16 -> _ -> _)
                typeof<int32>,  make ((>>>) : int32 -> _ -> _)
                typeof<uint32>, make ((>>>) : uint32 -> _ -> _)
                typeof<int64>,  make ((>>>) : int64 -> _ -> _)
                typeof<uint64>, make ((>>>) : uint64 -> _ -> _)
            ]

        LookupTable.tryLookup [
            getMethodInfo <@ (<<<) : int -> int -> int @>, bitwiseLsh
            getMethodInfo <@ (>>>) : int -> int -> int @>, bitwiseRsh
        ]

    [<return: Struct>]
    let private (|Enum|_|) (t : Type) =
        if t.IsEnum then ValueSome <| t.GetEnumUnderlyingType()
        else ValueNone

    [<return: Struct>]
    let (|EnumBitwiseOp|_|) (mi : MethodInfo) =
        let mdef = getMethodDefinition mi

        match bitwiseOps mdef with
        | Some op ->
            let typ = mi.GetGenericArguments().[0]
            match typ with
            | Enum baseType ->
                ValueSome (typ, baseType, op baseType)
            | _ ->
                ValueNone
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|EnumShiftOp|_|) (mi : MethodInfo) =
        let mdef = getMethodDefinition mi

        match shiftOps mdef with
        | Some op ->
            let typ = mi.GetGenericArguments().[0]
            match typ with
            | Enum baseType ->
                ValueSome (typ, baseType, op baseType)
            | _ ->
                ValueNone
        | _ ->
            ValueNone

    let private converters : Type -> Option<obj -> obj> =
        let make f = fun (i : obj) -> box (f i)

        LookupTable.tryLookup [
            typeof<int8>,       make Convert.ToSByte
            typeof<uint8>,      make Convert.ToByte
            typeof<int16>,      make Convert.ToInt16
            typeof<uint16>,     make Convert.ToUInt16
            typeof<int32>,      make Convert.ToInt32
            typeof<uint32>,     make Convert.ToUInt32
            typeof<int64>,      make Convert.ToInt64
            typeof<uint64>,     make Convert.ToUInt64
            typeof<float32>,    make Convert.ToSingle
            typeof<float>,      make Convert.ToDouble
        ]

    [<return: Struct>]
    let (|EnumConversion|_|) (mi : MethodInfo) =
        let mdef = getMethodDefinition mi

        if conversionMethods.Contains mdef then
            match mi.GetGenericArguments().[0] with
            | Enum baseType ->
                match converters mi.ReturnType with
                | Some f -> ValueSome (f, baseType)
                | _ -> ValueNone
            | _ ->
                ValueNone
        else
            ValueNone

[<AutoOpen>]
module ExprExtensions =

    module private Methods = 

        let operators = getMethodInfo(<@ (+) @>).DeclaringType

        let getArray = getMethodInfo <@ Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetArray @>
        let setArray = getMethodInfo <@ Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.SetArray @>
        let unboxGeneric = getArray.DeclaringType.GetMethod "UnboxGeneric"

        let opRangeStep = operators.GetMethod("op_RangeStep")
        let ignore = getMethodInfo <@ ignore @>

        let unroll = getMethodInfo <@ Preprocessor.unroll : unit -> unit @>

    // Temporary work around for
    // https://github.com/dotnet/fsharp/issues/9903
    let rec private namedValues = function
        | ValueWithName(v, t, name) ->
            if name.Contains "@" then Map.empty
            else Map.ofList [name, (v,t)]
        | ShapeVar _ -> Map.empty
        | ShapeLambda(_,b) -> namedValues b
        | ShapeCombination(_,args) ->
            let mutable res = Map.empty
            for a in args do
                res <- Map.union res (namedValues a)
            res

    module private Witness =
        open System.Collections.Concurrent

        let private buildFun =
            match <@ FunctionReflection.buildFunction null null : int -> int @> with
            | Patterns.Call(None, mi, _) ->
                mi.GetGenericMethodDefinition()
            | _ ->
                failwith "bad"

        let private witnessTable =
            ConcurrentDictionary<MethodInfo, obj>()

        let compile (e : Expr) =
            match e with
            | DerivedPatterns.Lambdas(_, Patterns.Call(None, mi, _)) ->
                witnessTable.GetOrAdd(mi, fun mi ->
                    let m = buildFun.MakeGenericMethod [|e.Type|]
                    m.Invoke(null, [|null; mi|])
                )
            | _ ->
                failwith "bad witness"

    type Expr with
    
        member x.DebugRange =
            x.CustomAttributes |> List.tryPick (fun e ->
                match e with
                    | NewTuple [String "DebugRange"; NewTuple [ String file; Int32 startLine; Int32 startCol; Int32 endLine; Int32 endCol]] ->
                        Some(file, startLine, startCol, endLine, endCol)
                    | _ ->
                        None
            )

        member x.Method =
            x.CustomAttributes |> List.tryPick (fun e ->
                match e with
                    | NewTuple [String "Method"; Value((:? System.Reflection.MethodBase as m),_)] ->
                        Some m
                    | _ ->
                        None
            )
            
        member x.NamedValues =
            namedValues x

        static member Ignore(e : Expr) =
            if e.Type = typeof<unit> then 
                e
            else
                Expr.Call(Methods.ignore.MakeGenericMethod [| e.Type |], [e])

        static member Seq (es : list<Expr>) =
            match es with
                | [] -> Expr.Unit
                | l :: rest -> 
                    match l, Expr.Seq rest with
                        | Unit, r -> r
                        | l, Unit -> l
                        | l, r -> Expr.Sequential(l, r)


        static member Lambdas(args : list<Var>, body : Expr) =
            match args with
            | [] -> body
            | a :: r -> Expr.Lambda(a, Expr.Lambdas(r, body))

        static member Unit =
            Expr.Value(())

        /// creates an array-indexing expression using the supplied arguments
        static member ArrayAccess(arr : Expr, index : Expr) =
            match arr.Type with
                | ArrayOf t -> 
                    let get = Methods.getArray.MakeGenericMethod([| arr.Type.GetElementType() |])
                    Expr.Call(get, [arr;index])
                | t ->
                    let prop = t.GetProperty "Item"

                    if isNull prop then
                        failwithf "[FShade] not an array type: %A" t

                    let ip = prop.GetIndexParameters()
                    if ip.Length <> 1 || ip.[0].ParameterType <> typeof<int> then
                        failwithf "[FShade] not an array type: %A" t

                    Expr.PropertyGet(arr, prop, [index])
            
        /// creates an array-indexing expression using the supplied arguments
        static member ArraySet(arr : Expr, index : Expr, value : Expr) =
            match arr.Type with
                | ArrayOf t -> 
                    let set = Methods.setArray.MakeGenericMethod([| t |])
                    Expr.Call(set, [arr;index;value])
                | t ->
                    let prop = t.GetProperty "Item"

                    if isNull prop then
                        failwithf "[FShade] not an array type: %A" t

                    let ip = prop.GetIndexParameters()
                    if ip.Length <> 1 || ip.[0].ParameterType <> typeof<int> then
                        failwithf "[FShade] not an array type: %A" t
                        

                    Expr.PropertySet(arr, prop, value, [index])

        /// creates a new fixed-size-array using the given element-type and values
        static member NewFixedArray(et : Type, values : list<Expr>) =
            let len = values |> List.length
            let arrType = Peano.getArrayType len et
            let seqType = typedefof<seq<_>>.MakeGenericType et
            let ctor = arrType.GetConstructor [| seqType |]
            Expr.NewObject(ctor, [Expr.Coerce(Expr.NewArray(et, values), seqType)])

        /// creates a ForEach-Expression using the standard layout as used by F#-quotations
        static member ForEach(v : Var, seq : Expr, body : Expr) =
            let sType = typeof<System.Collections.Generic.IEnumerable<obj>>.GetGenericTypeDefinition().MakeGenericType([|v.Type|])
            let eType = typeof<System.Collections.Generic.IEnumerator<obj>>.GetGenericTypeDefinition().MakeGenericType([|v.Type|])
            let e = Var("enumerator", eType)

            let unboxDisposable = Methods.unboxGeneric.MakeGenericMethod([|typeof<IDisposable>|])

            let getEnumerator = sType.GetMethod("GetEnumerator")
            let dispose = typeof<IDisposable>.GetMethod("Dispose")
            let moveNext = typeof<System.Collections.IEnumerator>.GetMethod("MoveNext")

            Expr.Let(e, Expr.Call(Expr.Coerce(seq, sType), getEnumerator, []),
                Expr.TryFinally(
                    Expr.WhileLoop(Expr.Call(Expr.Var e, moveNext, []),
                        Expr.Let(v, Expr.PropertyGet(Expr.Var e, eType.GetProperty("Current"), []),
                            body
                        )
                    ),
                    Expr.IfThenElse(Expr.TypeTest(Expr.Coerce(Expr.Var e, typeof<obj>), typeof<IDisposable>),
                        Expr.Call(Expr.Call(unboxDisposable, [Expr.Coerce(Expr.Var e, typeof<obj>)]), dispose, []),
                        Expr.Value(())
                    )
                )
            )

        static member ForInteger(v : Var, first : Expr, step : Expr, last : Expr, body : Expr) =
            match step with
                | Value((:? int as i), _) when i = 1 -> 
                    Expr.ForIntegerRangeLoop(v, first, last, body)

                | _ ->
                    let seq = Expr.Call(Methods.opRangeStep.MakeGenericMethod [| first.Type; step.Type |], [first;step;last])
                    let inputSequence = Var("inputSequence", seq.Type)
                    Expr.Let(
                        inputSequence, seq, 
                        Expr.ForEach(v, Expr.Var inputSequence, body)
                    )

        static member CompileWitness(e : Expr) =
            Witness.compile e

        /// tries to evaluate the supplied expressions and returns their values on success
        static member TryEval(e : Expr list) =
            let values = e |> List.map Expr.TryEval
            if values |> List.forall Option.isSome then
                values |> List.map Option.get |> Some
            else
                None

        /// tries to evaluate the supplied expression and returns its value on success
        static member TryEval(e : Expr) =
            match e with
            | Patterns.PropertyGet(t, p, args) ->
                match t with
                | Some t ->
                    match Expr.TryEval t, Expr.TryEval args with
                    | Some t, Some args -> p.TryGetValue(t, args)
                    | _ -> None
                | None ->
                    match Expr.TryEval args with
                    | Some args -> p.TryGetValue(null, args)
                    | _ -> None

            | Patterns.FieldGet(t,f) ->
                match t with
                | Some t ->
                    match Expr.TryEval t with
                    | Some t -> f.TryGetValue t
                    | None -> None
                | None ->
                    f.TryGetValue(null)

            | Patterns.Call(None, EnumBitwiseOp (enumType, baseType, op), [l; r]) ->
                match Expr.TryEval l, Expr.TryEval r with
                | Some l, Some r ->
                    let x = Convert.ChangeType(l, baseType)
                    let y = Convert.ChangeType(r, baseType)
                    Some <| Enum.ToObject(enumType, op x y)
                | _ ->
                    None

            | Patterns.Call(None, EnumShiftOp (enumType, baseType, op), [l; r]) ->
                match Expr.TryEval l, Expr.TryEval r with
                | Some l, Some (:? int32 as shift) ->
                    let x = Convert.ChangeType(l, baseType)
                    Some <| Enum.ToObject(enumType, op x shift)
                | _ ->
                    None

            | Patterns.Call(None, EnumConversion (fromInt, intType), [x]) ->
                match Expr.TryEval x with
                | Some x ->
                    let xi = Convert.ChangeType(x, intType)
                    Some <| fromInt xi
                | _ ->
                    None

            | Patterns.CallWithWitnesses(t, _, mi, witnesses, args) ->
                match t with
                | Some t ->
                    match Expr.TryEval t, Expr.TryEval args with
                    | Some t, Some args ->
                        try
                            let ws = witnesses |> List.map Expr.CompileWitness
                            mi.TryInvoke(t, ws @ args)
                        with _ ->
                            None
                    | _ -> None

                | None ->
                    match Expr.TryEval args with
                    | Some args ->
                        try
                            let ws = witnesses |> List.map Expr.CompileWitness
                            mi.TryInvoke(null, ws @ args)
                        with _ ->
                            None
                    | _ -> None

            | Patterns.Call(t, mi, args) ->
                match t with
                | Some t ->
                    match Expr.TryEval t, Expr.TryEval args with
                    | Some t, Some args -> mi.TryInvoke(t, args)
                    | _ -> None

                | None ->
                    match Expr.TryEval args with
                    | Some args -> mi.TryInvoke(null, args)
                    | _ -> None

            | Patterns.Let(var,value,body) ->
                let value = Expr.TryEval value
                match value with
                | Some value ->
                    let body = body.Substitute(fun vi -> if vi = var then Expr.Value(value, vi.Type) |> Some else None)
                    Expr.TryEval body
                | None -> None

            | Patterns.IfThenElse (condition, a, b) ->
                match Expr.TryEval condition with
                | Some (:? bool as flag) -> Expr.TryEval (if flag then a else b)
                | _ -> None

            | Patterns.Value(v,_) ->
                v |> Some

            | Patterns.DefaultValue t ->
                Activator.CreateInstance(t) |> Some

            | _ -> None

    [<return: Struct>]
    let (|Seq|_|) (e : Expr) =
        let rec all (e : Expr) =
            match e with
            | Sequential(l,r) -> all l @ all r
            | Unit -> []
            | e -> [e]

        match e with
        | Sequential(l, r) -> all l @ all r |> ValueSome
        | _ -> ValueNone

    [<return: Struct>]
    let (|NewFixedArray|_|) (e : Expr) =
        match e with
        | NewObject(ctor, []) ->
            let targs = ctor.DeclaringType.GetGenericArguments()
            let len = Peano.getSize (targs.[0])
            let et = targs.[1]
            ValueSome(len, et, [])

        | NewObject(ctor, [Coerce(NewArray(et, args),_)]) ->
            if ctor.DeclaringType.IsGenericType && ctor.DeclaringType.GetGenericTypeDefinition() = typedefof<Arr<_,_>> then
                let len = Peano.getSize (ctor.DeclaringType.GetGenericArguments().[0])
                ValueSome(len, et, args)
            else 
                ValueNone
        | _ ->
            ValueNone

    let rec private deconstructTuples x =
        match x with
        | NewTuple(args) -> args |> List.collect deconstructTuples
        | _ -> [x]

    let rec private inlineUselessAbstractions(e : Expr) =
        match e with
        | Application(Lambda(v,b),a) ->
            let b = b.Substitute(fun vi -> if vi = v then Some a else None)
            inlineUselessAbstractions b

        | Application(Let(f,l0, b), a) ->
            let b = b.Substitute(fun vi -> if vi = f then Some l0 else None)
            inlineUselessAbstractions (Expr.Application(b,a))

        | _ ->
            e

    /// detects the type of an expression
    let (|ExprOf|) (e : Expr) =
        ExprOf(e.Type)

    [<return: Struct>]
    let (|Ignore|_|) (e : Expr) =
        match e with
        | Call(None, mi, [a]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = Methods.ignore ->
            ValueSome a
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|GetArray|_|) (e : Expr) =
        match e with
        | Call(None, mi, [arr;idx]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = Methods.getArray ->
            ValueSome(arr, idx)
        | PropertyGet(Some arr, prop, [idx]) when prop.Name = "Item" && arr.Type.IsGenericType && arr.Type.GetGenericTypeDefinition() = typedefof<Arr<_,_>> ->
            ValueSome(arr, idx)
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|SetArray|_|) (e : Expr) =
        match e with
        | Call(None, mi, [arr;idx;value]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = Methods.setArray ->
            ValueSome(arr, idx, value)

        | PropertySet(Some arr, prop, [idx], value) when prop.Name = "Item" && arr.Type.IsGenericType && arr.Type.GetGenericTypeDefinition() = typedefof<Arr<_,_>> ->
            ValueSome(arr, idx, value)

        | _ ->
            ValueNone

    /// detects FieldGet and PropertyGet expressions having no indexers
    [<return: Struct>]
    let (|MemberFieldGet|_|) (e : Expr) =
        match e with
        | PropertyGet(Some t,p,[]) ->
            ValueSome(t, p :> MemberInfo)
        | FieldGet(Some t, f) ->
            ValueSome(t, f :> MemberInfo)
        | _ -> 
            ValueNone
                
    /// detects FieldSet and PropertySet expressions having no indexers
    [<return: Struct>]
    let (|MemberFieldSet|_|) (e : Expr) =
        match e with
        | PropertySet(Some t,p,[] , v) ->
            ValueSome(t, p :> MemberInfo, v)
        | FieldSet(Some t, f, v) ->
            ValueSome(t, f :> MemberInfo, v)
        | _ ->
            ValueNone
               
    /// detects pipe-expressions like (a |> sin, a |> clamp 0 1, sin <| a + b, etc.)
    [<return: Struct>]
    let (|Pipe|_|) (e : Expr) =
        match e with
        | Call(None, Method("op_PipeLeft", _), [Lambda(v,l);r]) ->
            let e = l.Substitute(fun vi -> if vi = v then Some r else None)
            Pipe(e) |> ValueSome

        | Call(None, Method("op_PipeRight", _), [l;Lambda(v, r)]) ->
            let e = r.Substitute(fun vi -> if vi = v then Some l else None)
            Pipe(e) |> ValueSome

        | Call(None, Method("op_PipeLeft", _), [PropertyGet(t,p, []);r]) ->
            let r = deconstructTuples r
            match t with
            | None -> 
                let mi = p.DeclaringType.GetMethod(p.Name, r |> Seq.map (fun ai -> ai.Type) |> Seq.toArray)
                if mi <> null then(Expr.Call(mi, r)) |> ValueSome
                else failwith "function is not a method"
            | Some t -> 
                failwith "function is not a method"

        | Call(None, Method("op_PipeRight", _), [l;PropertyGet(t,p, [])]) ->
            let l = deconstructTuples l
            match t with
            | None -> 
                let mi = p.DeclaringType.GetMethod(p.Name, l |> Seq.map (fun ai -> ai.Type) |> Seq.toArray)
                if mi <> null then(Expr.Call(mi, l)) |> ValueSome
                else failwith "function is not a method"
            | Some t -> 
                failwith "function is not a method"

        | Call(None, Method("op_PipeLeft", _), [f;r]) ->
            let e = Expr.Application(f, r) |> inlineUselessAbstractions
            Pipe(e) |> ValueSome

        | Call(None, Method("op_PipeRight", _), [l;f]) ->
            let e = Expr.Application(f, l) |> inlineUselessAbstractions
            Pipe(e) |> ValueSome

        | _ -> ValueNone

    /// F# creates mutable copies for structs when accessing properties/methods
    /// since mutating the original is not possible (and not desired).
    /// since everything is mutable in C we don't care for those copies making the
    /// code less readable and more complicated
    [<return: Struct>]
    let (|LetCopyOfStruct|_|) (e : Expr) =
        match e with
        | Let(v, e, b) when v.Name = "copyOfStruct" && v.Type.IsValueType ->
            // TODO: find a better way for detecting this
            let mutable count = 0
            let newBody = b.Substitute(fun vi -> if v = vi then count <- count + 1; Some e else None) 
            if count = 0 then ValueSome b
            elif count = 1 then ValueSome newBody
            else ValueNone
        | _ ->
            ValueNone

    /// detects a trivial (low runtime) expression like Var/Value/FieldGet/PropertyGet(for f# types)/etc.
    [<return: Struct>]
    let rec (|Trivial|_|) (e : Expr) =
        match e with
        | Var _ 
        | Value _
        | FieldGet(None, _)
        | PropertyGet(None, _, [])
        | TupleGet(Trivial, _)
        | PropertyGet(Some Trivial, (FSharpTypeProperty | ArrayLengthProperty), [])
        | FieldGet(Some Trivial, _) ->
            ValueSome ()
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|OperatorMethod|_|) (mi : MethodInfo) =
        if mi.Name.StartsWith "op_" then ValueSome ()
        else ValueNone



    /// detects a trivial (low runtime) expression like (|Trivial|_|) but also allows for operator-calls
    [<return: Struct>]
    let rec (|TrivialOp|_|) (e : Expr) =
        match e with
        | Var _ 
        | Value _
        | FieldGet(None, _)
        | PropertyGet(None, _, [])
        | TupleGet(TrivialOp, _)
        | PropertyGet(Some TrivialOp, (FSharpTypeProperty | ArrayLengthProperty), [])
        | Call(None, OperatorMethod, [TrivialOp; TrivialOp])
        | Call(None, OperatorMethod, [TrivialOp])
        | FieldGet(Some TrivialOp, _) ->
            ValueSome ()
        | _ ->
            ValueNone


    [<return: Struct>]
    let rec (|SaturatedLambda|_|) (e : Expr) : ValueOption<list<Var * Expr> * Expr> =
        match e with
        | Application(Lambda(v,b), a) ->
            ValueSome ([v,a], b)

        | Application(SaturatedLambda(bindings, Lambda(v,body)), a) ->
            ValueSome ((v,a) :: bindings, body)

        | _ ->
            ValueNone

    /// detects direct applications of lambdas and replaces them with let
    /// bindings or inlines the expression if either the argument is trivial or it occurs
    /// only once in the lambda-body
    [<return: Struct>]
    let rec (|LambdaApp|_|) (e : Expr) =
        match e with
        | SaturatedLambda(bindings, body) ->
            let mutable b = body

            for (v,e) in bindings do
                let mutable cnt = 0
                let nb = b.Substitute(fun vi -> if vi = v then cnt <- cnt + 1; Some e else None)
                    
                match e with
                    | Trivial | _ when cnt <= 1 -> 
                        b <- nb
                    | _ ->
                        b <- Expr.Let(v, e, b)
            ValueSome(b)
                    

//            | Application(Lambda(v,b), arg) ->
//                let mutable cnt = 0
//                let nb = b.Substitute(fun vi -> if vi = v then cnt <- cnt + 1; ValueSome arg else ValueNone)
//                
//                match arg with
//                    | Trivial | _ when cnt <= 1 -> 
//                        match nb with 
//                            | LambdaApp(nb) -> ValueSome nb
//                            | _ -> ValueSome nb
//                    | _ ->  
//                        match b with
//                            | LambdaApp(b) -> Expr.Let(v, arg, b) |> ValueSome
//                            | _ -> Expr.Let(v, arg, b) |> ValueSome
//
//
//            | Application(LambdaApp(Let(v,e,Lambda(vl,bl))), arg) ->
//                let mutable cnt = 0
//                let nb = bl.Substitute(fun vi -> if vi = vl then cnt <- cnt + 1; ValueSome arg else ValueNone)
//                
//                match arg with
//                    | Trivial | _ when cnt <= 1 -> Expr.Let(v,e,nb) |> ValueSome
//                    | _ ->  Expr.Let(v, e, Expr.Let(vl, arg, bl)) |> ValueSome
//                

        | _ ->
            ValueNone

    

    /// reduces an expression
    [<return: Struct>]
    let (|ReducibleExpression|_|) (e : Expr) =
        match e with
        | LetCopyOfStruct e     -> ValueSome e
        | Pipe e                -> ValueSome e
        | LambdaApp e           -> ValueSome e
        | Ignore e              -> ValueSome e

        | _                     -> ValueNone

    [<return: Struct>]
    let (|OptionalCoerce|_|) (e : Expr) =
        match e with
        | Coerce(e,t) -> ValueSome(e,t)
        | _ -> ValueSome(e, e.Type)

    /// detects foreach expressions using the F# standard-layout
    [<return: Struct>]
    let (|ForEach|_|) (e : Expr) =
        match e with
        | Let(e, Call(Some(OptionalCoerce(seq,_)), Method("GetEnumerator",_), []),
                TryFinally(
                    WhileLoop(Call(Some (Var e1), Method("MoveNext",_), []),
                        Let(i, PropertyGet(Some (Var e2), current, []), b)
                    ),
                    IfThenElse(TypeTest(OptionalCoerce(Var e3, oType0), dType),
                        Call(Some (Call(None, Method("UnboxGeneric",_), [OptionalCoerce(e4, oType1)])), Method("Dispose",_), []),
                        Value(_)
                    )
                )
            ) when e1 = e && e2 = e && e3 = e && current.Name = "Current" && oType0 = typeof<obj> && oType1 = typeof<obj> && dType = typeof<System.IDisposable> ->
            ValueSome(i, seq, b)
        | _ -> 
            ValueNone

    [<return: Struct>]
    let (|CreateRange|_|) (e : Expr) =
        match e with
        | Call(None, Method("op_RangeStep",_), [first; step; last]) -> ValueSome(first, step, last)
        | Call(None, Method("op_Range",_), [first; last]) -> ValueSome(first, Expr.Value(1), last)
        | _ -> ValueNone

    [<return: Struct>]
    let rec (|RangeSequence|_|) (e : Expr) =
        match e with
        | Call(None, Method(("ToArray" | "ToList"), _), [RangeSequence(first, step, last)]) ->
            ValueSome(first, step, last)

        | Call(None, Method("CreateSequence",_), [RangeSequence(first, step, last)]) ->
            ValueSome(first, step, last)

        | CreateRange(first, step, last) ->
            ValueSome(first, step, last)

        | _ ->
            ValueNone

    [<return: Struct>]
    let (|ForInteger|_|) (e : Expr) =
        match e with
        | ForIntegerRangeLoop(v,first,last,body) -> 
            ValueSome(v,first,Expr.Value(1),last,body)

        | Let(seq, RangeSequence(first, step, last), ForEach(v, Var seq1, body)) when seq = seq1 ->
            ValueSome(v, first, step, last, body)

        | _ -> ValueNone

    let rec private findSwitchCases (value : Expr) (label : ValueOption<obj>) (e : Expr) =
        match label,e with
        | ValueNone, IfThenElse(Call(None, Method("op_Equality", [SwitchableType; SwitchableType]), [a;Value(c,SwitchableType)]),ifTrue,ifFalse) when a = value -> 
            let l = findSwitchCases value (ValueSome c) ifTrue
            let r = findSwitchCases value ValueNone ifFalse
            match l,r with
                | ValueSome l, ValueSome r -> List.concat [l; r] |> ValueSome
                | _ -> ValueNone

        | ValueSome l, e -> ValueSome [(l,e)]
        | ValueNone, e -> ValueSome [(null,e)]

    /// detects ifthenelse cascades which use only equality on integral types
    [<return: Struct>]
    let (|Switch|_|) (e : Expr) =
        match e with
        | IfThenElse(Call(None, Method("op_Equality", [SwitchableType; SwitchableType]), [a;Value(c,SwitchableType)]),_,_) -> 
            match findSwitchCases a ValueNone e with
            | ValueSome(cases) -> Switch(a, cases) |> ValueSome
            | _ -> ValueNone
        | _ -> ValueNone

    let rec private findAlternatives (e : Expr) =
        match e with
        | IfThenElse(c,i,e) ->
            (ValueSome c,i)::findAlternatives e
        | _ -> 
            [ValueNone,e]

    /// detects ifthenelse cascades and returns them as list
    [<return: Struct>]
    let (|Alternatives|_|) (e : Expr) =
        match e with
        | IfThenElse(_, _, IfThenElse(_,_,_)) -> 
            let alts = findAlternatives e

            let e = alts |> List.filter(fun (c,_) -> c.IsNone) |> List.head |> snd
            let c = alts |> List.choose(fun (c,b) -> match c with | ValueSome c -> Some(c,b) | _ -> None)

            Alternatives(c, e) |> ValueSome

        | _ -> ValueNone

    [<return: Struct>]
    let (|Unroll|_|) (e : Expr) =
        match e with
        | Call(None, mi, []) when mi = Methods.unroll -> ValueSome ()
        | _ -> ValueNone

    let (|Constant|_|) (e : Expr) =
        Expr.TryEval e

[<AutoOpen>]
module StateExtensions =
    open Aardvark.Base.Monads.State

    module Array =
        let mapS (f : 'a -> State<'s, 'b>) (m : 'a[]) : State<'s, 'b[]> =
            { new State<'s, 'b[]>() with
                override x.Run(s : byref<'s>) =
                    let res = Array.zeroCreate m.Length
                    for i in 0 .. m.Length - 1 do
                        res.[i] <- f(m.[i]).Run(&s)
                    res
            }

    module Map =
        let mapS (f : 'a -> 'b -> State<'s, 'c>) (m : Map<'a, 'b>) : State<'s, Map<'a, 'c>> =
            { new State<'s, Map<'a, 'c>>() with
                override x.Run(s : byref<'s>) =
                    let mutable res = Map.empty
                    for (KeyValue(k,v)) in m do
                        let v = (f k v).Run(&s)
                        res <- Map.add k v res
                    res
            }

    module List =
        let rec choose2 (f : 'a -> Choice<'b, 'c>) (m : list<'a>) =
            match m with
                | [] -> [], []
                | h :: t ->
                    let l, r = choose2 f t
                    match f h with
                        | Choice1Of2 b -> (b :: l, r)
                        | Choice2Of2 c -> (l, c :: r)


        let rec choose2S (f : 'a -> State<'s, Choice<'b, 'c>>) (m : list<'a>) =
            state {
                match m with
                    | [] -> return [], []
                    | h :: t ->
                        let! (l, r) = choose2S f t
                        let! res = f h
                        match res with
                            | Choice1Of2 b -> return (b :: l, r)
                            | Choice2Of2 c -> return (l, c :: r)
            }

[<AutoOpen>]
module private Helpers = 
    open System.Text.RegularExpressions
    open Aardvark.Base.Monads.State

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
                if typeof<INatural>.IsAssignableFrom t then
                    let value = Peano.getSize t
                    sprintf "N%d" value
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

    let inline (>>=) (m : State<'s, 'a>) (f : 'a -> State<'s, 'b>) =
        m |> State.bind f

    let inline (>>>=) (m : 'a -> State<'s, 'b>) (f : 'b -> State<'s, 'c>) =
        m >> State.bind f

    let inline (|>>) (m : State<'s, 'a>) (f : 'a -> 'b) =
        m |> State.map f
