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
