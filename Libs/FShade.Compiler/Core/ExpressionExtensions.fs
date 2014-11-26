namespace FShade.Compiler

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open FShade.Utils
open Aardvark.Base.TypeInfo.Patterns

[<AutoOpen>]
module ExpressionExtensions =
    let private getArray = getMethodInfo <@ Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetArray @>

    /// <summary>
    /// Some missing Constructors and functions for F#'s Expr-type
    /// </summary>
    type Expr with
        /// <summary>
        /// Creates an array-indexing expression using the supplied arguments
        /// </summary>
        static member ArrayAccess(arr : Expr, index : Expr) =
            let get = getArray.MakeGenericMethod([| arr.Type.GetElementType() |])
            Expr.Call(get, [arr;index])

        /// <summary>
        /// Creates a ForEach-Expression using the standard layout as used by F#-quotations
        /// </summary>
        static member ForEach(v : Var, seq : Expr, body : Expr) =
            let sType = typeof<System.Collections.Generic.IEnumerable<obj>>.GetGenericTypeDefinition().MakeGenericType([|v.Type|])
            let eType = typeof<System.Collections.Generic.IEnumerator<obj>>.GetGenericTypeDefinition().MakeGenericType([|v.Type|])
            let e = Var("enumerator", eType)

            let intrinsics = getArray.DeclaringType
            let unboxDisposable = intrinsics.GetMethod("UnboxGeneric").MakeGenericMethod([|typeof<IDisposable>|])

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

        /// <summary>
        /// Tries to evaluate an expression and returns its value when possible.
        /// Note that this implementation may be incomplete.
        /// </summary>
        static member tryEval (e : Expr) : Option<obj> =

            let reduce (a : Option<obj>[]) =
                match a |> Array.forall(fun o -> o.IsSome) with
                    | true -> a |> Array.map (fun o -> o.Value) |> Some
                    | false -> None

            match e with
                | Patterns.PropertyGet(t , p, args) ->
                    match t with
                        | Some(t) -> 
                            let args = args |> List.map Expr.tryEval |> List.toArray
                            match Expr.tryEval t, args |> reduce with
                                | Some t, Some args -> p.GetValue(t, args) |> Some
                                | _ -> None
                        | None -> 
                            let args = args |> List.map Expr.tryEval |> List.toArray
                            match args |> reduce with
                                | Some args -> p.GetValue(null, args) |> Some
                                | _ -> None

                | Patterns.FieldGet(t,f) ->
                    match t with
                        | Some(t) -> 
                            match Expr.tryEval t with
                                | Some t -> f.GetValue(t) |> Some
                                | None -> None
                        | None -> 
                            f.GetValue(null) |> Some
                | Patterns.Call(t, mi, args) ->
                    match t with
                        | Some(t) -> 
                            let args = args |> List.map Expr.tryEval |> List.toArray
                            let t = Expr.tryEval t
                            match t, args |> reduce with
                                | Some t, Some args -> mi.Invoke(t, args) |> Some
                                | _ -> None

                        | None -> 
                            let args = args |> List.map Expr.tryEval |> List.toArray
                            match args |> reduce with
                                | Some args -> mi.Invoke(null, args) |> Some
                                | _ -> None

                | Patterns.Let(var,value,body) ->
                    let value = Expr.tryEval value
                    match value with
                        | Some value ->
                            let body = body.Substitute(fun vi -> if vi = var then Expr.Value(value, vi.Type) |> Some else None)
                            Expr.tryEval body
                        | None -> None

                | Patterns.Value(v,_) ->
                    v |> Some
                | _ -> None

    [<AutoOpen>]
    module Patterns =
        open Microsoft.FSharp.Quotations
        open Microsoft.FSharp.Quotations.Patterns

        let (<|>) (a : bool) (b : bool) =
            a || b

        let (<&>) (a : bool) (b : bool) =
            a && b

        let rec extractOrAndAlso (e : Expr) =
            match e with
                | IfThenElse(a, Value(v, Bool), b) when v |> unbox<bool> = true ->
                    // a || b => IfThenElse (a, Value (true), b)))
                    let ca = extractOrAndAlso a
                    let cb = extractOrAndAlso b
                    <@@ %%ca <|> %%cb @@>
                | IfThenElse(a, b, Value(v, Bool)) when v |> unbox<bool> = false ->
                    // a && b => IfThenElse (a, b, Value (false))))
                    let ca = extractOrAndAlso a
                    let cb = extractOrAndAlso b
                    <@@ %%ca <&> %%cb @@>
                | _ -> e

        let (|WhileLoopFlat|_|) (e : Expr) =
            match e with
                | WhileLoop(c,b) ->
                    let c = extractOrAndAlso c
                    WhileLoopFlat(c, b) |> Some
                | _ ->
                    None

        let (|IfThenFlat|_|) (e : Expr) =
            match e with
                | IfThenElse(c,i,Value(_,t)) when t = typeof<unit> -> 
                    IfThenFlat(extractOrAndAlso c,i) |> Some
                | _ -> None

        let (|IfThenElseFlat|_|) (e : Expr) =
            match e with
                | IfThenElse(c,i,e) -> 
                    IfThenElseFlat(extractOrAndAlso c,i, e) |> Some
                | _ -> None
        let (|MemberFieldGet|_|) (e : Expr) =
            match e with
                | PropertyGet(Some t,p,[]) ->
                    MemberFieldGet(t, p :> MemberInfo) |> Some
                | FieldGet(Some t, f) ->
                    MemberFieldGet(t, f :> MemberInfo) |> Some
                | _ -> None

        let (|MemberFieldSet|_|) (e : Expr) =
            match e with
                | PropertySet(Some t,p,[] , v) ->
                    MemberFieldSet(t, p :> MemberInfo, v) |> Some
                | FieldSet(Some t, f, v) ->
                    MemberFieldSet(t, f :> MemberInfo, v) |> Some
                | _ -> None
               

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

        let(|Pipe|_|) (e : Expr) =
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
                        | None -> let mi = p.DeclaringType.GetMethod(p.Name, r |> Seq.map (fun ai -> ai.Type) |> Seq.toArray)
                                  if mi <> null then(Expr.Call(mi, r)) |> Some
                                  else failwith "function is not a method"
                        | Some t -> failwith "function is not a method"

                | Call(None, Method("op_PipeRight", _), [l;PropertyGet(t,p, [])]) ->
                    let l = deconstructTuples l
                    match t with
                        | None -> let mi = p.DeclaringType.GetMethod(p.Name, l |> Seq.map (fun ai -> ai.Type) |> Seq.toArray)
                                  if mi <> null then(Expr.Call(mi, l)) |> Some
                                  else failwith "function is not a method"
                        | Some t -> failwith "function is not a method"

                | Call(None, Method("op_PipeLeft", _), [f;r]) ->

                    let e = Expr.Application(f, r) |> inlineUselessAbstractions
                    Pipe(e) |> Some

                | Call(None, Method("op_PipeRight", _), [l;f]) ->
                    let e = Expr.Application(f, l) |> inlineUselessAbstractions
                    Pipe(e) |> Some


                | _ -> None


        let rec private deconstructLambda (args : list<Var>) (e : Expr) =
            match e with
                | Lambda(v,b) -> deconstructLambda (v::args) b 
                | _ -> (List.rev args,e)

        
        let rec private removeTupleArgs (args : list<Var>) (usedTuples : Set<Var>) (e : Expr) =
            match e with
                | Let(v, TupleGet(Var(t), i), b) -> removeTupleArgs (v::args) (Set.add t usedTuples) b
                | _ -> (List.rev args, usedTuples, e)

        let (|MethodLambda|_|) (e : Expr) =
            let (args, body) = deconstructLambda [] e
            let (tupleArgs, tuples, body) = removeTupleArgs [] Set.empty body

            let args = seq {
                        for a in args do
                            if not <| Set.contains a tuples then
                                yield a
                        yield! tupleArgs
                       }

            MethodLambda(args |> Seq.filter(fun a -> a.Type <> typeof<unit>) |> Seq.toList, body) |> Some

        let (|ForEach|_|) (e : Expr) =
            match e with
                | Let(e, Call(Some(Coerce(seq,_)), Method("GetEnumerator",_), []),
                        TryFinally(
                            WhileLoop(Call(Some (Var e1), Method("MoveNext",_), []),
                                Let(i, PropertyGet(Some (Var e2), current, []), b)
                            ),
                            IfThenElse(TypeTest(Coerce(Var e3, oType0), dType),
                                Call(Some (Call(None, Method("UnboxGeneric",_), [Coerce(e4, oType1)])), Method("Dispose",_), []),
                                Value(_)
                            )
                        )
                    ) when e1 = e && e2 = e && e3 = e && current.Name = "Current" && oType0 = typeof<obj> && oType1 = typeof<obj> && dType = typeof<System.IDisposable> ->
                    ForEach(i, seq, b) |> Some
                | _ -> None

        let rec private tryGetPrintFormat (e : Expr) =
            let rec deconstructApplications args e =
                match e with 
                    | Application(f, arg) -> 
                        let args,inner = deconstructApplications (arg::args) f
                        (args, inner)
                    | _ -> (args, e)

            let args,inner = deconstructApplications [] e

            let formatAndFunction = match inner with
                                      | Let(var, Call(None, mi, [Coerce(NewObject(t, [Value(format, st)]), _)]), rest) when mi.Name.Contains "PrintFormat" && st = typeof<string> -> 
                                          Some (mi, format |> unbox<string>)

                                      | Call(None, mi, [Coerce(NewObject(t, [Value(format, st)]), _)]) when mi.Name.Contains "PrintFormat" && st = typeof<string> -> 
                                          Some (mi, format |> unbox<string>)

                                      | _ -> None

            match formatAndFunction with
                | Some(mi,fmt) -> Some(mi, fmt, args)
                | None -> None


        let (|PrintFormatFunction|_|) (e : Expr) =
            match tryGetPrintFormat e with
                | Some(mi, fmt, args) -> PrintFormatFunction(mi, fmt, args) |> Some
                | _ -> None
            

        let rec private findSwitchCases (value : Expr) (label : Option<obj>) (e : Expr) =
            match label,e with
                | None, IfThenElse(Call(None, Method("op_Equality", [Enum; Enum]), [a;Value(c,Enum)]),ifTrue,ifFalse) when a = value -> 
                    
                    let l = findSwitchCases value (Some c) ifTrue
                    let r = findSwitchCases value None ifFalse
                    match l,r with
                        | Some l, Some r -> List.concat [l; r] |> Some
                        | _ -> None

                | Some l, e -> Some [(l,e)]
                | None, e -> Some [(null,e)]

        let (|Switch|_|) (e : Expr) =
            match e with
                | IfThenElse(Call(None, Method("op_Equality", [Enum; Enum]), [a;Value(c,Enum)]),_,_) -> 
                    match findSwitchCases a None e with
                        | Some(cases) -> Switch(a, cases) |> Some
                        | _ -> None
                | _ -> None


        let rec private findAlternatives (e : Expr) =
            match e with
                | IfThenElseFlat(c,i,e) ->
                    (Some c,i)::findAlternatives e
                | _ -> [None,e]

        let (|Alternatives|_|) (e : Expr) =
            match e with
                | IfThenElse(_, _, IfThenElse(_,_,_)) -> 
                    let alts = findAlternatives e

                    let e = alts |> List.filter(fun (c,_) -> c.IsNone) |> List.head |> snd
                    let c = alts |> List.choose(fun (c,b) -> match c with | Some c -> Some(c,b) | _ -> None)

                    Alternatives(c, e) |> Some

                | _ -> None

        let (|ExprOf|) (e : Expr) =
            ExprOf(e.Type)
