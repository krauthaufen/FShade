namespace FShade

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape

open Aardvark.Base
open System.Text.RegularExpressions

open FShade
open FShade.Imperative
open NUnit.Framework.Constraints

#nowarn "4321"

[<AutoOpen>]
module Utilities = 
    do Serializer.Init()

    [<KeepCall>]
    let keep a = ()

    [<KeepCall>]
    let produce<'a>() : 'a = onlyInShaderCode "produce"

    let rec private normalize (e : Expr) =
        match e with
        | CallFunction(utility, args) ->
            let args = args |> List.map normalize

            match utility.functionMethodInfo with
            | Some mi ->
                if mi.IsStatic then
                    if args.Length = mi.GetParameters().Length then
                        Expr.Call(mi, args)
                    else
                        e
                else
                    match args with
                    | t :: args ->
                        if args.Length = mi.GetParameters().Length then
                            Expr.Call(t, mi, args)
                        else
                            e
                    | _ ->
                        e
            | _ ->
                e

        | Uniform u ->
            Expr.ReadInput(ParameterKind.Uniform, e.Type, u.uniformName)

        | LetCopyOfStruct e ->
            normalize e

        | Sequential(Sequential(a, b), c) ->
            normalize (Expr.Sequential(a, Expr.Sequential(b, c)))

        | ShapeCombination(o, args) ->
            RebuildShapeCombination(o, args |> List.map normalize)

        | ShapeVar _ ->
            e

        | ShapeLambda(v, b) ->
            Expr.Lambda(v, normalize b)

    let private exprComparer l =
        { new Constraint() with
            override x.ApplyTo<'B>(other : 'B) =
                match other :> obj with
                | :? Expr as r ->
                    let l = normalize l
                    let r = normalize r

                    if Expr.computeHash l = Expr.computeHash r then
                        ConstraintResult(x, r, true)
                    else
                        ConstraintResult(x, r, false)
                | _ ->
                    ConstraintResult(x, other, false)
        }

    let exprEqual (r : Expr) =
        exprComparer r

    let hasCall (nameRx : string) (e : Expr) =
        let rec get (e : Expr) =
            match e with
            | Call(this, mi, args) -> mi.Name :: List.collect get (Option.toList this @ args)
            | CallFunction(f, args) -> f.functionName :: List.collect get (f.functionBody :: args)
            | ShapeVar _ -> []
            | ShapeLambda(_, b) -> b |> get
            | ShapeCombination(_, args) -> args |> List.collect get

        e |> get |> List.exists (fun str -> Regex.IsMatch(str, nameRx))

    module Opt =
        open System.Reflection

        let isSideEffect (mi : MethodInfo) =
            mi.GetCustomAttributes<KeepCallAttribute>()
            |> Seq.isEmpty
            |> not

        let run (expression : Expr) =
            Serializer.Init()

            expression
                |> Preprocessor.preprocess V3i.Zero
                |> fst
                |> Optimizer.inlining isSideEffect
                |> Optimizer.evaluateConstants' isSideEffect
                |> Optimizer.inlining isSideEffect
                |> Optimizer.eliminateDeadCode' isSideEffect
                |> Optimizer.hoistImperativeConstructs