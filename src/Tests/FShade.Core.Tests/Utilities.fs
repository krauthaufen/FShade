namespace FShade

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open NUnit.Framework
open FsUnit

open Aardvark.Base
open Aardvark.Base.Monads.State

open FShade
open NUnit.Framework.Constraints

#nowarn "4321"

[<AutoOpen>]
module Utilities = 
    do Serializer.Init()

    let keep a = ()

    let produce<'a>() : 'a = onlyInShaderCode "produce"


    let rec normalize (e : Expr) =
        match e with
            | Sequential(Sequential(a,b), c) ->
                normalize (Expr.Sequential(a, Expr.Sequential(b,c)))
            | ShapeCombination(o, args) ->
                RebuildShapeCombination(o, args |> List.map normalize)
            | ShapeVar v ->
                e
            | ShapeLambda(v,b) ->
                Expr.Lambda(v, normalize b)

    let exprComparer l = 
        { new Constraint() with
            override x.ApplyTo<'B>(other : 'B) =    
                match other :> obj with
                | :? Expr as r -> 
                    if Expr.computeHash (normalize l) = Expr.computeHash (normalize r) then
                        ConstraintResult(x, other, true)
                    else
                        ConstraintResult(x, other, false)
                | _ ->
                    ConstraintResult(x, other, false)
        }
        //{ new NHamcrest.Core.IsEqualMatcher<obj>(l) with
        
        //    override x.Matches(r : obj) =
        //        match r with
        //            | :? Expr as r ->
        //                Expr.ComputeHash (normalize l) = Expr.ComputeHash (normalize r)
        //            | _ ->
        //                false
        //        //l.ToString() = r.ToString()
        //}

    let exprEqual (r : Expr) = 
        exprComparer r

    module Opt =
        open System.Reflection

        let private keepMeth = getMethodInfo <@ keep @>
        let private produceMeth = getMethodInfo <@ produce @>

        let isSideEffect (mi : MethodInfo) =
            mi.IsGenericMethod && (mi.GetGenericMethodDefinition() = keepMeth || mi.GetGenericMethodDefinition() = produceMeth)

        let run (expression : Expr) =
            expression
                |> Preprocessor.preprocess V3i.Zero
                |> fst
                |> Optimizer.inlining isSideEffect
                |> Optimizer.evaluateConstants' isSideEffect
                |> Optimizer.inlining isSideEffect
                |> Optimizer.eliminateDeadCode' isSideEffect
                |> Optimizer.hoistImperativeConstructs