module Optimizer

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open FsUnit
open NUnit.Framework
open NUnit.Framework.Constraints

open Aardvark.Base
open Aardvark.Base.Monads.State

open FShade

let keep a = ()

let exprComparer = 
    { new System.Collections.Generic.IEqualityComparer<Expr> with
        member x.GetHashCode(l : Expr) =
            0

        member x.Equals(l : Expr, r : Expr) =
            l.ToString() = r.ToString()
    }

let exprEqual (r : Expr) = EqualConstraint(r).Using exprComparer

module Opt =
    open System.Reflection

    let private keepMeth = getMethodInfo <@ keep @>
    let isSideEffect (mi : MethodInfo) =
        mi.IsGenericMethod && mi.GetGenericMethodDefinition() = keepMeth

    let run (expression : Expr) =
        expression
            |> Optimizer.evaluateConstants' isSideEffect
            |> Optimizer.eliminateDeadCode' isSideEffect

[<Test>]
let ``[For] long dependency chain for used variable``() =
    let input =
        <@
            // long dependency chain test
            // needs 4 iterations in fixpoint search when a is used
            // a <- b <- c <- d <- a ...
            let mutable a = 0
            let mutable b = 0
            let mutable c = 0
            let mutable d = 0
            for i in 0 .. 10 do
                a <- a + b
                b <- b + c
                c <- c + d
                d <- d + a
            keep a
        @>

    let expected =
        input

    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[For] parallel increment``() =
    let input =
        <@
            let mutable a = 0
            let mutable b = 0
            for i in 0 .. 10 do
                a <- a + 1
                b <- b + 1
            keep a
        @>

    let expected =
        <@
            let mutable a = 0
            for i in 0 .. 10 do
                a <- a + 1
            keep a
        @>

    input |> Opt.run |> should exprEqual expected



[<Test>]
let ``[While] changing unused value``() =
    let input =
        <@ 
            let mutable a = 0
            while a < 10 do
                a <- a + 1
        @>

    let expected =
        <@ () @>

    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[While] counting and changing value``() =
    let input =
        <@ 
            let mutable res = 1
            let mutable a = 0
            while a < 10 do
                a <- a + 1
                res <- res + res
            keep res
        @>

    let expected =
        input

    input |> Opt.run  |> should exprEqual expected

[<Test>]
let ``[While] counting and changing used/unused values``() =
    let input =
        <@ 
            let mutable res = 1
            let mutable a = 0
            let mutable b = 0
            while (b <- b + 1; a < 10) do
                a <- a + 1
                res <- res + res
            keep res
        @>

    let expected =
        <@ 
            let mutable res = 1
            let mutable a = 0
            while a < 10 do
                a <- a + 1
                res <- res + res
            keep res
        @>

    input |> Opt.run  |> should exprEqual expected



[<Test>]
let ``[If] positive constant folding``() =
    let input =
        <@
            let mutable a = 1000
            if a = 0 then
                // a is known to be 0 here
                keep a
                if a > 0 then 
                    true
                else
                    false
            else
                false
        @>

    let expected =
        <@
            let mutable a = 1000
            if a = 0 then
                keep 0
                false
            else
                false
        @>

    input |> Opt.run  |> should exprEqual expected

[<Test>]
let ``[If] negative constant folding``() =
    let input =
        <@
            let mutable a = 1000
            if a <> 0 then
                true
            else
                // a is known to be 0 here
                keep a
                if a > 0 then 
                    true
                else
                    false
        @>

    let expected =
        <@
            let mutable a = 1000
            if a <> 0 then
                true
            else
                keep 0
                false
        @>

    input |> Opt.run  |> should exprEqual expected