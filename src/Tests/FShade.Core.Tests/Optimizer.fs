module Optimizer

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open FsUnit
open Xunit
open Xunit.Sdk

open Aardvark.Base
open Aardvark.Base.Monads.State

open FShade


[<Fact>]
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

[<Fact>]
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



[<Fact>]
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

[<Fact>]
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

[<Fact>]
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



[<Fact>]
let ``[This] mutable this preseved``() =
    let v = V2d(1.0, 1.0)
    let input =
        <@
            let mutable a = v
            let b = a.Dot(a)
            keep a
        @>

    let expected =
        <@
            let mutable a = v
            ignore(a.Dot(a))
            keep a
        @>

    input |> Opt.run |> should exprEqual expected

[<Fact>]
let ``[This] immutable this removed``() =

    let v = V2d(1.0, 1.0)
    let input =
        <@
            let a = v
            let b = a.Dot(a)
            keep a
        @>

    let expected =
        <@
            keep v
        @>

    input |> Opt.run |> should exprEqual expected


[<Fact>]
let ``[Let] immutable binding inlined``() =
    let input =
        <@
            let a = 1
            keep a
        @>

    let expected =
        <@
            keep 1
        @>

    input |> Opt.run |> should exprEqual expected

[<Fact>]
let ``[Let] mutable binding preserved``() =
    let input =
        <@
            let mutable a = 1
            keep a
        @>

    let expected =
        <@
            let mutable a = 1
            keep a
        @>

    input |> Opt.run |> should exprEqual expected