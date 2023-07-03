module Optimizer

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open FsUnit

open Aardvark.Base
open Aardvark.Base.Monads.State
open FsUnit
open FShade
open NUnit.Framework


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
            keep a
        @>

    input |> Opt.run |> should exprEqual expected

[<Test>]
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

[<Test>]
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

[<Test>]
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

[<Test>]
let ``[Hoist] lifting bindings``() =
    let input =
        <@
            let a =
                let mutable b = 10
                b
            keep a
        @>

    let expected =
        <@
            let mutable b = 10
            let a = b
            keep a
        @>
    input |> Opt.run |> should exprEqual expected
    
[<Test>]
let ``[Hoist] preserving order``() =
    let input =
        <@
            let mutable b = 10.0
            let a =
                b <- b + 1.0
                b <- b + 2.0
                if b > 10.0 then keep b
                b * b
            keep a
            keep b
        @>

    let expected =
        <@
            let mutable b = 10.0
            b <- b + 1.0
            b <- b + 2.0
            if b > 10.0 then keep b
            let a =
                b * b
            keep a
            keep b
        @>
    let res = input |> Opt.run 
    res |> should exprEqual expected

[<Test>]
let ``[Hoist] nested lets``() =
    let input =
        <@
            let a =
                let b = 
                    let c = 
                        let d = produce<float>()
                        d * d
                    c * c
                b * b

            keep (a * a)
        @>

    let expected =
        <@
            let d = produce<float>()
            let c = d * d
            let b = c * c
            let a = b * b
            keep (a * a)
        @>
    let res = input |> Opt.run 
    res |> should exprEqual expected

[<Test>]
let ``[Hoist] lifting for loops``() =
    let input =
        <@
            let a =
                let mutable b = 10
                for i in 0 .. 10 do
                    b <- b + 1
                b
            keep a
        @>

    let expected =
        <@
            let mutable b = 10
            for i in 0 .. 10 do
                b <- b + 1
            let a = b
            keep a
        @>
    input |> Opt.run |> should exprEqual expected

[<ReflectedDefinition; Inline>]
let util (a : int) (b : int) =
    let mutable c = a + 2*b
    for i in 0 .. 10 do
        c <- c / 2
    c

[<Test>]
let ``[Hoist] inline function``() =
    let input =
        <@
            fun x y ->
                let a = util x y
                keep a
        @>

    let expected =
        <@
            fun x y ->
                let mutable c = x + 2*y
                for i in 0 .. 10 do
                    c <- c / 2
                let a = c
                keep a
        @>
    input |> Opt.run |> should exprEqual expected


[<Test>]
let ``[Hoist] if in expression``() =
    let input =
        <@
            fun x y ->
                let mutable c = x + 2*y
                let a = 
                    if x < 10 then
                        c <- 5
                    c
                keep a
        @>

    let expected =
        <@
            fun x y ->
                let mutable c = x + 2*y
                if x < 10 then
                    c <- 5
                let a = c
                keep a
        @>
    input |> Opt.run |> should exprEqual expected

let test (e : Expr<int -> int>) (v : Expr<int>) =
    <@
        (%e) %v
    @>

[<Test>]
let ``[Quote] inlining possible``() =
    let input =
        <@
            fun x ->
                %(test <@ (fun a -> a + 1) @> <@ x @>)
        @>

    let expected =
        <@
            fun x ->
                x + 1
        @>
    input |> Opt.run |> should exprEqual expected


[<Test>]
let ``[Ref] no constant folding``() =
    let input =
        <@
            fun x ->
                let a = ref 1
                !a
        @>

    let expected =
        <@
            fun x ->
                let a = ref 1
                !a
        @>
    input |> Opt.run |> should exprEqual expected



[<Test>]
let ``[Dead] removing unused array writes``() =
    let input =
        <@
            fun (a : Arr<N<16>, int>) ->
                let a : Arr<N<16>, int> = Unchecked.defaultof<_>
                a.[0] <- 10
        @>

    let expected =
        <@
            fun (a : Arr<N<16>, int>) ->
                ()
        @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Dead] keeping used array writes``() =
    let input =
        <@
            fun (a : Arr<N<16>, int>) ->
                a.[0] <- 10
                keep a
        @>

    input |> Opt.run |> should exprEqual input
    

[<Inline; ReflectedDefinition>]
let funny (a : int) =
    if a < 10 then
        (a, a * 10)
    else
        (5,5)

[<Test>]
let ``Sepp``() =
    let input =
        <@
            fun (a : int) ->
                let (x,y) = funny a
                x + y
        @>
    
    input |> Opt.run |> printfn "%A"
    

[<System.Flags>]
type MyEnum =
    | A  = 1us
    | B  = 2us
    | C  = 4us

module MyEnum =

    [<Literal>]
    let AB = MyEnum.A ||| MyEnum.B

[<Test>]
let ``[Constant] enum bitwise or``() =
    let input    = <@ keep (MyEnum.A ||| MyEnum.B) @>
    let expected = <@ keep MyEnum.AB @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum bitwise and``() =
    let input    = <@ keep ((MyEnum.A ||| MyEnum.B) &&& MyEnum.A) @>
    let expected = <@ keep MyEnum.A @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum bitwise xor``() =
    let input    = <@ keep ((MyEnum.A ||| MyEnum.B) ^^^ MyEnum.A) @>
    let expected = <@ keep MyEnum.B @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum shift right``() =
    let input    = <@ keep (MyEnum.B >>> 1) @>
    let expected = <@ keep MyEnum.A @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum shift left``() =
    let input    = <@ keep (MyEnum.A <<< 2) @>
    let expected = <@ keep MyEnum.C @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum to int8``() =
    let input    = <@ keep (int8 MyEnum.C) @>
    let expected = <@ keep 4y @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum to uint8``() =
    let input    = <@ keep (uint8 MyEnum.C) @>
    let expected = <@ keep 4uy @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum to int16``() =
    let input    = <@ keep (int16 MyEnum.C) @>
    let expected = <@ keep 4s @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum to uint16``() =
    let input    = <@ keep (uint16 MyEnum.C) @>
    let expected = <@ keep 4us @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum to int32``() =
    let input    = <@ keep (int32 MyEnum.C) @>
    let expected = <@ keep 4 @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum to uint32``() =
    let input    = <@ keep (uint32 MyEnum.C) @>
    let expected = <@ keep 4u @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum to int64``() =
    let input    = <@ keep (int64 MyEnum.C) @>
    let expected = <@ keep 4L @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum to uint64``() =
    let input    = <@ keep (uint64 MyEnum.C) @>
    let expected = <@ keep 4UL @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum to float``() =
    let input    = <@ keep (float MyEnum.C) @>
    let expected = <@ keep 4.0 @>
    input |> Opt.run |> should exprEqual expected

[<Test>]
let ``[Constant] enum to float32``() =
    let input    = <@ keep (float32 MyEnum.C) @>
    let expected = <@ keep 4.0f @>
    input |> Opt.run |> should exprEqual expected