module ArrayLiterals

// Array literals in expression position (e.g. as record fields or function arguments).

open Aardvark.Base
open Microsoft.FSharp.Quotations
open FShade
open FShade.Tests
open NUnit.Framework

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
        [<Semantic("Id")>] id : int
    }

type UniformScope with
    member x.Data : float32[] = x?StorageBuffer?Data
    member x.Headers : int[] = x?StorageBuffer?Headers

type Inner =
    {
        RA : V3f
        RB : float32
    }

type Nested =
    {
        NA : Inner
        NB : Arr<N<2>, V2f>
    }

[<ReflectedDefinition>]
let toV3f (x : Nested) =
    x.NA.RA * x.NA.RB + V3f(x.NB.[0].X, x.NB.[0].Y, x.NB.[1].X)

[<ReflectedDefinition>]
let sum2 (arr : Arr<N<2>, float32>) =
    arr.[0] + arr.[1]

let private helper = @"[A-Za-z0-9_]+"

[<Test>]
let ``Constant array literal becomes a global constant``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(sum2 (Arr<N<2>, float32>([| 1.0f; 2.0f |])) * v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^const float _constant[0-9]*\[2\] = float\[\]\(", 1
    ]
