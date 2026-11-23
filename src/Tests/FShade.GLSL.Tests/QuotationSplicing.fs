module QuotationSplicing

// Shader quotations may splice sub-quotations (%q). When the spliced quotation is computed, e.g. the result
// of a function call or an array element, F# keeps the splice lazy; it must be resolved during preprocessing
// so uniform accesses inside the spliced quotation are recognized.

open Aardvark.Base
open Microsoft.FSharp.Quotations
open FShade
open FShade.Tests
open NUnit.Framework

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
    }

type UniformScope with
    member x.Scale : float32 = x?Scale

let private read (i : int) : Expr<float32> =
    <@ (uniform?StorageBuffer?Spliced : float32[]).[i] @>

let private sumOf (count : int) : Expr<float32> =
    [ 1 .. count - 1 ] |> List.fold (fun acc i -> <@ %acc + %(read i) @>) (read 0)

[<Test>]
let ``Spliced quotation value``() =
    Setup.Run()

    let q = <@ uniform.Scale * 2.0f @>

    let frag (v : Vertex) =
        fragment {
            return v.c * %q
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"\(Scale \* 2\.0\)", 1
    ]

[<Test>]
let ``Spliced quotation computed by a function``() =
    Setup.Run()

    // %(read i) is an application of the read function
    let frag (v : Vertex) =
        fragment {
            return v.c * (%(read 0) + %(read 3))
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"Spliced\[0\]", 1
        @"Spliced\[3\]", 1
        @"null", 0
    ]

[<Test>]
let ``Spliced quotation built by folding``() =
    Setup.Run()

    let sum = sumOf 8

    let frag (v : Vertex) =
        fragment {
            return v.c * %sum
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"Spliced\[[0-9]\]", 8
        @"null", 0
    ]

[<Test>]
let ``Spliced quotation from an array``() =
    Setup.Run()

    let reads = [| read 1; read 2 |]

    let frag (v : Vertex) =
        fragment {
            return v.c * (%(reads.[0]) + %(reads.[1]))
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"Spliced\[1\]", 1
        @"Spliced\[2\]", 1
        @"null", 0
    ]

[<Test>]
let ``Spliced quotation in a vertex shader generating a helper``() =
    Setup.Run()

    let sum = sumOf 4

    // the let inside the branch of a value conditional becomes a compiler-generated helper
    let vert (v : Vertex) =
        vertex {
            return { v with pos = v.pos * ((if v.pos.X > 0.5f then (let a = %sum in a * a + a) else 0.0f) + 1.0f) }
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction vert ] [
        @"Spliced\[[0-9]\]", 4
        @"null", 0
    ]
