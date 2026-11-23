module ArrayLiterals

// Array literals in expression position (e.g. as record fields or function arguments) become global
// constants when all elements are constant; otherwise they are emitted as array constructors.

open Aardvark.Base
open Microsoft.FSharp.Quotations
open FShade
open FShade.GLSL
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
let ``Array of storage buffer reads in a record``() =
    Setup.Run()

    // offsets come from let-bound locals; the array must not become a global constant
    let frag (v : Vertex) =
        fragment {
            let o = uniform.Headers.[v.id] + 6
            let o1 = o + 4
            let nested =
                {
                    NA = { RA = V3f(uniform.Data.[o], uniform.Data.[o + 1], uniform.Data.[o + 2]); RB = uniform.Data.[o + 3] }
                    NB = Arr<N<2>, V2f>([| V2f(uniform.Data.[o1], uniform.Data.[o1 + 1]); V2f(uniform.Data.[o1 + 2], uniform.Data.[o1 + 3]) |])
                }
            return V4f(toV3f nested, 1.0f)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^const ", 0
        @"helper", 0
        @"vec2\[\]\( vec2\(Data\[o1\], Data\[\(o1 \+ 1\)\]\), vec2\(Data\[\(o1 \+ 2\)\], Data\[\(o1 \+ 3\)\]\) \)", 1
    ]

[<Test>]
let ``Array of input reads as function argument``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(sum2 (Arr<N<2>, float32>([| v.c.X; v.c.Y |])))
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^const ", 0
        @"helper", 0
        @"_sum2_" + helper + @"\(float\[\]\( fs_Colors\.x, fs_Colors\.y \)\)", 1
    ]

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

[<Test>]
let ``Array literal with utility function calls``() =
    Setup.Run()

    // calls of utility functions are not constant expressions in GLSL
    let frag (v : Vertex) =
        fragment {
            let a = Arr<N<2>, float32>([| 1.0f; 2.0f |])
            return V4f(sum2 (Arr<N<2>, float32>([| sum2 a; 3.0f |])))
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^const .*_sum2_", 0
        @"float\[\]\( " + helper + @"_sum2_" + helper + @"\(", 1
    ]

// an element expression with its own local, as produced by quotation rewriting (e.g. heap gathers)
let private gather (offset : int) : Expr<V2f> =
    <@
        let o = uniform.Headers.[0] + offset
        V2f(uniform.Data.[o], uniform.Data.[o + 1])
    @>

[<Test>]
let ``Array elements over two locals with the same name``() =
    Setup.Run()

    // both elements declare a local named o; hoisting makes them two variables with the same name
    let frag (v : Vertex) =
        fragment {
            let nested = { NA = { RA = V3f.Zero; RB = 1.0f }; NB = Arr<N<2>, V2f>([| %(gather 6); %(gather 8) |]) }
            return V4f(toV3f nested, 1.0f)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^const ", 0
        @"helper", 0
        @"vec2\(Data\[o\], Data\[\(o \+ 1\)\]\)", 1
        @"vec2\(Data\[o1\], Data\[\(o1 \+ 1\)\]\)", 1
    ]

[<Test>]
let ``Array constructor on GLSL 1.20``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(sum2 (Arr<N<2>, float32>([| v.c.X; v.c.Y |])))
        }

    // glslang validates for Vulkan here (GLSL >= 1.40), so only the generated text is checked:
    // array constructors are available since GLSL 1.20
    let glsl =
        Effect.ofFunction frag
        |> Effect.toModule { EffectConfig.empty with outputs = Map.ofList [ "Colors", (typeof<V4f>, 0) ] }
        |> ModuleCompiler.compileGLSL glsl120

    GLSL.shouldContainRegex glsl [ @"#version 120", Some 1; @"float\[\]\( ", Some 1 ]
