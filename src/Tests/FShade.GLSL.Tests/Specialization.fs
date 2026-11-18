module Specialization

// Utility functions are specialized for constant arguments that resolve a condition in their body.
// Equal constants share one function (identified by the hash of the specialized body), constants
// that do not change the control flow do not create variants.

open Aardvark.Base
open FShade
open FShade.Tests
open NUnit.Framework

type Vertex =
    {
        [<Position>] pos : V4f
        [<Semantic("TexCoord")>] tc : V2f
        [<Color>] c : V4f
    }

type UniformScope with
    member x.Threshold : float32 = x?Threshold
    member x.ShadowsEnabled : bool = x?ShadowsEnabled

let tex =
    sampler2d {
        texture uniform?DiffuseTexture
        filter Filter.MinMagLinear
        addressU WrapMode.Wrap
        addressV WrapMode.Wrap
    }

type Mode =
    | Add = 0
    | Mul = 1

type Shape =
    | Circle of radius : float32
    | Square of size : float32

[<ReflectedDefinition>]
let shade (useTex : bool) (uv : V2f) (c : V4f) =
    if useTex then tex.Sample(uv) * c
    else c

[<ReflectedDefinition>]
let shadeOuter (useTex : bool) (uv : V2f) (c : V4f) =
    shade useTex uv c * 0.5f

[<ReflectedDefinition>]
let scaleBranch (n : int) (x : float32) =
    (if n > 4 then x * 2.0f else x) * float32 n

[<ReflectedDefinition>]
let scale (f : float32) (x : float32) =
    x * f

[<ReflectedDefinition>]
let compareUniform (f : float32) (x : float32) =
    if f > uniform.Threshold then x else -x

[<ReflectedDefinition>]
let shadowed (useShadows : bool) (x : float32) =
    if useShadows && uniform.ShadowsEnabled then x * 0.5f else x

[<ReflectedDefinition>]
let apply (mode : Mode) (a : float32) (b : float32) =
    match mode with
    | Mode.Add -> a + b
    | _ -> a * b

[<ReflectedDefinition>]
let offset (o : V2f) (p : V2f) =
    if o.X > 0.0f then p + o else p - o

[<ReflectedDefinition>]
let area (s : Shape) =
    match s with
    | Circle r -> 3.14159f * r * r
    | Square a -> a * a

[<ReflectedDefinition>]
let keepRuntime ([<Specialize(SpecializationMode.Off)>] n : int) (x : float32) =
    if n > 4 then x * 2.0f else x

[<ReflectedDefinition; Specialize(SpecializationMode.Off)>]
let keepRuntimeFunction (n : int) (x : float32) =
    if n > 4 then x * 2.0f else x

[<ReflectedDefinition; Specialize(SpecializationMode.Off)>]
let overrideOff ([<Specialize(SpecializationMode.ControlFlow)>] n : int) (m : int) (x : float32) =
    (if n > 4 then x * 2.0f else x) + (if m > 4 then x * 3.0f else x)

[<ReflectedDefinition>]
let scaleSpecialized ([<Specialize>] f : float32) (x : float32) =
    x * f

[<ReflectedDefinition>]
let viaLet (n : int) (x : float32) =
    let big = n > 4
    if big then x * 2.0f else x

[<ReflectedDefinition>]
let scaleBranchPlusOne (n : int) (x : float32) =
    scaleBranch (n + 1) x

[<ReflectedDefinition>]
let sumTo (n : int) (x : float32) =
    let mutable s = 0.0f
    for i in 0 .. n - 1 do
        s <- s + x
    s

[<ReflectedDefinition>]
let incIf (flag : bool) (x : int ref) =
    if flag then x.Value <- x.Value + 1

let private helper = @"[A-Za-z0-9_]+"

/// regex for the definition line of the function with the given name and parameter list
let private def (ret : string) (name : string) (pars : string) =
    @"^" + ret + " " + helper + "_" + name + "_" + helper + @"\(" + pars + @"\)"

[<Test>]
let ``Constant argument resolves the branch``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return shade true v.tc v.c
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "vec4" "shade" "vec2 uv, vec4 c", 1
        @"useTex", 0
        @"texture\(", 1
    ]

[<Test>]
let ``Different constants give different functions``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return shade true v.tc v.c + shade false v.tc v.c
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "vec4" "shade" "vec2 uv, vec4 c", 1 // the true variant
        def "vec4" "shade" "vec4 c", 1 // the false variant does not use uv either
        @"useTex", 0
        @"texture\(", 1
    ]

[<Test>]
let ``Equal constants share one function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return shade true v.tc v.c + shade true (v.tc * 2.0f) v.c
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "vec4" "shade" "vec2 uv, vec4 c", 1
        @"_shade_", 3 // one definition, two calls
    ]

[<Test>]
let ``Non-constant argument keeps the generic function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return shade (v.c.X > 0.5f) v.tc v.c
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "vec4" "shade" "bool useTex, vec2 uv, vec4 c", 1
        @"useTex", 2 // parameter and condition
    ]

[<Test>]
let ``Constant forwarded to a nested function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return shadeOuter true v.tc v.c
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "vec4" "shadeOuter" "vec2 uv, vec4 c", 1
        def "vec4" "shade" "vec2 uv, vec4 c", 1
        @"useTex", 0
    ]

[<Test>]
let ``Int argument used in branch and arithmetic``() =
    Setup.Run()

    // 1, 2 and 3 resolve the condition the same way and share one function keeping the parameter
    let frag (v : Vertex) =
        fragment {
            return V4f(scaleBranch 1 v.c.X + scaleBranch 2 v.c.X + scaleBranch 3 v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "scaleBranch" "int n, float x", 1
        @"\(n > 4\)", 0
    ]

[<Test>]
let ``Int argument with different branch outcomes``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(scaleBranch 1 v.c.X + scaleBranch 5 v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "scaleBranch" "int n, float x", 2
        @"\(n > 4\)", 0
    ]

[<Test>]
let ``Constant not used in a condition is not specialized``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(scale 2.0f v.c.X + scale 3.0f v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "scale" "float f, float x", 1
    ]

[<Test>]
let ``Condition depending on a uniform is not specialized``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(compareUniform 1.0f v.c.X + compareUniform 2.0f v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "compareUniform" "float f, float x", 1
        @"Threshold", 2 // declaration and use
    ]

[<Test>]
let ``Short-circuit condition with a uniform``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(shadowed true v.c.X + shadowed false v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "shadowed" "float x", 2
        @"useShadows", 0
        @"ShadowsEnabled", 2 // declaration and the remaining condition
    ]

[<Test>]
let ``Enum argument``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(apply Mode.Add v.c.X v.c.Y)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "apply" "float a, float b", 1
        @"mode", 0
    ]

[<Test>]
let ``Struct argument``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(offset (V2f(1.0f, 0.0f)) v.tc, 0.0f, 1.0f)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "vec2" "offset" "vec2 o, vec2 p", 1
        @"o\.x", 0 // the condition is resolved, o is only used as a whole
    ]

[<Test>]
let ``Union argument``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(area (Circle 2.0f))
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"_area_", 2 // one definition, one call
        @"\bs\.tag", 0 // the tag test is resolved
    ]

[<Test>]
let ``Specialize Off on a parameter``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(keepRuntime 1 v.c.X + keepRuntime 5 v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "keepRuntime" "int n, float x", 1
        @"\(n > 4\)", 1
    ]

[<Test>]
let ``Specialize Off on a function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(keepRuntimeFunction 1 v.c.X + keepRuntimeFunction 5 v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "keepRuntimeFunction" "int n, float x", 1
        @"\(n > 4\)", 1
    ]

[<Test>]
let ``Parameter attribute overrides the function attribute``() =
    Setup.Run()

    // n is specialized (parameter attribute), m is not (function attribute)
    let frag (v : Vertex) =
        fragment {
            return V4f(overrideOff 1 1 v.c.X + overrideOff 5 5 v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "overrideOff" "int m, float x", 2
        @"\(n > 4\)", 0
        @"\(m > 4\)", 2
    ]

[<Test>]
let ``Specialize on a parameter folds all uses``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(scaleSpecialized 2.0f v.c.X + scaleSpecialized 3.0f v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "scaleSpecialized" "float x", 2
        @"\(x \* 2\.0\)", 1
        @"\(x \* 3\.0\)", 1
    ]

[<Test>]
let ``Condition through a let binding``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(viaLet 1 v.c.X + viaLet 5 v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "viaLet" "float x", 2
        @"big", 0
    ]

[<Test>]
let ``Nested call with computed constant argument``() =
    Setup.Run()

    // scaleBranch receives n + 1 = 5, which resolves its condition
    let frag (v : Vertex) =
        fragment {
            return V4f(scaleBranchPlusOne 4 v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "scaleBranchPlusOne" "int n, float x", 1
        def "float" "scaleBranch" "int n, float x", 1
        @"\(n > 4\)", 0
        @"\(x \* 2\.0\)", 1
    ]

[<Test>]
let ``Loop bound is not specialized``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(sumTo 4 v.c.X + sumTo 8 v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "float" "sumTo" "int n, float x", 1
    ]

[<Test>]
let ``Closure argument of the shader function``() =
    Setup.Run()

    // the typical use: an effect parameterized by a flag
    let frag (useTex : bool) (v : Vertex) =
        fragment {
            return shade useTex v.tc v.c
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction (frag true) ] [
        def "vec4" "shade" "vec2 uv, vec4 c", 1
        @"useTex", 0
        @"texture\(", 1
    ]

[<Test>]
let ``Specialized function with ref parameter``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let counter = 0
            incIf true &&counter
            incIf false &&counter
            return V4f(float32 counter)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        def "void" "incIf" "inout int x", 1 // the false variant has no effect and is removed
        @"flag", 0
        @"x = \(x \+ 1\);", 1
    ]
