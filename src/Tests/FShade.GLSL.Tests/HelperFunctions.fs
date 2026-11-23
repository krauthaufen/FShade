module HelperFunctions

// Expressions that cannot be translated to a C expression (e.g. a let inside the branch of a value
// conditional) are compiled to helper functions. Identical helpers must be emitted only once.

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
    member x.Data : float32[] = x?StorageBuffer?Data
    member x.Headers : int[] = x?StorageBuffer?Headers

// a let with several uses inside a value conditional; inlined at the call sites
[<ReflectedDefinition; Inline>]
let branchy (cond : bool) (x : float32) =
    if cond then
        let a = x * x
        a * a + a
    else
        0.0f

[<Test>]
let ``Identical helpers reading an input are shared``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(branchy (v.c.X > 0.5f) v.c.Y + branchy (v.c.X > 0.25f) v.c.Y)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^float helper[0-9]*\(", 1 // one definition
        @"helper[0-9]*\(", 3 // definition and two calls
    ]

[<Test>]
let ``Identical helpers over a local variable are shared``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let y = v.c.Y * 2.0f
            return V4f(branchy (v.c.X > 0.5f) y + branchy (v.c.X > 0.25f) y)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^float helper[0-9]*\(float y\)", 1
        @"helper[0-9]*\(", 3
    ]

[<Test>]
let ``Identical helpers reading a uniform are shared``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(branchy (v.c.X > 0.5f) uniform.Scale + branchy (v.c.X > 0.25f) uniform.Scale)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^float helper[0-9]*\(", 1
        @"helper[0-9]*\(", 3
    ]

[<Test>]
let ``Different helpers are not merged``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(branchy (v.c.X > 0.5f) v.c.Y + branchy (v.c.X > 0.25f) v.c.Z)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^float helper[0-9]*\(", 2
        @"helper[0-9]*\(", 4
    ]

// two locals with the same name (as produced by quotation rewriting), used inside the given expression;
// their values are storage buffer reads, which are never inlined
let private withSameNamedLocals (body : Expr<int> -> Expr<int> -> Expr<float32>) : Expr<float32> =
    let o1 = Var("o", typeof<int>)
    let o2 = Var("o", typeof<int>)
    Expr.Cast<float32>(
        Expr.Let(o1, <@ uniform.Headers.[0] + 6 @>,
            Expr.Let(o2, <@ uniform.Headers.[0] + 8 @>,
                body (Expr.Cast<int>(Expr.Var o1)) (Expr.Cast<int>(Expr.Var o2))))
    )

[<Test>]
let ``Free variables with the same name become distinct parameters``() =
    Setup.Run()

    let value =
        withSameNamedLocals (fun a b ->
            <@ if uniform.Scale > 0.0f then (let s = uniform.Data.[%a] + uniform.Data.[%b] in s * s + s) else 0.0f @>
        )

    let frag (v : Vertex) =
        fragment {
            return v.c * (%value + 1.0f)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^float helper[0-9]*\(int o, int o1\)", 1
        @"Data\[o\]", 1
        @"Data\[o1\]", 1
    ]
