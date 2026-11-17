module InlineRefParameters

// Inline utility functions with ref parameters must operate on the caller's variable, not on a copy.
// See: https://github.com/krauthaufen/FShade/issues/38

open Aardvark.Base
open FShade
open FShade.Tests
open NUnit.Framework

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
    }

type UniformScope with
    member x.RefData : int[] = x?StorageBuffer?RefData

[<ReflectedDefinition; Inline>]
let incInline (x : int ref) =
    x.Value <- x.Value + 1

[<ReflectedDefinition; Inline>]
let incAndGetInline (x : int ref) =
    x.Value <- x.Value + 1
    x.Value

[<ReflectedDefinition>]
let incTwiceViaInline (x : int ref) =
    incInline x
    incInline x

[<ReflectedDefinition; Inline>]
let incViaAlias (x : int ref) =
    let r = x
    r.Value <- r.Value + 1

let private helper = @"[A-Za-z0-9_]+"

[<Test>]
let ``Inline unit function modifies the caller's variable``() =
    Setup.Run()

    let frag () =
        fragment {
            let variable = 0
            incInline &&variable
            incInline &&variable
            return V4f(float32 variable)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"_incInline_", 0
        @"variable = \(variable \+ 1\);", 2
        @"ColorsOut = vec4\(float\(variable\)\);", 1
    ]

[<Test>]
let ``Inline function returning the modified value``() =
    Setup.Run()

    let frag () =
        fragment {
            let variable = 0
            let a = incAndGetInline &&variable
            let b = incAndGetInline &&variable
            return V4f(float32 a, float32 b, float32 variable, 1.0f)
        }

    // a = 1, b = 2, variable = 2: the reads must stay in place
    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"_incAndGetInline_", 0
        @"variable = \(variable \+ 1\);\s*int a = variable;\s*variable = \(variable \+ 1\);\s*int b = variable;", 1
    ]

[<Test>]
let ``Ref parameter forwarded by a non-inline function``() =
    Setup.Run()

    let frag () =
        fragment {
            let variable = 0
            incTwiceViaInline &&variable
            return V4f(float32 variable)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"_incInline_", 0
        @"void " + helper + @"_incTwiceViaInline_" + helper + @"\(inout int x\)\s*{\s*x = \(x \+ 1\);\s*x = \(x \+ 1\);\s*}", 1
    ]

[<Test>]
let ``Ref to array element``() =
    Setup.Run()

    let frag () =
        fragment {
            let arr = Arr<N<4>, int>()
            incInline &&arr.[2]
            return V4f(float32 arr.[2])
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"_incInline_", 0
        @"arr\[2\] = \(arr\[2\] \+ 1\);", 1
    ]

[<Test>]
let ``Ref to storage buffer element``() =
    Setup.Run()

    let frag () =
        fragment {
            incInline &&uniform.RefData.[0]
            return V4f(float32 uniform.RefData.[0])
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"_incInline_", 0
        @"RefData\[0\] = \(RefData\[0\] \+ 1\);", 1
        @"^buffer [rR]efDataBuffer", 1
    ]

[<Test>]
let ``Non-trivial index is evaluated once``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let arr = Arr<N<4>, int>()
            incInline &&arr.[int v.c.X]
            return V4f(float32 arr.[0])
        }

    // the index expression is hoisted into a temporary instead of being duplicated for read and write
    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"_incInline_", 0
        @"int\(fs_Colors\.x\)", 1
        @"arr\[" + helper + @"\] = \(arr\[" + helper + @"\] \+ 1\);", 1
    ]

[<Test>]
let ``Ref alias inside inline function is not copied``() =
    Setup.Run()

    let frag () =
        fragment {
            let variable = 0
            incViaAlias &&variable
            return V4f(float32 variable)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"_incViaAlias_", 0
        @"int r", 0
        @"variable = \(variable \+ 1\);", 1
    ]

[<Test>]
let ``Read before inlined write is not moved past it``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let variable = int v.c.X
            let before = variable
            incInline &&variable
            return V4f(float32 before, float32 variable, 0.0f, 1.0f)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"_incInline_", 0
        @"int before = variable;\s*variable = \(variable \+ 1\);", 1
    ]
