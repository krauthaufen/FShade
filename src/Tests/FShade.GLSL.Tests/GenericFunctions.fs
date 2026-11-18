module GenericFunctions

// Inline functions with SRTP constraints are compiled from the reflected definition of their
// witness-passing variant: the witnesses of the call site are substituted, so each instantiation
// becomes an ordinary utility function (named after the type arguments) using the resolved operators.

open Aardvark.Base
open FShade
open FShade.Tests
open NUnit.Framework

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
        [<Semantic("Normal")>] n : V3f
        [<Semantic("Id")>] id : int
    }

[<ReflectedDefinition>]
let inline genericAdd x y =
    x + y

[<ReflectedDefinition>]
let inline lerpGeneric a b t =
    a + (b - a) * t

[<ReflectedDefinition>]
let inline addTwice x y =
    genericAdd (genericAdd x y) y

[<ReflectedDefinition>]
let inline increment x =
    x + LanguagePrimitives.GenericOne

[<ReflectedDefinition>]
let inline sumArr (arr : Arr<N<4>, ^a>) =
    let mutable s = LanguagePrimitives.GenericZero
    for i in 0 .. 3 do
        s <- s + arr.[i]
    s

[<ReflectedDefinition>]
let inline maxOf a b =
    if a > b then a else b

[<ReflectedDefinition>]
let inline lengthOf (x : ^a) : float32 =
    (^a : (member Length : float32) x)

[<ReflectedDefinition>]
let inline toFloat x =
    float32 x

[<ReflectedDefinition>]
let inline squareRoot x =
    sqrt x

[<ReflectedDefinition; Inline>]
let inline scaleInline x s =
    x * s

[<ReflectedDefinition>]
let inline pick (flag : bool) a b =
    if flag then a + b else a - b

[<ReflectedDefinition>]
let addVectors (a : V3f) (b : V3f) =
    genericAdd a b

// generic functions without constraints (no witnesses involved)
[<ReflectedDefinition>]
let select (c : bool) (a : 'a) (b : 'a) =
    if c then a else b

type Pair<'a> =
    {
        first : 'a
        second : 'a
    }

[<ReflectedDefinition>]
let swap (p : Pair<'a>) =
    { first = p.second; second = p.first }

let private helper = @"[A-Za-z0-9_]+"

[<Test>]
let ``Generic add with vectors and scalars``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let x = genericAdd v.c.XYZ v.n
            let y = genericAdd v.id 1
            return V4f(x, float32 y)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^vec3 " + helper + @"_genericAdd_" + helper + @"\(vec3 x, vec3 y\)", 1
        @"^int " + helper + @"_genericAdd_" + helper + @"\(int x, int y\)", 1
        @"return \(x \+ y\);", 2
    ]

[<Test>]
let ``Generic function with several operators``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let a = lerpGeneric v.c.XYZ v.n 0.5f
            let b = lerpGeneric v.c.X v.c.Y 0.25f
            return V4f(a, b)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^vec3 " + helper + @"_lerpGeneric_" + helper + @"\(vec3 a, vec3 b, float t\)", 1
        @"^float " + helper + @"_lerpGeneric_" + helper + @"\(float a, float b, float t\)", 1
        @"return \(a \+ \(\(b - a\) \* t\)\);", 2
    ]

[<Test>]
let ``Generic function calling a generic function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(addTwice v.c.XYZ v.n, 1.0f)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^vec3 " + helper + @"_addTwice_" + helper + @"\(vec3 x, vec3 y\)", 1
        @"^vec3 " + helper + @"_genericAdd_" + helper + @"\(vec3 x, vec3 y\)", 1
    ]

[<Test>]
let ``GenericOne is folded``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let a = increment v.id
            let b = increment v.c.X
            let c = increment v.n
            return V4f(c, float32 a + b)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"return \(x \+ 1\);", 1
        @"return \(x \+ 1\.0\);", 1
        @"return \(x \+ vec3\(1\.0, 1\.0, 1\.0\)\);", 1
        @"GenericOne", 0
    ]

[<Test>]
let ``GenericZero accumulator in a loop``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let arr = Arr<N<4>, V3f>()
            arr.[0] <- v.n
            arr.[1] <- v.c.XYZ
            arr.[2] <- v.n * 2.0f
            arr.[3] <- v.c.XYZ * 2.0f
            return V4f(sumArr arr, 1.0f)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^vec3 " + helper + @"_sumArr_" + helper + @"\(vec3 arr\[4\]\)", 1
        @"vec3 s = vec3\(0\.0, 0\.0, 0\.0\);", 1
        @"GenericZero", 0
    ]

[<Test>]
let ``Generic comparison``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let a = maxOf v.id 3
            let b = maxOf v.c.X v.c.Y
            return V4f(float32 a + b)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^int " + helper + @"_maxOf_" + helper + @"\(int a, int b\)", 1
        @"^float " + helper + @"_maxOf_" + helper + @"\(float a, float b\)", 1
        @"\(a > b\)", 2
    ]

[<Test>]
let ``Explicit member constraint``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(lengthOf v.n + lengthOf v.c)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^float " + helper + @"_lengthOf_" + helper + @"\(vec3 x\)", 1
        @"^float " + helper + @"_lengthOf_" + helper + @"\(vec4 x\)", 1
        @"length\(x\)", 2
    ]

[<Test>]
let ``Generic conversion and math intrinsic``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(squareRoot (toFloat v.id) + squareRoot v.c.X)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"float\(x\)", 1
        @"sqrt\(x\)", 1
        @"_squareRoot_", 3 // one definition, two calls
    ]

[<Test>]
let ``Inline attribute on a generic function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(scaleInline v.n 2.0f, scaleInline v.c.X 3.0f)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"_scaleInline_", 0
        @"\(fs_Normal \* 2\.0\)", 1
        @"\(fs_Colors\.x \* 3\.0\)", 1
    ]

[<Test>]
let ``Generic function with a constant flag is specialized``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(pick true v.c.XYZ v.n + pick false v.c.XYZ v.n, 1.0f)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^vec3 " + helper + @"_pick_" + helper + @"\(vec3 a, vec3 b\)", 2
        @"flag", 0
    ]

[<Test>]
let ``Generic function called from a utility function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(addVectors v.c.XYZ v.n, 1.0f)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^vec3 " + helper + @"_addVectors_" + helper + @"\(vec3 a, vec3 b\)", 1
        @"^vec3 " + helper + @"_genericAdd_" + helper + @"\(vec3 x, vec3 y\)", 1
    ]

[<Test>]
let ``Generic function without constraints``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let a = select (v.id > 0) v.c.XYZ v.n
            let b = select (v.id > 1) v.id 2
            return V4f(a, float32 b)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^vec3 " + helper + @"_select_" + helper + @"\(bool c, vec3 a, vec3 b\)", 1
        @"^int " + helper + @"_select_" + helper + @"\(bool c, int a, int b\)", 1
    ]

[<Test>]
let ``Generic record type``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let p = swap { first = v.c.XYZ; second = v.n }
            let q = swap { first = v.id; second = 1 }
            return V4f(p.first, float32 q.first)
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"^struct " + helper + @"Pair" + helper, 2 // one struct per instantiation
        @"_swap_", 4 // two definitions, two calls
    ]
