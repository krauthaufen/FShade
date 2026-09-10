module StructsAndClasses

open Aardvark.Base
open FShade
open NUnit.Framework
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
        [<Interpolation(InterpolationMode.NoPerspective ||| InterpolationMode.Sample)>] hugo : V3f
        foo : V4f
    }

[<Struct; ReflectedDefinition>]
type Vector2f =
    val mutable X : float32
    val mutable Y : float32
    new (x, y)   = { X = x; Y = y }
    new (v)      = { X = v; Y = v }
    new (v: int) = Vector2f(float32 v)
    new (v: V2f) = Vector2f(v.X, v.Y)
    [<Inline>] static member Zero = Vector2f 0f
    static member One = Vector2f 1f

[<ReflectedDefinition>]
type CVector2f =
    val mutable X : float32
    val mutable Y : float32
    new ()       = { X = 0.0f; Y = 0.0f }
    new (x, y)   = { X = x; Y = y }
    new (v)      = { X = v; Y = v }
    new (v: int) = CVector2f(float32 v)
    new (v: V2f) = CVector2f(v.X, v.Y)
    member this.SetX x = this.X <- x
    [<Inline>] member this.SetY y = this.X <- y
    member this.Length = sqrt (sqr this.X + sqr this.Y)
    [<Inline>]
    member this.Item
        with get idx = if idx = 0 then this.X else this.Y
        and set idx value = if idx = 0 then this.X <- value else this.Y <- value

type RVector2f =
    {
        X : float32
        Y : float32
    }

[<ReflectedDefinition>]
let vectorDot2 (a: Vector2f) (b: Vector2f) =
    a.X * b.X + a.Y * b.Y

[<ReflectedDefinition>]
let cvectorDot2 (a: CVector2f) (b: CVector2f) =
    a.X * b.X + a.Y * b.Y

[<Test>]
let ``Custom struct with constructors``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = Vector2f()
            let b = Vector2f(v.hugo.XY)
            let mutable c = Vector2f()
            c <- Vector2f v.foo.X
            c.X <- v.foo.Z
            let d = Vector2f(3)
            return a.X + a.Y + vectorDot2 b c + d.X
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] [
        "Vector2f a;"
        "Vector2f_of_float32\("
        "return .+_Vector2f\(v, v\);"
        @"Vector2f_of_Aardvark_Base_V2f\(fs_hugo\.xy\)"
        @"\+ new_.+_Vector2f_of_int32\(3\)\.X"
    ]

[<Test>]
let ``Custom class with constructors``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = CVector2f()
            let b = CVector2f(v.hugo.XY)
            let mutable c = CVector2f()
            c <- CVector2f v.foo.X
            c.X <- v.foo.Z
            let d = CVector2f(3)
            return a.X + a.Y + cvectorDot2 b c + d.X
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] [
        "CVector2f a = new_.+_CVector2f\(\);"
        "CVector2f_of_float32\("
        "return .+_CVector2f\(v, v\)"
        @"CVector2f_of_Aardvark_Base_V2f\(fs_hugo\.xy\)"
        @"\+ new_.+_CVector2f_of_int32\(3\)\.X"
    ]

[<Test>]
let ``Custom class with instance methods``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = CVector2f v.hugo.XY
            a.SetX v.foo.X
            a.SetY v.foo.Y
            return a.X + a.Y
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] [
        @"void .+_CVector2f_SetX_.+\(inout .+_CVector2f _this, float x\)"
        @".+_CVector2f_SetX_.+\(a, fs_foo\.x\);"
        @"a\.X = fs_foo\.y;"
    ]

[<Test>]
let ``Custom class and struct with properties``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let zero = Vector2f.Zero
            let one = Vector2f.One
            let a = CVector2f v.hugo.XY
            a.[0] <- a.[1] + zero.Y + one.X
            return a.Length + zero.X + one.Y
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] [
        @".+_Vector2f zero = new_.+_Vector2f_of_float32\(0\.0\);"
        @".+_Vector2f one = .+_Vector2f_get_One_.+\(\);"
        @"a\.X = \(\(a\.Y \+ zero\.Y\) \+ one\.X\);"
        @"ColorsOut = \(\(.+_CVector2f_get_Length_.+\(a\) \+ zero\.X\) \+ one\.Y\);"
    ]

[<Test>]
let ``Construction with built-in GLSL constructors``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = { Y = v.hugo.Y; X = v.hugo.X }
            let b = { a with Y = v.foo.Z }
            let c, d = v.hugo, v.foo
            let e = Vector2f(v.hugo.X, v.hugo.Y)
            let f = CVector2f(v.hugo.X, v.hugo.Y)
            return a.X + a.Y + b.X + b.Y + c.X + c.Y + d.X + d.Y + e.X + e.Y + f.X + f.Y
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] [
        ".+_RVector2f a = .+_RVector2f\(fs_hugo\.x, fs_hugo\.y\);"
        ".+_RVector2f b = .+_RVector2f\(a\.X, fs_foo\.z\);"
        "= tup_Aardvark_Base_V3f_Aardvark_Base_V4f\(fs_hugo, fs_foo\);"
        ".+_Vector2f e = .+_Vector2f\(fs_hugo\.x, fs_hugo\.y\);"
        ".+_CVector2f f = .+_CVector2f\(fs_hugo\.x, fs_hugo\.y\);"
    ]