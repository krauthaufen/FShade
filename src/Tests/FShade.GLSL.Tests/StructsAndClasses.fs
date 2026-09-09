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
        "Vector2f_of_float32_float32\("
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
        "CVector2f_of_float32_float32\("
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