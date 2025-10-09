module Color

open Aardvark.Base
open System.Text.RegularExpressions
open FShade
open NUnit.Framework
open FShade.Tests

type Vertex =
    {
        [<Position>] pos   : V4f
        [<Color>]    color : C3b
    }

type Vertexui =
    {
        [<Position>] pos   : V4f
        [<Color>]    color : C4ui
    }

[<ReflectedDefinition>]
let assertT<'T> (v : 'T) = v

[<Test>]
let ``Color vertex attribute``() =
    Setup.Run()

    let vs1 (v : Vertex) =
        vertex {
            return v
        }

    let vs2 (v : Vertexui) =
        vertex {
            return v
        }

    let effect =
        Effect.compose [
            Effect.ofFunction vs1
            Effect.ofFunction vs2
        ]

    GLSL.shouldCompileAndContainRegex [effect] [ Regex.Escape "uvec4((uvec3(Colors) * 16843009u), 4294967295u)" ]

[<Test>]
let ``Color from RGB``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let c = C4f(v.pos.X, v.pos.Y, v.pos.Z, v.pos.W)
            let _ = c.R
            let _ = c.G
            let _ = c.B
            let _ = c.A
            let c = C4f(v.pos.X, v.pos.Y, v.pos.Z)
            let _ = c.R
            let _ = c.G
            let _ = c.B
            let _ = c.A
            let c = C4b(v.pos.X, v.pos.Y, v.pos.Z, v.pos.W)
            let _ = c.R
            let _ = c.G
            let _ = c.B
            let _ = c.A
            let c = C4b(v.pos.X, v.pos.Y, v.pos.Z)
            let _ = c.R
            let _ = c.G
            let _ = c.B
            let _ = c.A
            return v
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Color from gray``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let c = C4f(v.pos.X)
            let _ = c.R
            let _ = c.G
            let _ = c.B
            let _ = c.A
            let c = C4b(v.pos.X)
            let _ = c.R
            let _ = c.G
            let _ = c.B
            let _ = c.A
            return v
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Color conversions``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let cb = v.color
            let _ = cb.R
            let _ = cb.[0]
            let _ = c4us cb
            let _ = C4us cb
            let _ = C4us.FromC3b cb
            let _ = cb.ToC4us()
            let _ = C4ui cb
            let _ = c4ui cb
            let _ = C4ui.FromC3b cb
            let _ = cb.ToC4ui()
            let _ = v4ui cb
            let _ = V4ui cb
            let _ = V4ui.FromC3b cb
            let _ = cb.ToV4ui()
            let _ = v4i cb
            let _ = V4i cb
            let _ = V4i.FromC3b cb
            let _ = cb.ToV4i()
            let _ = v4l cb
            let _ = V4l cb
            let _ = V4l.FromC3b cb
            let _ = cb.ToV4l()
            let _ = c4f cb
            let _ = C4f cb
            let _ = C4f.FromC3b cb
            let _ = cb.ToC4f()
            let _ = v4f cb
            let _ = V4f cb
            let _ = V4f.FromC3b cb
            let _ = cb.ToV4f()
            let _ = c4d cb
            let _ = C4d cb
            let _ = C4d.FromC3b cb
            let _ = cb.ToC4d()
            let _ = v4d cb
            let _ = V4d cb
            let _ = V4d.FromC3b cb
            let _ = cb.ToV4d()
            return v
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Color channel conversions``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Col.ByteToUShort    v.color.R
            let _ = assertT <| Col.ByteToUInt      v.color.R
            let _ = assertT <| Col.ByteToHalf      v.color.R
            let _ = assertT <| Col.ByteToFloat     v.color.R
            let _ = assertT <| Col.ByteToDouble    v.color.R

            let _ = assertT <| Col.UShortToByte   (uint16 v.color.R)
            let _ = assertT <| Col.UShortToUInt   (uint16 v.color.R)
            let _ = assertT <| Col.UShortToHalf   (uint16 v.color.R)
            let _ = assertT <| Col.UShortToFloat  (uint16 v.color.R)
            let _ = assertT <| Col.UShortToDouble (uint16 v.color.R)

            let _ = assertT <| Col.UIntToByte   (uint32 v.color.R)
            let _ = assertT <| Col.UIntToUShort (uint32 v.color.R)
            let _ = assertT <| Col.UIntToHalf   (uint32 v.color.R)
            let _ = assertT <| Col.UIntToFloat  (uint32 v.color.R)
            let _ = assertT <| Col.UIntToDouble (uint32 v.color.R)

            let _ = assertT <| Col.HalfToByte   (float16 (uint v.color.R))
            let _ = assertT <| Col.HalfToUShort (float16 (uint v.color.R))
            let _ = assertT <| Col.HalfToUInt   (float16 (uint v.color.R))
            let _ = assertT <| Col.HalfToFloat  (float16 (uint v.color.R))
            let _ = assertT <| Col.HalfToDouble (float16 (uint v.color.R))

            let _ = assertT <| Col.FloatToByte   (float32 v.color.R)
            let _ = assertT <| Col.FloatToUShort (float32 v.color.R)
            let _ = assertT <| Col.FloatToUInt   (float32 v.color.R)
            let _ = assertT <| Col.FloatToHalf   (float32 v.color.R)
            let _ = assertT <| Col.FloatToDouble (float32 v.color.R)

            let _ = assertT <| Col.FloatToByteClamped   (float32 v.color.R)
            let _ = assertT <| Col.FloatToUShortClamped (float32 v.color.R)
            let _ = assertT <| Col.FloatToUIntClamped   (float32 v.color.R)
            let _ = assertT <| Col.FloatToDoubleClamped (float32 v.color.R)

            let _ = assertT <| Col.DoubleToByte   (float v.color.R)
            let _ = assertT <| Col.DoubleToUShort (float v.color.R)
            let _ = assertT <| Col.DoubleToUInt   (float v.color.R)
            let _ = assertT <| Col.DoubleToHalf   (float v.color.R)
            let _ = assertT <| Col.DoubleToFloat  (float v.color.R)

            let _ = assertT <| Col.DoubleToByteClamped   (float v.color.R)
            let _ = assertT <| Col.DoubleToUShortClamped (float v.color.R)
            let _ = assertT <| Col.DoubleToUIntClamped   (float v.color.R)
            let _ = assertT <| Col.DoubleToFloatClamped  (float v.color.R)

            return v
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Color arithmetic``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT (2.0 * v.color)
            let _ = assertT (v.color * 2.0)
            let _ = assertT (2.0 * C3us v.color)
            let _ = assertT (C3us v.color * 2.0)
            let _ = assertT (2.0 * C3ui v.color)
            let _ = assertT (C3ui v.color * 2.0)
            let _ = assertT (2.0 * C3d v.color)
            let _ = assertT (C3d v.color * 2.0)

            let _ = assertT (2.0 / v.color)
            let _ = assertT (v.color / 2.0)
            let _ = assertT (2.0 / C3us v.color)
            let _ = assertT (C3us v.color / 2.0)
            let _ = assertT (2.0 / C3ui v.color)
            let _ = assertT (C3ui v.color / 2.0)
            let _ = assertT (2.0 / C3d v.color)
            let _ = assertT (C3d v.color / 2.0)

            return v
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Color lerp``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Lerp(0.5f, v.color, v.color)
            let _ = assertT <| Fun.Lerp(0.5, v.color, v.color)
            let _ = assertT <| Fun.Lerp(0.5f, C4b v.color, C4b v.color)
            let _ = assertT <| Fun.Lerp(0.5, C4b v.color, C4b v.color)
            let _ = assertT <| Fun.Lerp(V3f 0.5f, v.color, v.color)
            let _ = assertT <| Fun.Lerp(V3d 0.5, v.color, v.color)
            let _ = assertT <| Fun.Lerp(V4f 0.5f, C4b v.color, C4b v.color)
            let _ = assertT <| Fun.Lerp(V4d 0.5, C4b v.color, C4b v.color)

            let _ = assertT <| lerp v.color v.color 0.5f
            let _ = assertT <| lerp v.color v.color 0.5
            let _ = assertT <| lerp (C4b v.color) (C4b v.color) 0.5f
            let _ = assertT <| lerp (C4b v.color) (C4b v.color) 0.5
            let _ = assertT <| lerp v.color v.color (V3f 0.5f)
            let _ = assertT <| lerp v.color v.color (V3d 0.5)
            let _ = assertT <| lerp (C4b v.color) (C4b v.color) (V4f 0.5f)
            let _ = assertT <| lerp (C4b v.color) (C4b v.color) (V4d 0.5)

            let _ = assertT <| Fun.Lerp(0.5f, C3us v.color, C3us v.color)
            let _ = assertT <| Fun.Lerp(0.5, C3us v.color, C3us v.color)
            let _ = assertT <| Fun.Lerp(0.5f, C4us v.color, C4us v.color)
            let _ = assertT <| Fun.Lerp(0.5, C4us v.color, C4us v.color)
            let _ = assertT <| Fun.Lerp(V3f 0.5f, C3us v.color, C3us v.color)
            let _ = assertT <| Fun.Lerp(V3d 0.5, C3us v.color, C3us v.color)
            let _ = assertT <| Fun.Lerp(V4f 0.5f, C4us v.color, C4us v.color)
            let _ = assertT <| Fun.Lerp(V4d 0.5, C4us v.color, C4us v.color)

            let _ = assertT <| lerp (C3us v.color) (C3us v.color) 0.5f
            let _ = assertT <| lerp (C3us v.color) (C3us v.color) 0.5
            let _ = assertT <| lerp (C4us v.color) (C4us v.color) 0.5f
            let _ = assertT <| lerp (C4us v.color) (C4us v.color) 0.5
            let _ = assertT <| lerp (C3us v.color) (C3us v.color) (V3f 0.5f)
            let _ = assertT <| lerp (C3us v.color) (C3us v.color) (V3d 0.5)
            let _ = assertT <| lerp (C4us v.color) (C4us v.color) (V4f 0.5f)
            let _ = assertT <| lerp (C4us v.color) (C4us v.color) (V4d 0.5)
            
            let _ = assertT <| Fun.Lerp(0.5f, C3ui v.color, C3ui v.color)
            let _ = assertT <| Fun.Lerp(0.5, C3ui v.color, C3ui v.color)
            let _ = assertT <| Fun.Lerp(0.5f, C4ui v.color, C4ui v.color)
            let _ = assertT <| Fun.Lerp(0.5, C4ui v.color, C4ui v.color)
            let _ = assertT <| Fun.Lerp(V3f 0.5f, C3ui v.color, C3ui v.color)
            let _ = assertT <| Fun.Lerp(V3d 0.5, C3ui v.color, C3ui v.color)
            let _ = assertT <| Fun.Lerp(V4f 0.5f, C4ui v.color, C4ui v.color)
            let _ = assertT <| Fun.Lerp(V4d 0.5, C4ui v.color, C4ui v.color)

            let _ = assertT <| lerp (C3ui v.color) (C3ui v.color) 0.5f
            let _ = assertT <| lerp (C3ui v.color) (C3ui v.color) 0.5
            let _ = assertT <| lerp (C4ui v.color) (C4ui v.color) 0.5f
            let _ = assertT <| lerp (C4ui v.color) (C4ui v.color) 0.5
            let _ = assertT <| lerp (C3ui v.color) (C3ui v.color) (V3f 0.5f)
            let _ = assertT <| lerp (C3ui v.color) (C3ui v.color) (V3d 0.5)
            let _ = assertT <| lerp (C4ui v.color) (C4ui v.color) (V4f 0.5f)
            let _ = assertT <| lerp (C4ui v.color) (C4ui v.color) (V4d 0.5)
            
            let _ = assertT <| Fun.Lerp(0.5f, C3f v.color, C3f v.color)
            let _ = assertT <| Fun.Lerp(0.5f, C4f v.color, C4f v.color)
            let _ = assertT <| Fun.Lerp(V3f 0.5f, C3f v.color, C3f v.color)
            let _ = assertT <| Fun.Lerp(V4f 0.5f, C4f v.color, C4f v.color)

            let _ = assertT <| lerp (C3f v.color) (C3f v.color) 0.5f
            let _ = assertT <| lerp (C4f v.color) (C4f v.color) 0.5f
            let _ = assertT <| lerp (C3f v.color) (C3f v.color) (V3f 0.5f)
            let _ = assertT <| lerp (C4f v.color) (C4f v.color) (V4f 0.5f)

            let _ = assertT <| Fun.Lerp(0.5, C3d v.color, C3d v.color)
            let _ = assertT <| Fun.Lerp(0.5, C4d v.color, C4d v.color)
            let _ = assertT <| Fun.Lerp(V3d 0.5, C3d v.color, C3d v.color)
            let _ = assertT <| Fun.Lerp(V4d 0.5, C4d v.color, C4d v.color)

            let _ = assertT <| lerp (C3d v.color) (C3d v.color) 0.5
            let _ = assertT <| lerp (C4d v.color) (C4d v.color) 0.5
            let _ = assertT <| lerp (C3d v.color) (C3d v.color) (V3d 0.5)
            let _ = assertT <| lerp (C4d v.color) (C4d v.color) (V4d 0.5)

            return v
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Color relations``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Col.AllSmaller(v.color, 128uy)
            let _ = assertT <| Col.AllSmaller(128uy, v.color)
            let _ = assertT <| Col.AllSmaller(v.color, v.color)

            let _ = assertT <| Col.AllSmallerOrEqual(v.color, 128uy)
            let _ = assertT <| Col.AllSmallerOrEqual(128uy, v.color)
            let _ = assertT <| Col.AllSmallerOrEqual(v.color, v.color)

            let _ = assertT <| Col.AllGreater(v.color, 128uy)
            let _ = assertT <| Col.AllGreater(128uy, v.color)
            let _ = assertT <| Col.AllGreater(v.color, v.color)

            let _ = assertT <| Col.AllGreaterOrEqual(v.color, 128uy)
            let _ = assertT <| Col.AllGreaterOrEqual(128uy, v.color)
            let _ = assertT <| Col.AllGreaterOrEqual(v.color, v.color)

            let _ = assertT <| Col.AllEqual(v.color, 128uy)
            let _ = assertT <| Col.AllEqual(128uy, v.color)
            let _ = assertT <| Col.AllEqual(v.color, v.color)

            let _ = assertT <| Col.AllDifferent(v.color, 128uy)
            let _ = assertT <| Col.AllDifferent(128uy, v.color)
            let _ = assertT <| Col.AllDifferent(v.color, v.color)

            let _ = assertT <| Col.AnySmaller(v.color, 128uy)
            let _ = assertT <| Col.AnySmaller(128uy, v.color)
            let _ = assertT <| Col.AnySmaller(v.color, v.color)

            let _ = assertT <| Col.AnySmallerOrEqual(v.color, 128uy)
            let _ = assertT <| Col.AnySmallerOrEqual(128uy, v.color)
            let _ = assertT <| Col.AnySmallerOrEqual(v.color, v.color)

            let _ = assertT <| Col.AnyGreater(v.color, 128uy)
            let _ = assertT <| Col.AnyGreater(128uy, v.color)
            let _ = assertT <| Col.AnyGreater(v.color, v.color)

            let _ = assertT <| Col.AnyGreaterOrEqual(v.color, 128uy)
            let _ = assertT <| Col.AnyGreaterOrEqual(128uy, v.color)
            let _ = assertT <| Col.AnyGreaterOrEqual(v.color, v.color)

            let _ = assertT <| Col.AnyEqual(v.color, 128uy)
            let _ = assertT <| Col.AnyEqual(128uy, v.color)
            let _ = assertT <| Col.AnyEqual(v.color, v.color)

            let _ = assertT <| Col.AnyDifferent(v.color, 128uy)
            let _ = assertT <| Col.AnyDifferent(128uy, v.color)
            let _ = assertT <| Col.AnyDifferent(v.color, v.color)

            return v
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] [
        Regex.Escape "all(lessThan(",         3
        Regex.Escape "all(lessThanEqual(",    3
        Regex.Escape "all(greaterThan(",      3
        Regex.Escape "all(greaterThanEqual(", 3
        Regex.Escape "==",                    3
        Regex.Escape "all(notEqual(",         3
        Regex.Escape "any(lessThan(",         3
        Regex.Escape "any(lessThanEqual(",    3
        Regex.Escape "any(greaterThan(",      3
        Regex.Escape "any(greaterThanEqual(", 3
        Regex.Escape "any(equal(",            3
        Regex.Escape "!=",                    3
    ]