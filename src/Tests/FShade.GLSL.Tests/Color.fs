module Color

open Aardvark.Base
open FShade
open NUnit.Framework
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4f
        color : C3b
    }

[<ReflectedDefinition>]
let assertT<'T> (v : 'T) = v

[<Test>]
let ``Color vertex attribute``() =
    Setup.Run()

    let shader (v : Vertex) =
        fragment {
            return v.color
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] [ ]

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

            let _ = assertT <| Col.DoubleToByte   (float v.color.R)
            let _ = assertT <| Col.DoubleToUShort (float v.color.R)
            let _ = assertT <| Col.DoubleToUInt   (float v.color.R)
            let _ = assertT <| Col.DoubleToHalf   (float v.color.R)
            let _ = assertT <| Col.DoubleToFloat  (float v.color.R)

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