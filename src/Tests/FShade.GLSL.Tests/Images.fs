module Images

open Aardvark.Base
open FShade
open NUnit.Framework
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
    }

[<AutoOpen>]
module ImageUniforms =

    type UniformScope with
        member x.Img1D        : Image1d<Formats.rgba8> = x?Foo1D
        member x.Img1DArray   : Image1dArray<Formats.rgba8> = x?Foo1DArray
        member x.Img2D        : Image2d<Formats.rgba8> = x?Foo2D
        member x.Img2DArray   : Image2dArray<Formats.rgba8> = x?Foo2DArray
        member x.Img2DMS      : Image2dMS<Formats.rgba8> = x?Foo2DMS
        member x.Img2DArrayMS : Image2dArrayMS<Formats.rgba8> = x?Foo2DArrayMS
        member x.Img3D        : Image3d<Formats.rgba8> = x?Foo3D
        member x.ImgCube      : ImageCube<Formats.rgba8> = x?FooCube
        member x.ImgCubeArray : ImageCubeArray<Formats.rgba8> = x?FooCubeArray

        member x.IntImg1D        : IntImage1d<Formats.r32i> = x?IntFoo1D
        member x.IntImg1DArray   : IntImage1dArray<Formats.r32i> = x?IntFoo1DArray
        member x.IntImg2D        : IntImage2d<Formats.r32i> = x?IntFoo2D
        member x.IntImg2DArray   : IntImage2dArray<Formats.r32i> = x?IntFoo2DArray
        member x.IntImg2DMS      : IntImage2dMS<Formats.r32i> = x?IntFoo2DMS
        member x.IntImg2DArrayMS : IntImage2dArrayMS<Formats.r32i> = x?IntFoo2DArrayMS
        member x.IntImg3D        : IntImage3d<Formats.r32i> = x?IntFoo3D
        member x.IntImgCube      : IntImageCube<Formats.r32i> = x?IntFooCube
        member x.IntImgCubeArray : IntImageCubeArray<Formats.r32i> = x?IntFooCubeArray

        member x.UIntImg1D        : UIntImage1d<Formats.r32ui> = x?UIntFoo1D
        member x.UIntImg1DArray   : UIntImage1dArray<Formats.r32ui> = x?UIntFoo1DArray
        member x.UIntImg2D        : UIntImage2d<Formats.r32ui> = x?UIntFoo2D
        member x.UIntImg2DArray   : UIntImage2dArray<Formats.r32ui> = x?UIntFoo2DArray
        member x.UIntImg2DMS      : UIntImage2dMS<Formats.r32ui> = x?UIntFoo2DMS
        member x.UIntImg2DArrayMS : UIntImage2dArrayMS<Formats.r32ui> = x?UIntFoo2DArrayMS
        member x.UIntImg3D        : UIntImage3d<Formats.r32ui> = x?UIntFoo3D
        member x.UIntImgCube      : UIntImageCube<Formats.r32ui> = x?UIntFooCube
        member x.UIntImgCubeArray : UIntImageCubeArray<Formats.r32ui> = x?UIntFooCubeArray

        member x.Img_rgba32f        : Image1d<Formats.rgba32f>        = x?rgba32f
        member x.Img_rgba16f        : Image1d<Formats.rgba16f>        = x?rgba16f
        member x.Img_rg32f          : Image1d<Formats.rg32f>          = x?rg32f
        member x.Img_rg16f          : Image1d<Formats.rg16f>          = x?rg16f
        member x.Img_r11g11b10f     : Image1d<Formats.r11g11b10f>     = x?r11g11b10f
        member x.Img_r32f           : Image1d<Formats.r32f>           = x?r32f
        member x.Img_r16f           : Image1d<Formats.r16f>           = x?r16f
        member x.Img_rgba16         : Image1d<Formats.rgba16>         = x?rgba16
        member x.Img_rgb10a2        : Image1d<Formats.rgb10a2>        = x?rgb10a2
        member x.Img_rgba8          : Image1d<Formats.rgba8>          = x?rgba8
        member x.Img_rg16           : Image1d<Formats.rg16>           = x?rg16
        member x.Img_rg8            : Image1d<Formats.rg8>            = x?rg8
        member x.Img_r16            : Image1d<Formats.r16>            = x?r16
        member x.Img_r8             : Image1d<Formats.r8>             = x?r8
        member x.Img_rgba16_snorm   : Image1d<Formats.rgba16_snorm>   = x?rgba16_snorm
        member x.Img_rgba8_snorm    : Image1d<Formats.rgba8_snorm>    = x?rgba8_snorm
        member x.Img_rg16_snorm     : Image1d<Formats.rg16_snorm>     = x?rg16_snorm
        member x.Img_rg8_snorm      : Image1d<Formats.rg8_snorm>      = x?rg8_snorm
        member x.Img_r16_snorm      : Image1d<Formats.r16_snorm>      = x?r16_snorm
        member x.Img_r8_snorm       : Image1d<Formats.r8_snorm>       = x?r8_snorm
        member x.Img_rgba32ui       : UIntImage1d<Formats.rgba32ui>   = x?rgba32ui
        member x.Img_rgba16ui       : UIntImage1d<Formats.rgba16ui>   = x?rgba16ui
        member x.Img_rgb10a2ui      : UIntImage1d<Formats.rgb10a2ui>  = x?rgb10a2ui
        member x.Img_rgba8ui        : UIntImage1d<Formats.rgba8ui>    = x?rgba8ui
        member x.Img_rg32ui         : UIntImage1d<Formats.rg32ui>     = x?rg32ui
        member x.Img_rg16ui         : UIntImage1d<Formats.rg16ui>     = x?rg16ui
        member x.Img_rg8ui          : UIntImage1d<Formats.rg8ui>      = x?rg8ui
        member x.Img_r32ui          : UIntImage1d<Formats.r32ui>      = x?r32ui
        member x.Img_r16ui          : UIntImage1d<Formats.r16ui>      = x?r16ui
        member x.Img_r8ui           : UIntImage1d<Formats.r8ui>       = x?r8ui
        member x.Img_rgba32i        : IntImage1d<Formats.rgba32i>     = x?rgba32i
        member x.Img_rgba16i        : IntImage1d<Formats.rgba16i>     = x?rgba16i
        member x.Img_rgba8i         : IntImage1d<Formats.rgba8i>      = x?rgba8i
        member x.Img_rg32i          : IntImage1d<Formats.rg32i>       = x?rg32i
        member x.Img_rg16i          : IntImage1d<Formats.rg16i>       = x?rg16i
        member x.Img_rg8i           : IntImage1d<Formats.rg8i>        = x?rg8i
        member x.Img_r32i           : IntImage1d<Formats.r32i>        = x?r32i
        member x.Img_r16i           : IntImage1d<Formats.r16i>        = x?r16i
        member x.Img_r8i            : IntImage1d<Formats.r8i>         = x?r8i

[<Test>]
let ``Size``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable ret = V3i.Zero

            ret <- ret + V3i(uniform.Img1D.Size, 0, 0)
            ret <- ret + V3i(uniform.Img1DArray.Size, 0)
            ret <- ret + V3i(uniform.Img2D.Size, 0)
            ret <- ret + uniform.Img2DArray.Size
            ret <- ret + uniform.Img3D.Size
            ret <- ret + V3i(uniform.ImgCube.Size, 0)
            ret <- ret + uniform.ImgCubeArray.Size
            ret <- ret + V3i(uniform.Img2DMS.Size, 0)
            ret <- ret + uniform.Img2DArrayMS.Size

            ret <- ret + V3i(uniform.IntImg1D.Size, 0, 0)
            ret <- ret + V3i(uniform.IntImg1DArray.Size, 0)
            ret <- ret + V3i(uniform.IntImg2D.Size, 0)
            ret <- ret + uniform.IntImg2DArray.Size
            ret <- ret + uniform.IntImg3D.Size
            ret <- ret + V3i(uniform.IntImgCube.Size, 0)
            ret <- ret + uniform.IntImgCubeArray.Size
            ret <- ret + V3i(uniform.IntImg2DMS.Size, 0)
            ret <- ret + uniform.IntImg2DArrayMS.Size

            ret <- ret + V3i(uniform.UIntImg1D.Size, 0, 0)
            ret <- ret + V3i(uniform.UIntImg1DArray.Size, 0)
            ret <- ret + V3i(uniform.UIntImg2D.Size, 0)
            ret <- ret + uniform.UIntImg2DArray.Size
            ret <- ret + uniform.UIntImg3D.Size
            ret <- ret + V3i(uniform.UIntImgCube.Size, 0)
            ret <- ret + uniform.UIntImgCubeArray.Size
            ret <- ret + V3i(uniform.UIntImg2DMS.Size, 0)
            ret <- ret + uniform.UIntImg2DArrayMS.Size

            return ret
        }

    GLSL.shouldCompile [Effect.ofFunction fs]


[<Test>]
let ``Samples``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = uniform.Img2DMS.Samples
            let _ = uniform.Img2DArrayMS.Samples
            let _ = uniform.IntImg2DMS.Samples
            let _ = uniform.IntImg2DArrayMS.Samples
            let _ = uniform.UIntImg2DMS.Samples
            let _ = uniform.UIntImg2DArrayMS.Samples

            return 0
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Load``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + uniform.Img1D.Load(1)                      + uniform.Img1D.[1]
            c <- c + uniform.Img1DArray.Load(0, 1)              + uniform.Img1DArray.[0, 1]
            c <- c + uniform.Img2D.Load V2i.Zero                + uniform.Img2D.[V2i.Zero]
            c <- c + uniform.Img2DArray.Load(V2i.Zero, 1)       + uniform.Img2DArray.[V2i.Zero, 1]
            c <- c + uniform.Img3D.Load(V3i.Zero)               + uniform.Img3D.[V3i.Zero]
            c <- c + uniform.ImgCube.Load(V2i.Zero, 3)          + uniform.ImgCube.[V2i.Zero, 3]
            c <- c + uniform.ImgCubeArray.Load(V2i.Zero, 4)     + uniform.ImgCubeArray.[V2i.Zero, 4]
            c <- c + uniform.Img2DMS.Load(V2i.Zero, 0)          + uniform.Img2DMS.[V2i.Zero, 0]
            c <- c + uniform.Img2DArrayMS.Load(V2i.Zero, 1, 0)  + uniform.Img2DArrayMS.[V2i.Zero, 1, 0]

            let mutable ci = V4i.Zero
            ci <- ci + uniform.IntImg1D.Load(1)                     + uniform.IntImg1D.[1]
            ci <- ci + uniform.IntImg1DArray.Load(0, 1)             + uniform.IntImg1DArray.[0, 1]
            ci <- ci + uniform.IntImg2D.Load(V2i.Zero)              + uniform.IntImg2D.[V2i.Zero]
            ci <- ci + uniform.IntImg2DArray.Load(V2i.Zero, 1)      + uniform.IntImg2DArray.[V2i.Zero, 1]
            ci <- ci + uniform.IntImg3D.Load(V3i.Zero)              + uniform.IntImg3D.[V3i.Zero]
            ci <- ci + uniform.IntImgCube.Load(V2i.Zero, 3)         + uniform.IntImgCube.[V2i.Zero, 3]
            ci <- ci + uniform.IntImgCubeArray.Load(V2i.Zero, 4)    + uniform.IntImgCubeArray.[V2i.Zero, 4]
            ci <- ci + uniform.IntImg2DMS.Load(V2i.Zero, 0)         + uniform.IntImg2DMS.[V2i.Zero, 0]
            ci <- ci + uniform.IntImg2DArrayMS.Load(V2i.Zero, 1, 0) + uniform.IntImg2DArrayMS.[V2i.Zero, 1, 0]

            let mutable cui = V4ui.Zero
            cui <- cui + uniform.UIntImg1D.Load(1)                     + uniform.UIntImg1D.[1]
            cui <- cui + uniform.UIntImg1DArray.Load(0, 1)             + uniform.UIntImg1DArray.[0, 1]
            cui <- cui + uniform.UIntImg2D.Load(V2i.Zero)              + uniform.UIntImg2D.[V2i.Zero]
            cui <- cui + uniform.UIntImg2DArray.Load(V2i.Zero, 1)      + uniform.UIntImg2DArray.[V2i.Zero, 1]
            cui <- cui + uniform.UIntImg3D.Load(V3i.Zero)              + uniform.UIntImg3D.[V3i.Zero]
            cui <- cui + uniform.UIntImgCube.Load(V2i.Zero, 3)         + uniform.UIntImgCube.[V2i.Zero, 3]
            cui <- cui + uniform.UIntImgCubeArray.Load(V2i.Zero, 4)    + uniform.UIntImgCubeArray.[V2i.Zero, 4]
            cui <- cui + uniform.UIntImg2DMS.Load(V2i.Zero, 0)         + uniform.UIntImg2DMS.[V2i.Zero, 0]
            cui <- cui + uniform.UIntImg2DArrayMS.Load(V2i.Zero, 1, 0) + uniform.UIntImg2DArrayMS.[V2i.Zero, 1, 0]

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Store``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            uniform.Img1D.Store(1, V4d.Zero)
            uniform.Img1DArray.Store(0, 1, V4d.Zero)
            uniform.Img2D.Store(V2i.Zero, V4d.Zero)
            uniform.Img2DArray.Store(V2i.Zero, 1, V4d.Zero)
            uniform.Img3D.Store(V3i.Zero, V4d.Zero)
            uniform.ImgCube.Store(V2i.Zero, 3, V4d.Zero)
            uniform.ImgCubeArray.Store(V2i.Zero, 4, V4d.Zero)
            uniform.Img2DMS.Store(V2i.Zero, 0, V4d.Zero)
            uniform.Img2DArrayMS.Store(V2i.Zero, 1, 0, V4d.Zero)

            uniform.Img1D.[1] <- V4d.Zero
            uniform.Img1DArray.[0, 1] <- V4d.Zero
            uniform.Img2D.[V2i.Zero] <- V4d.Zero
            uniform.Img2DArray.[V2i.Zero, 1] <- V4d.Zero
            uniform.Img3D.[V3i.Zero] <- V4d.Zero
            uniform.ImgCube.[V2i.Zero, 3] <- V4d.Zero
            uniform.ImgCubeArray.[V2i.Zero, 4] <- V4d.Zero
            uniform.Img2DMS.[V2i.Zero, 0] <- V4d.Zero
            uniform.Img2DArrayMS.[V2i.Zero, 1, 0] <- V4d.Zero

            uniform.IntImg1D.Store(1, V4i.Zero)
            uniform.IntImg1DArray.Store(0, 1, V4i.Zero)
            uniform.IntImg2D.Store(V2i.Zero, V4i.Zero)
            uniform.IntImg2DArray.Store(V2i.Zero, 1, V4i.Zero)
            uniform.IntImg3D.Store(V3i.Zero, V4i.Zero)
            uniform.IntImgCube.Store(V2i.Zero, 3, V4i.Zero)
            uniform.IntImgCubeArray.Store(V2i.Zero, 4, V4i.Zero)
            uniform.IntImg2DMS.Store(V2i.Zero, 0, V4i.Zero)
            uniform.IntImg2DArrayMS.Store(V2i.Zero, 1, 0, V4i.Zero)

            uniform.IntImg1D.[1] <- V4i.Zero
            uniform.IntImg1DArray.[0, 1] <- V4i.Zero
            uniform.IntImg2D.[V2i.Zero] <- V4i.Zero
            uniform.IntImg2DArray.[V2i.Zero, 1] <- V4i.Zero
            uniform.IntImg3D.[V3i.Zero] <- V4i.Zero
            uniform.IntImgCube.[V2i.Zero, 3] <- V4i.Zero
            uniform.IntImgCubeArray.[V2i.Zero, 4] <- V4i.Zero
            uniform.IntImg2DMS.[V2i.Zero, 0] <- V4i.Zero
            uniform.IntImg2DArrayMS.[V2i.Zero, 1, 0] <- V4i.Zero

            uniform.UIntImg1D.Store(1, V4ui.Zero)
            uniform.UIntImg1DArray.Store(0, 1, V4ui.Zero)
            uniform.UIntImg2D.Store(V2i.Zero, V4ui.Zero)
            uniform.UIntImg2DArray.Store(V2i.Zero, 1, V4ui.Zero)
            uniform.UIntImg3D.Store(V3i.Zero, V4ui.Zero)
            uniform.UIntImgCube.Store(V2i.Zero, 3, V4ui.Zero)
            uniform.UIntImgCubeArray.Store(V2i.Zero, 4, V4ui.Zero)
            uniform.UIntImg2DMS.Store(V2i.Zero, 0, V4ui.Zero)
            uniform.UIntImg2DArrayMS.Store(V2i.Zero, 1, 0, V4ui.Zero)

            uniform.UIntImg1D.[1] <- V4ui.Zero
            uniform.UIntImg1DArray.[0, 1] <- V4ui.Zero
            uniform.UIntImg2D.[V2i.Zero] <- V4ui.Zero
            uniform.UIntImg2DArray.[V2i.Zero, 1] <- V4ui.Zero
            uniform.UIntImg3D.[V3i.Zero] <- V4ui.Zero
            uniform.UIntImgCube.[V2i.Zero, 3] <- V4ui.Zero
            uniform.UIntImgCubeArray.[V2i.Zero, 4] <- V4ui.Zero
            uniform.UIntImg2DMS.[V2i.Zero, 0] <- V4ui.Zero
            uniform.UIntImg2DArrayMS.[V2i.Zero, 1, 0] <- V4ui.Zero

            return V3d.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``AtomicAdd``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = uniform.IntImg1D.AtomicAdd(1, 0)
            let _ = uniform.IntImg1DArray.AtomicAdd(0, 1, 0)
            let _ = uniform.IntImg2D.AtomicAdd(V2i.Zero, 0)
            let _ = uniform.IntImg2DArray.AtomicAdd(V2i.Zero, 1, 0)
            let _ = uniform.IntImg3D.AtomicAdd(V3i.Zero, 0)
            let _ = uniform.IntImgCube.AtomicAdd(V2i.Zero, 3, 0)
            let _ = uniform.IntImgCubeArray.AtomicAdd(V2i.Zero, 4, 0)
            let _ = uniform.IntImg2DMS.AtomicAdd(V2i.Zero, 0, 0)
            let _ = uniform.IntImg2DArrayMS.AtomicAdd(V2i.Zero, 1, 0, 0)

            let _ = uniform.UIntImg1D.AtomicAdd(1, 0u)
            let _ = uniform.UIntImg1DArray.AtomicAdd(0, 1, 0u)
            let _ = uniform.UIntImg2D.AtomicAdd(V2i.Zero, 0u)
            let _ = uniform.UIntImg2DArray.AtomicAdd(V2i.Zero, 1, 0u)
            let _ = uniform.UIntImg3D.AtomicAdd(V3i.Zero, 0u)
            let _ = uniform.UIntImgCube.AtomicAdd(V2i.Zero, 3, 0u)
            let _ = uniform.UIntImgCubeArray.AtomicAdd(V2i.Zero, 4, 0u)
            let _ = uniform.UIntImg2DMS.AtomicAdd(V2i.Zero, 0, 0u)
            let _ = uniform.UIntImg2DArrayMS.AtomicAdd(V2i.Zero, 1, 0, 0u)

            return V3d.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``AtomicMin``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = uniform.IntImg1D.AtomicMin(1, 0)
            let _ = uniform.IntImg1DArray.AtomicMin(0, 1, 0)
            let _ = uniform.IntImg2D.AtomicMin(V2i.Zero, 0)
            let _ = uniform.IntImg2DArray.AtomicMin(V2i.Zero, 1, 0)
            let _ = uniform.IntImg3D.AtomicMin(V3i.Zero, 0)
            let _ = uniform.IntImgCube.AtomicMin(V2i.Zero, 3, 0)
            let _ = uniform.IntImgCubeArray.AtomicMin(V2i.Zero, 4, 0)
            let _ = uniform.IntImg2DMS.AtomicMin(V2i.Zero, 0, 0)
            let _ = uniform.IntImg2DArrayMS.AtomicMin(V2i.Zero, 1, 0, 0)

            let _ = uniform.UIntImg1D.AtomicMin(1, 0u)
            let _ = uniform.UIntImg1DArray.AtomicMin(0, 1, 0u)
            let _ = uniform.UIntImg2D.AtomicMin(V2i.Zero, 0u)
            let _ = uniform.UIntImg2DArray.AtomicMin(V2i.Zero, 1, 0u)
            let _ = uniform.UIntImg3D.AtomicMin(V3i.Zero, 0u)
            let _ = uniform.UIntImgCube.AtomicMin(V2i.Zero, 3, 0u)
            let _ = uniform.UIntImgCubeArray.AtomicMin(V2i.Zero, 4, 0u)
            let _ = uniform.UIntImg2DMS.AtomicMin(V2i.Zero, 0, 0u)
            let _ = uniform.UIntImg2DArrayMS.AtomicMin(V2i.Zero, 1, 0, 0u)

            return V3d.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``AtomicMax``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = uniform.IntImg1D.AtomicMax(1, 0)
            let _ = uniform.IntImg1DArray.AtomicMax(0, 1, 0)
            let _ = uniform.IntImg2D.AtomicMax(V2i.Zero, 0)
            let _ = uniform.IntImg2DArray.AtomicMax(V2i.Zero, 1, 0)
            let _ = uniform.IntImg3D.AtomicMax(V3i.Zero, 0)
            let _ = uniform.IntImgCube.AtomicMax(V2i.Zero, 3, 0)
            let _ = uniform.IntImgCubeArray.AtomicMax(V2i.Zero, 4, 0)
            let _ = uniform.IntImg2DMS.AtomicMax(V2i.Zero, 0, 0)
            let _ = uniform.IntImg2DArrayMS.AtomicMax(V2i.Zero, 1, 0, 0)

            let _ = uniform.UIntImg1D.AtomicMax(1, 0u)
            let _ = uniform.UIntImg1DArray.AtomicMax(0, 1, 0u)
            let _ = uniform.UIntImg2D.AtomicMax(V2i.Zero, 0u)
            let _ = uniform.UIntImg2DArray.AtomicMax(V2i.Zero, 1, 0u)
            let _ = uniform.UIntImg3D.AtomicMax(V3i.Zero, 0u)
            let _ = uniform.UIntImgCube.AtomicMax(V2i.Zero, 3, 0u)
            let _ = uniform.UIntImgCubeArray.AtomicMax(V2i.Zero, 4, 0u)
            let _ = uniform.UIntImg2DMS.AtomicMax(V2i.Zero, 0, 0u)
            let _ = uniform.UIntImg2DArrayMS.AtomicMax(V2i.Zero, 1, 0, 0u)

            return V3d.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``AtomicAnd``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = uniform.IntImg1D.AtomicAnd(1, 0)
            let _ = uniform.IntImg1DArray.AtomicAnd(0, 1, 0)
            let _ = uniform.IntImg2D.AtomicAnd(V2i.Zero, 0)
            let _ = uniform.IntImg2DArray.AtomicAnd(V2i.Zero, 1, 0)
            let _ = uniform.IntImg3D.AtomicAnd(V3i.Zero, 0)
            let _ = uniform.IntImgCube.AtomicAnd(V2i.Zero, 3, 0)
            let _ = uniform.IntImgCubeArray.AtomicAnd(V2i.Zero, 4, 0)
            let _ = uniform.IntImg2DMS.AtomicAnd(V2i.Zero, 0, 0)
            let _ = uniform.IntImg2DArrayMS.AtomicAnd(V2i.Zero, 1, 0, 0)

            let _ = uniform.UIntImg1D.AtomicAnd(1, 0u)
            let _ = uniform.UIntImg1DArray.AtomicAnd(0, 1, 0u)
            let _ = uniform.UIntImg2D.AtomicAnd(V2i.Zero, 0u)
            let _ = uniform.UIntImg2DArray.AtomicAnd(V2i.Zero, 1, 0u)
            let _ = uniform.UIntImg3D.AtomicAnd(V3i.Zero, 0u)
            let _ = uniform.UIntImgCube.AtomicAnd(V2i.Zero, 3, 0u)
            let _ = uniform.UIntImgCubeArray.AtomicAnd(V2i.Zero, 4, 0u)
            let _ = uniform.UIntImg2DMS.AtomicAnd(V2i.Zero, 0, 0u)
            let _ = uniform.UIntImg2DArrayMS.AtomicAnd(V2i.Zero, 1, 0, 0u)

            return V3d.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``AtomicOr``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = uniform.IntImg1D.AtomicOr(1, 0)
            let _ = uniform.IntImg1DArray.AtomicOr(0, 1, 0)
            let _ = uniform.IntImg2D.AtomicOr(V2i.Zero, 0)
            let _ = uniform.IntImg2DArray.AtomicOr(V2i.Zero, 1, 0)
            let _ = uniform.IntImg3D.AtomicOr(V3i.Zero, 0)
            let _ = uniform.IntImgCube.AtomicOr(V2i.Zero, 3, 0)
            let _ = uniform.IntImgCubeArray.AtomicOr(V2i.Zero, 4, 0)
            let _ = uniform.IntImg2DMS.AtomicOr(V2i.Zero, 0, 0)
            let _ = uniform.IntImg2DArrayMS.AtomicOr(V2i.Zero, 1, 0, 0)

            let _ = uniform.UIntImg1D.AtomicOr(1, 0u)
            let _ = uniform.UIntImg1DArray.AtomicOr(0, 1, 0u)
            let _ = uniform.UIntImg2D.AtomicOr(V2i.Zero, 0u)
            let _ = uniform.UIntImg2DArray.AtomicOr(V2i.Zero, 1, 0u)
            let _ = uniform.UIntImg3D.AtomicOr(V3i.Zero, 0u)
            let _ = uniform.UIntImgCube.AtomicOr(V2i.Zero, 3, 0u)
            let _ = uniform.UIntImgCubeArray.AtomicOr(V2i.Zero, 4, 0u)
            let _ = uniform.UIntImg2DMS.AtomicOr(V2i.Zero, 0, 0u)
            let _ = uniform.UIntImg2DArrayMS.AtomicOr(V2i.Zero, 1, 0, 0u)

            return V3d.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``AtomicXor``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = uniform.IntImg1D.AtomicXor(1, 0)
            let _ = uniform.IntImg1DArray.AtomicXor(0, 1, 0)
            let _ = uniform.IntImg2D.AtomicXor(V2i.Zero, 0)
            let _ = uniform.IntImg2DArray.AtomicXor(V2i.Zero, 1, 0)
            let _ = uniform.IntImg3D.AtomicXor(V3i.Zero, 0)
            let _ = uniform.IntImgCube.AtomicXor(V2i.Zero, 3, 0)
            let _ = uniform.IntImgCubeArray.AtomicXor(V2i.Zero, 4, 0)
            let _ = uniform.IntImg2DMS.AtomicXor(V2i.Zero, 0, 0)
            let _ = uniform.IntImg2DArrayMS.AtomicXor(V2i.Zero, 1, 0, 0)

            let _ = uniform.UIntImg1D.AtomicXor(1, 0u)
            let _ = uniform.UIntImg1DArray.AtomicXor(0, 1, 0u)
            let _ = uniform.UIntImg2D.AtomicXor(V2i.Zero, 0u)
            let _ = uniform.UIntImg2DArray.AtomicXor(V2i.Zero, 1, 0u)
            let _ = uniform.UIntImg3D.AtomicXor(V3i.Zero, 0u)
            let _ = uniform.UIntImgCube.AtomicXor(V2i.Zero, 3, 0u)
            let _ = uniform.UIntImgCubeArray.AtomicXor(V2i.Zero, 4, 0u)
            let _ = uniform.UIntImg2DMS.AtomicXor(V2i.Zero, 0, 0u)
            let _ = uniform.UIntImg2DArrayMS.AtomicXor(V2i.Zero, 1, 0, 0u)

            return V3d.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``AtomicExchange``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = uniform.IntImg1D.AtomicExchange(1, 0)
            let _ = uniform.IntImg1DArray.AtomicExchange(0, 1, 0)
            let _ = uniform.IntImg2D.AtomicExchange(V2i.Zero, 0)
            let _ = uniform.IntImg2DArray.AtomicExchange(V2i.Zero, 1, 0)
            let _ = uniform.IntImg3D.AtomicExchange(V3i.Zero, 0)
            let _ = uniform.IntImgCube.AtomicExchange(V2i.Zero, 3, 0)
            let _ = uniform.IntImgCubeArray.AtomicExchange(V2i.Zero, 4, 0)
            let _ = uniform.IntImg2DMS.AtomicExchange(V2i.Zero, 0, 0)
            let _ = uniform.IntImg2DArrayMS.AtomicExchange(V2i.Zero, 1, 0, 0)

            let _ = uniform.UIntImg1D.AtomicExchange(1, 0u)
            let _ = uniform.UIntImg1DArray.AtomicExchange(0, 1, 0u)
            let _ = uniform.UIntImg2D.AtomicExchange(V2i.Zero, 0u)
            let _ = uniform.UIntImg2DArray.AtomicExchange(V2i.Zero, 1, 0u)
            let _ = uniform.UIntImg3D.AtomicExchange(V3i.Zero, 0u)
            let _ = uniform.UIntImgCube.AtomicExchange(V2i.Zero, 3, 0u)
            let _ = uniform.UIntImgCubeArray.AtomicExchange(V2i.Zero, 4, 0u)
            let _ = uniform.UIntImg2DMS.AtomicExchange(V2i.Zero, 0, 0u)
            let _ = uniform.UIntImg2DArrayMS.AtomicExchange(V2i.Zero, 1, 0, 0u)

            return V3d.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``AtomicCompareExchange``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = uniform.IntImg1D.AtomicCompareExchange(1, 42, 0)
            let _ = uniform.IntImg1DArray.AtomicCompareExchange(0, 1, 42, 0)
            let _ = uniform.IntImg2D.AtomicCompareExchange(V2i.Zero, 42, 0)
            let _ = uniform.IntImg2DArray.AtomicCompareExchange(V2i.Zero, 1, 42, 0)
            let _ = uniform.IntImg3D.AtomicCompareExchange(V3i.Zero, 42, 0)
            let _ = uniform.IntImgCube.AtomicCompareExchange(V2i.Zero, 3, 42, 0)
            let _ = uniform.IntImgCubeArray.AtomicCompareExchange(V2i.Zero, 4, 42, 0)
            let _ = uniform.IntImg2DMS.AtomicCompareExchange(V2i.Zero, 0, 42, 0)
            let _ = uniform.IntImg2DArrayMS.AtomicCompareExchange(V2i.Zero, 1, 0, 42, 0)

            let _ = uniform.UIntImg1D.AtomicCompareExchange(1, 42u, 0u)
            let _ = uniform.UIntImg1DArray.AtomicCompareExchange(0, 1, 42u, 0u)
            let _ = uniform.UIntImg2D.AtomicCompareExchange(V2i.Zero, 42u, 0u)
            let _ = uniform.UIntImg2DArray.AtomicCompareExchange(V2i.Zero, 1, 42u, 0u)
            let _ = uniform.UIntImg3D.AtomicCompareExchange(V3i.Zero, 42u, 0u)
            let _ = uniform.UIntImgCube.AtomicCompareExchange(V2i.Zero, 3, 42u, 0u)
            let _ = uniform.UIntImgCubeArray.AtomicCompareExchange(V2i.Zero, 4, 42u, 0u)
            let _ = uniform.UIntImg2DMS.AtomicCompareExchange(V2i.Zero, 0, 42u, 0u)
            let _ = uniform.UIntImg2DArrayMS.AtomicCompareExchange(V2i.Zero, 1, 0, 42u, 0u)

            return V3d.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Formats``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = uniform.Img_rgba32f.Load 0
            let _ = uniform.Img_rgba16f.Load 0
            let _ = uniform.Img_rg32f.Load 0
            let _ = uniform.Img_rg16f.Load 0
            let _ = uniform.Img_r11g11b10f.Load 0
            let _ = uniform.Img_r32f.Load 0
            let _ = uniform.Img_r16f.Load 0
            let _ = uniform.Img_rgba16.Load 0
            let _ = uniform.Img_rgb10a2.Load 0
            let _ = uniform.Img_rgba8.Load 0
            let _ = uniform.Img_rg16.Load 0
            let _ = uniform.Img_rg8.Load 0
            let _ = uniform.Img_r16.Load 0
            let _ = uniform.Img_r8.Load 0
            let _ = uniform.Img_rgba16_snorm.Load 0
            let _ = uniform.Img_rgba8_snorm.Load 0
            let _ = uniform.Img_rg16_snorm.Load 0
            let _ = uniform.Img_rg8_snorm.Load 0
            let _ = uniform.Img_r16_snorm.Load 0
            let _ = uniform.Img_r8_snorm.Load 0
            let _ = uniform.Img_rgba32ui.Load 0
            let _ = uniform.Img_rgba16ui.Load 0
            let _ = uniform.Img_rgb10a2ui.Load 0
            let _ = uniform.Img_rgba8ui.Load 0
            let _ = uniform.Img_rg32ui.Load 0
            let _ = uniform.Img_rg16ui.Load 0
            let _ = uniform.Img_rg8ui.Load 0
            let _ = uniform.Img_r32ui.Load 0
            let _ = uniform.Img_r16ui.Load 0
            let _ = uniform.Img_r8ui.Load 0
            let _ = uniform.Img_rgba32i.Load 0
            let _ = uniform.Img_rgba16i.Load 0
            let _ = uniform.Img_rgba8i.Load 0
            let _ = uniform.Img_rg32i.Load 0
            let _ = uniform.Img_rg16i.Load 0
            let _ = uniform.Img_rg8i.Load 0
            let _ = uniform.Img_r32i.Load 0
            let _ = uniform.Img_r16i.Load 0
            let _ = uniform.Img_r8i.Load 0

            return V3d.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]