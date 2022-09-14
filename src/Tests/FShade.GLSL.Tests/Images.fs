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

        member x.IntImg1D        : IntImage1d<Formats.rgba8i> = x?IntFoo1D
        member x.IntImg1DArray   : IntImage1dArray<Formats.rgba8i> = x?IntFoo1DArray
        member x.IntImg2D        : IntImage2d<Formats.rgba8i> = x?IntFoo2D
        member x.IntImg2DArray   : IntImage2dArray<Formats.rgba8i> = x?IntFoo2DArray
        member x.IntImg2DMS      : IntImage2dMS<Formats.rgba8i> = x?IntFoo2DMS
        member x.IntImg2DArrayMS : IntImage2dArrayMS<Formats.rgba8i> = x?IntFoo2DArrayMS
        member x.IntImg3D        : IntImage3d<Formats.rgba8i> = x?IntFoo3D
        member x.IntImgCube      : IntImageCube<Formats.rgba8i> = x?IntFooCube
        member x.IntImgCubeArray : IntImageCubeArray<Formats.rgba8i> = x?IntFooCubeArray


[<Test>]
let ``Size``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a0 = V3i(uniform.Img1D.Size, 0, 0)
            let a1 = V3i(uniform.Img1DArray.Size, 0)
            let a2 = V3i(uniform.Img2D.Size, 0)
            let a3 = uniform.Img2DArray.Size
            let a4 = uniform.Img3D.Size
            let a5 = V3i(uniform.ImgCube.Size, 0)
            let a6 = uniform.ImgCubeArray.Size
            let a7 = V3i(uniform.Img2DMS.Size, 0)
            let a8 = uniform.Img2DArrayMS.Size

            let a9  = V3i(uniform.IntImg1D.Size, 0, 0)
            let a10 = V3i(uniform.IntImg1DArray.Size, 0)
            let a11 = V3i(uniform.IntImg2D.Size, 0)
            let a12 = uniform.IntImg2DArray.Size
            let a13 = uniform.IntImg3D.Size
            let a14 = V3i(uniform.IntImgCube.Size, 0)
            let a15 = uniform.IntImgCubeArray.Size
            let a16 = V3i(uniform.IntImg2DMS.Size, 0)
            let a17 = uniform.IntImg2DArrayMS.Size

            return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 +
                   a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17
        }

    GLSL.shouldCompile [Effect.ofFunction fs]


[<Test>]
let ``Load``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {

            let a0 = uniform.Img1D.Load(1)                      + uniform.Img1D.[1]
            let a1 = uniform.Img1DArray.Load(0, 1)              + uniform.Img1DArray.[0, 1]
            let a2 = uniform.Img2D.Load V2i.Zero                + uniform.Img2D.[V2i.Zero]
            let a3 = uniform.Img2DArray.Load(V2i.Zero, 1)       + uniform.Img2DArray.[V2i.Zero, 1]
            let a4 = uniform.Img3D.Load(V3i.Zero)               + uniform.Img3D.[V3i.Zero]
            let a5 = uniform.ImgCube.Load(V2i.Zero, 3)          + uniform.ImgCube.[V2i.Zero, 3]
            let a6 = uniform.ImgCubeArray.Load(V2i.Zero, 4)     + uniform.ImgCubeArray.[V2i.Zero, 4]
            let a7 = uniform.Img2DMS.Load(V2i.Zero, 0)          + uniform.Img2DMS.[V2i.Zero, 0]
            let a8 = uniform.Img2DArrayMS.Load(V2i.Zero, 1, 0)  + uniform.Img2DArrayMS.[V2i.Zero, 1, 0]

            let a9  = uniform.IntImg1D.Load(1)                     + uniform.IntImg1D.[1]
            let a10 = uniform.IntImg1DArray.Load(0, 1)             + uniform.IntImg1DArray.[0, 1]
            let a11 = uniform.IntImg2D.Load(V2i.Zero)              + uniform.IntImg2D.[V2i.Zero]
            let a12 = uniform.IntImg2DArray.Load(V2i.Zero, 1)      + uniform.IntImg2DArray.[V2i.Zero, 1]
            let a13 = uniform.IntImg3D.Load(V3i.Zero)              + uniform.IntImg3D.[V3i.Zero]
            let a14 = uniform.IntImgCube.Load(V2i.Zero, 3)         + uniform.IntImgCube.[V2i.Zero, 3]
            let a15 = uniform.IntImgCubeArray.Load(V2i.Zero, 4)    + uniform.IntImgCubeArray.[V2i.Zero, 4]
            let a16 = uniform.IntImg2DMS.Load(V2i.Zero, 0)         + uniform.IntImg2DMS.[V2i.Zero, 0]
            let a17 = uniform.IntImg2DArrayMS.Load(V2i.Zero, 1, 0) + uniform.IntImg2DArrayMS.[V2i.Zero, 1, 0]

            return {| Color = a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
                      Colori = a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 |}
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

            return V3d.Zero
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction fs] ["IntFoo1DArray", 3]
