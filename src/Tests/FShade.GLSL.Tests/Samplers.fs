module Samplers

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
module private Samplers =

    let sam1D        = sampler1d { texture uniform?DiffuseTexture }
    let sam1DArray   = sampler1dArray { texture uniform?DiffuseTexture }
    let sam2D        = sampler2d { texture uniform?DiffuseTexture }
    let sam2DArray   = sampler2dArray { texture uniform?DiffuseTexture }
    let sam2DMS      = sampler2dMS { texture uniform?DiffuseTexture }
    let sam2DArrayMS = sampler2dArrayMS { texture uniform?DiffuseTexture }
    let sam3D        = sampler3d { texture uniform?DiffuseTexture }
    let samCube      = samplerCube { texture uniform?DiffuseTexture }
    let samCubeArray = samplerCubeArray { texture uniform?DiffuseTexture }

    let intSam1D        = intSampler1d { texture uniform?DiffuseTexture }
    let intSam1DArray   = intSampler1dArray { texture uniform?DiffuseTexture }
    let intSam2D        = intSampler2d { texture uniform?DiffuseTexture }
    let intSam2DArray   = intSampler2dArray { texture uniform?DiffuseTexture }
    let intSam2DMS      = intSampler2dMS { texture uniform?DiffuseTexture }
    let intSam2DArrayMS = intSampler2dArrayMS { texture uniform?DiffuseTexture }
    let intSam3D        = intSampler3d { texture uniform?DiffuseTexture }
    let intSamCube      = intSamplerCube { texture uniform?DiffuseTexture }
    let intSamCubeArray = intSamplerCubeArray { texture uniform?DiffuseTexture }

    let uintSam1D        = uintSampler1d { texture uniform?DiffuseTexture }
    let uintSam1DArray   = uintSampler1dArray { texture uniform?DiffuseTexture }
    let uintSam2D        = uintSampler2d { texture uniform?DiffuseTexture }
    let uintSam2DArray   = uintSampler2dArray { texture uniform?DiffuseTexture }
    let uintSam2DMS      = uintSampler2dMS { texture uniform?DiffuseTexture }
    let uintSam2DArrayMS = uintSampler2dArrayMS { texture uniform?DiffuseTexture }
    let uintSam3D        = uintSampler3d { texture uniform?DiffuseTexture }
    let uintSamCube      = uintSamplerCube { texture uniform?DiffuseTexture }
    let uintSamCubeArray = uintSamplerCubeArray { texture uniform?DiffuseTexture }

    let sam1DShadow        = sampler1dShadow { texture uniform?DiffuseTexture }
    let sam1DArrayShadow   = sampler1dArrayShadow { texture uniform?DiffuseTexture }
    let sam2DShadow        = sampler2dShadow { texture uniform?DiffuseTexture }
    let sam2DArrayShadow   = sampler2dArrayShadow { texture uniform?DiffuseTexture }
    let samCubeShadow      = samplerCubeShadow { texture uniform?DiffuseTexture }
    let samCubeArrayShadow = samplerCubeArrayShadow { texture uniform?DiffuseTexture }

[<Test>]
let ``Array Samplers`` () =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let lc = v.pos.XY
            let mutable sum = V4d.Zero
            for i in 0..uniform?TextureCount-1 do
                let layer = sam2DArray.Read(V2i(int lc.X, int lc.Y), i, 0)
                sum <- sum + layer

            return sum
        }

    GLSL.shouldCompile [ Effect.ofFunction (frag) ]

[<Test>]
let ``Simple Fetch`` () =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + sam2D.[V2i.IO]
            c <- c + sam2D.[V2i.OI, 1]
            c <- c + sam2D.Read(V2i.II, 7)

            let mutable ci = V4i.Zero
            ci <- ci + intSam2D.[V2i.IO]
            ci <- ci + intSam2D.[V2i.OI, 1]
            ci <- ci + intSam2D.Read(V2i.II, 7)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam2D.[V2i.IO]
            cui <- cui + uintSam2D.[V2i.OI, 1]
            cui <- cui + uintSam2D.Read(V2i.II, 7)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [ Effect.ofFunction (frag) ]

[<Test>]
let ``IntSampler`` () =
    Setup.Run()

    let ps (v : Vertex) =
        fragment {
            let value = intSam2D.Sample(v.pos.XY).X
            return V4d(value, 1, 1, 1)
        }

    GLSL.shouldCompile [ Effect.ofFunction ps; ]

[<Test>]
let ``Texture Gather``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + sam2D.Gather V2d.Zero + sam2D.Gather(V2d.Zero, 1)
            c <- c + sam2DArray.Gather(V2d.Zero, 5) + sam2DArray.Gather(V2d.Zero, 5, 1)
            c <- c + samCube.Gather V3d.Zero + samCube.Gather(V3d.Zero, 1)
            c <- c + samCubeArray.Gather(V3d.Zero, 5) + samCubeArray.Gather(V3d.Zero, 5, 1)
            c <- c + sam2DShadow.Gather(V2d.Zero, 0.5)
            c <- c + sam2DArrayShadow.Gather(V2d.Zero, 5, 0.5)
            c <- c + samCubeShadow.Gather(V3d.Zero, 0.5)
            c <- c + samCubeArrayShadow.Gather(V3d.Zero, 5, 0.5)

            let mutable ci = V4i.Zero
            ci <- ci + intSam2D.Gather V2d.Zero + intSam2D.Gather(V2d.Zero, 1)
            ci <- ci + intSam2DArray.Gather(V2d.Zero, 5) + intSam2DArray.Gather(V2d.Zero, 5, 1)
            ci <- ci + intSamCube.Gather V3d.Zero + intSamCube.Gather(V3d.Zero, 1)
            ci <- ci + intSamCubeArray.Gather(V3d.Zero, 5) + intSamCubeArray.Gather(V3d.Zero, 5, 1)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam2D.Gather V2d.Zero + uintSam2D.Gather(V2d.Zero, 1)
            cui <- cui + uintSam2DArray.Gather(V2d.Zero, 5) + uintSam2DArray.Gather(V2d.Zero, 5, 1)
            cui <- cui + uintSamCube.Gather V3d.Zero + uintSamCube.Gather(V3d.Zero, 1)
            cui <- cui + uintSamCubeArray.Gather(V3d.Zero, 5) + uintSamCubeArray.Gather(V3d.Zero, 5, 1)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Gather with Offset``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + sam2D.GatherOffset(V2d.Zero, V2i.Zero)         + sam2D.GatherOffset(V2d.Zero, V2i.Zero, 1)
            c <- c + sam2DArray.GatherOffset(V2d.Zero, 5, V2i.Zero) + sam2DArray.GatherOffset(V2d.Zero, 5, V2i.Zero, 1)
            c <- c + sam2DShadow.GatherOffset(V2d.Zero, 0.5, V2i.Zero)
            c <- c + sam2DArrayShadow.GatherOffset(V2d.Zero, 5, 0.5, V2i.Zero)

            let mutable ci = V4i.Zero
            ci <- ci + intSam2D.GatherOffset(V2d.Zero, V2i.Zero)         + intSam2D.GatherOffset(V2d.Zero, V2i.Zero, 1)
            ci <- ci + intSam2DArray.GatherOffset(V2d.Zero, 5, V2i.Zero) + intSam2DArray.GatherOffset(V2d.Zero, 5, V2i.Zero, 1)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam2D.GatherOffset(V2d.Zero, V2i.Zero)         + uintSam2D.GatherOffset(V2d.Zero, V2i.Zero, 1)
            cui <- cui + uintSam2DArray.GatherOffset(V2d.Zero, 5, V2i.Zero) + uintSam2DArray.GatherOffset(V2d.Zero, 5, V2i.Zero, 1)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Size``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable s = V3i.Zero
            s <- s + V3i sam1D.Size
            s <- s + V3i(sam1DArray.Size, 0)
            s <- s + V3i(sam2D.Size, 0)
            s <- s + sam2DArray.Size
            s <- s + sam3D.GetSize 5
            s <- s + V3i(samCube.GetSize 10, 0)
            s <- s + samCubeArray.Size
            s <- s + V3i(sam2DMS.Size, 0)
            s <- s + sam2DArrayMS.Size

            s <- s + V3i intSam1D.Size
            s <- s + V3i(intSam1DArray.Size, 0)
            s <- s + V3i(intSam2D.Size, 0)
            s <- s + intSam2DArray.Size
            s <- s + intSam3D.GetSize 5
            s <- s + V3i(intSamCube.GetSize 10, 0)
            s <- s + intSamCubeArray.Size
            s <- s + V3i(intSam2DMS.Size, 0)
            s <- s + intSam2DArrayMS.Size

            s <- s + V3i uintSam1D.Size
            s <- s + V3i(uintSam1DArray.Size, 0)
            s <- s + V3i(uintSam2D.Size, 0)
            s <- s + uintSam2DArray.Size
            s <- s + uintSam3D.GetSize 5
            s <- s + V3i(uintSamCube.GetSize 10, 0)
            s <- s + uintSamCubeArray.Size
            s <- s + V3i(uintSam2DMS.Size, 0)
            s <- s + uintSam2DArrayMS.Size

            s <- s + V3i sam1DShadow.Size
            s <- s + V3i(sam1DArrayShadow.Size, 0)
            s <- s + V3i(sam2DShadow.Size, 0)
            s <- s + sam2DArrayShadow.Size
            s <- s + V3i(samCubeShadow.Size, 0)
            s <- s + samCubeArrayShadow.Size

            return V4i(s, 1)
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Samples``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = sam2DMS.Samples
            let _ = sam2DArrayMS.Samples
            let _ = intSam2DMS.Samples
            let _ = intSam2DArrayMS.Samples

            return V3i.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Levels``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = sam1D.MipMapLevels
            let _ = sam1DArray.MipMapLevels
            let _ = sam2D.MipMapLevels
            let _ = sam2DArray.MipMapLevels
            let _ = sam3D.MipMapLevels
            let _ = samCube.MipMapLevels
            let _ = samCubeArray.MipMapLevels

            let _ = intSam1D.MipMapLevels
            let _ = intSam1DArray.MipMapLevels
            let _ = intSam2D.MipMapLevels
            let _ = intSam2DArray.MipMapLevels
            let _ = intSam3D.MipMapLevels
            let _ = intSamCube.MipMapLevels
            let _ = intSamCubeArray.MipMapLevels

            let _ = uintSam1D.MipMapLevels
            let _ = uintSam1DArray.MipMapLevels
            let _ = uintSam2D.MipMapLevels
            let _ = uintSam2DArray.MipMapLevels
            let _ = uintSam3D.MipMapLevels
            let _ = uintSamCube.MipMapLevels
            let _ = uintSamCubeArray.MipMapLevels

            let _ = sam1DShadow.MipMapLevels
            let _ = sam1DArrayShadow.MipMapLevels
            let _ = sam2DShadow.MipMapLevels
            let _ = sam2DArrayShadow.MipMapLevels
            let _ = samCubeShadow.MipMapLevels
            let _ = samCubeArrayShadow.MipMapLevels

            return V3i.Zero
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Query LoD``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable r = V2d.Zero
            r <- r + sam1D.QueryLod 0.0
            r <- r + sam1DArray.QueryLod 0.0
            r <- r + sam2D.QueryLod V2d.Zero
            r <- r + sam2DArray.QueryLod V2d.Zero
            r <- r + sam3D.QueryLod V3d.Zero
            r <- r + samCube.QueryLod V3d.Zero
            r <- r + samCubeArray.QueryLod V3d.Zero

            r <- r + intSam1D.QueryLod 0.0
            r <- r + intSam1DArray.QueryLod 0.0
            r <- r + intSam2D.QueryLod V2d.Zero
            r <- r + intSam2DArray.QueryLod V2d.Zero
            r <- r + intSam3D.QueryLod V3d.Zero
            r <- r + intSamCube.QueryLod V3d.Zero
            r <- r + intSamCubeArray.QueryLod V3d.Zero

            r <- r + uintSam1D.QueryLod 0.0
            r <- r + uintSam1DArray.QueryLod 0.0
            r <- r + uintSam2D.QueryLod V2d.Zero
            r <- r + uintSam2DArray.QueryLod V2d.Zero
            r <- r + uintSam3D.QueryLod V3d.Zero
            r <- r + uintSamCube.QueryLod V3d.Zero
            r <- r + uintSamCubeArray.QueryLod V3d.Zero

            r <- r + sam1DShadow.QueryLod 0.0
            r <- r + sam1DArrayShadow.QueryLod 0.0
            r <- r + sam2DShadow.QueryLod V2d.Zero
            r <- r + sam2DArrayShadow.QueryLod V2d.Zero
            r <- r + samCubeShadow.QueryLod V3d.Zero
            r <- r + samCubeArrayShadow.QueryLod V3d.Zero

            return V4d(r, 1.0, 1.0)
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Grad``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + sam1D.SampleGrad(0.0, 0.0, 0.0)
            c <- c + sam1DArray.SampleGrad(0.0, 0, 0.0, 0.0)
            c <- c + sam2D.SampleGrad(V2d.Zero, V2d.Zero, V2d.Zero)
            c <- c + sam2DArray.SampleGrad(V2d.Zero, 0, V2d.Zero, V2d.Zero)
            c <- c + sam3D.SampleGrad(V3d.Zero, V3d.Zero, V3d.Zero)
            c <- c + samCube.SampleGrad(V3d.Zero, V3d.Zero, V3d.Zero)
            c <- c + samCubeArray.SampleGrad(V3d.Zero, 0, V3d.Zero, V3d.Zero)

            c <- c + V4d(sam1DShadow.SampleGrad(0.0, 0.5, 0.0, 0.0), 0.0, 0.0, 0.0)
            c <- c + V4d(sam1DArrayShadow.SampleGrad(0.0, 0, 0.5, 0.0, 0.0), 0.0, 0.0, 0.0)
            c <- c + V4d(sam2DShadow.SampleGrad(V2d.Zero, 0.5, V2d.Zero, V2d.Zero), 0.0, 0.0, 0.0)
            c <- c + V4d(sam2DArrayShadow.SampleGrad(V2d.Zero, 0, 0.5, V2d.Zero, V2d.Zero), 0.0, 0.0, 0.0)
            c <- c + V4d(samCubeShadow.SampleGrad(V3d.Zero, 0.5, V3d.Zero, V3d.Zero), 0.0, 0.0, 0.0)
            c <- c + V4d(samCubeArrayShadow.SampleGrad(V3d.Zero, 0, 0.5, V3d.Zero, V3d.Zero), 0.0, 0.0, 0.0)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.SampleGrad(0.0, 0.0, 0.0)
            ci <- ci + intSam1DArray.SampleGrad(0.0, 0, 0.0, 0.0)
            ci <- ci + intSam2D.SampleGrad(V2d.Zero, V2d.Zero, V2d.Zero)
            ci <- ci + intSam2DArray.SampleGrad(V2d.Zero, 0, V2d.Zero, V2d.Zero)
            ci <- ci + intSam3D.SampleGrad(V3d.Zero, V3d.Zero, V3d.Zero)
            ci <- ci + intSamCube.SampleGrad(V3d.Zero, V3d.Zero, V3d.Zero)
            ci <- ci + intSamCubeArray.SampleGrad(V3d.Zero, 0, V3d.Zero, V3d.Zero)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.SampleGrad(0.0, 0.0, 0.0)
            cui <- cui + uintSam1DArray.SampleGrad(0.0, 0, 0.0, 0.0)
            cui <- cui + uintSam2D.SampleGrad(V2d.Zero, V2d.Zero, V2d.Zero)
            cui <- cui + uintSam2DArray.SampleGrad(V2d.Zero, 0, V2d.Zero, V2d.Zero)
            cui <- cui + uintSam3D.SampleGrad(V3d.Zero, V3d.Zero, V3d.Zero)
            cui <- cui + uintSamCube.SampleGrad(V3d.Zero, V3d.Zero, V3d.Zero)
            cui <- cui + uintSamCubeArray.SampleGrad(V3d.Zero, 0, V3d.Zero, V3d.Zero)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texel Fetch``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + sam1D.Read(0, 7)
            c <- c + sam1DArray.Read(0, 1, 7)        + sam1DArray.[0, 1, 7]
            c <- c + sam2D.Read(V2i.Zero, 7)
            c <- c + sam2DArray.Read(V2i.Zero, 1, 7) + sam2DArray.[V2i.Zero, 1]
            c <- c + sam2DMS.Read(V2i.Zero, 7)
            c <- c + sam2DArrayMS.Read(V2i.Zero, 1, 7)
            c <- c + sam3D.Read(V3i.Zero, 7)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.Read(0, 7)
            ci <- ci + intSam1DArray.Read(0, 1, 7)        + intSam1DArray.[0, 1, 7]
            ci <- ci + intSam2D.Read(V2i.Zero, 7)
            ci <- ci + intSam2DArray.Read(V2i.Zero, 1, 7) + intSam2DArray.[V2i.Zero, 1]
            ci <- ci + intSam2DMS.Read(V2i.Zero, 7)
            ci <- ci + intSam2DArrayMS.Read(V2i.Zero, 1, 7)
            ci <- ci + intSam3D.Read(V3i.Zero, 7)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.Read(0, 7)
            cui <- cui + uintSam1DArray.Read(0, 1, 7)        + uintSam1DArray.[0, 1, 7]
            cui <- cui + uintSam2D.Read(V2i.Zero, 7)
            cui <- cui + uintSam2DArray.Read(V2i.Zero, 1, 7) + uintSam2DArray.[V2i.Zero, 1]
            cui <- cui + uintSam2DMS.Read(V2i.Zero, 7)
            cui <- cui + uintSam2DArrayMS.Read(V2i.Zero, 1, 7)
            cui <- cui + uintSam3D.Read(V3i.Zero, 7)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]


[<Test>]
let ``Texture LoD``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + sam1D.SampleLevel(0.0, 0.0)
            c <- c + sam1DArray.SampleLevel(0.0, 0, 0.0)
            c <- c + sam2D.SampleLevel(V2d.Zero, 0.0)
            c <- c + sam2DArray.SampleLevel(V2d.Zero, 0, 0.0)
            c <- c + sam3D.SampleLevel(V3d.Zero, 0.0)
            c <- c + samCube.SampleLevel(V3d.Zero, 0.0)
            c <- c + samCubeArray.SampleLevel(V3d.Zero, 0, 0.0)

            c <- c + V4d(sam1DShadow.SampleLevel(0.0, 0.5, 0.0), 0.0, 0.0, 0.0)
            c <- c + V4d(sam1DArrayShadow.SampleLevel(0.0, 0, 0.5, 0.0), 0.0, 0.0, 0.0)
            c <- c + V4d(sam2DShadow.SampleLevel(V2d.Zero, 0.5, 0.0), 0.0, 0.0, 0.0)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.SampleLevel(0.0, 0.0)
            ci <- ci + intSam1DArray.SampleLevel(0.0, 0, 0.0)
            ci <- ci + intSam2D.SampleLevel(V2d.Zero, 0.0)
            ci <- ci + intSam2DArray.SampleLevel(V2d.Zero, 0, 0.0)
            ci <- ci + intSam3D.SampleLevel(V3d.Zero, 0.0)
            ci <- ci + intSamCube.SampleLevel(V3d.Zero, 0.0)
            ci <- ci + intSamCubeArray.SampleLevel(V3d.Zero, 0, 0.0)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.SampleLevel(0.0, 0.0)
            cui <- cui + uintSam1DArray.SampleLevel(0.0, 0, 0.0)
            cui <- cui + uintSam2D.SampleLevel(V2d.Zero, 0.0)
            cui <- cui + uintSam2DArray.SampleLevel(V2d.Zero, 0, 0.0)
            cui <- cui + uintSam3D.SampleLevel(V3d.Zero, 0.0)
            cui <- cui + uintSamCube.SampleLevel(V3d.Zero, 0.0)
            cui <- cui + uintSamCubeArray.SampleLevel(V3d.Zero, 0, 0.0)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture LoD with Offset``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + sam1D.SampleLevelOffset(0.0, 4.0, 3)
            c <- c + sam1DArray.SampleLevelOffset(0.0, 0, 3.0, 4)
            c <- c + sam2D.SampleLevelOffset(V2d.Zero, 2.0, V2i.Zero)
            c <- c + sam2DArray.SampleLevelOffset(V2d.Zero, 2, 3.0, V2i.Zero)
            c <- c + sam3D.SampleLevelOffset(V3d.Zero, 3.0, V3i.Zero)

            c <- c + V4d(sam1DShadow.SampleLevelOffset(0.0, 0.5, 3.0, -2), 0.0, 0.0, 0.0)
            c <- c + V4d(sam1DArrayShadow.SampleLevelOffset(0.0, 3, 0.5, 4.0, -1), 0.0, 0.0, 0.0)
            c <- c + V4d(sam2DShadow.SampleLevelOffset(V2d.Zero, 0.5, 3.0, V2i.One), 0.0, 0.0, 0.0)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.SampleLevelOffset(0.0, 4.0, 3)
            ci <- ci + intSam1DArray.SampleLevelOffset(0.0, 0, 3.0, 4)
            ci <- ci + intSam2D.SampleLevelOffset(V2d.Zero, 2.0, V2i.Zero)
            ci <- ci + intSam2DArray.SampleLevelOffset(V2d.Zero, 2, 3.0, V2i.Zero)
            ci <- ci + intSam3D.SampleLevelOffset(V3d.Zero, 3.0, V3i.Zero)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.SampleLevelOffset(0.0, 4.0, 3)
            cui <- cui + uintSam1DArray.SampleLevelOffset(0.0, 0, 3.0, 4)
            cui <- cui + uintSam2D.SampleLevelOffset(V2d.Zero, 2.0, V2i.Zero)
            cui <- cui + uintSam2DArray.SampleLevelOffset(V2d.Zero, 2, 3.0, V2i.Zero)
            cui <- cui + uintSam3D.SampleLevelOffset(V3d.Zero, 3.0, V3i.Zero)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Proj``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + sam1D.SampleProj(V2d.Zero)
            c <- c + sam2D.SampleProj(V3d.Zero, 1.0)
            c <- c + sam3D.SampleProj(V4d.Zero, 1.0)

            c <- c + V4d(sam1DShadow.SampleProj(V2d(0.1, 1.0), 0.6, 1.0), 0.0, 0.0, 0.0)
            c <- c + V4d(sam2DShadow.SampleProj(V3d(0.1, 0.2, 1.0), 0.4), 0.0, 0.0, 0.0)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.SampleProj(V2d.Zero)
            ci <- ci + intSam2D.SampleProj(V3d.Zero, 1.0)
            ci <- ci + intSam3D.SampleProj(V4d.Zero, 1.0)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.SampleProj(V2d.Zero)
            cui <- cui + uintSam2D.SampleProj(V3d.Zero, 1.0)
            cui <- cui + uintSam3D.SampleProj(V4d.Zero, 1.0)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]


[<Test>]
let ``Texture Offset``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + sam1D.SampleOffset(0.0, 7)
            c <- c + sam1DArray.SampleOffset(0.0, 0, 1, 0.0)
            c <- c + sam2D.SampleOffset(V2d.Zero, V2i.Zero, 0.0)
            c <- c + sam2DArray.SampleOffset(V2d.Zero, 0, V2i.Zero)
            c <- c + sam3D.SampleOffset(V3d.Zero, V3i.Zero, 1.0)

            c <- c + V4d(sam1DShadow.SampleOffset(0.0, 0.5, 1, 0.0), 0.0, 0.0, 0.0)
            c <- c + V4d(sam1DArrayShadow.SampleOffset(0.0, 0, 0.5, 3, 0.0), 0.0, 0.0, 0.0)
            c <- c + V4d(sam2DShadow.SampleOffset(V2d.Zero, 0.5, V2i.Zero, 1.0), 0.0, 0.0, 0.0)
            c <- c + V4d(sam2DArrayShadow.SampleOffset(V2d.Zero, 0, 0.5, V2i.Zero), 0.0, 0.0, 0.0)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.SampleOffset(0.0, 7)
            ci <- ci + intSam1DArray.SampleOffset(0.0, 0, 1, 0.0)
            ci <- ci + intSam2D.SampleOffset(V2d.Zero, V2i.Zero, 0.0)
            ci <- ci + intSam2DArray.SampleOffset(V2d.Zero, 0, V2i.Zero)
            ci <- ci + intSam3D.SampleOffset(V3d.Zero, V3i.Zero, 1.0)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.SampleOffset(0.0, 7)
            cui <- cui + uintSam1DArray.SampleOffset(0.0, 0, 1, 0.0)
            cui <- cui + uintSam2D.SampleOffset(V2d.Zero, V2i.Zero, 0.0)
            cui <- cui + uintSam2DArray.SampleOffset(V2d.Zero, 0, V2i.Zero)
            cui <- cui + uintSam3D.SampleOffset(V3d.Zero, V3i.Zero, 1.0)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4d.Zero
            c <- c + sam1D.Sample 0.0
            c <- c + sam1DArray.Sample(0.0, 1, 2.0)
            c <- c + sam2D.Sample V2d.Zero
            c <- c + sam2DArray.Sample(V2d.Zero, 1, 2.0)
            c <- c + sam3D.Sample V3d.Zero
            c <- c + samCube.Sample(V3d.Zero, 3.0)
            c <- c + samCubeArray.Sample(V3d.Zero, 1, -1.0)

            c <- c + V4d(sam1DShadow.Sample(0.0, 0.5, 1.0), 0.0, 0.0, 0.0)
            c <- c + V4d(sam1DArrayShadow.Sample(0.0, 2, 0.5), 0.0, 0.0, 0.0)
            c <- c + V4d(sam2DShadow.Sample(V2d.Zero, 0.5), 0.0, 0.0, 0.0)
            c <- c + V4d(sam2DArrayShadow.Sample(V2d.Zero, 3, 0.5), 0.0, 0.0, 0.0)
            c <- c + V4d(samCubeShadow.Sample(V3d.Zero, 0.4), 0.0, 0.0, 0.0)
            c <- c + V4d(samCubeArrayShadow.Sample(V3d.Zero, 1, 0.2), 0.0, 0.0, 0.0)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.Sample 0.0
            ci <- ci + intSam1DArray.Sample(0.0, 1, 2.0)
            ci <- ci + intSam2D.Sample V2d.Zero
            ci <- ci + intSam2DArray.Sample(V2d.Zero, 1, 2.0)
            ci <- ci + intSam3D.Sample V3d.Zero
            ci <- ci + intSamCube.Sample(V3d.Zero, 3.0)
            ci <- ci + intSamCubeArray.Sample(V3d.Zero, 1, -1.0)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.Sample 0.0
            cui <- cui + uintSam1DArray.Sample(0.0, 1, 2.0)
            cui <- cui + uintSam2D.Sample V2d.Zero
            cui <- cui + uintSam2DArray.Sample(V2d.Zero, 1, 2.0)
            cui <- cui + uintSam3D.Sample V3d.Zero
            cui <- cui + uintSamCube.Sample(V3d.Zero, 3.0)
            cui <- cui + uintSamCubeArray.Sample(V3d.Zero, 1, -1.0)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]