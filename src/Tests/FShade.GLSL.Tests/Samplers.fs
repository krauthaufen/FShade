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

    let sam1DShadow        = sampler1dShadow { texture uniform?DiffuseTexture }
    let sam1DArrayShadow   = sampler1dArrayShadow { texture uniform?DiffuseTexture }
    let sam2DShadow        = sampler2dShadow { texture uniform?DiffuseTexture }
    let sam2DArrayShadow   = sampler2dArrayShadow { texture uniform?DiffuseTexture }
    let samCubeShadow      = samplerCubeShadow { texture uniform?DiffuseTexture }
    let samCubeArrayShadow = samplerCubeArrayShadow { texture uniform?DiffuseTexture }

    let intSam2D        = intSampler2d { texture uniform?IntTexture }
    let intSam2DMS      = intSampler2dMS { texture uniform?DiffuseTexture }
    let intSam2DArrayMS = intSampler2dArrayMS { texture uniform?DiffuseTexture }

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
            let a = sam2D.[V2i.IO]
            let b = sam2D.[V2i.OI, 1]
            let c = sam2D.Read(V2i.II, 7)

            return a + b + c
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
            let a = sam2D.Gather V2d.Zero + sam2D.Gather(V2d.Zero, 1)
            let b = sam2DArray.Gather(V2d.Zero, 5) + sam2DArray.Gather(V2d.Zero, 5, 1)
            let c = samCube.Gather V3d.Zero + samCube.Gather(V3d.Zero, 1)
            let d = samCubeArray.Gather(V3d.Zero, 5) + samCubeArray.Gather(V3d.Zero, 5, 1)

            let e = sam2DShadow.Gather(V2d.Zero, 0.5)
            let f = sam2DArrayShadow.Gather(V2d.Zero, 5, 0.5)
            let g = samCubeShadow.Gather(V3d.Zero, 0.5)
            let h = samCubeArrayShadow.Gather(V3d.Zero, 5, 0.5)

            return a + b + c + d + e + f + g + h
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Gather with Offset``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = sam2D.GatherOffset(V2d.Zero, V2i.Zero) + sam2D.GatherOffset(V2d.Zero, V2i.Zero, 1)
            let b = sam2DArray.GatherOffset(V2d.Zero, 5, V2i.Zero) + sam2DArray.GatherOffset(V2d.Zero, 5, V2i.Zero, 1)
            let c = sam2DShadow.GatherOffset(V2d.Zero, 0.5, V2i.Zero)
            let d = sam2DArrayShadow.GatherOffset(V2d.Zero, 5, 0.5, V2i.Zero)

            return a + b + c + d
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Size``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = V3i sam1D.Size
            let b = V3i(sam1DArray.Size, 0)
            let c = V3i(sam2D.Size, 0)
            let d = sam2DArray.Size
            let e = sam3D.GetSize 5
            let f = V3i(samCube.GetSize 10, 0)
            let g = samCubeArray.Size
            let h = V3i(sam2DMS.Size, 0)
            let i = sam2DArrayMS.Size

            let j = V3i sam1DShadow.Size
            let k = V3i(sam1DArrayShadow.Size, 0)
            let l = V3i(sam2DShadow.Size, 0)
            let m = sam2DArrayShadow.Size
            let n = V3i(samCubeShadow.Size, 0)
            let o = samCubeArrayShadow.Size

            return V4i(
                a + b + c + d + e + f + g + h + i +
                j + k + l + m + n + o,
                1
            )
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
            let a = sam1D.QueryLod 0.0
            let b = sam1DArray.QueryLod 0.0
            let c = sam2D.QueryLod V2d.Zero
            let d = sam2DArray.QueryLod V2d.Zero
            let e = sam3D.QueryLod <| V3d.Zero
            let f = samCube.QueryLod V3d.Zero
            let g = samCubeArray.QueryLod V3d.Zero

            let h = sam1DShadow.QueryLod 0.0
            let i = sam1DArrayShadow.QueryLod 0.0
            let j = sam2DShadow.QueryLod V2d.Zero
            let k = sam2DArrayShadow.QueryLod V2d.Zero
            let l = samCubeShadow.QueryLod V3d.Zero
            let m = samCubeArrayShadow.QueryLod V3d.Zero

            return V4d(
                a + b + c + d + e + f + g +
                h + i + j + k + l + m, 1.0, 1.0
            )
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Grad``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = sam1D.SampleGrad(0.0, 0.0, 0.0)
            let b = sam1DArray.SampleGrad(0.0, 0, 0.0, 0.0)
            let c = sam2D.SampleGrad(V2d.Zero, V2d.Zero, V2d.Zero)
            let d = sam2DArray.SampleGrad(V2d.Zero, 0, V2d.Zero, V2d.Zero)
            let e = sam3D.SampleGrad(V3d.Zero, V3d.Zero, V3d.Zero)
            let f = samCube.SampleGrad(V3d.Zero, V3d.Zero, V3d.Zero)
            let g = samCubeArray.SampleGrad(V3d.Zero, 0, V3d.Zero, V3d.Zero)

            let h = V4d(sam1DShadow.SampleGrad(0.0, 0.5, 0.0, 0.0), 0.0, 0.0, 0.0)
            let i = V4d(sam1DArrayShadow.SampleGrad(0.0, 0, 0.5, 0.0, 0.0), 0.0, 0.0, 0.0)
            let j = V4d(sam2DShadow.SampleGrad(V2d.Zero, 0.5, V2d.Zero, V2d.Zero), 0.0, 0.0, 0.0)
            let k = V4d(sam2DArrayShadow.SampleGrad(V2d.Zero, 0, 0.5, V2d.Zero, V2d.Zero), 0.0, 0.0, 0.0)
            let l = V4d(samCubeShadow.SampleGrad(V3d.Zero, 0.5, V3d.Zero, V3d.Zero), 0.0, 0.0, 0.0)
            let m = V4d(samCubeArrayShadow.SampleGrad(V3d.Zero, 0, 0.5, V3d.Zero, V3d.Zero), 0.0, 0.0, 0.0)

            return a + b + c + d + e + f + g +
                   h + i + j + k + l + m
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texel Fetch``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = sam1D.Read(0, 7)
            let b = sam1DArray.Read(0, 1, 7) + sam1DArray.[0, 1, 7]
            let c = sam2D.Read(V2i.Zero, 7)
            let d = sam2DArray.Read(V2i.Zero, 1, 7) + sam2DArray.[V2i.Zero, 1]
            let e = sam2DMS.Read(V2i.Zero, 7)
            let f = sam2DArrayMS.Read(V2i.Zero, 1, 7)
            let g = sam3D.Read(V3i.Zero, 7)

            return a + b + c + d + e + f + g
        }

    GLSL.shouldCompile [Effect.ofFunction fs]


[<Test>]
let ``Texture LoD``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = sam1D.SampleLevel(0.0, 0.0)
            let b = sam1DArray.SampleLevel(0.0, 0, 0.0)
            let c = sam2D.SampleLevel(V2d.Zero, 0.0)
            let d = sam2DArray.SampleLevel(V2d.Zero, 0, 0.0)
            let e = sam3D.SampleLevel(V3d.Zero, 0.0)
            let f = samCube.SampleLevel(V3d.Zero, 0.0)
            let g = samCubeArray.SampleLevel(V3d.Zero, 0, 0.0)

            let h = V4d(sam1DShadow.SampleLevel(0.0, 0.5, 0.0), 0.0, 0.0, 0.0)
            let i = V4d(sam1DArrayShadow.SampleLevel(0.0, 0, 0.5, 0.0), 0.0, 0.0, 0.0)
            let j = V4d(sam2DShadow.SampleLevel(V2d.Zero, 0.5, 0.0), 0.0, 0.0, 0.0)

            return a + b + c + d + e + f + g +
                   h + i + j
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture LoD with Offset``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = sam1D.SampleLevelOffset(0.0, 4.0, 3)
            let b = sam1DArray.SampleLevelOffset(0.0, 0, 3.0, 4)
            let c = sam2D.SampleLevelOffset(V2d.Zero, 2.0, V2i.Zero)
            let d = sam2DArray.SampleLevelOffset(V2d.Zero, 2, 3.0, V2i.Zero)
            let e = sam3D.SampleLevelOffset(V3d.Zero, 3.0, V3i.Zero)

            let f = V4d(sam1DShadow.SampleLevelOffset(0.0, 0.5, 3.0, -2), 0.0, 0.0, 0.0)
            let g = V4d(sam1DArrayShadow.SampleLevelOffset(0.0, 3, 0.5, 4.0, -1), 0.0, 0.0, 0.0)
            let h = V4d(sam2DShadow.SampleLevelOffset(V2d.Zero, 0.5, 3.0, V2i.One), 0.0, 0.0, 0.0)

            return a + b + c + d + e +
                   f + g + h
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Proj``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = sam1D.SampleProj(V2d.Zero)
            let b = sam2D.SampleProj(V3d.Zero, 1.0)
            let c = sam3D.SampleProj(V4d.Zero, 1.0)

            let d = V4d(sam1DShadow.SampleProj(V2d(0.1, 1.0), 0.6, 1.0), 0.0, 0.0, 0.0)
            let e = V4d(sam2DShadow.SampleProj(V3d(0.1, 0.2, 1.0), 0.4), 0.0, 0.0, 0.0)

            return a + b + c + d + e
        }

    GLSL.shouldCompile [Effect.ofFunction fs]


[<Test>]
let ``Texture Offset``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = sam1D.SampleOffset(0.0, 7)
            let b = sam1DArray.SampleOffset(0.0, 0, 1, 0.0)
            let c = sam2D.SampleOffset(V2d.Zero, V2i.Zero, 0.0)
            let d = sam2DArray.SampleOffset(V2d.Zero, 0, V2i.Zero)
            let e = sam3D.SampleOffset(V3d.Zero, V3i.Zero, 1.0)

            let f = V4d(sam1DShadow.SampleOffset(0.0, 0.5, 1, 0.0), 0.0, 0.0, 0.0)
            let g = V4d(sam1DArrayShadow.SampleOffset(0.0, 0, 0.5, 3, 0.0), 0.0, 0.0, 0.0)
            let h = V4d(sam2DShadow.SampleOffset(V2d.Zero, 0.5, V2i.Zero, 1.0), 0.0, 0.0, 0.0)
            let i = V4d(sam2DArrayShadow.SampleOffset(V2d.Zero, 0, 0.5, V2i.Zero), 0.0, 0.0, 0.0)

            return a + b + c + d + e +
                   f + g + h + i
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let a = sam1D.Sample 0.0
            let b = sam1DArray.Sample(0.0, 1, 2.0)
            let c = sam2D.Sample V2d.Zero
            let d = sam2DArray.Sample(V2d.Zero, 1, 2.0)
            let e = sam3D.Sample V3d.Zero
            let f = samCube.Sample(V3d.Zero, 3.0)
            let g = samCubeArray.Sample(V3d.Zero, 1, -1.0)

            let h = V4d(sam1DShadow.Sample(0.0, 0.5, 1.0), 0.0, 0.0, 0.0)
            let i = V4d(sam1DArrayShadow.Sample(0.0, 2, 0.5), 0.0, 0.0, 0.0)
            let j = V4d(sam2DShadow.Sample(V2d.Zero, 0.5), 0.0, 0.0, 0.0)
            let k = V4d(sam2DArrayShadow.Sample(V2d.Zero, 3, 0.5), 0.0, 0.0, 0.0)
            let l = V4d(samCubeShadow.Sample(V3d.Zero, 0.4), 0.0, 0.0, 0.0)
            let m = V4d(samCubeArrayShadow.Sample(V3d.Zero, 1, 0.2), 0.0, 0.0, 0.0)

            return a + b + c + d + e + f + g +
                   h + i + j + k + l + m
        }

    GLSL.shouldCompile [Effect.ofFunction fs]