module Samplers

open Aardvark.Base
open FShade
open NUnit.Framework
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
    }

[<AutoOpen>]
module private Samplers =

    let sam1D        = sampler1d { texture uniform?DiffuseTexture; borderColor C4f.Black; borderColor V4f.Zero }
    let sam1DArray   = sampler1dArray { texture uniform?DiffuseTexture }
    let sam2D        = sampler2d { texture uniform?DiffuseTexture }
    let sam2DArray   = sampler2dArray { texture uniform?DiffuseTexture }
    let sam2DMS      = sampler2dMS { texture uniform?DiffuseTexture }
    let sam2DArrayMS = sampler2dArrayMS { texture uniform?DiffuseTexture }
    let sam3D        = sampler3d { texture uniform?DiffuseTexture }
    let samCube      = samplerCube { texture uniform?DiffuseTexture }
    let samCubeArray = samplerCubeArray { texture uniform?DiffuseTexture }

    let intSam1D        = intSampler1d { texture uniform?DiffuseTexture; borderColor V4i.Zero }
    let intSam1DArray   = intSampler1dArray { texture uniform?DiffuseTexture }
    let intSam2D        = intSampler2d { texture uniform?DiffuseTexture }
    let intSam2DArray   = intSampler2dArray { texture uniform?DiffuseTexture }
    let intSam2DMS      = intSampler2dMS { texture uniform?DiffuseTexture }
    let intSam2DArrayMS = intSampler2dArrayMS { texture uniform?DiffuseTexture }
    let intSam3D        = intSampler3d { texture uniform?DiffuseTexture }
    let intSamCube      = intSamplerCube { texture uniform?DiffuseTexture }
    let intSamCubeArray = intSamplerCubeArray { texture uniform?DiffuseTexture }

    let uintSam1D        = uintSampler1d { texture uniform?DiffuseTexture; borderColor V4ui.Zero; borderColor C4ui.Black }
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


[<ReflectedDefinition>]
let getColor (sampler : Sampler2d) (coord : V2f) =
    sampler.Sample coord

[<Test>]
let ``Sampler Argument``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return getColor sam2D v.pos.XY
        }

    GLSL.shouldCompile [ Effect.ofFunction (frag) ]

[<Test>]
let ``Array Samplers`` () =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let lc = v.pos.XY
            let mutable sum = V4f.Zero
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
            let mutable c = V4f.Zero
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
            return V4f(value, 1, 1, 1)
        }

    GLSL.shouldCompile [ Effect.ofFunction ps; ]

[<Test>]
let ``Texture Gather``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4f.Zero
            c <- c + sam2D.Gather V2f.Zero + sam2D.Gather(V2f.Zero, 1)
            c <- c + sam2DArray.Gather(V2f.Zero, 5) + sam2DArray.Gather(V2f.Zero, 5, 1)
            c <- c + samCube.Gather V3f.Zero + samCube.Gather(V3f.Zero, 1)
            c <- c + samCubeArray.Gather(V3f.Zero, 5) + samCubeArray.Gather(V3f.Zero, 5, 1)
            c <- c + sam2DShadow.Gather(V2f.Zero, 0.5f)
            c <- c + sam2DArrayShadow.Gather(V2f.Zero, 5, 0.5f)
            c <- c + samCubeShadow.Gather(V3f.Zero, 0.5f)
            c <- c + samCubeArrayShadow.Gather(V3f.Zero, 5, 0.5f)

            let mutable ci = V4i.Zero
            ci <- ci + intSam2D.Gather V2f.Zero + intSam2D.Gather(V2f.Zero, 1)
            ci <- ci + intSam2DArray.Gather(V2f.Zero, 5) + intSam2DArray.Gather(V2f.Zero, 5, 1)
            ci <- ci + intSamCube.Gather V3f.Zero + intSamCube.Gather(V3f.Zero, 1)
            ci <- ci + intSamCubeArray.Gather(V3f.Zero, 5) + intSamCubeArray.Gather(V3f.Zero, 5, 1)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam2D.Gather V2f.Zero + uintSam2D.Gather(V2f.Zero, 1)
            cui <- cui + uintSam2DArray.Gather(V2f.Zero, 5) + uintSam2DArray.Gather(V2f.Zero, 5, 1)
            cui <- cui + uintSamCube.Gather V3f.Zero + uintSamCube.Gather(V3f.Zero, 1)
            cui <- cui + uintSamCubeArray.Gather(V3f.Zero, 5) + uintSamCubeArray.Gather(V3f.Zero, 5, 1)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Gather with Offset``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4f.Zero
            c <- c + sam2D.GatherOffset(V2f.Zero, V2i.Zero)         + sam2D.GatherOffset(V2f.Zero, V2i.Zero, 1)
            c <- c + sam2DArray.GatherOffset(V2f.Zero, 5, V2i.Zero) + sam2DArray.GatherOffset(V2f.Zero, 5, V2i.Zero, 1)
            c <- c + sam2DShadow.GatherOffset(V2f.Zero, 0.5f, V2i.Zero)
            c <- c + sam2DArrayShadow.GatherOffset(V2f.Zero, 5, 0.5f, V2i.Zero)

            let mutable ci = V4i.Zero
            ci <- ci + intSam2D.GatherOffset(V2f.Zero, V2i.Zero)         + intSam2D.GatherOffset(V2f.Zero, V2i.Zero, 1)
            ci <- ci + intSam2DArray.GatherOffset(V2f.Zero, 5, V2i.Zero) + intSam2DArray.GatherOffset(V2f.Zero, 5, V2i.Zero, 1)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam2D.GatherOffset(V2f.Zero, V2i.Zero)         + uintSam2D.GatherOffset(V2f.Zero, V2i.Zero, 1)
            cui <- cui + uintSam2DArray.GatherOffset(V2f.Zero, 5, V2i.Zero) + uintSam2DArray.GatherOffset(V2f.Zero, 5, V2i.Zero, 1)

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
            let mutable r = V2f.Zero
            r <- r + sam1D.QueryLod 0.0f
            r <- r + sam1DArray.QueryLod 0.0f
            r <- r + sam2D.QueryLod V2f.Zero
            r <- r + sam2DArray.QueryLod V2f.Zero
            r <- r + sam3D.QueryLod V3f.Zero
            r <- r + samCube.QueryLod V3f.Zero
            r <- r + samCubeArray.QueryLod V3f.Zero

            r <- r + intSam1D.QueryLod 0.0f
            r <- r + intSam1DArray.QueryLod 0.0f
            r <- r + intSam2D.QueryLod V2f.Zero
            r <- r + intSam2DArray.QueryLod V2f.Zero
            r <- r + intSam3D.QueryLod V3f.Zero
            r <- r + intSamCube.QueryLod V3f.Zero
            r <- r + intSamCubeArray.QueryLod V3f.Zero

            r <- r + uintSam1D.QueryLod 0.0f
            r <- r + uintSam1DArray.QueryLod 0.0f
            r <- r + uintSam2D.QueryLod V2f.Zero
            r <- r + uintSam2DArray.QueryLod V2f.Zero
            r <- r + uintSam3D.QueryLod V3f.Zero
            r <- r + uintSamCube.QueryLod V3f.Zero
            r <- r + uintSamCubeArray.QueryLod V3f.Zero

            r <- r + sam1DShadow.QueryLod 0.0f
            r <- r + sam1DArrayShadow.QueryLod 0.0f
            r <- r + sam2DShadow.QueryLod V2f.Zero
            r <- r + sam2DArrayShadow.QueryLod V2f.Zero
            r <- r + samCubeShadow.QueryLod V3f.Zero
            r <- r + samCubeArrayShadow.QueryLod V3f.Zero

            return V4f(r, 1.0f, 1.0f)
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Grad``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4f.Zero
            c <- c + sam1D.SampleGrad(0.0f, 0.0f, 0.0f)
            c <- c + sam1DArray.SampleGrad(0.0f, 0, 0.0f, 0.0f)
            c <- c + sam2D.SampleGrad(V2f.Zero, V2f.Zero, V2f.Zero)
            c <- c + sam2DArray.SampleGrad(V2f.Zero, 0, V2f.Zero, V2f.Zero)
            c <- c + sam3D.SampleGrad(V3f.Zero, V3f.Zero, V3f.Zero)
            c <- c + samCube.SampleGrad(V3f.Zero, V3f.Zero, V3f.Zero)
            c <- c + samCubeArray.SampleGrad(V3f.Zero, 0, V3f.Zero, V3f.Zero)

            c <- c + V4f(sam1DShadow.SampleGrad(0.0f, 0.5f, 0.0f, 0.0f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam1DArrayShadow.SampleGrad(0.0f, 0, 0.5f, 0.0f, 0.0f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam2DShadow.SampleGrad(V2f.Zero, 0.5f, V2f.Zero, V2f.Zero), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam2DArrayShadow.SampleGrad(V2f.Zero, 0, 0.5f, V2f.Zero, V2f.Zero), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(samCubeShadow.SampleGrad(V3f.Zero, 0.5f, V3f.Zero, V3f.Zero), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(samCubeArrayShadow.SampleGrad(V3f.Zero, 0, 0.5f, V3f.Zero, V3f.Zero), 0.0f, 0.0f, 0.0f)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.SampleGrad(0.0f, 0.0f, 0.0f)
            ci <- ci + intSam1DArray.SampleGrad(0.0f, 0, 0.0f, 0.0f)
            ci <- ci + intSam2D.SampleGrad(V2f.Zero, V2f.Zero, V2f.Zero)
            ci <- ci + intSam2DArray.SampleGrad(V2f.Zero, 0, V2f.Zero, V2f.Zero)
            ci <- ci + intSam3D.SampleGrad(V3f.Zero, V3f.Zero, V3f.Zero)
            ci <- ci + intSamCube.SampleGrad(V3f.Zero, V3f.Zero, V3f.Zero)
            ci <- ci + intSamCubeArray.SampleGrad(V3f.Zero, 0, V3f.Zero, V3f.Zero)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.SampleGrad(0.0f, 0.0f, 0.0f)
            cui <- cui + uintSam1DArray.SampleGrad(0.0f, 0, 0.0f, 0.0f)
            cui <- cui + uintSam2D.SampleGrad(V2f.Zero, V2f.Zero, V2f.Zero)
            cui <- cui + uintSam2DArray.SampleGrad(V2f.Zero, 0, V2f.Zero, V2f.Zero)
            cui <- cui + uintSam3D.SampleGrad(V3f.Zero, V3f.Zero, V3f.Zero)
            cui <- cui + uintSamCube.SampleGrad(V3f.Zero, V3f.Zero, V3f.Zero)
            cui <- cui + uintSamCubeArray.SampleGrad(V3f.Zero, 0, V3f.Zero, V3f.Zero)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texel Fetch``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4f.Zero
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
            let mutable c = V4f.Zero
            c <- c + sam1D.SampleLevel(0.0f, 0.0f)
            c <- c + sam1DArray.SampleLevel(0.0f, 0, 0.0f)
            c <- c + sam2D.SampleLevel(V2f.Zero, 0.0f)
            c <- c + sam2DArray.SampleLevel(V2f.Zero, 0, 0.0f)
            c <- c + sam3D.SampleLevel(V3f.Zero, 0.0f)
            c <- c + samCube.SampleLevel(V3f.Zero, 0.0f)
            c <- c + samCubeArray.SampleLevel(V3f.Zero, 0, 0.0f)

            c <- c + V4f(sam1DShadow.SampleLevel(0.0f, 0.5f, 0.0f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam1DArrayShadow.SampleLevel(0.0f, 0, 0.5f, 0.0f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam2DShadow.SampleLevel(V2f.Zero, 0.5f, 0.0f), 0.0f, 0.0f, 0.0f)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.SampleLevel(0.0f, 0.0f)
            ci <- ci + intSam1DArray.SampleLevel(0.0f, 0, 0.0f)
            ci <- ci + intSam2D.SampleLevel(V2f.Zero, 0.0f)
            ci <- ci + intSam2DArray.SampleLevel(V2f.Zero, 0, 0.0f)
            ci <- ci + intSam3D.SampleLevel(V3f.Zero, 0.0f)
            ci <- ci + intSamCube.SampleLevel(V3f.Zero, 0.0f)
            ci <- ci + intSamCubeArray.SampleLevel(V3f.Zero, 0, 0.0f)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.SampleLevel(0.0f, 0.0f)
            cui <- cui + uintSam1DArray.SampleLevel(0.0f, 0, 0.0f)
            cui <- cui + uintSam2D.SampleLevel(V2f.Zero, 0.0f)
            cui <- cui + uintSam2DArray.SampleLevel(V2f.Zero, 0, 0.0f)
            cui <- cui + uintSam3D.SampleLevel(V3f.Zero, 0.0f)
            cui <- cui + uintSamCube.SampleLevel(V3f.Zero, 0.0f)
            cui <- cui + uintSamCubeArray.SampleLevel(V3f.Zero, 0, 0.0f)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture LoD with Offset``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4f.Zero
            c <- c + sam1D.SampleLevelOffset(0.0f, 4.0f, 3)
            c <- c + sam1DArray.SampleLevelOffset(0.0f, 0, 3.0f, 4)
            c <- c + sam2D.SampleLevelOffset(V2f.Zero, 2.0f, V2i.Zero)
            c <- c + sam2DArray.SampleLevelOffset(V2f.Zero, 2, 3.0f, V2i.Zero)
            c <- c + sam3D.SampleLevelOffset(V3f.Zero, 3.0f, V3i.Zero)

            c <- c + V4f(sam1DShadow.SampleLevelOffset(0.0f, 0.5f, 3.0f, -2), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam1DArrayShadow.SampleLevelOffset(0.0f, 3, 0.5f, 4.0f, -1), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam2DShadow.SampleLevelOffset(V2f.Zero, 0.5f, 3.0f, V2i.One), 0.0f, 0.0f, 0.0f)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.SampleLevelOffset(0.0f, 4.0f, 3)
            ci <- ci + intSam1DArray.SampleLevelOffset(0.0f, 0, 3.0f, 4)
            ci <- ci + intSam2D.SampleLevelOffset(V2f.Zero, 2.0f, V2i.Zero)
            ci <- ci + intSam2DArray.SampleLevelOffset(V2f.Zero, 2, 3.0f, V2i.Zero)
            ci <- ci + intSam3D.SampleLevelOffset(V3f.Zero, 3.0f, V3i.Zero)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.SampleLevelOffset(0.0f, 4.0f, 3)
            cui <- cui + uintSam1DArray.SampleLevelOffset(0.0f, 0, 3.0f, 4)
            cui <- cui + uintSam2D.SampleLevelOffset(V2f.Zero, 2.0f, V2i.Zero)
            cui <- cui + uintSam2DArray.SampleLevelOffset(V2f.Zero, 2, 3.0f, V2i.Zero)
            cui <- cui + uintSam3D.SampleLevelOffset(V3f.Zero, 3.0f, V3i.Zero)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture Proj``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4f.Zero
            c <- c + sam1D.SampleProj(V2f.Zero)
            c <- c + sam2D.SampleProj(V3f.Zero, 1.0f)
            c <- c + sam3D.SampleProj(V4f.Zero, 1.0f)

            c <- c + V4f(sam1DShadow.SampleProj(V2f(0.1f, 1.0f), 0.6f, 1.0f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam2DShadow.SampleProj(V3f(0.1f, 0.2f, 1.0f), 0.4f), 0.0f, 0.0f, 0.0f)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.SampleProj(V2f.Zero)
            ci <- ci + intSam2D.SampleProj(V3f.Zero, 1.0f)
            ci <- ci + intSam3D.SampleProj(V4f.Zero, 1.0f)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.SampleProj(V2f.Zero)
            cui <- cui + uintSam2D.SampleProj(V3f.Zero, 1.0f)
            cui <- cui + uintSam3D.SampleProj(V4f.Zero, 1.0f)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]


[<Test>]
let ``Texture Offset``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4f.Zero
            c <- c + sam1D.SampleOffset(0.0f, 7)
            c <- c + sam1DArray.SampleOffset(0.0f, 0, 1, 0.0f)
            c <- c + sam2D.SampleOffset(V2f.Zero, V2i.Zero, 0.0f)
            c <- c + sam2DArray.SampleOffset(V2f.Zero, 0, V2i.Zero)
            c <- c + sam3D.SampleOffset(V3f.Zero, V3i.Zero, 1.0f)

            c <- c + V4f(sam1DShadow.SampleOffset(0.0f, 0.5f, 1, 0.0f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam1DArrayShadow.SampleOffset(0.0f, 0, 0.5f, 3, 0.0f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam2DShadow.SampleOffset(V2f.Zero, 0.5f, V2i.Zero, 1.0f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam2DArrayShadow.SampleOffset(V2f.Zero, 0, 0.5f, V2i.Zero), 0.0f, 0.0f, 0.0f)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.SampleOffset(0.0f, 7)
            ci <- ci + intSam1DArray.SampleOffset(0.0f, 0, 1, 0.0f)
            ci <- ci + intSam2D.SampleOffset(V2f.Zero, V2i.Zero, 0.0f)
            ci <- ci + intSam2DArray.SampleOffset(V2f.Zero, 0, V2i.Zero)
            ci <- ci + intSam3D.SampleOffset(V3f.Zero, V3i.Zero, 1.0f)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.SampleOffset(0.0f, 7)
            cui <- cui + uintSam1DArray.SampleOffset(0.0f, 0, 1, 0.0f)
            cui <- cui + uintSam2D.SampleOffset(V2f.Zero, V2i.Zero, 0.0f)
            cui <- cui + uintSam2DArray.SampleOffset(V2f.Zero, 0, V2i.Zero)
            cui <- cui + uintSam3D.SampleOffset(V3f.Zero, V3i.Zero, 1.0f)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Texture``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mutable c = V4f.Zero
            c <- c + sam1D.Sample 0.0f
            c <- c + sam1DArray.Sample(0.0f, 1, 2.0f)
            c <- c + sam2D.Sample V2f.Zero
            c <- c + sam2DArray.Sample(V2f.Zero, 1, 2.0f)
            c <- c + sam3D.Sample V3f.Zero
            c <- c + samCube.Sample(V3f.Zero, 3.0f)
            c <- c + samCubeArray.Sample(V3f.Zero, 1, -1.0f)

            c <- c + V4f(sam1DShadow.Sample(0.0f, 0.5f, 1.0f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam1DArrayShadow.Sample(0.0f, 2, 0.5f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam2DShadow.Sample(V2f.Zero, 0.5f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(sam2DArrayShadow.Sample(V2f.Zero, 3, 0.5f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(samCubeShadow.Sample(V3f.Zero, 0.4f), 0.0f, 0.0f, 0.0f)
            c <- c + V4f(samCubeArrayShadow.Sample(V3f.Zero, 1, 0.2f), 0.0f, 0.0f, 0.0f)

            let mutable ci = V4i.Zero
            ci <- ci + intSam1D.Sample 0.0f
            ci <- ci + intSam1DArray.Sample(0.0f, 1, 2.0f)
            ci <- ci + intSam2D.Sample V2f.Zero
            ci <- ci + intSam2DArray.Sample(V2f.Zero, 1, 2.0f)
            ci <- ci + intSam3D.Sample V3f.Zero
            ci <- ci + intSamCube.Sample(V3f.Zero, 3.0f)
            ci <- ci + intSamCubeArray.Sample(V3f.Zero, 1, -1.0f)

            let mutable cui = V4ui.Zero
            cui <- cui + uintSam1D.Sample 0.0f
            cui <- cui + uintSam1DArray.Sample(0.0f, 1, 2.0f)
            cui <- cui + uintSam2D.Sample V2f.Zero
            cui <- cui + uintSam2DArray.Sample(V2f.Zero, 1, 2.0f)
            cui <- cui + uintSam3D.Sample V3f.Zero
            cui <- cui + uintSamCube.Sample(V3f.Zero, 3.0f)
            cui <- cui + uintSamCubeArray.Sample(V3f.Zero, 1, -1.0f)

            return {| Color = c; Colori = ci; Colorui = cui |}
        }

    GLSL.shouldCompile [Effect.ofFunction fs]