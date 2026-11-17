module MemoryQualifiers

open System.Text.RegularExpressions
open Aardvark.Base
open FShade
open FShade.GLSL
open FShade.Tests
open NUnit.Framework

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
    }

type UniformScope with
    [<Coherent>]
    member x.CoherentImage : UIntImage2d<Formats.r32ui> = x?CoherentImage

    [<Coherent; Restrict>]
    member x.CoherentBuffer : int[] = x?StorageBuffer?CoherentBuffer

    [<Volatile>]
    member x.VolatileBuffer : int[] = x?StorageBuffer?VolatileBuffer

    // MergeA and MergeB deliberately declare the SAME uniform "Merged" (the uniform identity is the
    // string after ?, not the member name); their qualifiers must be unioned.
    [<Coherent>]
    member x.MergeA : int[] = x?StorageBuffer?Merged

    [<Restrict>]
    member x.MergeB : int[] = x?StorageBuffer?Merged

    member x.PlainBuffer : int[] = x?StorageBuffer?PlainBuffer

    [<Coherent>]
    member x.CoherentScalar : float32 = x?CoherentScalar

[<Test>]
let ``Coherent image``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let old = uniform.CoherentImage.[V2i v.pos.XY]
            uniform.CoherentImage.[V2i v.pos.XY] <- old + V4ui.IIII
            return v.c
        }

    let expected = [ "coherent uniform uimage2D CoherentImage;" ]
    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] expected
    GLSL.shouldCompileAndContainRegex' glslVulkan [ Effect.ofFunction fs ] expected

[<Test>]
let ``Memory qualifiers on storage buffers``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            uniform.CoherentBuffer.[0] <- uniform.CoherentBuffer.[1]
            uniform.VolatileBuffer.[0] <- 1
            let a = uniform.PlainBuffer.[0]
            return V4f(float32 a) + v.c
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] [
        "coherent restrict buffer CoherentBufferBuffer"
        "volatile writeonly buffer VolatileBufferBuffer"
        "^readonly buffer PlainBufferBuffer"
    ]

[<Test>]
let ``Qualifiers are merged across shaders``() =
    Setup.Run()

    // same storage buffer declared with different qualifiers in two shaders
    let fs1 (v : Vertex) =
        fragment {
            uniform.MergeA.[0] <- 1
            return v.c
        }

    let fs2 (v : Vertex) =
        fragment {
            let x = uniform.MergeB.[1]
            return v.c * float32 x
        }

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction fs1; Effect.ofFunction fs2 ] [
        "coherent restrict buffer MergedBuffer", 1
    ]

let private computeWithQualifiers ([<Coherent>] data : int[]) ([<Volatile; Restrict>] img : IntImage2d<Formats.r32i>) =
    compute {
        let id = getGlobalId()
        data.[id.X] <- img.[id.XY].X
        memoryBarrierBuffer()
        memoryBarrierImage()
        barrier()
    }

[<Test>]
let ``Compute parameters``() =
    Setup.Run()

    let shader = ComputeShader.ofFunction (V3i 128) computeWithQualifiers
    Assert.That(shader.csBuffers.["data"].memory, Is.EqualTo MemoryQualifier.Coherent)
    Assert.That(shader.csImages.["cs_img"].memory, Is.EqualTo (MemoryQualifier.Volatile ||| MemoryQualifier.Restrict))

    GLSL.shouldCompileComputeAndContainRegex shader [
        "coherent writeonly buffer dataBuffer"
        "volatile restrict uniform iimage2D cs_img;"
        Regex.Escape "memoryBarrierBuffer();"
        Regex.Escape "memoryBarrierImage();"
    ]

[<Test>]
let ``Memory barriers``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let id = getGlobalId()
            data.[id.X] <- id.Y
            memoryBarrier()
            memoryBarrierShared()
            groupMemoryBarrier()
        }

    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i 128) cs) [
        Regex.Escape "memoryBarrier();"
        Regex.Escape "memoryBarrierShared();"
        Regex.Escape "groupMemoryBarrier();"
    ]

[<Test>]
let ``Interface reports qualifiers``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            uniform.CoherentBuffer.[0] <- 1
            uniform.CoherentImage.[V2i.Zero] <- V4ui.Zero
            return v.c
        }

    let (glsl : GLSLShader), _ = GLSL.compile [ Effect.ofFunction fs ]
    Assert.That(glsl.iface.storageBuffers.["CoherentBuffer"].ssbMemory, Is.EqualTo (MemoryQualifier.Coherent ||| MemoryQualifier.Restrict))
    Assert.That(glsl.iface.images.["CoherentImage"].imageMemory, Is.EqualTo MemoryQualifier.Coherent)

    let roundtrip = glsl |> GLSLShader.pickle |> GLSLShader.unpickle
    Assert.That(roundtrip, Is.EqualTo glsl)

[<Test>]
let ``Qualifier on plain uniform fails``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return v.c * uniform.CoherentScalar
        }

    let exn = Assert.Throws(fun () -> GLSL.compile [ Effect.ofFunction fs ] |> ignore)
    Assert.That(exn.Message, Does.Contain "only allowed on images and storage buffers")
