module FunctionInterface

// Uniforms, storage buffers, images and samplers that are only accessed inside utility functions must
// be attributed to every stage calling the function (and to no other stage) in the program interface.

open Aardvark.Base
open FSharp.Data.Adaptive
open FShade
open FShade.GLSL
open FShade.Tests
open NUnit.Framework
open FsUnit

type UniformScope with
    member x.DeadValue : float32 = x?DeadUniforms?DeadValue
    member x.VsValue : float32 = x?VsUniforms?VsValue
    member x.FsValue : float32 = x?FsUniforms?FsValue
    member x.SharedValue : float32 = x?SharedUniforms?SharedValue
    member x.NestedValue : float32 = x?NestedUniforms?NestedValue
    member x.FsData : float32[] = x?StorageBuffer?FsData
    member x.FsImage : Image2d<Formats.r32f> = x?FsImage
    member x.RgValue : float32 = x?RgUniforms?RgValue
    member x.ChValue : float32 = x?ChUniforms?ChValue
    member x.MsValue : float32 = x?MsUniforms?MsValue
    member x.RtImage : Image2d<Formats.rgba32f> = x?RtImage

let fsSampler =
    sampler2d {
        texture uniform?FsTexture
        filter Filter.MinMagLinear
        addressU WrapMode.Clamp
        addressV WrapMode.Clamp
    }

[<ReflectedDefinition>]
let readFs () = uniform.FsValue

[<ReflectedDefinition>]
let readDead () = uniform.DeadValue

[<ReflectedDefinition>]
let readShared () = uniform.SharedValue

[<ReflectedDefinition>]
let readNested () = uniform.NestedValue

[<ReflectedDefinition>]
let readNestedOuter () = readNested () + 1.0f

[<ReflectedDefinition>]
let readStorage (i : int) = uniform.FsData.[i]

[<ReflectedDefinition>]
let loadImage (c : V2i) = uniform.FsImage.[c].X

[<ReflectedDefinition>]
let sampleTexture (uv : V2f) = fsSampler.Sample(uv)

[<ReflectedDefinition>]
let readRg () = uniform.RgValue

[<ReflectedDefinition>]
let readCh () = uniform.ChValue

[<ReflectedDefinition>]
let readMs () = uniform.MsValue

type Payload =
    {
        color : V3f
    }

type Vertex =
    {
        [<Position>] pos : V4f
        [<Semantic("TexCoord")>] tc : V2f
        [<Color>] c : V4f
    }

let private compile (effects : list<Effect>) =
    let glsl, res = GLSL.compile effects
    GLSL.printResults None res glsl
    glsl.iface.shaders

let private vertDirect (v : Vertex) =
    vertex {
        return { v with pos = v.pos * uniform.VsValue }
    }

let private vertShared (v : Vertex) =
    vertex {
        return { v with pos = v.pos * readShared () }
    }

[<Test>]
let ``Uniform read in utility function is attributed to the calling stage``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c * readFs ()
        }

    let shaders = compile [ Effect.ofFunction vertDirect; Effect.ofFunction frag ]

    shaders.[ShaderSlot.Vertex].shaderUniformBuffers |> HashSet.contains "VsUniforms" |> should be True
    shaders.[ShaderSlot.Vertex].shaderUniformBuffers |> HashSet.contains "FsUniforms" |> should be False
    shaders.[ShaderSlot.Fragment].shaderUniformBuffers |> HashSet.contains "FsUniforms" |> should be True
    shaders.[ShaderSlot.Fragment].shaderUniformBuffers |> HashSet.contains "VsUniforms" |> should be False

[<Test>]
let ``Uniform read in utility function shared by two stages``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c * readShared ()
        }

    let shaders = compile [ Effect.ofFunction vertShared; Effect.ofFunction frag ]

    shaders.[ShaderSlot.Vertex].shaderUniformBuffers |> HashSet.contains "SharedUniforms" |> should be True
    shaders.[ShaderSlot.Fragment].shaderUniformBuffers |> HashSet.contains "SharedUniforms" |> should be True

[<Test>]
let ``Uniform read in nested utility function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c * readNestedOuter ()
        }

    let shaders = compile [ Effect.ofFunction vertDirect; Effect.ofFunction frag ]

    shaders.[ShaderSlot.Fragment].shaderUniformBuffers |> HashSet.contains "NestedUniforms" |> should be True
    shaders.[ShaderSlot.Vertex].shaderUniformBuffers |> HashSet.contains "NestedUniforms" |> should be False

[<Test>]
let ``Storage buffer and image read in utility function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c * (readStorage 0 + loadImage V2i.Zero)
        }

    let shaders = compile [ Effect.ofFunction vertDirect; Effect.ofFunction frag ]

    shaders.[ShaderSlot.Fragment].shaderStorageBuffers |> HashSet.contains "FsData" |> should be True
    shaders.[ShaderSlot.Fragment].shaderImages |> HashSet.contains "FsImage" |> should be True
    shaders.[ShaderSlot.Vertex].shaderStorageBuffers |> HashSet.contains "FsData" |> should be False
    shaders.[ShaderSlot.Vertex].shaderImages |> HashSet.contains "FsImage" |> should be False

[<Test>]
let ``Sampler used in utility function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c * sampleTexture v.tc
        }

    let shaders = compile [ Effect.ofFunction vertDirect; Effect.ofFunction frag ]

    shaders.[ShaderSlot.Fragment].shaderSamplers |> HashSet.contains "fsSampler" |> should be True
    shaders.[ShaderSlot.Vertex].shaderSamplers |> HashSet.contains "fsSampler" |> should be False

[<Test>]
let ``Utility function used directly and inside another function``() =
    Setup.Run()

    // readShared is called from both stages, twice in the fragment shader
    let frag (v : Vertex) =
        fragment {
            return v.c * (readShared () + readShared ())
        }

    let shaders = compile [ Effect.ofFunction vertShared; Effect.ofFunction frag ]

    shaders.[ShaderSlot.Vertex].shaderUniformBuffers |> HashSet.contains "SharedUniforms" |> should be True
    shaders.[ShaderSlot.Fragment].shaderUniformBuffers |> HashSet.contains "SharedUniforms" |> should be True

// Compute

[<Test>]
let ``Compute shader uniforms and sampler used in utility functions``() =
    Setup.Run()

    let cs (data : float32[]) =
        compute {
            data.[0] <- readFs () + readStorage 1 + loadImage V2i.Zero + (sampleTexture V2f.Zero).X
        }

    let glsl, res = GLSL.compileCompute (ComputeShader.ofFunction (V3i 64) cs)
    GLSL.printResults None res glsl
    let shader = glsl.iface.shaders.[ShaderSlot.Compute]

    shader.shaderUniformBuffers |> HashSet.contains "FsUniforms" |> should be True
    shader.shaderStorageBuffers |> HashSet.contains "FsData" |> should be True
    shader.shaderImages |> HashSet.contains "FsImage" |> should be True
    shader.shaderSamplers |> HashSet.contains "fsSampler" |> should be True

// Raytracing

let private compileRaytracing (effect : RaytracingEffect) =
    let glsl, res = GLSL.compileRaytracing effect
    GLSL.printResults (Some effect) res glsl
    glsl.iface.shaders

let private chitSlot = ShaderSlot.ClosestHit (Sym.ofString "Main", Sym.ofString "Primary")
let private missSlot = ShaderSlot.Miss (Sym.ofString "MyMiss")

[<Test>]
let ``Raytracing uniforms read in utility functions are attributed per shader``() =
    Setup.Run()

    let raygenShader (input : RayGenerationInput) =
        raygen {
            uniform.RtImage.[input.work.id.XY] <- V4f(readRg ())
        }

    let chitShader (input : RayHitInput<Payload>) =
        closestHit {
            return { color = V3f(readCh ()) }
        }

    let missShader (input : RayMissInput<Payload>) =
        miss {
            return { color = V3f(readMs ()) }
        }

    let effect =
        let mainGroup = hitgroup { closestHit "Primary" chitShader }

        raytracingEffect {
            raygen raygenShader
            miss "MyMiss" missShader
            hitgroup "Main" mainGroup
        }

    let shaders = compileRaytracing effect

    shaders.[ShaderSlot.RayGeneration].shaderUniformBuffers |> HashSet.contains "RgUniforms" |> should be True
    shaders.[ShaderSlot.RayGeneration].shaderUniformBuffers |> HashSet.contains "ChUniforms" |> should be False
    shaders.[ShaderSlot.RayGeneration].shaderUniformBuffers |> HashSet.contains "MsUniforms" |> should be False
    shaders.[chitSlot].shaderUniformBuffers |> HashSet.contains "ChUniforms" |> should be True
    shaders.[chitSlot].shaderUniformBuffers |> HashSet.contains "RgUniforms" |> should be False
    shaders.[missSlot].shaderUniformBuffers |> HashSet.contains "MsUniforms" |> should be True
    shaders.[missSlot].shaderUniformBuffers |> HashSet.contains "ChUniforms" |> should be False

[<Test>]
let ``Raytracing utility function shared by raygen and closest hit``() =
    Setup.Run()

    let raygenShader (input : RayGenerationInput) =
        raygen {
            uniform.RtImage.[input.work.id.XY] <- V4f(readShared ())
        }

    let chitShader (input : RayHitInput<Payload>) =
        closestHit {
            return { color = V3f(readShared ()) }
        }

    let missShader (input : RayMissInput<Payload>) =
        miss {
            return { color = V3f.Zero }
        }

    let effect =
        let mainGroup = hitgroup { closestHit "Primary" chitShader }

        raytracingEffect {
            raygen raygenShader
            miss "MyMiss" missShader
            hitgroup "Main" mainGroup
        }

    let shaders = compileRaytracing effect

    shaders.[ShaderSlot.RayGeneration].shaderUniformBuffers |> HashSet.contains "SharedUniforms" |> should be True
    shaders.[chitSlot].shaderUniformBuffers |> HashSet.contains "SharedUniforms" |> should be True
    shaders.[missSlot].shaderUniformBuffers |> HashSet.contains "SharedUniforms" |> should be False

[<Test>]
let ``Raytracing sampler and storage buffer used in utility functions``() =
    Setup.Run()

    let raygenShader (input : RayGenerationInput) =
        raygen {
            uniform.RtImage.[input.work.id.XY] <- V4f(readStorage 0)
        }

    let chitShader (input : RayHitInput<Payload>) =
        closestHit {
            return { color = (sampleTexture V2f.Zero).XYZ }
        }

    let missShader (input : RayMissInput<Payload>) =
        miss {
            return { color = V3f.Zero }
        }

    let effect =
        let mainGroup = hitgroup { closestHit "Primary" chitShader }

        raytracingEffect {
            raygen raygenShader
            miss "MyMiss" missShader
            hitgroup "Main" mainGroup
        }

    let shaders = compileRaytracing effect

    shaders.[ShaderSlot.RayGeneration].shaderStorageBuffers |> HashSet.contains "FsData" |> should be True
    shaders.[chitSlot].shaderStorageBuffers |> HashSet.contains "FsData" |> should be False
    shaders.[chitSlot].shaderSamplers |> HashSet.contains "fsSampler" |> should be True
    shaders.[ShaderSlot.RayGeneration].shaderSamplers |> HashSet.contains "fsSampler" |> should be False
    shaders.[missSlot].shaderSamplers |> HashSet.contains "fsSampler" |> should be False

[<Test>]
let ``Uniform read only by an eliminated function is not attributed``() =
    Setup.Run()

    // the call is dead and removed by the optimizer, so the uniform must not appear in the interface
    let frag (v : Vertex) =
        fragment {
            let unused = readDead ()
            return v.c * readFs ()
        }

    let shaders = compile [ Effect.ofFunction vertDirect; Effect.ofFunction frag ]

    shaders.[ShaderSlot.Fragment].shaderUniformBuffers |> HashSet.contains "FsUniforms" |> should be True
    shaders.[ShaderSlot.Fragment].shaderUniformBuffers |> HashSet.contains "DeadUniforms" |> should be False

// Textures and sampler states only exist as separate interface entries when the backend splits combined
// samplers; this is the configuration in which uses inside utility functions were previously lost.
let private separateSamplers =
    Backend.Create { glslVulkan.Config with separateTexturesAndSamplers = true }

[<Test>]
let ``Separate texture and sampler state used in utility function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c * sampleTexture v.tc
        }

    let glsl, res = GLSL.compile' separateSamplers [ Effect.ofFunction vertDirect; Effect.ofFunction frag ]
    GLSL.printResults None res glsl
    let shaders = glsl.iface.shaders

    shaders.[ShaderSlot.Fragment].shaderTextures |> HashSet.isEmpty |> should be False
    shaders.[ShaderSlot.Fragment].shaderSamplerStates |> HashSet.isEmpty |> should be False
    shaders.[ShaderSlot.Vertex].shaderTextures |> HashSet.isEmpty |> should be True
    shaders.[ShaderSlot.Vertex].shaderSamplerStates |> HashSet.isEmpty |> should be True
