module GLSLSerializer

open FShade
open FShade.GLSL
open FShade.Tests
open FsUnit
open NUnit.Framework
open Aardvark.Base
open System.IO

let private testRoundtrip (glsl: GLSLShader) =
    use ms = new MemoryStream()
    glsl |> GLSLShader.serialize ms
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    let result = GLSLShader.deserialize ms

    result |> should equal glsl

let private testRoundtripEffect (effects: Effect list) =
    let glsl, _ = GLSL.compile effects
    testRoundtrip glsl

let private testRoundtripRaytracing (effect: RaytracingEffect) =
    let glsl, _ = GLSL.compileRaytracing effect
    testRoundtrip glsl

module Simple =
    type Vertex =
        {
            [<Position>] pos : V4f
            [<Semantic("TexCoord")>] tc : V2f
        }

    let mySampler =
        sampler2d {
            texture uniform?Texture
        }

    let constantArray = [| V4f.Zero |]

    [<ReflectedDefinition>]
    let getOffset() : V3f =
        uniform?Offset

    [<ReflectedDefinition>]
    let sample (index : int) (tc : V2f) =
        mySampler.Sample(tc) + constantArray.[index]

[<Test>]
let ``[GLSL Serializer] simple roundtrip``() =
    Setup.Run()

    let vs (v : Simple.Vertex) =
        vertex {
            return v.pos + V4f(Simple.getOffset(), 1.0f)
        }

    let fs (v : Simple.Vertex) =
        fragment {
            return Simple.sample uniform?Bla v.tc
        }

    testRoundtripEffect [
        Effect.ofFunction vs
        Effect.ofFunction fs
    ]

module RTX =

    type UniformScope with
        member x.OutputBuffer : Image2d<Formats.rgba32f> = uniform?OutputBuffer

    type Payload =
        {
            foo : float32
            flag : bool
        }

    type CallableData =
        {
            flag : bool
            value : float32
        }

    let scene =
        scene { accelerationStructure uniform?RaytracingScene }

    let callableShader (input : RayCallableInput<CallableData>) =
        callable {
            if input.data.flag then
                return { value = 0.5f; flag = false }
            else
                return { value = 1.0f; flag = false }
        }

    let intersectionShader (input : RayIntersectionInput) =
        intersection {
            Intersection.Report(0.5f, true, RayHitKind.FrontFacingTriangle) |> ignore
        }

    let missShader (input : RayMissInput<Payload>) =
        miss {
            if input.payload.flag then
                return { foo = 1.0f; flag = false }
            else
                return { foo = 0.0f; flag = false }
        }

    let anyHitShader (input : RayHitInput<Payload>) =
        anyHit {
            if input.ray.direction = V3f.Zero then
                ignoreIntersection()
            elif input.hit.attribute.X = 0.0f then
                terminateRay()
        }

    let closestHitShader (input : RayHitInput<Payload>) =
        closestHit {
            let whatever = scene.TraceRay(V3f.Zero, V3f.XAxis)
            return { foo = input.hit.attribute.X; flag = whatever }
        }

    let raygenShader (input : RayGenerationInput) =
        let secondaryRayFlags = RayFlags.Opaque ||| RayFlags.SkipClosestHitShader
        let rayType = Sym.ofString "rayMain"
        let missType = Sym.ofString "missMain"

        raygen {
            let whatever = Callable.Execute({ flag = true; value = 0.0f })
            let result = scene.TraceRay<Payload>(scene.TraceRay<V3f>(V3f.Zero, V3f.ZAxis, V3f.One), V3f.YAxis, miss = missType, ray = rayType, flags = secondaryRayFlags)
            uniform.OutputBuffer.[input.work.id.XY] <- V4f(result.foo + whatever.value)
        }

[<Test>]
let ``[GLSL Serializer] raytracing roundtrip``() =
    Setup.Run()

    let effect =
        let defaultHitGroup =
            hitgroup {
                anyHit RTX.anyHitShader
                closestHit RTX.closestHitShader
                intersection RTX.intersectionShader
            }

        raytracingEffect {
            raygen RTX.raygenShader
            hitgroup "Main" defaultHitGroup
            miss RTX.missShader
            callable RTX.callableShader
        }

    testRoundtripRaytracing effect