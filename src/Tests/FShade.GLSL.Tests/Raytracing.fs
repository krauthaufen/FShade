module Raytracing

open System
open Aardvark.Base
open FShade
open NUnit.Framework
open FShade.Tests

type UniformScope with
    member x.SomeUniform : V3d = uniform?SomeUniform
    member x.OutputBuffer : Image2d<Formats.rgba32f> = uniform?OutputBuffer
    member x.Flags : RayFlags = uniform?Flags
    member x.SomeAttribute : V3d[] = uniform?StorageBuffer?SomeAttribute

type Payload =
    {
        color : V3d
        depth : int
    }

let scene =
    scene { accelerationStructure uniform?RaytracingScene }

[<ReflectedDefinition>]
let trace (input : RayHitInput<Payload>) =
    if input.payload.depth < 16 then
        let payload = { color = V3d.Zero; depth = input.payload.depth + 1}
        let result = scene.TraceRay(input.ray.origin, input.ray.direction, payload, flags = uniform.Flags)
        result.color
    else
        V3d.Zero

[<ReflectedDefinition>]
let whatever() =
    V4d(uniform.SomeUniform, 1.0)


[<Test>]
let ``Reflected functions``() =
    Setup.Run()

    let raygenShader (input : RayGenerationInput) =
        raygen {
            uniform.OutputBuffer.[input.work.id.XY] <- whatever()
        }

    let chitShader (input : RayHitInput<Payload>) =
        closestHit {
            return { color = trace input; depth = 0 }
        }

    let chitShaderShadow (input : RayHitInput<Payload>) =
        closestHit {
            let shadowed = scene.TraceRay<bool>(V3d.Zero, V3d.XAxis)
            if shadowed then
                return { color = V3d.Zero; depth = 0 }
            else
                return { color = trace input; depth = 0 }
        }

    let effect =
         let hitgroupMain =
             hitgroup {
                closestHit ("1", chitShader)
                closestHit ("2", chitShader)
            }

         let hitgroupShadow =
             hitgroup { closestHit chitShaderShadow }

         raytracingEffect {
             raygen raygenShader
             hitgroup ("Main", hitgroupMain)
             hitgroup ("Shadow", hitgroupShadow)
         }

    let regex = "helper\(vec3 WorldRayDirection, vec3 WorldRayOrigin\)"
    GLSL.shouldCompileRaytracingAndContainRegex effect [regex]

[<Test>]
let ``Simple uniform access in reflected function``() =
    Setup.Run()

    let raygenShader =
        raygen {
            ()
        }

    let chitShader =
        closestHit {
            return whatever()
        }

    let effect =
         let hitgroup1 =
             hitgroup { closestHit chitShader }

         let hitgroup2 =
             hitgroup { closestHit chitShader }

         raytracingEffect {
             raygen raygenShader
             hitgroup ("1", hitgroup1)
             hitgroup ("2", hitgroup2)
         }

    GLSL.shouldCompileRaytracing effect

[<ReflectedDefinition>]
let getValueWithPrimitiveId (input : RayHitInput<'T, 'V>) =
    input.geometry.primitiveId

[<ReflectedDefinition>]
let getSomeAttribute (ai : int) =
    uniform.SomeAttribute.[ai]

let chitWithPrimitiveId (input : RayHitInput<Payload>) =
    closestHit {
        let ai = getValueWithPrimitiveId input

        let attr = getSomeAttribute ai

        return { color = attr; depth = 0 }
    }

[<Test>]
let ``Helper with PrimitiveId``() =
    Setup.Run()

    let raygenShader =
        raygen {
            ()
        }

    let effect =
         let hitgroup1 =
             hitgroup { closestHit chitWithPrimitiveId }

         raytracingEffect {
             raygen raygenShader
             hitgroup ("1", hitgroup1)
         }

    GLSL.shouldCompileRaytracing effect