module Raytracing

open System
open Aardvark.Base
open FShade
open NUnit.Framework
open FShade.Tests

type UniformScope with
    member x.SomeUniform : V3f = uniform?SomeUniform
    member x.OutputBuffer : Image2d<Formats.rgba32f> = uniform?OutputBuffer
    member x.Flags : RayFlags = uniform?Flags
    member x.SomeAttribute : V3f[] = uniform?StorageBuffer?SomeAttribute

type Payload =
    {
        color : V3f
        depth : int
    }

type BigPayload =
    {
        color : V3f
        depth : int
        origin : V3f
        direction : V3f
        flag : bool
    }

let scene =
    scene { accelerationStructure uniform?RaytracingScene }

[<ReflectedDefinition>]
let trace (input : RayHitInput<Payload>) =
    if input.payload.depth < 16 then
        let payload = { color = V3f.Zero; depth = input.payload.depth + 1}
        let result = scene.TraceRay(input.ray.origin, input.ray.direction, payload, flags = uniform.Flags)
        result.color
    else
        V3f.Zero

[<ReflectedDefinition>]
let whatever() =
    V4f(uniform.SomeUniform, 1.0f)


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
            let shadowed = scene.TraceRay<bool>(V3f.Zero, V3f.XAxis, miss = "ShadowMiss", flags = RayFlags.SkipClosestHitShader)
            if shadowed then
                return { color = V3f.Zero; depth = 0 }
            else
                return { color = trace input; depth = 0 }
        }

    let effect =
         let hitgroupMain =
             hitgroup {
                closestHit "1" chitShader
                closestHit "2" chitShader
            }

         let hitgroupShadow =
             hitgroup { closestHit chitShaderShadow }

         raytracingEffect {
             raygen raygenShader
             hitgroup "Main" hitgroupMain
             hitgroup "Shadow" hitgroupShadow
         }

    GLSL.shouldCompileRaytracingAndContainRegex effect [ "_trace_"; "_whatever_"]

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
             hitgroup "1" hitgroup1
             hitgroup "2" hitgroup2
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
             hitgroup "1" hitgroup1
         }

    GLSL.shouldCompileRaytracing effect

[<Test>]
let ``Callable shader``() =
    Setup.Run()

    let someName = Sym.ofString "someName"

    let raygenShader =
        raygen {
            let data = { unchanged<BigPayload> with flag = true }
            Callable.Execute(data, "big")     |> ignore
            Callable.Execute(0, "someName")   |> ignore
            Callable.Execute<int>("someName") |> ignore
            Callable.Execute(0, someName)     |> ignore
            Callable.Execute<int>(someName)   |> ignore
            ()
        }

    let callableShader (input : RayCallableInput<int>)=
        callable {
            return input.data
        }

    let callableShaderBig (input : RayCallableInput<BigPayload>)=
        callable {
            let result =
                if input.data.flag then
                    V3f.One
                else
                    V3f.Zero
            return { unchanged<BigPayload> with color = result }
        }

    let effect =
         raytracingEffect {
             raygen raygenShader
             callable someName callableShader
             callable "big" callableShaderBig
         }

    GLSL.shouldCompileRaytracing effect

type RayHitKind with
    static member SomeWeirdStuff = unbox<RayHitKind> 1234

[<Test>]
let ``Intersection shader with custom hit kind``() =
    Setup.Run()

    let raygenShader =
        raygen {
            ()
        }

    let intersectionShader (input : RayIntersectionInput)=
        intersection {
            Intersection.Report(0.5f, RayHitKind.SomeWeirdStuff) |> ignore
        }

    let hitgroupMain =
        hitgroup { intersection intersectionShader }

    let effect =
        raytracingEffect {
            raygen raygenShader
            hitgroup "Main" hitgroupMain
        }

    GLSL.shouldCompileRaytracingAndContainRegex effect [ "1234" ]

[<Flags>]
type MyEnum =
    | None = 0u
    | A = 1u
    | B = 2u
    | C = 4u

[<Test>]
let ``Ray type based on enum``() =
    Setup.Run()

    let raygenShader (input : MyEnum) =
        raygen {
            scene.TraceRay<int>(V3f.Zero, V3f.ZAxis, ray = if float ((input &&& MyEnum.A) <<< 2) <> 123.0 then "Yay" else "Nay") |> ignore
            scene.TraceRay<int>(V3f.Zero, V3f.ZAxis, ray = if uint8 ((input ^^^ MyEnum.A) >>> 2) <> 123uy then "Nay" else "Yay") |> ignore
        }

    let effect =
        raytracingEffect {
            raygen (raygenShader (MyEnum.A ||| MyEnum.B))
        }

    GLSL.shouldCompileRaytracing effect

[<Test>]
let ``Ray type based on SRTP``() =
    Setup.Run()

    let raygenShader =
        raygen {
            scene.TraceRay<int>(V3f.Zero, V3f.ZAxis, ray = if atanh 0.0 = 0.0 then "Yay" else "Nay") |> ignore
        }

    let effect =
        raytracingEffect {
            raygen raygenShader
        }

    GLSL.shouldCompileRaytracing effect

[<Test>]
let ``ignoreIntersection / terminateRay``() =
    Setup.Run()

    let raygenShader =
        raygen {
            ()
        }

    let anyhitShader (input: RayHitInput<int>) =
        anyHit {
            ignoreIntersection()
            terminateRay()
            return int input.hit.attribute.X
        }

    let effect =
         let hitgroup1 =
             hitgroup { anyHit anyhitShader }

         raytracingEffect {
             raygen raygenShader
             hitgroup "1" hitgroup1
         }

    GLSL.shouldCompileRaytracingAndContainRegex effect [ "ignoreIntersectionEXT"; "terminateRayEXT" ]

[<Test>]
let ``Hit triangle vertex positions``() =
    Setup.Run()

    let raygenShader =
        raygen {
            ()
        }

    let chitShader (input: RayHitInput<V3f>) =
        closestHit {
            return input.hit.positions.[0]
        }

    let anyhitShader (input: RayHitInput<V3f>) =
        anyHit {
            return input.hit.positions.[2]
        }

    let effect =
         let hitgroup1 =
             hitgroup { closestHit chitShader; anyHit anyhitShader }

         raytracingEffect {
             raygen raygenShader
             hitgroup "1" hitgroup1
         }

    GLSL.shouldCompileRaytracing effect

[<Test>]
let ``Object / World transforms``() =
    Setup.Run()

    let raygenShader =
        raygen {
            ()
        }

    let chitShader (input: RayHitInput<V4f>) =
        closestHit {
            return input.objectSpace.objectToWorld.R0 + input.objectSpace.worldToObject.R0 + input.payload
        }

    let anyhitShader (input: RayHitInput<V3f>) =
        anyHit {
            return input.objectSpace.objectToWorld.C0 + input.objectSpace.worldToObject.C0 + input.payload
        }

    let intersectionShader (input : RayIntersectionInput) =
        intersection {
            let _ = input.objectSpace.objectToWorld
            let _ = input.objectSpace.worldToObject
            Intersection.Report(0.5f, RayHitKind.SomeWeirdStuff) |> ignore
        }

    let effect =
         let hitgroup1 =
             hitgroup { closestHit chitShader; anyHit anyhitShader; intersection intersectionShader }

         raytracingEffect {
             raygen raygenShader
             hitgroup "1" hitgroup1
         }

    GLSL.shouldCompileRaytracing effect

[<ReflectedDefinition>]
let invokeCallable (id: CallableId) : int =
    Callable.Execute(id)

[<Test>]
let ``CallableId based on uniform``() =
    Setup.Run()

    let id2 = CallableId "Foo2"

    let raygenShader =
        raygen {
            let mutable id = CallableId.None
            id <- if uniform?BlaBlub then CallableId "Foo" else CallableId "Haah"
            invokeCallable id |> ignore
            ()
        }

    let callableShader (input : RayCallableInput<CallableId>)=
        callable {
            return id2
        }

    let effect =
         raytracingEffect {
             raygen raygenShader
             callable "Foo" callableShader
         }

    let hasCallable name = effect.ShaderBindingTableLayout.CallableIndices.ContainsKey name
    Assert.IsFalse <| hasCallable Symbol.Empty
    Assert.IsTrue <| hasCallable (Sym.ofString "Foo")
    Assert.IsTrue <| hasCallable (Sym.ofString "Haah")

    GLSL.shouldCompileRaytracing effect

[<ReflectedDefinition>]
let traceHitObject<'T> (origin: V3f) (direction: V3f) (payload: 'T) (ho: HitObject) =
    ho.TraceRay<'T>(scene, origin, direction) |> ignore
    Thread.Reorder ho
    let hint: int = uniform?Blub
    Thread.Reorder(32u, 1u)
    Thread.Reorder(hint, 1)
    Thread.Reorder(ho, 32u, 1u)
    Thread.Reorder(ho, 32, 1)
    ho.ExecuteShader<'T>(payload)

[<ReflectedDefinition>]
let myHitObject() =
    HitObject()

[<Test>]
let ``Shader execution reordering``() =
    Setup.Run()

    let raygenShader =
        raygen {
            HitObject() |> traceHitObject V3f.Zero V3f.ZAxis 42 |> ignore
            myHitObject() |> traceHitObject V3f.Zero V3f.ZAxis 42 |> ignore
        }

    let effect =
         raytracingEffect {
             raygen raygenShader
         }

    GLSL.shouldCompileRaytracing effect

[<Test>]
let ``Shader execution reordering intrinsics``() =
    Setup.Run()

    let raygenShader =
        raygen {
            let ho = HitObject()
            let _ = ho.IsEmpty
            let _ = ho.IsMiss
            let _ = ho.IsHit
            let _ = ho.RayMinT
            let _ = ho.RayMaxT
            let _ = ho.RayOrigin
            let _ = ho.RayDirection
            let _ = ho.RayObjectOrigin
            let _ = ho.RayObjectDirection
            let _ = ho.ObjectToWorld.C3
            let _ = ho.WorldToObject.R2
            let _ = ho.InstanceCustomIndex
            let _ = ho.InstanceId
            let _ = ho.GeometryIndex
            let _ = ho.PrimitiveIndex
            let _ = ho.HitKind
            let _ = ho.GetAttribute<V4f>()
            ho.RecordEmpty()
            ho.RecordHit<float32>(scene, 1, 2, 3, V3f.Zero, V3f.ZAxis, "OtherRay")
            ho.RecordMiss(V3f.Zero, V3f.ZAxis)
            ho |> traceHitObject V3f.Zero V3f.ZAxis 42 |> ignore
        }

    let effect =
         raytracingEffect {
             raygen raygenShader
         }

    GLSL.shouldCompileRaytracingAndContainRegex effect [
        "hitObjectTraceRayNV"
        "hitObjectRecordHitNV"
        "hitObjectRecordMissNV"
        "hitObjectRecordEmptyNV"
        "hitObjectExecuteShaderNV"
        "hitObjectGetAttributesNV"
        "hitObjectIsEmptyNV"
        "hitObjectIsMissNV"
        "hitObjectIsHitNV"
        "hitObjectGetRayTMinNV"
        "hitObjectGetRayTMaxNV"
        "hitObjectGetWorldRayOriginNV"
        "hitObjectGetWorldRayDirectionNV"
        "hitObjectGetObjectRayOriginNV"
        "hitObjectGetObjectRayDirectionNV"
        "hitObjectGetObjectToWorldNV"
        "hitObjectGetWorldToObjectNV"
        "hitObjectGetInstanceCustomIndexNV"
        "hitObjectGetInstanceIdNV"
        "hitObjectGetGeometryIndexNV"
        "hitObjectGetPrimitiveIndexNV"
        "hitObjectGetHitKindNV"
        "reorderThreadNV"
    ]

[<Test>]
let ``HitObject inlining``() =
    Setup.Run()

    let raygenShader =
        raygen {
            let ho = HitObject()
            ho.TraceRay<V3f>(scene, V3f.Zero, V3f.ZAxis) |> ignore
        }

    let effect =
         raytracingEffect {
             raygen raygenShader
         }

    GLSL.shouldCompileRaytracingAndContainRegex effect ["hitObjectNV ho;"]

[<ReflectedDefinition; KeepCall>]
let traceAndIgnore() =
    scene.TraceRay<V3f>(V3f.Zero, V3f.ZAxis) |> ignore
    V3f.One

[<Test>]
let ``Eliminate unused payload read in utility function``() =
    Setup.Run()

    let raygenShader =
        raygen {
            traceAndIgnore() |> ignore
        }

    let effect =
         raytracingEffect {
             raygen raygenShader
         }

    GLSL.shouldCompileRaytracingAndContainRegex effect [ @"traceRayEXT.+;\s*return vec3" ]


[<Test>]
let ``Write to payload fields``() =
    Setup.Run()

    let raygenShader =
        raygen {
            ()
        }

    let chitShader (input: RayHitInput<BigPayload>) =
        closestHit {
            return {
                color = V3f.ZAxis
                depth = 1
                origin = uniform?Ori
                direction = uniform?Dir
                flag = false
            }
        }

    let mainHitgroup =
        hitgroup {
            closestHit chitShader
        }

    let effect =
         raytracingEffect {
             raygen raygenShader
             hitgroup "Main" mainHitgroup
         }

    GLSL.shouldCompileRaytracingAndContainRegex effect [ @"rayPayloadIn\.color =" ]

[<ReflectedDefinition>]
let traceBig (input : RayHitInput<BigPayload>) =
    if input.payload.depth < 16 then
        let payload = { unchanged<BigPayload> with depth = input.payload.depth + 1 }
        let result = scene.TraceRay(input.ray.origin, input.ray.direction, payload, flags = uniform.Flags)
        result.color
    else
        V3f.Zero

[<Test>]
let ``Write to payload fields partial``() =
    Setup.Run()

    let raygenShader =
        raygen {
            ()
        }

    let chitShader (input: RayHitInput<BigPayload>) =
        closestHit {
            let inner = traceBig input
            let newPayload = { unchanged<BigPayload> with color = inner + input.payload.color * 0.5f }
            let newPayload2 = newPayload
            return newPayload2
        }

    let mainHitgroup =
        hitgroup {
            closestHit chitShader
        }

    let effect =
         raytracingEffect {
             raygen raygenShader
             hitgroup "Main" mainHitgroup
         }

    GLSL.shouldCompileRaytracingAndContainRegex effect [
        @"rayPayload0.depth =.+;\s+traceRayEXT"
        @"rayPayloadIn.color = \(inner \+ \(rayPayloadIn.color \* 0\.5\)\);\s*\}"
    ]

[<Test>]
let ``Overloads and optional arguments``() =
    Setup.Run()

    let raygenShader =
        raygen {
            scene.TraceRay<V3f>(V3f.Zero, V3f.ZAxis) |> ignore
            scene.TraceRay<V3f>(V3f.Zero, V3f.ZAxis, miss = MissId "Miss") |> ignore
            scene.TraceRay<V3f>(V3f.Zero, V3f.ZAxis, ray = RayId "Ray") |> ignore
            scene.TraceRay<V3f>(V3f.Zero, V3f.ZAxis, miss = "Miss") |> ignore
            scene.TraceRay<V3f>(V3f.Zero, V3f.ZAxis, ray = "Ray") |> ignore
            scene.TraceRay<V3f>(V3f.Zero, V3f.ZAxis, miss = Sym.ofString "Miss") |> ignore
            scene.TraceRay<V3f>(V3f.Zero, V3f.ZAxis, ray = Sym.ofString "Ray") |> ignore

            let ho = HitObject()
            ho.TraceRay<V3f>(scene, V3f.Zero, V3f.ZAxis) |> ignore
            ho.TraceRay<V3f>(scene, V3f.Zero, V3f.ZAxis, miss = MissId "Miss") |> ignore
            ho.TraceRay<V3f>(scene, V3f.Zero, V3f.ZAxis, ray = RayId "Ray") |> ignore
            ho.TraceRay<V3f>(scene, V3f.Zero, V3f.ZAxis, miss = "Miss") |> ignore
            ho.TraceRay<V3f>(scene, V3f.Zero, V3f.ZAxis, ray = "Ray") |> ignore
            ho.TraceRay<V3f>(scene, V3f.Zero, V3f.ZAxis, miss = Sym.ofString "Miss") |> ignore
            ho.TraceRay<V3f>(scene, V3f.Zero, V3f.ZAxis, ray = Sym.ofString "Ray") |> ignore
        }

    let effect =
         raytracingEffect {
             raygen raygenShader
         }

    GLSL.shouldCompileRaytracingAndContainRegex effect ["hitObjectNV ho;"]