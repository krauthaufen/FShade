# Raytracing

Vulkan ray tracing (`GL_EXT_ray_tracing`) support: six shader stages, payload/callable-data
plumbing, a shader binding table (SBT) layout derived from symbolic ids, and optional NVIDIA
shader execution reordering (SER). Compiled through the GLSL backend with `glslVulkan`; consumers
(Aardvark.Rendering.Vulkan) run glslang with SPIR-V 1.4.

## Stages and Inputs

| Builder | Input record | Returns | Notes |
|---------|--------------|---------|-------|
| `raygen { }` | `RayGenerationInput` (`work.id`, `work.size`) | nothing | writes images / buffers |
| `intersection { }` | `RayIntersectionInput` (`work`, `geometry`, `ray`, `objectSpace`) | nothing | calls `Intersection.Report(t, attribute, kind)` |
| `anyHit { }` | `RayHitInput<'Payload, 'Attribute>` | new payload | may call `ignoreIntersection()` / `terminateRay()` |
| `closestHit { }` | `RayHitInput<'Payload, 'Attribute>` | new payload | may trace rays and execute callables |
| `miss { }` | `RayMissInput<'Payload>` | new payload | may trace rays |
| `callable { }` | `RayCallableInput<'Data>` | new data | invoked by `Callable.Execute` |

Input records (`src/Libs/FShade.Core/Intrinsics.fs`): `work : WorkDimensions` (`LaunchId`, `LaunchSize`),
`geometry : GeometryInstance` (`primitiveId`, `instanceId`, `instanceCustomIndex`, `geometryIndex`),
`ray : RayParameters` (`origin`, `direction`, `minT`, `maxT`, `flags`), `objectSpace : ObjectSpace`
(`rayOrigin`, `rayDirection`, `objectToWorld : M34f`, `worldToObject : M34f`),
`hit : RayHit<'Attribute>` (`t`, `kind : RayHitKind`, `attribute`, `positions : Arr<3 N, V3f>` via
`GL_EXT_ray_tracing_position_fetch`), `payload : 'Payload`, `data : 'Data`.
`RayHitInput<'Payload>` defaults the attribute to `V2f` (triangle barycentrics); `RayHitInput` and
`RayMissInput` default the payload to `unit`.

```fsharp
type Payload = { color : V3f; depth : int }

type UniformScope with
    member x.Output : Image2d<Formats.rgba32f> = x?OutputBuffer

let mainScene = scene { accelerationStructure uniform?MainScene }

let rgen (input : RayGenerationInput) =
    raygen {
        let uv = (V2f input.work.id.XY + 0.5f) / V2f input.work.size.XY
        let dir = V3f(uv * 2.0f - 1.0f, 1.0f) |> Vec.normalize
        let result = mainScene.TraceRay<Payload>(V3f.Zero, dir, { color = V3f.Zero; depth = 0 })
        uniform.Output.[input.work.id.XY] <- V4f(result.color, 1.0f)
    }

let chit (input : RayHitInput<Payload>) =
    closestHit {
        let shadowed = mainScene.TraceRay<bool>(input.ray.origin, V3f.ZAxis, miss = "Shadow",
                                                flags = RayFlags.SkipClosestHitShader ||| RayFlags.TerminateOnFirstHit)
        return { unchanged with color = (if shadowed then V3f.Zero else V3f.One) }
    }

let missSky (input : RayMissInput<Payload>) = miss { return { unchanged with color = V3f(0.2f, 0.4f, 1.0f) } }
let missShadow () = miss { return false }
```

## Tracing and Intrinsics

| API (`src/Libs/FShade.Core/Raytracing.fs`) | GLSL | Allowed in |
|---------------------------------------------|------|------------|
| `scene.TraceRay<'P>(origin, direction, [payload], ray = RayId, miss = MissId, minT, maxT, flags, cullMask)` | `traceRayEXT` + `rayPayloadEXT` | raygen, closest hit, miss |
| `Callable.Execute<'D>([data], id = CallableId)` | `executeCallableEXT` + `callableDataEXT` | raygen, closest hit, miss, callable |
| `Intersection.Report(t, [attribute], kind)` | `reportIntersectionEXT` + `hitAttributeEXT` | intersection |
| `ignoreIntersection()`, `terminateRay()` | `ignoreIntersectionEXT`, `terminateRayEXT` | any hit |
| `unchanged<'T>` | - | `{ unchanged with field = v }` writes only `field` of the incoming payload/callable data (must be inlinable; otherwise `Failed inline 'unchanged' expressions`) |

Defaults: `minT = 0.001f`, `maxT = 10000f`, `flags = RayFlags.None`, `cullMask = 0xFF`,
`ray = RayId.Default`, `miss = MissId.Default`. `RayFlags` and `RayHitKind` are in
`src/Libs/FShade.Imperative/Types.fs`; custom hit kinds are integers (`unbox<RayHitKind> 1234`).

Payload types: records, primitives, vectors, bools. Each distinct payload type used by `TraceRay`
in a shader gets its own `rayPayloadEXT` location; each shader has at most one incoming payload
type (`Can only use one type of incoming payload`). `obj` payloads are rejected.

## Ids, Slots and the Shader Binding Table

Ray types, miss shaders and callables are addressed by name (`Symbol` or `string`) with the
struct ids `RayId`, `MissId`, `CallableId`. `RayId.Default`/`MissId.Default`/`CallableId.Default`
are the empty symbol; `*.None` means "no entry". Implicit conversion from `string`/`Symbol` exists.

`ShaderSlot` (`Types.fs:178`) identifies a shader inside an effect:
`RayGeneration`, `Miss name`, `Callable name`, `AnyHit (hitGroup, rayType)`,
`ClosestHit (hitGroup, rayType)`, `Intersection (hitGroup, rayType)`.

`RaytracingEffect.ShaderBindingTableLayout` (`ShaderBindingTableLayout.fs`) maps:
- `RayOffsets : Map<Symbol, int>` (ray type -> SBT record offset; `RayStride = RayOffsets.Count`),
- `MissIndices`, `CallableIndices`.

Before compilation `RaytracingEffect.Shaders` substitutes every `RayId`/`MissId`/`CallableId`
literal with its resolved index and patches the `sbtRecordStride` argument of `traceRayEXT`
(`RaytracingEffect.fs` `substituteStubs`). `ShadersWithStubs` are the unresolved originals (used by
the debugger). Ray/miss/callable names must be constant expressions in the shader (an `if` on a
constant is folded; `Ray type name must be constant` otherwise).

## Building an Effect

```fsharp
let hitModel = hitgroup { closestHit chit; anyHit (ahit 0.5f) }
let hitSphere = hitgroup { closestHit chitSphere; intersection (intersectSphere 0.2f) }
let hitMulti = hitgroup { closestHit "Primary" chitA; closestHit "Shadow" chitB }   // per ray type

let effect =
    raytracingEffect {
        raygen rgen                       // required, first
        miss missSky                      // MissId.Default
        miss "Shadow" missShadow          // named miss shader
        callable "Lighting" lightingCallable
        hitgroup "Model" hitModel         // name = hit group symbol used by the acceleration structure
        hitgroup "Sphere" hitSphere
    }

let module_ = RaytracingEffect.toModule effect
let glsl    = ModuleCompiler.compileGLSLVulkan module_       // one source, one entry per slot
```

Builder operations accept a shader function, an `Expr`, or a `Shader` (`RaytracingEffect.fs:196`).
Duplicate slots fail (`Cannot set multiple X shaders for ray type Y`, `Cannot set multiple hit
groups with name X`); stage mismatches fail (`Expected ClosestHit shader but got Miss shader`).

| Type / function | Notes |
|-----------------|-------|
| `RaytracingShader` (`RaytracingShader.fs`) | `Id`, `Shader`, `Stage`, `Uniforms`, `RayTypes`, `MissShaders`, `CallableShaders`, `SourceDefinition`; cached by body hash |
| `RaytracingShader.ofFunction` / `ofExpr` / `ofShader` | constructors |
| `RaytracingEffect.ofShaders : Map<ShaderSlot, RaytracingShader> -> RaytracingEffect` | cached by slot/hash set |
| `effect.Id`, `effect.Uniforms`, `effect.Shaders`, `effect.ShadersWithStubs`, `effect.ShaderBindingTableLayout` | |
| `RaytracingEffect.toModule` | all slots become entry points guarded by `#ifdef <slot.Conditional>` (e.g. `ClosestHit_Model_Primary`); compile each stage with the matching define |

The GLSL contains **all** shaders in one file; the test helper `GLSL.compileRaytracing` compiles
each slot by passing `slot.Conditional` as a preprocessor define to glslang with target
SPIR-V 1.4 (`src/Tests/FShade.GLSL.Tests/Utilities.fs`).

## Shader Execution Reordering (NVIDIA)

`src/Libs/FShade.Core/RaytracingSER.fs` wraps `GL_NV_shader_invocation_reorder`:

```fsharp
raygen {
    let hit = HitObject()                                          // hitObjectNV declaration
    hit.TraceRay<Payload>(mainScene, origin, dir, payload)          // traversal only, no chit/miss
    Thread.Reorder(hit, hint, 3)                                    // coherence hint
    let result = hit.ExecuteShader<Payload>(payload)                // run chit/miss now
    ()
}
```

`HitObject` members: `IsEmpty/IsMiss/IsHit`, ray and instance queries, `TraceRay`, `RecordHit`,
`RecordMiss`, `RecordEmpty`, `ExecuteShader`, `GetAttribute`. Rules enforced by the preprocessor:
`HitObject` values must be immutable `let`s initialized via `HitObject()`; `Thread.Reorder` only
in raygen; `RecordHit`/`RecordMiss`/`ExecuteShader` only in raygen, closest hit and miss.

## Gotchas

- `hitgroup { ... }` cannot be written inline inside `raytracingEffect { ... }` (`hitgroup "Main" (hitgroup { closestHit chit })` fails with FS3095 `'hitgroup' is not used correctly`, because the inner `hitgroup` is parsed as the builder's custom operation). Bind the hit group with `let` first, as in the example above.
- Shaders registered with the debugger must be functions (`let missShadow () = miss { ... }`), not values; see `DEBUG.md`.
- Uniform reads inside `[<ReflectedDefinition>]` helpers that trace rays are specialized per slot (`setShaderSlotForReads`), so helper code is duplicated per shader. This is expected.
- `hit.positions` requires `GL_EXT_ray_tracing_position_fetch`; guard with a constant bool argument so the read is eliminated when disabled (pattern in aardvark.rendering example `36 - Raytracing`). Utility functions are specialized for constant arguments that resolve a condition, so `[<Inline>]` is no longer required for this.
- `objectToWorld`/`worldToObject` are `M34f` since 5.7.0 (`TransformPos`/`TransformDir` work; no inverse).
- Payloads are copied defensively when read (5.7.21) because GLSL payload variables are globals; returning `input.payload` unchanged is cheap, mutating fields requires `unchanged` or a full record.
- `RayFlags` are a `[<Flags>]` enum; combine with `|||`. Mutually exclusive flag pairs are documented in `Types.fs`.
- `cullMask` is compared against the instance mask; only the low 8 bits are used.
- No raytracing support exists in the GL backend; `glsl430` will emit the `EXT` functions but drivers reject them.

## See Also

- `SHADER-DSL.md` - uniforms, samplers, images, utility functions
- `GLSL-BACKEND.md` - `glslVulkan`, descriptor sets, acceleration structure bindings
- `DEBUG.md` - hot reload of raytracing effects
- aardvark.rendering `src/Examples (netcore)/36 - Raytracing` - a complete consumer-side example

**Source Files:**
- `src/Libs/FShade.Core/Raytracing.fs` (`RayId`, `MissId`, `CallableId`, `Scene`, `Callable`, `Intersection`, intrinsics)
- `src/Libs/FShade.Core/RaytracingSER.fs` (`HitObject`, `Thread`)
- `src/Libs/FShade.Core/RaytracingShader.fs`, `RaytracingEffect.fs`, `ShaderBindingTableLayout.fs`
- `src/Libs/FShade.Core/Intrinsics.fs` (input records), `Shader.fs` (`preprocessRaytracingS`)
- `src/Tests/FShade.GLSL.Tests/Raytracing.fs`
