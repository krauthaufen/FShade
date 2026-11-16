# GLSL Backend

`FShade.GLSL` turns an imperative `Module` into GLSL source plus a reflected program interface.
It is the only maintained backend; Vulkan consumers feed its output to glslang. (`FShade.SpirV`
is unused and unmaintained; do not extend it.)

## Entry Points

```fsharp
open FShade
open FShade.GLSL

let glsl : GLSLShader =
    effect
    |> Effect.toModule config            // graphics; or ComputeShader.toModule / RaytracingEffect.toModule
    |> ModuleCompiler.compileGLSL glsl430   // Backend value

glsl.code   : string                // one source file with all stages (#ifdef-guarded per stage)
glsl.iface  : GLSLProgramInterface  // bindings, locations, layouts
printfn "%s" (string glsl)          // code + interface dump
GLSLProgramInterface.print glsl.iface
```

`ModuleCompiler.compileGLSL` (`src/Libs/FShade.GLSL/Backends.fs`) runs `ModuleCompiler.compile`
(Imperative lowering), optionally `SamplerSplitter.splitTexturesAndSamplers`, then
`Assembler.assemble`. Shortcuts: `compileGLSL120`, `compileGLSL410`, `compileGLSL430`,
`compileGLSLVulkan`.

## Backend Presets and Config

`Backend.Create (config : Config)` is interned per config (`Assembler.fs:570`). Presets in
`Backends.fs`:

| Preset | Version | Bindings | Descriptor sets | Push constants | Notes |
|--------|---------|----------|-----------------|----------------|-------|
| `glsl120` | 1.20 | none | no | no | no uniform buffers, no locations, `attribute`/`varying` |
| `glsl410` | 4.10 | `PerKind` | no | no | OpenGL 4.1 (macOS) |
| `glsl430` | 4.30 | `PerKind` | no | no | OpenGL; `depthWriteMode` on (conservative depth) |
| `glslVulkan` | 4.60 | `Global` | yes, stepped | yes | enables `GL_ARB_tessellation_shader`, `GL_ARB_separate_shader_objects`, `GL_ARB_shading_language_420pack`; reverse tess winding |

Aardvark.Rendering.GL builds its own config from the driver's GLSL version (`Program.fs:117`), and
Aardvark.Rendering.Vulkan uses `{ glslVulkan.Config with availableExtensions = ... }`.

| `Config` field | Effect on output |
|----------------|------------------|
| `version : GLSLVersion` | `#version MMm` line (`GLSLVersion(4,6,0)`; optional suffix e.g. `es`) |
| `enabledExtensions : Set<string>` | always emitted as `#extension X : enable` |
| `availableExtensions : Map<string, bool>` | `false` suppresses an extension the assembler would otherwise require (e.g. `GL_EXT_debug_printf` -> printf becomes a comment); unlisted = available |
| `createUniformBuffers` | `layout(...) uniform Name { ... }` blocks per scope vs. loose `uniform T x;` |
| `pushConstants` | scope `PushConstant` becomes `layout(push_constant) uniform PushConstant { ... }` |
| `bindingMode` | `None`: no `binding=`; `PerKind`: separate counters for UBO/SSBO/sampler/image/accel; `Global`: one counter (Vulkan) |
| `createDescriptorSets` | emit `set = N`; descriptors occupy one binding slot regardless of array count |
| `stepDescriptorSets` | each uniform group (buffer) gets its own set (`finishSet`), capped at 31 |
| `createInputLocations` / `createOutputLocations` / `createPassingLocations` | `layout(location = n)` on vertex inputs / fragment outputs / inter-stage varyings |
| `createPerStageUniforms` | emit uniform declarations inside each stage's `#ifdef` block instead of once at the top |
| `reverseMatrixLogic` | swap operands of matrix products so row-major Aardvark matrices can be uploaded as-is (always `true` in practice) |
| `reverseTessellationWinding` | `layout(cw)` instead of `ccw` in tess eval (Vulkan's flipped y) |
| `depthWriteMode` | emit `layout(depth_less)` etc. from `DepthWriteMode` on `gl_FragDepth` |
| `useInOut` | `in`/`out` qualifiers (GLSL >= 1.30) vs `attribute`/`varying` |
| `separateTexturesAndSamplers` | split every `sampler2D` into `texture2D` + `sampler`, reported in `iface.textures` / `iface.samplerStates` (WGSL/Vulkan interop) |

## Generated Code Shape

```glsl
#version 460
#extension GL_ARB_separate_shader_objects : enable
struct Vertex { vec4 pos; vec4 c; };                       // user types
layout(set = 0, binding = 0, std140) uniform PerModel { mat4 ModelTrafo; };
layout(set = 1, binding = 0, std430) readonly buffer ColorsBuffer { vec4 Colors[]; };
layout(set = 2, binding = 0) uniform sampler2D DiffuseColorTexture;
vec4 helper_1a2b3c(vec4 x) { ... }                          // utility functions: name_hash

#ifdef Vertex
layout(location = 0) in vec4 Positions;                     // vertex inputs: semantic name
layout(location = 0) out vec4 fs_Positions;                 // varyings: <nextStagePrefix>_<semantic>
void main() { ... gl_Position = ...; }
#endif

#ifdef Fragment
layout(location = 0) in vec4 fs_Positions;
layout(location = 0) out vec4 ColorsOut;                    // outputs: <semantic>Out
void main() { ... }
#endif
```

- Stage prefixes (`ShaderStage.prefix`): `vs`, `tc`, `te`, `gs`, `fs`, `cs`, `rgen`, `rint`, `rahit`, `rchit`, `rmiss`, `rcall`.
- Each entry is wrapped in `#ifdef <conditional>`; graphics stages use the stage name, raytracing slots use `ShaderSlot.Conditional` (e.g. `ClosestHit_Model_Primary`). Consumers compile the same source once per stage with the matching `#define`.
- Names pass through `glslName` (`Assembler.fs:1325`): invalid characters become `_` or their char code, reserved words (`src/Libs/FShade.GLSL/Keywords.fs`) get a `_` prefix. `checkName` is the strict variant that throws (`[GLSL] "x" is a reserved keyword ...`).
- Storage buffers are `std430`, uniform buffers `std140` (`Assembler.fs:2151`). A `T[][]` storage buffer becomes `buffer XBuffer { T[] data; } X[];` (`ssbCount = -1`).
- Shared memory (`allocateShared`) becomes `shared T name[n];`.
- Unbounded sampler/image/SSBO arrays each get their **own descriptor set** (must be the last binding; `Assembler.fs:2203`). Dynamic indexing of descriptor arrays emits `nonuniformEXT` only when `createDescriptorSets` is on.
- 8/16/64-bit types pull in the matching `GL_EXT_shader_explicit_arithmetic_types_*` / storage extensions automatically (`useTypeExtensions`).

## Program Interface

`GLSLProgramInterface` (`src/Libs/FShade.GLSL/UniformBufferLayout.fs:856`):

| Field | Type | Content |
|-------|------|---------|
| `inputs`, `outputs` | `list<GLSLParameter>` | first-stage inputs and last-stage outputs: `paramName`, `paramSemantic`, `paramLocation`, `paramType`, `paramInterpolation` |
| `uniformBuffers` | `MapExt<string, GLSLUniformBuffer>` | `ubSet`, `ubBinding`, `ubName`, `ubFields : list<GLSLUniformBufferField>` (`ufName`, `ufType`, `ufOffset`), `ubSize` (std140 applied) |
| `storageBuffers` | `MapExt<string, GLSLStorageBuffer>` | `ssbSet`, `ssbBinding`, `ssbType`, `ssbAccess : StorageAccess`, `ssbCount` (1 or -1) |
| `samplers` | `MapExt<string, GLSLSampler>` | `samplerSet/Binding`, `samplerCount` (-1 unbounded), `samplerTextures : list<string * SamplerState>` (texture semantic + state per element), `samplerType` |
| `images` | `MapExt<string, GLSLImage>` | set/binding, `imageType` (format, dimension, arrayness, value type) |
| `accelerationStructures` | `MapExt<string, GLSLAccelerationStructure>` | set/binding |
| `textures`, `samplerStates` | | only populated with `separateTexturesAndSamplers` |
| `shaders` | `GLSLProgramShaders` | `Compute iface`, `Graphics { stages }`, `Raytracing { slots }`; per-stage `GLSLShaderInterface` with used uniform names, builtins, `shaderDecorations` (`GLSLLocalSize`, `GLSLMaxVertices`, `GLSLInputTopology`, `GLSLOutputTopology`, `GLSLInvocations`, `GLSLSpacing`, `GLSLWinding`) |

`iface.GetUniformStages name` tells which stages use a uniform. `GLSLType` (`UniformBufferLayout.fs:9`)
mirrors GLSL types with sizes/strides (`Int`, `Float`, `Vec`, `Mat`, `Struct(name, fields, size)`,
`Array(len, elem, stride)`, `DynamicArray`, `Sampler`, `Image`, ...). `LayoutStd140.applyLayout`
computes offsets; `src/Tests/FShade.GLSL.Tests/Layout.fs` fuzzes it against glslang reflection.

Serialization: `GLSLShader.pickle` / `unpickle` / `serialize` / `deserialize` (round-trip tested in
`src/Tests/FShade.GLSL.Tests/Serializer.fs`); consumers cache `GLSLShader` keyed by effect id + config.

## Intrinsic Mapping

`src/Libs/FShade.GLSL/IntrinsicFunctions.fs` is a `MethodTable` from .NET methods to `CIntrinsic`
templates: `exactly <@ Fun.Sin : V3f -> V3f @>` and `generic <@ sin @>` entries map to `"sin"`;
texture lookups are matched by `(|TextureLookup|_|)`. Anything not in the table falls back to the
method's `[<GLSLIntrinsic>]` attribute, then to a reflected definition (utility function), then
fails with `cannot call function X since it is not reflectable`.

`IntrinsicParameters.fs` maps builtin semantics to `gl_*` names per stage and lists builtins that
need extensions (`HitPositions` -> `GL_EXT_ray_tracing_position_fetch`).

To add a GLSL function for an existing Aardvark/F# method: add a row to the table (with
`requireExtension` if needed) and a test in `src/Tests/FShade.GLSL.Tests/Intrinsics.fs`.

## Gotchas

- The Vulkan backend of Aardvark post-processes the code: it replaces `gl_InstanceID`/`gl_VertexID` with `gl_InstanceIndex`/`gl_VertexIndex` and rewrites `#version` (`Aardvark.Rendering.Vulkan/Resources/Shaders/ShaderProgram.fs:295`). FShade itself always emits the GL names.
- `bindingMode = Global` without `createDescriptorSets = true` produced overlapping bindings for unbounded arrays before 5.7.9; keep the preset pairs as-is.
- Changing `Config` changes code and interface but not `Effect.Id`; cache keys must include the config.
- `availableExtensions` only filters extensions the assembler *requires*; it does not make FShade choose a different code path (except `GL_EXT_debug_printf`).
- Vertex input names are the raw semantic (`Positions`), not prefixed; fragment outputs are `<semantic>Out`. Tests match on these (`fs_Positions`, `ColorsOut`).
- Matrices: `reverseMatrixLogic` swaps matrix multiplication operands and dimensions so row-major Aardvark matrices can be uploaded without transposing; generated matrix code intentionally differs from hand-written column-major GLSL.
- `GLSLVersion` is a struct without comparison; compare components.
- Double-precision (`float`, `V4d`) requires `GL_ARB_gpu_shader_fp64`; on GL 4.1 drivers this fails at glslang/driver level, not in FShade.

## See Also

- `EFFECTS.md` - producing the `Module` (`Effect.toModule`, `EffectConfig`)
- `COMPILER-INTERNALS.md` - the Imperative lowering that precedes assembly
- `COMPUTE.md`, `RAYTRACING.md` - stage-specific interface details
- `TESTING.md` - `GLSL.shouldCompile*`, `shouldCompileAndContainRegex`

**Source Files:**
- `src/Libs/FShade.GLSL/Backends.fs` (presets, `ModuleCompiler.compileGLSL*`)
- `src/Libs/FShade.GLSL/Assembler.fs` (`Config`, `Backend`, `GLSLShader`, `AssemblerState`, `Assembler`)
- `src/Libs/FShade.GLSL/UniformBufferLayout.fs` (`GLSLType`, interface records, `LayoutStd140`)
- `src/Libs/FShade.GLSL/IntrinsicFunctions.fs`, `IntrinsicParameters.fs`, `Keywords.fs`, `Extensions.fs`
