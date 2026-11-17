# FShade AI Reference

Index for AI coding assistants. Read `AGENTS.md` (repo root) for commands and rules, then only
the doc you need. Verify API names in source with `rg`; source is truth.

## By Task

| Task | Document | Size |
|------|----------|------|
| Write vertex/fragment/geometry/tessellation shaders, uniforms, samplers, types | SHADER-DSL.md | ~18 KB |
| Create, compose, link effects; `EffectConfig`; shader-level transforms | EFFECTS.md | ~12 KB |
| Compute shaders, `LocalSize`, shared memory, arguments | COMPUTE.md | ~8 KB |
| Raytracing stages, payloads, hit groups, SBT, SER | RAYTRACING.md | ~10 KB |
| GLSL output, backend presets/config, program interface, bindings | GLSL-BACKEND.md | ~11 KB |
| Preprocessor, optimizer passes, Imperative IR, serializer, where to change what | COMPILER-INTERNALS.md | ~11 KB |
| Printing GLSL, `Debug.Printf`, hot-reload debugger, error messages | DEBUG.md | ~9 KB |
| Effect pickling, `fshadeaot`, blob versions, shader caches | AOT.md | ~7 KB |
| Writing and running tests, helpers, conventions | TESTING.md | ~7 KB |

Using FShade from an Aardvark.Rendering application (`Sg.shader`, `DefaultSurfaces`, uniform
conventions, compute/raytracing runtime APIs) is documented in that repository's `ai/` folder
(`SG-SEMANTICS.md`, `DEBUG.md`, `RENDER-PATTERNS.md`), not here.

## By Symbol

### DSL
- `vertex`, `fragment`, `triangle`/`line`/`point`, `tessellation`, `_triangle<'d>` -> SHADER-DSL.md
- `[<Position>]`, `[<Color>]`, `[<Semantic>]`, `[<Depth>]`, `[<Interpolation>]`, builtin attributes -> SHADER-DSL.md
- `uniform?Name`, `UniformScope`, `StorageBuffer`, `Arr<N<n>,T>` -> SHADER-DSL.md
- `sampler2d { }`, `textureArray`, `Image2d<Formats.rgba8>`, `SamplerState` -> SHADER-DSL.md
- `[<ReflectedDefinition>]`, `[<Inline>]`, `[<KeepCall>]`, `[<GLSLIntrinsic>]`, `onlyInShaderCode` -> SHADER-DSL.md
- `unroll`, `discard`, `ddx`, `Atomic`, `Bitwise`, pack/unpack -> SHADER-DSL.md
- `beginInvocationInterlock`, `endInvocationInterlock`, `FragmentInterlockMode` -> SHADER-DSL.md
- `compute { }`, `[<LocalSize>]`, `getGlobalId`, `allocateShared`, `barrier` -> COMPUTE.md
- `raygen`, `miss`, `closestHit`, `anyHit`, `intersection`, `callable`, `scene { }`, `TraceRay`, `unchanged`, `HitObject` -> RAYTRACING.md

### Effects
- `Effect`, `Effect.ofFunction`, `Effect.compose`, `Effect.toModule`, `EffectConfig` -> EFFECTS.md
- `Effect.Dependencies`, `EffectDeps`, `EffectInputLayout` -> EFFECTS.md
- `Effect.substituteUniforms`, `toLayeredEffect`, `tryReplaceGeometry` -> EFFECTS.md
- `Shader` record, `Shader.withBody`, `substituteReads/Writes`, `Shader.compose2` -> EFFECTS.md
- `ComputeShader.ofFunction`, `csBuffers`, `csUniforms` -> COMPUTE.md
- `RaytracingEffect`, `raytracingEffect { }`, `hitgroup { }`, `ShaderBindingTableLayout`, `RayId`/`MissId`/`CallableId` -> RAYTRACING.md
- `Effect.pickle`, `unpickleResource`, `UseAOT`, `blobFormatVersion` -> AOT.md

### Backend
- `Backend.Create`, `Config`, `glsl410`/`glsl430`/`glslVulkan`, `ModuleCompiler.compileGLSL` -> GLSL-BACKEND.md
- `GLSLShader`, `GLSLProgramInterface`, `GLSLUniformBuffer`, `GLSLStorageBuffer`, `GLSLSampler` -> GLSL-BACKEND.md
- `BindingMode`, descriptor sets, unbounded arrays, `separateTexturesAndSamplers` -> GLSL-BACKEND.md
- `IntrinsicFunctions`, `IntrinsicParameters`, `Keywords` -> GLSL-BACKEND.md

### Internals
- `Preprocessor`, `preprocessNormalS`/`ComputeS`/`RaytracingS`, `Preprocessor.State` -> COMPILER-INTERNALS.md
- `Optimizer.inlining`, `eliminateDeadCode'`, `evaluateConstants'`, `liftInputs`, `hoistImperativeConstructs` -> COMPILER-INTERNALS.md
- `Module`, `EntryPoint`, `CModule`, `CExpr`, `CStatement`, `CType`, `Compiler.Backend` -> COMPILER-INTERNALS.md
- `UtilityFunction`, `Expr.ComputeHash`, `Serializer.Init` -> COMPILER-INTERNALS.md

### Tooling
- `FShade.Debug.ShaderDebugger.attach`, `ShaderDebugger.tryRegisterEffect`, `Debug.Printf` -> DEBUG.md
- `fshadeaot`, `--double-check` -> AOT.md
- `GLSL.shouldCompile*`, `Setup.Run`, `Opt.run`, `exprEqual` -> TESTING.md

## Quick Reference

- **Pipeline**: quotation -> `Preprocessor` -> `Shader` -> `Effect` (compose/link) -> Imperative `Module` -> `ModuleCompiler.compile` -> GLSL `Assembler` -> `GLSLShader`
- **Float type**: `float32`/`V4f`/`M44f`; `float`/`V4d` are GLSL doubles since 5.7.0
- **Uniform naming**: `uniform?Scope?Name` -> uniform buffer `Scope`, field `Name`; `StorageBuffer` scope = SSBO
- **Backend**: GL via `glsl430`-like config, Vulkan via `glslVulkan` + glslang; `FShade.SpirV` is unmaintained
- **Caching**: effects by content hash (`Effect.Id`); `Effect.clearCaches()` in tests
- **Build/test**: `./build.sh`, `dotnet test src/FShade.sln`

## When to Read

| Situation | Document |
|-----------|----------|
| A shader does not compile or an output is missing | SHADER-DSL.md, DEBUG.md |
| Composition produces unexpected results | EFFECTS.md |
| Generated GLSL has wrong bindings/sets/layouts | GLSL-BACKEND.md |
| Adding an intrinsic, builtin, optimizer rule or IR node | COMPILER-INTERNALS.md, GLSL-BACKEND.md |
| Hot reload or printf does not work | DEBUG.md |
| Startup is slow / AOT blobs fail | AOT.md |
| Adding or fixing tests | TESTING.md |
