# Effects and Composition

An `Effect` is an immutable, content-addressed bundle of shaders, at most one per graphics stage.
This doc covers creating effects from shader functions, composing them, linking them for a
render target (`Effect.toModule`), and the `Shader`-level transformation API.

## Core Types

| Type | Location | Purpose |
|------|----------|---------|
| `Shader` (record) | `src/Libs/FShade.Core/Shader.fs:26` | one stage: `shaderStage`, `shaderInputs/Outputs : Map<string, ParameterDescription>`, `shaderUniforms : Map<string, UniformParameter>`, `shaderBody : Expr`, topologies, raytracing data, `shaderDepthWriteMode`, `shaderStorageBufferAccess` |
| `Effect` (class) | `src/Libs/FShade.Core/Effect.fs:132` | `Id : string` (hash), `Shaders : Map<ShaderStage, Shader>` (lazy), `ComposedOf : Effect list`, `Dependencies : EffectDeps`, `SourceDefinition` (for the debugger), accessors `VertexShader`, `FragmentShader`, `Inputs`, `Outputs`, `Uniforms`, `InputToplogy` (sic), `FirstShader`, `LastShader`, `LastPrimShader` |
| `EffectConfig` | `Effect.fs:236` | `depthRange : Range1f`, `flipHandedness : bool`, `lastStage : ShaderStage`, `outputs : Map<string, Type * int>` |
| `EffectDeps` / `OutputDeps` | `Effect.fs:28` | per-output inputs + uniforms, split into `Primitive` and `Fragment` maps |
| `Module` (Imperative) | `src/Libs/FShade.Imperative/Frontend.fs:564` | `Hash`, `UserData` (the effect), lazy `Entries : EntryPoint list`, `TryGetOverrideCode` |

`ShaderStage` ordering matters: `Vertex = 0 < TessControl < TessEval < Geometry < Fragment = 4`;
compute and raytracing stages are negative (`src/Libs/FShade.Imperative/Types.fs:115`).

## Creating Effects

```fsharp
let e1 = Effect.ofFunction trafo                 // 'v -> Expr<'b>
let e2 = Effect.ofFunction (constantColor C4f.Red)   // closure args are allowed and hashed
let e3 = Effect.ofExpr typeof<Vertex> expr       // raw quotation + input type
let e4 = Effect.ofList [ shaderA; shaderB ]      // from Shader records (one per stage)
let e5 = Effect.ofShader shader
```

| Function | Notes |
|----------|-------|
| `Effect.ofFunction` | invokes the function with `Unchecked.defaultof<'a>` to obtain the quotation, inlines splices (`%expr`), hashes `(body, input semantics, output semantics)` with SHA1 and returns the cached instance for equal hashes |
| `Effect.ofExpr inputType expr` | same without a function; used by the debugger and AOT |
| `Effect.empty` | `Id = ""`, neutral element of `compose` |
| `Effect.clearCaches()` | drops the `effectCache` and `composeCache` (tests call it in `Setup.Run()`) |
| `Effect.UsePrecompiled` / `UseAOT` (mutable) | gate the AOT path, see `AOT.md` |

Identity: two `Effect.ofFunction` calls on the same function (and equal closure arguments) return
the same object (`src/Tests/FShade.Core.Tests/Effect.fs` `[OfFunction] *` tests). The `Id` is
stable across processes and is what consumers key shader caches on.

Preprocessing is lazy: `Shaders` is a `Lazy<Map<...>>`; `Effect.ofFunction` only hashes the raw
quotation. Errors from the preprocessor therefore surface when a shader is first inspected
(composition that needs stage info, `toModule`, `Inputs`, ...), not at creation.

## Composition

```fsharp
let composed = Effect.compose [ trafoEffect; textureEffect; lightingEffect ]
// or the Aardvark builder:  Sg.shader { do! trafo; do! texture; do! lighting }
```

`Effect.compose2 l r` (`Effect.fs:1060`) works per **abstract stage**:
- Primitive stages (vertex, tess control, tess eval, geometry) are concatenated in order; where
  `r`'s first primitive shader has the same or an earlier stage than `l`'s last one, the two
  shaders are merged with `Shader.compose2` (vertex+vertex, geometry+vertex, geometry+geometry,
  vertex+geometry are implemented; anything else raises `cannot compose XShader with YShader`).
- Fragment shaders are merged with `Shader.compose2` (fragment+fragment).
- Result `Id` is `l.Id + r.Id`; results are cached in `composeCache`; `ComposedOf = [l; r]`.

Semantics of merging two shaders of the same stage (`Shader.Composition.simpleInlined`):
1. `l` runs first; outputs of `l` that `r` reads become local variables (converted if the types differ).
2. Outputs written by `r` win; outputs of `l` that `r` does not write are passed through.
3. Inputs of `r` that `l` does not produce become inputs of the merged shader.
4. Uniforms are unioned; a name with conflicting types raises `uniform 'X' has conflicting types`. Storage buffer access flags are unioned too (a buffer written by one shader and read by the other becomes read-write; `Shader.optimize` re-derives the access from the merged body). The same union happens across stages when the assembler merges the per-stage declarations (`CUniform.mergeMany`).
5. `DepthWriteMode` of `r` wins if set.

Consequences: order matters ("last writer wins"), `compose` is associative with `Effect.empty` as
neutral element (tested in `Effect.fs` `[Compose] *`), and a vertex shader placed after a geometry
shader is fused into the geometry shader (`gsvs`), so `[ gs; vs ]` is valid.

## Linking: `Effect.toModule`

```fsharp
let config =
    { EffectConfig.empty with
        lastStage = ShaderStage.Fragment
        outputs   = Map.ofList [ "Colors", (typeof<V4f>, 0); "Normals", (typeof<V3f>, 1) ]
        depthRange = Range1f(0.0f, 1.0f)      // Vulkan: map clip z from [-1,1] to [0,1]
        flipHandedness = false }

let module_ = effect |> Effect.toModule config
let glsl = module_ |> ModuleCompiler.compileGLSL glsl430   // FShade.GLSL, see GLSL-BACKEND.md
```

`toModule` (`Effect.fs:954`):
1. `link`: adds a passing shader for `lastStage` if missing, adds an empty vertex shader if
   missing, then walks the stages **backwards** calling `Shader.withOutputs desired` so every stage
   produces exactly what the next stage needs (plus system outputs). Unused outputs are dropped,
   missing ones are passed through from inputs of the same semantic.
2. `withDepthRange`: rewrites the `Positions` write of the last primitive shader to remap `z`
   and optionally negate `y`.
3. Builds one `EntryPoint` per stage, resolving interpolation modes across the boundary and
   assigning output slots from `config.outputs` (unlisted outputs get the next free slot).

`EffectConfig.ofList [ name, type, slot ]` is the usual constructor. `lastStage` lets you stop
before the fragment stage (transform feedback style).

## Dependencies

`effect.Dependencies : EffectDeps` reports, per output semantic, which inputs and uniforms it
needs (`OutputDeps.Inputs : Map<string, Type>`, `Uniforms`). Leaf effects derive it from the shaders
via `EffectDeps.ofShaders`; composed effects combine maps with `EffectDeps.compose` without
forcing shader bodies. `EffectDeps.resolveTop` flattens to the final outputs. Serialized effect
blobs carry the deps eagerly so cached effects can be inspected cheaply (`Effect.read`).

## Transformations

| Function | Effect |
|----------|--------|
| `Effect.map f` | apply `f : Shader -> Shader` to every stage (new id) |
| `Effect.add shader` / `Effect.remove stage` / `Effect.alter stage f` | replace/remove a stage |
| `Effect.substituteUniforms f` | replace uniform reads: `f name type index slot : Expr option` (used to redirect uniforms to storage-buffer arenas, see `src/Tests/FShade.GLSL.Tests/Heap.fs`) |
| `Effect.inputsToUniforms scopes` / `Effect.uniformsToInputs semantics` | turn vertex inputs into uniforms (instancing) and back |
| `Effect.toLayeredEffect layers perLayerUniforms topology` | emit a geometry shader with `invocations = layers` writing `gl_Layer`; `perLayerUniforms : Map<old, newArrayName>` are indexed by layer; fails for tessellation |
| `Effect.toMultiViewportEffect` | same with `ViewportIndex` |
| `Effect.toLayeredEffect' layerSem customSem ...` | custom semantics (Aardvark uses `"GeometryInvocationId"` when layered inputs are unsupported) |
| `Effect.tryReplaceGeometry` | rewrites a geometry shader with constant output count into a vertex shader (`Id + "VSS"`); returns `None` for tessellation or unknown vertex counts |
| `Effect.pickle` / `unpickleWithId` / `serialize` / `deserialize` / `read` | binary blob (`blobFormatVersion = 1`), see `AOT.md` |

Shader-level API (`Shader` module, `Shader.fs:3249`):

| Function | Use |
|----------|-----|
| `Shader.ofFunction` / `Shader.ofExpr inputTypes expr` | preprocess a quotation into one or more `Shader` records (a `tessellation { }` block yields two) |
| `Shader.withBody expr shader` | replace the body and recompute inputs/uniforms/storage access |
| `Shader.substituteReads f` / `substituteWrites f` | rewrite `ReadInput`/`WriteOutputs` nodes (`f kind type name index slot`) |
| `Shader.withOutputs map` / `removeOutputs set` / `neededInputs` / `outputs` | linking primitives |
| `Shader.passing attributes stage` / `Shader.empty stage` | pass-through shader |
| `Shader.optimize` | run the optimizer pipeline (inlining, constant folding, DCE, loop unrolling); already applied by `ofExpr` |
| `Shader.toEntryPoint prev shader next` | build the imperative `EntryPoint` |
| `Shader.compose2` / `Shader.compose` | stage-aware merge used by `Effect.compose2` |

Quotation helpers live in `src/Libs/FShade.Imperative/UtilityFunction.fs` (`Expr.substituteReads`,
`Expr.substituteWrites`, `Expr.ReadInput`, `Expr.WriteOutputs`, `Expr.getAffectedOutputsMap`).

## Input Layouts

`EffectInputLayout` (`src/Libs/FShade.Core/EffectInputLayout.fs`) unifies the uniform buffers,
inputs and uniforms of several modules so a pool of effects shares one binding layout:

```fsharp
let modules = effects |> Array.map (Effect.toModule config)
let layout  = EffectInputLayout.ofModules modules
let unified = modules |> Array.map (EffectInputLayout.apply layout)
```

Aardvark's `Sg.effectPool` / `Surface.effectPool` use this so switching the active effect does
not change the pipeline layout.

## Gotchas

- `Effect.Id` includes closure argument values; an effect created inside a per-frame loop with a changing argument creates a new effect (and a new GPU program) every time.
- `compose` of two effects whose vertex shaders both read an input that neither writes is fine; two shaders writing the same output: the later one wins silently.
- `toModule` silently drops outputs not requested in `config.outputs` **and** not consumed by a later stage. If a varying "disappears", check the fragment shader actually reads it.
- `[FShade] positions not written before fragment shader`: no stage writes `Positions`; add a vertex shader with `[<Position>]`.
- Changing `EffectConfig.outputs` slots changes the generated code and the `GLSLShader`, but not the `Effect.Id`; consumers must include the config in their cache key.
- `Effect.Shaders` forcing can throw preprocessor errors late; `Effect.ofFunction` succeeding does not mean the shader compiles.
- The `Effect` constructor overloads that take no id generate a fresh GUID-like id; such effects are never cache hits. Prefer `Effect.ofFunction`/`ofList`.
- `Serializer.Init()` is called by every public function; if you call internal `Shader.*` functions from a fresh process, call it yourself.

## See Also

- `SHADER-DSL.md` - writing the shader functions
- `GLSL-BACKEND.md` - `ModuleCompiler.compileGLSL`, backend presets, program interface
- `COMPILER-INTERNALS.md` - preprocessing and optimization that `Shader.ofExpr` performs
- `AOT.md` - effect serialization and ahead-of-time compilation
- aardvark.rendering `ai/SG-SEMANTICS.md` - how Aardvark.Rendering applies and links effects (`Sg.shader`, `Sg.effectPool`)

**Source Files:**
- `src/Libs/FShade.Core/Effect.fs` (`Effect`, `EffectConfig`, `EffectDeps`, `Effect` module)
- `src/Libs/FShade.Core/Shader.fs` (`Shader` record, `Shader` module, `Composition`)
- `src/Libs/FShade.Core/EffectInputLayout.fs`
- `src/Libs/FShade.Core/SourceDefinition.fs` (debugger metadata attached to effects)
- `src/Tests/FShade.Core.Tests/Effect.fs` (identity, composition, dependency tests)
