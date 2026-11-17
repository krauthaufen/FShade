# AI Agent Reference

Primary entry point for AI agents working with FShade. Indexed reference docs live in `ai/README.md`.

FShade is an F# embedded DSL: shaders are written as F# code quotations (`vertex { ... }`,
`fragment { ... }`, `compute { ... }`, `raygen { ... }`, ...), composed into `Effect` values,
optimized, and compiled to GLSL. It is consumed primarily by
[Aardvark.Rendering](https://github.com/aardvark-platform/aardvark.rendering).

## Commands

| Task | Command | Notes |
|------|---------|-------|
| Restore | `dotnet tool restore && dotnet paket restore` | Tools: paket, aardpack |
| Build (Windows) | `.\build.cmd` | Restores, then builds `src/FShade.sln` |
| Build (Linux/macOS) | `./build.sh` | Same solution; no platform-specific projects |
| Build one project | `dotnet build src/Libs/FShade.Core/FShade.Core.fsproj` | |
| Test all | `dotnet test src/FShade.sln` | NUnit; GLSL tests validate output with glslang |
| Test one project | `dotnet test src/Tests/FShade.GLSL.Tests/FShade.GLSL.Tests.fsproj` | Prefer over whole solution |
| Test with filter | `dotnet test src/Tests/FShade.GLSL.Tests/FShade.GLSL.Tests.fsproj --filter "FullyQualifiedName~Samplers"` | NUnit adapter syntax |
| Run GLSL test scratch main | `dotnet run --project src/Tests/FShade.GLSL.Tests/FShade.GLSL.Tests.fsproj` | `Program.fs` has an `[<EntryPoint>]` that prints GLSL |
| Pack (CI) | `.\pack.cmd` | `dotnet aardpack src\FShade.sln --notag` plus `dotnet pack` of `fshadeaot`; version comes from `RELEASE_NOTES.md` |

Build and test scripts stop at the first failure. CI (`.github/workflows/test.yml`) runs
`dotnet build -c Release` and `dotnet test -c Release --no-build` on ubuntu, windows and macos.

## Dependency Management (Paket)

| Task | Command |
|------|---------|
| Restore packages | `dotnet paket restore` |
| Add package | Edit `paket.dependencies`, add to the project's `paket.references`, run `dotnet paket install` |
| Update all | `dotnet paket update` |
| Update one | `dotnet paket update PackageName` |
| Why dependency | `dotnet paket why PackageName` |

**Rules:**
- NEVER use `dotnet restore` alone; always use `dotnet paket restore` (the `.paket/Paket.Restore.targets` import needs it)
- `paket.lock` is committed; never edit it by hand
- Per-project dependencies go in `paket.references`, not in the `.fsproj`
- Test-only packages (NUnit, FsUnit, FsCheck, GLSLangSharp) live in the `Test` group of `paket.dependencies`

## Release Notes and Publishing

`RELEASE_NOTES.md` drives releases. The newest version is the first `### x.y.z` heading at the
top of the file. `aardpack --parse-only` reads that heading; the Publish workflow runs on every
push to `master` that touches `RELEASE_NOTES.md` and creates a GitHub release + NuGet packages
if no release with that version exists yet.

- To record unreleased work, add plain `- ...` bullets at the very top of the file, above the first `### x.y.z` heading. Do not add a version heading; the maintainer adds it at release time
- Never append to an already released block (its release already exists)
- Keep entries short and user-facing (what changed, which API), no implementation details
- Prefix backend-specific entries with `[GLSL]`, `[Raytracing]`, `[Debugger]`, `[Compute]`, `[Optimizer]`, `[fshadeaot]`
- Some notes describe features that were later reverted (e.g. `SpecConstants` in 5.7.15 was reverted
  before 5.7.16). Source is truth; verify with `rg` before relying on a note

## File Ownership by Change Type

| Change Type | Files to Modify | Files to Avoid |
|-------------|-----------------|----------------|
| New DSL intrinsic / builtin function | `src/Libs/FShade.Core/Frontend.fs` (F# side), `src/Libs/FShade.GLSL/IntrinsicFunctions.fs` (GLSL mapping) | |
| New builtin stage input/output semantic | `src/Libs/FShade.Core/Intrinsics.fs` (name + attribute), `src/Libs/FShade.Core/Shader.fs` (`builtInInputs`/`builtInOutputs`), `src/Libs/FShade.GLSL/IntrinsicParameters.fs` (`gl_*` name) | |
| Sampler/image API | `src/Libs/FShade.Core/SamplerCodeGen.fsx` then regenerate `Samplers.fs` | Hand-editing `Samplers.fs` (generated) |
| Quotation preprocessing / stage semantics | `src/Libs/FShade.Core/Shader.fs` (`Preprocessor`, `Shader` modules) | |
| Optimizer passes | `src/Libs/FShade.Core/Optimizer.fs` + tests in `src/Tests/FShade.Core.Tests/Optimizer.fs` | |
| Effect composition / linking | `src/Libs/FShade.Core/Effect.fs`, `Shader.fs` (`Composition` module) | |
| Compute shaders | `src/Libs/FShade.Core/ComputeShader.fs`, `Shader.fs` (`preprocessComputeS`) | |
| Raytracing | `src/Libs/FShade.Core/Raytracing*.fs`, `ShaderBindingTableLayout.fs`, `Shader.fs` (`preprocessRaytracingS`) | |
| GLSL code generation | `src/Libs/FShade.GLSL/Assembler.fs`, `UniformBufferLayout.fs`, `Keywords.fs`, `Extensions.fs` | |
| Backend presets | `src/Libs/FShade.GLSL/Backends.fs` | |
| Imperative IR / C-like lowering | `src/Libs/FShade.Imperative/**` | |
| Effect serialization / hashing | `src/Libs/FShade.Core/Serializer.fs`, `src/Libs/FShade.Imperative/Hashing.fs` + tests in `FShade.Core.Tests/Hashes.fs` | Changing the blob format without bumping `blobFormatVersion` in `Effect.fs` |
| Shader debugger (hot reload) | `src/Libs/FShade.Debug/**` | |
| AOT tool | `src/FShade.Preprocessor/**` | |
| Add test | `src/Tests/FShade.GLSL.Tests/*.fs` (code generation), `src/Tests/FShade.Core.Tests/*.fs` (optimizer, hashing, effects) | Follow `ai/TESTING.md` |
| AI documentation | `ai/**`, `AGENTS.md` | |
| Package dependencies | `paket.dependencies`, `paket.lock`, `*/paket.references` | |
| CI/CD | `.github/workflows/**` | |

## Framework & SDK

- .NET SDK: 10.0.100 (`global.json`, `rollForward: latestFeature`)
- Libraries target `netstandard2.0`; `FShade.Debug` targets `netstandard2.0; net10.0`; tests, demo and `fshadeaot` target `net10.0`
- FSharp.Core `>= 8.0.0` (lowest matching); implicit FSharp.Core reference is disabled in all projects
- Aardvark.Base `~> 5.3.18` provides vector/matrix/color types (`V4f`, `M44f`, `C4f`, `Arr<N<4>, T>`)
- `GenerateDocumentationFile` is on for all libraries: keep `///` doc comments on public API

## Project Structure

```text
src/
|- FShade.sln
|- Libs/
|  |- FShade.Imperative/   # stage-agnostic imperative IR (CExpr/CStatement/CType), EntryPoint/Module,
|  |                       #   UtilityFunction, ModuleCompiler, expression hashing
|  |- FShade.Core/         # DSL (builders, attributes, samplers, intrinsics), Preprocessor, Optimizer,
|  |                       #   Shader/Effect/ComputeShader/RaytracingEffect, Serializer, ShaderDebugger hook
|  |- FShade.GLSL/         # GLSL backend: Config/Backend presets, Assembler, std140/std430 layout,
|  |                       #   intrinsic tables, keyword mangling, extensions
|  |- FShade.SpirV/        # direct SPIR-V backend: UNUSED and UNMAINTAINED (Vulkan goes through GLSL + glslang)
|  `- FShade.Debug/        # hot-reload shader debugger (rebuilds projects, swaps shaders at runtime)
|- FShade.Preprocessor/    # `fshadeaot` dotnet tool: ahead-of-time effect pickling via Mono.Cecil IL patching
`- Tests/
   |- FShade.GLSL.Tests/   # NUnit: compile effects to GLSL, validate with glslang, regex-check output
   |- FShade.Core.Tests/   # NUnit: optimizer, effect identity/caching, dependencies, serializer hashes
   `- FShade.Demo/         # scratch console app (not a test)
```

Compilation pipeline (see `ai/COMPILER-INTERNALS.md`):

```text
F# quotation  ->  Preprocessor (Shader.fs)  ->  Shader record (per stage)
  ->  Effect (Map<ShaderStage, Shader>)  ->  Effect.compose / Effect.toModule (link stages, EffectConfig)
  ->  Imperative Module (EntryPoint list)  ->  ModuleCompiler.compile (CModule)  ->  GLSL Assembler
  ->  GLSLShader { code; iface : GLSLProgramInterface }
```

## Key Concepts

- **Quotations, not strings**: shader functions return `Expr<'b>`; `Effect.ofFunction` evaluates the function with a `null` input, so the function body must not touch its input outside the builder block
- **Effect = Map<ShaderStage, Shader>** with a content hash `Id`; effects and compositions are cached process-wide by hash (`Effect.clearCaches()` resets)
- **Composition semantics**: `Effect.compose [a; b]` runs `a` before `b` per abstract stage (primitive stages: vertex/tess/geometry; fragment); later shaders override outputs of earlier ones, missing inputs are passed through
- **Uniforms by name**: `uniform?Name` or `uniform?Scope?Name`; the scope name becomes the uniform buffer name (`StorageBuffer` = SSBO, `Arguments` = compute argument buffer)
- **Samplers are static values** built with `sampler2d { texture uniform?Name; ... }`; the sampler state is carried in the effect interface, not in GLSL
- **float32 is the shader float**: `float`/`V4d` map to GLSL `double`/`dvec4` since 5.7.0 (the old "double as float" mode is gone)
- **Serializer.Init()** must have run before hashing quotations; all public entry points call it
- **SPIR-V backend is dead**: Vulkan consumers compile GLSL with `glslVulkan` and run glslang; do not invest in `FShade.SpirV`

## Pre-Commit Checklist

- [ ] `./build.sh` (or `.\build.cmd`) succeeds
- [ ] `dotnet test src/FShade.sln` passes (GLSL tests need the native glslang that ships with GLSLangSharp)
- [ ] New code generation behaviour has a `GLSL.shouldCompile*` test; new optimizer behaviour has an `Opt.run |> should exprEqual` test
- [ ] Serializer changes: bump `blobFormatVersion` in `src/Libs/FShade.Core/Effect.fs` if the blob layout changed
- [ ] `RELEASE_NOTES.md` has a bullet for the change at the top of the file (no version heading)
- [ ] No new warnings (`#nowarn "4321"` is expected where `Preprocessor` is used directly)

## Common Failures

| Symptom | Cause | Fix |
|---------|-------|-----|
| `[FShade] shader functions may not access their vertex-input statically` | Shader function evaluates its input outside the builder (input is `null` at effect creation) | Move all input reads inside `vertex { ... }` etc. |
| `[FShade] cannot call function X since it is not reflectable` | Helper function lacks `[<ReflectedDefinition>]` | Add the attribute (or put it on the enclosing module/type) |
| `FShadeOnlyInShaderCodeException` at runtime | An `onlyInShaderCode` intrinsic was called on the CPU | Only call intrinsics inside shader quotations |
| `not initialized: use FShade.Serializer.Init()` | Hashing a quotation before the serializer was initialized | Call `Serializer.Init()` (tests do this in `Setup.Run()`) |
| `[FShade] cannot compose XShader with YShader` | `Shader.compose2` only supports vs+vs, fs+fs, gs+vs, gs+gs, vs+gs | Compose at the `Effect` level, or restructure stages |
| `[FShade] effect blob format version mismatch` | AOT/cached effect blob from an older FShade | Re-run `fshadeaot` / clear the consumer's shader cache |
| `could not load glslangnative` in tests | Native glslang not found | Ensure `GLSLangSharp` restored; `Setup.Run()` loads it via `Aardvark.LoadLibrary` |
| Vulkan validation `VUID-VkDescriptorSetLayoutCreateInfo-binding-00279` / overlapping bindings | Backend config with `bindingMode = Global` but wrong descriptor-set stepping | Use `glslVulkan` preset (`createDescriptorSets = true`, `stepDescriptorSets = true`) |
| `undefined variable "nonuniformEXT"` on GL | Dynamic descriptor-array indexing emitted for a non-descriptor backend | Fixed in 5.7.8; ensure `createDescriptorSets = false` on GL backends |
| Debugger: `WARNING: Cannot find shader definition for ...` | PDB paths from a NuGet package do not exist locally | Build the project locally or ignore for shaders you do not edit |
| Debugger: `Failed to resolve shader <StartupCode$...>` | Shader defined as a value, not a function | Define as `let shader () = ...` or `let shader (v : Vertex) = ...` |
| glslang: `'fragment shader interlock layout qualifier' : not supported for this version` | glslang requires `#version 450` for interlock qualifiers | Validate with a 4.50 (or Vulkan) backend; GL drivers accept 4.20+ |
| `[FShade] beginInvocationInterlock ... must be called at the top level` | Interlock call inside `if`/loop/helper function | Move both calls to the shader body's top level |

## Agent Workflow Tips

1. Read only the `ai/*.md` doc you need; verify API names with `rg` against `src/Libs`. Source is truth.
2. Prototype code generation questions in `src/Tests/FShade.GLSL.Tests` with `GLSL.shouldCompile [ Effect.ofFunction shader ]`; it prints the generated GLSL and the interface.
3. Run the focused test project for the area you touched before the whole solution.
4. When changing the serializer, optimizer or composition, add a regression test; these areas are where most historical bugs lived (see `RELEASE_NOTES.md`).
5. Keep `Samplers.fs` generated from `SamplerCodeGen.fsx`; do not patch the generated file.
