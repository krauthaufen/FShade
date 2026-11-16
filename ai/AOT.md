# Ahead-of-Time Compilation and Serialization

Creating an `Effect` at runtime means walking quotations, preprocessing and hashing, which is
slow on first use. FShade can serialize effects to a binary blob and the `fshadeaot` tool can
patch compiled assemblies so `Effect.ofFunction` calls load that blob instead.

## Effect Serialization API

| Function (`src/Libs/FShade.Core/Effect.fs`) | Purpose |
|---------------------------------------------|---------|
| `Effect.pickle : Effect -> byte[]` | serialize id, dependencies and all shaders |
| `Effect.serialize stream effect` / `Effect.deserialize stream` / `tryDeserialize` | stream variants |
| `Effect.read (bytes : byte[])` | eager header (version, id, `EffectDeps`), lazy shaders |
| `Effect.unpickleWithId id base64` | blob as base64 with the id supplied out of band |
| `Effect.unpickleResource lambda id assemblyName` | load from an embedded manifest resource; falls back to re-running `lambda` (the original shader function) if loading fails or `Effect.UseAOT = false` |
| `Effect.unpickleInternal lambda id base64` | same with inline base64 |
| `Effect.UseAOT` (mutable, default `true`) | `false` forces the fallback path (warning `Effect deserialization disabled, re-creating Effect using ofFunction`) |
| `Effect.UsePrecompiled` (mutable, default `true`) | honour an `Effect` carried inside a quotation via `WithValue` |
| `GLSLShader.pickle` / `unpickle` (`FShade.GLSL`) | cache compiled GLSL + interface (consumer shader caches) |

Blob layout: `[version : byte = 1][id : string][EffectDeps][shaderCount : int32][(stage : int32, Shader)*]`.
`blobFormatVersion` lives in `Effect.fs` (`let private blobFormatVersion = 1uy`); a mismatch raises
`[FShade] effect blob format version mismatch: got X, expected 1`. There is intentionally no
backwards compatibility: bump the version whenever `Serializer.Shader` output changes.

The lazy-shader design means consumers can read `effect.Dependencies` and `effect.Id` from a blob
without deserializing shader bodies (`src/Tests/FShade.Core.Tests/Effect.fs` `[Deps] read accesses
Dependencies without forcing shaders`, `[AOT] unpickleInternal reads pickled blob without fallback`).

Serialization covers quotations referencing reflected methods, generic instantiations, samplers
and their states, storage buffers, enums of any width, uniform scopes (`src/Tests/FShade.Core.Tests/Hashes.fs`
`[Serializer] *`). Values captured in closures must be serializable by `Serializer.Value`.

## The `fshadeaot` Tool

Project: `src/FShade.Preprocessor/fshadeaot.fsproj` (packed as a dotnet tool, command `fshadeaot`).

```sh
dotnet tool install fshadeaot                      # or a local tool manifest
dotnet build -c Release
fshadeaot bin/Release/net10.0/MyApp.dll [--verbose] [--double-check]
```

What `Program.fs` does:
1. Copies the entry assembly's directory to a temp folder and loads everything into an isolated `AssemblyLoadContext` (`loadAllAssemblies`).
2. `getReplacableShaderCompileMethods`: finds every method in assemblies referencing `FShade.Core` that takes an `FSharpFunc<_, Expr<_>>` parameter and either returns `Effect` (i.e. `Effect.ofFunction`, `toEffect`) or has an overload taking an `Effect` in that position (e.g. Aardvark's `Sg.shader` builder `Bind` overloads).
3. For every call site of such a method (Mono.Cecil IL scan), uses `Interpreter.fs` to locate and evaluate the argument push (`tryFindParameterPushLocation`, `tryGetTopOfStack`): it must be a constant shader function (static closure, constant arguments). Non-constant arguments are skipped (`non-constant argument in: X`).
4. Invokes the shader function, runs `Effect.ofExpr`, `Effect.pickle`, and rewrites the IL: the `ofFunction` call is replaced by `Effect.unpickleResource(lambda, id, assemblyName)` (resource embedded in the module) or by the overload taking an `Effect`. Falls back to `unpickleInternal`/`unpickleWithId`/`read` for older FShade.Core versions.
5. Writes the patched assemblies back over the originals (`patched N effect-creations`).

Only `net*`/`netstandard` assemblies are supported (`netframework dlls cannot be patched atm.`).
The patched call still carries the original lambda, so a blob mismatch degrades to runtime
compilation with a warning rather than failing.

### `--double-check`

`DoubleChecker.run` (`src/FShade.Preprocessor/Double.fs`) scans shader expressions, `UniformScope`
extension members and generic shader helpers for `double`-based types (`float`, `V2d..V4d`, `M*d`)
and prints warnings such as `X defines uniform Y with double-based type` or `X uses double-based
types`. Use it when migrating from pre-5.7 code where doubles were silently treated as floats.

## Relation to Shader Caches

Rendering backends typically cache `GLSLShader.pickle` output on disk keyed by `Effect.Id` plus
their own layout/config. That is independent of `fshadeaot`: AOT removes the quotation-processing
cost, a shader cache removes the GLSL compilation cost. `Effect.Id` is stable across runs as long
as the shader source, closure values and FShade version are unchanged.

## Gotchas

- AOT blobs embed the FShade serializer format; upgrading FShade requires re-running `fshadeaot` (version byte check makes this loud).
- Shader functions with non-constant closure arguments (runtime values) cannot be precompiled; they are left untouched and compiled at runtime.
- The tool patches **copies** in temp and writes them back; run it after the final build step and before packaging. It processes referenced assemblies too (anything referencing `FShade.Core` except `FShade.GLSL`).
- `Effect.UseAOT <- false` at startup is the quickest way to rule out stale blobs when debugging odd shader behaviour.
- Pickled effects keep `SourceDefinition = None`, so the shader debugger cannot hot-reload AOT-loaded effects unless the fallback path recreated them.
- `--parse-only`/`--parseonly` on `aardpack` (CI version parsing) is unrelated to `fshadeaot`.

## See Also

- `EFFECTS.md` - `Effect` identity and `Dependencies`
- `COMPILER-INTERNALS.md` - the serializer and hashing
- `DEBUG.md` - interaction with the hot-reload debugger

**Source Files:**
- `src/Libs/FShade.Core/Effect.fs` (pickle/unpickle, `blobFormatVersion`, `UseAOT`)
- `src/Libs/FShade.Core/Serializer.fs`
- `src/FShade.Preprocessor/Program.fs`, `Interpreter.fs`, `Cecil.fs`, `Double.fs`
- `src/Tests/FShade.Core.Tests/Effect.fs` (`[AOT]`, `[Deps]` tests)
