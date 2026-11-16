# Testing

Two NUnit test projects. `FShade.GLSL.Tests` compiles effects to GLSL and validates them with the
native glslang compiler; `FShade.Core.Tests` tests the optimizer, effect identity, dependencies
and the serializer on quotations. Both use `FsUnit` (`should equal`, `should be True`).

## Running

```sh
dotnet tool restore && dotnet paket restore
dotnet test src/FShade.sln                                             # everything
dotnet test src/Tests/FShade.GLSL.Tests/FShade.GLSL.Tests.fsproj       # one project
dotnet test src/Tests/FShade.Core.Tests/FShade.Core.Tests.fsproj --filter "FullyQualifiedName~Optimizer"
dotnet test ... --filter "Name~Storage buffer read"                    # by test name fragment
```

CI runs both projects in Release on Linux, Windows and macOS. GLSL tests need the native glslang
shipped with the `GLSLangSharp` package; `Setup.Run()` loads it via `Aardvark.LoadLibrary("GLSLangNative", ...)`
and fails with `could not load glslangnative` if missing.

## GLSL Tests (`src/Tests/FShade.GLSL.Tests`)

One module per topic (`SimpleTests.fs` = `module SingleEffects`, `Samplers.fs`, `Images.fs`,
`Loops.fs`, `StructsAndClasses.fs`, `DataTypes.fs`, `Intrinsics.fs`, `Color.fs`, `Compute.fs`,
`Raytracing.fs`, `StorageAccess.fs`, `ConstantFolding.fs`, `Serializer.fs`, `Heap.fs`). Every test
starts with `Setup.Run()` (initializes Aardvark, loads glslang, clears effect caches).

```fsharp
module MyTopic

open Aardvark.Base
open FShade
open FShade.Tests
open NUnit.Framework

type Vertex = { [<Position>] pos : V4f; [<Color>] c : V4f }

[<Test>]
let ``Storage buffer write in fragment shader``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            uniform.FloatOutput.[0] <- v.pos.X
            return v.c
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] [
        "^writeonly buffer [fF]loatOutputBuffer {"
    ]
```

Helpers in `src/Tests/FShade.GLSL.Tests/Utilities.fs` (`module FShade.Tests.GLSL`):

| Helper | Does |
|--------|------|
| `GLSL.compile effects` / `compile' backend effects` | compose, `Effect.toModule` with outputs taken from the last shader (default `Colors : V4f`), compile, run glslang per stage; returns `GLSLShader * (stage * CompilerResult) list` |
| `GLSL.compileWithOutputs backend outputs effects` | explicit output list |
| `GLSL.compileCompute shader` / `compileCompute' backend` | compute shader (default backend `glslVulkan`) |
| `GLSL.compileRaytracing effect` | all slots, glslang target SPIR-V 1.4, one define per slot |
| `GLSL.shouldCompile effects` / `shouldCompile' backend effects` | compile, print code + interface, fail on glslang error |
| `GLSL.shouldCompileCompute`, `shouldCompileRaytracing` | same for the other kinds |
| `GLSL.shouldCompileAndContainRegex effects [regex]` (+ `'` backend variant) | additionally assert each regex matches the code (multiline) |
| `GLSL.shouldCompileAndContainRegexWithCount effects [regex, n]` | exact match counts |
| `GLSL.shouldCompileComputeAndContainRegex`, `shouldCompileRaytracingAndContainRegex` | |
| `GLSL.shouldContainRegex glsl [regex, count option]` | on an existing `GLSLShader` |
| `GLSL.glslang stage code` / `glslangWithDefines` / `glslangWithTarget` | raw glslang checks (`Success`, `Warning`, `Error`) |
| `GLSL.printResults rtx results glsl` | prints code with line numbers, interface, SBT layout |
| `Code.equal a b` | whitespace-insensitive string comparison |

Default backend for graphics tests is `glsl430`; use `shouldCompile' glslVulkan` for descriptor
sets, 8/16/64-bit types or raytracing-adjacent features. Regex assertions should match the
semantic naming (`fs_Positions`, `ColorsOut`, `<name>Buffer`) and tolerate mangled identifiers.

`Program.fs` (`[<EntryPoint>]`, `GenerateProgramFile=false`) is a scratch main for printing code;
`Layout.fs` and `Reflector.fs` are a std140 fuzzer against glslang reflection (`randomHate`) and a
SPIR-V reflector helper; they contain no `[<Test>]` cases. `FsCheck` is referenced but unused.

## Core Tests (`src/Tests/FShade.Core.Tests`)

`Utilities.fs` provides:

| Helper | Does |
|--------|------|
| `Opt.run expr` | preprocess + inlining + constant folding + inlining + DCE + hoisting, with `[<KeepCall>]` methods as side effects |
| `should exprEqual expected` | structural comparison after normalizing utility calls, uniform reads, nested sequentials (hash-based) |
| `keep x` (`[<KeepCall>]`), `produce<'a>()` | mark a value as used / produce an opaque value in test quotations |
| `hasCall regex expr` | whether a call with matching method name survives |

```fsharp
[<Test>]
let ``[For] parallel increment``() =
    let input = <@ let mutable a = 0 in let mutable b = 0 in (for i in 0 .. 10 do a <- a + 1; b <- b + 1); keep a @>
    let expected = <@ let mutable a = 0 in (for i in 0 .. 10 do a <- a + 1); keep a @>
    input |> Opt.run |> should exprEqual expected
```

- `Effect.fs`: `[OfFunction]` identity/caching, `[Compose]` associativity and neutral element, `[Deps]` dependency tracking vs. the linker, `[AOT]` blob round trips. Tests call `Effect.clearCaches()` in `setup()`.
- `Hashes.fs`: `[Serializer]` round trips of quotations through `Serializer.Expr`, `[Hashing]` that sampler state, texture name, image format, uniform semantic and type change `Effect.Id`, `[ComputeHashing]` for compute shaders.
- `Optimizer.fs`: DCE/constant folding/inlining behaviours on loops, refs, utility functions.

## Conventions

- Test names are backtick strings with an optional bracketed category prefix (`[For]`, `[Serializer]`, `[OfFunction]`); keep them unique per module.
- Define vertex record types and samplers at module level (samplers must be static); shader functions can be local to the test.
- Prefer `shouldCompileAndContainRegex` over `shouldCompile` so the test asserts the emitted construct, not just validity.
- When a bug is backend specific, test both `glsl430` and `glslVulkan`.
- For new intrinsics add a case to `Intrinsics.fs` next to the related functions (108 tests there follow the same shape).
- Do not add tests that depend on exact mangled utility-function names (`name_<hash>`); use `.+_Name` patterns as the existing tests do.

## Gotchas

- Forgetting `Setup.Run()` gives `not initialized: use FShade.Serializer.Init()` or a glslang load failure.
- Effects are cached by hash across tests; `Setup.Run()` clears caches, but a shader defined at module level is still the same function (same hash) in every test, which is fine.
- glslang `Warning` results are printed but do not fail; `Error` does. Check the printed GLSL when a test fails only on one OS (driver-independent, so usually an extension/version issue).
- The regex helpers use `RegexOptions.Multiline`; anchor with `^` to match declarations at line start.
- `GLSL.compile` picks outputs from the last shader's outputs; a vertex-only effect list gets a passing fragment stage with `Colors : V4f` output.
- Debug builds of the test project run the shader pipeline much slower; `-c Release` for the full suite.

## See Also

- `DEBUG.md` - printing generated code
- `GLSL-BACKEND.md` - backend presets used by the helpers
- `COMPILER-INTERNALS.md` - what `Opt.run` exercises

**Source Files:**
- `src/Tests/FShade.GLSL.Tests/Utilities.fs`, `Program.fs`
- `src/Tests/FShade.Core.Tests/Utilities.fs`
- `.github/workflows/test.yml`
