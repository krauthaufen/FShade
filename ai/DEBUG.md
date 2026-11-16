# Debugging

Three tools: printing the generated GLSL, `Debug.Printf` inside shaders (Vulkan), and the hot-reload
shader debugger in `FShade.Debug`.

## 1. Inspecting Generated Code

Fastest loop: a scratch test (or `src/Tests/FShade.Demo`) that compiles the effect and prints
code + interface:

```fsharp
open FShade
open FShade.GLSL
open FShade.Tests          // test helpers in src/Tests/FShade.GLSL.Tests/Utilities.fs

Setup.Run()
GLSL.shouldCompile [ Effect.ofFunction vs; Effect.ofFunction fs ]   // prints code with line numbers, iface, glslang result
let glsl, _ = GLSL.compile' glslVulkan [ Effect.ofFunction fs ]      // keep the GLSLShader
printfn "%s" (string glsl)
```

Outside the test project: `Effect.toModule config |> ModuleCompiler.compileGLSL backend` and print
`glsl.code` (see `EFFECTS.md`). `src/Tests/FShade.GLSL.Tests/Program.fs` has an `[<EntryPoint>]`
for ad-hoc experiments (`dotnet run --project src/Tests/FShade.GLSL.Tests/FShade.GLSL.Tests.fsproj`).

Rendering backends usually offer a switch to log every program they compile; consult the consumer's
own documentation (aardvark.rendering `ai/DEBUG.md`). Programs served from a shader cache are not
recompiled, so a cached `GLSLShader` must be invalidated to see fresh output.

The optimizer has a pretty printer: `Optimizer.PrettyPrinter.print expr` (`Optimizer.fs:16`) renders
an FShade quotation as pseudo code; useful when writing optimizer tests.

## 2. Debug.Printf

```fsharp
let printPosition (v : Vertex) =
    vertex {
        Debug.Printfn("P = %v4f id = %d", v.pos, v.id)
        return v
    }
```

- `Debug.Printf` / `Debug.Printfn` (`src/Libs/FShade.Core/Frontend.fs:343`) are `[<KeepCall>]` so DCE keeps them.
- Emits `debugPrintfEXT(fmt, args)` and requires `GL_EXT_debug_printf`; format syntax is the Vulkan validation-layer one (`%d`, `%u`, `%f`, `%v2f`, `%v4f`, `%v3i`, ...).
- Works only on Vulkan with the validation layers' debug-printf feature enabled by the host application.
- On backends whose config declares the extension unavailable (`Config.availableExtensions.[GL_EXT_debug_printf] = false`, typical for OpenGL) the call becomes a GLSL comment.
- Mixing constant and non-constant arguments was fixed in 5.7.21.

## 3. Shader Debugger (Hot Reload)

`FShade.Debug` recompiles the project that defines a shader when one of its source files changes
and swaps the shader into running effects. A consumer opts in by calling
`ShaderDebugger.tryRegisterEffect/-RaytracingEffect/-ComputeShader` for every shader it compiles and
observing the returned `aval` (Aardvark.Rendering does this for all effects once the debugger is attached).

```fsharp
// Program.fs of the application
Aardvark.Init()
FShade.Debug.ShaderDebugger.attach() |> ignore   // before any effect is created; returns IDisposable
```

What happens (`src/Libs/FShade.Debug/Debugger.fs`):
1. `attach` scans all loaded assemblies that reference `FShade.Core`, finds their `.fsproj` by walking up from the assembly location (`Assembly.tryGetProjectData`), parses `Compile` items, target frameworks and `ProjectReference`s (`ProjectData.fs`). Logs `Found <project>` per project.
2. When a consumer registers an effect, each leaf shader's `SourceDefinition` (file/line from the quotation's `DebugRange`) is matched to a method in that project's assembly (PDB-based, `Assembly.tryGetMethodName`). Success is logged with the location; failures print `WARNING: Cannot find shader definition for ...` and the shader stays static.
3. File watchers (`FileWatchers.fs`, activated per project on demand because of Linux inotify limits) mark the project dirty; a worker thread runs `dotnet build <proj> --framework <tfm> --output <tmp> --no-dependencies --no-restore -property:DebugType=None ...` (`Compiler.fs`), loads the new assembly, re-invokes the shader method with the originally applied closure arguments and pushes the new `Effect`/`RaytracingShader`/`ComputeShader` into an `aval` inside a `transact`.
4. Consumers observe the `aval<Effect>` returned by `tryRegisterEffect` and recompile their programs.

Core hook (`src/Libs/FShade.Core/ShaderDebugger.fs`): `ShaderDebugger.initialize`, `tryRegisterEffect`,
`tryRegisterRaytracingEffect`, `tryRegisterComputeShader`, `isInitialized`. Only one debugger instance
can be installed; a second `initialize` is a no-op.

### Rules for Debuggable Shaders

| Rule | Why |
|------|-----|
| Shaders must be functions (`let s () = fragment {...}` or `let s (v : V) = ...`), not values | values compile to static initializers that cannot be re-invoked (`Failed to resolve shader <StartupCode$...>`) |
| Parameter count/types must not change between edits | the debugger re-applies the recorded closure arguments; a changed signature throws |
| Only leaf shaders are reloaded; `Effect.compose` structure is fixed | composition happens in the consumer, which is not rebuilt |
| Changes in referenced projects are ignored (`--no-dependencies`) | keeps rebuilds fast; move shaders to a small project if needed |
| Compute shader interface changes are rejected (warning, update ignored) | input bindings are created once; removing trailing uniform fields is tolerated |
| Project must build with `Debug` config and have PDBs with local paths | source locations come from `DebugRange`; NuGet-shipped shaders resolve to CI paths |

Known issue: `dotnet build --no-dependencies` on net6.0+ looks for referenced assemblies in the
referenced projects' `obj/Debug/<tfm>/ref` folders; build those projects once in Debug if you see
`FS0078: Unable to find the file ... obj\Debug\...\ref\X.dll`.

## 4. Reading Errors

| Message | Meaning / fix |
|---------|---------------|
| `[FShade] shader functions may not access their vertex-input statically` | input is `null` at effect creation; move reads inside the builder |
| `[FShade] cannot call function X since it is not reflectable` / `utility function X is not reflectable` | add `[<ReflectedDefinition>]` (module-level works) |
| `[FShade] unsupported expression Lambda ...` | closures/lambdas/`Application` in shader code; hoist into a `[<ReflectedDefinition>]` function or an `Expr<_>` splice |
| `[FShade] uniform 'X' has conflicting types A and B` | same uniform name annotated with different types in composed shaders |
| `[FShade] positions not written before fragment shader` | no `[<Position>]` output in any primitive stage |
| `[FShade] cannot compose GeometryShader without vertex-count` | use `_triangle<N<k>>` / constant loop bounds |
| `[FShade] could not determine max-vertex-count (using 32)` (warning) | same, non-fatal |
| `[FShade] Ray type name must be constant` | `TraceRay(ray = ...)` argument not constant-foldable |
| `[FShade] Failed inline 'unchanged' expressions` | `unchanged` used in a non-inlinable position (e.g. stored in a mutable) |
| `[GLSL] "x" is a reserved keyword and cannot be used as name` | uniform/attribute semantic collides with a GLSL keyword; rename the semantic |
| `[FShade] effect blob format version mismatch` | stale AOT blob / shader cache |
| glslang errors printed by tests (`ERROR: 0:123: ...`) | the generated GLSL is invalid for the chosen backend/version; check extensions and types |
| `FShadeOnlyInShaderCodeException` | intrinsic executed on the CPU; typically a module-level `let` calling `getGlobalId()`, `discard()`, sampler `.Sample` |

## Gotchas

- Effects are cached by hash: after fixing a shader in a running debugger session the old effect object is still referenced by the scene; only the `aval` produced by `tryRegisterEffect` changes.
- `Effect.ofFunction` is lazy about preprocessing, so a broken shader may only throw when the renderer first uses it. Force early with `effect.Shaders |> ignore` or by compiling in a test.
- When the debugger is attached, `Report.Line(2, ...)` output explains registration; raise the Aardvark.Base report verbosity to see it.
- `Debug.Printf` output arrives through the Vulkan validation layer's debug callback, not stdout.
- The debugger builds into a fresh directory under `<temp>/fshade-debug/`, copies referenced assemblies there first, and deletes everything but the built artifact; build output is logged at `Report.Line(2, ...)` verbosity when a rebuild fails.

## See Also

- `TESTING.md` - compiling shaders in tests, glslang validation
- `EFFECTS.md` - effect identity and caching
- `AOT.md` - blob versions and the `fshadeaot` tool
- aardvark.rendering `ai/DEBUG.md` - backend debug configuration, shader printing, RenderDoc

**Source Files:**
- `src/Libs/FShade.Core/ShaderDebugger.fs` (registration hook)
- `src/Libs/FShade.Debug/Debugger.fs`, `ProjectData.fs`, `Assembly.fs`, `Compiler.fs`, `FileWatchers.fs`, `Framework.fs`
- `src/Libs/FShade.Core/Frontend.fs` (`Debug.Printf`), `src/Libs/FShade.GLSL/Assembler.fs:1963` (`CDebugPrintf`)
- `src/Libs/FShade.Core/SourceDefinition.fs` (closure argument capture)
