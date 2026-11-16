# Compute Shaders

Compute shaders are F# functions whose parameters are the shader's resources and whose body is a
`compute { ... }` block. `ComputeShader.ofFunction` turns them into a `ComputeShader` with a
reflected interface; `ComputeShader.toModule` + `ModuleCompiler.compileGLSL` produce GLSL.

## Writing a Compute Shader

```fsharp
open Aardvark.Base
open FShade

type UniformScope with
    member x.Factor : float32 = x?Factor               // regular uniform (Global buffer)
    member x.Output : V4f[]   = x?StorageBuffer?Output // storage buffer declared via uniform scope

[<LocalSize(X = 64)>]
let scale (n : int) (input : V4f[]) (output : V4f[]) =
    compute {
        let id = getGlobalId().X
        if id < n then
            output.[id] <- input.[id] * uniform.Factor
    }

let shader = ComputeShader.ofFunction (V3i(1024, 1024, 64)) scale   // arg = max local size of the device
let glsl   = shader |> ComputeShader.toModule |> ModuleCompiler.compileGLSLVulkan
```

Function parameters become shader inputs by name:

| Parameter type | Becomes | Interface name |
|----------------|---------|----------------|
| `T[]` (array) | storage buffer, access (`readonly`/`writeonly`/rw) inferred from the body | `name` (no prefix) |
| `Arr<N<k>, T>` | fixed-size array in the `Arguments` uniform buffer | `cs_name` |
| scalar, vector, matrix, bool, enum | field of the `Arguments` uniform buffer | `cs_name` |
| `Image2d<fmt>`, `IntImage1dArray<fmt>`, ... | storage image | `cs_name` |
| `Expr<'a -> 'b>` | spliced into the body with `(%op) a b`; part of the hash, not an input | none |
| extra value args (e.g. `scanSize : int` used only in constants) | baked constants | none |

The `cs_` prefix on non-buffer arguments is a legacy quirk kept for backwards compatibility
(`Shader.fs` `preprocessComputeS`, test `Compute.fs` `Arguments`). Arguments can also be read via
`uniform?Arguments?name` inside the body (`Aardvark.GPGPU` kernels do this).

### Builtins

| Function (`Frontend.fs`) | GLSL |
|--------------------------|------|
| `getGlobalId() : V3i` | `gl_GlobalInvocationID` |
| `getLocalId() : V3i`, `getLocalIndex() : int` | `gl_LocalInvocationID`, `gl_LocalInvocationIndex` |
| `getWorkGroupId() : V3i`, `getWorkGroupCount() : V3i`, `getWorkGroupSize() : V3i` | `gl_WorkGroupID`, `gl_NumWorkGroups`, `gl_WorkGroupSize` |
| `LocalSize : V3i` | constant-folded to the attribute value |
| `barrier()` | `barrier()`; treated as a side effect |
| `allocateShared<'a> size : 'a[]` | `shared 'a name[size]`; `size` must be a compile-time constant, result must be bound with `let` and only indexed |
| `Atomic.Add(&&buffer.[i], 1)`, `Atomic.Min/Max/And/Or/Xor/Exchange/CompareExchange` | `atomicAdd(...)` etc. on `int`/`uint` storage locations |
| `img.AtomicAdd(coord, v)` | `imageAtomicAdd` (int/uint images) |
| `Debug.Printf` | `debugPrintfEXT` (Vulkan with validation layers) |

Samplers are module-level values as in graphics shaders; their state is reported in
`csSamplerStates`/`csTextureNames` and `GLSLSampler.samplerTextures`.

### Local Size

`[<LocalSize(X = 64, Y = 1, Z = 1)>]` on the shader function sets `layout(local_size_x = ...)`.
Use `MaxLocalSize` (`Int32.MaxValue`) for a dimension to take the device maximum passed to
`ComputeShader.ofFunction maxLocalSize`. Missing attribute: warning `compute shader without
local-size` and `1x1x1`. The attribute is read from `Expr.Method` metadata that `ComputeBuilder.Run`
attaches (`Frontend.fs:498`), so the body must be a `compute { }` block directly in the function.

## ComputeShader API

| Member / function | Meaning |
|-------------------|---------|
| `ComputeShader.ofFunction (maxLocalSize : V3i) f` | reflect, preprocess (lazy), cache by hash of body + local size |
| `ComputeShader.ofExpr localSize expr` | from a quotation |
| `csId`, `csLocalSize`, `csMethod`, `csSourceDefinition` | identity / debugger metadata |
| `csBuffers : Map<string, ComputeBuffer>` (`contentType`, `access`), `csBuffers2` (keeps the array type) | storage buffers |
| `csImages : Map<string, ComputeImage>` | storage images with format, dimension, arrayness |
| `csUniforms : Map<string, UniformParameter>` | arguments and other uniforms (with their scope) |
| `csSamplerStates`, `csTextureNames` | `(samplerName, index) -> SamplerState / texture semantic` |
| `csShared` | always empty today (shared memory is emitted via uniforms of scope `SharedMemory`) |
| `csBody : Expr` | optimized body |
| `ComputeShader.toEntryPoint` / `toModule` | imperative module with `EntryDecoration.LocalSize` |

The optimizer pipeline for compute (`ComputeShader.fs` `ofExprInternal`) is inlining, statement
hoisting, constant folding, dead-code elimination, constant folding again, inlining again, input
lifting, then a second preprocess pass. Only
calls to `barrier`, `[<KeepCall>]` functions and `Atomic`/image atomics survive as side effects
when their result is unused.

## Interface in GLSL

Compiling with `glslVulkan` yields a `GLSLShader` whose `iface` has:
- `uniformBuffers.["Arguments"]` with `cs_*` fields (std140 offsets in `ubFields`),
- `storageBuffers.[name]` with `ssbAccess` and `ssbType = DynamicArray(elem, stride)` (std430),
- `images`, `samplers`, and `shaders = GLSLProgramShaders.Compute iface` whose `shaderDecorations`
  contains `GLSLLocalSize`.

```glsl
#version 460
layout(local_size_x = 64, local_size_y = 1, local_size_z = 1) in;
layout(set = 0, binding = 0, std140) uniform Arguments { int cs_n; };
layout(set = 0, binding = 1, std430) readonly buffer inputBuffer { vec4 input_[]; };
layout(set = 0, binding = 2, std430) writeonly buffer outputBuffer { vec4 output_[]; };
```

(Names are illustrative; reserved words are mangled, see `GLSL-BACKEND.md`.)

Consumers bind inputs by the F# parameter names (the `cs_` prefix is an interface detail they
translate). Larger real-world kernels (scan, radix sort, jpeg) using `allocateShared`, `barrier`,
generic element types and spliced operators live in aardvark.rendering's `Aardvark.GPGPU` project.
Hot reload of compute shaders via `FShade.Debug` is supported but interface changes are rejected
(see `DEBUG.md`).

## Gotchas

- `allocateShared` must be `let mem : T[] = allocateShared n` at statement level with a constant `n`; anything else fails with `non-static call to allocateShared` / `could not evaluate size for allocateShared`, and using `mem` as a value (not indexed) fails with `cannot use shared memory as value`.
- A `T[]` parameter is always a storage buffer; a storage buffer declared via `uniform?StorageBuffer?X` works too, but a fixed-size `Arr<_,_>` parameter is a uniform-buffer field, never an SSBO.
- Storage buffer access flags are inferred per shader; a buffer only written is `writeonly` and cannot be read back in the same shader without FShade seeing the read.
- Local size is part of the hash: the same function with different `maxLocalSize` yields different shaders when `MaxLocalSize` is used.
- `.NET SDK 10.0.401` Debug builds broke `ComputeBuilder.Run` method detection; fixed in 5.7.18 by walking one stack frame up. If local size is `1x1x1` unexpectedly, check `Expr.Method`.
- There is no separate "compute effect"; `Effect.compose` does not apply. One function = one program.
- Generic kernels (`'a[]`) need the element type fixed at `ComputeShader.ofFunction` time (e.g. `scanKernel<int> ...` via a typed `Expr` argument); FShade resolves `'a` from the closure.

## See Also

- `SHADER-DSL.md` - types, uniforms, samplers, utility functions shared with compute
- `GLSL-BACKEND.md` - backend presets and the program interface
- `DEBUG.md` - `Debug.Printf` and hot reload
- `TESTING.md` - `GLSL.shouldCompileCompute`

**Source Files:**
- `src/Libs/FShade.Core/ComputeShader.fs`
- `src/Libs/FShade.Core/Frontend.fs` (compute builtins, `LocalSizeAttribute`, `ComputeBuilder`)
- `src/Libs/FShade.Core/Shader.fs` (`preprocessComputeS`)
- `src/Tests/FShade.GLSL.Tests/Compute.fs`, `StorageAccess.fs`
