# Shader DSL

How to write graphics-stage shaders in FShade. Shaders are F# functions whose body is a
computation expression (`vertex { ... }`, `fragment { ... }`, ...) that yields a quotation.
Compute and raytracing have their own docs (`COMPUTE.md`, `RAYTRACING.md`); turning shader
functions into `Effect` values is in `EFFECTS.md`.

## Stage Builders

| Builder | Input type | Body ends with | GLSL stage |
|---------|-----------|----------------|------------|
| `vertex { }` | vertex record `'v` | `return v'` | vertex |
| `tessellation { }` | `Patch<3 N, 'v>`, `Patch<4 N, 'v>`, `Patch3<'v>`, `Patch4<'v>` | `let! coord = tessellateTriangle ...` then `return v'` | tess control + tess eval (one builder, split at `let!`) |
| `triangle { }` / `line { }` / `point { }` | `Triangle<'v>`, `Line<'v>`, `Point<'v>`, `LineAdjacency<'v>`, `TriangleAdjacency<'v>` | `yield v'` (any number), `restartStrip()` | geometry (triangle strip / line strip / points) |
| `_triangle<'d> { }` etc. | same | same | geometry with explicit `max_vertices` = `typeSize<'d>` (e.g. `_triangle<N<6>>`) |
| `fragment { }` | vertex record `'v` | `return v'` or `return color` | fragment |
| `compute { }` | function arguments | statements | compute (see `COMPUTE.md`) |
| `raygen`, `miss`, `anyHit`, `closestHit`, `callable`, `intersection` | raytracing input records | see `RAYTRACING.md` | raytracing |

All builders are in `src/Libs/FShade.Core/Frontend.fs` (`ShaderBuilders` module). Builder bodies
are quoted; the builder's `Run`/`Quote` members make `vertex { ... }` evaluate to an `Expr<'b>`.

```fsharp
open Aardvark.Base
open FShade

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>]    c   : V4f
        [<Semantic("Normals")>] n : V3f
    }

let trafo (v : Vertex) =
    vertex {
        let wp = uniform.ModelTrafo * v.pos
        return { v with pos = uniform.ViewProjTrafo * wp }
    }

let shade (v : Vertex) =
    fragment {
        return V4f(v.n * 0.5f + 0.5f, 1.0f)
    }
```

## Vertex Records and Semantics

Inputs and outputs are F# records (anonymous records work for outputs). Each field is a shader
variable identified by its **semantic** string:

| Attribute | Semantic | Notes |
|-----------|----------|-------|
| none | field name | `foo : V4f` is the attribute/varying `foo` |
| `[<Semantic("Name")>]` | `Name` | explicit name; this is what Aardvark binds buffers/uniforms by |
| `[<Position>]` | `Positions` | written to `gl_Position` by the last primitive stage; required before a fragment shader |
| `[<Color>]` | `Colors` | default fragment output slot 0 |
| `[<Depth>]`, `[<Depth(DepthWriteMode.OnlyLess)>]` | `Depth` | fragment `gl_FragDepth`; mode emits conservative-depth layout when the backend supports it |
| `[<Interpolation(InterpolationMode.Flat \|\|\| InterpolationMode.Centroid)>]` | | flags: `NoPerspective`, `Flat`, `Centroid`, `Sample`, `PerPatch` (combine with `\|\|\|`) |
| builtin attributes | see table below | read-only system values |

Semantic string constants live in `src/Libs/FShade.Core/Intrinsics.fs` (`Intrinsics.Position = "Positions"`,
`Intrinsics.Color = "Colors"`, ...). Custom attributes are one-liners:

```fsharp
type WorldPositionAttribute() = inherit SemanticAttribute("WorldPosition")
```

Builtin inputs/outputs per stage (`Shader.fs` `builtInInputs`/`builtInOutputs`; GLSL names in
`src/Libs/FShade.GLSL/IntrinsicParameters.fs`):

| Stage | Inputs | Outputs |
|-------|--------|---------|
| Vertex | `VertexId`, `InstanceId` | `PointSize`, `ClipDistance` |
| TessControl | `PointSize`, `ClipDistance`, `PatchVertices`, `PrimitiveId`, `InvocationId` | `TessLevelInner`, `TessLevelOuter` |
| TessEval | `PointSize`, `ClipDistance`, `TessCoord`, `PatchVertices`, `PrimitiveId`, `TessLevelInner/Outer` | `PointSize`, `ClipDistance` |
| Geometry | `SourceVertexIndex`, `PointSize`, `ClipDistance`, `PrimitiveId`, `InvocationId` | `PointSize`, `ClipDistance`, `Layer`, `ViewportIndex` |
| Fragment | `FragCoord`, `PointCoord`, `FrontFacing`, `SampleId`, `SamplePosition`, `SampleMask`, `ClipDistance`, `PrimitiveId`, `Layer`, `ViewportIndex` | `Depth`, `SampleMask` |

Use them as record fields: `[<FragCoord>] fc : V4f`, `[<PrimitiveId>] primId : int`. Types are
checked (`[FShade] invalid system-input type ...`).

**Return conventions** (`Shader.fs` `writeOutputValuesS`):
- `return record` writes every field as an output under its semantic.
- `return scalar/vector/color` writes the stage default: `Colors` in `fragment`, `Positions` elsewhere.
- Integral and 64-bit float outputs are implicitly `flat`.
- Stage boundaries convert compatible types automatically (`V3f` -> `V4f` appends `w = 1.0`, `V4f` -> `V3f` drops `w`, `C4f` <-> `V4f`, ...; table in `Shader.fs` `typeConversions`).

## Uniforms

`uniform` is the global `UniformScope`; the dynamic `?` operator (`src/Libs/FShade.Core/Core.fs`)
reads a uniform by name. A type annotation is required at the use site unless wrapped in an
extension member, which is the idiomatic form:

```fsharp
type UniformScope with
    member x.ModelTrafo : M44f = x?PerModel?ModelTrafo       // uniform "ModelTrafo" in buffer "PerModel"
    member x.Time       : float32 = x?Time                   // uniform in the "Global" buffer
    member x.Colors     : V4f[] = x?StorageBuffer?Colors     // SSBO (runtime-sized)
    member x.Weights    : Arr<N<8>, float32> = x?Weights     // fixed-size array in a uniform buffer
    member x.Out        : Image2d<Formats.rgba32f> = x?Out   // storage image
    member x.Scene      : Scene = scene { accelerationStructure x?Scene }

let f (v : Vertex) = fragment { let t : float32 = uniform?Time in return v.c * t }
```

| Scope name | Meaning |
|------------|---------|
| (none) / `Global` | regular uniform; backends with `createUniformBuffers` put it in the `Global` uniform buffer |
| any other name (`PerModel`, `PerView`, ...) | uniform buffer with that name; Aardvark.Rendering groups update frequency by it |
| `StorageBuffer` | shader storage buffer object; `T[]` element type, read/write access is inferred per shader (`readonly`/`writeonly`/none) |
| `Arguments` | compute argument buffer (see `COMPUTE.md`) |
| `SharedMemory` | internal, used by `allocateShared` and `allocateSharedValue` |
| `PushConstant` | push constant block when `Config.pushConstants` is on (Vulkan preset) |

Storage buffers support element writes (`uniform.Colors.[i] <- v`, `uniform.Data.[i].Field.X <- 0.0f`),
structs as elements, fixed-size `Arr<N<n>, T>` elements, 8/16/64-bit element types, and indexing
with `int` or `uint`. A `T[][]` storage buffer is an unbounded (bindless) array of SSBOs.
Uniform values must be constant per draw; FShade does not know about adaptive values.

**Memory qualifiers** (`src/Libs/FShade.Core/Core.fs`): `[<Coherent>]`, `[<Volatile>]` and
`[<Restrict>]` on a `UniformScope` extension member (or on a compute shader parameter, see
`COMPUTE.md`) emit the GLSL `coherent`/`volatile`/`restrict` qualifiers. They are only allowed on
images and storage buffers (`[FShade] memory qualifiers are only allowed on images and storage
buffers` otherwise). Qualifiers of the same uniform declared in several shaders are unioned when
composing, and they are part of the effect hash. Visibility of coherent writes to other
invocations still requires a barrier: `memoryBarrier()`, `memoryBarrierBuffer()`,
`memoryBarrierImage()`, `memoryBarrierShared()` or `groupMemoryBarrier()` (`Frontend.fs`).

```fsharp
type UniformScope with
    [<Coherent>]           member x.Heads : int[] = x?StorageBuffer?Heads
    [<Coherent; Restrict>] member x.Accum : UIntImage2d<Formats.r32ui> = x?Accum
```

## Samplers and Images

Samplers are module-level values built with a sampler builder; they must be static so the
preprocessor can evaluate them (`Reflection.fs` `(|Uniform|_|)`). The texture is a uniform name,
the sampler state is recorded in the effect interface (`GLSLSampler.samplerTextures`), not in GLSL.

```fsharp
let diffuse =
    sampler2d {
        texture uniform?DiffuseColorTexture
        filter Filter.MinMagMipLinear
        addressU WrapMode.Wrap
        addressV WrapMode.Wrap
        // addressW, maxAnisotropy, borderColor, maxLod, minLod, mipLodBias, filterReduction, comparison
    }

let samplers =
    sampler2d { textureArray uniform?TextureArray 28 }   // Sampler2d[] of 28; name TextureArray0..27
    // count -1 = unbounded (runtime-sized, descriptor indexing); index with samplers.[i]

let shadow = sampler2dShadow { texture uniform?ShadowMap; comparison ComparisonFunction.LessOrEqual }
```

Builders (`src/Libs/FShade.Core/Samplers.fs`, generated by `SamplerCodeGen.fsx`):
`sampler{1d,2d,3d,Cube}`, `...Array`, `...Shadow`, `...ArrayShadow`, `sampler2dMS`, `sampler2dArrayMS`,
and `intSampler*` / `uintSampler*` variants (value type `V4i` / `V4ui`). Images are generic in a
format type and are declared as uniforms: `Image2d<Formats.rgba8>`, `IntImage2d<Formats.r32i>`,
`UIntImage3d<Formats.r32ui>`, `Image2dArray<_>`, `ImageCube<_>`, `Image2dMS<_>`, ...

| Member (Sampler2d) | GLSL |
|--------------------|------|
| `Sample(coord)`, `Sample(coord, lodBias)` | `texture` |
| `SampleOffset`, `SampleProj`, `SampleLevel`, `SampleLevelOffset`, `SampleGrad` | `textureOffset`, `textureProj`, `textureLod`, `textureLodOffset`, `textureGrad` |
| `Gather(coord, comp)`, `GatherOffset` | `textureGather` |
| `Read(coord, lod)`, `s.[coord, lod]` | `texelFetch` |
| `Size`, `GetSize(level)`, `MipMapLevels`, `QueryLod` | `textureSize`, `textureQueryLevels`, `textureQueryLod` |
| `Samples` (MS only) | `textureSamples` |

| Member (Image2d) | GLSL |
|------------------|------|
| `Load(coord)`, `img.[coord]` | `imageLoad` |
| `Store(coord, v)`, `img.[coord] <- v` | `imageStore` |
| `Size`, `Samples` | `imageSize`, `imageSamples` |
| `AtomicAdd/Min/Max/And/Or/Xor/Exchange/CompareExchange` (int/uint images) | `imageAtomic*` |

Array/cube/shadow variants take an extra layer or reference-depth argument (see the tests in
`src/Tests/FShade.GLSL.Tests/Samplers.fs` and `Images.fs` for every overload).

## Geometry Shaders

```fsharp
let extrude (t : Triangle<Vertex>) =
    triangle {
        yield t.P0
        yield t.P1
        yield { t.P2 with c = V4f.IOII }
        restartStrip()                 // or endPrimitive()
        for i in 0 .. 2 do yield t.[i]
    }
```

- Input primitives: `Point<'v>` (`.Value`), `Line<'v>` (`.P0 .P1`), `Triangle<'v>` (`.P0 .P1 .P2`),
  `LineAdjacency<'v>` (`.Left .P0 .P1 .Right`), `TriangleAdjacency<'v>` (`.P0 .N01 .P1 .N12 .P2 .N20`); all support `.[i]`.
- `max_vertices` is computed from the body (`GeometryInfo.geometryStats`); use `_triangle<N<6>> { }` when it cannot be (loops with non-constant bounds fail with `cannot compose GeometryShader without vertex-count`).
- Fields not present in the yielded record are copied from the input vertex named by
  `[<SourceVertexIndex>] vi : int` (semantic `GeometrySourceVertexIndex`); without it, composing a fragment shader that needs an extra attribute fails with `cannot add output X to GeometryShader since no SourceVertexIndex was available`.
- `Layer` / `ViewportIndex` outputs are regular record fields with those attributes.
- `Effect.tryReplaceGeometry` can lower a constant-count geometry shader into a vertex shader for backends without GS (see `EFFECTS.md`).

## Tessellation

```fsharp
let tess (patch : Patch<3 N, Vertex>) =
    tessellation {
        let inner = 4.0f
        // everything above runs in the tess control shader (once per patch)
        let! coord = tessellateTriangle inner (4.0f, 4.0f, 4.0f)   // (l01, l12, l20) outer levels
        // everything below runs in the tess eval shader, coord : V3f barycentrics
        let pos = patch.[0].pos * coord.X + patch.[1].pos * coord.Y + patch.[2].pos * coord.Z
        return { patch.[0] with pos = pos }
    }
```

`tessellateQuad (lu, lv) (l01, l30, l23, l12)` yields `V2f` coordinates. Outputs written before
`let!` become per-patch. `Patch<'d, 'v>` takes a type-level size (`3 N`, `4 N`); `Patch3`/`Patch4`
are fixed variants with `.P0 .P1 .P2 (.P3)` and `.Interpolate`. Effects containing tessellation
cannot be layered (`Effect.toLayeredEffect`), and two tessellation shaders cannot be composed
with each other (`Shader.compose2` has no tess+tess case).

## Utility Functions and Custom Intrinsics

| Mechanism | Use |
|-----------|-----|
| `[<ReflectedDefinition>]` on a `let`, module, type or member | required for any function/method/constructor called from a shader; emitted as a GLSL function |
| `[<Inline>]` | force the optimizer to inline the function at call sites (default: not inlined, except trivial bodies) |
| `[<KeepCall>]` | mark a `unit`-returning function as a side effect so dead-code elimination keeps it |
| Specialization for constant arguments | a call with a constant argument (literal, enum, constant struct/union) gets its own variant of the function when the constant resolves an `if`/`match` in the body (`[<Inline>]` is not needed for that); variants are named `name_<hash>`, calls with equal control flow share one variant, and a parameter that is still used as a value stays a parameter. `[<Specialize>]` on a parameter folds the constant into every use (`SpecializationMode.Full`); `[<Specialize(SpecializationMode.Off)>]` on a parameter or function disables it; a parameter attribute overrides the function attribute. Tests: `Specialization.fs` |
| `[<GLSLIntrinsic("fmt({0}, {1})")>]` + body `onlyInShaderCode "name"` | map an F# function to a GLSL expression; `{n}` are arguments; optional required extensions: `[<GLSLIntrinsic("f({0})", "GL_EXT_foo")>]` |
| `onlyInShaderCode "name"` | placeholder body; raises `FShadeOnlyInShaderCodeException` if called on the CPU |
| `&&x` and `ref` parameters | pass by reference (`inout`); when such a function is `[<Inline>]`, the reference is substituted so the body writes the caller's variable directly |
| `Arr<_,_>` / array parameters | passed by value (`T[n]`), as in GLSL; writes to the parameter affect a copy and the call is removed as dead code if nothing else is observable. To fill an array in a function take `Arr<N<n>, T> ref` and call with `&&arr` |
| `unchanged<'T>` | raytracing payload helper, see `RAYTRACING.md` |

```fsharp
[<ReflectedDefinition>]
let lerp3 (a : V3f) (b : V3f) (c : V3f) (w : V3f) = a * w.X + b * w.Y + c * w.Z

[<GLSLIntrinsic("gl_FrontFacing")>]
let frontFacing () : bool = onlyInShaderCode "frontFacing"

// arrays are values: take a ref to modify the caller's array (-> inout vec4 array[10])
[<ReflectedDefinition>]
let fillArray (array : Arr<N<10>, V4f> ref) (denom : float32) =
    for i in 0 .. 9 do
        array.Value.[i] <- V4f(float32 i / denom)

// usage inside a shader: let array = Arr<N<10>, V4f>() in fillArray &&array 10.0f

[<Struct; ReflectedDefinition>]
type Ray = val mutable O : V3f; val mutable D : V3f; new(o, d) = { O = o; D = d }
```

Classes, structs, records, tuples and unions defined in F# are supported, including constructors,
instance methods, properties and indexers (`src/Tests/FShade.GLSL.Tests/StructsAndClasses.fs`).
Generic utility functions are instantiated per use; this includes `let inline` functions with SRTP
constraints (`x + y`, `LanguagePrimitives.GenericOne`, `(^a : (member Length : float32) x)`, ...),
which are compiled from the witness-passing variant the F# compiler emits (tests:
`GenericFunctions.fs`). Builtin math comes from Aardvark.Base
(`Vec.dot`, `Vec.normalize`, `Fun.Sin`, `clamp`, `lerp`, `M44f.TransformPos`, swizzles `v.XYZ`, ...);
the full mapping is the `IntrinsicFunction` table in `src/Libs/FShade.GLSL/IntrinsicFunctions.fs`
and the 100+ tests in `src/Tests/FShade.GLSL.Tests/Intrinsics.fs`. Pack/unpack helpers
(`packUnorm4x8`, ...), `Bitwise.*` and `Atomic.*` are in `Frontend.fs`. The pack/unpack helpers are GLSL
intrinsics in shaders; their F# bodies run on the CPU and produce the same bit patterns as GLSL (first
component in the least significant bits, snorm scaled by 32767 / 127), so they can be used to prepare
or read back packed data (tests: `FShade.Core.Tests/Packing.fs`).

## Types

| F# | GLSL | Notes |
|----|------|-------|
| `bool` | `bool` | |
| `int8`/`uint8`, `int16`/`uint16` | `int8_t`/`uint8_t`, `int16_t`/`uint16_t` | needs `GL_EXT_shader_explicit_arithmetic_types_*` / 8-16bit storage (Vulkan) |
| `int`/`uint` | `int`/`uint` | |
| `int64`/`uint64` | `int64_t`/`uint64_t` | `GL_ARB_gpu_shader_int64` or EXT |
| `float16` | `float16_t` | |
| `float32` | `float` | the normal shader float |
| `float` (double) | `double` | since 5.7.0; `V4d`/`M44d` are `dvec4`/`dmat4` |
| `V2f..V4f`, `V2i..V4i`, `V2ui..V4ui`, `V3l`, `V4d` | `vec`, `ivec`, `uvec`, `i64vec`, `dvec` | |
| `M22f..M44f`, `M34f`, `M33d`... | `mat2..mat4`, `mat3x4`, `dmat3` | row-major in Aardvark; `reverseMatrixLogic` flips operand order so uploads stay row-major |
| `C3f`, `C4f`, `C3b`, `C4b`, `C4us`, `C4ui` | vectors of the element type | color types convert to/from vectors |
| `Arr<N<4>, T>` | `T[4]` | fixed-size arrays; `.[i]`, `.Get(i)`, `for x in arr` |
| `T[]` | `T[]` in a storage buffer | only as uniforms/arguments |
| records, anonymous records, tuples, structs, classes | `struct` + constructor function | no inheritance, no recursion |
| unions | `struct { int tag; ... all case fields ... }` | pattern matching supported |
| enums (any underlying type) | the underlying integer | `Enum.HasFlag`, `LanguagePrimitives.EnumToValue`/`EnumOfValue`, `enum` work |

Function types, closures, `seq`, `list`, `option`, `string` (except `Debug.Printf` format) and
object-typed values are not supported.

## Control Flow

- `if/elif/else`, `match` on ints, enums and union cases, `let mutable`, `while`, early `return` inside branches
- `for i in a .. b`, `for i in a .. step .. b`, `for i = a to b`, `downto`, `for x in arr` (`Arr<_,_>` or fixed arrays); `int32` and `float32` loop variables
- `for i in unroll 0 .. 5 do` (or `for i = unroll 0 to 5 do`) unrolls at compile time; a `match i with` inside an unrolled loop is folded per iteration
- `discard()` in fragment shaders; `ddx`/`ddy`/`ddxFine`/`ddxCoarse` derivatives
- `beginInvocationInterlock mode` / `endInvocationInterlock()` delimit a fragment-shader critical section (see below)
- `Debug.Printf("P = %v4f", v.pos)` / `Debug.Printfn` emit `debugPrintfEXT` when `GL_EXT_debug_printf` is available (Vulkan validation layers), otherwise a comment

## Fragment Shader Interlock

`GL_ARB_fragment_shader_interlock` support (`src/Libs/FShade.Core/Frontend.fs`): invocations
covering the same pixel (or sample) execute the code between the two calls mutually exclusively,
which makes read-modify-write on images/storage buffers safe (order-independent transparency etc.).

```fsharp
type UniformScope with
    member x.Accum : Image2d<Formats.rgba32f> = x?Accum

type Fragment = { [<FragCoord>] fc : V4f; [<Color>] c : V4f }

let accumulate (v : Fragment) =
    fragment {
        beginInvocationInterlock FragmentInterlockMode.PixelOrdered
        let old = uniform.Accum.[V2i v.fc.XY]
        uniform.Accum.[V2i v.fc.XY] <- old + v.c
        endInvocationInterlock()
        return v.c
    }
```

| `FragmentInterlockMode` | GLSL qualifier |
|-------------------------|----------------|
| `PixelOrdered` | `layout(pixel_interlock_ordered) in;` (primitive order) |
| `PixelUnordered` | `layout(pixel_interlock_unordered) in;` |
| `SampleOrdered` | `layout(sample_interlock_ordered) in;` (only invocations sharing a sample) |
| `SampleUnordered` | `layout(sample_interlock_unordered) in;` |

Rules, checked when the effect is linked (`Shader.Interlock` in `Shader.fs`):
- fragment shaders only; the mode must be a compile-time constant (literal or closure argument)
- exactly one `begin` and one `end`, `begin` first, both as top-level statements of the shader body: not inside `if`/loops and not inside `[<ReflectedDefinition>]` helpers
- two fragment shaders that both use interlock cannot be composed; a plain fragment shader can be composed before or after an interlocked one (its code runs outside the critical section when the interlocked shader comes first)
- the mode is reported as `GLSLShaderDecoration.GLSLFragmentInterlock` in the program interface; Vulkan consumers must enable `VK_EXT_fragment_shader_interlock`

## Early Fragment Tests

`layout(early_fragment_tests) in;` makes depth and stencil tests run before the fragment shader
(needed for correct image/storage-buffer side effects of occluded fragments, and the usual companion
of interlock). Two equivalent ways to request it:

```fsharp
[<EarlyFragmentTests>]                       // attribute on the shader function ...
let accumulate (v : Fragment) =
    fragment { ... }

let accumulate2 (v : Fragment) =
    fragment {
        earlyFragmentTests()                 // ... or a marker call at the top level of the body
        ...
    }
```

- The attribute only works when the `fragment { }` block is directly in the attributed function (the
  builder records the method like `[<LocalSize>]` for compute); use the marker call for local or
  nested shader functions. The attribute is part of the effect hash.
- The marker is fragment-only; its position does not matter (control flow and utility functions are fine) and it is removed from the generated code.
- Composition unions the request; the qualifier is emitted once.
- A shader that also writes `[<Depth>]` compiles but logs a warning: with early tests the written
  depth has no effect. Conservative depth (`[<Depth(DepthWriteMode.OnlyLess)>]` etc.) is the way to
  combine early-z with a depth output.
- Depth/stencil are written before the shader runs, so a later `discard()` no longer prevents the write.
- GLSL < 4.20 gets `#extension GL_ARB_shader_image_load_store` automatically.

## Gotchas

- **Input access outside the builder**: `Effect.ofFunction` calls the shader function with `null`; `let x = v.pos in vertex { ... }` throws `[FShade] shader functions may not access their vertex-input statically`.
- **Array parameters are copies**: `let fill (arr : Arr<N<4>, int>) = arr.[0] <- 1` writes a GLSL copy, so `fill a` has no effect and is eliminated. Use a `ref` parameter and `&&a` (see Utility Functions); `let b = a` copies as well.
- **Closure arguments**: extra parameters (`let shader (radius : float32) (v : Vertex) = ...`) are fine and are baked into the effect hash; lambdas/`fun` values inside the body are `[FShade] unsupported expression`.
- **Missing `[<ReflectedDefinition>]`**: `[FShade] cannot call function X since it is not reflectable` / `utility function X is not reflectable`.
- **Samplers must be top-level**: a sampler created inside the shader body is not a static value and cannot be reflected into a uniform.
- **Uniform type must be annotated**: `uniform?Foo` without a type annotation infers `obj` and fails later; conflicting annotations across shaders fail at composition with `uniform 'Foo' has conflicting types`.
- **The uniform's identity is the string after `?`, not the member name**: `member x.A : int[] = x?StorageBuffer?Data` and `member x.B : int[] = x?StorageBuffer?Data` are the *same* uniform `Data`. They must agree on type and scope (`has conflicting types/values` otherwise); memory qualifiers of such aliases are unioned. Two members with different names only give two uniforms if the strings after `?` differ.
- **`float` literals are doubles**: `1.0 * v.pos` with `v.pos : V4f` does not compile in F#; use `1.0f`. Doubles in shaders require `GL_ARB_gpu_shader_fp64` and are slow.
- **Reserved GLSL names** (`input`, `output`, `sample`, `union`, names with `'` or `@`) are mangled by `src/Libs/FShade.GLSL/Keywords.fs`; do not rely on exact variable names in generated code.
- **`onlyInShaderCode` intrinsics evaluated eagerly**: do not call intrinsics in `let` bindings outside the quotation (e.g. module-level `let s = getGlobalId()` throws at module init).
- **Shared record types across stages**: a field read by the fragment shader must be written by an earlier stage or exist as a vertex attribute; otherwise it is passed through from the vertex input of that name.
- **Geometry shaders need `SourceVertexIndex`** to receive attributes added by later-composed vertex shaders.

## See Also

- `EFFECTS.md` - turning shader functions into effects, composition, linking
- `COMPUTE.md` - compute shader DSL and interface
- `RAYTRACING.md` - raytracing stages, payloads, SBT layout
- `GLSL-BACKEND.md` - how types, uniforms and samplers appear in GLSL
- `TESTING.md` - compiling a shader in a test to inspect the GLSL

**Source Files:**
- `src/Libs/FShade.Core/Frontend.fs` (builders, primitives, intrinsics, `Debug`, `Atomic`, `Bitwise`)
- `src/Libs/FShade.Core/Core.fs` (attributes, `UniformValue`, `?` operator, sampler builder base)
- `src/Libs/FShade.Core/Intrinsics.fs` (semantic names, builtin attributes)
- `src/Libs/FShade.Core/Samplers.fs` (generated sampler/image types), `SamplerState.fs`
- `src/Libs/FShade.Core/Shader.fs` (`Preprocessor`: builder patterns, `writeOutputValuesS`, `builtInInputs`)
- `src/Libs/FShade.Imperative/Types.fs` (`InterpolationMode`, `DepthWriteMode`, `ShaderStage`, topologies)
- `src/Libs/FShade.Imperative/Intrinsics.fs` (`GLSLIntrinsicAttribute`)
