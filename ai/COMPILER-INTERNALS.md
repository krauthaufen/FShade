# Compiler Internals

How a quotation becomes GLSL. Read this before touching `Shader.fs`, `Optimizer.fs`, or anything in
`FShade.Imperative`.

## Pipeline

```text
shader function  'v -> Expr<'b>
  |  Shader.Utils.tryExtractExpr / Effect.ofFunction        (Shader.fs:3249, Effect.fs:734)
  v
Expr (F# quotation of the builder block)
  |  Preprocessor.toShaders                                  (Shader.fs:2774)
  |    - evaluates builder calls (Return/Yield/For/While/Bind)
  |    - resolves uniforms, samplers, inputs, primitives, tessellation split
  |    - raytracing/compute specific rewrites
  v
Shader record (per stage), body uses Expr.ReadInput / Expr.WriteOutputs / Expr.CallFunction
  |  Shader.optimize                                         (Shader.fs:3889)
  v
Effect  -->  Effect.compose / Effect.toModule (link, depth range, interpolation)
  v
Module (FShade.Imperative): lazy EntryPoint list
  |  ModuleCompiler.compile backend                          (ModuleCompiler.fs:452)
  |    ValueCompiler: EntryPoint -> CEntryDef, utility functions -> CFunctionDef, constants
  |    TypeCompiler: .NET types -> CStructDef
  v
CModule { types; values }
  |  Assembler.assemble (FShade.GLSL)                        (Assembler.fs:1284)
  v
GLSLShader { code; iface }
```

## Preprocessor (`Shader.fs`, module `Preprocessor`)

A state monad (`Aardvark.Base.Monads.State`) over `Preprocessor.State` (`Shader.fs:972`): collected
`inputs`, `outputs`, `uniforms`, `inputTopology`, `vertexType`, `builder`, `depthWriteMode`,
raytracing payload tables, `localSize`, `storageBufferAccess`, and `shaders` (extra shaders split
off, e.g. the tess control part).

| Function | Role |
|----------|------|
| `preprocessNormalS` (`:1886`) | graphics stages: builder patterns (`BuilderReturn`, `BuilderYield`, `BuilderFor`, `BuilderBind` for `tessellate*`), input/primitive property reads -> `Expr.ReadInput(ParameterKind.Input, ...)`, uniform reads via the `Uniform` active pattern (`Reflection.fs:137`), `for` loop normalization, record outputs via `writeOutputValuesS` |
| `preprocessComputeS` (`:1803`) | compute: `cs_` prefixing of value arguments, `LocalSize` folding, `allocateShared` -> `SharedMemory` uniform |
| `preprocessRaytracingS` (`:1523`) | `TraceRay`/`Callable.Execute`/`Intersection.Report`/`HitObject.*` lowering to the private intrinsics with payload locations, stage validity checks |
| `preprocessMethod` | preprocesses `[<ReflectedDefinition>]` methods on demand; cached in `shaderUtilityFunctions` keyed by `(localSize, MethodBase)` |
| `computeIO` / `usedInputs` | input/uniform discovery used by `Shader.withBody` |

Quotation node vocabulary added by FShade (`src/Libs/FShade.Imperative/Frontend.fs`
`ExpressionExtensions`, `UtilityFunction.fs`): `Expr.ReadInput(kind, type, name, ?index, ?slot)`,
`Expr.WriteOutputs [sem, index, value]`, `Expr.CallFunction(utilityFunction, args)`,
`Expr.ReadRaytracingData`, `Expr.NewFixedArray`, `Expr.Ignore`. Pattern names match
(`ReadInput`, `WriteOutputs`, `CallFunction`, `Uniform`, `StorageBuffer`, `DebugRange`).

`UtilityFunction` (`UtilityFunction.fs:23`): `functionId` (hash of the reflected lambda),
`functionName`, `functionArguments` (tuples flattened, `unit` removed, `this` made mutable),
`functionBody`, `functionIsInline` (`[<Inline>]`, never for `ref` args). `uniqueName` =
`name_hash` is the GLSL function name.

## Optimizer (`src/Libs/FShade.Core/Optimizer.fs`)

Passes run by `Shader.optimize` in this order:

| Pass | Function | What it does |
|------|----------|--------------|
| Inlining | `Optimizer.inlining isSideEffect` | inline copy variables, trivial expressions, input reads, mutable locals never mutated, functions with `[<Inline>]`; `inlining'` takes a predicate to force |
| Raytracing writes | `simplifyRaytracingWrites` | `{ unchanged with f = v }` -> field writes; fails if `unchanged` cannot be inlined |
| Hoisting | `hoistImperativeConstructs` | `let a = (let b = 1 in b * 2)` -> `let b = 1 in let a = b * 2`; brings loops/ifs to statement level so C-style emission works |
| Constant folding | `evaluateConstants'` | evaluates pure sub-expressions with constant operands (including Aardvark vector math, enum ops, `.Normalized`); `isSideEffect` marks methods that must stay |
| Dead code | `eliminateDeadCode'` | removes unused `let`s, writes to unused outputs, calls with unused results unless `isSideEffect`; handles `ref`/address-of; fixpoint over loop-carried variables |
| Constant folding + inlining again | | cleanup after DCE |
| Input lifting | `liftInputs` | moves `ReadInput` out of utility functions into parameters so only `main` reads inputs/uniforms directly |
| Loop unrolling | part of `ConstantFolding` (`Optimizer.fs:1668`) | `for i in unroll a .. b` with constant bounds is expanded into a sequence; the loop variable becomes a constant per copy |

Side effects: per-stage sets in `Shader.fs` (`discard`, `emitVertex`, `restartStrip`, `barrier`) plus
any method with `[<KeepCall>]`. `Optimizer.eliminateDeadCode`/`evaluateConstants` (no prime) assume
all non-unit methods are pure. `Optimizer.CSE` exists but is not wired in. Unit tests drive the
pipeline directly via `Opt.run` (`src/Tests/FShade.Core.Tests/Utilities.fs`) and compare with
`should exprEqual`.

After optimization `Shader.optimize` recomputes inputs/uniforms by presence (not by value) and,
for geometry shaders, derives `max_vertices` with `GeometryInfo.geometryStats` (warning
`could not determine max-vertex-count (using 32)` when loops are not analyzable).

## Imperative IR (`src/Libs/FShade.Imperative`)

| File | Content |
|------|---------|
| `Types.fs` | `ShaderStage`, `ShaderSlot`, `InterpolationMode`, `DepthWriteMode`, `RayFlags`, topologies, `StorageAccess` |
| `Frontend.fs` | `EntryPoint { conditional; entryName; inputs; outputs; uniforms; arguments; raytracingData; body; decorations }`, `EntryDecoration` (`Stages`, `InputTopology`, `OutputTopology`, `OutputVertices`, `LocalSize`, `Invocations`), `Uniform`, `EntryParameter`, `Module` class, quotation extensions |
| `Ast.fs` | C-like AST: `CType` (`CInt`, `CFloat`, `CVector`, `CMatrix`, `CArray`, `CPointer`, `CStruct`, `CIntrinsic`), `CExpr` (arithmetic, `CCall`, `CCallIntrinsic`, `CReadInput`, swizzles, matrix ops, `CNewStruct`, `CDebugPrintf`, ...), `CLExpr` (assignable), `CStatement` (`CDeclare`, `CWrite`, `CFor`, `CWhile`, `CIfThenElse`, `CSwitch`, `CWriteOutput`, `CReturnValue`, ...), `CValueDef` (`CConstant`, `CFunctionDef`, `CEntryDef`, `CConditionalDef`, `CUniformDef`, `CRaytracingDataDef`), `CTypeDef`, `CModule` |
| `Compiler.fs` | `Compiler.Backend` (abstract `TryGetIntrinsicMethod/Ctor/Type`), `toCExprS`/`toCStatementS`/`toCTypeS`, `compileEntryS`, `compileFunctionS`; this is where F# constructs (tuples, unions, records, loops, `match`) are lowered |
| `ModuleCompiler.fs` | `ValueCompiler.ofModule`: walks entry bodies, collects utility functions (`FunctionDefinition.Utility`), reflected methods (`ManagedFunction`), constants; `TypeCompiler.ofTypes` orders struct definitions |
| `UtilityFunction.fs` | `UtilityFunction`, `Expr.SubstituteReads/Writes`, `getAffectedOutputsMap` |
| `Hashing.fs` | `Expr.ComputeHash` (installed by `Serializer.Init()`), structural hashing including method identities and sampler states |
| `Intrinsics.fs` | `IntrinsicAttribute` base, `GLSLIntrinsicAttribute(format, extensions)` |

A backend implements `Compiler.Backend` (`Compiler.fs:24`): the GLSL `Backend` maps sampler/image
types to `CIntrinsic` type names and methods to `CIntrinsic` call templates.

## Serialization and Hashing (`src/Libs/FShade.Core/Serializer.fs`)

- `Serializer.Init()` installs `Expr.ComputeHash` and type/value/expression (de)serializers. Idempotent; every public API calls it.
- `Serializer.Type`, `Serializer.Value`, `Serializer.Expr`, `Serializer.Shader` provide `serialize`/`deserialize` over `BinaryWriter/Reader` with shared state for type tables.
- Hashes are SHA1 over the serialized form; they include method identities, generic arguments, sampler states, texture names, image formats, uniform semantics and types (`src/Tests/FShade.Core.Tests/Hashes.fs`).
- `Effect` blobs: `[version byte][id][EffectDeps][count][(stage, Shader)*]`, `blobFormatVersion = 1` in `Effect.fs`. Bump it when the shader serialization changes; old blobs then fail loudly instead of misparsing.

## Where to Change What

| Goal | Touch |
|------|-------|
| Support a new F#/Aardvark method in shaders | `IntrinsicFunctions.fs` (GLSL table) or make it `[<ReflectedDefinition>]`; add to `Intrinsics.fs` tests |
| New builtin variable (e.g. a `gl_*` input) | `Intrinsics.fs` (semantic + attribute), `Shader.fs` `builtInInputs/Outputs`, `IntrinsicParameters.fs` (name), maybe `builtInExtensions` |
| New DSL construct (builder operation) | `Frontend.fs` builder member + `Preprocessor.BuilderPatterns`/`preprocessNormalS` |
| New uniform scope with special layout | `Assembler.fs` `assembleUniformsS` (match on scope name like `StorageBuffer`, `SharedMemory`, `PushConstant`) |
| New optimizer rule | the matching module in `Optimizer.fs`; test in `FShade.Core.Tests/Optimizer.fs` with `Opt.run` |
| New type category (e.g. a struct kind) | `Ast.fs` `CType.ofType`, `Compiler.fs` constructors (`Constructors` module), `Assembler.fs` type assembly, `UniformBufferLayout.fs` layout |
| Raytracing intrinsic | `Raytracing.fs`/`RaytracingSER.fs` (F# API + private intrinsic), `Shader.fs` `preprocessRaytracingS`, `IntrinsicFunctions.fs` |

## Gotchas

- The preprocessor evaluates parts of the quotation on the CPU (`Expr.TryEval`) to resolve samplers, uniform scopes, `allocateShared` sizes, ray/miss names. Values must be static or constant-foldable.
- `Shader.optimize` runs `Preprocessor.preprocess` **again** after optimization to recompute IO; optimizer output must stay preprocessable.
- Utility function bodies are preprocessed with the caller's `localSize`; the cache key includes it.
- Input lifting means utility functions receive inputs as parameters; a utility function that reads `uniform?X` works, but one that reads a *storage buffer* is handled specially (`computeStorageAccessS`) so access flags propagate.
- DCE treats address-of (`&&x`) and `ref` parameters as potential writes; a historically bug-prone area (`RELEASE_NOTES.md` 5.7.10, 5.7.17, 5.7.18, 5.7.20).
- `Expr.ComputeHash` throws `not initialized: use FShade.Serializer.Init()` if called before init (e.g. in a bare script).
- Generic utility functions are hashed per instantiation; `functionId` differs for `f<int>` and `f<float32>`.
- `Module.Entries` is lazy: exceptions from linking/preprocessing surface when the assembler forces it.

## See Also

- `EFFECTS.md` - the public API around `Shader`/`Effect`
- `GLSL-BACKEND.md` - assembler configuration and output
- `TESTING.md` - optimizer and serializer test helpers

**Source Files:**
- `src/Libs/FShade.Core/Shader.fs` (Preprocessor, Shader module)
- `src/Libs/FShade.Core/Optimizer.fs`
- `src/Libs/FShade.Core/Reflection.fs` (quotation patterns: `Uniform`, `StorageBuffer`, `SamplerType`, `ImageType`)
- `src/Libs/FShade.Core/Serializer.fs`
- `src/Libs/FShade.Imperative/*.fs`
