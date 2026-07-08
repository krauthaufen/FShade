### 5.7.15
- GLSL: Vulkan specialization constants via the magic `SpecConstants` uniform scope — members emit `layout(constant_id = N) const` with zero defaults (no descriptor binding), name-keyed ids stable across stages, reported through `GLSLProgramInterface.specConstants`; `Config.specConstants` gates emission (Vulkan on; GL backends fall through to an ordinary uniform buffer).

### 5.7.14
- Restore the try/with around `Expr.TryGetReflectedDefinition` (reverts 5.7.13): the "not reflectable" issue it chased was a dotnet-8-vs-10 / FSharp.Core reflected-definition pickle mismatch, not this lookup.

### 5.7.13
- Removed the try/with around `Expr.TryGetReflectedDefinition` (reverted in 5.7.14).

### 5.7.12
- Fix descriptor binding computation. Descriptors occupy a single slot regardless of count.
- Optimize descriptor set computation for unbounded descriptors arrays.

### 5.7.11
- GLSL: every truly UNBOUNDED uniform (sampler/image array with `cnt = -1`, or `T[][]` storage-buffer array) now lands in its OWN descriptor set. Vulkan's `VK_EXT_descriptor_indexing` requires that the binding declared with `VARIABLE_DESCRIPTOR_COUNT_BIT` is the LAST binding in its set (spec VUID 03004). When multiple unbounded arrays shared one set, only one could be true variable-count; the others silently fell back to a fixed-capacity reserve. NVIDIA tolerated this; AMD/RADV did not — degrading bindless heap paths to slow / undefined behaviour. The fix splits each unbounded uniform into a single-field group with a freshly allocated descriptor set; fixed-size uniforms keep the shared-set grouping.

### 5.7.10
- Fixed invalid inlining in presence of address-of operators
- Fixed handling of functions with tuple arguments
- Fixed `Debug.Printfn` for compute shaders
- Added support for `fst` and `snd`
- Improved support for custom structs and classes

### 5.7.9
- GLSL: fixed binding allocator decrementing the global counter when an unbounded sampler/image array (`cnt = -1`, the bindless-array sentinel) was allocated. A `Backend.Create` with `BindingMode.Global` would silently produce overlapping bindings between images, samplers, SSBOs and UBOs whenever a composed effect mixed storage images, an unbounded sampler array and SSBOs on the same set — Vulkan validation flagged it with `VUID-VkDescriptorSetLayoutCreateInfo-binding-00279`, NVIDIA tolerated the collision by overwriting descriptors at runtime (so e.g. an `imageStore` to a storage image whose binding collided with an SSBO would silently land on the SSBO and the write was lost). The allocator now advances by `max 1 cnt` so an unbounded array consumes exactly one slot regardless of its element count.

### 5.7.8
- GLSL: only emit `nonuniformEXT` (and require `GL_EXT_nonuniform_qualifier`) for dynamic descriptor-array indexing when the target uses descriptor sets (Vulkan, `createDescriptorSets = true`). On the GL backend there is no such extension and a dynamically-uniform index needs no qualifier, so bindless storage-buffer arrays (`X[i].data[j]`) and sampler/image arrays now compile on GL instead of failing with `undefined variable "nonuniformEXT"`

### 5.7.7
- Fixed `Shader.withBody`/`substituteReads` dropping per-buffer storage access: a storage-buffer read spliced into a shader (e.g. bindless vertex-pull rewrites that replace input reads with `buffer[...]` gathers) now re-derives `StorageAccess` from the new body, so the buffer is emitted as `readonly` instead of read-write

### 5.7.6
- Unbounded (runtime-sized, count -1) sampler/image arrays via descriptor indexing, with `nonuniformEXT` for dynamic indices
- Unbounded (bindless) storage-buffer ARRAYS: a `T[][]` storage buffer assembles to `buffer { T[] data; } X[];`, indexed `X[i].data[j]`; `GLSLStorageBuffer.ssbCount` reports the count (-1 = unbounded array, 1 = single) so descriptor-indexing backends can bind an array of storage buffers; `nonuniformEXT` emitted for dynamic outer indices

### 5.7.5
- Fixed constant folding for non-static members (.Normalized etc.)

### 5.7.4
- Added `Effect.Dependencies : EffectDeps` — per-output input + uniform tracking, eagerly serialized in the binary header alongside `Id`. Composed effects derive their deps via pure map operations (no shader force at compose time); leaf effects analyse via `Shader.withOutputs` per-output. AOT/cached blobs carry the deps map and expose it without ever forcing the lazy shader bodies. Cross-validated against `Effect.toModule` linking — the deps map is exact (predicted vertex inputs == linker-demanded vertex inputs) for arbitrary subsets of requested outputs.
- Bumped binary effect blob format to v1 (added a leading version byte). Old (pre-v1) blobs fail with a clear error on deserialize — by intent, no backward-compat handling.

### 5.7.3
- Fixed computation of required slots for 64-bit attributes
- Added implicit `flat` interpolation for double-based attributes

### 5.7.2
- Removed warning related to `onlyInShaderCode`
- Fixed non-int32 `for` loops

### 5.7.1
- Added support for Enum.HasFlag
- Added support for LanguagePrimitives.EnumToValue, LanguagePrimitives.EnumOfValue, and enum

### 5.7.0
- Removed double as float feature
- Added basic support for 8-bit, 16-bit, and 64-bit data types
- Added support for color types
- Added support for integer-based border colors
- Added support for push constants
- Added support for GL_EXT_ray_tracing_position_fetch
- Added support for GL_NV_shader_invocation_reorder
- Added UniformDecoration.BufferAccess properly
- Simplified for-loop unrolling
- [Raytracing] Changed type of object-world transforms to M34f
- [Raytracing] Added RayId, MissId, and CallableId
- [GLSL] Improved name mangling for invalid characters
- [GLSL] Added `Config.separateTexturesAndSamplers` for automatically splitting samplers into separate texture/sampler uniforms for better interop with Vulkan/WGSL
- [Optimizer] Added step for simplifying raytracing writes
- [Optimizer] Fixed issue with dead-code elimination and utility functions
- [Debugger] Fixed handling of forward slashes in file paths
- [fshadeaot] Added double checking feature

### 5.7.0-prerelease0014
- Simplified for-loop unrolling
- Added RayFlags.ForceOpacityMicromap2State
- Added support for integer-based border colors
- Added support for color types
- [Raytracing] Reworked handling of ids, string, and Symbols
- [GLSL] Added missing keywords for name mangling
- [GLSL] Fixed intrinsics for non-standard types 

### 5.7.0-prerelease0013
- [ComputeShader] fixed ordering of UtilityFunctions and storage buffers in compute shaders

### 5.7.0-prerelease0012
- Added KeepCall attribute to image setters
- [Optimizer] Added step for simplifying raytracing writes
- [Optimizer] Fixed issue with dead-code elimination and utility functions
- [Optimizer] Prevent HitObject declaration from being inlined
- [Raytracing] Added Thread.Reorder overloads with int parameters

### 5.7.0-prerelease0011
- [ComputeShader] fixed usage for `cs_` prefixed uniforms

### 5.7.0-prerelease0010
- [ComputeShader] fixed read/write detection for argument-storage-buffers

### 5.7.0-prerelease0009
- [GLSL] fixed bug in float-image deserialize

### 5.7.0-prerelease0008 
- [GLSL] Fixed issue with UIntImage serialization (was deserialized to IntImage)

### 5.7.0-prerelease0007
- [GLSL] samplerShadow correctly emitted when splitting shadow sampler2D/etc.

### 5.7.0-prerelease0006
- [Compute] fixed shader-usages for storage outputs

### 5.7.0-prerelease0005
- [Compute] shader-usages for uniforms/storage/images/samplers are now properly handled

### 5.7.0-prerelease0004
- [Shader] added UniformDecoration.BufferAccess properly
- [GLSL] handled uniform usages properly per stage (correct GLSLShaderInterface)

### 5.7.0-prerelease0003
- [GLSL] added `Config.separateTexturesAndSamplers` for automatically splitting samplers into separate texture/sampler uniforms for better interop with Vulkan/WGSL

### 5.7.0-prerelease0002
- Added support for push constants
- Added support for GL_EXT_ray_tracing_position_fetch
- [Raytracing] Changed type of object-world transforms to M34f
- [Raytracing] Added RayId, MissId, and CallableId
- [GLSL] Improved name mangling for invalid characters
- [GLSL] Added Config.availableExtensions
- [GLSL] Merged Vulkan and Raytracing backends

### 5.7.0-prerelease0001
- Removed double as float feature
- Added basic support for 8-bit, 16-bit, and 64-bit data types
- Added support for indexing arrays with uint
- Removed unused original field from CType.CStruct
- [fshadeaot] Added double checking feature

### 5.6.0
- Updated to NET 8 and Aardvark.Base 5.3
- Use struct representation for partial active patterns
- Removed Effect.toLayered

### 5.6.0-prerelease0001
- Initial prerelease

### 5.5.4
* [GLSL] Fixed issue with variable names containing @
* [Raytracing] Fixed issue with callable data name indexing

### 5.5.3
* [Raytracing] Added duplication checks in effect builders
* [Debugger] Fixed issue with compiler options not being recognized due to double hyphens
* Added Effect.toLayered with custom semantic
* Use std430 layout for all storage buffers

### 5.5.2
* [GLSL] Fixed issue with image format names containing underscores

### 5.5.1
* Removed warning on field set for storage buffers
* Added caching for raytracing shaders and effects
* Made hashes of compute shaders more consistent
* Added check for type redefinitions in module compiler
* [Debugger] Fixed issue with single unit input shader functions
* [Debugger] Disabled symbol and other miscellaneous file generation on build
* [Debugger] Enable file watchers on demand due to inotify limit on Linux
* [Debugger] Fixed backslash handling in paths
* [Serializer] Include more type information in hashes to avoid conflicts when using shader debugger

### 5.5.0
* Reworked shader debugger (see https://github.com/aardvark-platform/aardvark.docs/wiki/FShade-ShaderDebugger)
* Reduced the number of OnlyInShaderCode exceptions thrown by the optimizer
* Fixed constant folding of bitwise operations on enums

### 5.4.1
* Added validation for uniform aliases
* Serializer includes uniform semantic and type for hashing

### 5.4.0
* Added sampler filter reduction mode
* [RTX] Added builder operation overloads with untupled arguments
* [RTX] Added RayHitKind enum
* [RTX] Ray flags and hit kind inputs use enums now
* Turned Module into a class to hide laziness of entries
* Added support for enums with arbitrary underlying type
* Added config for reversing tesselation winding order
* Preprocessor: merge aot2 + bugfixes
* Lazy modules entries
* Effect serialization bugfixes: array deserialization, uniform scopes
* Fixed issue with return type for integer vector dot product
* Include sampler texture name in hash
* Added support for float32-based vertex types

### 5.4.0-prerelease0004
* Added sampler filter reduction mode

### 5.4.0-prerelease0003
* [RTX] Added builder operation overloads with untupled arguments
* [RTX] Added RayHitKind enum
* [RTX] Ray flags and hit kind inputs use enums now
* Turned Module into a class to hide laziness of entries
* Added support for enums with arbitrary underlying type
* Added config for reversing tesselation winding order

### 5.4.0-prerelease0002
* Preprocessor: merge aot2 + bugfixes
* Lazy modules entries
* Effect serialization bugfixes: array deserialization, uniform scopes
* Fixed issue with return type for integer vector dot product
* Include sampler texture name in hash
* Added support for float32-based vertex types

### 5.4.0-prerelease0001
* Initial prerelease version for 5.4

### 5.3.6
* fshadeaot using resources instead of literal strings

### 5.3.5
* merged v52 (containing improved AOT)

### 5.3.4
* Added intrinsics for dynamically accessing matrix rows and columns (matrix.Row() / matrix.Column())
* Added intrinsics for dynamically accessing matrix elements (matrix.[x, y])
* Added missing intrinsics for special floating point checks (NaN, +-infinity)
* Added missing intrinsics for vector swizzles in Vec module
* Added and improved intrinsics for vector and matrix relations (AllEqual, AllDifferent, ...)
* Added intrinsics for DistanceSquared, Distance1, DistanceMin, DistanceMax, Norm1, Norm2, NormMin, NormMax
* Fixed return type for dot product of integer vectors

### 5.3.3
* Fixed issue with deserialization of raytracing interfaces

### 5.3.2
* Added simple hash based id to RaytracingEffect
* Added matrix transform intrinsics
* Removed redundant dimension parameter in CNewVector

### 5.3.1
* Fixed issue with reflected functions using sampler types as parameters

### 5.3.0
* Removed unused / unnecessary image and sampler types
* Remove unused image formats
* Remove MipMapLevels property for multisampled samplers
* Allow multiple InterpolationMode values
* Fixed various image and sampler methods and properties
* Added Samples property for multisampled images and samplers
* Added SampleLevelOffset
* Implemented implicit flat interpolation for integral types
* Fixed and added vector and matrix intrinsics
* Fixed various intrinsics with regard to duplicated expressions
* Added exp2 intrinsic
* Added step, linearstep intrinsics
* Implemented basic output type conversion (e.g. from V4d to V2d)
* Added Debug.Printfn (Vulkan via validation layers only)
* Implemented full support for unsigned integer types (images, samplers, output types)

### 5.3.0-prerelease0004
* Added support for unsigned integer types (output, samplers, images, ...) 

### 5.3.0-prerelease0003
* Removed interpolation modes for built-in fragement inputs
* Removed MipMapLevels for multisampled samplers
* Implemented Debug.Printf

### 5.3.0-prerelease0002
* Fixed regular expression for constant swizzles
* Improved error message for incompatible output types

### 5.3.0-prerelease0001
* Initial prerelease version for 5.3

### 5.2.13
* Fixed issue with constant swizzles

### 5.2.12
* disabled warnings again

### 5.2.11
* printing warnings when Expr.TryGetReflectedDefinition fails

### 5.2.10
* try/with for Expr.TryGetReflectedDefinition (seems to raise exceptions in some cases)

### 5.2.9
* Fixed issue with topological sort and raytracing data
* Removed warnings in raytracing shaders

### 5.2.8
* Updated to Aardvark.Base 5.2

### 5.2.7
* Aardvark.Build version 

### 5.2.7-prerelease0001
* aardpack 

### 5.2.6
* aardpack 