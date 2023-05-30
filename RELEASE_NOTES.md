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