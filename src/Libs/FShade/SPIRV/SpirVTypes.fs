namespace SpirV
//
///// <summary>
///// The source language is an annotation, with no semantics that effect the meaning of other parts of the module.
///// Used by OpSource.
///// </summary>
//type SourceLanguage =
//    | unknown = 0
//    | ESSL = 1
//    | GLSL = 2
//    | OpenCL = 3
//
///// <summary>
///// Used by OpEntryPoint.
///// </summary>
//type ExecutionModel =
//    /// <summary>
//    /// Vertex shading stage.
//    /// </summary>
//    | Vertex = 0
//    /// <summary>
//    /// Tessellation control (or hull) shading stage.
//    /// </summary>
//    | TessellationControl = 1
//    /// <summary>
//    /// Tessellation evaluation (or domain) shading stage.
//    /// </summary>
//    | TessellationEvaluation = 2
//    /// <summary>
//    /// Geometry shading stage.
//    /// </summary>
//    | Geometry = 3
//    /// <summary>
//    /// Fragment shading stage.
//    /// </summary>
//    | Fragment = 4
//    /// <summary>
//    /// Graphical compute shading stage.
//    /// </summary>
//    | GLCompute = 5
//    /// <summary>
//    /// Compute kernel.
//    /// </summary>
//    | Kernel = 6
//
///// <summary>
///// Used by OpMemoryModel.
///// </summary>
//type AddressingModel =
//    | Logical = 0
//    /// <summary>
//    /// Indicates a 32-bit module, where the address width is equal to 32 bits.
//    /// </summary>
//    | Physical32 = 1
//    /// <summary>
//    /// Indicates a 64-bit module, where the address width is equal to 64 bits.
//    /// </summary>
//    | Physical64 = 2
//
///// <summary>
///// Used by OpMemoryModel.
///// </summary>
//type MemoryModel =
//    /// <summary>
//    /// No shared memory consistency issues.
//    /// </summary>
//    | Simple = 0
//    /// <summary>
//    /// Memory model needed by later versions of GLSL and ESSL.  Works across multiple versions.
//    /// </summary>
//    | GLSL450 = 1
//    /// <summary>
//    /// OpenCL 1.2 memory model.
//    /// </summary>
//    | OpenCL1_2 = 2
//    /// <summary>
//    /// OpenCL 2.0 memory model.
//    /// </summary>
//    | OpenCL2_0 = 3
//    /// <summary>
//    /// OpenCL 2.1 memory model.
//    /// </summary>
//    | OpenCL2_1 = 4
//
///// <summary>
///// Declare the modes this module&#8217;s stage will execute in.
///// Used by OpExecutionMode.
///// </summary>
//type ExecutionMode =
//    /// <summary>
//    /// Number of times to invoke the geometry stage for each input primitive received. The default is to run once for each input primitive. If greater than the target-dependent maximum, it will fail to compile. Only valid with the Geometry Execution Model.
//    /// </summary>
//    | Invocations = 0
//    /// <summary>
//    /// Requests the tessellation primitive generator to divide edges into a collection of equal-sized segments. Only valid with one of the tessellation Execution Models.
//    /// </summary>
//    | SpacingEqual = 1
//    /// <summary>
//    /// Requests the tessellation primitive generator to divide edges into an even number of equal-length segments plus two additional shorter fractional segments. Only valid with one of the tessellation Execution Models.
//    /// </summary>
//    | SpacingFractionalEven = 2
//    /// <summary>
//    /// Requests the tessellation primitive generator to divide edges into an odd number of equal-length segments plus two additional shorter fractional segments. Only valid with one of the tessellation Execution Models.
//    /// </summary>
//    | SpacingFractionalOdd = 3
//    /// <summary>
//    /// Requests the tessellation primitive generator to generate triangles in clockwise order. Only valid with one of the tessellation Execution Models.
//    /// </summary>
//    | VertexOrderCw = 4
//    /// <summary>
//    /// Requests the tessellation primitive generator to generate triangles in counter-clockwise order. Only valid with one of the tessellation Execution Models.
//    /// </summary>
//    | VertexOrderCcw = 5
//    /// <summary>
//    /// Pixels appear centered on whole-number pixel offsets. E.g., the coordinate (0.5, 0.5) appears to move to (0.0, 0.0). Only valid with the Fragment Execution Model.
//    /// </summary>
//    | PixelCenterInteger = 6
//    /// <summary>
//    /// Pixel coordinates appear to originate in the upper left, and increase toward the right and downward. Only valid with the Fragment Execution Model.
//    /// </summary>
//    | OriginUpperLeft = 7
//    /// <summary>
//    /// Fragment tests are to be performed before fragment shader execution. Only valid with the Fragment Execution Model.
//    /// </summary>
//    | EarlyFragmentTests = 8
//    /// <summary>
//    /// Requests the tessellation primitive generator to generate a point for each distinct vertex in the subdivided primitive, rather than to generate lines or triangles. Only valid with one of the tessellation Execution Models.
//    /// </summary>
//    | PointMode = 9
//    /// <summary>
//    /// This stage will run in transform feedback-capturing mode and this module is responsible for describing the transform-feedback setup. See the XfbBuffer, Offset, and Stride Decorations.
//    /// </summary>
//    | Xfb = 10
//    /// <summary>
//    /// This mode must be declared if this module potentially changes the fragment&#8217;s depth. Only valid with the Fragment Execution Model.
//    /// </summary>
//    | DepthReplacing = 11
//    /// <summary>
//    /// TBD: this should probably be removed.  Depth testing will always be performed after the shader has executed. Only valid with the Fragment Execution Model.
//    /// </summary>
//    | DepthAny = 12
//    /// <summary>
//    /// External optimizations may assume depth modifications will leave the fragment&#8217;s depth as greater than or equal to the fragment&#8217;s interpolated depth value (given by the z component of the FragCoord Built-In decorated variable). Only valid with the Fragment Execution Model.
//    /// </summary>
//    | DepthGreater = 13
//    /// <summary>
//    /// External optimizations may assume depth modifications leave the fragment&#8217;s depth less than the fragment&#8217;s interpolated depth value, (given by the z component of the FragCoord Built-In decorated variable). Only valid with the Fragment Execution Model.
//    /// </summary>
//    | DepthLess = 14
//    /// <summary>
//    /// External optimizations may assume this stage did not modify the fragment&#8217;s depth. However, DepthReplacing mode must accurately represent depth modification. Only valid with the Fragment Execution Model.
//    /// </summary>
//    | DepthUnchanged = 15
//    /// <summary>
//    /// Indicates the work-group size in the x, y, and z dimensions. Only valid with the GLCompute or Kernel Execution Models.
//    /// </summary>
//    | LocalSize = 16
//    /// <summary>
//    /// A hint to the compiler, which indicates the most likely to be used work-group size in the x, y, and z dimensions. Only valid with the Kernel Execution Model.
//    /// </summary>
//    | LocalSizeHint = 17
//    /// <summary>
//    /// Stage input primitive is points. Only valid with the Geometry Execution Model.
//    /// </summary>
//    | InputPoints = 18
//    /// <summary>
//    /// Stage input primitive is lines. Only valid with the Geometry Execution Model.
//    /// </summary>
//    | InputLines = 19
//    /// <summary>
//    /// Stage input primitive is lines adjacency. Only valid with the Geometry Execution Model.
//    /// </summary>
//    | InputLinesAdjacency = 20
//    /// <summary>
//    /// For a geometry stage, input primitive is triangles. For a tessellation stage, requests the tessellation primitive generator to generate triangles. Only valid with the Geometry or one of the tessellation Execution Models.
//    /// </summary>
//    | InputTriangles = 21
//    /// <summary>
//    /// Geometry stage input primitive is triangles adjacency. Only valid with the Geometry Execution Model.
//    /// </summary>
//    | InputTrianglesAdjacency = 22
//    /// <summary>
//    /// Requests the tessellation primitive generator to generate quads. Only valid with one of the tessellation Execution Models.
//    /// </summary>
//    | InputQuads = 23
//    /// <summary>
//    /// Requests the tessellation primitive generator to generate isolines. Only valid with one of the tessellation Execution Models.
//    /// </summary>
//    | InputIsolines = 24
//    /// <summary>
//    /// For a geometry stage, the maximum number of vertices the shader will ever emit in a single invocation. For a tessellation-control stage, the number of vertices in the output patch produced by the tessellation control shader, which also specifies the number of times the tessellation control shader is invoked. Only valid with the Geometry or one of the tessellation Execution Models.
//    /// </summary>
//    | OutputVertices = 25
//    /// <summary>
//    /// Stage output primitive is points. Only valid with the Geometry Execution Model.
//    /// </summary>
//    | OutputPoints = 26
//    /// <summary>
//    /// Stage output primitive is line strip. Only valid with the Geometry Execution Model.
//    /// </summary>
//    | OutputLineStrip = 27
//    /// <summary>
//    /// Stage output primitive is triangle strip. Only valid with the Geometry Execution Model.
//    /// </summary>
//    | OutputTriangleStrip = 28
//    /// <summary>
//    /// A hint to the compiler, which indicates that most operations used in the entry point are explicitly vectorized using a particular vector type. Only valid with the Kernel Execution Model.
//    /// </summary>
//    | VecTypeHint = 29
//    /// <summary>
//    /// Indicates that floating-point-expressions contraction is disallowed. Only valid with the Kernel Execution Model.
//    /// </summary>
//    | ContractionOff = 30
//
///// <summary>
///// Class of storage for declared variables (does not include intermediate values).
///// Used by:
///// </summary>
//type StorageClass =
//    /// <summary>
//    /// Shared externally, read-only memory, visible across all instantiations or work groups. Graphics uniform memory.  OpenCL Constant memory.
//    /// </summary>
//    | UniformConstant = 0
//    /// <summary>
//    /// Input from pipeline.  Read only.
//    /// </summary>
//    | Input = 1
//    /// <summary>
//    /// Shared externally, visible across all instantiations or work groups.
//    /// </summary>
//    | Uniform = 2
//    /// <summary>
//    /// Output to pipeline.
//    /// </summary>
//    | Output = 3
//    /// <summary>
//    /// Shared across all work items within a work group. OpenGL "shared".  OpenCL local memory.
//    /// </summary>
//    | WorkgroupLocal = 4
//    /// <summary>
//    /// Visible across all work items of all work groups. OpenCL global memory.
//    /// </summary>
//    | WorkgroupGlobal = 5
//    /// <summary>
//    /// Accessible across functions within a module, non-IO (not visible outside the module).
//    /// </summary>
//    | PrivateGlobal = 6
//    /// <summary>
//    /// A variable local to a function.
//    /// </summary>
//    | Function = 7
//    /// <summary>
//    /// A generic pointer, which overloads StoragePrivate, StorageLocal, StorageGlobal. not a real storage class.
//    /// </summary>
//    | Generic = 8
//    /// <summary>
//    /// Private to a work-item and is not visible to another work-item. OpenCL private memory.
//    /// </summary>
//    | Private = 9
//    /// <summary>
//    /// For holding atomic counters.
//    /// </summary>
//    | AtomicCounter = 10
//
///// <summary>
///// Dimensionality of a texture.
///// Used by OpTypeSampler.
///// </summary>
//type Dim =
//    | D1D = 0
//    | D2D = 1
//    | D3D = 2
//    | Cube = 3
//    | Rect = 4
//    | Buffer = 5
//
///// <summary>
///// Define the addressing mode of read image extended instructions.
///// </summary>
//type SamplerAddressingMode =
//    /// <summary>
//    /// The image coordinates used to sample elements of the image refer to a location inside the image, otherwise the results are undefined.
//    /// </summary>
//    | None = 0
//    /// <summary>
//    /// Out-of-range image coordinates are clamped to the extent.
//    /// </summary>
//    | ClampEdge = 2
//    /// <summary>
//    /// Out-of-range image coordinates will return a border color.
//    /// </summary>
//    | Clamp = 4
//    /// <summary>
//    /// Out-of-range image coordinates are wrapped to the valid range. Can only be used with normalized coordinates.
//    /// </summary>
//    | Repeat = 6
//    /// <summary>
//    /// Flip the image coordinate at every integer junction. Can only be used with normalized coordinates.
//    /// </summary>
//    | RepeatMirrored = 8
//
///// <summary>
///// Define the filter mode of read image extended instructions.
///// </summary>
//type SamplerFilterMode =
//    /// <summary>
//    /// Use filter nearset mode when performing a read image operation.
//    /// </summary>
//    | Nearest = 16
//    /// <summary>
//    /// Use filter linear mode when performing a read image operation.
//    /// </summary>
//    | Linear = 32
//
///// <summary>
///// Enables fast math operations which are otherwise unsafe.
///// </summary>
//type FPFastMathMode =
//    /// <summary>
//    /// Assume parameters and result are not NaN.
//    /// </summary>
//    | NotNaN = 0
//    /// <summary>
//    /// Assume parameters and result are not +/- Inf.
//    /// </summary>
//    | NotInf = 2
//    /// <summary>
//    /// Treat the sign of a zero parameter or result as insignificant.
//    /// </summary>
//    | NSZ = 4
//    /// <summary>
//    /// Allow the usage of reciprocal rather than perform a division.
//    /// </summary>
//    | AllowRecip = 8
//    /// <summary>
//    /// Allow algebraic transformations according to real-number associative and distibutive algebra. This flag implies all the others.
//    /// </summary>
//    | Fast = 16
//
///// <summary>
///// Associate a rounding mode to a floating-point conversion instruction.
///// </summary>
//type FPRoundingMode =
//    /// <summary>
//    /// Round to nearest even.
//    /// </summary>
//    | RTE = 0
//    /// <summary>
//    /// Round towards zero.
//    /// </summary>
//    | RTZ = 1
//    /// <summary>
//    /// Round towards positive infinity.
//    /// </summary>
//    | RTP = 2
//    /// <summary>
//    /// Round towards negative infinity.
//    /// </summary>
//    | RTN = 3
//
///// <summary>
///// Associate a linkage type to functions or global variables. By default, functions and global variables are private to a module and cannot be accessed by other modules.
///// </summary>
//type LinkageType =
//    /// <summary>
//    /// Accessible by other modules as well.
//    /// </summary>
//    | Export = 0
//    /// <summary>
//    /// A declaration for a global identifier that exists in another module.
//    /// </summary>
//    | Import = 1
//
///// <summary>
///// Defines the access permissions of OpTypeSampler and OpTypePipe object.
///// </summary>
//type AccessQualifier =
//    /// <summary>
//    /// A read-only object.
//    /// </summary>
//    | ReadOnly = 0
//    /// <summary>
//    /// A write-only object.
//    /// </summary>
//    | WriteOnly = 1
//    /// <summary>
//    /// A readable and writable object.
//    /// </summary>
//    | ReadWrite = 2
//
///// <summary>
///// Adds additional information to the return type and to each parameter of a function.
///// </summary>
//type FunctionParameterAttribute =
//    /// <summary>
//    /// Value should be zero extended if needed.
//    /// </summary>
//    | Zext = 0
//    /// <summary>
//    /// Value should be sign extended if needed.
//    /// </summary>
//    | Sext = 1
//    /// <summary>
//    /// This indicates that the pointer parameter should really be passed by value to the function. Only valid for pointer parameters (not for ret value).
//    /// </summary>
//    | ByVal = 2
//    /// <summary>
//    /// Indicates that the pointer parameter specifies the address of a structure that is the return value of the function in the source program. Only applicable to the first parameter which must be a pointer parameters.
//    /// </summary>
//    | Sret = 3
//    /// <summary>
//    /// Indicates that the memory pointed by a pointer parameter is not accessed via pointer values which are not derived from this pointer parameter. Only valid for pointer parameters. Not valid on return values.
//    /// </summary>
//    | NoAlias = 4
//    /// <summary>
//    /// The callee does not make a copy of the pointer parameter into a location that is accessible after returning from the callee. Only valid for pointer parameters. Not valid on return values.
//    /// </summary>
//    | NoCapture = 5
//    /// <summary>
//    /// CL TBD
//    /// </summary>
//    | SVM = 6
//    /// <summary>
//    /// Can only read the memory pointed by a pointer parameter. Only valid for pointer parameters. Not valid on return values.
//    /// </summary>
//    | NoWrite = 7
//    /// <summary>
//    /// Cannot dereference the memory pointed by a pointer parameter. Only valid for pointer parameters. Not valid on return values.
//    /// </summary>
//    | NoReadWrite = 8
//
///// <summary>
///// Used by OpDecorate and OpMemberDecorate.
///// </summary>
//type Decoration =
//    /// <summary>
//    /// Apply as described in the ES Precision section.
//    /// </summary>
//    | PrecisionLow = 0
//    /// <summary>
//    /// Apply as described in the ES Precision section.
//    /// </summary>
//    | PrecisionMedium = 1
//    /// <summary>
//    /// Apply as described in the ES Precision section.
//    /// </summary>
//    | PrecisionHigh = 2
//    /// <summary>
//    /// Apply to a structure type to establish it is a non-SSBO-like shader-interface block.
//    /// TBD can this be removed?  Probably doesn&#8217;t add anything over a nonwritable structure in the UniformConstant or Uniform storage class. with a Binding and DescriptorSet decoration.
//    /// </summary>
//    | Block = 3
//    /// <summary>
//    /// Apply to a structure type to establish it is an SSBO-like shader-interface block.
//    /// TBD can this be removed?  Probably doesn&#8217;t add anything over a structure in the UniformConstant or Uniform storage class. with a Binding and DescriptorSet decoration.
//    /// </summary>
//    | BufferBlock = 4
//    /// <summary>
//    /// Apply to a variable or a member of a structure.  Must decorate an entity whose type is a matrix. Indicates that components within a row are contiguous in memory.
//    /// </summary>
//    | RowMajor = 5
//    /// <summary>
//    /// Apply to a variable or a member of a structure.  Must decorate an entity whose type is a matrix. Indicates that components within a column are contiguous in memory.
//    /// </summary>
//    | ColMajor = 6
//    /// <summary>
//    /// Apply to a structure type to get GLSL shared memory layout.
//    /// </summary>
//    | GLSLShared = 7
//    /// <summary>
//    /// Apply to a structure type to get GLSL std140 memory layout.
//    /// </summary>
//    | GLSLStd140 = 8
//    /// <summary>
//    /// Apply to a structure type to get GLSL std430 memory layout.
//    /// </summary>
//    | GLSLStd430 = 9
//    /// <summary>
//    /// Apply to a structure type to get GLSL packed memory layout.
//    /// </summary>
//    | GLSLPacked = 10
//    /// <summary>
//    /// Apply to a variable or a member of a structure.  Indicates that perspective-correct interpolation must be used. Only valid for the Input and Output Storage Classes.
//    /// </summary>
//    | Smooth = 11
//    /// <summary>
//    /// Apply to a variable or a member of a structure.  Indicates that linear, non-perspective correct, interpolation must be used. Only valid for the Input and Output Storage Classes.
//    /// </summary>
//    | Noperspective = 12
//    /// <summary>
//    /// Apply to a variable or a member of a structure.  Indicates no interpolation will be done. The non-interpolated value will come from a vertex, as described in the API specification. Only valid for the Input and Output Storage Classes.
//    /// </summary>
//    | Flat = 13
//    /// <summary>
//    /// Apply to a variable or a member of a structure. Indicates a tessellation patch. Only valid for the Input and Output Storage Classes.
//    /// </summary>
//    | Patch = 14
//    /// <summary>
//    /// Apply to a variable or a member of a structure. When used with multi-sampling rasterization, allows a single interpolation location for an entire pixel. The interpolation location must lie in both the pixel and in the primitive being rasterized. Only valid for the Input and Output Storage Classes.
//    /// </summary>
//    | Centroid = 15
//    /// <summary>
//    /// Apply to a variable or a member of a structure. When used with multi-sampling rasterization, requires per-sample interpolation. The interpolation locations must be the locations of the samples lying in both the pixel and in the primitive being rasterized. Only valid for the Input and Output Storage Classes.
//    /// </summary>
//    | Sample = 16
//    /// <summary>
//    /// Apply to a variable, to indicate expressions computing its value be done invariant with respect to other modules computing the same expressions.
//    /// </summary>
//    | Invariant = 17
//    /// <summary>
//    /// Apply to a variable, to indicate the compiler may compile as if there is no aliasing.  See the Aliasing section for more detail.
//    /// </summary>
//    | Restrict = 18
//    /// <summary>
//    /// Apply to a variable, to indicate the compiler is to generate accesses to the variable that work correctly in the presence of aliasing.  See the Aliasing section for more detail.
//    /// </summary>
//    | Aliased = 19
//    /// <summary>
//    /// Apply to a variable, to indicate the memory holding the variable is volatile.  See the Memory Model section for more detail.
//    /// </summary>
//    | Volatile = 20
//    /// <summary>
//    /// Indicates that a global variable is constant and will never be modified. Only allowed on global variables.
//    /// </summary>
//    | Constant = 21
//    /// <summary>
//    /// Apply to a variable, to indicate the memory holding the variable is coherent.  See the Memory Model section for more detail.
//    /// </summary>
//    | Coherent = 22
//    /// <summary>
//    /// Apply to a variable, to indicate the memory holding the variable is not writable, and that this module does not write to it.
//    /// </summary>
//    | Nonwritable = 23
//    /// <summary>
//    /// Apply to a variable, to indicate the memory holding the variable is not readable, and that this module does not read from it.
//    /// </summary>
//    | Nonreadable = 24
//    /// <summary>
//    /// Apply to a variable or a member of a structure.  Asserts that the value backing the decorated &lt;id&gt; is dynamically uniform across all instantiations that might run in parallel.
//    /// </summary>
//    | Uniform = 25
//    /// <summary>
//    /// Apply to a variable to indicate that it is known that this module does not read or write it.  Useful for establishing interface.
//    /// TBD consider removing this?
//    /// </summary>
//    | NoStaticUse = 26
//    /// <summary>
//    /// Marks a structure type as "packed", indicating that the alignment of the structure is one and that there is no padding between structure members.
//    /// </summary>
//    | CPacked = 27
//    /// <summary>
//    /// Indicates that a conversion to an integer type is saturated. Only valid for conversion instructions to integer type.
//    /// </summary>
//    | FPSaturatedConversion = 28
//    /// <summary>
//    /// Apply to a variable or a member of a structure.  Indicates the stream number to put an output on. Only valid for the Output Storage Class and the Geometry Execution Model.
//    /// </summary>
//    | Stream = 29
//    /// <summary>
//    /// Apply to a variable or a structure member.  Forms the main linkage for Storage Class Input and Output variables:
//    /// - between the API and vertex-stage inputs,
//    /// - between consecutive programmable stages, or
//    /// - between fragment-stage outputs and the API.
//    ///  Only valid for the Input and Output Storage Classes.
//    /// </summary>
//    | Location = 30
//    /// <summary>
//    /// Apply to a variable or a member of a structure.  Indicates which component within a Location will be taken by the decorated entity. Only valid for the Input and Output Storage Classes.
//    /// </summary>
//    | Component = 31
//    /// <summary>
//    /// Apply to a variable to identify a blend equation input index, used as described in the API specification. Only valid for the Output Storage Class and the Fragment Execution Model.
//    /// </summary>
//    | Index = 32
//    /// <summary>
//    /// Apply to a variable. Part of the main linkage between the API and SPIR-V modules for memory buffers, textures, etc. See the API specification for more information.
//    /// </summary>
//    | Binding = 33
//    /// <summary>
//    /// Apply to a variable. Part of the main linkage between the API and SPIR-V modules for memory buffers, textures, etc. See the API specification for more information.
//    /// </summary>
//    | DescriptorSet = 34
//    /// <summary>
//    /// Apply to a structure member.  This gives the byte offset of the member relative to the beginning of the structure. Can be used, for example, by both uniform and transform-feedback buffers.
//    /// </summary>
//    | Offset = 35
//    /// <summary>
//    /// TBD: This can probably be removed.
//    /// </summary>
//    | Alignment = 36
//    /// <summary>
//    /// Apply to a variable or a member of a structure.  Indicates which transform-feedback buffer an output is written to. Only valid for the Output Storage Classes of vertex processing Execution Models.
//    /// </summary>
//    | XfbBuffer = 37
//    /// <summary>
//    /// The stride, in bytes, of array elements or transform-feedback buffer vertices.
//    /// </summary>
//    | Stride = 38
//    /// <summary>
//    /// See Built-In
//    /// </summary>
//    | Literal_Number = 39
//    /// <summary>
//    /// Indicates a function return value or parameter attribute.
//    /// </summary>
//    | FuncParamAttr = 40
//    /// <summary>
//    /// Indicates a floating-point rounding mode.
//    /// </summary>
//    | FP_Rounding_Mode = 41
//    /// <summary>
//    /// Indicates a floating-point fast math flag.
//    /// </summary>
//    | FP_Fast_Math_Mode = 42
//    /// <summary>
//    /// Indicate a linkage type. Only valid on an OpFunction or a module scope OpVariable.
//    /// </summary>
//    | Linkage_Type = 43
//    /// <summary>
//    /// Apply to a specialization constant. Forms the API linkage for setting a specialized value. See specialization.
//    /// </summary>
//    | SpecId = 44
//
///// <summary>
///// Used when Decoration is Built-In. Apply to either
///// </summary>
//type BuiltIn =
//    | Position = 0
//    | PointSize = 1
//    | ClipVertex = 2
//    | ClipDistance = 3
//    | CullDistance = 4
//    | VertexId = 5
//    | InstanceId = 6
//    | PrimitiveId = 7
//    | InvocationId = 8
//    | Layer = 9
//    | ViewportIndex = 10
//    | TessLevelOuter = 11
//    | TessLevelInner = 12
//    | TessCoord = 13
//    | PatchVertices = 14
//    | FragCoord = 15
//    | PointCoord = 16
//    | FrontFacing = 17
//    | SampleId = 18
//    | SamplePosition = 19
//    | SampleMask = 20
//    | FragColor = 21
//    | FragDepth = 22
//    | HelperInvocation = 23
//    | NumWorkgroups = 24
//    | WorkgroupSize = 25
//    | WorkgroupId = 26
//    | LocalInvocationId = 27
//    | GlobalInvocationId = 28
//    | LocalInvocationIndex = 29
//    | WorkDim = 30
//    | GlobalSize = 31
//    | EnqueuedWorkgroupSize = 32
//    | GlobalOffset = 33
//    | GlobalLinearId = 34
//    | WorkgroupLinearId = 35
//    | SubgroupSize = 36
//    | SubgroupMaxSize = 37
//    | NumSubgroups = 38
//    | NumEnqueuedSubgroups = 39
//    | SubgroupId = 40
//    | SubgroupLocalInvocationId = 41
//
///// <summary>
///// Used by OpSelectionMerge.
///// </summary>
//type SelectionControl =
//    /// <summary>
//    /// No control requested.
//    /// </summary>
//    | NoControl = 0
//    /// <summary>
//    /// Strong request, to the extent possible, to remove the flow control for this selection.
//    /// </summary>
//    | Flatten = 1
//    /// <summary>
//    /// Strong request, to the extent possible, to keep this selection as flow control.
//    /// </summary>
//    | DontFlatten = 2
//
///// <summary>
///// Used by OpLoopMerge.
///// </summary>
//type LoopControl =
//    /// <summary>
//    /// No control requested.
//    /// </summary>
//    | NoControl = 0
//    /// <summary>
//    /// Strong request, to the extent possible, to unroll or unwind this loop.
//    /// </summary>
//    | Unroll = 1
//    /// <summary>
//    /// Strong request, to the extent possible, to keep this loop as a loop, without unrolling.
//    /// </summary>
//    | DontUnroll = 2
//
///// <summary>
///// Used by OpFunction.
///// </summary>
//type FunctionControlMask =
//    /// <summary>
//    /// Strong request, to the extent possible, to inline the function.
//    /// </summary>
//    | InLine = 1
//    /// <summary>
//    /// Strong request, to the extent possible, to not inline the function.
//    /// </summary>
//    | DontInline = 2
//    /// <summary>
//    /// Compiler can assume this function has no side effect, but might read global memory or read through dereferenced function parameters. Always computes the same result for the same argument values.
//    /// </summary>
//    | Pure = 4
//    /// <summary>
//    /// Compiler can assume this function has no side effects, and will not access global memory or dereference function parameters. Always computes the same result for the same argument values.
//    /// </summary>
//    | Const = 8
//
///// <summary>
///// Memory classification and ordering semantics.
///// Used by:
///// </summary>
//type MemorySemantics =
//    /// <summary>
//    /// TBD
//    /// </summary>
//    | Relaxed = 1
//    /// <summary>
//    /// All observers will see this memory access in the same order WRT to other sequentially-consistent memory accesses from this invocation.
//    /// </summary>
//    | SequentiallyConsistent = 2
//    /// <summary>
//    /// All memory operations provided in program order after this memory operation will execute after this memory operation.
//    /// </summary>
//    | Acquire = 4
//    /// <summary>
//    /// All memory operations provided in program order before this memory operation will execute before this memory operation.
//    /// </summary>
//    | Release = 8
//    /// <summary>
//    /// Filter the memory operations being constrained to just those accessing Uniform Storage Class memory.
//    /// </summary>
//    | Uniformmemory = 16
//    /// <summary>
//    /// The memory semantics only have to be correct WRT to this invocation&#8217;s subgroup memory.
//    /// </summary>
//    | Subgroupmemory = 32
//    /// <summary>
//    /// The memory semantics only have to be correct WRT to this invocation&#8217;s local workgroup memory.
//    /// </summary>
//    | Workgrouplocalmemory = 64
//    /// <summary>
//    /// The memory semantics only have to be correct WRT to this invocation&#8217;s global workgroup memory.
//    /// </summary>
//    | Workgroupglobalmemory = 128
//    /// <summary>
//    /// Filter the memory operations being constrained to just those accessing AtomicCounter Storage Class memory.
//    /// </summary>
//    | Atomiccountermemory = 256
//    /// <summary>
//    /// Filter the memory operations being constrained to just those accessing images (see OpTypeSampler Content).
//    /// </summary>
//    | Imagememory = 512
//
///// <summary>
///// Memory access semantics.
///// </summary>
//type MemoryAccess =
//    /// <summary>
//    /// This access cannot be optimized away; it has to be executed.
//    /// </summary>
//    | Volatile = 1
//    /// <summary>
//    /// This access has a known alignment, provided as a literal in the next operand.
//    /// </summary>
//    | Aligned = 2
//
///// <summary>
///// Scope of execution.
///// Used by:
///// </summary>
//type ExecutionScope =
//    /// <summary>
//    /// Everything executing on all the execution devices in the system.
//    /// </summary>
//    | CrossDevice = 0
//    /// <summary>
//    /// Everything executing on the device executing this invocation.
//    /// </summary>
//    | Device = 1
//    /// <summary>
//    /// All invocations for the invoking workgroup.
//    /// </summary>
//    | Workgroup = 2
//    /// <summary>
//    /// All invocations in the currently executing subgroup.
//    /// </summary>
//    | Subgroup = 3
//
///// <summary>
///// Defines the class of workgroup or subgroup operation.
///// Used by:
///// </summary>
//type GroupOperation =
//    /// <summary>
//    /// Returns the result of a reduction operation for all values of a specific value X specified by workitems within a workgroup.
//    /// </summary>
//    | Reduce = 0
//    /// <summary>
//    /// The inclusive scan performs a binary operation with an identity I and n (where n is the size of the workgroup) elements[a0, a1, &#8230; an-1] and returns [a0, (a0 op a1), &#8230;(a0 op a1 op &#8230; op an-1)]
//    /// </summary>
//    | InclusiveScan = 1
//    /// <summary>
//    /// The exclusive scan performs a binary operation with an identity I and n (where n is the size of the workgroup) elements[a0, a1, &#8230; an-1] and returns [I, a0, (a0 op a1), &#8230; (a0 op a1 op &#8230; op an-2)].
//    /// </summary>
//    | ExclusiveScan = 2
//
///// <summary>
///// Specify when the child kernel begins execution.
///// Note: Implementations are not required to honor this flag.  Implementations may not schedule kernel launch earlier than the point specified by this flag, however.
///// Used by OpEnqueueKernel.
///// </summary>
//type KernelEnqueueFlags =
//    /// <summary>
//    /// Indicates that the enqueued kernels do not need to wait for the parent kernel to finish execution before they begin execution.
//    /// </summary>
//    | NoWait = 0
//    /// <summary>
//    /// Indicates that all work-items of the parent kernel must finish executing and all immediate side effects committed before the enqueued child kernel may begin execution.
//    /// Note: Immediate meaning not side effects resulting from child kernels. The side effects would include stores to global memory and pipe reads and writes.
//    /// </summary>
//    | WaitKernel = 1
//    /// <summary>
//    /// Indicates that the enqueued kernels wait only for the workgroup that enqueued the kernels to finish before they begin execution.
//    /// Note: This acts as a memory synchronization point between work-items in a work-group and child kernels enqueued by work-items in the work-group.
//    /// </summary>
//    | WaitWorkGroup = 2
//
///// <summary>
///// Specify the profiling information to be queried.
///// Used by OpCaptureEventProfilingInfo.
///// </summary>
//type KernelProfilingInfo =
//    | CmdExecTime = 1
//
