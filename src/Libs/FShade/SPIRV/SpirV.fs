namespace SpirV
open System

type SourceLanguage =
    | Unknown = 0
    | ESSL = 1
    | GLSL = 2
    | OpenCL_C = 3
    | OpenCL_CPP = 4

type ExecutionModel =
    | Vertex = 0
    | TessellationControl = 1
    | TessellationEvaluation = 2
    | Geometry = 3
    | Fragment = 4
    | GLCompute = 5
    | Kernel = 6

type AddressingModel =
    | Logical = 0
    | Physical32 = 1
    | Physical64 = 2

type MemoryModel =
    | Simple = 0
    | GLSL450 = 1
    | OpenCL = 2

type ExecutionMode =
    | Invocations = 0
    | SpacingEqual = 1
    | SpacingFractionalEven = 2
    | SpacingFractionalOdd = 3
    | VertexOrderCw = 4
    | VertexOrderCcw = 5
    | PixelCenterInteger = 6
    | OriginUpperLeft = 7
    | OriginLowerLeft = 8
    | EarlyFragmentTests = 9
    | PointMode = 10
    | Xfb = 11
    | DepthReplacing = 12
    | DepthGreater = 14
    | DepthLess = 15
    | DepthUnchanged = 16
    | LocalSize = 17
    | LocalSizeHint = 18
    | InputPoints = 19
    | InputLines = 20
    | InputLinesAdjacency = 21
    | Triangles = 22
    | InputTrianglesAdjacency = 23
    | Quads = 24
    | Isolines = 25
    | OutputVertices = 26
    | OutputPoints = 27
    | OutputLineStrip = 28
    | OutputTriangleStrip = 29
    | VecTypeHint = 30
    | ContractionOff = 31

type StorageClass =
    | UniformConstant = 0
    | Input = 1
    | Uniform = 2
    | Output = 3
    | Workgroup = 4
    | CrossWorkgroup = 5
    | Private = 6
    | Function = 7
    | Generic = 8
    | PushConstant = 9
    | AtomicCounter = 10
    | Image = 11

type Dim =
    | Dim1D = 0
    | Dim2D = 1
    | Dim3D = 2
    | Cube = 3
    | Rect = 4
    | Buffer = 5
    | SubpassData = 6

type SamplerAddressingMode =
    | None = 0
    | ClampToEdge = 1
    | Clamp = 2
    | Repeat = 3
    | RepeatMirrored = 4

type SamplerFilterMode =
    | Nearest = 0
    | Linear = 1

type ImageFormat =
    | Unknown = 0
    | Rgba32f = 1
    | Rgba16f = 2
    | R32f = 3
    | Rgba8 = 4
    | Rgba8Snorm = 5
    | Rg32f = 6
    | Rg16f = 7
    | R11fG11fB10f = 8
    | R16f = 9
    | Rgba16 = 10
    | Rgb10A2 = 11
    | Rg16 = 12
    | Rg8 = 13
    | R16 = 14
    | R8 = 15
    | Rgba16Snorm = 16
    | Rg16Snorm = 17
    | Rg8Snorm = 18
    | R16Snorm = 19
    | R8Snorm = 20
    | Rgba32i = 21
    | Rgba16i = 22
    | Rgba8i = 23
    | R32i = 24
    | Rg32i = 25
    | Rg16i = 26
    | Rg8i = 27
    | R16i = 28
    | R8i = 29
    | Rgba32ui = 30
    | Rgba16ui = 31
    | Rgba8ui = 32
    | R32ui = 33
    | Rgb10a2ui = 34
    | Rg32ui = 35
    | Rg16ui = 36
    | Rg8ui = 37
    | R16ui = 38
    | R8ui = 39

type ImageChannelOrder =
    | R = 0
    | A = 1
    | RG = 2
    | RA = 3
    | RGB = 4
    | RGBA = 5
    | BGRA = 6
    | ARGB = 7
    | Intensity = 8
    | Luminance = 9
    | Rx = 10
    | RGx = 11
    | RGBx = 12
    | Depth = 13
    | DepthStencil = 14
    | sRGB = 15
    | sRGBx = 16
    | sRGBA = 17
    | sBGRA = 18

type ImageChannelDataType =
    | SnormInt8 = 0
    | SnormInt16 = 1
    | UnormInt8 = 2
    | UnormInt16 = 3
    | UnormShort565 = 4
    | UnormShort555 = 5
    | UnormInt101010 = 6
    | SignedInt8 = 7
    | SignedInt16 = 8
    | SignedInt32 = 9
    | UnsignedInt8 = 10
    | UnsignedInt16 = 11
    | UnsignedInt32 = 12
    | HalfFloat = 13
    | Float = 14
    | UnormInt24 = 15
    | UnormInt101010_2 = 16

[<Flags>]
type ImageOperands =
    | None = 0x00000000
    | Bias = 0x00000001
    | Lod = 0x00000002
    | Grad = 0x00000004
    | ConstOffset = 0x00000008
    | Offset = 0x00000010
    | ConstOffsets = 0x00000020
    | Sample = 0x00000040
    | MinLod = 0x00000080

[<Flags>]
type FPFastMathMode =
    | None = 0x00000000
    | NotNaN = 0x00000001
    | NotInf = 0x00000002
    | NSZ = 0x00000004
    | AllowRecip = 0x00000008
    | Fast = 0x00000010

type FPRoundingMode =
    | RTE = 0
    | RTZ = 1
    | RTP = 2
    | RTN = 3

type LinkageType =
    | Export = 0
    | Import = 1

type AccessQualifier =
    | ReadOnly = 0
    | WriteOnly = 1
    | ReadWrite = 2

type FunctionParameterAttribute =
    | Zext = 0
    | Sext = 1
    | ByVal = 2
    | Sret = 3
    | NoAlias = 4
    | NoCapture = 5
    | NoWrite = 6
    | NoReadWrite = 7

type Decoration =
    | RelaxedPrecision = 0
    | SpecId = 1
    | Block = 2
    | BufferBlock = 3
    | RowMajor = 4
    | ColMajor = 5
    | ArrayStride = 6
    | MatrixStride = 7
    | GLSLShared = 8
    | GLSLPacked = 9
    | CPacked = 10
    | BuiltIn = 11
    | NoPerspective = 13
    | Flat = 14
    | Patch = 15
    | Centroid = 16
    | Sample = 17
    | Invariant = 18
    | Restrict = 19
    | Aliased = 20
    | Volatile = 21
    | Constant = 22
    | Coherent = 23
    | NonWritable = 24
    | NonReadable = 25
    | Uniform = 26
    | SaturatedConversion = 28
    | Stream = 29
    | Location = 30
    | Component = 31
    | Index = 32
    | Binding = 33
    | DescriptorSet = 34
    | Offset = 35
    | XfbBuffer = 36
    | XfbStride = 37
    | FuncParamAttr = 38
    | FPRoundingMode = 39
    | FPFastMathMode = 40
    | LinkageAttributes = 41
    | NoContraction = 42
    | InputAttachmentIndex = 43
    | Alignment = 44

type BuiltIn =
    | Position = 0
    | PointSize = 1
    | ClipDistance = 3
    | CullDistance = 4
    | VertexId = 5
    | InstanceId = 6
    | PrimitiveId = 7
    | InvocationId = 8
    | Layer = 9
    | ViewportIndex = 10
    | TessLevelOuter = 11
    | TessLevelInner = 12
    | TessCoord = 13
    | PatchVertices = 14
    | FragCoord = 15
    | PointCoord = 16
    | FrontFacing = 17
    | SampleId = 18
    | SamplePosition = 19
    | SampleMask = 20
    | FragDepth = 22
    | HelperInvocation = 23
    | NumWorkgroups = 24
    | WorkgroupSize = 25
    | WorkgroupId = 26
    | LocalInvocationId = 27
    | GlobalInvocationId = 28
    | LocalInvocationIndex = 29
    | WorkDim = 30
    | GlobalSize = 31
    | EnqueuedWorkgroupSize = 32
    | GlobalOffset = 33
    | GlobalLinearId = 34
    | SubgroupSize = 36
    | SubgroupMaxSize = 37
    | NumSubgroups = 38
    | NumEnqueuedSubgroups = 39
    | SubgroupId = 40
    | SubgroupLocalInvocationId = 41
    | VertexIndex = 42
    | InstanceIndex = 43

[<Flags>]
type SelectionControl =
    | None = 0x00000000
    | Flatten = 0x00000001
    | DontFlatten = 0x00000002

[<Flags>]
type LoopControl =
    | None = 0x00000000
    | Unroll = 0x00000001
    | DontUnroll = 0x00000002

[<Flags>]
type FunctionControl =
    | None = 0x00000000
    | Inline = 0x00000001
    | DontInline = 0x00000002
    | Pure = 0x00000004
    | Const = 0x00000008

[<Flags>]
type MemorySemantics =
    | None = 0x00000000
    | Acquire = 0x00000002
    | Release = 0x00000004
    | AcquireRelease = 0x00000008
    | SequentiallyConsistent = 0x00000010
    | UniformMemory = 0x00000040
    | SubgroupMemory = 0x00000080
    | WorkgroupMemory = 0x00000100
    | CrossWorkgroupMemory = 0x00000200
    | AtomicCounterMemory = 0x00000400
    | ImageMemory = 0x00000800

[<Flags>]
type MemoryAccess =
    | None = 0x00000000
    | Volatile = 0x00000001
    | Aligned = 0x00000002
    | Nontemporal = 0x00000004

type Scope =
    | CrossDevice = 0
    | Device = 1
    | Workgroup = 2
    | Subgroup = 3
    | Invocation = 4

type GroupOperation =
    | Reduce = 0
    | InclusiveScan = 1
    | ExclusiveScan = 2

type KernelEnqueueFlags =
    | NoWait = 0
    | WaitKernel = 1
    | WaitWorkGroup = 2

[<Flags>]
type KernelProfilingInfo =
    | None = 0x00000000
    | CmdExecTime = 0x00000001

type Capability =
    | Matrix = 0
    | Shader = 1
    | Geometry = 2
    | Tessellation = 3
    | Addresses = 4
    | Linkage = 5
    | Kernel = 6
    | Vector16 = 7
    | Float16Buffer = 8
    | Float16 = 9
    | Float64 = 10
    | Int64 = 11
    | Int64Atomics = 12
    | ImageBasic = 13
    | ImageReadWrite = 14
    | ImageMipmap = 15
    | Pipes = 17
    | Groups = 18
    | DeviceEnqueue = 19
    | LiteralSampler = 20
    | AtomicStorage = 21
    | Int16 = 22
    | TessellationPointSize = 23
    | GeometryPointSize = 24
    | ImageGatherExtended = 25
    | StorageImageMultisample = 27
    | UniformBufferArrayDynamicIndexing = 28
    | SampledImageArrayDynamicIndexing = 29
    | StorageBufferArrayDynamicIndexing = 30
    | StorageImageArrayDynamicIndexing = 31
    | ClipDistance = 32
    | CullDistance = 33
    | ImageCubeArray = 34
    | SampleRateShading = 35
    | ImageRect = 36
    | SampledRect = 37
    | GenericPointer = 38
    | Int8 = 39
    | InputAttachment = 40
    | SparseResidency = 41
    | MinLod = 42
    | Sampled1D = 43
    | Image1D = 44
    | SampledCubeArray = 45
    | SampledBuffer = 46
    | ImageBuffer = 47
    | ImageMSArray = 48
    | StorageImageExtendedFormats = 49
    | ImageQuery = 50
    | DerivativeControl = 51
    | InterpolationFunction = 52
    | TransformFeedback = 53
    | GeometryStreams = 54
    | StorageImageReadWithoutFormat = 55
    | StorageImageWriteWithoutFormat = 56

type Op =
    | OpNop = 0
    | OpUndef = 1
    | OpSourceContinued = 2
    | OpSource = 3
    | OpSourceExtension = 4
    | OpName = 5
    | OpMemberName = 6
    | OpString = 7
    | OpLine = 8
    | OpExtension = 10
    | OpExtInstImport = 11
    | OpExtInst = 12
    | OpMemoryModel = 14
    | OpEntryPoint = 15
    | OpExecutionMode = 16
    | OpCapability = 17
    | OpTypeVoid = 19
    | OpTypeBool = 20
    | OpTypeInt = 21
    | OpTypeFloat = 22
    | OpTypeVector = 23
    | OpTypeMatrix = 24
    | OpTypeImage = 25
    | OpTypeSampler = 26
    | OpTypeSampledImage = 27
    | OpTypeArray = 28
    | OpTypeRuntimeArray = 29
    | OpTypeStruct = 30
    | OpTypeOpaque = 31
    | OpTypePointer = 32
    | OpTypeFunction = 33
    | OpTypeEvent = 34
    | OpTypeDeviceEvent = 35
    | OpTypeReserveId = 36
    | OpTypeQueue = 37
    | OpTypePipe = 38
    | OpTypeForwardPointer = 39
    | OpConstantTrue = 41
    | OpConstantFalse = 42
    | OpConstant = 43
    | OpConstantComposite = 44
    | OpConstantSampler = 45
    | OpConstantNull = 46
    | OpSpecConstantTrue = 48
    | OpSpecConstantFalse = 49
    | OpSpecConstant = 50
    | OpSpecConstantComposite = 51
    | OpSpecConstantOp = 52
    | OpFunction = 54
    | OpFunctionParameter = 55
    | OpFunctionEnd = 56
    | OpFunctionCall = 57
    | OpVariable = 59
    | OpImageTexelPointer = 60
    | OpLoad = 61
    | OpStore = 62
    | OpCopyMemory = 63
    | OpCopyMemorySized = 64
    | OpAccessChain = 65
    | OpInBoundsAccessChain = 66
    | OpPtrAccessChain = 67
    | OpArrayLength = 68
    | OpGenericPtrMemSemantics = 69
    | OpInBoundsPtrAccessChain = 70
    | OpDecorate = 71
    | OpMemberDecorate = 72
    | OpDecorationGroup = 73
    | OpGroupDecorate = 74
    | OpGroupMemberDecorate = 75
    | OpVectorExtractDynamic = 77
    | OpVectorInsertDynamic = 78
    | OpVectorShuffle = 79
    | OpCompositeConstruct = 80
    | OpCompositeExtract = 81
    | OpCompositeInsert = 82
    | OpCopyObject = 83
    | OpTranspose = 84
    | OpSampledImage = 86
    | OpImageSampleImplicitLod = 87
    | OpImageSampleExplicitLod = 88
    | OpImageSampleDrefImplicitLod = 89
    | OpImageSampleDrefExplicitLod = 90
    | OpImageSampleProjImplicitLod = 91
    | OpImageSampleProjExplicitLod = 92
    | OpImageSampleProjDrefImplicitLod = 93
    | OpImageSampleProjDrefExplicitLod = 94
    | OpImageFetch = 95
    | OpImageGather = 96
    | OpImageDrefGather = 97
    | OpImageRead = 98
    | OpImageWrite = 99
    | OpImage = 100
    | OpImageQueryFormat = 101
    | OpImageQueryOrder = 102
    | OpImageQuerySizeLod = 103
    | OpImageQuerySize = 104
    | OpImageQueryLod = 105
    | OpImageQueryLevels = 106
    | OpImageQuerySamples = 107
    | OpConvertFToU = 109
    | OpConvertFToS = 110
    | OpConvertSToF = 111
    | OpConvertUToF = 112
    | OpUConvert = 113
    | OpSConvert = 114
    | OpFConvert = 115
    | OpQuantizeToF16 = 116
    | OpConvertPtrToU = 117
    | OpSatConvertSToU = 118
    | OpSatConvertUToS = 119
    | OpConvertUToPtr = 120
    | OpPtrCastToGeneric = 121
    | OpGenericCastToPtr = 122
    | OpGenericCastToPtrExplicit = 123
    | OpBitcast = 124
    | OpSNegate = 126
    | OpFNegate = 127
    | OpIAdd = 128
    | OpFAdd = 129
    | OpISub = 130
    | OpFSub = 131
    | OpIMul = 132
    | OpFMul = 133
    | OpUDiv = 134
    | OpSDiv = 135
    | OpFDiv = 136
    | OpUMod = 137
    | OpSRem = 138
    | OpSMod = 139
    | OpFRem = 140
    | OpFMod = 141
    | OpVectorTimesScalar = 142
    | OpMatrixTimesScalar = 143
    | OpVectorTimesMatrix = 144
    | OpMatrixTimesVector = 145
    | OpMatrixTimesMatrix = 146
    | OpOuterProduct = 147
    | OpDot = 148
    | OpIAddCarry = 149
    | OpISubBorrow = 150
    | OpUMulExtended = 151
    | OpSMulExtended = 152
    | OpAny = 154
    | OpAll = 155
    | OpIsNan = 156
    | OpIsInf = 157
    | OpIsFinite = 158
    | OpIsNormal = 159
    | OpSignBitSet = 160
    | OpLessOrGreater = 161
    | OpOrdered = 162
    | OpUnordered = 163
    | OpLogicalEqual = 164
    | OpLogicalNotEqual = 165
    | OpLogicalOr = 166
    | OpLogicalAnd = 167
    | OpLogicalNot = 168
    | OpSelect = 169
    | OpIEqual = 170
    | OpINotEqual = 171
    | OpUGreaterThan = 172
    | OpSGreaterThan = 173
    | OpUGreaterThanEqual = 174
    | OpSGreaterThanEqual = 175
    | OpULessThan = 176
    | OpSLessThan = 177
    | OpULessThanEqual = 178
    | OpSLessThanEqual = 179
    | OpFOrdEqual = 180
    | OpFUnordEqual = 181
    | OpFOrdNotEqual = 182
    | OpFUnordNotEqual = 183
    | OpFOrdLessThan = 184
    | OpFUnordLessThan = 185
    | OpFOrdGreaterThan = 186
    | OpFUnordGreaterThan = 187
    | OpFOrdLessThanEqual = 188
    | OpFUnordLessThanEqual = 189
    | OpFOrdGreaterThanEqual = 190
    | OpFUnordGreaterThanEqual = 191
    | OpShiftRightLogical = 194
    | OpShiftRightArithmetic = 195
    | OpShiftLeftLogical = 196
    | OpBitwiseOr = 197
    | OpBitwiseXor = 198
    | OpBitwiseAnd = 199
    | OpNot = 200
    | OpBitFieldInsert = 201
    | OpBitFieldSExtract = 202
    | OpBitFieldUExtract = 203
    | OpBitReverse = 204
    | OpBitCount = 205
    | OpDPdx = 207
    | OpDPdy = 208
    | OpFwidth = 209
    | OpDPdxFine = 210
    | OpDPdyFine = 211
    | OpFwidthFine = 212
    | OpDPdxCoarse = 213
    | OpDPdyCoarse = 214
    | OpFwidthCoarse = 215
    | OpEmitVertex = 218
    | OpEndPrimitive = 219
    | OpEmitStreamVertex = 220
    | OpEndStreamPrimitive = 221
    | OpControlBarrier = 224
    | OpMemoryBarrier = 225
    | OpAtomicLoad = 227
    | OpAtomicStore = 228
    | OpAtomicExchange = 229
    | OpAtomicCompareExchange = 230
    | OpAtomicCompareExchangeWeak = 231
    | OpAtomicIIncrement = 232
    | OpAtomicIDecrement = 233
    | OpAtomicIAdd = 234
    | OpAtomicISub = 235
    | OpAtomicSMin = 236
    | OpAtomicUMin = 237
    | OpAtomicSMax = 238
    | OpAtomicUMax = 239
    | OpAtomicAnd = 240
    | OpAtomicOr = 241
    | OpAtomicXor = 242
    | OpPhi = 245
    | OpLoopMerge = 246
    | OpSelectionMerge = 247
    | OpLabel = 248
    | OpBranch = 249
    | OpBranchConditional = 250
    | OpSwitch = 251
    | OpKill = 252
    | OpReturn = 253
    | OpReturnValue = 254
    | OpUnreachable = 255
    | OpLifetimeStart = 256
    | OpLifetimeStop = 257
    | OpGroupAsyncCopy = 259
    | OpGroupWaitEvents = 260
    | OpGroupAll = 261
    | OpGroupAny = 262
    | OpGroupBroadcast = 263
    | OpGroupIAdd = 264
    | OpGroupFAdd = 265
    | OpGroupFMin = 266
    | OpGroupUMin = 267
    | OpGroupSMin = 268
    | OpGroupFMax = 269
    | OpGroupUMax = 270
    | OpGroupSMax = 271
    | OpReadPipe = 274
    | OpWritePipe = 275
    | OpReservedReadPipe = 276
    | OpReservedWritePipe = 277
    | OpReserveReadPipePackets = 278
    | OpReserveWritePipePackets = 279
    | OpCommitReadPipe = 280
    | OpCommitWritePipe = 281
    | OpIsValidReserveId = 282
    | OpGetNumPipePackets = 283
    | OpGetMaxPipePackets = 284
    | OpGroupReserveReadPipePackets = 285
    | OpGroupReserveWritePipePackets = 286
    | OpGroupCommitReadPipe = 287
    | OpGroupCommitWritePipe = 288
    | OpEnqueueMarker = 291
    | OpEnqueueKernel = 292
    | OpGetKernelNDrangeSubGroupCount = 293
    | OpGetKernelNDrangeMaxSubGroupSize = 294
    | OpGetKernelWorkGroupSize = 295
    | OpGetKernelPreferredWorkGroupSizeMultiple = 296
    | OpRetainEvent = 297
    | OpReleaseEvent = 298
    | OpCreateUserEvent = 299
    | OpIsValidEvent = 300
    | OpSetUserEventStatus = 301
    | OpCaptureEventProfilingInfo = 302
    | OpGetDefaultQueue = 303
    | OpBuildNDRange = 304
    | OpImageSparseSampleImplicitLod = 305
    | OpImageSparseSampleExplicitLod = 306
    | OpImageSparseSampleDrefImplicitLod = 307
    | OpImageSparseSampleDrefExplicitLod = 308
    | OpImageSparseSampleProjImplicitLod = 309
    | OpImageSparseSampleProjExplicitLod = 310
    | OpImageSparseSampleProjDrefImplicitLod = 311
    | OpImageSparseSampleProjDrefExplicitLod = 312
    | OpImageSparseFetch = 313
    | OpImageSparseGather = 314
    | OpImageSparseDrefGather = 315
    | OpImageSparseTexelsResident = 316
    | OpNoLine = 317
    | OpAtomicFlagTestAndSet = 318
    | OpAtomicFlagClear = 319



type Instruction = 
    | OpNop
    | OpUndef of resultType : uint32 * result : uint32
    | OpSourceContinued of sourceCont : string
    | OpSource of _source : SourceLanguage * version : uint32 * code : string
    | OpSourceExtension of extension : string
    | OpName of _target : uint32 * name : string
    | OpMemberName of _type : uint32 * _member : uint32 * name : string
    | OpString of result : uint32 * value : string
    | OpLine of file : uint32 * line : uint32 * column : uint32
    | OpExtension of name : string
    | OpExtInstImport of result : uint32 * name : string
    | OpExtInst of resultType : uint32 * result : uint32 * set : uint32 * instruction : uint32 * operands : uint32[]
    | OpMemoryModel of addressing : AddressingModel * memory : MemoryModel
    | OpEntryPoint of executionModel : ExecutionModel * entryPoint : uint32 * name : string
    | OpExecutionMode of entryPoint : uint32 * mode : ExecutionMode * modes : Option<uint32>
    | OpCapability of capability : Capability
    | OpTypeVoid of result : uint32
    | OpTypeBool of result : uint32
    | OpTypeInt of result : uint32 * width : uint32 * signedness : uint32
    | OpTypeFloat of result : uint32 * width : uint32
    | OpTypeVector of result : uint32 * compType : uint32 * compCount : uint32
    | OpTypeMatrix of result : uint32 * colType : uint32 * colCount : uint32
    | OpTypeImage of result : uint32 * sampledType : uint32 * dim : Dim * depth : uint32 * arrayed : uint32 * ms : uint32 * sampled : uint32 * format : int * access : Option<AccessQualifier>
    | OpTypeSampler of result : uint32
    | OpTypeSampledImage of result : uint32 * imageType : uint32
    | OpTypeArray of result : uint32 * elementType : uint32 * length : uint32
    | OpTypeRuntimeArray of result : uint32 * elementType : uint32
    | OpTypeStruct of result : uint32 * memberTypes : uint32[]
    | OpTypeOpaque of result : uint32 * opaqueTypeName : string
    | OpTypePointer of result : uint32 * storage : StorageClass * _type : uint32
    | OpTypeFunction of result : uint32 * retType : uint32 * paramTypes : uint32[]
    | OpTypeEvent of result : uint32
    | OpTypeDeviceEvent of result : uint32
    | OpTypeReserveId of result : uint32
    | OpTypeQueue of result : uint32
    | OpTypePipe of result : uint32 * qualifier : AccessQualifier
    | OpTypeForwardPointer of ptrType : uint32 * storage : StorageClass
    | OpConstantTrue of resultType : uint32 * result : uint32
    | OpConstantFalse of resultType : uint32 * result : uint32
    | OpConstant of resultType : uint32 * result : uint32 * value : uint32[]
    | OpConstantComposite of resultType : uint32 * result : uint32 * constituents : uint32[]
    | OpConstantSampler of resultType : uint32 * result : uint32 * addressingMode : SamplerAddressingMode * param : uint32 * filterMode : SamplerFilterMode
    | OpConstantNull of resultType : uint32 * result : uint32
    | OpSpecConstantTrue of resultType : uint32 * result : uint32
    | OpSpecConstantFalse of resultType : uint32 * result : uint32
    | OpSpecConstant of resultType : uint32 * result : uint32 * value : uint32[]
    | OpSpecConstantComposite of resultType : uint32 * result : uint32 * constituents : uint32[]
    | OpSpecConstantOp of resultType : uint32 * result : uint32 * opCode : uint32 * operands : uint32[]
    | OpFunction of resultType : uint32 * result : uint32 * _function : FunctionControl * funType : uint32
    | OpFunctionParameter of resultType : uint32 * result : uint32
    | OpFunctionEnd
    | OpFunctionCall of resultType : uint32 * result : uint32 * _function : uint32 * args : uint32[]
    | OpVariable of resultType : uint32 * result : uint32 * storage : StorageClass * initializer : Option<uint32>
    | OpImageTexelPointer of resultType : uint32 * result : uint32 * image : uint32 * coord : uint32 * sample : uint32
    | OpLoad of resultType : uint32 * result : uint32 * ptr : uint32 * access : Option<MemoryAccess>
    | OpStore of ptr : uint32 * _object : uint32 * access : Option<MemoryAccess>
    | OpCopyMemory of _target : uint32 * _source : uint32 * access : Option<MemoryAccess>
    | OpCopyMemorySized of _target : uint32 * _source : uint32 * size : uint32 * access : Option<MemoryAccess>
    | OpAccessChain of resultType : uint32 * result : uint32 * _base : uint32 * indices : uint32[]
    | OpInBoundsAccessChain of resultType : uint32 * result : uint32 * _base : uint32 * indices : uint32[]
    | OpPtrAccessChain of resultType : uint32 * result : uint32 * _base : uint32 * element : uint32 * indices : uint32[]
    | OpArrayLength of resultType : uint32 * result : uint32 * structure : uint32 * arrMember : uint32
    | OpGenericPtrMemSemantics of resultType : uint32 * result : uint32 * ptr : uint32
    | OpInBoundsPtrAccessChain of resultType : uint32 * result : uint32 * _base : uint32 * element : uint32 * indices : uint32[]
    | OpDecorate of _target : uint32 * decoration : Decoration * args : uint32[]
    | OpMemberDecorate of structType : uint32 * _member : uint32 * decoration : Decoration * args : uint32[]
    | OpDecorationGroup of result : uint32
    | OpGroupDecorate of group : uint32 * targets : uint32[]
    | OpGroupMemberDecorate of group : uint32 * targets : uint32[]
    | OpVectorExtractDynamic of resultType : uint32 * result : uint32 * vector : uint32 * index : uint32
    | OpVectorInsertDynamic of resultType : uint32 * result : uint32 * vector : uint32 * _component : uint32 * index : uint32
    | OpVectorShuffle of resultType : uint32 * result : uint32 * v0 : uint32 * v1 : uint32 * components : uint32[]
    | OpCompositeConstruct of resultType : uint32 * result : uint32 * constituents : uint32[]
    | OpCompositeExtract of resultType : uint32 * result : uint32 * composite : uint32 * indices : uint32[]
    | OpCompositeInsert of resultType : uint32 * result : uint32 * _object : uint32 * composite : uint32 * indices : uint32[]
    | OpCopyObject of resultType : uint32 * result : uint32 * operand : uint32
    | OpTranspose of resultType : uint32 * result : uint32 * mat : uint32
    | OpSampledImage of resultType : uint32 * result : uint32 * image : uint32 * sampler : uint32
    | OpImageSampleImplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * images : int * variables : uint32[]
    | OpImageSampleExplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * images : int * variables : uint32[]
    | OpImageSampleDrefImplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * depthRef : uint32 * images : int * variables : uint32[]
    | OpImageSampleDrefExplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * depthRef : uint32 * images : int * variables : uint32[]
    | OpImageSampleProjImplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * images : int * variables : uint32[]
    | OpImageSampleProjExplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * images : int * variables : uint32[]
    | OpImageSampleProjDrefImplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * depthRef : uint32 * images : int * variables : uint32[]
    | OpImageSampleProjDrefExplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * depthRef : uint32 * images : int * variables : uint32[]
    | OpImageFetch of resultType : uint32 * result : uint32 * image : uint32 * coord : uint32 * images : int * variables : uint32[]
    | OpImageGather of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * _component : uint32 * images : int * variables : uint32[]
    | OpImageDrefGather of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * depthRef : uint32 * images : int * variables : uint32[]
    | OpImageRead of resultType : uint32 * result : uint32 * image : uint32 * coords : uint32 * images : int * variables : uint32[]
    | OpImageWrite of image : uint32 * coord : uint32 * texel : uint32 * images : int * variables : uint32[]
    | OpImage of resultType : uint32 * result : uint32 * sampledImage : uint32
    | OpImageQueryFormat of resultType : uint32 * result : uint32 * image : uint32
    | OpImageQueryOrder of resultType : uint32 * result : uint32 * image : uint32
    | OpImageQuerySizeLod of resultType : uint32 * result : uint32 * image : uint32 * lod : uint32
    | OpImageQuerySize of resultType : uint32 * result : uint32 * image : uint32
    | OpImageQueryLod of resultType : uint32 * result : uint32 * image : uint32 * coord : uint32
    | OpImageQueryLevels of resultType : uint32 * result : uint32 * image : uint32
    | OpImageQuerySamples of resultType : uint32 * result : uint32 * image : uint32
    | OpConvertFToU of resultType : uint32 * result : uint32 * fvec : uint32
    | OpConvertFToS of resultType : uint32 * result : uint32 * floatVal : uint32
    | OpConvertSToF of resultType : uint32 * result : uint32 * sVal : uint32
    | OpConvertUToF of resultType : uint32 * result : uint32 * uVal : uint32
    | OpUConvert of resultType : uint32 * result : uint32 * uVal : uint32
    | OpSConvert of resultType : uint32 * result : uint32 * sVal : uint32
    | OpFConvert of resultType : uint32 * result : uint32 * floatVal : uint32
    | OpQuantizeToF16 of resultType : uint32 * result : uint32 * value : uint32
    | OpConvertPtrToU of resultType : uint32 * result : uint32 * ptr : uint32
    | OpSatConvertSToU of resultType : uint32 * result : uint32 * sVal : uint32
    | OpSatConvertUToS of resultType : uint32 * result : uint32 * uVal : uint32
    | OpConvertUToPtr of resultType : uint32 * result : uint32 * intVal : uint32
    | OpPtrCastToGeneric of resultType : uint32 * result : uint32 * ptr : uint32
    | OpGenericCastToPtr of resultType : uint32 * result : uint32 * ptr : uint32
    | OpGenericCastToPtrExplicit of resultType : uint32 * result : uint32 * ptr : uint32 * storage : StorageClass
    | OpBitcast of resultType : uint32 * result : uint32 * operand : uint32
    | OpSNegate of resultType : uint32 * result : uint32 * operand : uint32
    | OpFNegate of resultType : uint32 * result : uint32 * operand : uint32
    | OpIAdd of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFAdd of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpISub of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFSub of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpIMul of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFMul of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpUDiv of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpSDiv of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFDiv of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpUMod of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpSRem of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpSMod of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFRem of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFMod of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpVectorTimesScalar of resultType : uint32 * result : uint32 * vec : uint32 * scalar : uint32
    | OpMatrixTimesScalar of resultType : uint32 * result : uint32 * mat : uint32 * scalar : uint32
    | OpVectorTimesMatrix of resultType : uint32 * result : uint32 * vec : uint32 * mat : uint32
    | OpMatrixTimesVector of resultType : uint32 * result : uint32 * mat : uint32 * vec : uint32
    | OpMatrixTimesMatrix of resultType : uint32 * result : uint32 * left : uint32 * right : uint32
    | OpOuterProduct of resultType : uint32 * result : uint32 * left : uint32 * right : uint32
    | OpDot of resultType : uint32 * result : uint32 * left : uint32 * right : uint32
    | OpIAddCarry of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpISubBorrow of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpUMulExtended of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpSMulExtended of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpAny of resultType : uint32 * result : uint32 * vec : uint32
    | OpAll of resultType : uint32 * result : uint32 * vec : uint32
    | OpIsNan of resultType : uint32 * result : uint32 * x : uint32
    | OpIsInf of resultType : uint32 * result : uint32 * y : uint32
    | OpIsFinite of resultType : uint32 * result : uint32 * x : uint32
    | OpIsNormal of resultType : uint32 * result : uint32 * x : uint32
    | OpSignBitSet of resultType : uint32 * result : uint32 * x : uint32
    | OpLessOrGreater of resultType : uint32 * result : uint32 * x : uint32 * y : uint32
    | OpOrdered of resultType : uint32 * result : uint32 * x : uint32 * y : uint32
    | OpUnordered of resultType : uint32 * result : uint32 * x : uint32 * y : uint32
    | OpLogicalEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpLogicalNotEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpLogicalOr of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpLogicalAnd of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpLogicalNot of resultType : uint32 * result : uint32 * op : uint32
    | OpSelect of resultType : uint32 * result : uint32 * cond : uint32 * o1 : uint32 * o2 : uint32
    | OpIEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpINotEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpUGreaterThan of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpSGreaterThan of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpUGreaterThanEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpSGreaterThanEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpULessThan of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpSLessThan of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpULessThanEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpSLessThanEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFOrdEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFUnordEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFOrdNotEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFUnordNotEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFOrdLessThan of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFUnordLessThan of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFOrdGreaterThan of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFUnordGreaterThan of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFOrdLessThanEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFUnordLessThanEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFOrdGreaterThanEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpFUnordGreaterThanEqual of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpShiftRightLogical of resultType : uint32 * result : uint32 * _base : uint32 * shift : uint32
    | OpShiftRightArithmetic of resultType : uint32 * result : uint32 * _base : uint32 * shift : uint32
    | OpShiftLeftLogical of resultType : uint32 * result : uint32 * _base : uint32 * shift : uint32
    | OpBitwiseOr of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpBitwiseXor of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpBitwiseAnd of resultType : uint32 * result : uint32 * op1 : uint32 * op2 : uint32
    | OpNot of resultType : uint32 * result : uint32 * operand : uint32
    | OpBitFieldInsert of resultType : uint32 * result : uint32 * _base : uint32 * insert : uint32 * offset : uint32 * count : uint32
    | OpBitFieldSExtract of resultType : uint32 * result : uint32 * _base : uint32 * offset : uint32 * count : uint32
    | OpBitFieldUExtract of resultType : uint32 * result : uint32 * _base : uint32 * offset : uint32 * count : uint32
    | OpBitReverse of resultType : uint32 * result : uint32 * _base : uint32
    | OpBitCount of resultType : uint32 * result : uint32 * _base : uint32
    | OpDPdx of resultType : uint32 * result : uint32 * p : uint32
    | OpDPdy of resultType : uint32 * result : uint32 * p : uint32
    | OpFwidth of resultType : uint32 * result : uint32 * p : uint32
    | OpDPdxFine of resultType : uint32 * result : uint32 * p : uint32
    | OpDPdyFine of resultType : uint32 * result : uint32 * p : uint32
    | OpFwidthFine of resultType : uint32 * result : uint32 * p : uint32
    | OpDPdxCoarse of resultType : uint32 * result : uint32 * p : uint32
    | OpDPdyCoarse of resultType : uint32 * result : uint32 * p : uint32
    | OpFwidthCoarse of resultType : uint32 * result : uint32 * p : uint32
    | OpEmitVertex
    | OpEndPrimitive
    | OpEmitStreamVertex of stream : uint32
    | OpEndStreamPrimitive of stream : uint32
    | OpControlBarrier of exec : Scope * mem : Scope * sem : MemorySemantics
    | OpMemoryBarrier of mem : Scope * sen : MemorySemantics
    | OpAtomicLoad of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics
    | OpAtomicStore of ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpAtomicExchange of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpAtomicCompareExchange of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * eq : MemorySemantics * neq : MemorySemantics * value : uint32 * comparator : uint32
    | OpAtomicCompareExchangeWeak of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * eq : MemorySemantics * neq : MemorySemantics * value : uint32 * comparator : uint32
    | OpAtomicIIncrement of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics
    | OpAtomicIDecrement of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics
    | OpAtomicIAdd of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpAtomicISub of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpAtomicSMin of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpAtomicUMin of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpAtomicSMax of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpAtomicUMax of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpAtomicAnd of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpAtomicOr of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpAtomicXor of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics * value : uint32
    | OpPhi of resultType : uint32 * result : uint32 * varsAndParents : uint32[]
    | OpLoopMerge of mergeBlock : uint32 * contTarget : uint32 * loop : LoopControl
    | OpSelectionMerge of mergeBlock : uint32 * select : SelectionControl
    | OpLabel of result : uint32
    | OpBranch of _target : uint32
    | OpBranchConditional of condition : uint32 * lTrue : uint32 * lFalse : uint32 * weights : uint32[]
    | OpSwitch of sel : uint32 * def : uint32 * _target : uint32[]
    | OpKill
    | OpReturn
    | OpReturnValue of value : uint32
    | OpUnreachable
    | OpLifetimeStart of ptr : uint32 * size : uint32
    | OpLifetimeStop of ptr : uint32 * size : uint32
    | OpGroupAsyncCopy of resultType : uint32 * result : uint32 * execution : Scope * dest : uint32 * _source : uint32 * num : uint32 * stride : uint32 * evt : uint32
    | OpGroupWaitEvents of exec : Scope * num : uint32 * evtList : uint32
    | OpGroupAll of resultType : uint32 * result : uint32 * exec : Scope * pred : uint32
    | OpGroupAny of resultType : uint32 * result : uint32 * exec : Scope * pred : uint32
    | OpGroupBroadcast of resultType : uint32 * result : uint32 * exec : Scope * value : uint32 * localid : uint32
    | OpGroupIAdd of resultType : uint32 * result : uint32 * exec : Scope * operation : GroupOperation * x : uint32
    | OpGroupFAdd of resultType : uint32 * result : uint32 * exec : Scope * operation : GroupOperation * x : uint32
    | OpGroupFMin of resultType : uint32 * result : uint32 * exec : Scope * operation : GroupOperation * x : uint32
    | OpGroupUMin of resultType : uint32 * result : uint32 * exec : Scope * operation : GroupOperation * x : uint32
    | OpGroupSMin of resultType : uint32 * result : uint32 * exec : Scope * operation : GroupOperation * x : uint32
    | OpGroupFMax of resultType : uint32 * result : uint32 * exec : Scope * operation : GroupOperation * x : uint32
    | OpGroupUMax of resultType : uint32 * result : uint32 * exec : Scope * operation : GroupOperation * x : uint32
    | OpGroupSMax of resultType : uint32 * result : uint32 * exec : Scope * operation : GroupOperation * x : uint32
    | OpReadPipe of resultType : uint32 * result : uint32 * pipe : uint32 * ptr : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpWritePipe of resultType : uint32 * result : uint32 * pipe : uint32 * ptr : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpReservedReadPipe of resultType : uint32 * result : uint32 * pipe : uint32 * reserveId : uint32 * index : uint32 * ptr : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpReservedWritePipe of resultType : uint32 * result : uint32 * pipe : uint32 * reserveId : uint32 * index : uint32 * ptr : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpReserveReadPipePackets of resultType : uint32 * result : uint32 * pipe : uint32 * numPackets : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpReserveWritePipePackets of resultType : uint32 * result : uint32 * pipe : uint32 * numPackets : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpCommitReadPipe of pipe : uint32 * reserveId : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpCommitWritePipe of pipe : uint32 * reserveId : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpIsValidReserveId of resultType : uint32 * result : uint32 * reserveId : uint32
    | OpGetNumPipePackets of resultType : uint32 * result : uint32 * pipe : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpGetMaxPipePackets of resultType : uint32 * result : uint32 * pipe : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpGroupReserveReadPipePackets of resultType : uint32 * result : uint32 * exec : Scope * pipe : uint32 * numPackets : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpGroupReserveWritePipePackets of resultType : uint32 * result : uint32 * exec : Scope * pipe : uint32 * numPackets : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpGroupCommitReadPipe of exec : Scope * pipe : uint32 * reserveId : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpGroupCommitWritePipe of exec : Scope * pipe : uint32 * reserveId : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpEnqueueMarker of resultType : uint32 * result : uint32 * queue : uint32 * nEvts : uint32 * waitEvts : uint32 * retEvt : uint32
    | OpEnqueueKernel of resultType : uint32 * result : uint32 * queue : uint32 * flags : uint32 * ndRange : uint32 * nEvents : uint32 * waitEvts : uint32 * retEvt : uint32 * invoke : uint32 * param : uint32 * paramSize : uint32 * paramAlign : uint32 * localSize : uint32[]
    | OpGetKernelNDrangeSubGroupCount of resultType : uint32 * result : uint32 * ndRange : uint32 * invoke : uint32 * param : uint32 * paramSize : uint32 * paramAlign : uint32
    | OpGetKernelNDrangeMaxSubGroupSize of resultType : uint32 * result : uint32 * ndRange : uint32 * invoke : uint32 * param : uint32 * paramSize : uint32 * paramAlign : uint32
    | OpGetKernelWorkGroupSize of resultType : uint32 * result : uint32 * invoke : uint32 * param : uint32 * paramSize : uint32 * paramAlign : uint32
    | OpGetKernelPreferredWorkGroupSizeMultiple of resultType : uint32 * result : uint32 * invoke : uint32 * param : uint32 * paramSize : uint32 * paramAlign : uint32
    | OpRetainEvent of evt : uint32
    | OpReleaseEvent of evt : uint32
    | OpCreateUserEvent of resultType : uint32 * result : uint32
    | OpIsValidEvent of resultType : uint32 * result : uint32 * evt : uint32
    | OpSetUserEventStatus of evt : uint32 * status : uint32
    | OpCaptureEventProfilingInfo of evt : uint32 * profileInfo : uint32 * value : uint32
    | OpGetDefaultQueue of resultType : uint32 * result : uint32
    | OpBuildNDRange of resultType : uint32 * result : uint32 * globalSize : uint32 * localSize : uint32 * globalOffset : uint32
    | OpImageSparseSampleImplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * images : int * variables : uint32[]
    | OpImageSparseSampleExplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * images : int * variables : uint32[]
    | OpImageSparseSampleDrefImplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * depthRef : uint32 * images : int * variables : uint32[]
    | OpImageSparseSampleDrefExplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * depthRef : uint32 * images : int * variables : uint32[]
    | OpImageSparseSampleProjImplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * images : int * variables : uint32[]
    | OpImageSparseSampleProjExplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * images : int * variables : uint32[]
    | OpImageSparseSampleProjDrefImplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * depthRef : uint32 * images : int * variables : uint32[]
    | OpImageSparseSampleProjDrefExplicitLod of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * depthRef : uint32 * images : int * variables : uint32[]
    | OpImageSparseFetch of resultType : uint32 * result : uint32 * image : uint32 * coord : uint32 * images : int * variables : uint32[]
    | OpImageSparseGather of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * _component : uint32 * images : int * variables : uint32[]
    | OpImageSparseDrefGather of resultType : uint32 * result : uint32 * sampledImage : uint32 * coord : uint32 * depthRef : uint32 * images : int * variables : uint32[]
    | OpImageSparseTexelsResident of resultType : uint32 * result : uint32 * residentCode : uint32
    | OpNoLine
    | OpAtomicFlagTestAndSet of resultType : uint32 * result : uint32 * ptr : uint32 * scope : Scope * sem : MemorySemantics
    | OpAtomicFlagClear of ptr : uint32 * scope : Scope * sem : MemorySemantics
[<AutoOpen>]
module InstructionExtensions =
    type Instruction with
        member x.ResultType =
            match x with
                | OpUndef(tid,_) -> Some tid
                | OpExtInst(tid,_,_,_,_) -> Some tid
                | OpConstantTrue(tid,_) -> Some tid
                | OpConstantFalse(tid,_) -> Some tid
                | OpConstant(tid,_,_) -> Some tid
                | OpConstantComposite(tid,_,_) -> Some tid
                | OpConstantSampler(tid,_,_,_,_) -> Some tid
                | OpConstantNull(tid,_) -> Some tid
                | OpSpecConstantTrue(tid,_) -> Some tid
                | OpSpecConstantFalse(tid,_) -> Some tid
                | OpSpecConstant(tid,_,_) -> Some tid
                | OpSpecConstantComposite(tid,_,_) -> Some tid
                | OpSpecConstantOp(tid,_,_,_) -> Some tid
                | OpFunction(tid,_,_,_) -> Some tid
                | OpFunctionParameter(tid,_) -> Some tid
                | OpFunctionCall(tid,_,_,_) -> Some tid
                | OpVariable(tid,_,_,_) -> Some tid
                | OpImageTexelPointer(tid,_,_,_,_) -> Some tid
                | OpLoad(tid,_,_,_) -> Some tid
                | OpAccessChain(tid,_,_,_) -> Some tid
                | OpInBoundsAccessChain(tid,_,_,_) -> Some tid
                | OpPtrAccessChain(tid,_,_,_,_) -> Some tid
                | OpArrayLength(tid,_,_,_) -> Some tid
                | OpGenericPtrMemSemantics(tid,_,_) -> Some tid
                | OpInBoundsPtrAccessChain(tid,_,_,_,_) -> Some tid
                | OpVectorExtractDynamic(tid,_,_,_) -> Some tid
                | OpVectorInsertDynamic(tid,_,_,_,_) -> Some tid
                | OpVectorShuffle(tid,_,_,_,_) -> Some tid
                | OpCompositeConstruct(tid,_,_) -> Some tid
                | OpCompositeExtract(tid,_,_,_) -> Some tid
                | OpCompositeInsert(tid,_,_,_,_) -> Some tid
                | OpCopyObject(tid,_,_) -> Some tid
                | OpTranspose(tid,_,_) -> Some tid
                | OpSampledImage(tid,_,_,_) -> Some tid
                | OpImageSampleImplicitLod(tid,_,_,_,_,_) -> Some tid
                | OpImageSampleExplicitLod(tid,_,_,_,_,_) -> Some tid
                | OpImageSampleDrefImplicitLod(tid,_,_,_,_,_,_) -> Some tid
                | OpImageSampleDrefExplicitLod(tid,_,_,_,_,_,_) -> Some tid
                | OpImageSampleProjImplicitLod(tid,_,_,_,_,_) -> Some tid
                | OpImageSampleProjExplicitLod(tid,_,_,_,_,_) -> Some tid
                | OpImageSampleProjDrefImplicitLod(tid,_,_,_,_,_,_) -> Some tid
                | OpImageSampleProjDrefExplicitLod(tid,_,_,_,_,_,_) -> Some tid
                | OpImageFetch(tid,_,_,_,_,_) -> Some tid
                | OpImageGather(tid,_,_,_,_,_,_) -> Some tid
                | OpImageDrefGather(tid,_,_,_,_,_,_) -> Some tid
                | OpImageRead(tid,_,_,_,_,_) -> Some tid
                | OpImage(tid,_,_) -> Some tid
                | OpImageQueryFormat(tid,_,_) -> Some tid
                | OpImageQueryOrder(tid,_,_) -> Some tid
                | OpImageQuerySizeLod(tid,_,_,_) -> Some tid
                | OpImageQuerySize(tid,_,_) -> Some tid
                | OpImageQueryLod(tid,_,_,_) -> Some tid
                | OpImageQueryLevels(tid,_,_) -> Some tid
                | OpImageQuerySamples(tid,_,_) -> Some tid
                | OpConvertFToU(tid,_,_) -> Some tid
                | OpConvertFToS(tid,_,_) -> Some tid
                | OpConvertSToF(tid,_,_) -> Some tid
                | OpConvertUToF(tid,_,_) -> Some tid
                | OpUConvert(tid,_,_) -> Some tid
                | OpSConvert(tid,_,_) -> Some tid
                | OpFConvert(tid,_,_) -> Some tid
                | OpQuantizeToF16(tid,_,_) -> Some tid
                | OpConvertPtrToU(tid,_,_) -> Some tid
                | OpSatConvertSToU(tid,_,_) -> Some tid
                | OpSatConvertUToS(tid,_,_) -> Some tid
                | OpConvertUToPtr(tid,_,_) -> Some tid
                | OpPtrCastToGeneric(tid,_,_) -> Some tid
                | OpGenericCastToPtr(tid,_,_) -> Some tid
                | OpGenericCastToPtrExplicit(tid,_,_,_) -> Some tid
                | OpBitcast(tid,_,_) -> Some tid
                | OpSNegate(tid,_,_) -> Some tid
                | OpFNegate(tid,_,_) -> Some tid
                | OpIAdd(tid,_,_,_) -> Some tid
                | OpFAdd(tid,_,_,_) -> Some tid
                | OpISub(tid,_,_,_) -> Some tid
                | OpFSub(tid,_,_,_) -> Some tid
                | OpIMul(tid,_,_,_) -> Some tid
                | OpFMul(tid,_,_,_) -> Some tid
                | OpUDiv(tid,_,_,_) -> Some tid
                | OpSDiv(tid,_,_,_) -> Some tid
                | OpFDiv(tid,_,_,_) -> Some tid
                | OpUMod(tid,_,_,_) -> Some tid
                | OpSRem(tid,_,_,_) -> Some tid
                | OpSMod(tid,_,_,_) -> Some tid
                | OpFRem(tid,_,_,_) -> Some tid
                | OpFMod(tid,_,_,_) -> Some tid
                | OpVectorTimesScalar(tid,_,_,_) -> Some tid
                | OpMatrixTimesScalar(tid,_,_,_) -> Some tid
                | OpVectorTimesMatrix(tid,_,_,_) -> Some tid
                | OpMatrixTimesVector(tid,_,_,_) -> Some tid
                | OpMatrixTimesMatrix(tid,_,_,_) -> Some tid
                | OpOuterProduct(tid,_,_,_) -> Some tid
                | OpDot(tid,_,_,_) -> Some tid
                | OpIAddCarry(tid,_,_,_) -> Some tid
                | OpISubBorrow(tid,_,_,_) -> Some tid
                | OpUMulExtended(tid,_,_,_) -> Some tid
                | OpSMulExtended(tid,_,_,_) -> Some tid
                | OpAny(tid,_,_) -> Some tid
                | OpAll(tid,_,_) -> Some tid
                | OpIsNan(tid,_,_) -> Some tid
                | OpIsInf(tid,_,_) -> Some tid
                | OpIsFinite(tid,_,_) -> Some tid
                | OpIsNormal(tid,_,_) -> Some tid
                | OpSignBitSet(tid,_,_) -> Some tid
                | OpLessOrGreater(tid,_,_,_) -> Some tid
                | OpOrdered(tid,_,_,_) -> Some tid
                | OpUnordered(tid,_,_,_) -> Some tid
                | OpLogicalEqual(tid,_,_,_) -> Some tid
                | OpLogicalNotEqual(tid,_,_,_) -> Some tid
                | OpLogicalOr(tid,_,_,_) -> Some tid
                | OpLogicalAnd(tid,_,_,_) -> Some tid
                | OpLogicalNot(tid,_,_) -> Some tid
                | OpSelect(tid,_,_,_,_) -> Some tid
                | OpIEqual(tid,_,_,_) -> Some tid
                | OpINotEqual(tid,_,_,_) -> Some tid
                | OpUGreaterThan(tid,_,_,_) -> Some tid
                | OpSGreaterThan(tid,_,_,_) -> Some tid
                | OpUGreaterThanEqual(tid,_,_,_) -> Some tid
                | OpSGreaterThanEqual(tid,_,_,_) -> Some tid
                | OpULessThan(tid,_,_,_) -> Some tid
                | OpSLessThan(tid,_,_,_) -> Some tid
                | OpULessThanEqual(tid,_,_,_) -> Some tid
                | OpSLessThanEqual(tid,_,_,_) -> Some tid
                | OpFOrdEqual(tid,_,_,_) -> Some tid
                | OpFUnordEqual(tid,_,_,_) -> Some tid
                | OpFOrdNotEqual(tid,_,_,_) -> Some tid
                | OpFUnordNotEqual(tid,_,_,_) -> Some tid
                | OpFOrdLessThan(tid,_,_,_) -> Some tid
                | OpFUnordLessThan(tid,_,_,_) -> Some tid
                | OpFOrdGreaterThan(tid,_,_,_) -> Some tid
                | OpFUnordGreaterThan(tid,_,_,_) -> Some tid
                | OpFOrdLessThanEqual(tid,_,_,_) -> Some tid
                | OpFUnordLessThanEqual(tid,_,_,_) -> Some tid
                | OpFOrdGreaterThanEqual(tid,_,_,_) -> Some tid
                | OpFUnordGreaterThanEqual(tid,_,_,_) -> Some tid
                | OpShiftRightLogical(tid,_,_,_) -> Some tid
                | OpShiftRightArithmetic(tid,_,_,_) -> Some tid
                | OpShiftLeftLogical(tid,_,_,_) -> Some tid
                | OpBitwiseOr(tid,_,_,_) -> Some tid
                | OpBitwiseXor(tid,_,_,_) -> Some tid
                | OpBitwiseAnd(tid,_,_,_) -> Some tid
                | OpNot(tid,_,_) -> Some tid
                | OpBitFieldInsert(tid,_,_,_,_,_) -> Some tid
                | OpBitFieldSExtract(tid,_,_,_,_) -> Some tid
                | OpBitFieldUExtract(tid,_,_,_,_) -> Some tid
                | OpBitReverse(tid,_,_) -> Some tid
                | OpBitCount(tid,_,_) -> Some tid
                | OpDPdx(tid,_,_) -> Some tid
                | OpDPdy(tid,_,_) -> Some tid
                | OpFwidth(tid,_,_) -> Some tid
                | OpDPdxFine(tid,_,_) -> Some tid
                | OpDPdyFine(tid,_,_) -> Some tid
                | OpFwidthFine(tid,_,_) -> Some tid
                | OpDPdxCoarse(tid,_,_) -> Some tid
                | OpDPdyCoarse(tid,_,_) -> Some tid
                | OpFwidthCoarse(tid,_,_) -> Some tid
                | OpAtomicLoad(tid,_,_,_,_) -> Some tid
                | OpAtomicExchange(tid,_,_,_,_,_) -> Some tid
                | OpAtomicCompareExchange(tid,_,_,_,_,_,_,_) -> Some tid
                | OpAtomicCompareExchangeWeak(tid,_,_,_,_,_,_,_) -> Some tid
                | OpAtomicIIncrement(tid,_,_,_,_) -> Some tid
                | OpAtomicIDecrement(tid,_,_,_,_) -> Some tid
                | OpAtomicIAdd(tid,_,_,_,_,_) -> Some tid
                | OpAtomicISub(tid,_,_,_,_,_) -> Some tid
                | OpAtomicSMin(tid,_,_,_,_,_) -> Some tid
                | OpAtomicUMin(tid,_,_,_,_,_) -> Some tid
                | OpAtomicSMax(tid,_,_,_,_,_) -> Some tid
                | OpAtomicUMax(tid,_,_,_,_,_) -> Some tid
                | OpAtomicAnd(tid,_,_,_,_,_) -> Some tid
                | OpAtomicOr(tid,_,_,_,_,_) -> Some tid
                | OpAtomicXor(tid,_,_,_,_,_) -> Some tid
                | OpPhi(tid,_,_) -> Some tid
                | OpGroupAsyncCopy(tid,_,_,_,_,_,_,_) -> Some tid
                | OpGroupAll(tid,_,_,_) -> Some tid
                | OpGroupAny(tid,_,_,_) -> Some tid
                | OpGroupBroadcast(tid,_,_,_,_) -> Some tid
                | OpGroupIAdd(tid,_,_,_,_) -> Some tid
                | OpGroupFAdd(tid,_,_,_,_) -> Some tid
                | OpGroupFMin(tid,_,_,_,_) -> Some tid
                | OpGroupUMin(tid,_,_,_,_) -> Some tid
                | OpGroupSMin(tid,_,_,_,_) -> Some tid
                | OpGroupFMax(tid,_,_,_,_) -> Some tid
                | OpGroupUMax(tid,_,_,_,_) -> Some tid
                | OpGroupSMax(tid,_,_,_,_) -> Some tid
                | OpReadPipe(tid,_,_,_,_,_) -> Some tid
                | OpWritePipe(tid,_,_,_,_,_) -> Some tid
                | OpReservedReadPipe(tid,_,_,_,_,_,_,_) -> Some tid
                | OpReservedWritePipe(tid,_,_,_,_,_,_,_) -> Some tid
                | OpReserveReadPipePackets(tid,_,_,_,_,_) -> Some tid
                | OpReserveWritePipePackets(tid,_,_,_,_,_) -> Some tid
                | OpIsValidReserveId(tid,_,_) -> Some tid
                | OpGetNumPipePackets(tid,_,_,_,_) -> Some tid
                | OpGetMaxPipePackets(tid,_,_,_,_) -> Some tid
                | OpGroupReserveReadPipePackets(tid,_,_,_,_,_,_) -> Some tid
                | OpGroupReserveWritePipePackets(tid,_,_,_,_,_,_) -> Some tid
                | OpEnqueueMarker(tid,_,_,_,_,_) -> Some tid
                | OpEnqueueKernel(tid,_,_,_,_,_,_,_,_,_,_,_,_) -> Some tid
                | OpGetKernelNDrangeSubGroupCount(tid,_,_,_,_,_,_) -> Some tid
                | OpGetKernelNDrangeMaxSubGroupSize(tid,_,_,_,_,_,_) -> Some tid
                | OpGetKernelWorkGroupSize(tid,_,_,_,_,_) -> Some tid
                | OpGetKernelPreferredWorkGroupSizeMultiple(tid,_,_,_,_,_) -> Some tid
                | OpCreateUserEvent(tid,_) -> Some tid
                | OpIsValidEvent(tid,_,_) -> Some tid
                | OpGetDefaultQueue(tid,_) -> Some tid
                | OpBuildNDRange(tid,_,_,_,_) -> Some tid
                | OpImageSparseSampleImplicitLod(tid,_,_,_,_,_) -> Some tid
                | OpImageSparseSampleExplicitLod(tid,_,_,_,_,_) -> Some tid
                | OpImageSparseSampleDrefImplicitLod(tid,_,_,_,_,_,_) -> Some tid
                | OpImageSparseSampleDrefExplicitLod(tid,_,_,_,_,_,_) -> Some tid
                | OpImageSparseSampleProjImplicitLod(tid,_,_,_,_,_) -> Some tid
                | OpImageSparseSampleProjExplicitLod(tid,_,_,_,_,_) -> Some tid
                | OpImageSparseSampleProjDrefImplicitLod(tid,_,_,_,_,_,_) -> Some tid
                | OpImageSparseSampleProjDrefExplicitLod(tid,_,_,_,_,_,_) -> Some tid
                | OpImageSparseFetch(tid,_,_,_,_,_) -> Some tid
                | OpImageSparseGather(tid,_,_,_,_,_,_) -> Some tid
                | OpImageSparseDrefGather(tid,_,_,_,_,_,_) -> Some tid
                | OpImageSparseTexelsResident(tid,_,_) -> Some tid
                | OpAtomicFlagTestAndSet(tid,_,_,_,_) -> Some tid
                | _ -> None


        member x.SubstituteIds(f : uint32 -> uint32) =
            match x with
                | OpNop -> OpNop
                | OpUndef(a0, a1) -> OpUndef(f a0, f a1)
                | OpSourceContinued(a0) -> OpSourceContinued(a0)
                | OpSource(a0, a1, a2) -> OpSource(a0, a1, a2)
                | OpSourceExtension(a0) -> OpSourceExtension(a0)
                | OpName(a0, a1) -> OpName(f a0, a1)
                | OpMemberName(a0, a1, a2) -> OpMemberName(f a0, a1, a2)
                | OpString(a0, a1) -> OpString(f a0, a1)
                | OpLine(a0, a1, a2) -> OpLine(f a0, a1, a2)
                | OpExtension(a0) -> OpExtension(a0)
                | OpExtInstImport(a0, a1) -> OpExtInstImport(f a0, a1)
                | OpExtInst(a0, a1, a2, a3, a4) -> OpExtInst(f a0, f a1, f a2, a3, Array.map f a4)
                | OpMemoryModel(a0, a1) -> OpMemoryModel(a0, a1)
                | OpEntryPoint(a0, a1, a2) -> OpEntryPoint(a0, f a1, a2)
                | OpExecutionMode(a0, a1, a2) -> OpExecutionMode(f a0, a1, a2)
                | OpCapability(a0) -> OpCapability(a0)
                | OpTypeVoid(a0) -> OpTypeVoid(f a0)
                | OpTypeBool(a0) -> OpTypeBool(f a0)
                | OpTypeInt(a0, a1, a2) -> OpTypeInt(f a0, a1, a2)
                | OpTypeFloat(a0, a1) -> OpTypeFloat(f a0, a1)
                | OpTypeVector(a0, a1, a2) -> OpTypeVector(f a0, f a1, a2)
                | OpTypeMatrix(a0, a1, a2) -> OpTypeMatrix(f a0, f a1, a2)
                | OpTypeImage(a0, a1, a2, a3, a4, a5, a6, a7, a8) -> OpTypeImage(f a0, f a1, a2, a3, a4, a5, a6, a7, a8)
                | OpTypeSampler(a0) -> OpTypeSampler(f a0)
                | OpTypeSampledImage(a0, a1) -> OpTypeSampledImage(f a0, f a1)
                | OpTypeArray(a0, a1, a2) -> OpTypeArray(f a0, f a1, f a2)
                | OpTypeRuntimeArray(a0, a1) -> OpTypeRuntimeArray(f a0, f a1)
                | OpTypeStruct(a0, a1) -> OpTypeStruct(f a0, Array.map f a1)
                | OpTypeOpaque(a0, a1) -> OpTypeOpaque(f a0, a1)
                | OpTypePointer(a0, a1, a2) -> OpTypePointer(f a0, a1, f a2)
                | OpTypeFunction(a0, a1, a2) -> OpTypeFunction(f a0, f a1, Array.map f a2)
                | OpTypeEvent(a0) -> OpTypeEvent(f a0)
                | OpTypeDeviceEvent(a0) -> OpTypeDeviceEvent(f a0)
                | OpTypeReserveId(a0) -> OpTypeReserveId(f a0)
                | OpTypeQueue(a0) -> OpTypeQueue(f a0)
                | OpTypePipe(a0, a1) -> OpTypePipe(f a0, a1)
                | OpTypeForwardPointer(a0, a1) -> OpTypeForwardPointer(f a0, a1)
                | OpConstantTrue(a0, a1) -> OpConstantTrue(f a0, f a1)
                | OpConstantFalse(a0, a1) -> OpConstantFalse(f a0, f a1)
                | OpConstant(a0, a1, a2) -> OpConstant(f a0, f a1, a2)
                | OpConstantComposite(a0, a1, a2) -> OpConstantComposite(f a0, f a1, Array.map f a2)
                | OpConstantSampler(a0, a1, a2, a3, a4) -> OpConstantSampler(f a0, f a1, a2, a3, a4)
                | OpConstantNull(a0, a1) -> OpConstantNull(f a0, f a1)
                | OpSpecConstantTrue(a0, a1) -> OpSpecConstantTrue(f a0, f a1)
                | OpSpecConstantFalse(a0, a1) -> OpSpecConstantFalse(f a0, f a1)
                | OpSpecConstant(a0, a1, a2) -> OpSpecConstant(f a0, f a1, a2)
                | OpSpecConstantComposite(a0, a1, a2) -> OpSpecConstantComposite(f a0, f a1, Array.map f a2)
                | OpSpecConstantOp(a0, a1, a2, a3) -> OpSpecConstantOp(f a0, f a1, a2, Array.map f a3)
                | OpFunction(a0, a1, a2, a3) -> OpFunction(f a0, f a1, a2, f a3)
                | OpFunctionParameter(a0, a1) -> OpFunctionParameter(f a0, f a1)
                | OpFunctionEnd -> OpFunctionEnd
                | OpFunctionCall(a0, a1, a2, a3) -> OpFunctionCall(f a0, f a1, f a2, Array.map f a3)
                | OpVariable(a0, a1, a2, a3) -> OpVariable(f a0, f a1, a2, Option.map f a3)
                | OpImageTexelPointer(a0, a1, a2, a3, a4) -> OpImageTexelPointer(f a0, f a1, f a2, f a3, f a4)
                | OpLoad(a0, a1, a2, a3) -> OpLoad(f a0, f a1, f a2, a3)
                | OpStore(a0, a1, a2) -> OpStore(f a0, f a1, a2)
                | OpCopyMemory(a0, a1, a2) -> OpCopyMemory(f a0, f a1, a2)
                | OpCopyMemorySized(a0, a1, a2, a3) -> OpCopyMemorySized(f a0, f a1, f a2, a3)
                | OpAccessChain(a0, a1, a2, a3) -> OpAccessChain(f a0, f a1, f a2, Array.map f a3)
                | OpInBoundsAccessChain(a0, a1, a2, a3) -> OpInBoundsAccessChain(f a0, f a1, f a2, Array.map f a3)
                | OpPtrAccessChain(a0, a1, a2, a3, a4) -> OpPtrAccessChain(f a0, f a1, f a2, f a3, Array.map f a4)
                | OpArrayLength(a0, a1, a2, a3) -> OpArrayLength(f a0, f a1, f a2, a3)
                | OpGenericPtrMemSemantics(a0, a1, a2) -> OpGenericPtrMemSemantics(f a0, f a1, f a2)
                | OpInBoundsPtrAccessChain(a0, a1, a2, a3, a4) -> OpInBoundsPtrAccessChain(f a0, f a1, f a2, f a3, Array.map f a4)
                | OpDecorate(a0, a1, a2) -> OpDecorate(f a0, a1, a2)
                | OpMemberDecorate(a0, a1, a2, a3) -> OpMemberDecorate(f a0, a1, a2, a3)
                | OpDecorationGroup(a0) -> OpDecorationGroup(f a0)
                | OpGroupDecorate(a0, a1) -> OpGroupDecorate(f a0, Array.map f a1)
                | OpGroupMemberDecorate(a0, a1) -> OpGroupMemberDecorate(f a0, a1)
                | OpVectorExtractDynamic(a0, a1, a2, a3) -> OpVectorExtractDynamic(f a0, f a1, f a2, f a3)
                | OpVectorInsertDynamic(a0, a1, a2, a3, a4) -> OpVectorInsertDynamic(f a0, f a1, f a2, f a3, f a4)
                | OpVectorShuffle(a0, a1, a2, a3, a4) -> OpVectorShuffle(f a0, f a1, f a2, f a3, a4)
                | OpCompositeConstruct(a0, a1, a2) -> OpCompositeConstruct(f a0, f a1, Array.map f a2)
                | OpCompositeExtract(a0, a1, a2, a3) -> OpCompositeExtract(f a0, f a1, f a2, a3)
                | OpCompositeInsert(a0, a1, a2, a3, a4) -> OpCompositeInsert(f a0, f a1, f a2, f a3, a4)
                | OpCopyObject(a0, a1, a2) -> OpCopyObject(f a0, f a1, f a2)
                | OpTranspose(a0, a1, a2) -> OpTranspose(f a0, f a1, f a2)
                | OpSampledImage(a0, a1, a2, a3) -> OpSampledImage(f a0, f a1, f a2, f a3)
                | OpImageSampleImplicitLod(a0, a1, a2, a3, a4, a5) -> OpImageSampleImplicitLod(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageSampleExplicitLod(a0, a1, a2, a3, a4, a5) -> OpImageSampleExplicitLod(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageSampleDrefImplicitLod(a0, a1, a2, a3, a4, a5, a6) -> OpImageSampleDrefImplicitLod(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageSampleDrefExplicitLod(a0, a1, a2, a3, a4, a5, a6) -> OpImageSampleDrefExplicitLod(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageSampleProjImplicitLod(a0, a1, a2, a3, a4, a5) -> OpImageSampleProjImplicitLod(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageSampleProjExplicitLod(a0, a1, a2, a3, a4, a5) -> OpImageSampleProjExplicitLod(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageSampleProjDrefImplicitLod(a0, a1, a2, a3, a4, a5, a6) -> OpImageSampleProjDrefImplicitLod(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageSampleProjDrefExplicitLod(a0, a1, a2, a3, a4, a5, a6) -> OpImageSampleProjDrefExplicitLod(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageFetch(a0, a1, a2, a3, a4, a5) -> OpImageFetch(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageGather(a0, a1, a2, a3, a4, a5, a6) -> OpImageGather(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageDrefGather(a0, a1, a2, a3, a4, a5, a6) -> OpImageDrefGather(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageRead(a0, a1, a2, a3, a4, a5) -> OpImageRead(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageWrite(a0, a1, a2, a3, a4) -> OpImageWrite(f a0, f a1, f a2, a3, Array.map f a4)
                | OpImage(a0, a1, a2) -> OpImage(f a0, f a1, f a2)
                | OpImageQueryFormat(a0, a1, a2) -> OpImageQueryFormat(f a0, f a1, f a2)
                | OpImageQueryOrder(a0, a1, a2) -> OpImageQueryOrder(f a0, f a1, f a2)
                | OpImageQuerySizeLod(a0, a1, a2, a3) -> OpImageQuerySizeLod(f a0, f a1, f a2, f a3)
                | OpImageQuerySize(a0, a1, a2) -> OpImageQuerySize(f a0, f a1, f a2)
                | OpImageQueryLod(a0, a1, a2, a3) -> OpImageQueryLod(f a0, f a1, f a2, f a3)
                | OpImageQueryLevels(a0, a1, a2) -> OpImageQueryLevels(f a0, f a1, f a2)
                | OpImageQuerySamples(a0, a1, a2) -> OpImageQuerySamples(f a0, f a1, f a2)
                | OpConvertFToU(a0, a1, a2) -> OpConvertFToU(f a0, f a1, f a2)
                | OpConvertFToS(a0, a1, a2) -> OpConvertFToS(f a0, f a1, f a2)
                | OpConvertSToF(a0, a1, a2) -> OpConvertSToF(f a0, f a1, f a2)
                | OpConvertUToF(a0, a1, a2) -> OpConvertUToF(f a0, f a1, f a2)
                | OpUConvert(a0, a1, a2) -> OpUConvert(f a0, f a1, f a2)
                | OpSConvert(a0, a1, a2) -> OpSConvert(f a0, f a1, f a2)
                | OpFConvert(a0, a1, a2) -> OpFConvert(f a0, f a1, f a2)
                | OpQuantizeToF16(a0, a1, a2) -> OpQuantizeToF16(f a0, f a1, f a2)
                | OpConvertPtrToU(a0, a1, a2) -> OpConvertPtrToU(f a0, f a1, f a2)
                | OpSatConvertSToU(a0, a1, a2) -> OpSatConvertSToU(f a0, f a1, f a2)
                | OpSatConvertUToS(a0, a1, a2) -> OpSatConvertUToS(f a0, f a1, f a2)
                | OpConvertUToPtr(a0, a1, a2) -> OpConvertUToPtr(f a0, f a1, f a2)
                | OpPtrCastToGeneric(a0, a1, a2) -> OpPtrCastToGeneric(f a0, f a1, f a2)
                | OpGenericCastToPtr(a0, a1, a2) -> OpGenericCastToPtr(f a0, f a1, f a2)
                | OpGenericCastToPtrExplicit(a0, a1, a2, a3) -> OpGenericCastToPtrExplicit(f a0, f a1, f a2, a3)
                | OpBitcast(a0, a1, a2) -> OpBitcast(f a0, f a1, f a2)
                | OpSNegate(a0, a1, a2) -> OpSNegate(f a0, f a1, f a2)
                | OpFNegate(a0, a1, a2) -> OpFNegate(f a0, f a1, f a2)
                | OpIAdd(a0, a1, a2, a3) -> OpIAdd(f a0, f a1, f a2, f a3)
                | OpFAdd(a0, a1, a2, a3) -> OpFAdd(f a0, f a1, f a2, f a3)
                | OpISub(a0, a1, a2, a3) -> OpISub(f a0, f a1, f a2, f a3)
                | OpFSub(a0, a1, a2, a3) -> OpFSub(f a0, f a1, f a2, f a3)
                | OpIMul(a0, a1, a2, a3) -> OpIMul(f a0, f a1, f a2, f a3)
                | OpFMul(a0, a1, a2, a3) -> OpFMul(f a0, f a1, f a2, f a3)
                | OpUDiv(a0, a1, a2, a3) -> OpUDiv(f a0, f a1, f a2, f a3)
                | OpSDiv(a0, a1, a2, a3) -> OpSDiv(f a0, f a1, f a2, f a3)
                | OpFDiv(a0, a1, a2, a3) -> OpFDiv(f a0, f a1, f a2, f a3)
                | OpUMod(a0, a1, a2, a3) -> OpUMod(f a0, f a1, f a2, f a3)
                | OpSRem(a0, a1, a2, a3) -> OpSRem(f a0, f a1, f a2, f a3)
                | OpSMod(a0, a1, a2, a3) -> OpSMod(f a0, f a1, f a2, f a3)
                | OpFRem(a0, a1, a2, a3) -> OpFRem(f a0, f a1, f a2, f a3)
                | OpFMod(a0, a1, a2, a3) -> OpFMod(f a0, f a1, f a2, f a3)
                | OpVectorTimesScalar(a0, a1, a2, a3) -> OpVectorTimesScalar(f a0, f a1, f a2, f a3)
                | OpMatrixTimesScalar(a0, a1, a2, a3) -> OpMatrixTimesScalar(f a0, f a1, f a2, f a3)
                | OpVectorTimesMatrix(a0, a1, a2, a3) -> OpVectorTimesMatrix(f a0, f a1, f a2, f a3)
                | OpMatrixTimesVector(a0, a1, a2, a3) -> OpMatrixTimesVector(f a0, f a1, f a2, f a3)
                | OpMatrixTimesMatrix(a0, a1, a2, a3) -> OpMatrixTimesMatrix(f a0, f a1, f a2, f a3)
                | OpOuterProduct(a0, a1, a2, a3) -> OpOuterProduct(f a0, f a1, f a2, f a3)
                | OpDot(a0, a1, a2, a3) -> OpDot(f a0, f a1, f a2, f a3)
                | OpIAddCarry(a0, a1, a2, a3) -> OpIAddCarry(f a0, f a1, f a2, f a3)
                | OpISubBorrow(a0, a1, a2, a3) -> OpISubBorrow(f a0, f a1, f a2, f a3)
                | OpUMulExtended(a0, a1, a2, a3) -> OpUMulExtended(f a0, f a1, f a2, f a3)
                | OpSMulExtended(a0, a1, a2, a3) -> OpSMulExtended(f a0, f a1, f a2, f a3)
                | OpAny(a0, a1, a2) -> OpAny(f a0, f a1, f a2)
                | OpAll(a0, a1, a2) -> OpAll(f a0, f a1, f a2)
                | OpIsNan(a0, a1, a2) -> OpIsNan(f a0, f a1, f a2)
                | OpIsInf(a0, a1, a2) -> OpIsInf(f a0, f a1, f a2)
                | OpIsFinite(a0, a1, a2) -> OpIsFinite(f a0, f a1, f a2)
                | OpIsNormal(a0, a1, a2) -> OpIsNormal(f a0, f a1, f a2)
                | OpSignBitSet(a0, a1, a2) -> OpSignBitSet(f a0, f a1, f a2)
                | OpLessOrGreater(a0, a1, a2, a3) -> OpLessOrGreater(f a0, f a1, f a2, f a3)
                | OpOrdered(a0, a1, a2, a3) -> OpOrdered(f a0, f a1, f a2, f a3)
                | OpUnordered(a0, a1, a2, a3) -> OpUnordered(f a0, f a1, f a2, f a3)
                | OpLogicalEqual(a0, a1, a2, a3) -> OpLogicalEqual(f a0, f a1, f a2, f a3)
                | OpLogicalNotEqual(a0, a1, a2, a3) -> OpLogicalNotEqual(f a0, f a1, f a2, f a3)
                | OpLogicalOr(a0, a1, a2, a3) -> OpLogicalOr(f a0, f a1, f a2, f a3)
                | OpLogicalAnd(a0, a1, a2, a3) -> OpLogicalAnd(f a0, f a1, f a2, f a3)
                | OpLogicalNot(a0, a1, a2) -> OpLogicalNot(f a0, f a1, f a2)
                | OpSelect(a0, a1, a2, a3, a4) -> OpSelect(f a0, f a1, f a2, f a3, f a4)
                | OpIEqual(a0, a1, a2, a3) -> OpIEqual(f a0, f a1, f a2, f a3)
                | OpINotEqual(a0, a1, a2, a3) -> OpINotEqual(f a0, f a1, f a2, f a3)
                | OpUGreaterThan(a0, a1, a2, a3) -> OpUGreaterThan(f a0, f a1, f a2, f a3)
                | OpSGreaterThan(a0, a1, a2, a3) -> OpSGreaterThan(f a0, f a1, f a2, f a3)
                | OpUGreaterThanEqual(a0, a1, a2, a3) -> OpUGreaterThanEqual(f a0, f a1, f a2, f a3)
                | OpSGreaterThanEqual(a0, a1, a2, a3) -> OpSGreaterThanEqual(f a0, f a1, f a2, f a3)
                | OpULessThan(a0, a1, a2, a3) -> OpULessThan(f a0, f a1, f a2, f a3)
                | OpSLessThan(a0, a1, a2, a3) -> OpSLessThan(f a0, f a1, f a2, f a3)
                | OpULessThanEqual(a0, a1, a2, a3) -> OpULessThanEqual(f a0, f a1, f a2, f a3)
                | OpSLessThanEqual(a0, a1, a2, a3) -> OpSLessThanEqual(f a0, f a1, f a2, f a3)
                | OpFOrdEqual(a0, a1, a2, a3) -> OpFOrdEqual(f a0, f a1, f a2, f a3)
                | OpFUnordEqual(a0, a1, a2, a3) -> OpFUnordEqual(f a0, f a1, f a2, f a3)
                | OpFOrdNotEqual(a0, a1, a2, a3) -> OpFOrdNotEqual(f a0, f a1, f a2, f a3)
                | OpFUnordNotEqual(a0, a1, a2, a3) -> OpFUnordNotEqual(f a0, f a1, f a2, f a3)
                | OpFOrdLessThan(a0, a1, a2, a3) -> OpFOrdLessThan(f a0, f a1, f a2, f a3)
                | OpFUnordLessThan(a0, a1, a2, a3) -> OpFUnordLessThan(f a0, f a1, f a2, f a3)
                | OpFOrdGreaterThan(a0, a1, a2, a3) -> OpFOrdGreaterThan(f a0, f a1, f a2, f a3)
                | OpFUnordGreaterThan(a0, a1, a2, a3) -> OpFUnordGreaterThan(f a0, f a1, f a2, f a3)
                | OpFOrdLessThanEqual(a0, a1, a2, a3) -> OpFOrdLessThanEqual(f a0, f a1, f a2, f a3)
                | OpFUnordLessThanEqual(a0, a1, a2, a3) -> OpFUnordLessThanEqual(f a0, f a1, f a2, f a3)
                | OpFOrdGreaterThanEqual(a0, a1, a2, a3) -> OpFOrdGreaterThanEqual(f a0, f a1, f a2, f a3)
                | OpFUnordGreaterThanEqual(a0, a1, a2, a3) -> OpFUnordGreaterThanEqual(f a0, f a1, f a2, f a3)
                | OpShiftRightLogical(a0, a1, a2, a3) -> OpShiftRightLogical(f a0, f a1, f a2, f a3)
                | OpShiftRightArithmetic(a0, a1, a2, a3) -> OpShiftRightArithmetic(f a0, f a1, f a2, f a3)
                | OpShiftLeftLogical(a0, a1, a2, a3) -> OpShiftLeftLogical(f a0, f a1, f a2, f a3)
                | OpBitwiseOr(a0, a1, a2, a3) -> OpBitwiseOr(f a0, f a1, f a2, f a3)
                | OpBitwiseXor(a0, a1, a2, a3) -> OpBitwiseXor(f a0, f a1, f a2, f a3)
                | OpBitwiseAnd(a0, a1, a2, a3) -> OpBitwiseAnd(f a0, f a1, f a2, f a3)
                | OpNot(a0, a1, a2) -> OpNot(f a0, f a1, f a2)
                | OpBitFieldInsert(a0, a1, a2, a3, a4, a5) -> OpBitFieldInsert(f a0, f a1, f a2, f a3, f a4, f a5)
                | OpBitFieldSExtract(a0, a1, a2, a3, a4) -> OpBitFieldSExtract(f a0, f a1, f a2, f a3, f a4)
                | OpBitFieldUExtract(a0, a1, a2, a3, a4) -> OpBitFieldUExtract(f a0, f a1, f a2, f a3, f a4)
                | OpBitReverse(a0, a1, a2) -> OpBitReverse(f a0, f a1, f a2)
                | OpBitCount(a0, a1, a2) -> OpBitCount(f a0, f a1, f a2)
                | OpDPdx(a0, a1, a2) -> OpDPdx(f a0, f a1, f a2)
                | OpDPdy(a0, a1, a2) -> OpDPdy(f a0, f a1, f a2)
                | OpFwidth(a0, a1, a2) -> OpFwidth(f a0, f a1, f a2)
                | OpDPdxFine(a0, a1, a2) -> OpDPdxFine(f a0, f a1, f a2)
                | OpDPdyFine(a0, a1, a2) -> OpDPdyFine(f a0, f a1, f a2)
                | OpFwidthFine(a0, a1, a2) -> OpFwidthFine(f a0, f a1, f a2)
                | OpDPdxCoarse(a0, a1, a2) -> OpDPdxCoarse(f a0, f a1, f a2)
                | OpDPdyCoarse(a0, a1, a2) -> OpDPdyCoarse(f a0, f a1, f a2)
                | OpFwidthCoarse(a0, a1, a2) -> OpFwidthCoarse(f a0, f a1, f a2)
                | OpEmitVertex -> OpEmitVertex
                | OpEndPrimitive -> OpEndPrimitive
                | OpEmitStreamVertex(a0) -> OpEmitStreamVertex(f a0)
                | OpEndStreamPrimitive(a0) -> OpEndStreamPrimitive(f a0)
                | OpControlBarrier(a0, a1, a2) -> OpControlBarrier(a0, a1, a2)
                | OpMemoryBarrier(a0, a1) -> OpMemoryBarrier(a0, a1)
                | OpAtomicLoad(a0, a1, a2, a3, a4) -> OpAtomicLoad(f a0, f a1, f a2, a3, a4)
                | OpAtomicStore(a0, a1, a2, a3) -> OpAtomicStore(f a0, a1, a2, f a3)
                | OpAtomicExchange(a0, a1, a2, a3, a4, a5) -> OpAtomicExchange(f a0, f a1, f a2, a3, a4, f a5)
                | OpAtomicCompareExchange(a0, a1, a2, a3, a4, a5, a6, a7) -> OpAtomicCompareExchange(f a0, f a1, f a2, a3, a4, a5, f a6, f a7)
                | OpAtomicCompareExchangeWeak(a0, a1, a2, a3, a4, a5, a6, a7) -> OpAtomicCompareExchangeWeak(f a0, f a1, f a2, a3, a4, a5, f a6, f a7)
                | OpAtomicIIncrement(a0, a1, a2, a3, a4) -> OpAtomicIIncrement(f a0, f a1, f a2, a3, a4)
                | OpAtomicIDecrement(a0, a1, a2, a3, a4) -> OpAtomicIDecrement(f a0, f a1, f a2, a3, a4)
                | OpAtomicIAdd(a0, a1, a2, a3, a4, a5) -> OpAtomicIAdd(f a0, f a1, f a2, a3, a4, f a5)
                | OpAtomicISub(a0, a1, a2, a3, a4, a5) -> OpAtomicISub(f a0, f a1, f a2, a3, a4, f a5)
                | OpAtomicSMin(a0, a1, a2, a3, a4, a5) -> OpAtomicSMin(f a0, f a1, f a2, a3, a4, f a5)
                | OpAtomicUMin(a0, a1, a2, a3, a4, a5) -> OpAtomicUMin(f a0, f a1, f a2, a3, a4, f a5)
                | OpAtomicSMax(a0, a1, a2, a3, a4, a5) -> OpAtomicSMax(f a0, f a1, f a2, a3, a4, f a5)
                | OpAtomicUMax(a0, a1, a2, a3, a4, a5) -> OpAtomicUMax(f a0, f a1, f a2, a3, a4, f a5)
                | OpAtomicAnd(a0, a1, a2, a3, a4, a5) -> OpAtomicAnd(f a0, f a1, f a2, a3, a4, f a5)
                | OpAtomicOr(a0, a1, a2, a3, a4, a5) -> OpAtomicOr(f a0, f a1, f a2, a3, a4, f a5)
                | OpAtomicXor(a0, a1, a2, a3, a4, a5) -> OpAtomicXor(f a0, f a1, f a2, a3, a4, f a5)
                | OpPhi(a0, a1, a2) -> OpPhi(f a0, f a1, Array.map f a2)
                | OpLoopMerge(a0, a1, a2) -> OpLoopMerge(f a0, f a1, a2)
                | OpSelectionMerge(a0, a1) -> OpSelectionMerge(f a0, a1)
                | OpLabel(a0) -> OpLabel(f a0)
                | OpBranch(a0) -> OpBranch(f a0)
                | OpBranchConditional(a0, a1, a2, a3) -> OpBranchConditional(f a0, f a1, f a2, a3)
                | OpSwitch(a0, a1, a2) -> OpSwitch(f a0, f a1, a2)
                | OpKill -> OpKill
                | OpReturn -> OpReturn
                | OpReturnValue(a0) -> OpReturnValue(f a0)
                | OpUnreachable -> OpUnreachable
                | OpLifetimeStart(a0, a1) -> OpLifetimeStart(f a0, a1)
                | OpLifetimeStop(a0, a1) -> OpLifetimeStop(f a0, a1)
                | OpGroupAsyncCopy(a0, a1, a2, a3, a4, a5, a6, a7) -> OpGroupAsyncCopy(f a0, f a1, a2, f a3, f a4, f a5, f a6, f a7)
                | OpGroupWaitEvents(a0, a1, a2) -> OpGroupWaitEvents(a0, f a1, f a2)
                | OpGroupAll(a0, a1, a2, a3) -> OpGroupAll(f a0, f a1, a2, f a3)
                | OpGroupAny(a0, a1, a2, a3) -> OpGroupAny(f a0, f a1, a2, f a3)
                | OpGroupBroadcast(a0, a1, a2, a3, a4) -> OpGroupBroadcast(f a0, f a1, a2, f a3, f a4)
                | OpGroupIAdd(a0, a1, a2, a3, a4) -> OpGroupIAdd(f a0, f a1, a2, a3, f a4)
                | OpGroupFAdd(a0, a1, a2, a3, a4) -> OpGroupFAdd(f a0, f a1, a2, a3, f a4)
                | OpGroupFMin(a0, a1, a2, a3, a4) -> OpGroupFMin(f a0, f a1, a2, a3, f a4)
                | OpGroupUMin(a0, a1, a2, a3, a4) -> OpGroupUMin(f a0, f a1, a2, a3, f a4)
                | OpGroupSMin(a0, a1, a2, a3, a4) -> OpGroupSMin(f a0, f a1, a2, a3, f a4)
                | OpGroupFMax(a0, a1, a2, a3, a4) -> OpGroupFMax(f a0, f a1, a2, a3, f a4)
                | OpGroupUMax(a0, a1, a2, a3, a4) -> OpGroupUMax(f a0, f a1, a2, a3, f a4)
                | OpGroupSMax(a0, a1, a2, a3, a4) -> OpGroupSMax(f a0, f a1, a2, a3, f a4)
                | OpReadPipe(a0, a1, a2, a3, a4, a5) -> OpReadPipe(f a0, f a1, f a2, f a3, f a4, f a5)
                | OpWritePipe(a0, a1, a2, a3, a4, a5) -> OpWritePipe(f a0, f a1, f a2, f a3, f a4, f a5)
                | OpReservedReadPipe(a0, a1, a2, a3, a4, a5, a6, a7) -> OpReservedReadPipe(f a0, f a1, f a2, f a3, f a4, f a5, f a6, f a7)
                | OpReservedWritePipe(a0, a1, a2, a3, a4, a5, a6, a7) -> OpReservedWritePipe(f a0, f a1, f a2, f a3, f a4, f a5, f a6, f a7)
                | OpReserveReadPipePackets(a0, a1, a2, a3, a4, a5) -> OpReserveReadPipePackets(f a0, f a1, f a2, f a3, f a4, f a5)
                | OpReserveWritePipePackets(a0, a1, a2, a3, a4, a5) -> OpReserveWritePipePackets(f a0, f a1, f a2, f a3, f a4, f a5)
                | OpCommitReadPipe(a0, a1, a2, a3) -> OpCommitReadPipe(f a0, f a1, f a2, f a3)
                | OpCommitWritePipe(a0, a1, a2, a3) -> OpCommitWritePipe(f a0, f a1, f a2, f a3)
                | OpIsValidReserveId(a0, a1, a2) -> OpIsValidReserveId(f a0, f a1, f a2)
                | OpGetNumPipePackets(a0, a1, a2, a3, a4) -> OpGetNumPipePackets(f a0, f a1, f a2, f a3, f a4)
                | OpGetMaxPipePackets(a0, a1, a2, a3, a4) -> OpGetMaxPipePackets(f a0, f a1, f a2, f a3, f a4)
                | OpGroupReserveReadPipePackets(a0, a1, a2, a3, a4, a5, a6) -> OpGroupReserveReadPipePackets(f a0, f a1, a2, f a3, f a4, f a5, f a6)
                | OpGroupReserveWritePipePackets(a0, a1, a2, a3, a4, a5, a6) -> OpGroupReserveWritePipePackets(f a0, f a1, a2, f a3, f a4, f a5, f a6)
                | OpGroupCommitReadPipe(a0, a1, a2, a3, a4) -> OpGroupCommitReadPipe(a0, f a1, f a2, f a3, f a4)
                | OpGroupCommitWritePipe(a0, a1, a2, a3, a4) -> OpGroupCommitWritePipe(a0, f a1, f a2, f a3, f a4)
                | OpEnqueueMarker(a0, a1, a2, a3, a4, a5) -> OpEnqueueMarker(f a0, f a1, f a2, f a3, f a4, f a5)
                | OpEnqueueKernel(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) -> OpEnqueueKernel(f a0, f a1, f a2, f a3, f a4, f a5, f a6, f a7, f a8, f a9, f a10, f a11, Array.map f a12)
                | OpGetKernelNDrangeSubGroupCount(a0, a1, a2, a3, a4, a5, a6) -> OpGetKernelNDrangeSubGroupCount(f a0, f a1, f a2, f a3, f a4, f a5, f a6)
                | OpGetKernelNDrangeMaxSubGroupSize(a0, a1, a2, a3, a4, a5, a6) -> OpGetKernelNDrangeMaxSubGroupSize(f a0, f a1, f a2, f a3, f a4, f a5, f a6)
                | OpGetKernelWorkGroupSize(a0, a1, a2, a3, a4, a5) -> OpGetKernelWorkGroupSize(f a0, f a1, f a2, f a3, f a4, f a5)
                | OpGetKernelPreferredWorkGroupSizeMultiple(a0, a1, a2, a3, a4, a5) -> OpGetKernelPreferredWorkGroupSizeMultiple(f a0, f a1, f a2, f a3, f a4, f a5)
                | OpRetainEvent(a0) -> OpRetainEvent(f a0)
                | OpReleaseEvent(a0) -> OpReleaseEvent(f a0)
                | OpCreateUserEvent(a0, a1) -> OpCreateUserEvent(f a0, f a1)
                | OpIsValidEvent(a0, a1, a2) -> OpIsValidEvent(f a0, f a1, f a2)
                | OpSetUserEventStatus(a0, a1) -> OpSetUserEventStatus(f a0, f a1)
                | OpCaptureEventProfilingInfo(a0, a1, a2) -> OpCaptureEventProfilingInfo(f a0, f a1, f a2)
                | OpGetDefaultQueue(a0, a1) -> OpGetDefaultQueue(f a0, f a1)
                | OpBuildNDRange(a0, a1, a2, a3, a4) -> OpBuildNDRange(f a0, f a1, f a2, f a3, f a4)
                | OpImageSparseSampleImplicitLod(a0, a1, a2, a3, a4, a5) -> OpImageSparseSampleImplicitLod(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageSparseSampleExplicitLod(a0, a1, a2, a3, a4, a5) -> OpImageSparseSampleExplicitLod(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageSparseSampleDrefImplicitLod(a0, a1, a2, a3, a4, a5, a6) -> OpImageSparseSampleDrefImplicitLod(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageSparseSampleDrefExplicitLod(a0, a1, a2, a3, a4, a5, a6) -> OpImageSparseSampleDrefExplicitLod(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageSparseSampleProjImplicitLod(a0, a1, a2, a3, a4, a5) -> OpImageSparseSampleProjImplicitLod(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageSparseSampleProjExplicitLod(a0, a1, a2, a3, a4, a5) -> OpImageSparseSampleProjExplicitLod(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageSparseSampleProjDrefImplicitLod(a0, a1, a2, a3, a4, a5, a6) -> OpImageSparseSampleProjDrefImplicitLod(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageSparseSampleProjDrefExplicitLod(a0, a1, a2, a3, a4, a5, a6) -> OpImageSparseSampleProjDrefExplicitLod(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageSparseFetch(a0, a1, a2, a3, a4, a5) -> OpImageSparseFetch(f a0, f a1, f a2, f a3, a4, Array.map f a5)
                | OpImageSparseGather(a0, a1, a2, a3, a4, a5, a6) -> OpImageSparseGather(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageSparseDrefGather(a0, a1, a2, a3, a4, a5, a6) -> OpImageSparseDrefGather(f a0, f a1, f a2, f a3, f a4, a5, Array.map f a6)
                | OpImageSparseTexelsResident(a0, a1, a2) -> OpImageSparseTexelsResident(f a0, f a1, f a2)
                | OpNoLine -> OpNoLine
                | OpAtomicFlagTestAndSet(a0, a1, a2, a3, a4) -> OpAtomicFlagTestAndSet(f a0, f a1, f a2, a3, a4)
                | OpAtomicFlagClear(a0, a1, a2) -> OpAtomicFlagClear(f a0, a1, a2)


        member x.WithResultType(t : uint32) =
            match x with
                | OpUndef(_,a0) -> OpUndef(t,a0)
                | OpExtInst(_,a0,a1,a2,a3) -> OpExtInst(t,a0,a1,a2,a3)
                | OpConstantTrue(_,a0) -> OpConstantTrue(t,a0)
                | OpConstantFalse(_,a0) -> OpConstantFalse(t,a0)
                | OpConstant(_,a0,a1) -> OpConstant(t,a0,a1)
                | OpConstantComposite(_,a0,a1) -> OpConstantComposite(t,a0,a1)
                | OpConstantSampler(_,a0,a1,a2,a3) -> OpConstantSampler(t,a0,a1,a2,a3)
                | OpConstantNull(_,a0) -> OpConstantNull(t,a0)
                | OpSpecConstantTrue(_,a0) -> OpSpecConstantTrue(t,a0)
                | OpSpecConstantFalse(_,a0) -> OpSpecConstantFalse(t,a0)
                | OpSpecConstant(_,a0,a1) -> OpSpecConstant(t,a0,a1)
                | OpSpecConstantComposite(_,a0,a1) -> OpSpecConstantComposite(t,a0,a1)
                | OpSpecConstantOp(_,a0,a1,a2) -> OpSpecConstantOp(t,a0,a1,a2)
                | OpFunction(_,a0,a1,a2) -> OpFunction(t,a0,a1,a2)
                | OpFunctionParameter(_,a0) -> OpFunctionParameter(t,a0)
                | OpFunctionCall(_,a0,a1,a2) -> OpFunctionCall(t,a0,a1,a2)
                | OpVariable(_,a0,a1,a2) -> OpVariable(t,a0,a1,a2)
                | OpImageTexelPointer(_,a0,a1,a2,a3) -> OpImageTexelPointer(t,a0,a1,a2,a3)
                | OpLoad(_,a0,a1,a2) -> OpLoad(t,a0,a1,a2)
                | OpAccessChain(_,a0,a1,a2) -> OpAccessChain(t,a0,a1,a2)
                | OpInBoundsAccessChain(_,a0,a1,a2) -> OpInBoundsAccessChain(t,a0,a1,a2)
                | OpPtrAccessChain(_,a0,a1,a2,a3) -> OpPtrAccessChain(t,a0,a1,a2,a3)
                | OpArrayLength(_,a0,a1,a2) -> OpArrayLength(t,a0,a1,a2)
                | OpGenericPtrMemSemantics(_,a0,a1) -> OpGenericPtrMemSemantics(t,a0,a1)
                | OpInBoundsPtrAccessChain(_,a0,a1,a2,a3) -> OpInBoundsPtrAccessChain(t,a0,a1,a2,a3)
                | OpVectorExtractDynamic(_,a0,a1,a2) -> OpVectorExtractDynamic(t,a0,a1,a2)
                | OpVectorInsertDynamic(_,a0,a1,a2,a3) -> OpVectorInsertDynamic(t,a0,a1,a2,a3)
                | OpVectorShuffle(_,a0,a1,a2,a3) -> OpVectorShuffle(t,a0,a1,a2,a3)
                | OpCompositeConstruct(_,a0,a1) -> OpCompositeConstruct(t,a0,a1)
                | OpCompositeExtract(_,a0,a1,a2) -> OpCompositeExtract(t,a0,a1,a2)
                | OpCompositeInsert(_,a0,a1,a2,a3) -> OpCompositeInsert(t,a0,a1,a2,a3)
                | OpCopyObject(_,a0,a1) -> OpCopyObject(t,a0,a1)
                | OpTranspose(_,a0,a1) -> OpTranspose(t,a0,a1)
                | OpSampledImage(_,a0,a1,a2) -> OpSampledImage(t,a0,a1,a2)
                | OpImageSampleImplicitLod(_,a0,a1,a2,a3,a4) -> OpImageSampleImplicitLod(t,a0,a1,a2,a3,a4)
                | OpImageSampleExplicitLod(_,a0,a1,a2,a3,a4) -> OpImageSampleExplicitLod(t,a0,a1,a2,a3,a4)
                | OpImageSampleDrefImplicitLod(_,a0,a1,a2,a3,a4,a5) -> OpImageSampleDrefImplicitLod(t,a0,a1,a2,a3,a4,a5)
                | OpImageSampleDrefExplicitLod(_,a0,a1,a2,a3,a4,a5) -> OpImageSampleDrefExplicitLod(t,a0,a1,a2,a3,a4,a5)
                | OpImageSampleProjImplicitLod(_,a0,a1,a2,a3,a4) -> OpImageSampleProjImplicitLod(t,a0,a1,a2,a3,a4)
                | OpImageSampleProjExplicitLod(_,a0,a1,a2,a3,a4) -> OpImageSampleProjExplicitLod(t,a0,a1,a2,a3,a4)
                | OpImageSampleProjDrefImplicitLod(_,a0,a1,a2,a3,a4,a5) -> OpImageSampleProjDrefImplicitLod(t,a0,a1,a2,a3,a4,a5)
                | OpImageSampleProjDrefExplicitLod(_,a0,a1,a2,a3,a4,a5) -> OpImageSampleProjDrefExplicitLod(t,a0,a1,a2,a3,a4,a5)
                | OpImageFetch(_,a0,a1,a2,a3,a4) -> OpImageFetch(t,a0,a1,a2,a3,a4)
                | OpImageGather(_,a0,a1,a2,a3,a4,a5) -> OpImageGather(t,a0,a1,a2,a3,a4,a5)
                | OpImageDrefGather(_,a0,a1,a2,a3,a4,a5) -> OpImageDrefGather(t,a0,a1,a2,a3,a4,a5)
                | OpImageRead(_,a0,a1,a2,a3,a4) -> OpImageRead(t,a0,a1,a2,a3,a4)
                | OpImage(_,a0,a1) -> OpImage(t,a0,a1)
                | OpImageQueryFormat(_,a0,a1) -> OpImageQueryFormat(t,a0,a1)
                | OpImageQueryOrder(_,a0,a1) -> OpImageQueryOrder(t,a0,a1)
                | OpImageQuerySizeLod(_,a0,a1,a2) -> OpImageQuerySizeLod(t,a0,a1,a2)
                | OpImageQuerySize(_,a0,a1) -> OpImageQuerySize(t,a0,a1)
                | OpImageQueryLod(_,a0,a1,a2) -> OpImageQueryLod(t,a0,a1,a2)
                | OpImageQueryLevels(_,a0,a1) -> OpImageQueryLevels(t,a0,a1)
                | OpImageQuerySamples(_,a0,a1) -> OpImageQuerySamples(t,a0,a1)
                | OpConvertFToU(_,a0,a1) -> OpConvertFToU(t,a0,a1)
                | OpConvertFToS(_,a0,a1) -> OpConvertFToS(t,a0,a1)
                | OpConvertSToF(_,a0,a1) -> OpConvertSToF(t,a0,a1)
                | OpConvertUToF(_,a0,a1) -> OpConvertUToF(t,a0,a1)
                | OpUConvert(_,a0,a1) -> OpUConvert(t,a0,a1)
                | OpSConvert(_,a0,a1) -> OpSConvert(t,a0,a1)
                | OpFConvert(_,a0,a1) -> OpFConvert(t,a0,a1)
                | OpQuantizeToF16(_,a0,a1) -> OpQuantizeToF16(t,a0,a1)
                | OpConvertPtrToU(_,a0,a1) -> OpConvertPtrToU(t,a0,a1)
                | OpSatConvertSToU(_,a0,a1) -> OpSatConvertSToU(t,a0,a1)
                | OpSatConvertUToS(_,a0,a1) -> OpSatConvertUToS(t,a0,a1)
                | OpConvertUToPtr(_,a0,a1) -> OpConvertUToPtr(t,a0,a1)
                | OpPtrCastToGeneric(_,a0,a1) -> OpPtrCastToGeneric(t,a0,a1)
                | OpGenericCastToPtr(_,a0,a1) -> OpGenericCastToPtr(t,a0,a1)
                | OpGenericCastToPtrExplicit(_,a0,a1,a2) -> OpGenericCastToPtrExplicit(t,a0,a1,a2)
                | OpBitcast(_,a0,a1) -> OpBitcast(t,a0,a1)
                | OpSNegate(_,a0,a1) -> OpSNegate(t,a0,a1)
                | OpFNegate(_,a0,a1) -> OpFNegate(t,a0,a1)
                | OpIAdd(_,a0,a1,a2) -> OpIAdd(t,a0,a1,a2)
                | OpFAdd(_,a0,a1,a2) -> OpFAdd(t,a0,a1,a2)
                | OpISub(_,a0,a1,a2) -> OpISub(t,a0,a1,a2)
                | OpFSub(_,a0,a1,a2) -> OpFSub(t,a0,a1,a2)
                | OpIMul(_,a0,a1,a2) -> OpIMul(t,a0,a1,a2)
                | OpFMul(_,a0,a1,a2) -> OpFMul(t,a0,a1,a2)
                | OpUDiv(_,a0,a1,a2) -> OpUDiv(t,a0,a1,a2)
                | OpSDiv(_,a0,a1,a2) -> OpSDiv(t,a0,a1,a2)
                | OpFDiv(_,a0,a1,a2) -> OpFDiv(t,a0,a1,a2)
                | OpUMod(_,a0,a1,a2) -> OpUMod(t,a0,a1,a2)
                | OpSRem(_,a0,a1,a2) -> OpSRem(t,a0,a1,a2)
                | OpSMod(_,a0,a1,a2) -> OpSMod(t,a0,a1,a2)
                | OpFRem(_,a0,a1,a2) -> OpFRem(t,a0,a1,a2)
                | OpFMod(_,a0,a1,a2) -> OpFMod(t,a0,a1,a2)
                | OpVectorTimesScalar(_,a0,a1,a2) -> OpVectorTimesScalar(t,a0,a1,a2)
                | OpMatrixTimesScalar(_,a0,a1,a2) -> OpMatrixTimesScalar(t,a0,a1,a2)
                | OpVectorTimesMatrix(_,a0,a1,a2) -> OpVectorTimesMatrix(t,a0,a1,a2)
                | OpMatrixTimesVector(_,a0,a1,a2) -> OpMatrixTimesVector(t,a0,a1,a2)
                | OpMatrixTimesMatrix(_,a0,a1,a2) -> OpMatrixTimesMatrix(t,a0,a1,a2)
                | OpOuterProduct(_,a0,a1,a2) -> OpOuterProduct(t,a0,a1,a2)
                | OpDot(_,a0,a1,a2) -> OpDot(t,a0,a1,a2)
                | OpIAddCarry(_,a0,a1,a2) -> OpIAddCarry(t,a0,a1,a2)
                | OpISubBorrow(_,a0,a1,a2) -> OpISubBorrow(t,a0,a1,a2)
                | OpUMulExtended(_,a0,a1,a2) -> OpUMulExtended(t,a0,a1,a2)
                | OpSMulExtended(_,a0,a1,a2) -> OpSMulExtended(t,a0,a1,a2)
                | OpAny(_,a0,a1) -> OpAny(t,a0,a1)
                | OpAll(_,a0,a1) -> OpAll(t,a0,a1)
                | OpIsNan(_,a0,a1) -> OpIsNan(t,a0,a1)
                | OpIsInf(_,a0,a1) -> OpIsInf(t,a0,a1)
                | OpIsFinite(_,a0,a1) -> OpIsFinite(t,a0,a1)
                | OpIsNormal(_,a0,a1) -> OpIsNormal(t,a0,a1)
                | OpSignBitSet(_,a0,a1) -> OpSignBitSet(t,a0,a1)
                | OpLessOrGreater(_,a0,a1,a2) -> OpLessOrGreater(t,a0,a1,a2)
                | OpOrdered(_,a0,a1,a2) -> OpOrdered(t,a0,a1,a2)
                | OpUnordered(_,a0,a1,a2) -> OpUnordered(t,a0,a1,a2)
                | OpLogicalEqual(_,a0,a1,a2) -> OpLogicalEqual(t,a0,a1,a2)
                | OpLogicalNotEqual(_,a0,a1,a2) -> OpLogicalNotEqual(t,a0,a1,a2)
                | OpLogicalOr(_,a0,a1,a2) -> OpLogicalOr(t,a0,a1,a2)
                | OpLogicalAnd(_,a0,a1,a2) -> OpLogicalAnd(t,a0,a1,a2)
                | OpLogicalNot(_,a0,a1) -> OpLogicalNot(t,a0,a1)
                | OpSelect(_,a0,a1,a2,a3) -> OpSelect(t,a0,a1,a2,a3)
                | OpIEqual(_,a0,a1,a2) -> OpIEqual(t,a0,a1,a2)
                | OpINotEqual(_,a0,a1,a2) -> OpINotEqual(t,a0,a1,a2)
                | OpUGreaterThan(_,a0,a1,a2) -> OpUGreaterThan(t,a0,a1,a2)
                | OpSGreaterThan(_,a0,a1,a2) -> OpSGreaterThan(t,a0,a1,a2)
                | OpUGreaterThanEqual(_,a0,a1,a2) -> OpUGreaterThanEqual(t,a0,a1,a2)
                | OpSGreaterThanEqual(_,a0,a1,a2) -> OpSGreaterThanEqual(t,a0,a1,a2)
                | OpULessThan(_,a0,a1,a2) -> OpULessThan(t,a0,a1,a2)
                | OpSLessThan(_,a0,a1,a2) -> OpSLessThan(t,a0,a1,a2)
                | OpULessThanEqual(_,a0,a1,a2) -> OpULessThanEqual(t,a0,a1,a2)
                | OpSLessThanEqual(_,a0,a1,a2) -> OpSLessThanEqual(t,a0,a1,a2)
                | OpFOrdEqual(_,a0,a1,a2) -> OpFOrdEqual(t,a0,a1,a2)
                | OpFUnordEqual(_,a0,a1,a2) -> OpFUnordEqual(t,a0,a1,a2)
                | OpFOrdNotEqual(_,a0,a1,a2) -> OpFOrdNotEqual(t,a0,a1,a2)
                | OpFUnordNotEqual(_,a0,a1,a2) -> OpFUnordNotEqual(t,a0,a1,a2)
                | OpFOrdLessThan(_,a0,a1,a2) -> OpFOrdLessThan(t,a0,a1,a2)
                | OpFUnordLessThan(_,a0,a1,a2) -> OpFUnordLessThan(t,a0,a1,a2)
                | OpFOrdGreaterThan(_,a0,a1,a2) -> OpFOrdGreaterThan(t,a0,a1,a2)
                | OpFUnordGreaterThan(_,a0,a1,a2) -> OpFUnordGreaterThan(t,a0,a1,a2)
                | OpFOrdLessThanEqual(_,a0,a1,a2) -> OpFOrdLessThanEqual(t,a0,a1,a2)
                | OpFUnordLessThanEqual(_,a0,a1,a2) -> OpFUnordLessThanEqual(t,a0,a1,a2)
                | OpFOrdGreaterThanEqual(_,a0,a1,a2) -> OpFOrdGreaterThanEqual(t,a0,a1,a2)
                | OpFUnordGreaterThanEqual(_,a0,a1,a2) -> OpFUnordGreaterThanEqual(t,a0,a1,a2)
                | OpShiftRightLogical(_,a0,a1,a2) -> OpShiftRightLogical(t,a0,a1,a2)
                | OpShiftRightArithmetic(_,a0,a1,a2) -> OpShiftRightArithmetic(t,a0,a1,a2)
                | OpShiftLeftLogical(_,a0,a1,a2) -> OpShiftLeftLogical(t,a0,a1,a2)
                | OpBitwiseOr(_,a0,a1,a2) -> OpBitwiseOr(t,a0,a1,a2)
                | OpBitwiseXor(_,a0,a1,a2) -> OpBitwiseXor(t,a0,a1,a2)
                | OpBitwiseAnd(_,a0,a1,a2) -> OpBitwiseAnd(t,a0,a1,a2)
                | OpNot(_,a0,a1) -> OpNot(t,a0,a1)
                | OpBitFieldInsert(_,a0,a1,a2,a3,a4) -> OpBitFieldInsert(t,a0,a1,a2,a3,a4)
                | OpBitFieldSExtract(_,a0,a1,a2,a3) -> OpBitFieldSExtract(t,a0,a1,a2,a3)
                | OpBitFieldUExtract(_,a0,a1,a2,a3) -> OpBitFieldUExtract(t,a0,a1,a2,a3)
                | OpBitReverse(_,a0,a1) -> OpBitReverse(t,a0,a1)
                | OpBitCount(_,a0,a1) -> OpBitCount(t,a0,a1)
                | OpDPdx(_,a0,a1) -> OpDPdx(t,a0,a1)
                | OpDPdy(_,a0,a1) -> OpDPdy(t,a0,a1)
                | OpFwidth(_,a0,a1) -> OpFwidth(t,a0,a1)
                | OpDPdxFine(_,a0,a1) -> OpDPdxFine(t,a0,a1)
                | OpDPdyFine(_,a0,a1) -> OpDPdyFine(t,a0,a1)
                | OpFwidthFine(_,a0,a1) -> OpFwidthFine(t,a0,a1)
                | OpDPdxCoarse(_,a0,a1) -> OpDPdxCoarse(t,a0,a1)
                | OpDPdyCoarse(_,a0,a1) -> OpDPdyCoarse(t,a0,a1)
                | OpFwidthCoarse(_,a0,a1) -> OpFwidthCoarse(t,a0,a1)
                | OpAtomicLoad(_,a0,a1,a2,a3) -> OpAtomicLoad(t,a0,a1,a2,a3)
                | OpAtomicExchange(_,a0,a1,a2,a3,a4) -> OpAtomicExchange(t,a0,a1,a2,a3,a4)
                | OpAtomicCompareExchange(_,a0,a1,a2,a3,a4,a5,a6) -> OpAtomicCompareExchange(t,a0,a1,a2,a3,a4,a5,a6)
                | OpAtomicCompareExchangeWeak(_,a0,a1,a2,a3,a4,a5,a6) -> OpAtomicCompareExchangeWeak(t,a0,a1,a2,a3,a4,a5,a6)
                | OpAtomicIIncrement(_,a0,a1,a2,a3) -> OpAtomicIIncrement(t,a0,a1,a2,a3)
                | OpAtomicIDecrement(_,a0,a1,a2,a3) -> OpAtomicIDecrement(t,a0,a1,a2,a3)
                | OpAtomicIAdd(_,a0,a1,a2,a3,a4) -> OpAtomicIAdd(t,a0,a1,a2,a3,a4)
                | OpAtomicISub(_,a0,a1,a2,a3,a4) -> OpAtomicISub(t,a0,a1,a2,a3,a4)
                | OpAtomicSMin(_,a0,a1,a2,a3,a4) -> OpAtomicSMin(t,a0,a1,a2,a3,a4)
                | OpAtomicUMin(_,a0,a1,a2,a3,a4) -> OpAtomicUMin(t,a0,a1,a2,a3,a4)
                | OpAtomicSMax(_,a0,a1,a2,a3,a4) -> OpAtomicSMax(t,a0,a1,a2,a3,a4)
                | OpAtomicUMax(_,a0,a1,a2,a3,a4) -> OpAtomicUMax(t,a0,a1,a2,a3,a4)
                | OpAtomicAnd(_,a0,a1,a2,a3,a4) -> OpAtomicAnd(t,a0,a1,a2,a3,a4)
                | OpAtomicOr(_,a0,a1,a2,a3,a4) -> OpAtomicOr(t,a0,a1,a2,a3,a4)
                | OpAtomicXor(_,a0,a1,a2,a3,a4) -> OpAtomicXor(t,a0,a1,a2,a3,a4)
                | OpPhi(_,a0,a1) -> OpPhi(t,a0,a1)
                | OpGroupAsyncCopy(_,a0,a1,a2,a3,a4,a5,a6) -> OpGroupAsyncCopy(t,a0,a1,a2,a3,a4,a5,a6)
                | OpGroupAll(_,a0,a1,a2) -> OpGroupAll(t,a0,a1,a2)
                | OpGroupAny(_,a0,a1,a2) -> OpGroupAny(t,a0,a1,a2)
                | OpGroupBroadcast(_,a0,a1,a2,a3) -> OpGroupBroadcast(t,a0,a1,a2,a3)
                | OpGroupIAdd(_,a0,a1,a2,a3) -> OpGroupIAdd(t,a0,a1,a2,a3)
                | OpGroupFAdd(_,a0,a1,a2,a3) -> OpGroupFAdd(t,a0,a1,a2,a3)
                | OpGroupFMin(_,a0,a1,a2,a3) -> OpGroupFMin(t,a0,a1,a2,a3)
                | OpGroupUMin(_,a0,a1,a2,a3) -> OpGroupUMin(t,a0,a1,a2,a3)
                | OpGroupSMin(_,a0,a1,a2,a3) -> OpGroupSMin(t,a0,a1,a2,a3)
                | OpGroupFMax(_,a0,a1,a2,a3) -> OpGroupFMax(t,a0,a1,a2,a3)
                | OpGroupUMax(_,a0,a1,a2,a3) -> OpGroupUMax(t,a0,a1,a2,a3)
                | OpGroupSMax(_,a0,a1,a2,a3) -> OpGroupSMax(t,a0,a1,a2,a3)
                | OpReadPipe(_,a0,a1,a2,a3,a4) -> OpReadPipe(t,a0,a1,a2,a3,a4)
                | OpWritePipe(_,a0,a1,a2,a3,a4) -> OpWritePipe(t,a0,a1,a2,a3,a4)
                | OpReservedReadPipe(_,a0,a1,a2,a3,a4,a5,a6) -> OpReservedReadPipe(t,a0,a1,a2,a3,a4,a5,a6)
                | OpReservedWritePipe(_,a0,a1,a2,a3,a4,a5,a6) -> OpReservedWritePipe(t,a0,a1,a2,a3,a4,a5,a6)
                | OpReserveReadPipePackets(_,a0,a1,a2,a3,a4) -> OpReserveReadPipePackets(t,a0,a1,a2,a3,a4)
                | OpReserveWritePipePackets(_,a0,a1,a2,a3,a4) -> OpReserveWritePipePackets(t,a0,a1,a2,a3,a4)
                | OpIsValidReserveId(_,a0,a1) -> OpIsValidReserveId(t,a0,a1)
                | OpGetNumPipePackets(_,a0,a1,a2,a3) -> OpGetNumPipePackets(t,a0,a1,a2,a3)
                | OpGetMaxPipePackets(_,a0,a1,a2,a3) -> OpGetMaxPipePackets(t,a0,a1,a2,a3)
                | OpGroupReserveReadPipePackets(_,a0,a1,a2,a3,a4,a5) -> OpGroupReserveReadPipePackets(t,a0,a1,a2,a3,a4,a5)
                | OpGroupReserveWritePipePackets(_,a0,a1,a2,a3,a4,a5) -> OpGroupReserveWritePipePackets(t,a0,a1,a2,a3,a4,a5)
                | OpEnqueueMarker(_,a0,a1,a2,a3,a4) -> OpEnqueueMarker(t,a0,a1,a2,a3,a4)
                | OpEnqueueKernel(_,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) -> OpEnqueueKernel(t,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
                | OpGetKernelNDrangeSubGroupCount(_,a0,a1,a2,a3,a4,a5) -> OpGetKernelNDrangeSubGroupCount(t,a0,a1,a2,a3,a4,a5)
                | OpGetKernelNDrangeMaxSubGroupSize(_,a0,a1,a2,a3,a4,a5) -> OpGetKernelNDrangeMaxSubGroupSize(t,a0,a1,a2,a3,a4,a5)
                | OpGetKernelWorkGroupSize(_,a0,a1,a2,a3,a4) -> OpGetKernelWorkGroupSize(t,a0,a1,a2,a3,a4)
                | OpGetKernelPreferredWorkGroupSizeMultiple(_,a0,a1,a2,a3,a4) -> OpGetKernelPreferredWorkGroupSizeMultiple(t,a0,a1,a2,a3,a4)
                | OpCreateUserEvent(_,a0) -> OpCreateUserEvent(t,a0)
                | OpIsValidEvent(_,a0,a1) -> OpIsValidEvent(t,a0,a1)
                | OpGetDefaultQueue(_,a0) -> OpGetDefaultQueue(t,a0)
                | OpBuildNDRange(_,a0,a1,a2,a3) -> OpBuildNDRange(t,a0,a1,a2,a3)
                | OpImageSparseSampleImplicitLod(_,a0,a1,a2,a3,a4) -> OpImageSparseSampleImplicitLod(t,a0,a1,a2,a3,a4)
                | OpImageSparseSampleExplicitLod(_,a0,a1,a2,a3,a4) -> OpImageSparseSampleExplicitLod(t,a0,a1,a2,a3,a4)
                | OpImageSparseSampleDrefImplicitLod(_,a0,a1,a2,a3,a4,a5) -> OpImageSparseSampleDrefImplicitLod(t,a0,a1,a2,a3,a4,a5)
                | OpImageSparseSampleDrefExplicitLod(_,a0,a1,a2,a3,a4,a5) -> OpImageSparseSampleDrefExplicitLod(t,a0,a1,a2,a3,a4,a5)
                | OpImageSparseSampleProjImplicitLod(_,a0,a1,a2,a3,a4) -> OpImageSparseSampleProjImplicitLod(t,a0,a1,a2,a3,a4)
                | OpImageSparseSampleProjExplicitLod(_,a0,a1,a2,a3,a4) -> OpImageSparseSampleProjExplicitLod(t,a0,a1,a2,a3,a4)
                | OpImageSparseSampleProjDrefImplicitLod(_,a0,a1,a2,a3,a4,a5) -> OpImageSparseSampleProjDrefImplicitLod(t,a0,a1,a2,a3,a4,a5)
                | OpImageSparseSampleProjDrefExplicitLod(_,a0,a1,a2,a3,a4,a5) -> OpImageSparseSampleProjDrefExplicitLod(t,a0,a1,a2,a3,a4,a5)
                | OpImageSparseFetch(_,a0,a1,a2,a3,a4) -> OpImageSparseFetch(t,a0,a1,a2,a3,a4)
                | OpImageSparseGather(_,a0,a1,a2,a3,a4,a5) -> OpImageSparseGather(t,a0,a1,a2,a3,a4,a5)
                | OpImageSparseDrefGather(_,a0,a1,a2,a3,a4,a5) -> OpImageSparseDrefGather(t,a0,a1,a2,a3,a4,a5)
                | OpImageSparseTexelsResident(_,a0,a1) -> OpImageSparseTexelsResident(t,a0,a1)
                | OpAtomicFlagTestAndSet(_,a0,a1,a2,a3) -> OpAtomicFlagTestAndSet(t,a0,a1,a2,a3)
                | _ -> x


        member x.ResultId =
            match x with
                | OpUndef(_,id) -> Some id
                | OpString(id,_) -> Some id
                | OpExtInstImport(id,_) -> Some id
                | OpExtInst(_,id,_,_,_) -> Some id
                | OpTypeVoid(id) -> Some id
                | OpTypeBool(id) -> Some id
                | OpTypeInt(id,_,_) -> Some id
                | OpTypeFloat(id,_) -> Some id
                | OpTypeVector(id,_,_) -> Some id
                | OpTypeMatrix(id,_,_) -> Some id
                | OpTypeImage(id,_,_,_,_,_,_,_,_) -> Some id
                | OpTypeSampler(id) -> Some id
                | OpTypeSampledImage(id,_) -> Some id
                | OpTypeArray(id,_,_) -> Some id
                | OpTypeRuntimeArray(id,_) -> Some id
                | OpTypeStruct(id,_) -> Some id
                | OpTypeOpaque(id,_) -> Some id
                | OpTypePointer(id,_,_) -> Some id
                | OpTypeFunction(id,_,_) -> Some id
                | OpTypeEvent(id) -> Some id
                | OpTypeDeviceEvent(id) -> Some id
                | OpTypeReserveId(id) -> Some id
                | OpTypeQueue(id) -> Some id
                | OpTypePipe(id,_) -> Some id
                | OpConstantTrue(_,id) -> Some id
                | OpConstantFalse(_,id) -> Some id
                | OpConstant(_,id,_) -> Some id
                | OpConstantComposite(_,id,_) -> Some id
                | OpConstantSampler(_,id,_,_,_) -> Some id
                | OpConstantNull(_,id) -> Some id
                | OpSpecConstantTrue(_,id) -> Some id
                | OpSpecConstantFalse(_,id) -> Some id
                | OpSpecConstant(_,id,_) -> Some id
                | OpSpecConstantComposite(_,id,_) -> Some id
                | OpSpecConstantOp(_,id,_,_) -> Some id
                | OpFunction(_,id,_,_) -> Some id
                | OpFunctionParameter(_,id) -> Some id
                | OpFunctionCall(_,id,_,_) -> Some id
                | OpVariable(_,id,_,_) -> Some id
                | OpImageTexelPointer(_,id,_,_,_) -> Some id
                | OpLoad(_,id,_,_) -> Some id
                | OpAccessChain(_,id,_,_) -> Some id
                | OpInBoundsAccessChain(_,id,_,_) -> Some id
                | OpPtrAccessChain(_,id,_,_,_) -> Some id
                | OpArrayLength(_,id,_,_) -> Some id
                | OpGenericPtrMemSemantics(_,id,_) -> Some id
                | OpInBoundsPtrAccessChain(_,id,_,_,_) -> Some id
                | OpDecorationGroup(id) -> Some id
                | OpVectorExtractDynamic(_,id,_,_) -> Some id
                | OpVectorInsertDynamic(_,id,_,_,_) -> Some id
                | OpVectorShuffle(_,id,_,_,_) -> Some id
                | OpCompositeConstruct(_,id,_) -> Some id
                | OpCompositeExtract(_,id,_,_) -> Some id
                | OpCompositeInsert(_,id,_,_,_) -> Some id
                | OpCopyObject(_,id,_) -> Some id
                | OpTranspose(_,id,_) -> Some id
                | OpSampledImage(_,id,_,_) -> Some id
                | OpImageSampleImplicitLod(_,id,_,_,_,_) -> Some id
                | OpImageSampleExplicitLod(_,id,_,_,_,_) -> Some id
                | OpImageSampleDrefImplicitLod(_,id,_,_,_,_,_) -> Some id
                | OpImageSampleDrefExplicitLod(_,id,_,_,_,_,_) -> Some id
                | OpImageSampleProjImplicitLod(_,id,_,_,_,_) -> Some id
                | OpImageSampleProjExplicitLod(_,id,_,_,_,_) -> Some id
                | OpImageSampleProjDrefImplicitLod(_,id,_,_,_,_,_) -> Some id
                | OpImageSampleProjDrefExplicitLod(_,id,_,_,_,_,_) -> Some id
                | OpImageFetch(_,id,_,_,_,_) -> Some id
                | OpImageGather(_,id,_,_,_,_,_) -> Some id
                | OpImageDrefGather(_,id,_,_,_,_,_) -> Some id
                | OpImageRead(_,id,_,_,_,_) -> Some id
                | OpImage(_,id,_) -> Some id
                | OpImageQueryFormat(_,id,_) -> Some id
                | OpImageQueryOrder(_,id,_) -> Some id
                | OpImageQuerySizeLod(_,id,_,_) -> Some id
                | OpImageQuerySize(_,id,_) -> Some id
                | OpImageQueryLod(_,id,_,_) -> Some id
                | OpImageQueryLevels(_,id,_) -> Some id
                | OpImageQuerySamples(_,id,_) -> Some id
                | OpConvertFToU(_,id,_) -> Some id
                | OpConvertFToS(_,id,_) -> Some id
                | OpConvertSToF(_,id,_) -> Some id
                | OpConvertUToF(_,id,_) -> Some id
                | OpUConvert(_,id,_) -> Some id
                | OpSConvert(_,id,_) -> Some id
                | OpFConvert(_,id,_) -> Some id
                | OpQuantizeToF16(_,id,_) -> Some id
                | OpConvertPtrToU(_,id,_) -> Some id
                | OpSatConvertSToU(_,id,_) -> Some id
                | OpSatConvertUToS(_,id,_) -> Some id
                | OpConvertUToPtr(_,id,_) -> Some id
                | OpPtrCastToGeneric(_,id,_) -> Some id
                | OpGenericCastToPtr(_,id,_) -> Some id
                | OpGenericCastToPtrExplicit(_,id,_,_) -> Some id
                | OpBitcast(_,id,_) -> Some id
                | OpSNegate(_,id,_) -> Some id
                | OpFNegate(_,id,_) -> Some id
                | OpIAdd(_,id,_,_) -> Some id
                | OpFAdd(_,id,_,_) -> Some id
                | OpISub(_,id,_,_) -> Some id
                | OpFSub(_,id,_,_) -> Some id
                | OpIMul(_,id,_,_) -> Some id
                | OpFMul(_,id,_,_) -> Some id
                | OpUDiv(_,id,_,_) -> Some id
                | OpSDiv(_,id,_,_) -> Some id
                | OpFDiv(_,id,_,_) -> Some id
                | OpUMod(_,id,_,_) -> Some id
                | OpSRem(_,id,_,_) -> Some id
                | OpSMod(_,id,_,_) -> Some id
                | OpFRem(_,id,_,_) -> Some id
                | OpFMod(_,id,_,_) -> Some id
                | OpVectorTimesScalar(_,id,_,_) -> Some id
                | OpMatrixTimesScalar(_,id,_,_) -> Some id
                | OpVectorTimesMatrix(_,id,_,_) -> Some id
                | OpMatrixTimesVector(_,id,_,_) -> Some id
                | OpMatrixTimesMatrix(_,id,_,_) -> Some id
                | OpOuterProduct(_,id,_,_) -> Some id
                | OpDot(_,id,_,_) -> Some id
                | OpIAddCarry(_,id,_,_) -> Some id
                | OpISubBorrow(_,id,_,_) -> Some id
                | OpUMulExtended(_,id,_,_) -> Some id
                | OpSMulExtended(_,id,_,_) -> Some id
                | OpAny(_,id,_) -> Some id
                | OpAll(_,id,_) -> Some id
                | OpIsNan(_,id,_) -> Some id
                | OpIsInf(_,id,_) -> Some id
                | OpIsFinite(_,id,_) -> Some id
                | OpIsNormal(_,id,_) -> Some id
                | OpSignBitSet(_,id,_) -> Some id
                | OpLessOrGreater(_,id,_,_) -> Some id
                | OpOrdered(_,id,_,_) -> Some id
                | OpUnordered(_,id,_,_) -> Some id
                | OpLogicalEqual(_,id,_,_) -> Some id
                | OpLogicalNotEqual(_,id,_,_) -> Some id
                | OpLogicalOr(_,id,_,_) -> Some id
                | OpLogicalAnd(_,id,_,_) -> Some id
                | OpLogicalNot(_,id,_) -> Some id
                | OpSelect(_,id,_,_,_) -> Some id
                | OpIEqual(_,id,_,_) -> Some id
                | OpINotEqual(_,id,_,_) -> Some id
                | OpUGreaterThan(_,id,_,_) -> Some id
                | OpSGreaterThan(_,id,_,_) -> Some id
                | OpUGreaterThanEqual(_,id,_,_) -> Some id
                | OpSGreaterThanEqual(_,id,_,_) -> Some id
                | OpULessThan(_,id,_,_) -> Some id
                | OpSLessThan(_,id,_,_) -> Some id
                | OpULessThanEqual(_,id,_,_) -> Some id
                | OpSLessThanEqual(_,id,_,_) -> Some id
                | OpFOrdEqual(_,id,_,_) -> Some id
                | OpFUnordEqual(_,id,_,_) -> Some id
                | OpFOrdNotEqual(_,id,_,_) -> Some id
                | OpFUnordNotEqual(_,id,_,_) -> Some id
                | OpFOrdLessThan(_,id,_,_) -> Some id
                | OpFUnordLessThan(_,id,_,_) -> Some id
                | OpFOrdGreaterThan(_,id,_,_) -> Some id
                | OpFUnordGreaterThan(_,id,_,_) -> Some id
                | OpFOrdLessThanEqual(_,id,_,_) -> Some id
                | OpFUnordLessThanEqual(_,id,_,_) -> Some id
                | OpFOrdGreaterThanEqual(_,id,_,_) -> Some id
                | OpFUnordGreaterThanEqual(_,id,_,_) -> Some id
                | OpShiftRightLogical(_,id,_,_) -> Some id
                | OpShiftRightArithmetic(_,id,_,_) -> Some id
                | OpShiftLeftLogical(_,id,_,_) -> Some id
                | OpBitwiseOr(_,id,_,_) -> Some id
                | OpBitwiseXor(_,id,_,_) -> Some id
                | OpBitwiseAnd(_,id,_,_) -> Some id
                | OpNot(_,id,_) -> Some id
                | OpBitFieldInsert(_,id,_,_,_,_) -> Some id
                | OpBitFieldSExtract(_,id,_,_,_) -> Some id
                | OpBitFieldUExtract(_,id,_,_,_) -> Some id
                | OpBitReverse(_,id,_) -> Some id
                | OpBitCount(_,id,_) -> Some id
                | OpDPdx(_,id,_) -> Some id
                | OpDPdy(_,id,_) -> Some id
                | OpFwidth(_,id,_) -> Some id
                | OpDPdxFine(_,id,_) -> Some id
                | OpDPdyFine(_,id,_) -> Some id
                | OpFwidthFine(_,id,_) -> Some id
                | OpDPdxCoarse(_,id,_) -> Some id
                | OpDPdyCoarse(_,id,_) -> Some id
                | OpFwidthCoarse(_,id,_) -> Some id
                | OpAtomicLoad(_,id,_,_,_) -> Some id
                | OpAtomicExchange(_,id,_,_,_,_) -> Some id
                | OpAtomicCompareExchange(_,id,_,_,_,_,_,_) -> Some id
                | OpAtomicCompareExchangeWeak(_,id,_,_,_,_,_,_) -> Some id
                | OpAtomicIIncrement(_,id,_,_,_) -> Some id
                | OpAtomicIDecrement(_,id,_,_,_) -> Some id
                | OpAtomicIAdd(_,id,_,_,_,_) -> Some id
                | OpAtomicISub(_,id,_,_,_,_) -> Some id
                | OpAtomicSMin(_,id,_,_,_,_) -> Some id
                | OpAtomicUMin(_,id,_,_,_,_) -> Some id
                | OpAtomicSMax(_,id,_,_,_,_) -> Some id
                | OpAtomicUMax(_,id,_,_,_,_) -> Some id
                | OpAtomicAnd(_,id,_,_,_,_) -> Some id
                | OpAtomicOr(_,id,_,_,_,_) -> Some id
                | OpAtomicXor(_,id,_,_,_,_) -> Some id
                | OpPhi(_,id,_) -> Some id
                | OpLabel(id) -> Some id
                | OpGroupAsyncCopy(_,id,_,_,_,_,_,_) -> Some id
                | OpGroupAll(_,id,_,_) -> Some id
                | OpGroupAny(_,id,_,_) -> Some id
                | OpGroupBroadcast(_,id,_,_,_) -> Some id
                | OpGroupIAdd(_,id,_,_,_) -> Some id
                | OpGroupFAdd(_,id,_,_,_) -> Some id
                | OpGroupFMin(_,id,_,_,_) -> Some id
                | OpGroupUMin(_,id,_,_,_) -> Some id
                | OpGroupSMin(_,id,_,_,_) -> Some id
                | OpGroupFMax(_,id,_,_,_) -> Some id
                | OpGroupUMax(_,id,_,_,_) -> Some id
                | OpGroupSMax(_,id,_,_,_) -> Some id
                | OpReadPipe(_,id,_,_,_,_) -> Some id
                | OpWritePipe(_,id,_,_,_,_) -> Some id
                | OpReservedReadPipe(_,id,_,_,_,_,_,_) -> Some id
                | OpReservedWritePipe(_,id,_,_,_,_,_,_) -> Some id
                | OpReserveReadPipePackets(_,id,_,_,_,_) -> Some id
                | OpReserveWritePipePackets(_,id,_,_,_,_) -> Some id
                | OpIsValidReserveId(_,id,_) -> Some id
                | OpGetNumPipePackets(_,id,_,_,_) -> Some id
                | OpGetMaxPipePackets(_,id,_,_,_) -> Some id
                | OpGroupReserveReadPipePackets(_,id,_,_,_,_,_) -> Some id
                | OpGroupReserveWritePipePackets(_,id,_,_,_,_,_) -> Some id
                | OpEnqueueMarker(_,id,_,_,_,_) -> Some id
                | OpEnqueueKernel(_,id,_,_,_,_,_,_,_,_,_,_,_) -> Some id
                | OpGetKernelNDrangeSubGroupCount(_,id,_,_,_,_,_) -> Some id
                | OpGetKernelNDrangeMaxSubGroupSize(_,id,_,_,_,_,_) -> Some id
                | OpGetKernelWorkGroupSize(_,id,_,_,_,_) -> Some id
                | OpGetKernelPreferredWorkGroupSizeMultiple(_,id,_,_,_,_) -> Some id
                | OpCreateUserEvent(_,id) -> Some id
                | OpIsValidEvent(_,id,_) -> Some id
                | OpGetDefaultQueue(_,id) -> Some id
                | OpBuildNDRange(_,id,_,_,_) -> Some id
                | OpImageSparseSampleImplicitLod(_,id,_,_,_,_) -> Some id
                | OpImageSparseSampleExplicitLod(_,id,_,_,_,_) -> Some id
                | OpImageSparseSampleDrefImplicitLod(_,id,_,_,_,_,_) -> Some id
                | OpImageSparseSampleDrefExplicitLod(_,id,_,_,_,_,_) -> Some id
                | OpImageSparseSampleProjImplicitLod(_,id,_,_,_,_) -> Some id
                | OpImageSparseSampleProjExplicitLod(_,id,_,_,_,_) -> Some id
                | OpImageSparseSampleProjDrefImplicitLod(_,id,_,_,_,_,_) -> Some id
                | OpImageSparseSampleProjDrefExplicitLod(_,id,_,_,_,_,_) -> Some id
                | OpImageSparseFetch(_,id,_,_,_,_) -> Some id
                | OpImageSparseGather(_,id,_,_,_,_,_) -> Some id
                | OpImageSparseDrefGather(_,id,_,_,_,_,_) -> Some id
                | OpImageSparseTexelsResident(_,id,_) -> Some id
                | OpAtomicFlagTestAndSet(_,id,_,_,_) -> Some id
                | _ -> None


        member x.WithResultId(id : uint32) =
            match x with
                | OpUndef(tid,_) -> OpUndef(tid,id)
                | OpString(_,a0) -> OpString(id,a0)
                | OpExtInstImport(_,a0) -> OpExtInstImport(id,a0)
                | OpExtInst(tid,_,a0,a1,a2) -> OpExtInst(tid,id,a0,a1,a2)
                | OpTypeVoid(_) -> OpTypeVoid(id)
                | OpTypeBool(_) -> OpTypeBool(id)
                | OpTypeInt(_,a0,a1) -> OpTypeInt(id,a0,a1)
                | OpTypeFloat(_,a0) -> OpTypeFloat(id,a0)
                | OpTypeVector(_,a0,a1) -> OpTypeVector(id,a0,a1)
                | OpTypeMatrix(_,a0,a1) -> OpTypeMatrix(id,a0,a1)
                | OpTypeImage(_,a0,a1,a2,a3,a4,a5,a6,a7) -> OpTypeImage(id,a0,a1,a2,a3,a4,a5,a6,a7)
                | OpTypeSampler(_) -> OpTypeSampler(id)
                | OpTypeSampledImage(_,a0) -> OpTypeSampledImage(id,a0)
                | OpTypeArray(_,a0,a1) -> OpTypeArray(id,a0,a1)
                | OpTypeRuntimeArray(_,a0) -> OpTypeRuntimeArray(id,a0)
                | OpTypeStruct(_,a0) -> OpTypeStruct(id,a0)
                | OpTypeOpaque(_,a0) -> OpTypeOpaque(id,a0)
                | OpTypePointer(_,a0,a1) -> OpTypePointer(id,a0,a1)
                | OpTypeFunction(_,a0,a1) -> OpTypeFunction(id,a0,a1)
                | OpTypeEvent(_) -> OpTypeEvent(id)
                | OpTypeDeviceEvent(_) -> OpTypeDeviceEvent(id)
                | OpTypeReserveId(_) -> OpTypeReserveId(id)
                | OpTypeQueue(_) -> OpTypeQueue(id)
                | OpTypePipe(_,a0) -> OpTypePipe(id,a0)
                | OpConstantTrue(tid,_) -> OpConstantTrue(tid,id)
                | OpConstantFalse(tid,_) -> OpConstantFalse(tid,id)
                | OpConstant(tid,_,a0) -> OpConstant(tid,id,a0)
                | OpConstantComposite(tid,_,a0) -> OpConstantComposite(tid,id,a0)
                | OpConstantSampler(tid,_,a0,a1,a2) -> OpConstantSampler(tid,id,a0,a1,a2)
                | OpConstantNull(tid,_) -> OpConstantNull(tid,id)
                | OpSpecConstantTrue(tid,_) -> OpSpecConstantTrue(tid,id)
                | OpSpecConstantFalse(tid,_) -> OpSpecConstantFalse(tid,id)
                | OpSpecConstant(tid,_,a0) -> OpSpecConstant(tid,id,a0)
                | OpSpecConstantComposite(tid,_,a0) -> OpSpecConstantComposite(tid,id,a0)
                | OpSpecConstantOp(tid,_,a0,a1) -> OpSpecConstantOp(tid,id,a0,a1)
                | OpFunction(tid,_,a0,a1) -> OpFunction(tid,id,a0,a1)
                | OpFunctionParameter(tid,_) -> OpFunctionParameter(tid,id)
                | OpFunctionCall(tid,_,a0,a1) -> OpFunctionCall(tid,id,a0,a1)
                | OpVariable(tid,_,a0,a1) -> OpVariable(tid,id,a0,a1)
                | OpImageTexelPointer(tid,_,a0,a1,a2) -> OpImageTexelPointer(tid,id,a0,a1,a2)
                | OpLoad(tid,_,a0,a1) -> OpLoad(tid,id,a0,a1)
                | OpAccessChain(tid,_,a0,a1) -> OpAccessChain(tid,id,a0,a1)
                | OpInBoundsAccessChain(tid,_,a0,a1) -> OpInBoundsAccessChain(tid,id,a0,a1)
                | OpPtrAccessChain(tid,_,a0,a1,a2) -> OpPtrAccessChain(tid,id,a0,a1,a2)
                | OpArrayLength(tid,_,a0,a1) -> OpArrayLength(tid,id,a0,a1)
                | OpGenericPtrMemSemantics(tid,_,a0) -> OpGenericPtrMemSemantics(tid,id,a0)
                | OpInBoundsPtrAccessChain(tid,_,a0,a1,a2) -> OpInBoundsPtrAccessChain(tid,id,a0,a1,a2)
                | OpDecorationGroup(_) -> OpDecorationGroup(id)
                | OpVectorExtractDynamic(tid,_,a0,a1) -> OpVectorExtractDynamic(tid,id,a0,a1)
                | OpVectorInsertDynamic(tid,_,a0,a1,a2) -> OpVectorInsertDynamic(tid,id,a0,a1,a2)
                | OpVectorShuffle(tid,_,a0,a1,a2) -> OpVectorShuffle(tid,id,a0,a1,a2)
                | OpCompositeConstruct(tid,_,a0) -> OpCompositeConstruct(tid,id,a0)
                | OpCompositeExtract(tid,_,a0,a1) -> OpCompositeExtract(tid,id,a0,a1)
                | OpCompositeInsert(tid,_,a0,a1,a2) -> OpCompositeInsert(tid,id,a0,a1,a2)
                | OpCopyObject(tid,_,a0) -> OpCopyObject(tid,id,a0)
                | OpTranspose(tid,_,a0) -> OpTranspose(tid,id,a0)
                | OpSampledImage(tid,_,a0,a1) -> OpSampledImage(tid,id,a0,a1)
                | OpImageSampleImplicitLod(tid,_,a0,a1,a2,a3) -> OpImageSampleImplicitLod(tid,id,a0,a1,a2,a3)
                | OpImageSampleExplicitLod(tid,_,a0,a1,a2,a3) -> OpImageSampleExplicitLod(tid,id,a0,a1,a2,a3)
                | OpImageSampleDrefImplicitLod(tid,_,a0,a1,a2,a3,a4) -> OpImageSampleDrefImplicitLod(tid,id,a0,a1,a2,a3,a4)
                | OpImageSampleDrefExplicitLod(tid,_,a0,a1,a2,a3,a4) -> OpImageSampleDrefExplicitLod(tid,id,a0,a1,a2,a3,a4)
                | OpImageSampleProjImplicitLod(tid,_,a0,a1,a2,a3) -> OpImageSampleProjImplicitLod(tid,id,a0,a1,a2,a3)
                | OpImageSampleProjExplicitLod(tid,_,a0,a1,a2,a3) -> OpImageSampleProjExplicitLod(tid,id,a0,a1,a2,a3)
                | OpImageSampleProjDrefImplicitLod(tid,_,a0,a1,a2,a3,a4) -> OpImageSampleProjDrefImplicitLod(tid,id,a0,a1,a2,a3,a4)
                | OpImageSampleProjDrefExplicitLod(tid,_,a0,a1,a2,a3,a4) -> OpImageSampleProjDrefExplicitLod(tid,id,a0,a1,a2,a3,a4)
                | OpImageFetch(tid,_,a0,a1,a2,a3) -> OpImageFetch(tid,id,a0,a1,a2,a3)
                | OpImageGather(tid,_,a0,a1,a2,a3,a4) -> OpImageGather(tid,id,a0,a1,a2,a3,a4)
                | OpImageDrefGather(tid,_,a0,a1,a2,a3,a4) -> OpImageDrefGather(tid,id,a0,a1,a2,a3,a4)
                | OpImageRead(tid,_,a0,a1,a2,a3) -> OpImageRead(tid,id,a0,a1,a2,a3)
                | OpImage(tid,_,a0) -> OpImage(tid,id,a0)
                | OpImageQueryFormat(tid,_,a0) -> OpImageQueryFormat(tid,id,a0)
                | OpImageQueryOrder(tid,_,a0) -> OpImageQueryOrder(tid,id,a0)
                | OpImageQuerySizeLod(tid,_,a0,a1) -> OpImageQuerySizeLod(tid,id,a0,a1)
                | OpImageQuerySize(tid,_,a0) -> OpImageQuerySize(tid,id,a0)
                | OpImageQueryLod(tid,_,a0,a1) -> OpImageQueryLod(tid,id,a0,a1)
                | OpImageQueryLevels(tid,_,a0) -> OpImageQueryLevels(tid,id,a0)
                | OpImageQuerySamples(tid,_,a0) -> OpImageQuerySamples(tid,id,a0)
                | OpConvertFToU(tid,_,a0) -> OpConvertFToU(tid,id,a0)
                | OpConvertFToS(tid,_,a0) -> OpConvertFToS(tid,id,a0)
                | OpConvertSToF(tid,_,a0) -> OpConvertSToF(tid,id,a0)
                | OpConvertUToF(tid,_,a0) -> OpConvertUToF(tid,id,a0)
                | OpUConvert(tid,_,a0) -> OpUConvert(tid,id,a0)
                | OpSConvert(tid,_,a0) -> OpSConvert(tid,id,a0)
                | OpFConvert(tid,_,a0) -> OpFConvert(tid,id,a0)
                | OpQuantizeToF16(tid,_,a0) -> OpQuantizeToF16(tid,id,a0)
                | OpConvertPtrToU(tid,_,a0) -> OpConvertPtrToU(tid,id,a0)
                | OpSatConvertSToU(tid,_,a0) -> OpSatConvertSToU(tid,id,a0)
                | OpSatConvertUToS(tid,_,a0) -> OpSatConvertUToS(tid,id,a0)
                | OpConvertUToPtr(tid,_,a0) -> OpConvertUToPtr(tid,id,a0)
                | OpPtrCastToGeneric(tid,_,a0) -> OpPtrCastToGeneric(tid,id,a0)
                | OpGenericCastToPtr(tid,_,a0) -> OpGenericCastToPtr(tid,id,a0)
                | OpGenericCastToPtrExplicit(tid,_,a0,a1) -> OpGenericCastToPtrExplicit(tid,id,a0,a1)
                | OpBitcast(tid,_,a0) -> OpBitcast(tid,id,a0)
                | OpSNegate(tid,_,a0) -> OpSNegate(tid,id,a0)
                | OpFNegate(tid,_,a0) -> OpFNegate(tid,id,a0)
                | OpIAdd(tid,_,a0,a1) -> OpIAdd(tid,id,a0,a1)
                | OpFAdd(tid,_,a0,a1) -> OpFAdd(tid,id,a0,a1)
                | OpISub(tid,_,a0,a1) -> OpISub(tid,id,a0,a1)
                | OpFSub(tid,_,a0,a1) -> OpFSub(tid,id,a0,a1)
                | OpIMul(tid,_,a0,a1) -> OpIMul(tid,id,a0,a1)
                | OpFMul(tid,_,a0,a1) -> OpFMul(tid,id,a0,a1)
                | OpUDiv(tid,_,a0,a1) -> OpUDiv(tid,id,a0,a1)
                | OpSDiv(tid,_,a0,a1) -> OpSDiv(tid,id,a0,a1)
                | OpFDiv(tid,_,a0,a1) -> OpFDiv(tid,id,a0,a1)
                | OpUMod(tid,_,a0,a1) -> OpUMod(tid,id,a0,a1)
                | OpSRem(tid,_,a0,a1) -> OpSRem(tid,id,a0,a1)
                | OpSMod(tid,_,a0,a1) -> OpSMod(tid,id,a0,a1)
                | OpFRem(tid,_,a0,a1) -> OpFRem(tid,id,a0,a1)
                | OpFMod(tid,_,a0,a1) -> OpFMod(tid,id,a0,a1)
                | OpVectorTimesScalar(tid,_,a0,a1) -> OpVectorTimesScalar(tid,id,a0,a1)
                | OpMatrixTimesScalar(tid,_,a0,a1) -> OpMatrixTimesScalar(tid,id,a0,a1)
                | OpVectorTimesMatrix(tid,_,a0,a1) -> OpVectorTimesMatrix(tid,id,a0,a1)
                | OpMatrixTimesVector(tid,_,a0,a1) -> OpMatrixTimesVector(tid,id,a0,a1)
                | OpMatrixTimesMatrix(tid,_,a0,a1) -> OpMatrixTimesMatrix(tid,id,a0,a1)
                | OpOuterProduct(tid,_,a0,a1) -> OpOuterProduct(tid,id,a0,a1)
                | OpDot(tid,_,a0,a1) -> OpDot(tid,id,a0,a1)
                | OpIAddCarry(tid,_,a0,a1) -> OpIAddCarry(tid,id,a0,a1)
                | OpISubBorrow(tid,_,a0,a1) -> OpISubBorrow(tid,id,a0,a1)
                | OpUMulExtended(tid,_,a0,a1) -> OpUMulExtended(tid,id,a0,a1)
                | OpSMulExtended(tid,_,a0,a1) -> OpSMulExtended(tid,id,a0,a1)
                | OpAny(tid,_,a0) -> OpAny(tid,id,a0)
                | OpAll(tid,_,a0) -> OpAll(tid,id,a0)
                | OpIsNan(tid,_,a0) -> OpIsNan(tid,id,a0)
                | OpIsInf(tid,_,a0) -> OpIsInf(tid,id,a0)
                | OpIsFinite(tid,_,a0) -> OpIsFinite(tid,id,a0)
                | OpIsNormal(tid,_,a0) -> OpIsNormal(tid,id,a0)
                | OpSignBitSet(tid,_,a0) -> OpSignBitSet(tid,id,a0)
                | OpLessOrGreater(tid,_,a0,a1) -> OpLessOrGreater(tid,id,a0,a1)
                | OpOrdered(tid,_,a0,a1) -> OpOrdered(tid,id,a0,a1)
                | OpUnordered(tid,_,a0,a1) -> OpUnordered(tid,id,a0,a1)
                | OpLogicalEqual(tid,_,a0,a1) -> OpLogicalEqual(tid,id,a0,a1)
                | OpLogicalNotEqual(tid,_,a0,a1) -> OpLogicalNotEqual(tid,id,a0,a1)
                | OpLogicalOr(tid,_,a0,a1) -> OpLogicalOr(tid,id,a0,a1)
                | OpLogicalAnd(tid,_,a0,a1) -> OpLogicalAnd(tid,id,a0,a1)
                | OpLogicalNot(tid,_,a0) -> OpLogicalNot(tid,id,a0)
                | OpSelect(tid,_,a0,a1,a2) -> OpSelect(tid,id,a0,a1,a2)
                | OpIEqual(tid,_,a0,a1) -> OpIEqual(tid,id,a0,a1)
                | OpINotEqual(tid,_,a0,a1) -> OpINotEqual(tid,id,a0,a1)
                | OpUGreaterThan(tid,_,a0,a1) -> OpUGreaterThan(tid,id,a0,a1)
                | OpSGreaterThan(tid,_,a0,a1) -> OpSGreaterThan(tid,id,a0,a1)
                | OpUGreaterThanEqual(tid,_,a0,a1) -> OpUGreaterThanEqual(tid,id,a0,a1)
                | OpSGreaterThanEqual(tid,_,a0,a1) -> OpSGreaterThanEqual(tid,id,a0,a1)
                | OpULessThan(tid,_,a0,a1) -> OpULessThan(tid,id,a0,a1)
                | OpSLessThan(tid,_,a0,a1) -> OpSLessThan(tid,id,a0,a1)
                | OpULessThanEqual(tid,_,a0,a1) -> OpULessThanEqual(tid,id,a0,a1)
                | OpSLessThanEqual(tid,_,a0,a1) -> OpSLessThanEqual(tid,id,a0,a1)
                | OpFOrdEqual(tid,_,a0,a1) -> OpFOrdEqual(tid,id,a0,a1)
                | OpFUnordEqual(tid,_,a0,a1) -> OpFUnordEqual(tid,id,a0,a1)
                | OpFOrdNotEqual(tid,_,a0,a1) -> OpFOrdNotEqual(tid,id,a0,a1)
                | OpFUnordNotEqual(tid,_,a0,a1) -> OpFUnordNotEqual(tid,id,a0,a1)
                | OpFOrdLessThan(tid,_,a0,a1) -> OpFOrdLessThan(tid,id,a0,a1)
                | OpFUnordLessThan(tid,_,a0,a1) -> OpFUnordLessThan(tid,id,a0,a1)
                | OpFOrdGreaterThan(tid,_,a0,a1) -> OpFOrdGreaterThan(tid,id,a0,a1)
                | OpFUnordGreaterThan(tid,_,a0,a1) -> OpFUnordGreaterThan(tid,id,a0,a1)
                | OpFOrdLessThanEqual(tid,_,a0,a1) -> OpFOrdLessThanEqual(tid,id,a0,a1)
                | OpFUnordLessThanEqual(tid,_,a0,a1) -> OpFUnordLessThanEqual(tid,id,a0,a1)
                | OpFOrdGreaterThanEqual(tid,_,a0,a1) -> OpFOrdGreaterThanEqual(tid,id,a0,a1)
                | OpFUnordGreaterThanEqual(tid,_,a0,a1) -> OpFUnordGreaterThanEqual(tid,id,a0,a1)
                | OpShiftRightLogical(tid,_,a0,a1) -> OpShiftRightLogical(tid,id,a0,a1)
                | OpShiftRightArithmetic(tid,_,a0,a1) -> OpShiftRightArithmetic(tid,id,a0,a1)
                | OpShiftLeftLogical(tid,_,a0,a1) -> OpShiftLeftLogical(tid,id,a0,a1)
                | OpBitwiseOr(tid,_,a0,a1) -> OpBitwiseOr(tid,id,a0,a1)
                | OpBitwiseXor(tid,_,a0,a1) -> OpBitwiseXor(tid,id,a0,a1)
                | OpBitwiseAnd(tid,_,a0,a1) -> OpBitwiseAnd(tid,id,a0,a1)
                | OpNot(tid,_,a0) -> OpNot(tid,id,a0)
                | OpBitFieldInsert(tid,_,a0,a1,a2,a3) -> OpBitFieldInsert(tid,id,a0,a1,a2,a3)
                | OpBitFieldSExtract(tid,_,a0,a1,a2) -> OpBitFieldSExtract(tid,id,a0,a1,a2)
                | OpBitFieldUExtract(tid,_,a0,a1,a2) -> OpBitFieldUExtract(tid,id,a0,a1,a2)
                | OpBitReverse(tid,_,a0) -> OpBitReverse(tid,id,a0)
                | OpBitCount(tid,_,a0) -> OpBitCount(tid,id,a0)
                | OpDPdx(tid,_,a0) -> OpDPdx(tid,id,a0)
                | OpDPdy(tid,_,a0) -> OpDPdy(tid,id,a0)
                | OpFwidth(tid,_,a0) -> OpFwidth(tid,id,a0)
                | OpDPdxFine(tid,_,a0) -> OpDPdxFine(tid,id,a0)
                | OpDPdyFine(tid,_,a0) -> OpDPdyFine(tid,id,a0)
                | OpFwidthFine(tid,_,a0) -> OpFwidthFine(tid,id,a0)
                | OpDPdxCoarse(tid,_,a0) -> OpDPdxCoarse(tid,id,a0)
                | OpDPdyCoarse(tid,_,a0) -> OpDPdyCoarse(tid,id,a0)
                | OpFwidthCoarse(tid,_,a0) -> OpFwidthCoarse(tid,id,a0)
                | OpAtomicLoad(tid,_,a0,a1,a2) -> OpAtomicLoad(tid,id,a0,a1,a2)
                | OpAtomicExchange(tid,_,a0,a1,a2,a3) -> OpAtomicExchange(tid,id,a0,a1,a2,a3)
                | OpAtomicCompareExchange(tid,_,a0,a1,a2,a3,a4,a5) -> OpAtomicCompareExchange(tid,id,a0,a1,a2,a3,a4,a5)
                | OpAtomicCompareExchangeWeak(tid,_,a0,a1,a2,a3,a4,a5) -> OpAtomicCompareExchangeWeak(tid,id,a0,a1,a2,a3,a4,a5)
                | OpAtomicIIncrement(tid,_,a0,a1,a2) -> OpAtomicIIncrement(tid,id,a0,a1,a2)
                | OpAtomicIDecrement(tid,_,a0,a1,a2) -> OpAtomicIDecrement(tid,id,a0,a1,a2)
                | OpAtomicIAdd(tid,_,a0,a1,a2,a3) -> OpAtomicIAdd(tid,id,a0,a1,a2,a3)
                | OpAtomicISub(tid,_,a0,a1,a2,a3) -> OpAtomicISub(tid,id,a0,a1,a2,a3)
                | OpAtomicSMin(tid,_,a0,a1,a2,a3) -> OpAtomicSMin(tid,id,a0,a1,a2,a3)
                | OpAtomicUMin(tid,_,a0,a1,a2,a3) -> OpAtomicUMin(tid,id,a0,a1,a2,a3)
                | OpAtomicSMax(tid,_,a0,a1,a2,a3) -> OpAtomicSMax(tid,id,a0,a1,a2,a3)
                | OpAtomicUMax(tid,_,a0,a1,a2,a3) -> OpAtomicUMax(tid,id,a0,a1,a2,a3)
                | OpAtomicAnd(tid,_,a0,a1,a2,a3) -> OpAtomicAnd(tid,id,a0,a1,a2,a3)
                | OpAtomicOr(tid,_,a0,a1,a2,a3) -> OpAtomicOr(tid,id,a0,a1,a2,a3)
                | OpAtomicXor(tid,_,a0,a1,a2,a3) -> OpAtomicXor(tid,id,a0,a1,a2,a3)
                | OpPhi(tid,_,a0) -> OpPhi(tid,id,a0)
                | OpLabel(_) -> OpLabel(id)
                | OpGroupAsyncCopy(tid,_,a0,a1,a2,a3,a4,a5) -> OpGroupAsyncCopy(tid,id,a0,a1,a2,a3,a4,a5)
                | OpGroupAll(tid,_,a0,a1) -> OpGroupAll(tid,id,a0,a1)
                | OpGroupAny(tid,_,a0,a1) -> OpGroupAny(tid,id,a0,a1)
                | OpGroupBroadcast(tid,_,a0,a1,a2) -> OpGroupBroadcast(tid,id,a0,a1,a2)
                | OpGroupIAdd(tid,_,a0,a1,a2) -> OpGroupIAdd(tid,id,a0,a1,a2)
                | OpGroupFAdd(tid,_,a0,a1,a2) -> OpGroupFAdd(tid,id,a0,a1,a2)
                | OpGroupFMin(tid,_,a0,a1,a2) -> OpGroupFMin(tid,id,a0,a1,a2)
                | OpGroupUMin(tid,_,a0,a1,a2) -> OpGroupUMin(tid,id,a0,a1,a2)
                | OpGroupSMin(tid,_,a0,a1,a2) -> OpGroupSMin(tid,id,a0,a1,a2)
                | OpGroupFMax(tid,_,a0,a1,a2) -> OpGroupFMax(tid,id,a0,a1,a2)
                | OpGroupUMax(tid,_,a0,a1,a2) -> OpGroupUMax(tid,id,a0,a1,a2)
                | OpGroupSMax(tid,_,a0,a1,a2) -> OpGroupSMax(tid,id,a0,a1,a2)
                | OpReadPipe(tid,_,a0,a1,a2,a3) -> OpReadPipe(tid,id,a0,a1,a2,a3)
                | OpWritePipe(tid,_,a0,a1,a2,a3) -> OpWritePipe(tid,id,a0,a1,a2,a3)
                | OpReservedReadPipe(tid,_,a0,a1,a2,a3,a4,a5) -> OpReservedReadPipe(tid,id,a0,a1,a2,a3,a4,a5)
                | OpReservedWritePipe(tid,_,a0,a1,a2,a3,a4,a5) -> OpReservedWritePipe(tid,id,a0,a1,a2,a3,a4,a5)
                | OpReserveReadPipePackets(tid,_,a0,a1,a2,a3) -> OpReserveReadPipePackets(tid,id,a0,a1,a2,a3)
                | OpReserveWritePipePackets(tid,_,a0,a1,a2,a3) -> OpReserveWritePipePackets(tid,id,a0,a1,a2,a3)
                | OpIsValidReserveId(tid,_,a0) -> OpIsValidReserveId(tid,id,a0)
                | OpGetNumPipePackets(tid,_,a0,a1,a2) -> OpGetNumPipePackets(tid,id,a0,a1,a2)
                | OpGetMaxPipePackets(tid,_,a0,a1,a2) -> OpGetMaxPipePackets(tid,id,a0,a1,a2)
                | OpGroupReserveReadPipePackets(tid,_,a0,a1,a2,a3,a4) -> OpGroupReserveReadPipePackets(tid,id,a0,a1,a2,a3,a4)
                | OpGroupReserveWritePipePackets(tid,_,a0,a1,a2,a3,a4) -> OpGroupReserveWritePipePackets(tid,id,a0,a1,a2,a3,a4)
                | OpEnqueueMarker(tid,_,a0,a1,a2,a3) -> OpEnqueueMarker(tid,id,a0,a1,a2,a3)
                | OpEnqueueKernel(tid,_,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) -> OpEnqueueKernel(tid,id,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
                | OpGetKernelNDrangeSubGroupCount(tid,_,a0,a1,a2,a3,a4) -> OpGetKernelNDrangeSubGroupCount(tid,id,a0,a1,a2,a3,a4)
                | OpGetKernelNDrangeMaxSubGroupSize(tid,_,a0,a1,a2,a3,a4) -> OpGetKernelNDrangeMaxSubGroupSize(tid,id,a0,a1,a2,a3,a4)
                | OpGetKernelWorkGroupSize(tid,_,a0,a1,a2,a3) -> OpGetKernelWorkGroupSize(tid,id,a0,a1,a2,a3)
                | OpGetKernelPreferredWorkGroupSizeMultiple(tid,_,a0,a1,a2,a3) -> OpGetKernelPreferredWorkGroupSizeMultiple(tid,id,a0,a1,a2,a3)
                | OpCreateUserEvent(tid,_) -> OpCreateUserEvent(tid,id)
                | OpIsValidEvent(tid,_,a0) -> OpIsValidEvent(tid,id,a0)
                | OpGetDefaultQueue(tid,_) -> OpGetDefaultQueue(tid,id)
                | OpBuildNDRange(tid,_,a0,a1,a2) -> OpBuildNDRange(tid,id,a0,a1,a2)
                | OpImageSparseSampleImplicitLod(tid,_,a0,a1,a2,a3) -> OpImageSparseSampleImplicitLod(tid,id,a0,a1,a2,a3)
                | OpImageSparseSampleExplicitLod(tid,_,a0,a1,a2,a3) -> OpImageSparseSampleExplicitLod(tid,id,a0,a1,a2,a3)
                | OpImageSparseSampleDrefImplicitLod(tid,_,a0,a1,a2,a3,a4) -> OpImageSparseSampleDrefImplicitLod(tid,id,a0,a1,a2,a3,a4)
                | OpImageSparseSampleDrefExplicitLod(tid,_,a0,a1,a2,a3,a4) -> OpImageSparseSampleDrefExplicitLod(tid,id,a0,a1,a2,a3,a4)
                | OpImageSparseSampleProjImplicitLod(tid,_,a0,a1,a2,a3) -> OpImageSparseSampleProjImplicitLod(tid,id,a0,a1,a2,a3)
                | OpImageSparseSampleProjExplicitLod(tid,_,a0,a1,a2,a3) -> OpImageSparseSampleProjExplicitLod(tid,id,a0,a1,a2,a3)
                | OpImageSparseSampleProjDrefImplicitLod(tid,_,a0,a1,a2,a3,a4) -> OpImageSparseSampleProjDrefImplicitLod(tid,id,a0,a1,a2,a3,a4)
                | OpImageSparseSampleProjDrefExplicitLod(tid,_,a0,a1,a2,a3,a4) -> OpImageSparseSampleProjDrefExplicitLod(tid,id,a0,a1,a2,a3,a4)
                | OpImageSparseFetch(tid,_,a0,a1,a2,a3) -> OpImageSparseFetch(tid,id,a0,a1,a2,a3)
                | OpImageSparseGather(tid,_,a0,a1,a2,a3,a4) -> OpImageSparseGather(tid,id,a0,a1,a2,a3,a4)
                | OpImageSparseDrefGather(tid,_,a0,a1,a2,a3,a4) -> OpImageSparseDrefGather(tid,id,a0,a1,a2,a3,a4)
                | OpImageSparseTexelsResident(tid,_,a0) -> OpImageSparseTexelsResident(tid,id,a0)
                | OpAtomicFlagTestAndSet(tid,_,a0,a1,a2) -> OpAtomicFlagTestAndSet(tid,id,a0,a1,a2)
                | _ -> x


        member x.Operands =
            match x with
                | OpUndef(_,_) -> []
                | OpSourceContinued(sourceCont) -> [sourceCont :> obj]
                | OpSource(_source,version,code) -> [_source :> obj; version :> obj; code :> obj]
                | OpSourceExtension(extension) -> [extension :> obj]
                | OpName(_target,name) -> [_target :> obj; name :> obj]
                | OpMemberName(_type,_member,name) -> [_type :> obj; _member :> obj; name :> obj]
                | OpString(_,value) -> [value :> obj]
                | OpLine(file,line,column) -> [file :> obj; line :> obj; column :> obj]
                | OpExtension(name) -> [name :> obj]
                | OpExtInstImport(_,name) -> [name :> obj]
                | OpExtInst(_,_,set,instruction,operands) -> [set :> obj; instruction :> obj; operands :> obj]
                | OpMemoryModel(addressing,memory) -> [addressing :> obj; memory :> obj]
                | OpEntryPoint(executionModel,entryPoint,name) -> [executionModel :> obj; entryPoint :> obj; name :> obj]
                | OpExecutionMode(entryPoint,mode,modes) -> [entryPoint :> obj; mode :> obj; modes :> obj]
                | OpCapability(capability) -> [capability :> obj]
                | OpTypeVoid(_) -> []
                | OpTypeBool(_) -> []
                | OpTypeInt(_,width,signedness) -> [width :> obj; signedness :> obj]
                | OpTypeFloat(_,width) -> [width :> obj]
                | OpTypeVector(_,compType,compCount) -> [compType :> obj; compCount :> obj]
                | OpTypeMatrix(_,colType,colCount) -> [colType :> obj; colCount :> obj]
                | OpTypeImage(_,sampledType,dim,depth,arrayed,ms,sampled,format,access) -> [sampledType :> obj; dim :> obj; depth :> obj; arrayed :> obj; ms :> obj; sampled :> obj; format :> obj; access :> obj]
                | OpTypeSampler(_) -> []
                | OpTypeSampledImage(_,imageType) -> [imageType :> obj]
                | OpTypeArray(_,elementType,length) -> [elementType :> obj; length :> obj]
                | OpTypeRuntimeArray(_,elementType) -> [elementType :> obj]
                | OpTypeStruct(_,memberTypes) -> [memberTypes :> obj]
                | OpTypeOpaque(_,opaqueTypeName) -> [opaqueTypeName :> obj]
                | OpTypePointer(_,storage,_type) -> [storage :> obj; _type :> obj]
                | OpTypeFunction(_,retType,paramTypes) -> [retType :> obj; paramTypes :> obj]
                | OpTypeEvent(_) -> []
                | OpTypeDeviceEvent(_) -> []
                | OpTypeReserveId(_) -> []
                | OpTypeQueue(_) -> []
                | OpTypePipe(_,qualifier) -> [qualifier :> obj]
                | OpTypeForwardPointer(ptrType,storage) -> [ptrType :> obj; storage :> obj]
                | OpConstantTrue(_,_) -> []
                | OpConstantFalse(_,_) -> []
                | OpConstant(_,_,value) -> [value :> obj]
                | OpConstantComposite(_,_,constituents) -> [constituents :> obj]
                | OpConstantSampler(_,_,addressingMode,param,filterMode) -> [addressingMode :> obj; param :> obj; filterMode :> obj]
                | OpConstantNull(_,_) -> []
                | OpSpecConstantTrue(_,_) -> []
                | OpSpecConstantFalse(_,_) -> []
                | OpSpecConstant(_,_,value) -> [value :> obj]
                | OpSpecConstantComposite(_,_,constituents) -> [constituents :> obj]
                | OpSpecConstantOp(_,_,opCode,operands) -> [opCode :> obj; operands :> obj]
                | OpFunction(_,_,_function,funType) -> [_function :> obj; funType :> obj]
                | OpFunctionParameter(_,_) -> []
                | OpFunctionCall(_,_,_function,args) -> [_function :> obj; args :> obj]
                | OpVariable(_,_,storage,initializer) -> [storage :> obj; initializer :> obj]
                | OpImageTexelPointer(_,_,image,coord,sample) -> [image :> obj; coord :> obj; sample :> obj]
                | OpLoad(_,_,ptr,access) -> [ptr :> obj; access :> obj]
                | OpStore(ptr,_object,access) -> [ptr :> obj; _object :> obj; access :> obj]
                | OpCopyMemory(_target,_source,access) -> [_target :> obj; _source :> obj; access :> obj]
                | OpCopyMemorySized(_target,_source,size,access) -> [_target :> obj; _source :> obj; size :> obj; access :> obj]
                | OpAccessChain(_,_,_base,indices) -> [_base :> obj; indices :> obj]
                | OpInBoundsAccessChain(_,_,_base,indices) -> [_base :> obj; indices :> obj]
                | OpPtrAccessChain(_,_,_base,element,indices) -> [_base :> obj; element :> obj; indices :> obj]
                | OpArrayLength(_,_,structure,arrMember) -> [structure :> obj; arrMember :> obj]
                | OpGenericPtrMemSemantics(_,_,ptr) -> [ptr :> obj]
                | OpInBoundsPtrAccessChain(_,_,_base,element,indices) -> [_base :> obj; element :> obj; indices :> obj]
                | OpDecorate(_target,decoration,args) -> [_target :> obj; decoration :> obj; args :> obj]
                | OpMemberDecorate(structType,_member,decoration,args) -> [structType :> obj; _member :> obj; decoration :> obj; args :> obj]
                | OpDecorationGroup(_) -> []
                | OpGroupDecorate(group,targets) -> [group :> obj; targets :> obj]
                | OpGroupMemberDecorate(group,targets) -> [group :> obj; targets :> obj]
                | OpVectorExtractDynamic(_,_,vector,index) -> [vector :> obj; index :> obj]
                | OpVectorInsertDynamic(_,_,vector,_component,index) -> [vector :> obj; _component :> obj; index :> obj]
                | OpVectorShuffle(_,_,v0,v1,components) -> [v0 :> obj; v1 :> obj; components :> obj]
                | OpCompositeConstruct(_,_,constituents) -> [constituents :> obj]
                | OpCompositeExtract(_,_,composite,indices) -> [composite :> obj; indices :> obj]
                | OpCompositeInsert(_,_,_object,composite,indices) -> [_object :> obj; composite :> obj; indices :> obj]
                | OpCopyObject(_,_,operand) -> [operand :> obj]
                | OpTranspose(_,_,mat) -> [mat :> obj]
                | OpSampledImage(_,_,image,sampler) -> [image :> obj; sampler :> obj]
                | OpImageSampleImplicitLod(_,_,sampledImage,coord,images,variables) -> [sampledImage :> obj; coord :> obj; images :> obj; variables :> obj]
                | OpImageSampleExplicitLod(_,_,sampledImage,coord,images,variables) -> [sampledImage :> obj; coord :> obj; images :> obj; variables :> obj]
                | OpImageSampleDrefImplicitLod(_,_,sampledImage,coord,depthRef,images,variables) -> [sampledImage :> obj; coord :> obj; depthRef :> obj; images :> obj; variables :> obj]
                | OpImageSampleDrefExplicitLod(_,_,sampledImage,coord,depthRef,images,variables) -> [sampledImage :> obj; coord :> obj; depthRef :> obj; images :> obj; variables :> obj]
                | OpImageSampleProjImplicitLod(_,_,sampledImage,coord,images,variables) -> [sampledImage :> obj; coord :> obj; images :> obj; variables :> obj]
                | OpImageSampleProjExplicitLod(_,_,sampledImage,coord,images,variables) -> [sampledImage :> obj; coord :> obj; images :> obj; variables :> obj]
                | OpImageSampleProjDrefImplicitLod(_,_,sampledImage,coord,depthRef,images,variables) -> [sampledImage :> obj; coord :> obj; depthRef :> obj; images :> obj; variables :> obj]
                | OpImageSampleProjDrefExplicitLod(_,_,sampledImage,coord,depthRef,images,variables) -> [sampledImage :> obj; coord :> obj; depthRef :> obj; images :> obj; variables :> obj]
                | OpImageFetch(_,_,image,coord,images,variables) -> [image :> obj; coord :> obj; images :> obj; variables :> obj]
                | OpImageGather(_,_,sampledImage,coord,_component,images,variables) -> [sampledImage :> obj; coord :> obj; _component :> obj; images :> obj; variables :> obj]
                | OpImageDrefGather(_,_,sampledImage,coord,depthRef,images,variables) -> [sampledImage :> obj; coord :> obj; depthRef :> obj; images :> obj; variables :> obj]
                | OpImageRead(_,_,image,coords,images,variables) -> [image :> obj; coords :> obj; images :> obj; variables :> obj]
                | OpImageWrite(image,coord,texel,images,variables) -> [image :> obj; coord :> obj; texel :> obj; images :> obj; variables :> obj]
                | OpImage(_,_,sampledImage) -> [sampledImage :> obj]
                | OpImageQueryFormat(_,_,image) -> [image :> obj]
                | OpImageQueryOrder(_,_,image) -> [image :> obj]
                | OpImageQuerySizeLod(_,_,image,lod) -> [image :> obj; lod :> obj]
                | OpImageQuerySize(_,_,image) -> [image :> obj]
                | OpImageQueryLod(_,_,image,coord) -> [image :> obj; coord :> obj]
                | OpImageQueryLevels(_,_,image) -> [image :> obj]
                | OpImageQuerySamples(_,_,image) -> [image :> obj]
                | OpConvertFToU(_,_,fvec) -> [fvec :> obj]
                | OpConvertFToS(_,_,floatVal) -> [floatVal :> obj]
                | OpConvertSToF(_,_,sVal) -> [sVal :> obj]
                | OpConvertUToF(_,_,uVal) -> [uVal :> obj]
                | OpUConvert(_,_,uVal) -> [uVal :> obj]
                | OpSConvert(_,_,sVal) -> [sVal :> obj]
                | OpFConvert(_,_,floatVal) -> [floatVal :> obj]
                | OpQuantizeToF16(_,_,value) -> [value :> obj]
                | OpConvertPtrToU(_,_,ptr) -> [ptr :> obj]
                | OpSatConvertSToU(_,_,sVal) -> [sVal :> obj]
                | OpSatConvertUToS(_,_,uVal) -> [uVal :> obj]
                | OpConvertUToPtr(_,_,intVal) -> [intVal :> obj]
                | OpPtrCastToGeneric(_,_,ptr) -> [ptr :> obj]
                | OpGenericCastToPtr(_,_,ptr) -> [ptr :> obj]
                | OpGenericCastToPtrExplicit(_,_,ptr,storage) -> [ptr :> obj; storage :> obj]
                | OpBitcast(_,_,operand) -> [operand :> obj]
                | OpSNegate(_,_,operand) -> [operand :> obj]
                | OpFNegate(_,_,operand) -> [operand :> obj]
                | OpIAdd(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFAdd(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpISub(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFSub(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpIMul(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFMul(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpUDiv(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpSDiv(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFDiv(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpUMod(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpSRem(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpSMod(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFRem(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFMod(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpVectorTimesScalar(_,_,vec,scalar) -> [vec :> obj; scalar :> obj]
                | OpMatrixTimesScalar(_,_,mat,scalar) -> [mat :> obj; scalar :> obj]
                | OpVectorTimesMatrix(_,_,vec,mat) -> [vec :> obj; mat :> obj]
                | OpMatrixTimesVector(_,_,mat,vec) -> [mat :> obj; vec :> obj]
                | OpMatrixTimesMatrix(_,_,left,right) -> [left :> obj; right :> obj]
                | OpOuterProduct(_,_,left,right) -> [left :> obj; right :> obj]
                | OpDot(_,_,left,right) -> [left :> obj; right :> obj]
                | OpIAddCarry(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpISubBorrow(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpUMulExtended(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpSMulExtended(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpAny(_,_,vec) -> [vec :> obj]
                | OpAll(_,_,vec) -> [vec :> obj]
                | OpIsNan(_,_,x) -> [x :> obj]
                | OpIsInf(_,_,y) -> [y :> obj]
                | OpIsFinite(_,_,x) -> [x :> obj]
                | OpIsNormal(_,_,x) -> [x :> obj]
                | OpSignBitSet(_,_,x) -> [x :> obj]
                | OpLessOrGreater(_,_,x,y) -> [x :> obj; y :> obj]
                | OpOrdered(_,_,x,y) -> [x :> obj; y :> obj]
                | OpUnordered(_,_,x,y) -> [x :> obj; y :> obj]
                | OpLogicalEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpLogicalNotEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpLogicalOr(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpLogicalAnd(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpLogicalNot(_,_,op) -> [op :> obj]
                | OpSelect(_,_,cond,o1,o2) -> [cond :> obj; o1 :> obj; o2 :> obj]
                | OpIEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpINotEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpUGreaterThan(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpSGreaterThan(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpUGreaterThanEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpSGreaterThanEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpULessThan(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpSLessThan(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpULessThanEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpSLessThanEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFOrdEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFUnordEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFOrdNotEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFUnordNotEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFOrdLessThan(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFUnordLessThan(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFOrdGreaterThan(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFUnordGreaterThan(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFOrdLessThanEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFUnordLessThanEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFOrdGreaterThanEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpFUnordGreaterThanEqual(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpShiftRightLogical(_,_,_base,shift) -> [_base :> obj; shift :> obj]
                | OpShiftRightArithmetic(_,_,_base,shift) -> [_base :> obj; shift :> obj]
                | OpShiftLeftLogical(_,_,_base,shift) -> [_base :> obj; shift :> obj]
                | OpBitwiseOr(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpBitwiseXor(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpBitwiseAnd(_,_,op1,op2) -> [op1 :> obj; op2 :> obj]
                | OpNot(_,_,operand) -> [operand :> obj]
                | OpBitFieldInsert(_,_,_base,insert,offset,count) -> [_base :> obj; insert :> obj; offset :> obj; count :> obj]
                | OpBitFieldSExtract(_,_,_base,offset,count) -> [_base :> obj; offset :> obj; count :> obj]
                | OpBitFieldUExtract(_,_,_base,offset,count) -> [_base :> obj; offset :> obj; count :> obj]
                | OpBitReverse(_,_,_base) -> [_base :> obj]
                | OpBitCount(_,_,_base) -> [_base :> obj]
                | OpDPdx(_,_,p) -> [p :> obj]
                | OpDPdy(_,_,p) -> [p :> obj]
                | OpFwidth(_,_,p) -> [p :> obj]
                | OpDPdxFine(_,_,p) -> [p :> obj]
                | OpDPdyFine(_,_,p) -> [p :> obj]
                | OpFwidthFine(_,_,p) -> [p :> obj]
                | OpDPdxCoarse(_,_,p) -> [p :> obj]
                | OpDPdyCoarse(_,_,p) -> [p :> obj]
                | OpFwidthCoarse(_,_,p) -> [p :> obj]
                | OpEmitStreamVertex(stream) -> [stream :> obj]
                | OpEndStreamPrimitive(stream) -> [stream :> obj]
                | OpControlBarrier(exec,mem,sem) -> [exec :> obj; mem :> obj; sem :> obj]
                | OpMemoryBarrier(mem,sen) -> [mem :> obj; sen :> obj]
                | OpAtomicLoad(_,_,ptr,scope,sem) -> [ptr :> obj; scope :> obj; sem :> obj]
                | OpAtomicStore(ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpAtomicExchange(_,_,ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpAtomicCompareExchange(_,_,ptr,scope,eq,neq,value,comparator) -> [ptr :> obj; scope :> obj; eq :> obj; neq :> obj; value :> obj; comparator :> obj]
                | OpAtomicCompareExchangeWeak(_,_,ptr,scope,eq,neq,value,comparator) -> [ptr :> obj; scope :> obj; eq :> obj; neq :> obj; value :> obj; comparator :> obj]
                | OpAtomicIIncrement(_,_,ptr,scope,sem) -> [ptr :> obj; scope :> obj; sem :> obj]
                | OpAtomicIDecrement(_,_,ptr,scope,sem) -> [ptr :> obj; scope :> obj; sem :> obj]
                | OpAtomicIAdd(_,_,ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpAtomicISub(_,_,ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpAtomicSMin(_,_,ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpAtomicUMin(_,_,ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpAtomicSMax(_,_,ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpAtomicUMax(_,_,ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpAtomicAnd(_,_,ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpAtomicOr(_,_,ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpAtomicXor(_,_,ptr,scope,sem,value) -> [ptr :> obj; scope :> obj; sem :> obj; value :> obj]
                | OpPhi(_,_,varsAndParents) -> [varsAndParents :> obj]
                | OpLoopMerge(mergeBlock,contTarget,loop) -> [mergeBlock :> obj; contTarget :> obj; loop :> obj]
                | OpSelectionMerge(mergeBlock,select) -> [mergeBlock :> obj; select :> obj]
                | OpLabel(_) -> []
                | OpBranch(_target) -> [_target :> obj]
                | OpBranchConditional(condition,lTrue,lFalse,weights) -> [condition :> obj; lTrue :> obj; lFalse :> obj; weights :> obj]
                | OpSwitch(sel,def,_target) -> [sel :> obj; def :> obj; _target :> obj]
                | OpReturnValue(value) -> [value :> obj]
                | OpLifetimeStart(ptr,size) -> [ptr :> obj; size :> obj]
                | OpLifetimeStop(ptr,size) -> [ptr :> obj; size :> obj]
                | OpGroupAsyncCopy(_,_,execution,dest,_source,num,stride,evt) -> [execution :> obj; dest :> obj; _source :> obj; num :> obj; stride :> obj; evt :> obj]
                | OpGroupWaitEvents(exec,num,evtList) -> [exec :> obj; num :> obj; evtList :> obj]
                | OpGroupAll(_,_,exec,pred) -> [exec :> obj; pred :> obj]
                | OpGroupAny(_,_,exec,pred) -> [exec :> obj; pred :> obj]
                | OpGroupBroadcast(_,_,exec,value,localid) -> [exec :> obj; value :> obj; localid :> obj]
                | OpGroupIAdd(_,_,exec,operation,x) -> [exec :> obj; operation :> obj; x :> obj]
                | OpGroupFAdd(_,_,exec,operation,x) -> [exec :> obj; operation :> obj; x :> obj]
                | OpGroupFMin(_,_,exec,operation,x) -> [exec :> obj; operation :> obj; x :> obj]
                | OpGroupUMin(_,_,exec,operation,x) -> [exec :> obj; operation :> obj; x :> obj]
                | OpGroupSMin(_,_,exec,operation,x) -> [exec :> obj; operation :> obj; x :> obj]
                | OpGroupFMax(_,_,exec,operation,x) -> [exec :> obj; operation :> obj; x :> obj]
                | OpGroupUMax(_,_,exec,operation,x) -> [exec :> obj; operation :> obj; x :> obj]
                | OpGroupSMax(_,_,exec,operation,x) -> [exec :> obj; operation :> obj; x :> obj]
                | OpReadPipe(_,_,pipe,ptr,packetSize,packetAlign) -> [pipe :> obj; ptr :> obj; packetSize :> obj; packetAlign :> obj]
                | OpWritePipe(_,_,pipe,ptr,packetSize,packetAlign) -> [pipe :> obj; ptr :> obj; packetSize :> obj; packetAlign :> obj]
                | OpReservedReadPipe(_,_,pipe,reserveId,index,ptr,packetSize,packetAlign) -> [pipe :> obj; reserveId :> obj; index :> obj; ptr :> obj; packetSize :> obj; packetAlign :> obj]
                | OpReservedWritePipe(_,_,pipe,reserveId,index,ptr,packetSize,packetAlign) -> [pipe :> obj; reserveId :> obj; index :> obj; ptr :> obj; packetSize :> obj; packetAlign :> obj]
                | OpReserveReadPipePackets(_,_,pipe,numPackets,packetSize,packetAlign) -> [pipe :> obj; numPackets :> obj; packetSize :> obj; packetAlign :> obj]
                | OpReserveWritePipePackets(_,_,pipe,numPackets,packetSize,packetAlign) -> [pipe :> obj; numPackets :> obj; packetSize :> obj; packetAlign :> obj]
                | OpCommitReadPipe(pipe,reserveId,packetSize,packetAlign) -> [pipe :> obj; reserveId :> obj; packetSize :> obj; packetAlign :> obj]
                | OpCommitWritePipe(pipe,reserveId,packetSize,packetAlign) -> [pipe :> obj; reserveId :> obj; packetSize :> obj; packetAlign :> obj]
                | OpIsValidReserveId(_,_,reserveId) -> [reserveId :> obj]
                | OpGetNumPipePackets(_,_,pipe,packetSize,packetAlign) -> [pipe :> obj; packetSize :> obj; packetAlign :> obj]
                | OpGetMaxPipePackets(_,_,pipe,packetSize,packetAlign) -> [pipe :> obj; packetSize :> obj; packetAlign :> obj]
                | OpGroupReserveReadPipePackets(_,_,exec,pipe,numPackets,packetSize,packetAlign) -> [exec :> obj; pipe :> obj; numPackets :> obj; packetSize :> obj; packetAlign :> obj]
                | OpGroupReserveWritePipePackets(_,_,exec,pipe,numPackets,packetSize,packetAlign) -> [exec :> obj; pipe :> obj; numPackets :> obj; packetSize :> obj; packetAlign :> obj]
                | OpGroupCommitReadPipe(exec,pipe,reserveId,packetSize,packetAlign) -> [exec :> obj; pipe :> obj; reserveId :> obj; packetSize :> obj; packetAlign :> obj]
                | OpGroupCommitWritePipe(exec,pipe,reserveId,packetSize,packetAlign) -> [exec :> obj; pipe :> obj; reserveId :> obj; packetSize :> obj; packetAlign :> obj]
                | OpEnqueueMarker(_,_,queue,nEvts,waitEvts,retEvt) -> [queue :> obj; nEvts :> obj; waitEvts :> obj; retEvt :> obj]
                | OpEnqueueKernel(_,_,queue,flags,ndRange,nEvents,waitEvts,retEvt,invoke,param,paramSize,paramAlign,localSize) -> [queue :> obj; flags :> obj; ndRange :> obj; nEvents :> obj; waitEvts :> obj; retEvt :> obj; invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj; localSize :> obj]
                | OpGetKernelNDrangeSubGroupCount(_,_,ndRange,invoke,param,paramSize,paramAlign) -> [ndRange :> obj; invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj]
                | OpGetKernelNDrangeMaxSubGroupSize(_,_,ndRange,invoke,param,paramSize,paramAlign) -> [ndRange :> obj; invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj]
                | OpGetKernelWorkGroupSize(_,_,invoke,param,paramSize,paramAlign) -> [invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj]
                | OpGetKernelPreferredWorkGroupSizeMultiple(_,_,invoke,param,paramSize,paramAlign) -> [invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj]
                | OpRetainEvent(evt) -> [evt :> obj]
                | OpReleaseEvent(evt) -> [evt :> obj]
                | OpCreateUserEvent(_,_) -> []
                | OpIsValidEvent(_,_,evt) -> [evt :> obj]
                | OpSetUserEventStatus(evt,status) -> [evt :> obj; status :> obj]
                | OpCaptureEventProfilingInfo(evt,profileInfo,value) -> [evt :> obj; profileInfo :> obj; value :> obj]
                | OpGetDefaultQueue(_,_) -> []
                | OpBuildNDRange(_,_,globalSize,localSize,globalOffset) -> [globalSize :> obj; localSize :> obj; globalOffset :> obj]
                | OpImageSparseSampleImplicitLod(_,_,sampledImage,coord,images,variables) -> [sampledImage :> obj; coord :> obj; images :> obj; variables :> obj]
                | OpImageSparseSampleExplicitLod(_,_,sampledImage,coord,images,variables) -> [sampledImage :> obj; coord :> obj; images :> obj; variables :> obj]
                | OpImageSparseSampleDrefImplicitLod(_,_,sampledImage,coord,depthRef,images,variables) -> [sampledImage :> obj; coord :> obj; depthRef :> obj; images :> obj; variables :> obj]
                | OpImageSparseSampleDrefExplicitLod(_,_,sampledImage,coord,depthRef,images,variables) -> [sampledImage :> obj; coord :> obj; depthRef :> obj; images :> obj; variables :> obj]
                | OpImageSparseSampleProjImplicitLod(_,_,sampledImage,coord,images,variables) -> [sampledImage :> obj; coord :> obj; images :> obj; variables :> obj]
                | OpImageSparseSampleProjExplicitLod(_,_,sampledImage,coord,images,variables) -> [sampledImage :> obj; coord :> obj; images :> obj; variables :> obj]
                | OpImageSparseSampleProjDrefImplicitLod(_,_,sampledImage,coord,depthRef,images,variables) -> [sampledImage :> obj; coord :> obj; depthRef :> obj; images :> obj; variables :> obj]
                | OpImageSparseSampleProjDrefExplicitLod(_,_,sampledImage,coord,depthRef,images,variables) -> [sampledImage :> obj; coord :> obj; depthRef :> obj; images :> obj; variables :> obj]
                | OpImageSparseFetch(_,_,image,coord,images,variables) -> [image :> obj; coord :> obj; images :> obj; variables :> obj]
                | OpImageSparseGather(_,_,sampledImage,coord,_component,images,variables) -> [sampledImage :> obj; coord :> obj; _component :> obj; images :> obj; variables :> obj]
                | OpImageSparseDrefGather(_,_,sampledImage,coord,depthRef,images,variables) -> [sampledImage :> obj; coord :> obj; depthRef :> obj; images :> obj; variables :> obj]
                | OpImageSparseTexelsResident(_,_,residentCode) -> [residentCode :> obj]
                | OpAtomicFlagTestAndSet(_,_,ptr,scope,sem) -> [ptr :> obj; scope :> obj; sem :> obj]
                | OpAtomicFlagClear(ptr,scope,sem) -> [ptr :> obj; scope :> obj; sem :> obj]
                | _ -> []


        member x.Name =
            match x with
                | OpNop -> "OpNop"
                | OpUndef(_) -> "OpUndef"
                | OpSourceContinued(_) -> "OpSourceContinued"
                | OpSource(_) -> "OpSource"
                | OpSourceExtension(_) -> "OpSourceExtension"
                | OpName(_) -> "OpName"
                | OpMemberName(_) -> "OpMemberName"
                | OpString(_) -> "OpString"
                | OpLine(_) -> "OpLine"
                | OpExtension(_) -> "OpExtension"
                | OpExtInstImport(_) -> "OpExtInstImport"
                | OpExtInst(_) -> "OpExtInst"
                | OpMemoryModel(_) -> "OpMemoryModel"
                | OpEntryPoint(_) -> "OpEntryPoint"
                | OpExecutionMode(_) -> "OpExecutionMode"
                | OpCapability(_) -> "OpCapability"
                | OpTypeVoid(_) -> "OpTypeVoid"
                | OpTypeBool(_) -> "OpTypeBool"
                | OpTypeInt(_) -> "OpTypeInt"
                | OpTypeFloat(_) -> "OpTypeFloat"
                | OpTypeVector(_) -> "OpTypeVector"
                | OpTypeMatrix(_) -> "OpTypeMatrix"
                | OpTypeImage(_) -> "OpTypeImage"
                | OpTypeSampler(_) -> "OpTypeSampler"
                | OpTypeSampledImage(_) -> "OpTypeSampledImage"
                | OpTypeArray(_) -> "OpTypeArray"
                | OpTypeRuntimeArray(_) -> "OpTypeRuntimeArray"
                | OpTypeStruct(_) -> "OpTypeStruct"
                | OpTypeOpaque(_) -> "OpTypeOpaque"
                | OpTypePointer(_) -> "OpTypePointer"
                | OpTypeFunction(_) -> "OpTypeFunction"
                | OpTypeEvent(_) -> "OpTypeEvent"
                | OpTypeDeviceEvent(_) -> "OpTypeDeviceEvent"
                | OpTypeReserveId(_) -> "OpTypeReserveId"
                | OpTypeQueue(_) -> "OpTypeQueue"
                | OpTypePipe(_) -> "OpTypePipe"
                | OpTypeForwardPointer(_) -> "OpTypeForwardPointer"
                | OpConstantTrue(_) -> "OpConstantTrue"
                | OpConstantFalse(_) -> "OpConstantFalse"
                | OpConstant(_) -> "OpConstant"
                | OpConstantComposite(_) -> "OpConstantComposite"
                | OpConstantSampler(_) -> "OpConstantSampler"
                | OpConstantNull(_) -> "OpConstantNull"
                | OpSpecConstantTrue(_) -> "OpSpecConstantTrue"
                | OpSpecConstantFalse(_) -> "OpSpecConstantFalse"
                | OpSpecConstant(_) -> "OpSpecConstant"
                | OpSpecConstantComposite(_) -> "OpSpecConstantComposite"
                | OpSpecConstantOp(_) -> "OpSpecConstantOp"
                | OpFunction(_) -> "OpFunction"
                | OpFunctionParameter(_) -> "OpFunctionParameter"
                | OpFunctionEnd -> "OpFunctionEnd"
                | OpFunctionCall(_) -> "OpFunctionCall"
                | OpVariable(_) -> "OpVariable"
                | OpImageTexelPointer(_) -> "OpImageTexelPointer"
                | OpLoad(_) -> "OpLoad"
                | OpStore(_) -> "OpStore"
                | OpCopyMemory(_) -> "OpCopyMemory"
                | OpCopyMemorySized(_) -> "OpCopyMemorySized"
                | OpAccessChain(_) -> "OpAccessChain"
                | OpInBoundsAccessChain(_) -> "OpInBoundsAccessChain"
                | OpPtrAccessChain(_) -> "OpPtrAccessChain"
                | OpArrayLength(_) -> "OpArrayLength"
                | OpGenericPtrMemSemantics(_) -> "OpGenericPtrMemSemantics"
                | OpInBoundsPtrAccessChain(_) -> "OpInBoundsPtrAccessChain"
                | OpDecorate(_) -> "OpDecorate"
                | OpMemberDecorate(_) -> "OpMemberDecorate"
                | OpDecorationGroup(_) -> "OpDecorationGroup"
                | OpGroupDecorate(_) -> "OpGroupDecorate"
                | OpGroupMemberDecorate(_) -> "OpGroupMemberDecorate"
                | OpVectorExtractDynamic(_) -> "OpVectorExtractDynamic"
                | OpVectorInsertDynamic(_) -> "OpVectorInsertDynamic"
                | OpVectorShuffle(_) -> "OpVectorShuffle"
                | OpCompositeConstruct(_) -> "OpCompositeConstruct"
                | OpCompositeExtract(_) -> "OpCompositeExtract"
                | OpCompositeInsert(_) -> "OpCompositeInsert"
                | OpCopyObject(_) -> "OpCopyObject"
                | OpTranspose(_) -> "OpTranspose"
                | OpSampledImage(_) -> "OpSampledImage"
                | OpImageSampleImplicitLod(_) -> "OpImageSampleImplicitLod"
                | OpImageSampleExplicitLod(_) -> "OpImageSampleExplicitLod"
                | OpImageSampleDrefImplicitLod(_) -> "OpImageSampleDrefImplicitLod"
                | OpImageSampleDrefExplicitLod(_) -> "OpImageSampleDrefExplicitLod"
                | OpImageSampleProjImplicitLod(_) -> "OpImageSampleProjImplicitLod"
                | OpImageSampleProjExplicitLod(_) -> "OpImageSampleProjExplicitLod"
                | OpImageSampleProjDrefImplicitLod(_) -> "OpImageSampleProjDrefImplicitLod"
                | OpImageSampleProjDrefExplicitLod(_) -> "OpImageSampleProjDrefExplicitLod"
                | OpImageFetch(_) -> "OpImageFetch"
                | OpImageGather(_) -> "OpImageGather"
                | OpImageDrefGather(_) -> "OpImageDrefGather"
                | OpImageRead(_) -> "OpImageRead"
                | OpImageWrite(_) -> "OpImageWrite"
                | OpImage(_) -> "OpImage"
                | OpImageQueryFormat(_) -> "OpImageQueryFormat"
                | OpImageQueryOrder(_) -> "OpImageQueryOrder"
                | OpImageQuerySizeLod(_) -> "OpImageQuerySizeLod"
                | OpImageQuerySize(_) -> "OpImageQuerySize"
                | OpImageQueryLod(_) -> "OpImageQueryLod"
                | OpImageQueryLevels(_) -> "OpImageQueryLevels"
                | OpImageQuerySamples(_) -> "OpImageQuerySamples"
                | OpConvertFToU(_) -> "OpConvertFToU"
                | OpConvertFToS(_) -> "OpConvertFToS"
                | OpConvertSToF(_) -> "OpConvertSToF"
                | OpConvertUToF(_) -> "OpConvertUToF"
                | OpUConvert(_) -> "OpUConvert"
                | OpSConvert(_) -> "OpSConvert"
                | OpFConvert(_) -> "OpFConvert"
                | OpQuantizeToF16(_) -> "OpQuantizeToF16"
                | OpConvertPtrToU(_) -> "OpConvertPtrToU"
                | OpSatConvertSToU(_) -> "OpSatConvertSToU"
                | OpSatConvertUToS(_) -> "OpSatConvertUToS"
                | OpConvertUToPtr(_) -> "OpConvertUToPtr"
                | OpPtrCastToGeneric(_) -> "OpPtrCastToGeneric"
                | OpGenericCastToPtr(_) -> "OpGenericCastToPtr"
                | OpGenericCastToPtrExplicit(_) -> "OpGenericCastToPtrExplicit"
                | OpBitcast(_) -> "OpBitcast"
                | OpSNegate(_) -> "OpSNegate"
                | OpFNegate(_) -> "OpFNegate"
                | OpIAdd(_) -> "OpIAdd"
                | OpFAdd(_) -> "OpFAdd"
                | OpISub(_) -> "OpISub"
                | OpFSub(_) -> "OpFSub"
                | OpIMul(_) -> "OpIMul"
                | OpFMul(_) -> "OpFMul"
                | OpUDiv(_) -> "OpUDiv"
                | OpSDiv(_) -> "OpSDiv"
                | OpFDiv(_) -> "OpFDiv"
                | OpUMod(_) -> "OpUMod"
                | OpSRem(_) -> "OpSRem"
                | OpSMod(_) -> "OpSMod"
                | OpFRem(_) -> "OpFRem"
                | OpFMod(_) -> "OpFMod"
                | OpVectorTimesScalar(_) -> "OpVectorTimesScalar"
                | OpMatrixTimesScalar(_) -> "OpMatrixTimesScalar"
                | OpVectorTimesMatrix(_) -> "OpVectorTimesMatrix"
                | OpMatrixTimesVector(_) -> "OpMatrixTimesVector"
                | OpMatrixTimesMatrix(_) -> "OpMatrixTimesMatrix"
                | OpOuterProduct(_) -> "OpOuterProduct"
                | OpDot(_) -> "OpDot"
                | OpIAddCarry(_) -> "OpIAddCarry"
                | OpISubBorrow(_) -> "OpISubBorrow"
                | OpUMulExtended(_) -> "OpUMulExtended"
                | OpSMulExtended(_) -> "OpSMulExtended"
                | OpAny(_) -> "OpAny"
                | OpAll(_) -> "OpAll"
                | OpIsNan(_) -> "OpIsNan"
                | OpIsInf(_) -> "OpIsInf"
                | OpIsFinite(_) -> "OpIsFinite"
                | OpIsNormal(_) -> "OpIsNormal"
                | OpSignBitSet(_) -> "OpSignBitSet"
                | OpLessOrGreater(_) -> "OpLessOrGreater"
                | OpOrdered(_) -> "OpOrdered"
                | OpUnordered(_) -> "OpUnordered"
                | OpLogicalEqual(_) -> "OpLogicalEqual"
                | OpLogicalNotEqual(_) -> "OpLogicalNotEqual"
                | OpLogicalOr(_) -> "OpLogicalOr"
                | OpLogicalAnd(_) -> "OpLogicalAnd"
                | OpLogicalNot(_) -> "OpLogicalNot"
                | OpSelect(_) -> "OpSelect"
                | OpIEqual(_) -> "OpIEqual"
                | OpINotEqual(_) -> "OpINotEqual"
                | OpUGreaterThan(_) -> "OpUGreaterThan"
                | OpSGreaterThan(_) -> "OpSGreaterThan"
                | OpUGreaterThanEqual(_) -> "OpUGreaterThanEqual"
                | OpSGreaterThanEqual(_) -> "OpSGreaterThanEqual"
                | OpULessThan(_) -> "OpULessThan"
                | OpSLessThan(_) -> "OpSLessThan"
                | OpULessThanEqual(_) -> "OpULessThanEqual"
                | OpSLessThanEqual(_) -> "OpSLessThanEqual"
                | OpFOrdEqual(_) -> "OpFOrdEqual"
                | OpFUnordEqual(_) -> "OpFUnordEqual"
                | OpFOrdNotEqual(_) -> "OpFOrdNotEqual"
                | OpFUnordNotEqual(_) -> "OpFUnordNotEqual"
                | OpFOrdLessThan(_) -> "OpFOrdLessThan"
                | OpFUnordLessThan(_) -> "OpFUnordLessThan"
                | OpFOrdGreaterThan(_) -> "OpFOrdGreaterThan"
                | OpFUnordGreaterThan(_) -> "OpFUnordGreaterThan"
                | OpFOrdLessThanEqual(_) -> "OpFOrdLessThanEqual"
                | OpFUnordLessThanEqual(_) -> "OpFUnordLessThanEqual"
                | OpFOrdGreaterThanEqual(_) -> "OpFOrdGreaterThanEqual"
                | OpFUnordGreaterThanEqual(_) -> "OpFUnordGreaterThanEqual"
                | OpShiftRightLogical(_) -> "OpShiftRightLogical"
                | OpShiftRightArithmetic(_) -> "OpShiftRightArithmetic"
                | OpShiftLeftLogical(_) -> "OpShiftLeftLogical"
                | OpBitwiseOr(_) -> "OpBitwiseOr"
                | OpBitwiseXor(_) -> "OpBitwiseXor"
                | OpBitwiseAnd(_) -> "OpBitwiseAnd"
                | OpNot(_) -> "OpNot"
                | OpBitFieldInsert(_) -> "OpBitFieldInsert"
                | OpBitFieldSExtract(_) -> "OpBitFieldSExtract"
                | OpBitFieldUExtract(_) -> "OpBitFieldUExtract"
                | OpBitReverse(_) -> "OpBitReverse"
                | OpBitCount(_) -> "OpBitCount"
                | OpDPdx(_) -> "OpDPdx"
                | OpDPdy(_) -> "OpDPdy"
                | OpFwidth(_) -> "OpFwidth"
                | OpDPdxFine(_) -> "OpDPdxFine"
                | OpDPdyFine(_) -> "OpDPdyFine"
                | OpFwidthFine(_) -> "OpFwidthFine"
                | OpDPdxCoarse(_) -> "OpDPdxCoarse"
                | OpDPdyCoarse(_) -> "OpDPdyCoarse"
                | OpFwidthCoarse(_) -> "OpFwidthCoarse"
                | OpEmitVertex -> "OpEmitVertex"
                | OpEndPrimitive -> "OpEndPrimitive"
                | OpEmitStreamVertex(_) -> "OpEmitStreamVertex"
                | OpEndStreamPrimitive(_) -> "OpEndStreamPrimitive"
                | OpControlBarrier(_) -> "OpControlBarrier"
                | OpMemoryBarrier(_) -> "OpMemoryBarrier"
                | OpAtomicLoad(_) -> "OpAtomicLoad"
                | OpAtomicStore(_) -> "OpAtomicStore"
                | OpAtomicExchange(_) -> "OpAtomicExchange"
                | OpAtomicCompareExchange(_) -> "OpAtomicCompareExchange"
                | OpAtomicCompareExchangeWeak(_) -> "OpAtomicCompareExchangeWeak"
                | OpAtomicIIncrement(_) -> "OpAtomicIIncrement"
                | OpAtomicIDecrement(_) -> "OpAtomicIDecrement"
                | OpAtomicIAdd(_) -> "OpAtomicIAdd"
                | OpAtomicISub(_) -> "OpAtomicISub"
                | OpAtomicSMin(_) -> "OpAtomicSMin"
                | OpAtomicUMin(_) -> "OpAtomicUMin"
                | OpAtomicSMax(_) -> "OpAtomicSMax"
                | OpAtomicUMax(_) -> "OpAtomicUMax"
                | OpAtomicAnd(_) -> "OpAtomicAnd"
                | OpAtomicOr(_) -> "OpAtomicOr"
                | OpAtomicXor(_) -> "OpAtomicXor"
                | OpPhi(_) -> "OpPhi"
                | OpLoopMerge(_) -> "OpLoopMerge"
                | OpSelectionMerge(_) -> "OpSelectionMerge"
                | OpLabel(_) -> "OpLabel"
                | OpBranch(_) -> "OpBranch"
                | OpBranchConditional(_) -> "OpBranchConditional"
                | OpSwitch(_) -> "OpSwitch"
                | OpKill -> "OpKill"
                | OpReturn -> "OpReturn"
                | OpReturnValue(_) -> "OpReturnValue"
                | OpUnreachable -> "OpUnreachable"
                | OpLifetimeStart(_) -> "OpLifetimeStart"
                | OpLifetimeStop(_) -> "OpLifetimeStop"
                | OpGroupAsyncCopy(_) -> "OpGroupAsyncCopy"
                | OpGroupWaitEvents(_) -> "OpGroupWaitEvents"
                | OpGroupAll(_) -> "OpGroupAll"
                | OpGroupAny(_) -> "OpGroupAny"
                | OpGroupBroadcast(_) -> "OpGroupBroadcast"
                | OpGroupIAdd(_) -> "OpGroupIAdd"
                | OpGroupFAdd(_) -> "OpGroupFAdd"
                | OpGroupFMin(_) -> "OpGroupFMin"
                | OpGroupUMin(_) -> "OpGroupUMin"
                | OpGroupSMin(_) -> "OpGroupSMin"
                | OpGroupFMax(_) -> "OpGroupFMax"
                | OpGroupUMax(_) -> "OpGroupUMax"
                | OpGroupSMax(_) -> "OpGroupSMax"
                | OpReadPipe(_) -> "OpReadPipe"
                | OpWritePipe(_) -> "OpWritePipe"
                | OpReservedReadPipe(_) -> "OpReservedReadPipe"
                | OpReservedWritePipe(_) -> "OpReservedWritePipe"
                | OpReserveReadPipePackets(_) -> "OpReserveReadPipePackets"
                | OpReserveWritePipePackets(_) -> "OpReserveWritePipePackets"
                | OpCommitReadPipe(_) -> "OpCommitReadPipe"
                | OpCommitWritePipe(_) -> "OpCommitWritePipe"
                | OpIsValidReserveId(_) -> "OpIsValidReserveId"
                | OpGetNumPipePackets(_) -> "OpGetNumPipePackets"
                | OpGetMaxPipePackets(_) -> "OpGetMaxPipePackets"
                | OpGroupReserveReadPipePackets(_) -> "OpGroupReserveReadPipePackets"
                | OpGroupReserveWritePipePackets(_) -> "OpGroupReserveWritePipePackets"
                | OpGroupCommitReadPipe(_) -> "OpGroupCommitReadPipe"
                | OpGroupCommitWritePipe(_) -> "OpGroupCommitWritePipe"
                | OpEnqueueMarker(_) -> "OpEnqueueMarker"
                | OpEnqueueKernel(_) -> "OpEnqueueKernel"
                | OpGetKernelNDrangeSubGroupCount(_) -> "OpGetKernelNDrangeSubGroupCount"
                | OpGetKernelNDrangeMaxSubGroupSize(_) -> "OpGetKernelNDrangeMaxSubGroupSize"
                | OpGetKernelWorkGroupSize(_) -> "OpGetKernelWorkGroupSize"
                | OpGetKernelPreferredWorkGroupSizeMultiple(_) -> "OpGetKernelPreferredWorkGroupSizeMultiple"
                | OpRetainEvent(_) -> "OpRetainEvent"
                | OpReleaseEvent(_) -> "OpReleaseEvent"
                | OpCreateUserEvent(_) -> "OpCreateUserEvent"
                | OpIsValidEvent(_) -> "OpIsValidEvent"
                | OpSetUserEventStatus(_) -> "OpSetUserEventStatus"
                | OpCaptureEventProfilingInfo(_) -> "OpCaptureEventProfilingInfo"
                | OpGetDefaultQueue(_) -> "OpGetDefaultQueue"
                | OpBuildNDRange(_) -> "OpBuildNDRange"
                | OpImageSparseSampleImplicitLod(_) -> "OpImageSparseSampleImplicitLod"
                | OpImageSparseSampleExplicitLod(_) -> "OpImageSparseSampleExplicitLod"
                | OpImageSparseSampleDrefImplicitLod(_) -> "OpImageSparseSampleDrefImplicitLod"
                | OpImageSparseSampleDrefExplicitLod(_) -> "OpImageSparseSampleDrefExplicitLod"
                | OpImageSparseSampleProjImplicitLod(_) -> "OpImageSparseSampleProjImplicitLod"
                | OpImageSparseSampleProjExplicitLod(_) -> "OpImageSparseSampleProjExplicitLod"
                | OpImageSparseSampleProjDrefImplicitLod(_) -> "OpImageSparseSampleProjDrefImplicitLod"
                | OpImageSparseSampleProjDrefExplicitLod(_) -> "OpImageSparseSampleProjDrefExplicitLod"
                | OpImageSparseFetch(_) -> "OpImageSparseFetch"
                | OpImageSparseGather(_) -> "OpImageSparseGather"
                | OpImageSparseDrefGather(_) -> "OpImageSparseDrefGather"
                | OpImageSparseTexelsResident(_) -> "OpImageSparseTexelsResident"
                | OpNoLine -> "OpNoLine"
                | OpAtomicFlagTestAndSet(_) -> "OpAtomicFlagTestAndSet"
                | OpAtomicFlagClear(_) -> "OpAtomicFlagClear"




type Module =
    {
        magic : uint32
        version : uint32
        generatorMagic : uint32
        bound : uint32
        reserved : uint32
        instructions : list<Instruction>
    }

module Serializer = 
    open System.IO

    let private writeOpHeader (code : int) (wordSize : int) (target : BinaryWriter) =
        target.Write((uint32 (wordSize) <<< 16) ||| (uint32 code))

    let private readOpHeader (source : BinaryReader) =
        let v = source.ReadUInt32()
        let wordCount = int (v >>> 16)
        let opCode = int (v &&& 0xFFFFu)
        (opCode, wordCount)

    let private writeString (str : string) (target : BinaryWriter) =
        let alignedSize = (str.Length + 4) &&& ~~~3
        for c in str do target.Write(byte c)
        for i in str.Length..alignedSize-1 do target.Write(0uy)

    let private readString (source : BinaryReader) (wordCount : int) =
        if wordCount <= 0 then ""
        else
            let chars = Array.init (4 * wordCount) (fun _ -> source.ReadByte()) |> Array.filter (fun b -> b <> 0uy)
            System.Text.ASCIIEncoding.ASCII.GetString(chars)

    let writeInstructions (instructions : seq<Instruction>) (target : BinaryWriter) = 
        for i in instructions do
            match i with
                | OpNop -> writeOpHeader 0 1 target
                | OpUndef(resultType, result) -> 
                    writeOpHeader 1 3 target
                    target.Write(resultType)
                    target.Write(result)
                | OpSourceContinued(sourceCont) -> 
                    let wordCount = 1 + (sourceCont.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 2 wordCount target
                    writeString sourceCont target
                | OpSource(_source, version, code) -> 
                    let wordCount = 3 + (code.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 3 wordCount target
                    target.Write(uint32 (int _source))
                    target.Write(version)
                    writeString code target
                | OpSourceExtension(extension) -> 
                    let wordCount = 1 + (extension.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 4 wordCount target
                    writeString extension target
                | OpName(_target, name) -> 
                    let wordCount = 2 + (name.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 5 wordCount target
                    target.Write(_target)
                    writeString name target
                | OpMemberName(_type, _member, name) -> 
                    let wordCount = 3 + (name.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 6 wordCount target
                    target.Write(_type)
                    target.Write(_member)
                    writeString name target
                | OpString(result, value) -> 
                    let wordCount = 2 + (value.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 7 wordCount target
                    target.Write(result)
                    writeString value target
                | OpLine(file, line, column) -> 
                    writeOpHeader 8 4 target
                    target.Write(file)
                    target.Write(line)
                    target.Write(column)
                | OpExtension(name) -> 
                    let wordCount = 1 + (name.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 10 wordCount target
                    writeString name target
                | OpExtInstImport(result, name) -> 
                    let wordCount = 2 + (name.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 11 wordCount target
                    target.Write(result)
                    writeString name target
                | OpExtInst(resultType, result, set, instruction, operands) -> 
                    let wordCount = 5 + operands.Length
                    writeOpHeader 12 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(set)
                    target.Write(instruction)
                    for v in operands do
                        target.Write(v)
                | OpMemoryModel(addressing, memory) -> 
                    writeOpHeader 14 3 target
                    target.Write(uint32 (int addressing))
                    target.Write(uint32 (int memory))
                | OpEntryPoint(executionModel, entryPoint, name) -> 
                    let wordCount = 3 + (name.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 15 wordCount target
                    target.Write(uint32 (int executionModel))
                    target.Write(entryPoint)
                    writeString name target
                | OpExecutionMode(entryPoint, mode, modes) -> 
                    let wordCount = 3 + (if modes.IsSome then 1 else 0)
                    writeOpHeader 16 wordCount target
                    target.Write(entryPoint)
                    target.Write(uint32 (int mode))
                    match modes with
                        | Some v -> target.Write(v)
                        | None -> ()
                | OpCapability(capability) -> 
                    writeOpHeader 17 2 target
                    target.Write(uint32 (int capability))
                | OpTypeVoid(result) -> 
                    writeOpHeader 19 2 target
                    target.Write(result)
                | OpTypeBool(result) -> 
                    writeOpHeader 20 2 target
                    target.Write(result)
                | OpTypeInt(result, width, signedness) -> 
                    writeOpHeader 21 4 target
                    target.Write(result)
                    target.Write(width)
                    target.Write(signedness)
                | OpTypeFloat(result, width) -> 
                    writeOpHeader 22 3 target
                    target.Write(result)
                    target.Write(width)
                | OpTypeVector(result, compType, compCount) -> 
                    writeOpHeader 23 4 target
                    target.Write(result)
                    target.Write(compType)
                    target.Write(compCount)
                | OpTypeMatrix(result, colType, colCount) -> 
                    writeOpHeader 24 4 target
                    target.Write(result)
                    target.Write(colType)
                    target.Write(colCount)
                | OpTypeImage(result, sampledType, dim, depth, arrayed, ms, sampled, format, access) -> 
                    let wordCount = 9 + (if access.IsSome then 1 else 0)
                    writeOpHeader 25 wordCount target
                    target.Write(result)
                    target.Write(sampledType)
                    target.Write(uint32 (int dim))
                    target.Write(depth)
                    target.Write(arrayed)
                    target.Write(ms)
                    target.Write(sampled)
                    target.Write(uint32 (int format))
                    match access with
                        | Some v -> target.Write(uint32 (int v))
                        | None -> ()
                | OpTypeSampler(result) -> 
                    writeOpHeader 26 2 target
                    target.Write(result)
                | OpTypeSampledImage(result, imageType) -> 
                    writeOpHeader 27 3 target
                    target.Write(result)
                    target.Write(imageType)
                | OpTypeArray(result, elementType, length) -> 
                    writeOpHeader 28 4 target
                    target.Write(result)
                    target.Write(elementType)
                    target.Write(length)
                | OpTypeRuntimeArray(result, elementType) -> 
                    writeOpHeader 29 3 target
                    target.Write(result)
                    target.Write(elementType)
                | OpTypeStruct(result, memberTypes) -> 
                    let wordCount = 2 + memberTypes.Length
                    writeOpHeader 30 wordCount target
                    target.Write(result)
                    for v in memberTypes do
                        target.Write(v)
                | OpTypeOpaque(result, opaqueTypeName) -> 
                    let wordCount = 2 + (opaqueTypeName.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 31 wordCount target
                    target.Write(result)
                    writeString opaqueTypeName target
                | OpTypePointer(result, storage, _type) -> 
                    writeOpHeader 32 4 target
                    target.Write(result)
                    target.Write(uint32 (int storage))
                    target.Write(_type)
                | OpTypeFunction(result, retType, paramTypes) -> 
                    let wordCount = 3 + paramTypes.Length
                    writeOpHeader 33 wordCount target
                    target.Write(result)
                    target.Write(retType)
                    for v in paramTypes do
                        target.Write(v)
                | OpTypeEvent(result) -> 
                    writeOpHeader 34 2 target
                    target.Write(result)
                | OpTypeDeviceEvent(result) -> 
                    writeOpHeader 35 2 target
                    target.Write(result)
                | OpTypeReserveId(result) -> 
                    writeOpHeader 36 2 target
                    target.Write(result)
                | OpTypeQueue(result) -> 
                    writeOpHeader 37 2 target
                    target.Write(result)
                | OpTypePipe(result, qualifier) -> 
                    writeOpHeader 38 3 target
                    target.Write(result)
                    target.Write(uint32 (int qualifier))
                | OpTypeForwardPointer(ptrType, storage) -> 
                    writeOpHeader 39 3 target
                    target.Write(ptrType)
                    target.Write(uint32 (int storage))
                | OpConstantTrue(resultType, result) -> 
                    writeOpHeader 41 3 target
                    target.Write(resultType)
                    target.Write(result)
                | OpConstantFalse(resultType, result) -> 
                    writeOpHeader 42 3 target
                    target.Write(resultType)
                    target.Write(result)
                | OpConstant(resultType, result, value) -> 
                    let wordCount = 3 + value.Length
                    writeOpHeader 43 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    for v in value do
                        target.Write(v)
                | OpConstantComposite(resultType, result, constituents) -> 
                    let wordCount = 3 + constituents.Length
                    writeOpHeader 44 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    for v in constituents do
                        target.Write(v)
                | OpConstantSampler(resultType, result, addressingMode, param, filterMode) -> 
                    writeOpHeader 45 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int addressingMode))
                    target.Write(param)
                    target.Write(uint32 (int filterMode))
                | OpConstantNull(resultType, result) -> 
                    writeOpHeader 46 3 target
                    target.Write(resultType)
                    target.Write(result)
                | OpSpecConstantTrue(resultType, result) -> 
                    writeOpHeader 48 3 target
                    target.Write(resultType)
                    target.Write(result)
                | OpSpecConstantFalse(resultType, result) -> 
                    writeOpHeader 49 3 target
                    target.Write(resultType)
                    target.Write(result)
                | OpSpecConstant(resultType, result, value) -> 
                    let wordCount = 3 + value.Length
                    writeOpHeader 50 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    for v in value do
                        target.Write(v)
                | OpSpecConstantComposite(resultType, result, constituents) -> 
                    let wordCount = 3 + constituents.Length
                    writeOpHeader 51 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    for v in constituents do
                        target.Write(v)
                | OpSpecConstantOp(resultType, result, opCode, operands) -> 
                    let wordCount = 4 + operands.Length
                    writeOpHeader 52 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(opCode)
                    for v in operands do
                        target.Write(v)
                | OpFunction(resultType, result, _function, funType) -> 
                    writeOpHeader 54 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int _function))
                    target.Write(funType)
                | OpFunctionParameter(resultType, result) -> 
                    writeOpHeader 55 3 target
                    target.Write(resultType)
                    target.Write(result)
                | OpFunctionEnd -> writeOpHeader 56 1 target
                | OpFunctionCall(resultType, result, _function, args) -> 
                    let wordCount = 4 + args.Length
                    writeOpHeader 57 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_function)
                    for v in args do
                        target.Write(v)
                | OpVariable(resultType, result, storage, initializer) -> 
                    let wordCount = 4 + (if initializer.IsSome then 1 else 0)
                    writeOpHeader 59 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int storage))
                    match initializer with
                        | Some v -> target.Write(v)
                        | None -> ()
                | OpImageTexelPointer(resultType, result, image, coord, sample) -> 
                    writeOpHeader 60 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                    target.Write(coord)
                    target.Write(sample)
                | OpLoad(resultType, result, ptr, access) -> 
                    let wordCount = 4 + (if access.IsSome then 1 else 0)
                    writeOpHeader 61 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    match access with
                        | Some v -> target.Write(uint32 (int v))
                        | None -> ()
                | OpStore(ptr, _object, access) -> 
                    let wordCount = 3 + (if access.IsSome then 1 else 0)
                    writeOpHeader 62 wordCount target
                    target.Write(ptr)
                    target.Write(_object)
                    match access with
                        | Some v -> target.Write(uint32 (int v))
                        | None -> ()
                | OpCopyMemory(_target, _source, access) -> 
                    let wordCount = 3 + (if access.IsSome then 1 else 0)
                    writeOpHeader 63 wordCount target
                    target.Write(_target)
                    target.Write(_source)
                    match access with
                        | Some v -> target.Write(uint32 (int v))
                        | None -> ()
                | OpCopyMemorySized(_target, _source, size, access) -> 
                    let wordCount = 4 + (if access.IsSome then 1 else 0)
                    writeOpHeader 64 wordCount target
                    target.Write(_target)
                    target.Write(_source)
                    target.Write(size)
                    match access with
                        | Some v -> target.Write(uint32 (int v))
                        | None -> ()
                | OpAccessChain(resultType, result, _base, indices) -> 
                    let wordCount = 4 + indices.Length
                    writeOpHeader 65 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                    for v in indices do
                        target.Write(v)
                | OpInBoundsAccessChain(resultType, result, _base, indices) -> 
                    let wordCount = 4 + indices.Length
                    writeOpHeader 66 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                    for v in indices do
                        target.Write(v)
                | OpPtrAccessChain(resultType, result, _base, element, indices) -> 
                    let wordCount = 5 + indices.Length
                    writeOpHeader 67 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                    target.Write(element)
                    for v in indices do
                        target.Write(v)
                | OpArrayLength(resultType, result, structure, arrMember) -> 
                    writeOpHeader 68 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(structure)
                    target.Write(arrMember)
                | OpGenericPtrMemSemantics(resultType, result, ptr) -> 
                    writeOpHeader 69 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                | OpInBoundsPtrAccessChain(resultType, result, _base, element, indices) -> 
                    let wordCount = 5 + indices.Length
                    writeOpHeader 70 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                    target.Write(element)
                    for v in indices do
                        target.Write(v)
                | OpDecorate(_target, decoration, args) -> 
                    let wordCount = 3 + args.Length
                    writeOpHeader 71 wordCount target
                    target.Write(_target)
                    target.Write(uint32 (int decoration))
                    for v in args do
                        target.Write(v)
                | OpMemberDecorate(structType, _member, decoration, args) -> 
                    let wordCount = 4 + args.Length
                    writeOpHeader 72 wordCount target
                    target.Write(structType)
                    target.Write(_member)
                    target.Write(uint32 (int decoration))
                    for v in args do
                        target.Write(v)
                | OpDecorationGroup(result) -> 
                    writeOpHeader 73 2 target
                    target.Write(result)
                | OpGroupDecorate(group, targets) -> 
                    let wordCount = 2 + targets.Length
                    writeOpHeader 74 wordCount target
                    target.Write(group)
                    for v in targets do
                        target.Write(v)
                | OpGroupMemberDecorate(group, targets) -> 
                    let wordCount = 2 + targets.Length
                    writeOpHeader 75 wordCount target
                    target.Write(group)
                    for v in targets do
                        target.Write(v)
                | OpVectorExtractDynamic(resultType, result, vector, index) -> 
                    writeOpHeader 77 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(vector)
                    target.Write(index)
                | OpVectorInsertDynamic(resultType, result, vector, _component, index) -> 
                    writeOpHeader 78 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(vector)
                    target.Write(_component)
                    target.Write(index)
                | OpVectorShuffle(resultType, result, v0, v1, components) -> 
                    let wordCount = 5 + components.Length
                    writeOpHeader 79 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(v0)
                    target.Write(v1)
                    for v in components do
                        target.Write(v)
                | OpCompositeConstruct(resultType, result, constituents) -> 
                    let wordCount = 3 + constituents.Length
                    writeOpHeader 80 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    for v in constituents do
                        target.Write(v)
                | OpCompositeExtract(resultType, result, composite, indices) -> 
                    let wordCount = 4 + indices.Length
                    writeOpHeader 81 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(composite)
                    for v in indices do
                        target.Write(v)
                | OpCompositeInsert(resultType, result, _object, composite, indices) -> 
                    let wordCount = 5 + indices.Length
                    writeOpHeader 82 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_object)
                    target.Write(composite)
                    for v in indices do
                        target.Write(v)
                | OpCopyObject(resultType, result, operand) -> 
                    writeOpHeader 83 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(operand)
                | OpTranspose(resultType, result, mat) -> 
                    writeOpHeader 84 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(mat)
                | OpSampledImage(resultType, result, image, sampler) -> 
                    writeOpHeader 86 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                    target.Write(sampler)
                | OpImageSampleImplicitLod(resultType, result, sampledImage, coord, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 87 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSampleExplicitLod(resultType, result, sampledImage, coord, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 88 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSampleDrefImplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 89 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(depthRef)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSampleDrefExplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 90 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(depthRef)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSampleProjImplicitLod(resultType, result, sampledImage, coord, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 91 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSampleProjExplicitLod(resultType, result, sampledImage, coord, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 92 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSampleProjDrefImplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 93 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(depthRef)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSampleProjDrefExplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 94 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(depthRef)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageFetch(resultType, result, image, coord, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 95 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                    target.Write(coord)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageGather(resultType, result, sampledImage, coord, _component, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 96 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(_component)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageDrefGather(resultType, result, sampledImage, coord, depthRef, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 97 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(depthRef)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageRead(resultType, result, image, coords, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 98 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                    target.Write(coords)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageWrite(image, coord, texel, images, variables) -> 
                    let wordCount = 5 + variables.Length
                    writeOpHeader 99 wordCount target
                    target.Write(image)
                    target.Write(coord)
                    target.Write(texel)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImage(resultType, result, sampledImage) -> 
                    writeOpHeader 100 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                | OpImageQueryFormat(resultType, result, image) -> 
                    writeOpHeader 101 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                | OpImageQueryOrder(resultType, result, image) -> 
                    writeOpHeader 102 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                | OpImageQuerySizeLod(resultType, result, image, lod) -> 
                    writeOpHeader 103 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                    target.Write(lod)
                | OpImageQuerySize(resultType, result, image) -> 
                    writeOpHeader 104 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                | OpImageQueryLod(resultType, result, image, coord) -> 
                    writeOpHeader 105 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                    target.Write(coord)
                | OpImageQueryLevels(resultType, result, image) -> 
                    writeOpHeader 106 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                | OpImageQuerySamples(resultType, result, image) -> 
                    writeOpHeader 107 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                | OpConvertFToU(resultType, result, fvec) -> 
                    writeOpHeader 109 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(fvec)
                | OpConvertFToS(resultType, result, floatVal) -> 
                    writeOpHeader 110 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(floatVal)
                | OpConvertSToF(resultType, result, sVal) -> 
                    writeOpHeader 111 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sVal)
                | OpConvertUToF(resultType, result, uVal) -> 
                    writeOpHeader 112 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uVal)
                | OpUConvert(resultType, result, uVal) -> 
                    writeOpHeader 113 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uVal)
                | OpSConvert(resultType, result, sVal) -> 
                    writeOpHeader 114 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sVal)
                | OpFConvert(resultType, result, floatVal) -> 
                    writeOpHeader 115 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(floatVal)
                | OpQuantizeToF16(resultType, result, value) -> 
                    writeOpHeader 116 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(value)
                | OpConvertPtrToU(resultType, result, ptr) -> 
                    writeOpHeader 117 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                | OpSatConvertSToU(resultType, result, sVal) -> 
                    writeOpHeader 118 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sVal)
                | OpSatConvertUToS(resultType, result, uVal) -> 
                    writeOpHeader 119 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uVal)
                | OpConvertUToPtr(resultType, result, intVal) -> 
                    writeOpHeader 120 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(intVal)
                | OpPtrCastToGeneric(resultType, result, ptr) -> 
                    writeOpHeader 121 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                | OpGenericCastToPtr(resultType, result, ptr) -> 
                    writeOpHeader 122 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                | OpGenericCastToPtrExplicit(resultType, result, ptr, storage) -> 
                    writeOpHeader 123 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int storage))
                | OpBitcast(resultType, result, operand) -> 
                    writeOpHeader 124 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(operand)
                | OpSNegate(resultType, result, operand) -> 
                    writeOpHeader 126 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(operand)
                | OpFNegate(resultType, result, operand) -> 
                    writeOpHeader 127 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(operand)
                | OpIAdd(resultType, result, op1, op2) -> 
                    writeOpHeader 128 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFAdd(resultType, result, op1, op2) -> 
                    writeOpHeader 129 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpISub(resultType, result, op1, op2) -> 
                    writeOpHeader 130 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFSub(resultType, result, op1, op2) -> 
                    writeOpHeader 131 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpIMul(resultType, result, op1, op2) -> 
                    writeOpHeader 132 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFMul(resultType, result, op1, op2) -> 
                    writeOpHeader 133 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpUDiv(resultType, result, op1, op2) -> 
                    writeOpHeader 134 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpSDiv(resultType, result, op1, op2) -> 
                    writeOpHeader 135 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFDiv(resultType, result, op1, op2) -> 
                    writeOpHeader 136 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpUMod(resultType, result, op1, op2) -> 
                    writeOpHeader 137 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpSRem(resultType, result, op1, op2) -> 
                    writeOpHeader 138 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpSMod(resultType, result, op1, op2) -> 
                    writeOpHeader 139 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFRem(resultType, result, op1, op2) -> 
                    writeOpHeader 140 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFMod(resultType, result, op1, op2) -> 
                    writeOpHeader 141 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpVectorTimesScalar(resultType, result, vec, scalar) -> 
                    writeOpHeader 142 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(vec)
                    target.Write(scalar)
                | OpMatrixTimesScalar(resultType, result, mat, scalar) -> 
                    writeOpHeader 143 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(mat)
                    target.Write(scalar)
                | OpVectorTimesMatrix(resultType, result, vec, mat) -> 
                    writeOpHeader 144 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(vec)
                    target.Write(mat)
                | OpMatrixTimesVector(resultType, result, mat, vec) -> 
                    writeOpHeader 145 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(mat)
                    target.Write(vec)
                | OpMatrixTimesMatrix(resultType, result, left, right) -> 
                    writeOpHeader 146 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(left)
                    target.Write(right)
                | OpOuterProduct(resultType, result, left, right) -> 
                    writeOpHeader 147 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(left)
                    target.Write(right)
                | OpDot(resultType, result, left, right) -> 
                    writeOpHeader 148 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(left)
                    target.Write(right)
                | OpIAddCarry(resultType, result, op1, op2) -> 
                    writeOpHeader 149 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpISubBorrow(resultType, result, op1, op2) -> 
                    writeOpHeader 150 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpUMulExtended(resultType, result, op1, op2) -> 
                    writeOpHeader 151 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpSMulExtended(resultType, result, op1, op2) -> 
                    writeOpHeader 152 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpAny(resultType, result, vec) -> 
                    writeOpHeader 154 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(vec)
                | OpAll(resultType, result, vec) -> 
                    writeOpHeader 155 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(vec)
                | OpIsNan(resultType, result, x) -> 
                    writeOpHeader 156 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(x)
                | OpIsInf(resultType, result, y) -> 
                    writeOpHeader 157 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(y)
                | OpIsFinite(resultType, result, x) -> 
                    writeOpHeader 158 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(x)
                | OpIsNormal(resultType, result, x) -> 
                    writeOpHeader 159 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(x)
                | OpSignBitSet(resultType, result, x) -> 
                    writeOpHeader 160 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(x)
                | OpLessOrGreater(resultType, result, x, y) -> 
                    writeOpHeader 161 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(x)
                    target.Write(y)
                | OpOrdered(resultType, result, x, y) -> 
                    writeOpHeader 162 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(x)
                    target.Write(y)
                | OpUnordered(resultType, result, x, y) -> 
                    writeOpHeader 163 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(x)
                    target.Write(y)
                | OpLogicalEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 164 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpLogicalNotEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 165 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpLogicalOr(resultType, result, op1, op2) -> 
                    writeOpHeader 166 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpLogicalAnd(resultType, result, op1, op2) -> 
                    writeOpHeader 167 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpLogicalNot(resultType, result, op) -> 
                    writeOpHeader 168 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op)
                | OpSelect(resultType, result, cond, o1, o2) -> 
                    writeOpHeader 169 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(cond)
                    target.Write(o1)
                    target.Write(o2)
                | OpIEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 170 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpINotEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 171 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpUGreaterThan(resultType, result, op1, op2) -> 
                    writeOpHeader 172 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpSGreaterThan(resultType, result, op1, op2) -> 
                    writeOpHeader 173 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpUGreaterThanEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 174 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpSGreaterThanEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 175 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpULessThan(resultType, result, op1, op2) -> 
                    writeOpHeader 176 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpSLessThan(resultType, result, op1, op2) -> 
                    writeOpHeader 177 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpULessThanEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 178 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpSLessThanEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 179 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFOrdEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 180 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFUnordEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 181 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFOrdNotEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 182 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFUnordNotEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 183 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFOrdLessThan(resultType, result, op1, op2) -> 
                    writeOpHeader 184 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFUnordLessThan(resultType, result, op1, op2) -> 
                    writeOpHeader 185 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFOrdGreaterThan(resultType, result, op1, op2) -> 
                    writeOpHeader 186 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFUnordGreaterThan(resultType, result, op1, op2) -> 
                    writeOpHeader 187 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFOrdLessThanEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 188 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFUnordLessThanEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 189 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFOrdGreaterThanEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 190 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpFUnordGreaterThanEqual(resultType, result, op1, op2) -> 
                    writeOpHeader 191 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpShiftRightLogical(resultType, result, _base, shift) -> 
                    writeOpHeader 194 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                    target.Write(shift)
                | OpShiftRightArithmetic(resultType, result, _base, shift) -> 
                    writeOpHeader 195 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                    target.Write(shift)
                | OpShiftLeftLogical(resultType, result, _base, shift) -> 
                    writeOpHeader 196 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                    target.Write(shift)
                | OpBitwiseOr(resultType, result, op1, op2) -> 
                    writeOpHeader 197 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpBitwiseXor(resultType, result, op1, op2) -> 
                    writeOpHeader 198 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpBitwiseAnd(resultType, result, op1, op2) -> 
                    writeOpHeader 199 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(op1)
                    target.Write(op2)
                | OpNot(resultType, result, operand) -> 
                    writeOpHeader 200 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(operand)
                | OpBitFieldInsert(resultType, result, _base, insert, offset, count) -> 
                    writeOpHeader 201 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                    target.Write(insert)
                    target.Write(offset)
                    target.Write(count)
                | OpBitFieldSExtract(resultType, result, _base, offset, count) -> 
                    writeOpHeader 202 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                    target.Write(offset)
                    target.Write(count)
                | OpBitFieldUExtract(resultType, result, _base, offset, count) -> 
                    writeOpHeader 203 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                    target.Write(offset)
                    target.Write(count)
                | OpBitReverse(resultType, result, _base) -> 
                    writeOpHeader 204 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                | OpBitCount(resultType, result, _base) -> 
                    writeOpHeader 205 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(_base)
                | OpDPdx(resultType, result, p) -> 
                    writeOpHeader 207 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(p)
                | OpDPdy(resultType, result, p) -> 
                    writeOpHeader 208 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(p)
                | OpFwidth(resultType, result, p) -> 
                    writeOpHeader 209 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(p)
                | OpDPdxFine(resultType, result, p) -> 
                    writeOpHeader 210 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(p)
                | OpDPdyFine(resultType, result, p) -> 
                    writeOpHeader 211 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(p)
                | OpFwidthFine(resultType, result, p) -> 
                    writeOpHeader 212 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(p)
                | OpDPdxCoarse(resultType, result, p) -> 
                    writeOpHeader 213 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(p)
                | OpDPdyCoarse(resultType, result, p) -> 
                    writeOpHeader 214 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(p)
                | OpFwidthCoarse(resultType, result, p) -> 
                    writeOpHeader 215 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(p)
                | OpEmitVertex -> writeOpHeader 218 1 target
                | OpEndPrimitive -> writeOpHeader 219 1 target
                | OpEmitStreamVertex(stream) -> 
                    writeOpHeader 220 2 target
                    target.Write(stream)
                | OpEndStreamPrimitive(stream) -> 
                    writeOpHeader 221 2 target
                    target.Write(stream)
                | OpControlBarrier(exec, mem, sem) -> 
                    writeOpHeader 224 4 target
                    target.Write(uint32 (int exec))
                    target.Write(uint32 (int mem))
                    target.Write(uint32 (int sem))
                | OpMemoryBarrier(mem, sen) -> 
                    writeOpHeader 225 3 target
                    target.Write(uint32 (int mem))
                    target.Write(uint32 (int sen))
                | OpAtomicLoad(resultType, result, ptr, scope, sem) -> 
                    writeOpHeader 227 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                | OpAtomicStore(ptr, scope, sem, value) -> 
                    writeOpHeader 228 5 target
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpAtomicExchange(resultType, result, ptr, scope, sem, value) -> 
                    writeOpHeader 229 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpAtomicCompareExchange(resultType, result, ptr, scope, eq, neq, value, comparator) -> 
                    writeOpHeader 230 9 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int eq))
                    target.Write(uint32 (int neq))
                    target.Write(value)
                    target.Write(comparator)
                | OpAtomicCompareExchangeWeak(resultType, result, ptr, scope, eq, neq, value, comparator) -> 
                    writeOpHeader 231 9 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int eq))
                    target.Write(uint32 (int neq))
                    target.Write(value)
                    target.Write(comparator)
                | OpAtomicIIncrement(resultType, result, ptr, scope, sem) -> 
                    writeOpHeader 232 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                | OpAtomicIDecrement(resultType, result, ptr, scope, sem) -> 
                    writeOpHeader 233 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                | OpAtomicIAdd(resultType, result, ptr, scope, sem, value) -> 
                    writeOpHeader 234 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpAtomicISub(resultType, result, ptr, scope, sem, value) -> 
                    writeOpHeader 235 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpAtomicSMin(resultType, result, ptr, scope, sem, value) -> 
                    writeOpHeader 236 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpAtomicUMin(resultType, result, ptr, scope, sem, value) -> 
                    writeOpHeader 237 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpAtomicSMax(resultType, result, ptr, scope, sem, value) -> 
                    writeOpHeader 238 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpAtomicUMax(resultType, result, ptr, scope, sem, value) -> 
                    writeOpHeader 239 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpAtomicAnd(resultType, result, ptr, scope, sem, value) -> 
                    writeOpHeader 240 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpAtomicOr(resultType, result, ptr, scope, sem, value) -> 
                    writeOpHeader 241 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpAtomicXor(resultType, result, ptr, scope, sem, value) -> 
                    writeOpHeader 242 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                    target.Write(value)
                | OpPhi(resultType, result, varsAndParents) -> 
                    let wordCount = 3 + varsAndParents.Length
                    writeOpHeader 245 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    for v in varsAndParents do
                        target.Write(v)
                | OpLoopMerge(mergeBlock, contTarget, loop) -> 
                    writeOpHeader 246 4 target
                    target.Write(mergeBlock)
                    target.Write(contTarget)
                    target.Write(uint32 (int loop))
                | OpSelectionMerge(mergeBlock, select) -> 
                    writeOpHeader 247 3 target
                    target.Write(mergeBlock)
                    target.Write(uint32 (int select))
                | OpLabel(result) -> 
                    writeOpHeader 248 2 target
                    target.Write(result)
                | OpBranch(_target) -> 
                    writeOpHeader 249 2 target
                    target.Write(_target)
                | OpBranchConditional(condition, lTrue, lFalse, weights) -> 
                    let wordCount = 4 + weights.Length
                    writeOpHeader 250 wordCount target
                    target.Write(condition)
                    target.Write(lTrue)
                    target.Write(lFalse)
                    for v in weights do
                        target.Write(v)
                | OpSwitch(sel, def, _target) -> 
                    let wordCount = 3 + _target.Length
                    writeOpHeader 251 wordCount target
                    target.Write(sel)
                    target.Write(def)
                    for v in _target do
                        target.Write(v)
                | OpKill -> writeOpHeader 252 1 target
                | OpReturn -> writeOpHeader 253 1 target
                | OpReturnValue(value) -> 
                    writeOpHeader 254 2 target
                    target.Write(value)
                | OpUnreachable -> writeOpHeader 255 1 target
                | OpLifetimeStart(ptr, size) -> 
                    writeOpHeader 256 3 target
                    target.Write(ptr)
                    target.Write(size)
                | OpLifetimeStop(ptr, size) -> 
                    writeOpHeader 257 3 target
                    target.Write(ptr)
                    target.Write(size)
                | OpGroupAsyncCopy(resultType, result, execution, dest, _source, num, stride, evt) -> 
                    writeOpHeader 259 9 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int execution))
                    target.Write(dest)
                    target.Write(_source)
                    target.Write(num)
                    target.Write(stride)
                    target.Write(evt)
                | OpGroupWaitEvents(exec, num, evtList) -> 
                    writeOpHeader 260 4 target
                    target.Write(uint32 (int exec))
                    target.Write(num)
                    target.Write(evtList)
                | OpGroupAll(resultType, result, exec, pred) -> 
                    writeOpHeader 261 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(pred)
                | OpGroupAny(resultType, result, exec, pred) -> 
                    writeOpHeader 262 5 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(pred)
                | OpGroupBroadcast(resultType, result, exec, value, localid) -> 
                    writeOpHeader 263 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(value)
                    target.Write(localid)
                | OpGroupIAdd(resultType, result, exec, operation, x) -> 
                    writeOpHeader 264 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupFAdd(resultType, result, exec, operation, x) -> 
                    writeOpHeader 265 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupFMin(resultType, result, exec, operation, x) -> 
                    writeOpHeader 266 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupUMin(resultType, result, exec, operation, x) -> 
                    writeOpHeader 267 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupSMin(resultType, result, exec, operation, x) -> 
                    writeOpHeader 268 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupFMax(resultType, result, exec, operation, x) -> 
                    writeOpHeader 269 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupUMax(resultType, result, exec, operation, x) -> 
                    writeOpHeader 270 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupSMax(resultType, result, exec, operation, x) -> 
                    writeOpHeader 271 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpReadPipe(resultType, result, pipe, ptr, packetSize, packetAlign) -> 
                    writeOpHeader 274 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(pipe)
                    target.Write(ptr)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpWritePipe(resultType, result, pipe, ptr, packetSize, packetAlign) -> 
                    writeOpHeader 275 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(pipe)
                    target.Write(ptr)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpReservedReadPipe(resultType, result, pipe, reserveId, index, ptr, packetSize, packetAlign) -> 
                    writeOpHeader 276 9 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(index)
                    target.Write(ptr)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpReservedWritePipe(resultType, result, pipe, reserveId, index, ptr, packetSize, packetAlign) -> 
                    writeOpHeader 277 9 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(index)
                    target.Write(ptr)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpReserveReadPipePackets(resultType, result, pipe, numPackets, packetSize, packetAlign) -> 
                    writeOpHeader 278 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(pipe)
                    target.Write(numPackets)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpReserveWritePipePackets(resultType, result, pipe, numPackets, packetSize, packetAlign) -> 
                    writeOpHeader 279 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(pipe)
                    target.Write(numPackets)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpCommitReadPipe(pipe, reserveId, packetSize, packetAlign) -> 
                    writeOpHeader 280 5 target
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpCommitWritePipe(pipe, reserveId, packetSize, packetAlign) -> 
                    writeOpHeader 281 5 target
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpIsValidReserveId(resultType, result, reserveId) -> 
                    writeOpHeader 282 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(reserveId)
                | OpGetNumPipePackets(resultType, result, pipe, packetSize, packetAlign) -> 
                    writeOpHeader 283 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(pipe)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpGetMaxPipePackets(resultType, result, pipe, packetSize, packetAlign) -> 
                    writeOpHeader 284 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(pipe)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpGroupReserveReadPipePackets(resultType, result, exec, pipe, numPackets, packetSize, packetAlign) -> 
                    writeOpHeader 285 8 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(pipe)
                    target.Write(numPackets)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpGroupReserveWritePipePackets(resultType, result, exec, pipe, numPackets, packetSize, packetAlign) -> 
                    writeOpHeader 286 8 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(uint32 (int exec))
                    target.Write(pipe)
                    target.Write(numPackets)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpGroupCommitReadPipe(exec, pipe, reserveId, packetSize, packetAlign) -> 
                    writeOpHeader 287 6 target
                    target.Write(uint32 (int exec))
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpGroupCommitWritePipe(exec, pipe, reserveId, packetSize, packetAlign) -> 
                    writeOpHeader 288 6 target
                    target.Write(uint32 (int exec))
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(packetSize)
                    target.Write(packetAlign)
                | OpEnqueueMarker(resultType, result, queue, nEvts, waitEvts, retEvt) -> 
                    writeOpHeader 291 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(queue)
                    target.Write(nEvts)
                    target.Write(waitEvts)
                    target.Write(retEvt)
                | OpEnqueueKernel(resultType, result, queue, flags, ndRange, nEvents, waitEvts, retEvt, invoke, param, paramSize, paramAlign, localSize) -> 
                    let wordCount = 13 + localSize.Length
                    writeOpHeader 292 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(queue)
                    target.Write(flags)
                    target.Write(ndRange)
                    target.Write(nEvents)
                    target.Write(waitEvts)
                    target.Write(retEvt)
                    target.Write(invoke)
                    target.Write(param)
                    target.Write(paramSize)
                    target.Write(paramAlign)
                    for v in localSize do
                        target.Write(v)
                | OpGetKernelNDrangeSubGroupCount(resultType, result, ndRange, invoke, param, paramSize, paramAlign) -> 
                    writeOpHeader 293 8 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ndRange)
                    target.Write(invoke)
                    target.Write(param)
                    target.Write(paramSize)
                    target.Write(paramAlign)
                | OpGetKernelNDrangeMaxSubGroupSize(resultType, result, ndRange, invoke, param, paramSize, paramAlign) -> 
                    writeOpHeader 294 8 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ndRange)
                    target.Write(invoke)
                    target.Write(param)
                    target.Write(paramSize)
                    target.Write(paramAlign)
                | OpGetKernelWorkGroupSize(resultType, result, invoke, param, paramSize, paramAlign) -> 
                    writeOpHeader 295 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(invoke)
                    target.Write(param)
                    target.Write(paramSize)
                    target.Write(paramAlign)
                | OpGetKernelPreferredWorkGroupSizeMultiple(resultType, result, invoke, param, paramSize, paramAlign) -> 
                    writeOpHeader 296 7 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(invoke)
                    target.Write(param)
                    target.Write(paramSize)
                    target.Write(paramAlign)
                | OpRetainEvent(evt) -> 
                    writeOpHeader 297 2 target
                    target.Write(evt)
                | OpReleaseEvent(evt) -> 
                    writeOpHeader 298 2 target
                    target.Write(evt)
                | OpCreateUserEvent(resultType, result) -> 
                    writeOpHeader 299 3 target
                    target.Write(resultType)
                    target.Write(result)
                | OpIsValidEvent(resultType, result, evt) -> 
                    writeOpHeader 300 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(evt)
                | OpSetUserEventStatus(evt, status) -> 
                    writeOpHeader 301 3 target
                    target.Write(evt)
                    target.Write(status)
                | OpCaptureEventProfilingInfo(evt, profileInfo, value) -> 
                    writeOpHeader 302 4 target
                    target.Write(evt)
                    target.Write(profileInfo)
                    target.Write(value)
                | OpGetDefaultQueue(resultType, result) -> 
                    writeOpHeader 303 3 target
                    target.Write(resultType)
                    target.Write(result)
                | OpBuildNDRange(resultType, result, globalSize, localSize, globalOffset) -> 
                    writeOpHeader 304 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(globalSize)
                    target.Write(localSize)
                    target.Write(globalOffset)
                | OpImageSparseSampleImplicitLod(resultType, result, sampledImage, coord, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 305 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseSampleExplicitLod(resultType, result, sampledImage, coord, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 306 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseSampleDrefImplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 307 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(depthRef)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseSampleDrefExplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 308 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(depthRef)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseSampleProjImplicitLod(resultType, result, sampledImage, coord, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 309 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseSampleProjExplicitLod(resultType, result, sampledImage, coord, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 310 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseSampleProjDrefImplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 311 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(depthRef)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseSampleProjDrefExplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 312 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(depthRef)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseFetch(resultType, result, image, coord, images, variables) -> 
                    let wordCount = 6 + variables.Length
                    writeOpHeader 313 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(image)
                    target.Write(coord)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseGather(resultType, result, sampledImage, coord, _component, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 314 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(_component)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseDrefGather(resultType, result, sampledImage, coord, depthRef, images, variables) -> 
                    let wordCount = 7 + variables.Length
                    writeOpHeader 315 wordCount target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(sampledImage)
                    target.Write(coord)
                    target.Write(depthRef)
                    target.Write(uint32 (int images))
                    for v in variables do
                        target.Write(v)
                | OpImageSparseTexelsResident(resultType, result, residentCode) -> 
                    writeOpHeader 316 4 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(residentCode)
                | OpNoLine -> writeOpHeader 317 1 target
                | OpAtomicFlagTestAndSet(resultType, result, ptr, scope, sem) -> 
                    writeOpHeader 318 6 target
                    target.Write(resultType)
                    target.Write(result)
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))
                | OpAtomicFlagClear(ptr, scope, sem) -> 
                    writeOpHeader 319 4 target
                    target.Write(ptr)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int sem))

    let readInstructions (source : BinaryReader) = 
        seq {
            while source.BaseStream.Position < source.BaseStream.Length do
                let pos = source.BaseStream.Position
                let (code, size) = readOpHeader source
                match code with
                    | 0 ->
                        yield OpNop
                    | 1 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        yield OpUndef(resultType, result)
                    | 2 ->
                        let sourceContSize = max 0 (size - 1)
                        let sourceCont = readString source sourceContSize
                        yield OpSourceContinued(sourceCont)
                    | 3 ->
                        let codeSize = max 0 (size - 3)
                        let _source = unbox<SourceLanguage> (int (source.ReadUInt32()))
                        let version = source.ReadUInt32()
                        let code = readString source codeSize
                        yield OpSource(_source, version, code)
                    | 4 ->
                        let extensionSize = max 0 (size - 1)
                        let extension = readString source extensionSize
                        yield OpSourceExtension(extension)
                    | 5 ->
                        let nameSize = max 0 (size - 2)
                        let _target = source.ReadUInt32()
                        let name = readString source nameSize
                        yield OpName(_target, name)
                    | 6 ->
                        let nameSize = max 0 (size - 3)
                        let _type = source.ReadUInt32()
                        let _member = source.ReadUInt32()
                        let name = readString source nameSize
                        yield OpMemberName(_type, _member, name)
                    | 7 ->
                        let valueSize = max 0 (size - 2)
                        let result = source.ReadUInt32()
                        let value = readString source valueSize
                        yield OpString(result, value)
                    | 8 ->
                        let file = source.ReadUInt32()
                        let line = source.ReadUInt32()
                        let column = source.ReadUInt32()
                        yield OpLine(file, line, column)
                    | 10 ->
                        let nameSize = max 0 (size - 1)
                        let name = readString source nameSize
                        yield OpExtension(name)
                    | 11 ->
                        let nameSize = max 0 (size - 2)
                        let result = source.ReadUInt32()
                        let name = readString source nameSize
                        yield OpExtInstImport(result, name)
                    | 12 ->
                        let operandsSize = max 0 (size - 5)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let set = source.ReadUInt32()
                        let instruction = source.ReadUInt32()
                        let operands = Array.init operandsSize (fun _ -> source.ReadUInt32())
                        yield OpExtInst(resultType, result, set, instruction, operands)
                    | 14 ->
                        let addressing = unbox<AddressingModel> (int (source.ReadUInt32()))
                        let memory = unbox<MemoryModel> (int (source.ReadUInt32()))
                        yield OpMemoryModel(addressing, memory)
                    | 15 ->
                        let nameSize = max 0 (size - 3)
                        let executionModel = unbox<ExecutionModel> (int (source.ReadUInt32()))
                        let entryPoint = source.ReadUInt32()
                        let name = readString source nameSize
                        yield OpEntryPoint(executionModel, entryPoint, name)
                    | 16 ->
                        let modesSize = max 0 (size - 3)
                        let entryPoint = source.ReadUInt32()
                        let mode = unbox<ExecutionMode> (int (source.ReadUInt32()))
                        let modes = if modesSize = 0 then None else Some (source.ReadUInt32())
                        yield OpExecutionMode(entryPoint, mode, modes)
                    | 17 ->
                        let capability = unbox<Capability> (int (source.ReadUInt32()))
                        yield OpCapability(capability)
                    | 19 ->
                        let result = source.ReadUInt32()
                        yield OpTypeVoid(result)
                    | 20 ->
                        let result = source.ReadUInt32()
                        yield OpTypeBool(result)
                    | 21 ->
                        let result = source.ReadUInt32()
                        let width = source.ReadUInt32()
                        let signedness = source.ReadUInt32()
                        yield OpTypeInt(result, width, signedness)
                    | 22 ->
                        let result = source.ReadUInt32()
                        let width = source.ReadUInt32()
                        yield OpTypeFloat(result, width)
                    | 23 ->
                        let result = source.ReadUInt32()
                        let compType = source.ReadUInt32()
                        let compCount = source.ReadUInt32()
                        yield OpTypeVector(result, compType, compCount)
                    | 24 ->
                        let result = source.ReadUInt32()
                        let colType = source.ReadUInt32()
                        let colCount = source.ReadUInt32()
                        yield OpTypeMatrix(result, colType, colCount)
                    | 25 ->
                        let accessSize = max 0 (size - 9)
                        let result = source.ReadUInt32()
                        let sampledType = source.ReadUInt32()
                        let dim = unbox<Dim> (int (source.ReadUInt32()))
                        let depth = source.ReadUInt32()
                        let arrayed = source.ReadUInt32()
                        let ms = source.ReadUInt32()
                        let sampled = source.ReadUInt32()
                        let format = unbox<int> (int (source.ReadUInt32()))
                        let access = if accessSize = 0 then None else Some (unbox<AccessQualifier> (int (source.ReadUInt32())))
                        yield OpTypeImage(result, sampledType, dim, depth, arrayed, ms, sampled, format, access)
                    | 26 ->
                        let result = source.ReadUInt32()
                        yield OpTypeSampler(result)
                    | 27 ->
                        let result = source.ReadUInt32()
                        let imageType = source.ReadUInt32()
                        yield OpTypeSampledImage(result, imageType)
                    | 28 ->
                        let result = source.ReadUInt32()
                        let elementType = source.ReadUInt32()
                        let length = source.ReadUInt32()
                        yield OpTypeArray(result, elementType, length)
                    | 29 ->
                        let result = source.ReadUInt32()
                        let elementType = source.ReadUInt32()
                        yield OpTypeRuntimeArray(result, elementType)
                    | 30 ->
                        let memberTypesSize = max 0 (size - 2)
                        let result = source.ReadUInt32()
                        let memberTypes = Array.init memberTypesSize (fun _ -> source.ReadUInt32())
                        yield OpTypeStruct(result, memberTypes)
                    | 31 ->
                        let opaqueTypeNameSize = max 0 (size - 2)
                        let result = source.ReadUInt32()
                        let opaqueTypeName = readString source opaqueTypeNameSize
                        yield OpTypeOpaque(result, opaqueTypeName)
                    | 32 ->
                        let result = source.ReadUInt32()
                        let storage = unbox<StorageClass> (int (source.ReadUInt32()))
                        let _type = source.ReadUInt32()
                        yield OpTypePointer(result, storage, _type)
                    | 33 ->
                        let paramTypesSize = max 0 (size - 3)
                        let result = source.ReadUInt32()
                        let retType = source.ReadUInt32()
                        let paramTypes = Array.init paramTypesSize (fun _ -> source.ReadUInt32())
                        yield OpTypeFunction(result, retType, paramTypes)
                    | 34 ->
                        let result = source.ReadUInt32()
                        yield OpTypeEvent(result)
                    | 35 ->
                        let result = source.ReadUInt32()
                        yield OpTypeDeviceEvent(result)
                    | 36 ->
                        let result = source.ReadUInt32()
                        yield OpTypeReserveId(result)
                    | 37 ->
                        let result = source.ReadUInt32()
                        yield OpTypeQueue(result)
                    | 38 ->
                        let result = source.ReadUInt32()
                        let qualifier = unbox<AccessQualifier> (int (source.ReadUInt32()))
                        yield OpTypePipe(result, qualifier)
                    | 39 ->
                        let ptrType = source.ReadUInt32()
                        let storage = unbox<StorageClass> (int (source.ReadUInt32()))
                        yield OpTypeForwardPointer(ptrType, storage)
                    | 41 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        yield OpConstantTrue(resultType, result)
                    | 42 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        yield OpConstantFalse(resultType, result)
                    | 43 ->
                        let valueSize = max 0 (size - 3)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let value = Array.init valueSize (fun _ -> source.ReadUInt32())
                        yield OpConstant(resultType, result, value)
                    | 44 ->
                        let constituentsSize = max 0 (size - 3)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let constituents = Array.init constituentsSize (fun _ -> source.ReadUInt32())
                        yield OpConstantComposite(resultType, result, constituents)
                    | 45 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let addressingMode = unbox<SamplerAddressingMode> (int (source.ReadUInt32()))
                        let param = source.ReadUInt32()
                        let filterMode = unbox<SamplerFilterMode> (int (source.ReadUInt32()))
                        yield OpConstantSampler(resultType, result, addressingMode, param, filterMode)
                    | 46 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        yield OpConstantNull(resultType, result)
                    | 48 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        yield OpSpecConstantTrue(resultType, result)
                    | 49 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        yield OpSpecConstantFalse(resultType, result)
                    | 50 ->
                        let valueSize = max 0 (size - 3)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let value = Array.init valueSize (fun _ -> source.ReadUInt32())
                        yield OpSpecConstant(resultType, result, value)
                    | 51 ->
                        let constituentsSize = max 0 (size - 3)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let constituents = Array.init constituentsSize (fun _ -> source.ReadUInt32())
                        yield OpSpecConstantComposite(resultType, result, constituents)
                    | 52 ->
                        let operandsSize = max 0 (size - 4)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let opCode = source.ReadUInt32()
                        let operands = Array.init operandsSize (fun _ -> source.ReadUInt32())
                        yield OpSpecConstantOp(resultType, result, opCode, operands)
                    | 54 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _function = unbox<FunctionControl> (int (source.ReadUInt32()))
                        let funType = source.ReadUInt32()
                        yield OpFunction(resultType, result, _function, funType)
                    | 55 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        yield OpFunctionParameter(resultType, result)
                    | 56 ->
                        yield OpFunctionEnd
                    | 57 ->
                        let argsSize = max 0 (size - 4)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _function = source.ReadUInt32()
                        let args = Array.init argsSize (fun _ -> source.ReadUInt32())
                        yield OpFunctionCall(resultType, result, _function, args)
                    | 59 ->
                        let initializerSize = max 0 (size - 4)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let storage = unbox<StorageClass> (int (source.ReadUInt32()))
                        let initializer = if initializerSize = 0 then None else Some (source.ReadUInt32())
                        yield OpVariable(resultType, result, storage, initializer)
                    | 60 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let sample = source.ReadUInt32()
                        yield OpImageTexelPointer(resultType, result, image, coord, sample)
                    | 61 ->
                        let accessSize = max 0 (size - 4)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let access = if accessSize = 0 then None else Some (unbox<MemoryAccess> (int (source.ReadUInt32())))
                        yield OpLoad(resultType, result, ptr, access)
                    | 62 ->
                        let accessSize = max 0 (size - 3)
                        let ptr = source.ReadUInt32()
                        let _object = source.ReadUInt32()
                        let access = if accessSize = 0 then None else Some (unbox<MemoryAccess> (int (source.ReadUInt32())))
                        yield OpStore(ptr, _object, access)
                    | 63 ->
                        let accessSize = max 0 (size - 3)
                        let _target = source.ReadUInt32()
                        let _source = source.ReadUInt32()
                        let access = if accessSize = 0 then None else Some (unbox<MemoryAccess> (int (source.ReadUInt32())))
                        yield OpCopyMemory(_target, _source, access)
                    | 64 ->
                        let accessSize = max 0 (size - 4)
                        let _target = source.ReadUInt32()
                        let _source = source.ReadUInt32()
                        let size = source.ReadUInt32()
                        let access = if accessSize = 0 then None else Some (unbox<MemoryAccess> (int (source.ReadUInt32())))
                        yield OpCopyMemorySized(_target, _source, size, access)
                    | 65 ->
                        let indicesSize = max 0 (size - 4)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        let indices = Array.init indicesSize (fun _ -> source.ReadUInt32())
                        yield OpAccessChain(resultType, result, _base, indices)
                    | 66 ->
                        let indicesSize = max 0 (size - 4)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        let indices = Array.init indicesSize (fun _ -> source.ReadUInt32())
                        yield OpInBoundsAccessChain(resultType, result, _base, indices)
                    | 67 ->
                        let indicesSize = max 0 (size - 5)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        let element = source.ReadUInt32()
                        let indices = Array.init indicesSize (fun _ -> source.ReadUInt32())
                        yield OpPtrAccessChain(resultType, result, _base, element, indices)
                    | 68 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let structure = source.ReadUInt32()
                        let arrMember = source.ReadUInt32()
                        yield OpArrayLength(resultType, result, structure, arrMember)
                    | 69 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        yield OpGenericPtrMemSemantics(resultType, result, ptr)
                    | 70 ->
                        let indicesSize = max 0 (size - 5)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        let element = source.ReadUInt32()
                        let indices = Array.init indicesSize (fun _ -> source.ReadUInt32())
                        yield OpInBoundsPtrAccessChain(resultType, result, _base, element, indices)
                    | 71 ->
                        let argsSize = max 0 (size - 3)
                        let _target = source.ReadUInt32()
                        let decoration = unbox<Decoration> (int (source.ReadUInt32()))
                        let args = Array.init argsSize (fun _ -> source.ReadUInt32())
                        yield OpDecorate(_target, decoration, args)
                    | 72 ->
                        let argsSize = max 0 (size - 4)
                        let structType = source.ReadUInt32()
                        let _member = source.ReadUInt32()
                        let decoration = unbox<Decoration> (int (source.ReadUInt32()))
                        let args = Array.init argsSize (fun _ -> source.ReadUInt32())
                        yield OpMemberDecorate(structType, _member, decoration, args)
                    | 73 ->
                        let result = source.ReadUInt32()
                        yield OpDecorationGroup(result)
                    | 74 ->
                        let targetsSize = max 0 (size - 2)
                        let group = source.ReadUInt32()
                        let targets = Array.init targetsSize (fun _ -> source.ReadUInt32())
                        yield OpGroupDecorate(group, targets)
                    | 75 ->
                        let targetsSize = max 0 (size - 2)
                        let group = source.ReadUInt32()
                        let targets = Array.init targetsSize (fun _ -> source.ReadUInt32())
                        yield OpGroupMemberDecorate(group, targets)
                    | 77 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let vector = source.ReadUInt32()
                        let index = source.ReadUInt32()
                        yield OpVectorExtractDynamic(resultType, result, vector, index)
                    | 78 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let vector = source.ReadUInt32()
                        let _component = source.ReadUInt32()
                        let index = source.ReadUInt32()
                        yield OpVectorInsertDynamic(resultType, result, vector, _component, index)
                    | 79 ->
                        let componentsSize = max 0 (size - 5)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let v0 = source.ReadUInt32()
                        let v1 = source.ReadUInt32()
                        let components = Array.init componentsSize (fun _ -> source.ReadUInt32())
                        yield OpVectorShuffle(resultType, result, v0, v1, components)
                    | 80 ->
                        let constituentsSize = max 0 (size - 3)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let constituents = Array.init constituentsSize (fun _ -> source.ReadUInt32())
                        yield OpCompositeConstruct(resultType, result, constituents)
                    | 81 ->
                        let indicesSize = max 0 (size - 4)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let composite = source.ReadUInt32()
                        let indices = Array.init indicesSize (fun _ -> source.ReadUInt32())
                        yield OpCompositeExtract(resultType, result, composite, indices)
                    | 82 ->
                        let indicesSize = max 0 (size - 5)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _object = source.ReadUInt32()
                        let composite = source.ReadUInt32()
                        let indices = Array.init indicesSize (fun _ -> source.ReadUInt32())
                        yield OpCompositeInsert(resultType, result, _object, composite, indices)
                    | 83 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let operand = source.ReadUInt32()
                        yield OpCopyObject(resultType, result, operand)
                    | 84 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let mat = source.ReadUInt32()
                        yield OpTranspose(resultType, result, mat)
                    | 86 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        let sampler = source.ReadUInt32()
                        yield OpSampledImage(resultType, result, image, sampler)
                    | 87 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleImplicitLod(resultType, result, sampledImage, coord, images, variables)
                    | 88 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleExplicitLod(resultType, result, sampledImage, coord, images, variables)
                    | 89 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let depthRef = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleDrefImplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables)
                    | 90 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let depthRef = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleDrefExplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables)
                    | 91 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleProjImplicitLod(resultType, result, sampledImage, coord, images, variables)
                    | 92 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleProjExplicitLod(resultType, result, sampledImage, coord, images, variables)
                    | 93 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let depthRef = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleProjDrefImplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables)
                    | 94 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let depthRef = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleProjDrefExplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables)
                    | 95 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageFetch(resultType, result, image, coord, images, variables)
                    | 96 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let _component = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageGather(resultType, result, sampledImage, coord, _component, images, variables)
                    | 97 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let depthRef = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageDrefGather(resultType, result, sampledImage, coord, depthRef, images, variables)
                    | 98 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        let coords = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageRead(resultType, result, image, coords, images, variables)
                    | 99 ->
                        let variablesSize = max 0 (size - 5)
                        let image = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let texel = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageWrite(image, coord, texel, images, variables)
                    | 100 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        yield OpImage(resultType, result, sampledImage)
                    | 101 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        yield OpImageQueryFormat(resultType, result, image)
                    | 102 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        yield OpImageQueryOrder(resultType, result, image)
                    | 103 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        let lod = source.ReadUInt32()
                        yield OpImageQuerySizeLod(resultType, result, image, lod)
                    | 104 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        yield OpImageQuerySize(resultType, result, image)
                    | 105 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        yield OpImageQueryLod(resultType, result, image, coord)
                    | 106 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        yield OpImageQueryLevels(resultType, result, image)
                    | 107 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        yield OpImageQuerySamples(resultType, result, image)
                    | 109 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let fvec = source.ReadUInt32()
                        yield OpConvertFToU(resultType, result, fvec)
                    | 110 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let floatVal = source.ReadUInt32()
                        yield OpConvertFToS(resultType, result, floatVal)
                    | 111 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sVal = source.ReadUInt32()
                        yield OpConvertSToF(resultType, result, sVal)
                    | 112 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let uVal = source.ReadUInt32()
                        yield OpConvertUToF(resultType, result, uVal)
                    | 113 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let uVal = source.ReadUInt32()
                        yield OpUConvert(resultType, result, uVal)
                    | 114 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sVal = source.ReadUInt32()
                        yield OpSConvert(resultType, result, sVal)
                    | 115 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let floatVal = source.ReadUInt32()
                        yield OpFConvert(resultType, result, floatVal)
                    | 116 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let value = source.ReadUInt32()
                        yield OpQuantizeToF16(resultType, result, value)
                    | 117 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        yield OpConvertPtrToU(resultType, result, ptr)
                    | 118 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sVal = source.ReadUInt32()
                        yield OpSatConvertSToU(resultType, result, sVal)
                    | 119 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let uVal = source.ReadUInt32()
                        yield OpSatConvertUToS(resultType, result, uVal)
                    | 120 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let intVal = source.ReadUInt32()
                        yield OpConvertUToPtr(resultType, result, intVal)
                    | 121 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        yield OpPtrCastToGeneric(resultType, result, ptr)
                    | 122 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        yield OpGenericCastToPtr(resultType, result, ptr)
                    | 123 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let storage = unbox<StorageClass> (int (source.ReadUInt32()))
                        yield OpGenericCastToPtrExplicit(resultType, result, ptr, storage)
                    | 124 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let operand = source.ReadUInt32()
                        yield OpBitcast(resultType, result, operand)
                    | 126 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let operand = source.ReadUInt32()
                        yield OpSNegate(resultType, result, operand)
                    | 127 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let operand = source.ReadUInt32()
                        yield OpFNegate(resultType, result, operand)
                    | 128 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpIAdd(resultType, result, op1, op2)
                    | 129 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFAdd(resultType, result, op1, op2)
                    | 130 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpISub(resultType, result, op1, op2)
                    | 131 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFSub(resultType, result, op1, op2)
                    | 132 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpIMul(resultType, result, op1, op2)
                    | 133 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFMul(resultType, result, op1, op2)
                    | 134 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpUDiv(resultType, result, op1, op2)
                    | 135 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpSDiv(resultType, result, op1, op2)
                    | 136 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFDiv(resultType, result, op1, op2)
                    | 137 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpUMod(resultType, result, op1, op2)
                    | 138 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpSRem(resultType, result, op1, op2)
                    | 139 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpSMod(resultType, result, op1, op2)
                    | 140 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFRem(resultType, result, op1, op2)
                    | 141 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFMod(resultType, result, op1, op2)
                    | 142 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let vec = source.ReadUInt32()
                        let scalar = source.ReadUInt32()
                        yield OpVectorTimesScalar(resultType, result, vec, scalar)
                    | 143 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let mat = source.ReadUInt32()
                        let scalar = source.ReadUInt32()
                        yield OpMatrixTimesScalar(resultType, result, mat, scalar)
                    | 144 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let vec = source.ReadUInt32()
                        let mat = source.ReadUInt32()
                        yield OpVectorTimesMatrix(resultType, result, vec, mat)
                    | 145 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let mat = source.ReadUInt32()
                        let vec = source.ReadUInt32()
                        yield OpMatrixTimesVector(resultType, result, mat, vec)
                    | 146 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let left = source.ReadUInt32()
                        let right = source.ReadUInt32()
                        yield OpMatrixTimesMatrix(resultType, result, left, right)
                    | 147 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let left = source.ReadUInt32()
                        let right = source.ReadUInt32()
                        yield OpOuterProduct(resultType, result, left, right)
                    | 148 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let left = source.ReadUInt32()
                        let right = source.ReadUInt32()
                        yield OpDot(resultType, result, left, right)
                    | 149 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpIAddCarry(resultType, result, op1, op2)
                    | 150 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpISubBorrow(resultType, result, op1, op2)
                    | 151 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpUMulExtended(resultType, result, op1, op2)
                    | 152 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpSMulExtended(resultType, result, op1, op2)
                    | 154 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let vec = source.ReadUInt32()
                        yield OpAny(resultType, result, vec)
                    | 155 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let vec = source.ReadUInt32()
                        yield OpAll(resultType, result, vec)
                    | 156 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let x = source.ReadUInt32()
                        yield OpIsNan(resultType, result, x)
                    | 157 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let y = source.ReadUInt32()
                        yield OpIsInf(resultType, result, y)
                    | 158 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let x = source.ReadUInt32()
                        yield OpIsFinite(resultType, result, x)
                    | 159 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let x = source.ReadUInt32()
                        yield OpIsNormal(resultType, result, x)
                    | 160 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let x = source.ReadUInt32()
                        yield OpSignBitSet(resultType, result, x)
                    | 161 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let x = source.ReadUInt32()
                        let y = source.ReadUInt32()
                        yield OpLessOrGreater(resultType, result, x, y)
                    | 162 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let x = source.ReadUInt32()
                        let y = source.ReadUInt32()
                        yield OpOrdered(resultType, result, x, y)
                    | 163 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let x = source.ReadUInt32()
                        let y = source.ReadUInt32()
                        yield OpUnordered(resultType, result, x, y)
                    | 164 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpLogicalEqual(resultType, result, op1, op2)
                    | 165 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpLogicalNotEqual(resultType, result, op1, op2)
                    | 166 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpLogicalOr(resultType, result, op1, op2)
                    | 167 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpLogicalAnd(resultType, result, op1, op2)
                    | 168 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op = source.ReadUInt32()
                        yield OpLogicalNot(resultType, result, op)
                    | 169 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let cond = source.ReadUInt32()
                        let o1 = source.ReadUInt32()
                        let o2 = source.ReadUInt32()
                        yield OpSelect(resultType, result, cond, o1, o2)
                    | 170 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpIEqual(resultType, result, op1, op2)
                    | 171 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpINotEqual(resultType, result, op1, op2)
                    | 172 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpUGreaterThan(resultType, result, op1, op2)
                    | 173 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpSGreaterThan(resultType, result, op1, op2)
                    | 174 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpUGreaterThanEqual(resultType, result, op1, op2)
                    | 175 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpSGreaterThanEqual(resultType, result, op1, op2)
                    | 176 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpULessThan(resultType, result, op1, op2)
                    | 177 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpSLessThan(resultType, result, op1, op2)
                    | 178 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpULessThanEqual(resultType, result, op1, op2)
                    | 179 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpSLessThanEqual(resultType, result, op1, op2)
                    | 180 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFOrdEqual(resultType, result, op1, op2)
                    | 181 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFUnordEqual(resultType, result, op1, op2)
                    | 182 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFOrdNotEqual(resultType, result, op1, op2)
                    | 183 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFUnordNotEqual(resultType, result, op1, op2)
                    | 184 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFOrdLessThan(resultType, result, op1, op2)
                    | 185 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFUnordLessThan(resultType, result, op1, op2)
                    | 186 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFOrdGreaterThan(resultType, result, op1, op2)
                    | 187 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFUnordGreaterThan(resultType, result, op1, op2)
                    | 188 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFOrdLessThanEqual(resultType, result, op1, op2)
                    | 189 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFUnordLessThanEqual(resultType, result, op1, op2)
                    | 190 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFOrdGreaterThanEqual(resultType, result, op1, op2)
                    | 191 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpFUnordGreaterThanEqual(resultType, result, op1, op2)
                    | 194 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        let shift = source.ReadUInt32()
                        yield OpShiftRightLogical(resultType, result, _base, shift)
                    | 195 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        let shift = source.ReadUInt32()
                        yield OpShiftRightArithmetic(resultType, result, _base, shift)
                    | 196 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        let shift = source.ReadUInt32()
                        yield OpShiftLeftLogical(resultType, result, _base, shift)
                    | 197 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpBitwiseOr(resultType, result, op1, op2)
                    | 198 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpBitwiseXor(resultType, result, op1, op2)
                    | 199 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let op1 = source.ReadUInt32()
                        let op2 = source.ReadUInt32()
                        yield OpBitwiseAnd(resultType, result, op1, op2)
                    | 200 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let operand = source.ReadUInt32()
                        yield OpNot(resultType, result, operand)
                    | 201 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        let insert = source.ReadUInt32()
                        let offset = source.ReadUInt32()
                        let count = source.ReadUInt32()
                        yield OpBitFieldInsert(resultType, result, _base, insert, offset, count)
                    | 202 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        let offset = source.ReadUInt32()
                        let count = source.ReadUInt32()
                        yield OpBitFieldSExtract(resultType, result, _base, offset, count)
                    | 203 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        let offset = source.ReadUInt32()
                        let count = source.ReadUInt32()
                        yield OpBitFieldUExtract(resultType, result, _base, offset, count)
                    | 204 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        yield OpBitReverse(resultType, result, _base)
                    | 205 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let _base = source.ReadUInt32()
                        yield OpBitCount(resultType, result, _base)
                    | 207 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let p = source.ReadUInt32()
                        yield OpDPdx(resultType, result, p)
                    | 208 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let p = source.ReadUInt32()
                        yield OpDPdy(resultType, result, p)
                    | 209 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let p = source.ReadUInt32()
                        yield OpFwidth(resultType, result, p)
                    | 210 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let p = source.ReadUInt32()
                        yield OpDPdxFine(resultType, result, p)
                    | 211 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let p = source.ReadUInt32()
                        yield OpDPdyFine(resultType, result, p)
                    | 212 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let p = source.ReadUInt32()
                        yield OpFwidthFine(resultType, result, p)
                    | 213 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let p = source.ReadUInt32()
                        yield OpDPdxCoarse(resultType, result, p)
                    | 214 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let p = source.ReadUInt32()
                        yield OpDPdyCoarse(resultType, result, p)
                    | 215 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let p = source.ReadUInt32()
                        yield OpFwidthCoarse(resultType, result, p)
                    | 218 ->
                        yield OpEmitVertex
                    | 219 ->
                        yield OpEndPrimitive
                    | 220 ->
                        let stream = source.ReadUInt32()
                        yield OpEmitStreamVertex(stream)
                    | 221 ->
                        let stream = source.ReadUInt32()
                        yield OpEndStreamPrimitive(stream)
                    | 224 ->
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let mem = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpControlBarrier(exec, mem, sem)
                    | 225 ->
                        let mem = unbox<Scope> (int (source.ReadUInt32()))
                        let sen = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpMemoryBarrier(mem, sen)
                    | 227 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicLoad(resultType, result, ptr, scope, sem)
                    | 228 ->
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicStore(ptr, scope, sem, value)
                    | 229 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicExchange(resultType, result, ptr, scope, sem, value)
                    | 230 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let eq = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let neq = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        let comparator = source.ReadUInt32()
                        yield OpAtomicCompareExchange(resultType, result, ptr, scope, eq, neq, value, comparator)
                    | 231 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let eq = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let neq = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        let comparator = source.ReadUInt32()
                        yield OpAtomicCompareExchangeWeak(resultType, result, ptr, scope, eq, neq, value, comparator)
                    | 232 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicIIncrement(resultType, result, ptr, scope, sem)
                    | 233 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicIDecrement(resultType, result, ptr, scope, sem)
                    | 234 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicIAdd(resultType, result, ptr, scope, sem, value)
                    | 235 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicISub(resultType, result, ptr, scope, sem, value)
                    | 236 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicSMin(resultType, result, ptr, scope, sem, value)
                    | 237 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicUMin(resultType, result, ptr, scope, sem, value)
                    | 238 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicSMax(resultType, result, ptr, scope, sem, value)
                    | 239 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicUMax(resultType, result, ptr, scope, sem, value)
                    | 240 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicAnd(resultType, result, ptr, scope, sem, value)
                    | 241 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicOr(resultType, result, ptr, scope, sem, value)
                    | 242 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicXor(resultType, result, ptr, scope, sem, value)
                    | 245 ->
                        let varsAndParentsSize = max 0 (size - 3)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let varsAndParents = Array.init varsAndParentsSize (fun _ -> source.ReadUInt32())
                        yield OpPhi(resultType, result, varsAndParents)
                    | 246 ->
                        let mergeBlock = source.ReadUInt32()
                        let contTarget = source.ReadUInt32()
                        let loop = unbox<LoopControl> (int (source.ReadUInt32()))
                        yield OpLoopMerge(mergeBlock, contTarget, loop)
                    | 247 ->
                        let mergeBlock = source.ReadUInt32()
                        let select = unbox<SelectionControl> (int (source.ReadUInt32()))
                        yield OpSelectionMerge(mergeBlock, select)
                    | 248 ->
                        let result = source.ReadUInt32()
                        yield OpLabel(result)
                    | 249 ->
                        let _target = source.ReadUInt32()
                        yield OpBranch(_target)
                    | 250 ->
                        let weightsSize = max 0 (size - 4)
                        let condition = source.ReadUInt32()
                        let lTrue = source.ReadUInt32()
                        let lFalse = source.ReadUInt32()
                        let weights = Array.init weightsSize (fun _ -> source.ReadUInt32())
                        yield OpBranchConditional(condition, lTrue, lFalse, weights)
                    | 251 ->
                        let _targetSize = max 0 (size - 3)
                        let sel = source.ReadUInt32()
                        let def = source.ReadUInt32()
                        let _target = Array.init _targetSize (fun _ -> source.ReadUInt32())
                        yield OpSwitch(sel, def, _target)
                    | 252 ->
                        yield OpKill
                    | 253 ->
                        yield OpReturn
                    | 254 ->
                        let value = source.ReadUInt32()
                        yield OpReturnValue(value)
                    | 255 ->
                        yield OpUnreachable
                    | 256 ->
                        let ptr = source.ReadUInt32()
                        let size = source.ReadUInt32()
                        yield OpLifetimeStart(ptr, size)
                    | 257 ->
                        let ptr = source.ReadUInt32()
                        let size = source.ReadUInt32()
                        yield OpLifetimeStop(ptr, size)
                    | 259 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let execution = unbox<Scope> (int (source.ReadUInt32()))
                        let dest = source.ReadUInt32()
                        let _source = source.ReadUInt32()
                        let num = source.ReadUInt32()
                        let stride = source.ReadUInt32()
                        let evt = source.ReadUInt32()
                        yield OpGroupAsyncCopy(resultType, result, execution, dest, _source, num, stride, evt)
                    | 260 ->
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let num = source.ReadUInt32()
                        let evtList = source.ReadUInt32()
                        yield OpGroupWaitEvents(exec, num, evtList)
                    | 261 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let pred = source.ReadUInt32()
                        yield OpGroupAll(resultType, result, exec, pred)
                    | 262 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let pred = source.ReadUInt32()
                        yield OpGroupAny(resultType, result, exec, pred)
                    | 263 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        let localid = source.ReadUInt32()
                        yield OpGroupBroadcast(resultType, result, exec, value, localid)
                    | 264 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupIAdd(resultType, result, exec, operation, x)
                    | 265 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupFAdd(resultType, result, exec, operation, x)
                    | 266 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupFMin(resultType, result, exec, operation, x)
                    | 267 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupUMin(resultType, result, exec, operation, x)
                    | 268 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupSMin(resultType, result, exec, operation, x)
                    | 269 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupFMax(resultType, result, exec, operation, x)
                    | 270 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupUMax(resultType, result, exec, operation, x)
                    | 271 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupSMax(resultType, result, exec, operation, x)
                    | 274 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let pipe = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpReadPipe(resultType, result, pipe, ptr, packetSize, packetAlign)
                    | 275 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let pipe = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpWritePipe(resultType, result, pipe, ptr, packetSize, packetAlign)
                    | 276 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let index = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpReservedReadPipe(resultType, result, pipe, reserveId, index, ptr, packetSize, packetAlign)
                    | 277 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let index = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpReservedWritePipe(resultType, result, pipe, reserveId, index, ptr, packetSize, packetAlign)
                    | 278 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let pipe = source.ReadUInt32()
                        let numPackets = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpReserveReadPipePackets(resultType, result, pipe, numPackets, packetSize, packetAlign)
                    | 279 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let pipe = source.ReadUInt32()
                        let numPackets = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpReserveWritePipePackets(resultType, result, pipe, numPackets, packetSize, packetAlign)
                    | 280 ->
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpCommitReadPipe(pipe, reserveId, packetSize, packetAlign)
                    | 281 ->
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpCommitWritePipe(pipe, reserveId, packetSize, packetAlign)
                    | 282 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        yield OpIsValidReserveId(resultType, result, reserveId)
                    | 283 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let pipe = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpGetNumPipePackets(resultType, result, pipe, packetSize, packetAlign)
                    | 284 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let pipe = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpGetMaxPipePackets(resultType, result, pipe, packetSize, packetAlign)
                    | 285 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let numPackets = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpGroupReserveReadPipePackets(resultType, result, exec, pipe, numPackets, packetSize, packetAlign)
                    | 286 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let numPackets = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpGroupReserveWritePipePackets(resultType, result, exec, pipe, numPackets, packetSize, packetAlign)
                    | 287 ->
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpGroupCommitReadPipe(exec, pipe, reserveId, packetSize, packetAlign)
                    | 288 ->
                        let exec = unbox<Scope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpGroupCommitWritePipe(exec, pipe, reserveId, packetSize, packetAlign)
                    | 291 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let queue = source.ReadUInt32()
                        let nEvts = source.ReadUInt32()
                        let waitEvts = source.ReadUInt32()
                        let retEvt = source.ReadUInt32()
                        yield OpEnqueueMarker(resultType, result, queue, nEvts, waitEvts, retEvt)
                    | 292 ->
                        let localSizeSize = max 0 (size - 13)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let queue = source.ReadUInt32()
                        let flags = source.ReadUInt32()
                        let ndRange = source.ReadUInt32()
                        let nEvents = source.ReadUInt32()
                        let waitEvts = source.ReadUInt32()
                        let retEvt = source.ReadUInt32()
                        let invoke = source.ReadUInt32()
                        let param = source.ReadUInt32()
                        let paramSize = source.ReadUInt32()
                        let paramAlign = source.ReadUInt32()
                        let localSize = Array.init localSizeSize (fun _ -> source.ReadUInt32())
                        yield OpEnqueueKernel(resultType, result, queue, flags, ndRange, nEvents, waitEvts, retEvt, invoke, param, paramSize, paramAlign, localSize)
                    | 293 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ndRange = source.ReadUInt32()
                        let invoke = source.ReadUInt32()
                        let param = source.ReadUInt32()
                        let paramSize = source.ReadUInt32()
                        let paramAlign = source.ReadUInt32()
                        yield OpGetKernelNDrangeSubGroupCount(resultType, result, ndRange, invoke, param, paramSize, paramAlign)
                    | 294 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ndRange = source.ReadUInt32()
                        let invoke = source.ReadUInt32()
                        let param = source.ReadUInt32()
                        let paramSize = source.ReadUInt32()
                        let paramAlign = source.ReadUInt32()
                        yield OpGetKernelNDrangeMaxSubGroupSize(resultType, result, ndRange, invoke, param, paramSize, paramAlign)
                    | 295 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let invoke = source.ReadUInt32()
                        let param = source.ReadUInt32()
                        let paramSize = source.ReadUInt32()
                        let paramAlign = source.ReadUInt32()
                        yield OpGetKernelWorkGroupSize(resultType, result, invoke, param, paramSize, paramAlign)
                    | 296 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let invoke = source.ReadUInt32()
                        let param = source.ReadUInt32()
                        let paramSize = source.ReadUInt32()
                        let paramAlign = source.ReadUInt32()
                        yield OpGetKernelPreferredWorkGroupSizeMultiple(resultType, result, invoke, param, paramSize, paramAlign)
                    | 297 ->
                        let evt = source.ReadUInt32()
                        yield OpRetainEvent(evt)
                    | 298 ->
                        let evt = source.ReadUInt32()
                        yield OpReleaseEvent(evt)
                    | 299 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        yield OpCreateUserEvent(resultType, result)
                    | 300 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let evt = source.ReadUInt32()
                        yield OpIsValidEvent(resultType, result, evt)
                    | 301 ->
                        let evt = source.ReadUInt32()
                        let status = source.ReadUInt32()
                        yield OpSetUserEventStatus(evt, status)
                    | 302 ->
                        let evt = source.ReadUInt32()
                        let profileInfo = source.ReadUInt32()
                        let value = source.ReadUInt32()
                        yield OpCaptureEventProfilingInfo(evt, profileInfo, value)
                    | 303 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        yield OpGetDefaultQueue(resultType, result)
                    | 304 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let globalSize = source.ReadUInt32()
                        let localSize = source.ReadUInt32()
                        let globalOffset = source.ReadUInt32()
                        yield OpBuildNDRange(resultType, result, globalSize, localSize, globalOffset)
                    | 305 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleImplicitLod(resultType, result, sampledImage, coord, images, variables)
                    | 306 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleExplicitLod(resultType, result, sampledImage, coord, images, variables)
                    | 307 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let depthRef = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleDrefImplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables)
                    | 308 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let depthRef = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleDrefExplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables)
                    | 309 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleProjImplicitLod(resultType, result, sampledImage, coord, images, variables)
                    | 310 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleProjExplicitLod(resultType, result, sampledImage, coord, images, variables)
                    | 311 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let depthRef = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleProjDrefImplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables)
                    | 312 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let depthRef = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleProjDrefExplicitLod(resultType, result, sampledImage, coord, depthRef, images, variables)
                    | 313 ->
                        let variablesSize = max 0 (size - 6)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let image = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseFetch(resultType, result, image, coord, images, variables)
                    | 314 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let _component = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseGather(resultType, result, sampledImage, coord, _component, images, variables)
                    | 315 ->
                        let variablesSize = max 0 (size - 7)
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let sampledImage = source.ReadUInt32()
                        let coord = source.ReadUInt32()
                        let depthRef = source.ReadUInt32()
                        let images = unbox<int> (int (source.ReadUInt32()))
                        let variables = Array.init variablesSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseDrefGather(resultType, result, sampledImage, coord, depthRef, images, variables)
                    | 316 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let residentCode = source.ReadUInt32()
                        yield OpImageSparseTexelsResident(resultType, result, residentCode)
                    | 317 ->
                        yield OpNoLine
                    | 318 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicFlagTestAndSet(resultType, result, ptr, scope, sem)
                    | 319 ->
                        let ptr = source.ReadUInt32()
                        let scope = unbox<Scope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicFlagClear(ptr, scope, sem)
                    | c -> failwithf "invalid OpCode: %d" c
                source.BaseStream.Seek(pos + int64 (4*size), SeekOrigin.Begin) |> ignore
        }


    let write (m : Module) (target : BinaryWriter) = 
        target.Write(m.magic)
        target.Write(m.version)
        target.Write(m.generatorMagic)
        target.Write(m.bound)
        target.Write(m.reserved)
        writeInstructions m.instructions target

    let read (source : BinaryReader) = 
        let magic = source.ReadUInt32()
        let version = source.ReadUInt32()
        let generatorMagic = source.ReadUInt32()
        let bound = source.ReadUInt32()
        let reserved = source.ReadUInt32()
        let instructions = readInstructions source
        { magic = magic; version = version; generatorMagic = generatorMagic; bound = bound; reserved = reserved; instructions = Seq.toList instructions }
