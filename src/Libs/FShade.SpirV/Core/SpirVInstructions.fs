namespace FShade.SpirV
open System
open System.IO
open Aardvark.Base.Monads.Option
type SourceLanguage = 
    | Unknown = 0
    | ESSL = 1
    | GLSL = 2
    | OpenCL_C = 3
    | OpenCL_CPP = 4
    | HLSL = 5
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
    | Initializer = 33
    | Finalizer = 34
    | SubgroupSize = 35
    | SubgroupsPerWorkgroup = 36
    | SubgroupsPerWorkgroupId = 37
    | LocalSizeId = 38
    | LocalSizeHintId = 39
    | PostDepthCoverage = 4446
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
    | StorageBuffer = 12
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
    | ABGR = 19
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
    | None = 0
    | Bias = 1
    | Lod = 2
    | Grad = 4
    | ConstOffset = 8
    | Offset = 16
    | ConstOffsets = 32
    | Sample = 64
    | MinLod = 128
[<Flags>]
type FPFastMathMode = 
    | None = 0
    | NotNaN = 1
    | NotInf = 2
    | NSZ = 4
    | AllowRecip = 8
    | Fast = 16
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
    | MaxByteOffset = 45
    | AlignmentId = 46
    | MaxByteOffsetId = 47
    | ExplicitInterpAMD = 4999
    | OverrideCoverageNV = 5248
    | PassthroughNV = 5250
    | ViewportRelativeNV = 5252
    | SecondaryViewportRelativeNV = 5256
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
    | SubgroupEqMaskKHR = 4416
    | SubgroupGeMaskKHR = 4417
    | SubgroupGtMaskKHR = 4418
    | SubgroupLeMaskKHR = 4419
    | SubgroupLtMaskKHR = 4420
    | BaseVertex = 4424
    | BaseInstance = 4425
    | DrawIndex = 4426
    | DeviceIndex = 4438
    | ViewIndex = 4440
    | BaryCoordNoPerspAMD = 4992
    | BaryCoordNoPerspCentroidAMD = 4993
    | BaryCoordNoPerspSampleAMD = 4994
    | BaryCoordSmoothAMD = 4995
    | BaryCoordSmoothCentroidAMD = 4996
    | BaryCoordSmoothSampleAMD = 4997
    | BaryCoordPullModelAMD = 4998
    | FragStencilRefEXT = 5014
    | ViewportMaskNV = 5253
    | SecondaryPositionNV = 5257
    | SecondaryViewportMaskNV = 5258
    | PositionPerViewNV = 5261
    | ViewportMaskPerViewNV = 5262
[<Flags>]
type SelectionControl = 
    | None = 0
    | Flatten = 1
    | DontFlatten = 2
[<Flags>]
type LoopControl = 
    | None = 0
    | Unroll = 1
    | DontUnroll = 2
    | DependencyInfinite = 4
    | DependencyLength = 8
[<Flags>]
type FunctionControl = 
    | None = 0
    | Inline = 1
    | DontInline = 2
    | Pure = 4
    | Const = 8
[<Flags>]
type MemorySemantics = 
    | None = 0
    | Acquire = 2
    | Release = 4
    | AcquireRelease = 8
    | SequentiallyConsistent = 16
    | UniformMemory = 64
    | SubgroupMemory = 128
    | WorkgroupMemory = 256
    | CrossWorkgroupMemory = 512
    | AtomicCounterMemory = 1024
    | ImageMemory = 2048
[<Flags>]
type MemoryAccess = 
    | None = 0
    | Volatile = 1
    | Aligned = 2
    | Nontemporal = 4
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
    | None = 0
    | CmdExecTime = 1
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
    | MultiViewport = 57
    | SubgroupDispatch = 58
    | NamedBarrier = 59
    | PipeStorage = 60
    | SubgroupBallotKHR = 4423
    | DrawParameters = 4427
    | SubgroupVoteKHR = 4431
    | StorageBuffer16BitAccess = 4433
    | StorageUniformBufferBlock16 = 4433
    | StorageUniform16 = 4434
    | UniformAndStorageBuffer16BitAccess = 4434
    | StoragePushConstant16 = 4435
    | StorageInputOutput16 = 4436
    | DeviceGroup = 4437
    | MultiView = 4439
    | VariablePointersStorageBuffer = 4441
    | VariablePointers = 4442
    | AtomicStorageOps = 4445
    | SampleMaskPostDepthCoverage = 4447
    | ImageGatherBiasLodAMD = 5009
    | StencilExportEXT = 5013
    | SampleMaskOverrideCoverageNV = 5249
    | GeometryShaderPassthroughNV = 5251
    | ShaderViewportIndexLayerEXT = 5254
    | ShaderViewportIndexLayerNV = 5254
    | ShaderViewportMaskNV = 5255
    | ShaderStereoViewNV = 5259
    | PerViewAttributesNV = 5260
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
    | OpImageSparseRead = 320
    | OpSizeOf = 321
    | OpTypePipeStorage = 322
    | OpConstantPipeStorage = 323
    | OpCreatePipeFromPipeStorage = 324
    | OpGetKernelLocalSizeForSubgroupCount = 325
    | OpGetKernelMaxNumSubgroups = 326
    | OpTypeNamedBarrier = 327
    | OpNamedBarrierInitialize = 328
    | OpMemoryNamedBarrier = 329
    | OpModuleProcessed = 330
    | OpExecutionModeId = 331
    | OpDecorateId = 332
    | OpSubgroupBallotKHR = 4421
    | OpSubgroupFirstInvocationKHR = 4422
    | OpSubgroupAllKHR = 4428
    | OpSubgroupAnyKHR = 4429
    | OpSubgroupAllEqualKHR = 4430
    | OpSubgroupReadInvocationKHR = 4432
    | OpGroupIAddNonUniformAMD = 5000
    | OpGroupFAddNonUniformAMD = 5001
    | OpGroupFMinNonUniformAMD = 5002
    | OpGroupUMinNonUniformAMD = 5003
    | OpGroupSMinNonUniformAMD = 5004
    | OpGroupFMaxNonUniformAMD = 5005
    | OpGroupUMaxNonUniformAMD = 5006
    | OpGroupSMaxNonUniformAMD = 5007

type IdResult = uint32
type IdResultType = uint32
type IdRef = uint32
type LiteralExtInstInteger = int
type LiteralSpecConstantOpInteger = int
type PairIdRefIdRef =
    struct
        val mutable public E0 : IdRef
        val mutable public E1 : IdRef
        new(e0, e1) = { E0 = e0; E1 = e1 }
    end
type PairIdRefLiteralInteger =
    struct
        val mutable public IdRef : IdRef
        val mutable public Value : int
        new(id, value) = { IdRef = id; Value = value }
    end
type PairLiteralIntegerIdRef =
    struct
        val mutable public Value : int
        val mutable public IdRef : IdRef
        new(value, id) = { Value = value; IdRef = id }
    end


type Instruction = 
    | OpNop
    | OpUndef of op0 : IdResultType * op1 : IdResult
    | OpSourceContinued of continuedSource : string
    | OpSource of op0 : SourceLanguage * version : int * file : Option<IdRef> * source : Option<string>
    | OpSourceExtension of extension : string
    | OpName of target : IdRef * name : string
    | OpMemberName of _type : IdRef * _member : int * name : string
    | OpString of op0 : IdResult * string : string
    | OpLine of file : IdRef * line : int * column : int
    | OpExtension of name : string
    | OpExtInstImport of op0 : IdResult * name : string
    | OpExtInst of op0 : IdResultType * op1 : IdResult * set : IdRef * instruction : LiteralExtInstInteger * operandOperand : array<IdRef>
    | OpMemoryModel of op0 : AddressingModel * op1 : MemoryModel
    | OpEntryPoint of op0 : ExecutionModel * entryPoint : IdRef * name : string * _interface : array<IdRef>
    | OpExecutionMode of entryPoint : IdRef * mode : ExecutionMode
    | OpCapability of capability : Capability
    | OpTypeVoid of op0 : IdResult
    | OpTypeBool of op0 : IdResult
    | OpTypeInt of op0 : IdResult * width : int * signedness : int
    | OpTypeFloat of op0 : IdResult * width : int
    | OpTypeVector of op0 : IdResult * componentType : IdRef * componentCount : int
    | OpTypeMatrix of op0 : IdResult * columnType : IdRef * columnCount : int
    | OpTypeImage of op0 : IdResult * sampledType : IdRef * op2 : Dim * depth : int * arrayed : int * mS : int * sampled : int * op7 : ImageFormat * op8 : Option<AccessQualifier>
    | OpTypeSampler of op0 : IdResult
    | OpTypeSampledImage of op0 : IdResult * imageType : IdRef
    | OpTypeArray of op0 : IdResult * elementType : IdRef * length : IdRef
    | OpTypeRuntimeArray of op0 : IdResult * elementType : IdRef
    | OpTypeStruct of op0 : IdResult * membertypemembertype : array<IdRef>
    | OpTypeOpaque of op0 : IdResult * thenameoftheopaquetype : string
    | OpTypePointer of op0 : IdResult * op1 : StorageClass * _type : IdRef
    | OpTypeFunction of op0 : IdResult * returnType : IdRef * parameterTypeParameterType : array<IdRef>
    | OpTypeEvent of op0 : IdResult
    | OpTypeDeviceEvent of op0 : IdResult
    | OpTypeReserveId of op0 : IdResult
    | OpTypeQueue of op0 : IdResult
    | OpTypePipe of op0 : IdResult * qualifier : AccessQualifier
    | OpTypeForwardPointer of pointerType : IdRef * op1 : StorageClass
    | OpConstantTrue of op0 : IdResultType * op1 : IdResult
    | OpConstantFalse of op0 : IdResultType * op1 : IdResult
    | OpConstant of op0 : IdResultType * op1 : IdResult * value : array<int>
    | OpConstantComposite of op0 : IdResultType * op1 : IdResult * constituents : array<IdRef>
    | OpConstantSampler of op0 : IdResultType * op1 : IdResult * op2 : SamplerAddressingMode * param : int * op4 : SamplerFilterMode
    | OpConstantNull of op0 : IdResultType * op1 : IdResult
    | OpSpecConstantTrue of op0 : IdResultType * op1 : IdResult
    | OpSpecConstantFalse of op0 : IdResultType * op1 : IdResult
    | OpSpecConstant of op0 : IdResultType * op1 : IdResult * value : array<int>
    | OpSpecConstantComposite of op0 : IdResultType * op1 : IdResult * constituents : array<IdRef>
    | OpSpecConstantOp of op0 : IdResultType * op1 : IdResult * opcode : LiteralSpecConstantOpInteger
    | OpFunction of op0 : IdResultType * op1 : IdResult * op2 : FunctionControl * functionType : IdRef
    | OpFunctionParameter of op0 : IdResultType * op1 : IdResult
    | OpFunctionEnd
    | OpFunctionCall of op0 : IdResultType * op1 : IdResult * _function : IdRef * argumentArgument : array<IdRef>
    | OpVariable of op0 : IdResultType * op1 : IdResult * op2 : StorageClass * initializer : Option<IdRef>
    | OpImageTexelPointer of op0 : IdResultType * op1 : IdResult * image : IdRef * coordinate : IdRef * sample : IdRef
    | OpLoad of op0 : IdResultType * op1 : IdResult * pointer : IdRef * op3 : Option<MemoryAccess>
    | OpStore of pointer : IdRef * _object : IdRef * op2 : Option<MemoryAccess>
    | OpCopyMemory of target : IdRef * source : IdRef * op2 : Option<MemoryAccess>
    | OpCopyMemorySized of target : IdRef * source : IdRef * size : IdRef * op3 : Option<MemoryAccess>
    | OpAccessChain of op0 : IdResultType * op1 : IdResult * _base : IdRef * indexes : array<IdRef>
    | OpInBoundsAccessChain of op0 : IdResultType * op1 : IdResult * _base : IdRef * indexes : array<IdRef>
    | OpPtrAccessChain of op0 : IdResultType * op1 : IdResult * _base : IdRef * element : IdRef * indexes : array<IdRef>
    | OpArrayLength of op0 : IdResultType * op1 : IdResult * structure : IdRef * arraymember : int
    | OpGenericPtrMemSemantics of op0 : IdResultType * op1 : IdResult * pointer : IdRef
    | OpInBoundsPtrAccessChain of op0 : IdResultType * op1 : IdResult * _base : IdRef * element : IdRef * indexes : array<IdRef>
    | OpDecorate of target : IdRef * op1 : Decoration * parameters : array<int>
    | OpMemberDecorate of structureType : IdRef * _member : int * decoration : Decoration * values : array<int>
    | OpDecorationGroup of op0 : IdResult
    | OpGroupDecorate of decorationGroup : IdRef * targets : array<IdRef>
    | OpGroupMemberDecorate of decorationGroup : IdRef * targets : array<PairIdRefLiteralInteger>
    | OpVectorExtractDynamic of op0 : IdResultType * op1 : IdResult * vector : IdRef * index : IdRef
    | OpVectorInsertDynamic of op0 : IdResultType * op1 : IdResult * vector : IdRef * _component : IdRef * index : IdRef
    | OpVectorShuffle of op0 : IdResultType * op1 : IdResult * vector1 : IdRef * vector2 : IdRef * components : array<int>
    | OpCompositeConstruct of op0 : IdResultType * op1 : IdResult * constituents : array<IdRef>
    | OpCompositeExtract of op0 : IdResultType * op1 : IdResult * composite : IdRef * indexes : array<int>
    | OpCompositeInsert of op0 : IdResultType * op1 : IdResult * _object : IdRef * composite : IdRef * indexes : array<int>
    | OpCopyObject of op0 : IdResultType * op1 : IdResult * operand : IdRef
    | OpTranspose of op0 : IdResultType * op1 : IdResult * matrix : IdRef
    | OpSampledImage of op0 : IdResultType * op1 : IdResult * image : IdRef * sampler : IdRef
    | OpImageSampleImplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * op4 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSampleExplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * op4 : ImageOperands * parameters : array<IdRef>
    | OpImageSampleDrefImplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * dref : IdRef * op5 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSampleDrefExplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * dref : IdRef * op5 : ImageOperands * parameters : array<IdRef>
    | OpImageSampleProjImplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * op4 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSampleProjExplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * op4 : ImageOperands * parameters : array<IdRef>
    | OpImageSampleProjDrefImplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * dref : IdRef * op5 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSampleProjDrefExplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * dref : IdRef * op5 : ImageOperands * parameters : array<IdRef>
    | OpImageFetch of op0 : IdResultType * op1 : IdResult * image : IdRef * coordinate : IdRef * op4 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageGather of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * _component : IdRef * op5 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageDrefGather of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * dref : IdRef * op5 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageRead of op0 : IdResultType * op1 : IdResult * image : IdRef * coordinate : IdRef * op4 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageWrite of image : IdRef * coordinate : IdRef * texel : IdRef * op3 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImage of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef
    | OpImageQueryFormat of op0 : IdResultType * op1 : IdResult * image : IdRef
    | OpImageQueryOrder of op0 : IdResultType * op1 : IdResult * image : IdRef
    | OpImageQuerySizeLod of op0 : IdResultType * op1 : IdResult * image : IdRef * levelofDetail : IdRef
    | OpImageQuerySize of op0 : IdResultType * op1 : IdResult * image : IdRef
    | OpImageQueryLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef
    | OpImageQueryLevels of op0 : IdResultType * op1 : IdResult * image : IdRef
    | OpImageQuerySamples of op0 : IdResultType * op1 : IdResult * image : IdRef
    | OpConvertFToU of op0 : IdResultType * op1 : IdResult * floatValue : IdRef
    | OpConvertFToS of op0 : IdResultType * op1 : IdResult * floatValue : IdRef
    | OpConvertSToF of op0 : IdResultType * op1 : IdResult * signedValue : IdRef
    | OpConvertUToF of op0 : IdResultType * op1 : IdResult * unsignedValue : IdRef
    | OpUConvert of op0 : IdResultType * op1 : IdResult * unsignedValue : IdRef
    | OpSConvert of op0 : IdResultType * op1 : IdResult * signedValue : IdRef
    | OpFConvert of op0 : IdResultType * op1 : IdResult * floatValue : IdRef
    | OpQuantizeToF16 of op0 : IdResultType * op1 : IdResult * value : IdRef
    | OpConvertPtrToU of op0 : IdResultType * op1 : IdResult * pointer : IdRef
    | OpSatConvertSToU of op0 : IdResultType * op1 : IdResult * signedValue : IdRef
    | OpSatConvertUToS of op0 : IdResultType * op1 : IdResult * unsignedValue : IdRef
    | OpConvertUToPtr of op0 : IdResultType * op1 : IdResult * integerValue : IdRef
    | OpPtrCastToGeneric of op0 : IdResultType * op1 : IdResult * pointer : IdRef
    | OpGenericCastToPtr of op0 : IdResultType * op1 : IdResult * pointer : IdRef
    | OpGenericCastToPtrExplicit of op0 : IdResultType * op1 : IdResult * pointer : IdRef * storage : StorageClass
    | OpBitcast of op0 : IdResultType * op1 : IdResult * operand : IdRef
    | OpSNegate of op0 : IdResultType * op1 : IdResult * operand : IdRef
    | OpFNegate of op0 : IdResultType * op1 : IdResult * operand : IdRef
    | OpIAdd of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFAdd of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpISub of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFSub of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpIMul of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFMul of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpUDiv of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpSDiv of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFDiv of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpUMod of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpSRem of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpSMod of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFRem of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFMod of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpVectorTimesScalar of op0 : IdResultType * op1 : IdResult * vector : IdRef * scalar : IdRef
    | OpMatrixTimesScalar of op0 : IdResultType * op1 : IdResult * matrix : IdRef * scalar : IdRef
    | OpVectorTimesMatrix of op0 : IdResultType * op1 : IdResult * vector : IdRef * matrix : IdRef
    | OpMatrixTimesVector of op0 : IdResultType * op1 : IdResult * matrix : IdRef * vector : IdRef
    | OpMatrixTimesMatrix of op0 : IdResultType * op1 : IdResult * leftMatrix : IdRef * rightMatrix : IdRef
    | OpOuterProduct of op0 : IdResultType * op1 : IdResult * vector1 : IdRef * vector2 : IdRef
    | OpDot of op0 : IdResultType * op1 : IdResult * vector1 : IdRef * vector2 : IdRef
    | OpIAddCarry of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpISubBorrow of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpUMulExtended of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpSMulExtended of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpAny of op0 : IdResultType * op1 : IdResult * vector : IdRef
    | OpAll of op0 : IdResultType * op1 : IdResult * vector : IdRef
    | OpIsNan of op0 : IdResultType * op1 : IdResult * x : IdRef
    | OpIsInf of op0 : IdResultType * op1 : IdResult * x : IdRef
    | OpIsFinite of op0 : IdResultType * op1 : IdResult * x : IdRef
    | OpIsNormal of op0 : IdResultType * op1 : IdResult * x : IdRef
    | OpSignBitSet of op0 : IdResultType * op1 : IdResult * x : IdRef
    | OpLessOrGreater of op0 : IdResultType * op1 : IdResult * x : IdRef * y : IdRef
    | OpOrdered of op0 : IdResultType * op1 : IdResult * x : IdRef * y : IdRef
    | OpUnordered of op0 : IdResultType * op1 : IdResult * x : IdRef * y : IdRef
    | OpLogicalEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpLogicalNotEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpLogicalOr of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpLogicalAnd of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpLogicalNot of op0 : IdResultType * op1 : IdResult * operand : IdRef
    | OpSelect of op0 : IdResultType * op1 : IdResult * condition : IdRef * _object1 : IdRef * _object2 : IdRef
    | OpIEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpINotEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpUGreaterThan of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpSGreaterThan of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpUGreaterThanEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpSGreaterThanEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpULessThan of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpSLessThan of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpULessThanEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpSLessThanEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFOrdEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFUnordEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFOrdNotEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFUnordNotEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFOrdLessThan of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFUnordLessThan of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFOrdGreaterThan of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFUnordGreaterThan of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFOrdLessThanEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFUnordLessThanEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFOrdGreaterThanEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpFUnordGreaterThanEqual of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpShiftRightLogical of op0 : IdResultType * op1 : IdResult * _base : IdRef * shift : IdRef
    | OpShiftRightArithmetic of op0 : IdResultType * op1 : IdResult * _base : IdRef * shift : IdRef
    | OpShiftLeftLogical of op0 : IdResultType * op1 : IdResult * _base : IdRef * shift : IdRef
    | OpBitwiseOr of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpBitwiseXor of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpBitwiseAnd of op0 : IdResultType * op1 : IdResult * operand1 : IdRef * operand2 : IdRef
    | OpNot of op0 : IdResultType * op1 : IdResult * operand : IdRef
    | OpBitFieldInsert of op0 : IdResultType * op1 : IdResult * _base : IdRef * insert : IdRef * offset : IdRef * count : IdRef
    | OpBitFieldSExtract of op0 : IdResultType * op1 : IdResult * _base : IdRef * offset : IdRef * count : IdRef
    | OpBitFieldUExtract of op0 : IdResultType * op1 : IdResult * _base : IdRef * offset : IdRef * count : IdRef
    | OpBitReverse of op0 : IdResultType * op1 : IdResult * _base : IdRef
    | OpBitCount of op0 : IdResultType * op1 : IdResult * _base : IdRef
    | OpDPdx of op0 : IdResultType * op1 : IdResult * p : IdRef
    | OpDPdy of op0 : IdResultType * op1 : IdResult * p : IdRef
    | OpFwidth of op0 : IdResultType * op1 : IdResult * p : IdRef
    | OpDPdxFine of op0 : IdResultType * op1 : IdResult * p : IdRef
    | OpDPdyFine of op0 : IdResultType * op1 : IdResult * p : IdRef
    | OpFwidthFine of op0 : IdResultType * op1 : IdResult * p : IdRef
    | OpDPdxCoarse of op0 : IdResultType * op1 : IdResult * p : IdRef
    | OpDPdyCoarse of op0 : IdResultType * op1 : IdResult * p : IdRef
    | OpFwidthCoarse of op0 : IdResultType * op1 : IdResult * p : IdRef
    | OpEmitVertex
    | OpEndPrimitive
    | OpEmitStreamVertex of stream : IdRef
    | OpEndStreamPrimitive of stream : IdRef
    | OpControlBarrier of execution : Scope * memory : Scope * semantics : MemorySemantics
    | OpMemoryBarrier of memory : Scope * semantics : MemorySemantics
    | OpAtomicLoad of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics
    | OpAtomicStore of pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpAtomicExchange of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpAtomicCompareExchange of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * equal : MemorySemantics * unequal : MemorySemantics * value : IdRef * comparator : IdRef
    | OpAtomicCompareExchangeWeak of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * equal : MemorySemantics * unequal : MemorySemantics * value : IdRef * comparator : IdRef
    | OpAtomicIIncrement of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics
    | OpAtomicIDecrement of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics
    | OpAtomicIAdd of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpAtomicISub of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpAtomicSMin of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpAtomicUMin of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpAtomicSMax of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpAtomicUMax of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpAtomicAnd of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpAtomicOr of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpAtomicXor of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics * value : IdRef
    | OpPhi of op0 : IdResultType * op1 : IdResult * variableParent : array<PairIdRefIdRef>
    | OpLoopMerge of mergeBlock : IdRef * continueTarget : IdRef * op2 : LoopControl
    | OpSelectionMerge of mergeBlock : IdRef * op1 : SelectionControl
    | OpLabel of op0 : IdResult
    | OpBranch of targetLabel : IdRef
    | OpBranchConditional of condition : IdRef * trueLabel : IdRef * falseLabel : IdRef * branchweights : array<int>
    | OpSwitch of selector : IdRef * _default : IdRef * target : array<PairLiteralIntegerIdRef>
    | OpKill
    | OpReturn
    | OpReturnValue of value : IdRef
    | OpUnreachable
    | OpLifetimeStart of pointer : IdRef * size : int
    | OpLifetimeStop of pointer : IdRef * size : int
    | OpGroupAsyncCopy of op0 : IdResultType * op1 : IdResult * execution : Scope * destination : IdRef * source : IdRef * numElements : IdRef * stride : IdRef * event : IdRef
    | OpGroupWaitEvents of execution : Scope * numEvents : IdRef * eventsList : IdRef
    | OpGroupAll of op0 : IdResultType * op1 : IdResult * execution : Scope * predicate : IdRef
    | OpGroupAny of op0 : IdResultType * op1 : IdResult * execution : Scope * predicate : IdRef
    | OpGroupBroadcast of op0 : IdResultType * op1 : IdResult * execution : Scope * value : IdRef * localId : IdRef
    | OpGroupIAdd of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupFAdd of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupFMin of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupUMin of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupSMin of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupFMax of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupUMax of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupSMax of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpReadPipe of op0 : IdResultType * op1 : IdResult * pipe : IdRef * pointer : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpWritePipe of op0 : IdResultType * op1 : IdResult * pipe : IdRef * pointer : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpReservedReadPipe of op0 : IdResultType * op1 : IdResult * pipe : IdRef * reserveId : IdRef * index : IdRef * pointer : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpReservedWritePipe of op0 : IdResultType * op1 : IdResult * pipe : IdRef * reserveId : IdRef * index : IdRef * pointer : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpReserveReadPipePackets of op0 : IdResultType * op1 : IdResult * pipe : IdRef * numPackets : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpReserveWritePipePackets of op0 : IdResultType * op1 : IdResult * pipe : IdRef * numPackets : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpCommitReadPipe of pipe : IdRef * reserveId : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpCommitWritePipe of pipe : IdRef * reserveId : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpIsValidReserveId of op0 : IdResultType * op1 : IdResult * reserveId : IdRef
    | OpGetNumPipePackets of op0 : IdResultType * op1 : IdResult * pipe : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpGetMaxPipePackets of op0 : IdResultType * op1 : IdResult * pipe : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpGroupReserveReadPipePackets of op0 : IdResultType * op1 : IdResult * execution : Scope * pipe : IdRef * numPackets : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpGroupReserveWritePipePackets of op0 : IdResultType * op1 : IdResult * execution : Scope * pipe : IdRef * numPackets : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpGroupCommitReadPipe of execution : Scope * pipe : IdRef * reserveId : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpGroupCommitWritePipe of execution : Scope * pipe : IdRef * reserveId : IdRef * packetSize : IdRef * packetAlignment : IdRef
    | OpEnqueueMarker of op0 : IdResultType * op1 : IdResult * queue : IdRef * numEvents : IdRef * waitEvents : IdRef * retEvent : IdRef
    | OpEnqueueKernel of op0 : IdResultType * op1 : IdResult * queue : IdRef * flags : IdRef * nDRange : IdRef * numEvents : IdRef * waitEvents : IdRef * retEvent : IdRef * invoke : IdRef * param : IdRef * paramSize : IdRef * paramAlign : IdRef * localSize : array<IdRef>
    | OpGetKernelNDrangeSubGroupCount of op0 : IdResultType * op1 : IdResult * nDRange : IdRef * invoke : IdRef * param : IdRef * paramSize : IdRef * paramAlign : IdRef
    | OpGetKernelNDrangeMaxSubGroupSize of op0 : IdResultType * op1 : IdResult * nDRange : IdRef * invoke : IdRef * param : IdRef * paramSize : IdRef * paramAlign : IdRef
    | OpGetKernelWorkGroupSize of op0 : IdResultType * op1 : IdResult * invoke : IdRef * param : IdRef * paramSize : IdRef * paramAlign : IdRef
    | OpGetKernelPreferredWorkGroupSizeMultiple of op0 : IdResultType * op1 : IdResult * invoke : IdRef * param : IdRef * paramSize : IdRef * paramAlign : IdRef
    | OpRetainEvent of event : IdRef
    | OpReleaseEvent of event : IdRef
    | OpCreateUserEvent of op0 : IdResultType * op1 : IdResult
    | OpIsValidEvent of op0 : IdResultType * op1 : IdResult * event : IdRef
    | OpSetUserEventStatus of event : IdRef * status : IdRef
    | OpCaptureEventProfilingInfo of event : IdRef * profilingInfo : IdRef * value : IdRef
    | OpGetDefaultQueue of op0 : IdResultType * op1 : IdResult
    | OpBuildNDRange of op0 : IdResultType * op1 : IdResult * globalWorkSize : IdRef * localWorkSize : IdRef * globalWorkOffset : IdRef
    | OpImageSparseSampleImplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * op4 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSparseSampleExplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * op4 : ImageOperands * parameters : array<IdRef>
    | OpImageSparseSampleDrefImplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * dref : IdRef * op5 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSparseSampleDrefExplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * dref : IdRef * op5 : ImageOperands * parameters : array<IdRef>
    | OpImageSparseSampleProjImplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * op4 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSparseSampleProjExplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * op4 : ImageOperands * parameters : array<IdRef>
    | OpImageSparseSampleProjDrefImplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * dref : IdRef * op5 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSparseSampleProjDrefExplicitLod of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * dref : IdRef * op5 : ImageOperands * parameters : array<IdRef>
    | OpImageSparseFetch of op0 : IdResultType * op1 : IdResult * image : IdRef * coordinate : IdRef * op4 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSparseGather of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * _component : IdRef * op5 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSparseDrefGather of op0 : IdResultType * op1 : IdResult * sampledImage : IdRef * coordinate : IdRef * dref : IdRef * op5 : Option<ImageOperands> * parameters : array<IdRef>
    | OpImageSparseTexelsResident of op0 : IdResultType * op1 : IdResult * residentCode : IdRef
    | OpNoLine
    | OpAtomicFlagTestAndSet of op0 : IdResultType * op1 : IdResult * pointer : IdRef * scope : Scope * semantics : MemorySemantics
    | OpAtomicFlagClear of pointer : IdRef * scope : Scope * semantics : MemorySemantics
    | OpImageSparseRead of op0 : IdResultType * op1 : IdResult * image : IdRef * coordinate : IdRef * op4 : Option<ImageOperands> * parameters : array<IdRef>
    | OpSizeOf of op0 : IdResultType * op1 : IdResult * pointer : IdRef
    | OpTypePipeStorage of op0 : IdResult
    | OpConstantPipeStorage of op0 : IdResultType * op1 : IdResult * packetSize : int * packetAlignment : int * capacity : int
    | OpCreatePipeFromPipeStorage of op0 : IdResultType * op1 : IdResult * pipeStorage : IdRef
    | OpGetKernelLocalSizeForSubgroupCount of op0 : IdResultType * op1 : IdResult * subgroupCount : IdRef * invoke : IdRef * param : IdRef * paramSize : IdRef * paramAlign : IdRef
    | OpGetKernelMaxNumSubgroups of op0 : IdResultType * op1 : IdResult * invoke : IdRef * param : IdRef * paramSize : IdRef * paramAlign : IdRef
    | OpTypeNamedBarrier of op0 : IdResult
    | OpNamedBarrierInitialize of op0 : IdResultType * op1 : IdResult * subgroupCount : IdRef
    | OpMemoryNamedBarrier of namedBarrier : IdRef * memory : Scope * semantics : MemorySemantics
    | OpModuleProcessed of _process : string
    | OpExecutionModeId of entryPoint : IdRef * mode : ExecutionMode
    | OpDecorateId of target : IdRef * op1 : Decoration
    | OpSubgroupBallotKHR of op0 : IdResultType * op1 : IdResult * predicate : IdRef
    | OpSubgroupFirstInvocationKHR of op0 : IdResultType * op1 : IdResult * value : IdRef
    | OpSubgroupAllKHR of op0 : IdResultType * op1 : IdResult * predicate : IdRef
    | OpSubgroupAnyKHR of op0 : IdResultType * op1 : IdResult * predicate : IdRef
    | OpSubgroupAllEqualKHR of op0 : IdResultType * op1 : IdResult * predicate : IdRef
    | OpSubgroupReadInvocationKHR of op0 : IdResultType * op1 : IdResult * value : IdRef * index : IdRef
    | OpGroupIAddNonUniformAMD of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupFAddNonUniformAMD of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupFMinNonUniformAMD of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupUMinNonUniformAMD of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupSMinNonUniformAMD of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupFMaxNonUniformAMD of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupUMaxNonUniformAMD of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef
    | OpGroupSMaxNonUniformAMD of op0 : IdResultType * op1 : IdResult * execution : Scope * operation : GroupOperation * x : IdRef

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Instruction =
    [<Literal>]
    let Magic = 0x7230203u
    [<Literal>]
    let Version = 66048u
    [<Literal>]
    let Revision = 2u

    let private div4 (v : int) =
        if v % 4 = 0 then v / 4
        else 1 + v / 4

    type BinaryWriter with
        member inline x.Write<'a when 'a : enum<int> and 'a : (static member op_Explicit : 'a -> int)>(value : 'a) =
            x.Write(int value)
        member inline x.Write(o :PairIdRefLiteralInteger) =
            x.Write(o.IdRef)
            x.Write(o.Value)
        member inline x.Write(o :PairLiteralIntegerIdRef) =
            x.Write(o.Value)
            x.Write(o.IdRef)
        member inline x.Write(o :PairIdRefIdRef) =
            x.Write(o.E0)
            x.Write(o.E1)

        member inline x.Write(o : Option<int>) =
            match o with | Some o -> x.Write o | _ -> ()
        member inline x.Write(o : Option<uint32>) =
            match o with | Some o -> x.Write o | _ -> ()
        member inline x.Write<'a when 'a : enum<int> and 'a : (static member op_Explicit : 'a -> int)>(value : Option<'a>) =
            match value with | Some value -> x.Write(int value) | None -> ()

        member inline x.WriteString(str : string) =
            let len = str.Length + 1
            x.Write(str.ToCharArray())
            x.Write(char 0)
            if len % 4 <> 0 then
                for m in 1 .. (4 - len % 4) do x.Write(0uy)
        member inline x.WriteString(o : Option<string>) =
            match o with | Some o -> x.WriteString o | _ -> ()

        member inline x.WriteArray(arr : int[]) =
            for a in arr do x.Write(a)
        member inline x.WriteArray(arr : uint32[]) =
            for a in arr do x.Write(a)

        member inline x.WriteArray(arr : PairIdRefLiteralInteger[]) =
            for a in arr do x.Write(a)
        member inline x.WriteArray(arr : PairLiteralIntegerIdRef[]) =
            for a in arr do x.Write(a)
        member inline x.WriteArray(arr : PairIdRefIdRef[]) =
            for a in arr do x.Write(a)

    let writeTo (writer : BinaryWriter) (i : Instruction) = 
        match i with
            | OpNop ->
                // size: 1, opcode: 0
                writer.Write(0x10000u)
            | OpUndef(op0, op1) ->
                // size: 3, opcode: 1
                writer.Write(0x30001u)
                writer.Write(op0)
                writer.Write(op1)
            | OpSourceContinued(continuedSource) ->
                // opcode: 2
                let _size = 1 + div4 (1 + continuedSource.Length)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 2u)
                writer.WriteString(continuedSource)
            | OpSource(op0, version, file, source) ->
                // opcode: 3
                let _size = 3 + (if Option.isSome file then 1 else 0) + (match source with | Some str -> div4 (1 + str.Length) | None -> 0)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 3u)
                writer.Write(op0)
                writer.Write(version)
                writer.Write(file)
                writer.WriteString(source)
            | OpSourceExtension(extension) ->
                // opcode: 4
                let _size = 1 + div4 (1 + extension.Length)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 4u)
                writer.WriteString(extension)
            | OpName(target, name) ->
                // opcode: 5
                let _size = 2 + div4 (1 + name.Length)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 5u)
                writer.Write(target)
                writer.WriteString(name)
            | OpMemberName(_type, _member, name) ->
                // opcode: 6
                let _size = 3 + div4 (1 + name.Length)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 6u)
                writer.Write(_type)
                writer.Write(_member)
                writer.WriteString(name)
            | OpString(op0, string) ->
                // opcode: 7
                let _size = 2 + div4 (1 + string.Length)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 7u)
                writer.Write(op0)
                writer.WriteString(string)
            | OpLine(file, line, column) ->
                // size: 4, opcode: 8
                writer.Write(0x40008u)
                writer.Write(file)
                writer.Write(line)
                writer.Write(column)
            | OpExtension(name) ->
                // opcode: 10
                let _size = 1 + div4 (1 + name.Length)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 10u)
                writer.WriteString(name)
            | OpExtInstImport(op0, name) ->
                // opcode: 11
                let _size = 2 + div4 (1 + name.Length)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 11u)
                writer.Write(op0)
                writer.WriteString(name)
            | OpExtInst(op0, op1, set, instruction, operandOperand) ->
                // opcode: 12
                let _size = 5 + (operandOperand.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 12u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(set)
                writer.Write(instruction)
                writer.WriteArray(operandOperand)
            | OpMemoryModel(op0, op1) ->
                // size: 3, opcode: 14
                writer.Write(0x3000Eu)
                writer.Write(op0)
                writer.Write(op1)
            | OpEntryPoint(op0, entryPoint, name, _interface) ->
                // opcode: 15
                let _size = 3 + div4 (1 + name.Length) + (_interface.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 15u)
                writer.Write(op0)
                writer.Write(entryPoint)
                writer.WriteString(name)
                writer.WriteArray(_interface)
            | OpExecutionMode(entryPoint, mode) ->
                // size: 3, opcode: 16
                writer.Write(0x30010u)
                writer.Write(entryPoint)
                writer.Write(mode)
            | OpCapability(capability) ->
                // size: 2, opcode: 17
                writer.Write(0x20011u)
                writer.Write(capability)
            | OpTypeVoid(op0) ->
                // size: 2, opcode: 19
                writer.Write(0x20013u)
                writer.Write(op0)
            | OpTypeBool(op0) ->
                // size: 2, opcode: 20
                writer.Write(0x20014u)
                writer.Write(op0)
            | OpTypeInt(op0, width, signedness) ->
                // size: 4, opcode: 21
                writer.Write(0x40015u)
                writer.Write(op0)
                writer.Write(width)
                writer.Write(signedness)
            | OpTypeFloat(op0, width) ->
                // size: 3, opcode: 22
                writer.Write(0x30016u)
                writer.Write(op0)
                writer.Write(width)
            | OpTypeVector(op0, componentType, componentCount) ->
                // size: 4, opcode: 23
                writer.Write(0x40017u)
                writer.Write(op0)
                writer.Write(componentType)
                writer.Write(componentCount)
            | OpTypeMatrix(op0, columnType, columnCount) ->
                // size: 4, opcode: 24
                writer.Write(0x40018u)
                writer.Write(op0)
                writer.Write(columnType)
                writer.Write(columnCount)
            | OpTypeImage(op0, sampledType, op2, depth, arrayed, mS, sampled, op7, op8) ->
                // opcode: 25
                let _size = 9 + (if Option.isSome op8 then 1 else 0)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 25u)
                writer.Write(op0)
                writer.Write(sampledType)
                writer.Write(op2)
                writer.Write(depth)
                writer.Write(arrayed)
                writer.Write(mS)
                writer.Write(sampled)
                writer.Write(op7)
                writer.Write(op8)
            | OpTypeSampler(op0) ->
                // size: 2, opcode: 26
                writer.Write(0x2001Au)
                writer.Write(op0)
            | OpTypeSampledImage(op0, imageType) ->
                // size: 3, opcode: 27
                writer.Write(0x3001Bu)
                writer.Write(op0)
                writer.Write(imageType)
            | OpTypeArray(op0, elementType, length) ->
                // size: 4, opcode: 28
                writer.Write(0x4001Cu)
                writer.Write(op0)
                writer.Write(elementType)
                writer.Write(length)
            | OpTypeRuntimeArray(op0, elementType) ->
                // size: 3, opcode: 29
                writer.Write(0x3001Du)
                writer.Write(op0)
                writer.Write(elementType)
            | OpTypeStruct(op0, membertypemembertype) ->
                // opcode: 30
                let _size = 2 + (membertypemembertype.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 30u)
                writer.Write(op0)
                writer.WriteArray(membertypemembertype)
            | OpTypeOpaque(op0, thenameoftheopaquetype) ->
                // opcode: 31
                let _size = 2 + div4 (1 + thenameoftheopaquetype.Length)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 31u)
                writer.Write(op0)
                writer.WriteString(thenameoftheopaquetype)
            | OpTypePointer(op0, op1, _type) ->
                // size: 4, opcode: 32
                writer.Write(0x40020u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_type)
            | OpTypeFunction(op0, returnType, parameterTypeParameterType) ->
                // opcode: 33
                let _size = 3 + (parameterTypeParameterType.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 33u)
                writer.Write(op0)
                writer.Write(returnType)
                writer.WriteArray(parameterTypeParameterType)
            | OpTypeEvent(op0) ->
                // size: 2, opcode: 34
                writer.Write(0x20022u)
                writer.Write(op0)
            | OpTypeDeviceEvent(op0) ->
                // size: 2, opcode: 35
                writer.Write(0x20023u)
                writer.Write(op0)
            | OpTypeReserveId(op0) ->
                // size: 2, opcode: 36
                writer.Write(0x20024u)
                writer.Write(op0)
            | OpTypeQueue(op0) ->
                // size: 2, opcode: 37
                writer.Write(0x20025u)
                writer.Write(op0)
            | OpTypePipe(op0, qualifier) ->
                // size: 3, opcode: 38
                writer.Write(0x30026u)
                writer.Write(op0)
                writer.Write(qualifier)
            | OpTypeForwardPointer(pointerType, op1) ->
                // size: 3, opcode: 39
                writer.Write(0x30027u)
                writer.Write(pointerType)
                writer.Write(op1)
            | OpConstantTrue(op0, op1) ->
                // size: 3, opcode: 41
                writer.Write(0x30029u)
                writer.Write(op0)
                writer.Write(op1)
            | OpConstantFalse(op0, op1) ->
                // size: 3, opcode: 42
                writer.Write(0x3002Au)
                writer.Write(op0)
                writer.Write(op1)
            | OpConstant(op0, op1, value) ->
                // opcode: 43
                let _size = 3 + (value.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 43u)
                writer.Write(op0)
                writer.Write(op1)
                writer.WriteArray(value)
            | OpConstantComposite(op0, op1, constituents) ->
                // opcode: 44
                let _size = 3 + (constituents.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 44u)
                writer.Write(op0)
                writer.Write(op1)
                writer.WriteArray(constituents)
            | OpConstantSampler(op0, op1, op2, param, op4) ->
                // size: 6, opcode: 45
                writer.Write(0x6002Du)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(op2)
                writer.Write(param)
                writer.Write(op4)
            | OpConstantNull(op0, op1) ->
                // size: 3, opcode: 46
                writer.Write(0x3002Eu)
                writer.Write(op0)
                writer.Write(op1)
            | OpSpecConstantTrue(op0, op1) ->
                // size: 3, opcode: 48
                writer.Write(0x30030u)
                writer.Write(op0)
                writer.Write(op1)
            | OpSpecConstantFalse(op0, op1) ->
                // size: 3, opcode: 49
                writer.Write(0x30031u)
                writer.Write(op0)
                writer.Write(op1)
            | OpSpecConstant(op0, op1, value) ->
                // opcode: 50
                let _size = 3 + (value.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 50u)
                writer.Write(op0)
                writer.Write(op1)
                writer.WriteArray(value)
            | OpSpecConstantComposite(op0, op1, constituents) ->
                // opcode: 51
                let _size = 3 + (constituents.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 51u)
                writer.Write(op0)
                writer.Write(op1)
                writer.WriteArray(constituents)
            | OpSpecConstantOp(op0, op1, opcode) ->
                // size: 4, opcode: 52
                writer.Write(0x40034u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(opcode)
            | OpFunction(op0, op1, op2, functionType) ->
                // size: 5, opcode: 54
                writer.Write(0x50036u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(op2)
                writer.Write(functionType)
            | OpFunctionParameter(op0, op1) ->
                // size: 3, opcode: 55
                writer.Write(0x30037u)
                writer.Write(op0)
                writer.Write(op1)
            | OpFunctionEnd ->
                // size: 1, opcode: 56
                writer.Write(0x10038u)
            | OpFunctionCall(op0, op1, _function, argumentArgument) ->
                // opcode: 57
                let _size = 4 + (argumentArgument.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 57u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_function)
                writer.WriteArray(argumentArgument)
            | OpVariable(op0, op1, op2, initializer) ->
                // opcode: 59
                let _size = 4 + (if Option.isSome initializer then 1 else 0)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 59u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(op2)
                writer.Write(initializer)
            | OpImageTexelPointer(op0, op1, image, coordinate, sample) ->
                // size: 6, opcode: 60
                writer.Write(0x6003Cu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
                writer.Write(coordinate)
                writer.Write(sample)
            | OpLoad(op0, op1, pointer, op3) ->
                // opcode: 61
                let _size = 4 + (if Option.isSome op3 then 1 else 0)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 61u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(op3)
            | OpStore(pointer, _object, op2) ->
                // opcode: 62
                let _size = 3 + (if Option.isSome op2 then 1 else 0)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 62u)
                writer.Write(pointer)
                writer.Write(_object)
                writer.Write(op2)
            | OpCopyMemory(target, source, op2) ->
                // opcode: 63
                let _size = 3 + (if Option.isSome op2 then 1 else 0)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 63u)
                writer.Write(target)
                writer.Write(source)
                writer.Write(op2)
            | OpCopyMemorySized(target, source, size, op3) ->
                // opcode: 64
                let _size = 4 + (if Option.isSome op3 then 1 else 0)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 64u)
                writer.Write(target)
                writer.Write(source)
                writer.Write(size)
                writer.Write(op3)
            | OpAccessChain(op0, op1, _base, indexes) ->
                // opcode: 65
                let _size = 4 + (indexes.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 65u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
                writer.WriteArray(indexes)
            | OpInBoundsAccessChain(op0, op1, _base, indexes) ->
                // opcode: 66
                let _size = 4 + (indexes.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 66u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
                writer.WriteArray(indexes)
            | OpPtrAccessChain(op0, op1, _base, element, indexes) ->
                // opcode: 67
                let _size = 5 + (indexes.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 67u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
                writer.Write(element)
                writer.WriteArray(indexes)
            | OpArrayLength(op0, op1, structure, arraymember) ->
                // size: 5, opcode: 68
                writer.Write(0x50044u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(structure)
                writer.Write(arraymember)
            | OpGenericPtrMemSemantics(op0, op1, pointer) ->
                // size: 4, opcode: 69
                writer.Write(0x40045u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
            | OpInBoundsPtrAccessChain(op0, op1, _base, element, indexes) ->
                // opcode: 70
                let _size = 5 + (indexes.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 70u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
                writer.Write(element)
                writer.WriteArray(indexes)
            | OpDecorate(target, op1, parameters) ->
                // opcode: 71
                let _size = 3 + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 71u)
                writer.Write(target)
                writer.Write(op1)
                writer.WriteArray(parameters)
            | OpMemberDecorate(structureType, _member, decoration, values) ->
                // opcode: 72
                let _size = 4 + (values.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 72u)
                writer.Write(structureType)
                writer.Write(_member)
                writer.Write(decoration)
                writer.WriteArray(values)
            | OpDecorationGroup(op0) ->
                // size: 2, opcode: 73
                writer.Write(0x20049u)
                writer.Write(op0)
            | OpGroupDecorate(decorationGroup, targets) ->
                // opcode: 74
                let _size = 2 + (targets.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 74u)
                writer.Write(decorationGroup)
                writer.WriteArray(targets)
            | OpGroupMemberDecorate(decorationGroup, targets) ->
                // opcode: 75
                let _size = 2 + (targets.Length * 2)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 75u)
                writer.Write(decorationGroup)
                writer.WriteArray(targets)
            | OpVectorExtractDynamic(op0, op1, vector, index) ->
                // size: 5, opcode: 77
                writer.Write(0x5004Du)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(vector)
                writer.Write(index)
            | OpVectorInsertDynamic(op0, op1, vector, _component, index) ->
                // size: 6, opcode: 78
                writer.Write(0x6004Eu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(vector)
                writer.Write(_component)
                writer.Write(index)
            | OpVectorShuffle(op0, op1, vector1, vector2, components) ->
                // opcode: 79
                let _size = 5 + (components.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 79u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(vector1)
                writer.Write(vector2)
                writer.WriteArray(components)
            | OpCompositeConstruct(op0, op1, constituents) ->
                // opcode: 80
                let _size = 3 + (constituents.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 80u)
                writer.Write(op0)
                writer.Write(op1)
                writer.WriteArray(constituents)
            | OpCompositeExtract(op0, op1, composite, indexes) ->
                // opcode: 81
                let _size = 4 + (indexes.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 81u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(composite)
                writer.WriteArray(indexes)
            | OpCompositeInsert(op0, op1, _object, composite, indexes) ->
                // opcode: 82
                let _size = 5 + (indexes.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 82u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_object)
                writer.Write(composite)
                writer.WriteArray(indexes)
            | OpCopyObject(op0, op1, operand) ->
                // size: 4, opcode: 83
                writer.Write(0x40053u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand)
            | OpTranspose(op0, op1, matrix) ->
                // size: 4, opcode: 84
                writer.Write(0x40054u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(matrix)
            | OpSampledImage(op0, op1, image, sampler) ->
                // size: 5, opcode: 86
                writer.Write(0x50056u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
                writer.Write(sampler)
            | OpImageSampleImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) ->
                // opcode: 87
                let _size = 5 + (if Option.isSome op4 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 87u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageSampleExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) ->
                // opcode: 88
                let _size = 6 + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 88u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageSampleDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) ->
                // opcode: 89
                let _size = 6 + (if Option.isSome op5 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 89u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(dref)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageSampleDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) ->
                // opcode: 90
                let _size = 7 + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 90u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(dref)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageSampleProjImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) ->
                // opcode: 91
                let _size = 5 + (if Option.isSome op4 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 91u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageSampleProjExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) ->
                // opcode: 92
                let _size = 6 + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 92u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageSampleProjDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) ->
                // opcode: 93
                let _size = 6 + (if Option.isSome op5 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 93u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(dref)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageSampleProjDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) ->
                // opcode: 94
                let _size = 7 + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 94u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(dref)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageFetch(op0, op1, image, coordinate, op4, parameters) ->
                // opcode: 95
                let _size = 5 + (if Option.isSome op4 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 95u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageGather(op0, op1, sampledImage, coordinate, _component, op5, parameters) ->
                // opcode: 96
                let _size = 6 + (if Option.isSome op5 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 96u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(_component)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageDrefGather(op0, op1, sampledImage, coordinate, dref, op5, parameters) ->
                // opcode: 97
                let _size = 6 + (if Option.isSome op5 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 97u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(dref)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageRead(op0, op1, image, coordinate, op4, parameters) ->
                // opcode: 98
                let _size = 5 + (if Option.isSome op4 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 98u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageWrite(image, coordinate, texel, op3, parameters) ->
                // opcode: 99
                let _size = 4 + (if Option.isSome op3 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 99u)
                writer.Write(image)
                writer.Write(coordinate)
                writer.Write(texel)
                writer.Write(op3)
                writer.WriteArray(parameters)
            | OpImage(op0, op1, sampledImage) ->
                // size: 4, opcode: 100
                writer.Write(0x40064u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
            | OpImageQueryFormat(op0, op1, image) ->
                // size: 4, opcode: 101
                writer.Write(0x40065u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
            | OpImageQueryOrder(op0, op1, image) ->
                // size: 4, opcode: 102
                writer.Write(0x40066u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
            | OpImageQuerySizeLod(op0, op1, image, levelofDetail) ->
                // size: 5, opcode: 103
                writer.Write(0x50067u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
                writer.Write(levelofDetail)
            | OpImageQuerySize(op0, op1, image) ->
                // size: 4, opcode: 104
                writer.Write(0x40068u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
            | OpImageQueryLod(op0, op1, sampledImage, coordinate) ->
                // size: 5, opcode: 105
                writer.Write(0x50069u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
            | OpImageQueryLevels(op0, op1, image) ->
                // size: 4, opcode: 106
                writer.Write(0x4006Au)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
            | OpImageQuerySamples(op0, op1, image) ->
                // size: 4, opcode: 107
                writer.Write(0x4006Bu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
            | OpConvertFToU(op0, op1, floatValue) ->
                // size: 4, opcode: 109
                writer.Write(0x4006Du)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(floatValue)
            | OpConvertFToS(op0, op1, floatValue) ->
                // size: 4, opcode: 110
                writer.Write(0x4006Eu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(floatValue)
            | OpConvertSToF(op0, op1, signedValue) ->
                // size: 4, opcode: 111
                writer.Write(0x4006Fu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(signedValue)
            | OpConvertUToF(op0, op1, unsignedValue) ->
                // size: 4, opcode: 112
                writer.Write(0x40070u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(unsignedValue)
            | OpUConvert(op0, op1, unsignedValue) ->
                // size: 4, opcode: 113
                writer.Write(0x40071u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(unsignedValue)
            | OpSConvert(op0, op1, signedValue) ->
                // size: 4, opcode: 114
                writer.Write(0x40072u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(signedValue)
            | OpFConvert(op0, op1, floatValue) ->
                // size: 4, opcode: 115
                writer.Write(0x40073u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(floatValue)
            | OpQuantizeToF16(op0, op1, value) ->
                // size: 4, opcode: 116
                writer.Write(0x40074u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(value)
            | OpConvertPtrToU(op0, op1, pointer) ->
                // size: 4, opcode: 117
                writer.Write(0x40075u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
            | OpSatConvertSToU(op0, op1, signedValue) ->
                // size: 4, opcode: 118
                writer.Write(0x40076u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(signedValue)
            | OpSatConvertUToS(op0, op1, unsignedValue) ->
                // size: 4, opcode: 119
                writer.Write(0x40077u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(unsignedValue)
            | OpConvertUToPtr(op0, op1, integerValue) ->
                // size: 4, opcode: 120
                writer.Write(0x40078u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(integerValue)
            | OpPtrCastToGeneric(op0, op1, pointer) ->
                // size: 4, opcode: 121
                writer.Write(0x40079u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
            | OpGenericCastToPtr(op0, op1, pointer) ->
                // size: 4, opcode: 122
                writer.Write(0x4007Au)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
            | OpGenericCastToPtrExplicit(op0, op1, pointer, storage) ->
                // size: 5, opcode: 123
                writer.Write(0x5007Bu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(storage)
            | OpBitcast(op0, op1, operand) ->
                // size: 4, opcode: 124
                writer.Write(0x4007Cu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand)
            | OpSNegate(op0, op1, operand) ->
                // size: 4, opcode: 126
                writer.Write(0x4007Eu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand)
            | OpFNegate(op0, op1, operand) ->
                // size: 4, opcode: 127
                writer.Write(0x4007Fu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand)
            | OpIAdd(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 128
                writer.Write(0x50080u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFAdd(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 129
                writer.Write(0x50081u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpISub(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 130
                writer.Write(0x50082u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFSub(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 131
                writer.Write(0x50083u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpIMul(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 132
                writer.Write(0x50084u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFMul(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 133
                writer.Write(0x50085u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpUDiv(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 134
                writer.Write(0x50086u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpSDiv(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 135
                writer.Write(0x50087u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFDiv(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 136
                writer.Write(0x50088u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpUMod(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 137
                writer.Write(0x50089u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpSRem(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 138
                writer.Write(0x5008Au)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpSMod(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 139
                writer.Write(0x5008Bu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFRem(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 140
                writer.Write(0x5008Cu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFMod(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 141
                writer.Write(0x5008Du)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpVectorTimesScalar(op0, op1, vector, scalar) ->
                // size: 5, opcode: 142
                writer.Write(0x5008Eu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(vector)
                writer.Write(scalar)
            | OpMatrixTimesScalar(op0, op1, matrix, scalar) ->
                // size: 5, opcode: 143
                writer.Write(0x5008Fu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(matrix)
                writer.Write(scalar)
            | OpVectorTimesMatrix(op0, op1, vector, matrix) ->
                // size: 5, opcode: 144
                writer.Write(0x50090u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(vector)
                writer.Write(matrix)
            | OpMatrixTimesVector(op0, op1, matrix, vector) ->
                // size: 5, opcode: 145
                writer.Write(0x50091u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(matrix)
                writer.Write(vector)
            | OpMatrixTimesMatrix(op0, op1, leftMatrix, rightMatrix) ->
                // size: 5, opcode: 146
                writer.Write(0x50092u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(leftMatrix)
                writer.Write(rightMatrix)
            | OpOuterProduct(op0, op1, vector1, vector2) ->
                // size: 5, opcode: 147
                writer.Write(0x50093u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(vector1)
                writer.Write(vector2)
            | OpDot(op0, op1, vector1, vector2) ->
                // size: 5, opcode: 148
                writer.Write(0x50094u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(vector1)
                writer.Write(vector2)
            | OpIAddCarry(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 149
                writer.Write(0x50095u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpISubBorrow(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 150
                writer.Write(0x50096u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpUMulExtended(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 151
                writer.Write(0x50097u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpSMulExtended(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 152
                writer.Write(0x50098u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpAny(op0, op1, vector) ->
                // size: 4, opcode: 154
                writer.Write(0x4009Au)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(vector)
            | OpAll(op0, op1, vector) ->
                // size: 4, opcode: 155
                writer.Write(0x4009Bu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(vector)
            | OpIsNan(op0, op1, x) ->
                // size: 4, opcode: 156
                writer.Write(0x4009Cu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(x)
            | OpIsInf(op0, op1, x) ->
                // size: 4, opcode: 157
                writer.Write(0x4009Du)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(x)
            | OpIsFinite(op0, op1, x) ->
                // size: 4, opcode: 158
                writer.Write(0x4009Eu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(x)
            | OpIsNormal(op0, op1, x) ->
                // size: 4, opcode: 159
                writer.Write(0x4009Fu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(x)
            | OpSignBitSet(op0, op1, x) ->
                // size: 4, opcode: 160
                writer.Write(0x400A0u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(x)
            | OpLessOrGreater(op0, op1, x, y) ->
                // size: 5, opcode: 161
                writer.Write(0x500A1u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(x)
                writer.Write(y)
            | OpOrdered(op0, op1, x, y) ->
                // size: 5, opcode: 162
                writer.Write(0x500A2u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(x)
                writer.Write(y)
            | OpUnordered(op0, op1, x, y) ->
                // size: 5, opcode: 163
                writer.Write(0x500A3u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(x)
                writer.Write(y)
            | OpLogicalEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 164
                writer.Write(0x500A4u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpLogicalNotEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 165
                writer.Write(0x500A5u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpLogicalOr(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 166
                writer.Write(0x500A6u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpLogicalAnd(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 167
                writer.Write(0x500A7u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpLogicalNot(op0, op1, operand) ->
                // size: 4, opcode: 168
                writer.Write(0x400A8u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand)
            | OpSelect(op0, op1, condition, _object1, _object2) ->
                // size: 6, opcode: 169
                writer.Write(0x600A9u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(condition)
                writer.Write(_object1)
                writer.Write(_object2)
            | OpIEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 170
                writer.Write(0x500AAu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpINotEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 171
                writer.Write(0x500ABu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpUGreaterThan(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 172
                writer.Write(0x500ACu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpSGreaterThan(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 173
                writer.Write(0x500ADu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpUGreaterThanEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 174
                writer.Write(0x500AEu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpSGreaterThanEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 175
                writer.Write(0x500AFu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpULessThan(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 176
                writer.Write(0x500B0u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpSLessThan(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 177
                writer.Write(0x500B1u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpULessThanEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 178
                writer.Write(0x500B2u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpSLessThanEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 179
                writer.Write(0x500B3u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFOrdEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 180
                writer.Write(0x500B4u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFUnordEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 181
                writer.Write(0x500B5u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFOrdNotEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 182
                writer.Write(0x500B6u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFUnordNotEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 183
                writer.Write(0x500B7u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFOrdLessThan(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 184
                writer.Write(0x500B8u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFUnordLessThan(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 185
                writer.Write(0x500B9u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFOrdGreaterThan(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 186
                writer.Write(0x500BAu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFUnordGreaterThan(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 187
                writer.Write(0x500BBu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFOrdLessThanEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 188
                writer.Write(0x500BCu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFUnordLessThanEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 189
                writer.Write(0x500BDu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFOrdGreaterThanEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 190
                writer.Write(0x500BEu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpFUnordGreaterThanEqual(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 191
                writer.Write(0x500BFu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpShiftRightLogical(op0, op1, _base, shift) ->
                // size: 5, opcode: 194
                writer.Write(0x500C2u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
                writer.Write(shift)
            | OpShiftRightArithmetic(op0, op1, _base, shift) ->
                // size: 5, opcode: 195
                writer.Write(0x500C3u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
                writer.Write(shift)
            | OpShiftLeftLogical(op0, op1, _base, shift) ->
                // size: 5, opcode: 196
                writer.Write(0x500C4u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
                writer.Write(shift)
            | OpBitwiseOr(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 197
                writer.Write(0x500C5u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpBitwiseXor(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 198
                writer.Write(0x500C6u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpBitwiseAnd(op0, op1, operand1, operand2) ->
                // size: 5, opcode: 199
                writer.Write(0x500C7u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand1)
                writer.Write(operand2)
            | OpNot(op0, op1, operand) ->
                // size: 4, opcode: 200
                writer.Write(0x400C8u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(operand)
            | OpBitFieldInsert(op0, op1, _base, insert, offset, count) ->
                // size: 7, opcode: 201
                writer.Write(0x700C9u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
                writer.Write(insert)
                writer.Write(offset)
                writer.Write(count)
            | OpBitFieldSExtract(op0, op1, _base, offset, count) ->
                // size: 6, opcode: 202
                writer.Write(0x600CAu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
                writer.Write(offset)
                writer.Write(count)
            | OpBitFieldUExtract(op0, op1, _base, offset, count) ->
                // size: 6, opcode: 203
                writer.Write(0x600CBu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
                writer.Write(offset)
                writer.Write(count)
            | OpBitReverse(op0, op1, _base) ->
                // size: 4, opcode: 204
                writer.Write(0x400CCu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
            | OpBitCount(op0, op1, _base) ->
                // size: 4, opcode: 205
                writer.Write(0x400CDu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(_base)
            | OpDPdx(op0, op1, p) ->
                // size: 4, opcode: 207
                writer.Write(0x400CFu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(p)
            | OpDPdy(op0, op1, p) ->
                // size: 4, opcode: 208
                writer.Write(0x400D0u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(p)
            | OpFwidth(op0, op1, p) ->
                // size: 4, opcode: 209
                writer.Write(0x400D1u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(p)
            | OpDPdxFine(op0, op1, p) ->
                // size: 4, opcode: 210
                writer.Write(0x400D2u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(p)
            | OpDPdyFine(op0, op1, p) ->
                // size: 4, opcode: 211
                writer.Write(0x400D3u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(p)
            | OpFwidthFine(op0, op1, p) ->
                // size: 4, opcode: 212
                writer.Write(0x400D4u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(p)
            | OpDPdxCoarse(op0, op1, p) ->
                // size: 4, opcode: 213
                writer.Write(0x400D5u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(p)
            | OpDPdyCoarse(op0, op1, p) ->
                // size: 4, opcode: 214
                writer.Write(0x400D6u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(p)
            | OpFwidthCoarse(op0, op1, p) ->
                // size: 4, opcode: 215
                writer.Write(0x400D7u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(p)
            | OpEmitVertex ->
                // size: 1, opcode: 218
                writer.Write(0x100DAu)
            | OpEndPrimitive ->
                // size: 1, opcode: 219
                writer.Write(0x100DBu)
            | OpEmitStreamVertex(stream) ->
                // size: 2, opcode: 220
                writer.Write(0x200DCu)
                writer.Write(stream)
            | OpEndStreamPrimitive(stream) ->
                // size: 2, opcode: 221
                writer.Write(0x200DDu)
                writer.Write(stream)
            | OpControlBarrier(execution, memory, semantics) ->
                // size: 4, opcode: 224
                writer.Write(0x400E0u)
                writer.Write(execution)
                writer.Write(memory)
                writer.Write(semantics)
            | OpMemoryBarrier(memory, semantics) ->
                // size: 3, opcode: 225
                writer.Write(0x300E1u)
                writer.Write(memory)
                writer.Write(semantics)
            | OpAtomicLoad(op0, op1, pointer, scope, semantics) ->
                // size: 6, opcode: 227
                writer.Write(0x600E3u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
            | OpAtomicStore(pointer, scope, semantics, value) ->
                // size: 5, opcode: 228
                writer.Write(0x500E4u)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpAtomicExchange(op0, op1, pointer, scope, semantics, value) ->
                // size: 7, opcode: 229
                writer.Write(0x700E5u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpAtomicCompareExchange(op0, op1, pointer, scope, equal, unequal, value, comparator) ->
                // size: 9, opcode: 230
                writer.Write(0x900E6u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(equal)
                writer.Write(unequal)
                writer.Write(value)
                writer.Write(comparator)
            | OpAtomicCompareExchangeWeak(op0, op1, pointer, scope, equal, unequal, value, comparator) ->
                // size: 9, opcode: 231
                writer.Write(0x900E7u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(equal)
                writer.Write(unequal)
                writer.Write(value)
                writer.Write(comparator)
            | OpAtomicIIncrement(op0, op1, pointer, scope, semantics) ->
                // size: 6, opcode: 232
                writer.Write(0x600E8u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
            | OpAtomicIDecrement(op0, op1, pointer, scope, semantics) ->
                // size: 6, opcode: 233
                writer.Write(0x600E9u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
            | OpAtomicIAdd(op0, op1, pointer, scope, semantics, value) ->
                // size: 7, opcode: 234
                writer.Write(0x700EAu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpAtomicISub(op0, op1, pointer, scope, semantics, value) ->
                // size: 7, opcode: 235
                writer.Write(0x700EBu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpAtomicSMin(op0, op1, pointer, scope, semantics, value) ->
                // size: 7, opcode: 236
                writer.Write(0x700ECu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpAtomicUMin(op0, op1, pointer, scope, semantics, value) ->
                // size: 7, opcode: 237
                writer.Write(0x700EDu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpAtomicSMax(op0, op1, pointer, scope, semantics, value) ->
                // size: 7, opcode: 238
                writer.Write(0x700EEu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpAtomicUMax(op0, op1, pointer, scope, semantics, value) ->
                // size: 7, opcode: 239
                writer.Write(0x700EFu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpAtomicAnd(op0, op1, pointer, scope, semantics, value) ->
                // size: 7, opcode: 240
                writer.Write(0x700F0u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpAtomicOr(op0, op1, pointer, scope, semantics, value) ->
                // size: 7, opcode: 241
                writer.Write(0x700F1u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpAtomicXor(op0, op1, pointer, scope, semantics, value) ->
                // size: 7, opcode: 242
                writer.Write(0x700F2u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
                writer.Write(value)
            | OpPhi(op0, op1, variableParent) ->
                // opcode: 245
                let _size = 3 + (variableParent.Length * 2)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 245u)
                writer.Write(op0)
                writer.Write(op1)
                writer.WriteArray(variableParent)
            | OpLoopMerge(mergeBlock, continueTarget, op2) ->
                // size: 4, opcode: 246
                writer.Write(0x400F6u)
                writer.Write(mergeBlock)
                writer.Write(continueTarget)
                writer.Write(op2)
            | OpSelectionMerge(mergeBlock, op1) ->
                // size: 3, opcode: 247
                writer.Write(0x300F7u)
                writer.Write(mergeBlock)
                writer.Write(op1)
            | OpLabel(op0) ->
                // size: 2, opcode: 248
                writer.Write(0x200F8u)
                writer.Write(op0)
            | OpBranch(targetLabel) ->
                // size: 2, opcode: 249
                writer.Write(0x200F9u)
                writer.Write(targetLabel)
            | OpBranchConditional(condition, trueLabel, falseLabel, branchweights) ->
                // opcode: 250
                let _size = 4 + (branchweights.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 250u)
                writer.Write(condition)
                writer.Write(trueLabel)
                writer.Write(falseLabel)
                writer.WriteArray(branchweights)
            | OpSwitch(selector, _default, target) ->
                // opcode: 251
                let _size = 3 + (target.Length * 2)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 251u)
                writer.Write(selector)
                writer.Write(_default)
                writer.WriteArray(target)
            | OpKill ->
                // size: 1, opcode: 252
                writer.Write(0x100FCu)
            | OpReturn ->
                // size: 1, opcode: 253
                writer.Write(0x100FDu)
            | OpReturnValue(value) ->
                // size: 2, opcode: 254
                writer.Write(0x200FEu)
                writer.Write(value)
            | OpUnreachable ->
                // size: 1, opcode: 255
                writer.Write(0x100FFu)
            | OpLifetimeStart(pointer, size) ->
                // size: 3, opcode: 256
                writer.Write(0x30100u)
                writer.Write(pointer)
                writer.Write(size)
            | OpLifetimeStop(pointer, size) ->
                // size: 3, opcode: 257
                writer.Write(0x30101u)
                writer.Write(pointer)
                writer.Write(size)
            | OpGroupAsyncCopy(op0, op1, execution, destination, source, numElements, stride, event) ->
                // size: 9, opcode: 259
                writer.Write(0x90103u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(destination)
                writer.Write(source)
                writer.Write(numElements)
                writer.Write(stride)
                writer.Write(event)
            | OpGroupWaitEvents(execution, numEvents, eventsList) ->
                // size: 4, opcode: 260
                writer.Write(0x40104u)
                writer.Write(execution)
                writer.Write(numEvents)
                writer.Write(eventsList)
            | OpGroupAll(op0, op1, execution, predicate) ->
                // size: 5, opcode: 261
                writer.Write(0x50105u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(predicate)
            | OpGroupAny(op0, op1, execution, predicate) ->
                // size: 5, opcode: 262
                writer.Write(0x50106u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(predicate)
            | OpGroupBroadcast(op0, op1, execution, value, localId) ->
                // size: 6, opcode: 263
                writer.Write(0x60107u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(value)
                writer.Write(localId)
            | OpGroupIAdd(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 264
                writer.Write(0x60108u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupFAdd(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 265
                writer.Write(0x60109u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupFMin(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 266
                writer.Write(0x6010Au)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupUMin(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 267
                writer.Write(0x6010Bu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupSMin(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 268
                writer.Write(0x6010Cu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupFMax(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 269
                writer.Write(0x6010Du)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupUMax(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 270
                writer.Write(0x6010Eu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupSMax(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 271
                writer.Write(0x6010Fu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpReadPipe(op0, op1, pipe, pointer, packetSize, packetAlignment) ->
                // size: 7, opcode: 274
                writer.Write(0x70112u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pipe)
                writer.Write(pointer)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpWritePipe(op0, op1, pipe, pointer, packetSize, packetAlignment) ->
                // size: 7, opcode: 275
                writer.Write(0x70113u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pipe)
                writer.Write(pointer)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpReservedReadPipe(op0, op1, pipe, reserveId, index, pointer, packetSize, packetAlignment) ->
                // size: 9, opcode: 276
                writer.Write(0x90114u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pipe)
                writer.Write(reserveId)
                writer.Write(index)
                writer.Write(pointer)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpReservedWritePipe(op0, op1, pipe, reserveId, index, pointer, packetSize, packetAlignment) ->
                // size: 9, opcode: 277
                writer.Write(0x90115u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pipe)
                writer.Write(reserveId)
                writer.Write(index)
                writer.Write(pointer)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpReserveReadPipePackets(op0, op1, pipe, numPackets, packetSize, packetAlignment) ->
                // size: 7, opcode: 278
                writer.Write(0x70116u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pipe)
                writer.Write(numPackets)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpReserveWritePipePackets(op0, op1, pipe, numPackets, packetSize, packetAlignment) ->
                // size: 7, opcode: 279
                writer.Write(0x70117u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pipe)
                writer.Write(numPackets)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpCommitReadPipe(pipe, reserveId, packetSize, packetAlignment) ->
                // size: 5, opcode: 280
                writer.Write(0x50118u)
                writer.Write(pipe)
                writer.Write(reserveId)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpCommitWritePipe(pipe, reserveId, packetSize, packetAlignment) ->
                // size: 5, opcode: 281
                writer.Write(0x50119u)
                writer.Write(pipe)
                writer.Write(reserveId)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpIsValidReserveId(op0, op1, reserveId) ->
                // size: 4, opcode: 282
                writer.Write(0x4011Au)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(reserveId)
            | OpGetNumPipePackets(op0, op1, pipe, packetSize, packetAlignment) ->
                // size: 6, opcode: 283
                writer.Write(0x6011Bu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pipe)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpGetMaxPipePackets(op0, op1, pipe, packetSize, packetAlignment) ->
                // size: 6, opcode: 284
                writer.Write(0x6011Cu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pipe)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpGroupReserveReadPipePackets(op0, op1, execution, pipe, numPackets, packetSize, packetAlignment) ->
                // size: 8, opcode: 285
                writer.Write(0x8011Du)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(pipe)
                writer.Write(numPackets)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpGroupReserveWritePipePackets(op0, op1, execution, pipe, numPackets, packetSize, packetAlignment) ->
                // size: 8, opcode: 286
                writer.Write(0x8011Eu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(pipe)
                writer.Write(numPackets)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpGroupCommitReadPipe(execution, pipe, reserveId, packetSize, packetAlignment) ->
                // size: 6, opcode: 287
                writer.Write(0x6011Fu)
                writer.Write(execution)
                writer.Write(pipe)
                writer.Write(reserveId)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpGroupCommitWritePipe(execution, pipe, reserveId, packetSize, packetAlignment) ->
                // size: 6, opcode: 288
                writer.Write(0x60120u)
                writer.Write(execution)
                writer.Write(pipe)
                writer.Write(reserveId)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
            | OpEnqueueMarker(op0, op1, queue, numEvents, waitEvents, retEvent) ->
                // size: 7, opcode: 291
                writer.Write(0x70123u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(queue)
                writer.Write(numEvents)
                writer.Write(waitEvents)
                writer.Write(retEvent)
            | OpEnqueueKernel(op0, op1, queue, flags, nDRange, numEvents, waitEvents, retEvent, invoke, param, paramSize, paramAlign, localSize) ->
                // opcode: 292
                let _size = 13 + (localSize.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 292u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(queue)
                writer.Write(flags)
                writer.Write(nDRange)
                writer.Write(numEvents)
                writer.Write(waitEvents)
                writer.Write(retEvent)
                writer.Write(invoke)
                writer.Write(param)
                writer.Write(paramSize)
                writer.Write(paramAlign)
                writer.WriteArray(localSize)
            | OpGetKernelNDrangeSubGroupCount(op0, op1, nDRange, invoke, param, paramSize, paramAlign) ->
                // size: 8, opcode: 293
                writer.Write(0x80125u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(nDRange)
                writer.Write(invoke)
                writer.Write(param)
                writer.Write(paramSize)
                writer.Write(paramAlign)
            | OpGetKernelNDrangeMaxSubGroupSize(op0, op1, nDRange, invoke, param, paramSize, paramAlign) ->
                // size: 8, opcode: 294
                writer.Write(0x80126u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(nDRange)
                writer.Write(invoke)
                writer.Write(param)
                writer.Write(paramSize)
                writer.Write(paramAlign)
            | OpGetKernelWorkGroupSize(op0, op1, invoke, param, paramSize, paramAlign) ->
                // size: 7, opcode: 295
                writer.Write(0x70127u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(invoke)
                writer.Write(param)
                writer.Write(paramSize)
                writer.Write(paramAlign)
            | OpGetKernelPreferredWorkGroupSizeMultiple(op0, op1, invoke, param, paramSize, paramAlign) ->
                // size: 7, opcode: 296
                writer.Write(0x70128u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(invoke)
                writer.Write(param)
                writer.Write(paramSize)
                writer.Write(paramAlign)
            | OpRetainEvent(event) ->
                // size: 2, opcode: 297
                writer.Write(0x20129u)
                writer.Write(event)
            | OpReleaseEvent(event) ->
                // size: 2, opcode: 298
                writer.Write(0x2012Au)
                writer.Write(event)
            | OpCreateUserEvent(op0, op1) ->
                // size: 3, opcode: 299
                writer.Write(0x3012Bu)
                writer.Write(op0)
                writer.Write(op1)
            | OpIsValidEvent(op0, op1, event) ->
                // size: 4, opcode: 300
                writer.Write(0x4012Cu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(event)
            | OpSetUserEventStatus(event, status) ->
                // size: 3, opcode: 301
                writer.Write(0x3012Du)
                writer.Write(event)
                writer.Write(status)
            | OpCaptureEventProfilingInfo(event, profilingInfo, value) ->
                // size: 4, opcode: 302
                writer.Write(0x4012Eu)
                writer.Write(event)
                writer.Write(profilingInfo)
                writer.Write(value)
            | OpGetDefaultQueue(op0, op1) ->
                // size: 3, opcode: 303
                writer.Write(0x3012Fu)
                writer.Write(op0)
                writer.Write(op1)
            | OpBuildNDRange(op0, op1, globalWorkSize, localWorkSize, globalWorkOffset) ->
                // size: 6, opcode: 304
                writer.Write(0x60130u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(globalWorkSize)
                writer.Write(localWorkSize)
                writer.Write(globalWorkOffset)
            | OpImageSparseSampleImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) ->
                // opcode: 305
                let _size = 5 + (if Option.isSome op4 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 305u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageSparseSampleExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) ->
                // opcode: 306
                let _size = 6 + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 306u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageSparseSampleDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) ->
                // opcode: 307
                let _size = 6 + (if Option.isSome op5 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 307u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(dref)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageSparseSampleDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) ->
                // opcode: 308
                let _size = 7 + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 308u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(dref)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageSparseSampleProjImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) ->
                // opcode: 309
                let _size = 5 + (if Option.isSome op4 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 309u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageSparseSampleProjExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) ->
                // opcode: 310
                let _size = 6 + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 310u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageSparseSampleProjDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) ->
                // opcode: 311
                let _size = 6 + (if Option.isSome op5 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 311u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(dref)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageSparseSampleProjDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) ->
                // opcode: 312
                let _size = 7 + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 312u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(dref)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageSparseFetch(op0, op1, image, coordinate, op4, parameters) ->
                // opcode: 313
                let _size = 5 + (if Option.isSome op4 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 313u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpImageSparseGather(op0, op1, sampledImage, coordinate, _component, op5, parameters) ->
                // opcode: 314
                let _size = 6 + (if Option.isSome op5 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 314u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(_component)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageSparseDrefGather(op0, op1, sampledImage, coordinate, dref, op5, parameters) ->
                // opcode: 315
                let _size = 6 + (if Option.isSome op5 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 315u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(sampledImage)
                writer.Write(coordinate)
                writer.Write(dref)
                writer.Write(op5)
                writer.WriteArray(parameters)
            | OpImageSparseTexelsResident(op0, op1, residentCode) ->
                // size: 4, opcode: 316
                writer.Write(0x4013Cu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(residentCode)
            | OpNoLine ->
                // size: 1, opcode: 317
                writer.Write(0x1013Du)
            | OpAtomicFlagTestAndSet(op0, op1, pointer, scope, semantics) ->
                // size: 6, opcode: 318
                writer.Write(0x6013Eu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
            | OpAtomicFlagClear(pointer, scope, semantics) ->
                // size: 4, opcode: 319
                writer.Write(0x4013Fu)
                writer.Write(pointer)
                writer.Write(scope)
                writer.Write(semantics)
            | OpImageSparseRead(op0, op1, image, coordinate, op4, parameters) ->
                // opcode: 320
                let _size = 5 + (if Option.isSome op4 then 1 else 0) + (parameters.Length * 1)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 320u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(image)
                writer.Write(coordinate)
                writer.Write(op4)
                writer.WriteArray(parameters)
            | OpSizeOf(op0, op1, pointer) ->
                // size: 4, opcode: 321
                writer.Write(0x40141u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pointer)
            | OpTypePipeStorage(op0) ->
                // size: 2, opcode: 322
                writer.Write(0x20142u)
                writer.Write(op0)
            | OpConstantPipeStorage(op0, op1, packetSize, packetAlignment, capacity) ->
                // size: 6, opcode: 323
                writer.Write(0x60143u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(packetSize)
                writer.Write(packetAlignment)
                writer.Write(capacity)
            | OpCreatePipeFromPipeStorage(op0, op1, pipeStorage) ->
                // size: 4, opcode: 324
                writer.Write(0x40144u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(pipeStorage)
            | OpGetKernelLocalSizeForSubgroupCount(op0, op1, subgroupCount, invoke, param, paramSize, paramAlign) ->
                // size: 8, opcode: 325
                writer.Write(0x80145u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(subgroupCount)
                writer.Write(invoke)
                writer.Write(param)
                writer.Write(paramSize)
                writer.Write(paramAlign)
            | OpGetKernelMaxNumSubgroups(op0, op1, invoke, param, paramSize, paramAlign) ->
                // size: 7, opcode: 326
                writer.Write(0x70146u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(invoke)
                writer.Write(param)
                writer.Write(paramSize)
                writer.Write(paramAlign)
            | OpTypeNamedBarrier(op0) ->
                // size: 2, opcode: 327
                writer.Write(0x20147u)
                writer.Write(op0)
            | OpNamedBarrierInitialize(op0, op1, subgroupCount) ->
                // size: 4, opcode: 328
                writer.Write(0x40148u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(subgroupCount)
            | OpMemoryNamedBarrier(namedBarrier, memory, semantics) ->
                // size: 4, opcode: 329
                writer.Write(0x40149u)
                writer.Write(namedBarrier)
                writer.Write(memory)
                writer.Write(semantics)
            | OpModuleProcessed(_process) ->
                // opcode: 330
                let _size = 1 + div4 (1 + _process.Length)
                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| 330u)
                writer.WriteString(_process)
            | OpExecutionModeId(entryPoint, mode) ->
                // size: 3, opcode: 331
                writer.Write(0x3014Bu)
                writer.Write(entryPoint)
                writer.Write(mode)
            | OpDecorateId(target, op1) ->
                // size: 3, opcode: 332
                writer.Write(0x3014Cu)
                writer.Write(target)
                writer.Write(op1)
            | OpSubgroupBallotKHR(op0, op1, predicate) ->
                // size: 4, opcode: 4421
                writer.Write(0x41145u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(predicate)
            | OpSubgroupFirstInvocationKHR(op0, op1, value) ->
                // size: 4, opcode: 4422
                writer.Write(0x41146u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(value)
            | OpSubgroupAllKHR(op0, op1, predicate) ->
                // size: 4, opcode: 4428
                writer.Write(0x4114Cu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(predicate)
            | OpSubgroupAnyKHR(op0, op1, predicate) ->
                // size: 4, opcode: 4429
                writer.Write(0x4114Du)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(predicate)
            | OpSubgroupAllEqualKHR(op0, op1, predicate) ->
                // size: 4, opcode: 4430
                writer.Write(0x4114Eu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(predicate)
            | OpSubgroupReadInvocationKHR(op0, op1, value, index) ->
                // size: 5, opcode: 4432
                writer.Write(0x51150u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(value)
                writer.Write(index)
            | OpGroupIAddNonUniformAMD(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 5000
                writer.Write(0x61388u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupFAddNonUniformAMD(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 5001
                writer.Write(0x61389u)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupFMinNonUniformAMD(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 5002
                writer.Write(0x6138Au)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupUMinNonUniformAMD(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 5003
                writer.Write(0x6138Bu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupSMinNonUniformAMD(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 5004
                writer.Write(0x6138Cu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupFMaxNonUniformAMD(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 5005
                writer.Write(0x6138Du)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupUMaxNonUniformAMD(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 5006
                writer.Write(0x6138Eu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)
            | OpGroupSMaxNonUniformAMD(op0, op1, execution, operation, x) ->
                // size: 6, opcode: 5007
                writer.Write(0x6138Fu)
                writer.Write(op0)
                writer.Write(op1)
                writer.Write(execution)
                writer.Write(operation)
                writer.Write(x)

    let inline private tryDecrement (r : ref<int>) (v : int) (f : unit -> 'a) = if !r >= v then r := !r - v; Some(f()) else None
    
    type BinaryReader with
        member inline x.ReadAccessQualifier(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<AccessQualifier>(x.ReadInt32()))
        member inline x.ReadAddressingModel(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<AddressingModel>(x.ReadInt32()))
        member inline x.ReadCapability(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<Capability>(x.ReadInt32()))
        member inline x.ReadDecoration(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<Decoration>(x.ReadInt32()))
        member inline x.ReadDim(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<Dim>(x.ReadInt32()))
        member inline x.ReadExecutionMode(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<ExecutionMode>(x.ReadInt32()))
        member inline x.ReadExecutionModel(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<ExecutionModel>(x.ReadInt32()))
        member inline x.ReadFunctionControl(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<FunctionControl>(x.ReadInt32()))
        member inline x.ReadGroupOperation(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<GroupOperation>(x.ReadInt32()))
        member inline x.ReadIdMemorySemantics(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<MemorySemantics>(x.ReadInt32()))
        member inline x.ReadIdRef(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadUInt32())
        member inline x.ReadIdResult(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadUInt32())
        member inline x.ReadIdResultType(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadUInt32())
        member inline x.ReadIdScope(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<Scope>(x.ReadInt32()))
        member inline x.ReadImageFormat(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<ImageFormat>(x.ReadInt32()))
        member inline x.ReadImageOperands(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<ImageOperands>(x.ReadInt32()))
        member inline x.ReadLiteralExtInstInteger(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadInt32())
        member inline x.ReadLiteralInteger(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadInt32())
        member inline x.ReadLiteralSpecConstantOpInteger(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadInt32())
        member inline x.ReadLoopControl(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<LoopControl>(x.ReadInt32()))
        member inline x.ReadMemoryAccess(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<MemoryAccess>(x.ReadInt32()))
        member inline x.ReadMemoryModel(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<MemoryModel>(x.ReadInt32()))
        member inline x.ReadSamplerAddressingMode(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<SamplerAddressingMode>(x.ReadInt32()))
        member inline x.ReadSamplerFilterMode(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<SamplerFilterMode>(x.ReadInt32()))
        member inline x.ReadSelectionControl(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<SelectionControl>(x.ReadInt32()))
        member inline x.ReadSourceLanguage(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<SourceLanguage>(x.ReadInt32()))
        member inline x.ReadStorageClass(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<StorageClass>(x.ReadInt32()))
        member inline x.ReadPairIdRefIdRef(s : ref<int>) = tryDecrement s 2 (fun () -> PairIdRefIdRef(x.ReadUInt32(), x.ReadUInt32()))
        member inline x.ReadPairIdRefLiteralInteger(s : ref<int>) = tryDecrement s 2 (fun () -> PairIdRefLiteralInteger(x.ReadUInt32(), x.ReadInt32()))
        member inline x.ReadPairLiteralIntegerIdRef(s : ref<int>) = tryDecrement s 2 (fun () -> PairLiteralIntegerIdRef(x.ReadInt32(), x.ReadUInt32()))
        member inline x.ReadLiteralString(s : ref<int>) = 
            if !s > 0 then
                let mutable cont = true
                let mutable bytes = !s * 4
                let result = System.Collections.Generic.List<char>(!s * 4)
                while cont && bytes > 0 do
                    let c = x.ReadChar()
                    bytes <- bytes - 1
                    if c <> char 0 then
                        result.Add c
                    else
                        cont <- false
            
                let rest = bytes % 4
                bytes <- bytes - rest
                if rest > 0 then x.BaseStream.Seek(int64 rest, SeekOrigin.Current) |> ignore
                s := bytes / 4
                String(Seq.toArray result) |> Some
            else
                None

        member inline x.ReadArray(reader : ref<int> -> Option<'a>, remaining : ref<int>) =
            let rec run() =
                match reader(remaining) with
                    | Some v -> v :: run()
                    | None -> []
            List.toArray (run())

    let tryReadFrom (reader : BinaryReader) = 
        option {
            let encoded = reader.ReadUInt32()
            let code = int (encoded &&& 0xFFFFu)
            let remaining = ref (int (encoded >>> 16) - 1)
            
            match code with
                | 0 -> // OpNop
                    if !remaining = 0 then return OpNop
                    else return! None
                | 1 -> // OpUndef
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpUndef(op0, op1))
                    else return! None
                | 2 -> // OpSourceContinued
                    let! continuedSource = reader.ReadLiteralString(remaining)
                    if !remaining = 0 then return (OpSourceContinued(continuedSource))
                    else return! None
                | 3 -> // OpSource
                    let! op0 = reader.ReadSourceLanguage(remaining)
                    let! version = reader.ReadLiteralInteger(remaining)
                    let file = reader.ReadIdRef(remaining)
                    let source = reader.ReadLiteralString(remaining)
                    if !remaining = 0 then return (OpSource(op0, version, file, source))
                    else return! None
                | 4 -> // OpSourceExtension
                    let! extension = reader.ReadLiteralString(remaining)
                    if !remaining = 0 then return (OpSourceExtension(extension))
                    else return! None
                | 5 -> // OpName
                    let! target = reader.ReadIdRef(remaining)
                    let! name = reader.ReadLiteralString(remaining)
                    if !remaining = 0 then return (OpName(target, name))
                    else return! None
                | 6 -> // OpMemberName
                    let! _type = reader.ReadIdRef(remaining)
                    let! _member = reader.ReadLiteralInteger(remaining)
                    let! name = reader.ReadLiteralString(remaining)
                    if !remaining = 0 then return (OpMemberName(_type, _member, name))
                    else return! None
                | 7 -> // OpString
                    let! op0 = reader.ReadIdResult(remaining)
                    let! string = reader.ReadLiteralString(remaining)
                    if !remaining = 0 then return (OpString(op0, string))
                    else return! None
                | 8 -> // OpLine
                    let! file = reader.ReadIdRef(remaining)
                    let! line = reader.ReadLiteralInteger(remaining)
                    let! column = reader.ReadLiteralInteger(remaining)
                    if !remaining = 0 then return (OpLine(file, line, column))
                    else return! None
                | 10 -> // OpExtension
                    let! name = reader.ReadLiteralString(remaining)
                    if !remaining = 0 then return (OpExtension(name))
                    else return! None
                | 11 -> // OpExtInstImport
                    let! op0 = reader.ReadIdResult(remaining)
                    let! name = reader.ReadLiteralString(remaining)
                    if !remaining = 0 then return (OpExtInstImport(op0, name))
                    else return! None
                | 12 -> // OpExtInst
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! set = reader.ReadIdRef(remaining)
                    let! instruction = reader.ReadLiteralExtInstInteger(remaining)
                    let operandOperand = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpExtInst(op0, op1, set, instruction, operandOperand))
                    else return! None
                | 14 -> // OpMemoryModel
                    let! op0 = reader.ReadAddressingModel(remaining)
                    let! op1 = reader.ReadMemoryModel(remaining)
                    if !remaining = 0 then return (OpMemoryModel(op0, op1))
                    else return! None
                | 15 -> // OpEntryPoint
                    let! op0 = reader.ReadExecutionModel(remaining)
                    let! entryPoint = reader.ReadIdRef(remaining)
                    let! name = reader.ReadLiteralString(remaining)
                    let _interface = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpEntryPoint(op0, entryPoint, name, _interface))
                    else return! None
                | 16 -> // OpExecutionMode
                    let! entryPoint = reader.ReadIdRef(remaining)
                    let! mode = reader.ReadExecutionMode(remaining)
                    if !remaining = 0 then return (OpExecutionMode(entryPoint, mode))
                    else return! None
                | 17 -> // OpCapability
                    let! capability = reader.ReadCapability(remaining)
                    if !remaining = 0 then return (OpCapability(capability))
                    else return! None
                | 19 -> // OpTypeVoid
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpTypeVoid(op0))
                    else return! None
                | 20 -> // OpTypeBool
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpTypeBool(op0))
                    else return! None
                | 21 -> // OpTypeInt
                    let! op0 = reader.ReadIdResult(remaining)
                    let! width = reader.ReadLiteralInteger(remaining)
                    let! signedness = reader.ReadLiteralInteger(remaining)
                    if !remaining = 0 then return (OpTypeInt(op0, width, signedness))
                    else return! None
                | 22 -> // OpTypeFloat
                    let! op0 = reader.ReadIdResult(remaining)
                    let! width = reader.ReadLiteralInteger(remaining)
                    if !remaining = 0 then return (OpTypeFloat(op0, width))
                    else return! None
                | 23 -> // OpTypeVector
                    let! op0 = reader.ReadIdResult(remaining)
                    let! componentType = reader.ReadIdRef(remaining)
                    let! componentCount = reader.ReadLiteralInteger(remaining)
                    if !remaining = 0 then return (OpTypeVector(op0, componentType, componentCount))
                    else return! None
                | 24 -> // OpTypeMatrix
                    let! op0 = reader.ReadIdResult(remaining)
                    let! columnType = reader.ReadIdRef(remaining)
                    let! columnCount = reader.ReadLiteralInteger(remaining)
                    if !remaining = 0 then return (OpTypeMatrix(op0, columnType, columnCount))
                    else return! None
                | 25 -> // OpTypeImage
                    let! op0 = reader.ReadIdResult(remaining)
                    let! sampledType = reader.ReadIdRef(remaining)
                    let! op2 = reader.ReadDim(remaining)
                    let! depth = reader.ReadLiteralInteger(remaining)
                    let! arrayed = reader.ReadLiteralInteger(remaining)
                    let! mS = reader.ReadLiteralInteger(remaining)
                    let! sampled = reader.ReadLiteralInteger(remaining)
                    let! op7 = reader.ReadImageFormat(remaining)
                    let op8 = reader.ReadAccessQualifier(remaining)
                    if !remaining = 0 then return (OpTypeImage(op0, sampledType, op2, depth, arrayed, mS, sampled, op7, op8))
                    else return! None
                | 26 -> // OpTypeSampler
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpTypeSampler(op0))
                    else return! None
                | 27 -> // OpTypeSampledImage
                    let! op0 = reader.ReadIdResult(remaining)
                    let! imageType = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpTypeSampledImage(op0, imageType))
                    else return! None
                | 28 -> // OpTypeArray
                    let! op0 = reader.ReadIdResult(remaining)
                    let! elementType = reader.ReadIdRef(remaining)
                    let! length = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpTypeArray(op0, elementType, length))
                    else return! None
                | 29 -> // OpTypeRuntimeArray
                    let! op0 = reader.ReadIdResult(remaining)
                    let! elementType = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpTypeRuntimeArray(op0, elementType))
                    else return! None
                | 30 -> // OpTypeStruct
                    let! op0 = reader.ReadIdResult(remaining)
                    let membertypemembertype = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpTypeStruct(op0, membertypemembertype))
                    else return! None
                | 31 -> // OpTypeOpaque
                    let! op0 = reader.ReadIdResult(remaining)
                    let! thenameoftheopaquetype = reader.ReadLiteralString(remaining)
                    if !remaining = 0 then return (OpTypeOpaque(op0, thenameoftheopaquetype))
                    else return! None
                | 32 -> // OpTypePointer
                    let! op0 = reader.ReadIdResult(remaining)
                    let! op1 = reader.ReadStorageClass(remaining)
                    let! _type = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpTypePointer(op0, op1, _type))
                    else return! None
                | 33 -> // OpTypeFunction
                    let! op0 = reader.ReadIdResult(remaining)
                    let! returnType = reader.ReadIdRef(remaining)
                    let parameterTypeParameterType = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpTypeFunction(op0, returnType, parameterTypeParameterType))
                    else return! None
                | 34 -> // OpTypeEvent
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpTypeEvent(op0))
                    else return! None
                | 35 -> // OpTypeDeviceEvent
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpTypeDeviceEvent(op0))
                    else return! None
                | 36 -> // OpTypeReserveId
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpTypeReserveId(op0))
                    else return! None
                | 37 -> // OpTypeQueue
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpTypeQueue(op0))
                    else return! None
                | 38 -> // OpTypePipe
                    let! op0 = reader.ReadIdResult(remaining)
                    let! qualifier = reader.ReadAccessQualifier(remaining)
                    if !remaining = 0 then return (OpTypePipe(op0, qualifier))
                    else return! None
                | 39 -> // OpTypeForwardPointer
                    let! pointerType = reader.ReadIdRef(remaining)
                    let! op1 = reader.ReadStorageClass(remaining)
                    if !remaining = 0 then return (OpTypeForwardPointer(pointerType, op1))
                    else return! None
                | 41 -> // OpConstantTrue
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpConstantTrue(op0, op1))
                    else return! None
                | 42 -> // OpConstantFalse
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpConstantFalse(op0, op1))
                    else return! None
                | 43 -> // OpConstant
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let value = reader.ReadArray(reader.ReadLiteralInteger, remaining)
                    if !remaining = 0 then return (OpConstant(op0, op1, value))
                    else return! None
                | 44 -> // OpConstantComposite
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let constituents = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpConstantComposite(op0, op1, constituents))
                    else return! None
                | 45 -> // OpConstantSampler
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! op2 = reader.ReadSamplerAddressingMode(remaining)
                    let! param = reader.ReadLiteralInteger(remaining)
                    let! op4 = reader.ReadSamplerFilterMode(remaining)
                    if !remaining = 0 then return (OpConstantSampler(op0, op1, op2, param, op4))
                    else return! None
                | 46 -> // OpConstantNull
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpConstantNull(op0, op1))
                    else return! None
                | 48 -> // OpSpecConstantTrue
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpSpecConstantTrue(op0, op1))
                    else return! None
                | 49 -> // OpSpecConstantFalse
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpSpecConstantFalse(op0, op1))
                    else return! None
                | 50 -> // OpSpecConstant
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let value = reader.ReadArray(reader.ReadLiteralInteger, remaining)
                    if !remaining = 0 then return (OpSpecConstant(op0, op1, value))
                    else return! None
                | 51 -> // OpSpecConstantComposite
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let constituents = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpSpecConstantComposite(op0, op1, constituents))
                    else return! None
                | 52 -> // OpSpecConstantOp
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! opcode = reader.ReadLiteralSpecConstantOpInteger(remaining)
                    if !remaining = 0 then return (OpSpecConstantOp(op0, op1, opcode))
                    else return! None
                | 54 -> // OpFunction
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! op2 = reader.ReadFunctionControl(remaining)
                    let! functionType = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFunction(op0, op1, op2, functionType))
                    else return! None
                | 55 -> // OpFunctionParameter
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpFunctionParameter(op0, op1))
                    else return! None
                | 56 -> // OpFunctionEnd
                    if !remaining = 0 then return OpFunctionEnd
                    else return! None
                | 57 -> // OpFunctionCall
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _function = reader.ReadIdRef(remaining)
                    let argumentArgument = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpFunctionCall(op0, op1, _function, argumentArgument))
                    else return! None
                | 59 -> // OpVariable
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! op2 = reader.ReadStorageClass(remaining)
                    let initializer = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpVariable(op0, op1, op2, initializer))
                    else return! None
                | 60 -> // OpImageTexelPointer
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! sample = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpImageTexelPointer(op0, op1, image, coordinate, sample))
                    else return! None
                | 61 -> // OpLoad
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let op3 = reader.ReadMemoryAccess(remaining)
                    if !remaining = 0 then return (OpLoad(op0, op1, pointer, op3))
                    else return! None
                | 62 -> // OpStore
                    let! pointer = reader.ReadIdRef(remaining)
                    let! _object = reader.ReadIdRef(remaining)
                    let op2 = reader.ReadMemoryAccess(remaining)
                    if !remaining = 0 then return (OpStore(pointer, _object, op2))
                    else return! None
                | 63 -> // OpCopyMemory
                    let! target = reader.ReadIdRef(remaining)
                    let! source = reader.ReadIdRef(remaining)
                    let op2 = reader.ReadMemoryAccess(remaining)
                    if !remaining = 0 then return (OpCopyMemory(target, source, op2))
                    else return! None
                | 64 -> // OpCopyMemorySized
                    let! target = reader.ReadIdRef(remaining)
                    let! source = reader.ReadIdRef(remaining)
                    let! size = reader.ReadIdRef(remaining)
                    let op3 = reader.ReadMemoryAccess(remaining)
                    if !remaining = 0 then return (OpCopyMemorySized(target, source, size, op3))
                    else return! None
                | 65 -> // OpAccessChain
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    let indexes = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpAccessChain(op0, op1, _base, indexes))
                    else return! None
                | 66 -> // OpInBoundsAccessChain
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    let indexes = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpInBoundsAccessChain(op0, op1, _base, indexes))
                    else return! None
                | 67 -> // OpPtrAccessChain
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    let! element = reader.ReadIdRef(remaining)
                    let indexes = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpPtrAccessChain(op0, op1, _base, element, indexes))
                    else return! None
                | 68 -> // OpArrayLength
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! structure = reader.ReadIdRef(remaining)
                    let! arraymember = reader.ReadLiteralInteger(remaining)
                    if !remaining = 0 then return (OpArrayLength(op0, op1, structure, arraymember))
                    else return! None
                | 69 -> // OpGenericPtrMemSemantics
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGenericPtrMemSemantics(op0, op1, pointer))
                    else return! None
                | 70 -> // OpInBoundsPtrAccessChain
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    let! element = reader.ReadIdRef(remaining)
                    let indexes = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpInBoundsPtrAccessChain(op0, op1, _base, element, indexes))
                    else return! None
                | 71 -> // OpDecorate
                    let! target = reader.ReadIdRef(remaining)
                    let! op1 = reader.ReadDecoration(remaining)
                    let parameters = reader.ReadArray(reader.ReadLiteralInteger, remaining)
                    if !remaining = 0 then return (OpDecorate(target, op1, parameters))
                    else return! None
                | 72 -> // OpMemberDecorate
                    let! structureType = reader.ReadIdRef(remaining)
                    let! _member = reader.ReadLiteralInteger(remaining)
                    let! decoration = reader.ReadDecoration(remaining)
                    let values = reader.ReadArray(reader.ReadLiteralInteger, remaining)
                    if !remaining = 0 then return (OpMemberDecorate(structureType, _member, decoration, values))
                    else return! None
                | 73 -> // OpDecorationGroup
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpDecorationGroup(op0))
                    else return! None
                | 74 -> // OpGroupDecorate
                    let! decorationGroup = reader.ReadIdRef(remaining)
                    let targets = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpGroupDecorate(decorationGroup, targets))
                    else return! None
                | 75 -> // OpGroupMemberDecorate
                    let! decorationGroup = reader.ReadIdRef(remaining)
                    let targets = reader.ReadArray(reader.ReadPairIdRefLiteralInteger, remaining)
                    if !remaining = 0 then return (OpGroupMemberDecorate(decorationGroup, targets))
                    else return! None
                | 77 -> // OpVectorExtractDynamic
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! vector = reader.ReadIdRef(remaining)
                    let! index = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpVectorExtractDynamic(op0, op1, vector, index))
                    else return! None
                | 78 -> // OpVectorInsertDynamic
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! vector = reader.ReadIdRef(remaining)
                    let! _component = reader.ReadIdRef(remaining)
                    let! index = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpVectorInsertDynamic(op0, op1, vector, _component, index))
                    else return! None
                | 79 -> // OpVectorShuffle
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! vector1 = reader.ReadIdRef(remaining)
                    let! vector2 = reader.ReadIdRef(remaining)
                    let components = reader.ReadArray(reader.ReadLiteralInteger, remaining)
                    if !remaining = 0 then return (OpVectorShuffle(op0, op1, vector1, vector2, components))
                    else return! None
                | 80 -> // OpCompositeConstruct
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let constituents = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpCompositeConstruct(op0, op1, constituents))
                    else return! None
                | 81 -> // OpCompositeExtract
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! composite = reader.ReadIdRef(remaining)
                    let indexes = reader.ReadArray(reader.ReadLiteralInteger, remaining)
                    if !remaining = 0 then return (OpCompositeExtract(op0, op1, composite, indexes))
                    else return! None
                | 82 -> // OpCompositeInsert
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _object = reader.ReadIdRef(remaining)
                    let! composite = reader.ReadIdRef(remaining)
                    let indexes = reader.ReadArray(reader.ReadLiteralInteger, remaining)
                    if !remaining = 0 then return (OpCompositeInsert(op0, op1, _object, composite, indexes))
                    else return! None
                | 83 -> // OpCopyObject
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpCopyObject(op0, op1, operand))
                    else return! None
                | 84 -> // OpTranspose
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! matrix = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpTranspose(op0, op1, matrix))
                    else return! None
                | 86 -> // OpSampledImage
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    let! sampler = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSampledImage(op0, op1, image, sampler))
                    else return! None
                | 87 -> // OpImageSampleImplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSampleImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters))
                    else return! None
                | 88 -> // OpImageSampleExplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSampleExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters))
                    else return! None
                | 89 -> // OpImageSampleDrefImplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! dref = reader.ReadIdRef(remaining)
                    let op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSampleDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters))
                    else return! None
                | 90 -> // OpImageSampleDrefExplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! dref = reader.ReadIdRef(remaining)
                    let! op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSampleDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters))
                    else return! None
                | 91 -> // OpImageSampleProjImplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSampleProjImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters))
                    else return! None
                | 92 -> // OpImageSampleProjExplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSampleProjExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters))
                    else return! None
                | 93 -> // OpImageSampleProjDrefImplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! dref = reader.ReadIdRef(remaining)
                    let op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSampleProjDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters))
                    else return! None
                | 94 -> // OpImageSampleProjDrefExplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! dref = reader.ReadIdRef(remaining)
                    let! op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSampleProjDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters))
                    else return! None
                | 95 -> // OpImageFetch
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageFetch(op0, op1, image, coordinate, op4, parameters))
                    else return! None
                | 96 -> // OpImageGather
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! _component = reader.ReadIdRef(remaining)
                    let op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageGather(op0, op1, sampledImage, coordinate, _component, op5, parameters))
                    else return! None
                | 97 -> // OpImageDrefGather
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! dref = reader.ReadIdRef(remaining)
                    let op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageDrefGather(op0, op1, sampledImage, coordinate, dref, op5, parameters))
                    else return! None
                | 98 -> // OpImageRead
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageRead(op0, op1, image, coordinate, op4, parameters))
                    else return! None
                | 99 -> // OpImageWrite
                    let! image = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! texel = reader.ReadIdRef(remaining)
                    let op3 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageWrite(image, coordinate, texel, op3, parameters))
                    else return! None
                | 100 -> // OpImage
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpImage(op0, op1, sampledImage))
                    else return! None
                | 101 -> // OpImageQueryFormat
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpImageQueryFormat(op0, op1, image))
                    else return! None
                | 102 -> // OpImageQueryOrder
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpImageQueryOrder(op0, op1, image))
                    else return! None
                | 103 -> // OpImageQuerySizeLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    let! levelofDetail = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpImageQuerySizeLod(op0, op1, image, levelofDetail))
                    else return! None
                | 104 -> // OpImageQuerySize
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpImageQuerySize(op0, op1, image))
                    else return! None
                | 105 -> // OpImageQueryLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpImageQueryLod(op0, op1, sampledImage, coordinate))
                    else return! None
                | 106 -> // OpImageQueryLevels
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpImageQueryLevels(op0, op1, image))
                    else return! None
                | 107 -> // OpImageQuerySamples
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpImageQuerySamples(op0, op1, image))
                    else return! None
                | 109 -> // OpConvertFToU
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! floatValue = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpConvertFToU(op0, op1, floatValue))
                    else return! None
                | 110 -> // OpConvertFToS
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! floatValue = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpConvertFToS(op0, op1, floatValue))
                    else return! None
                | 111 -> // OpConvertSToF
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! signedValue = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpConvertSToF(op0, op1, signedValue))
                    else return! None
                | 112 -> // OpConvertUToF
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! unsignedValue = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpConvertUToF(op0, op1, unsignedValue))
                    else return! None
                | 113 -> // OpUConvert
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! unsignedValue = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpUConvert(op0, op1, unsignedValue))
                    else return! None
                | 114 -> // OpSConvert
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! signedValue = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSConvert(op0, op1, signedValue))
                    else return! None
                | 115 -> // OpFConvert
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! floatValue = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFConvert(op0, op1, floatValue))
                    else return! None
                | 116 -> // OpQuantizeToF16
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpQuantizeToF16(op0, op1, value))
                    else return! None
                | 117 -> // OpConvertPtrToU
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpConvertPtrToU(op0, op1, pointer))
                    else return! None
                | 118 -> // OpSatConvertSToU
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! signedValue = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSatConvertSToU(op0, op1, signedValue))
                    else return! None
                | 119 -> // OpSatConvertUToS
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! unsignedValue = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSatConvertUToS(op0, op1, unsignedValue))
                    else return! None
                | 120 -> // OpConvertUToPtr
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! integerValue = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpConvertUToPtr(op0, op1, integerValue))
                    else return! None
                | 121 -> // OpPtrCastToGeneric
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpPtrCastToGeneric(op0, op1, pointer))
                    else return! None
                | 122 -> // OpGenericCastToPtr
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGenericCastToPtr(op0, op1, pointer))
                    else return! None
                | 123 -> // OpGenericCastToPtrExplicit
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! storage = reader.ReadStorageClass(remaining)
                    if !remaining = 0 then return (OpGenericCastToPtrExplicit(op0, op1, pointer, storage))
                    else return! None
                | 124 -> // OpBitcast
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBitcast(op0, op1, operand))
                    else return! None
                | 126 -> // OpSNegate
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSNegate(op0, op1, operand))
                    else return! None
                | 127 -> // OpFNegate
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFNegate(op0, op1, operand))
                    else return! None
                | 128 -> // OpIAdd
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpIAdd(op0, op1, operand1, operand2))
                    else return! None
                | 129 -> // OpFAdd
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFAdd(op0, op1, operand1, operand2))
                    else return! None
                | 130 -> // OpISub
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpISub(op0, op1, operand1, operand2))
                    else return! None
                | 131 -> // OpFSub
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFSub(op0, op1, operand1, operand2))
                    else return! None
                | 132 -> // OpIMul
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpIMul(op0, op1, operand1, operand2))
                    else return! None
                | 133 -> // OpFMul
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFMul(op0, op1, operand1, operand2))
                    else return! None
                | 134 -> // OpUDiv
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpUDiv(op0, op1, operand1, operand2))
                    else return! None
                | 135 -> // OpSDiv
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSDiv(op0, op1, operand1, operand2))
                    else return! None
                | 136 -> // OpFDiv
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFDiv(op0, op1, operand1, operand2))
                    else return! None
                | 137 -> // OpUMod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpUMod(op0, op1, operand1, operand2))
                    else return! None
                | 138 -> // OpSRem
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSRem(op0, op1, operand1, operand2))
                    else return! None
                | 139 -> // OpSMod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSMod(op0, op1, operand1, operand2))
                    else return! None
                | 140 -> // OpFRem
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFRem(op0, op1, operand1, operand2))
                    else return! None
                | 141 -> // OpFMod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFMod(op0, op1, operand1, operand2))
                    else return! None
                | 142 -> // OpVectorTimesScalar
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! vector = reader.ReadIdRef(remaining)
                    let! scalar = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpVectorTimesScalar(op0, op1, vector, scalar))
                    else return! None
                | 143 -> // OpMatrixTimesScalar
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! matrix = reader.ReadIdRef(remaining)
                    let! scalar = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpMatrixTimesScalar(op0, op1, matrix, scalar))
                    else return! None
                | 144 -> // OpVectorTimesMatrix
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! vector = reader.ReadIdRef(remaining)
                    let! matrix = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpVectorTimesMatrix(op0, op1, vector, matrix))
                    else return! None
                | 145 -> // OpMatrixTimesVector
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! matrix = reader.ReadIdRef(remaining)
                    let! vector = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpMatrixTimesVector(op0, op1, matrix, vector))
                    else return! None
                | 146 -> // OpMatrixTimesMatrix
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! leftMatrix = reader.ReadIdRef(remaining)
                    let! rightMatrix = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpMatrixTimesMatrix(op0, op1, leftMatrix, rightMatrix))
                    else return! None
                | 147 -> // OpOuterProduct
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! vector1 = reader.ReadIdRef(remaining)
                    let! vector2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpOuterProduct(op0, op1, vector1, vector2))
                    else return! None
                | 148 -> // OpDot
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! vector1 = reader.ReadIdRef(remaining)
                    let! vector2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpDot(op0, op1, vector1, vector2))
                    else return! None
                | 149 -> // OpIAddCarry
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpIAddCarry(op0, op1, operand1, operand2))
                    else return! None
                | 150 -> // OpISubBorrow
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpISubBorrow(op0, op1, operand1, operand2))
                    else return! None
                | 151 -> // OpUMulExtended
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpUMulExtended(op0, op1, operand1, operand2))
                    else return! None
                | 152 -> // OpSMulExtended
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSMulExtended(op0, op1, operand1, operand2))
                    else return! None
                | 154 -> // OpAny
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! vector = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAny(op0, op1, vector))
                    else return! None
                | 155 -> // OpAll
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! vector = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAll(op0, op1, vector))
                    else return! None
                | 156 -> // OpIsNan
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpIsNan(op0, op1, x))
                    else return! None
                | 157 -> // OpIsInf
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpIsInf(op0, op1, x))
                    else return! None
                | 158 -> // OpIsFinite
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpIsFinite(op0, op1, x))
                    else return! None
                | 159 -> // OpIsNormal
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpIsNormal(op0, op1, x))
                    else return! None
                | 160 -> // OpSignBitSet
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSignBitSet(op0, op1, x))
                    else return! None
                | 161 -> // OpLessOrGreater
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    let! y = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpLessOrGreater(op0, op1, x, y))
                    else return! None
                | 162 -> // OpOrdered
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    let! y = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpOrdered(op0, op1, x, y))
                    else return! None
                | 163 -> // OpUnordered
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    let! y = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpUnordered(op0, op1, x, y))
                    else return! None
                | 164 -> // OpLogicalEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpLogicalEqual(op0, op1, operand1, operand2))
                    else return! None
                | 165 -> // OpLogicalNotEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpLogicalNotEqual(op0, op1, operand1, operand2))
                    else return! None
                | 166 -> // OpLogicalOr
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpLogicalOr(op0, op1, operand1, operand2))
                    else return! None
                | 167 -> // OpLogicalAnd
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpLogicalAnd(op0, op1, operand1, operand2))
                    else return! None
                | 168 -> // OpLogicalNot
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpLogicalNot(op0, op1, operand))
                    else return! None
                | 169 -> // OpSelect
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! condition = reader.ReadIdRef(remaining)
                    let! _object1 = reader.ReadIdRef(remaining)
                    let! _object2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSelect(op0, op1, condition, _object1, _object2))
                    else return! None
                | 170 -> // OpIEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpIEqual(op0, op1, operand1, operand2))
                    else return! None
                | 171 -> // OpINotEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpINotEqual(op0, op1, operand1, operand2))
                    else return! None
                | 172 -> // OpUGreaterThan
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpUGreaterThan(op0, op1, operand1, operand2))
                    else return! None
                | 173 -> // OpSGreaterThan
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSGreaterThan(op0, op1, operand1, operand2))
                    else return! None
                | 174 -> // OpUGreaterThanEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpUGreaterThanEqual(op0, op1, operand1, operand2))
                    else return! None
                | 175 -> // OpSGreaterThanEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSGreaterThanEqual(op0, op1, operand1, operand2))
                    else return! None
                | 176 -> // OpULessThan
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpULessThan(op0, op1, operand1, operand2))
                    else return! None
                | 177 -> // OpSLessThan
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSLessThan(op0, op1, operand1, operand2))
                    else return! None
                | 178 -> // OpULessThanEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpULessThanEqual(op0, op1, operand1, operand2))
                    else return! None
                | 179 -> // OpSLessThanEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSLessThanEqual(op0, op1, operand1, operand2))
                    else return! None
                | 180 -> // OpFOrdEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFOrdEqual(op0, op1, operand1, operand2))
                    else return! None
                | 181 -> // OpFUnordEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFUnordEqual(op0, op1, operand1, operand2))
                    else return! None
                | 182 -> // OpFOrdNotEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFOrdNotEqual(op0, op1, operand1, operand2))
                    else return! None
                | 183 -> // OpFUnordNotEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFUnordNotEqual(op0, op1, operand1, operand2))
                    else return! None
                | 184 -> // OpFOrdLessThan
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFOrdLessThan(op0, op1, operand1, operand2))
                    else return! None
                | 185 -> // OpFUnordLessThan
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFUnordLessThan(op0, op1, operand1, operand2))
                    else return! None
                | 186 -> // OpFOrdGreaterThan
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFOrdGreaterThan(op0, op1, operand1, operand2))
                    else return! None
                | 187 -> // OpFUnordGreaterThan
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFUnordGreaterThan(op0, op1, operand1, operand2))
                    else return! None
                | 188 -> // OpFOrdLessThanEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFOrdLessThanEqual(op0, op1, operand1, operand2))
                    else return! None
                | 189 -> // OpFUnordLessThanEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFUnordLessThanEqual(op0, op1, operand1, operand2))
                    else return! None
                | 190 -> // OpFOrdGreaterThanEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFOrdGreaterThanEqual(op0, op1, operand1, operand2))
                    else return! None
                | 191 -> // OpFUnordGreaterThanEqual
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFUnordGreaterThanEqual(op0, op1, operand1, operand2))
                    else return! None
                | 194 -> // OpShiftRightLogical
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    let! shift = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpShiftRightLogical(op0, op1, _base, shift))
                    else return! None
                | 195 -> // OpShiftRightArithmetic
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    let! shift = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpShiftRightArithmetic(op0, op1, _base, shift))
                    else return! None
                | 196 -> // OpShiftLeftLogical
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    let! shift = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpShiftLeftLogical(op0, op1, _base, shift))
                    else return! None
                | 197 -> // OpBitwiseOr
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBitwiseOr(op0, op1, operand1, operand2))
                    else return! None
                | 198 -> // OpBitwiseXor
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBitwiseXor(op0, op1, operand1, operand2))
                    else return! None
                | 199 -> // OpBitwiseAnd
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand1 = reader.ReadIdRef(remaining)
                    let! operand2 = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBitwiseAnd(op0, op1, operand1, operand2))
                    else return! None
                | 200 -> // OpNot
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! operand = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpNot(op0, op1, operand))
                    else return! None
                | 201 -> // OpBitFieldInsert
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    let! insert = reader.ReadIdRef(remaining)
                    let! offset = reader.ReadIdRef(remaining)
                    let! count = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBitFieldInsert(op0, op1, _base, insert, offset, count))
                    else return! None
                | 202 -> // OpBitFieldSExtract
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    let! offset = reader.ReadIdRef(remaining)
                    let! count = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBitFieldSExtract(op0, op1, _base, offset, count))
                    else return! None
                | 203 -> // OpBitFieldUExtract
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    let! offset = reader.ReadIdRef(remaining)
                    let! count = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBitFieldUExtract(op0, op1, _base, offset, count))
                    else return! None
                | 204 -> // OpBitReverse
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBitReverse(op0, op1, _base))
                    else return! None
                | 205 -> // OpBitCount
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! _base = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBitCount(op0, op1, _base))
                    else return! None
                | 207 -> // OpDPdx
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! p = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpDPdx(op0, op1, p))
                    else return! None
                | 208 -> // OpDPdy
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! p = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpDPdy(op0, op1, p))
                    else return! None
                | 209 -> // OpFwidth
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! p = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFwidth(op0, op1, p))
                    else return! None
                | 210 -> // OpDPdxFine
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! p = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpDPdxFine(op0, op1, p))
                    else return! None
                | 211 -> // OpDPdyFine
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! p = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpDPdyFine(op0, op1, p))
                    else return! None
                | 212 -> // OpFwidthFine
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! p = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFwidthFine(op0, op1, p))
                    else return! None
                | 213 -> // OpDPdxCoarse
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! p = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpDPdxCoarse(op0, op1, p))
                    else return! None
                | 214 -> // OpDPdyCoarse
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! p = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpDPdyCoarse(op0, op1, p))
                    else return! None
                | 215 -> // OpFwidthCoarse
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! p = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpFwidthCoarse(op0, op1, p))
                    else return! None
                | 218 -> // OpEmitVertex
                    if !remaining = 0 then return OpEmitVertex
                    else return! None
                | 219 -> // OpEndPrimitive
                    if !remaining = 0 then return OpEndPrimitive
                    else return! None
                | 220 -> // OpEmitStreamVertex
                    let! stream = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpEmitStreamVertex(stream))
                    else return! None
                | 221 -> // OpEndStreamPrimitive
                    let! stream = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpEndStreamPrimitive(stream))
                    else return! None
                | 224 -> // OpControlBarrier
                    let! execution = reader.ReadIdScope(remaining)
                    let! memory = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    if !remaining = 0 then return (OpControlBarrier(execution, memory, semantics))
                    else return! None
                | 225 -> // OpMemoryBarrier
                    let! memory = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    if !remaining = 0 then return (OpMemoryBarrier(memory, semantics))
                    else return! None
                | 227 -> // OpAtomicLoad
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    if !remaining = 0 then return (OpAtomicLoad(op0, op1, pointer, scope, semantics))
                    else return! None
                | 228 -> // OpAtomicStore
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicStore(pointer, scope, semantics, value))
                    else return! None
                | 229 -> // OpAtomicExchange
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicExchange(op0, op1, pointer, scope, semantics, value))
                    else return! None
                | 230 -> // OpAtomicCompareExchange
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! equal = reader.ReadIdMemorySemantics(remaining)
                    let! unequal = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    let! comparator = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicCompareExchange(op0, op1, pointer, scope, equal, unequal, value, comparator))
                    else return! None
                | 231 -> // OpAtomicCompareExchangeWeak
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! equal = reader.ReadIdMemorySemantics(remaining)
                    let! unequal = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    let! comparator = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicCompareExchangeWeak(op0, op1, pointer, scope, equal, unequal, value, comparator))
                    else return! None
                | 232 -> // OpAtomicIIncrement
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    if !remaining = 0 then return (OpAtomicIIncrement(op0, op1, pointer, scope, semantics))
                    else return! None
                | 233 -> // OpAtomicIDecrement
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    if !remaining = 0 then return (OpAtomicIDecrement(op0, op1, pointer, scope, semantics))
                    else return! None
                | 234 -> // OpAtomicIAdd
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicIAdd(op0, op1, pointer, scope, semantics, value))
                    else return! None
                | 235 -> // OpAtomicISub
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicISub(op0, op1, pointer, scope, semantics, value))
                    else return! None
                | 236 -> // OpAtomicSMin
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicSMin(op0, op1, pointer, scope, semantics, value))
                    else return! None
                | 237 -> // OpAtomicUMin
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicUMin(op0, op1, pointer, scope, semantics, value))
                    else return! None
                | 238 -> // OpAtomicSMax
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicSMax(op0, op1, pointer, scope, semantics, value))
                    else return! None
                | 239 -> // OpAtomicUMax
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicUMax(op0, op1, pointer, scope, semantics, value))
                    else return! None
                | 240 -> // OpAtomicAnd
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicAnd(op0, op1, pointer, scope, semantics, value))
                    else return! None
                | 241 -> // OpAtomicOr
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicOr(op0, op1, pointer, scope, semantics, value))
                    else return! None
                | 242 -> // OpAtomicXor
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpAtomicXor(op0, op1, pointer, scope, semantics, value))
                    else return! None
                | 245 -> // OpPhi
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let variableParent = reader.ReadArray(reader.ReadPairIdRefIdRef, remaining)
                    if !remaining = 0 then return (OpPhi(op0, op1, variableParent))
                    else return! None
                | 246 -> // OpLoopMerge
                    let! mergeBlock = reader.ReadIdRef(remaining)
                    let! continueTarget = reader.ReadIdRef(remaining)
                    let! op2 = reader.ReadLoopControl(remaining)
                    if !remaining = 0 then return (OpLoopMerge(mergeBlock, continueTarget, op2))
                    else return! None
                | 247 -> // OpSelectionMerge
                    let! mergeBlock = reader.ReadIdRef(remaining)
                    let! op1 = reader.ReadSelectionControl(remaining)
                    if !remaining = 0 then return (OpSelectionMerge(mergeBlock, op1))
                    else return! None
                | 248 -> // OpLabel
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpLabel(op0))
                    else return! None
                | 249 -> // OpBranch
                    let! targetLabel = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBranch(targetLabel))
                    else return! None
                | 250 -> // OpBranchConditional
                    let! condition = reader.ReadIdRef(remaining)
                    let! trueLabel = reader.ReadIdRef(remaining)
                    let! falseLabel = reader.ReadIdRef(remaining)
                    let branchweights = reader.ReadArray(reader.ReadLiteralInteger, remaining)
                    if !remaining = 0 then return (OpBranchConditional(condition, trueLabel, falseLabel, branchweights))
                    else return! None
                | 251 -> // OpSwitch
                    let! selector = reader.ReadIdRef(remaining)
                    let! _default = reader.ReadIdRef(remaining)
                    let target = reader.ReadArray(reader.ReadPairLiteralIntegerIdRef, remaining)
                    if !remaining = 0 then return (OpSwitch(selector, _default, target))
                    else return! None
                | 252 -> // OpKill
                    if !remaining = 0 then return OpKill
                    else return! None
                | 253 -> // OpReturn
                    if !remaining = 0 then return OpReturn
                    else return! None
                | 254 -> // OpReturnValue
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpReturnValue(value))
                    else return! None
                | 255 -> // OpUnreachable
                    if !remaining = 0 then return OpUnreachable
                    else return! None
                | 256 -> // OpLifetimeStart
                    let! pointer = reader.ReadIdRef(remaining)
                    let! size = reader.ReadLiteralInteger(remaining)
                    if !remaining = 0 then return (OpLifetimeStart(pointer, size))
                    else return! None
                | 257 -> // OpLifetimeStop
                    let! pointer = reader.ReadIdRef(remaining)
                    let! size = reader.ReadLiteralInteger(remaining)
                    if !remaining = 0 then return (OpLifetimeStop(pointer, size))
                    else return! None
                | 259 -> // OpGroupAsyncCopy
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! destination = reader.ReadIdRef(remaining)
                    let! source = reader.ReadIdRef(remaining)
                    let! numElements = reader.ReadIdRef(remaining)
                    let! stride = reader.ReadIdRef(remaining)
                    let! event = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupAsyncCopy(op0, op1, execution, destination, source, numElements, stride, event))
                    else return! None
                | 260 -> // OpGroupWaitEvents
                    let! execution = reader.ReadIdScope(remaining)
                    let! numEvents = reader.ReadIdRef(remaining)
                    let! eventsList = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupWaitEvents(execution, numEvents, eventsList))
                    else return! None
                | 261 -> // OpGroupAll
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! predicate = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupAll(op0, op1, execution, predicate))
                    else return! None
                | 262 -> // OpGroupAny
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! predicate = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupAny(op0, op1, execution, predicate))
                    else return! None
                | 263 -> // OpGroupBroadcast
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    let! localId = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupBroadcast(op0, op1, execution, value, localId))
                    else return! None
                | 264 -> // OpGroupIAdd
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupIAdd(op0, op1, execution, operation, x))
                    else return! None
                | 265 -> // OpGroupFAdd
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupFAdd(op0, op1, execution, operation, x))
                    else return! None
                | 266 -> // OpGroupFMin
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupFMin(op0, op1, execution, operation, x))
                    else return! None
                | 267 -> // OpGroupUMin
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupUMin(op0, op1, execution, operation, x))
                    else return! None
                | 268 -> // OpGroupSMin
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupSMin(op0, op1, execution, operation, x))
                    else return! None
                | 269 -> // OpGroupFMax
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupFMax(op0, op1, execution, operation, x))
                    else return! None
                | 270 -> // OpGroupUMax
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupUMax(op0, op1, execution, operation, x))
                    else return! None
                | 271 -> // OpGroupSMax
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupSMax(op0, op1, execution, operation, x))
                    else return! None
                | 274 -> // OpReadPipe
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpReadPipe(op0, op1, pipe, pointer, packetSize, packetAlignment))
                    else return! None
                | 275 -> // OpWritePipe
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpWritePipe(op0, op1, pipe, pointer, packetSize, packetAlignment))
                    else return! None
                | 276 -> // OpReservedReadPipe
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! reserveId = reader.ReadIdRef(remaining)
                    let! index = reader.ReadIdRef(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpReservedReadPipe(op0, op1, pipe, reserveId, index, pointer, packetSize, packetAlignment))
                    else return! None
                | 277 -> // OpReservedWritePipe
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! reserveId = reader.ReadIdRef(remaining)
                    let! index = reader.ReadIdRef(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpReservedWritePipe(op0, op1, pipe, reserveId, index, pointer, packetSize, packetAlignment))
                    else return! None
                | 278 -> // OpReserveReadPipePackets
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! numPackets = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpReserveReadPipePackets(op0, op1, pipe, numPackets, packetSize, packetAlignment))
                    else return! None
                | 279 -> // OpReserveWritePipePackets
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! numPackets = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpReserveWritePipePackets(op0, op1, pipe, numPackets, packetSize, packetAlignment))
                    else return! None
                | 280 -> // OpCommitReadPipe
                    let! pipe = reader.ReadIdRef(remaining)
                    let! reserveId = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpCommitReadPipe(pipe, reserveId, packetSize, packetAlignment))
                    else return! None
                | 281 -> // OpCommitWritePipe
                    let! pipe = reader.ReadIdRef(remaining)
                    let! reserveId = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpCommitWritePipe(pipe, reserveId, packetSize, packetAlignment))
                    else return! None
                | 282 -> // OpIsValidReserveId
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! reserveId = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpIsValidReserveId(op0, op1, reserveId))
                    else return! None
                | 283 -> // OpGetNumPipePackets
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGetNumPipePackets(op0, op1, pipe, packetSize, packetAlignment))
                    else return! None
                | 284 -> // OpGetMaxPipePackets
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGetMaxPipePackets(op0, op1, pipe, packetSize, packetAlignment))
                    else return! None
                | 285 -> // OpGroupReserveReadPipePackets
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! numPackets = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupReserveReadPipePackets(op0, op1, execution, pipe, numPackets, packetSize, packetAlignment))
                    else return! None
                | 286 -> // OpGroupReserveWritePipePackets
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! numPackets = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupReserveWritePipePackets(op0, op1, execution, pipe, numPackets, packetSize, packetAlignment))
                    else return! None
                | 287 -> // OpGroupCommitReadPipe
                    let! execution = reader.ReadIdScope(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! reserveId = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupCommitReadPipe(execution, pipe, reserveId, packetSize, packetAlignment))
                    else return! None
                | 288 -> // OpGroupCommitWritePipe
                    let! execution = reader.ReadIdScope(remaining)
                    let! pipe = reader.ReadIdRef(remaining)
                    let! reserveId = reader.ReadIdRef(remaining)
                    let! packetSize = reader.ReadIdRef(remaining)
                    let! packetAlignment = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupCommitWritePipe(execution, pipe, reserveId, packetSize, packetAlignment))
                    else return! None
                | 291 -> // OpEnqueueMarker
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! queue = reader.ReadIdRef(remaining)
                    let! numEvents = reader.ReadIdRef(remaining)
                    let! waitEvents = reader.ReadIdRef(remaining)
                    let! retEvent = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpEnqueueMarker(op0, op1, queue, numEvents, waitEvents, retEvent))
                    else return! None
                | 292 -> // OpEnqueueKernel
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! queue = reader.ReadIdRef(remaining)
                    let! flags = reader.ReadIdRef(remaining)
                    let! nDRange = reader.ReadIdRef(remaining)
                    let! numEvents = reader.ReadIdRef(remaining)
                    let! waitEvents = reader.ReadIdRef(remaining)
                    let! retEvent = reader.ReadIdRef(remaining)
                    let! invoke = reader.ReadIdRef(remaining)
                    let! param = reader.ReadIdRef(remaining)
                    let! paramSize = reader.ReadIdRef(remaining)
                    let! paramAlign = reader.ReadIdRef(remaining)
                    let localSize = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpEnqueueKernel(op0, op1, queue, flags, nDRange, numEvents, waitEvents, retEvent, invoke, param, paramSize, paramAlign, localSize))
                    else return! None
                | 293 -> // OpGetKernelNDrangeSubGroupCount
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! nDRange = reader.ReadIdRef(remaining)
                    let! invoke = reader.ReadIdRef(remaining)
                    let! param = reader.ReadIdRef(remaining)
                    let! paramSize = reader.ReadIdRef(remaining)
                    let! paramAlign = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGetKernelNDrangeSubGroupCount(op0, op1, nDRange, invoke, param, paramSize, paramAlign))
                    else return! None
                | 294 -> // OpGetKernelNDrangeMaxSubGroupSize
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! nDRange = reader.ReadIdRef(remaining)
                    let! invoke = reader.ReadIdRef(remaining)
                    let! param = reader.ReadIdRef(remaining)
                    let! paramSize = reader.ReadIdRef(remaining)
                    let! paramAlign = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGetKernelNDrangeMaxSubGroupSize(op0, op1, nDRange, invoke, param, paramSize, paramAlign))
                    else return! None
                | 295 -> // OpGetKernelWorkGroupSize
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! invoke = reader.ReadIdRef(remaining)
                    let! param = reader.ReadIdRef(remaining)
                    let! paramSize = reader.ReadIdRef(remaining)
                    let! paramAlign = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGetKernelWorkGroupSize(op0, op1, invoke, param, paramSize, paramAlign))
                    else return! None
                | 296 -> // OpGetKernelPreferredWorkGroupSizeMultiple
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! invoke = reader.ReadIdRef(remaining)
                    let! param = reader.ReadIdRef(remaining)
                    let! paramSize = reader.ReadIdRef(remaining)
                    let! paramAlign = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGetKernelPreferredWorkGroupSizeMultiple(op0, op1, invoke, param, paramSize, paramAlign))
                    else return! None
                | 297 -> // OpRetainEvent
                    let! event = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpRetainEvent(event))
                    else return! None
                | 298 -> // OpReleaseEvent
                    let! event = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpReleaseEvent(event))
                    else return! None
                | 299 -> // OpCreateUserEvent
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpCreateUserEvent(op0, op1))
                    else return! None
                | 300 -> // OpIsValidEvent
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! event = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpIsValidEvent(op0, op1, event))
                    else return! None
                | 301 -> // OpSetUserEventStatus
                    let! event = reader.ReadIdRef(remaining)
                    let! status = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSetUserEventStatus(event, status))
                    else return! None
                | 302 -> // OpCaptureEventProfilingInfo
                    let! event = reader.ReadIdRef(remaining)
                    let! profilingInfo = reader.ReadIdRef(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpCaptureEventProfilingInfo(event, profilingInfo, value))
                    else return! None
                | 303 -> // OpGetDefaultQueue
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpGetDefaultQueue(op0, op1))
                    else return! None
                | 304 -> // OpBuildNDRange
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! globalWorkSize = reader.ReadIdRef(remaining)
                    let! localWorkSize = reader.ReadIdRef(remaining)
                    let! globalWorkOffset = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpBuildNDRange(op0, op1, globalWorkSize, localWorkSize, globalWorkOffset))
                    else return! None
                | 305 -> // OpImageSparseSampleImplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseSampleImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters))
                    else return! None
                | 306 -> // OpImageSparseSampleExplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseSampleExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters))
                    else return! None
                | 307 -> // OpImageSparseSampleDrefImplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! dref = reader.ReadIdRef(remaining)
                    let op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseSampleDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters))
                    else return! None
                | 308 -> // OpImageSparseSampleDrefExplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! dref = reader.ReadIdRef(remaining)
                    let! op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseSampleDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters))
                    else return! None
                | 309 -> // OpImageSparseSampleProjImplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseSampleProjImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters))
                    else return! None
                | 310 -> // OpImageSparseSampleProjExplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseSampleProjExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters))
                    else return! None
                | 311 -> // OpImageSparseSampleProjDrefImplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! dref = reader.ReadIdRef(remaining)
                    let op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseSampleProjDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters))
                    else return! None
                | 312 -> // OpImageSparseSampleProjDrefExplicitLod
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! dref = reader.ReadIdRef(remaining)
                    let! op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseSampleProjDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters))
                    else return! None
                | 313 -> // OpImageSparseFetch
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseFetch(op0, op1, image, coordinate, op4, parameters))
                    else return! None
                | 314 -> // OpImageSparseGather
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! _component = reader.ReadIdRef(remaining)
                    let op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseGather(op0, op1, sampledImage, coordinate, _component, op5, parameters))
                    else return! None
                | 315 -> // OpImageSparseDrefGather
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! sampledImage = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let! dref = reader.ReadIdRef(remaining)
                    let op5 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseDrefGather(op0, op1, sampledImage, coordinate, dref, op5, parameters))
                    else return! None
                | 316 -> // OpImageSparseTexelsResident
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! residentCode = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpImageSparseTexelsResident(op0, op1, residentCode))
                    else return! None
                | 317 -> // OpNoLine
                    if !remaining = 0 then return OpNoLine
                    else return! None
                | 318 -> // OpAtomicFlagTestAndSet
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    if !remaining = 0 then return (OpAtomicFlagTestAndSet(op0, op1, pointer, scope, semantics))
                    else return! None
                | 319 -> // OpAtomicFlagClear
                    let! pointer = reader.ReadIdRef(remaining)
                    let! scope = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    if !remaining = 0 then return (OpAtomicFlagClear(pointer, scope, semantics))
                    else return! None
                | 320 -> // OpImageSparseRead
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! image = reader.ReadIdRef(remaining)
                    let! coordinate = reader.ReadIdRef(remaining)
                    let op4 = reader.ReadImageOperands(remaining)
                    let parameters = reader.ReadArray(reader.ReadIdRef, remaining)
                    if !remaining = 0 then return (OpImageSparseRead(op0, op1, image, coordinate, op4, parameters))
                    else return! None
                | 321 -> // OpSizeOf
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pointer = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSizeOf(op0, op1, pointer))
                    else return! None
                | 322 -> // OpTypePipeStorage
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpTypePipeStorage(op0))
                    else return! None
                | 323 -> // OpConstantPipeStorage
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! packetSize = reader.ReadLiteralInteger(remaining)
                    let! packetAlignment = reader.ReadLiteralInteger(remaining)
                    let! capacity = reader.ReadLiteralInteger(remaining)
                    if !remaining = 0 then return (OpConstantPipeStorage(op0, op1, packetSize, packetAlignment, capacity))
                    else return! None
                | 324 -> // OpCreatePipeFromPipeStorage
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! pipeStorage = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpCreatePipeFromPipeStorage(op0, op1, pipeStorage))
                    else return! None
                | 325 -> // OpGetKernelLocalSizeForSubgroupCount
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! subgroupCount = reader.ReadIdRef(remaining)
                    let! invoke = reader.ReadIdRef(remaining)
                    let! param = reader.ReadIdRef(remaining)
                    let! paramSize = reader.ReadIdRef(remaining)
                    let! paramAlign = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGetKernelLocalSizeForSubgroupCount(op0, op1, subgroupCount, invoke, param, paramSize, paramAlign))
                    else return! None
                | 326 -> // OpGetKernelMaxNumSubgroups
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! invoke = reader.ReadIdRef(remaining)
                    let! param = reader.ReadIdRef(remaining)
                    let! paramSize = reader.ReadIdRef(remaining)
                    let! paramAlign = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGetKernelMaxNumSubgroups(op0, op1, invoke, param, paramSize, paramAlign))
                    else return! None
                | 327 -> // OpTypeNamedBarrier
                    let! op0 = reader.ReadIdResult(remaining)
                    if !remaining = 0 then return (OpTypeNamedBarrier(op0))
                    else return! None
                | 328 -> // OpNamedBarrierInitialize
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! subgroupCount = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpNamedBarrierInitialize(op0, op1, subgroupCount))
                    else return! None
                | 329 -> // OpMemoryNamedBarrier
                    let! namedBarrier = reader.ReadIdRef(remaining)
                    let! memory = reader.ReadIdScope(remaining)
                    let! semantics = reader.ReadIdMemorySemantics(remaining)
                    if !remaining = 0 then return (OpMemoryNamedBarrier(namedBarrier, memory, semantics))
                    else return! None
                | 330 -> // OpModuleProcessed
                    let! _process = reader.ReadLiteralString(remaining)
                    if !remaining = 0 then return (OpModuleProcessed(_process))
                    else return! None
                | 331 -> // OpExecutionModeId
                    let! entryPoint = reader.ReadIdRef(remaining)
                    let! mode = reader.ReadExecutionMode(remaining)
                    if !remaining = 0 then return (OpExecutionModeId(entryPoint, mode))
                    else return! None
                | 332 -> // OpDecorateId
                    let! target = reader.ReadIdRef(remaining)
                    let! op1 = reader.ReadDecoration(remaining)
                    if !remaining = 0 then return (OpDecorateId(target, op1))
                    else return! None
                | 4421 -> // OpSubgroupBallotKHR
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! predicate = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSubgroupBallotKHR(op0, op1, predicate))
                    else return! None
                | 4422 -> // OpSubgroupFirstInvocationKHR
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSubgroupFirstInvocationKHR(op0, op1, value))
                    else return! None
                | 4428 -> // OpSubgroupAllKHR
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! predicate = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSubgroupAllKHR(op0, op1, predicate))
                    else return! None
                | 4429 -> // OpSubgroupAnyKHR
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! predicate = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSubgroupAnyKHR(op0, op1, predicate))
                    else return! None
                | 4430 -> // OpSubgroupAllEqualKHR
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! predicate = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSubgroupAllEqualKHR(op0, op1, predicate))
                    else return! None
                | 4432 -> // OpSubgroupReadInvocationKHR
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! value = reader.ReadIdRef(remaining)
                    let! index = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpSubgroupReadInvocationKHR(op0, op1, value, index))
                    else return! None
                | 5000 -> // OpGroupIAddNonUniformAMD
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupIAddNonUniformAMD(op0, op1, execution, operation, x))
                    else return! None
                | 5001 -> // OpGroupFAddNonUniformAMD
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupFAddNonUniformAMD(op0, op1, execution, operation, x))
                    else return! None
                | 5002 -> // OpGroupFMinNonUniformAMD
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupFMinNonUniformAMD(op0, op1, execution, operation, x))
                    else return! None
                | 5003 -> // OpGroupUMinNonUniformAMD
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupUMinNonUniformAMD(op0, op1, execution, operation, x))
                    else return! None
                | 5004 -> // OpGroupSMinNonUniformAMD
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupSMinNonUniformAMD(op0, op1, execution, operation, x))
                    else return! None
                | 5005 -> // OpGroupFMaxNonUniformAMD
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupFMaxNonUniformAMD(op0, op1, execution, operation, x))
                    else return! None
                | 5006 -> // OpGroupUMaxNonUniformAMD
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupUMaxNonUniformAMD(op0, op1, execution, operation, x))
                    else return! None
                | 5007 -> // OpGroupSMaxNonUniformAMD
                    let! op0 = reader.ReadIdResultType(remaining)
                    let! op1 = reader.ReadIdResult(remaining)
                    let! execution = reader.ReadIdScope(remaining)
                    let! operation = reader.ReadGroupOperation(remaining)
                    let! x = reader.ReadIdRef(remaining)
                    if !remaining = 0 then return (OpGroupSMaxNonUniformAMD(op0, op1, execution, operation, x))
                    else return! None
                | _ -> return! None
        }

    let readFrom (reader : BinaryReader) = tryReadFrom reader |> Option.get


    let resultType (i : Instruction) = 
        match i with
        | OpUndef(op0, op1) -> Some op0
        | OpExtInst(op0, op1, set, instruction, operandOperand) -> Some op0
        | OpConstantTrue(op0, op1) -> Some op0
        | OpConstantFalse(op0, op1) -> Some op0
        | OpConstant(op0, op1, value) -> Some op0
        | OpConstantComposite(op0, op1, constituents) -> Some op0
        | OpConstantSampler(op0, op1, op2, param, op4) -> Some op0
        | OpConstantNull(op0, op1) -> Some op0
        | OpSpecConstantTrue(op0, op1) -> Some op0
        | OpSpecConstantFalse(op0, op1) -> Some op0
        | OpSpecConstant(op0, op1, value) -> Some op0
        | OpSpecConstantComposite(op0, op1, constituents) -> Some op0
        | OpSpecConstantOp(op0, op1, opcode) -> Some op0
        | OpFunction(op0, op1, op2, functionType) -> Some op0
        | OpFunctionParameter(op0, op1) -> Some op0
        | OpFunctionCall(op0, op1, _function, argumentArgument) -> Some op0
        | OpVariable(op0, op1, op2, initializer) -> Some op0
        | OpImageTexelPointer(op0, op1, image, coordinate, sample) -> Some op0
        | OpLoad(op0, op1, pointer, op3) -> Some op0
        | OpAccessChain(op0, op1, _base, indexes) -> Some op0
        | OpInBoundsAccessChain(op0, op1, _base, indexes) -> Some op0
        | OpPtrAccessChain(op0, op1, _base, element, indexes) -> Some op0
        | OpArrayLength(op0, op1, structure, arraymember) -> Some op0
        | OpGenericPtrMemSemantics(op0, op1, pointer) -> Some op0
        | OpInBoundsPtrAccessChain(op0, op1, _base, element, indexes) -> Some op0
        | OpVectorExtractDynamic(op0, op1, vector, index) -> Some op0
        | OpVectorInsertDynamic(op0, op1, vector, _component, index) -> Some op0
        | OpVectorShuffle(op0, op1, vector1, vector2, components) -> Some op0
        | OpCompositeConstruct(op0, op1, constituents) -> Some op0
        | OpCompositeExtract(op0, op1, composite, indexes) -> Some op0
        | OpCompositeInsert(op0, op1, _object, composite, indexes) -> Some op0
        | OpCopyObject(op0, op1, operand) -> Some op0
        | OpTranspose(op0, op1, matrix) -> Some op0
        | OpSampledImage(op0, op1, image, sampler) -> Some op0
        | OpImageSampleImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op0
        | OpImageSampleExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op0
        | OpImageSampleDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op0
        | OpImageSampleDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op0
        | OpImageSampleProjImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op0
        | OpImageSampleProjExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op0
        | OpImageSampleProjDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op0
        | OpImageSampleProjDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op0
        | OpImageFetch(op0, op1, image, coordinate, op4, parameters) -> Some op0
        | OpImageGather(op0, op1, sampledImage, coordinate, _component, op5, parameters) -> Some op0
        | OpImageDrefGather(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op0
        | OpImageRead(op0, op1, image, coordinate, op4, parameters) -> Some op0
        | OpImage(op0, op1, sampledImage) -> Some op0
        | OpImageQueryFormat(op0, op1, image) -> Some op0
        | OpImageQueryOrder(op0, op1, image) -> Some op0
        | OpImageQuerySizeLod(op0, op1, image, levelofDetail) -> Some op0
        | OpImageQuerySize(op0, op1, image) -> Some op0
        | OpImageQueryLod(op0, op1, sampledImage, coordinate) -> Some op0
        | OpImageQueryLevels(op0, op1, image) -> Some op0
        | OpImageQuerySamples(op0, op1, image) -> Some op0
        | OpConvertFToU(op0, op1, floatValue) -> Some op0
        | OpConvertFToS(op0, op1, floatValue) -> Some op0
        | OpConvertSToF(op0, op1, signedValue) -> Some op0
        | OpConvertUToF(op0, op1, unsignedValue) -> Some op0
        | OpUConvert(op0, op1, unsignedValue) -> Some op0
        | OpSConvert(op0, op1, signedValue) -> Some op0
        | OpFConvert(op0, op1, floatValue) -> Some op0
        | OpQuantizeToF16(op0, op1, value) -> Some op0
        | OpConvertPtrToU(op0, op1, pointer) -> Some op0
        | OpSatConvertSToU(op0, op1, signedValue) -> Some op0
        | OpSatConvertUToS(op0, op1, unsignedValue) -> Some op0
        | OpConvertUToPtr(op0, op1, integerValue) -> Some op0
        | OpPtrCastToGeneric(op0, op1, pointer) -> Some op0
        | OpGenericCastToPtr(op0, op1, pointer) -> Some op0
        | OpGenericCastToPtrExplicit(op0, op1, pointer, storage) -> Some op0
        | OpBitcast(op0, op1, operand) -> Some op0
        | OpSNegate(op0, op1, operand) -> Some op0
        | OpFNegate(op0, op1, operand) -> Some op0
        | OpIAdd(op0, op1, operand1, operand2) -> Some op0
        | OpFAdd(op0, op1, operand1, operand2) -> Some op0
        | OpISub(op0, op1, operand1, operand2) -> Some op0
        | OpFSub(op0, op1, operand1, operand2) -> Some op0
        | OpIMul(op0, op1, operand1, operand2) -> Some op0
        | OpFMul(op0, op1, operand1, operand2) -> Some op0
        | OpUDiv(op0, op1, operand1, operand2) -> Some op0
        | OpSDiv(op0, op1, operand1, operand2) -> Some op0
        | OpFDiv(op0, op1, operand1, operand2) -> Some op0
        | OpUMod(op0, op1, operand1, operand2) -> Some op0
        | OpSRem(op0, op1, operand1, operand2) -> Some op0
        | OpSMod(op0, op1, operand1, operand2) -> Some op0
        | OpFRem(op0, op1, operand1, operand2) -> Some op0
        | OpFMod(op0, op1, operand1, operand2) -> Some op0
        | OpVectorTimesScalar(op0, op1, vector, scalar) -> Some op0
        | OpMatrixTimesScalar(op0, op1, matrix, scalar) -> Some op0
        | OpVectorTimesMatrix(op0, op1, vector, matrix) -> Some op0
        | OpMatrixTimesVector(op0, op1, matrix, vector) -> Some op0
        | OpMatrixTimesMatrix(op0, op1, leftMatrix, rightMatrix) -> Some op0
        | OpOuterProduct(op0, op1, vector1, vector2) -> Some op0
        | OpDot(op0, op1, vector1, vector2) -> Some op0
        | OpIAddCarry(op0, op1, operand1, operand2) -> Some op0
        | OpISubBorrow(op0, op1, operand1, operand2) -> Some op0
        | OpUMulExtended(op0, op1, operand1, operand2) -> Some op0
        | OpSMulExtended(op0, op1, operand1, operand2) -> Some op0
        | OpAny(op0, op1, vector) -> Some op0
        | OpAll(op0, op1, vector) -> Some op0
        | OpIsNan(op0, op1, x) -> Some op0
        | OpIsInf(op0, op1, x) -> Some op0
        | OpIsFinite(op0, op1, x) -> Some op0
        | OpIsNormal(op0, op1, x) -> Some op0
        | OpSignBitSet(op0, op1, x) -> Some op0
        | OpLessOrGreater(op0, op1, x, y) -> Some op0
        | OpOrdered(op0, op1, x, y) -> Some op0
        | OpUnordered(op0, op1, x, y) -> Some op0
        | OpLogicalEqual(op0, op1, operand1, operand2) -> Some op0
        | OpLogicalNotEqual(op0, op1, operand1, operand2) -> Some op0
        | OpLogicalOr(op0, op1, operand1, operand2) -> Some op0
        | OpLogicalAnd(op0, op1, operand1, operand2) -> Some op0
        | OpLogicalNot(op0, op1, operand) -> Some op0
        | OpSelect(op0, op1, condition, _object1, _object2) -> Some op0
        | OpIEqual(op0, op1, operand1, operand2) -> Some op0
        | OpINotEqual(op0, op1, operand1, operand2) -> Some op0
        | OpUGreaterThan(op0, op1, operand1, operand2) -> Some op0
        | OpSGreaterThan(op0, op1, operand1, operand2) -> Some op0
        | OpUGreaterThanEqual(op0, op1, operand1, operand2) -> Some op0
        | OpSGreaterThanEqual(op0, op1, operand1, operand2) -> Some op0
        | OpULessThan(op0, op1, operand1, operand2) -> Some op0
        | OpSLessThan(op0, op1, operand1, operand2) -> Some op0
        | OpULessThanEqual(op0, op1, operand1, operand2) -> Some op0
        | OpSLessThanEqual(op0, op1, operand1, operand2) -> Some op0
        | OpFOrdEqual(op0, op1, operand1, operand2) -> Some op0
        | OpFUnordEqual(op0, op1, operand1, operand2) -> Some op0
        | OpFOrdNotEqual(op0, op1, operand1, operand2) -> Some op0
        | OpFUnordNotEqual(op0, op1, operand1, operand2) -> Some op0
        | OpFOrdLessThan(op0, op1, operand1, operand2) -> Some op0
        | OpFUnordLessThan(op0, op1, operand1, operand2) -> Some op0
        | OpFOrdGreaterThan(op0, op1, operand1, operand2) -> Some op0
        | OpFUnordGreaterThan(op0, op1, operand1, operand2) -> Some op0
        | OpFOrdLessThanEqual(op0, op1, operand1, operand2) -> Some op0
        | OpFUnordLessThanEqual(op0, op1, operand1, operand2) -> Some op0
        | OpFOrdGreaterThanEqual(op0, op1, operand1, operand2) -> Some op0
        | OpFUnordGreaterThanEqual(op0, op1, operand1, operand2) -> Some op0
        | OpShiftRightLogical(op0, op1, _base, shift) -> Some op0
        | OpShiftRightArithmetic(op0, op1, _base, shift) -> Some op0
        | OpShiftLeftLogical(op0, op1, _base, shift) -> Some op0
        | OpBitwiseOr(op0, op1, operand1, operand2) -> Some op0
        | OpBitwiseXor(op0, op1, operand1, operand2) -> Some op0
        | OpBitwiseAnd(op0, op1, operand1, operand2) -> Some op0
        | OpNot(op0, op1, operand) -> Some op0
        | OpBitFieldInsert(op0, op1, _base, insert, offset, count) -> Some op0
        | OpBitFieldSExtract(op0, op1, _base, offset, count) -> Some op0
        | OpBitFieldUExtract(op0, op1, _base, offset, count) -> Some op0
        | OpBitReverse(op0, op1, _base) -> Some op0
        | OpBitCount(op0, op1, _base) -> Some op0
        | OpDPdx(op0, op1, p) -> Some op0
        | OpDPdy(op0, op1, p) -> Some op0
        | OpFwidth(op0, op1, p) -> Some op0
        | OpDPdxFine(op0, op1, p) -> Some op0
        | OpDPdyFine(op0, op1, p) -> Some op0
        | OpFwidthFine(op0, op1, p) -> Some op0
        | OpDPdxCoarse(op0, op1, p) -> Some op0
        | OpDPdyCoarse(op0, op1, p) -> Some op0
        | OpFwidthCoarse(op0, op1, p) -> Some op0
        | OpAtomicLoad(op0, op1, pointer, scope, semantics) -> Some op0
        | OpAtomicExchange(op0, op1, pointer, scope, semantics, value) -> Some op0
        | OpAtomicCompareExchange(op0, op1, pointer, scope, equal, unequal, value, comparator) -> Some op0
        | OpAtomicCompareExchangeWeak(op0, op1, pointer, scope, equal, unequal, value, comparator) -> Some op0
        | OpAtomicIIncrement(op0, op1, pointer, scope, semantics) -> Some op0
        | OpAtomicIDecrement(op0, op1, pointer, scope, semantics) -> Some op0
        | OpAtomicIAdd(op0, op1, pointer, scope, semantics, value) -> Some op0
        | OpAtomicISub(op0, op1, pointer, scope, semantics, value) -> Some op0
        | OpAtomicSMin(op0, op1, pointer, scope, semantics, value) -> Some op0
        | OpAtomicUMin(op0, op1, pointer, scope, semantics, value) -> Some op0
        | OpAtomicSMax(op0, op1, pointer, scope, semantics, value) -> Some op0
        | OpAtomicUMax(op0, op1, pointer, scope, semantics, value) -> Some op0
        | OpAtomicAnd(op0, op1, pointer, scope, semantics, value) -> Some op0
        | OpAtomicOr(op0, op1, pointer, scope, semantics, value) -> Some op0
        | OpAtomicXor(op0, op1, pointer, scope, semantics, value) -> Some op0
        | OpPhi(op0, op1, variableParent) -> Some op0
        | OpGroupAsyncCopy(op0, op1, execution, destination, source, numElements, stride, event) -> Some op0
        | OpGroupAll(op0, op1, execution, predicate) -> Some op0
        | OpGroupAny(op0, op1, execution, predicate) -> Some op0
        | OpGroupBroadcast(op0, op1, execution, value, localId) -> Some op0
        | OpGroupIAdd(op0, op1, execution, operation, x) -> Some op0
        | OpGroupFAdd(op0, op1, execution, operation, x) -> Some op0
        | OpGroupFMin(op0, op1, execution, operation, x) -> Some op0
        | OpGroupUMin(op0, op1, execution, operation, x) -> Some op0
        | OpGroupSMin(op0, op1, execution, operation, x) -> Some op0
        | OpGroupFMax(op0, op1, execution, operation, x) -> Some op0
        | OpGroupUMax(op0, op1, execution, operation, x) -> Some op0
        | OpGroupSMax(op0, op1, execution, operation, x) -> Some op0
        | OpReadPipe(op0, op1, pipe, pointer, packetSize, packetAlignment) -> Some op0
        | OpWritePipe(op0, op1, pipe, pointer, packetSize, packetAlignment) -> Some op0
        | OpReservedReadPipe(op0, op1, pipe, reserveId, index, pointer, packetSize, packetAlignment) -> Some op0
        | OpReservedWritePipe(op0, op1, pipe, reserveId, index, pointer, packetSize, packetAlignment) -> Some op0
        | OpReserveReadPipePackets(op0, op1, pipe, numPackets, packetSize, packetAlignment) -> Some op0
        | OpReserveWritePipePackets(op0, op1, pipe, numPackets, packetSize, packetAlignment) -> Some op0
        | OpIsValidReserveId(op0, op1, reserveId) -> Some op0
        | OpGetNumPipePackets(op0, op1, pipe, packetSize, packetAlignment) -> Some op0
        | OpGetMaxPipePackets(op0, op1, pipe, packetSize, packetAlignment) -> Some op0
        | OpGroupReserveReadPipePackets(op0, op1, execution, pipe, numPackets, packetSize, packetAlignment) -> Some op0
        | OpGroupReserveWritePipePackets(op0, op1, execution, pipe, numPackets, packetSize, packetAlignment) -> Some op0
        | OpEnqueueMarker(op0, op1, queue, numEvents, waitEvents, retEvent) -> Some op0
        | OpEnqueueKernel(op0, op1, queue, flags, nDRange, numEvents, waitEvents, retEvent, invoke, param, paramSize, paramAlign, localSize) -> Some op0
        | OpGetKernelNDrangeSubGroupCount(op0, op1, nDRange, invoke, param, paramSize, paramAlign) -> Some op0
        | OpGetKernelNDrangeMaxSubGroupSize(op0, op1, nDRange, invoke, param, paramSize, paramAlign) -> Some op0
        | OpGetKernelWorkGroupSize(op0, op1, invoke, param, paramSize, paramAlign) -> Some op0
        | OpGetKernelPreferredWorkGroupSizeMultiple(op0, op1, invoke, param, paramSize, paramAlign) -> Some op0
        | OpCreateUserEvent(op0, op1) -> Some op0
        | OpIsValidEvent(op0, op1, event) -> Some op0
        | OpGetDefaultQueue(op0, op1) -> Some op0
        | OpBuildNDRange(op0, op1, globalWorkSize, localWorkSize, globalWorkOffset) -> Some op0
        | OpImageSparseSampleImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op0
        | OpImageSparseSampleExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op0
        | OpImageSparseSampleDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op0
        | OpImageSparseSampleDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op0
        | OpImageSparseSampleProjImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op0
        | OpImageSparseSampleProjExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op0
        | OpImageSparseSampleProjDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op0
        | OpImageSparseSampleProjDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op0
        | OpImageSparseFetch(op0, op1, image, coordinate, op4, parameters) -> Some op0
        | OpImageSparseGather(op0, op1, sampledImage, coordinate, _component, op5, parameters) -> Some op0
        | OpImageSparseDrefGather(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op0
        | OpImageSparseTexelsResident(op0, op1, residentCode) -> Some op0
        | OpAtomicFlagTestAndSet(op0, op1, pointer, scope, semantics) -> Some op0
        | OpImageSparseRead(op0, op1, image, coordinate, op4, parameters) -> Some op0
        | OpSizeOf(op0, op1, pointer) -> Some op0
        | OpConstantPipeStorage(op0, op1, packetSize, packetAlignment, capacity) -> Some op0
        | OpCreatePipeFromPipeStorage(op0, op1, pipeStorage) -> Some op0
        | OpGetKernelLocalSizeForSubgroupCount(op0, op1, subgroupCount, invoke, param, paramSize, paramAlign) -> Some op0
        | OpGetKernelMaxNumSubgroups(op0, op1, invoke, param, paramSize, paramAlign) -> Some op0
        | OpNamedBarrierInitialize(op0, op1, subgroupCount) -> Some op0
        | OpSubgroupBallotKHR(op0, op1, predicate) -> Some op0
        | OpSubgroupFirstInvocationKHR(op0, op1, value) -> Some op0
        | OpSubgroupAllKHR(op0, op1, predicate) -> Some op0
        | OpSubgroupAnyKHR(op0, op1, predicate) -> Some op0
        | OpSubgroupAllEqualKHR(op0, op1, predicate) -> Some op0
        | OpSubgroupReadInvocationKHR(op0, op1, value, index) -> Some op0
        | OpGroupIAddNonUniformAMD(op0, op1, execution, operation, x) -> Some op0
        | OpGroupFAddNonUniformAMD(op0, op1, execution, operation, x) -> Some op0
        | OpGroupFMinNonUniformAMD(op0, op1, execution, operation, x) -> Some op0
        | OpGroupUMinNonUniformAMD(op0, op1, execution, operation, x) -> Some op0
        | OpGroupSMinNonUniformAMD(op0, op1, execution, operation, x) -> Some op0
        | OpGroupFMaxNonUniformAMD(op0, op1, execution, operation, x) -> Some op0
        | OpGroupUMaxNonUniformAMD(op0, op1, execution, operation, x) -> Some op0
        | OpGroupSMaxNonUniformAMD(op0, op1, execution, operation, x) -> Some op0
        | _ -> None

    let resultId (i : Instruction) = 
        match i with
        | OpUndef(op0, op1) -> Some op1
        | OpString(op0, string) -> Some op0
        | OpExtInstImport(op0, name) -> Some op0
        | OpExtInst(op0, op1, set, instruction, operandOperand) -> Some op1
        | OpTypeVoid(op0) -> Some op0
        | OpTypeBool(op0) -> Some op0
        | OpTypeInt(op0, width, signedness) -> Some op0
        | OpTypeFloat(op0, width) -> Some op0
        | OpTypeVector(op0, componentType, componentCount) -> Some op0
        | OpTypeMatrix(op0, columnType, columnCount) -> Some op0
        | OpTypeImage(op0, sampledType, op2, depth, arrayed, mS, sampled, op7, op8) -> Some op0
        | OpTypeSampler(op0) -> Some op0
        | OpTypeSampledImage(op0, imageType) -> Some op0
        | OpTypeArray(op0, elementType, length) -> Some op0
        | OpTypeRuntimeArray(op0, elementType) -> Some op0
        | OpTypeStruct(op0, membertypemembertype) -> Some op0
        | OpTypeOpaque(op0, thenameoftheopaquetype) -> Some op0
        | OpTypePointer(op0, op1, _type) -> Some op0
        | OpTypeFunction(op0, returnType, parameterTypeParameterType) -> Some op0
        | OpTypeEvent(op0) -> Some op0
        | OpTypeDeviceEvent(op0) -> Some op0
        | OpTypeReserveId(op0) -> Some op0
        | OpTypeQueue(op0) -> Some op0
        | OpTypePipe(op0, qualifier) -> Some op0
        | OpConstantTrue(op0, op1) -> Some op1
        | OpConstantFalse(op0, op1) -> Some op1
        | OpConstant(op0, op1, value) -> Some op1
        | OpConstantComposite(op0, op1, constituents) -> Some op1
        | OpConstantSampler(op0, op1, op2, param, op4) -> Some op1
        | OpConstantNull(op0, op1) -> Some op1
        | OpSpecConstantTrue(op0, op1) -> Some op1
        | OpSpecConstantFalse(op0, op1) -> Some op1
        | OpSpecConstant(op0, op1, value) -> Some op1
        | OpSpecConstantComposite(op0, op1, constituents) -> Some op1
        | OpSpecConstantOp(op0, op1, opcode) -> Some op1
        | OpFunction(op0, op1, op2, functionType) -> Some op1
        | OpFunctionParameter(op0, op1) -> Some op1
        | OpFunctionCall(op0, op1, _function, argumentArgument) -> Some op1
        | OpVariable(op0, op1, op2, initializer) -> Some op1
        | OpImageTexelPointer(op0, op1, image, coordinate, sample) -> Some op1
        | OpLoad(op0, op1, pointer, op3) -> Some op1
        | OpAccessChain(op0, op1, _base, indexes) -> Some op1
        | OpInBoundsAccessChain(op0, op1, _base, indexes) -> Some op1
        | OpPtrAccessChain(op0, op1, _base, element, indexes) -> Some op1
        | OpArrayLength(op0, op1, structure, arraymember) -> Some op1
        | OpGenericPtrMemSemantics(op0, op1, pointer) -> Some op1
        | OpInBoundsPtrAccessChain(op0, op1, _base, element, indexes) -> Some op1
        | OpDecorationGroup(op0) -> Some op0
        | OpVectorExtractDynamic(op0, op1, vector, index) -> Some op1
        | OpVectorInsertDynamic(op0, op1, vector, _component, index) -> Some op1
        | OpVectorShuffle(op0, op1, vector1, vector2, components) -> Some op1
        | OpCompositeConstruct(op0, op1, constituents) -> Some op1
        | OpCompositeExtract(op0, op1, composite, indexes) -> Some op1
        | OpCompositeInsert(op0, op1, _object, composite, indexes) -> Some op1
        | OpCopyObject(op0, op1, operand) -> Some op1
        | OpTranspose(op0, op1, matrix) -> Some op1
        | OpSampledImage(op0, op1, image, sampler) -> Some op1
        | OpImageSampleImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op1
        | OpImageSampleExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op1
        | OpImageSampleDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op1
        | OpImageSampleDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op1
        | OpImageSampleProjImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op1
        | OpImageSampleProjExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op1
        | OpImageSampleProjDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op1
        | OpImageSampleProjDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op1
        | OpImageFetch(op0, op1, image, coordinate, op4, parameters) -> Some op1
        | OpImageGather(op0, op1, sampledImage, coordinate, _component, op5, parameters) -> Some op1
        | OpImageDrefGather(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op1
        | OpImageRead(op0, op1, image, coordinate, op4, parameters) -> Some op1
        | OpImage(op0, op1, sampledImage) -> Some op1
        | OpImageQueryFormat(op0, op1, image) -> Some op1
        | OpImageQueryOrder(op0, op1, image) -> Some op1
        | OpImageQuerySizeLod(op0, op1, image, levelofDetail) -> Some op1
        | OpImageQuerySize(op0, op1, image) -> Some op1
        | OpImageQueryLod(op0, op1, sampledImage, coordinate) -> Some op1
        | OpImageQueryLevels(op0, op1, image) -> Some op1
        | OpImageQuerySamples(op0, op1, image) -> Some op1
        | OpConvertFToU(op0, op1, floatValue) -> Some op1
        | OpConvertFToS(op0, op1, floatValue) -> Some op1
        | OpConvertSToF(op0, op1, signedValue) -> Some op1
        | OpConvertUToF(op0, op1, unsignedValue) -> Some op1
        | OpUConvert(op0, op1, unsignedValue) -> Some op1
        | OpSConvert(op0, op1, signedValue) -> Some op1
        | OpFConvert(op0, op1, floatValue) -> Some op1
        | OpQuantizeToF16(op0, op1, value) -> Some op1
        | OpConvertPtrToU(op0, op1, pointer) -> Some op1
        | OpSatConvertSToU(op0, op1, signedValue) -> Some op1
        | OpSatConvertUToS(op0, op1, unsignedValue) -> Some op1
        | OpConvertUToPtr(op0, op1, integerValue) -> Some op1
        | OpPtrCastToGeneric(op0, op1, pointer) -> Some op1
        | OpGenericCastToPtr(op0, op1, pointer) -> Some op1
        | OpGenericCastToPtrExplicit(op0, op1, pointer, storage) -> Some op1
        | OpBitcast(op0, op1, operand) -> Some op1
        | OpSNegate(op0, op1, operand) -> Some op1
        | OpFNegate(op0, op1, operand) -> Some op1
        | OpIAdd(op0, op1, operand1, operand2) -> Some op1
        | OpFAdd(op0, op1, operand1, operand2) -> Some op1
        | OpISub(op0, op1, operand1, operand2) -> Some op1
        | OpFSub(op0, op1, operand1, operand2) -> Some op1
        | OpIMul(op0, op1, operand1, operand2) -> Some op1
        | OpFMul(op0, op1, operand1, operand2) -> Some op1
        | OpUDiv(op0, op1, operand1, operand2) -> Some op1
        | OpSDiv(op0, op1, operand1, operand2) -> Some op1
        | OpFDiv(op0, op1, operand1, operand2) -> Some op1
        | OpUMod(op0, op1, operand1, operand2) -> Some op1
        | OpSRem(op0, op1, operand1, operand2) -> Some op1
        | OpSMod(op0, op1, operand1, operand2) -> Some op1
        | OpFRem(op0, op1, operand1, operand2) -> Some op1
        | OpFMod(op0, op1, operand1, operand2) -> Some op1
        | OpVectorTimesScalar(op0, op1, vector, scalar) -> Some op1
        | OpMatrixTimesScalar(op0, op1, matrix, scalar) -> Some op1
        | OpVectorTimesMatrix(op0, op1, vector, matrix) -> Some op1
        | OpMatrixTimesVector(op0, op1, matrix, vector) -> Some op1
        | OpMatrixTimesMatrix(op0, op1, leftMatrix, rightMatrix) -> Some op1
        | OpOuterProduct(op0, op1, vector1, vector2) -> Some op1
        | OpDot(op0, op1, vector1, vector2) -> Some op1
        | OpIAddCarry(op0, op1, operand1, operand2) -> Some op1
        | OpISubBorrow(op0, op1, operand1, operand2) -> Some op1
        | OpUMulExtended(op0, op1, operand1, operand2) -> Some op1
        | OpSMulExtended(op0, op1, operand1, operand2) -> Some op1
        | OpAny(op0, op1, vector) -> Some op1
        | OpAll(op0, op1, vector) -> Some op1
        | OpIsNan(op0, op1, x) -> Some op1
        | OpIsInf(op0, op1, x) -> Some op1
        | OpIsFinite(op0, op1, x) -> Some op1
        | OpIsNormal(op0, op1, x) -> Some op1
        | OpSignBitSet(op0, op1, x) -> Some op1
        | OpLessOrGreater(op0, op1, x, y) -> Some op1
        | OpOrdered(op0, op1, x, y) -> Some op1
        | OpUnordered(op0, op1, x, y) -> Some op1
        | OpLogicalEqual(op0, op1, operand1, operand2) -> Some op1
        | OpLogicalNotEqual(op0, op1, operand1, operand2) -> Some op1
        | OpLogicalOr(op0, op1, operand1, operand2) -> Some op1
        | OpLogicalAnd(op0, op1, operand1, operand2) -> Some op1
        | OpLogicalNot(op0, op1, operand) -> Some op1
        | OpSelect(op0, op1, condition, _object1, _object2) -> Some op1
        | OpIEqual(op0, op1, operand1, operand2) -> Some op1
        | OpINotEqual(op0, op1, operand1, operand2) -> Some op1
        | OpUGreaterThan(op0, op1, operand1, operand2) -> Some op1
        | OpSGreaterThan(op0, op1, operand1, operand2) -> Some op1
        | OpUGreaterThanEqual(op0, op1, operand1, operand2) -> Some op1
        | OpSGreaterThanEqual(op0, op1, operand1, operand2) -> Some op1
        | OpULessThan(op0, op1, operand1, operand2) -> Some op1
        | OpSLessThan(op0, op1, operand1, operand2) -> Some op1
        | OpULessThanEqual(op0, op1, operand1, operand2) -> Some op1
        | OpSLessThanEqual(op0, op1, operand1, operand2) -> Some op1
        | OpFOrdEqual(op0, op1, operand1, operand2) -> Some op1
        | OpFUnordEqual(op0, op1, operand1, operand2) -> Some op1
        | OpFOrdNotEqual(op0, op1, operand1, operand2) -> Some op1
        | OpFUnordNotEqual(op0, op1, operand1, operand2) -> Some op1
        | OpFOrdLessThan(op0, op1, operand1, operand2) -> Some op1
        | OpFUnordLessThan(op0, op1, operand1, operand2) -> Some op1
        | OpFOrdGreaterThan(op0, op1, operand1, operand2) -> Some op1
        | OpFUnordGreaterThan(op0, op1, operand1, operand2) -> Some op1
        | OpFOrdLessThanEqual(op0, op1, operand1, operand2) -> Some op1
        | OpFUnordLessThanEqual(op0, op1, operand1, operand2) -> Some op1
        | OpFOrdGreaterThanEqual(op0, op1, operand1, operand2) -> Some op1
        | OpFUnordGreaterThanEqual(op0, op1, operand1, operand2) -> Some op1
        | OpShiftRightLogical(op0, op1, _base, shift) -> Some op1
        | OpShiftRightArithmetic(op0, op1, _base, shift) -> Some op1
        | OpShiftLeftLogical(op0, op1, _base, shift) -> Some op1
        | OpBitwiseOr(op0, op1, operand1, operand2) -> Some op1
        | OpBitwiseXor(op0, op1, operand1, operand2) -> Some op1
        | OpBitwiseAnd(op0, op1, operand1, operand2) -> Some op1
        | OpNot(op0, op1, operand) -> Some op1
        | OpBitFieldInsert(op0, op1, _base, insert, offset, count) -> Some op1
        | OpBitFieldSExtract(op0, op1, _base, offset, count) -> Some op1
        | OpBitFieldUExtract(op0, op1, _base, offset, count) -> Some op1
        | OpBitReverse(op0, op1, _base) -> Some op1
        | OpBitCount(op0, op1, _base) -> Some op1
        | OpDPdx(op0, op1, p) -> Some op1
        | OpDPdy(op0, op1, p) -> Some op1
        | OpFwidth(op0, op1, p) -> Some op1
        | OpDPdxFine(op0, op1, p) -> Some op1
        | OpDPdyFine(op0, op1, p) -> Some op1
        | OpFwidthFine(op0, op1, p) -> Some op1
        | OpDPdxCoarse(op0, op1, p) -> Some op1
        | OpDPdyCoarse(op0, op1, p) -> Some op1
        | OpFwidthCoarse(op0, op1, p) -> Some op1
        | OpAtomicLoad(op0, op1, pointer, scope, semantics) -> Some op1
        | OpAtomicExchange(op0, op1, pointer, scope, semantics, value) -> Some op1
        | OpAtomicCompareExchange(op0, op1, pointer, scope, equal, unequal, value, comparator) -> Some op1
        | OpAtomicCompareExchangeWeak(op0, op1, pointer, scope, equal, unequal, value, comparator) -> Some op1
        | OpAtomicIIncrement(op0, op1, pointer, scope, semantics) -> Some op1
        | OpAtomicIDecrement(op0, op1, pointer, scope, semantics) -> Some op1
        | OpAtomicIAdd(op0, op1, pointer, scope, semantics, value) -> Some op1
        | OpAtomicISub(op0, op1, pointer, scope, semantics, value) -> Some op1
        | OpAtomicSMin(op0, op1, pointer, scope, semantics, value) -> Some op1
        | OpAtomicUMin(op0, op1, pointer, scope, semantics, value) -> Some op1
        | OpAtomicSMax(op0, op1, pointer, scope, semantics, value) -> Some op1
        | OpAtomicUMax(op0, op1, pointer, scope, semantics, value) -> Some op1
        | OpAtomicAnd(op0, op1, pointer, scope, semantics, value) -> Some op1
        | OpAtomicOr(op0, op1, pointer, scope, semantics, value) -> Some op1
        | OpAtomicXor(op0, op1, pointer, scope, semantics, value) -> Some op1
        | OpPhi(op0, op1, variableParent) -> Some op1
        | OpLabel(op0) -> Some op0
        | OpGroupAsyncCopy(op0, op1, execution, destination, source, numElements, stride, event) -> Some op1
        | OpGroupAll(op0, op1, execution, predicate) -> Some op1
        | OpGroupAny(op0, op1, execution, predicate) -> Some op1
        | OpGroupBroadcast(op0, op1, execution, value, localId) -> Some op1
        | OpGroupIAdd(op0, op1, execution, operation, x) -> Some op1
        | OpGroupFAdd(op0, op1, execution, operation, x) -> Some op1
        | OpGroupFMin(op0, op1, execution, operation, x) -> Some op1
        | OpGroupUMin(op0, op1, execution, operation, x) -> Some op1
        | OpGroupSMin(op0, op1, execution, operation, x) -> Some op1
        | OpGroupFMax(op0, op1, execution, operation, x) -> Some op1
        | OpGroupUMax(op0, op1, execution, operation, x) -> Some op1
        | OpGroupSMax(op0, op1, execution, operation, x) -> Some op1
        | OpReadPipe(op0, op1, pipe, pointer, packetSize, packetAlignment) -> Some op1
        | OpWritePipe(op0, op1, pipe, pointer, packetSize, packetAlignment) -> Some op1
        | OpReservedReadPipe(op0, op1, pipe, reserveId, index, pointer, packetSize, packetAlignment) -> Some op1
        | OpReservedWritePipe(op0, op1, pipe, reserveId, index, pointer, packetSize, packetAlignment) -> Some op1
        | OpReserveReadPipePackets(op0, op1, pipe, numPackets, packetSize, packetAlignment) -> Some op1
        | OpReserveWritePipePackets(op0, op1, pipe, numPackets, packetSize, packetAlignment) -> Some op1
        | OpIsValidReserveId(op0, op1, reserveId) -> Some op1
        | OpGetNumPipePackets(op0, op1, pipe, packetSize, packetAlignment) -> Some op1
        | OpGetMaxPipePackets(op0, op1, pipe, packetSize, packetAlignment) -> Some op1
        | OpGroupReserveReadPipePackets(op0, op1, execution, pipe, numPackets, packetSize, packetAlignment) -> Some op1
        | OpGroupReserveWritePipePackets(op0, op1, execution, pipe, numPackets, packetSize, packetAlignment) -> Some op1
        | OpEnqueueMarker(op0, op1, queue, numEvents, waitEvents, retEvent) -> Some op1
        | OpEnqueueKernel(op0, op1, queue, flags, nDRange, numEvents, waitEvents, retEvent, invoke, param, paramSize, paramAlign, localSize) -> Some op1
        | OpGetKernelNDrangeSubGroupCount(op0, op1, nDRange, invoke, param, paramSize, paramAlign) -> Some op1
        | OpGetKernelNDrangeMaxSubGroupSize(op0, op1, nDRange, invoke, param, paramSize, paramAlign) -> Some op1
        | OpGetKernelWorkGroupSize(op0, op1, invoke, param, paramSize, paramAlign) -> Some op1
        | OpGetKernelPreferredWorkGroupSizeMultiple(op0, op1, invoke, param, paramSize, paramAlign) -> Some op1
        | OpCreateUserEvent(op0, op1) -> Some op1
        | OpIsValidEvent(op0, op1, event) -> Some op1
        | OpGetDefaultQueue(op0, op1) -> Some op1
        | OpBuildNDRange(op0, op1, globalWorkSize, localWorkSize, globalWorkOffset) -> Some op1
        | OpImageSparseSampleImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op1
        | OpImageSparseSampleExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op1
        | OpImageSparseSampleDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op1
        | OpImageSparseSampleDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op1
        | OpImageSparseSampleProjImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op1
        | OpImageSparseSampleProjExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> Some op1
        | OpImageSparseSampleProjDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op1
        | OpImageSparseSampleProjDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op1
        | OpImageSparseFetch(op0, op1, image, coordinate, op4, parameters) -> Some op1
        | OpImageSparseGather(op0, op1, sampledImage, coordinate, _component, op5, parameters) -> Some op1
        | OpImageSparseDrefGather(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> Some op1
        | OpImageSparseTexelsResident(op0, op1, residentCode) -> Some op1
        | OpAtomicFlagTestAndSet(op0, op1, pointer, scope, semantics) -> Some op1
        | OpImageSparseRead(op0, op1, image, coordinate, op4, parameters) -> Some op1
        | OpSizeOf(op0, op1, pointer) -> Some op1
        | OpTypePipeStorage(op0) -> Some op0
        | OpConstantPipeStorage(op0, op1, packetSize, packetAlignment, capacity) -> Some op1
        | OpCreatePipeFromPipeStorage(op0, op1, pipeStorage) -> Some op1
        | OpGetKernelLocalSizeForSubgroupCount(op0, op1, subgroupCount, invoke, param, paramSize, paramAlign) -> Some op1
        | OpGetKernelMaxNumSubgroups(op0, op1, invoke, param, paramSize, paramAlign) -> Some op1
        | OpTypeNamedBarrier(op0) -> Some op0
        | OpNamedBarrierInitialize(op0, op1, subgroupCount) -> Some op1
        | OpSubgroupBallotKHR(op0, op1, predicate) -> Some op1
        | OpSubgroupFirstInvocationKHR(op0, op1, value) -> Some op1
        | OpSubgroupAllKHR(op0, op1, predicate) -> Some op1
        | OpSubgroupAnyKHR(op0, op1, predicate) -> Some op1
        | OpSubgroupAllEqualKHR(op0, op1, predicate) -> Some op1
        | OpSubgroupReadInvocationKHR(op0, op1, value, index) -> Some op1
        | OpGroupIAddNonUniformAMD(op0, op1, execution, operation, x) -> Some op1
        | OpGroupFAddNonUniformAMD(op0, op1, execution, operation, x) -> Some op1
        | OpGroupFMinNonUniformAMD(op0, op1, execution, operation, x) -> Some op1
        | OpGroupUMinNonUniformAMD(op0, op1, execution, operation, x) -> Some op1
        | OpGroupSMinNonUniformAMD(op0, op1, execution, operation, x) -> Some op1
        | OpGroupFMaxNonUniformAMD(op0, op1, execution, operation, x) -> Some op1
        | OpGroupUMaxNonUniformAMD(op0, op1, execution, operation, x) -> Some op1
        | OpGroupSMaxNonUniformAMD(op0, op1, execution, operation, x) -> Some op1
        | _ -> None

    let name (i : Instruction) = 
        match i with
        | OpNop -> "OpNop"
        | OpUndef _ -> "OpUndef"
        | OpSourceContinued _ -> "OpSourceContinued"
        | OpSource _ -> "OpSource"
        | OpSourceExtension _ -> "OpSourceExtension"
        | OpName _ -> "OpName"
        | OpMemberName _ -> "OpMemberName"
        | OpString _ -> "OpString"
        | OpLine _ -> "OpLine"
        | OpExtension _ -> "OpExtension"
        | OpExtInstImport _ -> "OpExtInstImport"
        | OpExtInst _ -> "OpExtInst"
        | OpMemoryModel _ -> "OpMemoryModel"
        | OpEntryPoint _ -> "OpEntryPoint"
        | OpExecutionMode _ -> "OpExecutionMode"
        | OpCapability _ -> "OpCapability"
        | OpTypeVoid _ -> "OpTypeVoid"
        | OpTypeBool _ -> "OpTypeBool"
        | OpTypeInt _ -> "OpTypeInt"
        | OpTypeFloat _ -> "OpTypeFloat"
        | OpTypeVector _ -> "OpTypeVector"
        | OpTypeMatrix _ -> "OpTypeMatrix"
        | OpTypeImage _ -> "OpTypeImage"
        | OpTypeSampler _ -> "OpTypeSampler"
        | OpTypeSampledImage _ -> "OpTypeSampledImage"
        | OpTypeArray _ -> "OpTypeArray"
        | OpTypeRuntimeArray _ -> "OpTypeRuntimeArray"
        | OpTypeStruct _ -> "OpTypeStruct"
        | OpTypeOpaque _ -> "OpTypeOpaque"
        | OpTypePointer _ -> "OpTypePointer"
        | OpTypeFunction _ -> "OpTypeFunction"
        | OpTypeEvent _ -> "OpTypeEvent"
        | OpTypeDeviceEvent _ -> "OpTypeDeviceEvent"
        | OpTypeReserveId _ -> "OpTypeReserveId"
        | OpTypeQueue _ -> "OpTypeQueue"
        | OpTypePipe _ -> "OpTypePipe"
        | OpTypeForwardPointer _ -> "OpTypeForwardPointer"
        | OpConstantTrue _ -> "OpConstantTrue"
        | OpConstantFalse _ -> "OpConstantFalse"
        | OpConstant _ -> "OpConstant"
        | OpConstantComposite _ -> "OpConstantComposite"
        | OpConstantSampler _ -> "OpConstantSampler"
        | OpConstantNull _ -> "OpConstantNull"
        | OpSpecConstantTrue _ -> "OpSpecConstantTrue"
        | OpSpecConstantFalse _ -> "OpSpecConstantFalse"
        | OpSpecConstant _ -> "OpSpecConstant"
        | OpSpecConstantComposite _ -> "OpSpecConstantComposite"
        | OpSpecConstantOp _ -> "OpSpecConstantOp"
        | OpFunction _ -> "OpFunction"
        | OpFunctionParameter _ -> "OpFunctionParameter"
        | OpFunctionEnd -> "OpFunctionEnd"
        | OpFunctionCall _ -> "OpFunctionCall"
        | OpVariable _ -> "OpVariable"
        | OpImageTexelPointer _ -> "OpImageTexelPointer"
        | OpLoad _ -> "OpLoad"
        | OpStore _ -> "OpStore"
        | OpCopyMemory _ -> "OpCopyMemory"
        | OpCopyMemorySized _ -> "OpCopyMemorySized"
        | OpAccessChain _ -> "OpAccessChain"
        | OpInBoundsAccessChain _ -> "OpInBoundsAccessChain"
        | OpPtrAccessChain _ -> "OpPtrAccessChain"
        | OpArrayLength _ -> "OpArrayLength"
        | OpGenericPtrMemSemantics _ -> "OpGenericPtrMemSemantics"
        | OpInBoundsPtrAccessChain _ -> "OpInBoundsPtrAccessChain"
        | OpDecorate _ -> "OpDecorate"
        | OpMemberDecorate _ -> "OpMemberDecorate"
        | OpDecorationGroup _ -> "OpDecorationGroup"
        | OpGroupDecorate _ -> "OpGroupDecorate"
        | OpGroupMemberDecorate _ -> "OpGroupMemberDecorate"
        | OpVectorExtractDynamic _ -> "OpVectorExtractDynamic"
        | OpVectorInsertDynamic _ -> "OpVectorInsertDynamic"
        | OpVectorShuffle _ -> "OpVectorShuffle"
        | OpCompositeConstruct _ -> "OpCompositeConstruct"
        | OpCompositeExtract _ -> "OpCompositeExtract"
        | OpCompositeInsert _ -> "OpCompositeInsert"
        | OpCopyObject _ -> "OpCopyObject"
        | OpTranspose _ -> "OpTranspose"
        | OpSampledImage _ -> "OpSampledImage"
        | OpImageSampleImplicitLod _ -> "OpImageSampleImplicitLod"
        | OpImageSampleExplicitLod _ -> "OpImageSampleExplicitLod"
        | OpImageSampleDrefImplicitLod _ -> "OpImageSampleDrefImplicitLod"
        | OpImageSampleDrefExplicitLod _ -> "OpImageSampleDrefExplicitLod"
        | OpImageSampleProjImplicitLod _ -> "OpImageSampleProjImplicitLod"
        | OpImageSampleProjExplicitLod _ -> "OpImageSampleProjExplicitLod"
        | OpImageSampleProjDrefImplicitLod _ -> "OpImageSampleProjDrefImplicitLod"
        | OpImageSampleProjDrefExplicitLod _ -> "OpImageSampleProjDrefExplicitLod"
        | OpImageFetch _ -> "OpImageFetch"
        | OpImageGather _ -> "OpImageGather"
        | OpImageDrefGather _ -> "OpImageDrefGather"
        | OpImageRead _ -> "OpImageRead"
        | OpImageWrite _ -> "OpImageWrite"
        | OpImage _ -> "OpImage"
        | OpImageQueryFormat _ -> "OpImageQueryFormat"
        | OpImageQueryOrder _ -> "OpImageQueryOrder"
        | OpImageQuerySizeLod _ -> "OpImageQuerySizeLod"
        | OpImageQuerySize _ -> "OpImageQuerySize"
        | OpImageQueryLod _ -> "OpImageQueryLod"
        | OpImageQueryLevels _ -> "OpImageQueryLevels"
        | OpImageQuerySamples _ -> "OpImageQuerySamples"
        | OpConvertFToU _ -> "OpConvertFToU"
        | OpConvertFToS _ -> "OpConvertFToS"
        | OpConvertSToF _ -> "OpConvertSToF"
        | OpConvertUToF _ -> "OpConvertUToF"
        | OpUConvert _ -> "OpUConvert"
        | OpSConvert _ -> "OpSConvert"
        | OpFConvert _ -> "OpFConvert"
        | OpQuantizeToF16 _ -> "OpQuantizeToF16"
        | OpConvertPtrToU _ -> "OpConvertPtrToU"
        | OpSatConvertSToU _ -> "OpSatConvertSToU"
        | OpSatConvertUToS _ -> "OpSatConvertUToS"
        | OpConvertUToPtr _ -> "OpConvertUToPtr"
        | OpPtrCastToGeneric _ -> "OpPtrCastToGeneric"
        | OpGenericCastToPtr _ -> "OpGenericCastToPtr"
        | OpGenericCastToPtrExplicit _ -> "OpGenericCastToPtrExplicit"
        | OpBitcast _ -> "OpBitcast"
        | OpSNegate _ -> "OpSNegate"
        | OpFNegate _ -> "OpFNegate"
        | OpIAdd _ -> "OpIAdd"
        | OpFAdd _ -> "OpFAdd"
        | OpISub _ -> "OpISub"
        | OpFSub _ -> "OpFSub"
        | OpIMul _ -> "OpIMul"
        | OpFMul _ -> "OpFMul"
        | OpUDiv _ -> "OpUDiv"
        | OpSDiv _ -> "OpSDiv"
        | OpFDiv _ -> "OpFDiv"
        | OpUMod _ -> "OpUMod"
        | OpSRem _ -> "OpSRem"
        | OpSMod _ -> "OpSMod"
        | OpFRem _ -> "OpFRem"
        | OpFMod _ -> "OpFMod"
        | OpVectorTimesScalar _ -> "OpVectorTimesScalar"
        | OpMatrixTimesScalar _ -> "OpMatrixTimesScalar"
        | OpVectorTimesMatrix _ -> "OpVectorTimesMatrix"
        | OpMatrixTimesVector _ -> "OpMatrixTimesVector"
        | OpMatrixTimesMatrix _ -> "OpMatrixTimesMatrix"
        | OpOuterProduct _ -> "OpOuterProduct"
        | OpDot _ -> "OpDot"
        | OpIAddCarry _ -> "OpIAddCarry"
        | OpISubBorrow _ -> "OpISubBorrow"
        | OpUMulExtended _ -> "OpUMulExtended"
        | OpSMulExtended _ -> "OpSMulExtended"
        | OpAny _ -> "OpAny"
        | OpAll _ -> "OpAll"
        | OpIsNan _ -> "OpIsNan"
        | OpIsInf _ -> "OpIsInf"
        | OpIsFinite _ -> "OpIsFinite"
        | OpIsNormal _ -> "OpIsNormal"
        | OpSignBitSet _ -> "OpSignBitSet"
        | OpLessOrGreater _ -> "OpLessOrGreater"
        | OpOrdered _ -> "OpOrdered"
        | OpUnordered _ -> "OpUnordered"
        | OpLogicalEqual _ -> "OpLogicalEqual"
        | OpLogicalNotEqual _ -> "OpLogicalNotEqual"
        | OpLogicalOr _ -> "OpLogicalOr"
        | OpLogicalAnd _ -> "OpLogicalAnd"
        | OpLogicalNot _ -> "OpLogicalNot"
        | OpSelect _ -> "OpSelect"
        | OpIEqual _ -> "OpIEqual"
        | OpINotEqual _ -> "OpINotEqual"
        | OpUGreaterThan _ -> "OpUGreaterThan"
        | OpSGreaterThan _ -> "OpSGreaterThan"
        | OpUGreaterThanEqual _ -> "OpUGreaterThanEqual"
        | OpSGreaterThanEqual _ -> "OpSGreaterThanEqual"
        | OpULessThan _ -> "OpULessThan"
        | OpSLessThan _ -> "OpSLessThan"
        | OpULessThanEqual _ -> "OpULessThanEqual"
        | OpSLessThanEqual _ -> "OpSLessThanEqual"
        | OpFOrdEqual _ -> "OpFOrdEqual"
        | OpFUnordEqual _ -> "OpFUnordEqual"
        | OpFOrdNotEqual _ -> "OpFOrdNotEqual"
        | OpFUnordNotEqual _ -> "OpFUnordNotEqual"
        | OpFOrdLessThan _ -> "OpFOrdLessThan"
        | OpFUnordLessThan _ -> "OpFUnordLessThan"
        | OpFOrdGreaterThan _ -> "OpFOrdGreaterThan"
        | OpFUnordGreaterThan _ -> "OpFUnordGreaterThan"
        | OpFOrdLessThanEqual _ -> "OpFOrdLessThanEqual"
        | OpFUnordLessThanEqual _ -> "OpFUnordLessThanEqual"
        | OpFOrdGreaterThanEqual _ -> "OpFOrdGreaterThanEqual"
        | OpFUnordGreaterThanEqual _ -> "OpFUnordGreaterThanEqual"
        | OpShiftRightLogical _ -> "OpShiftRightLogical"
        | OpShiftRightArithmetic _ -> "OpShiftRightArithmetic"
        | OpShiftLeftLogical _ -> "OpShiftLeftLogical"
        | OpBitwiseOr _ -> "OpBitwiseOr"
        | OpBitwiseXor _ -> "OpBitwiseXor"
        | OpBitwiseAnd _ -> "OpBitwiseAnd"
        | OpNot _ -> "OpNot"
        | OpBitFieldInsert _ -> "OpBitFieldInsert"
        | OpBitFieldSExtract _ -> "OpBitFieldSExtract"
        | OpBitFieldUExtract _ -> "OpBitFieldUExtract"
        | OpBitReverse _ -> "OpBitReverse"
        | OpBitCount _ -> "OpBitCount"
        | OpDPdx _ -> "OpDPdx"
        | OpDPdy _ -> "OpDPdy"
        | OpFwidth _ -> "OpFwidth"
        | OpDPdxFine _ -> "OpDPdxFine"
        | OpDPdyFine _ -> "OpDPdyFine"
        | OpFwidthFine _ -> "OpFwidthFine"
        | OpDPdxCoarse _ -> "OpDPdxCoarse"
        | OpDPdyCoarse _ -> "OpDPdyCoarse"
        | OpFwidthCoarse _ -> "OpFwidthCoarse"
        | OpEmitVertex -> "OpEmitVertex"
        | OpEndPrimitive -> "OpEndPrimitive"
        | OpEmitStreamVertex _ -> "OpEmitStreamVertex"
        | OpEndStreamPrimitive _ -> "OpEndStreamPrimitive"
        | OpControlBarrier _ -> "OpControlBarrier"
        | OpMemoryBarrier _ -> "OpMemoryBarrier"
        | OpAtomicLoad _ -> "OpAtomicLoad"
        | OpAtomicStore _ -> "OpAtomicStore"
        | OpAtomicExchange _ -> "OpAtomicExchange"
        | OpAtomicCompareExchange _ -> "OpAtomicCompareExchange"
        | OpAtomicCompareExchangeWeak _ -> "OpAtomicCompareExchangeWeak"
        | OpAtomicIIncrement _ -> "OpAtomicIIncrement"
        | OpAtomicIDecrement _ -> "OpAtomicIDecrement"
        | OpAtomicIAdd _ -> "OpAtomicIAdd"
        | OpAtomicISub _ -> "OpAtomicISub"
        | OpAtomicSMin _ -> "OpAtomicSMin"
        | OpAtomicUMin _ -> "OpAtomicUMin"
        | OpAtomicSMax _ -> "OpAtomicSMax"
        | OpAtomicUMax _ -> "OpAtomicUMax"
        | OpAtomicAnd _ -> "OpAtomicAnd"
        | OpAtomicOr _ -> "OpAtomicOr"
        | OpAtomicXor _ -> "OpAtomicXor"
        | OpPhi _ -> "OpPhi"
        | OpLoopMerge _ -> "OpLoopMerge"
        | OpSelectionMerge _ -> "OpSelectionMerge"
        | OpLabel _ -> "OpLabel"
        | OpBranch _ -> "OpBranch"
        | OpBranchConditional _ -> "OpBranchConditional"
        | OpSwitch _ -> "OpSwitch"
        | OpKill -> "OpKill"
        | OpReturn -> "OpReturn"
        | OpReturnValue _ -> "OpReturnValue"
        | OpUnreachable -> "OpUnreachable"
        | OpLifetimeStart _ -> "OpLifetimeStart"
        | OpLifetimeStop _ -> "OpLifetimeStop"
        | OpGroupAsyncCopy _ -> "OpGroupAsyncCopy"
        | OpGroupWaitEvents _ -> "OpGroupWaitEvents"
        | OpGroupAll _ -> "OpGroupAll"
        | OpGroupAny _ -> "OpGroupAny"
        | OpGroupBroadcast _ -> "OpGroupBroadcast"
        | OpGroupIAdd _ -> "OpGroupIAdd"
        | OpGroupFAdd _ -> "OpGroupFAdd"
        | OpGroupFMin _ -> "OpGroupFMin"
        | OpGroupUMin _ -> "OpGroupUMin"
        | OpGroupSMin _ -> "OpGroupSMin"
        | OpGroupFMax _ -> "OpGroupFMax"
        | OpGroupUMax _ -> "OpGroupUMax"
        | OpGroupSMax _ -> "OpGroupSMax"
        | OpReadPipe _ -> "OpReadPipe"
        | OpWritePipe _ -> "OpWritePipe"
        | OpReservedReadPipe _ -> "OpReservedReadPipe"
        | OpReservedWritePipe _ -> "OpReservedWritePipe"
        | OpReserveReadPipePackets _ -> "OpReserveReadPipePackets"
        | OpReserveWritePipePackets _ -> "OpReserveWritePipePackets"
        | OpCommitReadPipe _ -> "OpCommitReadPipe"
        | OpCommitWritePipe _ -> "OpCommitWritePipe"
        | OpIsValidReserveId _ -> "OpIsValidReserveId"
        | OpGetNumPipePackets _ -> "OpGetNumPipePackets"
        | OpGetMaxPipePackets _ -> "OpGetMaxPipePackets"
        | OpGroupReserveReadPipePackets _ -> "OpGroupReserveReadPipePackets"
        | OpGroupReserveWritePipePackets _ -> "OpGroupReserveWritePipePackets"
        | OpGroupCommitReadPipe _ -> "OpGroupCommitReadPipe"
        | OpGroupCommitWritePipe _ -> "OpGroupCommitWritePipe"
        | OpEnqueueMarker _ -> "OpEnqueueMarker"
        | OpEnqueueKernel _ -> "OpEnqueueKernel"
        | OpGetKernelNDrangeSubGroupCount _ -> "OpGetKernelNDrangeSubGroupCount"
        | OpGetKernelNDrangeMaxSubGroupSize _ -> "OpGetKernelNDrangeMaxSubGroupSize"
        | OpGetKernelWorkGroupSize _ -> "OpGetKernelWorkGroupSize"
        | OpGetKernelPreferredWorkGroupSizeMultiple _ -> "OpGetKernelPreferredWorkGroupSizeMultiple"
        | OpRetainEvent _ -> "OpRetainEvent"
        | OpReleaseEvent _ -> "OpReleaseEvent"
        | OpCreateUserEvent _ -> "OpCreateUserEvent"
        | OpIsValidEvent _ -> "OpIsValidEvent"
        | OpSetUserEventStatus _ -> "OpSetUserEventStatus"
        | OpCaptureEventProfilingInfo _ -> "OpCaptureEventProfilingInfo"
        | OpGetDefaultQueue _ -> "OpGetDefaultQueue"
        | OpBuildNDRange _ -> "OpBuildNDRange"
        | OpImageSparseSampleImplicitLod _ -> "OpImageSparseSampleImplicitLod"
        | OpImageSparseSampleExplicitLod _ -> "OpImageSparseSampleExplicitLod"
        | OpImageSparseSampleDrefImplicitLod _ -> "OpImageSparseSampleDrefImplicitLod"
        | OpImageSparseSampleDrefExplicitLod _ -> "OpImageSparseSampleDrefExplicitLod"
        | OpImageSparseSampleProjImplicitLod _ -> "OpImageSparseSampleProjImplicitLod"
        | OpImageSparseSampleProjExplicitLod _ -> "OpImageSparseSampleProjExplicitLod"
        | OpImageSparseSampleProjDrefImplicitLod _ -> "OpImageSparseSampleProjDrefImplicitLod"
        | OpImageSparseSampleProjDrefExplicitLod _ -> "OpImageSparseSampleProjDrefExplicitLod"
        | OpImageSparseFetch _ -> "OpImageSparseFetch"
        | OpImageSparseGather _ -> "OpImageSparseGather"
        | OpImageSparseDrefGather _ -> "OpImageSparseDrefGather"
        | OpImageSparseTexelsResident _ -> "OpImageSparseTexelsResident"
        | OpNoLine -> "OpNoLine"
        | OpAtomicFlagTestAndSet _ -> "OpAtomicFlagTestAndSet"
        | OpAtomicFlagClear _ -> "OpAtomicFlagClear"
        | OpImageSparseRead _ -> "OpImageSparseRead"
        | OpSizeOf _ -> "OpSizeOf"
        | OpTypePipeStorage _ -> "OpTypePipeStorage"
        | OpConstantPipeStorage _ -> "OpConstantPipeStorage"
        | OpCreatePipeFromPipeStorage _ -> "OpCreatePipeFromPipeStorage"
        | OpGetKernelLocalSizeForSubgroupCount _ -> "OpGetKernelLocalSizeForSubgroupCount"
        | OpGetKernelMaxNumSubgroups _ -> "OpGetKernelMaxNumSubgroups"
        | OpTypeNamedBarrier _ -> "OpTypeNamedBarrier"
        | OpNamedBarrierInitialize _ -> "OpNamedBarrierInitialize"
        | OpMemoryNamedBarrier _ -> "OpMemoryNamedBarrier"
        | OpModuleProcessed _ -> "OpModuleProcessed"
        | OpExecutionModeId _ -> "OpExecutionModeId"
        | OpDecorateId _ -> "OpDecorateId"
        | OpSubgroupBallotKHR _ -> "OpSubgroupBallotKHR"
        | OpSubgroupFirstInvocationKHR _ -> "OpSubgroupFirstInvocationKHR"
        | OpSubgroupAllKHR _ -> "OpSubgroupAllKHR"
        | OpSubgroupAnyKHR _ -> "OpSubgroupAnyKHR"
        | OpSubgroupAllEqualKHR _ -> "OpSubgroupAllEqualKHR"
        | OpSubgroupReadInvocationKHR _ -> "OpSubgroupReadInvocationKHR"
        | OpGroupIAddNonUniformAMD _ -> "OpGroupIAddNonUniformAMD"
        | OpGroupFAddNonUniformAMD _ -> "OpGroupFAddNonUniformAMD"
        | OpGroupFMinNonUniformAMD _ -> "OpGroupFMinNonUniformAMD"
        | OpGroupUMinNonUniformAMD _ -> "OpGroupUMinNonUniformAMD"
        | OpGroupSMinNonUniformAMD _ -> "OpGroupSMinNonUniformAMD"
        | OpGroupFMaxNonUniformAMD _ -> "OpGroupFMaxNonUniformAMD"
        | OpGroupUMaxNonUniformAMD _ -> "OpGroupUMaxNonUniformAMD"
        | OpGroupSMaxNonUniformAMD _ -> "OpGroupSMaxNonUniformAMD"

    let operands (i : Instruction) = 
        match i with
        | OpNop -> [|  |]
        | OpUndef(op0, op1) -> [|  |]
        | OpSourceContinued(continuedSource) -> [| continuedSource :> obj |]
        | OpSource(op0, version, file, source) -> [| op0 :> obj; version :> obj; file :> obj; source :> obj |]
        | OpSourceExtension(extension) -> [| extension :> obj |]
        | OpName(target, name) -> [| target :> obj; name :> obj |]
        | OpMemberName(_type, _member, name) -> [| _type :> obj; _member :> obj; name :> obj |]
        | OpString(op0, string) -> [| op0 :> obj; string :> obj |]
        | OpLine(file, line, column) -> [| file :> obj; line :> obj; column :> obj |]
        | OpExtension(name) -> [| name :> obj |]
        | OpExtInstImport(op0, name) -> [| op0 :> obj; name :> obj |]
        | OpExtInst(op0, op1, set, instruction, operandOperand) -> [| set :> obj; instruction :> obj; operandOperand :> obj |]
        | OpMemoryModel(op0, op1) -> [| op0 :> obj; op1 :> obj |]
        | OpEntryPoint(op0, entryPoint, name, _interface) -> [| op0 :> obj; entryPoint :> obj; name :> obj; _interface :> obj |]
        | OpExecutionMode(entryPoint, mode) -> [| entryPoint :> obj; mode :> obj |]
        | OpCapability(capability) -> [| capability :> obj |]
        | OpTypeVoid(op0) -> [| op0 :> obj |]
        | OpTypeBool(op0) -> [| op0 :> obj |]
        | OpTypeInt(op0, width, signedness) -> [| op0 :> obj; width :> obj; signedness :> obj |]
        | OpTypeFloat(op0, width) -> [| op0 :> obj; width :> obj |]
        | OpTypeVector(op0, componentType, componentCount) -> [| op0 :> obj; componentType :> obj; componentCount :> obj |]
        | OpTypeMatrix(op0, columnType, columnCount) -> [| op0 :> obj; columnType :> obj; columnCount :> obj |]
        | OpTypeImage(op0, sampledType, op2, depth, arrayed, mS, sampled, op7, op8) -> [| op0 :> obj; sampledType :> obj; op2 :> obj; depth :> obj; arrayed :> obj; mS :> obj; sampled :> obj; op7 :> obj; op8 :> obj |]
        | OpTypeSampler(op0) -> [| op0 :> obj |]
        | OpTypeSampledImage(op0, imageType) -> [| op0 :> obj; imageType :> obj |]
        | OpTypeArray(op0, elementType, length) -> [| op0 :> obj; elementType :> obj; length :> obj |]
        | OpTypeRuntimeArray(op0, elementType) -> [| op0 :> obj; elementType :> obj |]
        | OpTypeStruct(op0, membertypemembertype) -> [| op0 :> obj; membertypemembertype :> obj |]
        | OpTypeOpaque(op0, thenameoftheopaquetype) -> [| op0 :> obj; thenameoftheopaquetype :> obj |]
        | OpTypePointer(op0, op1, _type) -> [| op0 :> obj; op1 :> obj; _type :> obj |]
        | OpTypeFunction(op0, returnType, parameterTypeParameterType) -> [| op0 :> obj; returnType :> obj; parameterTypeParameterType :> obj |]
        | OpTypeEvent(op0) -> [| op0 :> obj |]
        | OpTypeDeviceEvent(op0) -> [| op0 :> obj |]
        | OpTypeReserveId(op0) -> [| op0 :> obj |]
        | OpTypeQueue(op0) -> [| op0 :> obj |]
        | OpTypePipe(op0, qualifier) -> [| op0 :> obj; qualifier :> obj |]
        | OpTypeForwardPointer(pointerType, op1) -> [| pointerType :> obj; op1 :> obj |]
        | OpConstantTrue(op0, op1) -> [|  |]
        | OpConstantFalse(op0, op1) -> [|  |]
        | OpConstant(op0, op1, value) -> [| value :> obj |]
        | OpConstantComposite(op0, op1, constituents) -> [| constituents :> obj |]
        | OpConstantSampler(op0, op1, op2, param, op4) -> [| op2 :> obj; param :> obj; op4 :> obj |]
        | OpConstantNull(op0, op1) -> [|  |]
        | OpSpecConstantTrue(op0, op1) -> [|  |]
        | OpSpecConstantFalse(op0, op1) -> [|  |]
        | OpSpecConstant(op0, op1, value) -> [| value :> obj |]
        | OpSpecConstantComposite(op0, op1, constituents) -> [| constituents :> obj |]
        | OpSpecConstantOp(op0, op1, opcode) -> [| opcode :> obj |]
        | OpFunction(op0, op1, op2, functionType) -> [| op2 :> obj; functionType :> obj |]
        | OpFunctionParameter(op0, op1) -> [|  |]
        | OpFunctionEnd -> [|  |]
        | OpFunctionCall(op0, op1, _function, argumentArgument) -> [| _function :> obj; argumentArgument :> obj |]
        | OpVariable(op0, op1, op2, initializer) -> [| op2 :> obj; initializer :> obj |]
        | OpImageTexelPointer(op0, op1, image, coordinate, sample) -> [| image :> obj; coordinate :> obj; sample :> obj |]
        | OpLoad(op0, op1, pointer, op3) -> [| pointer :> obj; op3 :> obj |]
        | OpStore(pointer, _object, op2) -> [| pointer :> obj; _object :> obj; op2 :> obj |]
        | OpCopyMemory(target, source, op2) -> [| target :> obj; source :> obj; op2 :> obj |]
        | OpCopyMemorySized(target, source, size, op3) -> [| target :> obj; source :> obj; size :> obj; op3 :> obj |]
        | OpAccessChain(op0, op1, _base, indexes) -> [| _base :> obj; indexes :> obj |]
        | OpInBoundsAccessChain(op0, op1, _base, indexes) -> [| _base :> obj; indexes :> obj |]
        | OpPtrAccessChain(op0, op1, _base, element, indexes) -> [| _base :> obj; element :> obj; indexes :> obj |]
        | OpArrayLength(op0, op1, structure, arraymember) -> [| structure :> obj; arraymember :> obj |]
        | OpGenericPtrMemSemantics(op0, op1, pointer) -> [| pointer :> obj |]
        | OpInBoundsPtrAccessChain(op0, op1, _base, element, indexes) -> [| _base :> obj; element :> obj; indexes :> obj |]
        | OpDecorate(target, op1, parameters) -> [| target :> obj; op1 :> obj; parameters :> obj |]
        | OpMemberDecorate(structureType, _member, decoration, values) -> [| structureType :> obj; _member :> obj; decoration :> obj; values :> obj |]
        | OpDecorationGroup(op0) -> [| op0 :> obj |]
        | OpGroupDecorate(decorationGroup, targets) -> [| decorationGroup :> obj; targets :> obj |]
        | OpGroupMemberDecorate(decorationGroup, targets) -> [| decorationGroup :> obj; targets :> obj |]
        | OpVectorExtractDynamic(op0, op1, vector, index) -> [| vector :> obj; index :> obj |]
        | OpVectorInsertDynamic(op0, op1, vector, _component, index) -> [| vector :> obj; _component :> obj; index :> obj |]
        | OpVectorShuffle(op0, op1, vector1, vector2, components) -> [| vector1 :> obj; vector2 :> obj; components :> obj |]
        | OpCompositeConstruct(op0, op1, constituents) -> [| constituents :> obj |]
        | OpCompositeExtract(op0, op1, composite, indexes) -> [| composite :> obj; indexes :> obj |]
        | OpCompositeInsert(op0, op1, _object, composite, indexes) -> [| _object :> obj; composite :> obj; indexes :> obj |]
        | OpCopyObject(op0, op1, operand) -> [| operand :> obj |]
        | OpTranspose(op0, op1, matrix) -> [| matrix :> obj |]
        | OpSampledImage(op0, op1, image, sampler) -> [| image :> obj; sampler :> obj |]
        | OpImageSampleImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> [| sampledImage :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageSampleExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> [| sampledImage :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageSampleDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; dref :> obj; op5 :> obj; parameters :> obj |]
        | OpImageSampleDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; dref :> obj; op5 :> obj; parameters :> obj |]
        | OpImageSampleProjImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> [| sampledImage :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageSampleProjExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> [| sampledImage :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageSampleProjDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; dref :> obj; op5 :> obj; parameters :> obj |]
        | OpImageSampleProjDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; dref :> obj; op5 :> obj; parameters :> obj |]
        | OpImageFetch(op0, op1, image, coordinate, op4, parameters) -> [| image :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageGather(op0, op1, sampledImage, coordinate, _component, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; _component :> obj; op5 :> obj; parameters :> obj |]
        | OpImageDrefGather(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; dref :> obj; op5 :> obj; parameters :> obj |]
        | OpImageRead(op0, op1, image, coordinate, op4, parameters) -> [| image :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageWrite(image, coordinate, texel, op3, parameters) -> [| image :> obj; coordinate :> obj; texel :> obj; op3 :> obj; parameters :> obj |]
        | OpImage(op0, op1, sampledImage) -> [| sampledImage :> obj |]
        | OpImageQueryFormat(op0, op1, image) -> [| image :> obj |]
        | OpImageQueryOrder(op0, op1, image) -> [| image :> obj |]
        | OpImageQuerySizeLod(op0, op1, image, levelofDetail) -> [| image :> obj; levelofDetail :> obj |]
        | OpImageQuerySize(op0, op1, image) -> [| image :> obj |]
        | OpImageQueryLod(op0, op1, sampledImage, coordinate) -> [| sampledImage :> obj; coordinate :> obj |]
        | OpImageQueryLevels(op0, op1, image) -> [| image :> obj |]
        | OpImageQuerySamples(op0, op1, image) -> [| image :> obj |]
        | OpConvertFToU(op0, op1, floatValue) -> [| floatValue :> obj |]
        | OpConvertFToS(op0, op1, floatValue) -> [| floatValue :> obj |]
        | OpConvertSToF(op0, op1, signedValue) -> [| signedValue :> obj |]
        | OpConvertUToF(op0, op1, unsignedValue) -> [| unsignedValue :> obj |]
        | OpUConvert(op0, op1, unsignedValue) -> [| unsignedValue :> obj |]
        | OpSConvert(op0, op1, signedValue) -> [| signedValue :> obj |]
        | OpFConvert(op0, op1, floatValue) -> [| floatValue :> obj |]
        | OpQuantizeToF16(op0, op1, value) -> [| value :> obj |]
        | OpConvertPtrToU(op0, op1, pointer) -> [| pointer :> obj |]
        | OpSatConvertSToU(op0, op1, signedValue) -> [| signedValue :> obj |]
        | OpSatConvertUToS(op0, op1, unsignedValue) -> [| unsignedValue :> obj |]
        | OpConvertUToPtr(op0, op1, integerValue) -> [| integerValue :> obj |]
        | OpPtrCastToGeneric(op0, op1, pointer) -> [| pointer :> obj |]
        | OpGenericCastToPtr(op0, op1, pointer) -> [| pointer :> obj |]
        | OpGenericCastToPtrExplicit(op0, op1, pointer, storage) -> [| pointer :> obj; storage :> obj |]
        | OpBitcast(op0, op1, operand) -> [| operand :> obj |]
        | OpSNegate(op0, op1, operand) -> [| operand :> obj |]
        | OpFNegate(op0, op1, operand) -> [| operand :> obj |]
        | OpIAdd(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFAdd(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpISub(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFSub(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpIMul(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFMul(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpUDiv(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpSDiv(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFDiv(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpUMod(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpSRem(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpSMod(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFRem(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFMod(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpVectorTimesScalar(op0, op1, vector, scalar) -> [| vector :> obj; scalar :> obj |]
        | OpMatrixTimesScalar(op0, op1, matrix, scalar) -> [| matrix :> obj; scalar :> obj |]
        | OpVectorTimesMatrix(op0, op1, vector, matrix) -> [| vector :> obj; matrix :> obj |]
        | OpMatrixTimesVector(op0, op1, matrix, vector) -> [| matrix :> obj; vector :> obj |]
        | OpMatrixTimesMatrix(op0, op1, leftMatrix, rightMatrix) -> [| leftMatrix :> obj; rightMatrix :> obj |]
        | OpOuterProduct(op0, op1, vector1, vector2) -> [| vector1 :> obj; vector2 :> obj |]
        | OpDot(op0, op1, vector1, vector2) -> [| vector1 :> obj; vector2 :> obj |]
        | OpIAddCarry(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpISubBorrow(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpUMulExtended(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpSMulExtended(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpAny(op0, op1, vector) -> [| vector :> obj |]
        | OpAll(op0, op1, vector) -> [| vector :> obj |]
        | OpIsNan(op0, op1, x) -> [| x :> obj |]
        | OpIsInf(op0, op1, x) -> [| x :> obj |]
        | OpIsFinite(op0, op1, x) -> [| x :> obj |]
        | OpIsNormal(op0, op1, x) -> [| x :> obj |]
        | OpSignBitSet(op0, op1, x) -> [| x :> obj |]
        | OpLessOrGreater(op0, op1, x, y) -> [| x :> obj; y :> obj |]
        | OpOrdered(op0, op1, x, y) -> [| x :> obj; y :> obj |]
        | OpUnordered(op0, op1, x, y) -> [| x :> obj; y :> obj |]
        | OpLogicalEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpLogicalNotEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpLogicalOr(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpLogicalAnd(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpLogicalNot(op0, op1, operand) -> [| operand :> obj |]
        | OpSelect(op0, op1, condition, _object1, _object2) -> [| condition :> obj; _object1 :> obj; _object2 :> obj |]
        | OpIEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpINotEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpUGreaterThan(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpSGreaterThan(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpUGreaterThanEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpSGreaterThanEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpULessThan(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpSLessThan(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpULessThanEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpSLessThanEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFOrdEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFUnordEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFOrdNotEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFUnordNotEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFOrdLessThan(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFUnordLessThan(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFOrdGreaterThan(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFUnordGreaterThan(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFOrdLessThanEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFUnordLessThanEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFOrdGreaterThanEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpFUnordGreaterThanEqual(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpShiftRightLogical(op0, op1, _base, shift) -> [| _base :> obj; shift :> obj |]
        | OpShiftRightArithmetic(op0, op1, _base, shift) -> [| _base :> obj; shift :> obj |]
        | OpShiftLeftLogical(op0, op1, _base, shift) -> [| _base :> obj; shift :> obj |]
        | OpBitwiseOr(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpBitwiseXor(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpBitwiseAnd(op0, op1, operand1, operand2) -> [| operand1 :> obj; operand2 :> obj |]
        | OpNot(op0, op1, operand) -> [| operand :> obj |]
        | OpBitFieldInsert(op0, op1, _base, insert, offset, count) -> [| _base :> obj; insert :> obj; offset :> obj; count :> obj |]
        | OpBitFieldSExtract(op0, op1, _base, offset, count) -> [| _base :> obj; offset :> obj; count :> obj |]
        | OpBitFieldUExtract(op0, op1, _base, offset, count) -> [| _base :> obj; offset :> obj; count :> obj |]
        | OpBitReverse(op0, op1, _base) -> [| _base :> obj |]
        | OpBitCount(op0, op1, _base) -> [| _base :> obj |]
        | OpDPdx(op0, op1, p) -> [| p :> obj |]
        | OpDPdy(op0, op1, p) -> [| p :> obj |]
        | OpFwidth(op0, op1, p) -> [| p :> obj |]
        | OpDPdxFine(op0, op1, p) -> [| p :> obj |]
        | OpDPdyFine(op0, op1, p) -> [| p :> obj |]
        | OpFwidthFine(op0, op1, p) -> [| p :> obj |]
        | OpDPdxCoarse(op0, op1, p) -> [| p :> obj |]
        | OpDPdyCoarse(op0, op1, p) -> [| p :> obj |]
        | OpFwidthCoarse(op0, op1, p) -> [| p :> obj |]
        | OpEmitVertex -> [|  |]
        | OpEndPrimitive -> [|  |]
        | OpEmitStreamVertex(stream) -> [| stream :> obj |]
        | OpEndStreamPrimitive(stream) -> [| stream :> obj |]
        | OpControlBarrier(execution, memory, semantics) -> [| execution :> obj; memory :> obj; semantics :> obj |]
        | OpMemoryBarrier(memory, semantics) -> [| memory :> obj; semantics :> obj |]
        | OpAtomicLoad(op0, op1, pointer, scope, semantics) -> [| pointer :> obj; scope :> obj; semantics :> obj |]
        | OpAtomicStore(pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpAtomicExchange(op0, op1, pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpAtomicCompareExchange(op0, op1, pointer, scope, equal, unequal, value, comparator) -> [| pointer :> obj; scope :> obj; equal :> obj; unequal :> obj; value :> obj; comparator :> obj |]
        | OpAtomicCompareExchangeWeak(op0, op1, pointer, scope, equal, unequal, value, comparator) -> [| pointer :> obj; scope :> obj; equal :> obj; unequal :> obj; value :> obj; comparator :> obj |]
        | OpAtomicIIncrement(op0, op1, pointer, scope, semantics) -> [| pointer :> obj; scope :> obj; semantics :> obj |]
        | OpAtomicIDecrement(op0, op1, pointer, scope, semantics) -> [| pointer :> obj; scope :> obj; semantics :> obj |]
        | OpAtomicIAdd(op0, op1, pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpAtomicISub(op0, op1, pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpAtomicSMin(op0, op1, pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpAtomicUMin(op0, op1, pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpAtomicSMax(op0, op1, pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpAtomicUMax(op0, op1, pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpAtomicAnd(op0, op1, pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpAtomicOr(op0, op1, pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpAtomicXor(op0, op1, pointer, scope, semantics, value) -> [| pointer :> obj; scope :> obj; semantics :> obj; value :> obj |]
        | OpPhi(op0, op1, variableParent) -> [| variableParent :> obj |]
        | OpLoopMerge(mergeBlock, continueTarget, op2) -> [| mergeBlock :> obj; continueTarget :> obj; op2 :> obj |]
        | OpSelectionMerge(mergeBlock, op1) -> [| mergeBlock :> obj; op1 :> obj |]
        | OpLabel(op0) -> [| op0 :> obj |]
        | OpBranch(targetLabel) -> [| targetLabel :> obj |]
        | OpBranchConditional(condition, trueLabel, falseLabel, branchweights) -> [| condition :> obj; trueLabel :> obj; falseLabel :> obj; branchweights :> obj |]
        | OpSwitch(selector, _default, target) -> [| selector :> obj; _default :> obj; target :> obj |]
        | OpKill -> [|  |]
        | OpReturn -> [|  |]
        | OpReturnValue(value) -> [| value :> obj |]
        | OpUnreachable -> [|  |]
        | OpLifetimeStart(pointer, size) -> [| pointer :> obj; size :> obj |]
        | OpLifetimeStop(pointer, size) -> [| pointer :> obj; size :> obj |]
        | OpGroupAsyncCopy(op0, op1, execution, destination, source, numElements, stride, event) -> [| execution :> obj; destination :> obj; source :> obj; numElements :> obj; stride :> obj; event :> obj |]
        | OpGroupWaitEvents(execution, numEvents, eventsList) -> [| execution :> obj; numEvents :> obj; eventsList :> obj |]
        | OpGroupAll(op0, op1, execution, predicate) -> [| execution :> obj; predicate :> obj |]
        | OpGroupAny(op0, op1, execution, predicate) -> [| execution :> obj; predicate :> obj |]
        | OpGroupBroadcast(op0, op1, execution, value, localId) -> [| execution :> obj; value :> obj; localId :> obj |]
        | OpGroupIAdd(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupFAdd(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupFMin(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupUMin(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupSMin(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupFMax(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupUMax(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupSMax(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpReadPipe(op0, op1, pipe, pointer, packetSize, packetAlignment) -> [| pipe :> obj; pointer :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpWritePipe(op0, op1, pipe, pointer, packetSize, packetAlignment) -> [| pipe :> obj; pointer :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpReservedReadPipe(op0, op1, pipe, reserveId, index, pointer, packetSize, packetAlignment) -> [| pipe :> obj; reserveId :> obj; index :> obj; pointer :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpReservedWritePipe(op0, op1, pipe, reserveId, index, pointer, packetSize, packetAlignment) -> [| pipe :> obj; reserveId :> obj; index :> obj; pointer :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpReserveReadPipePackets(op0, op1, pipe, numPackets, packetSize, packetAlignment) -> [| pipe :> obj; numPackets :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpReserveWritePipePackets(op0, op1, pipe, numPackets, packetSize, packetAlignment) -> [| pipe :> obj; numPackets :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpCommitReadPipe(pipe, reserveId, packetSize, packetAlignment) -> [| pipe :> obj; reserveId :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpCommitWritePipe(pipe, reserveId, packetSize, packetAlignment) -> [| pipe :> obj; reserveId :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpIsValidReserveId(op0, op1, reserveId) -> [| reserveId :> obj |]
        | OpGetNumPipePackets(op0, op1, pipe, packetSize, packetAlignment) -> [| pipe :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpGetMaxPipePackets(op0, op1, pipe, packetSize, packetAlignment) -> [| pipe :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpGroupReserveReadPipePackets(op0, op1, execution, pipe, numPackets, packetSize, packetAlignment) -> [| execution :> obj; pipe :> obj; numPackets :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpGroupReserveWritePipePackets(op0, op1, execution, pipe, numPackets, packetSize, packetAlignment) -> [| execution :> obj; pipe :> obj; numPackets :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpGroupCommitReadPipe(execution, pipe, reserveId, packetSize, packetAlignment) -> [| execution :> obj; pipe :> obj; reserveId :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpGroupCommitWritePipe(execution, pipe, reserveId, packetSize, packetAlignment) -> [| execution :> obj; pipe :> obj; reserveId :> obj; packetSize :> obj; packetAlignment :> obj |]
        | OpEnqueueMarker(op0, op1, queue, numEvents, waitEvents, retEvent) -> [| queue :> obj; numEvents :> obj; waitEvents :> obj; retEvent :> obj |]
        | OpEnqueueKernel(op0, op1, queue, flags, nDRange, numEvents, waitEvents, retEvent, invoke, param, paramSize, paramAlign, localSize) -> [| queue :> obj; flags :> obj; nDRange :> obj; numEvents :> obj; waitEvents :> obj; retEvent :> obj; invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj; localSize :> obj |]
        | OpGetKernelNDrangeSubGroupCount(op0, op1, nDRange, invoke, param, paramSize, paramAlign) -> [| nDRange :> obj; invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj |]
        | OpGetKernelNDrangeMaxSubGroupSize(op0, op1, nDRange, invoke, param, paramSize, paramAlign) -> [| nDRange :> obj; invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj |]
        | OpGetKernelWorkGroupSize(op0, op1, invoke, param, paramSize, paramAlign) -> [| invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj |]
        | OpGetKernelPreferredWorkGroupSizeMultiple(op0, op1, invoke, param, paramSize, paramAlign) -> [| invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj |]
        | OpRetainEvent(event) -> [| event :> obj |]
        | OpReleaseEvent(event) -> [| event :> obj |]
        | OpCreateUserEvent(op0, op1) -> [|  |]
        | OpIsValidEvent(op0, op1, event) -> [| event :> obj |]
        | OpSetUserEventStatus(event, status) -> [| event :> obj; status :> obj |]
        | OpCaptureEventProfilingInfo(event, profilingInfo, value) -> [| event :> obj; profilingInfo :> obj; value :> obj |]
        | OpGetDefaultQueue(op0, op1) -> [|  |]
        | OpBuildNDRange(op0, op1, globalWorkSize, localWorkSize, globalWorkOffset) -> [| globalWorkSize :> obj; localWorkSize :> obj; globalWorkOffset :> obj |]
        | OpImageSparseSampleImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> [| sampledImage :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageSparseSampleExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> [| sampledImage :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageSparseSampleDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; dref :> obj; op5 :> obj; parameters :> obj |]
        | OpImageSparseSampleDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; dref :> obj; op5 :> obj; parameters :> obj |]
        | OpImageSparseSampleProjImplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> [| sampledImage :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageSparseSampleProjExplicitLod(op0, op1, sampledImage, coordinate, op4, parameters) -> [| sampledImage :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageSparseSampleProjDrefImplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; dref :> obj; op5 :> obj; parameters :> obj |]
        | OpImageSparseSampleProjDrefExplicitLod(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; dref :> obj; op5 :> obj; parameters :> obj |]
        | OpImageSparseFetch(op0, op1, image, coordinate, op4, parameters) -> [| image :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpImageSparseGather(op0, op1, sampledImage, coordinate, _component, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; _component :> obj; op5 :> obj; parameters :> obj |]
        | OpImageSparseDrefGather(op0, op1, sampledImage, coordinate, dref, op5, parameters) -> [| sampledImage :> obj; coordinate :> obj; dref :> obj; op5 :> obj; parameters :> obj |]
        | OpImageSparseTexelsResident(op0, op1, residentCode) -> [| residentCode :> obj |]
        | OpNoLine -> [|  |]
        | OpAtomicFlagTestAndSet(op0, op1, pointer, scope, semantics) -> [| pointer :> obj; scope :> obj; semantics :> obj |]
        | OpAtomicFlagClear(pointer, scope, semantics) -> [| pointer :> obj; scope :> obj; semantics :> obj |]
        | OpImageSparseRead(op0, op1, image, coordinate, op4, parameters) -> [| image :> obj; coordinate :> obj; op4 :> obj; parameters :> obj |]
        | OpSizeOf(op0, op1, pointer) -> [| pointer :> obj |]
        | OpTypePipeStorage(op0) -> [| op0 :> obj |]
        | OpConstantPipeStorage(op0, op1, packetSize, packetAlignment, capacity) -> [| packetSize :> obj; packetAlignment :> obj; capacity :> obj |]
        | OpCreatePipeFromPipeStorage(op0, op1, pipeStorage) -> [| pipeStorage :> obj |]
        | OpGetKernelLocalSizeForSubgroupCount(op0, op1, subgroupCount, invoke, param, paramSize, paramAlign) -> [| subgroupCount :> obj; invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj |]
        | OpGetKernelMaxNumSubgroups(op0, op1, invoke, param, paramSize, paramAlign) -> [| invoke :> obj; param :> obj; paramSize :> obj; paramAlign :> obj |]
        | OpTypeNamedBarrier(op0) -> [| op0 :> obj |]
        | OpNamedBarrierInitialize(op0, op1, subgroupCount) -> [| subgroupCount :> obj |]
        | OpMemoryNamedBarrier(namedBarrier, memory, semantics) -> [| namedBarrier :> obj; memory :> obj; semantics :> obj |]
        | OpModuleProcessed(_process) -> [| _process :> obj |]
        | OpExecutionModeId(entryPoint, mode) -> [| entryPoint :> obj; mode :> obj |]
        | OpDecorateId(target, op1) -> [| target :> obj; op1 :> obj |]
        | OpSubgroupBallotKHR(op0, op1, predicate) -> [| predicate :> obj |]
        | OpSubgroupFirstInvocationKHR(op0, op1, value) -> [| value :> obj |]
        | OpSubgroupAllKHR(op0, op1, predicate) -> [| predicate :> obj |]
        | OpSubgroupAnyKHR(op0, op1, predicate) -> [| predicate :> obj |]
        | OpSubgroupAllEqualKHR(op0, op1, predicate) -> [| predicate :> obj |]
        | OpSubgroupReadInvocationKHR(op0, op1, value, index) -> [| value :> obj; index :> obj |]
        | OpGroupIAddNonUniformAMD(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupFAddNonUniformAMD(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupFMinNonUniformAMD(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupUMinNonUniformAMD(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupSMinNonUniformAMD(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupFMaxNonUniformAMD(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupUMaxNonUniformAMD(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
        | OpGroupSMaxNonUniformAMD(op0, op1, execution, operation, x) -> [| execution :> obj; operation :> obj; x :> obj |]
