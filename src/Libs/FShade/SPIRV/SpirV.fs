namespace SpirV

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
    | OpCapability of capability : int
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
    | OpFunction of resultType : uint32 * result : uint32 * _function : FunctionControlMask * funType : uint32
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
    | OpControlBarrier of exec : ExecutionScope * mem : ExecutionScope * sem : MemorySemantics
    | OpMemoryBarrier of mem : ExecutionScope * sen : MemorySemantics
    | OpAtomicLoad of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics
    | OpAtomicStore of ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
    | OpAtomicExchange of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
    | OpAtomicCompareExchange of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * eq : MemorySemantics * neq : MemorySemantics * value : uint32 * comparator : uint32
    | OpAtomicCompareExchangeWeak of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * eq : MemorySemantics * neq : MemorySemantics * value : uint32 * comparator : uint32
    | OpAtomicIIncrement of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics
    | OpAtomicIDecrement of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics
    | OpAtomicIAdd of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
    | OpAtomicISub of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
    | OpAtomicSMin of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
    | OpAtomicUMin of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
    | OpAtomicSMax of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
    | OpAtomicUMax of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
    | OpAtomicAnd of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
    | OpAtomicOr of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
    | OpAtomicXor of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics * value : uint32
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
    | OpGroupAsyncCopy of resultType : uint32 * result : uint32 * execution : ExecutionScope * dest : uint32 * _source : uint32 * num : uint32 * stride : uint32 * evt : uint32
    | OpGroupWaitEvents of exec : ExecutionScope * num : uint32 * evtList : uint32
    | OpGroupAll of resultType : uint32 * result : uint32 * exec : ExecutionScope * pred : uint32
    | OpGroupAny of resultType : uint32 * result : uint32 * exec : ExecutionScope * pred : uint32
    | OpGroupBroadcast of resultType : uint32 * result : uint32 * exec : ExecutionScope * value : uint32 * localid : uint32
    | OpGroupIAdd of resultType : uint32 * result : uint32 * exec : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupFAdd of resultType : uint32 * result : uint32 * exec : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupFMin of resultType : uint32 * result : uint32 * exec : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupUMin of resultType : uint32 * result : uint32 * exec : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupSMin of resultType : uint32 * result : uint32 * exec : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupFMax of resultType : uint32 * result : uint32 * exec : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupUMax of resultType : uint32 * result : uint32 * exec : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupSMax of resultType : uint32 * result : uint32 * exec : ExecutionScope * operation : GroupOperation * x : uint32
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
    | OpGroupReserveReadPipePackets of resultType : uint32 * result : uint32 * exec : ExecutionScope * pipe : uint32 * numPackets : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpGroupReserveWritePipePackets of resultType : uint32 * result : uint32 * exec : ExecutionScope * pipe : uint32 * numPackets : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpGroupCommitReadPipe of exec : ExecutionScope * pipe : uint32 * reserveId : uint32 * packetSize : uint32 * packetAlign : uint32
    | OpGroupCommitWritePipe of exec : ExecutionScope * pipe : uint32 * reserveId : uint32 * packetSize : uint32 * packetAlign : uint32
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
    | OpAtomicFlagTestAndSet of resultType : uint32 * result : uint32 * ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics
    | OpAtomicFlagClear of ptr : uint32 * scope : ExecutionScope * sem : MemorySemantics


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
                        let capability = unbox<int> (int (source.ReadUInt32()))
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
                        let _function = unbox<FunctionControlMask> (int (source.ReadUInt32()))
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
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let mem = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpControlBarrier(exec, mem, sem)
                    | 225 ->
                        let mem = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sen = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpMemoryBarrier(mem, sen)
                    | 227 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicLoad(resultType, result, ptr, scope, sem)
                    | 228 ->
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicStore(ptr, scope, sem, value)
                    | 229 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicExchange(resultType, result, ptr, scope, sem, value)
                    | 230 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let eq = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let neq = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        let comparator = source.ReadUInt32()
                        yield OpAtomicCompareExchange(resultType, result, ptr, scope, eq, neq, value, comparator)
                    | 231 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let eq = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let neq = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        let comparator = source.ReadUInt32()
                        yield OpAtomicCompareExchangeWeak(resultType, result, ptr, scope, eq, neq, value, comparator)
                    | 232 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicIIncrement(resultType, result, ptr, scope, sem)
                    | 233 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicIDecrement(resultType, result, ptr, scope, sem)
                    | 234 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicIAdd(resultType, result, ptr, scope, sem, value)
                    | 235 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicISub(resultType, result, ptr, scope, sem, value)
                    | 236 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicSMin(resultType, result, ptr, scope, sem, value)
                    | 237 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicUMin(resultType, result, ptr, scope, sem, value)
                    | 238 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicSMax(resultType, result, ptr, scope, sem, value)
                    | 239 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicUMax(resultType, result, ptr, scope, sem, value)
                    | 240 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicAnd(resultType, result, ptr, scope, sem, value)
                    | 241 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicOr(resultType, result, ptr, scope, sem, value)
                    | 242 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
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
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let dest = source.ReadUInt32()
                        let _source = source.ReadUInt32()
                        let num = source.ReadUInt32()
                        let stride = source.ReadUInt32()
                        let evt = source.ReadUInt32()
                        yield OpGroupAsyncCopy(resultType, result, execution, dest, _source, num, stride, evt)
                    | 260 ->
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let num = source.ReadUInt32()
                        let evtList = source.ReadUInt32()
                        yield OpGroupWaitEvents(exec, num, evtList)
                    | 261 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let pred = source.ReadUInt32()
                        yield OpGroupAll(resultType, result, exec, pred)
                    | 262 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let pred = source.ReadUInt32()
                        yield OpGroupAny(resultType, result, exec, pred)
                    | 263 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        let localid = source.ReadUInt32()
                        yield OpGroupBroadcast(resultType, result, exec, value, localid)
                    | 264 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupIAdd(resultType, result, exec, operation, x)
                    | 265 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupFAdd(resultType, result, exec, operation, x)
                    | 266 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupFMin(resultType, result, exec, operation, x)
                    | 267 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupUMin(resultType, result, exec, operation, x)
                    | 268 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupSMin(resultType, result, exec, operation, x)
                    | 269 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupFMax(resultType, result, exec, operation, x)
                    | 270 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupUMax(resultType, result, exec, operation, x)
                    | 271 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
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
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let numPackets = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpGroupReserveReadPipePackets(resultType, result, exec, pipe, numPackets, packetSize, packetAlign)
                    | 286 ->
                        let resultType = source.ReadUInt32()
                        let result = source.ReadUInt32()
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let numPackets = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpGroupReserveWritePipePackets(resultType, result, exec, pipe, numPackets, packetSize, packetAlign)
                    | 287 ->
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlign = source.ReadUInt32()
                        yield OpGroupCommitReadPipe(exec, pipe, reserveId, packetSize, packetAlign)
                    | 288 ->
                        let exec = unbox<ExecutionScope> (int (source.ReadUInt32()))
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
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let sem = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicFlagTestAndSet(resultType, result, ptr, scope, sem)
                    | 319 ->
                        let ptr = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
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
