namespace SpirV

type Instruction = 
    | OpNop
    | OpUndef
    | OpSourceContinued of continuedSource : string
    | OpSource of _OperandSource : SourceLanguage * version : uint32 * _source : string
    | OpSourceExtension of extension : string
    | OpName of _target : uint32 * name : string
    | OpMemberName of _type : uint32 * _member : uint32 * name : string
    | OpString of string : string
    | OpLine of file : uint32 * line : uint32 * column : uint32
    | OpExtension of name : string
    | OpExtInstImport of name : string
    | OpExtInst of set : uint32 * instruction : uint32 * args : uint32[]
    | OpMemoryModel of _OperandAddressing : AddressingModel * _OperandMemory : MemoryModel
    | OpEntryPoint of _OperandExecutionModel : ExecutionModel * entryPoint : uint32 * name : string
    | OpExecutionMode of entryPoint : uint32 * mode : ExecutionMode * args : Option<uint32>
    | OpCapability of capability : int
    | OpTypeVoid
    | OpTypeBool
    | OpTypeInt of width : uint32 * signedness : uint32
    | OpTypeFloat of width : uint32
    | OpTypeVector of componentType : uint32 * componentCount : uint32
    | OpTypeMatrix of columnType : uint32 * columnCount : uint32
    | OpTypeImage of sampledType : uint32 * _OperandDimensionality : Dim * depth : uint32 * arrayed : uint32 * mS : uint32 * sampled : uint32 * _OperandSamplerImageFormat : int * _OperandAccessQualifier : Option<AccessQualifier>
    | OpTypeSampler
    | OpTypeSampledImage of imageType : uint32
    | OpTypeArray of elementType : uint32 * length : uint32
    | OpTypeRuntimeArray of elementType : uint32
    | OpTypeStruct of args : uint32[]
    | OpTypeOpaque of thenameoftheopaquetype : string
    | OpTypePointer of _OperandStorage : StorageClass * _type : uint32
    | OpTypeFunction of returnType : uint32 * args : uint32[]
    | OpTypeEvent
    | OpTypeDeviceEvent
    | OpTypeReserveId
    | OpTypeQueue
    | OpTypePipe of qualifier : AccessQualifier
    | OpTypeForwardPointer of pointerType : uint32 * _OperandStorage : StorageClass
    | OpConstantTrue
    | OpConstantFalse
    | OpConstant of value : uint32[]
    | OpConstantComposite of constituents : uint32[]
    | OpConstantSampler of _OperandSamplerAddressingMode : SamplerAddressingMode * param : uint32 * _OperandSamplerFilterMode : SamplerFilterMode
    | OpConstantNull
    | OpSpecConstantTrue
    | OpSpecConstantFalse
    | OpSpecConstant of value : uint32[]
    | OpSpecConstantComposite of constituents : uint32[]
    | OpSpecConstantOp of opcode : uint32 * operands : uint32[]
    | OpFunction of _OperandFunction : FunctionControlMask * functionType : uint32
    | OpFunctionParameter
    | OpFunctionEnd
    | OpFunctionCall of _function : uint32 * args : uint32[]
    | OpVariable of _OperandStorage : StorageClass * initializer : Option<uint32>
    | OpImageTexelPointer of image : uint32 * coordinate : uint32 * sample : uint32
    | OpLoad of pointer : uint32 * _OperandMemoryAccess : Option<MemoryAccess>
    | OpStore of pointer : uint32 * _object : uint32 * _OperandMemoryAccess : Option<MemoryAccess>
    | OpCopyMemory of _target : uint32 * _source : uint32 * _OperandMemoryAccess : Option<MemoryAccess>
    | OpCopyMemorySized of _target : uint32 * _source : uint32 * size : uint32 * _OperandMemoryAccess : Option<MemoryAccess>
    | OpAccessChain of _base : uint32 * indexes : uint32[]
    | OpInBoundsAccessChain of _base : uint32 * indexes : uint32[]
    | OpPtrAccessChain of _base : uint32 * element : uint32 * indexes : uint32[]
    | OpArrayLength of structure : uint32 * arraymember : uint32
    | OpGenericPtrMemSemantics of pointer : uint32
    | OpInBoundsPtrAccessChain of _base : uint32 * element : uint32 * indexes : uint32[]
    | OpDecorate of _target : uint32 * _OperandDecoration : Decoration * args : uint32[]
    | OpMemberDecorate of structureType : uint32 * _member : uint32 * _OperandDecoration : Decoration * args : uint32[]
    | OpDecorationGroup
    | OpGroupDecorate of decorationGroup : uint32 * targets : uint32[]
    | OpGroupMemberDecorate of decorationGroup : uint32 * targets : uint32[]
    | OpVectorExtractDynamic of vector : uint32 * index : uint32
    | OpVectorInsertDynamic of vector : uint32 * _component : uint32 * index : uint32
    | OpVectorShuffle of vector1 : uint32 * vector2 : uint32 * components : uint32[]
    | OpCompositeConstruct of constituents : uint32[]
    | OpCompositeExtract of composite : uint32 * indexes : uint32[]
    | OpCompositeInsert of _object : uint32 * composite : uint32 * indexes : uint32[]
    | OpCopyObject of operand : uint32
    | OpTranspose of matrix : uint32
    | OpSampledImage of image : uint32 * sampler : uint32
    | OpImageSampleImplicitLod of sampledImage : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSampleExplicitLod of sampledImage : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSampleDrefImplicitLod of sampledImage : uint32 * coordinate : uint32 * dref : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSampleDrefExplicitLod of sampledImage : uint32 * coordinate : uint32 * dref : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSampleProjImplicitLod of sampledImage : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSampleProjExplicitLod of sampledImage : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSampleProjDrefImplicitLod of sampledImage : uint32 * coordinate : uint32 * dref : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSampleProjDrefExplicitLod of sampledImage : uint32 * coordinate : uint32 * dref : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageFetch of image : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageGather of sampledImage : uint32 * coordinate : uint32 * _component : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageDrefGather of sampledImage : uint32 * coordinate : uint32 * dref : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageRead of image : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageWrite of image : uint32 * coordinate : uint32 * texel : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImage of sampledImage : uint32
    | OpImageQueryFormat of image : uint32
    | OpImageQueryOrder of image : uint32
    | OpImageQuerySizeLod of image : uint32 * levelofDetail : uint32
    | OpImageQuerySize of image : uint32
    | OpImageQueryLod of image : uint32 * coordinate : uint32
    | OpImageQueryLevels of image : uint32
    | OpImageQuerySamples of image : uint32
    | OpConvertFToU of floatValue : uint32
    | OpConvertFToS of floatValue : uint32
    | OpConvertSToF of signedValue : uint32
    | OpConvertUToF of unsignedValue : uint32
    | OpUConvert of unsignedValue : uint32
    | OpSConvert of signedValue : uint32
    | OpFConvert of floatValue : uint32
    | OpQuantizeToF16 of value : uint32
    | OpConvertPtrToU of pointer : uint32
    | OpSatConvertSToU of signedValue : uint32
    | OpSatConvertUToS of unsignedValue : uint32
    | OpConvertUToPtr of integerValue : uint32
    | OpPtrCastToGeneric of pointer : uint32
    | OpGenericCastToPtr of pointer : uint32
    | OpGenericCastToPtrExplicit of pointer : uint32 * storage : StorageClass
    | OpBitcast of operand : uint32
    | OpSNegate of operand : uint32
    | OpFNegate of operand : uint32
    | OpIAdd of operand1 : uint32 * operand2 : uint32
    | OpFAdd of operand1 : uint32 * operand2 : uint32
    | OpISub of operand1 : uint32 * operand2 : uint32
    | OpFSub of operand1 : uint32 * operand2 : uint32
    | OpIMul of operand1 : uint32 * operand2 : uint32
    | OpFMul of operand1 : uint32 * operand2 : uint32
    | OpUDiv of operand1 : uint32 * operand2 : uint32
    | OpSDiv of operand1 : uint32 * operand2 : uint32
    | OpFDiv of operand1 : uint32 * operand2 : uint32
    | OpUMod of operand1 : uint32 * operand2 : uint32
    | OpSRem of operand1 : uint32 * operand2 : uint32
    | OpSMod of operand1 : uint32 * operand2 : uint32
    | OpFRem of operand1 : uint32 * operand2 : uint32
    | OpFMod of operand1 : uint32 * operand2 : uint32
    | OpVectorTimesScalar of vector : uint32 * scalar : uint32
    | OpMatrixTimesScalar of matrix : uint32 * scalar : uint32
    | OpVectorTimesMatrix of vector : uint32 * matrix : uint32
    | OpMatrixTimesVector of matrix : uint32 * vector : uint32
    | OpMatrixTimesMatrix of leftMatrix : uint32 * rightMatrix : uint32
    | OpOuterProduct of vector1 : uint32 * vector2 : uint32
    | OpDot of vector1 : uint32 * vector2 : uint32
    | OpIAddCarry of operand1 : uint32 * operand2 : uint32
    | OpISubBorrow of operand1 : uint32 * operand2 : uint32
    | OpUMulExtended of operand1 : uint32 * operand2 : uint32
    | OpSMulExtended of operand1 : uint32 * operand2 : uint32
    | OpAny of vector : uint32
    | OpAll of vector : uint32
    | OpIsNan of x : uint32
    | OpIsInf of x : uint32
    | OpIsFinite of x : uint32
    | OpIsNormal of x : uint32
    | OpSignBitSet of x : uint32
    | OpLessOrGreater of x : uint32 * y : uint32
    | OpOrdered of x : uint32 * y : uint32
    | OpUnordered of x : uint32 * y : uint32
    | OpLogicalEqual of operand1 : uint32 * operand2 : uint32
    | OpLogicalNotEqual of operand1 : uint32 * operand2 : uint32
    | OpLogicalOr of operand1 : uint32 * operand2 : uint32
    | OpLogicalAnd of operand1 : uint32 * operand2 : uint32
    | OpLogicalNot of operand : uint32
    | OpSelect of condition : uint32 * object1 : uint32 * object2 : uint32
    | OpIEqual of operand1 : uint32 * operand2 : uint32
    | OpINotEqual of operand1 : uint32 * operand2 : uint32
    | OpUGreaterThan of operand1 : uint32 * operand2 : uint32
    | OpSGreaterThan of operand1 : uint32 * operand2 : uint32
    | OpUGreaterThanEqual of operand1 : uint32 * operand2 : uint32
    | OpSGreaterThanEqual of operand1 : uint32 * operand2 : uint32
    | OpULessThan of operand1 : uint32 * operand2 : uint32
    | OpSLessThan of operand1 : uint32 * operand2 : uint32
    | OpULessThanEqual of operand1 : uint32 * operand2 : uint32
    | OpSLessThanEqual of operand1 : uint32 * operand2 : uint32
    | OpFOrdEqual of operand1 : uint32 * operand2 : uint32
    | OpFUnordEqual of operand1 : uint32 * operand2 : uint32
    | OpFOrdNotEqual of operand1 : uint32 * operand2 : uint32
    | OpFUnordNotEqual of operand1 : uint32 * operand2 : uint32
    | OpFOrdLessThan of operand1 : uint32 * operand2 : uint32
    | OpFUnordLessThan of operand1 : uint32 * operand2 : uint32
    | OpFOrdGreaterThan of operand1 : uint32 * operand2 : uint32
    | OpFUnordGreaterThan of operand1 : uint32 * operand2 : uint32
    | OpFOrdLessThanEqual of operand1 : uint32 * operand2 : uint32
    | OpFUnordLessThanEqual of operand1 : uint32 * operand2 : uint32
    | OpFOrdGreaterThanEqual of operand1 : uint32 * operand2 : uint32
    | OpFUnordGreaterThanEqual of operand1 : uint32 * operand2 : uint32
    | OpShiftRightLogical of _base : uint32 * shift : uint32
    | OpShiftRightArithmetic of _base : uint32 * shift : uint32
    | OpShiftLeftLogical of _base : uint32 * shift : uint32
    | OpBitwiseOr of operand1 : uint32 * operand2 : uint32
    | OpBitwiseXor of operand1 : uint32 * operand2 : uint32
    | OpBitwiseAnd of operand1 : uint32 * operand2 : uint32
    | OpNot of operand : uint32
    | OpBitFieldInsert of _base : uint32 * insert : uint32 * offset : uint32 * count : uint32
    | OpBitFieldSExtract of _base : uint32 * offset : uint32 * count : uint32
    | OpBitFieldUExtract of _base : uint32 * offset : uint32 * count : uint32
    | OpBitReverse of _base : uint32
    | OpBitCount of _base : uint32
    | OpDPdx of p : uint32
    | OpDPdy of p : uint32
    | OpFwidth of p : uint32
    | OpDPdxFine of p : uint32
    | OpDPdyFine of p : uint32
    | OpFwidthFine of p : uint32
    | OpDPdxCoarse of p : uint32
    | OpDPdyCoarse of p : uint32
    | OpFwidthCoarse of p : uint32
    | OpEmitVertex
    | OpEndPrimitive
    | OpEmitStreamVertex of stream : uint32
    | OpEndStreamPrimitive of stream : uint32
    | OpControlBarrier of execution : ExecutionScope * memory : ExecutionScope * semantics : MemorySemantics
    | OpMemoryBarrier of memory : ExecutionScope * semantics : MemorySemantics
    | OpAtomicLoad of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics
    | OpAtomicStore of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpAtomicExchange of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpAtomicCompareExchange of pointer : uint32 * scope : ExecutionScope * equal : MemorySemantics * unequal : MemorySemantics * value : uint32 * comparator : uint32
    | OpAtomicCompareExchangeWeak of pointer : uint32 * scope : ExecutionScope * equal : MemorySemantics * unequal : MemorySemantics * value : uint32 * comparator : uint32
    | OpAtomicIIncrement of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics
    | OpAtomicIDecrement of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics
    | OpAtomicIAdd of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpAtomicISub of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpAtomicSMin of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpAtomicUMin of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpAtomicSMax of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpAtomicUMax of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpAtomicAnd of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpAtomicOr of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpAtomicXor of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics * value : uint32
    | OpPhi of variableParent : uint32[]
    | OpLoopMerge of mergeBlock : uint32 * continueTarget : uint32 * _OperandLoop : LoopControl
    | OpSelectionMerge of mergeBlock : uint32 * _OperandSelect : SelectionControl
    | OpLabel
    | OpBranch of targetLabel : uint32
    | OpBranchConditional of condition : uint32 * trueLabel : uint32 * falseLabel : uint32 * branchweights : uint32[]
    | OpSwitch of selector : uint32 * _default : uint32 * _target : uint32[]
    | OpKill
    | OpReturn
    | OpReturnValue of value : uint32
    | OpUnreachable
    | OpLifetimeStart of pointer : uint32 * size : uint32
    | OpLifetimeStop of pointer : uint32 * size : uint32
    | OpGroupAsyncCopy of execution : ExecutionScope * destination : uint32 * _source : uint32 * numElements : uint32 * stride : uint32 * event : uint32
    | OpGroupWaitEvents of execution : ExecutionScope * numEvents : uint32 * eventsList : uint32
    | OpGroupAll of execution : ExecutionScope * predicate : uint32
    | OpGroupAny of execution : ExecutionScope * predicate : uint32
    | OpGroupBroadcast of execution : ExecutionScope * value : uint32 * localId : uint32
    | OpGroupIAdd of execution : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupFAdd of execution : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupFMin of execution : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupUMin of execution : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupSMin of execution : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupFMax of execution : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupUMax of execution : ExecutionScope * operation : GroupOperation * x : uint32
    | OpGroupSMax of execution : ExecutionScope * operation : GroupOperation * x : uint32
    | OpReadPipe of pipe : uint32 * pointer : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpWritePipe of pipe : uint32 * pointer : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpReservedReadPipe of pipe : uint32 * reserveId : uint32 * index : uint32 * pointer : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpReservedWritePipe of pipe : uint32 * reserveId : uint32 * index : uint32 * pointer : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpReserveReadPipePackets of pipe : uint32 * numPackets : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpReserveWritePipePackets of pipe : uint32 * numPackets : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpCommitReadPipe of pipe : uint32 * reserveId : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpCommitWritePipe of pipe : uint32 * reserveId : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpIsValidReserveId of reserveId : uint32
    | OpGetNumPipePackets of pipe : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpGetMaxPipePackets of pipe : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpGroupReserveReadPipePackets of execution : ExecutionScope * pipe : uint32 * numPackets : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpGroupReserveWritePipePackets of execution : ExecutionScope * pipe : uint32 * numPackets : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpGroupCommitReadPipe of execution : ExecutionScope * pipe : uint32 * reserveId : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpGroupCommitWritePipe of execution : ExecutionScope * pipe : uint32 * reserveId : uint32 * packetSize : uint32 * packetAlignment : uint32
    | OpEnqueueMarker of queue : uint32 * numEvents : uint32 * waitEvents : uint32 * retEvent : uint32
    | OpEnqueueKernel of queue : uint32 * flags : uint32 * nDRange : uint32 * numEvents : uint32 * waitEvents : uint32 * retEvent : uint32 * invoke : uint32 * param : uint32 * paramSize : uint32 * paramAlign : uint32 * localSize : uint32[]
    | OpGetKernelNDrangeSubGroupCount of nDRange : uint32 * invoke : uint32 * param : uint32 * paramSize : uint32 * paramAlign : uint32
    | OpGetKernelNDrangeMaxSubGroupSize of nDRange : uint32 * invoke : uint32 * param : uint32 * paramSize : uint32 * paramAlign : uint32
    | OpGetKernelWorkGroupSize of invoke : uint32 * param : uint32 * paramSize : uint32 * paramAlign : uint32
    | OpGetKernelPreferredWorkGroupSizeMultiple of invoke : uint32 * param : uint32 * paramSize : uint32 * paramAlign : uint32
    | OpRetainEvent of event : uint32
    | OpReleaseEvent of event : uint32
    | OpCreateUserEvent
    | OpIsValidEvent of event : uint32
    | OpSetUserEventStatus of event : uint32 * status : uint32
    | OpCaptureEventProfilingInfo of event : uint32 * profilingInfo : uint32 * value : uint32
    | OpGetDefaultQueue
    | OpBuildNDRange of globalWorkSize : uint32 * localWorkSize : uint32 * globalWorkOffset : uint32
    | OpImageSparseSampleImplicitLod of sampledImage : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseSampleExplicitLod of sampledImage : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseSampleDrefImplicitLod of sampledImage : uint32 * coordinate : uint32 * dref : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseSampleDrefExplicitLod of sampledImage : uint32 * coordinate : uint32 * dref : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseSampleProjImplicitLod of sampledImage : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseSampleProjExplicitLod of sampledImage : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseSampleProjDrefImplicitLod of sampledImage : uint32 * coordinate : uint32 * dref : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseSampleProjDrefExplicitLod of sampledImage : uint32 * coordinate : uint32 * dref : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseFetch of image : uint32 * coordinate : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseGather of sampledImage : uint32 * coordinate : uint32 * _component : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseDrefGather of sampledImage : uint32 * coordinate : uint32 * dref : uint32 * _OperandImageOperands : int * _OperandVariableIds : uint32[]
    | OpImageSparseTexelsResident of residentCode : uint32
    | OpNoLine
    | OpAtomicFlagTestAndSet of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics
    | OpAtomicFlagClear of pointer : uint32 * scope : ExecutionScope * semantics : MemorySemantics


type Module =
    {
        magic : uint32
        version : uint32
        generatorMagic : uint32
        bound : uint32
        reserved : uint32
        instructions : seq<Instruction>
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
                | OpUndef -> writeOpHeader 1 3 target
                | OpSourceContinued(continuedSource) -> 
                    let wordCount = 2 + (continuedSource.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 2 wordCount target
                    writeString continuedSource target
                | OpSource(_OperandSource, version, _source) -> 
                    let wordCount = 5 + (_source.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 3 wordCount target
                    target.Write(uint32 (int _OperandSource))
                    target.Write(version)
                    writeString _source target
                | OpSourceExtension(extension) -> 
                    let wordCount = 2 + (extension.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 4 wordCount target
                    writeString extension target
                | OpName(_target, name) -> 
                    let wordCount = 3 + (name.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 5 wordCount target
                    target.Write(_target)
                    writeString name target
                | OpMemberName(_type, _member, name) -> 
                    let wordCount = 4 + (name.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 6 wordCount target
                    target.Write(_type)
                    target.Write(_member)
                    writeString name target
                | OpString(string) -> 
                    let wordCount = 3 + (string.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 7 wordCount target
                    writeString string target
                | OpLine(file, line, column) -> 
                    writeOpHeader 8 4 target
                    target.Write(file)
                    target.Write(line)
                    target.Write(column)
                | OpExtension(name) -> 
                    let wordCount = 2 + (name.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 10 wordCount target
                    writeString name target
                | OpExtInstImport(name) -> 
                    let wordCount = 3 + (name.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 11 wordCount target
                    writeString name target
                | OpExtInst(set, instruction, args) -> 
                    let wordCount = 6 + args.Length
                    writeOpHeader 12 wordCount target
                    target.Write(set)
                    target.Write(instruction)
                    for v in args do
                        target.Write(v)
                | OpMemoryModel(_OperandAddressing, _OperandMemory) -> 
                    writeOpHeader 14 3 target
                    target.Write(uint32 (int _OperandAddressing))
                    target.Write(uint32 (int _OperandMemory))
                | OpEntryPoint(_OperandExecutionModel, entryPoint, name) -> 
                    let wordCount = 5 + (name.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 15 wordCount target
                    target.Write(uint32 (int _OperandExecutionModel))
                    target.Write(entryPoint)
                    writeString name target
                | OpExecutionMode(entryPoint, mode, args) -> 
                    let wordCount = 4 + (if args.IsSome then 1 else 0)
                    writeOpHeader 16 wordCount target
                    target.Write(entryPoint)
                    target.Write(uint32 (int mode))
                    match args with
                        | Some v -> target.Write(v)
                        | None -> ()
                | OpCapability(capability) -> 
                    writeOpHeader 17 2 target
                    target.Write(uint32 (int capability))
                | OpTypeVoid -> writeOpHeader 19 2 target
                | OpTypeBool -> writeOpHeader 20 2 target
                | OpTypeInt(width, signedness) -> 
                    writeOpHeader 21 4 target
                    target.Write(width)
                    target.Write(signedness)
                | OpTypeFloat(width) -> 
                    writeOpHeader 22 3 target
                    target.Write(width)
                | OpTypeVector(componentType, componentCount) -> 
                    writeOpHeader 23 4 target
                    target.Write(componentType)
                    target.Write(componentCount)
                | OpTypeMatrix(columnType, columnCount) -> 
                    writeOpHeader 24 4 target
                    target.Write(columnType)
                    target.Write(columnCount)
                | OpTypeImage(sampledType, _OperandDimensionality, depth, arrayed, mS, sampled, _OperandSamplerImageFormat, _OperandAccessQualifier) -> 
                    let wordCount = 10 + (if _OperandAccessQualifier.IsSome then 1 else 0)
                    writeOpHeader 25 wordCount target
                    target.Write(sampledType)
                    target.Write(uint32 (int _OperandDimensionality))
                    target.Write(depth)
                    target.Write(arrayed)
                    target.Write(mS)
                    target.Write(sampled)
                    target.Write(uint32 (int _OperandSamplerImageFormat))
                    match _OperandAccessQualifier with
                        | Some v -> target.Write(uint32 (int v))
                        | None -> ()
                | OpTypeSampler -> writeOpHeader 26 2 target
                | OpTypeSampledImage(imageType) -> 
                    writeOpHeader 27 3 target
                    target.Write(imageType)
                | OpTypeArray(elementType, length) -> 
                    writeOpHeader 28 4 target
                    target.Write(elementType)
                    target.Write(length)
                | OpTypeRuntimeArray(elementType) -> 
                    writeOpHeader 29 3 target
                    target.Write(elementType)
                | OpTypeStruct(args) -> 
                    let wordCount = 3 + args.Length
                    writeOpHeader 30 wordCount target
                    for v in args do
                        target.Write(v)
                | OpTypeOpaque(thenameoftheopaquetype) -> 
                    let wordCount = 3 + (thenameoftheopaquetype.Length + 4 &&& ~~~3) / 4
                    writeOpHeader 31 wordCount target
                    writeString thenameoftheopaquetype target
                | OpTypePointer(_OperandStorage, _type) -> 
                    writeOpHeader 32 4 target
                    target.Write(uint32 (int _OperandStorage))
                    target.Write(_type)
                | OpTypeFunction(returnType, args) -> 
                    let wordCount = 4 + args.Length
                    writeOpHeader 33 wordCount target
                    target.Write(returnType)
                    for v in args do
                        target.Write(v)
                | OpTypeEvent -> writeOpHeader 34 2 target
                | OpTypeDeviceEvent -> writeOpHeader 35 2 target
                | OpTypeReserveId -> writeOpHeader 36 2 target
                | OpTypeQueue -> writeOpHeader 37 2 target
                | OpTypePipe(qualifier) -> 
                    writeOpHeader 38 3 target
                    target.Write(uint32 (int qualifier))
                | OpTypeForwardPointer(pointerType, _OperandStorage) -> 
                    writeOpHeader 39 3 target
                    target.Write(pointerType)
                    target.Write(uint32 (int _OperandStorage))
                | OpConstantTrue -> writeOpHeader 41 3 target
                | OpConstantFalse -> writeOpHeader 42 3 target
                | OpConstant(value) -> 
                    let wordCount = 4 + value.Length
                    writeOpHeader 43 wordCount target
                    for v in value do
                        target.Write(v)
                | OpConstantComposite(constituents) -> 
                    let wordCount = 4 + constituents.Length
                    writeOpHeader 44 wordCount target
                    for v in constituents do
                        target.Write(v)
                | OpConstantSampler(_OperandSamplerAddressingMode, param, _OperandSamplerFilterMode) -> 
                    writeOpHeader 45 6 target
                    target.Write(uint32 (int _OperandSamplerAddressingMode))
                    target.Write(param)
                    target.Write(uint32 (int _OperandSamplerFilterMode))
                | OpConstantNull -> writeOpHeader 46 3 target
                | OpSpecConstantTrue -> writeOpHeader 48 3 target
                | OpSpecConstantFalse -> writeOpHeader 49 3 target
                | OpSpecConstant(value) -> 
                    let wordCount = 4 + value.Length
                    writeOpHeader 50 wordCount target
                    for v in value do
                        target.Write(v)
                | OpSpecConstantComposite(constituents) -> 
                    let wordCount = 4 + constituents.Length
                    writeOpHeader 51 wordCount target
                    for v in constituents do
                        target.Write(v)
                | OpSpecConstantOp(opcode, operands) -> 
                    let wordCount = 5 + operands.Length
                    writeOpHeader 52 wordCount target
                    target.Write(opcode)
                    for v in operands do
                        target.Write(v)
                | OpFunction(_OperandFunction, functionType) -> 
                    writeOpHeader 54 5 target
                    target.Write(uint32 (int _OperandFunction))
                    target.Write(functionType)
                | OpFunctionParameter -> writeOpHeader 55 3 target
                | OpFunctionEnd -> writeOpHeader 56 1 target
                | OpFunctionCall(_function, args) -> 
                    let wordCount = 5 + args.Length
                    writeOpHeader 57 wordCount target
                    target.Write(_function)
                    for v in args do
                        target.Write(v)
                | OpVariable(_OperandStorage, initializer) -> 
                    let wordCount = 5 + (if initializer.IsSome then 1 else 0)
                    writeOpHeader 59 wordCount target
                    target.Write(uint32 (int _OperandStorage))
                    match initializer with
                        | Some v -> target.Write(v)
                        | None -> ()
                | OpImageTexelPointer(image, coordinate, sample) -> 
                    writeOpHeader 60 6 target
                    target.Write(image)
                    target.Write(coordinate)
                    target.Write(sample)
                | OpLoad(pointer, _OperandMemoryAccess) -> 
                    let wordCount = 5 + (if _OperandMemoryAccess.IsSome then 1 else 0)
                    writeOpHeader 61 wordCount target
                    target.Write(pointer)
                    match _OperandMemoryAccess with
                        | Some v -> target.Write(uint32 (int v))
                        | None -> ()
                | OpStore(pointer, _object, _OperandMemoryAccess) -> 
                    let wordCount = 4 + (if _OperandMemoryAccess.IsSome then 1 else 0)
                    writeOpHeader 62 wordCount target
                    target.Write(pointer)
                    target.Write(_object)
                    match _OperandMemoryAccess with
                        | Some v -> target.Write(uint32 (int v))
                        | None -> ()
                | OpCopyMemory(_target, _source, _OperandMemoryAccess) -> 
                    let wordCount = 4 + (if _OperandMemoryAccess.IsSome then 1 else 0)
                    writeOpHeader 63 wordCount target
                    target.Write(_target)
                    target.Write(_source)
                    match _OperandMemoryAccess with
                        | Some v -> target.Write(uint32 (int v))
                        | None -> ()
                | OpCopyMemorySized(_target, _source, size, _OperandMemoryAccess) -> 
                    let wordCount = 5 + (if _OperandMemoryAccess.IsSome then 1 else 0)
                    writeOpHeader 64 wordCount target
                    target.Write(_target)
                    target.Write(_source)
                    target.Write(size)
                    match _OperandMemoryAccess with
                        | Some v -> target.Write(uint32 (int v))
                        | None -> ()
                | OpAccessChain(_base, indexes) -> 
                    let wordCount = 5 + indexes.Length
                    writeOpHeader 65 wordCount target
                    target.Write(_base)
                    for v in indexes do
                        target.Write(v)
                | OpInBoundsAccessChain(_base, indexes) -> 
                    let wordCount = 5 + indexes.Length
                    writeOpHeader 66 wordCount target
                    target.Write(_base)
                    for v in indexes do
                        target.Write(v)
                | OpPtrAccessChain(_base, element, indexes) -> 
                    let wordCount = 6 + indexes.Length
                    writeOpHeader 67 wordCount target
                    target.Write(_base)
                    target.Write(element)
                    for v in indexes do
                        target.Write(v)
                | OpArrayLength(structure, arraymember) -> 
                    writeOpHeader 68 5 target
                    target.Write(structure)
                    target.Write(arraymember)
                | OpGenericPtrMemSemantics(pointer) -> 
                    writeOpHeader 69 4 target
                    target.Write(pointer)
                | OpInBoundsPtrAccessChain(_base, element, indexes) -> 
                    let wordCount = 6 + indexes.Length
                    writeOpHeader 70 wordCount target
                    target.Write(_base)
                    target.Write(element)
                    for v in indexes do
                        target.Write(v)
                | OpDecorate(_target, _OperandDecoration, args) -> 
                    let wordCount = 4 + args.Length
                    writeOpHeader 71 wordCount target
                    target.Write(_target)
                    target.Write(uint32 (int _OperandDecoration))
                    for v in args do
                        target.Write(v)
                | OpMemberDecorate(structureType, _member, _OperandDecoration, args) -> 
                    let wordCount = 5 + args.Length
                    writeOpHeader 72 wordCount target
                    target.Write(structureType)
                    target.Write(_member)
                    target.Write(uint32 (int _OperandDecoration))
                    for v in args do
                        target.Write(v)
                | OpDecorationGroup -> writeOpHeader 73 2 target
                | OpGroupDecorate(decorationGroup, targets) -> 
                    let wordCount = 3 + targets.Length
                    writeOpHeader 74 wordCount target
                    target.Write(decorationGroup)
                    for v in targets do
                        target.Write(v)
                | OpGroupMemberDecorate(decorationGroup, targets) -> 
                    let wordCount = 3 + targets.Length
                    writeOpHeader 75 wordCount target
                    target.Write(decorationGroup)
                    for v in targets do
                        target.Write(v)
                | OpVectorExtractDynamic(vector, index) -> 
                    writeOpHeader 77 5 target
                    target.Write(vector)
                    target.Write(index)
                | OpVectorInsertDynamic(vector, _component, index) -> 
                    writeOpHeader 78 6 target
                    target.Write(vector)
                    target.Write(_component)
                    target.Write(index)
                | OpVectorShuffle(vector1, vector2, components) -> 
                    let wordCount = 6 + components.Length
                    writeOpHeader 79 wordCount target
                    target.Write(vector1)
                    target.Write(vector2)
                    for v in components do
                        target.Write(v)
                | OpCompositeConstruct(constituents) -> 
                    let wordCount = 4 + constituents.Length
                    writeOpHeader 80 wordCount target
                    for v in constituents do
                        target.Write(v)
                | OpCompositeExtract(composite, indexes) -> 
                    let wordCount = 5 + indexes.Length
                    writeOpHeader 81 wordCount target
                    target.Write(composite)
                    for v in indexes do
                        target.Write(v)
                | OpCompositeInsert(_object, composite, indexes) -> 
                    let wordCount = 6 + indexes.Length
                    writeOpHeader 82 wordCount target
                    target.Write(_object)
                    target.Write(composite)
                    for v in indexes do
                        target.Write(v)
                | OpCopyObject(operand) -> 
                    writeOpHeader 83 4 target
                    target.Write(operand)
                | OpTranspose(matrix) -> 
                    writeOpHeader 84 4 target
                    target.Write(matrix)
                | OpSampledImage(image, sampler) -> 
                    writeOpHeader 86 5 target
                    target.Write(image)
                    target.Write(sampler)
                | OpImageSampleImplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 87 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSampleExplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 88 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSampleDrefImplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 89 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(dref)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSampleDrefExplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 90 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(dref)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSampleProjImplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 91 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSampleProjExplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 92 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSampleProjDrefImplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 93 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(dref)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSampleProjDrefExplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 94 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(dref)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageFetch(image, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 95 wordCount target
                    target.Write(image)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageGather(sampledImage, coordinate, _component, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 96 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(_component)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageDrefGather(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 97 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(dref)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageRead(image, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 98 wordCount target
                    target.Write(image)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageWrite(image, coordinate, texel, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 6 + _OperandVariableIds.Length
                    writeOpHeader 99 wordCount target
                    target.Write(image)
                    target.Write(coordinate)
                    target.Write(texel)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImage(sampledImage) -> 
                    writeOpHeader 100 4 target
                    target.Write(sampledImage)
                | OpImageQueryFormat(image) -> 
                    writeOpHeader 101 4 target
                    target.Write(image)
                | OpImageQueryOrder(image) -> 
                    writeOpHeader 102 4 target
                    target.Write(image)
                | OpImageQuerySizeLod(image, levelofDetail) -> 
                    writeOpHeader 103 5 target
                    target.Write(image)
                    target.Write(levelofDetail)
                | OpImageQuerySize(image) -> 
                    writeOpHeader 104 4 target
                    target.Write(image)
                | OpImageQueryLod(image, coordinate) -> 
                    writeOpHeader 105 5 target
                    target.Write(image)
                    target.Write(coordinate)
                | OpImageQueryLevels(image) -> 
                    writeOpHeader 106 4 target
                    target.Write(image)
                | OpImageQuerySamples(image) -> 
                    writeOpHeader 107 4 target
                    target.Write(image)
                | OpConvertFToU(floatValue) -> 
                    writeOpHeader 109 4 target
                    target.Write(floatValue)
                | OpConvertFToS(floatValue) -> 
                    writeOpHeader 110 4 target
                    target.Write(floatValue)
                | OpConvertSToF(signedValue) -> 
                    writeOpHeader 111 4 target
                    target.Write(signedValue)
                | OpConvertUToF(unsignedValue) -> 
                    writeOpHeader 112 4 target
                    target.Write(unsignedValue)
                | OpUConvert(unsignedValue) -> 
                    writeOpHeader 113 4 target
                    target.Write(unsignedValue)
                | OpSConvert(signedValue) -> 
                    writeOpHeader 114 4 target
                    target.Write(signedValue)
                | OpFConvert(floatValue) -> 
                    writeOpHeader 115 4 target
                    target.Write(floatValue)
                | OpQuantizeToF16(value) -> 
                    writeOpHeader 116 4 target
                    target.Write(value)
                | OpConvertPtrToU(pointer) -> 
                    writeOpHeader 117 4 target
                    target.Write(pointer)
                | OpSatConvertSToU(signedValue) -> 
                    writeOpHeader 118 4 target
                    target.Write(signedValue)
                | OpSatConvertUToS(unsignedValue) -> 
                    writeOpHeader 119 4 target
                    target.Write(unsignedValue)
                | OpConvertUToPtr(integerValue) -> 
                    writeOpHeader 120 4 target
                    target.Write(integerValue)
                | OpPtrCastToGeneric(pointer) -> 
                    writeOpHeader 121 4 target
                    target.Write(pointer)
                | OpGenericCastToPtr(pointer) -> 
                    writeOpHeader 122 4 target
                    target.Write(pointer)
                | OpGenericCastToPtrExplicit(pointer, storage) -> 
                    writeOpHeader 123 5 target
                    target.Write(pointer)
                    target.Write(uint32 (int storage))
                | OpBitcast(operand) -> 
                    writeOpHeader 124 4 target
                    target.Write(operand)
                | OpSNegate(operand) -> 
                    writeOpHeader 126 4 target
                    target.Write(operand)
                | OpFNegate(operand) -> 
                    writeOpHeader 127 4 target
                    target.Write(operand)
                | OpIAdd(operand1, operand2) -> 
                    writeOpHeader 128 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFAdd(operand1, operand2) -> 
                    writeOpHeader 129 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpISub(operand1, operand2) -> 
                    writeOpHeader 130 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFSub(operand1, operand2) -> 
                    writeOpHeader 131 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpIMul(operand1, operand2) -> 
                    writeOpHeader 132 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFMul(operand1, operand2) -> 
                    writeOpHeader 133 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpUDiv(operand1, operand2) -> 
                    writeOpHeader 134 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpSDiv(operand1, operand2) -> 
                    writeOpHeader 135 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFDiv(operand1, operand2) -> 
                    writeOpHeader 136 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpUMod(operand1, operand2) -> 
                    writeOpHeader 137 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpSRem(operand1, operand2) -> 
                    writeOpHeader 138 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpSMod(operand1, operand2) -> 
                    writeOpHeader 139 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFRem(operand1, operand2) -> 
                    writeOpHeader 140 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFMod(operand1, operand2) -> 
                    writeOpHeader 141 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpVectorTimesScalar(vector, scalar) -> 
                    writeOpHeader 142 5 target
                    target.Write(vector)
                    target.Write(scalar)
                | OpMatrixTimesScalar(matrix, scalar) -> 
                    writeOpHeader 143 5 target
                    target.Write(matrix)
                    target.Write(scalar)
                | OpVectorTimesMatrix(vector, matrix) -> 
                    writeOpHeader 144 5 target
                    target.Write(vector)
                    target.Write(matrix)
                | OpMatrixTimesVector(matrix, vector) -> 
                    writeOpHeader 145 5 target
                    target.Write(matrix)
                    target.Write(vector)
                | OpMatrixTimesMatrix(leftMatrix, rightMatrix) -> 
                    writeOpHeader 146 5 target
                    target.Write(leftMatrix)
                    target.Write(rightMatrix)
                | OpOuterProduct(vector1, vector2) -> 
                    writeOpHeader 147 5 target
                    target.Write(vector1)
                    target.Write(vector2)
                | OpDot(vector1, vector2) -> 
                    writeOpHeader 148 5 target
                    target.Write(vector1)
                    target.Write(vector2)
                | OpIAddCarry(operand1, operand2) -> 
                    writeOpHeader 149 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpISubBorrow(operand1, operand2) -> 
                    writeOpHeader 150 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpUMulExtended(operand1, operand2) -> 
                    writeOpHeader 151 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpSMulExtended(operand1, operand2) -> 
                    writeOpHeader 152 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpAny(vector) -> 
                    writeOpHeader 154 4 target
                    target.Write(vector)
                | OpAll(vector) -> 
                    writeOpHeader 155 4 target
                    target.Write(vector)
                | OpIsNan(x) -> 
                    writeOpHeader 156 4 target
                    target.Write(x)
                | OpIsInf(x) -> 
                    writeOpHeader 157 4 target
                    target.Write(x)
                | OpIsFinite(x) -> 
                    writeOpHeader 158 4 target
                    target.Write(x)
                | OpIsNormal(x) -> 
                    writeOpHeader 159 4 target
                    target.Write(x)
                | OpSignBitSet(x) -> 
                    writeOpHeader 160 4 target
                    target.Write(x)
                | OpLessOrGreater(x, y) -> 
                    writeOpHeader 161 5 target
                    target.Write(x)
                    target.Write(y)
                | OpOrdered(x, y) -> 
                    writeOpHeader 162 5 target
                    target.Write(x)
                    target.Write(y)
                | OpUnordered(x, y) -> 
                    writeOpHeader 163 5 target
                    target.Write(x)
                    target.Write(y)
                | OpLogicalEqual(operand1, operand2) -> 
                    writeOpHeader 164 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpLogicalNotEqual(operand1, operand2) -> 
                    writeOpHeader 165 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpLogicalOr(operand1, operand2) -> 
                    writeOpHeader 166 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpLogicalAnd(operand1, operand2) -> 
                    writeOpHeader 167 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpLogicalNot(operand) -> 
                    writeOpHeader 168 4 target
                    target.Write(operand)
                | OpSelect(condition, object1, object2) -> 
                    writeOpHeader 169 6 target
                    target.Write(condition)
                    target.Write(object1)
                    target.Write(object2)
                | OpIEqual(operand1, operand2) -> 
                    writeOpHeader 170 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpINotEqual(operand1, operand2) -> 
                    writeOpHeader 171 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpUGreaterThan(operand1, operand2) -> 
                    writeOpHeader 172 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpSGreaterThan(operand1, operand2) -> 
                    writeOpHeader 173 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpUGreaterThanEqual(operand1, operand2) -> 
                    writeOpHeader 174 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpSGreaterThanEqual(operand1, operand2) -> 
                    writeOpHeader 175 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpULessThan(operand1, operand2) -> 
                    writeOpHeader 176 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpSLessThan(operand1, operand2) -> 
                    writeOpHeader 177 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpULessThanEqual(operand1, operand2) -> 
                    writeOpHeader 178 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpSLessThanEqual(operand1, operand2) -> 
                    writeOpHeader 179 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFOrdEqual(operand1, operand2) -> 
                    writeOpHeader 180 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFUnordEqual(operand1, operand2) -> 
                    writeOpHeader 181 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFOrdNotEqual(operand1, operand2) -> 
                    writeOpHeader 182 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFUnordNotEqual(operand1, operand2) -> 
                    writeOpHeader 183 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFOrdLessThan(operand1, operand2) -> 
                    writeOpHeader 184 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFUnordLessThan(operand1, operand2) -> 
                    writeOpHeader 185 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFOrdGreaterThan(operand1, operand2) -> 
                    writeOpHeader 186 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFUnordGreaterThan(operand1, operand2) -> 
                    writeOpHeader 187 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFOrdLessThanEqual(operand1, operand2) -> 
                    writeOpHeader 188 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFUnordLessThanEqual(operand1, operand2) -> 
                    writeOpHeader 189 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFOrdGreaterThanEqual(operand1, operand2) -> 
                    writeOpHeader 190 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpFUnordGreaterThanEqual(operand1, operand2) -> 
                    writeOpHeader 191 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpShiftRightLogical(_base, shift) -> 
                    writeOpHeader 194 5 target
                    target.Write(_base)
                    target.Write(shift)
                | OpShiftRightArithmetic(_base, shift) -> 
                    writeOpHeader 195 5 target
                    target.Write(_base)
                    target.Write(shift)
                | OpShiftLeftLogical(_base, shift) -> 
                    writeOpHeader 196 5 target
                    target.Write(_base)
                    target.Write(shift)
                | OpBitwiseOr(operand1, operand2) -> 
                    writeOpHeader 197 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpBitwiseXor(operand1, operand2) -> 
                    writeOpHeader 198 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpBitwiseAnd(operand1, operand2) -> 
                    writeOpHeader 199 5 target
                    target.Write(operand1)
                    target.Write(operand2)
                | OpNot(operand) -> 
                    writeOpHeader 200 4 target
                    target.Write(operand)
                | OpBitFieldInsert(_base, insert, offset, count) -> 
                    writeOpHeader 201 7 target
                    target.Write(_base)
                    target.Write(insert)
                    target.Write(offset)
                    target.Write(count)
                | OpBitFieldSExtract(_base, offset, count) -> 
                    writeOpHeader 202 6 target
                    target.Write(_base)
                    target.Write(offset)
                    target.Write(count)
                | OpBitFieldUExtract(_base, offset, count) -> 
                    writeOpHeader 203 6 target
                    target.Write(_base)
                    target.Write(offset)
                    target.Write(count)
                | OpBitReverse(_base) -> 
                    writeOpHeader 204 4 target
                    target.Write(_base)
                | OpBitCount(_base) -> 
                    writeOpHeader 205 4 target
                    target.Write(_base)
                | OpDPdx(p) -> 
                    writeOpHeader 207 4 target
                    target.Write(p)
                | OpDPdy(p) -> 
                    writeOpHeader 208 4 target
                    target.Write(p)
                | OpFwidth(p) -> 
                    writeOpHeader 209 4 target
                    target.Write(p)
                | OpDPdxFine(p) -> 
                    writeOpHeader 210 4 target
                    target.Write(p)
                | OpDPdyFine(p) -> 
                    writeOpHeader 211 4 target
                    target.Write(p)
                | OpFwidthFine(p) -> 
                    writeOpHeader 212 4 target
                    target.Write(p)
                | OpDPdxCoarse(p) -> 
                    writeOpHeader 213 4 target
                    target.Write(p)
                | OpDPdyCoarse(p) -> 
                    writeOpHeader 214 4 target
                    target.Write(p)
                | OpFwidthCoarse(p) -> 
                    writeOpHeader 215 4 target
                    target.Write(p)
                | OpEmitVertex -> writeOpHeader 218 1 target
                | OpEndPrimitive -> writeOpHeader 219 1 target
                | OpEmitStreamVertex(stream) -> 
                    writeOpHeader 220 2 target
                    target.Write(stream)
                | OpEndStreamPrimitive(stream) -> 
                    writeOpHeader 221 2 target
                    target.Write(stream)
                | OpControlBarrier(execution, memory, semantics) -> 
                    writeOpHeader 224 4 target
                    target.Write(uint32 (int execution))
                    target.Write(uint32 (int memory))
                    target.Write(uint32 (int semantics))
                | OpMemoryBarrier(memory, semantics) -> 
                    writeOpHeader 225 3 target
                    target.Write(uint32 (int memory))
                    target.Write(uint32 (int semantics))
                | OpAtomicLoad(pointer, scope, semantics) -> 
                    writeOpHeader 227 6 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                | OpAtomicStore(pointer, scope, semantics, value) -> 
                    writeOpHeader 228 5 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpAtomicExchange(pointer, scope, semantics, value) -> 
                    writeOpHeader 229 7 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpAtomicCompareExchange(pointer, scope, equal, unequal, value, comparator) -> 
                    writeOpHeader 230 9 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int equal))
                    target.Write(uint32 (int unequal))
                    target.Write(value)
                    target.Write(comparator)
                | OpAtomicCompareExchangeWeak(pointer, scope, equal, unequal, value, comparator) -> 
                    writeOpHeader 231 9 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int equal))
                    target.Write(uint32 (int unequal))
                    target.Write(value)
                    target.Write(comparator)
                | OpAtomicIIncrement(pointer, scope, semantics) -> 
                    writeOpHeader 232 6 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                | OpAtomicIDecrement(pointer, scope, semantics) -> 
                    writeOpHeader 233 6 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                | OpAtomicIAdd(pointer, scope, semantics, value) -> 
                    writeOpHeader 234 7 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpAtomicISub(pointer, scope, semantics, value) -> 
                    writeOpHeader 235 7 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpAtomicSMin(pointer, scope, semantics, value) -> 
                    writeOpHeader 236 7 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpAtomicUMin(pointer, scope, semantics, value) -> 
                    writeOpHeader 237 7 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpAtomicSMax(pointer, scope, semantics, value) -> 
                    writeOpHeader 238 7 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpAtomicUMax(pointer, scope, semantics, value) -> 
                    writeOpHeader 239 7 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpAtomicAnd(pointer, scope, semantics, value) -> 
                    writeOpHeader 240 7 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpAtomicOr(pointer, scope, semantics, value) -> 
                    writeOpHeader 241 7 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpAtomicXor(pointer, scope, semantics, value) -> 
                    writeOpHeader 242 7 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                    target.Write(value)
                | OpPhi(variableParent) -> 
                    let wordCount = 4 + variableParent.Length
                    writeOpHeader 245 wordCount target
                    for v in variableParent do
                        target.Write(v)
                | OpLoopMerge(mergeBlock, continueTarget, _OperandLoop) -> 
                    writeOpHeader 246 4 target
                    target.Write(mergeBlock)
                    target.Write(continueTarget)
                    target.Write(uint32 (int _OperandLoop))
                | OpSelectionMerge(mergeBlock, _OperandSelect) -> 
                    writeOpHeader 247 3 target
                    target.Write(mergeBlock)
                    target.Write(uint32 (int _OperandSelect))
                | OpLabel -> writeOpHeader 248 2 target
                | OpBranch(targetLabel) -> 
                    writeOpHeader 249 2 target
                    target.Write(targetLabel)
                | OpBranchConditional(condition, trueLabel, falseLabel, branchweights) -> 
                    let wordCount = 5 + branchweights.Length
                    writeOpHeader 250 wordCount target
                    target.Write(condition)
                    target.Write(trueLabel)
                    target.Write(falseLabel)
                    for v in branchweights do
                        target.Write(v)
                | OpSwitch(selector, _default, _target) -> 
                    let wordCount = 4 + _target.Length
                    writeOpHeader 251 wordCount target
                    target.Write(selector)
                    target.Write(_default)
                    for v in _target do
                        target.Write(v)
                | OpKill -> writeOpHeader 252 1 target
                | OpReturn -> writeOpHeader 253 1 target
                | OpReturnValue(value) -> 
                    writeOpHeader 254 2 target
                    target.Write(value)
                | OpUnreachable -> writeOpHeader 255 1 target
                | OpLifetimeStart(pointer, size) -> 
                    writeOpHeader 256 3 target
                    target.Write(pointer)
                    target.Write(size)
                | OpLifetimeStop(pointer, size) -> 
                    writeOpHeader 257 3 target
                    target.Write(pointer)
                    target.Write(size)
                | OpGroupAsyncCopy(execution, destination, _source, numElements, stride, event) -> 
                    writeOpHeader 259 9 target
                    target.Write(uint32 (int execution))
                    target.Write(destination)
                    target.Write(_source)
                    target.Write(numElements)
                    target.Write(stride)
                    target.Write(event)
                | OpGroupWaitEvents(execution, numEvents, eventsList) -> 
                    writeOpHeader 260 4 target
                    target.Write(uint32 (int execution))
                    target.Write(numEvents)
                    target.Write(eventsList)
                | OpGroupAll(execution, predicate) -> 
                    writeOpHeader 261 5 target
                    target.Write(uint32 (int execution))
                    target.Write(predicate)
                | OpGroupAny(execution, predicate) -> 
                    writeOpHeader 262 5 target
                    target.Write(uint32 (int execution))
                    target.Write(predicate)
                | OpGroupBroadcast(execution, value, localId) -> 
                    writeOpHeader 263 6 target
                    target.Write(uint32 (int execution))
                    target.Write(value)
                    target.Write(localId)
                | OpGroupIAdd(execution, operation, x) -> 
                    writeOpHeader 264 6 target
                    target.Write(uint32 (int execution))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupFAdd(execution, operation, x) -> 
                    writeOpHeader 265 6 target
                    target.Write(uint32 (int execution))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupFMin(execution, operation, x) -> 
                    writeOpHeader 266 6 target
                    target.Write(uint32 (int execution))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupUMin(execution, operation, x) -> 
                    writeOpHeader 267 6 target
                    target.Write(uint32 (int execution))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupSMin(execution, operation, x) -> 
                    writeOpHeader 268 6 target
                    target.Write(uint32 (int execution))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupFMax(execution, operation, x) -> 
                    writeOpHeader 269 6 target
                    target.Write(uint32 (int execution))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupUMax(execution, operation, x) -> 
                    writeOpHeader 270 6 target
                    target.Write(uint32 (int execution))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpGroupSMax(execution, operation, x) -> 
                    writeOpHeader 271 6 target
                    target.Write(uint32 (int execution))
                    target.Write(uint32 (int operation))
                    target.Write(x)
                | OpReadPipe(pipe, pointer, packetSize, packetAlignment) -> 
                    writeOpHeader 274 7 target
                    target.Write(pipe)
                    target.Write(pointer)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpWritePipe(pipe, pointer, packetSize, packetAlignment) -> 
                    writeOpHeader 275 7 target
                    target.Write(pipe)
                    target.Write(pointer)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpReservedReadPipe(pipe, reserveId, index, pointer, packetSize, packetAlignment) -> 
                    writeOpHeader 276 9 target
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(index)
                    target.Write(pointer)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpReservedWritePipe(pipe, reserveId, index, pointer, packetSize, packetAlignment) -> 
                    writeOpHeader 277 9 target
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(index)
                    target.Write(pointer)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpReserveReadPipePackets(pipe, numPackets, packetSize, packetAlignment) -> 
                    writeOpHeader 278 7 target
                    target.Write(pipe)
                    target.Write(numPackets)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpReserveWritePipePackets(pipe, numPackets, packetSize, packetAlignment) -> 
                    writeOpHeader 279 7 target
                    target.Write(pipe)
                    target.Write(numPackets)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpCommitReadPipe(pipe, reserveId, packetSize, packetAlignment) -> 
                    writeOpHeader 280 5 target
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpCommitWritePipe(pipe, reserveId, packetSize, packetAlignment) -> 
                    writeOpHeader 281 5 target
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpIsValidReserveId(reserveId) -> 
                    writeOpHeader 282 4 target
                    target.Write(reserveId)
                | OpGetNumPipePackets(pipe, packetSize, packetAlignment) -> 
                    writeOpHeader 283 6 target
                    target.Write(pipe)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpGetMaxPipePackets(pipe, packetSize, packetAlignment) -> 
                    writeOpHeader 284 6 target
                    target.Write(pipe)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpGroupReserveReadPipePackets(execution, pipe, numPackets, packetSize, packetAlignment) -> 
                    writeOpHeader 285 8 target
                    target.Write(uint32 (int execution))
                    target.Write(pipe)
                    target.Write(numPackets)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpGroupReserveWritePipePackets(execution, pipe, numPackets, packetSize, packetAlignment) -> 
                    writeOpHeader 286 8 target
                    target.Write(uint32 (int execution))
                    target.Write(pipe)
                    target.Write(numPackets)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpGroupCommitReadPipe(execution, pipe, reserveId, packetSize, packetAlignment) -> 
                    writeOpHeader 287 6 target
                    target.Write(uint32 (int execution))
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpGroupCommitWritePipe(execution, pipe, reserveId, packetSize, packetAlignment) -> 
                    writeOpHeader 288 6 target
                    target.Write(uint32 (int execution))
                    target.Write(pipe)
                    target.Write(reserveId)
                    target.Write(packetSize)
                    target.Write(packetAlignment)
                | OpEnqueueMarker(queue, numEvents, waitEvents, retEvent) -> 
                    writeOpHeader 291 7 target
                    target.Write(queue)
                    target.Write(numEvents)
                    target.Write(waitEvents)
                    target.Write(retEvent)
                | OpEnqueueKernel(queue, flags, nDRange, numEvents, waitEvents, retEvent, invoke, param, paramSize, paramAlign, localSize) -> 
                    let wordCount = 14 + localSize.Length
                    writeOpHeader 292 wordCount target
                    target.Write(queue)
                    target.Write(flags)
                    target.Write(nDRange)
                    target.Write(numEvents)
                    target.Write(waitEvents)
                    target.Write(retEvent)
                    target.Write(invoke)
                    target.Write(param)
                    target.Write(paramSize)
                    target.Write(paramAlign)
                    for v in localSize do
                        target.Write(v)
                | OpGetKernelNDrangeSubGroupCount(nDRange, invoke, param, paramSize, paramAlign) -> 
                    writeOpHeader 293 8 target
                    target.Write(nDRange)
                    target.Write(invoke)
                    target.Write(param)
                    target.Write(paramSize)
                    target.Write(paramAlign)
                | OpGetKernelNDrangeMaxSubGroupSize(nDRange, invoke, param, paramSize, paramAlign) -> 
                    writeOpHeader 294 8 target
                    target.Write(nDRange)
                    target.Write(invoke)
                    target.Write(param)
                    target.Write(paramSize)
                    target.Write(paramAlign)
                | OpGetKernelWorkGroupSize(invoke, param, paramSize, paramAlign) -> 
                    writeOpHeader 295 7 target
                    target.Write(invoke)
                    target.Write(param)
                    target.Write(paramSize)
                    target.Write(paramAlign)
                | OpGetKernelPreferredWorkGroupSizeMultiple(invoke, param, paramSize, paramAlign) -> 
                    writeOpHeader 296 7 target
                    target.Write(invoke)
                    target.Write(param)
                    target.Write(paramSize)
                    target.Write(paramAlign)
                | OpRetainEvent(event) -> 
                    writeOpHeader 297 2 target
                    target.Write(event)
                | OpReleaseEvent(event) -> 
                    writeOpHeader 298 2 target
                    target.Write(event)
                | OpCreateUserEvent -> writeOpHeader 299 3 target
                | OpIsValidEvent(event) -> 
                    writeOpHeader 300 4 target
                    target.Write(event)
                | OpSetUserEventStatus(event, status) -> 
                    writeOpHeader 301 3 target
                    target.Write(event)
                    target.Write(status)
                | OpCaptureEventProfilingInfo(event, profilingInfo, value) -> 
                    writeOpHeader 302 4 target
                    target.Write(event)
                    target.Write(profilingInfo)
                    target.Write(value)
                | OpGetDefaultQueue -> writeOpHeader 303 3 target
                | OpBuildNDRange(globalWorkSize, localWorkSize, globalWorkOffset) -> 
                    writeOpHeader 304 6 target
                    target.Write(globalWorkSize)
                    target.Write(localWorkSize)
                    target.Write(globalWorkOffset)
                | OpImageSparseSampleImplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 305 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseSampleExplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 306 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseSampleDrefImplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 307 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(dref)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseSampleDrefExplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 308 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(dref)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseSampleProjImplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 309 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseSampleProjExplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 310 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseSampleProjDrefImplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 311 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(dref)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseSampleProjDrefExplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 312 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(dref)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseFetch(image, coordinate, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 7 + _OperandVariableIds.Length
                    writeOpHeader 313 wordCount target
                    target.Write(image)
                    target.Write(coordinate)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseGather(sampledImage, coordinate, _component, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 314 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(_component)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseDrefGather(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds) -> 
                    let wordCount = 8 + _OperandVariableIds.Length
                    writeOpHeader 315 wordCount target
                    target.Write(sampledImage)
                    target.Write(coordinate)
                    target.Write(dref)
                    target.Write(uint32 (int _OperandImageOperands))
                    for v in _OperandVariableIds do
                        target.Write(v)
                | OpImageSparseTexelsResident(residentCode) -> 
                    writeOpHeader 316 4 target
                    target.Write(residentCode)
                | OpNoLine -> writeOpHeader 317 1 target
                | OpAtomicFlagTestAndSet(pointer, scope, semantics) -> 
                    writeOpHeader 318 6 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))
                | OpAtomicFlagClear(pointer, scope, semantics) -> 
                    writeOpHeader 319 4 target
                    target.Write(pointer)
                    target.Write(uint32 (int scope))
                    target.Write(uint32 (int semantics))

    let readInstructions (source : BinaryReader) = 
        seq {
            while source.BaseStream.Position < source.BaseStream.Length do
                let pos = source.BaseStream.Position
                let (code, size) = readOpHeader source
                match code with
                    | 0 ->
                        yield OpNop
                    | 1 ->
                        yield OpUndef
                    | 2 ->
                        let continuedSourceSize = max 0 (size - 1)
                        let continuedSource = readString source continuedSourceSize
                        yield OpSourceContinued(continuedSource)
                    | 3 ->
                        let _sourceSize = max 0 (size - 3)
                        let _OperandSource = unbox<SourceLanguage> (int (source.ReadUInt32()))
                        let version = source.ReadUInt32()
                        let _source = readString source _sourceSize
                        yield OpSource(_OperandSource, version, _source)
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
                        let stringSize = max 0 (size - 1)
                        let string = readString source stringSize
                        yield OpString(string)
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
                        let nameSize = max 0 (size - 1)
                        let name = readString source nameSize
                        yield OpExtInstImport(name)
                    | 12 ->
                        let argsSize = max 0 (size - 3)
                        let set = source.ReadUInt32()
                        let instruction = source.ReadUInt32()
                        let args = Array.init argsSize (fun _ -> source.ReadUInt32())
                        yield OpExtInst(set, instruction, args)
                    | 14 ->
                        let _OperandAddressing = unbox<AddressingModel> (int (source.ReadUInt32()))
                        let _OperandMemory = unbox<MemoryModel> (int (source.ReadUInt32()))
                        yield OpMemoryModel(_OperandAddressing, _OperandMemory)
                    | 15 ->
                        let nameSize = max 0 (size - 3)
                        let _OperandExecutionModel = unbox<ExecutionModel> (int (source.ReadUInt32()))
                        let entryPoint = source.ReadUInt32()
                        let name = readString source nameSize
                        yield OpEntryPoint(_OperandExecutionModel, entryPoint, name)
                    | 16 ->
                        let argsSize = max 0 (size - 3)
                        let entryPoint = source.ReadUInt32()
                        let mode = unbox<ExecutionMode> (int (source.ReadUInt32()))
                        let args = if argsSize = 0 then None else Some (source.ReadUInt32())
                        yield OpExecutionMode(entryPoint, mode, args)
                    | 17 ->
                        let capability = unbox<int> (int (source.ReadUInt32()))
                        yield OpCapability(capability)
                    | 19 ->
                        yield OpTypeVoid
                    | 20 ->
                        yield OpTypeBool
                    | 21 ->
                        let width = source.ReadUInt32()
                        let signedness = source.ReadUInt32()
                        yield OpTypeInt(width, signedness)
                    | 22 ->
                        let width = source.ReadUInt32()
                        yield OpTypeFloat(width)
                    | 23 ->
                        let componentType = source.ReadUInt32()
                        let componentCount = source.ReadUInt32()
                        yield OpTypeVector(componentType, componentCount)
                    | 24 ->
                        let columnType = source.ReadUInt32()
                        let columnCount = source.ReadUInt32()
                        yield OpTypeMatrix(columnType, columnCount)
                    | 25 ->
                        let _OperandAccessQualifierSize = max 0 (size - 8)
                        let sampledType = source.ReadUInt32()
                        let _OperandDimensionality = unbox<Dim> (int (source.ReadUInt32()))
                        let depth = source.ReadUInt32()
                        let arrayed = source.ReadUInt32()
                        let mS = source.ReadUInt32()
                        let sampled = source.ReadUInt32()
                        let _OperandSamplerImageFormat = unbox<int> (int (source.ReadUInt32()))
                        let _OperandAccessQualifier = if _OperandAccessQualifierSize = 0 then None else Some (unbox<AccessQualifier> (int (source.ReadUInt32())))
                        yield OpTypeImage(sampledType, _OperandDimensionality, depth, arrayed, mS, sampled, _OperandSamplerImageFormat, _OperandAccessQualifier)
                    | 26 ->
                        yield OpTypeSampler
                    | 27 ->
                        let imageType = source.ReadUInt32()
                        yield OpTypeSampledImage(imageType)
                    | 28 ->
                        let elementType = source.ReadUInt32()
                        let length = source.ReadUInt32()
                        yield OpTypeArray(elementType, length)
                    | 29 ->
                        let elementType = source.ReadUInt32()
                        yield OpTypeRuntimeArray(elementType)
                    | 30 ->
                        let argsSize = max 0 (size - 1)
                        let args = Array.init argsSize (fun _ -> source.ReadUInt32())
                        yield OpTypeStruct(args)
                    | 31 ->
                        let thenameoftheopaquetypeSize = max 0 (size - 1)
                        let thenameoftheopaquetype = readString source thenameoftheopaquetypeSize
                        yield OpTypeOpaque(thenameoftheopaquetype)
                    | 32 ->
                        let _OperandStorage = unbox<StorageClass> (int (source.ReadUInt32()))
                        let _type = source.ReadUInt32()
                        yield OpTypePointer(_OperandStorage, _type)
                    | 33 ->
                        let argsSize = max 0 (size - 2)
                        let returnType = source.ReadUInt32()
                        let args = Array.init argsSize (fun _ -> source.ReadUInt32())
                        yield OpTypeFunction(returnType, args)
                    | 34 ->
                        yield OpTypeEvent
                    | 35 ->
                        yield OpTypeDeviceEvent
                    | 36 ->
                        yield OpTypeReserveId
                    | 37 ->
                        yield OpTypeQueue
                    | 38 ->
                        let qualifier = unbox<AccessQualifier> (int (source.ReadUInt32()))
                        yield OpTypePipe(qualifier)
                    | 39 ->
                        let pointerType = source.ReadUInt32()
                        let _OperandStorage = unbox<StorageClass> (int (source.ReadUInt32()))
                        yield OpTypeForwardPointer(pointerType, _OperandStorage)
                    | 41 ->
                        yield OpConstantTrue
                    | 42 ->
                        yield OpConstantFalse
                    | 43 ->
                        let valueSize = max 0 (size - 1)
                        let value = Array.init valueSize (fun _ -> source.ReadUInt32())
                        yield OpConstant(value)
                    | 44 ->
                        let constituentsSize = max 0 (size - 1)
                        let constituents = Array.init constituentsSize (fun _ -> source.ReadUInt32())
                        yield OpConstantComposite(constituents)
                    | 45 ->
                        let _OperandSamplerAddressingMode = unbox<SamplerAddressingMode> (int (source.ReadUInt32()))
                        let param = source.ReadUInt32()
                        let _OperandSamplerFilterMode = unbox<SamplerFilterMode> (int (source.ReadUInt32()))
                        yield OpConstantSampler(_OperandSamplerAddressingMode, param, _OperandSamplerFilterMode)
                    | 46 ->
                        yield OpConstantNull
                    | 48 ->
                        yield OpSpecConstantTrue
                    | 49 ->
                        yield OpSpecConstantFalse
                    | 50 ->
                        let valueSize = max 0 (size - 1)
                        let value = Array.init valueSize (fun _ -> source.ReadUInt32())
                        yield OpSpecConstant(value)
                    | 51 ->
                        let constituentsSize = max 0 (size - 1)
                        let constituents = Array.init constituentsSize (fun _ -> source.ReadUInt32())
                        yield OpSpecConstantComposite(constituents)
                    | 52 ->
                        let operandsSize = max 0 (size - 2)
                        let opcode = source.ReadUInt32()
                        let operands = Array.init operandsSize (fun _ -> source.ReadUInt32())
                        yield OpSpecConstantOp(opcode, operands)
                    | 54 ->
                        let _OperandFunction = unbox<FunctionControlMask> (int (source.ReadUInt32()))
                        let functionType = source.ReadUInt32()
                        yield OpFunction(_OperandFunction, functionType)
                    | 55 ->
                        yield OpFunctionParameter
                    | 56 ->
                        yield OpFunctionEnd
                    | 57 ->
                        let argsSize = max 0 (size - 2)
                        let _function = source.ReadUInt32()
                        let args = Array.init argsSize (fun _ -> source.ReadUInt32())
                        yield OpFunctionCall(_function, args)
                    | 59 ->
                        let initializerSize = max 0 (size - 2)
                        let _OperandStorage = unbox<StorageClass> (int (source.ReadUInt32()))
                        let initializer = if initializerSize = 0 then None else Some (source.ReadUInt32())
                        yield OpVariable(_OperandStorage, initializer)
                    | 60 ->
                        let image = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let sample = source.ReadUInt32()
                        yield OpImageTexelPointer(image, coordinate, sample)
                    | 61 ->
                        let _OperandMemoryAccessSize = max 0 (size - 2)
                        let pointer = source.ReadUInt32()
                        let _OperandMemoryAccess = if _OperandMemoryAccessSize = 0 then None else Some (unbox<MemoryAccess> (int (source.ReadUInt32())))
                        yield OpLoad(pointer, _OperandMemoryAccess)
                    | 62 ->
                        let _OperandMemoryAccessSize = max 0 (size - 3)
                        let pointer = source.ReadUInt32()
                        let _object = source.ReadUInt32()
                        let _OperandMemoryAccess = if _OperandMemoryAccessSize = 0 then None else Some (unbox<MemoryAccess> (int (source.ReadUInt32())))
                        yield OpStore(pointer, _object, _OperandMemoryAccess)
                    | 63 ->
                        let _OperandMemoryAccessSize = max 0 (size - 3)
                        let _target = source.ReadUInt32()
                        let _source = source.ReadUInt32()
                        let _OperandMemoryAccess = if _OperandMemoryAccessSize = 0 then None else Some (unbox<MemoryAccess> (int (source.ReadUInt32())))
                        yield OpCopyMemory(_target, _source, _OperandMemoryAccess)
                    | 64 ->
                        let _OperandMemoryAccessSize = max 0 (size - 4)
                        let _target = source.ReadUInt32()
                        let _source = source.ReadUInt32()
                        let size = source.ReadUInt32()
                        let _OperandMemoryAccess = if _OperandMemoryAccessSize = 0 then None else Some (unbox<MemoryAccess> (int (source.ReadUInt32())))
                        yield OpCopyMemorySized(_target, _source, size, _OperandMemoryAccess)
                    | 65 ->
                        let indexesSize = max 0 (size - 2)
                        let _base = source.ReadUInt32()
                        let indexes = Array.init indexesSize (fun _ -> source.ReadUInt32())
                        yield OpAccessChain(_base, indexes)
                    | 66 ->
                        let indexesSize = max 0 (size - 2)
                        let _base = source.ReadUInt32()
                        let indexes = Array.init indexesSize (fun _ -> source.ReadUInt32())
                        yield OpInBoundsAccessChain(_base, indexes)
                    | 67 ->
                        let indexesSize = max 0 (size - 3)
                        let _base = source.ReadUInt32()
                        let element = source.ReadUInt32()
                        let indexes = Array.init indexesSize (fun _ -> source.ReadUInt32())
                        yield OpPtrAccessChain(_base, element, indexes)
                    | 68 ->
                        let structure = source.ReadUInt32()
                        let arraymember = source.ReadUInt32()
                        yield OpArrayLength(structure, arraymember)
                    | 69 ->
                        let pointer = source.ReadUInt32()
                        yield OpGenericPtrMemSemantics(pointer)
                    | 70 ->
                        let indexesSize = max 0 (size - 3)
                        let _base = source.ReadUInt32()
                        let element = source.ReadUInt32()
                        let indexes = Array.init indexesSize (fun _ -> source.ReadUInt32())
                        yield OpInBoundsPtrAccessChain(_base, element, indexes)
                    | 71 ->
                        let argsSize = max 0 (size - 3)
                        let _target = source.ReadUInt32()
                        let _OperandDecoration = unbox<Decoration> (int (source.ReadUInt32()))
                        let args = Array.init argsSize (fun _ -> source.ReadUInt32())
                        yield OpDecorate(_target, _OperandDecoration, args)
                    | 72 ->
                        let argsSize = max 0 (size - 4)
                        let structureType = source.ReadUInt32()
                        let _member = source.ReadUInt32()
                        let _OperandDecoration = unbox<Decoration> (int (source.ReadUInt32()))
                        let args = Array.init argsSize (fun _ -> source.ReadUInt32())
                        yield OpMemberDecorate(structureType, _member, _OperandDecoration, args)
                    | 73 ->
                        yield OpDecorationGroup
                    | 74 ->
                        let targetsSize = max 0 (size - 2)
                        let decorationGroup = source.ReadUInt32()
                        let targets = Array.init targetsSize (fun _ -> source.ReadUInt32())
                        yield OpGroupDecorate(decorationGroup, targets)
                    | 75 ->
                        let targetsSize = max 0 (size - 2)
                        let decorationGroup = source.ReadUInt32()
                        let targets = Array.init targetsSize (fun _ -> source.ReadUInt32())
                        yield OpGroupMemberDecorate(decorationGroup, targets)
                    | 77 ->
                        let vector = source.ReadUInt32()
                        let index = source.ReadUInt32()
                        yield OpVectorExtractDynamic(vector, index)
                    | 78 ->
                        let vector = source.ReadUInt32()
                        let _component = source.ReadUInt32()
                        let index = source.ReadUInt32()
                        yield OpVectorInsertDynamic(vector, _component, index)
                    | 79 ->
                        let componentsSize = max 0 (size - 3)
                        let vector1 = source.ReadUInt32()
                        let vector2 = source.ReadUInt32()
                        let components = Array.init componentsSize (fun _ -> source.ReadUInt32())
                        yield OpVectorShuffle(vector1, vector2, components)
                    | 80 ->
                        let constituentsSize = max 0 (size - 1)
                        let constituents = Array.init constituentsSize (fun _ -> source.ReadUInt32())
                        yield OpCompositeConstruct(constituents)
                    | 81 ->
                        let indexesSize = max 0 (size - 2)
                        let composite = source.ReadUInt32()
                        let indexes = Array.init indexesSize (fun _ -> source.ReadUInt32())
                        yield OpCompositeExtract(composite, indexes)
                    | 82 ->
                        let indexesSize = max 0 (size - 3)
                        let _object = source.ReadUInt32()
                        let composite = source.ReadUInt32()
                        let indexes = Array.init indexesSize (fun _ -> source.ReadUInt32())
                        yield OpCompositeInsert(_object, composite, indexes)
                    | 83 ->
                        let operand = source.ReadUInt32()
                        yield OpCopyObject(operand)
                    | 84 ->
                        let matrix = source.ReadUInt32()
                        yield OpTranspose(matrix)
                    | 86 ->
                        let image = source.ReadUInt32()
                        let sampler = source.ReadUInt32()
                        yield OpSampledImage(image, sampler)
                    | 87 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleImplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 88 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleExplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 89 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let dref = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleDrefImplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds)
                    | 90 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let dref = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleDrefExplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds)
                    | 91 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleProjImplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 92 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleProjExplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 93 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let dref = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleProjDrefImplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds)
                    | 94 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let dref = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSampleProjDrefExplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds)
                    | 95 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let image = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageFetch(image, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 96 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _component = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageGather(sampledImage, coordinate, _component, _OperandImageOperands, _OperandVariableIds)
                    | 97 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let dref = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageDrefGather(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds)
                    | 98 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let image = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageRead(image, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 99 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let image = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let texel = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageWrite(image, coordinate, texel, _OperandImageOperands, _OperandVariableIds)
                    | 100 ->
                        let sampledImage = source.ReadUInt32()
                        yield OpImage(sampledImage)
                    | 101 ->
                        let image = source.ReadUInt32()
                        yield OpImageQueryFormat(image)
                    | 102 ->
                        let image = source.ReadUInt32()
                        yield OpImageQueryOrder(image)
                    | 103 ->
                        let image = source.ReadUInt32()
                        let levelofDetail = source.ReadUInt32()
                        yield OpImageQuerySizeLod(image, levelofDetail)
                    | 104 ->
                        let image = source.ReadUInt32()
                        yield OpImageQuerySize(image)
                    | 105 ->
                        let image = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        yield OpImageQueryLod(image, coordinate)
                    | 106 ->
                        let image = source.ReadUInt32()
                        yield OpImageQueryLevels(image)
                    | 107 ->
                        let image = source.ReadUInt32()
                        yield OpImageQuerySamples(image)
                    | 109 ->
                        let floatValue = source.ReadUInt32()
                        yield OpConvertFToU(floatValue)
                    | 110 ->
                        let floatValue = source.ReadUInt32()
                        yield OpConvertFToS(floatValue)
                    | 111 ->
                        let signedValue = source.ReadUInt32()
                        yield OpConvertSToF(signedValue)
                    | 112 ->
                        let unsignedValue = source.ReadUInt32()
                        yield OpConvertUToF(unsignedValue)
                    | 113 ->
                        let unsignedValue = source.ReadUInt32()
                        yield OpUConvert(unsignedValue)
                    | 114 ->
                        let signedValue = source.ReadUInt32()
                        yield OpSConvert(signedValue)
                    | 115 ->
                        let floatValue = source.ReadUInt32()
                        yield OpFConvert(floatValue)
                    | 116 ->
                        let value = source.ReadUInt32()
                        yield OpQuantizeToF16(value)
                    | 117 ->
                        let pointer = source.ReadUInt32()
                        yield OpConvertPtrToU(pointer)
                    | 118 ->
                        let signedValue = source.ReadUInt32()
                        yield OpSatConvertSToU(signedValue)
                    | 119 ->
                        let unsignedValue = source.ReadUInt32()
                        yield OpSatConvertUToS(unsignedValue)
                    | 120 ->
                        let integerValue = source.ReadUInt32()
                        yield OpConvertUToPtr(integerValue)
                    | 121 ->
                        let pointer = source.ReadUInt32()
                        yield OpPtrCastToGeneric(pointer)
                    | 122 ->
                        let pointer = source.ReadUInt32()
                        yield OpGenericCastToPtr(pointer)
                    | 123 ->
                        let pointer = source.ReadUInt32()
                        let storage = unbox<StorageClass> (int (source.ReadUInt32()))
                        yield OpGenericCastToPtrExplicit(pointer, storage)
                    | 124 ->
                        let operand = source.ReadUInt32()
                        yield OpBitcast(operand)
                    | 126 ->
                        let operand = source.ReadUInt32()
                        yield OpSNegate(operand)
                    | 127 ->
                        let operand = source.ReadUInt32()
                        yield OpFNegate(operand)
                    | 128 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpIAdd(operand1, operand2)
                    | 129 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFAdd(operand1, operand2)
                    | 130 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpISub(operand1, operand2)
                    | 131 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFSub(operand1, operand2)
                    | 132 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpIMul(operand1, operand2)
                    | 133 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFMul(operand1, operand2)
                    | 134 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpUDiv(operand1, operand2)
                    | 135 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpSDiv(operand1, operand2)
                    | 136 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFDiv(operand1, operand2)
                    | 137 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpUMod(operand1, operand2)
                    | 138 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpSRem(operand1, operand2)
                    | 139 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpSMod(operand1, operand2)
                    | 140 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFRem(operand1, operand2)
                    | 141 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFMod(operand1, operand2)
                    | 142 ->
                        let vector = source.ReadUInt32()
                        let scalar = source.ReadUInt32()
                        yield OpVectorTimesScalar(vector, scalar)
                    | 143 ->
                        let matrix = source.ReadUInt32()
                        let scalar = source.ReadUInt32()
                        yield OpMatrixTimesScalar(matrix, scalar)
                    | 144 ->
                        let vector = source.ReadUInt32()
                        let matrix = source.ReadUInt32()
                        yield OpVectorTimesMatrix(vector, matrix)
                    | 145 ->
                        let matrix = source.ReadUInt32()
                        let vector = source.ReadUInt32()
                        yield OpMatrixTimesVector(matrix, vector)
                    | 146 ->
                        let leftMatrix = source.ReadUInt32()
                        let rightMatrix = source.ReadUInt32()
                        yield OpMatrixTimesMatrix(leftMatrix, rightMatrix)
                    | 147 ->
                        let vector1 = source.ReadUInt32()
                        let vector2 = source.ReadUInt32()
                        yield OpOuterProduct(vector1, vector2)
                    | 148 ->
                        let vector1 = source.ReadUInt32()
                        let vector2 = source.ReadUInt32()
                        yield OpDot(vector1, vector2)
                    | 149 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpIAddCarry(operand1, operand2)
                    | 150 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpISubBorrow(operand1, operand2)
                    | 151 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpUMulExtended(operand1, operand2)
                    | 152 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpSMulExtended(operand1, operand2)
                    | 154 ->
                        let vector = source.ReadUInt32()
                        yield OpAny(vector)
                    | 155 ->
                        let vector = source.ReadUInt32()
                        yield OpAll(vector)
                    | 156 ->
                        let x = source.ReadUInt32()
                        yield OpIsNan(x)
                    | 157 ->
                        let x = source.ReadUInt32()
                        yield OpIsInf(x)
                    | 158 ->
                        let x = source.ReadUInt32()
                        yield OpIsFinite(x)
                    | 159 ->
                        let x = source.ReadUInt32()
                        yield OpIsNormal(x)
                    | 160 ->
                        let x = source.ReadUInt32()
                        yield OpSignBitSet(x)
                    | 161 ->
                        let x = source.ReadUInt32()
                        let y = source.ReadUInt32()
                        yield OpLessOrGreater(x, y)
                    | 162 ->
                        let x = source.ReadUInt32()
                        let y = source.ReadUInt32()
                        yield OpOrdered(x, y)
                    | 163 ->
                        let x = source.ReadUInt32()
                        let y = source.ReadUInt32()
                        yield OpUnordered(x, y)
                    | 164 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpLogicalEqual(operand1, operand2)
                    | 165 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpLogicalNotEqual(operand1, operand2)
                    | 166 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpLogicalOr(operand1, operand2)
                    | 167 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpLogicalAnd(operand1, operand2)
                    | 168 ->
                        let operand = source.ReadUInt32()
                        yield OpLogicalNot(operand)
                    | 169 ->
                        let condition = source.ReadUInt32()
                        let object1 = source.ReadUInt32()
                        let object2 = source.ReadUInt32()
                        yield OpSelect(condition, object1, object2)
                    | 170 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpIEqual(operand1, operand2)
                    | 171 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpINotEqual(operand1, operand2)
                    | 172 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpUGreaterThan(operand1, operand2)
                    | 173 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpSGreaterThan(operand1, operand2)
                    | 174 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpUGreaterThanEqual(operand1, operand2)
                    | 175 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpSGreaterThanEqual(operand1, operand2)
                    | 176 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpULessThan(operand1, operand2)
                    | 177 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpSLessThan(operand1, operand2)
                    | 178 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpULessThanEqual(operand1, operand2)
                    | 179 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpSLessThanEqual(operand1, operand2)
                    | 180 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFOrdEqual(operand1, operand2)
                    | 181 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFUnordEqual(operand1, operand2)
                    | 182 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFOrdNotEqual(operand1, operand2)
                    | 183 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFUnordNotEqual(operand1, operand2)
                    | 184 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFOrdLessThan(operand1, operand2)
                    | 185 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFUnordLessThan(operand1, operand2)
                    | 186 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFOrdGreaterThan(operand1, operand2)
                    | 187 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFUnordGreaterThan(operand1, operand2)
                    | 188 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFOrdLessThanEqual(operand1, operand2)
                    | 189 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFUnordLessThanEqual(operand1, operand2)
                    | 190 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFOrdGreaterThanEqual(operand1, operand2)
                    | 191 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpFUnordGreaterThanEqual(operand1, operand2)
                    | 194 ->
                        let _base = source.ReadUInt32()
                        let shift = source.ReadUInt32()
                        yield OpShiftRightLogical(_base, shift)
                    | 195 ->
                        let _base = source.ReadUInt32()
                        let shift = source.ReadUInt32()
                        yield OpShiftRightArithmetic(_base, shift)
                    | 196 ->
                        let _base = source.ReadUInt32()
                        let shift = source.ReadUInt32()
                        yield OpShiftLeftLogical(_base, shift)
                    | 197 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpBitwiseOr(operand1, operand2)
                    | 198 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpBitwiseXor(operand1, operand2)
                    | 199 ->
                        let operand1 = source.ReadUInt32()
                        let operand2 = source.ReadUInt32()
                        yield OpBitwiseAnd(operand1, operand2)
                    | 200 ->
                        let operand = source.ReadUInt32()
                        yield OpNot(operand)
                    | 201 ->
                        let _base = source.ReadUInt32()
                        let insert = source.ReadUInt32()
                        let offset = source.ReadUInt32()
                        let count = source.ReadUInt32()
                        yield OpBitFieldInsert(_base, insert, offset, count)
                    | 202 ->
                        let _base = source.ReadUInt32()
                        let offset = source.ReadUInt32()
                        let count = source.ReadUInt32()
                        yield OpBitFieldSExtract(_base, offset, count)
                    | 203 ->
                        let _base = source.ReadUInt32()
                        let offset = source.ReadUInt32()
                        let count = source.ReadUInt32()
                        yield OpBitFieldUExtract(_base, offset, count)
                    | 204 ->
                        let _base = source.ReadUInt32()
                        yield OpBitReverse(_base)
                    | 205 ->
                        let _base = source.ReadUInt32()
                        yield OpBitCount(_base)
                    | 207 ->
                        let p = source.ReadUInt32()
                        yield OpDPdx(p)
                    | 208 ->
                        let p = source.ReadUInt32()
                        yield OpDPdy(p)
                    | 209 ->
                        let p = source.ReadUInt32()
                        yield OpFwidth(p)
                    | 210 ->
                        let p = source.ReadUInt32()
                        yield OpDPdxFine(p)
                    | 211 ->
                        let p = source.ReadUInt32()
                        yield OpDPdyFine(p)
                    | 212 ->
                        let p = source.ReadUInt32()
                        yield OpFwidthFine(p)
                    | 213 ->
                        let p = source.ReadUInt32()
                        yield OpDPdxCoarse(p)
                    | 214 ->
                        let p = source.ReadUInt32()
                        yield OpDPdyCoarse(p)
                    | 215 ->
                        let p = source.ReadUInt32()
                        yield OpFwidthCoarse(p)
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
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let memory = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpControlBarrier(execution, memory, semantics)
                    | 225 ->
                        let memory = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpMemoryBarrier(memory, semantics)
                    | 227 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicLoad(pointer, scope, semantics)
                    | 228 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicStore(pointer, scope, semantics, value)
                    | 229 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicExchange(pointer, scope, semantics, value)
                    | 230 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let equal = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let unequal = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        let comparator = source.ReadUInt32()
                        yield OpAtomicCompareExchange(pointer, scope, equal, unequal, value, comparator)
                    | 231 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let equal = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let unequal = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        let comparator = source.ReadUInt32()
                        yield OpAtomicCompareExchangeWeak(pointer, scope, equal, unequal, value, comparator)
                    | 232 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicIIncrement(pointer, scope, semantics)
                    | 233 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicIDecrement(pointer, scope, semantics)
                    | 234 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicIAdd(pointer, scope, semantics, value)
                    | 235 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicISub(pointer, scope, semantics, value)
                    | 236 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicSMin(pointer, scope, semantics, value)
                    | 237 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicUMin(pointer, scope, semantics, value)
                    | 238 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicSMax(pointer, scope, semantics, value)
                    | 239 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicUMax(pointer, scope, semantics, value)
                    | 240 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicAnd(pointer, scope, semantics, value)
                    | 241 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicOr(pointer, scope, semantics, value)
                    | 242 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        yield OpAtomicXor(pointer, scope, semantics, value)
                    | 245 ->
                        let variableParentSize = max 0 (size - 1)
                        let variableParent = Array.init variableParentSize (fun _ -> source.ReadUInt32())
                        yield OpPhi(variableParent)
                    | 246 ->
                        let mergeBlock = source.ReadUInt32()
                        let continueTarget = source.ReadUInt32()
                        let _OperandLoop = unbox<LoopControl> (int (source.ReadUInt32()))
                        yield OpLoopMerge(mergeBlock, continueTarget, _OperandLoop)
                    | 247 ->
                        let mergeBlock = source.ReadUInt32()
                        let _OperandSelect = unbox<SelectionControl> (int (source.ReadUInt32()))
                        yield OpSelectionMerge(mergeBlock, _OperandSelect)
                    | 248 ->
                        yield OpLabel
                    | 249 ->
                        let targetLabel = source.ReadUInt32()
                        yield OpBranch(targetLabel)
                    | 250 ->
                        let branchweightsSize = max 0 (size - 4)
                        let condition = source.ReadUInt32()
                        let trueLabel = source.ReadUInt32()
                        let falseLabel = source.ReadUInt32()
                        let branchweights = Array.init branchweightsSize (fun _ -> source.ReadUInt32())
                        yield OpBranchConditional(condition, trueLabel, falseLabel, branchweights)
                    | 251 ->
                        let _targetSize = max 0 (size - 3)
                        let selector = source.ReadUInt32()
                        let _default = source.ReadUInt32()
                        let _target = Array.init _targetSize (fun _ -> source.ReadUInt32())
                        yield OpSwitch(selector, _default, _target)
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
                        let pointer = source.ReadUInt32()
                        let size = source.ReadUInt32()
                        yield OpLifetimeStart(pointer, size)
                    | 257 ->
                        let pointer = source.ReadUInt32()
                        let size = source.ReadUInt32()
                        yield OpLifetimeStop(pointer, size)
                    | 259 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let destination = source.ReadUInt32()
                        let _source = source.ReadUInt32()
                        let numElements = source.ReadUInt32()
                        let stride = source.ReadUInt32()
                        let event = source.ReadUInt32()
                        yield OpGroupAsyncCopy(execution, destination, _source, numElements, stride, event)
                    | 260 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let numEvents = source.ReadUInt32()
                        let eventsList = source.ReadUInt32()
                        yield OpGroupWaitEvents(execution, numEvents, eventsList)
                    | 261 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let predicate = source.ReadUInt32()
                        yield OpGroupAll(execution, predicate)
                    | 262 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let predicate = source.ReadUInt32()
                        yield OpGroupAny(execution, predicate)
                    | 263 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let value = source.ReadUInt32()
                        let localId = source.ReadUInt32()
                        yield OpGroupBroadcast(execution, value, localId)
                    | 264 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupIAdd(execution, operation, x)
                    | 265 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupFAdd(execution, operation, x)
                    | 266 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupFMin(execution, operation, x)
                    | 267 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupUMin(execution, operation, x)
                    | 268 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupSMin(execution, operation, x)
                    | 269 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupFMax(execution, operation, x)
                    | 270 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupUMax(execution, operation, x)
                    | 271 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let operation = unbox<GroupOperation> (int (source.ReadUInt32()))
                        let x = source.ReadUInt32()
                        yield OpGroupSMax(execution, operation, x)
                    | 274 ->
                        let pipe = source.ReadUInt32()
                        let pointer = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpReadPipe(pipe, pointer, packetSize, packetAlignment)
                    | 275 ->
                        let pipe = source.ReadUInt32()
                        let pointer = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpWritePipe(pipe, pointer, packetSize, packetAlignment)
                    | 276 ->
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let index = source.ReadUInt32()
                        let pointer = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpReservedReadPipe(pipe, reserveId, index, pointer, packetSize, packetAlignment)
                    | 277 ->
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let index = source.ReadUInt32()
                        let pointer = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpReservedWritePipe(pipe, reserveId, index, pointer, packetSize, packetAlignment)
                    | 278 ->
                        let pipe = source.ReadUInt32()
                        let numPackets = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpReserveReadPipePackets(pipe, numPackets, packetSize, packetAlignment)
                    | 279 ->
                        let pipe = source.ReadUInt32()
                        let numPackets = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpReserveWritePipePackets(pipe, numPackets, packetSize, packetAlignment)
                    | 280 ->
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpCommitReadPipe(pipe, reserveId, packetSize, packetAlignment)
                    | 281 ->
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpCommitWritePipe(pipe, reserveId, packetSize, packetAlignment)
                    | 282 ->
                        let reserveId = source.ReadUInt32()
                        yield OpIsValidReserveId(reserveId)
                    | 283 ->
                        let pipe = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpGetNumPipePackets(pipe, packetSize, packetAlignment)
                    | 284 ->
                        let pipe = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpGetMaxPipePackets(pipe, packetSize, packetAlignment)
                    | 285 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let numPackets = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpGroupReserveReadPipePackets(execution, pipe, numPackets, packetSize, packetAlignment)
                    | 286 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let numPackets = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpGroupReserveWritePipePackets(execution, pipe, numPackets, packetSize, packetAlignment)
                    | 287 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpGroupCommitReadPipe(execution, pipe, reserveId, packetSize, packetAlignment)
                    | 288 ->
                        let execution = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let pipe = source.ReadUInt32()
                        let reserveId = source.ReadUInt32()
                        let packetSize = source.ReadUInt32()
                        let packetAlignment = source.ReadUInt32()
                        yield OpGroupCommitWritePipe(execution, pipe, reserveId, packetSize, packetAlignment)
                    | 291 ->
                        let queue = source.ReadUInt32()
                        let numEvents = source.ReadUInt32()
                        let waitEvents = source.ReadUInt32()
                        let retEvent = source.ReadUInt32()
                        yield OpEnqueueMarker(queue, numEvents, waitEvents, retEvent)
                    | 292 ->
                        let localSizeSize = max 0 (size - 11)
                        let queue = source.ReadUInt32()
                        let flags = source.ReadUInt32()
                        let nDRange = source.ReadUInt32()
                        let numEvents = source.ReadUInt32()
                        let waitEvents = source.ReadUInt32()
                        let retEvent = source.ReadUInt32()
                        let invoke = source.ReadUInt32()
                        let param = source.ReadUInt32()
                        let paramSize = source.ReadUInt32()
                        let paramAlign = source.ReadUInt32()
                        let localSize = Array.init localSizeSize (fun _ -> source.ReadUInt32())
                        yield OpEnqueueKernel(queue, flags, nDRange, numEvents, waitEvents, retEvent, invoke, param, paramSize, paramAlign, localSize)
                    | 293 ->
                        let nDRange = source.ReadUInt32()
                        let invoke = source.ReadUInt32()
                        let param = source.ReadUInt32()
                        let paramSize = source.ReadUInt32()
                        let paramAlign = source.ReadUInt32()
                        yield OpGetKernelNDrangeSubGroupCount(nDRange, invoke, param, paramSize, paramAlign)
                    | 294 ->
                        let nDRange = source.ReadUInt32()
                        let invoke = source.ReadUInt32()
                        let param = source.ReadUInt32()
                        let paramSize = source.ReadUInt32()
                        let paramAlign = source.ReadUInt32()
                        yield OpGetKernelNDrangeMaxSubGroupSize(nDRange, invoke, param, paramSize, paramAlign)
                    | 295 ->
                        let invoke = source.ReadUInt32()
                        let param = source.ReadUInt32()
                        let paramSize = source.ReadUInt32()
                        let paramAlign = source.ReadUInt32()
                        yield OpGetKernelWorkGroupSize(invoke, param, paramSize, paramAlign)
                    | 296 ->
                        let invoke = source.ReadUInt32()
                        let param = source.ReadUInt32()
                        let paramSize = source.ReadUInt32()
                        let paramAlign = source.ReadUInt32()
                        yield OpGetKernelPreferredWorkGroupSizeMultiple(invoke, param, paramSize, paramAlign)
                    | 297 ->
                        let event = source.ReadUInt32()
                        yield OpRetainEvent(event)
                    | 298 ->
                        let event = source.ReadUInt32()
                        yield OpReleaseEvent(event)
                    | 299 ->
                        yield OpCreateUserEvent
                    | 300 ->
                        let event = source.ReadUInt32()
                        yield OpIsValidEvent(event)
                    | 301 ->
                        let event = source.ReadUInt32()
                        let status = source.ReadUInt32()
                        yield OpSetUserEventStatus(event, status)
                    | 302 ->
                        let event = source.ReadUInt32()
                        let profilingInfo = source.ReadUInt32()
                        let value = source.ReadUInt32()
                        yield OpCaptureEventProfilingInfo(event, profilingInfo, value)
                    | 303 ->
                        yield OpGetDefaultQueue
                    | 304 ->
                        let globalWorkSize = source.ReadUInt32()
                        let localWorkSize = source.ReadUInt32()
                        let globalWorkOffset = source.ReadUInt32()
                        yield OpBuildNDRange(globalWorkSize, localWorkSize, globalWorkOffset)
                    | 305 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleImplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 306 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleExplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 307 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let dref = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleDrefImplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds)
                    | 308 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let dref = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleDrefExplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds)
                    | 309 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleProjImplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 310 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleProjExplicitLod(sampledImage, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 311 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let dref = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleProjDrefImplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds)
                    | 312 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let dref = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseSampleProjDrefExplicitLod(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds)
                    | 313 ->
                        let _OperandVariableIdsSize = max 0 (size - 4)
                        let image = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseFetch(image, coordinate, _OperandImageOperands, _OperandVariableIds)
                    | 314 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let _component = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseGather(sampledImage, coordinate, _component, _OperandImageOperands, _OperandVariableIds)
                    | 315 ->
                        let _OperandVariableIdsSize = max 0 (size - 5)
                        let sampledImage = source.ReadUInt32()
                        let coordinate = source.ReadUInt32()
                        let dref = source.ReadUInt32()
                        let _OperandImageOperands = unbox<int> (int (source.ReadUInt32()))
                        let _OperandVariableIds = Array.init _OperandVariableIdsSize (fun _ -> source.ReadUInt32())
                        yield OpImageSparseDrefGather(sampledImage, coordinate, dref, _OperandImageOperands, _OperandVariableIds)
                    | 316 ->
                        let residentCode = source.ReadUInt32()
                        yield OpImageSparseTexelsResident(residentCode)
                    | 317 ->
                        yield OpNoLine
                    | 318 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicFlagTestAndSet(pointer, scope, semantics)
                    | 319 ->
                        let pointer = source.ReadUInt32()
                        let scope = unbox<ExecutionScope> (int (source.ReadUInt32()))
                        let semantics = unbox<MemorySemantics> (int (source.ReadUInt32()))
                        yield OpAtomicFlagClear(pointer, scope, semantics)
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
        { magic = magic; version = version; generatorMagic = generatorMagic; bound = bound; reserved = reserved; instructions = instructions }
