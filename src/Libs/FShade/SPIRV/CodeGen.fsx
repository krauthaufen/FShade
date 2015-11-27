#I @"..\..\..\..\Packages\Newtonsoft.Json\lib\net45"
#r "Newtonsoft.Json.dll"
#r "System.Xml.dll"
#r "System.Xml.Linq.dll"

open System
open System.IO
open System.Text.RegularExpressions
open Newtonsoft
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Serialization

let file = Path.Combine(__SOURCE_DIRECTORY__, "spirv.json")
let outputFile = Path.Combine(__SOURCE_DIRECTORY__, "SpirV.fs")

[<AutoOpen>]
module Types = 
    type OpcodeClass =
        | OpClassMisc = 0
        | OpClassDebug = 1
        | OpClassAnnotate = 2
        | OpClassExtension = 3
        | OpClassMode = 4
        | OpClassType = 5
        | OpClassConstant = 6
        | OpClassMemory = 7
        | OpClassFunction = 8
        | OpClassImage = 9
        | OpClassConvert = 10
        | OpClassComposite = 11
        | OpClassArithmetic = 12
        | OpClassBit = 13
        | OpClassRelationalLogical = 14
        | OpClassDerivative = 15
        | OpClassFlowControl = 16
        | OpClassAtomic = 17
        | OpClassPrimitive = 18
        | OpClassBarrier = 19
        | OpClassGroup = 20
        | OpClassDeviceSideEnqueue = 21
        | OpClassPipe = 22
        | OpClassCount = 23
        | OpClassMissing = 24

    type OperandClass =
        | OperandNone = 0
        | OperandId = 1
        | OperandVariableIds = 2
        | OperandOptionalLiteral = 3
        | OperandOptionalLiteralString = 4
        | OperandVariableLiterals = 5
        | OperandVariableIdLiteral = 6
        | OperandVariableLiteralId = 7
        | OperandLiteralNumber = 8
        | OperandLiteralString = 9
        | OperandSource = 10
        | OperandExecutionModel = 11
        | OperandAddressing = 12
        | OperandMemory = 13
        | OperandExecutionMode = 14
        | OperandStorage = 15
        | OperandDimensionality = 16
        | OperandSamplerAddressingMode = 17
        | OperandSamplerFilterMode = 18
        | OperandSamplerImageFormat = 19
        | OperandImageChannelOrder = 20
        | OperandImageChannelDataType = 21
        | OperandImageOperands = 22
        | OperandFPFastMath = 23
        | OperandFPRoundingMode = 24
        | OperandLinkageType = 25
        | OperandAccessQualifier = 26
        | OperandFuncParamAttr = 27
        | OperandDecoration = 28
        | OperandBuiltIn = 29
        | OperandSelect = 30
        | OperandLoop = 31
        | OperandFunction = 32
        | OperandMemorySemantics = 33
        | OperandMemoryAccess = 34
        | OperandScope = 35
        | OperandGroupOperation = 36
        | OperandKernelEnqueueFlags = 37
        | OperandKernelProfilingInfo = 38
        | OperandCapability = 39
        | OperandOpcode = 40
        | OperandCount = 41

    [<AutoOpen>]
    module EnumExtensions = 

        let private arrayTypes =
            Set.ofList [
                OperandClass.OperandVariableIds 
                OperandClass.OperandImageOperands 
                OperandClass.OperandVariableIdLiteral
                OperandClass.OperandVariableLiteralId
            ]

        let private enumTypes =
            Set.ofList [
                OperandClass.OperandVariableLiterals
                OperandClass.OperandSource
                OperandClass.OperandExecutionModel
                OperandClass.OperandAddressing
                OperandClass.OperandMemory
                OperandClass.OperandExecutionMode
                OperandClass.OperandStorage
                OperandClass.OperandDimensionality
                OperandClass.OperandSamplerAddressingMode
                OperandClass.OperandSamplerFilterMode
                OperandClass.OperandSamplerImageFormat
                OperandClass.OperandImageChannelOrder
                OperandClass.OperandImageChannelDataType
                OperandClass.OperandImageOperands
                OperandClass.OperandFPFastMath
                OperandClass.OperandFPRoundingMode
                OperandClass.OperandLinkageType
                OperandClass.OperandAccessQualifier
                OperandClass.OperandFuncParamAttr
                OperandClass.OperandDecoration
                OperandClass.OperandBuiltIn
                OperandClass.OperandSelect
                OperandClass.OperandLoop
                OperandClass.OperandFunction
                OperandClass.OperandMemorySemantics
                OperandClass.OperandMemoryAccess
                OperandClass.OperandScope
                OperandClass.OperandGroupOperation
                OperandClass.OperandKernelEnqueueFlags
                OperandClass.OperandKernelProfilingInfo
                OperandClass.OperandCapability
                OperandClass.OperandOpcode
            ]

        let private stringTypes =
            Set.ofList [
                OperandClass.OperandOptionalLiteralString
                OperandClass.OperandLiteralString
            ]

        type OperandClass with
            member x.IsArray = Set.contains x arrayTypes

            member x.IsString = Set.contains x stringTypes //x.ToString().Contains "String"

            member x.IsEnum = Set.contains x enumTypes

    type Operand =
        {
            Name : string
            Type : OperandClass
            Optional : bool
        }

    type Operation =
        {
            Name : string
            OpCode : int
            Category : OpcodeClass

            MinWordCount : int
            HasVariableWordCount : bool
            MinVariableUsedCount : int

            HasResultType : bool
            HasResult : bool
            Operands : list<Operand>
        }


    type EnumValue =
        {
            Value : int
            Name : string
            Comment : string
            CommentPlain : string
            Capabilities : list<string>
            ExtraOperands : list<string>
        }

    type Enum =
        {
            Name : string
            Values : Map<string, EnumValue>
            Comment : string
            CommentPlain : string
        }


    type SpirVSpec =
        {
            OpCodes : list<Operation>
            Enums : list<Enum>
        }

[<AutoOpen>]
module Reader =


    let private fixedCodes =
        Map.ofList [
            "OpNop", 0
            "OpUndef", 1
            "OpSourceContinued", 2
            "OpSource", 3
            "OpSourceExtension", 4
            "OpName", 5
            "OpMemberName", 6
            "OpString", 7
            "OpLine", 8
            "OpExtension", 10
            "OpExtInstImport", 11
            "OpExtInst", 12
            "OpMemoryModel", 14
            "OpEntryPoint", 15
            "OpExecutionMode", 16
            "OpCapability", 17
            "OpTypeVoid", 19
            "OpTypeBool", 20
            "OpTypeInt", 21
            "OpTypeFloat", 22
            "OpTypeVector", 23
            "OpTypeMatrix", 24
            "OpTypeImage", 25
            "OpTypeSampler", 26
            "OpTypeSampledImage", 27
            "OpTypeArray", 28
            "OpTypeRuntimeArray", 29
            "OpTypeStruct", 30
            "OpTypeOpaque", 31
            "OpTypePointer", 32
            "OpTypeFunction", 33
            "OpTypeEvent", 34
            "OpTypeDeviceEvent", 35
            "OpTypeReserveId", 36
            "OpTypeQueue", 37
            "OpTypePipe", 38
            "OpTypeForwardPointer", 39
            "OpConstantTrue", 41
            "OpConstantFalse", 42
            "OpConstant", 43
            "OpConstantComposite", 44
            "OpConstantSampler", 45
            "OpConstantNull", 46
            "OpSpecConstantTrue", 48
            "OpSpecConstantFalse", 49
            "OpSpecConstant", 50
            "OpSpecConstantComposite", 51
            "OpSpecConstantOp", 52
            "OpFunction", 54
            "OpFunctionParameter", 55
            "OpFunctionEnd", 56
            "OpFunctionCall", 57
            "OpVariable", 59
            "OpImageTexelPointer", 60
            "OpLoad", 61
            "OpStore", 62
            "OpCopyMemory", 63
            "OpCopyMemorySized", 64
            "OpAccessChain", 65
            "OpInBoundsAccessChain", 66
            "OpPtrAccessChain", 67
            "OpArrayLength", 68
            "OpGenericPtrMemSemantics", 69
            "OpInBoundsPtrAccessChain", 70
            "OpDecorate", 71
            "OpMemberDecorate", 72
            "OpDecorationGroup", 73
            "OpGroupDecorate", 74
            "OpGroupMemberDecorate", 75
            "OpVectorExtractDynamic", 77
            "OpVectorInsertDynamic", 78
            "OpVectorShuffle", 79
            "OpCompositeConstruct", 80
            "OpCompositeExtract", 81
            "OpCompositeInsert", 82
            "OpCopyObject", 83
            "OpTranspose", 84
            "OpSampledImage", 86
            "OpImageSampleImplicitLod", 87
            "OpImageSampleExplicitLod", 88
            "OpImageSampleDrefImplicitLod", 89
            "OpImageSampleDrefExplicitLod", 90
            "OpImageSampleProjImplicitLod", 91
            "OpImageSampleProjExplicitLod", 92
            "OpImageSampleProjDrefImplicitLod", 93
            "OpImageSampleProjDrefExplicitLod", 94
            "OpImageFetch", 95
            "OpImageGather", 96
            "OpImageDrefGather", 97
            "OpImageRead", 98
            "OpImageWrite", 99
            "OpImage", 100
            "OpImageQueryFormat", 101
            "OpImageQueryOrder", 102
            "OpImageQuerySizeLod", 103
            "OpImageQuerySize", 104
            "OpImageQueryLod", 105
            "OpImageQueryLevels", 106
            "OpImageQuerySamples", 107
            "OpConvertFToU", 109
            "OpConvertFToS", 110
            "OpConvertSToF", 111
            "OpConvertUToF", 112
            "OpUConvert", 113
            "OpSConvert", 114
            "OpFConvert", 115
            "OpQuantizeToF16", 116
            "OpConvertPtrToU", 117
            "OpSatConvertSToU", 118
            "OpSatConvertUToS", 119
            "OpConvertUToPtr", 120
            "OpPtrCastToGeneric", 121
            "OpGenericCastToPtr", 122
            "OpGenericCastToPtrExplicit", 123
            "OpBitcast", 124
            "OpSNegate", 126
            "OpFNegate", 127
            "OpIAdd", 128
            "OpFAdd", 129
            "OpISub", 130
            "OpFSub", 131
            "OpIMul", 132
            "OpFMul", 133
            "OpUDiv", 134
            "OpSDiv", 135
            "OpFDiv", 136
            "OpUMod", 137
            "OpSRem", 138
            "OpSMod", 139
            "OpFRem", 140
            "OpFMod", 141
            "OpVectorTimesScalar", 142
            "OpMatrixTimesScalar", 143
            "OpVectorTimesMatrix", 144
            "OpMatrixTimesVector", 145
            "OpMatrixTimesMatrix", 146
            "OpOuterProduct", 147
            "OpDot", 148
            "OpIAddCarry", 149
            "OpISubBorrow", 150
            "OpUMulExtended", 151
            "OpSMulExtended", 152
            "OpAny", 154
            "OpAll", 155
            "OpIsNan", 156
            "OpIsInf", 157
            "OpIsFinite", 158
            "OpIsNormal", 159
            "OpSignBitSet", 160
            "OpLessOrGreater", 161
            "OpOrdered", 162
            "OpUnordered", 163
            "OpLogicalEqual", 164
            "OpLogicalNotEqual", 165
            "OpLogicalOr", 166
            "OpLogicalAnd", 167
            "OpLogicalNot", 168
            "OpSelect", 169
            "OpIEqual", 170
            "OpINotEqual", 171
            "OpUGreaterThan", 172
            "OpSGreaterThan", 173
            "OpUGreaterThanEqual", 174
            "OpSGreaterThanEqual", 175
            "OpULessThan", 176
            "OpSLessThan", 177
            "OpULessThanEqual", 178
            "OpSLessThanEqual", 179
            "OpFOrdEqual", 180
            "OpFUnordEqual", 181
            "OpFOrdNotEqual", 182
            "OpFUnordNotEqual", 183
            "OpFOrdLessThan", 184
            "OpFUnordLessThan", 185
            "OpFOrdGreaterThan", 186
            "OpFUnordGreaterThan", 187
            "OpFOrdLessThanEqual", 188
            "OpFUnordLessThanEqual", 189
            "OpFOrdGreaterThanEqual", 190
            "OpFUnordGreaterThanEqual", 191
            "OpShiftRightLogical", 194
            "OpShiftRightArithmetic", 195
            "OpShiftLeftLogical", 196
            "OpBitwiseOr", 197
            "OpBitwiseXor", 198
            "OpBitwiseAnd", 199
            "OpNot", 200
            "OpBitFieldInsert", 201
            "OpBitFieldSExtract", 202
            "OpBitFieldUExtract", 203
            "OpBitReverse", 204
            "OpBitCount", 205
            "OpDPdx", 207
            "OpDPdy", 208
            "OpFwidth", 209
            "OpDPdxFine", 210
            "OpDPdyFine", 211
            "OpFwidthFine", 212
            "OpDPdxCoarse", 213
            "OpDPdyCoarse", 214
            "OpFwidthCoarse", 215
            "OpEmitVertex", 218
            "OpEndPrimitive", 219
            "OpEmitStreamVertex", 220
            "OpEndStreamPrimitive", 221
            "OpControlBarrier", 224
            "OpMemoryBarrier", 225
            "OpAtomicLoad", 227
            "OpAtomicStore", 228
            "OpAtomicExchange", 229
            "OpAtomicCompareExchange", 230
            "OpAtomicCompareExchangeWeak", 231
            "OpAtomicIIncrement", 232
            "OpAtomicIDecrement", 233
            "OpAtomicIAdd", 234
            "OpAtomicISub", 235
            "OpAtomicSMin", 236
            "OpAtomicUMin", 237
            "OpAtomicSMax", 238
            "OpAtomicUMax", 239
            "OpAtomicAnd", 240
            "OpAtomicOr", 241
            "OpAtomicXor", 242
            "OpPhi", 245
            "OpLoopMerge", 246
            "OpSelectionMerge", 247
            "OpLabel", 248
            "OpBranch", 249
            "OpBranchConditional", 250
            "OpSwitch", 251
            "OpKill", 252
            "OpReturn", 253
            "OpReturnValue", 254
            "OpUnreachable", 255
            "OpLifetimeStart", 256
            "OpLifetimeStop", 257
            "OpGroupAsyncCopy", 259
            "OpGroupWaitEvents", 260
            "OpGroupAll", 261
            "OpGroupAny", 262
            "OpGroupBroadcast", 263
            "OpGroupIAdd", 264
            "OpGroupFAdd", 265
            "OpGroupFMin", 266
            "OpGroupUMin", 267
            "OpGroupSMin", 268
            "OpGroupFMax", 269
            "OpGroupUMax", 270
            "OpGroupSMax", 271
            "OpReadPipe", 274
            "OpWritePipe", 275
            "OpReservedReadPipe", 276
            "OpReservedWritePipe", 277
            "OpReserveReadPipePackets", 278
            "OpReserveWritePipePackets", 279
            "OpCommitReadPipe", 280
            "OpCommitWritePipe", 281
            "OpIsValidReserveId", 282
            "OpGetNumPipePackets", 283
            "OpGetMaxPipePackets", 284
            "OpGroupReserveReadPipePackets", 285
            "OpGroupReserveWritePipePackets", 286
            "OpGroupCommitReadPipe", 287
            "OpGroupCommitWritePipe", 288
            "OpEnqueueMarker", 291
            "OpEnqueueKernel", 292
            "OpGetKernelNDrangeSubGroupCount", 293
            "OpGetKernelNDrangeMaxSubGroupSize", 294
            "OpGetKernelWorkGroupSize", 295
            "OpGetKernelPreferredWorkGroupSizeMultiple", 296
            "OpRetainEvent", 297
            "OpReleaseEvent", 298
            "OpCreateUserEvent", 299
            "OpIsValidEvent", 300
            "OpSetUserEventStatus", 301
            "OpCaptureEventProfilingInfo", 302
            "OpGetDefaultQueue", 303
            "OpBuildNDRange", 304
            "OpImageSparseSampleImplicitLod", 305
            "OpImageSparseSampleExplicitLod", 306
            "OpImageSparseSampleDrefImplicitLod", 307
            "OpImageSparseSampleDrefExplicitLod", 308
            "OpImageSparseSampleProjImplicitLod", 309
            "OpImageSparseSampleProjExplicitLod", 310
            "OpImageSparseSampleProjDrefImplicitLod", 311
            "OpImageSparseSampleProjDrefExplicitLod", 312
            "OpImageSparseFetch", 313
            "OpImageSparseGather", 314
            "OpImageSparseDrefGather", 315
            "OpImageSparseTexelsResident", 316
            "OpNoLine", 317
            "OpAtomicFlagTestAndSet", 318
            "OpAtomicFlagClear", 319
        ]

    let readSpec() =

        let wordCountRx = Regex @"(?<value>[0-9]+)(?<v>[ \t]*\+[ \t]*variable)?"

        let obj = JsonConvert.DeserializeObject(File.ReadAllText file) |> unbox<JObject>
    
        let readOpCodes() =
            match obj.TryGetValue "OpCodes" with
                | (true, codes) ->
                    [
                        let mutable current = codes.First

                        while current <> null do
                            let name : string = current.["Name"].ToObject()
                            if name <> "Bad" then
                                let cat : int = current.["Category"].ToObject()
                                let hasResult : bool = current.["HasResult"].ToObject()
                                let hasResultType : bool = current.["HasResultType"].ToObject()

                                let code = current.["OpCode"].ToObject()
                                let minWordCount = current.["MinWordCount"].ToObject()
                                let isVariable = current.["HasVariableWordCount"].ToObject()
                                let minUsedWordCount = current.["MinVariableUsedCount"].ToObject()

                                let operands =
                                    [
                                        if hasResultType then
                                            yield { Name = "ResultType"; Type = OperandClass.OperandId; Optional = false}

                                        if hasResult then
                                            yield { Name = "Result"; Type = OperandClass.OperandId; Optional = false}


                                        for e in current.["Operands"] do
                                            let n : string = e.["Name"].ToObject()
                                            let t : int = e.["Type"].ToObject()
                                            let n = 
                                                if String.IsNullOrWhiteSpace n then
                                                    sprintf "_%A" (unbox<OperandClass> t)
                                                else
                                                    n
                                            let optional : bool = e.["Optional"].ToObject()
                                            yield { Name = n; Type = unbox t; Optional = optional }
                                    ]

                                yield {
                                    Name = name
                                    OpCode = code
                                    Category = unbox cat
                                    MinWordCount = minWordCount
                                    HasVariableWordCount = isVariable
                                    MinVariableUsedCount = minUsedWordCount
                                    HasResult = hasResult
                                    HasResultType = hasResultType
                                    Operands = operands
                                }

                            current <- current.Next
                    ]
                | _ ->
                    []

        let readEnums() =
            match obj.TryGetValue "Enums" with
                | (true, enums) ->
                    [
                        for e in enums do
                            yield {
                                Name = e.["Name"].ToObject()
                                Values = 
                                    Map.ofList [
                                        for a in e.["Values"] do
                                            let name = a.["Name"].ToObject()
                                            yield name, {
                                                Name = name
                                                Value = a.["Value"].ToObject()
                                                Comment = a.["Comment"].ToObject()
                                                CommentPlain = a.["CommentPlain"].ToObject()
                                                Capabilities = a.["Capabilities"].ToObject() |> Array.toList
                                                ExtraOperands = [] //a.["ExtraOperands"].ToObject() |> Array.toList
                                            }
                                        
                                    ]
                                Comment = e.["Comment"].ToObject()
                                CommentPlain = e.["CommentPlain"].ToObject()
                            }

                    ]

                | _ ->
                    []

        let ops = readOpCodes()
        let enums = readEnums()

        {
            OpCodes = ops
            Enums = enums
        }

module Scraper =
    open System.Net
    open System.Xml
    open System.Xml.Linq

    let getHtml(url : string) =
        let request = HttpWebRequest.Create(url) //"https://www.khronos.org/registry/spir-v/specs/1.0/SPIRV.html")
        let response = request.GetResponse()
        let s = new StreamReader(response.GetResponseStream())
        s.ReadToEnd()

    let getDom(url : string) =
        let html = getHtml url
        let doc = XDocument.Parse(html)

        doc

    let xname str =
        XName.Get(str, "")

    let read() =
        let dom = getDom "https://www.khronos.org/registry/spir-v/specs/1.0/SPIRV.html"

        let tables = dom.Descendants(xname "table")

        for t in tables do
            printfn "%A" (t.Attribute(xname "class"))






[<AutoOpen>]
module Writer =
    let mutable private builder = System.Text.StringBuilder()

    let printf fmt = Printf.kprintf (fun str -> builder.Append(str) |> ignore; Console.Write("{0}", str)) fmt
    let printfn fmt = Printf.kprintf (fun str -> builder.AppendLine(str) |> ignore; Console.WriteLine("{0}", str)) fmt

    let splitLines (str : string) =
        str.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)

    let types =
        Map.ofList [
            OperandClass.OperandNone, "unit"
            OperandClass.OperandId, "uint32"
            OperandClass.OperandVariableIds, "uint32[]"
            OperandClass.OperandOptionalLiteral, "Option<uint32>"
            OperandClass.OperandOptionalLiteralString, "string"
            OperandClass.OperandVariableLiterals, "uint32[]"
            OperandClass.OperandVariableIdLiteral, "uint32[]"
            OperandClass.OperandVariableLiteralId, "uint32[]"
            OperandClass.OperandLiteralNumber, "uint32"
            OperandClass.OperandLiteralString, "string"
            OperandClass.OperandSource, "SourceLanguage"
            OperandClass.OperandExecutionModel, "ExecutionModel"
            OperandClass.OperandAddressing, "AddressingModel"
            OperandClass.OperandMemory, "MemoryModel"
            OperandClass.OperandExecutionMode, "ExecutionMode"
            OperandClass.OperandStorage, "StorageClass"
            OperandClass.OperandDimensionality, "Dim"
            OperandClass.OperandSamplerAddressingMode, "SamplerAddressingMode"
            OperandClass.OperandSamplerFilterMode, "SamplerFilterMode"
            OperandClass.OperandSamplerImageFormat, "int"
            OperandClass.OperandImageChannelOrder, "int"
            OperandClass.OperandImageChannelDataType, "int"
            OperandClass.OperandImageOperands, "int"
            OperandClass.OperandFPFastMath, "FPFastMathMode"
            OperandClass.OperandFPRoundingMode, "FPRoundingMode"
            OperandClass.OperandLinkageType, "LinkageType"
            OperandClass.OperandAccessQualifier, "AccessQualifier"
            OperandClass.OperandFuncParamAttr, "FunctionParameterAttribute"
            OperandClass.OperandDecoration, "Decoration"
            OperandClass.OperandBuiltIn, "BuiltIn"
            OperandClass.OperandSelect, "SelectionControl"
            OperandClass.OperandLoop, "LoopControl"
            OperandClass.OperandFunction, "FunctionControlMask"
            OperandClass.OperandMemorySemantics, "MemorySemantics"
            OperandClass.OperandMemoryAccess, "MemoryAccess"
            OperandClass.OperandScope, "ExecutionScope"
            OperandClass.OperandGroupOperation, "GroupOperation"
            OperandClass.OperandKernelEnqueueFlags, "KernelEnqueueFlags"
            OperandClass.OperandKernelProfilingInfo, "KernelProfilingInfo"
            OperandClass.OperandCapability, "int"
            OperandClass.OperandOpcode, "int"
        ]


    let fsharpName (n : string) =
        let n = n.Replace("'", "").Replace("-", "").Replace(" ", "").Replace(".", "").Replace("~", "").Replace(",", "")
        let n = 
            if n.Length > 0 then
                if n.[0] >= 'A' && n.[0] <= 'Z' then
                    (string n.[0]).ToLower() + n.Substring(1)
                else
                    n
            else
                n

        if n.Contains "+" || n.Contains "<" then "args"
        else

            match n with
                | "type" -> "_type"
                | "component" -> "_component"
                | "target" -> "_target"
                | "i" -> "_i"
                | "object" -> "_object"
                | "function" -> "_function"
                | "member" -> "_member"
                | "base" -> "_base"
                | "default" -> "_default"
                | "source" -> "_source"
                | "interface" -> "_interface"
                | n -> n


    let writeOpDefinition (spec : SpirVSpec) =

        printfn "type Instruction = "
        for o in spec.OpCodes do
            if o.Name <> "Bad" then
                match o.Operands with
                    | [] -> printfn "    | %s" o.Name
                    | args ->
                        let args =
                            args 
                                |> List.map (fun a -> 
                                
                                    match Map.tryFind a.Type types with
                                        | Some t ->
                                            let t = 
                                                if a.Optional && not (a.Type.IsArray || a.Type.IsString) then sprintf "Option<%s>" t
                                                else t

                                            if String.IsNullOrWhiteSpace a.Name then
                                                sprintf "%s" t
                                            else
                                                sprintf "%s : %s" (fsharpName a.Name) t
                                        | None -> failwithf "unknown type: %A" a.Type
                                   ) 
                                |> String.concat " * "

                        printfn "    | %s of %s" o.Name args

            ()

        printfn "[<AutoOpen>]"
        printfn "module InstructionExtensions ="
        printfn "    type Instruction with"
        printfn "        member x.ResultType ="
        printfn "            match x with"

        for o in spec.OpCodes do
            if o.Name <> "Bad" then
                let argCount = o.Operands |> List.length
                if o.HasResultType then
                    let args = "tid," + (List.init (argCount-1) (fun _ -> "_") |> String.concat ",")
                    printfn "                | %s(%s) -> Some tid" o.Name args

        printfn "                | _ -> None"
        printfn ""
        printfn ""



        printfn "        member x.ResultId ="
        printfn "            match x with"

        for o in spec.OpCodes do
            if o.Name <> "Bad" then
                let argCount = o.Operands |> List.length
                if o.HasResult then
                    let prefix,skip = 
                        if o.HasResultType then "_,id",2
                        else "id",1

                    let args = prefix :: List.init (argCount-skip) (fun _ -> "_") |> String.concat ","
                    printfn "                | %s(%s) -> Some id" o.Name args

        printfn "                | _ -> None"
        printfn ""
        printfn ""


        printfn "        member x.Operands ="
        printfn "            match x with"

        for o in spec.OpCodes do
            if o.Name <> "Bad" then
                let argCount = o.Operands |> List.length
                let skip = 
                    if o.HasResultType && o.HasResult then 2
                    elif o.HasResultType then 1
                    elif o.HasResult then 1
                    else 0

                let opNames = o.Operands |> List.skip skip |> List.map (fun o -> fsharpName o.Name)
                let args = (List.init skip (fun _ -> "_") @ opNames) |> String.concat ","
                let listStr = opNames |> List.map (sprintf "%s :> obj") |> String.concat "; "

                if argCount <> 0 then
                    printfn "                | %s(%s) -> [%s]" o.Name args listStr

        printfn "                | _ -> []"
        printfn ""
        printfn ""


        printfn "        member x.Name ="
        printfn "            match x with"

        for o in spec.OpCodes do
            if o.Name <> "Bad" then
                if not (List.isEmpty o.Operands) then
                    printfn "                | %s(_) -> \"%s\"" o.Name o.Name
                else
                    printfn "                | %s -> \"%s\"" o.Name o.Name
                    

        printfn ""
        printfn ""





    let writeSerializer (spec : SpirVSpec) =
        let enumTypes = spec.Enums |> List.map (fun e -> e.Name) |> Set.ofList

        printfn "module Serializer = "
        printfn "    open System.IO"
        printfn ""
        printfn "    let private writeOpHeader (code : int) (wordSize : int) (target : BinaryWriter) ="
        printfn "        target.Write((uint32 (wordSize) <<< 16) ||| (uint32 code))"
        printfn ""
        printfn "    let private readOpHeader (source : BinaryReader) ="
        printfn "        let v = source.ReadUInt32()"
        printfn "        let wordCount = int (v >>> 16)"
        printfn "        let opCode = int (v &&& 0xFFFFu)"
        printfn "        (opCode, wordCount)"
        
        printfn ""
        printfn "    let private writeString (str : string) (target : BinaryWriter) ="
        printfn "        let alignedSize = (str.Length + 4) &&& ~~~3"
        printfn "        for c in str do target.Write(byte c)"
        printfn "        for i in str.Length..alignedSize-1 do target.Write(0uy)"
        printfn ""
        printfn "    let private readString (source : BinaryReader) (wordCount : int) ="
        printfn "        if wordCount <= 0 then \"\""
        printfn "        else"
        printfn "            let chars = Array.init (4 * wordCount) (fun _ -> source.ReadByte()) |> Array.filter (fun b -> b <> 0uy)"
        printfn "            System.Text.ASCIIEncoding.ASCII.GetString(chars)"


        
        let defineWriter() =
            printfn "    let writeInstructions (instructions : seq<Instruction>) (target : BinaryWriter) = "
            printfn "        for i in instructions do"
            printfn "            match i with"

            for o in spec.OpCodes do
                match o.Operands with
                    | [] -> 
                        let wordCount = o.MinWordCount
                        printfn "                | %s -> writeOpHeader %d %d target" o.Name o.OpCode wordCount
                    | _ ->
                        if o.HasVariableWordCount then
                            let operandNames = o.Operands |> List.map (fun o -> fsharpName o.Name)
                            let operandPattern = operandNames |> String.concat ", "
                            printfn "                | %s(%s) -> " o.Name operandPattern

                            let lastArg = o.Operands |> Seq.last
                            let lastArgName = lastArg.Name |> fsharpName

                            let fixedOperands = o.Operands.Length
                            if lastArg.Type.IsString then
                                printfn "                    let wordCount = %d + (%s.Length + 4 &&& ~~~3) / 4" fixedOperands lastArgName
                            elif (lastArg.Optional && not (lastArg.Type.IsArray || lastArg.Type.IsString))  || lastArg.Type = OperandClass.OperandOptionalLiteral then
                                printfn "                    let wordCount = %d + (if %s.IsSome then 1 else 0)" fixedOperands lastArgName
                            else
                                printfn "                    let wordCount = %d + %s.Length" fixedOperands lastArgName
                        
                            printfn "                    writeOpHeader %d wordCount target" o.OpCode

                            for op in o.Operands do
                                if op = lastArg then
                                    if lastArg.Type.IsString then
                                        printfn "                    writeString %s target" lastArgName
                                    elif lastArg.Type.IsArray then
                                        printfn "                    for v in %s do" lastArgName
                                        printfn "                        target.Write(v)"
                                    elif lastArg.Optional && lastArg.Type.IsEnum then
                                        printfn "                    match %s with" lastArgName
                                        printfn "                        | Some v -> target.Write(uint32 (int v))"
                                        printfn "                        | None -> ()"
                                    elif lastArg.Optional || lastArg.Type = OperandClass.OperandOptionalLiteral then
                                        printfn "                    match %s with" lastArgName
                                        printfn "                        | Some v -> target.Write(v)"
                                        printfn "                        | None -> ()"
                                    else
                                        printfn "                    for v in %s do" lastArgName
                                        printfn "                        target.Write(v)"
                                else
                                    if op.Type.IsEnum then
                                        printfn "                    target.Write(uint32 (int %s))" (fsharpName op.Name)
                                    else
                                        printfn "                    target.Write(%s)" (fsharpName op.Name)


                        else
                            let operandPattern = o.Operands |> List.map (fun o -> fsharpName o.Name) |> String.concat ", "
                            printfn "                | %s(%s) -> " o.Name operandPattern


                            let wordCount = o.MinWordCount
                            printfn "                    writeOpHeader %d %d target" o.OpCode wordCount

                            for op in o.Operands do
                                if op.Type.IsEnum then
                                    printfn "                    target.Write(uint32 (int %s))" (fsharpName op.Name)
                                else
                                    printfn "                    target.Write(%s)" (fsharpName op.Name)

        let defineReader() =
            printfn "    let readInstructions (source : BinaryReader) = "
            printfn "        seq {"
            printfn "            while source.BaseStream.Position < source.BaseStream.Length do"
            printfn "                let pos = source.BaseStream.Position"
            printfn "                let (code, size) = readOpHeader source"
            printfn "                match code with"

            for op in spec.OpCodes do
                printfn "                    | %d ->" op.OpCode
                if op.HasVariableWordCount then
                    let lastArg = op.Operands |> Seq.last
                    let lastArgName = lastArg.Name |> fsharpName
                    printfn "                        let %sSize = max 0 (size - %d)" lastArgName (op.Operands.Length)


                    for a in op.Operands do
                        if a <> lastArg then
                            let name = fsharpName a.Name
                            if a.Type.IsEnum then
                                let enumName = types.[a.Type]
                                printfn "                        let %s = unbox<%s> (int (source.ReadUInt32()))" name enumName
                            else
                                printfn "                        let %s = source.ReadUInt32()" name

                    match lastArg.Type with
                        | t when t.IsString ->
                            printfn "                        let %s = readString source %sSize" lastArgName lastArgName

                        | t when lastArg.Type.IsArray ->
                            printfn "                        let %s = Array.init %sSize (fun _ -> source.ReadUInt32())" lastArgName lastArgName

                            

                        | t when lastArg.Optional || t = OperandClass.OperandOptionalLiteral ->
                            if t.IsEnum then
                                let enumName = types.[t]
                                printfn "                        let %s = if %sSize = 0 then None else Some (unbox<%s> (int (source.ReadUInt32())))" lastArgName lastArgName enumName
                            else
                                printfn "                        let %s = if %sSize = 0 then None else Some (source.ReadUInt32())" lastArgName lastArgName

                        | _ ->
                            printfn "                        let %s = Array.init %sSize (fun _ -> source.ReadUInt32())" lastArgName lastArgName


                    let operands =
                        op.Operands |> List.map (fun op -> fsharpName op.Name)

                    printfn "                        yield %s(%s)" op.Name (String.concat ", " operands)
                else
                    for a in op.Operands do
                        let name = fsharpName a.Name
                        if a.Type.IsEnum then
                            let enumName = types.[a.Type]
                            printfn "                        let %s = unbox<%s> (int (source.ReadUInt32()))" name enumName
                        else
                            printfn "                        let %s = source.ReadUInt32()" name
                        ()

                    let operands =
                        op.Operands |> List.map (fun op -> fsharpName op.Name)

                    match operands with
                        | [] -> printfn "                        yield %s" op.Name
                        | _ -> printfn "                        yield %s(%s)" op.Name (String.concat ", " operands)
            printfn "                    | c -> failwithf \"invalid OpCode: %%d\" c"
            printfn "                source.BaseStream.Seek(pos + int64 (4*size), SeekOrigin.Begin) |> ignore"
            printfn "        }"


        let defineModuleWriter() =

            printfn "    let write (m : Module) (target : BinaryWriter) = "
            printfn "        target.Write(m.magic)"
            printfn "        target.Write(m.version)"
            printfn "        target.Write(m.generatorMagic)"
            printfn "        target.Write(m.bound)"
            printfn "        target.Write(m.reserved)"
            printfn "        writeInstructions m.instructions target"

        let defineModuleReader() =

            printfn "    let read (source : BinaryReader) = "
            printfn "        let magic = source.ReadUInt32()"
            printfn "        let version = source.ReadUInt32()"
            printfn "        let generatorMagic = source.ReadUInt32()"
            printfn "        let bound = source.ReadUInt32()"
            printfn "        let reserved = source.ReadUInt32()"
            printfn "        let instructions = readInstructions source"
            printfn "        { magic = magic; version = version; generatorMagic = generatorMagic; bound = bound; reserved = reserved; instructions = Seq.toList instructions }"



        printfn ""
        defineWriter()
        printfn ""
        defineReader()
        printfn ""
        printfn ""
        defineModuleWriter()
        printfn ""
        defineModuleReader()

        
        ()


    let write (spec : SpirVSpec) =
        builder <- System.Text.StringBuilder()

        printfn "namespace SpirV"
        printfn ""
        writeOpDefinition spec

        printfn ""
        printfn ""
        printfn "type Module ="
        printfn "    {"
        printfn "        magic : uint32"
        printfn "        version : uint32"
        printfn "        generatorMagic : uint32"
        printfn "        bound : uint32"
        printfn "        reserved : uint32"
        printfn "        instructions : list<Instruction>"
        printfn "    }"

        printfn ""
        writeSerializer spec


        let str = builder.ToString()
        File.WriteAllText(outputFile, str)





let run() =
    let spec = readSpec()


    write spec