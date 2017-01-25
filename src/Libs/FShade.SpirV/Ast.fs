namespace FShade.SpirV

open System
open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative


type ConcList<'a> =
    | Empty
    | Single of list<'a>
    | Concat of ConcList<'a> * ConcList<'a>

type AssemblerState =
    {
        reverseMatrix   : bool
        typeIds         : HashMap<obj, uint32>
        variableIds     : HashMap<CVar, uint32>
        functionIds     : HashMap<CFunctionSignature, uint32>
        inputs          : HashMap<string, uint32>
        uniforms        : HashMap<string, uint32>
        currentId       : uint32
        labelId         : uint32
        extensions      : Map<string, uint32>
        constants       : HashMap<CType * CLiteral, uint32>
        instructions    : ConcList<Instruction>
    }

type Asm<'a> = State<AssemblerState, 'a>

type AsmBuilder() =
    inherit StateBuilder()
    
    member x.Yield(i : Instruction) =
        State.modify (fun s ->
            { s with AssemblerState.instructions = Concat(s.instructions, Single [i]) }
        )

    member x.Yield(i : list<Instruction>) =
        State.modify (fun s ->
            { s with AssemblerState.instructions = Concat(s.instructions, Single i) }
        )

    member x.Run(a : Asm<'a>) = a


module Assembler =
    let asm = AsmBuilder()

    [<Literal>]
    let glsl410 = "GLSL.std.450"

    let newId = 
        State.custom (fun s ->
            { s with currentId = s.currentId + 1u }, s.currentId
        )

    let newLabel = 
        State.custom (fun s ->
            { s with labelId = s.labelId + 1u }, s.labelId
        )

    let getVarId (v : CVar) =
        State.get |> State.map (fun s -> HashMap.find v s.variableIds)
        
    let getFunctionId (v : CFunctionSignature) =
        State.get |> State.map (fun s -> HashMap.find v s.functionIds)

    let getInput (name : string) =
        State.get |> State.map (fun s -> HashMap.find name s.inputs)

    let getUniform (name : string) =
        State.get |> State.map (fun s -> HashMap.find name s.uniforms)

    let reverseMatrix =
        State.get |> State.map (fun s -> s.reverseMatrix)
        
    let import (ext : string) =
        asm {
            let! s = State.get
            match Map.tryFind ext s.extensions with
                | Some ext -> 
                    return ext
                | None ->
                    let! id = newId
                    yield OpExtInstImport(id, ext)
                    do! State.modify (fun s -> { s with extensions = Map.add ext id s.extensions })
                    return id
        }

    type private CacheTypeBuilder(t : obj) =
        inherit StateBuilder()    

        member x.Yield(i : Instruction) =
            State.modify (fun s ->
                { s with AssemblerState.instructions = Concat(s.instructions, Single [i]) }
            )

        member x.Yield(i : list<Instruction>) =
            State.modify (fun s ->
                { s with AssemblerState.instructions = Concat(s.instructions, Single i) }
            )
        member x.Run(m : Asm<uint32>) : Asm<uint32> =
            State.custom (fun s ->
                match HashMap.tryFind t s.typeIds with
                    | Some tid -> 
                        s, tid
                    | None ->
                        let mutable s = s
                        let tid = m.Run(&s)
                        s <- { s with typeIds = HashMap.add t tid s.typeIds }
                        s, tid
            )

    let setLength (len : int) (v : byte[]) =
        let arr = 
            if v.Length < len then
                Array.append v (Array.zeroCreate (len - v.Length))
            elif v.Length > len then
                Array.take len v
            else
                v
        Array.init (arr.Length / 4) (fun i ->
            BitConverter.ToUInt32(arr, 4 * i)
        )

    let rec private (|Floating|Signed|Unsigned|Other|) (t : CType) =
        match t with
            | CFloat _ -> Floating
            | CInt(signed, _) ->
                if signed then Signed
                else Unsigned

            | CMatrix(t, _, _) -> (|Floating|Signed|Unsigned|Other|) t
            | CVector(t, _) -> (|Floating|Signed|Unsigned|Other|) t

            | _ ->
                Other

    let private (|Scalar|_|) (t : CType) =
        match t with
            | CInt _ 
            | CFloat _ ->
                Some ()
            | _ ->
                None


    let private cacheType (t : obj) =
        CacheTypeBuilder(t)

    let assembleBool (b : bool) =
        if b then 1u
        else 0u

    let rec assembleSystemType (t : Type) =
        cacheType t {
            match t with
                | ArrOf(len, et) ->
                    let! et = assembleSystemType et
                    let! id = newId 
                    yield OpTypeArray(id, et, uint32 len)
                    return id

                | SamplerType(dim, isArray, isShadow, isMS, valueType) ->
                    let! valueType = assembleSystemType valueType
                    let! imgId = newId

                    let dim = 
                        match dim with
                            | SamplerDimension.Sampler1d -> Dim.Dim1D
                            | SamplerDimension.Sampler2d -> Dim.Dim2D
                            | SamplerDimension.Sampler3d -> Dim.Dim3D
                            | SamplerDimension.SamplerCube -> Dim.Cube
                            | dim -> failwithf "[SpirV] invalid sampler dimension %A" dim

                    yield OpTypeImage(imgId, valueType, dim, assembleBool isShadow, assembleBool isArray, assembleBool isMS, 1u, 0, None)

                    let! tid = newId
                    yield OpTypeSampledImage(tid, imgId)
                    return tid
                | _ ->
                    let ct = CType.ofType t
                    return! assembleType ct
        }

    and assembleType (t : CType) =
        cacheType t {
            match t with
                | CType.CBool -> 
                    let! tid = newId
                    yield OpTypeBool tid
                    return tid

                | CType.CVoid ->
                    let! tid = newId
                    yield OpTypeVoid tid
                    return tid

                | CType.CInt(signed, width) ->
                    let! tid = newId
                    yield OpTypeInt(tid, uint32 width, (if signed then 1u else 0u))
                    return tid

                | CType.CFloat(width) ->
                    let! tid = newId
                    yield OpTypeFloat(tid, uint32 width)
                    return tid

                | CType.CVector(et, len) ->
                    let! et = assembleType et
                    let! tid = newId
                    yield OpTypeVector(tid, et, uint32 len)
                    return tid

                | CType.CMatrix(et, rows, cols) ->
                    let! et = assembleType (CType.CVector(et, rows))
                    let! tid = newId
                    yield OpTypeMatrix(tid, et, uint32 cols)
                    return tid

                | CType.CArray(et, len) ->
                    let! et = assembleType et
                    let! tid = newId
                    yield OpTypeArray(tid, et, uint32 len)
                    return tid

                | CType.CPointer(m, et) ->
                    let! et = assembleType et
                    let! tid = newId
                    yield OpTypeRuntimeArray(tid, et)
                    return tid

                | CType.CStruct(name, fields, _) ->
                    let! fields =
                        fields |> List.mapS (fun (t, n) ->
                            asm {
                                let! t = assembleType t
                                return t, n
                            }
                        )
                        
                    let fields = fields |> List.toArray
                    let! tid = newId
                    yield OpTypeStruct(tid, fields |> Array.map fst)
                    for i in 0 .. fields.Length - 1 do
                        let (_,n) = fields.[i]
                        yield OpMemberName(tid, uint32 i, n)

                    yield OpDecorate(tid, Decoration.GLSLPacked, [||])
                    yield OpName(tid, name)
                    return tid


                | CType.CIntrinsic i ->
                    match i.tag with
                        | :? Type as t ->
                            return! assembleSystemType t
                        | tag ->
                            return failwithf "[SpirV] unexpected type-tag %A" tag
                                


        }

    and assemblePtrType (clazz : StorageClass) (t : CType) =
        cacheType (t, "ptr") {
            let! tid = assembleType t
            let! id = newId
            yield OpTypePointer(id, clazz, tid)
            return id
        }

    let assembleConstant (t : CType) (v : CLiteral) =
        asm {
            let! s = State.get
            match HashMap.tryFind (t, v) s.constants with
                | Some cid ->
                    return cid
                | None ->
                    let! tid = assembleType t
                    let! id = newId
                    match v with
                        | CIntegral v ->
                            match t with    
                                | CInt(_,8)     -> yield OpConstant(tid, id, [| uint32 (int8 v) |])
                                | CInt(_,16)    -> yield OpConstant(tid, id, [| uint32 (int16 v) |])
                                | CInt(_,32)    -> yield OpConstant(tid, id, [| uint32 v |])
                                | CInt(_,64)    -> yield OpConstant(tid, id, v |> BitConverter.GetBytes |> setLength 8)
                                | _             -> failwithf "[SpirV] invalid integral type %A" t

                        | CFractional v ->
                            let arr = v |> float32 |> BitConverter.GetBytes |> setLength 4
                            yield OpConstant(tid, id, arr)

                        | CBool v ->
                            if v then yield OpConstantTrue(tid, id)
                            else yield OpConstantFalse(tid, id)

                        | Null ->
                            yield OpConstantNull(tid, id)

                        | CString str ->
                            yield OpString(id, str)

                    do! State.modify (fun s -> { s with constants = HashMap.add (t,v) id s.constants })
                    return id

        }

    let assembleInt32 (v : int) =
        assembleConstant (CType.CInt(true, 32)) (CIntegral (int64 v))

    let rec assembleExpr (e : CExpr) =
        asm {
            match e with
                | CVar v ->
                    let! id = getVarId v
                    return id

                | CValue(t, v) ->
                    return! assembleConstant t v

                | CCall(func, args) ->
                    let! ret = assembleType func.returnType
                    let! fid = getFunctionId func
                    let! args = args |> Array.mapS assembleExpr
                    let! id = newId
                    yield OpFunctionCall(ret, id, fid, args)
                    return id

                | CCallIntrinsic _ ->
                    return failwith "not implemented"
                    

                | CConditional(t, cond, i, e) ->
                    let! t = assembleType t
                    let! cond = assembleExpr cond
                    let! lTrue = newLabel
                    let! lFalse = newLabel
                    let! lEnd = newLabel
                    yield OpBranchConditional(cond, lTrue, lFalse, [||])

                    yield OpLabel(lTrue)
                    let! tid = assembleExpr i
                    yield OpBranch(lEnd)

                    yield OpLabel(lFalse)
                    let! fid = assembleExpr e

                    let! id = newId
                    yield OpLabel(lEnd)
                    yield OpPhi(t, id, [| tid; lTrue; fid; lFalse |])
                    return id

                | CReadInput(ParameterKind.Input, t, name, index) ->
                    let! t = assembleType t
                    let! input = getInput name
                    match index with
                        | Some index ->
                            let! index = assembleExpr index
                            let! id = newId
                            yield OpPtrAccessChain(t, id, input, index, [||])
                            return id
                        | None ->
                            let! id = newId
                            yield OpLoad(t, id, input, None)
                            return id

                | CReadInput(ParameterKind.Uniform, t, name, index) ->
                    let! t = assembleType t
                    let! input = getUniform name
                    match index with
                        | Some index ->
                            let! index = assembleExpr index
                            let! id = newId
                            yield OpPtrAccessChain(t, id, input, index, [||])
                            return id
                        | None ->
                            let! id = newId
                            yield OpLoad(t, id, input, None)
                            return id

                | CReadInput(_, _, _, _) ->
                    return failwithf "[SpirV] invalid input %A" e

                | CNeg(t, v) ->
                    let! tid = assembleType t
                    let! v = assembleExpr v
                    let! id = newId
                    match t with
                        | Floating ->
                            yield OpFNegate(tid, id, v)
                        | Signed -> 
                            yield OpSNegate(tid, id, v)
                        | _ ->
                            failwithf "[SpirV] bad type in negation %A" t
                    return id

                | CNot(t, v) ->
                    let! t = assembleType t
                    let! v = assembleExpr v
                    let! id = newId
                    yield OpLogicalNot(t, id, v)
                    return id


                | CAdd(t, l, r) ->
                    let! tid = assembleType t
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = newId
                    match t with
                        | Floating -> yield OpFAdd(tid, id, l, r)
                        | Signed | Unsigned -> yield OpIAdd(tid, id, l, r)
                        | _ -> failwithf "[SpirV] bad type in addition %A" t
                    return id

                | CSub(t, l, r) ->
                    let! tid = assembleType t
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = newId
                    match t with
                        | Floating -> yield OpFSub(tid, id, l, r)
                        | Signed | Unsigned -> yield OpISub(tid, id, l, r)
                        | _ -> failwithf "[SpirV] bad type in subtraction %A" t
                    return id

                | CMul(t, l, r) ->
                    let! tid = assembleType t
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = newId

                    match l.ctype, r.ctype with 
                        | CVector _, Scalar -> yield OpVectorTimesScalar(tid, id, lid, rid)
                        | CMatrix _, Scalar -> yield OpMatrixTimesScalar(tid, id, lid, rid)
                        | Scalar, CVector _ -> yield OpVectorTimesScalar(tid, id, rid, lid)
                        | Scalar, CMatrix _ -> yield OpMatrixTimesScalar(tid, id, rid, lid)
                        | _ ->
                            match t with
                                | Floating -> yield OpFMul(tid, id, lid, rid)
                                | Signed | Unsigned -> yield OpIMul(tid, id, lid, rid)
                                | _ -> failwithf "[SpirV] bad type in multiplication %A" t
                    return id

                | CDiv(t, l, r) ->
                    let! tid = assembleType t
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = newId
                    match t with
                        | Floating -> yield OpFDiv(tid, id, l, r)
                        | Signed -> yield OpSDiv(tid, id, l, r)
                        | Unsigned -> yield OpUDiv(tid, id, l, r)
                        | _ -> failwithf "[SpirV] bad type in division %A" t
                    return id

                | CMod(t, l, r) ->
                    let! tid = assembleType t
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = newId
                    match t with
                        | Floating -> yield OpFMod(tid, id, l, r)
                        | Signed -> yield OpSMod(tid, id, l, r)
                        | Unsigned -> yield OpUMod(tid, id, l, r)
                        | _ -> failwithf "[SpirV] bad type in modulus %A" t
                    return id

                | CTranspose(t, v) ->
                    let! t = assembleType t
                    let! v = assembleExpr v
                    let! id = newId
                    yield OpTranspose(t, id, v)
                    return id

                | CMulMatMat(t, l, r) ->
                    let! tid = assembleType t
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = newId
                    let! reverse = reverseMatrix
                    if reverse then yield OpMatrixTimesMatrix(tid, id, r, l)
                    else yield OpMatrixTimesMatrix(tid, id, l, r)
                    return id

                | CMulMatVec(t, l, r) ->
                    let! tid = assembleType t
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = newId
                    let! reverse = reverseMatrix
                    if reverse then yield OpVectorTimesMatrix(tid, id, r, l)
                    else yield OpMatrixTimesVector(tid, id, l, r)
                    return id
                    
                | CDot(t, l, r) ->
                    let! tid = assembleType t
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = newId
                    yield OpDot(tid, id, l, r)
                    return id
                    
                | CCross(t, l, r) ->
                    let! tid = assembleType t
                    let! glsl = import glsl410
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = newId
                    yield OpExtInst(tid, id, 1u, uint32 GLSLExtInstruction.GLSLstd450Cross, [| l; r |])
                    return id

                | CVecSwizzle(t, v, fields) ->
                    let! tid = assembleType t
                    let! v = assembleExpr v
                    let! id = newId
                    let comp = fields |> List.map uint32 |> List.toArray
                    yield OpVectorShuffle(tid, id, v, v, comp)
                    return id

                | CNewVector(t, d, args) ->
                    let! tid = assembleType t
                    let! args = args |> List.mapS assembleExpr
                    let! id = newId
                    yield OpCompositeConstruct(tid, id, List.toArray args)
                    return id

                | CVecLength(t, v) ->
                    let! tid = assembleType t
                    let! v = assembleExpr v
                    let! glsl = import glsl410
                    let! id = newId
                    yield OpExtInst(tid, id, 1u, uint32 GLSLExtInstruction.GLSLstd450Length, [| v |])
                    return id

                | CMatrixElement _ | CMatrixFromCols _ | CMatrixFromRows _ ->
                    return failwith "not implemented"
                    
                | CConvert(t, e) ->
                    let! tid = assembleType t
                    let! eid = assembleExpr e
                    let! id = newId
                    match e.ctype, t with
                        | Floating, Floating    -> yield OpFConvert(tid, id, eid)
                        | Floating, Signed      -> yield OpConvertFToS(tid, id, eid)
                        | Floating, Unsigned    -> yield OpConvertFToU(tid, id, eid)
                        | Signed, Floating      -> yield OpConvertSToF(tid, id, eid)
                        | Signed, Signed        -> yield OpSConvert(tid, id, eid)
                        | Signed, Unsigned      -> yield OpSatConvertSToU(tid, id, eid)
                        | Unsigned, Floating    -> yield OpConvertUToF(tid, id, eid)
                        | Unsigned, Signed      -> yield OpSatConvertUToS(tid, id, eid)
                        | Unsigned, Unsigned    -> yield OpUConvert(tid, id, eid)
                        | s, t                  -> failwithf "[SpirV] unknown conversion from %A to %A" s t

                    return id

                | CLess(l, r) -> 
                    let! tid = assembleType CType.CBool
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = newId
                    match l.ctype with
                        | Floating -> yield OpFOrdLessThan(tid, id, lid, rid)
                        | Signed -> yield OpSLessThan(tid, id, lid, rid)
                        | Unsigned -> yield OpULessThan(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CLequal(l, r) -> 
                    let! tid = assembleType CType.CBool
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = newId
                    match l.ctype with
                        | Floating -> yield OpFOrdLessThanEqual(tid, id, lid, rid)
                        | Signed -> yield OpSLessThanEqual(tid, id, lid, rid)
                        | Unsigned -> yield OpULessThanEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CGreater(l, r) -> 
                    let! tid = assembleType CType.CBool
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = newId
                    match l.ctype with
                        | Floating -> yield OpFOrdGreaterThan(tid, id, lid, rid)
                        | Signed -> yield OpSGreaterThan(tid, id, lid, rid)
                        | Unsigned -> yield OpUGreaterThan(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CGequal(l, r) -> 
                    let! tid = assembleType CType.CBool
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = newId
                    match l.ctype with
                        | Floating -> yield OpFOrdGreaterThanEqual(tid, id, lid, rid)
                        | Signed -> yield OpSGreaterThanEqual(tid, id, lid, rid)
                        | Unsigned -> yield OpUGreaterThanEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CEqual(l, r) -> 
                    let! tid = assembleType CType.CBool
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = newId
                    match l.ctype with
                        | Floating -> yield OpFOrdEqual(tid, id, lid, rid)
                        | Signed | Unsigned -> yield OpIEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CNotEqual(l, r) -> 
                    let! tid = assembleType CType.CBool
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = newId
                    match l.ctype with
                        | Floating -> yield OpFOrdNotEqual(tid, id, lid, rid)
                        | Signed | Unsigned -> yield OpINotEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CAnd(l, r) ->
                    return! assembleExpr (CConditional(CType.CBool, l, r, CValue(CType.CBool, CBool false)))

                | COr(l, r) ->
                    return! assembleExpr (CConditional(CType.CBool, l,  CValue(CType.CBool, CBool true), r))

                | CBitAnd(t, l, r) ->
                    let! tid = assembleType t
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = newId
                    yield OpBitwiseAnd(tid, id, lid, rid)
                    return id

                | CBitOr(t, l, r) ->
                    let! tid = assembleType t
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = newId
                    yield OpBitwiseOr(tid, id, lid, rid)
                    return id

                | CBitXor(t, l, r) ->
                    let! tid = assembleType t
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = newId
                    yield OpBitwiseXor(tid, id, lid, rid)
                    return id

                | CAddressOf _ ->
                    return failwith "not implemented"

                | CField(t, v, f) ->
                    let! tid = assembleType t
                    let! vid = assembleExpr v
                    let! id = newId
                    match v.ctype with
                        | CStruct(_,fields,_) ->
                            let index = fields |> List.findIndex (fun (_,n) -> n = f)
                            yield OpCompositeExtract(tid, id, vid, [| uint32 index |])
                        | _ ->
                            failwithf "[SpirV] cannot access field %A" f
                    return id

                | CItem(t, v, i) ->
                    let! tid = assembleType t
                    let! v = assembleExpr v
                    let! i = assembleExpr i
                    let! id = newId
                    yield OpInBoundsAccessChain(tid, id, v, [| i |])
                    return id

        }

    let assembleRExpr (e : CRExpr) =
        asm {
            match e with
                | CRExpr e -> return! assembleExpr e
                | CRArray(t, args) ->
                    let! tid = assembleType t
                    let! args = args |> List.mapS assembleExpr
                    let! id = newId
                    yield OpCompositeConstruct(tid, id, List.toArray args)
                    return id
        }

    let rec assembleLExpr (e : CLExpr) =
        asm {
            let! tid = assemblePtrType StorageClass.Private e.ctype
            match e with
                | CLVar v ->
                    let! id = getVarId v
                    return id

                | CLField(t, l, f) ->
                    let! lid = assembleLExpr l
                    match l.ctype with
                        | CStruct(_, fields, _) ->
                            let index = fields |> List.findIndex (fun (_,n) -> f = n)
                            let! index = assembleInt32 index
                            let! id = newId
                            yield OpInBoundsAccessChain(tid, id, lid, [|index|])
                            return id
                        | t ->
                            return failwithf "[SpirV] accessing field on non-struct-type %A" t

                | CLItem(t, l, i) ->
                    let! lid = assembleLExpr l
                    let! index = assembleExpr i
                    let! id = newId
                    yield OpInBoundsAccessChain(tid, id, lid, [|index|])
                    return id
                    
                | CLPtr _ | CLVecSwizzle _ | CLMatrixElement _ ->
                    return failwith "not implemented"

                    
        }
