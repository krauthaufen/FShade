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
        outputs         : HashMap<string, uint32>
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
        
    let getOutput (name : string) =
        State.get |> State.map (fun s -> HashMap.find name s.outputs)

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

    let setVarId (v : CVar) (id : uint32) =
        State.modify (fun s ->
            { s with AssemblerState.variableIds = HashMap.add v id s.variableIds  }
        )

    let rec assembleStatement (breakLabel : Option<uint32>) (contLabel : Option<uint32>) (s : CStatement) =
        asm {
            match s with
                | CNop ->
                    ()

                | CDo e ->
                    do! State.ignore (assembleExpr e)

                | CDeclare(v, e) ->
                    let! e = e |> Option.mapS assembleRExpr
                    let! tid = assemblePtrType StorageClass.Private v.ctype
                    let! vid = newId
                    yield OpVariable(tid, vid, StorageClass.Private, e)
                    do! setVarId v vid

                | CWrite(l, v) ->
                    let! l = assembleLExpr l
                    let! v = assembleExpr v
                    yield OpStore(l, v, None)

                | CIncrement(pre, l) ->
                    return! assembleStatement breakLabel contLabel (CWrite(l, CExpr.CAdd(l.ctype, CLExpr.toExpr l, CValue(l.ctype, CIntegral 1L))))

                | CDecrement(pre, l) ->
                    return! assembleStatement breakLabel contLabel (CWrite(l, CExpr.CSub(l.ctype, CLExpr.toExpr l, CValue(l.ctype, CIntegral 1L))))

                | CSequential s ->
                    for s in s do 
                        do! assembleStatement breakLabel contLabel s

                | CReturn ->
                    yield OpReturn

                | CReturnValue v ->
                    let! vid = assembleExpr v
                    yield OpReturnValue vid

                | CBreak ->
                    match breakLabel with
                        | Some l -> yield OpBranch l
                        | _ -> failwith "[SpirV] break outside loop encountered"

                | CContinue ->
                    match contLabel with
                        | Some l -> yield OpBranch l
                        | _ -> failwith "[SpirV] break outside loop encountered"

                | CFor(init, cond, step, body) ->
                    do! assembleStatement breakLabel contLabel init
                    
                    let! lCond = newLabel
                    let! lStart = newLabel
                    let! lEnd = newLabel

                    yield OpLabel lCond
                    let! cid = assembleExpr cond
                    yield OpBranchConditional(cid, lStart, lEnd, [||])

                    yield OpLabel(lStart)
                    do! assembleStatement (Some lEnd) (Some lCond) body
                    do! assembleStatement breakLabel contLabel step
                    yield OpBranch(lCond)

                    yield OpLabel(lEnd)

                | CWhile(guard, body) ->
                    
                    let! lCond = newLabel
                    let! lStart = newLabel
                    let! lEnd = newLabel

                    yield OpLabel lCond
                    let! cid = assembleExpr guard
                    yield OpBranchConditional(cid, lStart, lEnd, [||])

                    yield OpLabel(lStart)
                    do! assembleStatement (Some lEnd) (Some lCond) body
                    yield OpBranch(lCond)

                    yield OpLabel(lEnd)

                | CDoWhile(guard, body) ->
                    
                    let! lCond = newLabel
                    let! lStart = newLabel
                    let! lEnd = newLabel
                    yield OpLabel(lStart)
                    do! assembleStatement (Some lEnd) (Some lCond) body
                    yield OpLabel(lCond)
                    let! v = assembleExpr guard
                    yield OpBranchConditional(v, lStart, lEnd, [||])
                    yield OpLabel(lEnd)

                | CIfThenElse(cond, i, e) ->
                    let! cond = assembleExpr cond
                    let! lTrue = newLabel
                    let! lFalse = newLabel
                    let! lEnd = newLabel

                    yield OpBranchConditional(cond, lTrue, lFalse, [||])
                    yield OpLabel lTrue
                    do! assembleStatement breakLabel contLabel i
                    yield OpBranch lEnd
                    yield OpLabel lFalse
                    do! assembleStatement breakLabel contLabel e
                    yield OpLabel lEnd

                | CWriteOutput(name, index, value) ->
                    let! output = getOutput name
                    let! value = assembleRExpr value
                    match index with
                        | None -> 
                            yield OpStore(output, value, None)
                        | Some i ->
                            failwith "[SpirV] indexed output not implemented"
                            
                            
                | CSwitch _ ->
                    failwith "[SpirV] switch not implemented"
                    

        }

    let assembleEntryParameter (clazz : StorageClass) (p : CEntryParameter) =
        asm {
            let! t = assemblePtrType clazz p.cParamType
            let! id = newId
            yield OpVariable(t, id, clazz, None)



        }

    let assembleEntry (e : CEntryDef) =
        asm {
            
            ()
        }


module Assembler2 =

    type SpirVState =
        {
            currentId           : uint32
            valueIds            : HashMap<obj, uint32>
            uniformIds          : Map<string, uint32 * list<uint32>>
            fieldIds            : HashMap<CType, HashMap<string, int>>
            reversedInstuctions : list<Instruction>
            currentBinding      : uint32
            currentSet          : uint32
        }

    type SpirV<'a> = State<SpirVState, 'a>

    type SpirVBuilder() =
        inherit StateBuilder()

        member x.Yield(i : Instruction) =
            State.modify (fun s ->
                { s with reversedInstuctions = i :: s.reversedInstuctions }
            )

        member x.Run(m : SpirV<'a>) : SpirV<'a> =
            m

    let spirv = SpirVBuilder()

    module SpirV =
        let tryGetId (a : 'a) : SpirV<Option<uint32>> =
            State.get |> State.map (fun s -> HashMap.tryFind (a :> obj) s.valueIds)
            
        let setId (a : 'a) (id : uint32) : SpirV<unit> =
            State.modify (fun s -> { s with valueIds = HashMap.add (a :> obj) id  s.valueIds })

            
        let setUniformId (name : string) (var : uint32) (fields : list<uint32>) : SpirV<unit> =
            State.modify (fun s -> { s with uniformIds = Map.add name (var, fields) s.uniformIds })


        type CachedSpirVBuilder(key : obj) =
            inherit StateBuilder()

            member x.Yield(i : Instruction) =
                State.modify (fun s ->
                    { s with reversedInstuctions = i :: s.reversedInstuctions }
                )

            member x.Run(m : SpirV<uint32>) : SpirV<uint32> =
                state {
                    let! v = tryGetId key 
                    match v with
                        | Some id -> 
                            return id
                        | None ->
                            let! id = m
                            do! setId key id
                            return id
                }

        let cached (v : 'a) =
            CachedSpirVBuilder(v :> obj)

        let id = 
            State.custom (fun s ->
                let id = s.currentId
                { s with currentId = id + 1u }, id
            )

        let setFieldId (t : CType) (name : string) (id : int) =
            State.modify (fun s ->
                match HashMap.tryFind t s.fieldIds with
                    | Some ids ->
                        { s with fieldIds = HashMap.add t (HashMap.add name id ids) s.fieldIds }
                    | None ->
                        { s with fieldIds = HashMap.add t (HashMap.ofList [name,id]) s.fieldIds }
            )

        let tryGetFieldId (t : CType) (name : string) =
            State.get |> State.map (fun s ->
                match HashMap.tryFind t s.fieldIds with
                    | Some ids ->
                        HashMap.tryFind name ids
                    | None ->
                        None
            )

        let newBinding : SpirV<uint32> =
            State.custom (fun s ->
                let c = s.currentBinding
                { s with currentBinding = c + 1u }, c
            )

        let newSet : SpirV<uint32> =
            State.custom (fun s ->
                let c = s.currentSet
                { s with currentSet = c + 1u; currentBinding = 0u }, c
            )

    let rec assembleType (t : CType) =
        SpirV.cached t {
            match t with
                | CType.CVoid -> 
                    let! id = SpirV.id
                    yield OpTypeVoid id
                    return id

                | CType.CBool ->
                    let! id = SpirV.id
                    yield OpTypeBool id
                    return id
                
                | CType.CInt(s,w) ->
                    let! id = SpirV.id
                    yield OpTypeInt(id, uint32 w, (if s then 1u else 0u))
                    return id

                | CType.CFloat(32 | 64) ->
                    let! id = SpirV.id
                    yield OpTypeFloat(id, 32u)
                    return id

                | CType.CFloat(w) ->
                    let! id = SpirV.id
                    yield OpTypeFloat(id, uint32 w)
                    return id

                | CType.CVector(e,d) ->
                    let! e = assembleType e
                    let! id = SpirV.id
                    yield OpTypeVector(id, e, uint32 d)
                    return id

                | CType.CMatrix(e,r,c) ->
                    let! ev = assembleType (CVector(e, r))
                    let! id = SpirV.id
                    yield OpTypeMatrix(id, ev, uint32 c)
                    return id
                    
                | CType.CArray(e,l) ->
                    let! e = assembleType e
                    let! id = SpirV.id
                    yield OpTypeArray(id, e, uint32 l)
                    return id
                    
                | CType.CPointer(_,e) ->
                    let! e = assembleType e
                    let! id = SpirV.id
                    yield OpTypeRuntimeArray(id, e)
                    return id
                    
                | CType.CStruct(name, fields, _) ->
                    let fields = List.toArray fields
                    let! fieldTypes = fields |> Array.mapS (fst >> assembleType)

                    let! id = SpirV.id
                    yield OpTypeStruct(id, fieldTypes)

                    yield OpName(id, name)
                    for i in 0 .. fields.Length - 1 do
                        let (_,n) = fields.[i]
                        yield OpMemberName(id, uint32 i, n)
                        do! SpirV.setFieldId t n i

                    return id

                | CType.CIntrinsic it ->
                    return! assembleIntrinsicType it
                    
        }

    and assemblePtrType (clazz : StorageClass) (t : CType) =
        SpirV.cached (clazz, t) {
            let! t = assembleType t
            let! id = SpirV.id
            yield OpTypePointer(id, clazz, t)
            return id
        }

    and assembleIntrinsicType (t : CIntrinsicType) : SpirV<uint32> =
        spirv {
            return failwith "not implemented"
        }

    let rec assembleExpr (s : CExpr) : SpirV<uint32> =
        spirv {
            return failwith "not implemented"
        }

    let rec assembleRExpr (s : CRExpr) : SpirV<uint32> =
        spirv {
            return failwith "not implemented"
        }

    let rec assembleLExpr (s : CRExpr) : SpirV<uint32> =
        spirv {
            return failwith "not implemented"
        }
   
    let rec assembleStatement (s : CStatement) : SpirV<unit> =
        spirv {
            return failwith "not implemented"
        }

    let tryGetBuiltIn (kind : ParameterKind) (stage : ShaderStageDescription) (name : string) : Option<BuiltIn> =
        failwith "not implemented"



    let assembleFunctionType (args : array<uint32>) (ret : uint32) =
        SpirV.cached ("fun", args, ret) {  
            let! id = SpirV.id
            yield OpTypeFunction(id, ret, args)
            return id
        }

    let rec assembleLiteral (t : CType) (v : CLiteral) =
        SpirV.cached (t, v) {
            let! tid = assembleType t
            let! id = SpirV.id
            match t, v with
                | CInt(_, 8), CIntegral v ->
                    yield OpConstant(tid, id, [| uint32 (int8 v) |])
                | CInt(_, 16), CIntegral v ->
                    yield OpConstant(tid, id, [| uint32 (int16 v) |])
                | CInt(_, 32), CIntegral v ->
                    yield OpConstant(tid, id, [| uint32 (int32 v) |])
                | CInt(_, 64), CIntegral v ->
                    yield OpConstant(tid, id, [| uint32 (v >>> 32); uint32 v |])

                | CFloat(32 | 64), CFractional v ->
                    let value = v |> BitConverter.GetBytes
                    yield OpConstant(tid, id, [| BitConverter.ToUInt32(value, 0) |])
                    
                | CFloat(16), CFractional v ->
                    let mutable value = float16()
                    value.Float32 <- float32 v
                    yield OpConstant(tid, id, [| uint32 value.UInt16 |])

                | _, CBool v ->
                    if v then yield OpConstantTrue(tid, id)
                    else yield OpConstantFalse(tid, id)

                | _, Null ->
                    yield OpConstantNull(tid, id)

                | _ ->
                    failwithf "[FShade] unsupported literal value %A : %A" v t

            return id
        }

    let rec assembleConstantExpr (e : CExpr) =
        spirv {
            match e with
                | CValue(t,v) ->
                    return! assembleLiteral t v

                | CNewVector(t, d, comp) ->
                    let! tid = assembleType t
                    let! comp = comp |> List.mapS assembleConstantExpr
                    let! id = SpirV.id
                    yield OpConstantComposite(tid, id, List.toArray comp)
                    return id


                | _ ->
                    return failwithf "[FShade] not a constant expression %A" e
        }

    let assembleConstantRExpr (e : CRExpr) =
        spirv {
            match e with
                | CRExpr e -> 
                    return! assembleConstantExpr e

                | CRArray(t,args) ->
                    let! t = assembleType t
                    let! args = args |> List.mapS assembleConstantExpr
                    let! id = SpirV.id
                    yield OpConstantComposite(t, id, List.toArray args)
                    return id
        }


    let assembleEntryDef (cond : Option<string>) (e : CEntryDef) =
        spirv {
            let args = e.cArguments |> List.toArray

            let! ret = assembleType e.cReturnType
            let! argTypes = args |> Array.mapS (fun a -> a.cParamType |> assembleType)
            let! fType = assembleFunctionType argTypes ret

            let mutable inputLocation = 0
            let mutable outputLocation = 0

            let stages = e.cDecorations |> List.pick (function (EntryDecoration.Stages s) -> Some s | _ -> None)
            let model =
                match stages.self with
                    | ShaderStage.Vertex -> ExecutionModel.Vertex
                    | ShaderStage.TessControl -> ExecutionModel.TessellationControl
                    | ShaderStage.TessEval -> ExecutionModel.TessellationEvaluation
                    | ShaderStage.Geometry -> ExecutionModel.Geometry
                    | ShaderStage.Fragment -> ExecutionModel.Fragment
                    | _ -> ExecutionModel.GLCompute

            let iface = System.Collections.Generic.List<uint32>()

            for i in e.cInputs do
                let! t = assembleType i.cParamType
                let! id = SpirV.id
                yield OpVariable(t, id, StorageClass.Input, None)
                yield OpName(id, i.cParamName)
                match tryGetBuiltIn ParameterKind.Input stages i.cParamName with
                    | Some b ->
                        yield OpDecorate(id, Decoration.BuiltIn, [| uint32 b |])
                    | None ->   
                        yield OpDecorate(id, Decoration.Location, [| uint32 inputLocation |])
                        inc &inputLocation

                do! SpirV.setId (ParameterKind.Input, i.cParamName) id
                iface.Add id

            for i in e.cOutputs do
                let! t = assembleType i.cParamType
                let! id = SpirV.id
                yield OpVariable(t, id, StorageClass.Output, None)
                yield OpName(id, i.cParamName)
                match tryGetBuiltIn ParameterKind.Output stages i.cParamName with
                    | Some b ->
                        yield OpDecorate(id, Decoration.BuiltIn, [| uint32 b |])
                    | None ->   
                        yield OpDecorate(id, Decoration.Location, [| uint32 outputLocation |])
                        inc &outputLocation

                do! SpirV.setId (ParameterKind.Output, i.cParamName) id
                iface.Add id

            let entryName =
                match cond with
                    | Some c -> c
                    | None -> e.cEntryName

            let! eid = SpirV.id

            yield OpEntryPoint(model, eid, entryName, iface.ToArray())
            yield OpFunction(ret, eid, FunctionControl.None, fType)
            yield OpName(eid, entryName)

            for ai in 0 .. argTypes.Length - 1 do
                let name = args.[ai].cParamName
                let tid = argTypes.[ai]
                let! id = SpirV.id
                yield OpFunctionParameter(tid, id)
                yield OpName(id, name)
                do! SpirV.setId (ParameterKind.Argument, name) id


            do! assembleStatement e.cBody

            yield OpFunctionEnd

        }

    let rec assembleValueDef (cond : Option<string>) (d : CValueDef) : SpirV<unit> =
        spirv {
            match d with
                | CValueDef.CConstant(t, name, value) ->
                    let! value = assembleConstantRExpr value
                    yield OpName(value, name)
                    do! SpirV.setId name value
                    
                | CValueDef.CUniformDef uniforms ->
                    let buffers =
                        uniforms 
                            |> List.groupBy (fun u -> u.cUniformBuffer)

                    let! set = SpirV.newSet
                    for (name, fields) in buffers do
                        match name with
                            | Some name ->
                                let! binding = SpirV.newBinding

                                let t = CStruct(name, fields |> List.map (fun f -> f.cUniformType, f.cUniformName), None)
                                let! tid = assemblePtrType StorageClass.Uniform t

                                yield OpDecorate(tid, Decoration.GLSLPacked, [||])
                                yield OpDecorate(tid, Decoration.BufferBlock, [||])

                                let! id = SpirV.id
                                yield OpVariable(tid, id, StorageClass.Uniform, None)
                                yield OpDecorate(id, Decoration.DescriptorSet, [| set |])
                                yield OpDecorate(id, Decoration.Binding, [| binding |])

                                let fields = List.toArray fields
                                for i in 0 .. fields.Length do
                                    let u = fields.[i]
                                    do! SpirV.setUniformId u.cUniformName id [uint32 i]


                                ()

                            | None ->
                                for f in fields do
                                    let! tid = assemblePtrType StorageClass.Uniform f.cUniformType
                                    let! binding = SpirV.newBinding
                                    let! id = SpirV.id
                                    yield OpVariable(tid, id, StorageClass.Uniform, None)
                                    yield OpDecorate(id, Decoration.DescriptorSet, [| set |])
                                    yield OpDecorate(id, Decoration.Binding, [| binding |])
                                    do! SpirV.setUniformId f.cUniformName id []
        
                | CValueDef.CConditionalDef(cond, inner) ->
                    for v in inner do
                        do! assembleValueDef (Some cond) v

                | CValueDef.CFunctionDef(signature, body) ->
                    let! argTypes = signature.parameters |> Array.mapS (fun p -> assembleType p.ctype)
                    let! ret = assembleType signature.returnType
                    let! tFun = assembleFunctionType argTypes ret

                    let! id = SpirV.id
                    yield OpFunction(ret, id, FunctionControl.None, tFun)
                    for i in 0 .. argTypes.Length - 1 do
                        let p = signature.parameters.[i]
                        let! id = SpirV.id
                        yield OpFunctionParameter(argTypes.[i], id)
                        do! SpirV.setId p.name id


                    do! assembleStatement body

                    yield OpFunctionEnd
                    do! SpirV.setId signature id

                    ()

                | CValueDef.CEntryDef e ->
                    do! assembleEntryDef cond e
        }

    let assembleModuleS (m : CModule) =
        spirv {
            let usage = CModule.usageInfo m

            // assemble all used types
            for t in usage.usedTypes do
                let! _ = assembleType t
                ()

            for v in m.values do
                do! assembleValueDef None v


        }

    let assembleModule (m : CModule) : SpirV.Module =
        let mutable state = 
            {
                currentId           = 0u
                valueIds            = HashMap.empty
                uniformIds          = Map.empty
                fieldIds            = HashMap.empty
                reversedInstuctions = []
            }
        assembleModuleS(m).Run(&state)
        {
            magic = 0x07230203u
            version = 0x00010000u
            generatorMagic = (0xFADEu <<< 16) ||| 1u
            bound = state.currentId
            reserved = 0u
            instructions = state.reversedInstuctions |> List.rev
        }


