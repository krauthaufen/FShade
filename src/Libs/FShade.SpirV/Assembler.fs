namespace FShade.SpirV

open System
open System.Reflection

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative


type SpirVIntrinsicType = { compileType : SpirV<uint32> }

type SpirVIntrinsicFunction = { compileFunction : uint32 -> uint32[] -> SpirV<uint32> }

module Assembler =
    [<Literal>]
    let glsl410 = "GLSL.std.450"

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
            match t.tag with
                | :? SpirVIntrinsicType as t ->
                    return! t.compileType
                | _ ->
                    return failwithf "[FShade] cannot compile type %A" t
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
                    let value = v |> float32 |> BitConverter.GetBytes
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

    [<AutoOpen>]
    module Patterns = 
        let rec (|Floating|Signed|Unsigned|Other|) (t : CType) =
            match t with
                | CFloat _ -> Floating
                | CInt(signed, _) ->
                    if signed then Signed
                    else Unsigned

                | CMatrix(t, _, _) -> (|Floating|Signed|Unsigned|Other|) t
                | CVector(t, _) -> (|Floating|Signed|Unsigned|Other|) t

                | _ ->
                    Other

        let (|Scalar|Vector|Matrix|NonNumeric|) (t : CType) =
            match t with
                | CInt _ 
                | CFloat _ ->
                    Scalar
                | CVector(et,_) ->
                    Vector et
                | CMatrix(et, _, _) ->
                    Matrix et
                | _ ->
                    NonNumeric

    let private newVector (et : CType) (d : int) (vid : uint32) =
        spirv {
            let! vType = assembleType (CVector(et, d))
            let! tid = SpirV.id
            yield OpCompositeConstruct(vType, tid, Array.create d vid)
            return tid
        }

    let rec assembleExpr (e : CExpr) : SpirV<uint32> =
        spirv {
            let! tid = assembleType e.ctype
            match e with
                | CVar v ->
                    let! vid = SpirV.getId v.name
                    let! id = SpirV.id
                    yield OpLoad(tid, id, vid, None)
                    return id

                | CValue(t,v) -> 
                    return! assembleLiteral t v

                | CCall(f, args) ->
                    let! fid = SpirV.getId f
                    let! id = SpirV.id
                    let! args = args |> Array.mapS assembleExpr
                    yield OpFunctionCall(tid, id, fid, args)
                    return id

                | CCallIntrinsic(_,f,args) ->
                    let! args = args |> Array.mapS assembleExpr
                    match f.tag with
                        | :? SpirVIntrinsicFunction as f ->
                            return! f.compileFunction tid args
                        | _ ->
                            return failwith ""

                | CConditional(_, cond, i, e) ->
                    let! cond = assembleExpr cond

                    let! lTrue = SpirV.id
                    let! lFalse = SpirV.id
                    let! lEnd = SpirV.id

                    yield OpBranchConditional(cond, lTrue, lFalse, [||])
                    yield OpLabel(lTrue)
                    let! vTrue = assembleExpr i
                    yield OpBranch(lEnd)
                    yield OpLabel(lFalse)
                    let! vFalse = assembleExpr e
                    yield OpLabel(lEnd)

                    let! id = SpirV.id
                    yield OpPhi(tid, id, [| vTrue; lTrue; vFalse; lFalse |])
                    return id

                | CReadInput(ParameterKind.Uniform, t, name, None) ->
                    let! vid, path = SpirV.getUniformId name
                    match path with
                        | [] -> 
                            let! id = SpirV.id
                            yield OpLoad(tid, id, vid, None)
                            return id

                        | p ->  
                            let! ptrType = assemblePtrType StorageClass.Uniform t
                            let! ptr = SpirV.id
                            yield OpInBoundsAccessChain(ptrType, ptr, vid, List.toArray p)
                            let! id = SpirV.id
                            yield OpLoad(tid, id, ptr, None)
                            return id
                            
                | CReadInput(kind, t, name, index) ->
                    let! vid = SpirV.getId (kind, name)
                    match index with
                        | None ->
                            let! id = SpirV.id
                            yield OpLoad(tid, id, vid, None)
                            return id
                        | Some index ->
                            let! index = assembleExpr index
                            let clazz = StorageClass.Input

                            let! ptrType = assemblePtrType clazz (CPointer(CPointerModifier.None, t))
                            let! ptr = SpirV.id
                            yield OpAccessChain(ptrType, ptr, vid, [| index |])
                            let! id = SpirV.id
                            yield OpLoad(tid, id, ptr, None)
                            return id

                | CNeg(t,e) ->
                    let! eid = assembleExpr e
                    let! id = SpirV.id
                    match t with
                        | Signed -> yield OpSNegate(tid, id, eid)
                        | Floating -> yield OpFNegate(tid, id, eid)
                        | _ -> failwithf "[FShade] cannot negate %A" t

                    return id

                | CNot(t,e) ->
                    let! eid = assembleExpr e
                    let! id = SpirV.id
                    yield OpLogicalNot(tid, id, eid)
                    return id

                | CAdd(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match t with
                        | Signed | Unsigned -> yield OpIAdd(tid, id, lid, rid)
                        | Floating -> yield OpFAdd(tid, id, lid, rid)
                        | _ -> failwithf "[FShade] cannot add %A" t
                    return id

                | CSub(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match t with
                        | Signed | Unsigned -> yield OpISub(tid, id, lid, rid)
                        | Floating -> yield OpFSub(tid, id, lid, rid)
                        | _ -> failwithf "[FShade] cannot add %A" t
                    return id
                    
                | CMul(t, l, r) | CMulMatVec(t, l, r) | CMulMatMat(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id

                    let reverse = true

                    match l.ctype, r.ctype with
                        | Scalar, Vector _          -> yield OpVectorTimesScalar(tid, id, rid, lid)
                        | Vector _, Scalar          -> yield OpVectorTimesScalar(tid, id, lid, rid)
                        | Scalar, Matrix _          -> yield OpMatrixTimesScalar(tid, id, rid, lid)
                        | Matrix _, Scalar          -> yield OpMatrixTimesScalar(tid, id, lid, rid)
                        | Floating, Floating        -> yield OpFMul(tid, id, lid, rid)

                        | (Signed | Unsigned), (Signed | Unsigned) -> 
                            yield OpIMul(tid, id, lid, rid)

                        | Matrix _, Vector _ -> 
                            if reverse then yield OpVectorTimesMatrix(tid, id, rid, lid)
                            else yield OpMatrixTimesVector(tid, id, lid, rid)

                        | Vector _, Matrix _ -> 
                            if reverse then yield OpMatrixTimesVector(tid, id, rid, lid)
                            else yield OpVectorTimesMatrix(tid, id, lid, rid)

                        | Matrix _, Matrix _ -> 
                            if reverse then yield OpMatrixTimesMatrix(tid, id, rid, lid)
                            else yield OpMatrixTimesMatrix(tid, id, lid, rid)

                        | l, r ->
                            failwithf "[FShade] cannot multiply %A and %A" l r

                    return id

                | CDiv(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let lType = l.ctype
                    let rType = r.ctype
                    match lType, rType with
                        | CVector(et,d), Scalar ->
                            let! vid = newVector rType d rid
                            let! id = SpirV.id
                            match et with
                                | Floating -> yield OpFDiv(tid, id, lid, vid)
                                | Signed -> yield OpSDiv(tid, id, lid, vid)
                                | Unsigned -> yield OpUDiv(tid, id, lid, vid)
                                | _ -> failwith ""

                            return id

                        | Scalar, CVector(et, d) ->
                            let! vid = newVector lType d lid
                            let! id = SpirV.id
                            match et with
                                | Floating -> yield OpFDiv(tid, id, vid, rid)
                                | Signed -> yield OpSDiv(tid, id, vid, rid)
                                | Unsigned -> yield OpUDiv(tid, id, vid, rid)
                                | _ -> failwith ""

                            return id
                            
                        | Signed, Signed ->
                            let! id = SpirV.id
                            yield OpSDiv(tid, id, lid, rid)
                            return id

                        | Unsigned, Unsigned ->
                            let! id = SpirV.id
                            yield OpUDiv(tid, id, lid, rid)
                            return id

                        | Floating, Floating ->
                            let! id = SpirV.id
                            yield OpFDiv(tid, id, lid, rid)
                            return id

                        | _ -> 
                            return failwith "not implemented"

                | CMod(t, l, r) ->
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = SpirV.id
                    match t with
                        | Floating -> yield OpFMod(tid, id, l, r)
                        | Signed -> yield OpSMod(tid, id, l, r)
                        | Unsigned -> yield OpUMod(tid, id, l, r)
                        | _ -> failwithf "[SpirV] bad type in modulus %A" t
                    return id

                | CTranspose(t, v) ->
                    let! v = assembleExpr v
                    let! id = SpirV.id
                    yield OpTranspose(tid, id, v)
                    return id

                | CDot(t, l, r) ->
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = SpirV.id
                    yield OpDot(tid, id, l, r)
                    return id
                    
                | CCross(t, l, r) ->
                    let! glsl = SpirV.import glsl410
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = SpirV.id
                    yield OpExtInst(tid, id, 1u, uint32 GLSLExtInstruction.GLSLstd450Cross, [| l; r |])
                    return id

                | CVecSwizzle(t, v, fields) ->
                    let! v = assembleExpr v
                    let! id = SpirV.id
                    let comp = fields |> List.map uint32 |> List.toArray
                    yield OpVectorShuffle(tid, id, v, v, comp)
                    return id

                | CNewVector(t, d, args) ->
                    let! args = args |> List.mapS assembleExpr
                    let! id = SpirV.id
                    yield OpCompositeConstruct(tid, id, List.toArray args)
                    return id

                | CVecLength(t, v) ->
                    let! v = assembleExpr v
                    let! glsl = SpirV.import glsl410
                    let! id = SpirV.id
                    yield OpExtInst(tid, id, 1u, uint32 GLSLExtInstruction.GLSLstd450Length, [| v |])
                    return id

                | CMatrixElement _ | CMatrixFromCols _ | CMatrixFromRows _ ->
                    return failwith "not implemented"
                    
                | CConvert(t, e) ->
                    let! eid = assembleExpr e
                    let! id = SpirV.id
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
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdLessThan(tid, id, lid, rid)
                        | Signed -> yield OpSLessThan(tid, id, lid, rid)
                        | Unsigned -> yield OpULessThan(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CLequal(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdLessThanEqual(tid, id, lid, rid)
                        | Signed -> yield OpSLessThanEqual(tid, id, lid, rid)
                        | Unsigned -> yield OpULessThanEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CGreater(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdGreaterThan(tid, id, lid, rid)
                        | Signed -> yield OpSGreaterThan(tid, id, lid, rid)
                        | Unsigned -> yield OpUGreaterThan(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CGequal(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdGreaterThanEqual(tid, id, lid, rid)
                        | Signed -> yield OpSGreaterThanEqual(tid, id, lid, rid)
                        | Unsigned -> yield OpUGreaterThanEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CEqual(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdEqual(tid, id, lid, rid)
                        | Signed | Unsigned -> yield OpIEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CNotEqual(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
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
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    yield OpBitwiseAnd(tid, id, lid, rid)
                    return id

                | CBitOr(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    yield OpBitwiseOr(tid, id, lid, rid)
                    return id

                | CBitXor(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    yield OpBitwiseXor(tid, id, lid, rid)
                    return id

                | CLeftShift(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    yield OpShiftLeftLogical(tid, id, lid, rid)
                    return id

                | CRightShift(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    yield OpShiftRightLogical(tid, id, lid, rid)
                    return id

                | CAddressOf _ ->
                    return failwith "not implemented"

                | CField(t, e, f) ->
                    let! eid = assembleExpr e
                    let! fid = SpirV.tryGetFieldId e.ctype f
                    match fid with
                        | Some fid ->
                            let! id = SpirV.id
                            //let! iid = assembleLiteral (CType.CInt(true, 32)) (CIntegral (int64 fid))
                            yield OpCompositeExtract(tid, id, eid, [|uint32 fid|])
                            return id
                        | None ->
                            return failwithf "could not get field-id for %s on %A" f e.ctype

                | CItem(t, e, i) ->
                    let! eid = assembleExpr e
                    let! iid = assembleExpr i
                    let! id = SpirV.id
                    failwith "not implemented"
                    return id

        }

    let rec assembleRExpr (e : CRExpr) : SpirV<uint32> =
        spirv {
            match e with
                | CRExpr e -> 
                    return! assembleExpr e

                | CRArray(t, args) ->
                    let! tid = assembleType t
                    let! args = args |> List.mapS assembleExpr
                    let! id = SpirV.id
                    yield OpCompositeConstruct(tid, id, List.toArray args)
                    return id
        }

    let rec assembleLExpr (e : CLExpr) : SpirV<uint32> =
        spirv {
            let! tid = assemblePtrType StorageClass.Private e.ctype
            match e with
                | CLVar v ->
                    return! SpirV.getId v.name

                | CLExpr.CLField(_, e, f) ->
                    let! eid = assembleLExpr e
                    let! fid = SpirV.tryGetFieldId e.ctype f
                    match fid with
                        | Some fid ->
                            let! fid = assembleLiteral (CType.CInt(true, 32)) (CIntegral (int64 fid))
                            let! id = SpirV.id
                            yield OpInBoundsAccessChain(tid, id, eid, [| fid |])
                            return id
                        | None ->
                            return failwithf "[FShade] unknown field %A" f

                | CLExpr.CLItem(_, e, i) ->
                    let! eid = assembleLExpr e
                    let! iid = assembleExpr i
                    let! id = SpirV.id
                    yield OpInBoundsAccessChain(tid, id, eid, [| iid |])
                    return id

                | _ ->
                    return failwith "not implemented"
                    
        }
   
    let rec assembleStatement (lBreak : Option<uint32>) (lCont : Option<uint32>) (s : CStatement) : SpirV<unit> =
        spirv {
            match s with
                | CNop ->
                    ()

                | CDo e ->
                    let! id = assembleExpr e
                    ()

                | CDeclare(v, r) ->
                    let! t = assemblePtrType StorageClass.Private v.ctype
                    let! vid = SpirV.id

                    match r with
                        | Some r ->
                            let! rid = assembleRExpr r
                            yield OpVariable(t, vid, StorageClass.Private, Some rid)
                        | None ->
                            yield OpVariable(t, vid, StorageClass.Private, None)

                    do! SpirV.setId v.name vid

                | CWrite(l, v) ->
                    let! lid = assembleLExpr l
                    let! vid = assembleExpr v
                    yield OpStore(lid, vid, None)

                | CIncrement(_, v) ->
                    let t = v.ctype
                    let! tid = assembleType t
                    let! v = assembleLExpr v
                    let! c1 = assembleLiteral t (CIntegral 1L)
                    let! id0 = SpirV.id
                    let! id1 = SpirV.id
                    yield OpLoad(tid, id0, v, None)
                    yield OpIAdd(tid, id1, id0, c1)
                    yield OpStore(v, id1, None)

                | CDecrement(_, v) ->
                    let t = v.ctype
                    let! tid = assembleType t
                    let! v = assembleLExpr v
                    let! c1 = assembleLiteral t (CIntegral 1L)
                    let! id0 = SpirV.id
                    let! id1 = SpirV.id
                    yield OpLoad(tid, id0, v, None)
                    yield OpISub(tid, id1, id0, c1)
                    yield OpStore(v, id1, None)

                | CSequential ss | CIsolated ss ->
                    for s in ss do 
                        do! assembleStatement lBreak lCont s

                | CReturnValue v ->
                    let! v = assembleExpr v
                    yield OpReturnValue v

                | CReturn ->
                    yield OpReturn

                | CBreak ->
                    match lBreak with
                        | Some lBreak -> yield OpBranch lBreak
                        | _ -> failwith "break outside loop"

                | CContinue ->
                    match lCont with
                        | Some lCont -> yield OpBranch lCont
                        | _ -> failwith "continue outside loop"

                | CWriteOutput(name, index, value) ->
                    let! vid = assembleRExpr value
                    match index with
                        | Some _ -> failwith "not implemented"
                        | None ->
                            let! id = SpirV.getId (ParameterKind.Output, name)
                            yield OpStore(id, vid, None)

                | CFor(init, cond, step, body) ->
                    do! assembleStatement lBreak lCont init

                    let! lStart = SpirV.id
                    let! lBody = SpirV.id
                    let! lEnd = SpirV.id
                    let! lStep = SpirV.id

                    yield OpLabel lStart
                    let! c = assembleExpr cond
                    yield OpBranchConditional(c, lBody, lEnd, [||])
                    yield OpLabel lBody

                    do! assembleStatement (Some lEnd) (Some lStep) body

                    yield OpLabel lStep
                    do! assembleStatement lBreak lCont step
                    yield OpBranch lStart

                    yield OpLabel lEnd

                | CWhile(guard, body) ->
                    let! lStart = SpirV.id
                    let! lBody = SpirV.id
                    let! lEnd = SpirV.id

                    yield OpLabel lStart
                    let! v = assembleExpr guard
                    yield OpBranchConditional(v, lBody, lEnd, [||])
                    yield OpLabel lBody
                    do! assembleStatement (Some lEnd) (Some lStart) body
                    yield OpBranch lStart
                    yield OpLabel lEnd

                | CDoWhile(guard, body) ->
                    let! lStart = SpirV.id
                    let! lEnd = SpirV.id

                    yield OpLabel lStart
                    do! assembleStatement (Some lEnd) (Some lStart) body
                    let! v = assembleExpr guard
                    yield OpBranchConditional(v, lStart, lEnd, [||])
                    yield OpLabel lEnd
                    
                | CIfThenElse(guard, bTrue, bFalse) ->
                    let! g = assembleExpr guard
                    let! lTrue = SpirV.id
                    let! lFalse = SpirV.id
                    let! lEnd = SpirV.id

                    yield OpBranchConditional(g, lTrue, lFalse, [||])
                    yield OpLabel lTrue
                    do! assembleStatement lBreak lCont bTrue
                    yield OpBranch lEnd

                    yield OpLabel lFalse
                    do! assembleStatement lBreak lCont bFalse
                    yield OpLabel lEnd

                | CSwitch _ ->
                    return failwith "not implemented"
        }

    let tryGetBuiltIn (kind : ParameterKind) (stage : ShaderStageDescription) (name : string) : Option<BuiltIn> =
        Log.warn "not implemented"
        None


    let assembleFunctionType (args : array<uint32>) (ret : uint32) =
        SpirV.cached ("fun", args, ret) {  
            let! id = SpirV.id
            yield OpTypeFunction(id, ret, args)
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


            do! assembleStatement None None e.cBody

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


                    do! assembleStatement None None body

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
                currentBinding      = 0u
                currentSet          = 0u
                imports             = Map.empty
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





type Backend private() =
    inherit Compiler.Backend()
    static let instance = Backend()

    static member Instance = instance

    override x.TryGetIntrinsicMethod (c : MethodInfo) =
        None

    override x.TryGetIntrinsicCtor (c : ConstructorInfo) =
        None

    override x.TryGetIntrinsicType (t : Type) =
        None