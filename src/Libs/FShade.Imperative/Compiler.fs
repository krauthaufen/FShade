namespace FShade.Imperative

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.CompilerServices

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Aardvark.Base
open Aardvark.Base.TypeInfo
open Aardvark.Base.TypeInfo.Patterns

open FShade
open FShade.Imperative

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Compiler =
    open Aardvark.Base.Monads.State

    [<AllowNullLiteral; AbstractClass>]
    type Backend() =
        let intrinsicFunctions = System.Collections.Concurrent.ConcurrentDictionary<MethodBase, Option<CIntrinsic>>()
        let intrinsicTypes = System.Collections.Concurrent.ConcurrentDictionary<Type, Option<CIntrinsicType>>()

        abstract member TryGetIntrinsicMethod : MethodInfo -> Option<CIntrinsic>
        abstract member TryGetIntrinsicCtor : ConstructorInfo -> Option<CIntrinsic>
        abstract member TryGetIntrinsicType : Type -> Option<CIntrinsicType>

        interface IBackend with
            member x.TryGetIntrinsic (t : Type) = x.TryGetIntrinsic t

        member x.TryGetIntrinsic (m : MethodBase) =
            intrinsicFunctions.GetOrAdd(m, fun m ->
                match m with
                    | :? MethodInfo as mi -> x.TryGetIntrinsicMethod mi
                    | :? ConstructorInfo as ci -> x.TryGetIntrinsicCtor ci
                    | _ -> None
            )

        member x.TryGetIntrinsic (t : Type) =
            intrinsicTypes.GetOrAdd(t, fun t ->
                x.TryGetIntrinsicType t
            )

    type FunctionDefinition = 
        | ManagedFunction of name : string * args : list<Var> * body : Expr
        | CompiledFunction of signature : CFunctionSignature * body : CStatement
        | ManagedFunctionWithSignature of signature : CFunctionSignature * body : Expr

        member x.Signature (b : IBackend) =
            match x with
                | CompiledFunction(s,_) -> 
                    s

                | ManagedFunction(name,args,body) ->
                    CFunctionSignature.ofFunction b name args body.Type

                | ManagedFunctionWithSignature(s,_) ->
                    s

    type ConstantDefinition = { cName : string; cType : Type; cValue : Expr }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FunctionDefinition =
        let ofMethodBase (tryGetOverrideCode : MethodBase -> Option<Expr>) (mi : MethodBase) =
            let name = methodName mi
            match tryGetOverrideCode mi with
                | Some (Lambdas(args, body)) ->
                    ManagedFunction(name, List.concat args, body)
                | _ ->
                    match Expr.TryGetReflectedDefinition mi with
                        | Some (Lambdas(args, body)) ->
                            ManagedFunction(name, List.concat args, body)
                        | _ ->
                            failwithf "[FShade] cannot call function %A since it is not reflectable" mi

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Constructors =
        
        let cache = System.Collections.Concurrent.ConcurrentDictionary<IBackend * Type, FunctionDefinition>()
        let unionCache = System.Collections.Concurrent.ConcurrentDictionary<IBackend * Type, hmap<UnionCaseInfo, FunctionDefinition>>()
        let ctorCache = System.Collections.Concurrent.ConcurrentDictionary<IBackend * ConstructorInfo, FunctionDefinition>()

        let tuple (b : IBackend) (t : Type) =
            cache.GetOrAdd((b,t), fun (b,t) ->
                let cName = 
                    "new_" + typeName t

                let cParameters = 
                    FSharpType.GetTupleElements t
                        |> Array.map (CType.ofType b)
                        |> Array.mapi (fun i ct -> { name = sprintf "item%d" i; ctype = ct; modifier = CParameterModifier.In })

                let cType =
                    CType.ofType b t
            
                let cSignature =
                    {
                        name = cName
                        parameters = cParameters
                        returnType = cType
                    }

                let cDefinition =
                    let res = { name = "res"; ctype = cType }
                    let writeArgs = 
                        cParameters
                        |> Array.toList
                        |> List.mapi (fun i p -> 
                            let v = { name = sprintf "item%d" i; ctype = p.ctype }
                            CWrite(CLField(p.ctype, CLVar res, sprintf "Item%d" i), CExpr.CVar v)
                        )
                    CSequential [
                        yield CDeclare(res, None)
                        yield! writeArgs
                        yield CReturnValue (CExpr.CVar res)
                    ]

                CompiledFunction(cSignature, cDefinition)
            )

        let record (b : IBackend) (t : Type) =
            cache.GetOrAdd((b,t), fun (b,t) ->
                let cName = 
                    "new_" + typeName t

                let cFields = 
                    FSharpType.GetRecordFields(t, true)

                let cParameters =
                    cFields |> Array.map (fun pi ->
                        { name = pi.Name.ToLower(); ctype = CType.ofType b pi.PropertyType; modifier = CParameterModifier.In }
                    )   
                    
                let cType =
                    CType.ofType b t
            
                let cSignature =
                    {
                        name = cName
                        parameters = cParameters
                        returnType = cType
                    }

                let cDefinition =
                    let res = { name = "res"; ctype = cType }
                    let writeArgs = 
                        Array.zip cFields cParameters
                        |> Array.toList
                        |> List.mapi (fun i (f, p) -> 
                            let v = { name = p.name; ctype = p.ctype }
                            CWrite(CLField(p.ctype, CLVar res, f.Name), CExpr.CVar v)
                        )
                    CSequential [
                        yield CDeclare(res, None)
                        yield! writeArgs
                        yield CReturnValue (CExpr.CVar res)
                    ]

                CompiledFunction(cSignature, cDefinition)
            )

        let union (b : IBackend) (t : Type) =
            unionCache.GetOrAdd((b,t), fun (b,t) ->
                let cType = CType.ofType b t
                let cTypeName = typeName t
                
                FSharpType.GetUnionCases(t, true)
                    |> Seq.map (fun ci ->
                        
                        let cFields = ci.GetFields()

                        let cParameters =
                            cFields |> Array.map (fun pi ->
                                { name = pi.Name.ToLower(); ctype = CType.ofType b pi.PropertyType; modifier = CParameterModifier.In }
                            )  

                        let cType =
                            CType.ofType b t
            
                        let cSignature =
                            {
                                name = "new_" + cTypeName + "_" + ci.Name
                                parameters = cParameters
                                returnType = cType
                            }
                            
                        let cBody =
                            let res = { name = "res"; ctype = cType }
                            let writeArgs = 
                                Array.zip cFields cParameters
                                |> Array.toList
                                |> List.mapi (fun i (f, p) -> 
                                    let v = { name = p.name; ctype = p.ctype }
                                    CWrite(CLField(p.ctype, CLVar res, ci.Name + "_" + f.Name), CExpr.CVar v)
                                )
                            let tInt32 = CType.CInt(true, 32)
                            CSequential [
                                yield CDeclare(res, None)
                                yield CWrite(CLField(tInt32, CLVar res, "tag"), CValue(tInt32, CIntegral(int64 ci.Tag)))
                                yield! writeArgs
                                yield CReturnValue (CExpr.CVar res)
                            ]

                        let cDefinition = CompiledFunction(cSignature, cBody)
                        (ci, cDefinition)
                    )
                    |> HMap.ofSeq
            )

        let custom (b : IBackend) (ctor : ConstructorInfo) =
            let baseType = ctor.DeclaringType.BaseType
            if baseType <> typeof<obj> && baseType <> typeof<ValueType> then
                failwithf "[FShade] cannot compile constructor for OOP-style type inheriting from a different class"

            let preprocessCtor (e : Expr) =
                let rec preprocess (this : Var) (arg : Var) (e : Expr) =
                    match e with
                        | Let(v, TupleGet(Var(a), i), e) when a = arg ->
                            let args, body = preprocess this arg e
                            v::args, body

                        | Sequential(NewObject(ctor, args), rest) when ctor.DeclaringType = baseType ->
                            [], Expr.Let(this, Expr.DefaultValue(this.Type), rest)

                        | _ -> 
                            failwithf "[FShade] unexpected constructor definition %A" e

                let this = e.GetFreeVars() |> Seq.exactlyOne
                match e with
                    | Lambda(arg, body) -> 
                        preprocess this arg body
                    | _ ->
                        failwithf "[FShade] unexpected constructor definition %A" e

            ctorCache.GetOrAdd((b,ctor), fun (b, ctor) ->
                match Expr.TryGetReflectedDefinition ctor with
                    | Some e ->
                        let args, body = preprocessCtor e
                        let argTypeNames = args |> List.map (fun a -> typeName a.Type) |> String.concat "_"
                        let cName = "new_" + typeName ctor.DeclaringType + "_of_" + argTypeNames
                        ManagedFunction(cName, args, body)
                    | None ->
                        failwithf "[FShade] cannot compile constructor without reflected definition %A" ctor
            )

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Helpers = 
        let rec tryDeconstructValue (t : Type) (v : obj) =
            if t.IsEnum then
                Expr.Value(unbox<int> v, t) |> Some

            elif FSharpType.IsTuple t then
                let elements = FSharpType.GetTupleElements t |> Array.toList
                let arguments =
                    elements |> List.mapi (fun i t ->
                        Expr.Value(FSharpValue.GetTupleField(v, i), t)
                    )
                Expr.NewTuple(arguments) |> Some

            elif FSharpType.IsRecord(t, true) then
                let fields = FSharpType.GetRecordFields(t, true) |> Array.toList
                let arguments = 
                    fields |> List.map (fun pi ->
                        Expr.Value(pi.GetValue(v), pi.PropertyType)
                    )
                Expr.NewRecord(t, arguments) |> Some

            elif FSharpType.IsUnion(t, true) then
                let case, fields = FSharpValue.GetUnionFields(v, t)
                let arguments = 
                    List.map2 (fun (value : obj) (pi : PropertyInfo) -> Expr.Value(value, pi.PropertyType)) (Array.toList fields) (Array.toList (case.GetFields()))
      
                Expr.NewUnionCase(case, arguments) |> Some

            else 
                match v with
                    | VectorValue(bt, args) ->
                        let ctor = t.GetConstructor(Array.create args.Length bt)
                        Expr.NewObject(ctor, args |> Array.toList |> List.map (fun v -> Expr.Value(v, bt))) |> Some
                    | _ ->
                        None

        let rec zero (t : CType) =
            match t with
                | CVoid             -> CValue(t, CLiteral.Null)
                | CType.CBool       -> CValue(t, CLiteral.CBool false)
                | CInt _            -> CValue(t, CIntegral 0L)
                | CFloat _          -> CValue(t, CFractional 0.0)
                | CVector(bt, d)    -> CNewVector(t, d, List.replicate d (zero bt))
                | CMatrix(bt, r, c) -> CMatrixFromRows(t, zero (CVector(bt, c)) |> List.replicate r)
                | _                 -> failwithf "[FShade] cannot create zero-value for type %A" t

        let rec one (t : CType) =
            match t with
                | CType.CBool       -> CValue(t, CLiteral.CBool true)
                | CInt _            -> CValue(t, CIntegral 1L)
                | CFloat _          -> CValue(t, CFractional 1.0)
                | CVector(bt, d)    -> CNewVector(t, d, List.replicate d (one bt))
                | _                 -> failwithf "[FShade] cannot create one-value for type %A" t

        module private Generators =         
            let rec allSubsets (l : list<'a>) =
                match l with
                    | [] -> [[]]
                    | h :: rest ->
                        let rest = allSubsets rest

                        [
                            yield! rest
                            for r in rest do
                                for i in 0 .. List.length r do
                                    let l, r = List.splitAt i r
                                    yield l @ [h] @ r

                        ]

            let allSwizzles (len : int) (l : list<string>) =
                l |> allSubsets 
                    |> List.filter (fun c -> List.length c = len)
                    |> List.map (fun c ->
                        let m = "get_" + String.concat "" c
                        let c = c |> List.map (sprintf "CVecComponent.%s") |> String.concat "; "
                        sprintf "| Method(\"%s\", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [%s]) |> Some" m c
                    )
                    |> String.concat "\r\n"

        let rec tryGetBuiltInMethod (b : IBackend) (mi : MethodInfo) (args : list<CExpr>) =
            let ct = CType.ofType b mi.ReturnType
            match mi, args with
                | Method("op_UnaryNegation", _), [l]        -> CExpr.CNeg(ct, l) |> Some
                | MethodQuote <@ not @> _, [l]              -> CExpr.CNot(ct, l) |> Some

                | Method("op_Addition", _), [l;r]           -> CExpr.CAdd(ct, l, r) |> Some
                | Method("op_Subtraction", _), [l;r]        -> CExpr.CSub(ct, l, r) |> Some
                | Method("op_Division", _), [l;r]           -> CExpr.CDiv(ct, l, r) |> Some
                | Method("op_Modulus", _), [l;r]            -> CExpr.CMod(ct, l, r) |> Some

                | Method("op_LeftShift", _), [l;r]          -> CExpr.CLeftShift(ct, l, r) |> Some
                | Method("op_RightShift", _), [l;r]         -> CExpr.CRightShift(ct, l, r) |> Some
            
                | Method("op_Multiply", _), [l;r] ->
                    let lt = l.ctype
                    let rt = r.ctype 
                    match lt, rt with
                        | CMatrix _, CMatrix _              -> CMulMatMat(ct, l, r) |> Some
                        | CMatrix _, CVector _              -> CMulMatVec(ct, l, r) |> Some
                        | CVector _, CMatrix(b,rows,cols)   -> CMulMatVec(ct, CTranspose(CMatrix(b,cols,rows), r), l) |> Some
                        | _                                 -> CExpr.CMul(ct, l, r) |> Some

                // transpose
                | MethodQuote <@ Mat.transpose : M44d -> M44d @> _, [m]    
                | Method("Transpose", [MatrixOf _]) , [m]
                | Method("get_Transposed", _), [m] -> 
                    match ct with
                        | CMatrix(b,r,c) -> CTranspose(CMatrix(b,c,r), m) |> Some
                        | _ -> None
                   
                // dot         
                | MethodQuote <@ Vec.dot : V4d -> V4d -> float @> _, [l;r]
                | Method("Dot", [VectorOf _; VectorOf _]), [l;r] ->
                    CDot(ct, l, r) |> Some
              
                // length         
                | MethodQuote <@ Vec.length : V4d -> float @> _, [v]
                | Method("get_Length", [VectorOf _]), [v] ->
                    CVecLength(ct, v) |> Some

                | Method("get_Length", [ArrOf(l,_)]), [a] ->
                    CValue(CType.CInt(true, 32), CIntegral (int64 l)) |> Some

                 // lengthSquared     
                | MethodQuote <@ Vec.lengthSquared : V4d -> float @> _, [v]
                | Method("get_LengthSquared", [VectorOf _]), [v] ->
                    CDot(ct, v, v) |> Some
                               

                // cross              
                | MethodQuote <@ Vec.cross : V3d -> V3d -> V3d @> _, [l;r]
                | Method("Cross", [VectorOf _; VectorOf _]), [l;r] ->
                    CCross(ct, l, r) |> Some

                // transformDir
                | MethodQuote <@ Mat.transformDir : M44d -> V3d -> V3d @> _, [m;v] 
                | Method("TransformDir", [MatrixOf _; VectorOf _]), [m;v] ->
                    match ct, v.ctype with
                        | CVector(rt, rd), CVector(t, d) ->
                            let res = CMulMatVec(CVector(rt, rd + 1), m, CNewVector(CVector(t, d + 1), d, [v; zero t]))
                            CVecSwizzle(ct, res, CVecComponent.first rd) |> Some
                        | _ ->
                            None

                // transformPos
                | MethodQuote <@ Mat.transformPos : M44d -> V3d -> V3d @> _, [m;v] 
                | Method("TransformPos", [MatrixOf _; VectorOf _]), [m;v] ->
                    match ct, v.ctype with
                        | CVector(rt, rd), CVector(t, d) ->
                            let res = CMulMatVec(CVector(rt, rd + 1), m, CNewVector(CVector(t, d + 1), d, [v; one t]))
                            CVecSwizzle(ct, res, CVecComponent.first rd) |> Some
                        | _ ->
                            None

                // vector swizzles
                | (MethodQuote <@ Vec.xy : V4d -> V2d @> _ ), [v] -> CVecSwizzle(ct, v, CVecComponent.xy) |> Some
                | (MethodQuote <@ Vec.yz : V4d -> V2d @> _), [v] -> CVecSwizzle(ct, v, CVecComponent.yz) |> Some
                | (MethodQuote <@ Vec.zw : V4d -> V2d @> _), [v] -> CVecSwizzle(ct, v, CVecComponent.zw) |> Some
                | (MethodQuote <@ Vec.xyz : V4d -> V3d @> _), [v] -> CVecSwizzle(ct, v, CVecComponent.xyz) |> Some
                | (MethodQuote <@ Vec.yzw : V4d -> V3d @> _), [v] -> CVecSwizzle(ct, v, CVecComponent.yzw) |> Some


                | Method("get_ZW", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Z; CVecComponent.W]) |> Some
                | Method("get_WZ", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.W; CVecComponent.Z]) |> Some
                | Method("get_YW", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Y; CVecComponent.W]) |> Some
                | Method("get_WY", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.W; CVecComponent.Y]) |> Some
                | Method("get_YZ", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Y; CVecComponent.Z]) |> Some
                | Method("get_ZY", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Z; CVecComponent.Y]) |> Some
                | Method("get_XW", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.X; CVecComponent.W]) |> Some
                | Method("get_WX", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.W; CVecComponent.X]) |> Some
                | Method("get_XZ", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.X; CVecComponent.Z]) |> Some
                | Method("get_ZX", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Z; CVecComponent.X]) |> Some
                | Method("get_XY", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.X; CVecComponent.Y]) |> Some
                | Method("get_YX", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Y; CVecComponent.X]) |> Some


                | Method("get_YZW", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Y; CVecComponent.Z; CVecComponent.W]) |> Some
                | Method("get_ZYW", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Z; CVecComponent.Y; CVecComponent.W]) |> Some
                | Method("get_ZWY", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Z; CVecComponent.W; CVecComponent.Y]) |> Some
                | Method("get_YWZ", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Y; CVecComponent.W; CVecComponent.Z]) |> Some
                | Method("get_WYZ", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.W; CVecComponent.Y; CVecComponent.Z]) |> Some
                | Method("get_WZY", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.W; CVecComponent.Z; CVecComponent.Y]) |> Some
                | Method("get_XZW", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.X; CVecComponent.Z; CVecComponent.W]) |> Some
                | Method("get_ZXW", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Z; CVecComponent.X; CVecComponent.W]) |> Some
                | Method("get_ZWX", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Z; CVecComponent.W; CVecComponent.X]) |> Some
                | Method("get_XWZ", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.X; CVecComponent.W; CVecComponent.Z]) |> Some
                | Method("get_WXZ", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.W; CVecComponent.X; CVecComponent.Z]) |> Some
                | Method("get_WZX", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.W; CVecComponent.Z; CVecComponent.X]) |> Some
                | Method("get_XYW", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.X; CVecComponent.Y; CVecComponent.W]) |> Some
                | Method("get_YXW", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Y; CVecComponent.X; CVecComponent.W]) |> Some
                | Method("get_YWX", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Y; CVecComponent.W; CVecComponent.X]) |> Some
                | Method("get_XWY", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.X; CVecComponent.W; CVecComponent.Y]) |> Some
                | Method("get_WXY", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.W; CVecComponent.X; CVecComponent.Y]) |> Some
                | Method("get_WYX", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.W; CVecComponent.Y; CVecComponent.X]) |> Some
                | Method("get_XYZ", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.X; CVecComponent.Y; CVecComponent.Z]) |> Some
                | Method("get_YXZ", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Y; CVecComponent.X; CVecComponent.Z]) |> Some
                | Method("get_YZX", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Y; CVecComponent.Z; CVecComponent.X]) |> Some
                | Method("get_XZY", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.X; CVecComponent.Z; CVecComponent.Y]) |> Some
                | Method("get_ZXY", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Z; CVecComponent.X; CVecComponent.Y]) |> Some
                | Method("get_ZYX", [VectorOf _]), [v] -> CVecSwizzle(ct, v, [CVecComponent.Z; CVecComponent.Y; CVecComponent.X]) |> Some

                // matrix creation
                | Method("FromRows", _), rows -> CMatrixFromRows(ct, rows) |> Some
                | Method("FromCols", _), rows -> CMatrixFromCols(ct, rows) |> Some

                // matrix swizzles
                | Method("get_R0", [MatrixOf _]), [m] -> 
                    match m.ctype, ct with
                        | CMatrix(_,r,c), CVector(t, d) -> CNewVector(ct, d, List.init c (fun c -> CMatrixElement(t, m, 0, c))) |> Some
                        | _ -> None

                | Method("get_R1", [MatrixOf _]), [m] -> 
                    match m.ctype, ct with
                        | CMatrix(_,r,c), CVector(t, d) -> CNewVector(ct, d, List.init c (fun c -> CMatrixElement(t, m, 1, c))) |> Some
                        | _ -> None
                    
                | Method("get_R2", [MatrixOf _]), [m] -> 
                    match m.ctype, ct with
                        | CMatrix(_,r,c), CVector(t, d) -> CNewVector(ct, d, List.init c (fun c -> CMatrixElement(t, m, 2, c))) |> Some
                        | _ -> None
                    
                | Method("get_R3", [MatrixOf _]), [m] -> 
                    match m.ctype, ct with
                        | CMatrix(_,r,c), CVector(t, d) -> CNewVector(ct, d, List.init c (fun c -> CMatrixElement(t, m, 3, c))) |> Some
                        | _ -> None

                | Method("get_C0", [MatrixOf _]), [m] -> 
                    match m.ctype, ct with
                        | CMatrix(_,r,c), CVector(t, d) -> CNewVector(ct, d, List.init r (fun r -> CMatrixElement(t, m, r, 0))) |> Some
                        | _ -> None

                | Method("get_C1", [MatrixOf _]), [m] -> 
                    match m.ctype, ct with
                        | CMatrix(_,r,c), CVector(t, d) -> CNewVector(ct, d, List.init r (fun r -> CMatrixElement(t, m, r, 1))) |> Some
                        | _ -> None

                | Method("get_C2", [MatrixOf _]), [m] -> 
                    match m.ctype, ct with
                        | CMatrix(_,r,c), CVector(t, d) -> CNewVector(ct, d, List.init r (fun r -> CMatrixElement(t, m, r, 2))) |> Some
                        | _ -> None

                | Method("get_C3", [MatrixOf _]), [m] -> 
                    match m.ctype, ct with
                        | CMatrix(_,r,c), CVector(t, d) -> CNewVector(ct, d, List.init r (fun r -> CMatrixElement(t, m, r, 3))) |> Some
                        | _ -> None




                | Method("op_BooleanAnd", _), [l;r]         -> CExpr.CAnd(l, r) |> Some
                | Method("op_BooleanOr", _), [l;r]          -> CExpr.COr(l, r) |> Some

                | Method("op_BitwiseAnd", _), [l;r]         -> CExpr.CBitAnd(ct, l, r) |> Some
                | Method("op_BitwiseOr", _), [l;r]          -> CExpr.CBitOr(ct, l, r) |> Some
                | Method("op_ExclusiveOr", _), [l;r]        -> CExpr.CBitXor(ct, l, r) |> Some

                | Method("op_LessThan", _), [l;r]           -> CExpr.CLess(l, r) |> Some
                | Method("op_LessThanOrEqual", _), [l;r]    -> CExpr.CLequal(l, r) |> Some
                | Method("op_GreaterThan", _), [l;r]        -> CExpr.CGreater(l, r) |> Some
                | Method("op_GreaterThanOrEqual", _), [l;r] -> CExpr.CGequal(l, r) |> Some
                | Method("op_Equality", _), [l;r]           -> CExpr.CEqual(l, r) |> Some
                | Method("op_Inequality", _), [l;r]         -> CExpr.CNotEqual(l, r) |> Some

                | Method("get_Item", [ArrOf(_,_); _]), [arr; index]
                | Method("GetArray", _), [arr; index] -> 
                    CExpr.CItem(ct, arr, index) |> Some
                    

                | ConversionMethod(_,o), [arg]              -> CExpr.CConvert(CType.ofType b o, arg) |> Some
                | _ -> None

        let rec tryGetBuiltInCtor (b : IBackend) (ctor : ConstructorInfo) (args : list<CExpr>) =
            match ctor.DeclaringType with
                | VectorOf(d, t) ->
                    CNewVector(CType.ofType b ctor.DeclaringType, d, args) |> Some

                | _ ->
                    None

        let tryGetBuiltInField (b : IBackend) (fi : FieldInfo) (arg : CExpr) =
            let ct = CType.ofType b fi.FieldType
            match fi.DeclaringType, fi.Name with
                | VectorOf _, "X" -> CVecSwizzle(ct, arg, [CVecComponent.X]) |> Some
                | VectorOf _, "Y" -> CVecSwizzle(ct, arg, [CVecComponent.Y]) |> Some
                | VectorOf _, "Z" -> CVecSwizzle(ct, arg, [CVecComponent.Z]) |> Some
                | VectorOf _, "W" -> CVecSwizzle(ct, arg, [CVecComponent.W]) |> Some

                | MatrixOf _, "M00" -> CMatrixElement(ct, arg, 0, 0) |> Some
                | MatrixOf _, "M01" -> CMatrixElement(ct, arg, 0, 1) |> Some
                | MatrixOf _, "M02" -> CMatrixElement(ct, arg, 0, 2) |> Some
                | MatrixOf _, "M03" -> CMatrixElement(ct, arg, 0, 3) |> Some
                | MatrixOf _, "M10" -> CMatrixElement(ct, arg, 1, 0) |> Some
                | MatrixOf _, "M11" -> CMatrixElement(ct, arg, 1, 1) |> Some
                | MatrixOf _, "M12" -> CMatrixElement(ct, arg, 1, 2) |> Some
                | MatrixOf _, "M13" -> CMatrixElement(ct, arg, 1, 3) |> Some
                | MatrixOf _, "M20" -> CMatrixElement(ct, arg, 2, 0) |> Some
                | MatrixOf _, "M21" -> CMatrixElement(ct, arg, 2, 1) |> Some
                | MatrixOf _, "M22" -> CMatrixElement(ct, arg, 2, 2) |> Some
                | MatrixOf _, "M23" -> CMatrixElement(ct, arg, 2, 3) |> Some
                | MatrixOf _, "M30" -> CMatrixElement(ct, arg, 3, 0) |> Some
                | MatrixOf _, "M31" -> CMatrixElement(ct, arg, 3, 1) |> Some
                | MatrixOf _, "M32" -> CMatrixElement(ct, arg, 3, 2) |> Some
                | MatrixOf _, "M33" -> CMatrixElement(ct, arg, 3, 3) |> Some

                | _ -> None

        let tryGetBuiltInStaticProperty (t : CType) (pi : PropertyInfo) =
            match pi.Name with
                | "Zero"    -> zero t |> Some
                | "One"     -> one t |> Some
                | _         -> None


    type ModuleState =
        {
            globalNameIndices   : Map<string, int>
            backend             : Backend
            constantIndex       : int
            usedTypes           : hmap<obj, CType>

            globalFunctions     : hmap<obj, FunctionDefinition>
            globalConstants     : hmap<obj, ConstantDefinition>

            globalParameters    : Set<string>
            tryGetOverrideCode  : MethodBase -> Option<Expr>
        }

    type CompilerState =
        {
            nameIndices         : Map<string, int>
            variables           : Map<Var, CVar>
            reservedNames       : Set<string>

            usedFunctions       : hmap<obj, FunctionDefinition>
            usedConstants       : hset<ConstantDefinition>
            usedGlobals         : hset<string>
            moduleState         : ModuleState
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CompilerState =

        let newGlobalName (name : string) =
            State.custom (fun s ->
                let m = s.moduleState

                let newM,res = 
                    match Map.tryFind name m.globalNameIndices with
                        | Some index ->
                            let state = { m with globalNameIndices = Map.add name (index + 1) m.globalNameIndices }
                            let name = name + string index
                            state, name
                        | None ->
                            let state = { m with globalNameIndices = Map.add name 1 m.globalNameIndices }
                            state, name

                { s with moduleState = newM }, res
            )


        let newName (name : string) =
            State.custom (fun s ->
                match Map.tryFind name s.nameIndices with
                    | Some index ->
                        let state = { s with nameIndices = Map.add name (index + 1) s.nameIndices }
                        let name = name + string index
                        state, name
                    | None ->
                        let state = { s with nameIndices = Map.add name 1 s.nameIndices }
                        state, name
            )

        let lastName (name : string) =
            State.get |> State.map (fun s ->
                match Map.tryFind name s.nameIndices with
                    | None | Some 1 -> name
                    | Some idx -> name + string idx
            )

//        let variableName (v : Var) =
//            State.custom (fun s ->
//                match Map.tryFind v s.variables with
//                    | Some name -> 
//                        s, name
//                    | None ->
//                        match Map.tryFind v.Name s.nameIndices with
//                            | Some index ->
//                                let name = v.Name + string index
//                                let state = { s with nameIndices = Map.add v.Name (index + 1) s.nameIndices; variableNames = Map.add v name s.variableNames }
//                                state, name
//                            | None ->
//                                let state = { s with nameIndices = Map.add v.Name 1 s.nameIndices; variableNames = Map.add v v.Name s.variableNames }
//                                state, v.Name
//
//            )

        let reserveName (n : string) =
            State.modify (fun s ->
                { s with reservedNames = Set.add n s.reservedNames; nameIndices = Map.add n 1 s.nameIndices }
            )

        let useLocalFunction (key : obj) (f : FunctionDefinition) =
            State.custom (fun s ->
                { s with usedFunctions = HMap.add key f s.usedFunctions }, f.Signature s.moduleState.backend
            )

        let useGlobalFunction (key : obj) (f : FunctionDefinition) =
            State.custom (fun s ->
                { s with moduleState = { s.moduleState with globalFunctions = HMap.add key f s.moduleState.globalFunctions } }, f.Signature s.moduleState.backend
            )

        let useCtor (key : obj) (f : FunctionDefinition) =
            useGlobalFunction ((key, "ctor") :> obj) f

        let useConstant (key : obj) (e : Expr) =
            state {
                let! s = State.get
                let ct = CType.ofType s.moduleState.backend e.Type
                match HMap.tryFind key s.moduleState.globalConstants with
                    | None -> 
                        let name = sprintf "_constant%d" s.moduleState.constantIndex
                        let c = { cName = name; cType = e.Type; cValue = e }
                        do! State.put { 
                                s with 
                                    moduleState = { s.moduleState with globalConstants = HMap.add key c s.moduleState.globalConstants; constantIndex = s.moduleState.constantIndex + 1 } 
                                    usedConstants = HSet.add c s.usedConstants
                            }
                    
                        return CVar { name = name; ctype = ct }
                    | Some c ->
                        do! State.put { s with usedConstants = HSet.add c s.usedConstants }
                        return CVar { name = c.cName; ctype = ct }

            }

        let tryGetIntrinsic (mi : MethodBase) =
            State.get |> State.map (fun s ->
                if isNull s.moduleState.backend then None
                else s.moduleState.backend.TryGetIntrinsic mi
            )

        type Free =
            | Variable of Var
            | Global of kind : ParameterKind * name : string * _type : Type * index : Option<Expr>

        let rec free (e : Expr) : hset<Free> =
            match e with
                | Let(v,e,b) ->
                    let fe = free e
                    let fb = free b
                    HSet.union fe (HSet.remove (Variable v) fb)

                | ReadInput(kind,name,idx) ->
                    match idx with
                        | Some idx -> free idx |> HSet.add (Global(kind, name, e.Type, Some idx))
                        | None -> HSet.ofList [ Global(kind, name, e.Type, None) ]

                | WriteOutputs values ->
                    let mutable res = HSet.empty
                    for (name, (index, value)) in Map.toSeq values do
                        match index with
                            | Some index -> res <- HSet.union res (free index)
                            | _ -> ()
                        res <- res |> HSet.union (free value) |> HSet.add (Global(ParameterKind.Output, name, value.Type, index))

                    res

                | ExprShape.ShapeCombination(o, args) ->
                    args |> List.fold (fun m e -> HSet.union m (free e)) HSet.empty

                | ExprShape.ShapeLambda(v, b) ->
                    free b |> HSet.remove (Variable v)

                | ExprShape.ShapeVar v ->
                    HSet.ofList [ Variable v ]

    let emptyState (m : ModuleState) =
        {
            nameIndices         = Map.empty
            variables           = Map.empty
            reservedNames       = Set.empty
            usedFunctions       = HMap.empty
            usedConstants       = HSet.empty
            usedGlobals         = HSet.empty
            moduleState         = m
        }


    let rec toCTypeS (t : Type) =
        State.custom (fun s ->
            let cType = CType.ofType s.moduleState.backend t

            let mutable usedTypes = s.moduleState.usedTypes
            let rec visit (t : CType) =
                match t with
                    | CVector(i,_) | CMatrix(i,_,_) | CArray(i,_) | CPointer(_,i) ->
                        visit i

                    | CStruct(_, fields, _) ->
                        usedTypes <- HMap.add (t :> obj) cType usedTypes
                        for (f,_) in fields do
                            visit f

                    | _ -> ()

            visit cType
            
            { s with moduleState = { s.moduleState with usedTypes = usedTypes } }, cType
        )
        
    /// converts a variable to a CVar using the cached name or by
    /// creating a fresh name for it (and caching it)
    let toCVarOfTypeS (v : Var) (t : CType) =
        State.custom (fun s ->
            match Map.tryFind v s.variables with
                | Some v -> s, v
                | None ->
                    match Map.tryFind v.Name s.nameIndices with
                        | Some index ->
                            let name = v.Name + string index
                            let res = { ctype = t; name = name }
                            let state = { s with nameIndices = Map.add v.Name (index + 1) s.nameIndices; variables = Map.add v res s.variables }
                            state, res
                        | None ->
                            let res = { ctype = t; name = v.Name }
                            let state = { s with nameIndices = Map.add v.Name 1 s.nameIndices; variables = Map.add v res s.variables }
                            state, res
        )

    let toCVarS (v : Var) =
        state {
            let! t = toCTypeS v.Type
            return! toCVarOfTypeS v t
        }

    /// converts an EntryParameter to a CEntryParameter 
    /// NOTE that names are not treated here
    let toCEntryParameterS (p : EntryParameter) =
        state {
            let! ctype = toCTypeS p.paramType
            return {
                cParamType           = ctype
                cParamName           = p.paramName
                cParamSemantic       = p.paramSemantic
                cParamDecorations    = p.paramDecorations
            }
        }
        
    /// converts an Uniform to a CUniform
    /// NOTE that names are not treated here
    let toCUniformS (v : Uniform) =
        state {
            let! ct = toCTypeS v.uniformType
            return {
                cUniformType = ct
                cUniformName = v.uniformName
                cUniformBuffer = v.uniformBuffer
                cUniformDecorations = v.uniformDecorations
            }
        }

    let rec asExternalS (e : Expr) =
        state {
            let free = CompilerState.free e
            if HSet.isEmpty free then
                return! CompilerState.useConstant e e
            else
                let! globals = State.get |> State.map (fun s -> s.moduleState.globalParameters)
                let free = free |> HSet.toArray
 
                let newVariables = HashSet()

                let freeArgs = 
                    free |> Array.choose (fun f ->
                        match f with
                            | CompilerState.Free.Variable v ->
                                Some v

                            | CompilerState.Free.Global(kind, n, t, idx) ->
                                if kind = ParameterKind.Uniform && Set.contains n globals then 
                                    None
                                else 
                                    let v = Var(n, t, (kind = ParameterKind.Output))
                                    newVariables.Add v |> ignore
                                    Some v
                    )


                let! parameters =
                    freeArgs |> Array.mapS (fun v ->
                        state {
                            let! t = toCTypeS v.Type
                            return { name = v.Name; ctype = t; modifier = (if v.IsMutable then CParameterModifier.ByRef else CParameterModifier.In) }
                        }
                    )   
                    
                let! args = 
                    free |> Array.mapS (fun f ->
                        state {
                            match f with
                                | CompilerState.Free.Variable v ->
                                    let! v = toCVarS v
                                    return Some (CVar v)

                                | CompilerState.Free.Global(kind, n, t, idx) ->
                                    if kind = ParameterKind.Uniform && Set.contains n globals then 
                                        return None
                                    else 
                                        match kind with
                                            | ParameterKind.Input | ParameterKind.Uniform ->
                                                let expression = 
                                                    match idx with
                                                        | Some idx -> Expr.ReadInput(kind, t, n, idx)
                                                        | None -> Expr.ReadInput(kind, t, n)
                                                let! e = toCExprS expression
                                                return Some e
                                            | _ ->
                                                return failwithf "[FShade] cannot use output %A as closure in function" n
                        }
                    ) 

                let args = Array.choose id args

                let! name = CompilerState.newGlobalName "helper"
                let! returnType = toCTypeS e.Type

                let signature =
                    {
                        name = name
                        returnType = returnType
                        parameters = parameters
                    }

                let definition = ManagedFunctionWithSignature(signature, e)

                let! signature = 
                    if freeArgs.Length = free.Length then CompilerState.useGlobalFunction (e :> obj) definition
                    else CompilerState.useLocalFunction (e :> obj) definition


                return CCall(signature, args)
        }

    and toCExprS (e : Expr) : State<_, CExpr> =
        state {

            match e with
                | ReducibleExpression e ->
                    return! toCExprS e

                | NewFixedArray _
                | AddressSet _
                | DefaultValue _
                | FieldSet _
                | ForInteger _
                | LetRecursive _
                | Let _
                | NewArray _
                | PropertySet _
                | Sequential _
                | TryFinally _
                | TryWith _
                | VarSet _
                | WhileLoop _ ->
                    return! asExternalS e

                | ReadInput(kind, name, index) ->
                    let! ct = toCTypeS e.Type
                    let! s = State.get
                    if Set.contains name s.moduleState.globalParameters then
                        do! State.put { s with usedGlobals = HSet.add name s.usedGlobals }

                    match index with
                        | Some idx -> 
                            let! idx = toCExprS idx
                            return CReadInput(kind, ct, name, Some idx)
                        | _ ->
                            return CReadInput(kind, ct, name, None)


                | Var v ->
                    let! v = toCVarS v
                    return CVar v

                | Value(v, t) ->
                    let! ct = toCTypeS t
                    match CLiteral.tryCreate v with
                        | Some literal -> 
                            return CValue(ct, literal)

                        | None ->
                            match Helpers.tryDeconstructValue t v with
                                | Some e -> 
                                    return! toCExprS e

                                | _ -> 
                                    let! e = asExternalS e
                                    return e

                | GetArray(arr, i) ->
                    let! ct = toCTypeS e.Type
                    let! arr = toCExprS arr
                    let! i = toCExprS i
                    return CItem(ct, arr, i)

                | NewTuple(fields) ->
                    let! s = State.get
                    let! ctor = e.Type |> Constructors.tuple s.moduleState.backend |> CompilerState.useCtor e.Type
                    let! fields = fields |> List.mapS toCExprS |>> List.toArray
                    return CCall(ctor, fields)

                | NewRecord(t, fields) ->
                    let! s = State.get
                    let! ctor = t |> Constructors.record s.moduleState.backend |> CompilerState.useCtor t
                    let! fields = fields |> List.mapS toCExprS |>> List.toArray
                    return CCall(ctor, fields)

                | NewUnionCase(ci, fields) ->
                    let! s = State.get
                    let ctors = ci.DeclaringType |> Constructors.union s.moduleState.backend
                    let! ctor = ctors |> HMap.find ci |> CompilerState.useGlobalFunction ci
                    let! fields = fields |> List.mapS toCExprS |>> List.toArray
                    return CCall(ctor, fields)

                | NewObject(ctor, args) ->
                    let! s = State.get
                    let! args = args  |> List.mapS toCExprS
                    match Helpers.tryGetBuiltInCtor s.moduleState.backend ctor args with
                        | Some b ->
                            return b
                        | None -> 
                            let! ct = toCTypeS e.Type
                            let! intrinsic = CompilerState.tryGetIntrinsic ctor
                            match intrinsic with
                                | Some i -> 
                                    return CCallIntrinsic(ct, i, List.toArray args)
                                | None -> 
                                    let! ctor = ctor |> Constructors.custom s.moduleState.backend |> CompilerState.useGlobalFunction ctor
                                    return CCall(ctor, List.toArray args)


                | UnionCaseTest(e, ci) ->
                    let! e = toCExprS e
                    let tInt32 = CInt(true, 32)
                    return CEqual(CField(tInt32, e, "tag"), CValue(tInt32, CLiteral.CIntegral(int64 ci.Tag)))

                | TupleGet(t, i) ->
                    let! ct = toCTypeS e.Type
                    let! t = toCExprS t
                    return CField(ct, t, sprintf "Item%d" i)


                | Call(None, mi, []) ->
                    let! intrinsic = CompilerState.tryGetIntrinsic mi
                    match intrinsic with
                        | Some i -> 
                            let! ct = toCTypeS e.Type
                            return CCallIntrinsic(ct, i, [||])
                        | None -> 
                            // TODO: assumes that the function does not have side-effects
                            return! Expr.Value(mi.Invoke(null, [||]), mi.ReturnType) |> toCExprS

                | Call(None, mi, t :: args) | Call(Some t, mi, args) ->
                    let args = t :: args
                    let! args = args |> List.mapS toCExprS
                    let! s = State.get
                    match Helpers.tryGetBuiltInMethod s.moduleState.backend mi args with
                        | Some e -> 
                            return e
                        | None ->
                            let! intrinsic = CompilerState.tryGetIntrinsic mi
                            match intrinsic with
                                | Some i ->
                                    let! ct = toCTypeS e.Type
                                    let args = 
                                        match i.arguments with
                                            | Some order -> order |> List.map (fun i -> args.[i])
                                            | None -> args
                                    return CCallIntrinsic(ct, i, List.toArray args)
                                | _ ->
                                    
                                    let! def = mi |> FunctionDefinition.ofMethodBase s.moduleState.tryGetOverrideCode |> CompilerState.useGlobalFunction mi
                                    return CCall(def, List.toArray args)


                | FieldGet(None, f) ->
                    return! Expr.Value(f.GetValue(null), f.FieldType) |> toCExprS
                    
                | FieldGet(Some t, f) ->
                    let! t = toCExprS t
                    let! s = State.get
                    match Helpers.tryGetBuiltInField s.moduleState.backend f t with
                        | Some e ->
                            return e
                        | _ ->
                            let! ct = toCTypeS e.Type
                            return CField(ct, t, f.Name)

                | PropertyGet(None, pi, []) ->
                    return! Expr.Value(pi.GetValue(null), pi.PropertyType) |> toCExprS
                    
                | PropertyGet(None, pi, args) ->
                    return! Expr.Call(pi.GetMethod, args) |> toCExprS

                | PropertyGet(Some t, pi, []) ->
                    if FSharpType.IsRecord(pi.DeclaringType, true) then
                        
                        let! ct = toCTypeS e.Type
                        let! t = toCExprS t
                        return CField(ct, t, pi.Name)

                    elif FSharpType.IsUnion(pi.DeclaringType.BaseType, true) then
                        
                        let! ct = toCTypeS e.Type
                        let case = 
                            FSharpType.GetUnionCases(pi.DeclaringType.BaseType, true)
                                |> Seq.find (fun ci -> ci.Name = pi.DeclaringType.Name)
                        let! t = toCExprS t
                        return CField(ct, t, case.Name + "_" + pi.Name)
                        
                    else
                        return! Expr.Call(t, pi.GetMethod, []) |> toCExprS
                    
                | PropertyGet(Some t, pi, args) ->
                    return! Expr.Call(t, pi.GetMethod, args) |> toCExprS

                | Coerce(e, t) ->
                    let! ce = toCExprS e
                    if t.IsAssignableFrom e.Type then
                        return ce
                    else
                        let! ct = toCTypeS e.Type
                        return CConvert(ct, ce)

                | AndAlso(l, r) ->
                    let! l = toCExprS l
                    let! r = toCExprS r
                    return CAnd(l, r)

                | OrElse(l, r) ->
                    let! l = toCExprS l
                    let! r = toCExprS r
                    return COr(l, r)

                | IfThenElse(c, i, e) ->
                    let! ct = toCTypeS e.Type
                    let! c = toCExprS c
                    let! i = toCExprS i
                    let! e = toCExprS e
                    return CConditional(ct, c, i, e)




                | AddressOf e ->
                    let! e = toCExprS e
                    return CAddressOf(CPointer(CPointerModifier.None, e.ctype), e)



                | Application _ 
                | Lambda _ ->
                    return failwith "[FShade] lambdas not implemented atm."


                | NewDelegate _
                | QuoteTyped _ 
                | QuoteRaw _
                | TypeTest _ ->
                    return failwithf "[FShade] unsupported expression %A" e


                | _ ->
                    return failwithf "[FShade unexpected expression kind %A" e
        }

    and toCLExprS (e : Expr) =
        e |> toCExprS |> State.map CLExpr.ofExprSafe

    let rec toCRExprS (e : Expr) =
        state {
            match e with
                | ReducibleExpression(e) ->
                    return! toCRExprS e

                | NewFixedArray(cnt, et, args) ->
                    let! ct = toCTypeS et
                    let! args = args |> List.mapS toCExprS
                    match args with
                        | [] -> return None
                        | args -> return CRArray(CArray(ct, cnt), args) |> Some

                | NewArray(et, args) ->
                    let! ct = toCTypeS et
                    let cnt = List.length args
                    let! args = args |> List.mapS toCExprS
                    return CRArray(CArray(ct, cnt), args) |> Some


                | Value(null, _) ->
                    return None

                | Value(v, EnumerableOf et) ->
                    let! ct = toCTypeS et
                    let enumerable = v |> unbox<System.Collections.IEnumerable>
                    let values = System.Collections.Generic.List<Expr>()
                    let e = enumerable.GetEnumerator()
                    while e.MoveNext() do
                        values.Add(Expr.Value(e.Current, et))
                    
                    return! Expr.NewFixedArray(et, CSharpList.toList values) |> toCRExprS

                | DefaultValue t ->
                    return None

                | _ ->
                    let! res = e |> toCExprS
                    return CRExpr.ofExpr res |> Some
        }

    let rec private (|Seq|_|) (e : Expr) =
        match e with
            | Sequential(Seq l, Seq r) ->
                Some (l @ r)
            | Sequential(l, Seq r) ->
                Some (l :: r)
            | Sequential(Seq l, r) ->
                Some (l @ [r])
            | _ ->
                Some [e]

    let rec private (|Cons|_|) (e : Expr) =
        match e with
            | Seq (l :: rest) ->
                Some (l, rest)
            | e ->
                Some(e, [])


    let rec toCStatementS (isLast : bool) (e : Expr) =
        state {
            match e with
                | ReducibleExpression e ->
                    return! toCStatementS isLast e

                | Quotations.DerivedPatterns.Unit ->
                    return CNop
                    
                | Sequential(Sequential(a,b), c) ->
                    return! toCStatementS isLast (Expr.Sequential(a, Expr.Sequential(b, c)))

                | Sequential(Unroll, Cons((ForInteger(v, first, step, last, b) as loop), rest)) ->
                    let! rest = rest |> List.mapS (toCStatementS isLast)

                    let! v = toCVarS v
                    match first, step, last with
                        | DerivedPatterns.Int32 first, DerivedPatterns.Int32 step, DerivedPatterns.Int32 last ->
                            let range = [ first .. step .. last ]
                            let! b = toCStatementS false b
                            return CSequential [
                                yield CDeclare(v, None)
                                for i in range do
                                    yield CWrite(CLVar v, CExpr.CValue(CType.CInt(true, 32), CIntegral (int64 i)))
                                    yield CIsolated [b]
                                yield! rest
                            ]
                        | _ ->
                            Log.warn "cannot unroll loop 'for i in %A .. %A .. %A'" first step last
                            let! loop = toCStatementS false loop
                            return CSequential [
                                yield loop
                                yield! rest
                            ]


                | Unroll ->
                    Log.warn "[FShade] orphaned unroll detected"
                    return CNop

                | ForEach(v, seq, body) ->
                    let! e = asExternalS seq

                    match e.ctype with
                        | CArray(et, len) ->
                            let! i = toCVarS (Var("i", typeof<int>, true))
                            let! v = toCVarS v
                            let! cbody = toCStatementS false body

                            let tint32 = CType.CInt(true, 32)
                            let condition = CLess(CVar i, CValue(tint32, CIntegral (int64 len)))
                            let increment = CIncrement(false, CLVar i)
                            return
                                CFor(
                                    CDeclare(i, Some (CRExpr(CValue(tint32, CIntegral 0L)))), 
                                    condition,                          
                                    increment,
                                    CSequential [
                                        CDeclare(v, Some (CRExpr(CItem(v.ctype, e, CVar i))))
                                        cbody
                                    ]                             
                                )


                        | _ -> 
                            return failwith "foreach not implemented"

                | ForInteger(v, first, step, last, b) ->
                    match step, last with
                        | Trivial, TrivialOp -> 
                            let! v = toCVarS v
                            let! cfirst = toCRExprS first
                            let! cstep = toCExprS step
                            let! clast = toCExprS last
                            let! cbody = toCStatementS false b

                            let increment =
                                match cstep with
                                    | CValue(_, CIntegral 1L)   -> CIncrement(false, CLVar v)   // i++
                                    | CValue(_, CIntegral -1L)  -> CDecrement(false, CLVar v)   // i--
                                    | CNeg(_,step)              -> CWrite(CLVar v, CExpr.CSub(v.ctype, CVar v, cstep))   // i = i - <step>
                                    | step                      -> CWrite(CLVar v, CExpr.CAdd(v.ctype, CVar v, cstep))   // i = i + <step>

                            let stepPositive =
                                match cstep with
                                    | CValue(_, CIntegral v) -> 
                                        if v = 0L then failwithf "[FShade] infinite loop detected: %A" e
                                        Some (v > 0L)
                                    | _ ->
                                        None

                            let condition =
                                match stepPositive with
                                    | Some true ->
                                        match clast with
                                            // i <= last - 1 --> i < last
                                            | CValue(ct, CIntegral value) -> CLess(CVar v, CValue(ct, CIntegral (value + 1L)))
                                            | CSub(_, last, CValue(ct, CIntegral 1L)) -> CLess(CVar v, last)
                                            | _ -> CLequal(CVar v, clast)

                                    | Some false ->
                                        match clast with
                                            // i >= last + 1 --> i > last
                                            | CAdd(_, last, CValue(ct, CIntegral 1L)) -> CGreater(CVar v, last)
                                            | _ -> CGequal(CVar v, clast)
                                
                                    | None -> 
                                        // i <> last
                                        CNotEqual(CVar v, clast)


                            return 
                                CFor(
                                    CDeclare(v, cfirst),                 // int i = <first>
                                    condition,                          
                                    increment,
                                    cbody                              
                                )
                        | Trivial, last ->
                            let vLast = Var("last", last.Type)
                            return! toCStatementS isLast (Expr.Let(vLast, last, Expr.ForInteger(v, first, step, Expr.Var vLast, b)))

                        | step, TrivialOp -> 
                            let vStep = Var("step", step.Type)
                            return! toCStatementS isLast (Expr.Let(vStep, step, Expr.ForInteger(v, first, Expr.Var vStep, last, b)))

                        | step, last ->
                            let vStep = Var("step", step.Type)
                            let vLast = Var("last", last.Type)
                            return! toCStatementS isLast (Expr.Let(vStep, step, Expr.Let(vLast, last, Expr.ForInteger(v, first, Expr.Var vStep, Expr.Var vLast, b))))
                            

                            
                | WriteOutputs values ->
                    let! writes =
                        values |> Map.toList |> List.chooseS (fun (name, (index, value)) ->
                            state {
                                let! index = 
                                    match index with
                                        | Some index -> toCExprS index |> State.map Some
                                        | _ -> State.value None

                                let! value = toCRExprS value
                                match value with
                                    | Some value -> return CWriteOutput(name, index, value) |> Some
                                    | None -> return None
                            }
                        )


                    return CSequential writes

//                | WriteOutput(name, index, value) ->
//                    let! value = toCExpr value
//                    let v = CLExpr.CLVar { ctype = value.ctype; name = name }
//                    match index with
//                        | Some idx ->
//                            let! idx = toCExpr idx 
//                            return CWriteOutput(name, Some idx, value)
//                        | None ->
//                            return CWriteOutput(name, None, value)


                | AddressSet(a, v) ->
                    let! a = toCLExprS a
                    let! v = toCExprS v
                    match a with
                        | Some a ->
                            return CWrite(a, v)
                        | None ->
                            return failwithf "[FShade] cannot set value for ptr %A" e

                | FieldSet(Some t, f, value) ->
                    let! t = toCLExprS (Expr.FieldGet(t, f))
                    match t with
                        | Some t ->
                            let! value = toCExprS value
                            return CWrite(t, value)
                        | None ->
                            return failwithf "[FShade] cannot set field %A" e

                | FieldSet(None, f, value) ->
                    return failwithf "[FShade] cannot set static field %A to value %A" f value


                | Let(v, e, b) ->
                    let! e = toCRExprS e
                    let! v = 
                        match e with
                            | Some e -> toCVarOfTypeS v e.ctype
                            | None -> toCVarS v

                    let! body = toCStatementS isLast b
                    return CSequential [
                        CDeclare(v, e)
                        body
                    ]

                | LetRecursive(bindings, b) ->
                    return failwithf "[FShade] cannot compile recursice bindings %A" bindings

                | TryWith _
                | TryFinally _ ->
                    return failwithf "[FShade] cannot compile exception handlers %A" e

                | Sequential(l, r) ->
                    let! l = toCStatementS false l
                    let! r = toCStatementS isLast r
                    return CSequential [l;r]

                | VarSet(v, value) ->
                    let! v = toCVarS v
                    let! value = toCExprS value
                    return CWrite(CLVar v, value)

                | SetArray(arr, index, value) ->
                    let! carr = toCLExprS arr
                    let! ci = toCExprS index
                    let! cvalue = toCExprS value
                    return CWrite(CLExpr.CLItem(cvalue.ctype, carr.Value, ci), cvalue)

                | PropertySet(None, pi, i, a) ->
                    return failwithf "[FShade] cannot set static property %A" pi

                | PropertySet(Some t, pi, i, a) ->
                    let! lexpr = Expr.PropertyGet(t, pi, i) |> toCLExprS
                    match lexpr with
                        | Some l ->
                            let! a = toCExprS a
                            return CWrite(l, a)
                        | None ->
                            return! Expr.Call(t, pi.SetMethod, i @ [a]) |> toCStatementS isLast


                | WhileLoop(guard, body) ->
                    let! guard = toCExprS guard
                    let! body = toCStatementS false body
                    return CWhile(guard, body)

                | IfThenElse(c, i, e) ->
                    let! c = toCExprS c
                    let! i = toCStatementS isLast i
                    let! e = toCStatementS isLast e
                    return CIfThenElse(c, i, e)

                | e ->
                    let! ce = toCRExprS e
                    match ce with
                        | Some (CRExpr e) ->
                            if isLast && e.ctype <> CType.CVoid then
                                return CReturnValue e
                            else
                                return CDo e
                            
                        | Some (CRArray(t,_) as rhs) -> 
                            if isLast then 
                                let! name = CompilerState.newName "temp"
                                let cVar = { name = name; ctype = t }
                                return CSequential [
                                    CDeclare(cVar, Some rhs)
                                    CReturnValue(CVar cVar)
                                ]
                            else
                                return CNop

                        | None ->
                            if isLast then
                                let! name = CompilerState.newName "temp"
                                let! t = toCTypeS e.Type
                                let cVar = { name = name; ctype = t }
                                return CSequential [
                                    CDeclare(cVar, None)
                                    CReturnValue(CVar cVar)
                                ]
                                
                            else
                                return CNop

        }

    let compileUniformsS (u : list<Uniform>) =
        state {
            let! us = u |> List.mapS toCUniformS
            return CUniformDef us
        }

    let compileEntryS (f : EntryPoint) =
        state {
            // ensure that all inputs, outputs, arguments have their correct names
            for i in f.inputs do do! CompilerState.reserveName i.paramName
            for o in f.outputs do do! CompilerState.reserveName o.paramName
            for a in f.arguments do do! CompilerState.reserveName a.paramName
            for u in f.uniforms do do! CompilerState.reserveName u.uniformName
            
            let! inputs     = f.inputs |> List.mapS toCEntryParameterS
            let! outputs    = f.outputs |> List.mapS toCEntryParameterS
            let! args       = f.arguments |> List.mapS toCEntryParameterS


            // compile the body
            let! body = toCStatementS true f.body
            let! ret = toCTypeS f.body.Type

            return
                CEntryDef {
                    cEntryName   = f.entryName
                    cInputs      = inputs
                    cOutputs     = outputs
                    cArguments   = args
                    cReturnType  = ret
                    cBody        = body
                    cDecorations = f.decorations
                }
        }

    let compileFunctionS (f : FunctionDefinition) =
        state {
            match f with
                | ManagedFunction(name, args, body) ->
                    let! s = State.get
                    let signature = f.Signature s.moduleState.backend
                    let! body = toCStatementS true body
                    return CFunctionDef(signature, body)

                | CompiledFunction(signature, body) ->
                    return CFunctionDef(signature, body)
                    
                | ManagedFunctionWithSignature(signature, body) ->
                    let! body = toCStatementS true body
                    return CFunctionDef(signature, body)

        }

    let compileConstantS (c : ConstantDefinition) =
        state {
            let! ct = toCTypeS c.cType
            let! e = toCRExprS c.cValue
            match e with
                | Some e ->
                    return CValueDef.CConstant(ct, c.cName, e)
                | None ->
                    return failwithf "[FShade] constants must have a value %A" c
        }

