﻿namespace FShade.Imperative

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.CompilerServices

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Quotations.DerivedPatterns

open Aardvark.Base
open Aardvark.Base.TypeInfo
open Aardvark.Base.TypeInfo.Patterns
open FSharp.Data.Adaptive

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
        | Utility of UtilityFunction

        member x.Signature (b : IBackend) =
            match x with
                | CompiledFunction(s,_) -> 
                    s

                | ManagedFunction(name,args,body) ->
                    CFunctionSignature.ofFunction b name args body.Type

                | ManagedFunctionWithSignature(s,_) ->
                    s

                | Utility u ->
                    CFunctionSignature.ofFunction b u.uniqueName u.functionArguments u.returnType
                    
    type ConstantDefinition = { cName : string; cType : Type; cValue : Expr }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FunctionDefinition =
        let private handleThis (isStatic : bool) (args : list<list<Var>>) (body : Expr) =
            let args = List.concat args
            if isStatic then 
                args, body
            else
                match args with
                    | self :: rest ->
                        let mSelf = Var(self.Name, self.Type, true) 
                        let body = body.Substitute(fun vi -> if vi = self then Some (Expr.Var mSelf) else None)
                                    
                        mSelf :: rest, body
                    | _ ->
                        args, body

        let ofMethodBase (tryGetOverrideCode : MethodBase -> Option<Expr>) (mi : MethodBase) =
            let name = methodName mi
            match tryGetOverrideCode mi with
                | Some (Lambdas(args, body)) ->
                    let args, body = handleThis mi.IsStatic args body
                    ManagedFunction(name, args, body)
                | _ ->
                    match ExprWorkardound.TryGetReflectedDefinition mi with
                        | Some (Lambdas(args, body)) ->
                            let args, body = handleThis mi.IsStatic args body
                            ManagedFunction(name, args, body)
                        | _ ->
                            failwithf "[FShade] cannot call function %A since it is not reflectable" mi

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Constructors =
        
        let cache = System.Collections.Concurrent.ConcurrentDictionary<IBackend * Type, FunctionDefinition>()
        let unionCache = System.Collections.Concurrent.ConcurrentDictionary<IBackend * Type, HashMap<UnionCaseInfo, FunctionDefinition>>()
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
                        { name = pi.Name; ctype = CType.ofType b pi.PropertyType; modifier = CParameterModifier.In }
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
                                { name = pi.Name; ctype = CType.ofType b pi.PropertyType; modifier = CParameterModifier.In }
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
                    |> HashMap.ofSeq
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
                match ExprWorkardound.TryGetReflectedDefinition ctor with
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

                    | MatrixValue(bt, args) ->
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


        module private Simplification = 

            let mulMatVec (m : CExpr) (v : CExpr) =
                let rType =
                    match m.ctype with
                        | CMatrix(t, r, c) -> CVector(t, r)
                        | t -> failwithf "[FShade] not a matrix type: %A" t
                CMulMatVec(rType, m, v)

                
            let mulVecMat (v : CExpr) (m : CExpr) =
                let rType =
                    match m.ctype with
                        | CMatrix(t, r, c) -> CVector(t, c)
                        | t -> failwithf "[FShade] not a matrix type: %A" t

                CMulVecMat(rType, v, m)

            let rec extractMatrices (e : CExpr) =
                match e with
                    | CMulMatMat(t,l,r) ->
                        extractMatrices l @ extractMatrices r
                    | _ ->
                        [e]

            let rec simplifyMatrixTerm (e : CExpr) =
                match e with
                    | CMulMatVec(_,m, v) ->
                        match extractMatrices m with
                            | [m] -> e
                            | many -> List.foldBack mulMatVec many v

                    | CMulVecMat(_,v,m) ->
                        match extractMatrices m with
                            | [m] -> e
                            | many -> List.fold mulVecMat v many// v * m0 * m1 * m2
                            
                    | _ ->
                        e

        let rec tryGetBuiltInMethod (b : IBackend) (mi : MethodInfo) (args : list<CExpr>) =
            let ct = CType.ofType b mi.ReturnType
            match mi, args with
                | Method("op_UnaryNegation", _), [l]        -> CExpr.CNeg(ct, l) |> Some
                | MethodQuote <@ not @> _, [l]              -> CExpr.CNot(ct, l) |> Some

                | Method("op_LogicalNot", _), [l]           -> CExpr.CBitNot(ct, l) |> Some

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
                        | CMatrix _, CVector _              -> CMulMatVec(ct, l, r) |> Simplification.simplifyMatrixTerm |> Some
                        | CVector _, CMatrix(b,rows,cols)   -> CMulVecMat(ct, l, r) |> Simplification.simplifyMatrixTerm |> Some
                        | _                                 -> CExpr.CMul(ct, l, r) |> Some

                // transpose
                | MethodQuote <@ Mat.transpose : M44d -> M44d @> _, [m]    
                | MethodQuote <@ Mat.Transposed : M44d -> M44d @> _, [m]    
                | Method("Transpose", [MatrixOf _]) , [m]
                | Method("get_Transposed", _), [m] -> 
                    match ct with
                        | CMatrix(b,r,c) -> CTranspose(CMatrix(b,c,r), m) |> Some
                        | _ -> None
                   
                // dot         
                | MethodQuote <@ Vec.dot : V4d -> V4d -> float @> _, [l;r]
                | MethodQuote <@ Vec.Dot : V4d * V4d -> float @> _, [l;r]
                | Method("Dot", [VectorOf _; VectorOf _]), [l;r] ->
                    CDot(ct, l, r) |> Some
              
                // length         
                | MethodQuote <@ Vec.length : V4d -> float @> _, [v]
                | MethodQuote <@ Vec.Length : V4d -> float @> _, [v]
                | Method("get_Length", [VectorOf _]), [v] ->
                    CVecLength(ct, v) |> Some

                | Method("get_Length", [ArrOf(l,_)]), [a] ->
                    CValue(CType.CInt(true, 32), CIntegral (int64 l)) |> Some

                 // lengthSquared     
                | MethodQuote <@ Vec.lengthSquared : V4d -> float @> _, [v]
                | MethodQuote <@ Vec.LengthSquared : V4d -> float @> _, [v]
                | Method("get_LengthSquared", [VectorOf _]), [v] ->
                    CDot(ct, v, v) |> Some
                               

                // cross              
                | MethodQuote <@ Vec.cross : V3d -> V3d -> V3d @> _, [l;r]
                | MethodQuote <@ Vec.Cross : V3d * V3d -> V3d @> _, [l;r]
                | Method("Cross", [VectorOf _; VectorOf _]), [l;r] ->
                    CCross(ct, l, r) |> Some

                // transformDir
                | MethodQuote <@ Mat.transformDir : M44d -> V3d -> V3d @> _, [m;v]
                | MethodQuote <@ Mat.TransformDir : M44d * V3d -> V3d @> _, [m;v]
                | Method("TransformDir", [MatrixOf _; VectorOf _]), [m;v] ->
                    match ct, v.ctype with
                        | CVector(rt, rd), CVector(t, d) ->
                            let res = CMulMatVec(CVector(rt, rd + 1), m, CNewVector(CVector(t, d + 1), d, [v; zero t])) |> Simplification.simplifyMatrixTerm
                            CVecSwizzle(ct, res, CVecComponent.first rd) |> Some
                        | _ ->
                            None

                // transformPos
                | MethodQuote <@ Mat.transformPos : M44d -> V3d -> V3d @> _, [m;v] 
                | MethodQuote <@ Mat.TransformPos : M44d * V3d -> V3d @> _, [m;v] 
                | Method("TransformPos", [MatrixOf _; VectorOf _]), [m;v] ->
                    match ct, v.ctype with
                        | CVector(rt, rd), CVector(t, d) ->
                            let res = CMulMatVec(CVector(rt, rd + 1), m, CNewVector(CVector(t, d + 1), d, [v; one t])) |> Simplification.simplifyMatrixTerm
                            CVecSwizzle(ct, res, CVecComponent.first rd) |> Some
                        | _ ->
                            None

                // transposedTransformDir
                | MethodQuote <@ Mat.TransposedTransformDir : M44d * V3d -> V3d @> _, [m; v]
                | Method("TransposedTransformDir", [MatrixOf _; VectorOf _]), [m;v] ->
                    match ct, v.ctype with
                        | CVector(rt, rd), CVector(t, d) ->
                            let res = CMulVecMat(CVector(rt, rd + 1), CNewVector(CVector(t, d + 1), d, [v; zero t]), m) |> Simplification.simplifyMatrixTerm
                            CVecSwizzle(ct, res, CVecComponent.first rd) |> Some
                        | _ ->
                            None

                // transposedTransformDir
                | MethodQuote <@ Mat.TransposedTransformPos : M44d * V3d -> V3d @> _, [m; v]
                | Method("TransposedTransformPos", [MatrixOf _; VectorOf _]), [m;v] ->
                    match ct, v.ctype with
                        | CVector(rt, rd), CVector(t, d) ->
                            let res = CMulVecMat(CVector(rt, rd + 1), CNewVector(CVector(t, d + 1), d, [v; one t]), m) |> Simplification.simplifyMatrixTerm
                            CVecSwizzle(ct, res, CVecComponent.first rd) |> Some
                        | _ ->
                            None

                
                
                | MethodQuote <@ m22d : M22f -> _ @> _, [m] 
                | MethodQuote <@ m33d : M33f -> _ @> _, [m] 
                | MethodQuote <@ m34d : M34f -> _ @> _, [m] 
                | MethodQuote <@ m44d : M44f -> _ @> _, [m] 
                | MethodQuote <@ m22f : M22d -> _ @> _, [m] 
                | MethodQuote <@ m33f : M33d -> _ @> _, [m] 
                | MethodQuote <@ m34f : M34d -> _ @> _, [m] 
                | MethodQuote <@ m44f : M44d -> _ @> _, [m] 
                | MethodQuote <@ m22i : M22d -> _ @> _, [m] 
                | MethodQuote <@ m33i : M33d -> _ @> _, [m] 
                | MethodQuote <@ m34i : M34d -> _ @> _, [m] 
                | MethodQuote <@ m44i : M44d -> _ @> _, [m] 
                | MethodQuote <@ m22l : M22d -> _ @> _, [m] 
                | MethodQuote <@ m33l : M33d -> _ @> _, [m] 
                | MethodQuote <@ m34l : M34d -> _ @> _, [m] 
                | MethodQuote <@ m44l : M44d -> _ @> _, [m] 
                | Method("UpperLeftM33", [MatrixOf _]), [m]
                | Method("op_Explicit", [MatrixOf _]), [m] ->
                    match ct with
                        | CMatrix(et, r, c) ->
                            CConvertMatrix(ct, m) |> Some
                        | _ ->
                            None 


                // vector swizzles
                | (MethodQuote <@ Vec.xy : V4d -> V2d @> _ ), [v] -> CVecSwizzle(ct, v, CVecComponent.xy) |> Some
                | (MethodQuote <@ Vec.yz : V4d -> V2d @> _), [v] -> CVecSwizzle(ct, v, CVecComponent.yz) |> Some
                | (MethodQuote <@ Vec.zw : V4d -> V2d @> _), [v] -> CVecSwizzle(ct, v, CVecComponent.zw) |> Some
                | (MethodQuote <@ Vec.xyz : V4d -> V3d @> _), [v] -> CVecSwizzle(ct, v, CVecComponent.xyz) |> Some
                | (MethodQuote <@ Vec.yzw : V4d -> V3d @> _), [v] -> CVecSwizzle(ct, v, CVecComponent.yzw) |> Some

                | Method("get_Item", [VectorOf _; Int32]), [v;i] -> CVecItem(ct, v, i) |> Some

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
                    CMatrixRow(ct, m, 0) |> Some

                | Method("get_R1", [MatrixOf _]), [m] -> 
                    CMatrixRow(ct, m, 1) |> Some

                | Method("get_R2", [MatrixOf _]), [m] -> 
                    CMatrixRow(ct, m, 2) |> Some
                    
                | Method("get_R3", [MatrixOf _]), [m] -> 
                    CMatrixRow(ct, m, 3) |> Some

                | Method("get_C0", [MatrixOf _]), [m] -> 
                    CMatrixCol(ct, m, 0) |> Some

                | Method("get_C1", [MatrixOf _]), [m] -> 
                    CMatrixCol(ct, m, 1) |> Some

                | Method("get_C2", [MatrixOf _]), [m] -> 
                    CMatrixCol(ct, m, 2) |> Some

                | Method("get_C3", [MatrixOf _]), [m] -> 
                    CMatrixCol(ct, m, 3) |> Some


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

                    // Check if the provided args are sufficient for the dimension
                    let providedDim =
                        args |> List.sumBy (fun expr ->
                            match expr.ctype with
                            | CVector(_, d) -> d
                            | _ -> 1
                        )

                    // Fill in zeros for the missing dimensions
                    let args =
                        if providedDim > 1 && providedDim < d then
                            let newArgs =
                                List.init (d - providedDim) (fun _ ->
                                    CValue(CType.ofType b t, CIntegral 0L)
                                )

                            args @ newArgs
                        else
                            args

                    CNewVector(CType.ofType b ctor.DeclaringType, d, args) |> Some

                | MatrixOf(s, _) ->
                    let l = List.length args

                    if l = s.X * s.Y || l = 1 then
                        CNewMatrix(CType.ofType b ctor.DeclaringType, args) |> Some
                    else
                        None
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
            usedTypes           : HashMap<obj, CType>

            globalFunctions     : HashMap<obj, FunctionDefinition>
            globalConstants     : HashMap<obj, ConstantDefinition>

            globalParameters    : Set<string>
            tryGetOverrideCode  : MethodBase -> Option<Expr>
        }

    type CompilerState =
        {
            nameIndices         : Map<string, int>
            variables           : Map<Var, CVar>
            reservedNames       : Set<string>

            usedFunctions       : HashMap<obj, FunctionDefinition>
            usedGlobalFunctions : HashSet<FunctionDefinition>
            usedConstants       : HashSet<ConstantDefinition>
            usedGlobals         : HashSet<string>
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
                let sign = f.Signature s.moduleState.backend
                let s = { s with moduleState = { s.moduleState with usedTypes = HashMap.add (sign.returnType :> obj) sign.returnType s.moduleState.usedTypes }}
                { s with usedFunctions = HashMap.add key f s.usedFunctions }, sign
            )

        let useGlobalFunction (key : obj) (f : FunctionDefinition) =
            State.custom (fun s ->
                match HashMap.tryFind key s.moduleState.globalFunctions with
                    | Some signature ->
                        let sign = signature.Signature s.moduleState.backend
                        // use the return type
                        let s = { s with moduleState = { s.moduleState with usedTypes = HashMap.add (sign.returnType :> obj) sign.returnType s.moduleState.usedTypes }}
                        { s with usedGlobalFunctions = HashSet.add signature s.usedGlobalFunctions }, sign
                    | _ -> 
                        let signature = f.Signature s.moduleState.backend
                        let s = { s with moduleState = { s.moduleState with usedTypes = HashMap.add (signature.returnType :> obj) signature.returnType s.moduleState.usedTypes }}
                        { s with 
                            usedGlobalFunctions = HashSet.add f s.usedGlobalFunctions
                            moduleState = { s.moduleState with globalFunctions = HashMap.add key f s.moduleState.globalFunctions } 
                        }, signature 
            )

        let useCtor (key : obj) (f : FunctionDefinition) =
            useGlobalFunction ((key, "ctor") :> obj) f

        let useConstant (key : obj) (e : Expr) =
            state {
                let! s = State.get
                let ct = CType.ofType s.moduleState.backend e.Type
                match HashMap.tryFind key s.moduleState.globalConstants with
                    | None -> 
                        let name = sprintf "_constant%d" s.moduleState.constantIndex
                        let c = { cName = name; cType = e.Type; cValue = e }
                        do! State.put { 
                                s with 
                                    moduleState = { s.moduleState with globalConstants = HashMap.add key c s.moduleState.globalConstants; constantIndex = s.moduleState.constantIndex + 1 } 
                                    usedConstants = HashSet.add c s.usedConstants
                            }
                    
                        return CVar { name = name; ctype = ct }
                    | Some c ->
                        do! State.put { s with usedConstants = HashSet.add c s.usedConstants }
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

        let rec free (e : Expr) : HashSet<Free> =
            match e with
                | Let(v,e,b) ->
                    let fe = free e
                    let fb = free b
                    HashSet.union fe (HashSet.remove (Variable v) fb)

                | ReadInput(kind,name,idx,_) ->
                    match idx with
                        | Some idx -> free idx |> HashSet.add (Global(kind, name, e.Type, Some idx))
                        | None -> HashSet.ofList [ Global(kind, name, e.Type, None) ]

                | WriteOutputs values ->
                    let mutable res = HashSet.empty
                    for (name, (index, value)) in Map.toSeq values do
                        match index with
                            | Some index -> res <- HashSet.union res (free index)
                            | _ -> ()
                        res <- res |> HashSet.union (free value) |> HashSet.add (Global(ParameterKind.Output, name, value.Type, index))

                    res

                | ExprShape.ShapeCombination(o, args) ->
                    args |> List.fold (fun m e -> HashSet.union m (free e)) HashSet.empty

                | ExprShape.ShapeLambda(v, b) ->
                    free b |> HashSet.remove (Variable v)

                | ExprShape.ShapeVar v ->
                    HashSet.ofList [ Variable v ]

    let emptyState (m : ModuleState) =
        {
            nameIndices         = Map.empty
            variables           = Map.empty
            reservedNames       = Set.empty
            usedFunctions       = HashMap.empty
            usedGlobalFunctions = HashSet.empty
            usedConstants       = HashSet.empty
            usedGlobals         = HashSet.empty
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
                        usedTypes <- HashMap.add (t :> obj) cType usedTypes
                        for (f,_) in fields do
                            visit f

                    | _ -> ()

            visit cType
            
            { s with moduleState = { s.moduleState with usedTypes = usedTypes } }, cType
        )
      
    let freshCVarOfTypeS (v : Var) (t : CType) =
        State.custom (fun s ->
            match Map.tryFind v.Name s.nameIndices with
                | Some index ->
                    let mutable index = index
                    let mutable name = v.Name + string index

                    let rec findName (index : int) (indices : Map<string, int>) =
                        let n = v.Name + string index
                        match Map.tryFind n indices with
                            | Some i ->
                                findName (index + 1) indices
                            | None ->
                                let indices = Map.add n 1 indices
                                let indices = Map.add v.Name (index + 1) indices
                                n, indices

                    let name, indices = findName index s.nameIndices

                    let res = { ctype = t; name = name }
                    let state = { s with nameIndices = indices; variables = Map.add v res s.variables }
                    state, res
                | None ->
                    let res = { ctype = t; name = v.Name }
                    let state = { s with nameIndices = Map.add v.Name 1 s.nameIndices; variables = Map.add v res s.variables }
                    state, res
        )
        
    let freshCVarS (v : Var) =
        state {
            let! t = toCTypeS v.Type
            return! freshCVarOfTypeS v t
        }

    /// converts a variable to a CVar using the cached name or by
    /// creating a fresh name for it (and caching it)
    let toCVarOfTypeS (v : Var) (t : CType) =
        State.custom (fun s ->
            match Map.tryFind v s.variables with
                | Some v -> 
                    s, v

                | None ->
                    let mutable s = s
                    let res = (freshCVarOfTypeS v t).Run(&s)
                    s, res
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
            let mutable variables = HashMap.empty
            let mutable inputValues = HashMap.empty
            let! globals = State.get |> State.map (fun s -> s.moduleState.globalParameters)



            let getVar (kind : ParameterKind) (name : string) (typ : Type) (idx : Option<Expr>) =
                let key = (kind, name, typ, idx)
                match HashMap.tryFind key variables with
                    | Some v -> v
                    | None ->
                        let suffix =
                            match idx with
                                | Some idx -> Expr.ComputeHash idx
                                | None -> ""
                        let v = Var(name + suffix, typ)
                        variables <- HashMap.add key v variables
                        inputValues <- HashMap.add v key inputValues
                        v

            let mutable usesUniform = false
            let e = 
                e.SubstituteReads (fun kind typ name idx ->
                    if kind = ParameterKind.Uniform && Set.contains name globals then 
                        usesUniform <- true
                        None
                    else
                        let v = getVar kind name typ idx
                        Some (Expr.Var v)
                )

            let free = e.GetFreeVars() |> HashSet.ofSeq
            if not usesUniform && HashSet.isEmpty free then
                let hash = Expr.ComputeHash e
                return! CompilerState.useConstant hash e
            else
                let! globals = State.get |> State.map (fun s -> s.moduleState.globalParameters)
                let free = 
                    free
                        |> HashSet.toArray
                        |> Array.sortBy (fun v -> 
                            match HashMap.tryFind v inputValues with
                                | Some (_,n,_,_) -> 0, n
                                | None -> 1, v.Name
                            )
 
                    
                let! oldState = State.get
                let parameters, newState = 
                    let parameters =
                        free 
                        |> Array.mapS (fun v ->
                            state {
                                let! v1 = toCVarS v
                                return { name = v1.name; ctype = v1.ctype; modifier = (if v.IsMutable then CParameterModifier.ByRef else CParameterModifier.In) }
                            }
                        )   

                    let mutable oldState =
                        {
                            nameIndices         = Map.empty
                            variables           = Map.empty
                            reservedNames       = oldState.reservedNames
                            usedGlobalFunctions = HashSet.empty
                            usedFunctions       = HashMap.empty
                            usedConstants       = HashSet.empty
                            usedGlobals         = HashSet.empty
                            moduleState         = oldState.moduleState
                        }
                    parameters.Run(&oldState), oldState.moduleState 
                do! State.modify (fun s -> { s with moduleState = newState })


                let! args = 
                    free |> Array.mapS (fun f ->
                        state {
                            match HashMap.tryFind f inputValues with
                                | Some (kind, n, t, idx) ->
                                    match kind with
                                        | ParameterKind.Input | ParameterKind.Uniform ->
                                            let expression = 
                                                match idx with
                                                    | Some idx -> Expr.ReadInput(kind, t, n, idx)
                                                    | None -> Expr.ReadInput(kind, t, n)
                                            let! e = toCExprS expression
                                            return e
                                        | _ ->
                                            return failwithf "[FShade] cannot use output %A as closure in function" n
                                    
                                | None ->
                                    let! v = toCVarS f
                                    return CVar v

                        }
                    ) 
                    

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
                    if HashMap.isEmpty inputValues then CompilerState.useGlobalFunction (e :> obj) definition
                    else CompilerState.useLocalFunction (e :> obj) definition


                return CCall(signature, args)
        }

    and toCExprS (e : Expr) : State<_, CExpr> =
        state {
            //// ensure all types are used
            //let! __ = toCTypeS e.Type
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
                | SetRef _ 
                | WhileLoop _ ->
                    return! asExternalS e

                | ReadInput(kind, name, index, _) ->
                    let! ct = toCTypeS e.Type
                    let! s = State.get
                    if Set.contains name s.moduleState.globalParameters then
                        do! State.put { s with usedGlobals = HashSet.add name s.usedGlobals }

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

                | NewRef value ->
                    return! toCExprS value

                | DeRef ref ->
                    return! toCExprS ref

                | RefOf v ->
                    let! v = toCLExprS v
                    match v with
                        | Some v -> return CLExpr.toExpr v
                        | None -> return failwith "[FShade] cannot get address of non L-Expression"

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
                    let! ctor = ctors |> HashMap.find ci |> CompilerState.useGlobalFunction ci
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

                | CallFunction(f, args) ->
                    let! args = args |> List.mapS toCExprS
                    let! s = State.get

                    match f.functionMethod with
                        | Some (:? MethodInfo as mi) ->
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
                                            let! def = FunctionDefinition.Utility f |> CompilerState.useGlobalFunction f.uniqueName
                                            return CCall(def, List.toArray args)
                        | _ ->
                            let! def = FunctionDefinition.Utility f |> CompilerState.useGlobalFunction f.uniqueName
                            return CCall(def, List.toArray args)

                

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
                        
                    elif FSharpType.IsUnion(pi.DeclaringType, true) then
                        let! ct = toCTypeS e.Type
                        let case = 
                            FSharpType.GetUnionCases(pi.DeclaringType, true)
                                |> Seq.find (fun ci -> ci.GetFields() |> Array.exists ((=) pi))
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
                | Ignore e ->
                    return! toCStatementS false e

                | ReducibleExpression e ->
                    return! toCStatementS isLast e

                | Quotations.DerivedPatterns.Unit ->
                    return CNop
                    
                | Sequential(Sequential(a,b), c) ->
                    return! toCStatementS isLast (Expr.Sequential(a, Expr.Sequential(b, c)))

                | Sequential(Unroll, Cons((ForInteger(v, first, step, last, b) as loop), rest)) ->
                    let! rest = rest |> List.mapS (toCStatementS isLast)

                    let loopBody (i : int) = b.Substitute(fun vi -> if vi = v then Some (Expr.Value(i)) else None)


                    
                    match first, step, last with
                        | DerivedPatterns.Int32 first, DerivedPatterns.Int32 step, DerivedPatterns.Int32 last ->
                            let range = [ first .. step .. last ]

                            let! code = 
                                range |> List.mapS (fun i ->
                                    toCStatementS false (loopBody i) |> State.map (fun s -> CIsolated [s])
                                )

                            return CSequential [
                                yield! code
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

                | SetRef(ref, value) ->
                    let! r = toCLExprS ref
                    let! v = toCExprS value
                    match r with
                        | Some r -> return CWrite(r, v)
                        | None -> return failwith "[FShade] refs can only by variables"

                | ForEach(v, seq, body) ->
                    let! e = asExternalS seq

                    match e.ctype with
                        | CArray(et, len) ->
                            let! i = freshCVarS (Var("i", typeof<int>, true))
                            let! v = freshCVarS v
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
                            let! cfirst = toCRExprS first
                            let! cstep = toCExprS step
                            let! clast = toCExprS last
                            let! v = freshCVarS v
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
                            | Some e -> freshCVarOfTypeS v e.ctype
                            | None -> freshCVarS v

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

                

//                | SetArray(ReadInput(kind, name, None), index, value) ->
//                    return! toCStatementS isLast (Expr.WriteOutputs [name, Some index, value])
//                    //return failwith ""

                | SetArray(arr, index, value) ->
                    let! carr = toCExprS arr
                    let! ci = toCExprS index
                    let! cvalue = toCExprS value
                    return CWrite(CLExpr.CLItem(cvalue.ctype, carr, ci), cvalue)

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

                | UnsafeWrite(t, a) ->
                    let! lexpr = t |> toCLExprS
                    match lexpr with
                    | Some l ->
                        let! a = toCExprS a
                        return CWrite(l, a)
                    | None ->
                        return failwithf "[FShade] cannot write to expression %A" t 


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
            let! rtdata     = f.raytracingData |> List.mapS toCEntryParameterS


            // compile the body
            let! body = toCStatementS true f.body
            let! ret = toCTypeS f.body.Type

            return
                CEntryDef {
                    cEntryName      = f.entryName
                    cInputs         = inputs
                    cOutputs        = outputs
                    cArguments      = args
                    cRaytracingData = rtdata
                    cReturnType     = ret
                    cBody           = body
                    cDecorations    = f.decorations
                }
        }

    let compileFunctionS (f : FunctionDefinition) =
        state {
            match f with
                | ManagedFunction(name, args, body) ->
                    let! s = State.get
                    let signature = f.Signature s.moduleState.backend
                    let! body = toCStatementS true body
                   
                    do! State.modify (fun s -> 
                            let mutable ni = s.nameIndices
                            let mutable vs = s.variables
                            for a in args do
                                ni <- Map.add a.Name 1 ni
                                vs <- Map.add a { name = a.Name; ctype = CType.ofType s.moduleState.backend a.Type } vs
                            { s with nameIndices = ni; variables = vs }
                        )
                    return CFunctionDef(signature, body)

                | CompiledFunction(signature, body) ->
                    return CFunctionDef(signature, body)
                    
                | ManagedFunctionWithSignature(signature, body) ->
                    let pars = body.GetFreeVars()
                    do! State.modify (fun s -> 
                        let mutable ni = s.nameIndices
                        let mutable vs = s.variables
                        for a in pars do
                            ni <- Map.add a.Name 1 ni
                            vs <- Map.add a { name = a.Name; ctype = CType.ofType s.moduleState.backend a.Type } vs
                        { s with nameIndices = ni; variables = vs }
                    )
                    let! body = toCStatementS true body
                    return CFunctionDef(signature, body)

                | Utility u ->
                    let! s = State.get
                    let signature = f.Signature s.moduleState.backend

                    
                    do! State.modify (fun s -> 
                        let mutable ni = s.nameIndices
                        let mutable vs = s.variables
                        for a in u.functionArguments do
                            ni <- Map.add a.Name 1 ni
                            vs <- Map.add a { name = a.Name; ctype = CType.ofType s.moduleState.backend a.Type } vs
                        { s with nameIndices = ni; variables = vs }
                    )

                    let! body = toCStatementS true u.functionBody
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

