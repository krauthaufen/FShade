namespace FShade.Imperative

open System
open System.Reflection
open System.Collections.Generic

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Aardvark.Base
open Aardvark.Base.TypeInfo
open Aardvark.Base.TypeInfo.Patterns

open FShade.Types
open FShade.Parameters
open FShade.Primitives
open FShade.Builders

type EntryPoint =
    {
        conditional : Option<string>
        entryName   : string
        inputs      : list<Var>
        outputs     : list<Var>
        arguments   : list<Var>
        body        : Expr
    }

type Module = { entries : list<EntryPoint> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EntryPoint =

    let ofLambda (name : string) (e : Expr) =
        match e with
            | Lambdas(args, body) ->
                let args = List.concat args
                {
                    conditional = None
                    entryName = name
                    inputs = []
                    outputs = []
                    arguments = args
                    body = body
                }
            | e ->
                {
                    conditional = None
                    entryName = name
                    inputs = []
                    outputs = []
                    arguments = []
                    body = e
                }
                
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Module =

    let ofLambda (name : string) (e : Expr) =
        { entries = [EntryPoint.ofLambda name e] }

    let ofLambdas (l : list<string * Expr>) =
        { entries = l |> List.map (uncurry EntryPoint.ofLambda) }



module Compiler =
    open Aardvark.Base.Monads.State

    [<AutoOpen>]
    module private Helpers =
        let inline (>>=) (m : State<'s, 'a>) (f : 'a -> State<'s, 'b>) =
            m |> State.bind f

        let inline (|>>) (m : State<'s, 'a>) (f : 'a -> 'b) =
            m |> State.map f

    [<AllowNullLiteral; AbstractClass>]
    type Backend() =
        let intrinsicFunctions = System.Collections.Concurrent.ConcurrentDictionary<MethodBase, Option<CIntrinsic>>()

        abstract member TryGetIntrinsicMethod : MethodInfo -> Option<CIntrinsic>
        abstract member TryGetIntrinsicCtor : ConstructorInfo -> Option<CIntrinsic>

        member x.TryGetIntrinsic (m : MethodBase) =
            intrinsicFunctions.GetOrAdd(m, fun m ->
                match m with
                    | :? MethodInfo as mi -> x.TryGetIntrinsicMethod mi
                    | :? ConstructorInfo as ci -> x.TryGetIntrinsicCtor ci
                    | _ -> None
            )

    type FunctionDefinition = 
        | ManagedFunction of name : string * args : list<Var> * body : Expr
        | CompiledFunction of signature : CFunctionSignature * body : CStatement
        | EntryFunction of EntryPoint

        member x.Signature =
            match x with
                | CompiledFunction(s,_) -> 
                    s

                | ManagedFunction(name,args,body) ->
                    CFunctionSignature.ofFunction name args body.Type

                | EntryFunction f ->
                    CFunctionSignature.ofFunction f.entryName f.arguments f.body.Type

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FunctionDefinition =
        let ofMethodBase (mi : MethodBase) =
            let name = methodName mi
            match Expr.TryGetReflectedDefinition mi with
                | Some (Lambdas(args, body)) ->
                    ManagedFunction(name, List.concat args, body)
                | _ ->
                    failwithf "[FShade] cannot call function %A since it is not reflectable" mi

    type CompilerState =
        {
            backend             : Backend
            nameIndices         : Map<string, int>
            variableNames       : Map<Var, string>

            usedFunctions       : HashMap<obj, FunctionDefinition>
            usedTypes           : HashMap<obj, CType>
        }

    module Constructors =
        
        let cache = System.Collections.Concurrent.ConcurrentDictionary<Type, FunctionDefinition>()
        let unionCache = System.Collections.Concurrent.ConcurrentDictionary<Type, HashMap<UnionCaseInfo, FunctionDefinition>>()
        let ctorCache = System.Collections.Concurrent.ConcurrentDictionary<ConstructorInfo, FunctionDefinition>()

        let tuple (t : Type) =
            cache.GetOrAdd(t, fun t ->
                let cName = 
                    "new_" + typeName t

                let cParameters = 
                    FSharpType.GetTupleElements t
                        |> Array.map CType.ofType 
                        |> Array.mapi (fun i ct -> { name = sprintf "item%d" i; ctype = ct; modifier = CParameterModifier.In })

                let cType =
                    CType.ofType t
            
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

        let record (t : Type) =
            cache.GetOrAdd(t, fun t ->
                let cName = 
                    "new_" + typeName t

                let cFields = 
                    FSharpType.GetRecordFields(t, true)

                let cParameters =
                    cFields |> Array.map (fun pi ->
                        { name = pi.Name.ToLower(); ctype = CType.ofType pi.PropertyType; modifier = CParameterModifier.In }
                    )   
                    
                let cType =
                    CType.ofType t
            
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

        let union (t : Type) =
            unionCache.GetOrAdd(t, fun t ->
                let cType = CType.ofType t
                let cTypeName = typeName t
                
                FSharpType.GetUnionCases(t, true)
                    |> Seq.map (fun ci ->
                        
                        let cFields = ci.GetFields()

                        let cParameters =
                            cFields |> Array.map (fun pi ->
                                { name = pi.Name.ToLower(); ctype = CType.ofType pi.PropertyType; modifier = CParameterModifier.In }
                            )  

                        let cType =
                            CType.ofType t
            
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
                                    CWrite(CLField(p.ctype, CLVar res, f.Name), CExpr.CVar v)
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

        let custom (ctor : ConstructorInfo) =
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

            ctorCache.GetOrAdd(ctor, fun ctor ->
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
    module CompilerState =
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

        let variableName (v : Var) =
            State.custom (fun s ->
                match Map.tryFind v s.variableNames with
                    | Some name -> 
                        s, name
                    | None ->
                        match Map.tryFind v.Name s.nameIndices with
                            | Some index ->
                                let name = v.Name + string index
                                let state = { s with nameIndices = Map.add v.Name (index + 1) s.nameIndices; variableNames = Map.add v name s.variableNames }
                                state, name
                            | None ->
                                let state = { s with nameIndices = Map.add v.Name 1 s.nameIndices; variableNames = Map.add v v.Name s.variableNames }
                                state, v.Name

            )

        let useFunction (key : obj) (f : FunctionDefinition) =
            State.custom (fun s ->
                { s with usedFunctions = HashMap.add key f s.usedFunctions }, f.Signature
            )

        let useCtor (key : obj) (f : FunctionDefinition) =
            State.custom (fun s ->
                { s with usedFunctions = HashMap.add ((key, "ctor") :> obj) f s.usedFunctions }, f.Signature
            )

        let tryGetIntrinsic (mi : MethodBase) =
            State.get |> State.map (fun s ->
                if isNull s.backend then None
                else s.backend.TryGetIntrinsic mi
            )

    let emptyState (b : Backend) =
        {
            backend             = b
            nameIndices         = Map.empty
            variableNames       = Map.empty

            usedFunctions       = HashMap.empty
            usedTypes           = HashMap.empty
        }

    let toCType (t : Type) =
        State.custom (fun s ->
            let cType = CType.ofType t

            match cType with
                | CStruct _ ->
                    { s with CompilerState.usedTypes = HashMap.add (t :> obj) cType s.usedTypes }, cType
                | _ ->
                    s, cType
        )

    let toCVar (v : Var) =
        state {
            let! ctype = toCType v.Type
            let! name = CompilerState.variableName v
            return { name = name; ctype = ctype }
        }

    let rec tryDeconstructValue (t : Type) (v : obj) =
        if FSharpType.IsTuple t then
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

    let rec asExternal (e : Expr) =
        state {
            let free = e.GetFreeVars() |> Seq.toList
            if free.Length = 0 then
                let! name = CompilerState.newName "value"
                // TODO: support constants
                return failwithf "[FShade] constants not implemented %A" e
            else
                let! name = CompilerState.newName "helper"
                let definition = ManagedFunction(name, free, e)

                let! signature = CompilerState.useFunction (e :> obj) definition

                let! args = free |> List.mapS (toCVar >> State.map CVar) |>> List.toArray
                return CCall(signature, args)
        }

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

    let rec tryGetBuiltInMethod (mi : MethodInfo) (args : list<CExpr>) =
        let ct = CType.ofType mi.ReturnType
        match mi, args with
            | Method("op_UnaryNegation", _), [l]        -> CExpr.CNeg(ct, l) |> Some
            | MethodQuote <@ not @> _, [l]              -> CExpr.CNot(ct, l) |> Some

            | Method("op_Addition", _), [l;r]           -> CExpr.CAdd(ct, l, r) |> Some
            | Method("op_Subtraction", _), [l;r]        -> CExpr.CSub(ct, l, r) |> Some
            | Method("op_Division", _), [l;r]           -> CExpr.CDiv(ct, l, r) |> Some
            | Method("op_Modulus", _), [l;r]            -> CExpr.CMod(ct, l, r) |> Some

            
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
            | (MethodQuote <@ Vec.xy : V4d -> V2d @> _ | Method("get_XY", [VectorOf _])), [v] -> CVecSwizzle(ct, v, CVecComponent.xy) |> Some
            | (MethodQuote <@ Vec.yz : V4d -> V2d @> _ | Method("get_YZ", [VectorOf _])), [v] -> CVecSwizzle(ct, v, CVecComponent.yz) |> Some
            | (MethodQuote <@ Vec.zw : V4d -> V2d @> _ | Method("get_ZW", [VectorOf _])), [v] -> CVecSwizzle(ct, v, CVecComponent.zw) |> Some
            | (MethodQuote <@ Vec.xyz : V4d -> V3d @> _ | Method("get_XYZ", [VectorOf _])), [v] -> CVecSwizzle(ct, v, CVecComponent.xyz) |> Some
            | (MethodQuote <@ Vec.yzw : V4d -> V3d @> _ | Method("get_YZW", [VectorOf _])), [v] -> CVecSwizzle(ct, v, CVecComponent.yzw) |> Some

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

            | MethodQuote <@ fun (a : Arr<1 N, int>) -> a.[0] @> _, [arr; index]
            | Method("GetArray", _), [arr; index] -> 
                CExpr.CItem(ct, arr, index) |> Some


            | ConversionMethod(_,o), [arg]              -> CExpr.CConvert(CType.ofType o, arg) |> Some
            | _ -> None

    let rec tryGetBuiltInCtor (ctor : ConstructorInfo) (args : list<CExpr>) =
        match ctor.DeclaringType with
            | VectorOf(d, t) ->
                CNewVector(CType.ofType ctor.DeclaringType, d, args) |> Some

            | _ ->
                None

    let rec tryGetBuiltInField (fi : FieldInfo) (arg : CExpr) =
        let ct = CType.ofType fi.FieldType
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


    let rec toCExpr (e : Expr) =
        state {
            let! ct = toCType e.Type

            match e with
                | ReducibleExpression e ->
                    return! toCExpr e

                | Var v ->
                    let! v = toCVar v
                    return CVar v

                | Value(v, t) ->
                    let! ct = toCType t
                    match CLiteral.tryCreate v with
                        | Some literal -> 
                            return CValue(ct, literal)

                        | None ->
                            match tryDeconstructValue t v with
                                | Some e -> 
                                    return! toCExpr e

                                | _ -> 
                                    let! e = asExternal e
                                    return e


                | NewTuple(fields) ->
                    let! ctor = e.Type |> Constructors.tuple |> CompilerState.useCtor e.Type
                    let! fields = fields |> List.mapS toCExpr |>> List.toArray
                    return CCall(ctor, fields)

                | NewRecord(t, fields) ->
                    let! ctor = t |> Constructors.record |> CompilerState.useCtor t
                    let! fields = fields |> List.mapS toCExpr |>> List.toArray
                    return CCall(ctor, fields)

                | NewUnionCase(ci, fields) ->
                    let ctors = ci.DeclaringType |> Constructors.union
                    let! ctor = ctors |> HashMap.find ci |> CompilerState.useFunction ci
                    let! fields = fields |> List.mapS toCExpr |>> List.toArray
                    return CCall(ctor, fields)

                | NewObject(ctor, args) ->
                    let! args = args  |> List.mapS toCExpr
                    match tryGetBuiltInCtor ctor args with
                        | Some b ->
                            return b
                        | None -> 
                            let! intrinsic = CompilerState.tryGetIntrinsic ctor
                            match intrinsic with
                                | Some i -> 
                                    return CCallIntrinsic(ct, i, List.toArray args)
                                | None -> 
                                    let! ctor = ctor |> Constructors.custom |> CompilerState.useFunction ctor
                                    return CCall(ctor, List.toArray args)


                | UnionCaseTest(e, ci) ->
                    let! e = toCExpr e
                    let tInt32 = CInt(true, 32)
                    return CEqual(CField(tInt32, e, "tag"), CValue(tInt32, CLiteral.CIntegral(int64 ci.Tag)))

                | TupleGet(t, i) ->
                    let! t = toCExpr t
                    return CField(ct, t, sprintf "Item%d" i)


                | Call(None, mi, []) ->
                    let! intrinsic = CompilerState.tryGetIntrinsic mi
                    match intrinsic with
                        | Some i -> 
                            return CCallIntrinsic(ct, i, [||])
                        | None -> 
                            // TODO: assumes that the function does not have side-effects
                            return! Expr.Value(mi.Invoke(null, [||]), mi.ReturnType) |> toCExpr 

                | Call(None, mi, t :: args) | Call(Some t, mi, args) ->
                    let args = t :: args
                    let! args = args |> List.mapS toCExpr

                    match tryGetBuiltInMethod mi args with
                        | Some e -> 
                            return e
                        | None ->
                            let! intrinsic = CompilerState.tryGetIntrinsic mi
                            match intrinsic with
                                | Some i ->
                                    let args = 
                                        match i.arguments with
                                            | Some order -> order |> List.map (fun i -> args.[i])
                                            | None -> args
                                    return CCallIntrinsic(ct, i, List.toArray args)
                                | _ ->
                                    let! def = mi |> FunctionDefinition.ofMethodBase |> CompilerState.useFunction mi
                                    return CCall(def, List.toArray args)


                | FieldGet(None, f) ->
                    return! Expr.Value(f.GetValue(null), f.FieldType) |> toCExpr
                    
                | FieldGet(Some t, f) ->
                    let! t = toCExpr t
                    match tryGetBuiltInField f t with
                        | Some e ->
                            return e
                        | _ ->
                            return CField(ct, t, f.Name)

                | PropertyGet(None, pi, args) ->
                    return! Expr.Call(pi.GetMethod, args) |> toCExpr

                | PropertyGet(Some t, pi, args) ->
                    return! Expr.Call(t, pi.GetMethod, args) |> toCExpr
                    

                | Coerce(e, t) ->
                    let! e = toCExpr e
                    return CConvert(ct, e)

                | AndAlso(l, r) ->
                    let! l = toCExpr l
                    let! r = toCExpr r
                    return CAnd(l, r)

                | OrElse(l, r) ->
                    let! l = toCExpr l
                    let! r = toCExpr r
                    return COr(l, r)

                | IfThenElse(c, i, e) ->
                    let! c = toCExpr c
                    let! i = toCExpr i
                    let! e = toCExpr e
                    return CConditional(ct, c, i, e)




                | AddressOf e ->
                    let! e = toCExpr e
                    return CAddressOf(CPointer(CPointerModifier.None, e.ctype), e)

                | AddressSet _
                | DefaultValue _
                | FieldSet _
                | ForIntegerRangeLoop _
                | LetRecursive _
                | Let _
                | NewArray _
                | PropertySet _
                | Sequential _
                | TryFinally _
                | TryWith _
                | VarSet _
                | WhileLoop _ ->
                    return! asExternal e

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

    and toCLExpr (e : Expr) =
        e |> toCExpr |> State.map CLExpr.ofExprSafe

    let rec toCRExpr (e : Expr) =
        state {
            match e with
                | ReducibleExpression(e) ->
                    return! toCRExpr e

                | NewFixedArray(cnt, et, args) ->
                    let ct = CType.ofType et
                    let! args = args |> List.mapS toCExpr
                    return CRArray(CArray(ct, cnt), args) |> Some

                | NewArray(et, args) ->
                    let ct = CType.ofType et
                    let cnt = List.length args
                    let! args = args |> List.mapS toCExpr
                    return CRArray(CArray(ct, cnt), args) |> Some


                | Value(null, _) ->
                    return None

                | Value(v, EnumerableOf et) ->
                    let ct = CType.ofType et
                    let enumerable = v |> unbox<System.Collections.IEnumerable>
                    let values = System.Collections.Generic.List<Expr>()
                    let e = enumerable.GetEnumerator()
                    while e.MoveNext() do
                        values.Add(Expr.Value(e.Current, et))
                    
                    return! Expr.NewFixedArray(et, CSharpList.toList values) |> toCRExpr

                | DefaultValue t ->
                    return None

                | _ ->
                    let! res = e |> toCExpr
                    return CRExpr.ofExpr res |> Some
        }

    let rec toCStatement (last : bool) (e : Expr) =
        state {
            match e with
                | ReducibleExpression e ->
                    return! toCStatement last e

                | AddressSet(a, v) ->
                    let! a = toCLExpr a
                    let! v = toCExpr v
                    match a with
                        | Some a ->
                            return CWrite(a, v)
                        | None ->
                            return failwithf "[FShade] cannot set value for ptr %A" e

                | FieldSet(None, f, value) ->
                    return failwithf "[FShade] cannot set static field %A to value %A" f value


                | Let(v, e, b) ->
                    let! v = toCVar v
                    let! e = toCRExpr e
                    let! body = toCStatement last b
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
                    let! l = toCStatement false l
                    let! r = toCStatement last r
                    return CSequential [l;r]

                | VarSet(v, value) ->
                    let! v = toCVar v
                    let! value = toCExpr value
                    return CWrite(CLVar v, value)

                | PropertySet(None, pi, i, a) ->
                    return failwithf "[FShade] cannot set static property %A" pi

                | PropertySet(Some t, pi, i, a) ->
                    let! lexpr = Expr.PropertyGet(t, pi, i) |> toCLExpr
                    match lexpr with
                        | Some l ->
                            let! a = toCExpr a
                            return CWrite(l, a)
                        | None ->
                            return! Expr.Call(t, pi.SetMethod, i @ [a]) |> toCStatement last

                | ForIntegerRangeLoop(v, s, e, b) ->
                    let! v = toCVar v
                    let! s = toCRExpr s
                    let! e = toCExpr e
                    let! body = toCStatement false b

                    return 
                        CFor(
                            CDeclare(v, s),                 // int v = s
                            CLequal(CVar v, e),             // v <= e
                            CIncrement(false, CLVar v),     // v++
                            body                              
                        )

                | WhileLoop(guard, body) ->
                    let! guard = toCExpr guard
                    let! body = toCStatement false body
                    return CWhile(guard, body)

                | IfThenElse(c, i, e) ->
                    let! c = toCExpr c
                    let! i = toCStatement last i
                    let! e = toCStatement last e
                    return CIfThenElse(c, i, e)

                | e ->
                    let! ce = toCRExpr e
                    match ce with
                        | Some (CRExpr e) ->
                            if last then
                                return CReturnValue e
                            else
                                return CDo e
                            
                        | Some (CRArray(t,_) as rhs) -> 
                            if last then 
                                let! name = CompilerState.newName "temp"
                                let cVar = { name = name; ctype = t }
                                return CSequential [
                                    CDeclare(cVar, Some rhs)
                                    CReturnValue(CVar cVar)
                                ]
                            else
                                return CNop

                        | None ->
                            if last then
                                let! name = CompilerState.newName "temp"
                                let cVar = { name = name; ctype = CType.ofType e.Type }
                                return CSequential [
                                    CDeclare(cVar, None)
                                    CReturnValue(CVar cVar)
                                ]
                                
                            else
                                return CNop

        }

    let toCEntryDef (f : EntryPoint) =
        state {
            // ensure that all inputs, outputs, arguments have their correct names
            let! inputs     = f.inputs |> List.mapS toCVar
            let! outputs    = f.outputs |> List.mapS toCVar
            let! args       = f.arguments |> List.mapS toCVar

            // compile the body
            let! body = toCStatement true f.body
            let! ret = toCType f.body.Type

            return {
                cConditional = f.conditional
                cEntryName   = f.entryName
                cInputs      = inputs
                cOutputs     = outputs
                cArguments   = args
                cReturnType  = ret
                cBody        = body
            }
        }

    let toCValueDef (f : FunctionDefinition) =
        state {
            match f with
                | ManagedFunction(name, args, body) ->
                    let signature = f.Signature
                    let! body = toCStatement true body
                    return CFunctionDef(signature, body)

                | CompiledFunction(signature, body) ->
                    return CFunctionDef(signature, body)
                
                | EntryFunction e ->
                    let! e = toCEntryDef e
                    return CEntryDef(e) 
                    
        }


    type TypeGraph private(v : Option<CTypeDef>) =
        let mutable level = 0
        let dependencies = HashSet<TypeGraph>()

        member x.Definition = v
        member x.Dependencies = dependencies :> seq<_>

        member private x.MinLevel (l : int) =
            if l > level then
                level <- l
                for d in dependencies do d.MinLevel (level + 1)

        member x.Level
            with get() = level

        member x.AddDependencies (l : list<TypeGraph>) =
            dependencies.UnionWith l
            for d in l do d.MinLevel (level + 1)

        new() = TypeGraph(None)
        new(d : CTypeDef) = TypeGraph(Some d)
        
    type ValueGraph private(v : Option<CValueDef>) =
        let mutable level = 0
        let dependencies = HashSet<ValueGraph>()

        member x.Definition = v
        member x.Dependencies = dependencies :> seq<_>

        member private x.MinLevel (l : int) =
            if l > level then
                level <- l
                for d in dependencies do d.MinLevel (level + 1)

        member x.Level
            with get() = level

        member x.AddDependencies (l : list<ValueGraph>) =
            dependencies.UnionWith l
            for d in l do d.MinLevel (level + 1)


        new() = ValueGraph(None)
        new(d : CValueDef) = ValueGraph(Some d)

    type ValueGraphCache(b : Backend) =
        let store = Dict<obj, ValueGraph>()
        let mutable typeStore : HashMap<obj, CType> = HashMap.empty

        member x.Backend = b

        member x.UsedTypes
            with get() = typeStore
            and set v = typeStore <- v

        member x.GetOrAdd(def : 'a, create : 'a -> ValueGraph) =
            store.GetOrCreate(def :> obj, fun _ ->
                create def
            )

    type TypeGraphCache(b : Backend) =
        let store = Dict<obj, TypeGraph>()

        member x.Backend = b

        member x.GetOrAdd(def : 'a, create : 'a -> TypeGraph) =
            store.GetOrCreate(def :> obj, fun _ ->
                create def
            )

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TypeGraph = 
        let rec ofType (cache : TypeGraphCache) (t : CType) : TypeGraph =
            cache.GetOrAdd(t, fun t ->
                match t with
                    | CStruct(name, fields, _) ->
                        let res = TypeGraph(CStructDef(name, fields))
                        let nested = fields |> List.map (fst >> ofType cache)
                        res.AddDependencies nested
                        res

                    | CVector(t, _) 
                    | CMatrix(t, _, _) 
                    | CPointer(_, t) 
                    | CArray(t,_) ->
                        let res = TypeGraph()
                        res.AddDependencies [ofType cache t]
                        res
                        
                    | _ ->
                        TypeGraph()
            )

        let ofTypes (cache : TypeGraphCache) (ts : list<CType>) : TypeGraph =
            let deps = ts |> List.map (ofType cache)
            let res = TypeGraph()
            res.AddDependencies deps
            res

        let toList (g : TypeGraph) =
            let rec build (visited : HashSet<TypeGraph>) (g : TypeGraph) =
                if visited.Add g then
                    g.Dependencies |> Seq.iter (build visited)

            let all = HashSet()
            build all g
            all 
                |> Seq.toList
                |> List.sortByDescending (fun g -> g.Level) 
                |> List.choose (fun g -> g.Definition)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ValueGraph = 
        let rec ofFunction (cache : ValueGraphCache) (f : FunctionDefinition) : ValueGraph =
            cache.GetOrAdd(f, fun f -> 
                let mutable state = { emptyState cache.Backend with usedTypes = cache.UsedTypes }
                let def = toCValueDef(f).Run(&state)
                let usedFunctions = state.usedFunctions |> HashMap.toSeq |> Seq.map snd |> Seq.toList
                let entry = ValueGraph(def)
                entry.AddDependencies (usedFunctions |> List.map (ofFunction cache))
                cache.UsedTypes <- state.usedTypes

                entry
            )

        let ofFunctions (cache : ValueGraphCache) (fs : list<FunctionDefinition>) : ValueGraph =
            let deps = fs |> List.map (ofFunction cache)
            let res = ValueGraph()
            res.AddDependencies deps
            res

        let toList (g : ValueGraph) =
            let rec build (visited : HashSet<ValueGraph>) (g : ValueGraph) =
                if visited.Add g then
                    g.Dependencies |> Seq.iter (build visited)

            let all = HashSet()
            build all g
            all 
                |> Seq.toList
                |> List.sortByDescending (fun g -> g.Level) 
                |> List.choose (fun g -> g.Definition)

    let compile (b : Backend) (m : Module) : CModule =
        let cache = ValueGraphCache b
        let valueGraph = m.entries |> List.map EntryFunction |> ValueGraph.ofFunctions cache
        let usedTypes = cache.UsedTypes |> HashMap.toSeq |> Seq.map snd |> Seq.toList
        let typeCache = TypeGraphCache b
        let typeGraph = TypeGraph.ofTypes typeCache usedTypes

        let values = ValueGraph.toList valueGraph
        let types = TypeGraph.toList typeGraph

        {
            types = types
            uniforms = Map.empty
            values = values
        }
