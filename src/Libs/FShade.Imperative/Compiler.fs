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

open FShade
open FShade.Imperative

module Compiler =
    open Aardvark.Base.Monads.State

    [<AllowNullLiteral; AbstractClass>]
    type Backend() =
        let intrinsicFunctions = System.Collections.Concurrent.ConcurrentDictionary<MethodBase, Option<CIntrinsic>>()
        let intrinsicTypes = System.Collections.Concurrent.ConcurrentDictionary<Type, Option<CIntrinsicType>>()

        abstract member TryGetIntrinsicMethod : MethodInfo -> Option<CIntrinsic>
        abstract member TryGetIntrinsicCtor : ConstructorInfo -> Option<CIntrinsic>
        abstract member TryGetIntrinsicType : Type -> Option<CIntrinsicType>

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

        member x.Signature =
            match x with
                | CompiledFunction(s,_) -> 
                    s

                | ManagedFunction(name,args,body) ->
                    CFunctionSignature.ofFunction name args body.Type

                | ManagedFunctionWithSignature(s,_) ->
                    s

    type ConstantDefinition = { cName : string; cType : Type; cValue : Expr }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FunctionDefinition =
        let ofMethodBase (mi : MethodBase) =
            let name = methodName mi
            match Expr.TryGetReflectedDefinition mi with
                | Some (Lambdas(args, body)) ->
                    ManagedFunction(name, List.concat args, body)
                | _ ->
                    failwithf "[FShade] cannot call function %A since it is not reflectable" mi

    type ModuleState =
        {
            backend             : Backend
            constantIndex       : int
            usedTypes           : HashMap<obj, CType>

            globalFunctions     : HashMap<obj, FunctionDefinition>
            globalConstants     : HashMap<obj, ConstantDefinition>

            globalParameters    : Set<string>
        }

    type CompilerState =
        {
            nameIndices         : Map<string, int>
            variables           : Map<Var, CVar>
            reservedNames       : Set<string>

            usedFunctions       : HashMap<obj, FunctionDefinition>
            usedConstants       : pset<ConstantDefinition>
            usedGlobals         : pset<string>
            moduleState         : ModuleState
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
                { s with usedFunctions = HashMap.add key f s.usedFunctions }, f.Signature
            )

        let useGlobalFunction (key : obj) (f : FunctionDefinition) =
            State.custom (fun s ->
                { s with moduleState = { s.moduleState with globalFunctions = HashMap.add key f s.moduleState.globalFunctions } }, f.Signature
            )

        let useCtor (key : obj) (f : FunctionDefinition) =
            useGlobalFunction ((key, "ctor") :> obj) f

        let useConstant (key : obj) (e : Expr) =
            state {
                let ct = CType.ofType e.Type
                let! s = State.get
                match HashMap.tryFind key s.moduleState.globalConstants with
                    | None -> 
                        let name = sprintf "_constant%d" s.moduleState.constantIndex
                        let c = { cName = name; cType = e.Type; cValue = e }
                        do! State.put { 
                                s with 
                                    moduleState = { s.moduleState with globalConstants = HashMap.add key c s.moduleState.globalConstants; constantIndex = s.moduleState.constantIndex + 1 } 
                                    usedConstants = PSet.add c s.usedConstants
                            }
                    
                        return CVar { name = name; ctype = ct }
                    | Some c ->
                        do! State.put { s with usedConstants = PSet.add c s.usedConstants }
                        return CVar { name = c.cName; ctype = ct }

            }

        let tryGetIntrinsic (mi : MethodBase) =
            State.get |> State.map (fun s ->
                if isNull s.moduleState.backend then None
                else s.moduleState.backend.TryGetIntrinsic mi
            )

        type Free =
            | Variable of Var
            | Global of name : string * t : Type * isMutable : bool

        let rec free (e : Expr) : pset<Free> =
            match e with
                | Let(v,e,b) ->
                    let fe = free e
                    let fb = free b
                    PSet.union fe (PSet.remove (Variable v) fb)

                | ReadInput(kind,name,idx) ->
                    match idx with
                        | Some idx -> free idx |> PSet.add (Global(name, e.Type, false))
                        | None -> PSet.ofList [ Global(name, e.Type, false) ]

                | WriteOutputs values ->
                    let mutable res = PSet.empty
                    for (name, (index, value)) in Map.toSeq values do
                        match index with
                            | Some index -> res <- PSet.union res (free index)
                            | _ -> ()
                        res <- res |> PSet.union (free value) |> PSet.add (Global(name, value.Type, true))

                    res

                | ExprShape.ShapeCombination(o, args) ->
                    args |> List.fold (fun m e -> PSet.union m (free e)) PSet.empty

                | ExprShape.ShapeLambda(v, b) ->
                    free b |> PSet.remove (Variable v)

                | ExprShape.ShapeVar v ->
                    PSet.ofList [ Variable v ]

    let emptyState (m : ModuleState) =
        {
            nameIndices         = Map.empty
            variables           = Map.empty
            reservedNames       = Set.empty
            usedFunctions       = HashMap.empty
            usedConstants       = PSet.empty
            usedGlobals         = PSet.empty
            moduleState         = m
        }

    let toCType (t : Type) =
        State.custom (fun s ->
            match s.moduleState.backend.TryGetIntrinsic t with
                | Some ci -> 
                    s, CType.CIntrinsic ci

                | None -> 
                    let cType = CType.ofType t

                    match cType with
                        | CStruct _ ->
                            { s with moduleState = { s.moduleState with ModuleState.usedTypes = HashMap.add (t :> obj) cType s.moduleState.usedTypes } }, cType
                        | _ ->
                            s, cType
        )
        
    /// converts a variable to a CVar using the cached name or by
    /// creating a fresh name for it (and caching it)
    let toCVarOfType (v : Var) (t : CType) =
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

    let toCVar (v : Var) =
        state {
            let! t = toCType v.Type
            return! toCVarOfType v t
        }

    /// converts an EntryParameter to a CEntryParameter 
    /// NOTE that names are not treated here
    let toCEntryParameter (p : EntryParameter) =
        state {
            let! ctype = toCType p.paramType
            return {
                cParamType           = ctype
                cParamName           = p.paramName
                cParamSemantic       = p.paramSemantic
                cParamDecorations    = p.paramDecorations
            }
        }
        
    /// converts an Uniform to a CUniform
    /// NOTE that names are not treated here
    let toCUniform (v : Uniform) =
        state {
            let! ct = toCType v.uniformType
            return {
                cUniformType = ct
                cUniformName = v.uniformName
                cUniformBuffer = v.uniformBuffer
            }
        }

    [<AutoOpen>]
    module Helpers = 
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
                let free = CompilerState.free e
                if PSet.isEmpty free then
                    return! CompilerState.useConstant e e
                else
                    let! globals = State.get |> State.map (fun s -> s.moduleState.globalParameters)
                    let free = free |> PSet.toArray
 
                    let newVariables = HashSet()

                    let args = 
                        free |> Array.choose (fun f ->
                            match f with
                                | CompilerState.Free.Variable v ->
                                    Some v

                                | CompilerState.Free.Global(n, t, isMutable) ->
                                    if Set.contains n globals then 
                                        None
                                    else 
                                        let v = Var(n, t, isMutable)
                                        newVariables.Add v |> ignore
                                        Some v
                        )

                    
                    let! variables = 
                        args |> Array.mapS (fun v ->
                            state {
                                if newVariables.Contains v then
                                    let! t = toCType v.Type
                                    let res = { ctype = t; name = v.Name }
                                    do! State.modify (fun s -> { s with variables = Map.add v res s.variables })
                                    return res
                                else
                                    let! res = toCVar v
                                    return res
                            }
                        ) 

                    let! name = CompilerState.newName "helper"
                    let! returnType = toCType e.Type
                    let variables = variables

                    let signature =
                        {
                            name = name
                            returnType = returnType
                            parameters = 
                                Array.map2 
                                    (fun (a : CVar) (v : Var) -> { name = a.name; ctype = a.ctype; modifier = (if v.IsMutable then CParameterModifier.ByRef else CParameterModifier.In) }) 
                                    variables
                                    args
                        }

                    let definition = ManagedFunctionWithSignature(signature, e)

                    let! signature = 
                        if args.Length = free.Length then CompilerState.useGlobalFunction (e :> obj) definition
                        else CompilerState.useLocalFunction (e :> obj) definition


                    return CCall(signature, variables |> Array.map CVar)
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

                | Method("get_Item", [ArrOf(_,_); _]), [arr; index]
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

        let tryGetBuiltInField (fi : FieldInfo) (arg : CExpr) =
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

        let tryGetBuiltInStaticProperty (t : CType) (pi : PropertyInfo) =
            match pi.Name with
                | "Zero"    -> zero t |> Some
                | "One"     -> one t |> Some
                | _         -> None

    let rec toCExpr (e : Expr) =
        state {
            let! ct = toCType e.Type

            match e with
                | ReducibleExpression e ->
                    return! toCExpr e

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
                    return! asExternal e

                | ReadInput(kind, name, index) ->
                    let! s = State.get
                    if Set.contains name s.moduleState.globalParameters then
                        do! State.put { s with usedGlobals = PSet.add name s.usedGlobals }

                    match index with
                        | Some idx -> 
                            let! idx = toCExpr idx
                            return CReadInput(kind, ct, name, Some idx)
                        | _ ->
                            return CReadInput(kind, ct, name, None)


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

                | GetArray(arr, i) ->
                    let! arr = toCExpr arr
                    let! i = toCExpr i
                    return CItem(ct, arr, i)

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
                    let! ctor = ctors |> HashMap.find ci |> CompilerState.useGlobalFunction ci
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
                                    let! ctor = ctor |> Constructors.custom |> CompilerState.useGlobalFunction ctor
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
                                    let! def = mi |> FunctionDefinition.ofMethodBase |> CompilerState.useGlobalFunction mi
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

                | PropertyGet(None, pi, []) ->
                    return! Expr.Value(pi.GetValue(null), pi.PropertyType) |> toCExpr
                    
                | PropertyGet(None, pi, args) ->
                    return! Expr.Call(pi.GetMethod, args) |> toCExpr

                | PropertyGet(Some t, pi, []) ->
                    if FSharpType.IsRecord(pi.DeclaringType, true) then
                        let! t = toCExpr t
                        return CField(ct, t, pi.Name)

                    elif FSharpType.IsUnion(pi.DeclaringType.BaseType, true) then
                        let case = 
                            FSharpType.GetUnionCases(pi.DeclaringType.BaseType, true)
                                |> Seq.find (fun ci -> ci.Name = pi.DeclaringType.Name)
                        let! t = toCExpr t
                        return CField(ct, t, case.Name + "_" + pi.Name)
                        
                    else
                        return! Expr.Call(t, pi.GetMethod, []) |> toCExpr
                    
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

    let rec toCStatement (isLast : bool) (e : Expr) =
        state {
            match e with
                | ReducibleExpression e ->
                    return! toCStatement isLast e

                | Quotations.DerivedPatterns.Unit ->
                    return CNop

                | ForInteger(v, first, step, last, b) ->
                    match step, last with
                        | Trivial, TrivialOp -> 
                            let! v = toCVar v
                            let! cfirst = toCRExpr first
                            let! cstep = toCExpr step
                            let! clast = toCExpr last
                            let! cbody = toCStatement false b

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
                            return! toCStatement isLast (Expr.Let(vLast, last, Expr.ForInteger(v, first, step, Expr.Var vLast, b)))

                        | step, TrivialOp -> 
                            let vStep = Var("step", step.Type)
                            return! toCStatement isLast (Expr.Let(vStep, step, Expr.ForInteger(v, first, Expr.Var vStep, last, b)))

                        | step, last ->
                            let vStep = Var("step", step.Type)
                            let vLast = Var("last", last.Type)
                            return! toCStatement isLast (Expr.Let(vStep, step, Expr.Let(vLast, last, Expr.ForInteger(v, first, Expr.Var vStep, Expr.Var vLast, b))))
                            

                            
                | WriteOutputs values ->
                    let! writes =
                        values |> Map.toList |> List.mapS (fun (name, (index, value)) ->
                            state {
                                let! index = 
                                    match index with
                                        | Some index -> toCExpr index |> State.map Some
                                        | _ -> State.value None

                                let! value = toCStatement true value
                                let rec replaceReturn (s : CStatement) =
                                    match s with
                                        | CReturnValue v -> CWriteOutput(name, index, CRExpr v)
                                        | CSequential l -> l |> List.map replaceReturn |> CSequential
                                        | _ -> s

                                return replaceReturn value
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
                    let! a = toCLExpr a
                    let! v = toCExpr v
                    match a with
                        | Some a ->
                            return CWrite(a, v)
                        | None ->
                            return failwithf "[FShade] cannot set value for ptr %A" e

                | FieldSet(Some t, f, value) ->
                    let! t = toCLExpr (Expr.FieldGet(t, f))
                    match t with
                        | Some t ->
                            let! value = toCExpr value
                            return CWrite(t, value)
                        | None ->
                            return failwithf "[FShade] cannot set field %A" e

                | FieldSet(None, f, value) ->
                    return failwithf "[FShade] cannot set static field %A to value %A" f value


                | Let(v, e, b) ->
                    let! e = toCRExpr e
                    let! v = 
                        match e with
                            | Some e -> toCVarOfType v e.ctype
                            | None -> toCVar v

                    let! body = toCStatement isLast b
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
                    let! r = toCStatement isLast r
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
                            return! Expr.Call(t, pi.SetMethod, i @ [a]) |> toCStatement isLast


                | WhileLoop(guard, body) ->
                    let! guard = toCExpr guard
                    let! body = toCStatement false body
                    return CWhile(guard, body)

                | IfThenElse(c, i, e) ->
                    let! c = toCExpr c
                    let! i = toCStatement isLast i
                    let! e = toCStatement isLast e
                    return CIfThenElse(c, i, e)

                | e ->
                    let! ce = toCRExpr e
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
                                let cVar = { name = name; ctype = CType.ofType e.Type }
                                return CSequential [
                                    CDeclare(cVar, None)
                                    CReturnValue(CVar cVar)
                                ]
                                
                            else
                                return CNop

        }


    let compileUniforms (u : list<Uniform>) =
        state {
            let! us = u |> List.mapS toCUniform
            return CUniformDef us
        }

    let compileEntry (f : EntryPoint) =
        state {
            // ensure that all inputs, outputs, arguments have their correct names
            for i in f.inputs do do! CompilerState.reserveName i.paramName
            for o in f.outputs do do! CompilerState.reserveName o.paramName
            for a in f.arguments do do! CompilerState.reserveName a.paramName
            for u in f.uniforms do do! CompilerState.reserveName u.uniformName
            
            let! inputs     = f.inputs |> List.mapS toCEntryParameter
            let! outputs    = f.outputs |> List.mapS toCEntryParameter
            let! args       = f.arguments |> List.mapS toCEntryParameter


            // compile the body
            let! body = toCStatement true f.body
            let! ret = toCType f.body.Type

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

    let compileFunction (f : FunctionDefinition) =
        state {
            match f with
                | ManagedFunction(name, args, body) ->
                    let signature = f.Signature
                    let! body = toCStatement true body
                    return CFunctionDef(signature, body)

                | CompiledFunction(signature, body) ->
                    return CFunctionDef(signature, body)
                    
                | ManagedFunctionWithSignature(signature, body) ->
                    let! body = toCStatement true body
                    return CFunctionDef(signature, body)

        }

    let compileConstant (c : ConstantDefinition) =
        state {
            let! ct = toCType c.cType
            let! e = toCRExpr c.cValue
            match e with
                | Some e ->
                    return CValueDef.CConstant(ct, c.cName, e)
                | None ->
                    return failwithf "[FShade] constants must have a value %A" c
        }
