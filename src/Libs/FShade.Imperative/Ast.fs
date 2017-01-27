namespace FShade.Imperative


open System
open System.Reflection
open System.Collections.Generic

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open Aardvark.Base
open Aardvark.Base.TypeInfo
open Aardvark.Base.TypeInfo.Patterns

open FShade

type CIntrinsicType =
    {
        intrinsicTypeName   : string
        tag                 : obj
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CIntrinsicType =
    let simple (name : string) =
        {
            intrinsicTypeName = name
            tag = null
        }

    let tagged (tag : obj) =
        {
            intrinsicTypeName = null
            tag = tag
        }


/// represents a pointer modifier (const, etc.)
type CPointerModifier =
    | None      = 0x00
    | Const     = 0x01

/// represents a type available in C-style languages
type CType =
    | CBool
    | CVoid
    | CInt of signed : bool * width : int
    | CFloat of width : int

    | CVector of elementType : CType * dim : int
    | CMatrix of elementType : CType * rows : int * cols : int

    | CArray of elementType : CType * length : int
    | CPointer of modifier : CPointerModifier * elementType : CType
    | CStruct of name : string * fields : list<CType * string> * original : Option<Type>
    | CIntrinsic of CIntrinsicType

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CType =
    let private primitiveTypes =
        Dictionary.ofList [
            typeof<unit>, CType.CVoid
            typeof<System.Void>, CType.CVoid
            typeof<bool>, CType.CBool

            typeof<int8>, CType.CInt(true, 8)
            typeof<int16>, CType.CInt(true, 16)
            typeof<int32>, CType.CInt(true, 32)
            typeof<int64>, CType.CInt(true, 64)
            typeof<nativeint>, CType.CInt(true, sizeof<nativeint>)
            typeof<uint8>, CType.CInt(false, 8)
            typeof<uint16>, CType.CInt(false, 16)
            typeof<uint32>, CType.CInt(false, 32)
            typeof<uint64>, CType.CInt(false, 64)
            typeof<unativeint>, CType.CInt(false, sizeof<nativeint>)
            
            typeof<float16>, CType.CFloat(16)
            typeof<float32>, CType.CFloat(32)
            typeof<float>, CType.CFloat(64)
            typeof<decimal>, CType.CFloat(64)

        ]

    let private typeCache =
        let dict = System.Collections.Concurrent.ConcurrentDictionary<Type, CType>()
        for (k,v) in Dictionary.toSeq primitiveTypes do
            dict.[k] <- v
        dict

    /// creates a c representation for a given system type
    let rec ofType (t : Type) : CType =
        typeCache.GetOrAdd(t, fun t ->
            match t with
                | VectorOf(d, t)    -> CVector(ofType t, d)
                | MatrixOf(s, t)    -> CMatrix(ofType t, s.Y, s.X)
                | ArrOf(len, t)     -> CArray(ofType t, len)
                | Ref t             -> ofType t
                | t when t.IsArray  -> CType.CPointer(CPointerModifier.None, ofType (t.GetElementType()))
                | t                 -> ofCustomType t
        )

    /// creates a struct representation for a given system type
    and private ofCustomType (t : Type) =
        let name = typeName t
        if FSharpType.IsRecord(t, true) then
            let fields = FSharpType.GetRecordFields(t, true) |> Array.toList |> List.map (fun pi -> ofType pi.PropertyType, pi.Name) 
            CStruct(name, fields, Some t)
            
        elif FSharpType.IsTuple t then
            let fields = FSharpType.GetTupleElements(t) |> Array.toList |> List.mapi (fun i t -> ofType t, sprintf "Item%d" i)
            CStruct(name, fields, Some t)

        elif FSharpType.IsUnion(t, true) then
            let caseFields = 
                FSharpType.GetUnionCases(t, true) |> Array.toList |> List.collect (fun ci ->
                    ci.GetFields() |> Array.toList |> List.map (fun fi ->
                        let name = ci.Name + "_" + fi.Name
                        ofType fi.PropertyType, name
                    )
                )

            let tagField = (ofType typeof<int>, "tag")
            CStruct(name, tagField :: caseFields, Some t)
            
        else
            let fields = t.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
            let fields = fields |> Array.toList |> List.map (fun fi -> ofType fi.FieldType, fi.Name) 
            CStruct(name, fields, Some t)
   
       
/// represents a function-parameter modifier
type CParameterModifier =
    | In
    | ByRef

/// represents a parameter for a function
type CParameter =
    {
        name        : string
        ctype       : CType
        modifier    : CParameterModifier
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CParameter =

    /// creates a CParameter for a given ParameterInfo
    let ofParameterInfo (p : ParameterInfo) =
        let paramType, isByRef =
            if p.ParameterType.IsByRef then
                p.ParameterType.GetElementType(), true
            else
                p.ParameterType, false

        {
            name = p.Name
            ctype = CType.ofType paramType
            modifier = (if isByRef then CParameterModifier.ByRef else CParameterModifier.In)
        }

    /// creates a CParameter for a given Var
    let ofVar (p : Var) =
        let paramType, isByRef =
            if p.Type.IsByRef then
                p.Type.GetElementType(), true
            elif p.IsMutable then
                p.Type, true
            else
                p.Type, false

        {
            name = p.Name
            ctype = CType.ofType paramType
            modifier = (if isByRef then CParameterModifier.ByRef else CParameterModifier.In)
        }

/// represents a function signature
type CFunctionSignature = 
    { 
        name        : string 
        returnType  : CType
        parameters  : CParameter[]
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CFunctionSignature =

    /// creates a CFunctionSignature for a given MethodInfo
    let ofMethodInfo (mi : MethodInfo) =
        {
            name = methodName mi
            returnType = CType.ofType mi.ReturnType
            parameters = mi.GetParameters() |> Array.map CParameter.ofParameterInfo
        }

    let ofFunction (name : string) (args : list<Var>) (ret : Type) =
        {
            name = name
            returnType = CType.ofType ret
            parameters = args |> List.toArray |> Array.map CParameter.ofVar
        }
        

/// represents a variable
type CVar = 
    { 
        name    : string
        ctype   : CType 
    }

/// represents a literal primitive value
type CLiteral =
    | Null
    | CBool of bool
    | CIntegral of int64
    | CFractional of float
    | CString of string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CLiteral =
    /// tries to create a literal value for the given object
    let tryCreate (value : obj) =
        match value with
            | null                  -> CLiteral.Null |> Some
            | :? bool as v          -> CLiteral.CBool(v) |> Some
            | :? int8 as v          -> CLiteral.CIntegral(int64 v) |> Some
            | :? int16 as v         -> CLiteral.CIntegral(int64 v) |> Some
            | :? int32 as v         -> CLiteral.CIntegral(int64 v) |> Some
            | :? int64 as v         -> CLiteral.CIntegral(v) |> Some
            | :? nativeint as v     -> CLiteral.CIntegral(int64 v) |> Some
            | :? uint8 as v         -> CLiteral.CIntegral(int64 v) |> Some
            | :? uint16 as v        -> CLiteral.CIntegral(int64 v) |> Some
            | :? uint32 as v        -> CLiteral.CIntegral(int64 v) |> Some
            | :? uint64 as v        -> CLiteral.CIntegral(int64 v) |> Some
            | :? unativeint as v    -> CLiteral.CIntegral(int64 v) |> Some


            | :? float16 as v       -> CLiteral.CFractional(float v.Float32) |> Some
            | :? float32 as v       -> CLiteral.CFractional(float v) |> Some
            | :? float as v         -> CLiteral.CFractional(v) |> Some
            | :? decimal as v       -> CLiteral.CFractional(float v) |> Some
            | :? string as v        -> CLiteral.CString(v) |> Some
            | _                     -> None

type CVecComponent =
    | X = 0
    | Y = 1
    | Z = 2
    | W = 3

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CVecComponent =
    let private all = [CVecComponent.X; CVecComponent.Y; CVecComponent.Z; CVecComponent.W]
    let first (n : int) = List.take n all

    let xy = [CVecComponent.X; CVecComponent.Y]
    let yz = [CVecComponent.Y; CVecComponent.Z]
    let zw = [CVecComponent.Z; CVecComponent.W]
    let xyz = [CVecComponent.X; CVecComponent.Y; CVecComponent.Z]
    let yzw = [CVecComponent.Y; CVecComponent.Z; CVecComponent.W]

type CIntrinsic =
    {
        intrinsicName   : string
        tag             : obj
        arguments       : Option<list<int>>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CIntrinsic =
    let simple (name : string) =
        {
            intrinsicName = name
            tag = null
            arguments = None
        }

    let custom (name : string) (args : list<int>)=
        {
            intrinsicName = name
            tag = null
            arguments = Some args
        }

    let tagged (tag : obj)=
        {
            intrinsicName = null
            tag = tag
            arguments = None
        }

/// represents a c-style expression
type CExpr =
    | CVar of CVar
    | CValue of CType * CLiteral
    | CCall of func : CFunctionSignature * args : CExpr[]
    | CCallIntrinsic of t : CType * func : CIntrinsic * args : CExpr[]
    | CConditional of ctype : CType * cond : CExpr * ifTrue : CExpr * ifFalse : CExpr

    | CReadInput of kind : ParameterKind * ctype : CType * name : string * index : Option<CExpr>

    | CNeg of CType * CExpr
    | CNot of CType * CExpr

    | CAdd of CType * CExpr * CExpr
    | CSub of CType * CExpr * CExpr
    | CMul of CType * CExpr * CExpr
    | CDiv of CType * CExpr * CExpr
    | CMod of CType * CExpr * CExpr

    | CTranspose of CType * CExpr
    | CMulMatMat of CType * CExpr * CExpr
    | CMulMatVec of CType * CExpr * CExpr
    | CDot of CType * CExpr * CExpr
    | CCross of CType * CExpr * CExpr
    | CVecSwizzle of CType * CExpr * list<CVecComponent>
    | CMatrixElement of t : CType * m : CExpr * r : int * c : int
    | CNewVector of t : CType * d : int * components : list<CExpr>
    | CMatrixFromRows of t : CType * rows : list<CExpr>
    | CMatrixFromCols of t : CType * cols : list<CExpr>
    | CVecLength of t : CType * v : CExpr

    | CConvert of CType * CExpr

    | CAnd of CExpr * CExpr
    | COr of CExpr * CExpr
    | CBitAnd of CType * CExpr * CExpr
    | CBitOr of CType * CExpr * CExpr
    | CBitXor of CType * CExpr * CExpr

    | CLess of CExpr * CExpr
    | CLequal of CExpr * CExpr
    | CGreater of CExpr * CExpr
    | CGequal of CExpr * CExpr
    | CEqual of CExpr * CExpr
    | CNotEqual of CExpr * CExpr

    | CAddressOf of t : CType * target : CExpr
    | CField of t : CType * target : CExpr * fieldName : string
    | CItem of t : CType * target : CExpr * index : CExpr 

    member x.ctype =
        match x with
            | CVar v -> v.ctype
            | CValue(t,_) -> t
            | CCall(f,_) -> f.returnType
            | CCallIntrinsic(t,_,_) -> t
            | CConditional(t,_,_,_) -> t
            | CReadInput(_,t,_,_) -> t
            | CNeg(t,_) -> t
            | CNot(t,_) -> t
            | CAdd(t,_,_) -> t
            | CSub(t,_,_) -> t
            | CMul(t,_,_) -> t
            | CDiv(t,_,_) -> t
            | CMod(t,_,_) -> t
            | CConvert(t,_) -> t
            | CAnd _ | COr _ -> CType.CBool
            | CBitAnd(t,_,_) -> t
            | CBitOr(t,_,_) -> t
            | CBitXor(t,_,_) -> t

            | CLess _ | CLequal _ | CGreater _ | CGequal _ -> CType.CBool
            | CEqual _ | CNotEqual _ -> CType.CBool

            | CTranspose(t,_) -> t
            | CMulMatMat(t,_,_) -> t
            | CMulMatVec(t,_,_) -> t
            | CDot(t,_,_) -> t
            | CCross(t,_,_) -> t
            | CVecSwizzle(t,_,_) -> t
            | CMatrixElement(t,_,_,_) -> t
            | CNewVector(t,_,_) -> t
            | CMatrixFromRows(t,_) -> t
            | CMatrixFromCols(t,_) -> t
            | CVecLength(t,_) -> t



            | CAddressOf(t,_) -> t
            | CField(t,_,_) -> t
            | CItem(t,_,_) -> t


type internal Used() =
    let types = HashSet<CType>()
    let intrinsics = HashSet<CIntrinsic>()
        
    member x.Types = types
    member x.Intrinsics = intrinsics

    member x.AddType (t : CType) =
        if types.Add t then
            match t with
                | CMatrix(e,r,c) -> 
                    x.AddType(CVector(e, c))
                    x.AddType(CVector(e, r))
                    x.AddType e

                | CArray(e,_) | CPointer(_,e) | CVector(e,_) -> 
                    x.AddType e


                | CStruct(_,fields,_) ->
                    for (t,_) in fields do
                        x.AddType t
                | _ ->
                    ()

    member x.AddIntrinsic (i : CIntrinsic) =
        intrinsics.Add i |> ignore

    member x.AddInput (kind : ParameterKind, t : CType, name : string, indexed : bool) =
        ()

    member x.AddOutput (t : CType, name : string, indexed : bool) =
        ()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CExpr =

    let tryCreateValue (valueType : Type) (value : obj) =
        let t = CType.ofType valueType
        match CLiteral.tryCreate value with
            | Some literal ->
                CValue(t, literal) |> Some
            | None ->
                None

    let rec internal visit (used : Used) (e : CExpr) =
        match e with
            | CVar v -> 
                used.AddType v.ctype

            | CValue(t,_) ->
                used.AddType t
                
            | CCall(func, args) ->
                used.AddType func.returnType
                for a in args do
                    visit used a

            | CCallIntrinsic(t, func, args) ->
                used.AddType t
                used.AddIntrinsic func
                for a in args do visit used a
                
            | CConditional(t, cond, i, e) ->
                used.AddType t
                visit used cond
                visit used i
                visit used e

            | CReadInput(k,t,n,i) ->
                used.AddType t
                used.AddInput(k, t, n, Option.isSome i)
                match i with
                    | Some i -> visit used i
                    | _ -> ()

            | CNeg(t,e) | CNot(t,e) | CTranspose(t,e) | CVecLength(t,e) | CConvert(t,e) | CAddressOf(t,e) ->
                used.AddType t
                visit used e

            | CAdd(t,l,r) | CSub(t,l,r) | CMul(t,l,r) | CDiv(t,l,r) | CMod(t,l,r)
            | CMulMatMat(t,l,r) | CMulMatVec(t,l,r) | CDot(t,l,r) | CCross(t,l,r)
            | CBitAnd(t,l,r) | CBitOr(t,l,r) | CBitXor(t,l,r) ->
                used.AddType t
                visit used l
                visit used r

            | CVecSwizzle(t,e,_) ->
                used.AddType t
                visit used e

            | CMatrixElement(t, m, _, _) ->
                used.AddType t
                visit used m

            | CNewVector(t,_,c) ->
                used.AddType t
                for c in c do visit used c

            | CMatrixFromRows(t,r) | CMatrixFromCols(t,r) ->
                used.AddType t
                for r in r do visit used r


            | CAnd(l,r) | COr(l,r)
            | CLess(l,r) | CLequal(l,r) | CGreater(l,r) | CGequal(l,r) | CEqual(l,r) | CNotEqual(l,r) ->
                used.AddType CType.CBool
                visit used l
                visit used r
            
            | CField(t, target,_ ) ->
                used.AddType t
                visit used target

            | CItem(t, target, index) ->
                used.AddType t
                visit used target
                visit used index

/// represents a c-style rhs-expression
type CRExpr =
    | CRExpr of CExpr
    | CRArray of arrayType : CType * values : list<CExpr>

    member x.ctype =
        match x with
            | CRExpr e -> e.ctype
            | CRArray(t,_) -> t
   
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CRExpr =
    let ofExpr (e : CExpr) = CRExpr(e)
    let toExpr (e : CRExpr) =
        match e with
            | CRExpr e -> e
            | CRArray _ -> failwithf "[FShade] cannot convert CRExpr to CExpr (%A)" e
          
    let rec internal visit (used : Used) (e : CRExpr) =
        match e with
            | CRArray(t,args) ->
                used.AddType t
                for a in args do CExpr.visit used a
            | CRExpr e ->
                CExpr.visit used e
            
/// represents a c-style lhs-expression
type CLExpr =
    | CLVar of CVar
    | CLField of CType * CLExpr * string
    | CLItem of CType * CLExpr * CExpr
    | CLPtr of CType * CExpr
    | CLVecSwizzle of CType * CLExpr * list<CVecComponent>
    | CLMatrixElement of t : CType * m : CLExpr * r : int * c : int

    member x.ctype =
        match x with
            | CLVar v -> v.ctype
            | CLField(t,_,_) -> t
            | CLItem(t,_,_) -> t
            | CLPtr(t,_) -> t
            | CLVecSwizzle(t, _, _) -> t
            | CLMatrixElement(t, _, _, _) -> t

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CLExpr =
    open Aardvark.Base.Monads.Option

    let rec ofExpr (e : CExpr) =
        match e with
            | CVar v -> CLVar v
            | CField(t, e, name) -> CLField(t, ofExpr e, name)
            | CItem(t, e, index) -> CLItem(t, ofExpr e, index)
            | CAddressOf(t, e) -> CLPtr(t, e)
            | CVecSwizzle(t, e, c) -> CLVecSwizzle(t, ofExpr e, c)
            | CMatrixElement(t, e, r, c) -> CLMatrixElement(t, ofExpr e, r, c)
            | _ -> failwithf "[FShade] cannot convert CExpr toCLExpr (%A)" e
            
    let rec ofExprSafe (e : CExpr) =
        option {
            match e with
                | CVar v -> 
                    return CLVar v

                | CField(t, e, name) -> 
                    let! e = ofExprSafe e
                    return CLField(t, e, name)

                | CItem(t, e, index) -> 
                    let! e = ofExprSafe e
                    return CLItem(t, e, index)

                | CAddressOf(t, e) -> 
                    return CLPtr(t, e)

                | CVecSwizzle(t, e, c) ->
                    let! e = ofExprSafe e
                    return CLVecSwizzle(t, e, c)

                | CMatrixElement(t, e, r, c) ->
                    let! e = ofExprSafe e
                    return CLMatrixElement(t, e, r, c)

                | _ -> 
                    return! None
        }

    let rec toExpr (e : CLExpr) =
        match e with
            | CLVar v -> CVar v
            | CLField(t, e, name) -> CField(t, toExpr e, name)
            | CLItem(t, e, index) -> CItem(t, toExpr e, index)
            | CLPtr(t, e) -> CAddressOf(t, e)
            | CLVecSwizzle(t, e, c) -> CVecSwizzle(t, toExpr e, c)
            | CLMatrixElement(t, e, r, c) -> CMatrixElement(t, toExpr e, r, c)

    let rec internal visit (used : Used) (e : CLExpr) =
        match e with
            | CLVar v -> 
                used.AddType v.ctype

            | CLField(t,target,_) ->
                used.AddType t
                visit used target

            | CLItem(t, target, index) ->
                used.AddType t
                visit used target
                CExpr.visit used index

            | CLPtr(t,e) ->
                used.AddType t
                CExpr.visit used e
                
            | CLVecSwizzle(t,e,_) ->
                used.AddType t
                visit used e

            | CLMatrixElement(t,m,_,_) ->
                used.AddType t
                visit used m
                



/// represents a c-style statement
type CStatement =
    | CNop
    | CDo of CExpr
    | CDeclare of var : CVar * rhs : Option<CRExpr>
    | CWrite of lhs : CLExpr * value : CExpr

    | CIncrement of pre : bool * CLExpr
    | CDecrement of pre : bool * CLExpr

    | CSequential of list<CStatement>
    | CReturn

    | CWriteOutput of string * Option<CExpr> * CRExpr

    | CReturnValue of CExpr
    | CBreak
    | CContinue
    | CFor of init : CStatement * cond : CExpr * step : CStatement * body : CStatement
    | CWhile of guard : CExpr * body : CStatement
    | CDoWhile of guard : CExpr * body : CStatement
    | CIfThenElse of cond : CExpr * ifTrue : CStatement * ifFalse : CStatement
    | CSwitch of value : CExpr * cases : array<CLiteral * CStatement>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CStatement =
    
    let rec internal visit (used : Used) (s : CStatement) =
        match s with
            | CNop | CReturn | CBreak | CContinue -> 
                ()
            | CDo e ->
                CExpr.visit used e
            | CDeclare(v,r) ->
                used.AddType v.ctype
                match r with
                    | Some r -> CRExpr.visit used r
                    | _ -> ()
            | CWrite(l,r) ->
                CLExpr.visit used l
                CExpr.visit used r

            | CIncrement(_,v) | CDecrement(_,v) ->
                CLExpr.visit used v

            | CSequential s ->
                for s in s do visit used s

            | CWriteOutput(name, index, value) ->
                match index with
                    | Some i -> CExpr.visit used i
                    | _ -> ()
                CRExpr.visit used value
                used.AddOutput(value.ctype, name, Option.isSome index)


            | CReturnValue v ->
                CExpr.visit used v

            | CFor(init, cond, step, body) ->
                visit used init
                CExpr.visit used cond
                visit used step
                visit used body

            | CWhile(guard, body) | CDoWhile(guard, body) ->
                CExpr.visit used guard
                visit used body

            | CIfThenElse(c,i,e) ->
                CExpr.visit used c
                visit used i
                visit used e

            | CSwitch(value, cases) ->
                CExpr.visit used value
                for (_,b) in cases do 
                    visit used b


type CEntryParameter =
    {
        cParamType           : CType
        cParamName           : string
        cParamSemantic       : string
        cParamDecorations    : Set<ParameterDecoration>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CEntryParameter =
    let internal visit (used : Used) (e : CEntryParameter) =
        used.AddType e.cParamType

type CEntryDef =
    {
        cEntryName   : string
        cInputs      : list<CEntryParameter>
        cOutputs     : list<CEntryParameter>
        cArguments   : list<CEntryParameter>
        cReturnType  : CType
        cBody        : CStatement
        cDecorations : list<EntryDecoration>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CEntryDef =
    let internal visit (used : Used) (e : CEntryDef) =
        e.cInputs |> List.iter (CEntryParameter.visit used)
        e.cOutputs |> List.iter (CEntryParameter.visit used)
        e.cArguments |> List.iter (CEntryParameter.visit used)
        used.AddType e.cReturnType
        CStatement.visit used e.cBody


type CUniform =
    {
        cUniformType         : CType
        cUniformName         : string
        cUniformBuffer       : Option<string>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CUniform =
    let mergeMany (groups : seq<list<CUniform>>) =
        let all = Dictionary.empty
        let res = CSharpList.empty
        for uniforms in groups do
            for u in uniforms do
                match all.TryGetValue u.cUniformName with
                    | (true, o) ->
                        if o.cUniformType <> u.cUniformType then
                            failwithf "[FShade] conflicting uniform type for %A (%A vs %A)" u.cUniformName o.cUniformType u.cUniformType

                    | _ ->
                        all.[u.cUniformName] <- u
                        res.Add u

        res |> CSharpList.toList

    let internal visit (used : Used) (u : CUniform) =
        used.AddType u.cUniformType

type CValueDef =
    | CConstant of ctype : CType * name : string * init : CRExpr
    | CFunctionDef of signature : CFunctionSignature * body : CStatement
    | CEntryDef of CEntryDef
    | CConditionalDef of string * list<CValueDef>
    | CUniformDef of list<CUniform>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CValueDef =
    let rec internal visit (used : Used) (d : CValueDef) =
        match d with
            | CConstant(t, _, e) ->
                used.AddType t
                CRExpr.visit used e
            | CFunctionDef(s, b) ->
                used.AddType s.returnType
                for p in s.parameters do
                    used.AddType p.ctype
                CStatement.visit used b

            | CEntryDef e ->
                CEntryDef.visit used e

            | CConditionalDef(_,d) ->
                for d in d do visit used d

            | CUniformDef d ->
                for d in d do CUniform.visit used d

type CTypeDef =
    | CStructDef of name : string * fields : list<CType * string>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CTypeDef =
    let rec internal visit (used : Used) (d : CTypeDef) =
        match d with
            | CStructDef(name, fields) ->
                for (t,_) in fields do
                    used.AddType t 

type CModule =
    {
        types       : list<CTypeDef>
        values      : list<CValueDef>
    }
    member x.uniforms =
        let rec allUniforms (c : CValueDef) =
            match c with
                | CConditionalDef(c, inner) ->
                    inner |> List.collect allUniforms

                | CUniformDef us ->
                    [us]

                | _ ->
                    []

        x.values |> List.collect allUniforms |> CUniform.mergeMany


type UsageInfo =
    {
        usedTypes       : pset<CType>
        usedntrinsics   : pset<CIntrinsic>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CModule =
    let internal visit (used : Used) (m : CModule) =
        for t in m.types do CTypeDef.visit used t
        for v in m.values do CValueDef.visit used v

    let usageInfo (m : CModule) =
        let u = Used()
        visit u m

        {
            usedTypes = PSet.ofSeq u.Types
            usedntrinsics = PSet.ofSeq u.Intrinsics
        }

        



