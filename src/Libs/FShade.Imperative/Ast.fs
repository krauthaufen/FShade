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
            tag = name :> obj
            arguments = None
        }

    let custom (name : string) (args : list<int>)=
        {
            intrinsicName = name
            tag = name :> obj
            arguments = Some args
        }


/// represents a c-style expression
type CExpr =
    | CVar of CVar
    | CValue of CType * CLiteral
    | CCall of func : CFunctionSignature * args : CExpr[]
    | CCallIntrinsic of t : CType * func : CIntrinsic * args : CExpr[]
    | CConditional of ctype : CType * cond : CExpr * ifTrue : CExpr * ifFalse : CExpr

    | CReadInput of ctype : CType * name : string * index : Option<CExpr>

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
            | CReadInput(t,_,_) -> t
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CExpr =

    let tryCreateValue (valueType : Type) (value : obj) =
        let t = CType.ofType valueType
        match CLiteral.tryCreate value with
            | Some literal ->
                CValue(t, literal) |> Some
            | None ->
                None
                
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
            
/// represents a c-style lhs-expression
type CLExpr =
    | CLVar of CVar
    | CLField of CType * CLExpr * string
    | CLItem of CType * CLExpr * CExpr
    | CLPtr of CType * CExpr

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CLExpr =
    let rec ofExpr (e : CExpr) =
        match e with
            | CVar v -> CLVar v
            | CField(t, e, name) -> CLField(t, ofExpr e, name)
            | CItem(t, e, index) -> CLItem(t, ofExpr e, index)
            | CAddressOf(t, e) -> CLPtr(t, e)
            | _ -> failwithf "[FShade] cannot convert CExpr toCLExpr (%A)" e
            
    let rec ofExprSafe (e : CExpr) =
        match e with
            | CVar v ->  CLVar v |> Some
            | CField(t, e, name) -> CLField(t, ofExpr e, name) |> Some
            | CItem(t, e, index) -> CLItem(t, ofExpr e, index) |> Some
            | CAddressOf(t, e) -> CLPtr(t, e) |> Some
            | _ -> None

    let rec toExpr (e : CLExpr) =
        match e with
            | CLVar v -> CVar v
            | CLField(t, e, name) -> CField(t, toExpr e, name)
            | CLItem(t, e, index) -> CItem(t, toExpr e, index)
            | CLPtr(t, e) -> CAddressOf(t, e)

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

    | CWriteOutput of string * Option<CExpr> * CExpr

    | CReturnValue of CExpr
    | CBreak
    | CContinue
    | CFor of init : CStatement * cond : CExpr * step : CStatement * body : CStatement
    | CWhile of guard : CExpr * body : CStatement
    | CDoWhile of guard : CExpr * body : CStatement
    | CIfThenElse of cond : CExpr * ifTrue : CStatement * ifFalse : CStatement
    | CSwitch of value : CExpr * cases : array<CLiteral * CStatement>


type CEntryParameter =
    {
        cParamType           : CType
        cParamName           : string
        cParamSemantic       : string
        cParamDecorations    : Set<ParameterDecoration>
    }


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

type CValueDef =
    | CConstant of ctype : CType * name : string * init : CRExpr
    | CFunctionDef of signature : CFunctionSignature * body : CStatement
    | CEntryDef of CEntryDef
    | CConditionalDef of string * list<CValueDef>
    | CUniformDef of list<CUniform>

type CTypeDef =
    | CStructDef of name : string * fields : list<CType * string>


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

