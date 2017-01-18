namespace FShade.Compiler


type CPointerModifier =
    | None  = 0x00
    | Const = 0x01

type SystemType =
    | TSystem of System.Type
    | TCustom of name : string * fields : list<SystemType * string>

type CTypeRef =
    | CBool
    | CVoid
    | CInt of signed : bool * width : int
    | CFloat of width : int

    | CVector of elementType : CTypeRef * dim : int
    | CMatrix of elementType : CTypeRef * rows : int * cols : int

    | CArray of elementType : CTypeRef * length : int
    | CPointer of modifier : CPointerModifier * elementType : CTypeRef
    | CCustomType of name : string * t : SystemType

type CParameterModifier =
    | In
    | ByRef

type CParameter =
    {
        name        : string
        ctype       : CTypeRef
        modifier    : CParameterModifier
    }

type CFunctionRef = 
    { 
        name        : string 
        returnType  : CTypeRef
        parameters  : CParameter[]
    }

type CIntrinsicFunction =
    {
        name        : string
        tag         : obj
        arguments   : Option<list<int>>
    }

type CVar = 
    { 
        name : string
        ctype : CTypeRef 
    }

type CGlobalAnnotation =
    | CConst
    | CUniform
    | CLocation of int
    | CRowMajor of bool
    | CIn
    | COut

type CGlobal =
    {
        globalName          : string
        globalType          : CTypeRef
        globalAnnotations   : list<CGlobalAnnotation>
    }

type CPrimitiveValue =
    | Null
    | CBool of bool
    | CIntegral of int64
    | CFractional of float
    | CString of string

type CExpr =
    | CVar of CVar
    | CGlobal of CGlobal
    | CValue of CTypeRef * CPrimitiveValue
    | CCall of func : CFunctionRef * args : CExpr[]
    | CCallInrinsic of func : CIntrinsicFunction * args : CExpr[]
    | CConditional of cond : CExpr * ifTrue : CExpr * ifFalse : CExpr

    | CNeg of CExpr
    | CNot of CExpr

    | CAdd of CExpr * CExpr
    | CSub of CExpr * CExpr
    | CMul of CExpr * CExpr
    | CDiv of CExpr * CExpr
    | CMod of CExpr * CExpr

    | CConvert of CExpr * CTypeRef

    | CAnd of CExpr * CExpr
    | COr of CExpr * CExpr
    | CBitAnd of CExpr * CExpr
    | CBitOr of CExpr * CExpr
    | CBitXor of CExpr * CExpr

    | CLess of CExpr * CExpr
    | CLequal of CExpr * CExpr
    | CGreater of CExpr * CExpr
    | CGequal of CExpr * CExpr
    | CEqual of CExpr * CExpr
    | CNotEqual of CExpr * CExpr

    | CNewVector of t : CTypeRef * d : int * components : list<CExpr>
    | CNewMatrix of t : CTypeRef * rows : int * cols : int * components : list<CExpr>

    | CAddressOf of target : CExpr
    | CField of target : CExpr * fieldName : string
    | CItem of target : CExpr * index : CExpr 


type CDefExpr =
    | CDefExprEmpty
    | CDefExpr of CExpr
    | CDefExprArray of elementType : CTypeRef * values : list<CExpr>


type CLExpr =
    | CVar of CVar
    | CGlobal of CGlobal
    | CField of CLExpr * string
    | CItem of CLExpr * CExpr
    | CPtr of CExpr

type CStatement =
    | CNop
    | CDo of CExpr
    | CDeclare of var : CVar * value : CDefExpr
    | CWrite of var : CLExpr * value : CExpr

    | CPostIncrement of v : CLExpr
    | CPostDecrement of v : CLExpr
    | CPreIncrement of v : CLExpr
    | CPreDecrement of v : CLExpr

    | CSequential of body : list<CStatement>

    | CReturn
    | CReturnValue of CExpr
    | CBreak
    | CContinue
    | CFor of init : CStatement * cond : CExpr * step : CStatement * body : CStatement
    | CWhile of guard : CExpr * body : CStatement
    | CDoWhile of guard : CExpr * body : CStatement
    | CIfThenElse of cond : CExpr * ifTrue : CStatement * ifFalse : CStatement
    | CSwitch of value : CExpr * cases : array<CPrimitiveValue * CStatement>

type CDef =
    | CGlobal of variable : CGlobal * initializer : CDefExpr
    | CFunction of signature : CFunctionRef * body : CStatement
    | CStruct of name : string * fields : list<CTypeRef * string>
    | CShader of name : string * inputs : list<CGlobal> * outputs : list<CGlobal> * body : CStatement
    | CNoDef

type CModule = list<CDef>


open System
open System.Reflection
open Aardvark.Base
open Aardvark.Base.Monads.State
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open FShade.Utils


[<AutoOpen>]
module QuotationExtensions =

    module Peano = 
        let private peanoTypes =
            let s = typedefof<S<_>>
            Seq.initInfinite id 
                |> Seq.scan (fun last _ -> s.MakeGenericType [|last|]) typeof<Z>
                |> Seq.cache

        let getPeanoType (i : int) =
            Seq.item i peanoTypes

        let getArrayType (i : int) (content : Type) =
            typedefof<Arr<_,_>>.MakeGenericType [| getPeanoType i; content |]
           
    type Expr with
        static member NewFixedArray(t : Type, values : list<Expr>) =
            let len = values |> List.length
            let arrType = Peano.getArrayType len t
            let seqType = typedefof<seq<_>>.MakeGenericType t
            let ctor = arrType.GetConstructor [| seqType |]
            Expr.NewObject(ctor, [Expr.Coerce(Expr.NewArray(t, values), seqType)])

    let (|NewFixedArray|_|) (e : Expr) =
        match e with
            | NewObject(ctor, [Coerce(NewArray(et, args),_)]) ->
                if ctor.DeclaringType.IsGenericType && ctor.DeclaringType.GetGenericTypeDefinition() = typedefof<Arr<_,_>> then
                    let len = Peano.getSize (ctor.DeclaringType.GetGenericArguments().[0])
                    Some(len, et, args)
                else 
                    None
            | _ ->
                None

[<AutoOpen>]
module private Helpers = 
    open System.Text.RegularExpressions

    let rx = Regex @"(?<name>.*)`[0-9]+"


    let private typeNameCache = System.Collections.Concurrent.ConcurrentDictionary<Type, string>()
    let private methodNameCache = System.Collections.Concurrent.ConcurrentDictionary<MethodInfo, string>()

    let private builtIn =
        [
            typeof<unit>, "bool"
            typeof<System.Void>, "void"
            typeof<bool>, "bool"

            typeof<int8>, "int8"
            typeof<int16>, "int16"
            typeof<int32>, "int32"
            typeof<int64>, "int64"
            typeof<uint8>, "uint8"
            typeof<uint16>, "uint16"
            typeof<uint32>, "uint32"
            typeof<uint64>, "uint64"
            typeof<float16>, "float16"
            typeof<float32>, "float32"
            typeof<float>, "double"

        ]

    do for (t,n) in builtIn do
        typeNameCache.[t] <- n


    let rec typeName (t : Type) =
        typeNameCache.GetOrAdd(t, fun t ->
            if t.IsArray then
                "arr" + typeName(t.GetElementType())

            elif t.IsByRef then
                typeName(t.GetElementType())

            elif FSharpTypeExt.IsTuple t then
                let elements = FSharpTypeExt.GetTupleElements t |> Seq.map typeName |> String.concat "_"
                "tup_" + elements

            else
                let selfName = 
                    if t.IsGenericType then
                        let m = rx.Match t.Name
                        let targs = t.GetGenericArguments()
                        let targstr = targs |> Seq.map typeName |> String.concat "_"
                        m.Groups.["name"].Value + string targs.Length + "_" + targstr
                    else
                        t.Name

                if t.IsNested then 
                    (typeName t.DeclaringType) + "_" + selfName
                else 
                    if isNull t.Namespace then selfName
                    else t.Namespace.Replace('.', '_') + "_" + selfName
        )

    let rec methodName (mi : MethodInfo) =
        methodNameCache.GetOrAdd(mi, fun mi -> 
            let selfName =
                if mi.IsGenericMethod then
                    let m = rx.Match mi.Name
                    let targs = mi.GetGenericArguments() |> Seq.map typeName |> String.concat "_"
                    m.Groups.["name"].Value + "_" + targs
                else
                    mi.Name
            (typeName mi.DeclaringType) + "_" + selfName
        )

    let (|ArrOf|_|) (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Arr<_,_>> then
            let targs = t.GetGenericArguments()
            let len = targs.[0] |> getSize
            let content = targs.[1]
            Some(len, content)
        else
            None


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CType =
    open Aardvark.Base.TypeInfo
    open Aardvark.Base.TypeInfo.Patterns

    let primitiveTypes =
        Dictionary.ofList [
            typeof<unit>, CTypeRef.CVoid
            typeof<System.Void>, CTypeRef.CVoid
            typeof<bool>, CTypeRef.CBool

            typeof<int8>, CTypeRef.CInt(true, 8)
            typeof<int16>, CTypeRef.CInt(true, 16)
            typeof<int32>, CTypeRef.CInt(true, 32)
            typeof<int64>, CTypeRef.CInt(true, 64)
            typeof<uint8>, CTypeRef.CInt(false, 8)
            typeof<uint16>, CTypeRef.CInt(false, 16)
            typeof<uint32>, CTypeRef.CInt(false, 32)
            typeof<uint64>, CTypeRef.CInt(false, 64)
            
            typeof<float16>, CTypeRef.CFloat(16)
            typeof<float32>, CTypeRef.CFloat(32)
            typeof<float>, CTypeRef.CFloat(64)

        ]

    let private typeCache =
        let dict = System.Collections.Concurrent.ConcurrentDictionary<Type, CTypeRef>()
        for (k,v) in Dictionary.toSeq primitiveTypes do
            dict.[k] <- v
        dict

    let private extTypeCache = System.Collections.Concurrent.ConcurrentDictionary<Type, SystemType>()
    let rec toSystemType (t : Type) =
        extTypeCache.GetOrAdd(t, fun t ->
            let name = typeName t
            if FSharpTypeExt.IsRecord t then
                let fields = FSharpTypeExt.GetRecordFields(t) |> Array.toList |> List.map (fun pi -> toSystemType pi.PropertyType, pi.Name) 
                TCustom(name, fields)
            
            elif FSharpTypeExt.IsTuple t then
                let fields = FSharpTypeExt.GetTupleElements(t) |> Array.toList |> List.mapi (fun i t -> toSystemType t, sprintf "Item%d" i)
                TCustom(name, fields)

            elif FSharpTypeExt.IsUnion t then
                let caseFields = 
                    FSharpTypeExt.GetUnionCases(t) |> Array.toList |> List.collect (fun ci ->
                        ci.GetFields() |> Array.toList |> List.map (fun fi ->
                            let name = ci.Name + "_" + fi.Name
                            toSystemType fi.PropertyType, name
                        )
                    )

                let tagField = (toSystemType typeof<int>, "tag")
                TCustom(name, tagField :: caseFields)
            
            else
                TSystem t
        )
       

    let rec ofType (t : Type) : CTypeRef =
        typeCache.GetOrAdd(t, fun t ->
            match t with
                | VectorOf(d, t)    -> CVector(ofType t, d)
                | MatrixOf(s, t)    -> CMatrix(ofType t, s.Y, s.X)
                | ArrOf(len, t)     -> CArray(ofType t, len)
                | Ref t             -> ofType t
                | t when t.IsArray  -> CTypeRef.CPointer(CPointerModifier.None, ofType (t.GetElementType()))
                | t                 -> CCustomType(typeName t, toSystemType t)
        )


type ExternalFunction =
    | ExtCtor of CFunctionRef * ConstructorInfo
    | ExtMethod of CFunctionRef * MethodInfo
    | ExtCustom of CFunctionRef * string * list<Var> * Expr
    | ExtCompiled of CFunctionRef * CStatement
    | ExtAll of list<ExternalFunction>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExternalFunction =
    let custom (name : string) (args : list<Var>) (body : Expr) =
        let signature =
            {
                name = name
                returnType = CType.ofType body.Type
                parameters = args |> List.map (fun v -> { name = v.Name; ctype = CType.ofType v.Type; modifier = CParameterModifier.In }) |> List.toArray
            }
        ExternalFunction.ExtCustom(signature, name, args, body)

type ExternalGlobal =
    | ExtDeclare of CGlobal
    | ExtInit of CGlobal * Expr

[<AllowNullLiteral; AbstractClass>]
type Backend() =
    let functionCache = System.Collections.Concurrent.ConcurrentDictionary<MethodInfo, Option<CIntrinsicFunction>>()
    let typeCache = System.Collections.Concurrent.ConcurrentDictionary<Type, Option<CTypeRef>>()

    abstract member CompileIntrinsicFunction : MethodInfo -> Option<CIntrinsicFunction>
    abstract member CompileIntrinsicType : Type -> Option<CTypeRef>
    abstract member IsIntrinsicValue : CGlobal -> bool

    member x.TryGetIntrinsicFunction (mi : MethodInfo) =
        functionCache.GetOrAdd(mi, fun mi ->
            x.CompileIntrinsicFunction mi
        )

    member x.TryGetIntrinsicType (t : Type) =
        typeCache.GetOrAdd(t, fun t ->
            x.CompileIntrinsicType t
        )

type FunctionCompilerState =
    {
        fBackend             : Backend
        fHelperIndex         : int
        fNames               : Map<Var, string>
        fCurrentSuffixes     : Map<string, int>
        fCustomFunctions     : HashMap<obj, ExternalFunction>
        fUsedTypes           : pset<SystemType>
        fUsedGlobals         : HashMap<obj, ExternalGlobal>
    }

    member x.TryGetIntrinsicFunction (mi : MethodInfo) =
        if isNull x.fBackend then 
            None
        else
            x.fBackend.TryGetIntrinsicFunction mi



type CompilerState =
    {
        backend             : Backend
        helperIndex         : int
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CompilerState =

    let empty =
        {
            backend             = null
            helperIndex         = 0
        }

    let cName (v : Var) =
        state {
            let! s = State.get
            match Map.tryFind v s.fNames with
                | Some n -> 
                    return n

                | None ->
                    match Map.tryFind v.Name s.fCurrentSuffixes with
                        | Some i -> 
                            let name = v.Name + (string i)
                            do! State.put { s with fCurrentSuffixes = Map.add v.Name (i + 1) s.fCurrentSuffixes; fNames = Map.add v name s.fNames }
                            return v.Name + (string i)

                        | None ->
                            do! State.put { s with fCurrentSuffixes = Map.add v.Name 1 s.fCurrentSuffixes; fNames = Map.add v v.Name s.fNames }
                            return v.Name
        }


    let useExtMethod (f : CFunctionRef) (mi : MethodBase) =
        match mi with
            | :? MethodInfo as mi ->
                State.modify(fun s -> { s with fCustomFunctions = HashMap.add (mi :> obj) (ExtMethod(f, mi)) s.fCustomFunctions })
            | :? ConstructorInfo as ci ->
                State.modify(fun s -> { s with fCustomFunctions = HashMap.add (mi :> obj) (ExtCtor(f, ci)) s.fCustomFunctions })
            | _ ->
                failwithf "cannot use method %A" mi

    let asFunction (e : Expr) =
        state {
            let vars = e.GetFreeVars() |> Seq.toList
            let! s = State.get
            
            let name = "helper" + string s.fHelperIndex
            let parameters = vars |> List.map (fun v -> { name = v.Name; ctype = CType.ofType v.Type; modifier = CParameterModifier.ByRef }) |> List.toArray
            let signature = { name = name; parameters = parameters; returnType = CType.ofType e.Type }

            do! State.put { s with fHelperIndex = s.fHelperIndex + 1; fCustomFunctions = HashMap.add (signature :> obj) (ExtCustom(signature, name, vars, e)) s.fCustomFunctions }

            let args = 
                vars 
                    |> List.map (fun v -> CExpr.CVar { name = v.Name; ctype = CType.ofType v.Type })
                    |> List.toArray

            return CExpr.CCall(signature, args)
        }


    let private ctorCache = System.Collections.Concurrent.ConcurrentDictionary<Type, CFunctionRef * CStatement>() 
    let private unionCache = System.Collections.Concurrent.ConcurrentDictionary<Type, System.Collections.Generic.Dictionary<UnionCaseInfo, CFunctionRef * CStatement>>()
       
    let useType (st : SystemType) =
        state {
            let! s = State.get
            do! State.put { s with fUsedTypes = PSet.add st s.fUsedTypes }
        }

    let getAndUseType (t : Type) =
        state {
            let st = CType.toSystemType t
            let! s = State.get
            do! State.put { s with fUsedTypes = PSet.add st s.fUsedTypes }
            return CType.ofType t
        }

    let useGlobal (g : CGlobal) (value : Option<Expr>) =
        state {
            let! s = State.get
            match value with
                | Some v ->
                    do! State.put { s with fUsedGlobals = HashMap.add (g :> obj) (ExtInit(g, v)) s.fUsedGlobals }
                | None ->
                    do! State.put { s with fUsedGlobals = HashMap.add (g :> obj) (ExtDeclare(g)) s.fUsedGlobals }
        }


    let rec tryDeconstructValue (t : Type) (v : obj) =

        if FSharpTypeExt.IsRecord t then
            let fields = FSharpTypeExt.GetRecordFields t |> Array.toList
            let values = fields |> List.map (fun f -> Expr.Value(f.GetValue(v), f.PropertyType))
            Some (Expr.NewRecord(t, values))

        elif FSharpTypeExt.IsUnion t then
            let case, values = FSharpValue.GetUnionFields(v, t, true)
            let values = Array.toList values
            let fields = case.GetFields() |> Array.toList
            let args = List.map2 (fun (fi : PropertyInfo) (v : obj) -> Expr.Value(v, fi.PropertyType)) fields values
            Some (Expr.NewUnionCase(case, args))

        elif FSharpTypeExt.IsTuple t then
            let elements = FSharpTypeExt.GetTupleElements t |> Array.toList
            let values = elements |> List.mapi (fun i t -> Expr.Value(FSharpValue.GetTupleField(v, i), t))
            Some (Expr.NewTuple(values))


        else
            match v with
                | :? Array as arr -> 
                    let element = t.GetElementType()
                    let values = List.init arr.Length (fun i -> Expr.Value(arr.GetValue(i), element))
                    Some(Expr.NewFixedArray(element, values))

                | :? System.Collections.IEnumerable as e ->
                    let element = t.GetInterface(typedefof<seq<_>>.Name).GetGenericArguments().[0]

                    let all = System.Collections.Generic.List<Expr>()
                    let ee = e.GetEnumerator()
                    while ee.MoveNext() do
                        all.Add (Expr.Value(ee.Current, element))
                            
                    Some(Expr.NewFixedArray(element, CSharpList.toList all))


                | _ -> 
                    None

    let asConstant (t : Type) (o : obj) =
        state {
            let! s = State.get
            let name = "helper" + string s.fHelperIndex
            match tryDeconstructValue t o with
                | Some initializer ->
                    let! ct = 
                        match initializer with
                            | NewArray(t,values) ->
                                getAndUseType (Peano.getArrayType values.Length t)
                            | NewFixedArray(l,t,values) ->
                                getAndUseType initializer.Type
                            | _ ->
                                getAndUseType initializer.Type

                    let g = { globalName = name; globalType = ct; globalAnnotations = [CConst] }

                    do! State.put { s with fHelperIndex = s.fHelperIndex + 1; fUsedGlobals = HashMap.add o (ExtInit(g, initializer)) s.fUsedGlobals }

                    return CExpr.CGlobal(g) |> Some
                | _ ->
                    return None

        }


    let tupleConstructor(t : Type) =
        let signature, def = 
            ctorCache.GetOrAdd(t, fun t ->
                let tupleType = CType.ofType t
                let name = typeName t
                let args = FSharpTypeExt.GetTupleElements t |> Array.map CType.ofType |> Array.mapi (fun i ct -> { name = sprintf "item%d" i; ctype = ct; modifier = CParameterModifier.In })
            
                let signature =
                    {
                        name = "new_" + name
                        parameters = args
                        returnType = tupleType
                    }

                let definition =
                    let res = { name = "res"; ctype = tupleType }
                    let writeArgs = 
                        List.init args.Length (fun i -> 
                            let v = { name = sprintf "item%d" i; ctype = args.[i].ctype }
                            CWrite(CLExpr.CField(CLExpr.CVar res, sprintf "Item%d" i), CExpr.CVar v)
                        )
                    CSequential [
                        yield CDeclare(res, CDefExprEmpty)
                        yield! writeArgs
                        yield CReturnValue (CExpr.CVar res)
                    ]

                signature, definition
            )

        State.custom (fun s ->
            let s = { s with fCustomFunctions = HashMap.add ((t, "ctor") :> obj) (ExtCompiled(signature, def)) s.fCustomFunctions }
            s, signature
        )

    let recordConstructor(t : Type) =
        let signature, def =
            ctorCache.GetOrAdd(t, fun t ->
                let recordType = CType.ofType t
                let name = typeName t
                let args = FSharpTypeExt.GetRecordFields t |> Array.map (fun pi -> { name = pi.Name; ctype = CType.ofType pi.PropertyType; modifier = CParameterModifier.In })
            
                let signature =
                    {
                        name = "new_" + name
                        parameters = args
                        returnType = recordType
                    }

                let definition =
                    let res = { name = "res"; ctype = recordType }
                    let writeArgs = 
                        args |> Array.map (fun a ->
                            let v = { name = a.name; ctype = a.ctype }
                            CWrite(CLExpr.CField(CLExpr.CVar res, a.name), CExpr.CVar v)
                        )
                    CSequential [
                        yield CDeclare(res, CDefExprEmpty)
                        yield! writeArgs
                        yield CReturnValue (CExpr.CVar res)
                    ]

                signature, definition
            )

        State.custom (fun s ->
            let s = { s with fCustomFunctions = HashMap.add ((t, "ctor") :> obj) (ExtCompiled(signature, def)) s.fCustomFunctions }
            s, signature
        )

    let unionConstructor (ci : UnionCaseInfo) =
        let t = ci.DeclaringType
        let dict = 
            unionCache.GetOrAdd(t, fun t ->
                let cases = FSharpTypeExt.GetUnionCases t

                let ctors = 
                    cases |> Seq.map (fun ci ->
                        let unionType = CType.ofType t
                        let name = typeName t
                        let args = ci.GetFields() |> Array.map (fun pi -> { name = pi.Name; ctype = CType.ofType pi.PropertyType; modifier = CParameterModifier.In })

                        let signature =
                            {
                                name = "new_" + name + "_" + ci.Name
                                parameters = args
                                returnType = unionType
                            }

                        let definition =
                            let res = { name = "res"; ctype = unionType }
                            let writeArgs = 
                                args |> Array.map (fun a ->
                                    let v = { name = a.name; ctype = a.ctype }
                                    CWrite(CLExpr.CField(CLExpr.CVar res, ci.Name + "_" + a.name), CExpr.CVar v)
                                )
                            CSequential [
                                yield CDeclare(res, CDefExprEmpty)
                                yield CWrite(CLExpr.CField(CLExpr.CVar res, "tag"), CExpr.CValue(CTypeRef.CInt(true, 32), CPrimitiveValue.CIntegral(int64 ci.Tag)))
                                yield! writeArgs
                                yield CReturnValue (CExpr.CVar res)
                            ]

                        ci, (signature, definition)
                    )   

                Dictionary.ofSeq ctors
            )

        let signature, def = dict.[ci]
        State.custom (fun s ->
            let s = { s with fCustomFunctions = HashMap.add ((ci, "ctor") :> obj) (ExtCompiled(signature, def)) s.fCustomFunctions }
            s, signature
        )

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CVar =
    let ofVar (v : Var) : State<FunctionCompilerState, CVar> =
        state {
            let! ctype = CompilerState.getAndUseType v.Type
            let! name = CompilerState.cName v
            return { name = name; ctype = ctype }
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CLExpr =


    let rec ofCExpr (e : CExpr) : CLExpr =
        match e with
            | CExpr.CGlobal g -> 
                CLExpr.CGlobal g
            | CExpr.CVar(v) -> 
                CLExpr.CVar v
            | CExpr.CField(e, field) ->
                let e = ofCExpr e
                CLExpr.CField(e, field)
            | CExpr.CItem(e, index) ->
                let e = ofCExpr e
                CLExpr.CItem(e, index)
            | CExpr.CAddressOf(e) ->
                CLExpr.CPtr e
            | _ ->
                failwithf "not a l-expr: %A" e

    let rec toCExpr (e : CLExpr) =
        match e with
            | CLExpr.CGlobal g -> CExpr.CGlobal g
            | CLExpr.CVar v -> CExpr.CVar v
            | CLExpr.CField(e,f) -> CExpr.CField(toCExpr e, f)
            | CLExpr.CItem(e,f) -> CExpr.CItem(toCExpr e, f)
            | CLExpr.CPtr(e) -> CExpr.CAddressOf(e)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CExpr =
    open Aardvark.Base.TypeInfo
    open Aardvark.Base.TypeInfo.Patterns

    let private vecFields =
        let names =
            Dictionary.ofList [
                typeof<V2i>, ["X"; "Y"]
                typeof<V3i>, ["X"; "Y"; "Z"]
                typeof<V4i>, ["X"; "Y"; "Z"; "W"]
                typeof<V2f>, ["X"; "Y"]
                typeof<V3f>, ["X"; "Y"; "Z"]
                typeof<V4f>, ["X"; "Y"; "Z"; "W"]
                typeof<V2d>, ["X"; "Y"]
                typeof<V3d>, ["X"; "Y"; "Z"]
                typeof<V4d>, ["X"; "Y"; "Z"; "W"]
                typeof<V2l>, ["X"; "Y"]
                typeof<V3l>, ["X"; "Y"; "Z"]
                typeof<V4l>, ["X"; "Y"; "Z"; "W"]

                
                typeof<C3b>, ["R"; "G"; "B"]
                typeof<C4b>, ["R"; "G"; "B"; "A"]
                typeof<C3us>, ["R"; "G"; "B"]
                typeof<C4us>, ["R"; "G"; "B"; "A"]
                typeof<C3ui>, ["R"; "G"; "B"]
                typeof<C4ui>, ["R"; "G"; "B"; "A"]
                typeof<C3f>, ["R"; "G"; "B"]
                typeof<C4f>, ["R"; "G"; "B"; "A"]
                typeof<C3d>, ["R"; "G"; "B"]
                typeof<C4d>, ["R"; "G"; "B"; "A"]
            ]

        names  |> Dictionary.map (fun t n ->
            n |> List.map (fun n -> t.GetField(n)) |> List.toArray
        )

    let private conversionMethods =
        HashSet.ofList [
            getMethodInfo <@ int8 @> 
            getMethodInfo <@ uint8 @> 
            getMethodInfo <@ int16 @> 
            getMethodInfo <@ uint16 @>  
            getMethodInfo <@ int @> 
            getMethodInfo <@ int32 @> 
            getMethodInfo <@ uint32 @> 
            getMethodInfo <@ int64 @> 
            getMethodInfo <@ uint64 @> 
            getMethodInfo <@ nativeint @> 
            getMethodInfo <@ unativeint @> 
            getMethodInfo <@ float @> 
            getMethodInfo <@ float32 @> 
        ]

    let private (|ConversionMethod|_|) (mi : MethodInfo) =
        let meth = 
            if mi.IsGenericMethod then mi.GetGenericMethodDefinition()
            else mi
        if conversionMethods.Contains meth then
            let input = mi.GetParameters().[0]
            let output = mi.ReturnType
            Some(input.ParameterType, output)
        else
            None

    let rec ofExpr (e : Expr) =
        state {
            do! CompilerState.useType (CType.toSystemType e.Type)
            let ct = CType.ofType e.Type

            match e with
                | Var v -> 
                    let! v = CVar.ofVar v
                    return CExpr.CVar v

                | Value(v, t) ->
                    let primitiveValue = 
                        match v with
                            | null -> CPrimitiveValue.Null |> Some
                            | :? bool as v -> CPrimitiveValue.CBool(v) |> Some
                            | :? int8 as v -> CPrimitiveValue.CIntegral(int64 v) |> Some
                            | :? int16 as v -> CPrimitiveValue.CIntegral(int64 v) |> Some
                            | :? int32 as v -> CPrimitiveValue.CIntegral(int64 v) |> Some
                            | :? int64 as v -> CPrimitiveValue.CIntegral(v) |> Some
                            | :? uint8 as v -> CPrimitiveValue.CIntegral(int64 v) |> Some
                            | :? uint16 as v -> CPrimitiveValue.CIntegral(int64 v) |> Some
                            | :? uint32 as v -> CPrimitiveValue.CIntegral(int64 v) |> Some
                            | :? uint64 as v -> CPrimitiveValue.CIntegral(int64 v) |> Some

                            | :? float16 as v -> CPrimitiveValue.CFractional(float v.Float32) |> Some
                            | :? float32 as v -> CPrimitiveValue.CFractional(float v) |> Some
                            | :? float as v -> CPrimitiveValue.CFractional(v) |> Some
                            | :? string as v -> CPrimitiveValue.CString(v) |> Some

                            | _ -> None

                    match primitiveValue with
                        | Some v -> 
                            return CExpr.CValue(ct, v)
                        | None ->
                            // TODO: intrinsic ctors (mat, etc.)
                            match vecFields.TryGetValue t with
                                | (true, fields) -> 
                                    let! values = fields |> Array.toList |> List.mapS (fun f -> Expr.Value(f.GetValue(v), f.FieldType) |> ofExpr)
                                    return CNewVector(ct, fields.Length, values)

                                | _ ->
                                    
                                    let! c = CompilerState.asConstant t v
                                    match c with
                                        | Some c -> 
                                            return c
                                        | None ->
                                            return failwithf "cannot compile value %A (of type %A)" v t

                | Call(None, mi, args) ->
                    let! args = args |> List.mapS ofExpr

                    

                    match mi, args with
                        | Method("op_UnaryNegation", _), [l]        -> return CExpr.CNeg(l)
                        | MethodQuote <@ not @> _, [l]              -> return CExpr.CNot(l)

                        | Method("op_Addition", _), [l;r]           -> return CExpr.CAdd(l, r)
                        | Method("op_Subtraction", _), [l;r]        -> return CExpr.CSub(l, r)
                        | Method("op_Multiply", _), [l;r]           -> return CExpr.CMul(l, r)
                        | Method("op_Division", _), [l;r]           -> return CExpr.CDiv(l, r)
                        | Method("op_Modulus", _), [l;r]            -> return CExpr.CMod(l, r)
                            
                        | Method("op_BooleanAnd", _), [l;r]         -> return CExpr.CAnd(l, r)
                        | Method("op_BooleanOr", _), [l;r]          -> return CExpr.COr(l, r)

                        | Method("op_BitwiseAnd", _), [l;r]         -> return CExpr.CBitAnd(l, r)
                        | Method("op_BitwiseOr", _), [l;r]          -> return CExpr.CBitOr(l, r)
                        | Method("op_ExclusiveOr", _), [l;r]        -> return CExpr.CBitXor(l, r)

                        | Method("op_LessThan", _), [l;r]           -> return CExpr.CLess(l, r)
                        | Method("op_LessThanOrEqual", _), [l;r]    -> return CExpr.CLequal(l, r)
                        | Method("op_GreaterThan", _), [l;r]        -> return CExpr.CGreater(l, r)
                        | Method("op_GreaterThanOrEqual", _), [l;r] -> return CExpr.CGequal(l, r)
                        | Method("op_Equality", _), [l;r]           -> return CExpr.CEqual(l, r)
                        | Method("op_Inequality", _), [l;r]         -> return CExpr.CNotEqual(l, r)

                        | Method("GetArray", _), [arr; index]       -> return CExpr.CItem(arr, index)

                        | ConversionMethod(i,o), [arg] ->
                            return CExpr.CConvert(arg, CType.ofType o)


                        | _ -> 
                            let! s = State.get
                            let intrinsic = s.TryGetIntrinsicFunction mi //if isNull s.fBackend then None else s.fBackend.CompileIntrinsicFunction mi
                            match intrinsic with
                                | Some fmt -> 
                                    return CExpr.CCallInrinsic(fmt, List.toArray args)
                                | None -> 
                                    let parameters =
                                        mi.GetParameters() |> Array.map (fun p ->
                                            { name = p.Name; ctype = CType.ofType p.ParameterType; modifier = CParameterModifier.In }
                                        )
                                    let funRef = { name = mi.Name; parameters = parameters; returnType = ct }
                                    do! CompilerState.useExtMethod funRef mi
                            
                                    return CExpr.CCall(funRef, List.toArray args)

                | Call(Some target, mi, args) ->
                    let! t = ofExpr target
                    let! args = args |> List.mapS ofExpr
                    let args = t :: args |> List.toArray

                    let! s = State.get
                    let intrinsic = s.TryGetIntrinsicFunction mi //if isNull s.fBackend then None else s.fBackend.CompileIntrinsicFunction mi
                    match intrinsic with
                        | Some fmt -> 
                            return CExpr.CCallInrinsic(fmt, args)

                        | None ->
                            let thisParameter = { name = "self"; ctype = CType.ofType target.Type; modifier = CParameterModifier.ByRef }
                            let parameters =
                                mi.GetParameters() |> Array.map (fun p ->
                                    { name = p.Name; ctype = CType.ofType p.ParameterType; modifier = CParameterModifier.In }
                                )

                            let funRef = { name = mi.Name; parameters = Array.append [||] parameters; returnType = ct }
                            do! CompilerState.useExtMethod funRef mi
                            
                            return CExpr.CCall(funRef, args)

                | AndAlso(c0, c1) ->
                    let! c0 = ofExpr c0
                    let! c1 = ofExpr c1
                    return CAnd(c0, c1)

                | OrElse(c0, c1) ->
                    let! c0 = ofExpr c0
                    let! c1 = ofExpr c1
                    return CAnd(c0, c1)

                | IfThenElse(cond, ifTrue, ifFalse) ->  
                    let! cond = ofExpr cond
                    let! ifTrue = ofExpr ifTrue
                    let! ifFalse = ofExpr ifFalse
                    return CExpr.CConditional(cond, ifTrue, ifFalse)
    
                | FieldGet(None, fi) ->
                    let v = fi.GetValue(null)
                    return! ofExpr (Expr.Value(v, fi.FieldType))

                | PropertyGet(None, pi, []) ->
                    let v = pi.GetValue(null)
                    return! ofExpr (Expr.Value(v, pi.PropertyType))
                    
                | PropertyGet(None, pi, indices) ->
                    return! ofExpr (Expr.Call(pi.GetMethod, indices))
                    



                | FieldGet(Some t, fi) ->
                    let! t = ofExpr t
                    return CExpr.CField(t, fi.Name)

                | PropertyGet(Some target, pi, []) ->
                    if FSharpTypeExt.IsRecord target.Type then 
                        let! t = ofExpr target
                        return CExpr.CField(t, pi.Name)

                    elif FSharpTypeExt.IsUnion target.Type then
                        let! t = ofExpr target
                        let ci = FSharpTypeExt.GetUnionCases target.Type |> Array.find (fun ci -> ci.GetFields() |> Array.exists (fun fi -> fi = pi))
                        return CExpr.CField(t, ci.Name + "_" + pi.Name)

                    elif FSharpTypeExt.IsTuple target.Type then
                        let! t = ofExpr target
                        return CExpr.CField(t, pi.Name)

                    else
                        return! ofExpr (Expr.Call(target, pi.GetMethod, []))

                | PropertyGet(Some target, pi, [index]) when pi.Name = "Item" ->
                    if target.Type.IsArray || (target.Type.IsGenericType && target.Type.GetGenericTypeDefinition() = typedefof<Arr<_,_>>) then
                        let! t = ofExpr target
                        let! index = ofExpr index
                        return CExpr.CItem(t, index)
                    else
                        return! ofExpr (Expr.Call(target, pi.GetMethod, [index]))

                | PropertyGet(Some target, pi, indices) ->
                    return! ofExpr (Expr.Call(target, pi.GetMethod, indices))


                | TupleGet(e, i) ->
                    let! e = ofExpr e
                    let field = "Item" + string i
                    return CExpr.CField(e, field)

                | UnionCaseTest(e, ci) ->
                    let! e = ofExpr e
                    return CExpr.CEqual(CExpr.CField(e, "tag"), CExpr.CValue(CTypeRef.CInt(true, 32), CPrimitiveValue.CIntegral(int64 ci.Tag)))

                | Coerce(e,t) ->
                    return! ofExpr e

                | NewTuple(args) ->
                    let! args = args |> List.mapS ofExpr
                    let! ctor = CompilerState.tupleConstructor e.Type
                    return CExpr.CCall(ctor, List.toArray args)
                 
                | NewRecord(t, args) ->
                    let! args = args |> List.mapS ofExpr
                    let! ctor = CompilerState.recordConstructor t
                    return CExpr.CCall(ctor, List.toArray args)

                | NewUnionCase(ci, args) ->
                    let! args = args |> List.mapS ofExpr
                    let! ctor = CompilerState.unionConstructor ci
                    return CExpr.CCall(ctor, List.toArray args)

                | NewObject(ctor, args) ->
                    let! args = args |> List.mapS ofExpr
                    match ct with
                        | CVector(_,d) -> return CNewVector(ct, d, args)
                        | CMatrix(_,r,c) -> return CNewMatrix(ct, r, c, args)
                        | _ -> 
                            return failwith "not implemented"

                | AddressOf e ->
                    let! e = ofExpr e
                    return CExpr.CAddressOf e

                | Application(lambda, arg) ->
                    match lambda with
                        | Lambda(v, body) ->
                            return! Expr.Let(v, arg, body) |> ofExpr
                        | _ ->
                            return failwith "lambda application not implemented"

                | Lambda(v, body) ->
                    return failwith "lambdas not implemented"
                    
                | DefaultValue _
                | AddressSet _ 
                | FieldSet _
                | ForIntegerRangeLoop _
                | Let _ | LetRecursive _ 
                | NewArray _ 
                | PropertySet _ 
                | Sequential _ 
                | VarSet _ 
                | WhileLoop _ -> 
                    return! CompilerState.asFunction e

                | NewDelegate _ 
                | QuoteRaw _ | QuoteTyped _ 
                | TryWith _
                | TryFinally _ 
                | TypeTest _ ->
                    return failwithf "unsupported expression %A" e
                    

                | e -> 
                    return failwithf "unexpected expression %A" e
        }
        
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CDefExpr = 
    let rec ofExpr (e : Expr) =
        state {
            match e with
                | NewArray(t, elements) ->
                    let! ct = CompilerState.getAndUseType t
                    let! elements = elements |> List.mapS CExpr.ofExpr
                    return CDefExprArray(ct, elements)

                | NewFixedArray(l, t, elements) ->
                    let! ct = CompilerState.getAndUseType t
                    let! elements = elements |> List.mapS CExpr.ofExpr
                    return CDefExprArray(ct, elements)
                    
                | DefaultValue t ->
                    let! ct = CompilerState.getAndUseType t
                    return CDefExprEmpty

                | e -> 
                    let! value = CExpr.ofExpr e
                    return CDefExpr(value)
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CStatement =
    let rec ofExprInternal (last : bool) (e : Expr) =
        state {
            let ret (s : CStatement) =
                if last then CSequential[s; CReturn]
                else s

            match e with
                | VarSet(v, value) ->
                    let! var = CVar.ofVar v
                    let! value = CExpr.ofExpr value

                    return CWrite(CLExpr.CVar var, value) |> ret

                | Value(null, u) when u = typeof<unit> ->
                    if last then return CReturn
                    else return CNop

                | ForIntegerRangeLoop(v, min, max, body) ->
                    let! v = CVar.ofVar v
                    let! min = CExpr.ofExpr min
                    let! max = CExpr.ofExpr max
                    let! body = ofExprInternal false body

                    return CFor(
                        CDeclare(v, CDefExpr min),          // int i = min
                        CLequal(CExpr.CVar v, max),         // i <= max
                        CPostIncrement(CLExpr.CVar v),      // i++
                        body
                    ) |> ret

                | WhileLoop(guard, body) ->
                    let! guard = CExpr.ofExpr guard
                    let! body = ofExprInternal false body
                    return CWhile(guard, body) |> ret
                    
                | IfThenElse(guard, ifTrue, ifFalse) ->
                    let! guard = CExpr.ofExpr guard
                    let! ifTrue = ofExprInternal last ifTrue
                    let! ifFalse = ofExprInternal last ifFalse
                    return CIfThenElse(guard, ifTrue, ifFalse)
                    
                | Sequential(l, r) ->
                    let! l = ofExprInternal false l
                    let! r = ofExprInternal last r
                    return CSequential [l;r]

                | Let(v, e, body) ->
                    let! body = ofExprInternal last body
                    let! v = CVar.ofVar v
                    let! e = CDefExpr.ofExpr e
                    return CSequential [CStatement.CDeclare(v, e); body]


                | LetRecursive(bindings, body) ->
                    return failwith "recursive bindings are not supported"

                | DefaultValue t when last ->
                    let! v = CVar.ofVar (Var("temp", t))
                    return 
                        CSequential [
                            CDeclare(v, CDefExprEmpty)
                            CReturnValue (CExpr.CVar v)
                        ]

                | NewArray(et, args) when last ->
                    let et = CType.ofType et
                    let! args = args |> List.mapS CExpr.ofExpr
                    let args = args |> List.toArray

                    let len = args.Length
                    let ct = CTypeRef.CArray(et, len)
                    let! name = CompilerState.cName (Var("temp", typeof<int>))
                    let v = { name = name; ctype = ct }

                    return 
                        CSequential [
                            yield CDeclare(v, CDefExprEmpty)
                            for i in 0 .. args.Length - 1 do
                                let value = args.[i]
                                let index = CExpr.CValue(CTypeRef.CInt(true, 32), CPrimitiveValue.CIntegral (int64 i))
                                yield 
                                    CStatement.CWrite(
                                        CLExpr.CItem(CLExpr.CVar v, index),
                                        value
                                    )
                            yield CReturnValue(CExpr.CVar v)
                        ]

//                | Let(v, DefaultValue _, body) ->
//                    let! v = CVar.ofVar v
//                    let! body = ofExprInternal last body
//                    return 
//                        CSequential [
//                            CDeclare(v, CDefExprEmpty)
//                            body
//                        ]
//
//                | Let(v, NewArray(et, args), body) ->
//                    let et = CType.ofType et
//                    let! args = args |> List.mapS CExpr.ofExpr
//                    let args = args |> List.toArray
//
//                    let len = args.Length
//                    let ct = CTypeRef.CArray(et, len)
//                    let! name = CompilerState.cName v
//                    let v = { name = name; ctype = ct }
//
//                    return 
//                        CSequential [
//                            yield CDeclare(v, CDefExprEmpty)
//                            for i in 0 .. args.Length - 1 do
//                                let value = args.[i]
//                                let index = CExpr.CValue(CTypeRef.CInt(true, 32), CPrimitiveValue.CIntegral (int64 i))
//                                yield 
//                                    CStatement.CWrite(
//                                        CLExpr.CItem(CLExpr.CVar v, index),
//                                        value
//                                    )
//                            yield CReturnValue(CExpr.CVar v)
//                        ]

                | DefaultValue _ -> return failwith "default value in expr"
                | NewArray _ -> return failwith "array-value in expr"

                | FieldSet(None, fi, _) -> return failwithf "cannot set static field %A" fi
                | PropertySet(None, pi, _, _) -> return failwithf "cannot set static property %A" pi

                | FieldSet(Some t, fi, value) ->
                    let! t = CExpr.ofExpr t |> State.map CLExpr.ofCExpr
                    let! value = CExpr.ofExpr value
                    return CStatement.CWrite(CLExpr.CField(t, fi.Name), value)

                | AddressSet(addr, value) ->
                    let! addr = CExpr.ofExpr addr
                    let! value = CExpr.ofExpr value
                    return CStatement.CWrite(CLExpr.CPtr addr, value)

                | PropertySet(Some t, pi, [], value) ->
                    if FSharpTypeExt.IsRecord pi.DeclaringType then
                        let! t = CExpr.ofExpr t |> State.map CLExpr.ofCExpr
                        let! value = CExpr.ofExpr value
                        return CStatement.CWrite(CLExpr.CField(t, pi.Name), value)
                    else
                        return! ofExprInternal last (Expr.Call(t, pi.SetMethod, [value]))

                    
                | PropertySet(Some t, pi, [index], value) when pi.Name = "Item" ->
                    if t.Type.IsArray || (t.Type.IsGenericType && t.Type.GetGenericTypeDefinition() = typedefof<Arr<_,_>>) then
                        let! t = CExpr.ofExpr t |> State.map CLExpr.ofCExpr
                        let! index = CExpr.ofExpr index
                        let! value = CExpr.ofExpr value
                        return CStatement.CWrite(CLExpr.CItem(t, index), value)
                    else
                        return! ofExprInternal last (Expr.Call(t, pi.SetMethod, [index; value]))

                | PropertySet(Some t,pi, indices, value) ->
                    return! ofExprInternal last (Expr.Call(t, pi.SetMethod, indices @ [value]))
                    
                | PropertySet(None,pi,_,_) -> return failwithf "cannot set static property %A" pi


                | e when last ->
                    
                    let! v = CExpr.ofExpr e
                    return CStatement.CReturnValue v

                | e -> 
                    let! v = CExpr.ofExpr e
                    return CStatement.CDo v
        }

    let ofExpr (e : Expr) =
        ofExprInternal true e

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CModule =

    [<ReferenceEquality; NoComparison>]
    type private DefTree = { def : CDef; needed : list<DefTree>; mutable level : int }

    let private flatten (t : DefTree) =
        let rec flatten (seen : System.Collections.Generic.HashSet<DefTree>) (t : DefTree) =
            if seen.Add t then
                t :: (t.needed |> List.collect (flatten seen))
            else
                []

        let seen = HashSet.empty
        
        flatten seen t 
            |> List.sortByDescending (fun t -> t.level)
            |> List.map  (fun t -> t.def)

    type private GraphCache =
        {
            functionCache   : Dict<ExternalFunction, DefTree>
            typeCache       : Dict<SystemType, DefTree>
            globalCache     : Dict<ExternalGlobal, DefTree>
        }

    let rec private typeGraph (cache : GraphCache, level : int, t : SystemType) =
        state {
            match cache.typeCache.TryGetValue t with
                | (true, t) ->
                    t.level <- max t.level level
                    return Some t
                | _ ->
                    match t with
                        | TCustom(name, fields) ->
                            let! dependentOn = 
                                fields |> List.chooseS (fun (t,_) ->
                                    typeGraph(cache, level + 1, t)
                                )

                            let fields = 
                                fields |> List.map (fun (t,n) ->
                                    match t with
                                        | TSystem t -> CType.ofType t, n
                                        | TCustom(tn,_) -> CTypeRef.CCustomType(tn, t), n
                                )
                            let res = { def = CDef.CStruct(name, fields); needed = dependentOn; level = level }
                    

                            cache.typeCache.[t] <- res
                            return Some res
                        | _ ->
                            return None
        }

    and private functionGraph (cache : GraphCache, level : int, f : ExternalFunction) =
        state {
            match cache.functionCache.TryGetValue f with
                | (true, t) -> 
                    t.level <- max t.level level
                    return t
                | _ -> 

                    match f with    
                        | ExtAll functions ->
                            let! functions = functions |> List.mapS (fun f -> functionGraph(cache, level + 1, f))
                            let tree = { def = CNoDef; needed = functions; level = level }
                            cache.functionCache.[f] <- tree
                            return tree

                        | ExtCompiled(s,d) ->
                            let compile =
                                state {
                                    for p in s.parameters do
                                        match p.ctype with
                                            | CTypeRef.CCustomType(_,st) ->
                                                do! CompilerState.useType st
                                            | _ -> 
                                                ()
                                    match s.returnType with
                                        | CTypeRef.CCustomType(_,st) ->
                                            do! CompilerState.useType st
                                        | _ ->
                                            ()

                                    return CDef.CFunction(s,d)
                                }

                            let! state = State.get
                            let mutable functionState =
                                {
                                    fBackend            = Unchecked.defaultof<_>
                                    fHelperIndex        = 0
                                    fNames              = Map.empty
                                    fCurrentSuffixes    = Map.empty
                                    fCustomFunctions    = HashMap.empty
                                    fUsedTypes          = PSet.empty
                                    fUsedGlobals        = HashMap.empty
                                }

                            let def = compile.Run(&functionState)
                            let! tree = wrapDef(cache, level, functionState, def)
                            cache.functionCache.[f] <- tree
                            return tree

                        | _ ->
                            let signature, name, args, body =
                                match f with
                                    | ExtCompiled _ | ExtAll _ -> failwith "impossible"
                                    | ExtCtor _ -> failwith "not implemented"
                                    | ExtCustom(signature,name, args, body) -> signature, name, args, body
                                    | ExtMethod(signature, mi) ->
                                        let name = methodName mi
                                        match Expr.TryGetReflectedDefinition mi with
                                            | Some def ->
                                                match def with
                                                    | Lambdas(args, body) -> signature, name, List.concat args, body
                                                    | b -> signature, name, [], b

                                            | None -> 
                                                failwithf "no reflected definition for %A" mi

                            let compile =
                                state {
                                    let! parTypes = args |> List.mapS (fun a -> a.Type |> CompilerState.getAndUseType)
                                    let! ret = CompilerState.getAndUseType body.Type
                                    let! body = CStatement.ofExpr body
                                    return ret, parTypes, body
                                }

                            let! state = State.get
                            let mutable functionState =
                                {
                                    fBackend            = state.backend
                                    fHelperIndex        = state.helperIndex
                                    fNames              = args |> List.map (fun a -> a, a.Name) |> Map.ofList
                                    fCurrentSuffixes    = args |> List.map (fun a -> a.Name, 1) |> Map.ofList
                                    fCustomFunctions    = HashMap.empty
                                    fUsedTypes          = PSet.empty
                                    fUsedGlobals        = HashMap.empty
                                }


                            // compile the function
                            let ret, parTypes, body = compile.Run(&functionState)
                            let state = { state with helperIndex = functionState.fHelperIndex }
                            do! State.put state

                            let def = CDef.CFunction(signature, body)
                            let! tree = wrapDef(cache, level, functionState, def)
                            cache.functionCache.[f] <- tree
                            return tree
        }

    and private globalGraph(cache : GraphCache, level : int, g : ExternalGlobal) =
        state {
            match cache.globalCache.TryGetValue g with
                | (true, t) -> 
                    t.level <- max t.level level
                    return t
                | _ ->
                    
                    let compile =
                        state {
                            match g with
                                | ExtDeclare g ->
                                    match g.globalType with
                                        | CCustomType(_,st) -> do! CompilerState.useType st
                                        | _ -> ()

                                    return CDef.CGlobal(g, CDefExprEmpty)

                                | ExtInit(g, e) ->
                                    match g.globalType with
                                        | CCustomType(_,st) -> do! CompilerState.useType st
                                        | _ -> ()

                                    let! def = CDefExpr.ofExpr e
                                    return CDef.CGlobal(g, def)

//                                | ExtDeclareArray(g, arr) ->
//                                    match g.globalType with
//                                        | CCustomType(_,st) -> do! CompilerState.useType st
//                                        | _ -> ()
//
//                                    let et = arr.GetType().GetElementType()
//                                    let valueExpressions = 
//                                        List.init arr.Length (fun i ->
//                                            let v = arr.GetValue(i)
//                                            Expr.Value(v, et)
//                                        )
//
//                                    let! values = valueExpressions |> List.mapS CExpr.ofExpr
//                                    let! ct = CompilerState.getAndUseType et
//                                    return CDef.CGlobal(g, Some (CDefExprArray(ct, values)))
                                    
                                    
                        }

                    let! state = State.get
                    let mutable functionState =
                        {
                            fBackend            = state.backend
                            fHelperIndex        = state.helperIndex
                            fNames              = Map.empty
                            fCurrentSuffixes    = Map.empty
                            fCustomFunctions    = HashMap.empty
                            fUsedTypes          = PSet.empty
                            fUsedGlobals        = HashMap.empty
                        }

                    let def = compile.Run(&functionState)
                    let! tree = wrapDef(cache, level, functionState, def)
                    do! State.put { state with helperIndex = functionState.fHelperIndex }
                    cache.globalCache.[g] <- tree
                    return tree
        }

    and private wrapDef (cache : GraphCache, level : int, functionState : FunctionCompilerState, def : CDef) =
        state {
            let! usedFunctions =
                functionState.fCustomFunctions 
                    |> HashMap.toList
                    |> List.mapS (fun (signature, def) ->
                        functionGraph(cache, level + 1, def)
                    )

            let! usedTypes =
                functionState.fUsedTypes 
                    |> PSet.toList
                    |> List.chooseS (fun st ->
                        typeGraph(cache, level + 1, st)
                    )

            let! usedGlobals =
                functionState.fUsedGlobals
                    |> HashMap.toList
                    |> List.mapS (fun (_,g) ->
                        globalGraph(cache, level + 1, g)
                    )

            let t = { def = def; needed = usedFunctions @ usedTypes @ usedGlobals; level = level }
            return t
        }


    let ofLambda (backend : Backend) (name : string) (e : Expr) : CModule =
        let args, body = 
            match e with
                | Lambdas(args, body) -> List.concat args, body
                | b -> [], b

        let mutable initialState = { CompilerState.empty with backend = backend }
        let cache = { typeCache = Dict(); functionCache = Dict(); globalCache = Dict() }
        let tree = functionGraph(cache, 0, ExternalFunction.custom name args body).Run(&initialState)
        flatten tree

    let ofLambdas (backend : Backend) (e : list<string * Expr>) : CModule =
        let functions =
            e |> List.map (fun (name, e) ->
                let args, body = 
                    match e with
                        | Lambdas(args, body) -> List.concat args, body
                        | b -> [], b

                ExternalFunction.custom name args body
            )

        let mutable initialState = { CompilerState.empty with backend = backend }
        let cache = { typeCache = Dict(); functionCache = Dict(); globalCache = Dict() }
        let tree = functionGraph(cache, 0, ExtAll(functions)).Run(&initialState)
        flatten tree

module GLSL =
    module String =
        let private lineBreak = System.Text.RegularExpressions.Regex @"\r\n"

        let indent (str : string) =
            lineBreak.Split str |> Array.map (fun str -> "    " + str) |> String.concat "\r\n"

    module CType =
        let rec glsl (t : CTypeRef) =
            match t with
                | CTypeRef.CBool -> "bool"
                | CTypeRef.CVoid -> "void"

                | CTypeRef.CInt(true,  (8 | 16 | 32)) -> "int"
                | CTypeRef.CInt(false, (8 | 16 | 32)) -> "uint"
                
                | CTypeRef.CFloat(16 | 32 | 64) -> "float"
                
                | CTypeRef.CVector(CTypeRef.CInt(true, (8 | 16 | 32)), d)       -> sprintf "ivec%d" d
                | CTypeRef.CVector(CTypeRef.CFloat(16 | 32 | 64), d)            -> sprintf "vec%d" d
                | CTypeRef.CMatrix(CTypeRef.CFloat(16 | 32 | 64), r, c)         -> sprintf "mat%dx%d" c r

                | CTypeRef.CArray(t, len) ->
                    let t = glsl t
                    sprintf "%s[%d]" t len

                | CTypeRef.CCustomType(name,_) ->
                    name

                | _ -> 
                    // TODO: byte vectors, etc.
                    failwithf "[GLSL] unknown type %A" t

    module CParameter =
        let rec glsl (p : CParameter) =
            let t = CType.glsl p.ctype
            match p.modifier with
                | CParameterModifier.In ->
                    sprintf "%s %s" t p.name

                | CParameterModifier.ByRef ->
                    sprintf "inout %s %s" t p.name
               

    module CExpr = 
        let rec glsl (e : CExpr) =
            match e with
                | CExpr.CGlobal g -> g.globalName
                | CExpr.CVar v -> v.name

                | CExpr.CValue(_,CPrimitiveValue.CBool(v))          -> if v then "true" else "false"
                | CExpr.CValue(_,CPrimitiveValue.CFractional(v))    -> v.ToString(System.Globalization.CultureInfo.InvariantCulture)
                | CExpr.CValue(_,CPrimitiveValue.CIntegral(v))      -> string v
                | CExpr.CValue(_,CPrimitiveValue.Null)              -> failwith "[GLSL] cannot compile null-literal"
                | CExpr.CValue(_,CPrimitiveValue.CString(v))        -> failwithf "[GLSL] cannot compile string-literal %A" v

                | CExpr.CCall(f, args) ->
                    let args = args |> Array.map glsl |> String.concat ", "
                    sprintf "%s(%s)" f.name args

                | CExpr.CCallInrinsic(f, args) ->
                    let args = 
                        match f.arguments with
                            | Some order -> order |> List.map (fun i -> glsl args.[i]) |> String.concat ", "
                            | _ -> args |> Array.map glsl |> String.concat ", "
                    sprintf "%s(%s)" f.name args
                    

                | CExpr.CConditional(cond, i, e) ->
                    let cond = glsl cond
                    let i = glsl i
                    let e = glsl e
                    sprintf "((%s) ? (%s) : (%s))" cond i e

                | CExpr.CNeg(v) -> sprintf "(-(%s))" (glsl v)
                | CExpr.CNot(v) -> sprintf "(!(%s))" (glsl v)
                | CExpr.CConvert(v, t) -> sprintf "((%s)(%s))" (CType.glsl t) (glsl v)

                | CExpr.CAdd(l, r)      -> sprintf "((%s) + (%s))" (glsl l) (glsl r)
                | CExpr.CSub(l, r)      -> sprintf "((%s) - (%s))" (glsl l) (glsl r)
                | CExpr.CMul(l, r)      -> sprintf "((%s) * (%s))" (glsl l) (glsl r)
                | CExpr.CDiv(l, r)      -> sprintf "((%s) / (%s))" (glsl l) (glsl r)
                | CExpr.CMod(l, r)      -> sprintf "((%s) %% (%s))" (glsl l) (glsl r)

                | CExpr.CAnd(l, r)      -> sprintf "((%s) && (%s))" (glsl l) (glsl r)
                | CExpr.COr(l, r)       -> sprintf "((%s) || (%s))" (glsl l) (glsl r)
                | CExpr.CBitAnd(l, r)   -> sprintf "((%s) & (%s))" (glsl l) (glsl r)
                | CExpr.CBitOr(l, r)    -> sprintf "((%s) | (%s))" (glsl l) (glsl r)
                | CExpr.CBitXor(l, r)   -> sprintf "((%s) ^ (%s))" (glsl l) (glsl r)

                | CExpr.CLess(l, r)     -> sprintf "((%s) < (%s))" (glsl l) (glsl r)
                | CExpr.CLequal(l, r)   -> sprintf "((%s) <= (%s))" (glsl l) (glsl r)
                | CExpr.CGreater(l, r)  -> sprintf "((%s) > (%s))" (glsl l) (glsl r)
                | CExpr.CGequal(l, r)   -> sprintf "((%s) >= (%s))" (glsl l) (glsl r)
                | CExpr.CEqual(l, r)    -> sprintf "((%s) == (%s))" (glsl l) (glsl r)
                | CExpr.CNotEqual(l, r) -> sprintf "((%s) != (%s))" (glsl l) (glsl r)

                | CExpr.CNewVector(t, d, comp) ->
                    let t = CType.glsl t
                    let args = comp |> List.map glsl |> String.concat ", "
                    sprintf "%s(%s)" t args

                | CExpr.CField(e,f) -> glsl e + "." + f
                | CExpr.CItem(e,index) -> glsl e + "[" + glsl index + "]"
                
                | CExpr.CNewMatrix _ 
                | CExpr.CAddressOf _ ->
                    // TODO: implement
                    failwith "not implemented"

    module CLExpr =
        let glsl (e : CLExpr) = e |> CLExpr.toCExpr |> CExpr.glsl

    module CDefExpr =
        let glsl (e : CDefExpr) =
            match e with
                | CDefExprEmpty -> ""
                | CDefExpr e -> CExpr.glsl e
                | CDefExprArray(t, args) ->
                    args |> List.map CExpr.glsl |> String.concat ", " |> sprintf "{ %s }"

    module CStatement =
        let rec glsl (s : CStatement) =
            match s with
                | CStatement.CNop ->
                    ""
                | CStatement.CDo e ->
                    CExpr.glsl e

                | CStatement.CDeclare(v,e) ->
                    let assign =
                        match e with
                            | CDefExprEmpty -> ""
                            | e -> " = " + (CDefExpr.glsl e)

                    match v.ctype with
                        | CArray(t, len) ->
                            sprintf "%s %s[%d]%s" (CType.glsl t) v.name len assign
                        | _ -> 
                            sprintf "%s %s%s" (CType.glsl v.ctype) v.name assign

                | CStatement.CWrite(l, v) ->
                    (CLExpr.glsl l) + " = " + (CExpr.glsl v)
                    
                | CStatement.CPostIncrement(l) -> (CLExpr.glsl l) + "++"
                | CStatement.CPostDecrement(l) -> (CLExpr.glsl l) + "--"
                | CStatement.CPreIncrement(l) -> "++" + (CLExpr.glsl l)
                | CStatement.CPreDecrement(l) -> "--" + (CLExpr.glsl l)

                | CStatement.CSequential l -> 
                    l |> List.map glsl |> String.concat ";\r\n"

                | CStatement.CFor(init, cond, step, body) ->
                    let init = glsl init
                    let cond = CExpr.glsl cond
                    let step = glsl step
                    let body = glsl body
                    sprintf "for(%s;%s;%s)\r\n{\r\n%s;\r\n}" init cond step (String.indent body)
                
                | CStatement.CWhile(guard, body) ->
                    let guard = CExpr.glsl guard
                    let body = glsl body
                    sprintf "while(%s)\r\n{\r\n%s;\r\n}" guard (String.indent body)

                | CStatement.CDoWhile(guard, body) ->
                    let guard = CExpr.glsl guard
                    let body = glsl body
                    sprintf "do\r\n{\r\n%s;\r\n}\r\nwhile(%s)" (String.indent body) guard

                | CStatement.CIfThenElse(cond, i, e) ->
                    let cond = CExpr.glsl cond
                    let i = glsl i
                    let e = glsl e
                    sprintf "if(%s)\r\n{\r\n%s;\r\n}\r\nelse\r\n{\r\n%s;\r\n}" cond (String.indent i) (String.indent e)
                    
                | CStatement.CSwitch(value, cases) ->
                    failwith "switch not implemented"

                | CStatement.CReturnValue e -> 
                    "return " + (CExpr.glsl e)

                | CStatement.CReturn -> "return"
                | CStatement.CBreak -> "break"
                | CStatement.CContinue -> "continue"

               
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CModule =
        let rec glsl (m : CModule) =
            let definitions = 
                m |> List.map (fun d ->
                    match d with
                        | CNoDef ->
                            ""

                        | CDef.CShader(name, inputs, outputs, body) ->
                            let shader = sprintf "void %s()\r\n{\r\n%s\r\n}" name (CStatement.glsl body)

                            let inputs = glsl (inputs |> List.map (fun g -> CDef.CGlobal(g, CDefExprEmpty)))
                            let outputs = glsl (outputs |> List.map (fun g -> CDef.CGlobal(g, CDefExprEmpty)))
                            sprintf "%s\r\n%s\r\n%s" inputs outputs shader

                        | CDef.CFunction(signature, body) ->
                            let ret = CType.glsl signature.returnType
                            let args = signature.parameters |> Array.map CParameter.glsl |> String.concat ", "
                            let body = CStatement.glsl body
                            sprintf "%s %s(%s)\r\n{\r\n%s;\r\n}" ret signature.name args (String.indent body)

                        | CDef.CGlobal(g, initializer) ->
                            
                            let isConstant = g.globalAnnotations |> List.exists (fun a -> a = CConst)

                            // TODO: respect annotations
                            let prefix = 
                                if isConstant then "const "
                                else ""

                            let assign =
                                match initializer with
                                    | CDefExprEmpty -> ""
                                    | e -> CDefExpr.glsl e |> sprintf " = %s"

                            match g.globalType with
                                | CArray(t, len) ->
                                    sprintf "%s%s %s[%d]%s;" prefix (CType.glsl t) g.globalName len assign
                                | _ -> 
                                    sprintf "%s%s %s%s;" prefix (CType.glsl g.globalType) g.globalName assign

                        | CDef.CStruct(name, fields) ->
                            let fields = fields |> List.map (fun (t,n) -> sprintf "    %s %s;" (CType.glsl t) n) |> String.concat "\r\n"
                            sprintf "struct %s {\r\n%s\r\n};" name fields

                )

            definitions |> String.concat "\r\n\r\n"