namespace FShade.Compiler

type CPointerModifier =
    | None  = 0x00
    | Const = 0x01

type CTypeRef =
    | CBool
    | CVoid
    | CInt of signed : bool * width : int
    | CFloat of width : int

    | CVector of elementType : CTypeRef * dim : int
    | CMatrix of elementType : CTypeRef * rows : int * cols : int

    | CArray of elementType : CTypeRef * length : int
    | CPointer of modifier : CPointerModifier * elementType : CTypeRef
    | CCustomType of name : string

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

type CVar = 
    { 
        name : string
        ctype : CTypeRef 
    }


type CPrimitiveValue =
    | Null
    | CBool of bool
    | CIntegral of int64
    | CFractional of float
    | CString of string

type CExpr =
    | CVar of CVar
    | CValue of CTypeRef * CPrimitiveValue
    | CCall of func : CFunctionRef * args : CExpr[]
    | CConditional of cond : CExpr * ifTrue : CExpr * ifFalse : CExpr

    | CNeg of CExpr
    | CNot of CExpr

    | CAdd of CExpr * CExpr
    | CSub of CExpr * CExpr
    | CMul of CExpr * CExpr
    | CDiv of CExpr * CExpr
    | CMod of CExpr * CExpr

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

type CLExpr =
    | CVar of CVar
    | CField of CLExpr * string
    | CItem of CLExpr * CExpr
    | CPtr of CExpr

type CStatement =
    | CNop
    | CDo of CExpr
    | CDeclare of var : CVar * value : Option<CExpr>
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



type CGlobalKind =
    | CUniform
    | CInput
    | COutput
    | CConstant

type CDef =
    | CGlobal of kind : CGlobalKind * variable : CVar * initializer : Option<CExpr>
    | CFunction of signature : CFunctionRef * body : CStatement
    | CStruct of name : string * fields : list<CTypeRef * string>

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

    let rec ofType (t : Type) : CTypeRef =
        typeCache.GetOrAdd(t, fun t ->
            match t with
                | VectorOf(d, t)    -> CVector(ofType t, d)
                | MatrixOf(s, t)    -> CMatrix(ofType t, s.Y, s.X)
                | ArrOf(len, t)     -> CArray(ofType t, len)
                | Ref t             -> ofType t
                | t                 -> CCustomType(typeName t)
        )


type ExternalFunction =
    | ExtCtor of ConstructorInfo
    | ExtMethod of MethodInfo
    | ExtCustom of string * list<Var> * Expr
    | ExtCompiled of CFunctionRef * CStatement

type ExternalType =
    | TSystem of Type
    | TStruct of string * list<string * CTypeRef>

[<AllowNullLiteral>]
type IBackend =
    abstract member CompileIntrinsicFunction : MethodInfo -> Option<CExpr[] -> CExpr>
    abstract member CompileIntrinsicType : Type -> Option<CTypeRef>

type CompilerState =
    {
        backend             : IBackend
        cnames              : Map<Var, string>
        currentSuffixes     : Map<string, int>
        usedTypes           : pset<CTypeRef>


        customTypes         : HashMap<CTypeRef, ExternalType>
        customFunctions     : HashMap<CFunctionRef, ExternalFunction>

        helperIndex         : int

    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CompilerState =

    let empty =
        {
            backend             = null
            cnames              = Map.empty
            currentSuffixes     = Map.empty
            usedTypes           = PSet.empty
            customTypes         = HashMap.empty
            customFunctions     = HashMap.empty
            helperIndex         = 0
        }

    let cName (v : Var) =
        state {
            let! s = State.get
            match Map.tryFind v s.cnames with
                | Some n -> 
                    return n

                | None ->
                    match Map.tryFind v.Name s.currentSuffixes with
                        | Some i -> 
                            let name = v.Name + (string i)
                            do! State.put { s with currentSuffixes = Map.add v.Name (i + 1) s.currentSuffixes; cnames = Map.add v name s.cnames }
                            return v.Name + (string i)

                        | None ->
                            do! State.put { s with currentSuffixes = Map.add v.Name 1 s.currentSuffixes; cnames = Map.add v v.Name s.cnames }
                            return v.Name
        }


    let useExtMethod (f : CFunctionRef) (mi : MethodBase) =
        match mi with
            | :? MethodInfo as mi ->
                State.modify(fun s -> { s with customFunctions = HashMap.add f (ExtMethod mi) s.customFunctions })
            | :? ConstructorInfo as ci ->
                State.modify(fun s -> { s with customFunctions = HashMap.add f (ExtCtor ci) s.customFunctions })
            | _ ->
                failwithf "cannot use method %A" mi

    let asFunction (e : Expr) =
        state {
            let vars = e.GetFreeVars() |> Seq.toList
            let! s = State.get
            
            let name = "helper" + string s.helperIndex
            let parameters = vars |> List.map (fun v -> { name = v.Name; ctype = CType.ofType v.Type; modifier = if v.IsMutable then CParameterModifier.ByRef else CParameterModifier.In }) |> List.toArray
            let signature = { name = name; parameters = parameters; returnType = CType.ofType e.Type }

            do! State.put { s with helperIndex = s.helperIndex + 1; customFunctions = HashMap.add signature (ExtCustom(name, vars, e)) s.customFunctions }

            let args = 
                vars 
                    |> List.map (fun v -> CExpr.CVar { name = v.Name; ctype = CType.ofType v.Type })
                    |> List.toArray

            return CExpr.CCall(signature, args)
        }

    let private extTypeCache = System.Collections.Concurrent.ConcurrentDictionary<Type, ExternalType>()
    let private ctorCache = System.Collections.Concurrent.ConcurrentDictionary<Type, CFunctionRef * CStatement>() 
    let private unionCache = System.Collections.Concurrent.ConcurrentDictionary<Type, System.Collections.Generic.Dictionary<UnionCaseInfo, CFunctionRef * CStatement>>()

    let useExtType (r : CTypeRef) (t : Type) =
        let extType = 
            extTypeCache.GetOrAdd(t, fun t ->
                let name = typeName t
                if FSharpTypeExt.IsRecord t then
                    let fields = FSharpTypeExt.GetRecordFields(t) |> Array.toList |> List.map (fun pi -> pi.Name, CType.ofType pi.PropertyType) 
                    TStruct(name, fields)
            
                elif FSharpTypeExt.IsTuple t then
                    let fields = FSharpTypeExt.GetTupleElements(t) |> Array.toList |> List.mapi (fun i t -> sprintf "Item%d" i, CType.ofType t)
                    TStruct(name, fields)

                elif FSharpTypeExt.IsUnion t then
                    let caseFields = 
                        FSharpTypeExt.GetUnionCases(t) |> Array.toList |> List.collect (fun ci ->
                            ci.GetFields() |> Array.toList |> List.map (fun fi ->
                                let name = ci.Name + "_" + fi.Name
                                name, CType.ofType fi.PropertyType
                            )
                        )

                    let tagField = ("tag", CTypeRef.CInt(true, 32))
                    TStruct(name, tagField :: caseFields)
            
                else
                    TSystem t
            )

        State.modify(fun s -> { s with customTypes = HashMap.add r extType s.customTypes })
              
    let useType (t : Type) =
        state {
            let! s = State.get
            let ct = CType.ofType t
            do! State.put { s with usedTypes = PSet.add ct s.usedTypes }

            match ct with
                | CCustomType name -> 
                    do! useExtType ct t
                    return ct

                | _ -> 
                    return ct
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
                        yield CDeclare(res, None)
                        yield! writeArgs
                        yield CReturnValue (CExpr.CVar res)
                    ]

                signature, definition
            )

        State.custom (fun s ->
            let s = { s with customFunctions = HashMap.add signature (ExtCompiled(signature, def)) s.customFunctions }
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
                        yield CDeclare(res, None)
                        yield! writeArgs
                        yield CReturnValue (CExpr.CVar res)
                    ]

                signature, definition
            )

        State.custom (fun s ->
            let s = { s with customFunctions = HashMap.add signature (ExtCompiled(signature, def)) s.customFunctions }
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
                                yield CDeclare(res, None)
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
            let s = { s with customFunctions = HashMap.add signature (ExtCompiled(signature, def)) s.customFunctions }
            s, signature
        )

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CVar =
    let ofVar (v : Var) : State<CompilerState, CVar> =
        state {
            let ctype = CType.ofType v.Type
            let! name = CompilerState.cName v
            return { name = name; ctype = ctype }
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CLExpr =
    let rec ofExpr (e : Expr) : State<CompilerState, CLExpr> =
        failwith ""

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

    let rec ofExpr (e : Expr) =
        state {
            let! ct = CompilerState.useType e.Type

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
                            match vecFields.TryGetValue t with
                                | (true, fields) -> 
                                    let! values = fields |> Array.toList |> List.mapS (fun f -> Expr.Value(f.GetValue(v), f.FieldType) |> ofExpr)
                                    return CNewVector(ct, fields.Length, values)

                                | _ ->
                                    return failwith "not implemented"

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

                        | _ -> 
                            let! s = State.get
                            let intrinsic = if isNull s.backend then None else s.backend.CompileIntrinsicFunction mi
                            match intrinsic with
                                | Some fmt -> 
                                    return fmt (List.toArray args)
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
                    let intrinsic = if isNull s.backend then None else s.backend.CompileIntrinsicFunction mi
                    match intrinsic with
                        | Some fmt -> return fmt args
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
                    
                | MemberFieldGet(t, fi) ->
                    let! t = ofExpr t
                    return CExpr.CField(t, fi.Name)

                | PropertyGet(Some t, pi, [index]) when pi.Name = "Item" ->
                    let! t = ofExpr t
                    let! index = ofExpr index
                    return CExpr.CItem(t, index)

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
                | PropertyGet _
                | QuoteRaw _ | QuoteTyped _ 
                | TryWith _
                | TryFinally _ 
                | TypeTest _ ->
                    return failwithf "unsupported expression %A" e
                    

                | e -> 
                    return failwithf "unexpected expression %A" e
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
                        CDeclare(v, Some min),              // int i = min
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
                    let! v = CVar.ofVar v
                    let! e = CExpr.ofExpr e
                    let! body = ofExprInternal last body
                    return CSequential [CStatement.CDeclare(v, Some e); body]

                | LetRecursive(bindings, body) ->
                    return failwith "recursive bindings are not supported"

                | DefaultValue t when last ->
                    let! v = CVar.ofVar (Var("temp", t))
                    return 
                        CSequential [
                            CDeclare(v, None)
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
                            yield CDeclare(v, None)
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

                | Let(v, DefaultValue _, body) ->
                    let! v = CVar.ofVar v
                    let! body = ofExprInternal last body
                    return 
                        CSequential [
                            CDeclare(v, None)
                            body
                        ]

                | Let(v, NewArray(et, args), body) ->
                    let et = CType.ofType et
                    let! args = args |> List.mapS CExpr.ofExpr
                    let args = args |> List.toArray

                    let len = args.Length
                    let ct = CTypeRef.CArray(et, len)
                    let! name = CompilerState.cName v
                    let v = { name = name; ctype = ct }

                    return 
                        CSequential [
                            yield CDeclare(v, None)
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

                | DefaultValue _ -> return failwith "default value in expr"
                | NewArray _ -> return failwith "array-value in expr"

                | FieldSet(None, fi, _) -> return failwithf "cannot set static field %A" fi
                | PropertySet(None, pi, _, _) -> return failwithf "cannot set static property %A" pi

                | FieldSet(Some t, fi, value) ->
                    let! t = CLExpr.ofExpr t
                    let! value = CExpr.ofExpr value
                    return CStatement.CWrite(CLExpr.CField(t, fi.Name), value)

                | AddressSet(addr, value) ->
                    let! addr = CExpr.ofExpr addr
                    let! value = CExpr.ofExpr value
                    return CStatement.CWrite(CLExpr.CPtr addr, value)

                | PropertySet(Some t, pi, [], value) ->
                    let! t = CLExpr.ofExpr t
                    let! value = CExpr.ofExpr value
                    return CStatement.CWrite(CLExpr.CField(t, pi.Name), value)
                    
                | PropertySet(Some t, pi, [index], value) when pi.Name = "Item" ->
                    let! t = CLExpr.ofExpr t
                    let! index = CExpr.ofExpr index
                    let! value = CExpr.ofExpr value
                    return CStatement.CWrite(CLExpr.CItem(t, index), value)

                | PropertySet(_,pi,_,_) -> return failwithf "cannot set property %A" pi


                | e when last ->
                    let! v = CExpr.ofExpr e
                    return CStatement.CReturnValue v

                | _ ->
                    return failwith ""
        }

    let ofExpr (e : Expr) =
        ofExprInternal true e

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CModule =

    type private DefTree =
        | DefNode of CDef * list<DefTree>

    let rec private compileInternal (cache : Dict<ExternalFunction, DefTree>, state : byref<CompilerState>, f : ExternalFunction) =
        match cache.TryGetValue f with
            | (true, t) -> t
            | _ -> 
                match f with
                    | ExtCompiled(s,d) ->
                        DefNode(CDef.CFunction(s,d), [])
                    | _ ->
                        let name, args, body =
                            match f with
                                | ExtCompiled _ -> failwith "impossible"
                                | ExtCtor _ -> failwith "not implemented"
                                | ExtCustom(name, args, body) -> name, args, body
                                | ExtMethod mi ->
                                    let name = methodName mi
                                    match Expr.TryGetReflectedDefinition mi with
                                        | Some def ->
                                            match def with
                                                | Lambdas(args, body) -> name, List.concat args, body
                                                | b -> name, [], b

                                        | None -> 
                                            failwithf "no reflected definition for %A" mi
                        
                        let parTypes = args |> List.map (fun a -> a.Type |> CType.ofType)
                        let ret = CType.ofType body.Type
                        let body = CStatement.ofExpr body

                
                        let mutable initialState =
                            {
                                state with
                                    cnames              = args |> List.map (fun a -> a, a.Name) |> Map.ofList
                                    currentSuffixes     = args |> List.map (fun a -> a.Name, 1) |> Map.ofList
                                    customFunctions     = HashMap.empty
                            }

                        let body = body.Run(&initialState)

                        let signature =
                            { 
                                name        = name
                                returnType  = ret
                                parameters  = List.map2 (fun (v : Var) (ct : CTypeRef) -> { name = v.Name; ctype = ct; modifier = CParameterModifier.In }) args parTypes |> List.toArray
                            }

                        let used =
                            initialState.customFunctions 
                                |> HashMap.toSeq
                                |> Seq.map (fun (signature, def) ->
                                    compileInternal(cache, &initialState, def)
                                )
                                |> Seq.toList

                        let t = DefNode(CDef.CFunction(signature, body), used)
                        cache.[f] <- t
                        t

    let ofLambda (name : string) (e : Expr) : CModule =
        let args, body = 
            match e with
                | Lambdas(args, body) -> List.concat args, body
                | b -> [], b

        let mutable initialState = CompilerState.empty

        let tree = compileInternal(Dict(), &initialState, ExtCustom(name, args, body))

        let rec flatten (DefNode(self, children)) =
            (children |> List.collect flatten) @ [self]

        flatten tree

    let ofFunctions (functions : list<string * list<Var> * Expr>) : CModule =
        let mutable initialState = CompilerState.empty



        functions |> List.map (fun (name, args, body) ->
            let parTypes = args |> List.map (fun a -> a.Type |> CType.ofType)
            let ret = CType.ofType body.Type
            let body = CStatement.ofExpr body

            let mutable initialState =
                {
                    CompilerState.empty with
                        cnames              = args |> List.map (fun a -> a, a.Name) |> Map.ofList
                        currentSuffixes     = args |> List.map (fun a -> a.Name, 1) |> Map.ofList
                }

            let body = body.Run(&initialState)

            let signature =
                { 
                    name        = name
                    returnType  = ret
                    parameters  = List.map2 (fun (v : Var) (ct : CTypeRef) -> { name = v.Name; ctype = ct; modifier = CParameterModifier.In }) args parTypes |> List.toArray
                }

            CFunction(signature, body)
        )

