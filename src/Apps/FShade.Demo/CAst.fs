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

    | CField of target : CExpr * fieldName : string
    | CItem of target : CExpr * index : CExpr 

type CLExpr =
    | CVar of CVar
    | CField of CLExpr * string
    | CItem of CLExpr * CExpr
    | CDeref of CExpr

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
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open FShade.Utils


[<AutoOpen>]
module private Helpers = 
    open System.Text.RegularExpressions

    let rx = Regex @"(?<name>.*)`[0-9]+"



    let rec typeName (t : Type) =
        let selfName = 
            if t.IsGenericType then
                let m = rx.Match t.Name
                let targs = t.GetGenericArguments() |> Seq.map typeName |> String.concat "_"
                m.Groups.["name"].Value + "_" + targs
            else
                t.Name

        if t.IsNested then (typeName t.DeclaringType) + "_" + selfName
        else t.Namespace.Replace('.', '_') + "_" + selfName

    let rec methodName (mi : MethodInfo) =
        let selfName =
            if mi.IsGenericMethod then
                let m = rx.Match mi.Name
                let targs = mi.GetGenericArguments() |> Seq.map typeName |> String.concat "_"
                m.Groups.["name"].Value + "_" + targs
            else
                mi.Name
        (typeName mi.DeclaringType) + "_" + selfName


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
    | ExtMethod of MethodInfo
    | ExtCustom of string * list<Var> * Expr

[<AllowNullLiteral>]
type IBackend =
    abstract member CompileIntrinsicFunction : MethodInfo -> Option<CExpr[] -> CExpr>

type CompilerState =
    {
        backend         : IBackend
        cnames          : Map<Var, string>
        currentSuffixes : Map<string, int>

        usedTypes       : pset<CTypeRef>
        usedFunctions   : HashMap<CFunctionRef, ExternalFunction>

        helperIndex     : int

    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CompilerState =

    let empty =
        {
            backend         = null
            cnames          = Map.empty
            currentSuffixes = Map.empty
            usedTypes       = PSet.empty
            usedFunctions   = HashMap.empty
            helperIndex     = 0
        }

    let cName (v : Var) =
        state {
            let! s = State.get
            match Map.tryFind v s.cnames with
                | Some n -> return n
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

    let useType (t : CTypeRef) =
        State.modify(fun s -> { s with usedTypes = PSet.add t s.usedTypes })

    let useFunction (f : CFunctionRef) (mi : MethodInfo) =
        State.modify(fun s -> { s with usedFunctions = HashMap.add f (ExtMethod mi) s.usedFunctions })

    let asFunction (e : Expr) =
        state {
            let vars = e.GetFreeVars() |> Seq.toList
            let! s = State.get
            
            let name = "helper" + string s.helperIndex
            let parameters = vars |> List.map (fun v -> { name = v.Name; ctype = CType.ofType v.Type; modifier = CParameterModifier.In }) |> List.toArray
            let signature = { name = name; parameters = parameters; returnType = CType.ofType e.Type }

            do! State.put { s with helperIndex = s.helperIndex + 1; usedFunctions = HashMap.add signature (ExtCustom(name, vars, e)) s.usedFunctions }

            let args = 
                vars 
                    |> List.map (fun v -> CExpr.CVar { name = v.Name; ctype = CType.ofType v.Type })
                    |> List.toArray

            return CExpr.CCall(signature, args)
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CVar =
    let ofVar (v : Var) : State<CompilerState, CVar> =
        state {
            let ctype = CType.ofType v.Type
            let! name = CompilerState.cName v
            return { name = name; ctype = ctype }
        }


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
            let ct = CType.ofType e.Type
            do! State.modify(fun s -> { s with usedTypes = PSet.add ct s.usedTypes })

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
                                    do! CompilerState.useFunction funRef mi
                            
                                    return CExpr.CCall(funRef, List.toArray args)

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

                | NewObject(ctor, args) ->
                    return failwith "not implemented"



                | e -> 
                    return! CompilerState.asFunction e
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CStatement =
    let rec ofExprInternal (last : bool) (e : Expr) =
        let ret (s : CStatement) =
            if last then CSequential[s; CReturn]
            else s

        state {
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
                let name, args, body =
                    match f with
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
                            cnames          = args |> List.map (fun a -> a, a.Name) |> Map.ofList
                            currentSuffixes = args |> List.map (fun a -> a.Name, 1) |> Map.ofList
                            usedFunctions   = HashMap.empty
                    }

                let body = body.Run(&initialState)

                let signature =
                    { 
                        name        = name
                        returnType  = ret
                        parameters  = List.map2 (fun (v : Var) (ct : CTypeRef) -> { name = v.Name; ctype = ct; modifier = CParameterModifier.In }) args parTypes |> List.toArray
                    }

                let used =
                    initialState.usedFunctions 
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

        let mutable initialState =
            {
                backend         = null
                cnames          = Map.empty
                currentSuffixes = Map.empty
                usedTypes       = PSet.empty
                usedFunctions   = HashMap.empty
                helperIndex     = 0
            }

        let tree = compileInternal(Dict(), &initialState, ExtCustom(name, args, body))

        let rec flatten (DefNode(self, children)) =
            (children |> List.collect flatten) @ [self]

        flatten tree

    let ofFunctions (functions : list<string * list<Var> * Expr>) : CModule =
        let mutable initialState =
            {
                backend         = null
                cnames          = Map.empty
                currentSuffixes = Map.empty
                usedTypes       = PSet.empty
                usedFunctions   = HashMap.empty
                helperIndex     = 0
            }



        functions |> List.map (fun (name, args, body) ->
            let parTypes = args |> List.map (fun a -> a.Type |> CType.ofType)
            let ret = CType.ofType body.Type
            let body = CStatement.ofExpr body

            let mutable initialState =
                {
                    backend         = null
                    cnames          = args |> List.map (fun a -> a, a.Name) |> Map.ofList
                    currentSuffixes = args |> List.map (fun a -> a.Name, 1) |> Map.ofList
                    usedTypes       = PSet.empty
                    usedFunctions   = HashMap.empty
                    helperIndex     = 0
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

