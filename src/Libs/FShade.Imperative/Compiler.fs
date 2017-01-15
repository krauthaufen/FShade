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


module Compiler =
    open Aardvark.Base.Monads.State

    [<AutoOpen>]
    module private Helpers =
        let inline (>>=) (m : State<'s, 'a>) (f : 'a -> State<'s, 'b>) =
            m |> State.bind f

        let inline (|>>) (m : State<'s, 'a>) (f : 'a -> 'b) =
            m |> State.map f

    [<AbstractClass>]
    type Backend() =
        class end

    type FunctionDefinition = 
        | ManagedFunction of name : string * args : list<Var> * body : Expr
        | CompiledFunction of signature : CFunctionSignature * body : CStatement

        member x.Signature =
            match x with
                | CompiledFunction(s,_) -> s
                | ManagedFunction(name,args,body) ->
                    CFunctionSignature.ofFunction name args body.Type

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
            Unchecked.defaultof<FunctionDefinition>

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

    let toCType (t : Type) =
        State.custom (fun s ->
            let cType = CType.ofType t

            match cType with
                | CStruct _ ->
                    { s with usedTypes = HashMap.add (t :> obj) cType s.usedTypes }, cType
                | _ ->
                    s, cType
        )

    let toCVar (v : Var) =
        state {
            let! ctype = toCType v.Type
            let! name = CompilerState.variableName v
            return { name = name; ctype = ctype }
        }

    let rec toCExpr (e : Expr) =
        state {
            match e with
                | Var v ->
                    let! v = toCVar v
                    return CVar v

                | NewTuple(fields) ->
                    let! ctor = e.Type |> Constructors.tuple |> CompilerState.useCtor e.Type
                    let! fields = fields |> List.mapS toCExpr |>> List.toArray
                    return CCall(ctor, fields)



                | _ ->
                    return failwith ""
        }

    and toCLExpr (e : Expr) =
        e |> toCExpr |> State.map CLExpr.ofExpr

    and toCRExpr (e : Expr) =
        state {
            match e with
                | NewFixedArray(cnt, et, args) ->
                    let ct = CType.ofType et
                    let! args = args |> List.mapS toCExpr
                    return CRArray(CArray(ct, cnt), args)

                | NewArray(et, args) ->
                    let ct = CType.ofType et
                    let cnt = List.length args
                    let! args = args |> List.mapS toCExpr
                    return CRArray(CArray(ct, cnt), args)

                // TODO: also compile constant-values being seq<_>

                | _ ->
                    let! res = e |> toCExpr
                    return CRExpr.ofExpr res
        }

        
        

    let toCStatement (e : Expr) =
        CStatement.CNop
