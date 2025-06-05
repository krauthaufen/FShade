namespace FShade

open System
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative
open FSharp.Data.Adaptive

module PrettyPrinter =
    open Aardvark.Base.Monads.State

    let rec print (e : Expr) =
        match e with
            | IfThenElse(c,i,Unit) ->
                let c = print c
                let i = print i
                sprintf "if %s then\r\n%s" c (String.indent 1 i)
               
            | IfThenElse(c,i,e) ->
                let c = print c
                let i = print i
                let e = print e
                sprintf "if %s then\r\n%s\r\nelse\r\n%s" c (String.indent 1 i) (String.indent 2 i)
               
            | ForInteger(v, start, step, stop, body) ->
                let start = print start
                let stop = print stop
                let body = print body
                match step with
                    | Int32 1 ->
                        sprintf "for %s in %s .. %s do\r\n%s" v.Name start stop (String.indent 1 body)
                    | _ ->
                        let step = print step
                        sprintf "for %s in %s .. %s .. %s do\r\n%s" v.Name start step stop (String.indent 1 body)
                             
            | ForEach(v, seq, body) ->
                let seq = print seq
                let body = print body
                sprintf "for %s in %s do\r\n%s" v.Name seq (String.indent 1 body)

            | WhileLoop(guard, body) ->
                let guard = print guard
                let body = print body
                sprintf "while %s do\r\n%s" guard (String.indent 1 body)
                
            | Lambdas(args, body) ->
                let args = args |> List.map (List.map (fun v -> v.Name) >> String.concat ", " >> sprintf "(%s)") |> String.concat " "
                let body = print body
                sprintf "fun %s ->\r\n%s" args (String.indent 1 body)
                  
            | ReadInputOrRaytracingData(kind, name, index, _) ->
                let indexer = 
                    match index with
                        | Some idx -> print idx |> sprintf ".[%s]"
                        | None -> ""
                sprintf "%s%s" name indexer

            | WriteOutputs outputs ->
                outputs |> Map.toSeq |> Seq.map (fun (name,(index, value)) ->
                    let indexer = 
                        match index with
                            | Some idx -> print idx |> sprintf ".[%s]"
                            | None -> ""

                    let value = print value
                    sprintf "%s%s = %s" name indexer value
                )
                |> String.concat "\r\n"

            | Call(None, mi, args) ->
                let args = args |> List.map print |> String.concat ", "
                sprintf "%s(%s)" mi.Name args

            | Call(Some t, mi, args) ->
                let t = print t
                let args = args |> List.map print |> String.concat ", "
                sprintf "%s.%s(%s)" t mi.Name args
            
            | Let(v,e,b) ->
                let e = print e
                let b = print b
                if v.IsMutable then
                    sprintf "let mutable %s = %s\r\n%s" v.Name e b
                else
                    sprintf "let %s = %s\r\n%s" v.Name e b

            | Sequential(l,r) ->
                let l = print l
                let r = print r
                sprintf "%s\r\n%s" l r

            | Var v ->
                v.Name

            | FieldGet(None, f) ->
                sprintf "%s" f.Name

            | FieldGet(Some t, f) ->
                let t = print t
                sprintf "%s.%s" t f.Name
                
            | PropertyGet(None, p, indices) ->
                let indexers =
                    match indices with
                        | [] -> ""
                        | i -> i |> List.map print |> String.concat ", " |> sprintf ".[%s]"
                sprintf "%s%s" p.Name indexers

            | PropertyGet(Some t, f, indices) ->
                let indexers =
                    match indices with
                        | [] -> ""
                        | i -> i |> List.map print |> String.concat ", " |> sprintf ".[%s]"

                let t = print t
                if f.Name = "Item" && not (List.isEmpty indices) then
                    sprintf "%s%s" t indexers
                else
                    sprintf "%s.%s%s" t f.Name indexers

            | VarSet(v, e) ->
                let e = print e
                sprintf "%s <- %s" v.Name e

            | FieldSet(None, f, value) ->
                let value = print value
                sprintf "%s <- %s" f.Name value 

            | FieldSet(Some t, f, value) ->
                let t = print t
                let value = print value
                sprintf "%s.%s <- %s" t f.Name value 

            | PropertySet(None, p, indices, value) ->
                let indexers =
                    match indices with
                        | [] -> ""
                        | i -> i |> List.map print |> String.concat ", " |> sprintf ".[%s]"

                let value = print value
                sprintf "%s%s <- %s" p.Name indexers value
                
            | PropertySet(Some t, p, indices, value) ->
                let indexers =
                    match indices with
                        | [] -> ""
                        | i -> i |> List.map print |> String.concat ", " |> sprintf ".[%s]"

                let value = print value
                let t = print t
                if p.Name = "Item" && not (List.isEmpty indices) then
                    sprintf "%s.%s <- %s" t indexers value
                else
                    sprintf "%s.%s%s <- %s" t p.Name indexers value

            | UnsafeWrite(t, v) ->
                let t = print t
                let v = print v
                sprintf "%s <- %s" t v

            | Value(v,_) ->
                sprintf "%A" v

            | ShapeCombination(o, args) ->
                let args = args |> List.map print |> String.concat ", "
                sprintf "%A(%s)" o args

            | _ ->
                failwith ""


[<AutoOpen>]
module Optimizer =
    open Aardvark.Base.Monads.State

    module UtilityFunction =

        let rec singleReturn (e : Expr) =
            match e with
            | Lambda(_, b) ->
                singleReturn b
            | Sequential(_, r) -> 
                singleReturn r
            | IfThenElse _ ->
                false
            | LetRecursive(_, b)
            | Let(_, _, b) ->
                singleReturn b


            | e ->
                true
            

        let rec subsituteValue (s : Expr -> Expr) (e : Expr) =
            match e with
            | Lambda(v, b) ->
                Expr.Lambda(v, subsituteValue s b)
            | Sequential(l, r) -> 
                Expr.Sequential(l, subsituteValue s r)
            | IfThenElse(c, i, e) ->
                Expr.IfThenElse(c, subsituteValue s i, subsituteValue s e)
            | Let(v, e, b) ->
                Expr.Let(v, e, subsituteValue s b)
            | e ->
                s e

        
        let rec subsituteValueS (s : Expr -> State<'s, Expr>) (e : Expr) : State<'s, Expr> =
            state {
                match e with
                | Lambda(v, b) ->
                    let! b = subsituteValueS s b
                    return Expr.Lambda(v, b)
                | Sequential(l, r) -> 
                    let! r = subsituteValueS s r
                    return Expr.Sequential(l, r)
                | IfThenElse(c, i, e) ->
                    let! i = subsituteValueS s i
                    let! e = subsituteValueS s e
                    return Expr.IfThenElse(c, i, e)
                | Let(v, e, b) ->
                    let! b = subsituteValueS s b
                    return Expr.Let(v, e, b)
                | e ->
                    return! s e
            }

        [<return: Struct>]
        let rec (|TrivialOrInput|_|) (e : Expr) =
            match e with
            | Var _ 
            | Value _
            | FieldGet(None, _)
            | PropertyGet(None, _, [])
            | TupleGet(Trivial, _)
            | PropertyGet(Some TrivialOrInput, (FSharpTypeProperty | ArrayLengthProperty), [])
            | FieldGet(Some TrivialOrInput, _) 
            | ReadInput(_, _, None) 
            | ReadInput(_, _, Some TrivialOrInput) -> 
                ValueSome ()
            | _ ->
                ValueNone

        let inlineCode (f : UtilityFunction) (args : list<Expr>) =
            let rec wrap (v : list<Var>) (e : list<Expr>) (b : Expr) =
                match v, e with
                    | [], [] -> 
                        b

                    | v :: vs, (TrivialOrInput as ev) :: es ->
                        let b = b.Substitute(fun vi -> if vi = v then Some ev else None)
                        wrap vs es b

                    | v :: vs, e :: es ->
                        let mutable useCnt = 0
                        let bt = b.Substitute(fun vi -> if vi = v then inc &useCnt; Some e else None)
                        if useCnt <= 1 then
                            wrap vs es bt
                        else
                            Expr.Let(v,e, wrap vs es b)

                    | _ ->
                        failwithf "[FShade] bad arity for utility function call"

            wrap f.functionArguments args f.functionBody

    [<AutoOpen>]
    module Helpers = 
        module State =
            let withLocalState (m : State<'s, 'a>) =
                State.get |> State.map (fun s ->
                    m |> State.run s
                )

        module Option =
            let mapS (f : 'a -> State<'s, 'b>) (m : Option<'a>) =
                match m with
                    | Some a -> a |> f |> State.map Some
                    | None -> State.value None

        [<return: Struct>]
        let rec (|LExpr|_|) (e : Expr) =
            // TODO: complete?
            match e with
            | Var v -> ValueSome v
            | FieldGet(Some (LExpr v), fi) -> ValueSome v
            | PropertyGet(Some (LExpr v), _, _) -> ValueSome v
            | GetArray(LExpr a, i) -> ValueSome a
            | _ -> ValueNone

        [<return: Struct>]
        let rec (|MutableArgument|_|) (e : Expr) =
            match e with
            | Var v -> 
                if v.IsMutable then
                    ValueSome v
                else
                    match v.Type with
                    | ArrOf _ -> ValueSome v
                    | ArrayOf _ -> ValueSome v
                    | Ref _ -> ValueSome v
                    | _ -> ValueNone
            | GetArray(Var v, _) -> ValueSome v

            | RefOf (MutableArgument v) -> ValueSome v
            | AddressOf (MutableArgument v) -> ValueSome v
            | _ -> ValueNone

        [<return: Struct>]
        let rec (|StorageArgument|_|) (e : Expr) =
            match e with
            | RefOf (GetArray(ReadInputOrRaytracingData(ParameterKind.Uniform, _, _, _), _)) ->
                ValueSome ()
            | _ ->
                ValueNone

    /// The dead code elimination removes unused bindings from the code
    /// e.g. `let a = someFunction(x,y) in 10` gets reduced to `10` assuming
    /// that all functions are pure except the ones identified by the given function.
    /// this handling seems proper since most GLSL functions are pure (except for discard, EmitVertex, etc.)
    module DeadCodeElimination = 

        type EliminationState =
            {
                isGlobalSideEffect : MethodInfo -> bool
                usedVariables : Set<Var>
            }


        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module EliminationState =
            let empty = 
                { 
                    isGlobalSideEffect = fun mi -> mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<System.Void>
                    usedVariables = Set.empty
                }
            let useVar (v : Var) = State.modify (fun s -> { s with usedVariables = Set.add v s.usedVariables })
            let remVar (v : Var) = State.modify (fun s -> { s with usedVariables = Set.remove v s.usedVariables })
            let isUsed (v : Var) = State.get |> State.map (fun s -> Set.contains v s.usedVariables)
            let useVars (vars : seq<Var>) = State.modify (fun s -> { s with usedVariables = Set.union (Set.ofSeq vars) s.usedVariables })
       

            let merge (l : EliminationState) (r : EliminationState) =
                {
                    isGlobalSideEffect = l.isGlobalSideEffect
                    usedVariables = Set.union l.usedVariables r.usedVariables
                }

            let rec fix (m : State<EliminationState, 'a>) =
                state {
                    let! initial = State.get
                    let! res = m
                    let! final = State.get
                    if initial.usedVariables = final.usedVariables then return res
                    else return! fix m
                }
                
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module List =
            let rec existsS (f : 'a -> State<'s, bool>) (l : list<'a>) =
                state {
                    match l with
                        | [] -> return false
                        | h :: t ->
                            let! r = f h
                            if r then 
                                return true
                            else
                                return! existsS f t
                }

        let private staticallyNeededCalls =
            HashSet.ofList [
                MethodInfo.WriteOutputs
                MethodInfo.Unroll
            ]

        let private callNeededS (t : Option<Expr>) (mi : MethodInfo) (args : list<Expr>) =
            state {
                let! state = State.get

                if state.isGlobalSideEffect mi || staticallyNeededCalls.Contains mi then
                    return true
                else
                    let needsMutableParameter (p : ParameterInfo) (v : Expr) =
                        let pt = p.ParameterType
                        if pt.IsByRef || pt.IsArray || pt.IsArr || pt.IsRef then
                            match v with
                                | MutableArgument v -> Set.contains v state.usedVariables
                                | StorageArgument -> true
                                | _ -> false
                        else
                            false

                    let parameters = mi.GetParameters() |> Array.toList
                    match t with
                        | Some t ->
                            match t with
                                | MutableArgument v when Set.contains v state.usedVariables -> 
                                    return true
                                | StorageArgument -> 
                                    return true
                                | _ -> 
                                    return List.exists2 needsMutableParameter parameters args
                        | None ->
                            return List.exists2 needsMutableParameter parameters args
            }

        let rec private hasSideEffectsS (e : Expr) =
            state {
                match e with
                    | CallFunction(utility, args) ->
                        let! aa = args |> List.existsS hasSideEffectsS
                        if aa then
                            return true
                        else
                            return! hasSideEffectsS utility.functionBody
                    | Call(t, mi, args) ->
                        let! ts = t |> Option.mapS hasSideEffectsS
                        match ts with
                            | Some true -> 
                                return true
                            | _ ->
                                let! aa = args |> List.existsS hasSideEffectsS
                                if aa then 
                                    return true
                                else
                                    let! state = State.get

                                    if state.isGlobalSideEffect mi || staticallyNeededCalls.Contains mi then
                                        return true
                                    else
                                        return false

                    | ShapeLambda(_,b) ->
                        return! hasSideEffectsS b

                    | ShapeVar _ ->
                        return false

                    | ShapeCombination(o, args) ->
                        return! List.existsS hasSideEffectsS args
            }

        let rec withoutValueS (e : Expr) : State<EliminationState, Expr> =
            state {
                match e with
                    | ExprOf t when t = typeof<unit> ->
                        return! eliminateDeadCodeS e

                    
                    | IfThenElse(cond, i, e) ->
                        let! elseState, e = State.withLocalState (withoutValueS e)
                        let! ifState, i = State.withLocalState (withoutValueS i)
                        do! State.put (EliminationState.merge ifState elseState)
                    
                        match i, e with
                            | Unit, Unit -> 
                                return! withoutValueS cond
                            | _ ->
                                let! cond = eliminateDeadCodeS cond
                                return Expr.IfThenElse(cond, i, e)

                    | AddressOf e ->
                        return! withoutValueS e

                    | AddressSet(e,v) ->
                        let! v = withoutValueS v
                        let! e = withoutValueS e
                        return Expr.Seq [v; e]

                    | Application(l, a) ->
                        let! a = eliminateDeadCodeS a
                        let! l = eliminateDeadCodeS l
                        return Expr.Application(l, a)

                    | CallFunction(utility, args) ->
                        let! sideEffects = hasSideEffectsS utility.functionBody
                        if sideEffects then
                            let! self = eliminateDeadCodeS e
                            if self.Type = typeof<unit> then
                                return e
                            else
                                return Expr.Ignore e
                        else
                            return Expr.Unit

                    | CallWithWitnesses(t, oi, mi, ws, args) ->
                        let! needed = callNeededS t oi args

                        if needed then
                            let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                            let! t = t |> Option.mapS eliminateDeadCodeS

                            match t with
                                | Some t -> return Expr.CallWithWitnesses(t, oi, mi, ws, args) |> Expr.Ignore
                                | None -> return Expr.CallWithWitnesses(oi, mi, ws, args) |> Expr.Ignore
                        else
                            let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                            let! t = t |> Option.mapS withoutValueS

                            match t with
                                | Some t -> return Expr.Seq (t :: args)
                                | None -> return Expr.Seq args            

                        

                    | Call(t, mi, args) ->
                        let! needed = callNeededS t mi args

                        if needed then
                            let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                            let! t = t |> Option.mapS eliminateDeadCodeS

                            match t with
                                | Some t -> return Expr.Call(t, mi, args) |> Expr.Ignore
                                | None -> return Expr.Call(mi, args) |> Expr.Ignore
                        else
                            let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                            let! t = t |> Option.mapS withoutValueS

                            match t with
                                | Some t -> return Expr.Seq (t :: args)
                                | None -> return Expr.Seq args            


                    | Coerce(e,t) ->
                        return! withoutValueS e

                    | DefaultValue t ->
                        return Expr.Unit

                    | FieldGet(None, _) -> 
                        return Expr.Unit

                    | FieldGet(Some t, _) ->
                        return! withoutValueS t

                    | Lambda(v, b) ->
                        return Expr.Unit

                    | LetRecursive _ ->
                        return failwith "recursive bindings not implemented"
                    
                    | Let(v,e,b) ->
                        let! b = withoutValueS b
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! e = eliminateDeadCodeS e
                            return Expr.Let(v, e, b)

                        else
                            let! e = withoutValueS e
                            match e with
                                | Unit -> return b
                                | _ -> return Expr.Sequential(e, b)

                    | NewArray(t, args) ->
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq args

                    | NewDelegate(t, vars, e) ->
                        return! withoutValueS e
  
                    | NewObject(ctor, args) ->
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq args

                    | NewRecord(t, args) ->
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq args

                    | NewTuple(args) ->
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq args

                    | NewUnionCase(ci, args) ->
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq args


                    | PropertyGet(None, pi, idx) ->
                        let! idx = idx |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq idx

                    | PropertyGet(Some t, pi, idx) ->
                        let! idx = idx |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        let! t = withoutValueS t
                        return Expr.Seq (t :: idx)
                    
                    | QuoteRaw _ | QuoteTyped _ -> 
                        return failwith "not implemented"

                    | Sequential(l, r) ->
                        let! r = withoutValueS r
                        let! l = withoutValueS l
                        match l, r with
                            | Unit, r   -> return r
                            | l, Unit   -> return l
                            | l, r      -> return Expr.Sequential(l, r)

                    | TryWith _ | TryFinally _ -> 
                        return failwith "not implemented"

                    | TupleGet(t, i) ->
                        return! withoutValueS t

                    | TypeTest(e, t) ->
                        return! withoutValueS e

                    | UnionCaseTest(e, ci) ->
                        return! withoutValueS e

                    | Value _ ->
                        return Expr.Unit

                    | Var v ->
                        return Expr.Unit

                    | _ ->
                        return failwith ""

        
            }

        and eliminateDeadCodeS (e : Expr) : State<EliminationState, Expr> =
            state {
                match e with

                    // side effects affecting variables
                    | VarSet(v, e) ->
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! e = eliminateDeadCodeS e
                            return Expr.VarSet(v, e)
                        else
                            return! withoutValueS e

                    | FieldSet(Some (LExpr v as target), fi, value) ->
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! value = eliminateDeadCodeS value
                            let! target = eliminateDeadCodeS target
                            return Expr.FieldSet(target, fi, value)
                        else
                            let! v = withoutValueS value
                            let! t = withoutValueS target
                            return Expr.Seq [v;t]

                    | UnsafeWrite((LExpr v as target), value) ->
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! value = eliminateDeadCodeS value
                            let! target = eliminateDeadCodeS target
                            return Expr.UnsafeWrite(target, value)
                        else 
                            let! value = withoutValueS value
                            let! target = withoutValueS target

                            return Expr.Seq [
                                yield target
                                yield value
                            ]

                    | PropertySet(Some (LExpr v as target), pi, idx, value) ->
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! value = eliminateDeadCodeS value
                            let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                            let! target = eliminateDeadCodeS target
                            return Expr.PropertySet(target, pi, value, idx)
                        else 
                            let! value = withoutValueS value
                            let! idx = idx |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                            let! target = withoutValueS target

                            return Expr.Seq [
                                yield target
                                yield! idx
                                yield value
                            ]

                    // global side effects
                    | FieldSet(None, f, value) ->
                        let! value = eliminateDeadCodeS value
                        return Expr.FieldSet(f, value)

                    | PropertySet(None, pi, idx, value) ->
                        let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        let! value = eliminateDeadCodeS value
                        return Expr.PropertySet(pi, value, idx)

                    // unknown side effects
                    | FieldSet(Some t, f, value) ->
                        match t with
                        | GetArray(ReadInputOrRaytracingData(ParameterKind.Uniform, _, _, _), _) -> ()
                        | _ -> Log.warn "[FShade] found FieldSet on unknown expression: %A" t

                        let! value = eliminateDeadCodeS value
                        let! t = eliminateDeadCodeS t
                        return Expr.FieldSet(t, f, value)

                    | PropertySet(Some t, pi, idx, value) ->
                        match t.Type with
                         | ImageType _ -> ()
                         | _ -> Log.warn "[FShade] found PropertySet on unknown expression: %A" t

                        let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        let! value = eliminateDeadCodeS value
                        let! t = eliminateDeadCodeS t
                        return Expr.PropertySet(t, pi, value, idx)

                    | UnsafeWrite(t, value) ->
                        match t with
                        | ReadRaytracingData _ -> ()
                        | _ -> Log.warn "[FShade] found UnsafeWrite on unknown expression: %A" t

                        let! value = eliminateDeadCodeS value
                        let! t = eliminateDeadCodeS t
                        return Expr.UnsafeWrite(t, value)


                    // control-flow
                    | IfThenElse(cond, i, e) ->
                        let! elseState, e = State.withLocalState (eliminateDeadCodeS e)
                        let! ifState, i = State.withLocalState (eliminateDeadCodeS i)
                        do! State.put (EliminationState.merge ifState elseState)

                        match i, e with
                            | Unit, Unit ->
                                return! withoutValueS cond
                            | _ ->
                                let! cond = eliminateDeadCodeS cond
                                return Expr.IfThenElse(cond, i, e)

                    | ForEach(v, seq, body) ->
                        let iterate =
                            state {
                                let! body = eliminateDeadCodeS body
                                match body with
                                    | Unit ->
                                        return! withoutValueS seq
                                    | _ ->
                                        let! seq = eliminateDeadCodeS seq
                                        return Expr.ForEach(v, seq, body)
                                    
                            }
                        return! EliminationState.fix iterate

                    | ForInteger(v, first, step, last, body) ->
                        let iterate =
                            state {
                                let! body = eliminateDeadCodeS body
                                match body with
                                    | Unit ->
                                        let! last = withoutValueS last
                                        let! step = withoutValueS step
                                        let! first = withoutValueS first
                                        return Expr.Seq [first; step; last]
                                    | _ ->
                                        let! last = eliminateDeadCodeS last
                                        let! step = eliminateDeadCodeS step
                                        let! first = eliminateDeadCodeS first
                                        return Expr.ForInteger(v, first, step, last, body)
                                    
                            }

                        return! EliminationState.fix iterate

                    | WhileLoop(guard, body) ->
                        let iterate =
                            state {
                                let! body = eliminateDeadCodeS body
                                match body with
                                    | Unit -> 
                                        let! guardState, guard = State.withLocalState (withoutValueS guard)
                                        match guard with
                                            | Unit ->
                                                do! State.put guardState
                                                return Expr.Unit
                                            | _ -> 
                                                let! guard = eliminateDeadCodeS guard
                                                return Expr.WhileLoop(guard, body)
                                    | _ ->
                                        let! guard = eliminateDeadCodeS guard
                                        return Expr.WhileLoop(guard, body)
                            }

                        return! EliminationState.fix iterate


                    | Ignore e ->
                        return! withoutValueS e

                    | AddressOf e ->
                        let! e = eliminateDeadCodeS e
                        return Expr.AddressOf(e)

                    | AddressSet(e,v) ->
                        let! v = eliminateDeadCodeS v
                        let! e = eliminateDeadCodeS e
                        return Expr.AddressSet(e, v)

                    | Application(l, a) ->
                        let! a = eliminateDeadCodeS a
                        let! l = eliminateDeadCodeS l
                        return Expr.Application(l, a)
           
                    | CallFunction(utility, args) ->
                        // TODO: is the call needed???
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        let! s = State.get

                        let utility =
                            utility |> UtilityFunction.map (fun b ->
                                let mutable innerState = { EliminationState.empty with isGlobalSideEffect = s.isGlobalSideEffect }

                                for (v, a) in List.zip args utility.functionArguments do
                                    let isMutable = a.IsMutable || a.Type.IsArr || a.Type.IsArray || a.Type.IsRef
                                    let isUsed =
                                        match v with
                                            | LExpr v 
                                            | RefOf (LExpr v) -> Set.contains v s.usedVariables
                                            | _ -> false

                                    if isMutable && isUsed then
                                        innerState <- { innerState with usedVariables = Set.add a innerState.usedVariables }
                                        

                                let res = eliminateDeadCodeS(b).Run(&innerState)
                                res
                            )

                        let usedVariables = utility.functionBody.GetFreeVars() |> Set.ofSeq
                        let allArgsUsed = List.forall (fun v -> Set.contains v usedVariables) utility.functionArguments

                        if allArgsUsed then
                            return Expr.CallFunction(utility, args)
                        else
                            let args, values = 
                                List.zip utility.functionArguments args
                                    |> List.filter (fun (v,_) -> Set.contains v usedVariables)
                                    |> List.unzip

                            let utility =
                                { utility with
                                    functionArguments = args
                                    functionTag = null   
                                }

                            return Expr.CallFunction(utility, values)

                    | SetArray(arr, idx, value) ->
                        let! s = State.get
                        let needed = 
                            match arr with
                                | Var v -> Set.contains v s.usedVariables
                                | ReadInput _ -> true
                                | _ -> false

                        if needed then
                            let! value = eliminateDeadCodeS value
                            let! idx = eliminateDeadCodeS idx
                            let! arr = eliminateDeadCodeS arr
                            return Expr.ArraySet(arr, idx, value)
                        else
                            return Expr.Unit

                    | CallWithWitnesses(t, oi, mi, ws, args) ->
                        let! needed = 
                            if e.Type <> typeof<unit> then State.value true
                            else callNeededS t oi args

                        if needed then
                            let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                            let! t = t |> Option.mapS eliminateDeadCodeS

                            match t with
                                | Some t -> return Expr.CallWithWitnesses(t, oi, mi, ws, args)
                                | None -> return Expr.CallWithWitnesses(oi, mi, ws, args)
                        else
                            return Expr.Unit

                    | Call(t, mi, args) ->
                        let! needed = 
                            if e.Type <> typeof<unit> then State.value true
                            else callNeededS t mi args

                        if needed then
                            let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                            let! t = t |> Option.mapS eliminateDeadCodeS

                            match t with
                                | Some t -> return Expr.Call(t, mi, args)
                                | None -> return Expr.Call(mi, args)
                        else
                            return Expr.Unit

                    | Coerce(e,t) ->
                        let! e = eliminateDeadCodeS e
                        return Expr.Coerce(e, t)

                    | DefaultValue t ->
                        return e

                    | FieldGet(None, f) -> 
                        return e

                    | FieldGet(Some t, f) ->
                        let! t = eliminateDeadCodeS t
                        return Expr.FieldGet(t, f)

                    | Lambda(v, b) ->
                        let! b = eliminateDeadCodeS b
                        return Expr.Lambda(v, b)
  
                    | LetRecursive _ ->
                        return failwith "recursive bindings not implemented"
                    
                    | Let(v,e,b) ->
                        let! b = eliminateDeadCodeS b
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! e = eliminateDeadCodeS e
                            return Expr.Let(v, e, b)

                        else
                            let! e = withoutValueS e
                            match e with
                                | Unit -> return b
                                | _ -> return Expr.Sequential(e, b)

                    | NewArray(t, args) ->
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.NewArray(t, args)

                    | NewDelegate(t, vars, e) ->
                        let! e = eliminateDeadCodeS e
                        return Expr.NewDelegate(t, vars, e)
  
                    | NewObject(ctor, args) ->
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.NewObject(ctor, args)

                    | NewRecord(t, args) ->
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.NewRecord(t, args)

                    | NewTuple(args) ->
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.NewTuple(args)

                    | NewUnionCase(ci, args) ->
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.NewUnionCase(ci, args)

                    | PropertyGet(None, pi, idx) ->
                        let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.PropertyGet(pi, idx)

                    | PropertyGet(Some t, pi, idx) ->
                        let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        let! t = eliminateDeadCodeS t
                        return Expr.PropertyGet(t, pi, idx)
                    
                    | QuoteRaw _ | QuoteTyped _ -> 
                        return failwith "not implemented"

                    | Sequential(l, r) ->
                        let! r = eliminateDeadCodeS r
                        let! l = eliminateDeadCodeS l
                        match l, r with
                            | Unit, r   -> return r
                            | l, Unit   -> return l
                            | l, r      -> return Expr.Sequential(l, r)

                    | TryWith _ | TryFinally _ -> 
                        return failwith "not implemented"

                    | TupleGet(t, i) ->
                        let! t = eliminateDeadCodeS t
                        return Expr.TupleGet(t, i)

                    | TypeTest(e, t) ->
                        let! e = eliminateDeadCodeS e
                        return Expr.TypeTest(e, t)

                    | UnionCaseTest(e, ci) ->
                        let! e = eliminateDeadCodeS e
                        return Expr.UnionCaseTest(e, ci)

                    | Value _ ->
                        return e


                    | Var v ->
                        do! EliminationState.useVar v
                        return e

                    | _ ->
                        return failwithf "[FShade] unexpected expression %A" e
            }

        let withoutValue (e : Expr) =
            let run = withoutValueS e
            let mutable state = EliminationState.empty
            run.Run(&state)

        let eliminateDeadCode (e : Expr) =
            let run = eliminateDeadCodeS e
            let mutable state = EliminationState.empty
            run.Run(&state)

        let withoutValue' (isSideEffect : MethodInfo -> bool) (e : Expr) =
            let run = withoutValueS e
            let mutable state = { EliminationState.empty with isGlobalSideEffect = isSideEffect }
            run.Run(&state)

        let eliminateDeadCode' (isSideEffect : MethodInfo -> bool)  (e : Expr) =
            let run = eliminateDeadCodeS e
            let mutable state = { EliminationState.empty with isGlobalSideEffect = isSideEffect }
            run.Run(&state)

    /// the constant folding pass tries to evaluate constant expressions throughout the tree.
    /// e.g. `let a = 10 * 2 + 3 in ...` translates to `let a = 23 in ...` and so on.
    /// NOTE: since some built-in F# functions cannot be invoked dynamically (using reflection) the
    ///       module contains a list of many functions but may print a warning in certain scenarios.
    module ConstantFolding =
        
        type State = 
            {
                variableValues : Map<Var, obj>
                isGlobalSideEffect : MemberInfo -> bool
            }

        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module State =
            let empty = 
                { 
                    variableValues = Map.empty
                    isGlobalSideEffect = fun mi -> 
                        match mi with
                            | :? MethodInfo as mi -> mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<System.Void>
                            | _ -> false
                }

            let needsCall (mi : MethodInfo) : State<State, bool> =
                State.get |> State.map (fun s -> 
                    (mi.IsGenericMethod && mi.GetGenericMethodDefinition() = MethodInfo.ReadInput) ||
                    (mi.IsGenericMethod && mi.GetGenericMethodDefinition() = MethodInfo.ReadInputIndexed) ||
                    (mi = MethodInfo.WriteOutputs) || 
                    (mi = MethodInfo.Unroll) ||
                    (mi.IsGenericMethod && mi.GetGenericMethodDefinition() = MethodInfo.NewRef) ||
                    (s.isGlobalSideEffect (mi :> MemberInfo))
                )

            let needsField (f : FieldInfo) : State<State, bool> =
                State.get |> State.map (fun s ->
                    s.isGlobalSideEffect (f :> MemberInfo)
                )

            let needsProperty (p : PropertyInfo) : State<State, bool> =
                State.get |> State.map (fun s ->
                    s.isGlobalSideEffect (p :> MemberInfo)
                )

            let setVar (v : Var) (value : obj) =
                State.modify (fun s ->
                    { s with variableValues = Map.add v value s.variableValues }
                )

            let getVar (v : Var) =
                State.get |> State.map (fun s ->
                    Map.tryFind v s.variableValues
                )

        [<return: Struct>]
        let rec private (|AllConstant|_|) (e : list<Expr>) =
            match e with
            | [] -> ValueSome []
            | h :: t ->
                match h, t with
                | Value(h,_), AllConstant t -> ValueSome (h :: t)
                | _ -> ValueNone


        let functionTable (l : list<string * obj>) =
            let table = Dictionary.empty

            let rec getFunctionElements (t : Type) =
                if t.Name.StartsWith "FSharpFunc" || t.BaseType.Name.StartsWith "FSharpFunc" then
                    let a, r = FSharpType.GetFunctionElements t
                    let args, ret = getFunctionElements r
                    a :: args, ret
                else
                    [], t

            for (name, f) in l do
                let fType = f.GetType()
                let invoke : MethodInfo = 
                    fType.GetMethods(BindingFlags.Public ||| BindingFlags.Instance) 
                        |> Array.filter (fun mi -> mi.Name = "Invoke") 
                        |> Array.maxBy (fun mi -> mi.GetParameters().Length)

                let parameters =
                    invoke.GetParameters() |> Array.map (fun p -> p.ParameterType) |> Array.toList

                let run (args : list<obj>) = invoke.Invoke(f, List.toArray args)

                table.[(name, parameters)] <- run

            table

        let private operators =
            functionTable [
                "op_UnaryNegation", ((~-) : int8 -> int8) :> obj
                "op_UnaryNegation", ((~-) : int16 -> int16) :> obj
                "op_UnaryNegation", ((~-) : int32 -> int32) :> obj
                "op_UnaryNegation", ((~-) : int64 -> int64) :> obj
                "op_UnaryNegation", ((~-) : float32 -> float32) :> obj
                "op_UnaryNegation", ((~-) : float -> float) :> obj
                "op_UnaryNegation", ((~-) : decimal -> decimal) :> obj

                "op_Addition", ((+) : int8 -> int8 -> int8) :> obj
                "op_Addition", ((+) : int16 -> int16 -> int16) :> obj
                "op_Addition", ((+) : int32 -> int32 -> int32) :> obj
                "op_Addition", ((+) : int64 -> int64 -> int64) :> obj
                "op_Addition", ((+) : uint8 -> uint8 -> uint8) :> obj
                "op_Addition", ((+) : uint16 -> uint16 -> uint16) :> obj
                "op_Addition", ((+) : uint32 -> uint32 -> uint32) :> obj
                "op_Addition", ((+) : uint64 -> uint64 -> uint64) :> obj
                "op_Addition", ((+) : float32 -> float32 -> float32) :> obj
                "op_Addition", ((+) : float -> float -> float) :> obj
                "op_Addition", ((+) : decimal -> decimal -> decimal) :> obj

                
                "op_Subtraction", ((-) : int8 -> int8 -> int8) :> obj
                "op_Subtraction", ((-) : int16 -> int16 -> int16) :> obj
                "op_Subtraction", ((-) : int32 -> int32 -> int32) :> obj
                "op_Subtraction", ((-) : int64 -> int64 -> int64) :> obj
                "op_Subtraction", ((-) : uint8 -> uint8 -> uint8) :> obj
                "op_Subtraction", ((-) : uint16 -> uint16 -> uint16) :> obj
                "op_Subtraction", ((-) : uint32 -> uint32 -> uint32) :> obj
                "op_Subtraction", ((-) : uint64 -> uint64 -> uint64) :> obj
                "op_Subtraction", ((-) : float32 -> float32 -> float32) :> obj
                "op_Subtraction", ((-) : float -> float -> float) :> obj
                "op_Subtraction", ((-) : decimal -> decimal -> decimal) :> obj

                
                "op_Multiply", ((*) : int8 -> int8 -> int8) :> obj
                "op_Multiply", ((*) : int16 -> int16 -> int16) :> obj
                "op_Multiply", ((*) : int32 -> int32 -> int32) :> obj
                "op_Multiply", ((*) : int64 -> int64 -> int64) :> obj
                "op_Multiply", ((*) : uint8 -> uint8 -> uint8) :> obj
                "op_Multiply", ((*) : uint16 -> uint16 -> uint16) :> obj
                "op_Multiply", ((*) : uint32 -> uint32 -> uint32) :> obj
                "op_Multiply", ((*) : uint64 -> uint64 -> uint64) :> obj
                "op_Multiply", ((*) : float32 -> float32 -> float32) :> obj
                "op_Multiply", ((*) : float -> float -> float) :> obj
                "op_Multiply", ((*) : decimal -> decimal -> decimal) :> obj

                
                "op_Division", ((/) : int8 -> int8 -> int8) :> obj
                "op_Division", ((/) : int16 -> int16 -> int16) :> obj
                "op_Division", ((/) : int32 -> int32 -> int32) :> obj
                "op_Division", ((/) : int64 -> int64 -> int64) :> obj
                "op_Division", ((/) : uint8 -> uint8 -> uint8) :> obj
                "op_Division", ((/) : uint16 -> uint16 -> uint16) :> obj
                "op_Division", ((/) : uint32 -> uint32 -> uint32) :> obj
                "op_Division", ((/) : uint64 -> uint64 -> uint64) :> obj
                "op_Division", ((/) : float32 -> float32 -> float32) :> obj
                "op_Division", ((/) : float -> float -> float) :> obj
                "op_Division", ((/) : decimal -> decimal -> decimal) :> obj

                
                "op_Modulus", ((%) : int8 -> int8 -> int8) :> obj
                "op_Modulus", ((%) : int16 -> int16 -> int16) :> obj
                "op_Modulus", ((%) : int32 -> int32 -> int32) :> obj
                "op_Modulus", ((%) : int64 -> int64 -> int64) :> obj
                "op_Modulus", ((%) : uint8 -> uint8 -> uint8) :> obj
                "op_Modulus", ((%) : uint16 -> uint16 -> uint16) :> obj
                "op_Modulus", ((%) : uint32 -> uint32 -> uint32) :> obj
                "op_Modulus", ((%) : uint64 -> uint64 -> uint64) :> obj
                "op_Modulus", ((%) : float32 -> float32 -> float32) :> obj
                "op_Modulus", ((%) : float -> float -> float) :> obj
                "op_Modulus", ((%) : decimal -> decimal -> decimal) :> obj

                "op_Exponentiation", (( ** ) : float -> float -> float) :> obj
                "op_Exponentiation", (( ** ) : float32 -> float32 -> float32) :> obj
                "op_Exponentiation", (( ** ) : V2d -> float -> V2d) :> obj
                "op_Exponentiation", (( ** ) : V3d -> float -> V3d) :> obj
                "op_Exponentiation", (( ** ) : V4d -> float -> V4d) :> obj
                "op_Exponentiation", (( ** ) : V2f -> float32 -> V2f) :> obj
                "op_Exponentiation", (( ** ) : V3f -> float32 -> V3f) :> obj
                "op_Exponentiation", (( ** ) : V4f -> float32 -> V4f) :> obj
                "op_Exponentiation", (( ** ) : V2d -> V2d -> V2d) :> obj
                "op_Exponentiation", (( ** ) : V3d -> V3d -> V3d) :> obj
                "op_Exponentiation", (( ** ) : V4d -> V4d -> V4d) :> obj
                "op_Exponentiation", (( ** ) : V2f -> V2f -> V2f) :> obj
                "op_Exponentiation", (( ** ) : V3f -> V3f -> V3f) :> obj
                "op_Exponentiation", (( ** ) : V4f -> V4f -> V4f) :> obj

                
                "op_BitwiseOr", ((|||) : int8 -> int8 -> int8) :> obj
                "op_BitwiseOr", ((|||) : int16 -> int16 -> int16) :> obj
                "op_BitwiseOr", ((|||) : int32 -> int32 -> int32) :> obj
                "op_BitwiseOr", ((|||) : int64 -> int64 -> int64) :> obj
                "op_BitwiseOr", ((|||) : uint8 -> uint8 -> uint8) :> obj
                "op_BitwiseOr", ((|||) : uint16 -> uint16 -> uint16) :> obj
                "op_BitwiseOr", ((|||) : uint32 -> uint32 -> uint32) :> obj
                "op_BitwiseOr", ((|||) : uint64 -> uint64 -> uint64) :> obj

                
                "op_BitwiseAnd", ((&&&) : int8 -> int8 -> int8) :> obj
                "op_BitwiseAnd", ((&&&) : int16 -> int16 -> int16) :> obj
                "op_BitwiseAnd", ((&&&) : int32 -> int32 -> int32) :> obj
                "op_BitwiseAnd", ((&&&) : int64 -> int64 -> int64) :> obj
                "op_BitwiseAnd", ((&&&) : uint8 -> uint8 -> uint8) :> obj
                "op_BitwiseAnd", ((&&&) : uint16 -> uint16 -> uint16) :> obj
                "op_BitwiseAnd", ((&&&) : uint32 -> uint32 -> uint32) :> obj
                "op_BitwiseAnd", ((&&&) : uint64 -> uint64 -> uint64) :> obj

                
                "op_ExclusiveOr", ((^^^) : int8 -> int8 -> int8) :> obj
                "op_ExclusiveOr", ((^^^) : int16 -> int16 -> int16) :> obj
                "op_ExclusiveOr", ((^^^) : int32 -> int32 -> int32) :> obj
                "op_ExclusiveOr", ((^^^) : int64 -> int64 -> int64) :> obj
                "op_ExclusiveOr", ((^^^) : uint8 -> uint8 -> uint8) :> obj
                "op_ExclusiveOr", ((^^^) : uint16 -> uint16 -> uint16) :> obj
                "op_ExclusiveOr", ((^^^) : uint32 -> uint32 -> uint32) :> obj
                "op_ExclusiveOr", ((^^^) : uint64 -> uint64 -> uint64) :> obj

                "op_LeftShift", ((<<<) : int8 -> int -> int8) :> obj
                "op_LeftShift", ((<<<) : int16 -> int -> int16) :> obj
                "op_LeftShift", ((<<<) : int32 -> int -> int32) :> obj
                "op_LeftShift", ((<<<) : int64 -> int -> int64) :> obj
                "op_LeftShift", ((<<<) : uint8 -> int -> uint8) :> obj
                "op_LeftShift", ((<<<) : uint16 -> int -> uint16) :> obj
                "op_LeftShift", ((<<<) : uint32 -> int -> uint32) :> obj
                "op_LeftShift", ((<<<) : uint64 -> int -> uint64) :> obj

                "op_RightShift", ((>>>) : int8 -> int -> int8) :> obj
                "op_RightShift", ((>>>) : int16 -> int -> int16) :> obj
                "op_RightShift", ((>>>) : int32 -> int -> int32) :> obj
                "op_RightShift", ((>>>) : int64 -> int -> int64) :> obj
                "op_RightShift", ((>>>) : uint8 -> int -> uint8) :> obj
                "op_RightShift", ((>>>) : uint16 -> int -> uint16) :> obj
                "op_RightShift", ((>>>) : uint32 -> int -> uint32) :> obj
                "op_RightShift", ((>>>) : uint64 -> int -> uint64) :> obj

                
                "op_LessThan", ((<) : int8 -> int8 -> bool) :> obj
                "op_LessThan", ((<) : int16 -> int16 -> bool) :> obj
                "op_LessThan", ((<) : int32 -> int32 -> bool) :> obj
                "op_LessThan", ((<) : int64 -> int64 -> bool) :> obj
                "op_LessThan", ((<) : uint8 -> uint8 -> bool) :> obj
                "op_LessThan", ((<) : uint16 -> uint16 -> bool) :> obj
                "op_LessThan", ((<) : uint32 -> uint32 -> bool) :> obj
                "op_LessThan", ((<) : uint64 -> uint64 -> bool) :> obj
                "op_LessThan", ((<) : float32 -> float32 -> bool) :> obj
                "op_LessThan", ((<) : float -> float -> bool) :> obj
                "op_LessThan", ((<) : decimal -> decimal -> bool) :> obj

                
                "op_LessThanOrEqual", ((<=) : int8 -> int8 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : int16 -> int16 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : int32 -> int32 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : int64 -> int64 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : uint8 -> uint8 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : uint16 -> uint16 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : uint32 -> uint32 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : uint64 -> uint64 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : float32 -> float32 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : float -> float -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : decimal -> decimal -> bool) :> obj

                
                "op_GreaterThan", ((>) : int8 -> int8 -> bool) :> obj
                "op_GreaterThan", ((>) : int16 -> int16 -> bool) :> obj
                "op_GreaterThan", ((>) : int32 -> int32 -> bool) :> obj
                "op_GreaterThan", ((>) : int64 -> int64 -> bool) :> obj
                "op_GreaterThan", ((>) : uint8 -> uint8 -> bool) :> obj
                "op_GreaterThan", ((>) : uint16 -> uint16 -> bool) :> obj
                "op_GreaterThan", ((>) : uint32 -> uint32 -> bool) :> obj
                "op_GreaterThan", ((>) : uint64 -> uint64 -> bool) :> obj
                "op_GreaterThan", ((>) : float32 -> float32 -> bool) :> obj
                "op_GreaterThan", ((>) : float -> float -> bool) :> obj
                "op_GreaterThan", ((>) : decimal -> decimal -> bool) :> obj

                
                "op_GreaterThanOrEqual", ((>=) : int8 -> int8 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : int16 -> int16 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : int32 -> int32 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : int64 -> int64 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : uint8 -> uint8 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : uint16 -> uint16 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : uint32 -> uint32 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : uint64 -> uint64 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : float32 -> float32 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : float -> float -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : decimal -> decimal -> bool) :> obj

                
                "op_Equality", ((=) : int8 -> int8 -> bool) :> obj
                "op_Equality", ((=) : int16 -> int16 -> bool) :> obj
                "op_Equality", ((=) : int32 -> int32 -> bool) :> obj
                "op_Equality", ((=) : int64 -> int64 -> bool) :> obj
                "op_Equality", ((=) : uint8 -> uint8 -> bool) :> obj
                "op_Equality", ((=) : uint16 -> uint16 -> bool) :> obj
                "op_Equality", ((=) : uint32 -> uint32 -> bool) :> obj
                "op_Equality", ((=) : uint64 -> uint64 -> bool) :> obj
                "op_Equality", ((=) : float32 -> float32 -> bool) :> obj
                "op_Equality", ((=) : float -> float -> bool) :> obj
                "op_Equality", ((=) : decimal -> decimal -> bool) :> obj

                
                "op_Inequality", ((<>) : int8 -> int8 -> bool) :> obj
                "op_Inequality", ((<>) : int16 -> int16 -> bool) :> obj
                "op_Inequality", ((<>) : int32 -> int32 -> bool) :> obj
                "op_Inequality", ((<>) : int64 -> int64 -> bool) :> obj
                "op_Inequality", ((<>) : uint8 -> uint8 -> bool) :> obj
                "op_Inequality", ((<>) : uint16 -> uint16 -> bool) :> obj
                "op_Inequality", ((<>) : uint32 -> uint32 -> bool) :> obj
                "op_Inequality", ((<>) : uint64 -> uint64 -> bool) :> obj
                "op_Inequality", ((<>) : float32 -> float32 -> bool) :> obj
                "op_Inequality", ((<>) : float -> float -> bool) :> obj
                "op_Inequality", ((<>) : decimal -> decimal -> bool) :> obj

                
                "ToSByte", (int8 : int8 -> _) :> obj
                "ToSByte", (int8 : int16 -> _) :> obj
                "ToSByte", (int8 : int32 -> _) :> obj
                "ToSByte", (int8 : int64 -> _) :> obj
                "ToSByte", (int8 : uint8 -> _) :> obj
                "ToSByte", (int8 : uint16 -> _) :> obj
                "ToSByte", (int8 : uint32 -> _) :> obj
                "ToSByte", (int8 : uint64 -> _) :> obj
                "ToSByte", (int8 : float32 -> _) :> obj
                "ToSByte", (int8 : float -> _) :> obj
                "ToSByte", (int8 : decimal -> _) :> obj
                
                "ToByte", (uint8 : int8 -> _) :> obj
                "ToByte", (uint8 : int16 -> _) :> obj
                "ToByte", (uint8 : int32 -> _) :> obj
                "ToByte", (uint8 : int64 -> _) :> obj
                "ToByte", (uint8 : uint8 -> _) :> obj
                "ToByte", (uint8 : uint16 -> _) :> obj
                "ToByte", (uint8 : uint32 -> _) :> obj
                "ToByte", (uint8 : uint64 -> _) :> obj
                "ToByte", (uint8 : float32 -> _) :> obj
                "ToByte", (uint8 : float -> _) :> obj
                "ToByte", (uint8 : decimal -> _) :> obj
                
                "ToInt16", (int16 : int8 -> _) :> obj
                "ToInt16", (int16 : int16 -> _) :> obj
                "ToInt16", (int16 : int32 -> _) :> obj
                "ToInt16", (int16 : int64 -> _) :> obj
                "ToInt16", (int16 : uint8 -> _) :> obj
                "ToInt16", (int16 : uint16 -> _) :> obj
                "ToInt16", (int16 : uint32 -> _) :> obj
                "ToInt16", (int16 : uint64 -> _) :> obj
                "ToInt16", (int16 : float32 -> _) :> obj
                "ToInt16", (int16 : float -> _) :> obj
                "ToInt16", (int16 : decimal -> _) :> obj
                
                "ToUInt16", (uint16 : int8 -> _) :> obj
                "ToUInt16", (uint16 : int16 -> _) :> obj
                "ToUInt16", (uint16 : int32 -> _) :> obj
                "ToUInt16", (uint16 : int64 -> _) :> obj
                "ToUInt16", (uint16 : uint8 -> _) :> obj
                "ToUInt16", (uint16 : uint16 -> _) :> obj
                "ToUInt16", (uint16 : uint32 -> _) :> obj
                "ToUInt16", (uint16 : uint64 -> _) :> obj
                "ToUInt16", (uint16 : float32 -> _) :> obj
                "ToUInt16", (uint16 : float -> _) :> obj
                "ToUInt16", (uint16 : decimal -> _) :> obj
                
                
                "ToInt", (int : int8 -> _) :> obj
                "ToInt", (int : int16 -> _) :> obj
                "ToInt", (int : int32 -> _) :> obj
                "ToInt", (int : int64 -> _) :> obj
                "ToInt", (int : uint8 -> _) :> obj
                "ToInt", (int : uint16 -> _) :> obj
                "ToInt", (int : uint32 -> _) :> obj
                "ToInt", (int : uint64 -> _) :> obj
                "ToInt", (int : float32 -> _) :> obj
                "ToInt", (int : float -> _) :> obj
                "ToInt", (int : decimal -> _) :> obj
                
                "ToUInt32", (uint32 : int8 -> _) :> obj
                "ToUInt32", (uint32 : int16 -> _) :> obj
                "ToUInt32", (uint32 : int32 -> _) :> obj
                "ToUInt32", (uint32 : int64 -> _) :> obj
                "ToUInt32", (uint32 : uint8 -> _) :> obj
                "ToUInt32", (uint32 : uint16 -> _) :> obj
                "ToUInt32", (uint32 : uint32 -> _) :> obj
                "ToUInt32", (uint32 : uint64 -> _) :> obj
                "ToUInt32", (uint32 : float32 -> _) :> obj
                "ToUInt32", (uint32 : float -> _) :> obj
                "ToUInt32", (uint32 : decimal -> _) :> obj
                
                "ToInt64", (int64 : int8 -> _) :> obj
                "ToInt64", (int64 : int16 -> _) :> obj
                "ToInt64", (int64 : int32 -> _) :> obj
                "ToInt64", (int64 : int64 -> _) :> obj
                "ToInt64", (int64 : uint8 -> _) :> obj
                "ToInt64", (int64 : uint16 -> _) :> obj
                "ToInt64", (int64 : uint32 -> _) :> obj
                "ToInt64", (int64 : uint64 -> _) :> obj
                "ToInt64", (int64 : float32 -> _) :> obj
                "ToInt64", (int64 : float -> _) :> obj
                "ToInt64", (int64 : decimal -> _) :> obj
                
                "ToUInt64", (uint64 : int8 -> _) :> obj
                "ToUInt64", (uint64 : int16 -> _) :> obj
                "ToUInt64", (uint64 : int32 -> _) :> obj
                "ToUInt64", (uint64 : int64 -> _) :> obj
                "ToUInt64", (uint64 : uint8 -> _) :> obj
                "ToUInt64", (uint64 : uint16 -> _) :> obj
                "ToUInt64", (uint64 : uint32 -> _) :> obj
                "ToUInt64", (uint64 : uint64 -> _) :> obj
                "ToUInt64", (uint64 : float32 -> _) :> obj
                "ToUInt64", (uint64 : float -> _) :> obj
                "ToUInt64", (uint64 : decimal -> _) :> obj
                
                "ToSingle", (float32 : int8 -> _) :> obj
                "ToSingle", (float32 : int16 -> _) :> obj
                "ToSingle", (float32 : int32 -> _) :> obj
                "ToSingle", (float32 : int64 -> _) :> obj
                "ToSingle", (float32 : uint8 -> _) :> obj
                "ToSingle", (float32 : uint16 -> _) :> obj
                "ToSingle", (float32 : uint32 -> _) :> obj
                "ToSingle", (float32 : uint64 -> _) :> obj
                "ToSingle", (float32 : float32 -> _) :> obj
                "ToSingle", (float32 : float -> _) :> obj
                "ToSingle", (float32 : decimal -> _) :> obj
                
                "ToDouble", (float : int8 -> _) :> obj
                "ToDouble", (float : int16 -> _) :> obj
                "ToDouble", (float : int32 -> _) :> obj
                "ToDouble", (float : int64 -> _) :> obj
                "ToDouble", (float : uint8 -> _) :> obj
                "ToDouble", (float : uint16 -> _) :> obj
                "ToDouble", (float : uint32 -> _) :> obj
                "ToDouble", (float : uint64 -> _) :> obj
                "ToDouble", (float : float32 -> _) :> obj
                "ToDouble", (float : float -> _) :> obj
                "ToDouble", (float : decimal -> _) :> obj
            ]

        [<return: Struct>]
        let private (|Operator|_|) (mi : MethodInfo) =
            let parameters =
                mi.GetParameters()
                |> Array.toList
                |> List.map (fun p -> p.ParameterType)

            match operators.TryGetValue((mi.Name, parameters)) with
            | (true, f) -> ValueSome f
            | _ -> ValueNone

        [<return: Struct>]
        let rec (|SeqCons|_|) (e : Expr) =
            match e with
            | Unit ->
                ValueNone

            | Sequential(SeqCons(head, tail), r) ->
                let tail = 
                    match tail with
                    | ValueSome t -> Expr.Sequential(t, r) |> ValueSome
                    | ValueNone -> ValueSome r
                ValueSome (head, tail)

            | e ->
                ValueSome (e, ValueNone)
  
        [<return: Struct>]
        let rec (|SeqCons2|_|) (e : Expr) =
            match e with
            | SeqCons(a, ValueSome (SeqCons(b, c))) ->
                ValueSome (a,b,c)
            | _ ->
                ValueNone

        let rec evaluateConstantsS (e : Expr) =
            state {
                match e with
                    | WriteOutputs values ->
                        let! values = 
                            values |> Map.mapS (fun _ (i, v) -> 
                                state {
                                    let! v = 
                                        match v with
                                            | Coerce(v, t) when t = typeof<obj> -> evaluateConstantsS v |> State.map (fun v -> Expr.Coerce(v, t))
                                            | v -> evaluateConstantsS v

                                    let! i = i |> Option.mapS evaluateConstantsS
                                    return i, v
                                }
                            )
                        return Expr.WriteOutputs(values)

                    | AddressOf e ->
                        let! e = evaluateConstantsS e
                        return Expr.AddressOf(e)

                    | AddressSet(v, e) ->
                        let! v = evaluateConstantsS v
                        let! e = evaluateConstantsS e
                        return Expr.AddressSet(v, e)

                    | Application(lambda, arg) ->
                        let! lambda = evaluateConstantsS lambda
                        let! arg = evaluateConstantsS arg
                        match lambda, arg with
                            | Value(lambda, lt), Value(arg, at) ->
                                let mi = lt.GetMethod("Invoke", [|at|])
                                return Expr.Value(mi.Invoke(lambda, [|arg|]), e.Type)
                            | _ ->
                                return Expr.Application(lambda, arg)

                    | SeqCons2((Unroll as u), ForInteger(v, first, step, last, body), rest) ->
                        let! first = evaluateConstantsS first
                        let! step = evaluateConstantsS step
                        let! last = evaluateConstantsS last
                        
                        match first, step, last with
                            | Int32 f, Int32 s, Int32 l ->
                                let! unrolled = 
                                    [f .. s .. l] |> List.mapS (fun i -> 
                                        let value = Expr.Value(i)
                                        let body = body.Substitute(fun vi -> if vi = v then Some value else None)
                                        evaluateConstantsS body
                                    )
                                match rest with
                                | ValueSome rest -> 
                                    let! rest = evaluateConstantsS rest
                                    return Expr.Seq [Expr.Seq unrolled; rest]
                                | ValueNone ->
                                    return Expr.Seq unrolled
                            | _ ->
                                let! body = evaluateConstantsS body
                                match rest with
                                | ValueSome rest -> 
                                    let! rest = evaluateConstantsS rest
                                    match body with
                                        | Value _ -> return rest
                                        | _ -> 
                                            return Expr.Seq [u; Expr.ForInteger(v, first, step, last, body); rest]
                                | ValueNone ->
                                    return Expr.Seq [u; Expr.ForInteger(v, first, step, last, body)]


                        

                    | ForInteger(v, first, step, last, body) ->
                        let! first = evaluateConstantsS first
                        let! step = evaluateConstantsS step
                        let! last = evaluateConstantsS last
                        let! body = evaluateConstantsS body

                        match first, step, last with
                            | Int32 f, Int32 s, Int32 l ->
                                if s > 0 && l < f then return Expr.Unit
                                elif s < 0 && l > f then return Expr.Unit
                                else return Expr.ForInteger(v, first, step, last, body)
                            | _ ->

                                match body with
                                    | Value _ -> return Expr.Unit
                                    | _ -> return Expr.ForInteger(v, first, step, last, body)
                    
                    | ForEach(v, s, b) ->
                        let! s = evaluateConstantsS s
                        let! b = evaluateConstantsS b
                        match b with
                            | Unit -> return Expr.Unit
                            | _ -> return Expr.ForEach(v, s, b)

                    | CallFunction(utility, args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        let! s = State.get
                        let utility =
                            utility |> UtilityFunction.map (fun e ->
                                let mutable innerState = { State.empty with isGlobalSideEffect = s.isGlobalSideEffect }
                                evaluateConstantsS(e).Run(&innerState)
                            )

                        return Expr.CallFunction(utility, args)
                        

                    | Call(None, mi, [v]) when mi.Name = "op_Splice" || mi.Name = "op_SpliceUntyped" ->
                        let! v = evaluateConstantsS v
                        match v with
                            | ExprValue ve -> 
                                let! ve = evaluateConstantsS ve
                                return ve
                            | _ -> 
                                return Expr.Call(mi, [v])

                    | Call(None, (EnumBitwiseOp (enumType, baseType, op) as mi), [l; r]) ->
                        let! l = evaluateConstantsS l
                        let! r = evaluateConstantsS r

                        match l, r with
                        | Value(l, _), Value(r, _) ->
                            let x = Convert.ChangeType(l, baseType)
                            let y = Convert.ChangeType(r, baseType)
                            let z = Enum.ToObject(enumType, op x y)
                            return Expr.Value(z, enumType)
                        | _ ->
                            return Expr.Call(mi, [l; r])

                    | Call(None, (EnumShiftOp (enumType, baseType, op) as mi), [l; r]) ->
                        let! l = evaluateConstantsS l
                        let! r = evaluateConstantsS r

                        match l, r with
                        | Value(l, _), Value(:? int32 as shift, _) ->
                            let x = Convert.ChangeType(l, baseType)
                            let y = Enum.ToObject(enumType, op x shift)
                            return Expr.Value(y, enumType)
                        | _ ->
                            return Expr.Call(mi, [l; r])

                    | Call(None, (EnumConversion (fromInt, intType) as mi), [x]) ->
                        let! x = evaluateConstantsS x

                        match x with
                        | Value(x, _) ->
                            let xi = Convert.ChangeType(x, intType)
                            return Expr.Value(fromInt xi, mi.ReturnType)
                        | _ ->
                            return Expr.Call(mi, [x])

                    | CallWithWitnesses(None, original, meth, witnesses, args) ->
                        let! needed = State.needsCall original
                        let! args = args |> List.mapS evaluateConstantsS
                        if needed then
                            return Expr.CallWithWitnesses(original, meth, witnesses, args)
                        else
                            match args with
                            | AllConstant values ->
                                try
                                    let ws = witnesses |> List.map Expr.CompileWitness
                                    let result = meth.TryInvoke(null, List.toArray (ws @ values))

                                    match result with
                                    | Some value ->
                                        return Expr.Value(value, e.Type)
                                    | _ ->
                                        return Expr.CallWithWitnesses(original, meth, witnesses, args)
                                with _ ->
                                    return Expr.CallWithWitnesses(original, meth, witnesses, args)
                            | _ ->
                                return Expr.CallWithWitnesses(original, meth, witnesses, args)

                    | CallWithWitnesses(Some target, original, meth, witnesses, args) ->
                        let! needed = State.needsCall original
                        let! target = evaluateConstantsS target
                        let! args = args |> List.mapS evaluateConstantsS
                        if needed then
                            return Expr.CallWithWitnesses(target, original, meth, witnesses, args)
                        else
                            match target with
                            | Value (targetValue, _) ->
                                match args with
                                | AllConstant values ->
                                    try
                                        let ws = witnesses |> List.map Expr.CompileWitness
                                        let result = meth.TryInvoke(targetValue, List.toArray (ws @ values))

                                        match result with
                                        | Some value ->
                                            return Expr.Value(value, e.Type)
                                        | _ ->
                                            return Expr.CallWithWitnesses(target, original, meth, witnesses, args)
                                    with _ ->
                                        return Expr.CallWithWitnesses(target, original, meth, witnesses, args)
                                | _ ->
                                    return Expr.CallWithWitnesses(target, original, meth, witnesses, args)
                            | _ ->
                                return Expr.CallWithWitnesses(target, original, meth, witnesses, args)

                    | Call(None, mi, args) ->
                        let! needed = State.needsCall mi
                        let! args = args |> List.mapS evaluateConstantsS

                        if needed then
                            return Expr.Call(mi, args)
                        else
                            match args with
                            | AllConstant values ->
                                match mi with
                                | Operator f ->
                                    return Expr.Value(f values, e.Type)

                                | _ ->
                                    let value =
                                        try mi.TryInvoke(null, values |> List.toArray)
                                        with
                                        | _ ->
                                            Log.warn "[FShade] could not evaluate: %A" mi
                                            None

                                    match value with
                                    | Some v -> return Expr.Value(v, e.Type)
                                    | None -> return Expr.Call(mi, args)
                            | _ ->
                                return Expr.Call(mi, args)

                    | Call(Some t, mi, args) ->
                        let! needed = State.needsCall mi
                        let! t = evaluateConstantsS t
                        let! args = args |> List.mapS evaluateConstantsS

                        if needed then
                            return Expr.Call(t, mi, args)
                        else
                            match t, args with
                            | Value(tv,_), AllConstant values ->
                                let value =
                                    try mi.TryInvoke(tv, values |> List.toArray)
                                    with
                                    | _ ->
                                        Log.warn "[FShade] could not evaluate: %A" mi
                                        None

                                match value with
                                | Some v -> return Expr.Value(v, e.Type)
                                | None -> return Expr.Call(t, mi, args)
                            | _ ->
                                return Expr.Call(t, mi, args)

                    | Coerce(e, t) ->
                        let! e = evaluateConstantsS e
                        match e with
                            | Value(e,_) -> return Expr.Value(e, t)
                            | _ -> return Expr.Coerce(e, t)

                    | DefaultValue t ->
                        return Expr.DefaultValue t

                    | FieldGet(None, f) ->
                        let! n = State.needsField f
                        if n then return e
                        else return Expr.Value(f.GetValue null, e.Type)

                    | FieldGet(Some t, f) ->
                        let! t = evaluateConstantsS t
                        let! n = State.needsField f
                        match t with
                            | Value(t,_) when not n && not (isNull t) ->  return Expr.Value(f.GetValue t, e.Type)
                            | _ -> return Expr.FieldGet(t, f)

                    | FieldSet(None, f, value) ->
                        let! value = evaluateConstantsS value
                        return Expr.FieldSet(f, value)

                    | FieldSet(Some t, f, value) ->
                        let! t = evaluateConstantsS t
                        let! value = evaluateConstantsS value
                        return Expr.FieldSet(t, f, value)

                    | IfThenElse(cond, i, e) ->
                        let! cond = evaluateConstantsS cond
                        match cond with
                            | Bool true -> 
                                return! evaluateConstantsS i

                            | Bool false -> 
                                return! evaluateConstantsS e

                            | _ ->
                                let! i = evaluateConstantsS i
                                let! e = evaluateConstantsS e
                                return Expr.IfThenElse(cond, i, e)

                    | Lambda(v, b) ->
                        let! b = evaluateConstantsS b
                        return Expr.Lambda(v, b)

                    | Let(v, e, b) ->
                        let! e = evaluateConstantsS e
                        match e with
                            | Value(value,_) when not v.IsMutable ->
                                do! State.setVar v value
                                return! evaluateConstantsS b
                            | _ ->
                                let! b = evaluateConstantsS b
                                return Expr.Let(v, e, b)

                    | NewArray(t, args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        match args with
                            | AllConstant args ->
                                let value = 
                                    let arr = Array.CreateInstance(t, List.length args)
                                    let mutable i = 0
                                    for a in args do
                                        arr.SetValue(a, i)
                                        i <- i + 1
                                    arr

                                return Expr.Value(value, e.Type)
                            | _ ->
                                return Expr.NewArray(t, args)

                    | NewDelegate(t, vars, body) ->
                        let! body = evaluateConstantsS body
                        return Expr.NewDelegate(t, vars, body)

                    | NewObject(ctor, args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        match args with
                            | AllConstant args ->
                                return Expr.Value(ctor.Invoke(List.toArray args), e.Type)
                            | _ ->
                                return Expr.NewObject(ctor, args)
                        
                    | NewRecord(t, args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        match args with
                            | AllConstant args ->
                                return Expr.Value(FSharpValue.MakeRecord(t, List.toArray args, true), e.Type)
                            | _ ->
                                return Expr.NewRecord(t, args)

                    | NewTuple(args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        match args with
                            | AllConstant args ->
                                return Expr.Value(FSharpValue.MakeTuple(List.toArray args, e.Type), e.Type)
                            | _ ->
                                return Expr.NewTuple(args)
                        
                    | NewUnionCase(ci, args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        match args with
                            | AllConstant args ->
                                return Expr.Value(FSharpValue.MakeUnion(ci, List.toArray args, true), e.Type)
                            | _ ->
                                return Expr.NewUnionCase(ci, args)
                        
                    | PropertyGet(None, pi, indices) ->
                        let! indices = indices |> List.mapS evaluateConstantsS
                        let! n = State.needsProperty pi
                        match indices with
                            | AllConstant indexValues when not n ->
                                let value = 
                                    try pi.TryGetValue(null, List.toArray indexValues)
                                    with
                                    | _ ->
                                        Log.warn "[FShade] could not evaluate: %A" pi
                                        None
                                match value with
                                    | Some value -> return Expr.Value(value, e.Type)
                                    | None -> return Expr.PropertyGet(pi, indices)

                            | _ ->
                                return Expr.PropertyGet(pi, indices)

                    | PropertyGet(Some t, pi, indices) ->
                        let! t = evaluateConstantsS t
                        let! indices = indices |> List.mapS evaluateConstantsS
                        let! n = State.needsProperty pi
                        match t, indices with
                            | Value(tv,_), AllConstant indexValues when not n && not (isNull tv)->
                                let value =
                                    try pi.TryGetValue(tv, List.toArray indexValues)
                                    with
                                    | _ ->
                                        Log.warn "[FShade] could not evaluate: %A" pi
                                        None

                                match value with
                                    | Some value -> return Expr.Value(value, e.Type)
                                    | None -> return Expr.PropertyGet(t, pi, indices)
                            | _ ->
                                return Expr.PropertyGet(t, pi, indices)

                    | PropertySet(None, pi, indices, value) ->
                        let! indices = indices |> List.mapS evaluateConstantsS
                        let! value = evaluateConstantsS value
                        return Expr.PropertySet(pi, value, indices)

                    | PropertySet(Some t, pi, indices, value) ->
                        let! t = evaluateConstantsS t
                        let! indices = indices |> List.mapS evaluateConstantsS
                        let! value = evaluateConstantsS value
                        return Expr.PropertySet(t, pi, value, indices)

                    | UnsafeWrite(t, value) ->
                        let! t = evaluateConstantsS t
                        let! value = evaluateConstantsS value
                        return Expr.UnsafeWrite(t, value)
                        
                    | QuoteRaw e ->
                        //let! e = evaluateConstantsS e
                        return Expr.Value(e)
                        
                    | QuoteTyped te ->
                        let! te = evaluateConstantsS te
                        let mi = typeof<Expr>.GetMethod("Cast")
                        let mi = mi.MakeGenericMethod [| te.Type |]
                        let v = mi.Invoke(null, [|te :> obj|])
                        
                        return Expr.Value(v, e.Type)

                    | Sequential(l, r) ->
                        let! l = evaluateConstantsS l
                        let! r = evaluateConstantsS r
                        match l with
                            | Unit -> return r
                            | _ -> return Expr.Sequential(l, r)

                    | TryFinally(t,f) ->
                        let! t = evaluateConstantsS t
                        let! f = evaluateConstantsS f
                        return Expr.TryFinally(t, f)

                    | TryWith(a, b, c, d, e) ->
                        let! a = evaluateConstantsS a
                        let! c = evaluateConstantsS c
                        let! e = evaluateConstantsS e
                        return Expr.TryWith(a, b, c, d, e)

                    | TupleGet(t, i) ->
                        let! t = evaluateConstantsS t
                        match t with
                            | Value(t,_) -> return Expr.Value(FSharpValue.GetTupleField(t, i), e.Type)
                            | _ -> return Expr.TupleGet(t, i)

                    | TypeTest(v, t) ->
                        let! v = evaluateConstantsS v
                        match v with
                            | Value(v,_) ->
                                return Expr.Value(t.IsAssignableFrom(v.GetType()))
                            | _ ->
                                return Expr.TypeTest(v, t)

                    | UnionCaseTest(e, ci) ->
                        let! e = evaluateConstantsS e
                        // TODO: could be avoided
                        return Expr.UnionCaseTest(e, ci)

                    | Value _ ->
                        return e

                    | VarSet (v, e) ->
                        let! e = evaluateConstantsS e
                        return Expr.VarSet (v, e)

                    | Var v ->
                        let! value = State.getVar v
                        match value with
                            | Some v -> return Expr.Value(v, e.Type)
                            | _ -> return e

                    | WhileLoop(guard, body) ->
                        let! guard = evaluateConstantsS guard
                        let! body = evaluateConstantsS body
                        match guard, body with
                            | Bool false, _ -> return Expr.Unit
                            | _, Unit -> return Expr.Unit
                            | g, b -> return Expr.WhileLoop(g, b)

                    | _ ->
                        return failwithf "[FShade] unexpected expression %A" e
            }

        let evaluateConstants (e : Expr) =
            let run = evaluateConstantsS e
            let mutable state = State.empty
            run.Run(&state)

        let evaluateConstants' (isSideEffect : MethodInfo -> bool) (e : Expr) =
            let run = evaluateConstantsS e

            let isSideEffect (m : MemberInfo) =
                match m with    
                    | :? MethodInfo as mi -> isSideEffect mi
                    | _ -> false

            let mutable state = { State.empty with isGlobalSideEffect = isSideEffect }
            run.Run(&state)

        let evaluateConstants'' (isSideEffect : MemberInfo -> bool) (e : Expr) =
            let run = evaluateConstantsS e
            let mutable state = { State.empty with isGlobalSideEffect = isSideEffect }
            run.Run(&state)
       
    module InputLifting =
        type State =
            {
                usedInputs : Map<string, ParameterDescription>
            }
                 
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module State =
            let empty = 
                { 
                    usedInputs = Map.empty
                }

        
        let rec liftInputsS (e : Expr) : State<State, Expr> =
            state {
                match e with

                    | ReadInputOrRaytracingData(ParameterKind.Input, name, idx, _) ->
                        do! State.modify (fun s ->
                            if Map.containsKey name s.usedInputs then
                                s
                            else
                                let typ =
                                    match idx with
                                        | Some _ -> e.Type.MakeArrayType()
                                        | _ -> e.Type
                                { s with usedInputs = Map.add name (ParameterDescription.ofType typ) s.usedInputs }
                        )
                        return e

                    | ReadInputOrRaytracingData(kind, name, idx, _) ->
                        match idx with
                            | Some idx ->
                                let! idx = liftInputsS idx
                                return Expr.ReadInput(kind, e.Type, name, idx)
                            | None ->
                                return Expr.ReadInput(kind, e.Type, name)
                                
                    | WriteOutputs outputs -> 
                        let! outputs = 
                            outputs |> Map.mapS (fun name (idx, value) ->
                                state {
                                    let! idx = Option.mapS liftInputsS idx
                                    let! value = liftInputsS value
                                    return (idx, value)
                                }
                            )
                        return Expr.WriteOutputs outputs

                    | AddressOf e ->
                        let! e = liftInputsS e
                        return Expr.AddressOf e

                    | AddressSet(v, e) ->
                        let! v = liftInputsS v
                        let! e = liftInputsS e
                        return Expr.AddressSet(v,e)

                    | Application(lambda, arg) ->
                        let! lambda = liftInputsS lambda
                        let! arg = liftInputsS arg
                        return Expr.Application(lambda, arg)

                    | ForInteger(v, first, step, last, body) ->
                        let! first = liftInputsS first
                        let! step = liftInputsS step
                        let! last = liftInputsS last
                        let! body = liftInputsS body
                        return Expr.ForInteger(v, first, step, last, body)

                    | ForEach(v, s, b) ->
                        let! s = liftInputsS s
                        let! b = liftInputsS b
                        return Expr.ForEach(v, s, b)

                    | CallFunction(utility, args) ->
                        let mutable usedInputs = Map.empty
                        let newBody = liftFunctionInputsS(utility.functionBody).Run(&usedInputs)

                        let (vars, values) = usedInputs |> Map.toList |> List.map snd |> List.unzip

                        let inputVars = utility.functionArguments @ vars
                        let utility =
                            { utility with
                                functionArguments = inputVars
                                functionBody = newBody
                                functionId = Expr.ComputeHash (Expr.Lambdas(inputVars, newBody))
                                functionTag = null
                            }

                        let! args = args |> List.mapS liftInputsS
                        let! values = values |> List.mapS liftInputsS
                        return Expr.CallFunction(utility, args @ values)

                    | CallWithWitnesses(None, oi, mi, ws, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.CallWithWitnesses(oi, mi, ws, args)

                    | CallWithWitnesses(Some t, oi, mi, ws, args) ->
                        let! t = liftInputsS t
                        let! args = args |> List.mapS liftInputsS
                        return Expr.CallWithWitnesses(t, oi, mi, ws, args)
                        
                    | Call(None, mi, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.Call(mi, args)

                    | Call(Some t, mi, args) ->
                        let! t = liftInputsS t
                        let! args = args |> List.mapS liftInputsS
                        return Expr.Call(t, mi, args)
                        
                    | Coerce(e, t) ->
                        let! e = liftInputsS e
                        return Expr.Coerce(e, t)

                    | DefaultValue t ->
                        return e

                    | FieldGet(None, f) ->
                        return e

                    | FieldGet(Some t, f) ->
                        let! t = liftInputsS t
                        return Expr.FieldGet(t, f)

                    | FieldSet(None, f, value) ->
                        let! value = liftInputsS value
                        return Expr.FieldSet(f, value)

                    | FieldSet(Some t, f, value) ->
                        let! t = liftInputsS t
                        let! value = liftInputsS value
                        return Expr.FieldSet(t, f, value)

                    | IfThenElse(cond, i, e) ->
                        let! cond = liftInputsS cond
                        let! i = liftInputsS i
                        let! e = liftInputsS e
                        return Expr.IfThenElse(cond, i, e)

                    | Lambda(v,b) ->
                        let! b = liftInputsS b
                        return Expr.Lambda(v,b)

                    | Let(v, e, b) ->
                        let! e = liftInputsS e
                        let! b = liftInputsS b
                        return Expr.Let(v,e,b)

                    | NewTuple(args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.NewTuple args

                    | NewArray(t, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.NewArray(t, args)

                    | NewDelegate(t, vars, body) ->
                        let! body = liftInputsS body
                        return Expr.NewDelegate(t, vars, body)

                    | NewObject(ctor, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.NewObject(ctor, args)

                    | NewRecord(t, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.NewRecord(t, args)

                    | NewUnionCase(ci, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.NewUnionCase(ci, args)

                    | PropertyGet(None, pi, indices) ->
                        let! indices = indices |> List.mapS liftInputsS
                        return Expr.PropertyGet(pi, indices)

                    | PropertyGet(Some t, pi, indices) ->
                        let! t = liftInputsS t
                        let! indices = indices |> List.mapS liftInputsS
                        return Expr.PropertyGet(t, pi, indices)
                        
                    | PropertySet(None, pi, indices, value) ->
                        let! value = liftInputsS value
                        let! indices = indices |> List.mapS liftInputsS
                        return Expr.PropertySet(pi, value, indices)

                    | PropertySet(Some t, pi, indices, value) ->
                        let! t = liftInputsS t
                        let! value = liftInputsS value
                        let! indices = indices |> List.mapS liftInputsS
                        return Expr.PropertySet(t, pi, value, indices)

                    | UnsafeWrite(t, value) ->
                        let! t = liftInputsS t
                        let! value = liftInputsS value
                        return Expr.UnsafeWrite(t, value)
                        
                    | QuoteRaw e ->
                        let! e = liftInputsS e
                        return Expr.QuoteRaw e

                    | QuoteTyped e ->
                        let! e = liftInputsS e
                        return Expr.QuoteTyped e

                    | Sequential(l,r) ->
                        let! l = liftInputsS l
                        let! r = liftInputsS r
                        return Expr.Sequential(l,r)

                    | TryFinally(t, f) ->
                        let! t = liftInputsS t
                        let! f = liftInputsS f
                        return Expr.TryFinally(t, f)

                    | TryWith(a, b, c, d, e) ->
                        let! a = liftInputsS a
                        let! c = liftInputsS c
                        let! e = liftInputsS e
                        return Expr.TryWith(a, b, c, d, e)

                    | TupleGet(t, i) ->
                        let! t = liftInputsS t
                        return Expr.TupleGet(t, i)

                    | TypeTest(t, i) ->
                        let! t = liftInputsS t
                        return Expr.TypeTest(t,i)

                    | UnionCaseTest(e, ci) ->
                        let! e = liftInputsS e
                        return Expr.UnionCaseTest(e, ci)

                    | Value _ ->
                        return e

                    | VarSet(v, e) ->
                        let! e = liftInputsS e
                        return Expr.VarSet(v,e)

                    | Var v ->
                        return e

                    | WhileLoop(guard, body) ->
                        let! guard = liftInputsS guard
                        let! body = liftInputsS body
                        return Expr.WhileLoop(guard, body)


                    | _ ->
                        return failwithf "[FShade] unexpected expression %A" e
            }

        and liftFunctionInputsS (e : Expr) : State<Map<_,_>, Expr> =
            state {
                match e with
                    | ReadInput(ParameterKind.Input, name, idx) ->
                        let! (s : Map<string, Var * Expr>) = State.get

                        let idxHash = idx |> Option.map Expr.ComputeHash |> Option.defaultValue ""
                        let key = name + idxHash

                        match Map.tryFind key s with
                            | Some (v,_) -> 
                                return Expr.Var v
                            | None ->
                                let v = Var(name, e.Type)
                                do! State.put (Map.add key (v,e) s)
                                return Expr.Var v

                    | ReadInputOrRaytracingData(kind, name, idx, slot) ->
                        match idx with
                            | Some idx ->
                                let! idx = liftFunctionInputsS idx
                                return Expr.ReadInput(kind, e.Type, name, idx, slot)
                            | None ->
                                return Expr.ReadInput(kind, e.Type, name, slot)

                    | CallFunction(utility, args) ->
                        let! args = args |> List.mapS liftFunctionInputsS
                        let mutable usedInputs = Map.empty
                        let newBody = liftFunctionInputsS(utility.functionBody).Run(&usedInputs)

                        let (vars, values) = usedInputs |> Map.toList |> List.map snd |> List.unzip

                        let inputVars = utility.functionArguments @ vars
                        let utility =
                            { utility with
                                functionArguments = inputVars
                                functionBody = newBody
                                functionId = Expr.ComputeHash (Expr.Lambdas(inputVars, newBody))
                                functionTag = null
                            }


                        let! values = values |> List.mapS liftFunctionInputsS
                        return Expr.CallFunction(utility, args @ values)

                        
                    | ShapeVar v -> 
                        return Expr.Var v

                    | ShapeLambda(v, b) ->
                        let! b = liftFunctionInputsS b
                        return Expr.Lambda(v, b)

                    | ShapeCombination(o, args) -> 
                        let! args = args |> List.mapS liftFunctionInputsS
                        return RebuildShapeCombination(o, args)
            }

        let liftInputs (e : Expr) =
            let run = liftInputsS(e)
            let mutable state = State.empty
            let res = run.Run(&state)
            res

    module Inlining =
        type State =
            {
                variables       : bool
                trivial         : bool
                inputs          : bool
                nonMutable      : Set<Var>
                isSideEffect    : MethodInfo -> bool
                functions       : UtilityFunction -> bool
            }

                 
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module State =
            let empty (isSideEffect : MethodInfo -> bool) = 
                { 
                    variables = true
                    trivial = true
                    inputs = true
                    nonMutable = Set.empty
                    isSideEffect = isSideEffect
                    functions = fun f -> f.functionIsInline
                }

            let inlineFunction (f : UtilityFunction) =
                State.get |> State.map (fun s -> s.functions f)

            let variables = State.get |> State.map (fun s -> s.variables)
            let trivial = State.get |> State.map (fun s -> s.trivial)
            let inputs = State.get |> State.map (fun s -> s.inputs)

            let swizzle = System.Text.RegularExpressions.Regex @"[xyzw]+"

            [<return: Struct>]
            let rec (|TrivialOrInput|_|) (nonMutable : Set<Var>) (e : Expr) =
                match e with
                | Var v when v.IsMutable -> 
                    if Set.contains v nonMutable then ValueSome ()
                    else ValueNone

                | Var _
                | Value _
                | FieldGet(None, _)
                | PropertyGet(None, _, [])
                | TupleGet(Trivial, _)
                | PropertyGet(Some (TrivialOrInput nonMutable), (FSharpTypeProperty | ArrayLengthProperty), [])
                | FieldGet(Some (TrivialOrInput nonMutable), _) 
                | ReadInput(_, _, None) 
                | ReadInput(_, _, Some (TrivialOrInput nonMutable)) -> 
                    ValueSome()

                | PropertyGet(Some (TrivialOrInput nonMutable as t), prop, []) ->
                    match t.Type with
                    | TypeMeta.Patterns.VectorOf _ ->
                        if swizzle.IsMatch (prop.Name.ToLower()) then ValueSome ()
                        else ValueNone
                    | _ ->
                        ValueNone

                | _ ->
                    ValueNone

            [<return: Struct>]
            let rec (|OnlyVar|_|) (nonMutable : Set<Var>)  (e : Expr) =
                match e with
                | Var v when v.IsMutable -> 
                    if Set.contains v nonMutable then ValueSome ()
                    else ValueNone

                | Var _ 
                | Value _ ->
                    ValueSome ()
                | _ -> 
                    ValueNone
            
            [<return: Struct>]
            let rec (|OnlyInputs|_|) (e : Expr) =
                match e with
                | ReadInput _ ->
                    ValueSome ()
                | _ -> 
                    ValueNone
                        
            [<return: Struct>]
            let rec (|InputOrVar|_|)  (nonMutable : Set<Var>)   (e : Expr) =
                match e with
                | Var v when v.IsMutable -> 
                    if Set.contains v nonMutable then ValueSome ()
                    else ValueNone
                | ReadInput _ 
                | Var _ ->
                    ValueSome ()
                | _ -> 
                    ValueNone

            [<return: Struct>]
            let rec (|SamplerOrImageType|_|) (t : Type) =
                match t with
                | SamplerType _ 
                | ImageType _
                | ArrOf(_,SamplerOrImageType)
                | ArrayOf(SamplerOrImageType) ->
                    ValueSome ()
                | _ ->
                    ValueNone

            let letValuePattern =
                State.get |> State.map (fun s ->
                    let pat = 
                        match s.variables, s.trivial, s.inputs with
                        | (false | true), true, true ->
                            (|TrivialOrInput|_|) s.nonMutable

                        | (false | true), true, false ->
                            (|Trivial|_|)

                        | true, false, false ->
                            (|OnlyVar|_|) s.nonMutable

                        | false, false, true ->
                            (|OnlyInputs|_|)

                        | true, false, true ->
                            (|InputOrVar|_|) s.nonMutable

                        | false, false, false ->
                            fun _ -> ValueNone

                    let sam (e : Expr) =
                        match e.Type with
                        | SamplerOrImageType -> ValueSome ()
                        | _ -> ValueNone

                    fun e ->
                        match sam e with
                        | ValueSome () -> ValueSome ()
                        | ValueNone -> pat e
                )

        [<return: Struct>]
        let (|TupleBind|_|) (e : Expr) =
            match e with
            | Let(v, CallFunction(utility, args), body) when FSharpType.IsTuple v.Type ->
                ValueSome(v, utility, args, body)
            | _ ->
                ValueNone


        let rec inlineS (e : Expr) =
            state {
                match e with
                    | WriteOutputs map ->
                        let! map = map |> Map.mapS (fun name (idx, value) ->
                            state {
                                let! idx = idx |> Option.mapS inlineS
                                let! value = inlineS value
                                return (idx, value)
                            }
                        )
                        return Expr.WriteOutputs map

                    | ReadInputOrRaytracingData(kind, name, idx, volatile) ->
                        match idx with
                            | Some idx ->
                                let! idx = inlineS idx
                                return Expr.ReadInput(kind, e.Type, name, idx, volatile)
                            | None ->
                                return e

                    | AddressOf e ->
                        let! e = inlineS e
                        return Expr.AddressOf e

                    | AddressSet(v,e) ->
                        let! v = inlineS v
                        let! e = inlineS e
                        return Expr.AddressSet(v,e)

                    | Application(Lambda(v,b), e) ->
                        let r = Expr.Let(v, e, b)
                        return! inlineS r

                    | Application(lambda, arg) ->
                        let! lambda = inlineS lambda
                        let! arg = inlineS arg
                        return Expr.Application(lambda, arg)
                    
                    | ForInteger(v, first, step, last, body) ->
                        let! first = inlineS first
                        let! step = inlineS step
                        let! last = inlineS last
                        let! body = inlineS body
                        return Expr.ForInteger(v, first, step, last, body)

                    | ForEach(v, s, b) ->
                        let! s = inlineS s
                        let! b = inlineS b
                        return Expr.ForEach(v, s, b)

                    | TupleBind(v, utility, args, body) when utility.functionIsInline ->
                        let code = UtilityFunction.inlineCode utility args

                        let rec substitute (f : int -> Expr) (e : Expr) =
                            match e with
                            | TupleGet(Var vi, i) when vi = v -> f i
                            | ShapeCombination(o, args) -> RebuildShapeCombination(o, args |> List.map (substitute f))
                            | ShapeVar _ -> e
                            | ShapeLambda(v,b) -> Expr.Lambda(v, substitute f b)
                        

                        if UtilityFunction.singleReturn code then
                            let! code = inlineS code
                            return!
                                code |> UtilityFunction.subsituteValueS (fun ret ->
                                    state {
                                        match ret with
                                        | NewTuple args ->
                                            let! s = State.get
                                            let mutable nonMutable = s.nonMutable
                                            let elems =
                                                args |> List.mapi (fun i a ->
                                                    match a with
                                                    | Var v -> 
                                                        nonMutable <- Set.add v nonMutable
                                                        None, a
                                                    | Trivial -> None, a
                                                    | e -> 
                                                        let v = Var(sprintf "t%d" i, a.Type)
                                                        Some (v,e), Expr.Var v
                                                )
                                                
                                            do! State.modify (fun s -> { s with nonMutable = nonMutable })
                                            let bindings = elems |> List.choose fst
                                            let mapping = elems |> List.map snd |> List.toArray

                                    
                                            let rec bind (vs : list<Var * Expr>) (body : Expr) =
                                                match vs with
                                                | [] -> body
                                                | (v, e) :: vs -> Expr.Let(v, e, bind vs body)
                                            let res = body |> substitute (fun i -> mapping.[i]) |> bind bindings

                                            return! inlineS res

                                        | e ->
                                            let res = body |> substitute (fun i -> Expr.TupleGet(e, i))
                                            return! inlineS res
                                    }
                                )
                        else 
                            let vars = FSharpType.GetTupleElements v.Type |> Array.mapi (fun i t -> Var(sprintf "t%d" i, t, true))
                            let body = substitute (fun i -> Expr.Var vars.[i]) body

                            let vars = vars |> Array.toList
                            let code = 
                                code |> UtilityFunction.subsituteValue (fun v ->
                                    match v with
                                    | NewTuple args ->
                                        List.zip vars args |> List.map (fun (v,e) ->
                                            Expr.VarSet(v, e)
                                        ) |> Expr.Seq
                                    | e ->
                                        vars |> List.mapi (fun i v ->
                                            Expr.VarSet(v, Expr.TupleGet(e, i))
                                        ) |> Expr.Seq
                                )

                            let fin =
                                let rec bind (vs : list<Var>) =
                                    match vs with
                                    | [] -> Expr.Seq [code; body]
                                    | v :: vs -> Expr.Let(v, Expr.DefaultValue v.Type, bind vs)
                                bind vars
                            return! inlineS fin

                    //| Let(v, CallFunction(utility, args), Let(v0, TupleGet(Var va, i), body)) ->
                    //    failwith ""

                    | CallFunction(utility, args) ->
                        let! inl = State.inlineFunction utility
                        if inl || utility.functionIsInline then
                            let code = UtilityFunction.inlineCode utility args
                            return! inlineS code
                        else
                            let! args = args |> List.mapS inlineS
                            let! s = State.get
                            let utility =
                                utility |> UtilityFunction.map (fun b ->
                                    let mutable innerState = s
                                    inlineS(b).Run(&innerState)
                                )
                            let utility = { utility with functionTag = null }
                            return Expr.CallFunction(utility, args)
                            
                    | CallWithWitnesses(None, oi, mi, ws, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.CallWithWitnesses(oi, mi, ws, args)

                    | CallWithWitnesses(Some t, oi, mi, ws, args) ->
                        let! t = inlineS t
                        let! args = args |> List.mapS inlineS
                        return Expr.CallWithWitnesses(t, oi, mi, ws, args)

                    | Call(None, mi, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.Call(mi, args)

                    | Call(Some t, mi, args) ->
                        let! t = inlineS t
                        let! args = args |> List.mapS inlineS
                        return Expr.Call(t, mi, args)

                    | Coerce(e, t) ->
                        let! e = inlineS e
                        return Expr.Coerce(e,t)

                    | DefaultValue _ -> 
                        return e

                    | FieldGet(None, f) ->
                        return e

                    | FieldGet(Some t, f) ->
                        let! t = inlineS t
                        return Expr.FieldGet(t, f)

                    | FieldSet(None, f, value) ->
                        let! value = inlineS value
                        return Expr.FieldSet(f, value)

                    | FieldSet(Some t, f, value) ->
                        let! t = inlineS t
                        let! value = inlineS value
                        return Expr.FieldSet(t, f, value)

                    | IfThenElse(cond, i, e) ->
                        let! cond = inlineS cond
                        let! i = inlineS i
                        let! e = inlineS e
                        return Expr.IfThenElse(cond, i, e)

                    | Lambda(v, b) ->
                        let! b = inlineS b
                        return Expr.Lambda(v, b)

                    //| Let(v, e, b) when v.IsMutable || v.Type.IsRef || v.Type.IsArr || v.Type.IsArray ->
                    //    let! nonMutable = State.get |> State.map (fun s -> Set.contains v s.nonMutable)
                    //    let! e = inlineS e
                    //    let! b = inlineS b
                    //    return Expr.Let(v, e, b)

                    | Let(v, e, b) ->
                        //let! nonMutable = State.get |> State.map (fun s -> Set.contains v s.nonMutable)
                        if v.IsMutable || v.Type.IsRef  then
                            let! e = inlineS e
                            let! b = inlineS b
                            return Expr.Let(v, e, b)
                        else
                            let! e = inlineS e
                            let! state = State.get
                            let! pattern = State.letValuePattern
                            match pattern e with
                                | ValueSome () ->
                                    let b = b.Substitute(fun vi -> if vi = v then Some e else None)
                                    return! inlineS b
                                | ValueNone ->
                                    let nonMutable = state.nonMutable
                                    let rec canInline (e : Expr) =
                                        if e.Type.IsRef || e.Type.IsArr || e.Type.IsArray then
                                            match e with
                                            | Var v -> Set.contains v nonMutable
                                            | _ -> false
                                        else
                                            match e with
                                                | ReadInput(_, _, None) -> true
                                                | ReadInput(_, _, Some idx) -> canInline idx
                                        
                                                | GetArray(e,i) ->
                                                    let res = canInline e && canInline i
                                                    res
                                                | RefOf _
                                                | VarSet _ 
                                                | SetArray _
                                                | AddressOf _ 
                                                | AddressSet _
                                                | FieldSet _
                                                | PropertySet _
                                                | WriteOutputs _ ->
                                                    false




                                                | Call(None, mi, args) ->
                                                    if state.isSideEffect mi then false
                                                    else args |> List.forall canInline

                                                | ShapeVar v -> Set.contains v nonMutable || not v.IsMutable
                                                | ShapeCombination (_,args) -> args |> List.forall canInline
                                                | ShapeLambda(_,b) -> canInline b

                                    if canInline e then
                                        let mutable cnt = 0
                                        let nb = b.Substitute(fun vi -> if vi = v then cnt <- cnt + 1; Some e else None)
                                        if cnt = 1 then
                                            return! inlineS nb
                                        else
                                            let! b = inlineS b
                                            return Expr.Let(v, e, b)
                                    else
                                        let! b = inlineS b
                                        return Expr.Let(v, e, b)

                    | NewArray(t, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.NewArray(t, args)
                        
                    | NewDelegate(t, vars, body) ->
                        let! body = inlineS body
                        return Expr.NewDelegate(t, vars, body)

                    | NewObject(ctor, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.NewObject(ctor, args)

                    | NewRecord(t, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.NewRecord(t, args)
                        
                    | NewTuple(args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.NewTuple args
                        
                    | NewUnionCase(ci, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.NewUnionCase(ci, args)

                    | PropertyGet(None, pi, indices) ->
                        let! indices = indices |> List.mapS inlineS
                        return Expr.PropertyGet(pi, indices)
                        
                    | PropertyGet(Some (NewRecord(t, fields)), pi, []) ->
                        let decls = FSharpType.GetRecordFields(t, true) |> Array.toList 

                        let value = 
                            List.zip decls fields |> List.pick (fun (d,v) ->
                                if d = pi then Some v
                                else None
                            )

                        let! value = inlineS value
                        return value
                        
                    | PropertyGet(Some t, pi, indices) ->
                        
                        let! t = inlineS t
                        let! indices = indices |> List.mapS inlineS
                        return Expr.PropertyGet(t, pi, indices)
                        
                    | PropertySet(None, pi, indices, value) ->
                        let! value = inlineS value
                        let! indices = indices |> List.mapS inlineS
                        return Expr.PropertySet(pi, value, indices)
                        
                    | PropertySet(Some t, pi, indices, value) ->
                        let! t = inlineS t
                        let! value = inlineS value
                        let! indices = indices |> List.mapS inlineS
                        return Expr.PropertySet(t, pi, value, indices)

                    | UnsafeWrite(t, value) ->
                        let! t = inlineS t
                        let! value = inlineS value
                        return Expr.UnsafeWrite(t, value)

                    | QuoteRaw e ->
                        let! e = inlineS e
                        return Expr.QuoteRaw e
                        
                    | QuoteTyped e ->
                        let! e = inlineS e
                        return Expr.QuoteTyped e

                    | Sequential(l, r) ->
                        let! l = inlineS l
                        let! r = inlineS r
                        return Expr.Sequential(l, r)

                    | TryFinally(t,f) ->
                        let! t = inlineS t
                        let! f = inlineS f
                        return Expr.TryFinally(t, f)

                    | TryWith(a, b, c, d, e) ->
                        let! a = inlineS a
                        let! c = inlineS c
                        let! e = inlineS e
                        return Expr.TryWith(a, b, c, d, e)

                    | TupleGet(t, i) ->
                        let! t = inlineS t
                        return Expr.TupleGet(t, i)

                    | TypeTest(v, t) ->
                        let! v = inlineS v
                        return Expr.TypeTest(v, t)

                    | UnionCaseTest(e, ci) ->
                        let! e = inlineS e
                        return Expr.UnionCaseTest(e, ci)
                        
                    | Value _ ->
                        return e
                        
                    | VarSet (v, e) ->
                        let! e = inlineS e
                        return Expr.VarSet (v, e)

                    | Var v ->
                        return e

                    | WhileLoop(guard, body) ->
                        let! guard = inlineS guard
                        let! body = inlineS body
                        return Expr.WhileLoop(guard, body)

                    | _ -> 
                        return failwithf "[FShade] unexpected expression %A" (string e)
            }

        let inlining (isSideEffect : MethodInfo -> bool) (e : Expr) =
            let run = inlineS e
            let mutable state = State.empty isSideEffect
            run.Run(&state)


        let inlining' (isSideEffect : MethodInfo -> bool) (functions : UtilityFunction -> bool) (e : Expr) =
            let run = inlineS(e)
            let mutable state = { State.empty isSideEffect with functions = functions }
            run.Run(&state)

    module StatementHoisting =
        
        type Hoisting =
            {
                finalize : Option<Expr -> Expr>
            }

            static member Zero = { finalize = None }

            static member (+) (l : Hoisting, r : Hoisting) =
                match l.finalize, r.finalize with
                    | Some lf, Some rf -> { finalize = Some (lf >> rf) }
                    | Some _, None -> l
                    | None, Some _ -> r
                    | None, None -> l

            static member Sequential (e : Expr) =
                { finalize = Some (fun f -> Expr.Seq [e; f]) }

        let rec processExpression (expr : Expr) : Expr * Hoisting =
            match expr with
                | WithValue(_, _, e) -> 
                    processExpression e
                | WriteOutputs _
                | AddressSet _ 
                | ForInteger _ 
                | ForEach _ 
                | FieldSet _
                | PropertySet _ 
                | VarSet _
                | WhileLoop _ ->
                    let expr = processStatement expr
                    Expr.Unit, Hoisting.Sequential expr

                | IfThenElse _ when expr.Type = typeof<unit> ->
                    let expr = processStatement expr
                    Expr.Unit, Hoisting.Sequential expr
                    
                    
                | Sequential(a, Sequential(b,c)) ->
                    Expr.Sequential(Expr.Sequential(a,b), c) |> processExpression

                | Sequential(l,r) ->
                    let l = processStatement l
                    let r, rh = processExpression r
                    r, rh + Hoisting.Sequential l


                | Let(v,e,b) ->
                    let b, bh = processExpression b
                    let e, eh = processExpression e
                    b, bh + { finalize = Some (fun f -> Expr.Let(v, e, f)) } + eh


                | TryFinally _ -> failwith ""
                | TryWith _ -> failwith ""
                
                | AddressOf e ->
                    let e, eh = processExpression e
                    Expr.AddressOf(e), eh

                | Application(l,a) ->
                    let l, lh = processExpression l
                    let a, ah = processExpression a
                    Expr.Application(l,a), lh + ah

                | CallFunction(utility, args) ->
                    let args, ah = processManyExpresions args
                    let utility = utility |> UtilityFunction.map processStatement
                    Expr.CallFunction(utility, args), ah
                    
                | CallWithWitnesses(None, oi, mi, ws, args) ->
                    let args, ah = processManyExpresions args
                    Expr.CallWithWitnesses(oi, mi, ws, args), ah

                | CallWithWitnesses(Some t, oi, mi, ws, args) ->
                    let t, th = processExpression t
                    let args, ah = processManyExpresions args
                    Expr.CallWithWitnesses(t, oi, mi, ws, args), th + ah

                | Call(None, mi, args) ->
                    let args, ah = processManyExpresions args
                    Expr.Call(mi, args), ah
                    
                | Call(Some t, mi, args) ->
                    let t, th = processExpression t
                    let args, ah = processManyExpresions args
                    Expr.Call(t, mi, args), th + ah

                    
                | Coerce(e, t) ->
                    let e, eh = processExpression e
                    Expr.Coerce(e, t), eh

                | DefaultValue t ->
                    Expr.DefaultValue t, Hoisting.Zero

                | FieldGet(None, f) ->
                    Expr.FieldGet(f), Hoisting.Zero

                | FieldGet(Some t, f) ->
                    let t, th = processExpression t
                    Expr.FieldGet(t, f), th

                | IfThenElse(c, i, e) ->
                    let c, ch = processExpression c
                    Expr.IfThenElse(c, i, e), ch

                | Lambda(v,b) ->
                    Expr.Lambda(v, processStatement b), Hoisting.Zero

                | NewArray(t, args) ->
                    let args, ah = processManyExpresions args
                    Expr.NewArray(t, args), ah

                | NewDelegate(t, vars, body) ->
                    Expr.NewDelegate(t, vars, processStatement body), Hoisting.Zero

                | NewObject(ctor, args) ->
                    let args, ah = processManyExpresions args
                    Expr.NewObject(ctor, args), ah

                | NewRecord(t, args) ->
                    let args, ah = processManyExpresions args
                    Expr.NewRecord(t, args), ah

                | NewTuple(args) ->
                    let args, ah = processManyExpresions args
                    Expr.NewTuple(args), ah

                | NewUnionCase(ci, args) ->
                    let args, ah = processManyExpresions args
                    Expr.NewUnionCase(ci, args), ah

                | PropertyGet(None, pi, ii) ->
                    let ii, ih = processManyExpresions ii
                    Expr.PropertyGet(pi, ii), ih

                | PropertyGet(Some t, pi, ii) ->
                    let t, th = processExpression t
                    let ii, ih = processManyExpresions ii
                    Expr.PropertyGet(t, pi, ii), th + ih

                | QuoteRaw e ->
                    Expr.QuoteRaw e, Hoisting.Zero
                    
                | QuoteTyped e -> 
                    Expr.QuoteTyped e, Hoisting.Zero


                | TupleGet(v, i) ->
                    let v, vh = processExpression v
                    Expr.TupleGet(v, i), vh

                | TypeTest(v,t) ->
                    let v, vh = processExpression v
                    Expr.TypeTest(v, t), vh

                | UnionCaseTest(v, ci) ->
                    let v, vh = processExpression v
                    Expr.UnionCaseTest(v,ci), vh

                | Value _ ->
                    expr, Hoisting.Zero

                | Var _ ->
                    expr, Hoisting.Zero


                | _ -> failwithf "[FShade] unexpected expression %A" expr

        and processManyExpresions (l : list<Expr>) =
            List.foldBack (fun a (r, rh) ->
                let a, ah = processExpression a
                (a :: r, ah + rh)
            ) l ([], Hoisting.Zero)

        and processStatement (expr : Expr) : Expr =
            let inline apply (h : Hoisting) (e : Expr) =
                match h.finalize with
                    | Some f -> e |> f |> processStatement
                    | None -> e

            match expr with 
                | WriteOutputs outputs -> 
                    let mutable res = Map.empty
                    let mutable hoist = Hoisting.Zero
                    for (name, (idx,value)) in Map.toSeq outputs do
                        match idx with
                            | Some idx ->
                                let idx, ih = processExpression idx
                                let v, vh = processExpression value
                                res <- Map.add name (Some idx, v) res
                                hoist <- hoist + ih + vh
                            | None ->
                                let v, vh = processExpression value
                                res <- Map.add name (None, v) res
                                hoist <- hoist + vh
                          
                          
                    Expr.WriteOutputs(res) |> apply hoist 

                | AddressSet(v,e) ->
                    let v, vh = processExpression v
                    let e, eh = processExpression e 
                    Expr.AddressSet(v,e) |> apply (vh + eh)

                | ForInteger(v, start, step, stop, body) ->
                    let body = processStatement body
                    
                    let start, h0 = processExpression start
                    let step, h1 = processExpression step
                    let stop, h2 = processExpression stop

                    Expr.ForInteger(v, start, step, stop, body) |> apply (h0 + h1 + h2)

                     
                | ForEach(v,s,body) ->
                    let body = processStatement body
                    let s, sh = processExpression s

                    Expr.ForEach(v, s, body) |> apply sh


                | FieldSet(None, f, value) ->
                    let value, vh = processExpression value
                    Expr.FieldSet(f, value) |> apply vh

                | FieldSet(Some t, f, value) ->
                    let t, th = processExpression t
                    let value, vh = processExpression value
                    Expr.FieldSet(t, f, value) |> apply (th + vh)
                | PropertySet(None, pi, index, value) ->
                    let index, ih = processManyExpresions index
                    let value, vh = processExpression value
                    Expr.PropertySet(pi, value, index) |> apply (ih + vh)

                | PropertySet(Some t, pi, index, value) ->
                    let t, th = processExpression t 
                    let index, ih = processManyExpresions index
                    let value, vh = processExpression value
                    Expr.PropertySet(t, pi, value, index) |> apply (th + ih + vh)

                | UnsafeWrite(t, value) ->
                    let t, th = processExpression t 
                    let value, vh = processExpression value
                    Expr.UnsafeWrite(t, value) |> apply (th + vh)

                     
                | WhileLoop (guard, body) ->
                    let guard, gh = processExpression guard
                    let body = processStatement body
                    Expr.WhileLoop(guard, body) |> apply gh

                | Sequential(l,r) ->
                    let l = processStatement l
                    let r = processStatement r
                    Expr.Sequential(l,r)


                | Let(v,e,b) ->
                    let b = processStatement b
                    let e, eh = processExpression e
                    Expr.Let(v,e,b) |> apply eh

                | VarSet(v,e) ->
                    let e, eh = processExpression e
                    Expr.VarSet(v,e) |> apply eh

                | IfThenElse(c,i,e) when e.Type = typeof<unit> ->
                    let c, ch = processExpression c
                    let i = processStatement i
                    let e = processStatement e
                    Expr.IfThenElse(c,i,e) |> apply ch

                | e ->
                    let e, eh = processExpression e
                    match eh.finalize with
                        | Some f -> f e
                        | None -> e

        let hoistImperative (e : Expr) =
            processStatement e


    module CSE =
        
        module List =
            let rec mapOption (f : 'a -> Option<'b>) (l : list<'a>) =
                match l with
                    | [] -> Some []
                    | h :: t ->
                        match f h with
                            | Some h ->
                                match mapOption f t with
                                    | Some t -> Some (h :: t)
                                    | None -> None
                            | None ->
                                None

        type ExprPointer = { self : Expr; parent : Option<ExprPointer> }

        let rec getInputPointers (parent : Option<ExprPointer>) (e : Expr) =
            state {
                match e with
                    | ReadInput(ParameterKind.Input,name,index) ->
                        return MapExt.ofList [name, [(index, parent)]]
                           
                    | WriteOutputs outputs ->
                        let mutable res = MapExt.empty
                        for (_,(i, v)) in Map.toSeq outputs do
                            match i with
                                | Some i ->
                                    let! ip = getInputPointers (Some { self = e; parent = parent}) i
                                    res <- MapExt.unionWith (@) res ip
                                | None ->
                                    ()

                            let! vp = getInputPointers (Some { self = e; parent = parent}) v
                            res <- MapExt.unionWith (@) res vp


                        return res

                    | ShapeCombination(o, args) ->
                        let p = { self = e; parent = parent }
                        let mutable res = MapExt.empty
                        for a in args do
                            let! inner = getInputPointers (Some p) a
                            res <- MapExt.unionWith (@) res inner

                        return res

                    | ShapeLambda(_,b) ->
                        return! getInputPointers (Some { self = e; parent = parent }) b

                    | ShapeVar v ->
                        return MapExt.empty
            }

        let findIndexedCommonSubExpressions (body : Expr) =
            let pointers = getInputPointers None body
            let mutable state = ()
            let pointers = pointers.Run(&state)

            let removeIndex (e : Expr) =
                match e with
                    | WriteOutputs _ ->
                        None

                    | _ ->
                        let mutable uniqueIndex = None
                        let mutable success = true
                        let res = 
                            e.SubstituteReads(fun kind typ name index slot ->
                                match kind, index with
                                    | ParameterKind.Input, Some index ->
                                        let iHash = Expr.ComputeHash index
                                        match uniqueIndex with
                                            | Some (hash, i) ->
                                                if hash <> iHash then
                                                    success <- false
                                            | None ->
                                                uniqueIndex <- Some(iHash, index)


                                        Expr.Var(Var(name, typ)) |> Some
                                    | _ ->
                                        None
                            )
                        if success then
                            match uniqueIndex with
                                | Some(_,i) ->
                                    Some (i, res)
                                | None ->
                                    None
                        else
                            None

            let allEqualModIndex (es : list<ExprPointer>) =
                match es |> List.map (fun p -> p.self) |> List.mapOption removeIndex with
                    | Some indicesAndValues ->
                        let hashes = indicesAndValues |> List.map (snd >> Expr.ComputeHash)
                        match hashes with
                            | h :: t -> List.forall ((=) h) t
                            | _ -> true
                    | None ->
                        false


            let mutable before = Map.empty
            let mutable replacements = HashMap.empty

            for (v, ptrs) in MapExt.toSeq pointers do
                let mutable last = None
                let mutable parents =  ptrs |> List.mapOption snd
                        
                while Option.isSome parents && allEqualModIndex parents.Value do
                    last <- parents
                    parents <- List.mapOption (fun e -> e.parent) parents.Value

                match last with
                    | Some ((h::_) as last) ->
                        let last = last |> List.map (fun l -> l.self)

                        let h = 
                            h.self.SubstituteReads(fun kind typ name index slot ->
                                Expr.ReadInput(kind, typ, name, slot) |> Some
                            )

                        let mutable names = Set.empty

                        let repl =
                            last |> List.map (fun e ->
                                let mutable index = Unchecked.defaultof<Expr>
                 
                                e.SubstituteReads(fun kind typ name idx slot ->
                                    match kind, idx with
                                        | ParameterKind.Input, Some i ->
                                            index <- i
                                            names <- Set.add name names
                                        | _ -> 
                                            ()
                                    None
                                ) |> ignore

                                let inputName = names |> String.concat ""
                                e, Expr.ReadInput(ParameterKind.Input, e.Type, inputName, index)
                            ) |> HashMap.ofList

                        replacements <- HashMap.union replacements repl
                        let inputName = names |> String.concat ""
                        before <- Map.add inputName h before    
                        

                    | _ ->
                        ()


                
                ()

            let rec apply (e : Expr) =
                match HashMap.tryFind e replacements with
                    | Some r -> 
                        r
                    | None ->
                        match e with
                            | ShapeCombination(o, args) ->
                                RebuildShapeCombination(o, List.map apply args)
                            | ShapeLambda(v,b) ->
                                Expr.Lambda(v, apply b)
                            | ShapeVar v ->
                                Expr.Var v
           
            let body = apply body
            let vs = Expr.WriteOutputs(before |> Map.map (fun _ e -> None, e))
            
            printfn "VERTEX"
            printfn "%s" (PrettyPrinter.print vs)

            
            printfn "GEOMETRY"
            printfn "%s" (PrettyPrinter.print body)

            ()



    /// creates a new expression only containing e's visible side-effects.
    /// NOTE: all methods are assumed to be pure (except for the ones returning void/unit)
    let withoutValue (e : Expr) =
        DeadCodeElimination.withoutValue e
        
    /// creates a new expression by removing all unused variables/calls.
    /// NOTE: all methods are assumed to be pure (except for the ones returning void/unit)
    let eliminateDeadCode (e : Expr) =
        DeadCodeElimination.eliminateDeadCode e
        
    /// creates a new expression by evaluating all constant-subexpressions where possible.
    /// NOTE: all methods are assumed to be pure (except for the ones returning void/unit)
    let evaluateConstants (e : Expr) =
        ConstantFolding.evaluateConstants e
        
    /// creates a new expression only containing e's visible side-effects.
    /// NOTE: isSideEffect needs to determine whether a Method has non-local side-effects.
    let withoutValue' (isSideEffect : MethodInfo -> bool) (e : Expr) =
        DeadCodeElimination.withoutValue' isSideEffect e
        
    /// creates a new expression by removing all unused variables/calls.
    /// NOTE: isSideEffect needs to determine whether a Method has non-local side-effects.
    let eliminateDeadCode' (isSideEffect : MethodInfo -> bool)  (e : Expr) =
        DeadCodeElimination.eliminateDeadCode' isSideEffect e
        
    /// creates a new expression by evaluating all constant-subexpressions where possible.
    /// NOTE: isSideEffect needs to determine whether a Method has non-local side-effects.
    let evaluateConstants' (isSideEffect : MethodInfo -> bool)  (e : Expr) =
        ConstantFolding.evaluateConstants' isSideEffect e

    /// creates a new expression by lifting all shader inputs to function-arguments until they can be read. (in the shader's main entry)
    let liftInputs (e : Expr) =
        InputLifting.liftInputs e
        
    /// inlines copy variables, trivial expressions, input-reads and functions annotated with the [<Inline>] attribute
    let inlining (isSideEffect : MethodInfo -> bool) (e : Expr) =
        Inlining.inlining isSideEffect e
        
    /// inlines copy variables, trivial expressions, input-reads and functions annotated with the [<Inline>] attribute.
    /// Function inlining can be forced using the given callback.
    let inlining' (isSideEffect : MethodInfo -> bool) (f : UtilityFunction -> bool) (e : Expr) =
        Inlining.inlining' isSideEffect f e

    /// hoists imperative constructs. For example `let a = let b = 10 in b * 199` translates to `let b = 10 in let a = b * 199`.
    /// this ensures that most imperative constructs occur on statement-level and can easily be compiled to C like languages.
    let hoistImperativeConstructs (e : Expr) =
        StatementHoisting.processStatement e