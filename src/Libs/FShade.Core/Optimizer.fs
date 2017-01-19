namespace FShade

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative


[<AutoOpen>]
module Optimizer =
    open Aardvark.Base.Monads.State

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


//    let hasSideEffect (e : Expr) =
//        // TODO: validate
//        let rec hasSideEffect (locals : Set<Var>) (e : Expr) =
//            match e with
//                | Let(v, e, b) ->
//                    hasSideEffect locals e || 
//                    hasSideEffect (Set.add v locals) b
//                  
//                | LetRecursive(bindings, body) ->
//                    bindings |> List.exists (snd >> hasSideEffect locals) ||
//                    hasSideEffect (bindings |> List.map fst |> Set.ofList) body
//                 
//                | FieldSet _ -> true
//                | PropertySet _ -> true   
//                | VarSet(v,_) when not (Set.contains v locals) -> true
//                | Call(_, mi, _) when mi.ReturnType = typeof<unit> -> true
//
//
//                | ShapeCombination(_, args) -> args |> List.exists (hasSideEffect locals)
//                | ShapeVar _ -> false
//                | ShapeLambda _ -> false 
//
//        hasSideEffect Set.empty e
//
//    let hasExternalSideEffect (e : Expr) =
//        // TODO: validate
//        let rec hasSideEffect (locals : Set<Var>) (e : Expr) =
//            match e with
//                | Let(v, e, b) ->
//                    hasSideEffect locals e || 
//                    hasSideEffect (Set.add v locals) b
//                  
//                | LetRecursive(bindings, body) ->
//                    bindings |> List.exists (snd >> hasSideEffect locals) ||
//                    hasSideEffect (bindings |> List.map fst |> Set.ofList) body
//                 
////                | FieldSet _ -> true
////                | PropertySet _ -> true   
////                | VarSet(v,_) when not (Set.contains v locals) -> true
//                | Call(_, mi, _) when mi.ReturnType = typeof<unit> -> true
//
//
//                | ShapeCombination(_, args) -> args |> List.exists (hasSideEffect locals)
//                | ShapeVar _ -> false
//                | ShapeLambda _ -> false 
//
//        hasSideEffect Set.empty e


    type EliminationState =
        {
            usedVariables : Set<Var>
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module EliminationState =
        let empty = { usedVariables = Set.empty }
        let useVar (v : Var) = State.modify (fun s -> { s with usedVariables = Set.add v s.usedVariables })
        let remVar (v : Var) = State.modify (fun s -> { s with usedVariables = Set.remove v s.usedVariables })
        let isUsed (v : Var) = State.get |> State.map (fun s -> Set.contains v s.usedVariables)
        let useVars (vars : seq<Var>) = State.modify (fun s -> { s with usedVariables = Set.union (Set.ofSeq vars) s.usedVariables })
       

        let merge (l : EliminationState) (r : EliminationState) =
            {
                usedVariables = Set.union l.usedVariables r.usedVariables
            }

        let rec fix (m : State<EliminationState, 'a>) =
            state {
                let! initial = State.get
                let! res = m
                let! final = State.get
                if initial = final then return res
                else return! fix m
            }

    let onlySideEffects(e : Expr) =
        let rec onlySideEffects (locals : Set<Var>) (e : Expr) : Expr =
            match e with
                | Ignore e ->
                    onlySideEffects locals e

                // static calls returning unit are side-effects
                | Call(None, _, _) when e.Type = typeof<unit> ->
                    e

                // varsets on non-local variables are side-effects
                | VarSet(v,_) when not (Set.contains v locals) ->
                    e

                // field-sets on non-local variables are side-effects
                | FieldSet(Some (Var v), _,  _) when not (Set.contains v locals) ->
                    e

                // static field-sets are side-effects
                | FieldSet(None, _, _) ->
                    e

                    
                // property-sets on non-local variables are side-effects
                | PropertySet(Some (Var v), _, _, _) when not (Set.contains v locals) ->
                    e
                    
                // static property-sets are side-effects
                | PropertySet(None, _, _, _) ->
                    e

                | ForInteger(v, first, step, last, body) ->
                    match onlySideEffects (Set.add v locals) body with
                        | Unit -> Expr.Seq [onlySideEffects locals first; onlySideEffects locals step; onlySideEffects locals last]
                        | body -> Expr.ForInteger(v, first, step, last, body)

                | WhileLoop(guard, body) ->
                    match onlySideEffects locals body with
                        | Unit -> onlySideEffects locals guard
                        | body -> Expr.WhileLoop(guard, body)

                | IfThenElse(cond, i, e) ->
                    match onlySideEffects locals i, onlySideEffects locals e with
                        | Unit, Unit -> onlySideEffects locals cond
                        | i, e-> Expr.IfThenElse(cond, i, e)

                | Lambda(v,b) ->
                    Expr.Unit

                | Let(v,e,b) ->
                    let b = onlySideEffects locals b
                    if b.GetFreeVars() |> Seq.exists (fun vi -> vi = v) then
                        Expr.Let(v,e,b)
                    else
                        match onlySideEffects (Set.add v locals) e with
                            | Unit -> b
                            | e -> Expr.Sequential(e, b)


                | ShapeCombination(o, args) ->
                    args |> List.map (onlySideEffects locals) |> Expr.Seq

                | ShapeVar _
                | ShapeLambda _ -> Expr.Unit

        onlySideEffects Set.empty e
 

    // arr.[i].X <- 10

    let rec (|LExpr|_|) (e : Expr) =
        // TODO: complete?
        match e with
            | Var v -> Some v
            | FieldGet(Some (LExpr v), fi) -> Some v
            | PropertyGet(Some (LExpr v), _, _) -> Some v
            | GetArray(LExpr a, i) -> Some a
            | _ -> None

    let rec eliminateDeadCode (e : Expr) : State<EliminationState, Expr> =
        state {
            match e with

                // side effects affecting variables
                | VarSet(v, e) ->
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        let! e = eliminateDeadCode e
                        return Expr.VarSet(v, e)
                    else
                        return! eliminateDeadCode (onlySideEffects e)

                | FieldSet(Some (LExpr v as target), fi, value) ->
                    let e = ()
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        let! value = eliminateDeadCode value
                        let! target = eliminateDeadCode target
                        return Expr.FieldSet(target, fi, value)
                    else
                        let! v = eliminateDeadCode (onlySideEffects value)
                        let! t = eliminateDeadCode (onlySideEffects target)
                        return Expr.Seq [v;t]

                | PropertySet(Some (LExpr v as target), pi, idx, value) ->
                    let e = ()
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        let! value = eliminateDeadCode value
                        let! idx = idx |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                        let! target = eliminateDeadCode target
                        return Expr.PropertySet(target, pi, value, idx)
                    else
                        let all = (target :: idx) @ [value]
                        let effects = all |> List.map onlySideEffects |> Expr.Seq
                        return! eliminateDeadCode effects

                // global side effects
                | FieldSet(None, f, value) ->
                    let! value = eliminateDeadCode value
                    return Expr.FieldSet(f, value)

                | PropertySet(None, pi, idx, value) ->
                    let! idx = idx |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                    let! value = eliminateDeadCode value
                    return Expr.PropertySet(pi, value, idx)

                // unknown side effects
                | FieldSet(Some t, f, value) ->
                    Log.warn "[FShade] found FieldSet on unknown expression: %A" t
                    let! value = eliminateDeadCode value
                    let! t = eliminateDeadCode t
                    return Expr.FieldSet(t, f, value)

                | PropertySet(Some t, pi, idx, value) ->
                    Log.warn "[FShade] found PropertySet on unknown expression: %A" t
                    let! idx = idx |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                    let! value = eliminateDeadCode value
                    let! t = eliminateDeadCode t
                    return Expr.PropertySet(t, pi, value, idx)


                // control-flow
                | IfThenElse(cond, i, e) ->
                    let! elseState, e = State.withLocalState (eliminateDeadCode e)
                    let! ifState, i = State.withLocalState (eliminateDeadCode i)
                    do! State.put (EliminationState.merge ifState elseState)

                    match i, e with
                        | Unit, Unit -> 
                            let! cond = eliminateDeadCode (onlySideEffects cond)
                            return cond
                        | _ ->
                            let! cond = eliminateDeadCode cond
                            return Expr.IfThenElse(cond, i, e)

                | ForInteger(v, first, step, last, body) ->
                    let iterate =
                        state {
                            let! body = eliminateDeadCode body
                            match body with
                                | Unit ->
                                    let! last = eliminateDeadCode (onlySideEffects last)
                                    let! step = eliminateDeadCode (onlySideEffects step)
                                    let! first = eliminateDeadCode (onlySideEffects first)
                                    return Expr.Seq [first; step; last]
                                | _ ->
                                    let! last = eliminateDeadCode last
                                    let! step = eliminateDeadCode step
                                    let! first = eliminateDeadCode first
                                    return Expr.ForInteger(v, first, step, last, body)
                                    
                        }

                    return! EliminationState.fix iterate
//                    
//                    let! last = eliminateDeadCode last
//                    let! step = eliminateDeadCode step
//                    let! first = eliminateDeadCode first
//
//                    let! body = EliminationState.fix (eliminateDeadCode body)
//
//                    match body with
//                        | Unit -> 
//                            return Expr.Unit
//
//                        | _ ->
//                            return Expr.ForInteger(v, first, step, last, body)

                | WhileLoop(guard, body) ->
                    let iterate =
                        state {
                            let! body = eliminateDeadCode body
                            match body with
                                | Unit -> 
                                    return! eliminateDeadCode (onlySideEffects guard)
                                | _ ->
                                    let! guard = eliminateDeadCode guard
                                    return Expr.WhileLoop(guard, body)
                        }

                    return! EliminationState.fix iterate


                | Ignore e ->
                    let! e = eliminateDeadCode e
                    return Expr.Ignore e

                | AddressOf e ->
                    let! e = eliminateDeadCode e
                    return Expr.AddressOf(e)

                | AddressSet(e,v) ->
                    let! v = eliminateDeadCode v
                    let! e = eliminateDeadCode e
                    return Expr.AddressSet(e, v)

                | Application(l, a) ->
                    let! a = eliminateDeadCode a
                    let! l = eliminateDeadCode l
                    return Expr.Application(l, a)

                | Call(t, mi, args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                    let! t = t |> Option.mapS eliminateDeadCode

                    match t with
                        | Some t -> return Expr.Call(t, mi, args)
                        | None -> return Expr.Call(mi, args)

                | Coerce(e,t) ->
                    let! e = eliminateDeadCode e
                    return Expr.Coerce(e, t)

                | DefaultValue t ->
                    return e

                | FieldGet(None, f) -> 
                    return e

                | FieldGet(Some t, f) ->
                    let! t = eliminateDeadCode t
                    return Expr.FieldGet(t, f)

                | Lambda(v, b) ->
                    let! b = eliminateDeadCode b
                    return Expr.Lambda(v, b)
  
                | LetRecursive _ ->
                    return failwith "recursive bindings not implemented"
                    
                | Let(v,e,b) ->
                    let! b = eliminateDeadCode b
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        let! e = eliminateDeadCode e
                        return Expr.Let(v, e, b)

                    else
                        let! e = eliminateDeadCode (onlySideEffects e)
                        match e with
                            | Unit -> return b
                            | _ -> return Expr.Sequential(e, b)

                | NewArray(t, args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                    return Expr.NewArray(t, args)

                | NewDelegate(t, vars, e) ->
                    let! e = eliminateDeadCode e
                    return Expr.NewDelegate(t, vars, e)
  
                | NewObject(ctor, args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                    return Expr.NewObject(ctor, args)

                | NewRecord(t, args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                    return Expr.NewRecord(t, args)

                | NewTuple(args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                    return Expr.NewTuple(args)

                | NewUnionCase(ci, args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                    return Expr.NewUnionCase(ci, args)

                | PropertyGet(None, pi, idx) ->
                    let! idx = idx |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                    return Expr.PropertyGet(pi, idx)

                | PropertyGet(Some t, pi, idx) ->
                    let! idx = idx |> List.rev |> List.mapS eliminateDeadCode |> State.map List.rev
                    let! t = eliminateDeadCode t
                    return Expr.PropertyGet(t, pi, idx)
                    
                | QuoteRaw _ | QuoteTyped _ -> 
                    return failwith "not implemented"

                | Sequential(l, r) ->
                    let! r = eliminateDeadCode r
                    let! l = eliminateDeadCode l
                    match l, r with
                        | Unit, r   -> return r
                        | l, Unit   -> return l
                        | l, r      -> return Expr.Sequential(l, r)

                | TryWith _ | TryFinally _ -> 
                    return failwith "not implemented"

                | TupleGet(t, i) ->
                    let! t = eliminateDeadCode t
                    return Expr.TupleGet(t, i)

                | TypeTest(e, t) ->
                    let! e = eliminateDeadCode e
                    return Expr.TypeTest(e, t)

                | UnionCaseTest(e, ci) ->
                    let! e = eliminateDeadCode e
                    return Expr.UnionCaseTest(e, ci)

                | Value _ ->
                    return e


                | Var v ->
                    do! EliminationState.useVar v
                    return e

                | _ ->
                    return failwithf "[FShade] unexpected expression %A" e
        }


