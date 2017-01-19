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

    let rec (|LExpr|_|) (e : Expr) =
        // TODO: complete?
        match e with
            | Var v -> Some v
            | FieldGet(Some (LExpr v), fi) -> Some v
            | PropertyGet(Some (LExpr v), _, _) -> Some v
            | GetArray(LExpr a, i) -> Some a
            | _ -> None


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

                | Call(t, mi, args) ->
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
                    Log.warn "[FShade] found FieldSet on unknown expression: %A" t
                    let! value = eliminateDeadCodeS value
                    let! t = eliminateDeadCodeS t
                    return Expr.FieldSet(t, f, value)

                | PropertySet(Some t, pi, idx, value) ->
                    Log.warn "[FShade] found PropertySet on unknown expression: %A" t
                    let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    let! value = eliminateDeadCodeS value
                    let! t = eliminateDeadCodeS t
                    return Expr.PropertySet(t, pi, value, idx)


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
//                    
//                    let! last = eliminateDeadCodeS last
//                    let! step = eliminateDeadCodeS step
//                    let! first = eliminateDeadCodeS first
//
//                    let! body = EliminationState.fix (eliminateDeadCodeS body)
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

                | Call(t, mi, args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    let! t = t |> Option.mapS eliminateDeadCodeS

                    match t with
                        | Some t -> return Expr.Call(t, mi, args)
                        | None -> return Expr.Call(mi, args)

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
