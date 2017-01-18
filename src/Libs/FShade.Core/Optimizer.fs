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


    let hasSideEffect (e : Expr) =
        // TODO: validate
        let rec hasSideEffect (locals : Set<Var>) (e : Expr) =
            match e with
                | Let(v, e, b) ->
                    hasSideEffect locals e || 
                    hasSideEffect (Set.add v locals) b
                  
                | LetRecursive(bindings, body) ->
                    bindings |> List.exists (snd >> hasSideEffect locals) ||
                    hasSideEffect (bindings |> List.map fst |> Set.ofList) body
                 
                | FieldSet _ -> true
                | PropertySet _ -> true   
                | VarSet(v,_) when not (Set.contains v locals) -> true
                | Call(_, mi, _) when mi.ReturnType = typeof<unit> -> true


                | ShapeCombination(_, args) -> args |> List.exists (hasSideEffect locals)
                | ShapeVar _ -> false
                | ShapeLambda _ -> false 

        hasSideEffect Set.empty e

    let inline isPure (e : Expr) = not (hasSideEffect e)


    type DependencyState =
        {
            staticDeps : Set<Var>
            controlFlowDeps : Set<Var>
            dependencies : Map<Var, Set<Var>>
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DependencyState =

        let empty = 
            {
                staticDeps = Set.empty
                controlFlowDeps = Set.empty
                dependencies = Map.empty
            }

        let add (v : Var) (others : Set<Var>) =
            State.modify (fun s ->
                match Map.tryFind v s.dependencies with
                    | Some old -> { s with dependencies = Map.add v (Set.union old others) s.dependencies }
                    | None -> { s with dependencies = Map.add v others s.dependencies }
            )

        let set (v : Var) (others : Set<Var>) =
            State.modify (fun s ->
                { s with dependencies = Map.add v others s.dependencies }
            )

        let get (v : Var) =
            State.get |> State.map (fun s ->
                match Map.tryFind v s.dependencies with
                    | Some all -> Set.add v all |> Set.union s.controlFlowDeps
                    | None -> Set.add v s.controlFlowDeps
            )

        let addStatic (d : Set<Var>) =
            State.modify (fun s -> { s with staticDeps = Set.union s.staticDeps d })

        type UseBuilder(deps : Set<Var>) =
            inherit StateBuilder()

            member x.Run(m : State<DependencyState, 'a>) =
                state {
                    let! old = State.get
                    do! State.modify (fun s -> { s with controlFlowDeps = Set.union deps s.controlFlowDeps })
                    let! res = m
                    do! State.modify (fun s -> { s with controlFlowDeps = old.controlFlowDeps })
                    return res
                }


        let useDeps (deps : Set<Var>)  =
            UseBuilder(deps)


    // let x = a + b + c
    // a <- a + x

    // let mutable a = ()
    //
    // for i in 0 .. 10 do
    //      a <- if a < 0 then b <- b + 1
    //
    // a

    let rec buildDependencies (e : Expr) : State<DependencyState, Set<Var>> =
        state {
            match e with
                | Let(v,e,b) ->
                    let! e = buildDependencies e
                    do! DependencyState.add v e
                    let! b = buildDependencies b
                    return Set.union e b |> Set.remove v

                | VarSet(v, e) ->
                    let! e = buildDependencies e
                    do! DependencyState.set v e
                    return Set.empty

                | Sequential(l, r) ->
                    let! l = buildDependencies l
                    return! buildDependencies r
                    
                | IfThenElse(cond, i, e) ->
                    let! cond = buildDependencies cond

                    return!
                        DependencyState.useDeps cond {
                            let! i = buildDependencies i
                            let! e = buildDependencies e
                            return Set.union i e
                        }

                | ForInteger(v, first, step, last, body) ->
                    let! first = buildDependencies first
                    let! step = buildDependencies step
                    let! last = buildDependencies last
                    let deps = Set.unionMany [first; step; last]

                    return!
                        DependencyState.useDeps deps {
                            let! _ = buildDependencies body
                            return Set.empty
                        }

                | Value _ ->
                    return Set.empty 

                | Call(None, mi, args) when e.Type = typeof<unit> ->
                    let! argDeps = args |> List.mapS buildDependencies |> State.map Set.unionMany
                    do! DependencyState.addStatic argDeps
                    return Set.empty

                | FieldSet(Some e,_,v) ->
                    let! eDeps = buildDependencies e
                    let! vDeps = buildDependencies v
                    do! DependencyState.addStatic (Set.union eDeps vDeps)
                    return Set.empty

                | PropertySet(Some e, _, idx, v) ->
                    let! eDeps = buildDependencies e
                    let! idx = idx |> List.mapS buildDependencies |> State.map Set.unionMany
                    let! vDeps = buildDependencies v
                    do! DependencyState.addStatic (Set.unionMany [eDeps; idx; vDeps])
                    return Set.empty
                

                | ShapeCombination(_, args) ->
                    let! deps = args |> List.mapS buildDependencies
                    if e.Type = typeof<unit> then return Set.empty
                    else return Set.unionMany deps

                | ShapeLambda(v, b) ->
                    let! b = buildDependencies b
                    return b |> Set.remove v

                | ShapeVar(v) ->
                    return! DependencyState.get v
                    
        }


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

 
    let rec eliminateDeadCode (e : Expr) : State<EliminationState, Expr> =
        state {
            match e with

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

                | FieldSet(None, f, value) ->
                    let! value = eliminateDeadCode value
                    return Expr.FieldSet(f, value)

                | FieldSet(Some t, f, value) ->
                    let! value = eliminateDeadCode value
                    let! t = eliminateDeadCode t
                    return Expr.FieldSet(t, f, value)

                | ForInteger(v, first, step, last, body) ->
                    let dependencies, _ = buildDependencies body |> State.run DependencyState.empty
                   
                    let! last = eliminateDeadCode last
                    let! step = eliminateDeadCode step
                    let! first = eliminateDeadCode first
                   
                    do! State.modify (fun s ->
                            let newUsed = 
                                Set.union dependencies.staticDeps s.usedVariables |> Set.map (fun v ->
                                    match Map.tryFind v dependencies.dependencies with
                                        | Some deps -> Set.add v deps
                                        | None -> Set.singleton v
                                ) |> Set.unionMany
                            { s with usedVariables = newUsed }
                        )

                    let! body = eliminateDeadCode body

                    match body with
                        | Unit -> 
                            return Expr.Unit

                        | _ ->
                            return Expr.ForInteger(v, first, step, last, body)

                | IfThenElse(cond, i, e) ->
                    let! elseState, e = State.withLocalState (eliminateDeadCode e)
                    let! ifState, i = State.withLocalState (eliminateDeadCode i)
                    do! State.put (EliminationState.merge ifState elseState)

                    let! condState, cond = State.withLocalState (eliminateDeadCode cond)

                    match i, e with
                        | Unit, Unit when isPure cond -> 
                            return Expr.Unit
                        | _ ->
                            do! State.put condState
                            return Expr.IfThenElse(cond, i, e)

                | Lambda(v, b) ->
                    let! b = eliminateDeadCode b
                    return Expr.Lambda(v, b)
  
                | LetRecursive _
                | NewDelegate _ ->
                    return failwith "[FShade] recursi
                    ve let bindings not supported"
  
                | Let(v,e,b) ->
                    let! b = eliminateDeadCode b
                    let! e = eliminateDeadCode e
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        return Expr.Let(v, e, b)

                    elif hasSideEffect e then
                        return Expr.Sequential(e, b)

                    else
                        return b

                
                | WriteOutput(name, idx, value) -> 
                    let! value = eliminateDeadCode value
                    let! idx = idx |> Option.mapS eliminateDeadCode
                    match idx with
                        | Some idx -> return Expr.WriteOutput(name, idx, value)
                        | None -> return Expr.WriteOutput(name, value)
        
                | Var v ->
                    do! EliminationState.useVar v
                    return e

                | Value _ ->
                    return e

                | Sequential(l, r) ->
                    let! r = eliminateDeadCode r
                    let! l = eliminateDeadCode l
                    match l, r with
                        | Unit, r   -> return r
                        | l, Unit   -> return l
                        | l, r      -> return Expr.Sequential(l, r)

                | VarSet(v, e) ->
                    
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        let! e = eliminateDeadCode e
                        //let! _,e = State.withLocalState (eliminateDeadCode e)
                        //if not vUsed then do! EliminationState.remVar v
                        //let! vUsed = EliminationState.isUsed v
                        return Expr.VarSet(v, e)
                    else
                        if hasSideEffect e then
                            return! eliminateDeadCode e
                        else
                            return Expr.Unit

                | _ ->
                    return failwith ""
        }


