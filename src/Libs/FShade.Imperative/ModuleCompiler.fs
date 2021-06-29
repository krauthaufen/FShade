namespace FShade.Imperative

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.CompilerServices

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Aardvark.Base
open Aardvark.Base.TypeInfo
open Aardvark.Base.TypeInfo.Patterns

open FShade
open FShade.Imperative
open FSharp.Data.Adaptive

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ModuleCompiler =
    open Aardvark.Base.Monads.State
    open Compiler

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ValueCompiler =
        open SimpleOrder

        type GraphNode(definition : CValueDef, conditional : option<string>, dependencies : HashSet<GraphNode>) =
            let mutable sortKey : Option<SortKey> = None
            let mutable dependencies = dependencies
            member x.Conditional = conditional
            member x.Definition = definition
            member x.Dependencies 
                with get() = dependencies
                and set d = dependencies <- d

            member x.SortKey 
                with get() = sortKey
                and set v = sortKey <- v

            interface IComparable with
                member x.CompareTo o =
                    match o with
                        | :? GraphNode as o -> compare sortKey o.SortKey
                        | _ -> failwithf "[FShade] cannot compare GraphNode to %A" o


            override x.Equals o =
                System.Object.ReferenceEquals(x,o)

            override x.GetHashCode() =
                System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x)
      

            member x.AssignOrder(afterTime : SortKey) =
                match sortKey with
                    | Some s when s < afterTime -> s
                    | _ -> 
                        let t = afterTime.Clock.Before afterTime
                        sortKey <- Some t
                        for d in dependencies do
                            d.AssignOrder t |> ignore

                        t

        type GraphState =
            {
                moduleState     : ModuleState
                cache           : Dictionary<obj, GraphNode>
            }

        [<AutoOpen>]
        module private Helpers = 
            let rec build (globals : GraphNode) (key : 'a) (conditional : option<string>) (compile : 'a -> State<CompilerState, CValueDef>) : State<GraphState, GraphNode> =
                state {
                    let! (state : GraphState) = State.get
                    let cache : Dictionary<obj, GraphNode> = state.cache

                    match cache.TryGetValue (key :> obj) with
                        | (true, v) -> 
                            return v
                        | _ ->
                            let mutable s = emptyState state.moduleState
                            let def = compile(key).Run(&s)
                            do! State.modify (fun gs -> { gs with moduleState = s.moduleState })

                            let difference (l : HashMap<'k, 'v>) (r : HashMap<'k, 'x>) =
                                HashMap.choose2 (fun _ l r ->
                                    match r with
                                        | Some _ -> None
                                        | _ -> l
                                ) l r

                            let localFunctions      = s.usedFunctions |> HashMap.toSeq |> Seq.map snd |> Seq.toList
                            let globalFunctions     = s.usedGlobalFunctions |> HashSet.toList //difference s.moduleState.globalFunctions state.moduleState.globalFunctions |> HashMap.toSeq |> Seq.map snd |> Seq.toList
                            let globalConstants     = s.usedConstants |> HashSet.toList //difference s.moduleState.globalConstants state.moduleState.globalConstants |> HashMap.toSeq |> Seq.map snd |> Seq.toList
                            
                            
                            let node = GraphNode(def, conditional, HashSet.empty)
                            cache.[key] <- node

                            let! localFunctions = localFunctions |> List.mapS (ofFunction globals)
                            let! globalFunctions = globalFunctions |> List.mapS (ofFunction globals)
                            let! constants = globalConstants |> List.mapS (ofConstant globals)

                            let dependencies = 
                                List.concat [
                                    (if HashSet.isEmpty s.usedGlobals then [] else [globals])
                                    localFunctions
                                    globalFunctions
                                    constants
                                ]

                            node.Dependencies <- HashSet.ofList dependencies
                            

                            do! State.modify (fun gs -> 
                                { gs with moduleState = { gs.moduleState with globalFunctions = s.moduleState.globalFunctions; globalConstants = s.moduleState.globalConstants } }
                            )

                            return node

                }

            and ofFunction (globals : GraphNode) (f : FunctionDefinition) =
                build globals f None compileFunctionS

            and ofConstant (globals : GraphNode) (f : ConstantDefinition) =
                build globals f None compileConstantS


            let ofEntry (e : EntryPoint) =
                state {
                    let! (s : GraphState) = State.get
                    let! globals = build Unchecked.defaultof<_> (e,e.uniforms) None (snd >> compileUniformsS)

                    let globalNames = e.uniforms |> List.map (fun u -> u.uniformName) |> Set.ofList

                    do! State.modify (fun s -> { s with moduleState = { s.moduleState with ModuleState.globalParameters = globalNames } })
                    let! root = build globals e e.conditional compileEntryS

                    let root = GraphNode(root.Definition, e.conditional, HashSet.add globals root.Dependencies)

                    do! State.modify (fun s -> { s with moduleState = { s.moduleState with ModuleState.globalParameters = Set.empty } })

                    return e, globals, root
                }

            type Meta =
                | Ifdef of string
                | Endif of string


            let flatten (graphs : list<EntryPoint * GraphNode * GraphNode>) =
                
                let outEdges = 
                    let outEdges = Dict()
                    let rec run (g : GraphNode) =
                        for d in g.Dependencies do
                            let set = outEdges.GetOrCreate(d, fun _ -> System.Collections.Generic.HashSet())
                            set.Add g |> ignore
                    for (_,_,d) in graphs do run d
                    outEdges
                    |> Seq.map (fun (KeyValue(a, b)) ->
                        (a, HashSet.ofSeq b)
                    )
                    |> HashMap.ofSeq

                let terminals =
                    graphs |> List.filter (fun (_,_,d) -> not (outEdges.ContainsKey d))

                let mutable groups =
                    terminals |> List.map (fun (_,_,d) ->
                        HashSet.single d
                    )

                let allNodes =
                    let rec run (acc : HashSet<GraphNode>) (g : GraphNode) =
                        if HashSet.contains g acc then
                            acc
                        else
                            let mutable res = HashSet.add g acc
                            for d in g.Dependencies do
                                res <- run res d
                            res
                    let mutable result = HashSet.empty
                    for (_,_,d) in graphs do
                        result <- run result d
                    result

                let growGroups (gs : list<HashSet<GraphNode>>) =
                    gs
                    |> List.map (fun set ->
                        set |> HashSet.collect (fun d -> 
                            let additional =
                                d.Dependencies |> HashSet.filter (fun dep ->
                                    match HashMap.tryFind dep outEdges with
                                    | Some oe -> 
                                        HashSet.difference oe set
                                        |> HashSet.isEmpty
                                    | _ -> 
                                        true
                                )
                            HashSet.add d additional
                        )
                    )

                let mutable lastGroups = []
                while lastGroups <> groups do
                    lastGroups <- groups
                    groups <- growGroups groups

                let noGroup = 
                    allNodes |> HashSet.filter (fun n ->
                        groups |> List.forall (fun s -> not (HashSet.contains n s))
                    )

                let toposort (elements : HashSet<GraphNode>) =
                    let mutable dependents = HashMap.empty
                    let mutable dependencies = HashMap.empty

                    for e in elements do    
                        let deps = HashSet.intersect elements e.Dependencies
                        dependencies <- HashMap.add e deps dependencies
                        for d in HashSet.intersect elements e.Dependencies do
                            dependents <-   
                                dependents |> HashMap.alter d (function
                                    | Some o -> Some (HashSet.add e o)
                                    | None -> Some (HashSet.single e)
                                )

                    let mutable result = []

                    while dependencies.Count > 0 do
                        let noDep = dependencies |> HashMap.filter (fun e ds -> ds.IsEmpty) |> HashMap.keys
                        
                        result <- HashSet.toList noDep :: result
                        for d in noDep do dependencies <- HashMap.remove d dependencies
                        dependencies <-
                            dependencies |> HashMap.map (fun _ vs ->
                                HashSet.difference vs noDep
                            )

                    List.rev result
                    |> List.concat


                let all = 
                    [
                        yield! 
                            toposort noGroup
                            |> List.map (fun d -> d.Definition)

                        for g in groups do
                            let g = toposort g
                            let conditionals =
                                g |> Seq.choose (fun g -> g.Conditional) |> HashSet.ofSeq
                            if conditionals.Count = 1 then
                                let c = conditionals |> Seq.head
                                yield CConditionalDef(c, g |> List.map (fun d -> d.Definition))
                            else
                                yield! g |> List.map (fun d -> d.Definition)
                    ]
                all

                //let dependencies = Dict<GraphNode, System.Collections.Generic.HashSet<GraphNode>>()

                //let rec run (node : GraphNode) =
                //    let dep = dependencies.GetOrCreate(node, fun _ -> System.Collections.Generic.HashSet())
                //    for d in node.Dependencies do
                //        dep.Add d |> ignore
                //        run d

                //for (_, _, def) in graphs do
                //    run def


                //let mutable result = []

                //while dependencies.Count > 0 do
                //    let noDep = 
                //        dependencies |> Seq.choose (fun (KeyValue(k, v)) ->
                //            if v.Count = 0 then
                //                Some k
                //            else
                //                None
                //        )
                //        |> System.Collections.Generic.HashSet

                //    result <- result @ Seq.toList noDep

                //    for n in noDep do dependencies.Remove n |> ignore
                //    for (KeyValue(_, v)) in Seq.toArray dependencies do 
                //        for n in noDep do v.Remove n |> ignore

                //printfn "%A" result
                




                //let order = SimpleOrder.create()
                //let defines = Dictionary<SortKey, Meta>()
                //let mutable lastRootDef = order.Root



                //let rec flattenDependencies (stack : HashSet<GraphNode>) (globals : GraphNode) (g : GraphNode) =
                //    if HashSet.contains g stack then
                //        failwithf "[FShade] found cyclic definition for %A" g.Definition

                //    let dependencies = HashSet.toList g.Dependencies
                //    match dependencies with
                //        | [] -> 
                //            match g.SortKey with
                //            | Some _ -> ()
                //            | None ->
                //                let t = order.After lastRootDef
                //                lastRootDef <- t
                //                g.SortKey <- Some t

                //        | deps ->
                //            for d in dependencies do flattenDependencies (HashSet.add g stack) globals d
                //            let max = dependencies |> List.map (fun d -> d.SortKey.Value) |> List.max
                //            match g.SortKey with
                //            | Some o when o < max -> g.SortKey <- Some (order.After max)
                //            | None -> g.SortKey <- Some (order.After max)
                //            | _ -> ()

                //let mutable afterLastEntry = order.Root
                //for (entry, globals, definition) in graphs do
                    
                //    let t0 = order.After afterLastEntry
      
                //    globals.SortKey <- Some t0
                //    flattenDependencies HashSet.empty globals definition
                //    if definition.SortKey.Value < t0 then
                //        definition.SortKey <- Some (order.After t0)

                //    afterLastEntry <- order.After definition.SortKey.Value

                //    match entry.conditional with
                //    | Some d -> 
                //        defines.[globals.SortKey.Value] <- Ifdef d
                //        defines.[definition.SortKey.Value] <- Endif d
                //    | _ ->
                //        ()


                //let allDefinitions = 
                //    let list = List()
                //    let rec visit (defs : System.Collections.Generic.HashSet<GraphNode>) (d : GraphNode) =
                //        if defs.Add d then
                //            list.Add d
                //            for dep in d.Dependencies do visit defs dep

                //    let set = System.Collections.Generic.HashSet()
                //    for (_, g, d) in graphs do visit set g; visit set d

                //    list |> CSharpList.toArray |> Array.sort
                

                //let tryGetEvent (t : SortKey) =
                //    match defines.TryGetValue t with
                //        | (true, v) -> Some v
                //        | _ -> None

                //let res = List()
                //let mutable currentIfDef = None
                //let current = List()
                //for n in allDefinitions do
                //    match tryGetEvent n.SortKey.Value with
                //        | Some (Ifdef d) -> 
                //            if current.Count > 0 then res.AddRange current
                //            currentIfDef <- Some d
                //            current.Clear()
                //            match n.Definition with
                //                | CUniformDef [] -> ()
                //                | def -> current.Add def

                //        | Some (Endif d) ->
                //            match n.Definition with
                //                | CUniformDef [] -> ()
                //                | def -> current.Add def
                //            currentIfDef <- None
                //            res.Add (CConditionalDef(d, CSharpList.toList current))
                //            current.Clear()

                //        | None ->
                //            match n.Definition with
                //                | CUniformDef [] -> ()
                //                | def -> current.Add def
                        
                //if current.Count > 0 then
                //    match currentIfDef with   
                //        | Some d -> res.Add(CConditionalDef(d, CSharpList.toList current))
                //        | None -> res.AddRange(current)



                //CSharpList.toList res

        let ofModule (backend : Backend) (m : Module) =
            let compile =
                state {
                    let! nodes = m.entries |> List.mapS ofEntry
                    return nodes
                }

            let mutable state = 
                { 
                    moduleState =
                        {
                            backend             = backend
                            constantIndex       = 0
                            usedTypes           = HashMap.empty
                            globalFunctions     = HashMap.empty
                            globalConstants     = HashMap.empty
                            globalParameters    = Set.empty
                            tryGetOverrideCode  = m.tryGetOverrideCode
                            globalNameIndices   = Map.empty
                        }
                    cache = Dictionary() 
                }

            let nodes = compile.Run(&state)
            let usedTypes = state.moduleState.usedTypes |> HashMap.toSeq |> Seq.map snd |> Seq.toList
            flatten nodes, usedTypes

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TypeCompiler =
        open SimpleOrder

        type TypeGraphNode(definition : Option<CTypeDef>, dependencies : HashSet<TypeGraphNode>) =
            let mutable sortKey : SortKey = Unchecked.defaultof<SortKey>

            member x.Definition = definition
            member x.Dependencies = dependencies

            member x.SortKey = sortKey

            interface IComparable with
                member x.CompareTo o =
                    match o with
                        | :? TypeGraphNode as o -> compare sortKey o.SortKey
                        | _ -> failwithf "[FShade] cannot compare GraphNode to %A" o


            override x.Equals o =
                System.Object.ReferenceEquals(x,o)

            override x.GetHashCode() =
                System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x)
      

            member x.AssignOrder(o : Order) =
                match HashSet.toList dependencies with
                    | [] -> 
                        sortKey <- o.After(o.Root)

                    | deps ->
                        let max = deps |> List.map (fun d -> d.AssignOrder o) |> List.max
                        sortKey <- o.After(max)

                sortKey

        type TypeGraphState =
            {
                cache : Dictionary<CType, Option<TypeGraphNode>>
            }

        [<AutoOpen>]
        module private Helpers =
            let allDirectChildTypes (ctype : CType) =
                match ctype with
                    | CStruct(_, fields, _) -> 
                        fields |> List.map fst |> HashSet.ofList

                    | CVector(t,_) 
                    | CMatrix(t, _, _) 
                    | CPointer(_, t) 
                    | CArray(t,_) ->
                        HashSet.ofList [t]

                    | _ ->
                        HashSet.empty

            let rec ofType (t : CType) =
                state {
                    let! (s : TypeGraphState) = State.get
                    match s.cache.TryGetValue t with
                        | (true, n) ->
                            return n
                        | _ ->
                            let! used = t |> allDirectChildTypes |> HashSet.toList |> List.chooseS ofType |>> HashSet.ofList

                            match t with
                                | CStruct(name, fields, _) ->
                                    let def = CStructDef(name, fields)
                                    let node = TypeGraphNode(Some def, used)
                                    s.cache.[t] <- Some node
                                    return Some node

                                | _ ->
                                    if HashSet.isEmpty used then
                                        return None
                                    else
                                        return Some <| TypeGraphNode(None, used)


                }
            
            let flatten (graphs : list<TypeGraphNode>) =
                let order = SimpleOrder.create()
                for t in graphs do t.AssignOrder order |> ignore

                let rec visit (defs : System.Collections.Generic.HashSet<TypeGraphNode>) (d : TypeGraphNode) =
                    if defs.Add d then
                        for dep in d.Dependencies do visit defs dep

                let set = System.Collections.Generic.HashSet()
                for t in graphs do visit set t

                set |> Seq.toArray |> Array.sort |> Array.toList |> List.choose (fun t -> t.Definition)

        let ofTypes (types : list<CType>) =
            let compile =
                state {
                    let! nodes = types |> List.chooseS ofType
                    return nodes
                }

            let mutable state = 
                { 
                    cache = Dictionary() 
                }
            
            compile.Run(&state) |> flatten

    let compile (backend : Backend) (m : Module) =
        let values, types = ValueCompiler.ofModule backend m
        let types = TypeCompiler.ofTypes types

        {
            CModule.cuserData = m.userData
            CModule.types = types
            CModule.values = values
        }


[<AbstractClass; Sealed; Extension>]
type Compiler private() =

    [<Extension>]
    static member Compile(backend : Compiler.Backend, m : Module) =
        ModuleCompiler.compile backend m
