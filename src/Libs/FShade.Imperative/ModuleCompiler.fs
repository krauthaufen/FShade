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

        type GraphNode(definitions : CValueDef list, conditional : option<string>, dependencies : HashSet<GraphNode>) =
            let mutable sortKey : Option<SortKey> = None
            let mutable dependencies = dependencies
            member x.Conditional = conditional
            member x.Definitions = definitions
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
            let rec build (globals : GraphNode) (key : 'a) (conditional : option<string>) (compile : 'a -> State<CompilerState, CValueDef list>) : State<GraphState, GraphNode> =
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
                build globals f None (compileFunctionS >> State.map List.singleton)

            and ofConstant (globals : GraphNode) (f : ConstantDefinition) =
                build globals f None (compileConstantS >> State.map List.singleton)


            let ofEntry (e : EntryPoint) =
                state {
                    let! globals =
                        build Unchecked.defaultof<_> e.uniforms None (compileUniformsS >> State.map List.singleton)

                    let globalNames =
                        e.uniforms
                        |> List.map (fun u -> u.uniformName)
                        |> Set.ofList

                    do! State.modify (fun s -> { s with moduleState = { s.moduleState with ModuleState.globalParameters = globalNames } })

                    let! root =
                        let key = (e, e.raytracingData)
                        build globals key e.conditional (fun (entry, raydata) ->
                            state {
                                let! e = compileEntryS entry
                                let! r = compileRaytracingDataS raydata
                                return [e; r]
                            }
                        )

                    let root = GraphNode(root.Definitions, e.conditional, HashSet.add globals root.Dependencies)

                    do! State.modify (fun s -> { s with moduleState = { s.moduleState with ModuleState.globalParameters = Set.empty } })

                    return e, globals, root
                }

            type Meta =
                | Ifdef of string
                | Endif of string


            let flatten (graphs : list<EntryPoint * 'a * GraphNode>) =
                

                
                let groups =
                    let rec setColor (c : int) (colors : HashMap<GraphNode, int>) (node : GraphNode) =
                        let mutable colors = colors
                        match HashMap.tryFind node colors with
                        | Some oc ->
                            if oc <> c then
                                colors <- HashMap.add node -1 colors
                                for d in node.Dependencies do
                                    colors <- setColor -1 colors d
                                colors
                            else
                                colors
                        | None ->
                            colors <- HashMap.add node c colors
                            for d in node.Dependencies do
                                colors <- setColor c colors d
                            colors

                    let entries = Seq.toArray graphs

                    let mutable colors = HashMap.empty
                    let mutable i = 0
                    for (_,_,d) in entries do
                        colors <- setColor i colors d
                        i <- i + 1

                    colors 
                    |> Seq.groupBy snd
                    |> Seq.map (fun (c,grp) -> 
                        let entry =
                            if c >= 0 then 
                                let (e, _, _) = entries.[c]
                                Some e
                            else 
                                None
                        let nodes = grp |> Seq.map fst |> HashSet.ofSeq
                        entry, nodes
                    )
                    |> HashMap.ofSeq

                let globalGroup =
                    match HashMap.tryFind None groups with
                    | Some g -> g
                    | None -> HashSet.empty

                let entryGroups =
                    groups 
                    |> HashMap.toArray 
                    |> Array.choose (fun (e, g) ->
                        match e with
                        | Some e -> Some (e, g)
                        | None -> None
                    )
                    |> HashMap.ofArray

                let toposort (set : HashSet<GraphNode>) =
                    
                    let outEdges = 
                        let mutable outEdges = HashMap.empty
                        let rec run (g : GraphNode) =
                            for d in g.Dependencies do
                                if HashSet.contains d set then
                                    outEdges <- 
                                        outEdges |> HashMap.alter d (function
                                            | Some o -> Some (HashSet.add g o)
                                            | None -> Some (HashSet.single g)
                                        )
                        for d in set do run d
                        outEdges

                    let rec run (acc : list<_>) (outEdges : HashMap<GraphNode, HashSet<GraphNode>>) (set : HashSet<GraphNode>) =
                        if HashSet.isEmpty set then
                            acc
                        else
                            let terminals = set |> HashSet.filter (fun n -> not (HashMap.containsKey n outEdges))
                            let rest = HashSet.difference set terminals

                            let newEdges =
                                outEdges |> HashMap.choose (fun _ ds ->
                                    let rest = HashSet.difference ds terminals
                                    if HashSet.isEmpty rest then None
                                    else Some rest
                                )


                            run (terminals :: acc) newEdges rest

                    run [] outEdges set


                let inline sortKey (n : GraphNode) =
                    n.Definitions |> List.map (fun d -> d.SortKey) |> String.concat "; "

                [
                    for es in toposort globalGroup do
                        yield! 
                            es 
                            |> HashSet.toList 
                            |> List.sortBy sortKey 
                            |> List.collect (fun n -> n.Definitions)


                    for (entry, _, _) in graphs do
                        match HashMap.tryFind entry entryGroups with
                        | Some g ->
                            let definitions =
                                let ray, defs =
                                    toposort g
                                    |> List.collect (
                                        HashSet.toList >>
                                        List.sortBy sortKey >>
                                        List.collect (fun d -> d.Definitions)
                                    )
                                    |> List.partition (function CRaytracingDataDef _ -> true | _ -> false)

                                // Ray data definitions must be first and not moved out of the entry group
                                // Maybe there is a more elegant way to do this?
                                List.concat [ray; defs]

                            match entry.conditional with
                            | Some c ->
                                yield CConditionalDef(c, definitions)
                            | None ->
                                yield! definitions
                        | _ ->
                            ()
                ]
              

        let ofModule (backend : Backend) (m : Module) =
            let compile =
                state {
                    let! nodes = m.Entries |> List.mapS ofEntry
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
                            tryGetOverrideCode  = m.TryGetOverrideCode
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

                set
                |> Seq.toArray
                |> Array.sort
                |> Array.toList
                |> List.choose (fun t -> t.Definition)
                |> List.distinct
                |> List.groupBy (fun (CStructDef(n, _)) -> n)
                |> List.map (fun (n, defs) ->
                    if defs.Length > 1 then
                        let nl = Environment.NewLine
                        let defs = defs |> List.map string |> String.concat nl
                        failwithf $"[FShade] Multiple conflicting type definitions with name {n}:{nl}{defs}"
                    else
                        defs.Head
                )

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
            CModule.cuserData = m.UserData
            CModule.types = types
            CModule.values = values
        }


[<AbstractClass; Sealed; Extension>]
type Compiler private() =

    [<Extension>]
    static member Compile(backend : Compiler.Backend, m : Module) =
        ModuleCompiler.compile backend m
