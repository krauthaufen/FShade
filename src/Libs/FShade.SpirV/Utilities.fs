namespace FShade.SpirV

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade.Imperative

type SpirVState =
    {
        currentId           : uint32
        valueIds            : HashMap<obj, uint32>
        uniformIds          : Map<string, uint32 * list<uint32>>
        fieldIds            : HashMap<CType, HashMap<string, int>>
        reversedInstuctions : list<Instruction>
        currentBinding      : uint32
        currentSet          : uint32
        imports             : Map<string, uint32>
    }

type SpirV<'a> = State<SpirVState, 'a>

[<AutoOpen>]
module ``SpirV Builders`` =
    type SpirVBuilder() =
        inherit StateBuilder()

        member x.Yield(i : Instruction) =
            State.modify (fun s ->
                { s with reversedInstuctions = i :: s.reversedInstuctions }
            )

        member x.Run(m : SpirV<'a>) : SpirV<'a> =
            m


    let spirv = SpirVBuilder()

module SpirV =
    let getId (a : 'a) : SpirV<uint32> =
        State.get |> State.map (fun s -> HashMap.find (a :> obj) s.valueIds)
            
    let tryGetId (a : 'a) : SpirV<Option<uint32>> =
        State.get |> State.map (fun s -> HashMap.tryFind (a :> obj) s.valueIds)
            
    let setId (a : 'a) (id : uint32) : SpirV<unit> =
        State.modify (fun s -> { s with valueIds = HashMap.add (a :> obj) id  s.valueIds })

            
    let setUniformId (name : string) (var : uint32) (fields : list<uint32>) : SpirV<unit> =
        State.modify (fun s -> { s with uniformIds = Map.add name (var, fields) s.uniformIds })


    type CachedSpirVBuilder(key : obj) =
        inherit StateBuilder()

        member x.Yield(i : Instruction) =
            State.modify (fun s ->
                { s with reversedInstuctions = i :: s.reversedInstuctions }
            )

        member x.Run(m : SpirV<uint32>) : SpirV<uint32> =
            state {
                let! v = tryGetId key 
                match v with
                    | Some id -> 
                        return id
                    | None ->
                        let! id = m
                        do! setId key id
                        return id
            }



    let cached (v : 'a) =
        CachedSpirVBuilder(v :> obj)

    let id = 
        State.custom (fun s ->
            let id = s.currentId
            { s with currentId = id + 1u }, id
        )

    let setFieldId (t : CType) (name : string) (id : int) =
        State.modify (fun s ->
            match HashMap.tryFind t s.fieldIds with
                | Some ids ->
                    { s with fieldIds = HashMap.add t (HashMap.add name id ids) s.fieldIds }
                | None ->
                    { s with fieldIds = HashMap.add t (HashMap.ofList [name,id]) s.fieldIds }
        )

    let tryGetFieldId (t : CType) (name : string) =
        State.get |> State.map (fun s ->
            match HashMap.tryFind t s.fieldIds with
                | Some ids ->
                    HashMap.tryFind name ids
                | None ->
                    None
        )

    let newBinding : SpirV<uint32> =
        State.custom (fun s ->
            let c = s.currentBinding
            { s with currentBinding = c + 1u }, c
        )

    let newSet : SpirV<uint32> =
        State.custom (fun s ->
            let c = s.currentSet
            { s with currentSet = c + 1u; currentBinding = 0u }, c
        )

    let import (name : string) =
        state {
            let! s = State.get
            match Map.tryFind name s.imports with
                | Some id -> return id
                | None ->
                    let! id = id
                    do! State.put { s with imports = Map.add name id s.imports; reversedInstuctions = OpExtInstImport(id, name) :: s.reversedInstuctions }
                    return id
        }