﻿namespace FShade.SpirV

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade.Imperative
open FSharp.Data.Adaptive

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

    let getUniformId (name : string) : SpirV<uint32 * list<uint32>> =
        State.get |> State.map (fun s -> Map.find name s.uniformIds)

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

open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape

module internal MethodTable =
    let rec tryGetMethod (e : Expr) =
        match e with
            | Call(_,mi,_) -> Some mi
            | PropertyGet(_,pi,_) -> Some pi.GetMethod

            | ShapeVar _ -> None
            | ShapeLambda(_,b) -> tryGetMethod b
            | ShapeCombination(_,args) -> args |> List.tryPick tryGetMethod

    let getMethod (e : Expr) =
        e |> tryGetMethod |> Option.get

    let ofList (list : list<'a * list<MethodInfo>>) =
        let store = Dictionary<MethodInfo, 'a>()

        for (value, mis) in list do
            for mi in mis do
                store.[mi] <- value

        fun (mi : MethodInfo) ->
            match store.TryGetValue mi with
            | (true, v) -> 
                ValueSome v

            | _ ->
                if mi.IsGenericMethod then
                    match store.TryGetValue (mi.GetGenericMethodDefinition()) with
                    | (true, v) -> ValueSome v
                    | _ -> ValueNone
                else
                    ValueNone


[<AutoOpen>]
module internal Operators =
    let exactly (e : Expr) =
        MethodTable.getMethod e

    let generic (e : Expr) =
        let mi = MethodTable.getMethod e
        if mi.IsGenericMethod then mi.GetGenericMethodDefinition()
        else mi