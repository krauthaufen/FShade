namespace FShade

open Aardvark.Base

/// Describes the basic layout of a shader binding table.
type ShaderBindingTableLayout =
    {
        RayOffsets      : Map<Symbol, int>
        MissIndices     : Map<Symbol, int>
        CallableIndices : Map<Symbol, int>
    }

    member x.RayStride = x.RayOffsets.Count

    member x.GetRayOffset(id : Symbol) =
        match x.RayOffsets.TryGetValue id with
        | true, value -> value
        | _ -> failwithf "[FShade] Cannot find ray offset for %A" id

    member x.GetMissIndex(id : Symbol) =
        match x.MissIndices.TryGetValue id with
        | true, value -> value
        | _ -> failwithf "[FShade] Cannot find miss index for %A" id

    member x.GetCallableIndex(id : Symbol) =
        match x.CallableIndices.TryGetValue id with
        | true, value -> value
        | _ -> failwithf "[FShade] Cannot find callable index for %A" id


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal ShaderBindingTableLayout =

    let generate (shaders : RaytracingShader[]) =

        let makeMap (f : RaytracingShader -> Set<Symbol>) =
            shaders
            |> Array.collect (f >> Set.toArray)
            |> Array.distinct
            |> Array.mapi (fun i n -> n, i)
            |> Map.ofArray

        {
            RayOffsets = makeMap _.RayTypes
            MissIndices = makeMap _.MissShaders
            CallableIndices = makeMap _.CallableShaders
        }