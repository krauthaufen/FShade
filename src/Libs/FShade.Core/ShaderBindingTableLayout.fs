namespace FShade

open Aardvark.Base

/// Describes the basic layout of a shader binding table.
type ShaderBindingTableLayout =
    {
        RayOffsets : Map<Symbol, int>
        MissIndices : Map<Symbol, int>
        CallableIndices : Map<Symbol, int>
        HitGroupIndices : Map<Symbol, Map<Symbol, int>>
    }

    member x.RayStride = x.RayOffsets.Count

    member x.GetRayOffset(id : Symbol) =
        x.RayOffsets |> Map.find id

    member x.GetMissIndex(id : Symbol) =
        x.MissIndices |> Map.find id

    member x.GetCallableIndex(id : Symbol) =
        x.CallableIndices |> Map.find id

    member x.GetHitGroupIndex(groupId : Symbol, rayId : Symbol) =
        x.HitGroupIndices |> Map.find  groupId |> Map.find rayId


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal ShaderBindingTableLayout =

    let generate (shaders : Shader[]) (hitGroups : Set<Symbol>) =

        let rayOffsets = Dict<Symbol, int>()
        let missIndices = Dict<Symbol, int>()
        let callableIndices = Dict<Symbol, int>()
        let groupIndices = Dict<Symbol, Dict<Symbol, int>>()

        for shader in shaders do
            shader.shaderRayTypes |> Set.iter (fun id -> rayOffsets.Add(id, rayOffsets.Count))
            shader.shaderMissShaders |> Set.iter (fun id -> missIndices.Add(id, missIndices.Count))
            shader.shaderCallableShaders |> Set.iter (fun id -> callableIndices.Add(id, callableIndices.Count))

        for group in hitGroups do
            let perRayOffsets = Dict<Symbol, int>()

            rayOffsets.Keys |> Seq.iteri (fun i rayId ->
                perRayOffsets.Add(rayId, rayOffsets.Count * groupIndices.Count + i)
            )

            groupIndices.Add(group, perRayOffsets)

        {
            RayOffsets = Dict.toMap rayOffsets
            MissIndices = Dict.toMap missIndices
            CallableIndices = Dict.toMap callableIndices
            HitGroupIndices = Dict.toMap (groupIndices |> Dict.map (fun _ d -> Dict.toMap d))
        }