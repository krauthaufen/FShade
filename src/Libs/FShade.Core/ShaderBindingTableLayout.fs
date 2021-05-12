namespace FShade

open Aardvark.Base

/// Describes the basic layout of a shader binding table.
type ShaderBindingTableLayout =
    {
        RayOffsets : Map<string, int>
        MissIndices : Map<string, int>
        HitGroupIndices : Map<string, Map<string, int>>
    }

    member x.RayStride = x.RayOffsets.Count

    member x.GetRayOffset(id : string) =
        x.RayOffsets |> Map.find id

    member x.GetMissIndex(id : string) =
        x.MissIndices |> Map.find id

    member x.GetHitGroupIndex(groupId : string, rayId : string) =
        x.HitGroupIndices |> Map.find  groupId |> Map.find rayId

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal ShaderBindingTableLayout =

    let generate (shaders : Shader[]) (hitGroups : Set<string>) =

        let rayOffsets = Dict<string, int>()
        let missIndices = Dict<string, int>()
        let groupIndices = Dict<string, Dict<string, int>>()

        for shader in shaders do
            shader.shaderRayTypes |> Set.iter (fun id -> rayOffsets.Add(id, rayOffsets.Count))
            shader.shaderMissShaders |> Set.iter (fun id -> missIndices.Add(id, missIndices.Count))

        for group in hitGroups do
            let perRayOffsets = Dict<string, int>()

            rayOffsets.Keys |> Seq.iteri (fun i rayId ->
                perRayOffsets.Add(rayId, rayOffsets.Count * groupIndices.Count + i)
            )

            groupIndices.Add(group, perRayOffsets)

        {
            RayOffsets = Dict.toMap rayOffsets
            MissIndices = Dict.toMap missIndices
            HitGroupIndices = Map.empty
        }