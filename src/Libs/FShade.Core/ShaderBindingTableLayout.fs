namespace FShade

open Aardvark.Base

/// Describes the basic layout of a shader binding table.
type ShaderBindingTableLayout =
    {
        RayOffsets : Map<Symbol, int>
        MissIndices : Map<Symbol, int>
        CallableIndices : Map<Symbol, int>
    }

    member x.RayStride = x.RayOffsets.Count

    member x.GetRayOffset(id : Symbol) =
        x.RayOffsets |> Map.find id

    member x.GetMissIndex(id : Symbol) =
        x.MissIndices |> Map.find id

    member x.GetCallableIndex(id : Symbol) =
        x.CallableIndices |> Map.find id


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal ShaderBindingTableLayout =

    let generate (shaders : Shader[]) =

        let makeMap (f : Shader -> Set<Symbol>) =
            shaders
            |> Array.collect (f >> Set.toArray)
            |> Array.distinct
            |> Array.mapi (fun i n -> n, i)
            |> Map.ofArray

        {
            RayOffsets = makeMap (fun s -> s.shaderRayTypes)
            MissIndices = makeMap (fun s -> s.shaderMissShaders)
            CallableIndices = makeMap (fun s -> s.shaderCallableShaders)
        }