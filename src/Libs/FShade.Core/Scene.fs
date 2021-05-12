namespace FShade

open Aardvark.Base
open System.Runtime.InteropServices

module private TraceDefaults =
    [<Literal>]
    let Ray = "__DefaultRay__"

    [<Literal>]
    let Miss = "__DefaultMiss__"

    [<Literal>]
    let MinT = 0.001

    [<Literal>]
    let MaxT = 10000.0

    [<Literal>]
    let Flags = RayFlags.None

    [<Literal>]
    let CullMask = 0xFF


type IAccelerationStructure = interface end

type Scene(accelerationStructure : ISemanticValue) =
    interface IAccelerationStructure

    member x.AccelerationStructure = accelerationStructure

    member x.TraceRay<'T>(origin : V3d, direction : V3d,
                          [<Optional; DefaultParameterValue(TraceDefaults.Ray)>]      ray : string,
                          [<Optional; DefaultParameterValue(TraceDefaults.Miss)>]     miss : string,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

    member x.TraceRay<'T>(origin : V3d, direction : V3d, payload : 'T,
                          [<Optional; DefaultParameterValue(TraceDefaults.Ray)>]      ray : string,
                          [<Optional; DefaultParameterValue(TraceDefaults.Miss)>]     miss : string,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

    static member IsAvailable = function
        | ShaderStage.RayGeneration | ShaderStage.ClosestHit | ShaderStage.Miss -> true
        | _ -> false


[<AutoOpen>]
module SceneExtensions =

    type ShaderAccelerationStructureHandle(semantic : string, scope : UniformScope) =
        static member CreateUniform(semantic : string, scope : UniformScope) = ShaderAccelerationStructureHandle(semantic, scope)
        interface ISemanticValue with
            member x.Semantic = semantic
            member x.Scope = scope

        new() = ShaderAccelerationStructureHandle(null, Unchecked.defaultof<UniformScope>)

    type AccelerationStructureMustBeSpecified = AccelerationStructureMustBeSpecified

    type SceneBuilder() =
        member x.Yield(_) = AccelerationStructureMustBeSpecified

        [<CustomOperation("accelerationStructure")>]
        member x.AccelerationStructure(_ : AccelerationStructureMustBeSpecified, accelerationStructure : ShaderAccelerationStructureHandle) =
            accelerationStructure

        member x.Run(h : ShaderAccelerationStructureHandle) =
            Scene(h)

    let scene = SceneBuilder()