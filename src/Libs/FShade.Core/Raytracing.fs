namespace FShade

open FShade.Imperative

open Aardvark.Base
open System.Runtime.InteropServices

module private Identifier =
    [<Literal>]
    let Default = "__Default__"

module private TraceDefaults =
    [<Literal>]
    let MinT = 0.001

    [<Literal>]
    let MaxT = 10000.0

    [<Literal>]
    let Flags = RayFlags.None

    [<Literal>]
    let CullMask = 0xFF


[<AbstractClass; Sealed>]
type Callable private() =
    static member Execute<'T>([<Optional; DefaultParameterValue(Identifier.Default)>] id : string) : 'T = onlyInShaderCode "Callable.Execute"
    static member Execute<'T>(data : 'T, [<Optional; DefaultParameterValue(Identifier.Default)>] id : string) : 'T = onlyInShaderCode "Callable.Execute"
    static member Execute<'T>(id : Symbol) : 'T = onlyInShaderCode "Callable.Execute"
    static member Execute<'T>(data : 'T, id : Symbol) : 'T = onlyInShaderCode "Callable.Execute"


[<AbstractClass; Sealed>]
type Intersection private() =
    static member Report(t : float, [<Optional; DefaultParameterValue(RayHitKind.Default)>] hitKind : RayHitKind) : bool =
        onlyInShaderCode "Intersection.Report"

    static member Report(t : float, attribute : 'T, [<Optional; DefaultParameterValue(RayHitKind.Default)>] hitKind : RayHitKind) : bool =
        onlyInShaderCode "Intersection.Report"

type IAccelerationStructure = interface end

type Scene(accelerationStructure : ISemanticValue) =
    interface IAccelerationStructure

    member x.AccelerationStructure = accelerationStructure

    member x.TraceRay<'T>(origin : V3d, direction : V3d,
                          [<Optional; DefaultParameterValue(Identifier.Default)>]     ray : string,
                          [<Optional; DefaultParameterValue(Identifier.Default)>]     miss : string,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

    member x.TraceRay<'T>(origin : V3d, direction : V3d, payload : 'T,
                          [<Optional; DefaultParameterValue(Identifier.Default)>]     ray : string,
                          [<Optional; DefaultParameterValue(Identifier.Default)>]     miss : string,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

    [<Inline>]
    member x.TraceRay<'T>(origin : V3d, direction : V3d, ray : Symbol, miss : Symbol,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T =
        x.TraceRay<'T>(origin, direction,
                       ray = string ray, miss = string miss,
                       minT = minT, maxT = maxT, flags = flags,
                       cullMask = cullMask)

    [<Inline>]
    member x.TraceRay<'T>(origin : V3d, direction : V3d, payload : 'T, ray : Symbol, miss : Symbol,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T =
        x.TraceRay<'T>(origin, direction, payload,
                       ray = string ray, miss = string miss,
                       minT = minT, maxT = maxT, flags = flags,
                       cullMask = cullMask)


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