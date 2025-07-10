namespace FShade

open Aardvark.Base
open System.Runtime.InteropServices

module private Identifier =
    [<Literal>]
    let Default = "__Default__"

module private TraceDefaults =
    [<Literal>]
    let MinT = 0.001f

    [<Literal>]
    let MaxT = 10000.0f

    [<Literal>]
    let Flags = RayFlags.None

    [<Literal>]
    let CullMask = 0xFF

/// Identifies a ray type by its name.
/// Gets replaced by the index corresponding to the name when the raytracing effect is compiled.
[<Struct>]
type RayId =
    val Name : Symbol
    val private index : int

    internal new (name: Symbol, index: int) = { Name = name; index = index + 1 }
    new (name: Symbol) = RayId(name, -1)
    new (name: string) = RayId(Sym.ofString name)

    member internal this.IsEmpty = this.Name.IsEmpty

    interface IRaytracingId with
        member this.Index =
            if this.index > 0 || this.IsEmpty then this.index - 1
            else failwith $"[FShade] RayId \"{this.Name}\" is invalid."

/// Identifies a miss shader by its name.
/// Gets replaced by the index corresponding to the name when the raytracing effect is compiled.
[<Struct>]
type MissId =
    val Name : Symbol
    val private index : int

    internal new (name: Symbol, index: int) = { Name = name; index = index + 1 }
    new (name: Symbol) = MissId(name, -1)
    new (name: string) = MissId(Sym.ofString name)

    member internal this.IsEmpty = this.Name.IsEmpty

    interface IRaytracingId with
        member this.Index =
            if this.index > 0 || this.IsEmpty then this.index - 1
            else failwith $"[FShade] MissId \"{this.Name}\" is invalid."

/// Identifies a callable shader by its name.
/// Gets replaced by the index corresponding to the name when the raytracing effect is compiled.
[<Struct>]
type CallableId =
    val Name : Symbol
    val private index : int

    internal new (name: Symbol, index: int) = { Name = name; index = index + 1 }
    new (name: Symbol) = CallableId(name, -1)
    new (name: string) = CallableId(Sym.ofString name)

    member internal this.IsEmpty = this.Name.IsEmpty

    interface IRaytracingId with
        member this.Index =
            if this.index > 0 || this.IsEmpty then this.index - 1
            else failwith $"[FShade] CallableId \"{this.Name}\" is invalid."

[<AbstractClass; Sealed>]
type Callable private() =
    static member Execute<'T>(id : CallableId) : 'T = onlyInShaderCode "Callable.Execute"
    static member Execute<'T>(data : 'T, id : CallableId) : 'T = onlyInShaderCode "Callable.Execute"
    static member Execute<'T>([<Optional; DefaultParameterValue(Identifier.Default)>] id : string) : 'T = onlyInShaderCode "Callable.Execute"
    static member Execute<'T>(data : 'T, [<Optional; DefaultParameterValue(Identifier.Default)>] id : string) : 'T = onlyInShaderCode "Callable.Execute"
    static member Execute<'T>(id : Symbol) : 'T = onlyInShaderCode "Callable.Execute"
    static member Execute<'T>(data : 'T, id : Symbol) : 'T = onlyInShaderCode "Callable.Execute"


[<AbstractClass; Sealed>]
type Intersection private() =
    static member Report(t : float32, [<Optional; DefaultParameterValue(RayHitKind.Default)>] hitKind : RayHitKind) : bool =
        onlyInShaderCode "Intersection.Report"

    static member Report(t : float32, attribute : 'T, [<Optional; DefaultParameterValue(RayHitKind.Default)>] hitKind : RayHitKind) : bool =
        onlyInShaderCode "Intersection.Report"

type IAccelerationStructure = interface end

type Scene(accelerationStructure : ISemanticValue) =
    interface IAccelerationStructure

    member x.AccelerationStructure = accelerationStructure

    member x.TraceRay<'T>(origin : V3f, direction : V3f, ray : RayId, miss : MissId,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

    member x.TraceRay<'T>(origin : V3f, direction : V3f, payload : 'T, ray : RayId, miss : MissId,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

    member x.TraceRay<'T>(origin : V3f, direction : V3f,
                          [<Optional; DefaultParameterValue(Identifier.Default)>]     ray : string,
                          [<Optional; DefaultParameterValue(Identifier.Default)>]     miss : string,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

    member x.TraceRay<'T>(origin : V3f, direction : V3f, payload : 'T,
                          [<Optional; DefaultParameterValue(Identifier.Default)>]     ray : string,
                          [<Optional; DefaultParameterValue(Identifier.Default)>]     miss : string,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

    member x.TraceRay<'T>(origin : V3f, direction : V3f, ray : Symbol, miss : Symbol,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

    member x.TraceRay<'T>(origin : V3f, direction : V3f, payload : 'T, ray : Symbol, miss : Symbol,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

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