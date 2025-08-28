namespace FShade

open Aardvark.Base
open System.Runtime.InteropServices
open FShade.Imperative

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

    /// Special RayId that will not be considered as a valid identifier.
    /// Will not produce a new entry in the SBT and results in an invalid negative index.
    static member None = RayId(Symbol.Empty, -2)

    /// The default RayId when unspecified. Equivalent to RayId().
    static member Default = RayId(Symbol.Empty)

    static member op_Implicit (name: Symbol) = RayId(name)
    static member op_Implicit (name: string) = RayId(name)

    override this.ToString() = string this.Name
    member internal this.IsNone = (this.index - 1) = -2

    interface IRaytracingId with
        member this.Index =
            if this.index > 0 || this.IsNone then this.index - 1
            else failwith $"[FShade] Index of RayId \"{this.Name}\" is invalid."

/// Identifies a miss shader by its name.
/// Gets replaced by the index corresponding to the name when the raytracing effect is compiled.
[<Struct>]
type MissId =
    val Name : Symbol
    val private index : int

    internal new (name: Symbol, index: int) = { Name = name; index = index + 1 }
    new (name: Symbol) = MissId(name, -1)
    new (name: string) = MissId(Sym.ofString name)

    /// Special MissId that will not be considered as a valid identifier.
    /// Will not produce a new entry in the SBT and results in an invalid negative index.
    static member None = MissId(Symbol.Empty, -2)

    /// The default MissId when unspecified. Equivalent to MissId().
    static member Default = MissId(Symbol.Empty)

    static member op_Implicit (name: Symbol) = MissId(name)
    static member op_Implicit (name: string) = MissId(name)

    override this.ToString() = string this.Name
    member internal this.IsNone = (this.index - 1) = -2

    interface IRaytracingId with
        member this.Index =
            if this.index > 0 || this.IsNone then this.index - 1
            else failwith $"[FShade] Index of MissId \"{this.Name}\" is invalid."

/// Identifies a callable shader by its name.
/// Gets replaced by the index corresponding to the name when the raytracing effect is compiled.
[<Struct>]
type CallableId =
    val Name : Symbol
    val private index : int

    internal new (name: Symbol, index: int) = { Name = name; index = index + 1 }
    new (name: Symbol) = CallableId(name, -1)
    new (name: string) = CallableId(Sym.ofString name)

    /// Special CallableId that will not be considered as a valid identifier.
    /// Will not produce a new entry in the SBT and results in an invalid negative index.
    static member None = CallableId(Symbol.Empty, -2)

    /// The default CallableId when unspecified. Equivalent to CallableId().
    static member Default = CallableId(Symbol.Empty)

    static member op_Implicit (name: Symbol) = CallableId(name)
    static member op_Implicit (name: string) = CallableId(name)

    override this.ToString() = string this.Name
    member internal this.IsNone = (this.index - 1) = -2

    interface IRaytracingId with
        member this.Index =
            if this.index > 0 || this.IsNone then this.index - 1
            else failwith $"[FShade] Index of CallableId \"{this.Name}\" is invalid."

[<AbstractClass; Sealed>]
type Callable private() =
    static member Execute<'T>([<Optional; DefaultParameterValue(CallableId())>] id : CallableId) : 'T = onlyInShaderCode "Callable.Execute"
    static member Execute<'T>(data : 'T, [<Optional; DefaultParameterValue(CallableId())>] id : CallableId) : 'T = onlyInShaderCode "Callable.Execute"

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

    member x.TraceRay<'T>(origin : V3f, direction : V3f,
                          [<Optional; DefaultParameterValue(RayId())>]                ray : RayId,
                          [<Optional; DefaultParameterValue(MissId())>]               miss : MissId,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

    member x.TraceRay<'T>(origin : V3f, direction : V3f, payload : 'T,
                          [<Optional; DefaultParameterValue(RayId())>]                ray : RayId,
                          [<Optional; DefaultParameterValue(MissId())>]               miss : MissId,
                          [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT : float32,
                          [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags : RayFlags,
                          [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask : int) : 'T = onlyInShaderCode "TraceRay"

[<AutoOpen>]
module RaytracingIntrinsics =

    /// Utility to mark payloads and callable data as unmodified.
    /// E.g. { unchanged<Payload> with color = V3f.III } will only write to the color field of the payload.
    /// Fails if the optimizer cannot inline the expression.
    let unchanged<'T> : 'T = onlyInShaderCode "unchanged"

    [<KeepCall>]
    let ignoreIntersection() : unit = onlyInShaderCode "ignoreIntersection"

    [<KeepCall>]
    let terminateRay() : unit = onlyInShaderCode "terminateRay"

    [<KeepCall>]
    let private reportIntersection (t : float32) (hitKind : RayHitKind) : bool = onlyInShaderCode "reportIntersection"

    [<KeepCall>]
    let private executeCallable (id : CallableId) (callableDataLocation : int) : unit = onlyInShaderCode "executeCallable"

    [<KeepCall>]
    let private traceRay (accelerationStructure : IAccelerationStructure) (rayFlags : RayFlags) (cullMask : int)
                         (sbtRecordOffset : RayId) (sbtRecordStride : int) (missIndex : MissId) (origin : V3f) (minT : float32)
                         (direction : V3f) (maxT : float32) (payloadLocation : int) : unit =
        onlyInShaderCode "traceRay"

    module MethodInfo =
        let unchanged = getMethodInfo <@ unchanged @>
        let reportIntersection = getMethodInfo <@ reportIntersection @>
        let executeCallable = getMethodInfo <@ executeCallable @>
        let traceRay = getMethodInfo <@ traceRay @>

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