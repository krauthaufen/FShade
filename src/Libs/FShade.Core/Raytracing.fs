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

/// <summary>
/// Identifies a ray type by its symbolic name.
/// </summary>
/// <remarks>
/// During effect compilation the name is resolved to a zero-based Shader Binding Table (SBT) index.
/// In user code you typically construct a RayId from a string or Symbol; at runtime the compiler replaces it
/// with the corresponding index. Use RayId.Default for the default ray type or RayId.None to indicate that no
/// valid ray identifier should be used (e.g., when constructing parameters that are unused).
/// </remarks>
[<Struct>]
type RayId =
    val Name : Symbol
    val private index : int

    internal new (name: Symbol, index: int) = { Name = name; index = index + 1 }
    new (name: Symbol) = RayId(name, -1)
    new (name: string) = RayId(Sym.ofString name)

    /// <summary>
    /// Special value that does not represent a valid ray identifier.
    /// </summary>
    /// <remarks>
    /// Using RayId.None will not create a new SBT entry and results in a negative internal index.
    /// This is typically used to indicate that a ray identifier parameter is intentionally unspecified.
    /// </remarks>
    static member None = RayId(Symbol.Empty, -2)

    /// <summary>
    /// The default ray identifier used when no explicit name is provided.
    /// </summary>
    /// <remarks>Equivalent to RayId().</remarks>
    static member Default = RayId(Symbol.Empty)

    static member op_Implicit (name: Symbol) = RayId(name)
    static member op_Implicit (name: string) = RayId(name)

    override this.ToString() = string this.Name
    member internal this.IsNone = (this.index - 1) = -2

    interface IRaytracingId with
        member this.Index =
            if this.index > 0 || this.IsNone then this.index - 1
            else failwith $"[FShade] Index of RayId \"{this.Name}\" is invalid."

/// <summary>
/// Identifies a miss shader by its symbolic name.
/// </summary>
/// <remarks>
/// During effect compilation the name is resolved to a zero-based index in the miss section of the SBT.
/// You can construct a MissId from a string or Symbol in user code; at runtime the compiler replaces it with the corresponding index.
/// Use MissId.Default to select the default miss shader or MissId.None to indicate that no valid identifier is provided.
/// </remarks>
[<Struct>]
type MissId =
    val Name : Symbol
    val private index : int

    internal new (name: Symbol, index: int) = { Name = name; index = index + 1 }
    new (name: Symbol) = MissId(name, -1)
    new (name: string) = MissId(Sym.ofString name)

    /// <summary>
    /// Special value that does not represent a valid miss shader identifier.
    /// </summary>
    /// <remarks>
    /// Using MissId.None will not create a new SBT entry and results in a negative internal index.
    /// This can be used to indicate that a miss shader id is intentionally unspecified.
    /// </remarks>
    static member None = MissId(Symbol.Empty, -2)

    /// <summary>
    /// The default miss shader identifier used when no explicit name is provided.
    /// </summary>
    /// <remarks>Equivalent to MissId().</remarks>
    static member Default = MissId(Symbol.Empty)

    static member op_Implicit (name: Symbol) = MissId(name)
    static member op_Implicit (name: string) = MissId(name)

    override this.ToString() = string this.Name
    member internal this.IsNone = (this.index - 1) = -2

    interface IRaytracingId with
        member this.Index =
            if this.index > 0 || this.IsNone then this.index - 1
            else failwith $"[FShade] Index of MissId \"{this.Name}\" is invalid."

/// <summary>
/// Identifies a callable shader by its symbolic name.
/// </summary>
/// <remarks>
/// During effect compilation the name is resolved to a zero-based index in the callable section of the SBT.
/// In user code you can construct a CallableId from a string or Symbol; at runtime the compiler replaces it
/// with the corresponding index. Use CallableId.Default to select the default callable shader or CallableId.None
/// to indicate that no valid identifier is provided.
/// </remarks>
[<Struct>]
type CallableId =
    val Name : Symbol
    val private index : int

    internal new (name: Symbol, index: int) = { Name = name; index = index + 1 }
    new (name: Symbol) = CallableId(name, -1)
    new (name: string) = CallableId(Sym.ofString name)

    /// <summary>
    /// Special value that does not represent a valid callable shader identifier.
    /// </summary>
    /// <remarks>
    /// Using CallableId.None will not create a new SBT entry and results in a negative internal index.
    /// This can be used to indicate that a callable shader id is intentionally unspecified.
    /// </remarks>
    static member None = CallableId(Symbol.Empty, -2)

    /// <summary>
    /// The default callable shader identifier used when no explicit name is provided.
    /// </summary>
    /// <remarks>Equivalent to CallableId().</remarks>
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

    /// <summary>
    /// Executes the given callable shader.
    /// </summary>
    /// <remarks>
    /// Only allowed in callable, ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <param name="id">The id of the callable shader to be executed.</param>
    /// <typeparam name="'Data">The type of the data returned from the executed shader.</typeparam>
    /// <returns>Data returned by the executed shader.</returns>
    static member Execute<'Data>(
            [<Optional; DefaultParameterValue(CallableId())>] id: CallableId
        ) : 'Data = onlyInShaderCode "Callable.Execute"

    /// <summary>
    /// Executes the given callable shader.
    /// </summary>
    /// <remarks>
    /// Only allowed in callable, ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <param name="data">Data passed to the callable shader.</param>
    /// <param name="id">The id of the callable shader to be executed.</param>
    /// <typeparam name="'Data">The type of the data passed to and returned from the executed shader.</typeparam>
    /// <returns>Data returned by the executed shader.</returns>
    static member Execute<'Data>(
            data: 'Data,
            [<Optional; DefaultParameterValue(CallableId())>] id: CallableId
        ) : 'Data = onlyInShaderCode "Callable.Execute"

[<AbstractClass; Sealed>]
type Intersection private() =

    /// <summary>
    /// Reports an intersection back to the ray traversal infrastructure.
    /// If the ray parameter <paramref name="t"/> is within the current ray interval, intersection confirmation is performed (e.g. any-hit shader if non-opaque).
    /// </summary>
    /// <remarks>
    /// Only allowed in intersection shaders.
    /// </remarks>
    /// <param name="t">The ray parameter of the reported intersection.</param>
    /// <param name="kind">The hit kind of the reported intersection.</param>
    /// <returns>True if the ray parameter <paramref name="t"/> falls within the current ray interval and the intersection has been confirmed, false otherwise.</returns>
    static member Report(
            t: float32,
            [<Optional; DefaultParameterValue(RayHitKind.Default)>] kind: RayHitKind
        ) : bool = onlyInShaderCode "Intersection.Report"

    /// <summary>
    /// Reports an intersection back to the ray traversal infrastructure.
    /// If the ray parameter <paramref name="t"/> is within the current ray interval, intersection confirmation is performed (e.g. any-hit shader if non-opaque).
    /// </summary>
    /// <remarks>
    /// Only allowed in intersection shaders.
    /// </remarks>
    /// <param name="t">The ray parameter of the reported intersection.</param>
    /// <param name="attribute">The hit attribute of the reported intersection. May be read in the any-hit and closest-hit shaders.</param>
    /// <param name="kind">The hit kind of the reported intersection.</param>
    /// <typeparam name="'Attribute">The type of the hit attribute of the reported intersection.</typeparam>
    /// <returns>True if the ray parameter <paramref name="t"/> falls within the current ray interval and the intersection has been confirmed, false otherwise.</returns>
    static member Report<'Attribute>(
            t: float32, attribute: 'Attribute,
            [<Optional; DefaultParameterValue(RayHitKind.Default)>] kind: RayHitKind
        ) : bool = onlyInShaderCode "Intersection.Report"

type IAccelerationStructure = interface end

/// <summary>
/// Represents a top-level acceleration structure (TLAS) bound to a shader and provides methods to trace rays against it.
/// </summary>
type Scene(accelerationStructure : ISemanticValue) =
    interface IAccelerationStructure

    member x.AccelerationStructure = accelerationStructure

    /// <summary>
    /// Initiates a ray query against the top-level acceleration structure of the scene,
    /// triggering the execution of various intersection and any-hit
    /// shaders as ray-geometry intersections are being evaluated, and finally
    /// the execution of either a closest-hit or miss shader, depending on whether
    /// an intersection was found.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <param name="origin">The origin of the ray.</param>
    /// <param name="direction">The direction of the ray.</param>
    /// <param name="ray">
    /// The id of the ray type. The hit shaders (intersection, any-hit, closest-hit) to be invoked are
    /// determined by the hit group of the intersected geometry and the ray type.
    /// </param>
    /// <param name="miss">The id of the miss shader to invoke, if no valid hit is found.</param>
    /// <param name="minT">
    /// The lower bound of the parametric range of the ray in which intersections can occur.
    /// Must be non-negative and smaller than or equal to <paramref name="maxT"/>.
    /// </param>
    /// <param name="maxT">
    /// The upper bound of the parametric range of the ray in which intersections can occur.
    /// Must be non-negative and greater than or equal to <paramref name="minT"/>.
    /// </param>
    /// <param name="flags">Flags that control the behavior of the ray traversal.</param>
    /// <param name="cullMask">
    /// The ray cull mask is compared with the instance cull mask of intersection candidates.
    /// If the bitwise AND combination of the two masks is zero, the intersection is ignored. Only the 8 least-significant bits are considered.
    /// </param>
    /// <typeparam name="'Payload">The type of the data returned from shaders invoked during the ray traversal.</typeparam>
    /// <returns>Data returned by the miss, any-hit, or closest-hit shaders.</returns>
    member _.TraceRay<'Payload>(
            origin: V3f, direction: V3f,
            [<Optional; DefaultParameterValue(RayId())>]                ray: RayId,
            [<Optional; DefaultParameterValue(MissId())>]               miss: MissId,
            [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT: float32,
            [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT: float32,
            [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags: RayFlags,
            [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask: int
        ) : 'Payload = onlyInShaderCode "TraceRay"

    /// <summary>
    /// Initiates a ray query against the top-level acceleration structure of the scene,
    /// triggering the execution of various intersection and any-hit
    /// shaders as ray-geometry intersections are being evaluated, and finally
    /// the execution of either a closest-hit or miss shader, depending on whether
    /// an intersection was found.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <param name="origin">The origin of the ray.</param>
    /// <param name="direction">The direction of the ray.</param>
    /// <param name="payload">The data passed to shaders invoked during the ray traversal.</param>
    /// <param name="ray">
    /// The id of the ray type. The hit shaders (intersection, any-hit, closest-hit) to be invoked are
    /// determined by the hit group of the intersected geometry and the ray type.
    /// </param>
    /// <param name="miss">The id of the miss shader to invoke, if no valid hit is found.</param>
    /// <param name="minT">
    /// The lower bound of the parametric range of the ray in which intersections can occur.
    /// Must be non-negative and smaller than or equal to <paramref name="maxT"/>.
    /// </param>
    /// <param name="maxT">
    /// The upper bound of the parametric range of the ray in which intersections can occur.
    /// Must be non-negative and greater than or equal to <paramref name="minT"/>.
    /// </param>
    /// <param name="flags">Flags that control the behavior of the ray traversal.</param>
    /// <param name="cullMask">
    /// The ray cull mask is compared with the instance cull mask of intersection candidates.
    /// If the bitwise AND combination of the two masks is zero, the intersection is ignored. Only the 8 least-significant bits are considered.
    /// </param>
    /// <typeparam name="'Payload">The type of the data passed to and returned from shaders invoked during the ray traversal.</typeparam>
    /// <returns>Data returned by the miss, any-hit, or closest-hit shaders.</returns>
    member _.TraceRay<'Payload>(
            origin: V3f, direction: V3f, payload: 'Payload,
            [<Optional; DefaultParameterValue(RayId())>]                ray: RayId,
            [<Optional; DefaultParameterValue(MissId())>]               miss: MissId,
            [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT: float32,
            [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT: float32,
            [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags: RayFlags,
            [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask: int
        ) : 'Payload = onlyInShaderCode "TraceRay"

[<AutoOpen>]
module RaytracingIntrinsics =

    /// <summary>
    /// Utility to mark payloads and callable data as unmodified.
    /// </summary>
    /// <example>
    /// Write only to the color field of a payload:
    /// <code>{ unchanged&lt;Payload&gt; with color = V3f.III }</code>
    /// </example>
    /// <remarks>
    /// This fails at compile time if the optimizer cannot inline the expression.
    /// </remarks>
    let unchanged<'T> : 'T = onlyInShaderCode "unchanged"

    /// <summary>
    /// Ignores the current potential intersection, terminating the calling shader, and continues the ray traversal.
    /// </summary>
    /// <remarks>
    /// Only allowed in any-hit shaders.
    /// </remarks>
    [<KeepCall>]
    let ignoreIntersection() : unit = onlyInShaderCode "ignoreIntersection"

    /// <summary>
    /// Terminates the calling shader, stops the ray traversal, accepts the current hit, and invokes the closest-hit shader.
    /// </summary>
    /// <remarks>
    /// Only allowed in any-hit shaders.
    /// </remarks>
    [<KeepCall>]
    let terminateRay() : unit = onlyInShaderCode "terminateRay"

    [<KeepCall>]
    let private reportIntersection (t : float32) (kind : RayHitKind) : bool = onlyInShaderCode "reportIntersection"

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

    /// <summary>
    /// Computation expression builder for creating Scene values.
    /// </summary>
    type SceneBuilder() =
        member x.Yield(_) = AccelerationStructureMustBeSpecified

        /// <summary>
        /// Specifies the acceleration structure handle to use for the scene.
        /// </summary>
        [<CustomOperation("accelerationStructure")>]
        member x.AccelerationStructure(_ : AccelerationStructureMustBeSpecified, accelerationStructure : ShaderAccelerationStructureHandle) =
            accelerationStructure

        member x.Run(h : ShaderAccelerationStructureHandle) =
            Scene(h)

    /// <summary>
    /// Computation expression for constructing a Scene.
    /// </summary>
    /// <example>
    /// <code>scene { accelerationStructure uniform?RaytracingScene }</code>
    /// </example>
    let scene = SceneBuilder()