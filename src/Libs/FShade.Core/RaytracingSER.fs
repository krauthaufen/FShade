namespace FShade

open Aardvark.Base
open System.Runtime.InteropServices
open FShade.Imperative

// Shader execution reordering
// https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_shader_invocation_reorder.txt
// https://github.khronos.org/SPIRV-Registry/extensions/NV/SPV_NV_shader_invocation_reorder.html

/// <summary>
/// Encapsulates the state of the ray traversal and allows queries to read this state.
/// </summary>
/// <remarks> Requires GL_NV_shader_invocation_reorder.</remarks>
type HitObject =

    new() = onlyInShaderCode "HitObject()"; {}

    /// Returns if the hit object encodes a nop.
    member _.IsEmpty : bool = onlyInShaderCode "IsEmpty"

    /// Returns if the hit object encodes a miss.
    member _.IsMiss : bool = onlyInShaderCode "IsMiss"

    /// Returns if the hit object encodes a hit.
    member _.IsHit : bool = onlyInShaderCode "IsHit"

    /// The parametric minT value of the ray encoded in the hit object.
    member _.RayMinT : float32 = onlyInShaderCode "RayMinT"

    /// The parametric maxT value of the ray encoded in the hit object.
    member _.RayMaxT : float32 = onlyInShaderCode "RayMaxT"

    /// The origin of the ray encoded in the hit object in world space.
    member _.RayOrigin : V3f = onlyInShaderCode "RayOrigin"

    /// The direction of the ray encoded in the hit object in world space.
    member _.RayDirection : V3f = onlyInShaderCode "RayDirection"

    /// The origin of the ray encoded in the hit object in object space.
    member _.RayObjectOrigin : V3f = onlyInShaderCode "RayObjectOrigin"

    /// The direction of the ray encoded in the hit object in object space.
    member _.RayObjectDirection : V3f = onlyInShaderCode "RayObjectDirection"

    /// The object-to-world transformation matrix encoded in the hit object.
    member _.ObjectToWorld : M34f = onlyInShaderCode "ObjectToWorld"

    /// The world-to-object transformation matrix encoded in the hit object.
    member _.WorldToObject : M34f = onlyInShaderCode "ObjectToWorld"

    /// The custom index of the instance encoded in the hit object.
    member _.InstanceCustomIndex : int = onlyInShaderCode "InstanceCustomIndex"

    /// The index of the instance encoded in the hit object.
    member _.InstanceId : int = onlyInShaderCode "InstanceId"

    /// The index of the geometry encoded in the hit object.
    member _.GeometryIndex : int = onlyInShaderCode "GeometryIndex"

    /// The index of the primitive (triangle or bounding box) within the geometry as encoded in the hit object.
    member _.PrimitiveIndex : int = onlyInShaderCode "PrimitiveIndex"

    /// The kind of the hit encoded in the hit object.
    member _.HitKind : RayHitKind = onlyInShaderCode "HitKind"

    /// <summary>
    /// Initiates a ray query against a top-level acceleration
    /// structure, triggering the execution of various intersection and any-hit
    /// shaders as ray-geometry intersections are being evaluated, and returns the
    /// resulting hit or miss information in the hit object. This does not
    /// execute any closest-hit or miss shaders. No thread reordering
    /// or user-observable driver side scheduling occurs.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <param name="scene">The scene to perform the ray query against.</param>
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
    /// <returns>Data returned by the any-hit shaders.</returns>
    member _.TraceRay<'Payload>(
            scene: Scene, origin: V3f, direction: V3f,
            [<Optional; DefaultParameterValue(RayId())>]                ray : RayId,
            [<Optional; DefaultParameterValue(MissId())>]               miss : MissId,
            [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT: float32,
            [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT: float32,
            [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags: RayFlags,
            [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask: int
        ) : 'Payload = onlyInShaderCode "TraceRay"

    /// <summary>
    /// Initiates a ray query against a top-level acceleration
    /// structure, triggering the execution of various intersection and any-hit
    /// shaders as ray-geometry intersections are being evaluated, and returns the
    /// resulting hit or miss information in the hit object. This does not
    /// execute any closest-hit or miss shaders. No thread reordering
    /// or user-observable driver side scheduling occurs.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <param name="scene">The scene to perform the ray query against.</param>
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
    /// <returns>Data returned by the any-hit shaders.</returns>
    member _.TraceRay<'Payload>(
            scene: Scene, origin: V3f, direction: V3f, payload: 'Payload,
            [<Optional; DefaultParameterValue(RayId())>]                ray : RayId,
            [<Optional; DefaultParameterValue(MissId())>]               miss : MissId,
            [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT: float32,
            [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT:  float32,
            [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags: RayFlags,
            [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask: int
        ) : 'Payload = onlyInShaderCode "TraceRay"


    /// <summary>
    /// Populates the hit object representing a hit without tracing a ray.
    /// It is legal to construct a hit which is not the closest hit along the ray
    /// or a hit which is not located along the ray.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <param name="scene">The scene for which to record the hit.</param>
    /// <param name="instanceId">
    /// The index of the instance within the top-level acceleration structure for which the hit is recorded.
    /// Results are undefined if negative or out of bounds.</param>
    /// <param name="primitiveId">
    /// The index of the primitive within the geometry for which the hit is recorded.
    /// Results are undefined if negative or out of bounds.</param>
    /// <param name="geometryIndex">
    /// The index of the geometry within the instance for which the hit is recorded.
    /// Results are undefined if negative or out of bounds.
    /// </param>
    /// <param name="origin">The origin of the ray.</param>
    /// <param name="direction">The direction of the ray.</param>
    /// <param name="ray">
    /// The id of the ray type. The hit shaders (intersection, any-hit, closest-hit) to be invoked are
    /// determined by the hit group of the intersected geometry and the ray type.
    /// </param>
    /// <param name="minT">
    /// The lower bound of the parametric range of the ray in which intersections can occur.
    /// Must be non-negative and smaller than or equal to <paramref name="maxT"/>.
    /// </param>
    /// <param name="maxT">
    /// The parametric distance along the ray to the intersected primitive.
    /// Must be non-negative and greater than or equal to <paramref name="minT"/>.
    /// </param>
    /// <param name="kind">The kind of the recorded hit.</param>
    /// <typeparam name="'Attribute">The type of the attribute of the recorded hit, available in the closest-hit shader.</typeparam>
    member _.RecordHit<'Attribute>(
           scene: Scene, instanceId: int, primitiveId: int, geometryIndex: int, origin: V3f, direction: V3f,
           [<Optional; DefaultParameterValue(RayId())>]            ray : RayId,
           [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
           [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32,
           [<Optional; DefaultParameterValue(RayHitKind.Default)>] kind: RayHitKind
        ) : unit = onlyInShaderCode "RecordHit"

    /// <summary>
    /// Populates the hit object representing a hit without tracing a ray.
    /// It is legal to construct a hit which is not the closest hit along the ray
    /// or a hit which is not located along the ray.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <param name="scene">The scene for which to record the hit.</param>
    /// <param name="instanceId">
    /// The index of the instance within the top-level acceleration structure for which the hit is recorded.
    /// Results are undefined if negative or out of bounds.</param>
    /// <param name="primitiveId">
    /// The index of the primitive within the geometry for which the hit is recorded.
    /// Results are undefined if negative or out of bounds.</param>
    /// <param name="geometryIndex">
    /// The index of the geometry within the instance for which the hit is recorded.
    /// Results are undefined if negative or out of bounds.
    /// </param>
    /// <param name="origin">The origin of the ray.</param>
    /// <param name="direction">The direction of the ray.</param>
    /// <param name="attribute">The attribute of the recorded hit. May be read in the closest-hit shader.</param>
    /// <param name="ray">
    /// The id of the ray type. The hit shaders (intersection, any-hit, closest-hit) to be invoked are
    /// determined by the hit group of the intersected geometry and the ray type.
    /// </param>
    /// <param name="minT">
    /// The lower bound of the parametric range of the ray in which intersections can occur.
    /// Must be non-negative and smaller than or equal to <paramref name="maxT"/>.
    /// </param>
    /// <param name="maxT">
    /// The parametric distance along the ray to the intersected primitive.
    /// Must be non-negative and greater than or equal to <paramref name="minT"/>.
    /// </param>
    /// <param name="kind">The kind of the recorded hit.</param>
    /// <typeparam name="'Attribute">The type of the attribute of the recorded hit, available in the closest-hit shader.</typeparam>
    member _.RecordHit<'Attribute>(
            scene: Scene, instanceId: int, primitiveId: int, geometryIndex: int, origin: V3f, direction: V3f, attribute: 'Attribute,
            [<Optional; DefaultParameterValue(RayId())>]            ray : RayId,
            [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
            [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32,
            [<Optional; DefaultParameterValue(RayHitKind.Default)>] kind: RayHitKind
        ) : unit = onlyInShaderCode "RecordHit"


    /// <summary>
    /// Populates the hit object representing a miss without tracing a ray.
    /// It is legal to construct a miss in a hit object for a ray that
    /// could have hit some geometry if traced.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <param name="origin">The origin of the ray.</param>
    /// <param name="direction">The direction of the ray.</param>
    /// <param name="miss">The id of the miss shader to invoke.</param>
    /// <param name="minT">
    /// The lower bound of the parametric range of the ray in which intersections can occur.
    /// Must be non-negative and smaller than or equal to <paramref name="maxT"/>.
    /// </param>
    /// <param name="maxT">
    /// The upper bound of the parametric range of the ray in which intersections can occur.
    /// Must be non-negative and greater than or equal to <paramref name="minT"/>.
    /// </param>
    member _.RecordMiss(
            origin: V3f, direction: V3f,
            [<Optional; DefaultParameterValue(MissId())>]           miss: MissId,
            [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
            [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32
        ) : unit = onlyInShaderCode "RecordMiss"


    /// <summary>
    /// Encodes the hit object to represent neither a hit nor a miss.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    [<KeepCall>]
    member _.RecordEmpty() : unit = onlyInShaderCode "RecordEmpty"


    /// <summary>
    /// Execute the closest-hit or miss shader encoded in the hit object.
    /// This call does not trigger reordering of threads.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <typeparam name="'Payload">The type of the data returned from the executed shader.</typeparam>
    /// <returns>Data returned by the executed shader.</returns>
    member _.ExecuteShader<'Payload>() : 'Payload = onlyInShaderCode "ExecuteShader"

    /// <summary>
    /// Execute the closest-hit or miss shader encoded in the hit object.
    /// This call does not trigger reordering of threads.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <param name="payload">The data passed to the executed shader.</param>
    /// <typeparam name="'Payload">The type of the data passed to and returned from the executed shader.</typeparam>
    /// <returns>Data returned by executed shader.</returns>
    member _.ExecuteShader<'Payload>(payload: 'Payload) : 'Payload = onlyInShaderCode "ExecuteShader"


    /// <summary>
    /// Returns the attribute encoded in the hit object.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation, closest-hit, and miss shaders.
    /// </remarks>
    /// <typeparam name="'Attribute">The type of the attribute encoded in the hit object.</typeparam>
    member _.GetAttribute<'Attribute>() : 'Attribute = onlyInShaderCode "GetAttribute"

[<AbstractClass; Sealed>]
type Thread =

    /// <summary>
    /// Reorder threads based on a user provided hint. Similar hint values
    /// indicate similarity of subsequent work done after this call. Behavior
    /// is implementation defined.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation shaders.
    /// </remarks>
    /// <param name="hint">Determines desired ordering of threads relative to others.</param>
    /// <param name="bits">Number of least significant bits an implementation should take into account from <paramref name="hint"/> in determining ordering.</param>
    [<KeepCall>]
    static member Reorder(hint: uint, bits: uint) : unit = onlyInShaderCode "Thread.Reorder"

    /// <summary>
    /// Reorder threads based on a user provided hint. Similar hint values
    /// indicate similarity of subsequent work done after this call. Behavior
    /// is implementation defined.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation shaders.
    /// </remarks>
    /// <param name="hint">Determines desired ordering of threads relative to others.</param>
    /// <param name="bits">Number of least significant bits an implementation should take into account from <paramref name="hint"/> in determining ordering.</param>
    [<KeepCall>]
    static member Reorder(hint: int, bits: int) : unit = onlyInShaderCode "Thread.Reorder"

    /// <summary>
    /// Reorder threads based on the provided hit object. The exact properties
    /// from the hit object which are used to reorder the threads is implementation
    /// defined.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation shaders.
    /// </remarks>
    /// <param name="hitObject">The hit object to base the ordering on.</param>
    [<KeepCall>]
    static member Reorder(hitObject: HitObject) : unit = onlyInShaderCode "Thread.Reorder"

    /// <summary>
    /// Reorder threads based on the provided hit object supplemented by additional
    /// information based on a user provided hint. The exact properties from
    /// the hit object and the user specified hint which are used to reorder theads is
    /// implementation defined.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation shaders.
    /// </remarks>
    /// <param name="hitObject">The hit object to base the ordering on.</param>
    /// <param name="hint">Determines desired ordering of threads relative to others.</param>
    /// <param name="bits">Number of least significant bits an implementation should take into account from <paramref name="hint"/> in determining ordering.</param>
    [<KeepCall>]
    static member Reorder(hitObject: HitObject, hint: uint, bits: uint) : unit = onlyInShaderCode "Thread.Reorder"

    /// <summary>
    /// Reorder threads based on the provided hit object supplemented by additional
    /// information based on a user provided hint. The exact properties from
    /// the hit object and the user specified hint which are used to reorder theads is
    /// implementation defined.
    /// </summary>
    /// <remarks>
    /// Only allowed in ray generation shaders.
    /// </remarks>
    /// <param name="hitObject">The hit object to base the ordering on.</param>
    /// <param name="hint">Determines desired ordering of threads relative to others.</param>
    /// <param name="bits">Number of least significant bits an implementation should take into account from <paramref name="hint"/> in determining ordering.</param>
    [<KeepCall>]
    static member Reorder(hitObject: HitObject, hint: int, bits: int) : unit = onlyInShaderCode "Thread.Reorder"


[<AutoOpen>]
module RaytracingIntrinsicsSER =

    // The hitObjectNV is weird as it cannot be constructed, you can only declare a variable with type hitObjectNV.
    // In F# we would have to write `let ho = Unchecked.defaultof<HitObject>`, which is a bit cumbersome.
    // So we use this function which basically just declares a variable and returns it.
    [<ReflectedDefinition>]
    let private newHitObject() = Unchecked.defaultof<HitObject>

    [<KeepCall>]
    let private hitObjectTraceRay (hitObject : HitObject) (accelerationStructure : IAccelerationStructure) (rayFlags : RayFlags) (cullMask : int)
                                  (sbtRecordOffset : RayId) (sbtRecordStride : int) (missIndex : MissId) (origin : V3f) (minT : float32)
                                  (direction : V3f) (maxT : float32) (payloadLocation : int) : unit =
        onlyInShaderCode "traceRay"

    [<KeepCall>]
    let private hitObjectRecordHit (obj : HitObject) (scene : IAccelerationStructure) (instanceId : int) (primitiveId : int) (geometryIndex : int)
                                   (kind : RayHitKind) (sbtRecordOffset : RayId) (sbtRecordStride : int) (origin : V3f) (minT : float32)
                                   (direction : V3f) (maxT : float32) (attributeLocation : int) : unit =
        onlyInShaderCode "hitObjectRecordHit"

    [<KeepCall>]
    let private hitObjectRecordMiss (obj : HitObject) (miss : MissId) (origin : V3f) (minT : float32) (direction : V3f) (maxT : float32) : unit =
        onlyInShaderCode "hitObjectRecordMiss"

    [<KeepCall>]
    let private hitObjectExecuteShader (obj : HitObject) (payloadLocation : int) : unit =
        onlyInShaderCode "hitObjectExecuteShader"

    [<KeepCall>]
    let private hitObjectExtractAttributes (obj : HitObject) (attributeLocation : int) : unit =
        onlyInShaderCode "hitObjectExtractAttributes"

    module MethodInfo =
        let newHitObject               = getMethodInfo <@ newHitObject @>
        let hitObjectTraceRay          = getMethodInfo <@ hitObjectTraceRay @>
        let hitObjectRecordHit         = getMethodInfo <@ hitObjectRecordHit @>
        let hitObjectRecordMiss        = getMethodInfo <@ hitObjectRecordMiss @>
        let hitObjectExecuteShader     = getMethodInfo <@ hitObjectExecuteShader @>
        let hitObjectExtractAttributes = getMethodInfo <@ hitObjectExtractAttributes @>