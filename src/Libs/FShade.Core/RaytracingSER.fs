namespace FShade

open Aardvark.Base
open System.Runtime.InteropServices
open FShade.Imperative

// Shader execution reordering
// https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_shader_invocation_reorder.txt
// https://github.khronos.org/SPIRV-Registry/extensions/NV/SPV_NV_shader_invocation_reorder.html

/// Encapsulates the state of the ray traversal and allows queries to read this state.
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

    /// Initiates a ray query against a top-level acceleration
    /// structure, triggering the execution of various intersection and any-hit
    /// shaders as ray-geometry intersections are being evaluated, and returns the
    /// resulting hit or miss information in the hit object. This does not
    /// execute any closest-hit or miss shaders. No thread reordering
    /// or user-observable driver side scheduling occurs.
    member _.TraceRay<'Payload>(scene: Scene, origin: V3f, direction: V3f, ray: RayId, miss: MissId,
                                [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags: RayFlags,
                                [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask: int) : 'Payload = onlyInShaderCode "TraceRay"

    /// Initiates a ray query against a top-level acceleration
    /// structure, triggering the execution of various intersection and any-hit
    /// shaders as ray-geometry intersections are being evaluated, and returns the
    /// resulting hit or miss information in the hit object. This does not
    /// execute any closest-hit or miss shaders. No thread reordering
    /// or user-observable driver side scheduling occurs.
    member _.TraceRay<'Payload>(scene: Scene, origin: V3f, direction: V3f, payload: 'Payload, ray: RayId, miss: MissId,
                                [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT:  float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags: RayFlags,
                                [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask: int) : 'Payload = onlyInShaderCode "TraceRay"

    /// Initiates a ray query against a top-level acceleration
    /// structure, triggering the execution of various intersection and any-hit
    /// shaders as ray-geometry intersections are being evaluated, and returns the
    /// resulting hit or miss information in the hit object. This does not
    /// execute any closest-hit or miss shaders. No thread reordering
    /// or user-observable driver side scheduling occurs.
    member _.TraceRay<'Payload>(scene: Scene, origin: V3f, direction: V3f,
                                [<Optional; DefaultParameterValue(Identifier.Default)>]     ray: string,
                                [<Optional; DefaultParameterValue(Identifier.Default)>]     miss: string,
                                [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags: RayFlags,
                                [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask: int) : 'Payload = onlyInShaderCode "TraceRay"

    /// Initiates a ray query against a top-level acceleration
    /// structure, triggering the execution of various intersection and any-hit
    /// shaders as ray-geometry intersections are being evaluated, and returns the
    /// resulting hit or miss information in the hit object. This does not
    /// execute any closest-hit or miss shaders. No thread reordering
    /// or user-observable driver side scheduling occurs.
    member _.TraceRay<'Payload>(scene: Scene, origin: V3f, direction: V3f, payload: 'Payload,
                                [<Optional; DefaultParameterValue(Identifier.Default)>]     ray: string,
                                [<Optional; DefaultParameterValue(Identifier.Default)>]     miss: string,
                                [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags: RayFlags,
                                [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask: int) : 'Payload = onlyInShaderCode "TraceRay"

    /// Initiates a ray query against a top-level acceleration
    /// structure, triggering the execution of various intersection and any-hit
    /// shaders as ray-geometry intersections are being evaluated, and returns the
    /// resulting hit or miss information in the hit object. This does not
    /// execute any closest-hit or miss shaders. No thread reordering
    /// or user-observable driver side scheduling occurs.
    member _.TraceRay<'Payload>(scene: Scene, origin: V3f, direction: V3f, ray: Symbol, miss: Symbol,
                                [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags: RayFlags,
                                [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask: int) : 'Payload = onlyInShaderCode "TraceRay"

    /// Initiates a ray query against a top-level acceleration
    /// structure, triggering the execution of various intersection and any-hit
    /// shaders as ray-geometry intersections are being evaluated, and returns the
    /// resulting hit or miss information in the hit object. This does not
    /// execute any closest-hit or miss shaders. No thread reordering
    /// or user-observable driver side scheduling occurs.
    member _.TraceRay<'Payload>(scene: Scene, origin: V3f, direction: V3f, payload: 'Payload, ray: Symbol, miss: Symbol,
                                [<Optional; DefaultParameterValue(TraceDefaults.MinT)>]     minT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>]     maxT: float32,
                                [<Optional; DefaultParameterValue(TraceDefaults.Flags)>]    flags: RayFlags,
                                [<Optional; DefaultParameterValue(TraceDefaults.CullMask)>] cullMask: int) : 'Payload = onlyInShaderCode "TraceRay"


    /// Populates the hit object representing a hit without tracing a ray.
    /// It is legal to construct a hit which is not the closest hit along the ray
    /// or a hit which is not located along the ray.
    member _.RecordHit<'Attribute>(scene: Scene, instanceId: int, primitiveId: int, geometryIndex: int, origin: V3f, direction: V3f, ray: RayId,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32,
                                   [<Optional; DefaultParameterValue(RayHitKind.Default)>] kind: RayHitKind) : unit = onlyInShaderCode "RecordHit"

    /// Populates the hit object representing a hit without tracing a ray.
    /// It is legal to construct a hit which is not the closest hit along the ray
    /// or a hit which is not located along the ray.
    member _.RecordHit<'Attribute>(scene: Scene, instanceId: int, primitiveId: int, geometryIndex: int, origin: V3f, direction: V3f,
                                   [<Optional; DefaultParameterValue(Identifier.Default)>] ray: string,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32,
                                   [<Optional; DefaultParameterValue(RayHitKind.Default)>] kind: RayHitKind) : unit = onlyInShaderCode "RecordHit"

    /// Populates the hit object representing a hit without tracing a ray.
    /// It is legal to construct a hit which is not the closest hit along the ray
    /// or a hit which is not located along the ray.
    member _.RecordHit<'Attribute>(scene: Scene, instanceId: int, primitiveId: int, geometryIndex: int, origin: V3f, direction: V3f, ray: Symbol,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32,
                                   [<Optional; DefaultParameterValue(RayHitKind.Default)>] kind: RayHitKind) : unit = onlyInShaderCode "RecordHit"

    /// Populates the hit object representing a hit without tracing a ray.
    /// It is legal to construct a hit which is not the closest hit along the ray
    /// or a hit which is not located along the ray.
    member _.RecordHit<'Attribute>(scene: Scene, instanceId: int, primitiveId: int, geometryIndex: int, origin: V3f, direction: V3f, attribute: 'Attribute, ray: RayId,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32,
                                   [<Optional; DefaultParameterValue(RayHitKind.Default)>] kind: RayHitKind) : unit = onlyInShaderCode "RecordHit"

    /// Populates the hit object representing a hit without tracing a ray.
    /// It is legal to construct a hit which is not the closest hit along the ray
    /// or a hit which is not located along the ray.
    member _.RecordHit<'Attribute>(scene: Scene, instanceId: int, primitiveId: int, geometryIndex: int, origin: V3f, direction: V3f, attribute: 'Attribute,
                                   [<Optional; DefaultParameterValue(Identifier.Default)>] ray: string,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32,
                                   [<Optional; DefaultParameterValue(RayHitKind.Default)>] kind: RayHitKind) : unit = onlyInShaderCode "RecordHit"

    /// Populates the hit object representing a hit without tracing a ray.
    /// It is legal to construct a hit which is not the closest hit along the ray
    /// or a hit which is not located along the ray.
    member _.RecordHit<'Attribute>(scene: Scene, instanceId: int, primitiveId: int, geometryIndex: int, origin: V3f, direction: V3f, attribute: 'Attribute, ray: Symbol,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
                                   [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32,
                                   [<Optional; DefaultParameterValue(RayHitKind.Default)>] kind: RayHitKind) : unit = onlyInShaderCode "RecordHit"


    // Populates the hit object representing a miss without tracing a ray.
    // It is legal to construct a miss in a hit object for a ray that
    // could have hit some geometry if traced.
    member _.RecordMiss(origin: V3f, direction: V3f, miss: MissId,
                        [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
                        [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32) : unit = onlyInShaderCode "RecordMiss"

    // Populates the hit object representing a miss without tracing a ray.
    // It is legal to construct a miss in a hit object for a ray that
    // could have hit some geometry if traced.
    member _.RecordMiss(origin: V3f, direction: V3f,
                        [<Optional; DefaultParameterValue(Identifier.Default)>] miss: string,
                        [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
                        [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32) : unit = onlyInShaderCode "RecordMiss"

    // Populates the hit object representing a miss without tracing a ray.
    // It is legal to construct a miss in a hit object for a ray that
    // could have hit some geometry if traced.
    member _.RecordMiss(origin: V3f, direction: V3f, miss: Symbol,
                        [<Optional; DefaultParameterValue(TraceDefaults.MinT)>] minT: float32,
                        [<Optional; DefaultParameterValue(TraceDefaults.MaxT)>] maxT: float32) : unit = onlyInShaderCode "RecordMiss"


    /// Encodes the hit object to represent an empty hit object which represents neither a hit nor a miss.
    [<KeepCall>]
    member _.RecordEmpty() : unit = onlyInShaderCode "RecordEmpty"


    /// Execute the closest-hit or miss shader encoded in the hit object.
    /// This call does not trigger reordering of threads.
    member _.ExecuteShader<'Payload>() : 'Payload = onlyInShaderCode "ExecuteShader"

    /// Execute the closest-hit or miss shader encoded in the hit object.
    /// This call does not trigger reordering of threads.
    member _.ExecuteShader<'Payload>(payload: 'Payload) : 'Payload = onlyInShaderCode "ExecuteShader"


    /// Returns the attributes encoded in the hit object.
    member _.GetAttributes<'Attribute>() : 'Attribute = onlyInShaderCode "GetAttributes"

[<AbstractClass; Sealed>]
type Thread =

    /// <summary>
    /// Reorder threads based on user provided hint. Similar hint values
    /// indicate similarity of subsequent work done after this call. Behavior
    /// is implementation defined.
    /// </summary>
    /// <param name="hint">Determines desired ordering of threads relative to others.</param>
    /// <param name="bits">Number of least significant bits an implementation should take into account from <paramref name="hint"/> in determining ordering.</param>
    [<KeepCall>]
    static member Reorder(hint: uint, bits: uint) : unit = onlyInShaderCode "Thread.Reorder"

    /// <summary>
    /// Reorder threads based on provided hit object, The exact properties
    /// from hit object which are used to reorder the threads is implementation
    /// defined.
    /// </summary>
    /// <param name="hitObject">The hit object to base the ordering on.</param>
    [<KeepCall>]
    static member Reorder(hitObject: HitObject) : unit = onlyInShaderCode "Thread.Reorder"

    /// <summary>
    /// Reorder threads based on provided hit object supplemented by additional
    /// information based on user provided hint. The exact properties from
    /// hit object and user specified hint which are used to reorder theads is
    /// implementation defined.
    /// </summary>
    /// <param name="hitObject">The hit object to base the ordering on.</param>
    /// <param name="hint">Determines desired ordering of threads relative to others.</param>
    /// <param name="bits">Number of least significant bits an implementation should take into account from <paramref name="hint"/> in determining ordering.</param>
    [<KeepCall>]
    static member Reorder(hitObject: HitObject, hint: uint, bits: uint) : unit = onlyInShaderCode "Thread.Reorder"


[<AutoOpen>]
module RaytracingIntrinsicsSER =

    // The hitObjectNV is weird as it cannot be constructed, you can only declare a variable with type hitObjectNV.
    // In F# we would have to write `let ho = Unchecked.defaultof<HitObject>`, which is a bit cumbersome and not.
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
                                   (hitKind : RayHitKind) (sbtRecordOffset : RayId) (sbtRecordStride : int) (origin : V3f) (minT : float32)
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