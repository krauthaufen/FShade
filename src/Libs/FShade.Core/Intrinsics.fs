namespace FShade

open Aardvark.Base
open FShade

module Intrinsics =

    [<Literal>]
    let Position = "Positions"

    [<Literal>]
    let FragCoord = "FragCoord"


    [<Literal>]
    let VertexId = "VertexId"
    [<Literal>]
    let InstanceId = "InstanceId"
    [<Literal>]
    let PointSize = "PointSize"
    [<Literal>]
    let ClipDistance = "ClipDistance"

    [<Literal>]
    let PatchVertices = "PatchVertices"
    [<Literal>]
    let PrimitiveId = "PrimitiveId"
    [<Literal>]
    let InvocationId = "InvocationId"
    [<Literal>]
    let TessCoord = "TessCoord"

    [<Literal>]
    let Depth = "Depth"
    [<Literal>]
    let Color = "Colors"
    [<Literal>]
    let SecondaryColor = "Color2"

    [<Literal>]
    let TessLevelInner = "TessLevelInner"
    [<Literal>]
    let TessLevelOuter = "TessLevelOuter"

    [<Literal>]
    let FrontFacing = "FrontFacing"
    [<Literal>]
    let PointCoord = "PointCoord"
    [<Literal>]
    let SampleId = "SampleId"
    [<Literal>]
    let SamplePosition = "SamplePosition"
    [<Literal>]
    let SampleMask = "SampleMask"

    [<Literal>]
    let Layer = "Layer"
    [<Literal>]
    let ViewportIndex = "ViewportIndex"

    [<Literal>]
    let SourceVertexIndex = "GeometrySourceVertexIndex"

    // mesh shader primitive indices
    [<Literal>]
    let PrimitiveIndices = "MeshPrimitiveIndices"

    [<Literal>]
    let FragmentPosition = "Positions0"

    // Work dimensions
    [<Literal>]
    let LaunchId = "LaunchId"
    [<Literal>]
    let LaunchSize = "LaunchSize"

    // Geometry instance ids
    [<Literal>]
    let InstanceCustomIndex = "InstanceCustomIndex"
    [<Literal>]
    let GeometryIndex = "GeometryIndex"

    // World space parameters
    [<Literal>]
    let WorldRayOrigin = "WorldRayOrigin"
    [<Literal>]
    let WorldRayDirection = "WorldRayDirection"
    [<Literal>]
    let ObjectRayOrigin = "ObjectRayOrigin"
    [<Literal>]
    let ObjectRayDirection = "ObjectRayDirection"

    // Ray parameters
    [<Literal>]
    let RayTmin = "RayTmin"
    [<Literal>]
    let RayTmax = "RayTmax"
    [<Literal>]
    let IncomingRayFlags = "IncomingRayFlags"
    [<Literal>]
    let RayPayloadIn = "RayPayloadIn"

    // Ray hit info
    [<Literal>]
    let HitT = "HitT"
    [<Literal>]
    let HitKind = "HitKind"
    [<Literal>]
    let HitAttribute = "HitAttribute"
    [<Literal>]
    let HitPositions = "HitPositions"

    // Transform matrices
    [<Literal>]
    let ObjectToWorld = "ObjectToWorld"
    [<Literal>]
    let WorldToObject = "WorldToObject"

    // Callable
    [<Literal>]
    let CallableDataIn = "CallableDataIn"

[<AutoOpen>]
module InstrinsicAttributes =
    type PositionAttribute() = inherit SemanticAttribute(Intrinsics.Position)
    type FragCoordAttribute() = inherit SemanticAttribute(Intrinsics.FragCoord)
    type VertexIdAttribute() = inherit SemanticAttribute(Intrinsics.VertexId)
    type InstanceIdAttribute() = inherit SemanticAttribute(Intrinsics.InstanceId)
    type PointSizeAttribute() = inherit SemanticAttribute(Intrinsics.PointSize)
    type ClipDistanceAttribute() = inherit SemanticAttribute(Intrinsics.ClipDistance)
    type PatchVerticesAttribute() = inherit SemanticAttribute(Intrinsics.PatchVertices)
    type PrimitiveIdAttribute() = inherit SemanticAttribute(Intrinsics.PrimitiveId)
    type InvocationIdAttribute() = inherit SemanticAttribute(Intrinsics.InvocationId)
    type TessCoordAttribute() = inherit SemanticAttribute(Intrinsics.TessCoord)
    type ColorAttribute() = inherit SemanticAttribute(Intrinsics.Color)
    type SecondaryColorAttribute() = inherit SemanticAttribute(Intrinsics.SecondaryColor)
    type TessLevelInnerAttribute() = inherit SemanticAttribute(Intrinsics.TessLevelInner)
    type TessLevelOuterAttribute() = inherit SemanticAttribute(Intrinsics.TessLevelOuter)
    type FrontFacingAttribute() = inherit SemanticAttribute(Intrinsics.FrontFacing)
    type PointCoordAttribute() = inherit SemanticAttribute(Intrinsics.PointCoord)
    type SampleIdAttribute() = inherit SemanticAttribute(Intrinsics.SampleId)
    type SamplePositionAttribute() = inherit SemanticAttribute(Intrinsics.SamplePosition)
    type SampleMaskAttribute() = inherit SemanticAttribute(Intrinsics.SampleMask)
    type LayerAttribute() = inherit SemanticAttribute(Intrinsics.Layer)
    type ViewportIndexAttribute() = inherit SemanticAttribute(Intrinsics.ViewportIndex)

    type SourceVertexIndexAttribute() = inherit SemanticAttribute(Intrinsics.SourceVertexIndex)

    type LaunchIdAttribute() =            inherit SemanticAttribute(Intrinsics.LaunchId)
    type LaunchSizeAttribute() =          inherit SemanticAttribute(Intrinsics.LaunchSize)
    type InstanceCustomIndexAttribute() = inherit SemanticAttribute(Intrinsics.InstanceCustomIndex)
    type GeometryIndexAttribute() =       inherit SemanticAttribute(Intrinsics.GeometryIndex)
    type WorldRayOriginAttribute() =      inherit SemanticAttribute(Intrinsics.WorldRayOrigin)
    type WorldRayDirectionAttribute() =   inherit SemanticAttribute(Intrinsics.WorldRayDirection)
    type ObjectRayOriginAttribute() =     inherit SemanticAttribute(Intrinsics.ObjectRayOrigin)
    type ObjectRayDirectionAttribute() =  inherit SemanticAttribute(Intrinsics.ObjectRayDirection)
    type RayTminAttribute() =             inherit SemanticAttribute(Intrinsics.RayTmin)
    type RayTmaxAttribute() =             inherit SemanticAttribute(Intrinsics.RayTmax)
    type IncomingRayFlagsAttribute() =    inherit SemanticAttribute(Intrinsics.IncomingRayFlags)
    type HitTAttribute() =                inherit SemanticAttribute(Intrinsics.HitT)
    type HitKindAttribute() =             inherit SemanticAttribute(Intrinsics.HitKind)
    type HitAttributeAttribute() =        inherit SemanticAttribute(Intrinsics.HitAttribute)
    type HitPositionsAttribute() =        inherit SemanticAttribute(Intrinsics.HitPositions)
    type RayPayloadInAttribute() =        inherit SemanticAttribute(Intrinsics.RayPayloadIn)
    type ObjectToWorldAttribute() =       inherit SemanticAttribute(Intrinsics.ObjectToWorld)
    type WorldToObjectAttribute() =       inherit SemanticAttribute(Intrinsics.WorldToObject)
    type CallableDataInAttribute() =      inherit SemanticAttribute(Intrinsics.CallableDataIn)


type TessLevels =
    {
        [<TessLevelInner>] innerLevel : float32[]
        [<TessLevelOuter>] outerLevel : float32[]
    }

module RaytracingInputTypes =

    type WorkDimensions =
        {
            /// The index of the work item being processed.
            [<LaunchId>]   id   : V3i

            /// The number of work items in each dimension.
            [<LaunchSize>] size : V3i
        }

    type GeometryInstance =
        {
            /// The index of the triangle or bounding box being processed.
            [<PrimitiveId>]         primitiveId         : int

            /// The index of the instance that intersects the current ray.
            [<InstanceId>]          instanceId          : int

            /// The application defined value of the instance that intersects
            /// the current ray. The value provided in this built-in is obtained
            /// from the lower 24 bits of the variable, the upper 8 bits are zero.
            [<InstanceCustomIndex>] instanceCustomIndex : int

            /// The geometry index for the acceleration structure geometry currently being shaded.
            [<GeometryIndex>]       geometryIndex       : int
        }

    type RayParameters =
        {
            /// The origin of the ray being processed in world space.
            [<WorldRayOrigin>]    origin    : V3f

            /// The direction of the ray being processed in world space.
            [<WorldRayDirection>] direction : V3f

            /// The parametric minT value of the ray being processed.
            /// The value is independent of the space in which the ray origin and direction exist.
            [<RayTmin>]           minT      : float32

            /// The parametric maxT value of the ray being processed.
            /// The value is independent of the space in which the ray origin and direction exist.
            [<RayTmax>]           maxT      : float32

            /// The flags of the current ray.
            [<IncomingRayFlags>]  flags     : RayFlags
        }

    type ObjectSpace =
        {
            /// The origin of the ray being processed in object space.
            [<ObjectRayOrigin>]    rayOrigin     : V3f

            /// The direction of the ray being processed in object space.
            [<ObjectRayDirection>] rayDirection  : V3f

            /// The object-to-world transformation matrix determined
            /// by the instance of the current intersection.
            [<ObjectToWorld>]      objectToWorld : M34f

            /// The world-to-object transformation matrix determined
            /// by the instance of the current intersection.
            [<WorldToObject>]      worldToObject : M34f
        }

    type RayHit<'Attribute> =
        {
            /// The parametric value of the ray being processed.
            /// The value is independent of the space in which the ray origin and direction exist.
            [<HitT>]         t          : float32

            /// Describes the intersection that triggered the execution of the current
            /// shader. Values are sent from the intersection shader. For triangle
            /// geometry, kind is set to FrontFacingTriangle or BackFacingTriangle.
            [<HitKind>]      kind       : RayHitKind

            /// Attribute written by the intersection shader.
            /// For triangle geometry without custom intersection shader, the attribute
            /// is a V2f containing the barycentric coordinates of the hit.
            [<HitAttribute>] attribute  : 'Attribute

            /// <summary>
            /// The object space vertices of the triangle at the current intersection.
            /// The positions returned are transformed by the geometry transform.
            /// </summary>
            /// <remarks>
            /// Requires GL_EXT_ray_tracing_position_fetch.
            /// </remarks>
            [<HitPositions>] positions  : Arr<3 N, V3f>
        }

/// Type containing input available in ray generation shaders.
type RayGenerationInput =
    {
        work : RaytracingInputTypes.WorkDimensions
    }

/// Type containing input available in ray intersection shaders.
type RayIntersectionInput =
    {
        work        : RaytracingInputTypes.WorkDimensions
        geometry    : RaytracingInputTypes.GeometryInstance
        ray         : RaytracingInputTypes.RayParameters
        objectSpace : RaytracingInputTypes.ObjectSpace
    }

/// Type containing input available in ray any-hit and closest-hit shaders.
type RayHitInput<'Payload, 'Attribute> =
    {
        work        : RaytracingInputTypes.WorkDimensions
        geometry    : RaytracingInputTypes.GeometryInstance
        ray         : RaytracingInputTypes.RayParameters
        hit         : RaytracingInputTypes.RayHit<'Attribute>
        objectSpace : RaytracingInputTypes.ObjectSpace

        /// The payload passed to TraceRay().
        [<RayPayloadIn>] payload : 'Payload
    }

type RayHitInput           = RayHitInput<unit, V2f>
type RayHitInput<'Payload> = RayHitInput<'Payload, V2f>

/// Type containing input available in ray miss shaders.
type RayMissInput<'Payload> =
    {
        work : RaytracingInputTypes.WorkDimensions
        ray  : RaytracingInputTypes.RayParameters
        [<RayPayloadIn>] payload : 'Payload
    }

type RayMissInput = RayMissInput<unit>

/// Type containing input available in ray callable shaders.
type RayCallableInput<'Data> =
    {
        work : RaytracingInputTypes.WorkDimensions
        [<CallableDataIn>] data : 'Data
    }