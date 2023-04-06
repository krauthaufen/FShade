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
    type RayPayloadInAttribute() =        inherit SemanticAttribute(Intrinsics.RayPayloadIn)
    type ObjectToWorldAttribute() =       inherit SemanticAttribute(Intrinsics.ObjectToWorld)
    type WorldToObjectAttribute() =       inherit SemanticAttribute(Intrinsics.WorldToObject)
    type CallableDataInAttribute() =      inherit SemanticAttribute(Intrinsics.CallableDataIn)


type TessLevels =
    {
        [<TessLevelInner>] innerLevel : float[]
        [<TessLevelOuter>] outerLevel : float[]
    }

module RaytracingInputTypes =

    type WorkDimensions =
        {
            [<LaunchId>]   id   : V3i
            [<LaunchSize>] size : V3i
        }

    type GeometryInstance =
        {
            [<PrimitiveId>]         primitiveId         : int
            [<InstanceId>]          instanceId          : int
            [<InstanceCustomIndex>] instanceCustomIndex : int
            [<GeometryIndex>]       geometryIndex       : int
        }

    type RayParameters =
        {
            [<WorldRayOrigin>]    origin    : V3d
            [<WorldRayDirection>] direction : V3d
            [<RayTmin>]           minT      : float
            [<RayTmax>]           maxT      : float
            [<IncomingRayFlags>]  flags     : RayFlags
        }

    type ObjectSpace =
        {
            [<ObjectRayOrigin>]    rayOrigin     : V3d
            [<ObjectRayDirection>] rayDirection  : V3d
            [<ObjectToWorld>]      objectToWorld : M44d
            [<WorldToObject>]      worldToObject : M44d
        }

    type RayHit<'T> =
        {
            [<HitT>]         t          : float
            [<HitKind>]      kind       : RayHitKind
            [<HitAttribute>] attribute  : 'T
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

/// Type containing input available in ray any hit and closest hit shaders.
type RayHitInput<'Payload, 'HitAttribute> =
    {
        work        : RaytracingInputTypes.WorkDimensions
        geometry    : RaytracingInputTypes.GeometryInstance
        ray         : RaytracingInputTypes.RayParameters
        hit         : RaytracingInputTypes.RayHit<'HitAttribute>
        objectSpace : RaytracingInputTypes.ObjectSpace
        [<RayPayloadIn>] payload : 'Payload
    }

type RayHitInput           = RayHitInput<unit, V2d>
type RayHitInput<'Payload> = RayHitInput<'Payload, V2d>

/// Type containing input available in ray miss shaders.
type RayMissInput<'Payload> =
    {
        work : RaytracingInputTypes.WorkDimensions
        ray  : RaytracingInputTypes.RayParameters
        [<RayPayloadIn>] payload : 'Payload
    }

type RayMissInput = RayMissInput<unit>

/// Type containing input available in ray callable shaders.
type RayCallableInput<'T> =
    {
        work : RaytracingInputTypes.WorkDimensions
        [<CallableDataIn>] data : 'T
    }