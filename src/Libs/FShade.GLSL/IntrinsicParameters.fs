namespace FShade.GLSL.Utilities

open Aardvark.Base
open FShade

[<AutoOpen>]
module IntrinsicParameters =

    let nonIndexedGSInputs =
        Set.ofList [
            "gl_InvocationID"
            "gl_PrimitiveID"
        ]

    let builtInInputs =
        Dictionary.ofList [
            ShaderStage.Vertex,
                Map.ofList [
                    Intrinsics.VertexId, "gl_VertexID"
                    Intrinsics.InstanceId, "gl_InstanceID"
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.PatchVertices, "gl_PatchVertices"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.InvocationId, "gl_InvocationID"
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.TessCoord, "gl_TessCoord"
                    Intrinsics.PatchVertices, "gl_PatchVertices"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.TessLevelInner, "gl_TessLevelInner"
                    Intrinsics.TessLevelOuter, "gl_TessLevelOuter"
                ]

            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.InvocationId, "gl_InvocationID"
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.FragCoord, "gl_FragCoord"
                    Intrinsics.PointCoord, "gl_PointCoord"
                    Intrinsics.FrontFacing, "gl_FrontFacing"
                    Intrinsics.SampleId, "gl_SampleID"
                    Intrinsics.SamplePosition, "gl_SamplePosition"
                    Intrinsics.SampleMask, "gl_SampleMaskIn"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.Layer, "gl_Layer"
                    Intrinsics.ViewportIndex, "gl_ViewportIndex"
                ]

            ShaderStage.Compute, Map.empty

            ShaderStage.RayGeneration,
                Map.ofList [
                    Intrinsics.LaunchId, "ivec3(gl_LaunchIDEXT)"
                    Intrinsics.LaunchSize, "ivec3(gl_LaunchSizeEXT)"
                ]

            ShaderStage.Intersection,
                Map.ofList [
                    Intrinsics.LaunchId, "ivec3(gl_LaunchIDEXT)"
                    Intrinsics.LaunchSize, "ivec3(gl_LaunchSizeEXT)"

                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.InstanceId, "gl_InstanceID"
                    Intrinsics.InstanceCustomIndex, "gl_InstanceCustomIndexEXT"
                    Intrinsics.GeometryIndex, "gl_GeometryIndexEXT"

                    Intrinsics.WorldRayOrigin, "gl_WorldRayOriginEXT"
                    Intrinsics.WorldRayDirection, "gl_WorldRayDirectionEXT"
                    Intrinsics.ObjectRayOrigin, "gl_ObjectRayOriginEXT"
                    Intrinsics.ObjectRayDirection, "gl_ObjectRayDirectionEXT "

                    Intrinsics.RayTmin, "gl_RayTminEXT"
                    Intrinsics.RayTmax, "gl_RayTmaxEXT"
                    Intrinsics.IncomingRayFlags, "gl_IncomingRayFlagsEXT"

                    Intrinsics.ObjectToWorld, "mat4(gl_ObjectToWorldEXT)"
                    Intrinsics.WorldToObject, "mat4(gl_WorldToObjectEXT)"
                ]

            ShaderStage.AnyHit,
                Map.ofList [
                    Intrinsics.LaunchId, "ivec3(gl_LaunchIDEXT)"
                    Intrinsics.LaunchSize, "ivec3(gl_LaunchSizeEXT)"

                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.InstanceId, "gl_InstanceID"
                    Intrinsics.InstanceCustomIndex, "gl_InstanceCustomIndexEXT"
                    Intrinsics.GeometryIndex, "gl_GeometryIndexEXT"

                    Intrinsics.WorldRayOrigin, "gl_WorldRayOriginEXT"
                    Intrinsics.WorldRayDirection, "gl_WorldRayDirectionEXT"
                    Intrinsics.ObjectRayOrigin, "gl_ObjectRayOriginEXT"
                    Intrinsics.ObjectRayDirection, "gl_ObjectRayDirectionEXT "

                    Intrinsics.RayTmin, "gl_RayTminEXT"
                    Intrinsics.RayTmax, "gl_RayTmaxEXT"
                    Intrinsics.IncomingRayFlags, "gl_IncomingRayFlagsEXT"

                    Intrinsics.HitT, "gl_HitTEXT"
                    Intrinsics.HitKind, "gl_HitKindEXT"

                    Intrinsics.ObjectToWorld, "mat4(gl_ObjectToWorldEXT)"
                    Intrinsics.WorldToObject, "mat4(gl_WorldToObjectEXT)"
                ]

            ShaderStage.ClosestHit,
                Map.ofList [
                    Intrinsics.LaunchId, "ivec3(gl_LaunchIDEXT)"
                    Intrinsics.LaunchSize, "ivec3(gl_LaunchSizeEXT)"

                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.InstanceId, "gl_InstanceID"
                    Intrinsics.InstanceCustomIndex, "gl_InstanceCustomIndexEXT"
                    Intrinsics.GeometryIndex, "gl_GeometryIndexEXT"

                    Intrinsics.WorldRayOrigin, "gl_WorldRayOriginEXT"
                    Intrinsics.WorldRayDirection, "gl_WorldRayDirectionEXT"
                    Intrinsics.ObjectRayOrigin, "gl_ObjectRayOriginEXT"
                    Intrinsics.ObjectRayDirection, "gl_ObjectRayDirectionEXT "

                    Intrinsics.RayTmin, "gl_RayTminEXT"
                    Intrinsics.RayTmax, "gl_RayTmaxEXT"
                    Intrinsics.IncomingRayFlags, "gl_IncomingRayFlagsEXT"

                    Intrinsics.HitT, "gl_HitTEXT"
                    Intrinsics.HitKind, "gl_HitKindEXT"

                    Intrinsics.ObjectToWorld, "mat4(gl_ObjectToWorldEXT)"
                    Intrinsics.WorldToObject, "mat4(gl_WorldToObjectEXT)"
                ]

            ShaderStage.Miss,
                Map.ofList [
                    Intrinsics.LaunchId, "ivec3(gl_LaunchIDEXT)"
                    Intrinsics.LaunchSize, "ivec3(gl_LaunchSizeEXT)"

                    Intrinsics.WorldRayOrigin, "gl_WorldRayOriginEXT"
                    Intrinsics.WorldRayDirection, "gl_WorldRayDirectionEXT"

                    Intrinsics.RayTmin, "gl_RayTminEXT"
                    Intrinsics.RayTmax, "gl_RayTmaxEXT"
                    Intrinsics.IncomingRayFlags, "gl_IncomingRayFlagsEXT"
                ]

            ShaderStage.Callable,
                Map.ofList [
                    Intrinsics.LaunchId, "ivec3(gl_LaunchIDEXT)"
                    Intrinsics.LaunchSize, "ivec3(gl_LaunchSizeEXT)"
                ]
        ]


    let builtInOutputs =
        Dictionary.ofList [
            ShaderStage.Vertex,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.TessLevelInner, "gl_TessLevelInner"
                    Intrinsics.TessLevelOuter, "gl_TessLevelOuter"
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                ]

            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.Layer, "gl_Layer"
                    Intrinsics.ViewportIndex, "gl_ViewportIndex"
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.Depth, "gl_FragDepth"
                    Intrinsics.SampleMask, "gl_SampleMask"
                ]

            ShaderStage.Compute, Map.empty
            ShaderStage.RayGeneration, Map.empty
            ShaderStage.Intersection, Map.empty
            ShaderStage.AnyHit, Map.empty
            ShaderStage.ClosestHit, Map.empty
            ShaderStage.Miss, Map.empty
            ShaderStage.Callable, Map.empty
        ]