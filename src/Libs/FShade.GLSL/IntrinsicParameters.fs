namespace FShade.GLSL.Utilities

open System
open System.Reflection

open Aardvark.Base
open Aardvark.Base.Monads.State

open FShade
open FShade.Imperative

[<AutoOpen>]
module IntrinsicParameters =

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


            ShaderStage.RayHitShader, 
                Map.ofList [
                    Intrinsics.LaunchId, "gl_LaunchIDNV"
                    Intrinsics.LaunchSize, "gl_LaunchSizeNV"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.InstanceId, "gl_InstanceID"
                    Intrinsics.InstanceCustomIndex, "gl_InstanceCustomIndexNV"
                    Intrinsics.WorldRayOrigin, "gl_WorldRayOriginNV"
                    Intrinsics.WorldRayDirection, "gl_WorldRayDirectionNV"
                    Intrinsics.ObjectRayOrigin, "gl_ObjectRayOriginNV"
                    Intrinsics.ObjectRayDirection, "gl_ObjectRayDirectionNV"
                    Intrinsics.RayTmin, "gl_RayTminNV"
                    Intrinsics.RayTmax, "gl_RayTmaxNV"
                    Intrinsics.IncomingRayFlags, "gl_IncomingRayFlagsNV"
                    Intrinsics.HitT, "gl_HitTNV"
                    Intrinsics.HitKind, "gl_HitKindNV"
                    Intrinsics.ObjectToWorld, "gl_ObjectToWorldNV"
                    Intrinsics.WorldToObject, "gl_WorldToObjectNV"
                ]
            ShaderStage.RayMissShader, Map.empty
            ShaderStage.RayGenShader, Map.empty
            ShaderStage.RayIntersectionShader, Map.empty
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
            

            ShaderStage.RayHitShader, Map.empty
            ShaderStage.RayMissShader, Map.empty
            ShaderStage.RayGenShader, Map.empty
            ShaderStage.RayIntersectionShader, Map.empty
        ]

    let prefixes =
        Dictionary.ofList [
            ShaderStage.Vertex,         "vs_"
            ShaderStage.TessControl,    "tc_"
            ShaderStage.TessEval,       "te_"
            ShaderStage.Geometry,       "gs_"
            ShaderStage.Fragment,       "fs_"
            ShaderStage.Compute,        "cs_"

            
            ShaderStage.RayHitShader,           ""
            ShaderStage.RayGenShader,           ""
            ShaderStage.RayMissShader,          ""
            ShaderStage.RayIntersectionShader,  ""
        ]  