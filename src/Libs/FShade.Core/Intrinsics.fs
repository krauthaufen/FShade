namespace FShade

open System
open System.Reflection

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

type TessLevels = 
    { 
        [<TessLevelInner>] innerLevel : float[]
        [<TessLevelOuter>] outerLevel : float[] 
    }
