namespace FShade

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Aardvark.Base


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
    type DepthAttribute() = inherit SemanticAttribute(Intrinsics.Depth)
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


[<AutoOpen>]
module Builders =

    type TessLevels = 
        { [<TessLevelInner>] innerLevel : float[]; 
          [<TessLevelOuter>] outerLevel : float[] }


    type IShaderBuilder =
        abstract member ShaderType : ShaderType

    type BaseBuilder() =
        member x.For(a : Arr<'d, 'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.For(a : seq<'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.Combine(l : unit, r : 'a) = r

        member x.Zero() = ()
        member x.Delay f = f()

    type VertexBuilder() =
        inherit BaseBuilder()
        member x.Return(v) = v

        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderType = ShaderType.Vertex
    
    type FragmentBuilder() =
        inherit BaseBuilder()
        member x.Return(v) = v
        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderType = ShaderType.Fragment

    type GeometryBuilder(size : Option<int>, top : OutputTopology) =
        member x.For(a : Arr<'d, 'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            a |> Seq.collect f

        member x.For(a : seq<'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            a |> Seq.collect f

        member x.Size = size

        member x.Yield(v : 'a) : seq<Primitive<'a>> = 
            failwith ""

        member x.For(p : Primitive<'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            failwith ""

        member x.Delay(f : unit -> 'a) = f()

        member x.Quote() = ()

        member x.Combine(l : seq<Primitive<'a>>, r : seq<Primitive<'a>>) : seq<Primitive<'a>> =
            failwith ""

        member x.Zero() : seq<Primitive<'a>> = 
            Seq.empty

        interface IShaderBuilder with
            member x.ShaderType = ShaderType.Geometry (size, top)


    
    type TessControlBuilder() =
        inherit BaseBuilder()

        member x.Quote() = ()
        member x.Delay f = f()
        member x.Return(levels : TessLevels) =
            levels

        interface IShaderBuilder with
            member x.ShaderType = ShaderType.TessControl

    type TessEvalBuilder() =
        inherit BaseBuilder()

        member x.Quote() = ()
        member x.Delay f = f()
        member x.Return(v) = v
        interface IShaderBuilder with
            member x.ShaderType = ShaderType.TessEval

    let vertex = VertexBuilder()
    let fragment = FragmentBuilder()

    let triangle = GeometryBuilder(None, OutputTopology.TriangleStrip)
    let line = GeometryBuilder(None, OutputTopology.LineStrip)
    let point = GeometryBuilder(None, OutputTopology.Points)

    let _triangle<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.TriangleStrip)
    let _line<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.LineStrip)
    let _point<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.Points)


    let tessControl = TessControlBuilder()
    let tessEval = TessEvalBuilder()

    let (|BuilderCall|_|) (e : Expr) =
        match e with
            | Call(Some t, mi, args) when typeof<IShaderBuilder>.IsAssignableFrom t.Type ->
                BuilderCall(t, mi, args) |> Some
            | _ -> None