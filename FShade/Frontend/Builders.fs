namespace FShade

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Aardvark.Base

[<AutoOpen>]
module Builders =

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

    type GeometryBuilder(top : OutputTopology) =
        inherit BaseBuilder()
        member x.Yield(v : 'a) : seq<Primitive<'a>> = failwith ""
        member x.For(p : Primitive<'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            failwith ""
        member x.Delay(f : unit -> 'a) = f()
        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderType = ShaderType.Geometry top


    type TessLevels = { [<Semantic("TessLevelInner")>] innerLevel : float[]; [<Semantic("TessLevelOuter")>] outerLevel : float[] }

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
    let triangle = GeometryBuilder(OutputTopology.TriangleStrip)
    let line = GeometryBuilder(OutputTopology.LineStrip)
    let point = GeometryBuilder(OutputTopology.Points)
    let tessControl = TessControlBuilder()
    let tessEval = TessEvalBuilder()

    let (|BuilderCall|_|) (e : Expr) =
        match e with

            | Call(Some t, mi, args) when typeof<IShaderBuilder>.IsAssignableFrom t.Type ->
                BuilderCall(t, mi, args) |> Some
            | _ -> None