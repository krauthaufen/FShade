namespace FShade

open System
open System.Reflection

open Aardvark.Base

open FShade

[<AutoOpen>]
module Primitives =
    let inline private shaderOnlyAccess() = failwith "[FShade] cannot execute shader-only function"

    type Primitive<'a> = interface end

    type Point<'a> = Point of 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(0)>]
        member x.Value : 'a = shaderOnlyAccess()

        static member VertexCount = 1
        static member InputTopology = InputTopology.Point

    type Line<'a> = Line of 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = shaderOnlyAccess()

        static member VertexCount = 2
        static member InputTopology = InputTopology.Line
        member x.Interpolate(coord : float) : 'v = shaderOnlyAccess()

    type Triangle<'a> = Triangle of 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = shaderOnlyAccess()

        static member VertexCount = 3
        static member InputTopology = InputTopology.Triangle
        member x.Interpolate(coord : V3d) : 'v = shaderOnlyAccess()

    type LineAdjacency<'a> = Triangle of 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()
            
            
        [<PrimitiveIndex(1)>]
        member x.P0 : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(2)>]
        member x.P1 : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(0)>]
        member x.Left : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(3)>]
        member x.Right : 'a = shaderOnlyAccess()

        static member VertexCount = 4
        static member InputTopology = InputTopology.LineAdjacency

    type TriangleAdjacency<'a> = Triangle of 'a * 'a * 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(2)>]
        member x.P1 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(4)>]
        member x.P2 : 'a = shaderOnlyAccess()
        
        [<PrimitiveIndex(1)>]
        member x.N01 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(3)>]
        member x.N12 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(5)>]
        member x.N20 : 'a = shaderOnlyAccess()

        static member VertexCount = 6
        static member InputTopology = InputTopology.TriangleAdjacency

    type Patch3<'a> = Patch3 of 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = shaderOnlyAccess()


        member x.TessCoord : V3d = shaderOnlyAccess()

        static member VertexCount = 3
        static member InputTopology = InputTopology.Patch 3
        member x.Interpolate(coord : V3d) : 'v = shaderOnlyAccess()

    type Patch4<'a> = Patch4 of 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()


        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(3)>]
        member x.P3 : 'a = shaderOnlyAccess()

        member x.TessCoord : V3d = shaderOnlyAccess()

        static member VertexCount = 4
        static member InputTopology = InputTopology.Patch 4
        member x.Interpolate(coord : V2d) : 'v = shaderOnlyAccess()

    type Patch<'d, 'a when 'd :> INatural>() =
        static let dim = Peano.getSize typeof<'d>

        interface Primitive<'a>

        member x.InvocationId : int = failwith ""

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()
            
        static member VertexCount = dim
        static member InputTopology = InputTopology.Patch dim
        

    let emitVertex() = ()

    let endPrimitive() = ()
    let restartStrip() = ()

    let inline ddx< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = failwith "_"
    let inline ddy< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = failwith "_"
    let inline ddxFine< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = failwith "_"
    let inline ddyFine< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = failwith "_"
    let inline ddxCoarse< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = failwith "_"
    let inline ddyCoarse< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = failwith "_"
    let discard () : unit = failwith "_"

    [<ReflectedDefinition>]
    let packUnorm2x16 (v : V2d) : uint32 =
        let h = (clamp 0.0 1.0 v.X) * 65535.0 |> round |> uint32
        let l = (clamp 0.0 1.0 v.Y) * 65535.0 |> round |> uint32
        (h <<< 16) ||| l

    [<ReflectedDefinition>]
    let packSnorm2x16 (v : V2d) : uint32 = 
        let h = (clamp 0.0 1.0 (0.5 * v.X + 0.5)) * 65535.0 |> round |> uint32
        let l = (clamp 0.0 1.0 (0.5 * v.Y + 0.5)) * 65535.0 |> round |> uint32
        (h <<< 16) ||| l

    [<ReflectedDefinition>]
    let packUnorm4x8 (v : V4d) : uint32 =
        let r = (clamp 0.0 1.0 v.X) * 255.0 |> round |> uint32
        let g = (clamp 0.0 1.0 v.Y) * 255.0 |> round |> uint32
        let b = (clamp 0.0 1.0 v.Z) * 255.0 |> round |> uint32
        let a = (clamp 0.0 1.0 v.W) * 255.0 |> round |> uint32
        (r <<< 24) ||| (g <<< 16) ||| (b <<< 8) ||| a
        
    [<ReflectedDefinition>]
    let packSnorm4x8 (v : V4d) : uint32 = 
        let r = (clamp 0.0 1.0 (0.5 * v.X + 0.5)) * 255.0 |> round |> uint32
        let g = (clamp 0.0 1.0 (0.5 * v.Y + 0.5)) * 255.0 |> round |> uint32
        let b = (clamp 0.0 1.0 (0.5 * v.Z + 0.5)) * 255.0 |> round |> uint32
        let a = (clamp 0.0 1.0 (0.5 * v.W + 0.5)) * 255.0 |> round |> uint32
        (r <<< 24) ||| (g <<< 16) ||| (b <<< 8) ||| a
        
    [<ReflectedDefinition>]
    let unpackUnorm2x16 (v : uint32) : V2d =
        let x = float (v >>> 16) / 65535.0
        let y = float (v &&& 0xFFFFu) / 65535.0
        V2d(x,y)
        
    [<ReflectedDefinition>]
    let unpackSnorm2x16 (v : uint32) : V2d = 
        let x = float (v >>> 16) / 65535.0
        let y = float (v &&& 0xFFFFu) / 65535.0
        V2d(2.0 * x - 1.0, 2.0 * y - 1.0)
        
    [<ReflectedDefinition>]
    let unpackUnorm4x8 (v : uint32) : V4d = 
        let x = float (v >>> 24) / 255.0
        let y = float ((v >>> 16) &&& 0xFFu) / 255.0
        let z = float ((v >>> 8) &&& 0xFFu) / 255.0
        let w = float (v &&& 0xFFu) / 255.0
        V4d(x,y,z,w)
        
    [<ReflectedDefinition>]
    let unpackSnorm4x8 (v : uint32) : V4d = 
        let x = float (v >>> 24) / 255.0
        let y = float ((v >>> 16) &&& 0xFFu) / 255.0
        let z = float ((v >>> 8) &&& 0xFFu) / 255.0
        let w = float (v &&& 0xFFu) / 255.0
        V4d(2.0 * x - 1.0, 2.0 * y - 1.0, 2.0 * z - 1.0, 2.0 * w - 1.0)

    [<AbstractClass; Sealed>]
    type Bitwise private() = 
        static member BitFieldExtract (v : uint32, offset : int, bits : int) : uint32 = failwith ""
        static member BitFieldExtract (v : int, offset : int, bits : int) : int = failwith ""
        static member BitFieldExtract (v : uint64, offset : int, bits : int) : uint64 = failwith ""
        static member BitFieldExtract (v : int64, offset : int, bits : int) : int64 = failwith ""

        static member BitFieldInsert (v : uint32, insert : uint32, offset : int, bits : int) : uint32 = failwith ""
        static member BitFieldInsert (v : int, insert : int, offset : int, bits : int) : int = failwith ""
        static member BitFieldInsert (v : uint64, insert : uint64, offset : int, bits : int) : uint64 = failwith ""
        static member BitFieldInsert (v : int64, insert : int64, offset : int, bits : int) : int64 = failwith ""

        static member BitFieldReverse (v : uint32) : uint32 = failwith ""
        static member BitFieldReverse (v : int) : int = failwith ""
        static member BitFieldReverse (v : uint64) : uint64 = failwith ""
        static member BitFieldReverse (v : int64) : int64 = failwith ""

        static member BitCount (v : uint32) : int = failwith ""
        static member BitCount (v : int) : int = failwith ""
        static member BitCount (v : uint64) : int = failwith ""
        static member BitCount (v : int64) : int = failwith ""

        static member MSB(v : uint32) : int = failwith ""
        static member MSB(v : int) : int = failwith ""
        static member MSB(v : uint64) : int = failwith ""
        static member MSB(v : int64) : int = failwith ""

        static member LSB(v : uint32) : int = failwith ""
        static member LSB(v : int) : int = failwith ""
        static member LSB(v : uint64) : int = failwith ""
        static member LSB(v : int64) : int = failwith ""



    let getGlobalId() : V3i = failwith "_"
    let getWorkGroupId() : V3i = failwith ""
    let getLocalId() : V3i = failwith ""
    let getLocalIndex() : int = failwith ""
    let getWorkGroupSize() : V3i = failwith ""
    let getWorkGroupCount() : V3i = failwith ""
    let barrier() : unit = failwith ""
    let allocateShared<'a when 'a : unmanaged> (size : int) : 'a[] = failwith ""

    [<Literal>]
    let MaxLocalSize = 2147483647

    let LocalSize = V3i(2147483647, 2147483647, 2147483647)

    type LocalSizeAttribute() = 
        inherit System.Attribute()

        let mutable x = 1
        let mutable y = 1
        let mutable z = 1

        member __.X
            with get() = x
            and set v = x <- v

        member __.Y
            with get () = y
            and set v = y <- v

        member __.Z
            with get () = z
            and set v = z <- v


    type TessCoord<'a> = class end

    let tessellateTriangle (li : float) (l01 : float, l12 : float, l20 : float) : TessCoord<V3d> =
        failwith ""
        
    let tessellateQuad (lx : float, ly : float) (l01 : float, l12 : float, l23 : float, l30 : float) : TessCoord<V2d> =
        failwith ""


[<AutoOpen>]
module ShaderBuilders =
    type BaseBuilder() =
        member x.For(a : Arr<'d, 'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.For(a : seq<'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.While(guard : unit -> bool, b : unit) =
            ()

        member x.Combine(l : unit, r : 'a) = r

        member x.Zero() = ()
        member x.Delay f = f()

    type VertexBuilder() =
        inherit BaseBuilder()
        member x.Return(v) = v

        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Vertex
            member x.OutputTopology = None


    type FragmentBuilder() =
        inherit BaseBuilder()
        member x.Return(v) = v
        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Fragment
            member x.OutputTopology = None

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
            member x.ShaderStage = ShaderStage.Geometry
            member x.OutputTopology = Some top


    type TessBuilder() =
        inherit BaseBuilder()
        member x.Bind(t : TessCoord<'c>, f : 'c -> 'a) : 'a =
            failwith ""

        member x.Return(v) = v

        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.TessControl
            member x.OutputTopology = None

    type ComputeBuilder() =
        inherit BaseBuilder()

        member x.Quote() = ()
        member x.Zero() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Compute
            member x.OutputTopology = None


    let compute = ComputeBuilder()
    let vertex = VertexBuilder()
    let tessellation = TessBuilder()
    let fragment = FragmentBuilder()

    let triangle = GeometryBuilder(None, OutputTopology.TriangleStrip)
    let line = GeometryBuilder(None, OutputTopology.LineStrip)
    let point = GeometryBuilder(None, OutputTopology.Points)

    let _triangle<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.TriangleStrip)
    let _line<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.LineStrip)
    let _point<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.Points)
