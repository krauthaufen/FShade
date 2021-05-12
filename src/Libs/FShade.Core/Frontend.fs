namespace FShade

open System
open System.Reflection

open Aardvark.Base

open FShade

type InlineAttribute = FShade.Imperative.InlineAttribute
type KeepCallAttribute = FShade.Imperative.KeepCallAttribute

[<AutoOpen>]
module Primitives =
    type Primitive<'a> = interface end

    type Point<'a> = Point of 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(0)>]
        member x.Value : 'a = onlyInShaderCode "read"

        static member VertexCount = 1
        static member InputTopology = InputTopology.Point

    type Line<'a> = Line of 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = onlyInShaderCode "read"

        static member VertexCount = 2
        static member InputTopology = InputTopology.Line
        member x.Interpolate(coord : float) : 'v = onlyInShaderCode "read"

    type Triangle<'a> = Triangle of 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = onlyInShaderCode "read"

        static member VertexCount = 3
        static member InputTopology = InputTopology.Triangle
        member x.Interpolate(coord : V3d) : 'v = onlyInShaderCode "read"

    type LineAdjacency<'a> = Triangle of 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"
            
            
        [<PrimitiveIndex(1)>]
        member x.P0 : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(2)>]
        member x.P1 : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(0)>]
        member x.Left : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(3)>]
        member x.Right : 'a = onlyInShaderCode "read"

        static member VertexCount = 4
        static member InputTopology = InputTopology.LineAdjacency

    type TriangleAdjacency<'a> = Triangle of 'a * 'a * 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(2)>]
        member x.P1 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(4)>]
        member x.P2 : 'a = onlyInShaderCode "read"
        
        [<PrimitiveIndex(1)>]
        member x.N01 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(3)>]
        member x.N12 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(5)>]
        member x.N20 : 'a = onlyInShaderCode "read"

        static member VertexCount = 6
        static member InputTopology = InputTopology.TriangleAdjacency

    type Patch3<'a> = Patch3 of 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = onlyInShaderCode "read"


        member x.TessCoord : V3d = onlyInShaderCode "read"

        static member VertexCount = 3
        static member InputTopology = InputTopology.Patch 3
        member x.Interpolate(coord : V3d) : 'v = onlyInShaderCode "read"

    type Patch4<'a> = Patch4 of 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"


        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(3)>]
        member x.P3 : 'a = onlyInShaderCode "read"

        member x.TessCoord : V3d = onlyInShaderCode "read"

        static member VertexCount = 4
        static member InputTopology = InputTopology.Patch 4
        member x.Interpolate(coord : V2d) : 'v = onlyInShaderCode "read"

    type Patch<'d, 'a when 'd :> INatural>() =
        static let dim = Peano.getSize typeof<'d>

        interface Primitive<'a>

        member x.InvocationId : int = onlyInShaderCode "read"

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"
            
        static member VertexCount = dim
        static member InputTopology = InputTopology.Patch dim
        

    let emitVertex() = ()

    let endPrimitive() = ()
    let restartStrip() = ()

    let inline ddx< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ddx"
    let inline ddy< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ddy"
    let inline ddxFine< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ddxFine"
    let inline ddyFine< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ddyFine"
    let inline ddxCoarse< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ddxCoarse"
    let inline ddyCoarse< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ffyCoarse"
    let discard () : unit = onlyInShaderCode "discard"

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
        static member BitFieldExtract (v : uint32, offset : int, bits : int) : uint32 = onlyInShaderCode "BitFieldExtract"
        static member BitFieldExtract (v : int, offset : int, bits : int) : int = onlyInShaderCode "BitFieldExtract"
        static member BitFieldExtract (v : uint64, offset : int, bits : int) : uint64 = onlyInShaderCode "BitFieldExtract"
        static member BitFieldExtract (v : int64, offset : int, bits : int) : int64 = onlyInShaderCode "BitFieldExtract"

        static member BitFieldInsert (v : uint32, insert : uint32, offset : int, bits : int) : uint32 = onlyInShaderCode "BitFieldInsert"
        static member BitFieldInsert (v : int, insert : int, offset : int, bits : int) : int = onlyInShaderCode "BitFieldInsert"
        static member BitFieldInsert (v : uint64, insert : uint64, offset : int, bits : int) : uint64 = onlyInShaderCode "BitFieldInsert"
        static member BitFieldInsert (v : int64, insert : int64, offset : int, bits : int) : int64 = onlyInShaderCode "BitFieldInsert"

        static member BitFieldReverse (v : uint32) : uint32 = onlyInShaderCode "BitFieldReverse"
        static member BitFieldReverse (v : int) : int = onlyInShaderCode "BitFieldReverse"
        static member BitFieldReverse (v : uint64) : uint64 = onlyInShaderCode "BitFieldReverse"
        static member BitFieldReverse (v : int64) : int64 = onlyInShaderCode "BitFieldReverse"

        static member BitCount (v : uint32) : int = onlyInShaderCode "BitCount"
        static member BitCount (v : int) : int = onlyInShaderCode "BitCount"
        static member BitCount (v : uint64) : int = onlyInShaderCode "BitCount"
        static member BitCount (v : int64) : int = onlyInShaderCode "BitCount"

        static member MSB(v : uint32) : int = onlyInShaderCode "MSB"
        static member MSB(v : int) : int = onlyInShaderCode "MSB"
        static member MSB(v : uint64) : int = onlyInShaderCode "MSB"
        static member MSB(v : int64) : int = onlyInShaderCode "MSB"

        static member LSB(v : uint32) : int = onlyInShaderCode "LSB"
        static member LSB(v : int) : int = onlyInShaderCode "LSB"
        static member LSB(v : uint64) : int = onlyInShaderCode "LSB"
        static member LSB(v : int64) : int = onlyInShaderCode "LSB"


        static member FloatBitsToInt(v : float) : int = onlyInShaderCode "FloatBitsToInt"
        static member FloatBitsToUInt(v : float) : uint32 = onlyInShaderCode "FloatBitsToUInt"
        static member IntBitsToFloat(v : int) : float = onlyInShaderCode "IntBitsToFloat"
        static member UIntBitsToFloat(v : uint32) : float = onlyInShaderCode "UIntBitsToFloat"



    let getGlobalId() : V3i = onlyInShaderCode "getGlobalId"
    let getWorkGroupId() : V3i = onlyInShaderCode "getWorkGroupId"
    let getLocalId() : V3i = onlyInShaderCode "getLocalId"
    let getLocalIndex() : int = onlyInShaderCode "getLocalIndex"
    let getWorkGroupSize() : V3i = onlyInShaderCode "getWorkGroupSize"
    let getWorkGroupCount() : V3i = onlyInShaderCode "getWorkGroupCount"
    let barrier() : unit = onlyInShaderCode "barrier"
    let allocateShared<'a when 'a : unmanaged> (size : int) : 'a[] =  onlyInShaderCode "allocateShared"

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

        override __.ToString() =
            sprintf "LocalSize(X=%d, Y=%d, Z=%d)" x y z

    type TessCoord<'a> = class end

    let tessellateTriangle (li : float) (l01 : float, l12 : float, l20 : float) : TessCoord<V3d> =
        onlyInShaderCode "tessellateTriangle"
        
    /// lu:  Inner tessellation level in horizontal (u) direction
    /// li:  Inner tessellation level in vertical (v) direction
    /// l01: Outer tessellation level for edge defined by u=0 (i.e. edge 0-1)
    /// l03: Outer tessellation level for edge defined by v=0 (i.e. edge 3-0)
    /// l23: Outer tessellation level for edge defined by u=1 (i.e. edge 2-3)
    /// l30: Outer tessellation level for edge defined by v=1 (i.e. edge 1-2)
    let tessellateQuad (lu : float, lv : float) (l01 : float, l30 : float, l23 : float, l12 : float) : TessCoord<V2d> =
        onlyInShaderCode "tessellateQuad"


[<AutoOpen>]
module ShaderBuilders =
    open Microsoft.FSharp.Quotations

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
            onlyInShaderCode ""

        member x.For(p : Primitive<'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            onlyInShaderCode ""

        member x.Delay(f : unit -> 'a) = f()

        member x.Quote() = ()

        member x.Combine(l : seq<Primitive<'a>>, r : seq<Primitive<'a>>) : seq<Primitive<'a>> =
            onlyInShaderCode ""

        member x.Zero() : seq<Primitive<'a>> = 
            Seq.empty

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Geometry
            member x.OutputTopology = Some top


    type TessBuilder() =
        inherit BaseBuilder()
        member x.Bind(t : TessCoord<'c>, f : 'c -> 'a) : 'a =
            onlyInShaderCode ""

        member x.Return(v) = v

        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.TessControl
            member x.OutputTopology = None

    type ComputeBuilder() =
        member x.For(a : Arr<'d, 'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.For(a : seq<'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.While(guard : unit -> bool, b : unit) =
            ()

        member x.Combine(l : unit, r : 'a) = r

        member x.Zero() = ()
        member x.Delay f = f()

        member x.Quote() = ()

        member inline x.Run(e : Expr<'a>) : Expr<'a> =
            let m = MethodBase.GetCurrentMethod()
            if isNull m then
                e
            else
                e.WithAttributes(Expr.NewTuple [ Expr.Value "Method"; Expr.Value m] :: e.CustomAttributes) |> Expr.Cast

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Compute
            member x.OutputTopology = None

    type RayGenerationBuilder() =
        inherit BaseBuilder()

        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.RayGeneration
            member x.OutputTopology = None

    type RayMissBuilder() =
        inherit BaseBuilder()

        member x.Return(v) = v
        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Miss
            member x.OutputTopology = None

    //type RayHitBuilder() =
    //    member x.For(a : Arr<'d, 'a>, f : 'a -> unit) : unit =
    //        for i in a do f i

    //    member x.For(a : seq<'a>, f : 'a -> unit) : unit =
    //        for i in a do f i

    //    member x.While(guard : unit -> bool, b : unit) =
    //        ()

    //    member x.Combine(l : unit, r : 'a) = r

    //    member x.Zero() = ()
    //    member x.Delay f = f()

    //    member x.Quote() = ()

    //    member inline x.Run(e : Expr<'a>) : Expr<'a> =
    //        e

    //    member x.Return(value) = value

    //    interface IShaderBuilder with
    //        member x.ShaderStage = ShaderStage.RayHitShader
    //        member x.OutputTopology = None


    let compute = ComputeBuilder()
    let vertex = VertexBuilder()
    let tessellation = TessBuilder()
    let fragment = FragmentBuilder()

    let raygen = RayGenerationBuilder()
    let miss = RayMissBuilder()

    //let scene =
    //    scene {
    //        accelerationStructure Unchecked.defaultof<_>
    //    }

    //let defaultRay =
    //    raytype {
    //        hitGroup "DefaultHitGroup"
    //        miss "DefaultMissShader"
    //        flags RayFlags.CullOpaque
    //    }

    //let foo =
    //    raygen {
    //        let result : float = scene.TraceRay(V3d.Zero, V3d.ZAxis, ray = defaultRay, cullMask = 0xFE)
    //        ()
    //    }

    //let oof =
    //    intersection {
    //        return (0.0, 0, 0)
    //    }

    let triangle = GeometryBuilder(None, OutputTopology.TriangleStrip)
    let line = GeometryBuilder(None, OutputTopology.LineStrip)
    let point = GeometryBuilder(None, OutputTopology.Points)

    let _triangle<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.TriangleStrip)
    let _line<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.LineStrip)
    let _point<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.Points)
