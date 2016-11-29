namespace FShade

open Aardvark.Base

[<AllowNullLiteral>]
type PrimitiveIndexAttribute(index : int) =
    inherit System.Attribute()

    member x.Index = index

[<AutoOpen>]
module Primitives =

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


    let emitVertex() = ()

    let endPrimitive() = ()
    let restartStrip() = ()

    let inline ddx< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = failwith "_"
    let inline ddy< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = failwith "_"
    let discard () : unit = failwith "_"


    let getInnerTessLevel (i : int) : float = failwith ""
    let getOuterTessLevel (i : int) : float = failwith ""

open System.Reflection

type Preprocessor private() =
    static let methods = typeof<Preprocessor>.GetMethods(BindingFlags.Public ||| BindingFlags.Static)

    static member unroll() = ()
    static member unroll(min : int, max : int) = ()
