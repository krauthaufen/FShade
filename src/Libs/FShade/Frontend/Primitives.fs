namespace FShade

open Aardvark.Base

[<AutoOpen>]
module Primitives =

    type Primitive<'a> = interface end

    type Point<'a> = Point of 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()

        member x.Value : 'a = shaderOnlyAccess()

        static member VertexCount = 1
        static member InputTopology = InputTopology.Point

    type Line<'a> = Line of 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()

        member x.P0 : 'a = shaderOnlyAccess()
        member x.P1 : 'a = shaderOnlyAccess()

        static member VertexCount = 2
        static member InputTopology = InputTopology.Line

    type Triangle<'a> = Triangle of 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()

        member x.P0 : 'a = shaderOnlyAccess()
        member x.P1 : 'a = shaderOnlyAccess()
        member x.P2 : 'a = shaderOnlyAccess()

        static member VertexCount = 3
        static member InputTopology = InputTopology.Triangle

    type Patch3<'a> = Patch4 of 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()

        member x.P0 : 'a = shaderOnlyAccess()
        member x.P1 : 'a = shaderOnlyAccess()
        member x.P2 : 'a = shaderOnlyAccess()


        member x.TessCoord : V3d = shaderOnlyAccess()

        static member VertexCount = 3
        static member InputTopology = InputTopology.Patch 3

    type Patch4<'a> = Patch4 of 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()

        member x.P0 : 'a = shaderOnlyAccess()
        member x.P1 : 'a = shaderOnlyAccess()
        member x.P2 : 'a = shaderOnlyAccess()
        member x.P3 : 'a = shaderOnlyAccess()

        member x.TessCoord : V3d = shaderOnlyAccess()

        static member VertexCount = 4
        static member InputTopology = InputTopology.Patch 4


    let emitVertex() = ()
    let discard () : unit = failwith "_"