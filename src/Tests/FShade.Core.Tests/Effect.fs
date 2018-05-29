module Effect

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open FsUnit
open Xunit
open Xunit.Sdk

open Aardvark.Base
open Aardvark.Base.Monads.State

open FShade

#nowarn "4321"

type Vertex =
    {
        [<Position>] pos : V4d
        [<Color>] color : V4d
    }

let shader0 (v : Vertex) =
    vertex {
        return { v with pos = V4d.IIII + v.pos }
    }

let shader1 (offset : V4d) (v : Vertex) =
    vertex {
        return { v with pos = offset + v.pos }
    }

let shader3 (a : V4d) (b : V4d) (v : Vertex) =
    vertex {
        return { v with pos = a + b * v.pos }
    }
    
let setup() =
    Effect.clearCaches()

[<Fact>]
let ``[OfFunction] static``() =
    setup()
    let e0 = Effect.ofFunction shader0
    let e1 = Effect.ofFunction shader0
    e0 |> should equal e1

[<Fact>]
let ``[OfFunction] static with closure``() =
    setup()
    let e0 = Effect.ofFunction (shader1 V4d.OIOI)
    let e1 = Effect.ofFunction (shader1 V4d.OIOI)
    e0 |> should equal e1
    let e2 = Effect.ofFunction (shader1 V4d.IOIO)
    e2 |> should not' (equal e1)

[<Fact>]
let ``[OfFunction] local``() =
    setup()
    let shader2 (v : Vertex) =
        vertex {
            return { v with pos = V4d.IIII }
        } 
    let e0 = Effect.ofFunction shader2
    let e1 = Effect.ofFunction shader2
    e0 |> should equal e1

[<Fact>]
let ``[OfFunction] local with closure value``() =
    setup()
    let aaaa = 2.0
    let shader213 (p : V4d) (v : Vertex) =
        vertex {
            return {  pos = aaaa * p; color = v.color }
        } 

    let e0 = Effect.ofFunction (shader213 V4d.OIOI)
    let e1 = Effect.ofFunction (shader213 V4d.OIOI)
    e0 |> should equal e1
    let e2 = Effect.ofFunction (shader213 V4d.IOIO)
    e2 |> should not' (equal e1)

[<Fact>] 
let ``[OfFunction] static curried closure``() =
    setup()
    let t0 = shader3 V4d.Zero
    let t1 = t0 V4d.IIII

    let e0 = Effect.ofFunction t1
    let e1 = Effect.ofFunction (fun a -> shader3 V4d.Zero V4d.IIII a)
    let e2 = Effect.ofFunction (fun a -> t0 V4d.IIII a)
    e0 |> should equal e1
    e0 |> should equal e2
    let e3 = Effect.ofFunction (fun a -> shader3 V4d.IIII V4d.Zero a)
    e3 |> should not' (equal e0)

[<Fact>] 
let ``[OfFunction] local curried closure``() =
    setup()
    let shader2 (a : V4d) (b : V4d) (v : Vertex) =
        vertex {
            return { v with pos = a + b + v.pos }
        }


    let t0 = shader2 V4d.Zero
    let t1 = t0 V4d.IIII

    let e0 = Effect.ofFunction t1
    let e1 = Effect.ofFunction (fun a -> shader2 V4d.Zero V4d.IIII a)
    let e2 = Effect.ofFunction (fun a -> t0 V4d.IIII a)
    e0 |> should equal e1
    e0 |> should equal e2
    let e3 = Effect.ofFunction (fun a -> shader2 V4d.IIII V4d.Zero a)
    e3 |> should not' (equal e0)




[<Fact>]
let ``[Compose] associativity``() =
    setup()
    let a = Effect.ofFunction shader0
    let b = Effect.ofFunction (shader1 V4d.IIII)
    let c = Effect.ofFunction (shader3 V4d.IIII V4d.IIII)

    let r = Effect.compose [ a; Effect.compose [ b; c ] ]
    let l = Effect.compose [ Effect.compose [ a; b ]; c ]

    l |> should equal r

[<Fact>]
let ``[Compose] neutral element``() =
    setup()
    let z = Effect.empty
    let a = Effect.ofFunction shader0

    Effect.compose [ z; a ] |> should equal a
    Effect.compose [ a; z ] |> should equal a

[<Fact>] 
let ``[Compose] caching``() =
    setup()
    let a = Effect.ofFunction shader0
    let b = Effect.ofFunction (shader1 V4d.IIII)
    let c = Effect.ofFunction (shader3 V4d.IIII V4d.IIII)

    let e = Effect.compose [ a; b; c ]
    Effect.compose [ a; b; c ] |> should equal e
