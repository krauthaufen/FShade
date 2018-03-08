
open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open System.Reflection
open System.Threading
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open System

[<LocalSize(X = 64)>]
let test (f : Expr<int -> int>) (hugo : int) (a : int[]) (b : int[]) =
    compute {
        a.[0] <- (%f) b.[hugo]
    }


type Vertex =
    {
        [<Position>]
        pos : V4d
    }

let test2 (v : Vertex) =
    fragment {
        return V3d.IOO
    }


[<EntryPoint>]
let main args =
    let hugo = 100

    let effect = Effect.ofFunction test2

    let shader = ComputeShader.ofFunction (V3i(128, 128, 128)) (test <@ fun r -> hugo * r + 1 @>)

    let glsl =
        effect |> Effect.toModule { EffectConfig.empty with outputs = Map.ofList ["Colors", (typeof<V4d>, 0)] } |> ModuleCompiler.compileGLSL410

    printfn "%s" glsl.code


    0

