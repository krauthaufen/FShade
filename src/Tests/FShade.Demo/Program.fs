
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



[<EntryPoint>]
let main args =
    let hugo = 100
    let shader = ComputeShader.ofFunction (V3i(128, 128, 128)) (test <@ fun r -> hugo * r + 1 @>)

    let glsl =
        shader |> ComputeShader.toModule |> ModuleCompiler.compileGLSL410

    printfn "%s" glsl.code


    0

