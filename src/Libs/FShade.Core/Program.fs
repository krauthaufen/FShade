
open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open System.Reflection

let sink(a,b) = ()

type GLSLBackend private() =
    inherit Compiler.Backend()
    static let instance = GLSLBackend() :> Compiler.Backend

    static member Instance = instance

    override x.TryGetIntrinsicMethod (c : MethodInfo) =
        match c with
            | MethodQuote <@ sink @> _          -> CIntrinsic.simple "sink" |> Some
            | MethodQuote <@ sin @> _           -> CIntrinsic.simple "sin" |> Some
            | MethodQuote <@ cos @> _           -> CIntrinsic.simple "cos" |> Some
            | MethodQuote <@ clamp @> _         -> CIntrinsic.custom "clamp" [2;0;1] |> Some
            | MethodQuote <@ discard @> _       -> CIntrinsic.simple "discard" |> Some
            | _ -> None
    
    override x.TryGetIntrinsicCtor (c : ConstructorInfo) =
        None

[<EntryPoint>]
let main args =
    let optimized = 
        Optimizer.eliminateDeadCode 
            <@
                let mutable a = 0
                let mutable b = 0
                for i in 0 .. 2 .. 10 do
                    a <- a + b
                    b <- b + a
                sink(a,a)

                let mutable y = 0 
                let mutable z = 1
                if (z <- z + 1; y < 10) then
                    y <- 10
                else
                    y <- 100

                sink(z,z)

                ()
            @>
    
    let _, optimized = optimized |> State.run Optimizer.EliminationState.empty

    let entry =
        Expr.Lambda(Var("unitVar", typeof<unit>), optimized)
                |> Module.ofLambda "test"

//    let entry =
//        Module.ofLambda "test" <@ fun () ->
//            let mutable a = 0
//            let mutable b = 0
//            for i in 8 .. -1 .. 0 do
//                a <- a + 1
//                b <- b + 1
//            a
//        @>

    let entry =
        entry
            |> Linker.compileAndLink GLSLBackend.Instance
            |> GLSL.CModule.glsl  { 
                GLSL.Config.version = System.Version(4,1,0)
                GLSL.Config.locations = false
                GLSL.Config.perStageUniforms = true
                GLSL.Config.uniformBuffers = true 
            }

    printfn "%s" entry
    0

