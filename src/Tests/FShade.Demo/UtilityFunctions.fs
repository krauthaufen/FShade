namespace Examples

open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open System.Reflection

module UtiliyFunctions =
    
    type UniformScope with
        member x.A : float = x?A
        member x.B : float = x?B
        member x.AB : float = x?AB

    type Vertex = 
        { 
            [<Position>] pos : V4d
            [<Semantic("Hugo")>] hugo: V3d 
        }
        
    [<ReflectedDefinition; AutoOpen>]
    module Helpers =
        [<Inline>]
        let monster (a : float) =
            if a < 10.0 then
                discard()

        [<Inline>]
        let f (a : float) (b : float) =
            let x = 10.0 * FShade.Imperative.ExpressionExtensions.ShaderIO.ReadInput<float>(FShade.Imperative.ParameterKind.Input, "SomeInput")
            a + b * uniform.A + x
  
        let g (a : float) (b : float) =
            f a a + uniform.AB

    let vs (v : Vertex) =
        vertex {
            return { v with pos = uniform.A * v.pos }
        }

    let shader (v : Vertex) =
        fragment {
            // should be removed
            monster 12.0

            return V4d.IIII * g v.hugo.X v.hugo.Y
        }

    let gs0 (v : Triangle<Vertex>) =
        triangle {
            yield { v.P0 with hugo = V3d.IOO }
            yield { v.P1 with hugo = V3d.OIO }
            yield { v.P2 with hugo = V3d.OOI }
        }

    let gs1 (v : Triangle<Vertex>) =
        triangle {
            yield { v.P0 with pos = 2.0 * v.P0.pos }
            yield { v.P1 with pos = 2.0 * v.P1.pos }
            yield { v.P2 with pos = 2.0 * v.P2.pos }
            restartStrip()
            yield { v.P0 with pos = 3.0 * v.P0.pos }
            yield { v.P1 with pos = 3.0 * v.P1.pos }
            yield { v.P2 with pos = 3.0 * v.P2.pos }

        }

    let print (effect : Effect) =
        match effect.LastShader with
            | Some shader ->
                let mutable index = 0
                let id () =
                    let i = index
                    index <- i + 1
                    i

                let config =
                    {
                        depthRange      = Range1d(-1.0, 1.0)
                        flipHandedness  = false
                        lastStage       = shader.shaderStage
                        outputs         = shader.shaderOutputs |> Map.map (fun name desc -> desc.paramType, id())
                    }
                let glsl = 
                    effect
                        // compile the thing
                        |> Effect.toModule config
                        |> ModuleCompiler.compileGLSL410
                printfn "%s" glsl.code

            | None ->
                ()

    let run() =

        let effect =
            Effect.compose [
                Effect.ofFunction gs0
                Effect.ofFunction gs1
                Effect.ofFunction gs1
            ]

        print effect
        System.Environment.Exit 0

        let effect = 
            Effect.compose [
                Effect.ofFunction vs
                Effect.ofFunction shader
            ]

        let glsl = 
            effect
                // decompose derived uniforms here
                |> Effect.substituteUniforms (fun name typ index ->
                    if name = "AB" then
                        let a = Expr.ReadInput<float>(ParameterKind.Uniform, "A")
                        let b = Expr.ReadInput<float>(ParameterKind.Uniform, "B")
                        Some <@@ (%a) * (%b) @@>
                    else
                        None
                   )
                // one of the uniforms gets layered
                |> Effect.toLayeredEffect 2 (Set.ofList [ "A" ]) InputTopology.Triangle
                
                // compile the thing
                |> Effect.toModule (EffectConfig.ofList [Intrinsics.Color, typeof<V4d>, 0])
                |> ModuleCompiler.compileGLSL410

        printfn "%s" glsl.code
//
//        let e =
//            <@
//                let thing =
//                    if true then
//                        let mutable a = 10
//                        for i in 0 .. 10 do
//                            a <- a + i
//                        a
//                    else
//                        10
//
//                thing
//            @>
//
//        Optimizer.DeExpr.deExpr e |> string |> printfn "%s"


        ()


