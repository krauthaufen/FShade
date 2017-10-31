namespace Examples

open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open System.Reflection

[<ReflectedDefinition>]
module UtiliyFunctions =
    
    type UniformScope with
        member x.A : float = x?A
        member x.B : float = x?B

    type Vertex = 
        { 
            [<Position>] pos : V4d
            [<Semantic("Hugo")>] hugo: V3d 
        }

    
    [<Inline>]
    let monster (a : float) =
        if a < 10.0 then
            discard()

    [<Inline>]
    let f (a : float) (b : float) =
        let x = 10.0 * FShade.Imperative.ExpressionExtensions.ShaderIO.ReadInput<float>(FShade.Imperative.ParameterKind.Input, "SomeInput")
        a + b * uniform.A + x
  
    let g (a : float) (b : float) =
        f a a + uniform.B

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


    let run() =

        let effect = 
            Effect.compose [
                Effect.ofFunction vs
                Effect.ofFunction shader
            ]

        let glsl = 
            effect
                |> Effect.toLayeredEffect 2 (Set.ofList [ "A" ]) InputTopology.Triangle
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


