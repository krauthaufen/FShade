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


    let monster (a : float) =
        if a < 10.0 then
            discard()

    [<Inline>]
    let f (a : float) (b : float) =
        let x = 10.0 * FShade.Imperative.ExpressionExtensions.ShaderIO.ReadInput<float>(FShade.Imperative.ParameterKind.Input, "SomeInput")
        a + b * uniform.A + x
        
    [<Inline>]
    let g (a : float) (b : float) =
        f a a + uniform.B

    type Vertex = { [<Semantic("Hugo")>] v : V3d }

    let shader (v : Vertex) =
        fragment {
//            // should be inlined
//            monster v.v.X
//
//            // should be removed
//            monster 12.0


            return V4d.IIII * g v.v.X v.v.Y
        }


    let run() =

        let effect = Effect.ofFunction shader
        
        let glsl = 
            effect
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


