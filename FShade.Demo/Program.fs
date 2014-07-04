// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Aardvark.Base
open FShade
open FShade.Demo

[<EntryPoint>]
let main argv = 

    let effect = [Shaders.simpleTrafoShader |> toEffect
                  Shaders.simpleFragmentShader V4d.IIII |> toEffect] |> compose

    match GLSL.compileEffect effect with
        | Success (uniforms, code) ->
            printfn "%s" code
        | Error e ->
            printfn "ERROR: %A" e

    
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
