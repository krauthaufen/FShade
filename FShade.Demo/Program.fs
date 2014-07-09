// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open Aardvark.Base
open FShade
open FShade.Demo


[<EntryPoint>]
let main argv = 

    let effect = [Shaders.simpleTrafoShader |> toEffect
                  Shaders.textureShader |> toEffect] |> compose

//    match GLSL.compileEffect effect with
//        | Success (uniforms, code) ->
//            for (k,v) in uniforms |> Map.toSeq do
//                if v.IsSamplerUniform then
//                    let sem,sam = v.Value |> unbox<string * SamplerState>
//                    printfn "%s -> %s:\r\n%A" sem k sam
//
//            printfn "%s" code
//        | Error e ->
//            printfn "ERROR: %A" e
//

    //FShade.Resources.Icon |> printfn "%A"

    let w = new Window()

    let ps = [|V3f.OOO; V3f.IOO; V3f.IIO; V3f.OIO|] :> Array
    let tc = [|V2f(-0.5,-0.5); V2f(1.5, -0.5); V2f(1.5,1.5); V2f(-0.5, 1.5)|] :> Array
    let indices = [|0;1;2; 0;2;3|] :> Array

    let sg = Sg.geometry (Some indices) (["Positions", ps; "TexCoords", tc] |> Map.ofList)
    let sg = Sg.shader "Main" effect sg


    FShade.Debug.EffectEditor.runTray()

    let sg = Sg.fileTexture "DiffuseTexture" @"C:\Aardwork\DataSVN\pattern.jpg" sg

    w.Scene <- sg

    w.Run()

    0
