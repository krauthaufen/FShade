// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open Aardvark.Base
open FShade
open FShade.Demo


module Simple =
    open Aardvark.Base
    open FShade
    
    type V = { [<Semantic("Positions")>] p : V4d 
               [<Semantic("Normals")>] n : V3d
               [<Semantic("Tangents")>] t : V3d
               [<Semantic("BiNormals")>] b : V3d
               [<Semantic("TexCoords")>] tc : V2d
               [<Semantic("Colors")>] color : V4d }

    let DiffuseColorTexture =
           sampler2d {
               texture uniform?DiffuseTexture
        }
                
    let NormalMap =
           sampler2d {
               texture uniform?NormalMap
        }

    let trafo(v : V) =
        vertex {
            let model : M44d = uniform?ModelTrafo
            let vp : M44d = uniform?ViewProjTrafo
            let world = model * v.p
            return { v with p = vp * world }
        }

    let normals(v : V) =
        fragment {
            return V4d(0.5 * (v.n.Normalized + V3d.III), 1.0)
        }
        
    let bump (v : V) =
        fragment {
            let s = 2.0 * NormalMap.Sample(v.tc).XYZ - V3d.III
            let n = s.X * v.t + s.Y * v.b + s.Z * v.n
            return { v with n = n.Normalized }
        }
        
    let white (v : V) =
        fragment {
            return V4d.IIII
        }
            
    let texture (v : V) =
           fragment {
               return DiffuseColorTexture.Sample(v.tc)
        }
            
            
    let light (v : V) =
        fragment {
            let n = v.n.Normalized
            return  v.color.XYZ * (0.6 + 0.6 * Vec.dot n V3d.OIO)
        }    
            
            
            


[<EntryPoint>]
let main argv = 

    let effect = [Shaders.simpleTrafoShader |> toEffect
                  Shaders.textureShader |> toEffect] |> compose


    let effect = [Simple.trafo |> toEffect
                  Simple.texture |> toEffect
                  Simple.light |> toEffect] |> compose

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
    let n = [|V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI|] :> Array
    let indices = [|0;1;2; 0;2;3|] :> Array

    let sg = Sg.geometry (Some indices) (["Positions", ps; "TexCoords", tc; "Normals", n] |> Map.ofList)
    let sg = Sg.shader "Main" effect sg


    FShade.Debug.EffectEditor.runTray()

    let sg = Sg.fileTexture "DiffuseTexture" @"E:\Development\WorkDirectory\DataSVN\pattern.jpg" sg

    w.Scene <- sg

    w.Run()

    0
