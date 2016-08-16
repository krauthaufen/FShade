// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open Aardvark.Base
open FShade
open FShade.Demo


module Simple =
    open Aardvark.Base
    open FShade
    
    type UniformScope with
        member x.CameraLocation : V3d = x?PerView?CameraLocation
        member x.LightLocation : V3d = x?PerLight?LightLocation

        member x.ModelTrafo : M44d = x?PerModel?ModelTrafo
        member x.ViewProjTrafo : M44d = x?PerView?ViewProjTrafo

    type V = { [<Semantic("Positions")>] p : V4d 
               [<Semantic("World")>] wp : V4d
               [<Semantic("Normals")>] n : V3d
               [<Semantic("Tangents")>] t : V3d
               [<Semantic("BiNormals")>] b : V3d
               [<Semantic("TexCoords")>] tc : V2d
               [<Semantic("Colors")>] color : V4d 
               [<ClipDistance>] cd : float[] 
              }

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
            let world = uniform.ModelTrafo * v.p
            return { v with p = uniform.ViewProjTrafo * world; wp = world }
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
            let c : V4d = uniform?PerView?Color
            return c
        }
            
    let texture (v : V) =
           fragment {
               return DiffuseColorTexture.Sample(v.tc)
        }
            
    let pointSurface (size : V2d) (p : Point<V>) =
        let sx = size.X
        let sy = size.Y
        triangle {
            let v = p.Value

            match p.Value with
                | { p = pos; n = n } ->

                    let pxyz = pos.XYZ / pos.W
            
                    let p00 = V3d(pxyz + V3d( -sx, -sy, 0.0 ))
                    let p01 = V3d(pxyz + V3d( -sx,  sy, 0.0 ))
                    let p10 = V3d(pxyz + V3d(  sx, -sy, 0.0 ))
                    let p11 = V3d(pxyz + V3d(  sx,  sy, 0.0 ))

                    yield { p.Value with p = uniform.ViewProjTrafo * V4d(p00 * pos.W, pos.W); tc = V2d.OO; cd = [|1.0; 2.0|] } 
                    yield { p.Value with p = uniform.ViewProjTrafo * V4d(p10 * pos.W, pos.W); tc = V2d.IO; cd = [|1.0; 2.0|] }
                    yield { p.Value with p = uniform.ViewProjTrafo * V4d(p01 * pos.W, pos.W); tc = V2d.OI; cd = [|1.0; 2.0|] }
                    yield { p.Value with p = uniform.ViewProjTrafo * V4d(p11 * pos.W, pos.W); tc = V2d.II; cd = [|1.0; 2.0|] }

        }
            
    let light (v : V) =
        fragment {
            let n = v.n.Normalized

            let c = uniform.CameraLocation - v.wp.XYZ |> Vec.normalize
            let l = uniform.LightLocation - v.wp.XYZ |> Vec.normalize
            let r = -Vec.reflect c n |> Vec.normalize

            let d = Vec.dot l n |> clamp 0.0 1.0
            let s = Vec.dot r l |> clamp 0.0 1.0

            return  v.color.XYZ * (0.2 + 0.8 * d) + V3d.III * pow s 64.0
        }    
 
 
module Dead =
    type BillboardVertex =
        {
            [<Position>] position : V4d
            [<Color>] color : V4d
            [<SemanticAttribute("DiffuseColorCoordinate")>] texCoord : V2d
            [<SemanticAttribute("ViewPosition")>] viewPos : V4d
        }

    type UniformScope with
        member x.ModelViewTrafo : M44d = uniform?PerModel?ModelViewTrafo
        member x.ProjTrafo : M44d = uniform?PerView?ProjTrafo
        member x.UserSelected : bool = uniform?UserSelected

    let BillboardTrafo (v : BillboardVertex) =
        vertex {
            let vp = uniform.ModelViewTrafo * v.position
            let pp = uniform.ProjTrafo * vp
            return {
                position = pp
                texCoord = V2d(0,0)
                color = v.color
                viewPos = vp
            }
        }

    let BillboardGeometry (sizes: V2d) (distanceScalingFactor : float -> float) (v : Point<BillboardVertex>) =
        let targetRange = 5.0

        _triangle<4 N> {
            let s = sizes
            let offsetX = s.X
            let offsetY = s.Y

            let bv = v.Value
            let pos = bv.position
            let vp = bv.viewPos
            let c = bv.color

            let factor =
                let d = abs(vp.Z)
                let e = 
                    if d < targetRange then
                        1.0 - ( d / targetRange )
                    else
                        0.0
                1.0 + e
        
            let offsetX = offsetX * factor
            let offsetY = offsetY * factor

            let TopLeft =       V4d(vp.XYZ + V3d(   -offsetX,   -offsetY, 0.0),vp.W)
            let TopRight =      V4d(vp.XYZ + V3d(    offsetX,   -offsetY, 0.0),vp.W)
            let BottomLeft =    V4d(vp.XYZ + V3d(   -offsetX,    offsetY, 0.0),vp.W)
            let BottomRight =   V4d(vp.XYZ + V3d(    offsetX,    offsetY, 0.0),vp.W)

            let TLO =   uniform.ProjTrafo * TopLeft
            let TRO =   uniform.ProjTrafo * TopRight
            let BLO =   uniform.ProjTrafo * BottomLeft
            let BRO =   uniform.ProjTrafo * BottomRight
        
            yield { position = TLO; color = c; texCoord = V2d(0, 1); viewPos = vp }
            yield { position = TRO; color = c; texCoord = V2d(1, 1); viewPos = vp }
            yield { position = BLO; color = c; texCoord = V2d(0, 0); viewPos = vp }
            yield { position = BRO; color = c; texCoord = V2d(1, 0); viewPos = vp }
            }

    let BillboardFragment (color: V4d) (v : BillboardVertex) =
        fragment {
            let c = color
            let s = uniform.UserSelected
            let t = v.texCoord
            let comp = V2d(0.5, 0.5)
            let dist = t - comp
            let len = dist.Length
            if len < 0.5 then
                if s then
                    return c
                else
                    return v.color
            else
                discard()
                return V4d(0,0,0,0)
        } 
           
            
            


[<EntryPoint>]
let main argv = 

    let effect = [Simple.trafo |> toEffect
                  Simple.pointSurface (V2d(0.06, 0.08)) |> toEffect
                  Simple.white |> toEffect
                  ] |> compose

    let res = GLSL.compileEffect GLSL.version410 (Map.ofList["Colors", typeof<V4d>]) effect
    match res with
        | Success (uniforms, code) ->
            for (name,getter) in Map.toSeq uniforms do
                match getter with
                    | AttributeGetter(name, t) -> printfn "%s : %A" name t
                    | _ -> ()
            printfn "%s" code
        | _ ->
            ()

    Environment.Exit 0
//    let effect = [Simple.trafo |> toEffect
//                  Simple.pointSurface (V2d(0.06, 0.08)) |> toEffect
//                  Simple.white |> toEffect
//                  ] |> compose


    let w = new Window()

    let ps = [|V3f.OOO; V3f.IOO; V3f.IIO; V3f.OIO|] :> Array
    let tc = [|V2f.OO; V2f.IO; V2f.II; V2f.OI|] :> Array
    let n = [|V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI|] :> Array
    let b = [|V3f.IOO; V3f.IOO; V3f.IOO; V3f.IOO|] :> Array
    let t = [|V3f.OIO; V3f.OIO; V3f.OIO; V3f.OIO|] :> Array
    let indices = [|0;1;2; 0;2;3|] :> Array

    let sg = Sg.geometry (Some indices) (["Positions", ps; "TexCoords", tc; "Normals", n; "BiNormals", b; "Tangents", t] |> Map.ofList)
    let sg = Sg.shader "Main" effect sg


    FShade.Debug.EffectEditor.runTray()

    let sg = Sg.fileTexture "DiffuseTexture" @"E:\Development\WorkDirectory\DataSVN\pattern.jpg" sg
    let sg = Sg.fileTexture "NormalMap"      @"E:\Development\WorkDirectory\DataSVN\bump.jpg" sg

    let sg = Sg.uniform "Color" (V4f(1,1,1,1)) sg


    w.Scene <- sg

    w.Run()

    0
