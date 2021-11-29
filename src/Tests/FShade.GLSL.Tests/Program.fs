module Program

open System
open System.Reflection
open System.IO
open Aardvark.Base
open FShade
open FShade.Imperative
open Microsoft.FSharp.Quotations
open FShade.GLSL

module Trafo = 
    
    type Vertex = {
        [<Position>]        pos     : V4d
        [<Color>]           c       : V4d
        [<Semantic("Normals")>]           n       : V3d
    }


    type UniformScope with
        member x.ModelTrafo : M44d = x?PerModel?ModelTrafo
        member x.ViewTrafo : M44d = x?PerView?ViewTrafo
        member x.ProjTrafo : M44d = x?PerView?ProjTrafo
        member x.ViewProjTrafo : M44d = x?PerView?ViewProjTrafo
        member x.ModelViewTrafo : M44d = x?PerModel?ModelViewTrafo
        member x.ModelViewProjTrafo : M44d = x?PerModel?ModelViewProjTrafo
        member x.NormalMatrix : M33d = x?PerModel?NormalMatrix

        member x.ModelTrafoInv : M44d = x?PerModel?ModelTrafoInv
        member x.ViewTrafoInv : M44d = x?PerView?ViewTrafoInv
        member x.ProjTrafoInv : M44d = x?PerView?ProjTrafoInv
        member x.ViewProjTrafoInv : M44d = x?PerView?ViewProjTrafoInv
        member x.ModelViewTrafoInv : M44d = x?PerModel?ModelViewTrafoInv
        member x.ModelViewProjTrafoInv : M44d = x?PerModel?ModelViewProjTrafoInv

        member x.CameraLocation : V3d = x?PerView?CameraLocation
        member x.LightLocation : V3d = x?PerLight?LightLocation



        member x.LineWidth : float = x?LineWidth
        member x.LineColor : V4d = x?LineColor

        member x.PointSize : float = x?PointSize
        member x.PointColor : V4d = x?PointColor

        member x.ViewportSize : V2i = x?PerView?ViewportSize


        member x.DiffuseColor : V4d = x?PerMaterial?DiffuseColor
        member x.AmbientColor : V4d = x?PerMaterial?DiffuseColor
        member x.EmissiveColor : V4d = x?PerMaterial?EmissiveColor
        member x.SpecularColor : V4d = x?PerMaterial?SpecularColor
        member x.Shininess : float = x?PerMaterial?Shininess

        member x.DiffuseColorTexture : ShaderTextureHandle = x?DiffuseColorTexture
        member x.AmbientColorTexture : ShaderTextureHandle = x?AmbientColorTexture
        member x.EmissiveColorTexture : ShaderTextureHandle = x?EmissiveColorTexture
        member x.SpecularColorTexture : ShaderTextureHandle = x?SpecularColorTexture
        member x.ShininessTexture : ShaderTextureHandle = x?HasShininessTexture
        member x.LightMapTexture : ShaderTextureHandle = x?LightMapTexture
        member x.NormalMapTexture : ShaderTextureHandle = x?NormalMapTexture

        member x.HasDiffuseColorTexture : bool = x?PerMaterial?HasDiffuseColorTexture
        member x.HasAmbientColorTexture : bool = x?PerMaterial?HasAmbientColorTexture
        member x.HasEmissiveColorTexture : bool = x?PerMaterial?HasEmissiveColorTexture
        member x.HasSpecularColorTexture : bool = x?PerMaterial?HasSpecularColorTexture
        member x.HasShininessTexture : bool = x?PerMaterial?HasShininessTexture
        member x.HasLightMapTexture : bool = x?PerMaterial?HasLightMapTexture
        member x.HasNormalMapTexture : bool = x?PerMaterial?HasNormalMapTexture


    let internal trafo (v : Vertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            return {
                pos = uniform.ViewProjTrafo * wp
                c = v.c
                n = uniform.ModelTrafoInv.Transposed * V4d(v.n, 0.0) |> Vec.xyz
            }
        }

module ThickLine = 

    type ThickLineVertex = {
        [<Position>]                pos     : V4d
        [<Color>]                   c       : V4d
        [<Semantic("LineCoord")>]   lc      : V2d
        [<Semantic("Width")>]       w       : float
        [<Semantic("Normals")>]           n       : V3d
    }

    [<ReflectedDefinition>]
    let clipLine (plane : V4d) (p0 : ref<V4d>) (p1 : ref<V4d>) =
        let h0 = Vec.dot plane !p0
        let h1 = Vec.dot plane !p1

        // h = h0 + (h1 - h0)*t
        // 0 = h0 + (h1 - h0)*t
        // (h0 - h1)*t = h0
        // t = h0 / (h0 - h1)
        if h0 > 0.0 && h1 > 0.0 then
            false
        elif h0 < 0.0 && h1 > 0.0 then
            let t = h0 / (h0 - h1)
            p1 := !p0 + t * (!p1 - !p0)
            true
        elif h1 < 0.0 && h0 > 0.0 then
            let t = h0 / (h0 - h1)
            p0 := !p0 + t * (!p1 - !p0)
            true
        else
            true

    [<ReflectedDefinition>]
    let clipLinePure (plane : V4d) (p0 : V4d) (p1 : V4d) =
        let h0 = Vec.dot plane p0
        let h1 = Vec.dot plane p1

        // h = h0 + (h1 - h0)*t
        // 0 = h0 + (h1 - h0)*t
        // (h0 - h1)*t = h0
        // t = h0 / (h0 - h1)
        if h0 > 0.0 && h1 > 0.0 then
            (false, p0, p1)
        elif h0 < 0.0 && h1 > 0.0 then
            let t = h0 / (h0 - h1)
            let p11 = p0 + t * (p1 - p0)
            (true, p0, p11)
        elif h1 < 0.0 && h0 > 0.0 then
            let t = h0 / (h0 - h1)
            let p01 = p0 + t * (p1 - p0)
            
            (true, p01, p1)
        else
            (true, p0, p1)

    let internal thickLine (line : Line<ThickLineVertex>) =
        triangle {
            let t : float = uniform?LineWidth
            let vs : V2i = uniform?ViewportSize
            let sizeF = V3d(float vs.X, float vs.Y, 1.0)

            let mutable pp0 = line.P0.pos
            let mutable pp1 = line.P1.pos

            let w = 1.0
            
            //let (a0, pp0, pp1) = clipLinePure (V4d( 1.0,  0.0,  0.0, -w)) pp0 pp1
            //let (a1, pp0, pp1) = clipLinePure (V4d(-1.0,  0.0,  0.0, -w)) pp0 pp1
            //let (a2, pp0, pp1) = clipLinePure (V4d( 0.0,  1.0,  0.0, -w)) pp0 pp1
            //let (a3, pp0, pp1) = clipLinePure (V4d( 0.0, -1.0,  0.0, -w)) pp0 pp1
            //let (a4, pp0, pp1) = clipLinePure (V4d( 0.0,  0.0,  1.0, -1.0)) pp0 pp1
            //let (a5, pp0, pp1) = clipLinePure (V4d( 0.0,  0.0, -1.0, -1.0)) pp0 pp1
            
            let add = 2.0 * V2d(t,t) / sizeF.XY

            // x = w

            // p' = p / p.w
            // p' € [-1,1]
            // p' € [-1-add.X,1+add.X]


            // p.x - (1+add.X)*p.w = 0



            let a0 = clipLine (V4d( 1.0,  0.0,  0.0, -(1.0 + add.X))) &&pp0 &&pp1
            let a1 = clipLine (V4d(-1.0,  0.0,  0.0, -(1.0 + add.X))) &&pp0 &&pp1
            let a2 = clipLine (V4d( 0.0,  1.0,  0.0, -(1.0 + add.Y))) &&pp0 &&pp1
            let a3 = clipLine (V4d( 0.0, -1.0,  0.0, -(1.0 + add.Y))) &&pp0 &&pp1
            let a4 = clipLine (V4d( 0.0,  0.0,  1.0, -1.0)) &&pp0 &&pp1
            let a5 = clipLine (V4d( 0.0,  0.0, -1.0, -1.0)) &&pp0 &&pp1

            if a0 && a1 && a2 && a3 && a4 && a5 then
                let p0 = pp0.XYZ / pp0.W
                let p1 = pp1.XYZ / pp1.W

                let fwp = (p1.XYZ - p0.XYZ) * sizeF

                let fw = V3d(fwp.XY, 0.0) |> Vec.normalize
                let r = V3d(-fw.Y, fw.X, 0.0) / sizeF
                let d = fw / sizeF
                let p00 = p0 - r * t - d * t
                let p10 = p0 + r * t - d * t
                let p11 = p1 + r * t + d * t
                let p01 = p1 - r * t + d * t

                let rel = t / (Vec.length fwp)

                yield { line.P0 with pos = V4d(p00, 1.0); lc = V2d(-1.0, -rel); w = rel }
                yield { line.P0 with pos = V4d(p10, 1.0); lc = V2d( 1.0, -rel); w = rel }
                yield { line.P1 with pos = V4d(p01, 1.0); lc = V2d(-1.0, 1.0 + rel); w = rel }
                yield { line.P1 with pos = V4d(p11, 1.0); lc = V2d( 1.0, 1.0 + rel); w = rel }
        }


[<EntryPoint>]
let main args = 
    Aardvark.Base.Aardvark.Init()

    let e = 
        Effect.compose [
            Effect.ofFunction Trafo.trafo
            Effect.ofFunction ThickLine.thickLine
        ]

    let outputs = Map.ofList ["Colors", (typeof<V4d>, 0); "Normals", (typeof<V3d>, 1)]

    let glsl = 
        e
        |> Effect.tryReplaceGeometry
        |> Option.get
        |> Effect.toModule { depthRange = Range1d(-1.0, 1.0); flipHandedness = false; lastStage = ShaderStage.Fragment; outputs = outputs }
        |> ModuleCompiler.compileGLSL430

    printfn "%s" glsl.code

  
    0