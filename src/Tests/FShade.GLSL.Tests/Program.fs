module Program

open System
open System.Reflection
open System.IO
open Aardvark.Base
open FShade
open FShade.Imperative
open FShade.Tests.GLSL
open Microsoft.FSharp.Quotations
open FShade.GLSL

module Trafo = 
    
    type Vertex = {
        [<Position>]        pos     : V4f
        [<Color>]           c       : V4f
        [<Semantic("Normals")>]           n       : V3f
    }


    type UniformScope with
        member x.ModelTrafo : M44f = x?PerModel?ModelTrafo
        member x.ViewTrafo : M44f = x?PerView?ViewTrafo
        member x.ProjTrafo : M44f = x?PerView?ProjTrafo
        member x.ViewProjTrafo : M44f = x?PerView?ViewProjTrafo
        member x.ModelViewTrafo : M44f = x?PerModel?ModelViewTrafo
        member x.ModelViewProjTrafo : M44f = x?PerModel?ModelViewProjTrafo
        member x.NormalMatrix : M33f = x?PerModel?NormalMatrix

        member x.ModelTrafoInv : M44f = x?PerModel?ModelTrafoInv
        member x.ViewTrafoInv : M44f = x?PerView?ViewTrafoInv
        member x.ProjTrafoInv : M44f = x?PerView?ProjTrafoInv
        member x.ViewProjTrafoInv : M44f = x?PerView?ViewProjTrafoInv
        member x.ModelViewTrafoInv : M44f = x?PerModel?ModelViewTrafoInv
        member x.ModelViewProjTrafoInv : M44f = x?PerModel?ModelViewProjTrafoInv

        member x.CameraLocation : V3f = x?PerView?CameraLocation
        member x.LightLocation : V3f = x?PerLight?LightLocation



        member x.LineWidth : float32 = x?LineWidth
        member x.LineColor : V4f = x?LineColor

        member x.PointSize : float32 = x?PointSize
        member x.PointColor : V4f = x?PointColor

        member x.ViewportSize : V2i = x?PerView?ViewportSize


        member x.DiffuseColor : V4f = x?PerMaterial?DiffuseColor
        member x.AmbientColor : V4f = x?PerMaterial?DiffuseColor
        member x.EmissiveColor : V4f = x?PerMaterial?EmissiveColor
        member x.SpecularColor : V4f = x?PerMaterial?SpecularColor
        member x.Shininess : float32 = x?PerMaterial?Shininess

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
                n = uniform.ModelTrafoInv.Transposed * V4f(v.n, 0.0f) |> Vec.xyz
            }
        }

module ThickLine = 

    type ThickLineVertex = {
        [<Position>]                pos     : V4f
        [<Color>]                   c       : V4f
        [<Semantic("LineCoord")>]   lc      : V2f
        [<Semantic("Width")>]       w       : float32
        [<Semantic("Normals")>]           n       : V3f
    }

    [<ReflectedDefinition>]
    let clipLine (plane : V4f) (p0 : ref<V4f>) (p1 : ref<V4f>) =
        let h0 = Vec.dot plane !p0
        let h1 = Vec.dot plane !p1

        // h = h0 + (h1 - h0)*t
        // 0 = h0 + (h1 - h0)*t
        // (h0 - h1)*t = h0
        // t = h0 / (h0 - h1)
        if h0 > 0.0f && h1 > 0.0f then
            false
        elif h0 < 0.0f && h1 > 0.0f then
            let t = h0 / (h0 - h1)
            p1 := !p0 + t * (!p1 - !p0)
            true
        elif h1 < 0.0f && h0 > 0.0f then
            let t = h0 / (h0 - h1)
            p0 := !p0 + t * (!p1 - !p0)
            true
        else
            true

    [<ReflectedDefinition>]
    let clipLinePure (plane : V4f) (p0 : V4f) (p1 : V4f) =
        let h0 = Vec.dot plane p0
        let h1 = Vec.dot plane p1

        // h = h0 + (h1 - h0)*t
        // 0 = h0 + (h1 - h0)*t
        // (h0 - h1)*t = h0
        // t = h0 / (h0 - h1)
        if h0 > 0.0f && h1 > 0.0f then
            (false, p0, p1)
        elif h0 < 0.0f && h1 > 0.0f then
            let t = h0 / (h0 - h1)
            let p11 = p0 + t * (p1 - p0)
            (true, p0, p11)
        elif h1 < 0.0f && h0 > 0.0f then
            let t = h0 / (h0 - h1)
            let p01 = p0 + t * (p1 - p0)
            
            (true, p01, p1)
        else
            (true, p0, p1)

    let internal thickLine (line : Line<ThickLineVertex>) =
        triangle {
            let t : float32 = uniform?LineWidth
            let vs : V2i = uniform?ViewportSize
            let sizeF = V3f(float32 vs.X, float32 vs.Y, 1.0f)

            let mutable pp0 = line.P0.pos
            let mutable pp1 = line.P1.pos

            let w = 1.0f
            
            //let (a0, pp0, pp1) = clipLinePure (V4f( 1.0f,  0.0f,  0.0f, -w)) pp0 pp1
            //let (a1, pp0, pp1) = clipLinePure (V4f(-1.0f,  0.0f,  0.0f, -w)) pp0 pp1
            //let (a2, pp0, pp1) = clipLinePure (V4f( 0.0f,  1.0f,  0.0f, -w)) pp0 pp1
            //let (a3, pp0, pp1) = clipLinePure (V4f( 0.0f, -1.0f,  0.0f, -w)) pp0 pp1
            //let (a4, pp0, pp1) = clipLinePure (V4f( 0.0f,  0.0f,  1.0f, -1.0f)) pp0 pp1
            //let (a5, pp0, pp1) = clipLinePure (V4f( 0.0f,  0.0f, -1.0f, -1.0f)) pp0 pp1
            
            let add = 2.0f * V2f(t,t) / sizeF.XY

            // x = w

            // p' = p / p.w
            // p' € [-1,1]
            // p' € [-1-add.X,1+add.X]


            // p.x - (1+add.X)*p.w = 0



            let a0 = clipLine (V4f( 1.0f,  0.0f,  0.0f, -(1.0f + add.X))) &&pp0 &&pp1
            let a1 = clipLine (V4f(-1.0f,  0.0f,  0.0f, -(1.0f + add.X))) &&pp0 &&pp1
            let a2 = clipLine (V4f( 0.0f,  1.0f,  0.0f, -(1.0f + add.Y))) &&pp0 &&pp1
            let a3 = clipLine (V4f( 0.0f, -1.0f,  0.0f, -(1.0f + add.Y))) &&pp0 &&pp1
            let a4 = clipLine (V4f( 0.0f,  0.0f,  1.0f, -1.0f)) &&pp0 &&pp1
            let a5 = clipLine (V4f( 0.0f,  0.0f, -1.0f, -1.0f)) &&pp0 &&pp1

            if a0 && a1 && a2 && a3 && a4 && a5 then
                let p0 = pp0.XYZ / pp0.W
                let p1 = pp1.XYZ / pp1.W

                let fwp = (p1.XYZ - p0.XYZ) * sizeF

                let fw = V3f(fwp.XY, 0.0f) |> Vec.normalize
                let r = V3f(-fw.Y, fw.X, 0.0f) / sizeF
                let d = fw / sizeF
                let p00 = p0 - r * t - d * t
                let p10 = p0 + r * t - d * t
                let p11 = p1 + r * t + d * t
                let p01 = p1 - r * t + d * t

                let rel = t / (Vec.length fwp)

                yield { line.P0 with pos = V4f(p00, 1.0f); lc = V2f(-1.0f, -rel); w = rel }
                yield { line.P0 with pos = V4f(p10, 1.0f); lc = V2f( 1.0f, -rel); w = rel }
                yield { line.P1 with pos = V4f(p01, 1.0f); lc = V2f(-1.0f, 1.0f + rel); w = rel }
                yield { line.P1 with pos = V4f(p11, 1.0f); lc = V2f( 1.0f, 1.0f + rel); w = rel }
        }


    let sammy =
        [|
            sampler2d {
                texture uniform?DiffuseColorTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Clamp
                addressV WrapMode.Clamp
                addressW WrapMode.Clamp
            }
            sampler2d {
                texture uniform?Tex1
                filter Filter.MinMagMipLinear
                addressU WrapMode.Clamp
                addressV WrapMode.Clamp
                addressW WrapMode.Clamp
            }
        |]
        
    [<ReflectedDefinition>]
    let mySammy (s : Sampler2d) (tc : V2f) =
        s.Sample(tc)
        
    let frag (v : ThickLineVertex) =
        fragment {
            return mySammy sammy.[0] v.lc
        }


[<EntryPoint>]
let main args = 
    Aardvark.Base.Aardvark.Init()

    let e = 
        Effect.compose [
            Effect.ofFunction ThickLine.frag
        ]

    let outputs = Map.ofList ["Colors", (typeof<V4f>, 0)]
    
    let glslSeparateSamplersAndTextures =
        Backend.Create {
            version                     = GLSLVersion(4,5,0)
            enabledExtensions           = Set.ofList [ "GL_ARB_tessellation_shader"; "GL_ARB_separate_shader_objects"; "GL_ARB_shading_language_420pack" ]
            createUniformBuffers        = true
            bindingMode                 = BindingMode.Global
            createDescriptorSets        = true
            stepDescriptorSets          = false
            createInputLocations        = true
            createOutputLocations       = true
            createPassingLocations      = true
            createPerStageUniforms      = true
            reverseMatrixLogic          = true
            reverseTessellationWinding  = true
            depthWriteMode              = true
            useInOut                    = true
            separateTexturesAndSamplers = true
            pushConstants = false
            availableExtensions = Map.empty
        }
    let glsl = 
        e
        |> Effect.tryReplaceGeometry
        |> Option.get
        |> Effect.toModule { depthRange = Range1f(-1.0f, 1.0f); flipHandedness = false; lastStage = ShaderStage.Fragment; outputs = outputs }
        |> ModuleCompiler.compileGLSL glslSeparateSamplersAndTextures

    printfn "%s" glsl.code

    shouldCompile' glslSeparateSamplersAndTextures [e]
  
    0