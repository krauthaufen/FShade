namespace FShade.Demo

open Aardvark.Base
open FShade



/// <summary>
/// This module includes basic shaders and explains the basic features
/// of FShade by example. 
/// </summary>
module Shaders =
    
    /// <summary>
    /// by adding special attributes for default semantics avoids typing errors
    /// and makes shaders more readable. The library can be freely extended with
    /// further attributes by any project.
    /// </summary>
    type PositionAttribute() = inherit SemanticAttribute("Positions")
    type WorldPositionAttribute() = inherit SemanticAttribute("WorldPosition")
    type NormalAttribute() = inherit SemanticAttribute("Normals")
    type TexCoord() = inherit SemanticAttribute("TexCoords")

    /// <summary>
    /// Some typed extensions for common uniforms. Note that the 'anonymous' 
    /// variants could also be used in the shader directly. In order to
    /// fix their types extensions like these are sometimes helpful.
    /// </summary>
    type UniformScope with
        member x.ModelTrafo : M44d = uniform?PerModel?ModelTrafo
        member x.ViewTrafo : M44d = uniform?PerView?ViewTrafo
        member x.ProjTrafo : M44d = uniform?PerView?ProjTrafo
        member x.ViewProjTrafo : M44d = uniform?PerView?ViewProjTrafo

        member x.DiffuseTexture : ShaderTextureHandle = uniform?DiffuseTexture

    /// <summary>
    /// defining a vertex requires semantic annotations for all fields.
    /// </summary>
    type Vertex = 
        { [<Position>]          pos     : V4d
          [<WorldPosition>]     world   : V4d
          [<Normal>]            n       : V3d
          [<TexCoord>]          tc      : V2d }

    /// <summary>
    /// a very simple transformation-shader using the uniforms defined above.
    /// Note that this shader may be very inefficient but demonstrates the
    /// basic concepts of shaders in FShade.
    /// </summary>
    let simpleTrafoShader (v : Vertex) =
        vertex {
            let worldPos = uniform.ModelTrafo * v.pos

            let transformedPos = uniform.ViewProjTrafo * worldPos

            let normalMatrix = uniform.ModelTrafo |> Mat.transpose |> Mat.inverse
            let n = normalMatrix * V4d(v.n, 0.0) |> Vec.xyz

            return { v with pos = transformedPos
                            world = worldPos
                            n = n}
        }

    /// <summary>
    /// A very simple fragment-shader always returing a constant color. As seen in the 
    /// example, shader-functions may also be curried functions taking more than one argument.
    /// Since shaders are defined to have exactly one argument 'simpleFragmentShader' 
    /// itself is not a shader but (for example) 'simpleFragmentShader V4d.IIII' is.
    /// </summary>
    let simpleFragmentShader (color : V4d) (v : Vertex) =
        fragment {
            return V4d(v.tc.X, v.tc.Y, 1.0, 1.0)
        }

    let diffuseLinear = 
        sampler2d {
            texture     uniform.DiffuseTexture

            filter      Filter.MinMagMipLinear
            addressU    WrapMode.Wrap
            addressV    WrapMode.Wrap
        }

    let diffuseNearest = 
        sampler2d {
            texture     uniform.DiffuseTexture

            filter      Filter.MinMagMipPoint
            addressU    WrapMode.Border
            addressV    WrapMode.Border
            borderColor C4f.Red
        }



    let textureShader (v : Vertex) =
        fragment {

            if v.tc.X > 0.5 then
                let vv = uniform.ModelTrafo * V4d.Zero
                return vv + diffuseLinear.Sample(v.tc)
            else 
                let tc = v.tc * V2d diffuseNearest.Size |> V2i
                let s = diffuseNearest.[tc]

//                let dx0 = (s.Y - s.X) * 0.5 + 0.5
//                let dy0 = (s.X - s.W) * 0.5 + 0.5

                return s //V4d(dx0, dy0, 1.0, 1.0)
        }
    

