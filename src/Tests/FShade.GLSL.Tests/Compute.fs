module Compute

open Aardvark.Base
open FSharp.Quotations
open FShade
open FShade.Tests
open NUnit.Framework
open FsUnit

type UniformScope with
    member _.Input : V4f[] = uniform?StorageBuffer?Input
    member _.Output : V4f[] = uniform?StorageBuffer?Output

let sampler1 =
    sampler2d {
        texture uniform?texture
        filter Filter.MinMagMipLinear
        addressU WrapMode.Wrap
        addressV WrapMode.Wrap
    }

[<LocalSize(X = 8, Y = 4, Z = 2)>]
let shaderNop () =
    compute {
        ()
    }

[<Test>]
let ``Local size``() =
    let localSize = V3i(8, 4, 2)
    let shader = ComputeShader.ofFunction V3i.MaxValue shaderNop

    shader.csLocalSize |> should equal localSize

    let glsl =
        shader
        |> ComputeShader.toModule
        |> ModuleCompiler.compileGLSLVulkan

    glsl.iface.shaders.[ShaderSlot.Compute].shaderDecorations
    |> List.exists (function
        GLSL.GLSLLocalSize s -> s = localSize
        | _ -> false
    )
    |> should be True

[<Test>]
let ``Includes samplerInfo``() =
    let shader (v : V4f[]) =
        compute {
            let id = getGlobalId().XY
            let a = sampler1.Sample(V2f id)
            v.[id.X] <- a
        }

    let glsl =
        shader
        |> ComputeShader.ofFunction (V3i 128)
        |> ComputeShader.toModule
        |> ModuleCompiler.compileGLSLVulkan

    let state =
        samplerState {
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    glsl.iface.samplers.["sampler1"].samplerTextures |> should equal ["texture", state ]

[<Test>]
let ``Spliced expressions``() =
    Setup.Run()

    let shader (op: Expr<V4f -> V4f -> V4f>) () =
        compute {
            let id = getGlobalId()

            uniform.Output.[id.X] <-
                if id.X > 0 then
                    V4f.Zero
                else
                    (%op) uniform.Input.[id.X] V4f.One
        }

    GLSL.shouldCompileCompute (ComputeShader.ofFunction (V3i(128)) (shader <@ min @>))
