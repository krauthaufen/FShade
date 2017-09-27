module InputLayouts

open Aardvark.Base
open FShade
open FShade.GLSL

module Shaders = 
    type Vertex =
        {
            [<Position>] p : V4d
            [<Semantic("TexCoord")>] tc : V2d
        }

    let thing =
        sampler2d {
            texture uniform?ColorTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    type UniformScope with
        member x.A : float = uniform?BufferA?ValueA
        member x.B : float = uniform?BufferA?ValueB
        member x.C : float = uniform?BufferA?ValueC

    let vert (v : Vertex) =
        vertex {
            return { v with p = 3.0 * v.p }
        }
    
    let frag1 (v : Vertex) =
        fragment {
            return V4d.IIII * uniform.A * uniform.B
        }

    let frag2 (v : Vertex) =
        fragment {
            return thing.Sample(v.tc) * uniform.C
        }


let config =
    EffectConfig.ofList [
        "Colors", typeof<V4d>, 0
    ]

let run() =
    // sometimes rendering-systems want to create effects with identical
    // in-/output-signatures (ABIs). 
    // FShade has a simple mechanism for deriving/applying EffectInputLayouts to Modules.
    // Since EffectInputLayouts can also be unified (merging all inputs) one can easily compile
    // two shaders with a shared ABI without a-priori knowledge.

    // start by creating two effects
    let effect1 = 
        Effect.compose [
            Effect.ofFunction Shaders.vert
            Effect.ofFunction Shaders.frag1
        ]

    let effect2 = 
        Effect.compose [
            Effect.ofFunction Shaders.vert
            Effect.ofFunction Shaders.frag2
        ]

    // create modules for both effects
    let module1 = effect1 |> Effect.toModule config
    let module2 = effect2 |> Effect.toModule config
            
    // figure out an input-layout which both modules can agree on
    // NOTE: this might obviously fail if the two shaders define inputs/uniforms
    //       with identical names but different types
    let layout = EffectInputLayout.ofModules [module1; module2]

    // now apply that shared layout to both modules and compile them
    let glsl1 = module1 |> EffectInputLayout.apply layout |> ModuleCompiler.compileGLSL410
    let glsl2 = module2 |> EffectInputLayout.apply layout |> ModuleCompiler.compileGLSL410

    // finally print the code 
    printfn "// ============================================================================"
    printfn "// SHADER 1"
    printfn "// ============================================================================"
    printfn "%s" glsl1.code

    printfn "// ============================================================================"
    printfn "// SHADER 2"
    printfn "// ============================================================================"
    printfn "%s" glsl2.code

    ()
