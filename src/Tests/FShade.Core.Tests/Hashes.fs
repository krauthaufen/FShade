module Hashes

open FsUnit
open NUnit.Framework
open Aardvark.Base
open FShade

type Vertex = 
    {
        [<Position>] pos : V4d
        [<Semantic("TexCoord")>] tc : V2d
    }

type Shader1 private () =

    static member Sampler =
        sampler2d {
            texture uniform?texture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
            
    static member shader (v : Vertex) =
        fragment {
            return Shader1.Sampler.Sample(v.tc)
        }
            
type Shader2 private () =

    static member Sampler =
        sampler2d {
            texture uniform?texture
            filter Filter.MinMagPoint
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
            
    static member shader (v : Vertex) =
        fragment {
            return Shader2.Sampler.Sample(v.tc)
        }
            
type Shader3 private () =

    static member Sampler =
        sampler2d {
            texture uniform?texture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
            
    static member shader (v : Vertex) =
        fragment {
            return Shader3.Sampler.Sample(v.tc)
        }
                       
type Shader4 private() =

    [<GLSLIntrinsic("someFun({0})")>]
    static let sepp (a : V2d) : V4d =
        onlyInShaderCode "sepp"

    static member shader (v : Vertex) =
        fragment {
            return sepp v.tc
        }
            
type Shader5 private() =

    [<GLSLIntrinsic("someFun({0})")>]
    static let heinz (a : V2d) : V4d =
        onlyInShaderCode "sepp"

    static member shader (v : Vertex) =
        fragment {
            return heinz v.tc
        }

type Shader6 private () =

    static member shader (v : Vertex) =
        fragment {
            let a : V4d = uniform?Blubber?Value
            return a
        }
[<Test>]
let ``[Hashing] includes SamplerState``() =

    let e1 = Effect.ofFunction (Shader1.shader)
    let e2 = Effect.ofFunction (Shader2.shader)

    e1.Id |> should not' (equal e2.Id)
        
[<Test>]
let ``[Hashing] SamplerState hash works``() =

    let e1 = Effect.ofFunction (Shader1.shader)
    let e2 = Effect.ofFunction (Shader1.shader)

    e1.Id |> should equal e2.Id

[<Test>]
let ``[Hashing] SamplerState hash is structural``() =

    let e1 = Effect.ofFunction (Shader1.shader)
    let e2 = Effect.ofFunction (Shader3.shader)

    e1.Id |> should equal e2.Id


[<Test>]
let ``[Hashing] intrinsic hashes on target-function``() =

    let e1 = Effect.ofFunction Shader4.shader
    let e2 = Effect.ofFunction Shader5.shader

    e1.Id |> should equal e2.Id


[<Test>]
let ``[Hashing] deterministic uniforms``() =
    let subScopes = [ "Bla"; "Blurg"; "Blubber" ].RandomOrder() |> Seq.toList
    for s in subScopes do uniform.GetChildScope s |> ignore

    let e1 = Effect.ofFunction Shader6.shader

    e1.Id |> should equal "Rt5bdasgX4CG8+noOPDS++X7ipg="
    

[<Test>]
let ``[ComputeHashing] equal => equal hash``() =
    let s = V3i(128,128,128)
    let a (dst : int[]) (src : int[]) =
        compute {
            let id = getGlobalId().X
            dst.[id] <- src.[id] * 2
        }

    let b (dst : int[]) (src : int[]) =
        compute {
            let id = getGlobalId().X
            dst.[id] <- src.[id] * 2
        }
    let sa = ComputeShader.ofFunction s a
    let sb = ComputeShader.ofFunction s b

    sa.csId |> should equal sb.csId
    
[<Test>]
let ``[ComputeHashing] different => different hash``() =
    let s = V3i(128,128,128)
    let a (dst : int[]) (src : int[]) =
        compute {
            let id = getGlobalId().X
            let v : int = uniform?Bla
            dst.[id] <- src.[id] * v
        }

    let b (dst : int[]) (src : int[]) =
        compute {
            let id = getGlobalId().X
            let v : int = uniform?Bla
            dst.[id] <- src.[id] * 3
        }
    let sa = ComputeShader.ofFunction s a
    let sb = ComputeShader.ofFunction s b

    sa.csId |> should not' (equal sb.csId)

[<LocalSize(X = 16)>]
let a (dst : int[]) (src : int[]) =
    compute {
        let id = getGlobalId().X
        let v : int = uniform?Bla
        dst.[id] <- src.[id] * v
    }
    
[<LocalSize(X = 32)>]
let b (dst : int[]) (src : int[]) =
    compute {
        let id = getGlobalId().X
        let v : int = uniform?Bla
        dst.[id] <- src.[id] * v
    }
[<Test>]
let ``[ComputeHashing] including localSize``() =
    let s = V3i(128,128,128)
    let sa = ComputeShader.ofFunction s a
    let sb = ComputeShader.ofFunction s b

    sa.csId |> should not' (equal sb.csId)