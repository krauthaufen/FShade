namespace global

open FsUnit
open Xunit
open Aardvark.Base
open FShade

module Hashes = 

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

    [<Fact>]
    let ``[Hashing] includes SamplerState``() =

        let e1 = Effect.ofFunction (Shader1.shader)
        let e2 = Effect.ofFunction (Shader2.shader)

        e1.Id |> should not' (equal e2.Id)
        
    [<Fact>]
    let ``[Hashing] SamplerState hash works``() =

        let e1 = Effect.ofFunction (Shader1.shader)
        let e2 = Effect.ofFunction (Shader1.shader)

        e1.Id |> should equal e2.Id

    [<Fact>]
    let ``[Hashing] SamplerState hash is structural``() =

        let e1 = Effect.ofFunction (Shader1.shader)
        let e2 = Effect.ofFunction (Shader3.shader)

        e1.Id |> should equal e2.Id


    [<Fact>]
    let ``[Hashing] intrinsic hashes on target-function``() =

        let e1 = Effect.ofFunction Shader4.shader
        let e2 = Effect.ofFunction Shader5.shader

        e1.Id |> should equal e2.Id
