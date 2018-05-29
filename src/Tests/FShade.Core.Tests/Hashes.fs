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

    module Shader1 =

        let sam =
            sampler2d {
                texture uniform?texture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let shader (v : Vertex) =
            fragment {
                return sam.Sample(v.tc)
            }

    module Shader2 =
    
        let sam =
            sampler2d {
                texture uniform?texture
                filter Filter.MinMagPoint
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let shader (v : Vertex) =
            fragment {
                return sam.Sample(v.tc)
            }

    module Shader3 =

        let sam =
            sampler2d {
                texture uniform?texture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let shader (v : Vertex) =
            fragment {
                return sam.Sample(v.tc)
            }
            
    module Shader4 =

        [<GLSLIntrinsic("someFun({0})")>]
        let sepp (a : V2d) : V4d =
            onlyInShaderCode "sepp"

        let shader (v : Vertex) =
            fragment {
                return sepp v.tc
            }
            
    module Shader5 =

        [<GLSLIntrinsic("someFun({0})")>]
        let heinz (a : V2d) : V4d =
            onlyInShaderCode "sepp"

        let shader (v : Vertex) =
            fragment {
                return heinz v.tc
            }

    [<Fact(Skip = "static initializers broken in xunit")>]
    let ``[Hashing] static init running``() =
        Shader1.sam |> should not' (be Null)

    [<Fact(Skip = "static initializers broken in xunit")>]
    let ``[Hashing] includes SamplerState``() =

        let e1 = Effect.ofFunction Shader1.shader
        let e2 = Effect.ofFunction Shader2.shader

        e1.Id |> should not' (equal e2.Id)

    [<Fact>]
    let ``[Hashing] SamplerState hash is structural``() =

        let e1 = Effect.ofFunction Shader1.shader
        let e2 = Effect.ofFunction Shader3.shader

        e1.Id |> should equal e2.Id


    [<Fact>]
    let ``[Hashing] intrinsic hashes on target-function``() =

        let e1 = Effect.ofFunction Shader4.shader
        let e2 = Effect.ofFunction Shader5.shader

        e1.Id |> should equal e2.Id
