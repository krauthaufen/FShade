namespace FShade

open System
open Aardvark.Base

[<AutoOpen>]
module Textures = 
    open SamplerStateModule

    type ShaderTextureHandle(semantic : string, scope : UniformScope) =
        static member CreateUniform(semantic : string, scope : UniformScope) = ShaderTextureHandle(semantic, scope)
        interface ISemanticValue with
            member x.Semantic = semantic
            member x.ScopeUntyped = scope :> obj


        new() = ShaderTextureHandle(null, Unchecked.defaultof<UniformScope>)       

    type SamplerBaseBuilder() =
        member x.Yield(_) = TextureMustBeSpecified

        [<CustomOperation("texture")>]
        member x.Texture(b : TextureMustBeSpecified, t : ShaderTextureHandle) =
            (t, SamplerState.Empty)

        [<CustomOperation("addressU")>]
        member x.AddressU((t : ShaderTextureHandle, h : SamplerState), w : WrapMode) = t,{ h with AddressU = Some w }
             
        [<CustomOperation("addressV")>]
        member x.AddressV((t : ShaderTextureHandle, h : SamplerState), w : WrapMode) = t,{ h with AddressV = Some w }
             
        [<CustomOperation("addressW")>]
        member x.AddressW((t : ShaderTextureHandle, h : SamplerState), w : WrapMode) = t,{ h with AddressW = Some w }
             
        [<CustomOperation("maxAnisotropy")>]
        member x.MaxAnisotropy((t : ShaderTextureHandle, h : SamplerState), a : int) = t,{ h with MaxAnisotropy = Some a }
             
        [<CustomOperation("borderColor")>]
        member x.BorderColor((t : ShaderTextureHandle, h : SamplerState), c : C4f) = t,{ h with BorderColor = Some c }
             
        [<CustomOperation("maxLod")>]
        member x.MaxLod((t : ShaderTextureHandle, h : SamplerState), c : float) = t,{ h with MaxLod = Some c }
             
        [<CustomOperation("minLod")>]
        member x.MinLod((t : ShaderTextureHandle, h : SamplerState), c : float) = t,{ h with MinLod = Some c }
             
        [<CustomOperation("mipLodBias")>]
        member x.MipLodBias((t : ShaderTextureHandle, h : SamplerState), c : float) = t,{ h with MipLodBias = Some c }
             
        [<CustomOperation("filter")>]
        member x.Filter((t : ShaderTextureHandle, h : SamplerState), f : Filter) = t,{ h with Filter = Some f }
       
    type Sampler2dBuilder() =
        inherit SamplerBaseBuilder()

        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler2d(t, s)

    type SamplerCubeBuilder() =
        inherit SamplerBaseBuilder()

        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            SamplerCube(t, s)

    let sampler2d = Sampler2dBuilder()
    let samplerCube = SamplerCubeBuilder()