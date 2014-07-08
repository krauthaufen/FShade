namespace FShade

open System
open Aardvark.Base

[<AutoOpen>]
module Samplers =

    type SamplerStateBuilder() =
        member x.Yield(_) = SamplerState.Empty

        [<CustomOperation("addressU")>]
        member x.AddressU(h : SamplerState, w : WrapMode) = { h with AddressU = Some w }
             
        [<CustomOperation("addressV")>]
        member x.AddressV(h : SamplerState, w : WrapMode) = { h with AddressV = Some w }
             
        [<CustomOperation("addressW")>]
        member x.AddressW(h : SamplerState, w : WrapMode) = { h with AddressW = Some w }
             
        [<CustomOperation("maxAnisotropy")>]
        member x.MaxAnisotropy(h : SamplerState, a : int) = { h with MaxAnisotropy = Some a }
             
        [<CustomOperation("borderColor")>]
        member x.BorderColor(h : SamplerState, c : C4f) = { h with BorderColor = Some c }
             
        [<CustomOperation("maxLod")>]
        member x.MaxLod(h : SamplerState, c : float) = { h with MaxLod = Some c }
             
        [<CustomOperation("minLod")>]
        member x.MinLod(h : SamplerState, c : float) = { h with MinLod = Some c }
             
        [<CustomOperation("mipLodBias")>]
        member x.MipLodBias(h : SamplerState, c : float) = { h with MipLodBias = Some c }
             
        [<CustomOperation("filter")>]
        member x.Filter(h : SamplerState, f : Filter) = { h with Filter = Some f }
           
    type SamplerComparisonStateBuilder() =
        member x.Yield(_) = SamplerComparisonState.Empty

        [<CustomOperation("addressU")>]
        member x.AddressU(h : SamplerComparisonState, w : WrapMode) = { h with AddressU = Some w }
             
        [<CustomOperation("addressV")>]
        member x.AddressV(h : SamplerComparisonState, w : WrapMode) = { h with AddressV = Some w }
             
        [<CustomOperation("addressW")>]
        member x.AddressW(h : SamplerComparisonState, w : WrapMode) = { h with AddressW = Some w }
             
        [<CustomOperation("maxAnisotropy")>]
        member x.MaxAnisotropy(h : SamplerComparisonState, a : int) = { h with MaxAnisotropy = Some a }
             
        [<CustomOperation("borderColor")>]
        member x.BorderColor(h : SamplerComparisonState, c : C4f) = { h with BorderColor = Some c }
             
        [<CustomOperation("maxLod")>]
        member x.MaxLod(h : SamplerComparisonState, c : float) = { h with MaxLod = Some c }
             
        [<CustomOperation("minLod")>]
        member x.MinLod(h : SamplerComparisonState, c : float) = { h with MinLod = Some c }
             
        [<CustomOperation("mipLodBias")>]
        member x.MipLodBias(h : SamplerComparisonState, c : float) = { h with MipLodBias = Some c }
             
        [<CustomOperation("filter")>]
        member x.Filter(h : SamplerComparisonState, f : Filter) = { h with Filter = Some f }
          
        [<CustomOperation("comparison")>]
        member x.Comparison(h : SamplerComparisonState, f : ComparisonFunction) = { h with Comparison = Some f }
          
             
    let samplerState = SamplerStateBuilder()

    let samplerComparisonState = SamplerComparisonStateBuilder()


    type TextureMustBeSpecified = TextureMustBeSpecified

      



    let private fail() = failwith "can only be used in shaders"



    type ShaderTextureHandle(semantic : string, scope : UniformScope) =
        static member CreateUniform(semantic : string, scope : UniformScope) = ShaderTextureHandle(semantic, scope)
        interface ISemanticValue with
            member x.Semantic = semantic
            member x.ScopeUntyped = scope :> obj


        new() = ShaderTextureHandle(null, Unchecked.defaultof<UniformScope>)       

//        member x.Sample(state : SamplerState, coord : V2d, index : int) : V4d = fail()
//        member x.SampleCmp(state : SamplerComparisonState, coord : V2d, index : int, compareValue : float) : V4d = fail()
//        member x.SampleCmpLevelZero(state : SamplerComparisonState, coord : V2d, index : int, compareValue : float) : V4d = fail()
//          
//          
//    type Sampler2d(texture : ShaderTexture2D, state : SamplerState) =
//        member x.Texture = texture
//        member x.State = state
//        member x.Sample(coord : V2d) : V4d = fail()
//        member x.SampleLevel(coord : V2d, level : float) : V4d = fail()
//        new() = Sampler2d(Unchecked.defaultof<ShaderTexture2D>, SamplerState.Empty)
//
//    type Sampler2dArray = { texture : ShaderTexture2DArray; state : SamplerState }                 
//
//    let (|SamplerType|_|) (t : Type) =
//        if t = typeof<Sampler2d> then SamplerType(2, false) |> Some
//        elif t = typeof<Sampler2dArray> then SamplerType(2, true) |> Some
//        else None

//    let (|TextureType|_|) (t : Type) =
//        if t = typeof<ShaderTexture2D> then TextureType(2, false) |> Some
//        elif t = typeof<ShaderTexture2DArray> then TextureType(2, true) |> Some
//        else None

    type Sampler2dBuilder() =
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
       
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler2d(t, s)

    let sampler2d = Sampler2dBuilder()


    //(i|u)?sampler(1d|2d|3d|1dArray|2dArray|Cube)(Shadow|MS)?