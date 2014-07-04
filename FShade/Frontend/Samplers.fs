namespace FShade

open System
open Aardvark.Base

[<AutoOpen>]
module Samplers =
    type WrapMode = Wrap = 0
                    | Mirror = 1
                    | Clamp = 2
                    | Border = 3
                    | MirrorOnce = 4
                    
    type Filter = Anisotropic = 0
                | MinLinearMagMipPoint = 1
                | MinLinearMagPointMipLinear = 2
                | MinMagLinearMipPoint = 3
                | MinMagMipLinear = 4
                | MinMagMipPoint = 5
                | MinMagPointMipLinear = 6
                | MinPointMagLinearMipPoint = 7
                | MinPointMagMipLinear = 8
                | MinMagPoint = 9
                | MinMagLinear = 10
                | MinPointMagLinear = 11
                | MinLinearMagPoint = 12
                 
    type ComparisonFunction = Never = 1
                            | Less = 2
                            | Equal = 3
                            | LessOrEqual = 4
                            | Greater = 5
                            | GreaterOrEqual = 6
                            | NotEqual = 7
                            | Always = 8
                
    [<NoComparison>]      
    type SamplerState = {
            AddressU : Option<WrapMode>
            AddressV : Option<WrapMode>
            AddressW : Option<WrapMode>
            Filter : Option<Filter>
            BorderColor : Option<C4f>
            MaxAnisotropy : Option<int>
            MaxLod : Option<float>
            MinLod : Option<float>
            MipLodBias : Option<float>
        } with
        static member Empty = { AddressU = None; AddressV = None; AddressW = None; Filter = None; BorderColor = None; MaxAnisotropy = None; MaxLod = None; MinLod = None; MipLodBias = None }
            
    [<NoComparison>]
    type SamplerComparisonState = {
            AddressU : Option<WrapMode>
            AddressV : Option<WrapMode>
            AddressW : Option<WrapMode>
            Filter : Option<Filter>
            Comparison : Option<ComparisonFunction>
            BorderColor : Option<C4f>
            MaxAnisotropy : Option<int>
            MaxLod : Option<float>
            MinLod : Option<float>
            MipLodBias : Option<float>

        } with
        static member Empty = { AddressU = None; AddressV = None; AddressW = None; Filter = None; Comparison = None; BorderColor = None; MaxAnisotropy = None; MaxLod = None; MinLod = None; MipLodBias = None }
            

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


    let private fail() = failwith "can only be used in shaders"


    type ShaderTexture2D() =
        member x.Sample(state : SamplerState, coord : V2d) : V4d = fail()
        member x.SampleLevel(state : SamplerState, coord : V2d, level : float) : V4d = fail()
        member x.SampleCmp(state : SamplerComparisonState, coord : V2d, compareValue : float) : V4d = fail()
        member x.SampleCmpLevelZero(state : SamplerComparisonState, coord : V2d, compareValue : float) : V4d = fail()
        member x.SampleGrad(state : SamplerComparisonState, coord : V2d, ddx : V2d, ddy : V2d) : V4d = fail()
     
     
    type ShaderTexture2DArray() =
        member x.Sample(state : SamplerState, coord : V2d, index : int) : V4d = fail()
        member x.SampleCmp(state : SamplerComparisonState, coord : V2d, index : int, compareValue : float) : V4d = fail()
        member x.SampleCmpLevelZero(state : SamplerComparisonState, coord : V2d, index : int, compareValue : float) : V4d = fail()
                           

    let (|TextureType|_|) (t : Type) =
        if t = typeof<ShaderTexture2D> then TextureType(2, false) |> Some
        elif t = typeof<ShaderTexture2DArray> then TextureType(2, true) |> Some
        else None