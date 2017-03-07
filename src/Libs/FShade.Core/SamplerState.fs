namespace FShade

open System
open Aardvark.Base


type WrapMode = 
    | Wrap = 0
    | Mirror = 1
    | Clamp = 2
    | Border = 3
    | MirrorOnce = 4
                    
type Filter = 
    | Anisotropic = 0
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
                 
type ComparisonFunction = 
    | Never = 1
    | Less = 2
    | Equal = 3
    | LessOrEqual = 4
    | Greater = 5
    | GreaterOrEqual = 6
    | NotEqual = 7
    | Always = 8
                
[<NoComparison>]      
type SamplerState = 
    {
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
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SamplerState =
    let empty = 
        { 
            AddressU = None
            AddressV = None
            AddressW = None
            Filter = None
            Comparison = None
            BorderColor = None
            MaxAnisotropy = None
            MaxLod = None
            MinLod = None
            MipLodBias = None 
        }


type SamplerType = 
    | Float = 0
    | Int = 1

type SamplerDimension = 
    | Sampler1d = 0
    | Sampler2d = 1
    | Sampler3d = 2
    | SamplerCube = 3


[<AutoOpen>]
module SamplerExtensions =
    type SamplerStateBuilder() =
        member x.Yield(_) = SamplerState.empty

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
      
    let samplerState = SamplerStateBuilder()

