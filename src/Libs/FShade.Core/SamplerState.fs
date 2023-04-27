namespace FShade

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

type FilterReduction =
    | WeightedAverage = 0
    | Minimum = 1
    | Maximum = 2

type ComparisonFunction =
    | Never = 1
    | Less = 2
    | Equal = 3
    | LessOrEqual = 4
    | Greater = 5
    | GreaterOrEqual = 6
    | NotEqual = 7
    | Always = 8

type ImageFormat =
    | Rgba32f = 34836
    | Rgba16f = 34842
    | Rg32f = 33328
    | Rg16f = 33327
    | R11fG11fB10f = 35898
    | R32f = 33326
    | R16f = 33325

    | Rgba16 = 32859
    | Rgb10A2 = 32857
    | Rgba8 = 32856
    | Rg16 = 33324
    | Rg8 = 33323
    | R16 = 33322
    | R8 = 33321

    | Rgba16Snorm = 36763
    | Rgba8Snorm = 36759
    | Rg16Snorm = 36761
    | Rg8Snorm = 36757
    | R16Snorm = 36760
    | R8Snorm = 36756

    | Rgba32ui = 36208
    | Rgba16ui = 36214
    | Rgb10A2ui = 36975
    | Rgba8ui = 36220
    | Rg32ui = 33340
    | Rg16ui = 33338
    | Rg8ui = 33336
    | R32ui = 33334
    | R16ui = 33332
    | R8ui = 33330

    | Rgba32i = 36226
    | Rgba16i = 36232
    | Rgba8i = 36238
    | Rg32i = 33339
    | Rg16i = 33337
    | Rg8i = 33335
    | R32i = 33333
    | R16i = 33331
    | R8i = 33329

[<NoComparison>]
type SamplerState =
    {
        AddressU : Option<WrapMode>
        AddressV : Option<WrapMode>
        AddressW : Option<WrapMode>
        Filter : Option<Filter>
        FilterReduction : Option<FilterReduction>
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
            FilterReduction = None
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
    | UInt = 2

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

        [<CustomOperation("filterReduction")>]
        member x.FilterReduction(h : SamplerState, r : FilterReduction) = { h with FilterReduction = Some r }

    let samplerState = SamplerStateBuilder()