namespace FShade

open System
open Aardvark.Base

[<AutoOpen>]
module SamplerStateModule =
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


type ISemanticValue =
    //static member CreateUniform(semantic : string, scope : UniformScope)
    abstract member Semantic : string
    abstract member ScopeUntyped : obj

type SamplerType = Float = 0
                 | Int = 1

type SamplerDimension = Sampler1d = 0
                      | Sampler2d = 1
                      | Sampler3d = 2
                      | SamplerCube = 3


type ISampler =
    abstract member Texture : ISemanticValue
    abstract member State : SamplerState

[<AutoOpen>]
module ReflectionHelpers =

    let private getProp<'a> (name : string) (t : Type) =
        t.GetProperty(name).GetValue(null) |> unbox<'a>

    /// <summary>
    /// determines whether a given type is a Sampler and returns its properties if successful.
    /// The properties are given by SamplerType(dim, isArray, isShadow, isMS, valueType)
    /// </summary>
    let (|SamplerType|_|) (t : Type) =
        if typeof<ISampler>.IsAssignableFrom(t) then
            let dim : SamplerDimension = getProp "Dimension" t
            let valueType : Type = getProp "ValueType" t
            let coordType : Type = getProp "CoordType" t
            let isArray : bool = getProp "IsArray" t
            let isShadow : bool = getProp "IsShadow" t
            let isMS : bool = getProp "IsMultisampled" t

            SamplerType(dim, isArray, isShadow, isMS, valueType) |> Some


        else
            None


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





    //(i|u)?sampler(1d|2d|3d|1dArray|2dArray|Cube)(Shadow|MS)?