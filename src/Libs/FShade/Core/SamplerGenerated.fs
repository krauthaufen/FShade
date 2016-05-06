namespace FShade
open Aardvark.Base


type Sampler1dMSArrayShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<float>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, cmp : float, level : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int, sample : int) : float = failwith ""
    

type Sampler1dMSArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, level : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int, sample : int) : V4d = failwith ""
    

type Sampler1dArrayShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<float>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, cmp : float, lodBias : float) : float = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, slice : int, cmp : float, offset : int) : float = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, slice : int, cmp : float, offset : int, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, cmp : float, level : float) : float = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, slice : int, cmp : float, dTdx : float, dTdy : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int) : float = failwith ""
    

type Sampler1dArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, lodBias : float) : V4d = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, slice : int, offset : int) : V4d = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, slice : int, offset : int, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, level : float) : V4d = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, slice : int, dTdx : float, dTdy : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int) : V4d = failwith ""
    

type Sampler1dMSShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<float>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, cmp : float, level : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int, sample : int) : float = failwith ""
    

type Sampler1dMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, level : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int, sample : int) : V4d = failwith ""
    

type Sampler1dShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<float>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, cmp : float, lodBias : float) : float = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, cmp : float, offset : int) : float = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, cmp : float, offset : int, lodBias : float) : float = failwith ""
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V2d, cmp : float) : float = failwith ""
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V2d, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, cmp : float, level : float) : float = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, cmp : float, dTdx : float, dTdy : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int) : float = failwith ""
    

type Sampler1d(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, lodBias : float) : V4d = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, offset : int) : V4d = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, offset : int, lodBias : float) : V4d = failwith ""
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V2d) : V4d = failwith ""
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V2d, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, level : float) : V4d = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, dTdx : float, dTdy : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int) : V4d = failwith ""
    

type Sampler2dMSArrayShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, cmp : float, level : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int, sample : int) : float = failwith ""
    

type Sampler2dMSArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, level : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int, sample : int) : V4d = failwith ""
    

type Sampler2dArrayShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, cmp : float, lodBias : float) : float = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, slice : int, cmp : float, offset : V2i) : float = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, slice : int, cmp : float, offset : V2i, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, cmp : float, level : float) : float = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, slice : int, cmp : float, dTdx : V2d, dTdy : V2d) : float = failwith ""
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, slice : int, comp : int) : V4d = failwith ""
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, slice : int, offset : V2i, comp : int) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int) : float = failwith ""
    

type Sampler2dArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, lodBias : float) : V4d = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i) : V4d = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, level : float) : V4d = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, slice : int, dTdx : V2d, dTdy : V2d) : V4d = failwith ""
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, slice : int, comp : int) : V4d = failwith ""
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, slice : int, offset : V2i, comp : int) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int) : V4d = failwith ""
    

type Sampler2dMSShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, cmp : float, level : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int, sample : int) : float = failwith ""
    

type Sampler2dMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int, sample : int) : V4d = failwith ""
    

type Sampler2dShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, cmp : float, lodBias : float) : float = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, cmp : float, offset : V2i) : float = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, cmp : float, offset : V2i, lodBias : float) : float = failwith ""
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V3d, cmp : float) : float = failwith ""
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V3d, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, cmp : float, level : float) : float = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, cmp : float, dTdx : V2d, dTdy : V2d) : float = failwith ""
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, comp : int) : V4d = failwith ""
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, offset : V2i, comp : int) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int) : float = failwith ""
    

type Sampler2d(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, lodBias : float) : V4d = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, offset : V2i) : V4d = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, offset : V2i, lodBias : float) : V4d = failwith ""
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V3d) : V4d = failwith ""
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V3d, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4d = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, dTdx : V2d, dTdy : V2d) : V4d = failwith ""
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, comp : int) : V4d = failwith ""
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, offset : V2i, comp : int) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int) : V4d = failwith ""
    

type Sampler3dMSShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = failwith ""
    
    /// the size for the sampler
    member x.Size : V3i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, cmp : float, level : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int, sample : int) : float = failwith ""
    

type Sampler3dMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = failwith ""
    
    /// the size for the sampler
    member x.Size : V3i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, level : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int, sample : int) : V4d = failwith ""
    

type Sampler3dShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = failwith ""
    
    /// the size for the sampler
    member x.Size : V3i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, cmp : float, lodBias : float) : float = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V3d, cmp : float, offset : V3i) : float = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V3d, cmp : float, offset : V3i, lodBias : float) : float = failwith ""
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V4d, cmp : float) : float = failwith ""
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V4d, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, cmp : float, level : float) : float = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, cmp : float, dTdx : V3d, dTdy : V3d) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : float = failwith ""
    

type Sampler3d(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = failwith ""
    
    /// the size for the sampler
    member x.Size : V3i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4d = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V3d, offset : V3i) : V4d = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V3d, offset : V3i, lodBias : float) : V4d = failwith ""
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V4d) : V4d = failwith ""
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V4d, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, level : float) : V4d = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, dTdx : V3d, dTdy : V3d) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : V4d = failwith ""
    

type SamplerCubeMSArrayShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, slice : int, cmp : float, level : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int, sample : int) : float = failwith ""
    

type SamplerCubeMSArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, slice : int, level : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int, sample : int) : V4d = failwith ""
    

type SamplerCubeArrayShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, slice : int, cmp : float, level : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : float = failwith ""
    

type SamplerCubeArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, slice : int, level : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : V4d = failwith ""
    

type SamplerCubeMSShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, cmp : float, level : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int, sample : int) : float = failwith ""
    

type SamplerCubeMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, level : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int, sample : int) : V4d = failwith ""
    

type SamplerCubeShadow(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, cmp : float) : float = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, cmp : float, lodBias : float) : float = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, cmp : float, level : float) : float = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : float = failwith ""
    

type SamplerCube(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4d = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4d = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, level : float) : V4d = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : V4d = failwith ""
    

type IntSampler1dMSArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, level : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int, sample : int) : V4i = failwith ""
    

type IntSampler1dArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, lodBias : float) : V4i = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, slice : int, offset : int) : V4i = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, slice : int, offset : int, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, level : float) : V4i = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, slice : int, dTdx : float, dTdy : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int) : V4i = failwith ""
    

type IntSampler1dMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, level : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int, sample : int) : V4i = failwith ""
    

type IntSampler1d(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = failwith ""
    
    /// the size for the sampler
    member x.Size : int = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, lodBias : float) : V4i = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, offset : int) : V4i = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, offset : int, lodBias : float) : V4i = failwith ""
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V2d) : V4i = failwith ""
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V2d, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, level : float) : V4i = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, dTdx : float, dTdy : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int) : V4i = failwith ""
    

type IntSampler2dMSArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, level : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int, sample : int) : V4i = failwith ""
    

type IntSampler2dArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, lodBias : float) : V4i = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i) : V4i = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, level : float) : V4i = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, slice : int, dTdx : V2d, dTdy : V2d) : V4i = failwith ""
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, slice : int, comp : int) : V4i = failwith ""
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, slice : int, offset : V2i, comp : int) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int) : V4i = failwith ""
    

type IntSampler2dMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int, sample : int) : V4i = failwith ""
    

type IntSampler2d(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, lodBias : float) : V4i = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, offset : V2i) : V4i = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, offset : V2i, lodBias : float) : V4i = failwith ""
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V3d) : V4i = failwith ""
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V3d, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4i = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, dTdx : V2d, dTdy : V2d) : V4i = failwith ""
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, comp : int) : V4i = failwith ""
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, offset : V2i, comp : int) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int) : V4i = failwith ""
    

type IntSampler3dMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = failwith ""
    
    /// the size for the sampler
    member x.Size : V3i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, level : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int, sample : int) : V4i = failwith ""
    

type IntSampler3d(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = failwith ""
    
    /// the size for the sampler
    member x.Size : V3i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4i = failwith ""
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V3d, offset : V3i) : V4i = failwith ""
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V3d, offset : V3i, lodBias : float) : V4i = failwith ""
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V4d) : V4i = failwith ""
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V4d, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, level : float) : V4i = failwith ""
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, dTdx : V3d, dTdy : V3d) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : V4i = failwith ""
    

type IntSamplerCubeMSArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, slice : int, level : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int, sample : int) : V4i = failwith ""
    

type IntSamplerCubeArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, slice : int, level : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : V4i = failwith ""
    

type IntSamplerCubeMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, level : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int, sample : int) : V4i = failwith ""
    

type IntSamplerCube(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = failwith ""
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = failwith ""
    
    /// the size for the sampler
    member x.Size : V2i = failwith ""
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4i = failwith ""
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4i = failwith ""
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V4d, level : float) : V4i = failwith ""
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : V4i = failwith ""
    

