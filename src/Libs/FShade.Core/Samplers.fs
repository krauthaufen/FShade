namespace FShade
open Aardvark.Base
open System.Runtime.InteropServices


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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, slice : int, cmp : float, offset : int) : float = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, slice : int, cmp : float, offset : int, lodBias : float) : float = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, slice : int, cmp : float, level : float) : float = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : float, slice : int, cmp : float, level : float, offset : int) : float = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, slice : int, cmp : float, dTdx : float, dTdy : float) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, slice : int, offset : int) : V4d = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, slice : int, offset : int, lodBias : float) : V4d = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, slice : int, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : float, slice : int, level : float, offset : int) : V4d = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, slice : int, dTdx : float, dTdy : float) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4d = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : int, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4d = onlyInShaderCode "Fetch"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, cmp : float, offset : int) : float = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, cmp : float, offset : int, lodBias : float) : float = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V2d, cmp : float) : float = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V2d, cmp : float, lodBias : float) : float = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, cmp : float, level : float) : float = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : float, cmp : float, level : float, offset : int) : float = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, cmp : float, dTdx : float, dTdy : float) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, offset : int) : V4d = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, offset : int, lodBias : float) : V4d = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V2d) : V4d = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V2d, lodBias : float) : V4d = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : float, level : float, offset : int) : V4d = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, dTdx : float, dTdy : float) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4d = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4d = onlyInShaderCode "Fetch"
    

type Sampler2dArrayMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// the number of samples of the texture bound to the sampler
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] sample : int) : V4d = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] sample : int) : V4d = onlyInShaderCode "Fetch"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, slice : int, cmp : float, offset : V2i) : float = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, slice : int, cmp : float, offset : V2i, lodBias : float) : float = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, slice : int, cmp : float, dTdx : V2d, dTdy : V2d) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, slice : int, cmp : float) : V4d = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels
    member x.GatherOffset(coord : V2d, slice : int, cmp : float, offset : V2i) : V4d = onlyInShaderCode "GatherOffset"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i) : V4d = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i, lodBias : float) : V4d = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : V2d, slice : int, level : float, offset : V2i) : V4d = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, slice : int, dTdx : V2d, dTdy : V2d) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, slice : int, [<Optional; DefaultParameterValue(0)>] comp : int) : V4d = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels
    member x.GatherOffset(coord : V2d, slice : int, offset : V2i, [<Optional; DefaultParameterValue(0)>] comp : int) : V4d = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4d = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4d = onlyInShaderCode "Fetch"
    

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
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// the number of samples of the texture bound to the sampler
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, [<Optional; DefaultParameterValue(0)>] sample : int) : V4d = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, [<Optional; DefaultParameterValue(0)>] sample : int) : V4d = onlyInShaderCode "Fetch"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, cmp : float, offset : V2i) : float = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, cmp : float, offset : V2i, lodBias : float) : float = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V3d, cmp : float) : float = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V3d, cmp : float, lodBias : float) : float = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, cmp : float, level : float) : float = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : V2d, cmp : float, level : float, offset : V2i) : float = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, cmp : float, dTdx : V2d, dTdy : V2d) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, cmp : float) : V4d = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels
    member x.GatherOffset(coord : V2d, cmp : float, offset : V2i) : V4d = onlyInShaderCode "GatherOffset"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, offset : V2i) : V4d = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, offset : V2i, lodBias : float) : V4d = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V3d) : V4d = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V3d, lodBias : float) : V4d = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : V2d, level : float, offset : V2i) : V4d = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, dTdx : V2d, dTdy : V2d) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, [<Optional; DefaultParameterValue(0)>] comp : int) : V4d = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels
    member x.GatherOffset(coord : V2d, offset : V2i, [<Optional; DefaultParameterValue(0)>] comp : int) : V4d = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4d = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4d = onlyInShaderCode "Fetch"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V3d, offset : V3i) : V4d = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V3d, offset : V3i, lodBias : float) : V4d = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V4d) : V4d = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V4d, lodBias : float) : V4d = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : V3d, level : float, offset : V3i) : V4d = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, dTdx : V3d, dTdy : V3d) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : V3i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4d = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V3i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4d = onlyInShaderCode "Fetch"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, slice : int, cmp : float, dTdx : V3d, dTdy : V3d) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V3d, slice : int, cmp : float) : V4d = onlyInShaderCode "Gather"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, slice : int, dTdx : V3d, dTdy : V3d) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V3d, slice : int, [<Optional; DefaultParameterValue(0)>] comp : int) : V4d = onlyInShaderCode "Gather"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, cmp : float, dTdx : V3d, dTdy : V3d) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V3d, cmp : float) : V4d = onlyInShaderCode "Gather"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, dTdx : V3d, dTdy : V3d) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V3d, [<Optional; DefaultParameterValue(0)>] comp : int) : V4d = onlyInShaderCode "Gather"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, slice : int, offset : int) : V4i = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, slice : int, offset : int, lodBias : float) : V4i = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, slice : int, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : float, slice : int, level : float, offset : int) : V4i = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, slice : int, dTdx : float, dTdy : float) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4i = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : int, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4i = onlyInShaderCode "Fetch"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, offset : int) : V4i = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, offset : int, lodBias : float) : V4i = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V2d) : V4i = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V2d, lodBias : float) : V4i = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : float, level : float, offset : int) : V4i = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, dTdx : float, dTdy : float) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4i = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4i = onlyInShaderCode "Fetch"
    

type IntSampler2dArrayMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// the number of samples of the texture bound to the sampler
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] sample : int) : V4i = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] sample : int) : V4i = onlyInShaderCode "Fetch"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i) : V4i = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i, lodBias : float) : V4i = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : V2d, slice : int, level : float, offset : V2i) : V4i = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, slice : int, dTdx : V2d, dTdy : V2d) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, slice : int, [<Optional; DefaultParameterValue(0)>] comp : int) : V4i = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels
    member x.GatherOffset(coord : V2d, slice : int, offset : V2i, [<Optional; DefaultParameterValue(0)>] comp : int) : V4i = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4i = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4i = onlyInShaderCode "Fetch"
    

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
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// the number of samples of the texture bound to the sampler
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, [<Optional; DefaultParameterValue(0)>] sample : int) : V4i = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, [<Optional; DefaultParameterValue(0)>] sample : int) : V4i = onlyInShaderCode "Fetch"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, offset : V2i) : V4i = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, offset : V2i, lodBias : float) : V4i = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V3d) : V4i = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V3d, lodBias : float) : V4i = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : V2d, level : float, offset : V2i) : V4i = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, dTdx : V2d, dTdy : V2d) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, [<Optional; DefaultParameterValue(0)>] comp : int) : V4i = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels
    member x.GatherOffset(coord : V2d, offset : V2i, [<Optional; DefaultParameterValue(0)>] comp : int) : V4i = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4i = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4i = onlyInShaderCode "Fetch"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V3d, offset : V3i) : V4i = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V3d, offset : V3i, lodBias : float) : V4i = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V4d) : V4i = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V4d, lodBias : float) : V4i = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : V3d, level : float, offset : V3i) : V4i = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, dTdx : V3d, dTdy : V3d) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : V3i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4i = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V3i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4i = onlyInShaderCode "Fetch"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, slice : int, dTdx : V3d, dTdy : V3d) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V3d, slice : int, [<Optional; DefaultParameterValue(0)>] comp : int) : V4i = onlyInShaderCode "Gather"
    

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
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, dTdx : V3d, dTdy : V3d) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V3d, [<Optional; DefaultParameterValue(0)>] comp : int) : V4i = onlyInShaderCode "Gather"
    

type UIntSampler1dArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, lodBias : float) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, slice : int, offset : int) : V4ui = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, slice : int, offset : int, lodBias : float) : V4ui = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, slice : int, level : float) : V4ui = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : float, slice : int, level : float, offset : int) : V4ui = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, slice : int, dTdx : float, dTdy : float) : V4ui = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4ui = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : int, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4ui = onlyInShaderCode "Fetch"
    

type UIntSampler1d(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, lodBias : float) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, offset : int) : V4ui = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, offset : int, lodBias : float) : V4ui = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V2d) : V4ui = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V2d, lodBias : float) : V4ui = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, level : float) : V4ui = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : float, level : float, offset : int) : V4ui = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, dTdx : float, dTdy : float) : V4ui = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4ui = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4ui = onlyInShaderCode "Fetch"
    

type UIntSampler2dArrayMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// the number of samples of the texture bound to the sampler
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] sample : int) : V4ui = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] sample : int) : V4ui = onlyInShaderCode "Fetch"
    

type UIntSampler2dArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, lodBias : float) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i) : V4ui = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i, lodBias : float) : V4ui = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, level : float) : V4ui = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : V2d, slice : int, level : float, offset : V2i) : V4ui = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, slice : int, dTdx : V2d, dTdy : V2d) : V4ui = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, slice : int, [<Optional; DefaultParameterValue(0)>] comp : int) : V4ui = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels
    member x.GatherOffset(coord : V2d, slice : int, offset : V2i, [<Optional; DefaultParameterValue(0)>] comp : int) : V4ui = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4ui = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, slice : int, [<Optional; DefaultParameterValue(0)>] lod : int) : V4ui = onlyInShaderCode "Fetch"
    

type UIntSampler2dMS(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// the number of samples of the texture bound to the sampler
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, [<Optional; DefaultParameterValue(0)>] sample : int) : V4ui = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, [<Optional; DefaultParameterValue(0)>] sample : int) : V4ui = onlyInShaderCode "Fetch"
    

type UIntSampler2d(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, lodBias : float) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, offset : V2i) : V4ui = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, offset : V2i, lodBias : float) : V4ui = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V3d) : V4ui = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V3d, lodBias : float) : V4ui = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, level : float) : V4ui = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : V2d, level : float, offset : V2i) : V4ui = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, dTdx : V2d, dTdy : V2d) : V4ui = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, [<Optional; DefaultParameterValue(0)>] comp : int) : V4ui = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels
    member x.GatherOffset(coord : V2d, offset : V2i, [<Optional; DefaultParameterValue(0)>] comp : int) : V4ui = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4ui = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V2i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4ui = onlyInShaderCode "Fetch"
    

type UIntSampler3d(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V3d, offset : V3i) : V4ui = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V3d, offset : V3i, lodBias : float) : V4ui = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V4d) : V4ui = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V4d, lodBias : float) : V4ui = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4ui = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with given level and offset
    member x.SampleLevelOffset(coord : V3d, level : float, offset : V3i) : V4ui = onlyInShaderCode "SampleLevelOffset"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, dTdx : V3d, dTdy : V3d) : V4ui = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : V3i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4ui = onlyInShaderCode "Read"
    
    /// non-sampled texture read
    member x.Item
        with get (coord : V3i, [<Optional; DefaultParameterValue(0)>] lod : int) : V4ui = onlyInShaderCode "Fetch"
    

type UIntSamplerCubeArray(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, lodBias : float) : V4ui = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, level : float) : V4ui = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, slice : int, dTdx : V3d, dTdy : V3d) : V4ui = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V3d, slice : int, [<Optional; DefaultParameterValue(0)>] comp : int) : V4ui = onlyInShaderCode "Gather"
    

type UIntSamplerCube(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the number of mip-map levels of the texture bound to the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the level size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4ui = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4ui = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4ui = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, dTdx : V3d, dTdy : V3d) : V4ui = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V3d, [<Optional; DefaultParameterValue(0)>] comp : int) : V4ui = onlyInShaderCode "Gather"
    

[<AutoOpen>]
module SamplerBuilders = 
    type Sampler1dArrayShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler1dArrayShadow(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1dArrayShadow(t.WithIndex(i), s))

    let sampler1dArrayShadow = Sampler1dArrayShadowBuilder()
    
    type Sampler1dArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler1dArray(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1dArray(t.WithIndex(i), s))

    let sampler1dArray = Sampler1dArrayBuilder()
    
    type Sampler1dShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler1dShadow(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1dShadow(t.WithIndex(i), s))

    let sampler1dShadow = Sampler1dShadowBuilder()
    
    type Sampler1dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler1d(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1d(t.WithIndex(i), s))

    let sampler1d = Sampler1dBuilder()
    
    type Sampler2dArrayMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler2dArrayMS(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dArrayMS(t.WithIndex(i), s))

    let sampler2dArrayMS = Sampler2dArrayMSBuilder()
    
    type Sampler2dArrayShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler2dArrayShadow(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dArrayShadow(t.WithIndex(i), s))

    let sampler2dArrayShadow = Sampler2dArrayShadowBuilder()
    
    type Sampler2dArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler2dArray(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dArray(t.WithIndex(i), s))

    let sampler2dArray = Sampler2dArrayBuilder()
    
    type Sampler2dMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler2dMS(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dMS(t.WithIndex(i), s))

    let sampler2dMS = Sampler2dMSBuilder()
    
    type Sampler2dShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler2dShadow(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dShadow(t.WithIndex(i), s))

    let sampler2dShadow = Sampler2dShadowBuilder()
    
    type Sampler2dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler2d(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2d(t.WithIndex(i), s))

    let sampler2d = Sampler2dBuilder()
    
    type Sampler3dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            Sampler3d(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler3d(t.WithIndex(i), s))

    let sampler3d = Sampler3dBuilder()
    
    type SamplerCubeArrayShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            SamplerCubeArrayShadow(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCubeArrayShadow(t.WithIndex(i), s))

    let samplerCubeArrayShadow = SamplerCubeArrayShadowBuilder()
    
    type SamplerCubeArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            SamplerCubeArray(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCubeArray(t.WithIndex(i), s))

    let samplerCubeArray = SamplerCubeArrayBuilder()
    
    type SamplerCubeShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            SamplerCubeShadow(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCubeShadow(t.WithIndex(i), s))

    let samplerCubeShadow = SamplerCubeShadowBuilder()
    
    type SamplerCubeBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            SamplerCube(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCube(t.WithIndex(i), s))

    let samplerCube = SamplerCubeBuilder()
    
    type IntSampler1dArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            IntSampler1dArray(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler1dArray(t.WithIndex(i), s))

    let intSampler1dArray = IntSampler1dArrayBuilder()
    
    type IntSampler1dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            IntSampler1d(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler1d(t.WithIndex(i), s))

    let intSampler1d = IntSampler1dBuilder()
    
    type IntSampler2dArrayMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            IntSampler2dArrayMS(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler2dArrayMS(t.WithIndex(i), s))

    let intSampler2dArrayMS = IntSampler2dArrayMSBuilder()
    
    type IntSampler2dArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            IntSampler2dArray(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler2dArray(t.WithIndex(i), s))

    let intSampler2dArray = IntSampler2dArrayBuilder()
    
    type IntSampler2dMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            IntSampler2dMS(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler2dMS(t.WithIndex(i), s))

    let intSampler2dMS = IntSampler2dMSBuilder()
    
    type IntSampler2dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            IntSampler2d(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler2d(t.WithIndex(i), s))

    let intSampler2d = IntSampler2dBuilder()
    
    type IntSampler3dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            IntSampler3d(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler3d(t.WithIndex(i), s))

    let intSampler3d = IntSampler3dBuilder()
    
    type IntSamplerCubeArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            IntSamplerCubeArray(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSamplerCubeArray(t.WithIndex(i), s))

    let intSamplerCubeArray = IntSamplerCubeArrayBuilder()
    
    type IntSamplerCubeBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            IntSamplerCube(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSamplerCube(t.WithIndex(i), s))

    let intSamplerCube = IntSamplerCubeBuilder()
    
    type UIntSampler1dArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            UIntSampler1dArray(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> UIntSampler1dArray(t.WithIndex(i), s))

    let uintSampler1dArray = UIntSampler1dArrayBuilder()
    
    type UIntSampler1dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            UIntSampler1d(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> UIntSampler1d(t.WithIndex(i), s))

    let uintSampler1d = UIntSampler1dBuilder()
    
    type UIntSampler2dArrayMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            UIntSampler2dArrayMS(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> UIntSampler2dArrayMS(t.WithIndex(i), s))

    let uintSampler2dArrayMS = UIntSampler2dArrayMSBuilder()
    
    type UIntSampler2dArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            UIntSampler2dArray(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> UIntSampler2dArray(t.WithIndex(i), s))

    let uintSampler2dArray = UIntSampler2dArrayBuilder()
    
    type UIntSampler2dMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            UIntSampler2dMS(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> UIntSampler2dMS(t.WithIndex(i), s))

    let uintSampler2dMS = UIntSampler2dMSBuilder()
    
    type UIntSampler2dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            UIntSampler2d(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> UIntSampler2d(t.WithIndex(i), s))

    let uintSampler2d = UIntSampler2dBuilder()
    
    type UIntSampler3dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            UIntSampler3d(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> UIntSampler3d(t.WithIndex(i), s))

    let uintSampler3d = UIntSampler3dBuilder()
    
    type UIntSamplerCubeArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            UIntSamplerCubeArray(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> UIntSamplerCubeArray(t.WithIndex(i), s))

    let uintSamplerCubeArray = UIntSamplerCubeArrayBuilder()
    
    type UIntSamplerCubeBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : ShaderTextureHandle, s : SamplerState)) =
            UIntSamplerCube(t, s)
        member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) =
            Array.init count (fun i -> UIntSamplerCube(t.WithIndex(i), s))

    let uintSamplerCube = UIntSamplerCubeBuilder()
    

type Image1dArray<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<int>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : int, slice : int) : V4d = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : int, slice : int, data : V4d) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : int, slice : int) : V4d = onlyInShaderCode "Load"
        and set (coord : int, slice : int) (data : V4d) : unit = onlyInShaderCode "Store"
    

type Image1d<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<int>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : int = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : int) : V4d = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : int, data : V4d) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : int) : V4d = onlyInShaderCode "Load"
        and set (coord : int) (data : V4d) : unit = onlyInShaderCode "Store"
    

type Image2dArrayMS<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = true
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// the number of samples of the image
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// load single texel from image
    member x.Load(coord : V2i, slice : int, sample : int) : V4d = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, slice : int, sample : int, data : V4d) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, slice : int, sample : int) : V4d = onlyInShaderCode "Load"
        and set (coord : V2i, slice : int, sample : int) (data : V4d) : unit = onlyInShaderCode "Store"
    

type Image2dArray<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i, slice : int) : V4d = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, slice : int, data : V4d) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, slice : int) : V4d = onlyInShaderCode "Load"
        and set (coord : V2i, slice : int) (data : V4d) : unit = onlyInShaderCode "Store"
    

type Image2dMS<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// the number of samples of the image
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// load single texel from image
    member x.Load(coord : V2i, sample : int) : V4d = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, sample : int, data : V4d) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, sample : int) : V4d = onlyInShaderCode "Load"
        and set (coord : V2i, sample : int) (data : V4d) : unit = onlyInShaderCode "Store"
    

type Image2d<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i) : V4d = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, data : V4d) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i) : V4d = onlyInShaderCode "Load"
        and set (coord : V2i) (data : V4d) : unit = onlyInShaderCode "Store"
    

type Image3d<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V3i) : V4d = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V3i, data : V4d) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V3i) : V4d = onlyInShaderCode "Load"
        and set (coord : V3i) (data : V4d) : unit = onlyInShaderCode "Store"
    

type ImageCubeArray<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i, layerFace : int) : V4d = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, layerFace : int, data : V4d) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, layerFace : int) : V4d = onlyInShaderCode "Load"
        and set (coord : V2i, layerFace : int) (data : V4d) : unit = onlyInShaderCode "Store"
    

type ImageCube<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i, face : int) : V4d = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, face : int, data : V4d) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, face : int) : V4d = onlyInShaderCode "Load"
        and set (coord : V2i, face : int) (data : V4d) : unit = onlyInShaderCode "Store"
    

type IntImage1dArray<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<int>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : int, slice : int) : V4i = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : int, slice : int, data : V4i) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : int, slice : int) : V4i = onlyInShaderCode "Load"
        and set (coord : int, slice : int) (data : V4i) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : int, slice : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"
    

type IntImage1d<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<int>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : int = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : int) : V4i = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : int, data : V4i) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : int) : V4i = onlyInShaderCode "Load"
        and set (coord : int) (data : V4i) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : int, data : int) : int = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : int, data : int) : int = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : int, data : int) : int = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : int, data : int) : int = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"
    

type IntImage2dArrayMS<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = true
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// the number of samples of the image
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// load single texel from image
    member x.Load(coord : V2i, slice : int, sample : int) : V4i = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, slice : int, sample : int, data : V4i) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, slice : int, sample : int) : V4i = onlyInShaderCode "Load"
        and set (coord : V2i, slice : int, sample : int) (data : V4i) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, slice : int, sample : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"
    

type IntImage2dArray<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i, slice : int) : V4i = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, slice : int, data : V4i) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, slice : int) : V4i = onlyInShaderCode "Load"
        and set (coord : V2i, slice : int) (data : V4i) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, slice : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"
    

type IntImage2dMS<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// the number of samples of the image
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// load single texel from image
    member x.Load(coord : V2i, sample : int) : V4i = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, sample : int, data : V4i) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, sample : int) : V4i = onlyInShaderCode "Load"
        and set (coord : V2i, sample : int) (data : V4i) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, sample : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"
    

type IntImage2d<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i) : V4i = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, data : V4i) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i) : V4i = onlyInShaderCode "Load"
        and set (coord : V2i) (data : V4i) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, data : int) : int = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, data : int) : int = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, data : int) : int = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, data : int) : int = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, data : int) : int = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, data : int) : int = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, data : int) : int = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"
    

type IntImage3d<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V3i) : V4i = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V3i, data : V4i) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V3i) : V4i = onlyInShaderCode "Load"
        and set (coord : V3i) (data : V4i) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V3i, data : int) : int = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V3i, data : int) : int = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V3i, data : int) : int = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V3i, data : int) : int = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V3i, data : int) : int = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V3i, data : int) : int = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V3i, data : int) : int = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V3i, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"
    

type IntImageCubeArray<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i, layerFace : int) : V4i = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, layerFace : int, data : V4i) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, layerFace : int) : V4i = onlyInShaderCode "Load"
        and set (coord : V2i, layerFace : int) (data : V4i) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, layerFace : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, layerFace : int, data : int) : int = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, layerFace : int, data : int) : int = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, layerFace : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, layerFace : int, data : int) : int = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, layerFace : int, data : int) : int = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, layerFace : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, layerFace : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"
    

type IntImageCube<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i, face : int) : V4i = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, face : int, data : V4i) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, face : int) : V4i = onlyInShaderCode "Load"
        and set (coord : V2i, face : int) (data : V4i) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, face : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, face : int, data : int) : int = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, face : int, data : int) : int = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, face : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, face : int, data : int) : int = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, face : int, data : int) : int = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, face : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, face : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"
    

type UIntImage1dArray<'f when 'f :> Formats.IUnsignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<int>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : int, slice : int) : V4ui = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : int, slice : int, data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : int, slice : int) : V4ui = onlyInShaderCode "Load"
        and set (coord : int, slice : int) (data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : int, slice : int, data : uint) : uint = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : int, slice : int, data : uint) : uint = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : int, slice : int, data : uint) : uint = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : int, slice : int, data : uint) : uint = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : int, slice : int, data : uint) : uint = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : int, slice : int, data : uint) : uint = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : int, slice : int, data : uint) : uint = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : int, slice : int, cmp : uint, data : uint) : uint = onlyInShaderCode "AtomicCompareExchange"
    

type UIntImage1d<'f when 'f :> Formats.IUnsignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<int>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : int = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : int) : V4ui = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : int, data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : int) : V4ui = onlyInShaderCode "Load"
        and set (coord : int) (data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : int, data : uint) : uint = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : int, data : uint) : uint = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : int, data : uint) : uint = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : int, data : uint) : uint = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : int, data : uint) : uint = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : int, data : uint) : uint = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : int, data : uint) : uint = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : int, cmp : uint, data : uint) : uint = onlyInShaderCode "AtomicCompareExchange"
    

type UIntImage2dArrayMS<'f when 'f :> Formats.IUnsignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = true
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// the number of samples of the image
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// load single texel from image
    member x.Load(coord : V2i, slice : int, sample : int) : V4ui = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, slice : int, sample : int, data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, slice : int, sample : int) : V4ui = onlyInShaderCode "Load"
        and set (coord : V2i, slice : int, sample : int) (data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, slice : int, sample : int, data : uint) : uint = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, slice : int, sample : int, data : uint) : uint = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, slice : int, sample : int, data : uint) : uint = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, slice : int, sample : int, data : uint) : uint = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, slice : int, sample : int, data : uint) : uint = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, slice : int, sample : int, data : uint) : uint = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, slice : int, sample : int, data : uint) : uint = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, slice : int, sample : int, cmp : uint, data : uint) : uint = onlyInShaderCode "AtomicCompareExchange"
    

type UIntImage2dArray<'f when 'f :> Formats.IUnsignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i, slice : int) : V4ui = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, slice : int, data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, slice : int) : V4ui = onlyInShaderCode "Load"
        and set (coord : V2i, slice : int) (data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, slice : int, data : uint) : uint = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, slice : int, data : uint) : uint = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, slice : int, data : uint) : uint = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, slice : int, data : uint) : uint = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, slice : int, data : uint) : uint = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, slice : int, data : uint) : uint = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, slice : int, data : uint) : uint = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, slice : int, cmp : uint, data : uint) : uint = onlyInShaderCode "AtomicCompareExchange"
    

type UIntImage2dMS<'f when 'f :> Formats.IUnsignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// the number of samples of the image
    member x.Samples : int = onlyInShaderCode "Samples"
    
    /// load single texel from image
    member x.Load(coord : V2i, sample : int) : V4ui = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, sample : int, data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, sample : int) : V4ui = onlyInShaderCode "Load"
        and set (coord : V2i, sample : int) (data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, sample : int, data : uint) : uint = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, sample : int, data : uint) : uint = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, sample : int, data : uint) : uint = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, sample : int, data : uint) : uint = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, sample : int, data : uint) : uint = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, sample : int, data : uint) : uint = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, sample : int, data : uint) : uint = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, sample : int, cmp : uint, data : uint) : uint = onlyInShaderCode "AtomicCompareExchange"
    

type UIntImage2d<'f when 'f :> Formats.IUnsignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i) : V4ui = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i) : V4ui = onlyInShaderCode "Load"
        and set (coord : V2i) (data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, data : uint) : uint = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, data : uint) : uint = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, data : uint) : uint = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, data : uint) : uint = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, data : uint) : uint = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, data : uint) : uint = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, data : uint) : uint = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, cmp : uint, data : uint) : uint = onlyInShaderCode "AtomicCompareExchange"
    

type UIntImage3d<'f when 'f :> Formats.IUnsignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V3i) : V4ui = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V3i, data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V3i) : V4ui = onlyInShaderCode "Load"
        and set (coord : V3i) (data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V3i, data : uint) : uint = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V3i, data : uint) : uint = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V3i, data : uint) : uint = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V3i, data : uint) : uint = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V3i, data : uint) : uint = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V3i, data : uint) : uint = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V3i, data : uint) : uint = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V3i, cmp : uint, data : uint) : uint = onlyInShaderCode "AtomicCompareExchange"
    

type UIntImageCubeArray<'f when 'f :> Formats.IUnsignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i, layerFace : int) : V4ui = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, layerFace : int, data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, layerFace : int) : V4ui = onlyInShaderCode "Load"
        and set (coord : V2i, layerFace : int) (data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, layerFace : int, data : uint) : uint = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, layerFace : int, data : uint) : uint = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, layerFace : int, data : uint) : uint = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, layerFace : int, data : uint) : uint = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, layerFace : int, data : uint) : uint = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, layerFace : int, data : uint) : uint = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, layerFace : int, data : uint) : uint = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, layerFace : int, cmp : uint, data : uint) : uint = onlyInShaderCode "AtomicCompareExchange"
    

type UIntImageCube<'f when 'f :> Formats.IUnsignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4ui>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// load single texel from image
    member x.Load(coord : V2i, face : int) : V4ui = onlyInShaderCode "Load"
    
    /// write single texel into image
    [<FShade.Imperative.KeepCall>]
    member x.Store(coord : V2i, face : int, data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// access single texel of image
    member x.Item
        with get (coord : V2i, face : int) : V4ui = onlyInShaderCode "Load"
        and set (coord : V2i, face : int) (data : V4ui) : unit = onlyInShaderCode "Store"
    
    /// atomically add a value to an existing value in memory and return the original value
    member x.AtomicAdd(coord : V2i, face : int, data : uint) : uint = onlyInShaderCode "AtomicAdd"
    
    /// atomically compute the minimum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMin(coord : V2i, face : int, data : uint) : uint = onlyInShaderCode "AtomicMin"
    
    /// atomically compute the maximum of a value with an existing value in memory, store that value and return the original value
    member x.AtomicMax(coord : V2i, face : int, data : uint) : uint = onlyInShaderCode "AtomicMax"
    
    /// atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value
    member x.AtomicAnd(coord : V2i, face : int, data : uint) : uint = onlyInShaderCode "AtomicAnd"
    
    /// atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicOr(coord : V2i, face : int, data : uint) : uint = onlyInShaderCode "AtomicOr"
    
    /// atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value
    member x.AtomicXor(coord : V2i, face : int, data : uint) : uint = onlyInShaderCode "AtomicXor"
    
    /// atomically store supplied data into memory and return the original value from memory
    member x.AtomicExchange(coord : V2i, face : int, data : uint) : uint = onlyInShaderCode "AtomicExchange"
    
    /// atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp.
    member x.AtomicCompareExchange(coord : V2i, face : int, cmp : uint, data : uint) : uint = onlyInShaderCode "AtomicCompareExchange"
    

