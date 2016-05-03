namespace FShade

open Aardvark.Base

[<AutoOpen>]
module SamplerTypes = 

    type Sampler1dMSArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler1d
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<float>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : float, slice : int) : V4d = failwith ""
    


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

        member x.Sample(coord : float, slice : int) : V4d = failwith ""
    


    type Sampler1dArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler1d
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<float>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : float, slice : int) : V4d = failwith ""
    


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

        member x.Sample(coord : float, slice : int) : V4d = failwith ""
    


    type Sampler1dMSShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler1d
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<float>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : float) : V4d = failwith ""
    


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

        member x.Sample(coord : float) : V4d = failwith ""
    


    type Sampler1dShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler1d
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<float>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : float) : V4d = failwith ""
    


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

        member x.Sample(coord : float) : V4d = failwith ""
    


    type Sampler2dMSArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler2d
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<V2d>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : V2d, slice : int) : V4d = failwith ""
        member x.SampleLevel(coord : V2d, level : float) : V4d = failwith ""
    


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

        member x.Sample(coord : V2d, slice : int) : V4d = failwith ""
    


    type Sampler2dArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler2d
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<V2d>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : V2d, slice : int) : V4d = failwith ""
    


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

        member x.Sample(coord : V2d, slice : int) : V4d = failwith ""
    


    type Sampler2dMSShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler2d
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<V2d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : V2d) : V4d = failwith ""
    


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

        member x.Sample(coord : V2d) : V4d = failwith ""
    


    type Sampler2dShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler2d
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<V2d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : V2d) : V4d = failwith ""
    


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

        member x.Sample(coord : V2d) : V4d = failwith ""
    


    type Sampler3dMSShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler3d
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<V3d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : V3d) : V4d = failwith ""
    


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

        member x.Sample(coord : V3d) : V4d = failwith ""
    


    type Sampler3dShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler3d
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<V3d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : V3d) : V4d = failwith ""
    


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

        member x.Sample(coord : V3d) : V4d = failwith ""
    


    type SamplerCubeMSArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.SamplerCube
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<V3d>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : V3d, slice : int) : V4d = failwith ""
    


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

        member x.Sample(coord : V3d, slice : int) : V4d = failwith ""
    


    type SamplerCubeArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.SamplerCube
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<V3d>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : V3d, slice : int) : V4d = failwith ""
    


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

        member x.Sample(coord : V3d, slice : int) : V4d = failwith ""
    


    type SamplerCubeMSShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.SamplerCube
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<V3d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : V3d) : V4d = failwith ""
    


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

        member x.Sample(coord : V3d) : V4d = failwith ""
    


    type SamplerCubeShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.SamplerCube
        static member ValueType = typeof<V4d>
        static member CoordType = typeof<V3d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : V3d) : V4d = failwith ""
    


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

        member x.Sample(coord : V3d) : V4d = failwith ""
    


    type IntSampler1dMSArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler1d
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<float>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : float, slice : int) : V4i = failwith ""
    


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

        member x.Sample(coord : float, slice : int) : V4i = failwith ""
    


    type IntSampler1dArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler1d
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<float>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : float, slice : int) : V4i = failwith ""
    


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

        member x.Sample(coord : float, slice : int) : V4i = failwith ""
    


    type IntSampler1dMSShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler1d
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<float>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : float) : V4i = failwith ""
    


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

        member x.Sample(coord : float) : V4i = failwith ""
    


    type IntSampler1dShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler1d
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<float>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : float) : V4i = failwith ""
    


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

        member x.Sample(coord : float) : V4i = failwith ""
    


    type IntSampler2dMSArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler2d
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<V2d>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : V2d, slice : int) : V4i = failwith ""
    


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

        member x.Sample(coord : V2d, slice : int) : V4i = failwith ""
    


    type IntSampler2dArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler2d
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<V2d>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : V2d, slice : int) : V4i = failwith ""
    


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

        member x.Sample(coord : V2d, slice : int) : V4i = failwith ""
    


    type IntSampler2dMSShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler2d
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<V2d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : V2d) : V4i = failwith ""
    


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

        member x.Sample(coord : V2d) : V4i = failwith ""
    


    type IntSampler2dShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler2d
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<V2d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : V2d) : V4i = failwith ""
    


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

        member x.Sample(coord : V2d) : V4i = failwith ""
    


    type IntSampler3dMSShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler3d
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<V3d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : V3d) : V4i = failwith ""
    


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

        member x.Sample(coord : V3d) : V4i = failwith ""
    


    type IntSampler3dShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.Sampler3d
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<V3d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : V3d) : V4i = failwith ""
    


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

        member x.Sample(coord : V3d) : V4i = failwith ""
    


    type IntSamplerCubeMSArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.SamplerCube
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<V3d>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : V3d, slice : int) : V4i = failwith ""
    


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

        member x.Sample(coord : V3d, slice : int) : V4i = failwith ""
    


    type IntSamplerCubeArrayShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.SamplerCube
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<V3d>
        static member IsArray = true
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : V3d, slice : int) : V4i = failwith ""
    


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

        member x.Sample(coord : V3d, slice : int) : V4i = failwith ""
    


    type IntSamplerCubeMSShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.SamplerCube
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<V3d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = true

        member x.Sample(coord : V3d) : V4i = failwith ""
    


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

        member x.Sample(coord : V3d) : V4i = failwith ""
    


    type IntSamplerCubeShadow(tex : ISemanticValue, state : SamplerState) =
        interface ISampler with
            member x.Texture = tex
            member x.State = state

        static member Dimension = SamplerDimension.SamplerCube
        static member ValueType = typeof<V4i>
        static member CoordType = typeof<V3d>
        static member IsArray = false
        static member IsShadow = true
        static member IsMultisampled = false

        member x.Sample(coord : V3d) : V4i = failwith ""
    


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

        member x.Sample(coord : V3d) : V4i = failwith ""
