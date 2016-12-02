module Lui

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade

module IrradianceOutput = 

    type IrradianceAttribute() = inherit SemanticAttribute("Irradiance")

    type Vertex = {
        [<Color>] color : V4d
        [<Irradiance>] irradiance : V3d
    }

    let ps (v : Vertex) =
        fragment {
        
            return { v with color = V4d(v.irradiance, 1.0) }
        }

module LightmapSampling = 

    [<ReflectedDefinition>]
    let sampleNearest(coord : V2d, pointTexSampler : Sampler2d) : V3d = 
        pointTexSampler.Sample(coord).XYZ

    [<ReflectedDefinition>]
    let sampleBilinear(coord : V2d, linearTexSampler : Sampler2d) : V3d =
        let light = linearTexSampler.Sample(coord)

        if light.W > 0.0 then
            light.XYZ / light.W 
        else
            light.XYZ

    let Gauss3x3Weights = [| 0.25; 0.5; 0.25;
                             0.50; 1.0; 0.50;
                             0.25; 0.5; 0.25 |]
    
    // DOES NOT COMPILE ON AMD -> loop is not unrolled
    [<ReflectedDefinition>]
    let sampleGauss3x3(coord : V2d, linearTexSampler : Sampler2d) : V3d =

        let mutable lightSum = V3d.OOO
        let mutable weightSum = 0.0

        for y in -1..1 do
            for x in -1..1 do
                
                let light = linearTexSampler.SampleOffset(coord, V2i(x, y)) 
                if light.W > 0.0 then 
                    let weight = Gauss3x3Weights.[y * 3 + x];

                    lightSum <- lightSum + light.XYZ / light.W * weight
                    weightSum <- weightSum + weight
        
        if weightSum > 0.0 then
            lightSum / weightSum
        else
            lightSum

    let PoissonOffsets16 = [| V2d( 0.007937789, 0.73124397);
                              V2d(-0.10177308, -0.6509396);
                              V2d(-0.9906806,  -0.63400936);
                              V2d(-0.5583586,  -0.3614012);
                              V2d( 0.7163085,   0.22836149);
                              V2d(-0.65210974,  0.37117887);
                              V2d(-0.12714535,  0.112056136);
                              V2d( 0.48898065, -0.66669613);
                              V2d(-0.9744036,   0.9155904);
                              V2d( 0.9274436,  -0.9896486);
                              V2d( 0.9782181,   0.90990245);
                              V2d( 0.96427417, -0.25506377);
                              V2d(-0.5021933,  -0.9712455);
                              V2d( 0.3091557,  -0.17652994);
                              V2d( 0.4665941,   0.96454906);
                              V2d(-0.4617740,   0.9360856) |]

    [<ReflectedDefinition>]
    let samplePoisson16(coord : V2d, linearTexSampler : Sampler2d) : V3d =

        let pixelOffset = 1.0 / V2d(linearTexSampler.GetSize(0))

        let mutable light = V4d.OOOO

        Preprocessor.unroll()
        for i in 0..15 do
            let sc = coord + PoissonOffsets16.[i] * pixelOffset
            light <- light + linearTexSampler.Sample(sc)

        if light.W > 0.0 then
            light.XYZ / light.W
        else
            light.XYZ
    
    let GaussionWeights = [| 0.30534015083578900;
                             0.32110774218179328;
                             0.19975843378928843;
                             0.31977870111437373;
                             0.30072413935198478;
                             0.30105903090903080;
                             0.39325392215976251;
                             0.28344663323748465;
                             0.16319286323946733;
                             0.15902140007066781;
                             0.16343252881731307;
                             0.24259070726071005;
                             0.21943559641718186;
                             0.37444567789707134;
                             0.22470406924467798;
                             0.23138223290067966 |]

    [<ReflectedDefinition>]
    let samplePoissonGauss16(coord : V2d, linearTexSampler : Sampler2d) : V3d =
        let pixelOffset = 1.0 / V2d(linearTexSampler.GetSize(0))

        let mutable lightSum = V3d.OOO
        let mutable weightSum = 0.0

        
        for i in 0..15 do
            let sc = coord + PoissonOffsets16.[i] * pixelOffset
            let light = linearTexSampler.Sample(sc)
            if light.W > 0.0 then
                let weight = GaussionWeights.[i];
                weightSum <- weightSum + weight
                lightSum <- lightSum + light.XYZ / light.W * weight

        if weightSum > 0.0 then
            lightSum / weightSum
        else
            lightSum

module LightmapIrradiance = 

    let LayerCount      = Symbol.Create("LayerCount")
    let LayerIntensity  = Symbol.Create("LayerIntensity")

    type UniformScope with
        member x.LayerCount : int = x?LayerCount
        member x.LayerIntensity : Arr<N<12>, V3d> = x?LayerIntensity

    type LightMapCoordinatesAttribute() = inherit SemanticAttribute("LightMapCoordinates")
    type IrradianceAttribute() = inherit SemanticAttribute("Irradiance")

    type SampleType = Nearest=1 | Bilinear=2 | Gauss3x3=3 | Poisson16=4 | PoissonGauss16=5

    type Vertex = 
        {
            [<LightMapCoordinates>] 
            [<Interpolation(Interpolation.Perspective)>]
            lc : V2d

            [<Irradiance>] 
            irradiance : V3d
        }

    let private lightmapPointSampler = [|
        sampler2d {
            texture uniform?LightmapTexture0
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }; 
        sampler2d {
            texture uniform?LightmapTexture1
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture2
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture3
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture4
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture5
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture6
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture7
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture8
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture9
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture10
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture11
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }; |]

    let private lightmapLinearSampler = [|
        sampler2d {
            texture uniform?LightmapTexture0
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture1
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture2
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture3
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture4
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture5
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture6
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture7
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture8
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture9
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture10
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        };
        sampler2d {
            texture uniform?LightmapTexture11
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        } |]

    let ps (st : SampleType) (v : Vertex) =

        fragment {
        
            let lc = V2d(v.lc.X, 1.0-v.lc.Y)
            
            let mutable irr = V3d.Zero

            Preprocessor.unroll(0, 16)
            for i in 0..uniform?LayerCount-1 do
                let layer = 
                    match st with
                        | SampleType.Nearest -> LightmapSampling.sampleNearest(lc, lightmapPointSampler.[i])
                        | SampleType.Bilinear -> LightmapSampling.sampleBilinear(lc, lightmapLinearSampler.[i])
                        //| SampleType.Gauss3x3 -> LightmapSampling.sampleGauss3x3(lc, lightmapLinearSampler.[i])
                        | SampleType.Poisson16 -> LightmapSampling.samplePoisson16(lc, lightmapLinearSampler.[i])
                        | SampleType.PoissonGauss16 -> LightmapSampling.samplePoissonGauss16(lc, lightmapLinearSampler.[i])
                        | _ -> V3d.OOO
                let layerIntensity = uniform.LayerIntensity.[i]
                irr <- irr + layer * layerIntensity

            return { v with irradiance = irr }
        }

module MaterialTransmission = 

    let private Lerp (a : V3d) (b : V3d) (s : float) : V3d = failwith ""
    
    type Transmission() = inherit SemanticAttribute("Colors2")

    type Vertex = {
        [<Color>] color : V4d
        [<Transmission>] trans : V4d
    }

    let ps (v : Vertex) =

        fragment {
            
            let color = V4d(v.color.XYZ * v.color.W, 1.0)
            let trans = V4d(Lerp uniform?TransmissionColor V3d.III (1.0 - v.color.W), 1.0)

            return {
                color = color
                trans = trans
                }
        }

    let Effect = 
        toEffect ps 

module TextureCoordinateTransform = 
    type TexCoord() = inherit SemanticAttribute("DiffuseColorCoordinates")

    type Vertex = {
        [<TexCoord>] tc : V2d
    }

    let vs (v : Vertex) =
        vertex {
            let t : V3d = uniform?TexCoordTransform
            let texCoord = t * V3d(v.tc.X, 1.0 - v.tc.Y, 1.0)

            return {
                tc = texCoord.XY
            }
        }

    let Effect = 
        toEffect vs 

module AlphaTest = 

    type Vertex = {
        [<Color>] color : V4d
    }

    let ps (v : Vertex) =

        fragment {

            if v.color.W <= uniform?AlphaTestValue then
                discard()

            return v.color
        }

    let Effect = 
        toEffect ps 

module DiffuseMaterial = 
    type TexCoord() = inherit SemanticAttribute("DiffuseColorCoordinates")

    type Vertex = {
        [<TexCoord>] tc : V2d
        [<Color>] color : V4d
    }

    let private texSampler =
        sampler2d {
            texture uniform?ColorTexture
            filter Filter.Anisotropic
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let ps (v : Vertex) =

        fragment {
            let dc : V3d = uniform?DiffuseColor
            let color = V4d(v.color.XYZ * dc, v.color.W)

            let color = if uniform?HasColorTexture then
                            color * texSampler.Sample(v.tc)
                        else 
                            color
            return color
        }

    let Effect = 
        toEffect ps 