#if INTERACTIVE
#r "..\\..\\BinPrebuilt\\Aardvark.Base.TypeProviders.dll"
#r "..\\..\\BinPrebuilt\\Aardvark.Base.dll"
#r "..\\..\\BinPrebuilt\\Aardvark.Base.FSharp.dll"
#r "..\\..\\Bin\\Debug\\FShade.Compiler.dll"
#r "..\\..\\Bin\\Debug\\FShade.dll"
open FShade
#else
namespace FShade 
#endif
open Aardvark.Base
open FShade.Compiler

module GlslSamplers =
    open FShade.SamplerStateModule

    let compileWrapMode (name : string) (m : Option<WrapMode>) =
        match m with
            | Some m ->
                let v = match m with
                            | WrapMode.Wrap -> "Repeat"
                            | WrapMode.Mirror -> "MirroredRepeat"
                            | WrapMode.Clamp -> "ClampToEdge"
                            | WrapMode.Border -> "ClampToBorder"
                            | WrapMode.MirrorOnce -> "MirroredRepeat"
                            | _ -> "Repeat"

                sprintf "\t%s = %s;\r\n" name v
            | None -> ""

    let translateFilter (f : Option<Filter>) =
        match f with
            | Some f ->
                match f with
                    | Filter.Anisotropic -> Some("Anisotropic", "Linear")
                    | Filter.MinLinearMagMipPoint -> Some("LinearMipMapNearest", "Nearest")
                    | Filter.MinLinearMagPointMipLinear -> Some("LinearMipMapLinear", "Nearest")
                    | Filter.MinMagLinearMipPoint -> Some("LinearMipMapNearest", "Linear")
                    | Filter.MinMagMipLinear -> Some("LinearMipMapLinear", "Linear")
                    | Filter.MinMagMipPoint -> Some("NearestMipMapNearest", "Nearest")
                    | Filter.MinMagPointMipLinear -> Some("NearestMipMapLinear", "Nearest")
                    | Filter.MinPointMagLinearMipPoint -> Some("NearestMipMapNearest", "Linear")
                    | Filter.MinPointMagMipLinear -> Some("NearestMipMapLinear", "Linear")

                    | Filter.MinMagPoint -> Some("Nearest", "Nearest")
                    | Filter.MinMagLinear -> Some("Linear", "Linear")
                    | Filter.MinPointMagLinear -> Some("Nearest", "Linear")
                    | Filter.MinLinearMagPoint -> Some("Linear", "Nearest")
                    | _ -> None
            | None -> None

    let compileFilter (f : Option<Filter>) =
        match translateFilter f with
            | Some(min,mag) ->
                sprintf "\tMinFilter = %s;\r\n\tMagFilter = %s;\r\n" min mag
            | _ -> ""

    let compileBorderColor (f : Option<C4f>) =
        match f with
            | Some c -> sprintf "\tBorderColor = float4(%f, %f, %f, %f);\r\n" c.R c.G c.B c.A
            | None -> ""

    let translateCompareFunc (f : ComparisonFunction) =
        match f with
            | ComparisonFunction.Never -> "Never"
            | ComparisonFunction.Always -> "Always"

            | ComparisonFunction.Less -> "Less"
            | ComparisonFunction.LessOrEqual -> "Lequal"
            | ComparisonFunction.Greater -> "Greater"
            | ComparisonFunction.GreaterOrEqual -> "Gequal"

            | ComparisonFunction.Equal -> "Equal"
            | ComparisonFunction.NotEqual -> "Notequal"
                
            | _ -> "Never"
                
    let compileCompareFunc (f : Option<ComparisonFunction>) =
        match f with
            | Some f -> sprintf "\tComparisonFunc = %s;\r\n" (translateCompareFunc f)
            | None -> ""

    let compileOption (f : 'a -> string) (o : Option<'a>) =
        match o with
            | Some o -> f o
            | None -> ""



    let compileSamplerState (name : string) (v : SamplerState) =
        let mutable setters = ""

        setters <- setters + (compileFilter v.Filter)

        setters <- setters + (compileWrapMode "AddressU" v.AddressU)
        setters <- setters + (compileWrapMode "AddressV" v.AddressV)
        setters <- setters + (compileWrapMode "AddressW" v.AddressW)

        setters <- setters + (v.MipLodBias |> compileOption (fun b -> sprintf "\tMipLODBias = %f;\r\n" b))
        setters <- setters + (v.MaxAnisotropy |> compileOption (fun a -> sprintf "\tMaxAnisotropy = %d;\r\n" a))
        setters <- setters + (compileBorderColor v.BorderColor)
        setters <- setters + (v.MinLod |> compileOption (fun a -> sprintf "\tMinLOD = %f;\r\n" a))
        setters <- setters + (v.MaxLod |> compileOption (fun a -> sprintf "\tMaxLOD = %f;\r\n" a))
                            
        sprintf "SamplerState %s\r\n{\r\n%s};\r\n" name setters

    let compileSamplerComparisonState (name : string) (v : SamplerComparisonState) =
        let mutable setters = ""

        setters <- setters + (compileFilter v.Filter)
        setters <- setters + (compileCompareFunc v.Comparison)

        setters <- setters + (compileWrapMode "AddressU" v.AddressU)
        setters <- setters + (compileWrapMode "AddressV" v.AddressV)
        setters <- setters + (compileWrapMode "AddressW" v.AddressW)

        setters <- setters + (v.MipLodBias |> compileOption (fun b -> sprintf "\tMipLODBias = %f;\r\n" b))
        setters <- setters + (v.MaxAnisotropy |> compileOption (fun a -> sprintf "\tMaxAnisotropy = %d;\r\n" a))
        setters <- setters + (compileBorderColor v.BorderColor)
        setters <- setters + (v.MinLod |> compileOption (fun a -> sprintf "\tMinLOD = %f;\r\n" a))
        setters <- setters + (v.MaxLod |> compileOption (fun a -> sprintf "\tMaxLOD = %f;\r\n" a))
                            
        sprintf "SamplerComparisonState %s\r\n{\r\n%s};\r\n" name setters
