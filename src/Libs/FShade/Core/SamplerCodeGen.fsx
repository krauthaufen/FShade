#I @"..\..\..\..\Bin\Debug\"
#r @"Aardvark.Base.dll"
#r @"FShade.Compiler.dll"
#r @"FShade.dll"

open System
open System.IO
open FShade
open FShade.Utils


let namespaceName = "FShade"

let types = [ SamplerType.Float; SamplerType.Int ]
let dims = [ SamplerDimension.Sampler1d; SamplerDimension.Sampler2d; SamplerDimension.Sampler3d; SamplerDimension.SamplerCube]
let arr = [ true; false ]
let ms = [ true; false ]
let shadow = [ true; false ]

let allCombinations =
    [
        for t in types do
            for d in dims do
                let arr = if d = SamplerDimension.Sampler3d then [false] else arr
                for a in arr do
                    for m in ms do
                        for s in shadow do
                            if t = SamplerType.Int && s then 
                                ()
                            else
                                yield (t,d,a,m,s)
    ]

let mutable indent = ""

let builder = System.Text.StringBuilder()

let line fmt = Printf.kprintf (fun str -> Console.WriteLine("{0}{1}", indent, str); builder.AppendLine(indent + str) |> ignore) fmt
let start fmt = Printf.kprintf (fun str -> Console.WriteLine("{0}{1}", indent, str); builder.AppendLine(indent + str) |> ignore; indent <- indent + "    ") fmt
let stop() = Console.WriteLine(""); builder.AppendLine("") |> ignore; indent <- indent.Substring 4



type SampleVariants =
    | None = 0x0
    | Bias = 0x1
    

let samplerFunction (comment : string) (variants : SampleVariants) (name : string) (args : list<string * string>) (ret : string) =
    let args = args |> List.map (fun (n,t) -> sprintf "%s : %s" n t) |> String.concat ", "
    line "/// %s" comment
    line "member x.%s(%s) : %s = failwith \"\"" name args ret
    line ""

    if (variants &&& SampleVariants.Bias) <> SampleVariants.None then
        line "/// %s with lod-bias" comment
        line "member x.%s(%s, lodBias : float) : %s = failwith \"\"" name args ret
        line ""


let floatVec (c : int) =
    match c with
        | 1 -> "float"
        | n -> sprintf "V%dd" n

let intVec (c : int) =
    match c with
        | 1 -> "int"
        | n -> sprintf "V%di" n

let run() =
    builder.Clear() |> ignore

    line "namespace FShade"
    line "open Aardvark.Base"
    line ""
    line ""

    for (t,d,a,m,s) in allCombinations do
        let prefix =
            match t with
                | SamplerType.Float -> ""
                | SamplerType.Int -> "Int"
                | _ -> ""

        let dim =
            match d with
                | SamplerDimension.Sampler1d -> "1d"
                | SamplerDimension.Sampler2d -> "2d"
                | SamplerDimension.Sampler3d -> "3d"
                | SamplerDimension.SamplerCube -> "Cube"
                | _ -> "2d"

        let ms = if m then "MS" else ""
        let arr = if a then "Array" else ""
        let shadow = if s then "Shadow" else ""


        let name = sprintf "%sSampler%s%s%s%s" prefix dim arr shadow ms

        let returnType =
            match t with
                | SamplerType.Float -> 
                    if s then "float"
                    else "V4d"
                | SamplerType.Int -> "V4i"
                | _ -> failwith "unknown sampler baseType"

        let coordComponents =
            match d with
                | SamplerDimension.Sampler1d -> 1
                | SamplerDimension.Sampler2d -> 2
                | SamplerDimension.Sampler3d -> 3
                | SamplerDimension.SamplerCube -> 3
                | _ -> failwith "unsupported sampler-kind"

//        let coordComponents =
//            if a then coordComponents + 1
//            else coordComponents
//
//        let coordComponents =
//            if s then coordComponents + 1
//            else coordComponents

        let coordType = floatVec coordComponents
        let projCoordType = floatVec (coordComponents + 1)
        let texelCoordType = intVec coordComponents
//
//        let coordType, projCoordType, texelCoordType =
//            match d with
//                | SamplerDimension.Sampler1d -> "float", "V2d", "int"
//                | SamplerDimension.Sampler2d -> "V2d", "V3d", "V2i"
//                | SamplerDimension.Sampler3d -> "V3d", "V4d", "V3i"
//                | SamplerDimension.SamplerCube -> "V3d", "ERROR", "V3i"
//                | _ -> failwith "unsupported sampler-kind"
//

        let sizeType =
            match d with
                | SamplerDimension.Sampler1d -> "int"
                | SamplerDimension.Sampler2d -> "V2i"
                | SamplerDimension.Sampler3d -> "V3i"
                | SamplerDimension.SamplerCube -> "V2i"
                | _ -> failwith "unsupported sampler-kind"


        start "type %s(tex : ISemanticValue, state : SamplerState) =" name

        start "interface ISampler with"
        line  "member x.Texture = tex"
        line  "member x.State = state"
        stop  ()

        line  "static member Dimension = SamplerDimension.Sampler%s" dim
        line  "static member ValueType = typeof<%s>" returnType
        line  "static member CoordType = typeof<%s>" coordType
        line  "static member IsArray = %s" (if a then "true" else "false")
        line  "static member IsShadow = %s" (if s then "true" else "false")
        line  "static member IsMultisampled = %s" (if m then "true" else "false")
        line  ""


        line  "/// the mipmap-levels for the sampler"
        line  "member x.MipMapLevels : int = failwith \"\""
        line  ""

        line  "/// the size for the sampler"
        line  "member x.GetSize (level : int) : %s = failwith \"\"" sizeType
        line  ""

        line  "/// the size for the sampler"
        line  "member x.Size : %s = failwith \"\"" sizeType
        line  ""


        let additionalArgs =
            match a, s with
                | true, true -> ["slice", "int"; "cmp", returnType]
                | true, false -> ["slice", "int"]
                | false, true -> ["cmp", returnType]
                | false, false -> []

        samplerFunction 
            "regular sampled texture-lookup"
            SampleVariants.Bias
            "Sample"
            (["coord", coordType] @ additionalArgs)
            returnType

        // Cubemap, multisample, and buffer samplers are not allowed
        if d <> SamplerDimension.SamplerCube && not m then
            samplerFunction 
                "regular sampled texture-lookup with offset"
                SampleVariants.Bias
                "SampleOffset"
                (["coord", coordType] @ additionalArgs @ ["offset", texelCoordType])
                returnType

        // Array, cubemap, multisample, and buffer samplers cannot be used with this function
        if d <> SamplerDimension.SamplerCube && not m && not a then
            samplerFunction 
                "projective sampled texture-lookup"
                SampleVariants.Bias
                "SampleProj"
                (["coord", projCoordType] @ additionalArgs)
                returnType

        samplerFunction 
            "sampled texture-lookup with given level"
            SampleVariants.None
            "SampleLevel"
            (["coord", coordType] @ additionalArgs @ ["level", "float"])
            returnType

        // This function works for sampler types that are not multisample, buffer texture, or cubemap array samplers
        if d <> SamplerDimension.SamplerCube && not m then
            samplerFunction 
                "sampled texture-lookup with explicit gradients"
                SampleVariants.None
                "SampleGrad"
                (["coord", coordType] @ additionalArgs @ ["dTdx", coordType; "dTdy", coordType])
                returnType


        if d = SamplerDimension.Sampler2d && not m then
            let additionalArgs = 
                if a then ["slice", "int"]
                else []

            let gatherType =
                match t with
                    | SamplerType.Int -> "V4i"
                    | _ -> "V4d"

            samplerFunction 
                "gathers one component for the neighbouring 4 texels"
                SampleVariants.None
                "Gather"
                (["coord", coordType] @ additionalArgs @ ["comp", "int"])
                gatherType

            samplerFunction 
                "gathers one component for the neighbouring 4 texels with an offset"
                SampleVariants.None
                "GatherOffset"
                (["coord", coordType] @ additionalArgs @ ["offset", texelCoordType; "comp", "int"])
                gatherType



        if m then
            samplerFunction 
                "non-sampled texture read"
                SampleVariants.None
                "Read"
                (["coord", texelCoordType; "lod", "int"; "sample", "int"])
                returnType
        else
            samplerFunction 
                "non-sampled texture read"
                SampleVariants.None
                "Read"
                (["coord", texelCoordType; "lod", "int"])
                returnType

                        
        stop()


    line  "[<AutoOpen>]"
    start "module SamplerBuilders = "

    for (t,d,a,m,s) in allCombinations do
        let prefix =
            match t with
                | SamplerType.Float -> ""
                | SamplerType.Int -> "Int"
                | _ -> ""

        let dim =
            match d with
                | SamplerDimension.Sampler1d -> "1d"
                | SamplerDimension.Sampler2d -> "2d"
                | SamplerDimension.Sampler3d -> "3d"
                | SamplerDimension.SamplerCube -> "Cube"
                | _ -> "2d"

        let ms = if m then "MS" else ""
        let arr = if a then "Array" else ""
        let shadow = if s then "Shadow" else ""


        let typeName = sprintf "%sSampler%s%s%s%s" prefix dim arr shadow ms
        let builderName = sprintf "%sBuilder" typeName
        let valueName = 
            match t with
                | SamplerType.Float -> sprintf "sampler%s%s%s%s" dim arr shadow ms
                | _ -> sprintf "intSampler%s%s%s%s" dim arr shadow ms

        start "type %s() = " builderName
        line  "inherit SamplerBaseBuilder()"
        line  "member x.Run((t : ShaderTextureHandle, s : SamplerState)) ="
        line  "    %s(t, s)" typeName
        stop  ()
        line  "let %s = %s()" valueName builderName
        line  ""

    stop ()



    let str = builder.ToString()
    let fileName = Path.Combine(__SOURCE_DIRECTORY__, "SamplerGenerated.fs")
    File.WriteAllText(fileName, str)

    ()
