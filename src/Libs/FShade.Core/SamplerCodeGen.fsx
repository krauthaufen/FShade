#r "netstandard.dll"
#r @"..\..\..\packages\Aardvark.Base\lib\netstandard2.0\Aardvark.Base.dll"
#r @"..\..\..\packages\Aardvark.Base.TypeProviders\lib\netstandard2.0\Aardvark.Base.TypeProviders.dll"
#r @"..\..\..\packages\Aardvark.Base.FSharp\lib\netstandard2.0\Aardvark.Base.FSharp.dll"
#r @"..\..\..\packages\FSharp.Data.Adaptive\lib\netstandard2.0\FSharp.Data.Adaptive.dll"
#r @"..\..\..\bin\Debug\netstandard2.0\FShade.Imperative.dll"
#r @"..\..\..\bin\Debug\netstandard2.0\FShade.Core.dll"

open System
open System.IO
open FShade
open Aardvark.Base
open System.Runtime.CompilerServices


let namespaceName = "FShade"

let types = [ SamplerType.Float; SamplerType.Int ]
let dims = [ SamplerDimension.Sampler1d; SamplerDimension.Sampler2d; SamplerDimension.Sampler3d; SamplerDimension.SamplerCube]
let arr = [ true; false ]
let ms = [ true; false ]
let shadow = [ true; false ]


let allImageCombinations =
    [
        for t in types do
            for d in dims do
                let arr = if d = SamplerDimension.Sampler3d then [false] else arr
                for a in arr do
                    let ms = if d = SamplerDimension.Sampler2d then ms else [false]
                    for m in ms do
                        yield (t,d,a,m)
    ]

let allCombinations =
    [
        for t in types do
            for d in dims do
                let arr = if d = SamplerDimension.Sampler3d then [false] else arr
                for a in arr do
                    let ms = if d = SamplerDimension.Sampler2d then ms else [false]
                    for m in ms do
                        let shadow = if d = SamplerDimension.Sampler3d || t = SamplerType.Int || m then [false] else shadow
                        for s in shadow do
                            yield (t,d,a,m,s)
    ]

let formats =
    [
        "RGBA"
        "RGB"
        "RG"
        "R"
    ]

let mutable indent = ""

let builder = System.Text.StringBuilder()

let line fmt = Printf.kprintf (fun str -> Console.WriteLine("{0}{1}", indent, str); builder.AppendLine(indent + str) |> ignore) fmt
let start fmt = Printf.kprintf (fun str -> Console.WriteLine("{0}{1}", indent, str); builder.AppendLine(indent + str) |> ignore; indent <- indent + "    ") fmt
let stop() = Console.WriteLine(""); builder.AppendLine("") |> ignore; indent <- indent.Substring 4



type SampleVariants =
    | None = 0x0
    | Bias = 0x1
    

let samplerFunctionDefaultArgs (comment : string) (variants : SampleVariants) (name : string)
                               (args : list<string * string * Option<string>>) (ret : string) =
    let args =
        args |> List.map (fun (n,t,o) ->
            match o with
            | Some def -> sprintf "[<Optional; DefaultParameterValue(%s)>] %s : %s" def n t
            | _ -> sprintf "%s : %s" n t
        ) |> String.concat ", "


    line "/// %s" comment
    line "member x.%s(%s) : %s = onlyInShaderCode \"%s\"" name args ret name
    line ""

    if (variants &&& SampleVariants.Bias) <> SampleVariants.None then
        line "/// %s with lod-bias" comment
        line "member x.%s(%s, lodBias : float) : %s = onlyInShaderCode \"%s\"" name args ret name
        line ""

let memberDefaultArgs (comment : string) (name : string) (args : list<string * string * Option<string>>) (ret : string) =
    let args =
        args |> List.map (fun (n,t,o) ->
            match o with
            | Some def -> sprintf "[<Optional; DefaultParameterValue(%s)>] %s : %s" def n t
            | _ -> sprintf "%s : %s" n t
        ) |> String.concat ", "

    line "/// %s" comment
    line "member x.Item"
    line "    with get (%s) : %s = onlyInShaderCode \"%s\"" args ret name
    line ""

let samplerFunction (comment : string) (variants : SampleVariants) (name : string) (args : list<string * string>) (ret : string) =
    let args = args |> List.map (fun (n, t) -> n, t, None)
    samplerFunctionDefaultArgs comment variants name args ret

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
    line "open System.Runtime.InteropServices"
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

        let gatherReturnType =
            match t with
            | SamplerType.Float -> "V4d"
            | SamplerType.Int -> "V4i"
            | _ -> failwith "unknown sampler baseType"

        let coordComponents =
            match d with
                | SamplerDimension.Sampler1d -> 1
                | SamplerDimension.Sampler2d -> 2
                | SamplerDimension.Sampler3d -> 3
                | SamplerDimension.SamplerCube -> 3
                | _ -> failwith "unsupported sampler-kind"

        let coordType = floatVec coordComponents
        let projCoordType = floatVec (coordComponents + 1)
        let texelCoordType = intVec coordComponents

        let sizeType =
            let dim =
                match d with
                | SamplerDimension.Sampler1d -> 1
                | SamplerDimension.Sampler2d
                | SamplerDimension.SamplerCube -> 2
                | SamplerDimension.Sampler3d -> 3
                | _ -> failwith "unsupported sampler-kind"

            intVec <| if a then dim + 1 else dim

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
        line  "member x.MipMapLevels : int = onlyInShaderCode \"MipMapLevels\""
        line  ""

        if not m then
            line  "/// the level size for the sampler"
            line  "member x.GetSize (level : int) : %s = onlyInShaderCode \"GetSize\"" sizeType
            line  ""

        line  "/// the size for the sampler"
        line  "member x.Size : %s = onlyInShaderCode \"Size\"" sizeType
        line  ""


        let additionalArgs =
            match a, s with
            | true, true -> ["slice", "int"; "cmp", returnType]
            | true, false -> ["slice", "int"]
            | false, true -> ["cmp", returnType]
            | false, false -> []

        let additionalDefaultArgs =
            additionalArgs |> List.map (fun (n, t) -> n, t, None)

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/texture.xhtml
        if not m then
            let variant =
                if s && a && d = SamplerDimension.SamplerCube then
                    SampleVariants.None
                else
                    SampleVariants.Bias

            samplerFunction
                "regular sampled texture-lookup"
                variant
                "Sample"
                (["coord", coordType] @ additionalArgs)
                returnType

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureOffset.xhtml
        if d <> SamplerDimension.SamplerCube && not m then
            samplerFunction
                "regular sampled texture-lookup with offset"
                SampleVariants.Bias
                "SampleOffset"
                (["coord", coordType] @ additionalArgs @ ["offset", texelCoordType])
                returnType

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureProj.xhtml
        if d <> SamplerDimension.SamplerCube && not m && not a then
            samplerFunction
                "projective sampled texture-lookup"
                SampleVariants.Bias
                "SampleProj"
                (["coord", projCoordType] @ additionalArgs)
                returnType

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureLod.xhtml
        if not m && not (s && d = SamplerDimension.SamplerCube) && not (s && a && d = SamplerDimension.Sampler2d) then
            samplerFunction
                "sampled texture-lookup with given level"
                SampleVariants.None
                "SampleLevel"
                (["coord", coordType] @ additionalArgs @ ["level", "float"])
                returnType

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureLodOffset.xhtml
        if not m && d <> SamplerDimension.SamplerCube && not (s && a && d = SamplerDimension.Sampler2d) then
            samplerFunction
                "sampled texture-lookup with given level and offset"
                SampleVariants.None
                "SampleLevelOffset"
                (["coord", coordType] @ additionalArgs @ ["level", "float"; "offset", texelCoordType])
                returnType

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureGrad.xhtml
        if not m then
            samplerFunction
                "sampled texture-lookup with explicit gradients"
                SampleVariants.None
                "SampleGrad"
                (["coord", coordType] @ additionalArgs @ ["dTdx", coordType; "dTdy", coordType])
                returnType

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureQueryLod.xhtml
        if not m then
            samplerFunction
                "query lod levels"
                SampleVariants.None
                "QueryLod"
                ["coord", coordType]
                "V2d"

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureGather.xhtml
        if (d = SamplerDimension.Sampler2d || d = SamplerDimension.SamplerCube) && not m then

            let arguments =
                if s then
                    ["coord", coordType, None] @ additionalDefaultArgs
                else
                    ["coord", coordType, None] @ additionalDefaultArgs  @ ["comp", "int", Some "0"]

            samplerFunctionDefaultArgs
                "gathers one component for the neighbouring 4 texels"
                SampleVariants.None
                "Gather"
                arguments
                gatherReturnType

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureGatherOffset.xhtml
        if d = SamplerDimension.Sampler2d && not m then

            let arguments =
                if s then
                    ["coord", coordType, None] @ additionalDefaultArgs @ ["offset", texelCoordType, None]
                else
                    ["coord", coordType, None] @ additionalDefaultArgs @ ["offset", texelCoordType, None; "comp", "int", Some "0"]

            samplerFunctionDefaultArgs
                "gathers one component for the neighbouring 4 texels"
                SampleVariants.None
                "GatherOffset"
                arguments
                gatherReturnType

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/texelFetch.xhtml
        if d <> SamplerDimension.SamplerCube && not s then

            let arguments =
                if m then
                    ["coord", texelCoordType, None] @ additionalDefaultArgs @ ["sample", "int", Some "0"]
                else
                    ["coord", texelCoordType, None] @ additionalDefaultArgs @ ["lod", "int", Some "0"]

            samplerFunctionDefaultArgs
                "non-sampled texture read"
                SampleVariants.None
                "Read"
                arguments
                returnType

            memberDefaultArgs
                "non-sampled texture read"
                "Fetch"
                arguments
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
        line  "member x.Run(((t : ShaderTextureHandle, count : int), s : SamplerState)) ="
        line  "    Array.init count (fun i -> %s(t.WithIndex(i), s))" typeName
        stop  ()
        line  "let %s = %s()" valueName builderName
        line  ""

    stop ()


    for (t,d,a,m) in allImageCombinations do
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


        let name = sprintf "%sImage%s%s%s" prefix dim arr ms 

        let returnType =
            match t with
                | SamplerType.Float -> "V4d"
                | SamplerType.Int -> "V4i"
                | _ -> failwith "unknown image baseType"

        let coordComponents =
            match d with
                | SamplerDimension.Sampler1d -> 1
                | SamplerDimension.Sampler2d -> 2
                | SamplerDimension.Sampler3d -> 3
                | SamplerDimension.SamplerCube -> 3
                | _ -> failwith "unsupported image-kind"

        let coordType = intVec coordComponents

        let sizeType =
            let dim =
                match d with
                | SamplerDimension.Sampler1d -> 1
                | SamplerDimension.Sampler2d
                | SamplerDimension.SamplerCube -> 2
                | SamplerDimension.Sampler3d -> 3
                | _ -> failwith "unsupported sampler-kind"

            intVec <| if a then dim + 1 else dim

        let iface =
            match t with
                | SamplerType.Float -> "Formats.IFloatingFormat"
                | SamplerType.Int -> "Formats.ISignedFormat"
                | _ -> ""

        start "type %s<'f when 'f :> %s>() =" name iface
        
        line "interface IImage"
        
        line  "static member FormatType = typeof<'f>"
        line  "static member Dimension = SamplerDimension.Sampler%s" dim
        line  "static member ValueType = typeof<%s>" returnType
        line  "static member CoordType = typeof<%s>" coordType
        line  "static member IsArray = %s" (if a then "true" else "false")
        line  "static member IsMultisampled = %s" (if m then "true" else "false")
        line  ""

        line "member x.Size : %s = onlyInShaderCode \"Size\"" sizeType


        let args =
            [
                yield "coord", coordType
                if a then yield "slice", "int"
                if m then yield "sample", "int"
            ]

        let itemArgs = args |> List.map (fun (n,t) -> sprintf "%s : %s" n t) |> String.concat ", "

        start "member x.Item"
        line "with get(%s) : %s = onlyInShaderCode \"fetch\"" itemArgs returnType
        line "and set(%s) (v : %s) : unit = onlyInShaderCode \"write\"" itemArgs returnType
        stop()


        if t = SamplerType.Int then
            line "member x.AtomicAdd(%s, data : int) : int = onlyInShaderCode \"AtomicAdd\"" itemArgs
            line "member x.AtomicMin(%s, data : int) : int = onlyInShaderCode \"AtomicMin\"" itemArgs
            line "member x.AtomicMax(%s, data : int) : int = onlyInShaderCode \"AtomicMax\"" itemArgs
            line "member x.AtomicAnd(%s, data : int) : int = onlyInShaderCode \"AtomicAnd\"" itemArgs
            line "member x.AtomicOr(%s, data : int) : int = onlyInShaderCode \"AtomicOr\"" itemArgs
            line "member x.AtomicXor(%s, data : int) : int = onlyInShaderCode \"AtomicXor\"" itemArgs
            line "member x.AtomicExchange(%s, data : int) : int = onlyInShaderCode \"AtomicExchange\"" itemArgs
            line "member x.AtomicCompareExchange(%s, cmp : int, data : int) : int = onlyInShaderCode \"AtomicCompareExchange\"" itemArgs


        stop()
        ()


    let str = builder.ToString()
    let fileName = Path.Combine(__SOURCE_DIRECTORY__, "Samplers.fs")
    File.WriteAllText(fileName, str)

    ()

run()
exit 0