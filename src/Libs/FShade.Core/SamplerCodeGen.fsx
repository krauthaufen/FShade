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
open System.Runtime.InteropServices 

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


let mutable indent = ""

let builder = System.Text.StringBuilder()

let line fmt = Printf.kprintf (fun str -> Console.WriteLine("{0}{1}", indent, str); builder.AppendLine(indent + str) |> ignore) fmt
let start fmt = Printf.kprintf (fun str -> Console.WriteLine("{0}{1}", indent, str); builder.AppendLine(indent + str) |> ignore; indent <- indent + "    ") fmt
let stop() = Console.WriteLine(""); builder.AppendLine("") |> ignore; indent <- indent.Substring 4



type SampleVariants =
    | None = 0x0
    | Bias = 0x1

type Argument =
    { Name : string
      Type : string
      DefaultValue : Option<string> }

    override x.ToString() =
        match x.DefaultValue with
        | Some def -> sprintf "[<Optional; DefaultParameterValue(%s)>] %s : %s" def x.Name x.Type
        | _ -> sprintf "%s : %s" x.Name x.Type


module Argument =
    let create (name : string) (typ : string) (defaultValue : Option<String>) =
        { Name = name; Type = typ; DefaultValue = defaultValue }

    let simple (name : string) (typ : string) =
        { Name = name; Type = typ; DefaultValue = None }


[<Sealed; AbstractClass>]
type Write private() =

    static member MemberFunction(name : string, arguments : list<Argument>, returnType : string,
                                 [<Optional; DefaultParameterValue(null : string)>] comment : string,
                                 [<Optional; DefaultParameterValue(null : string)>] attributes : string,
                                 [<Optional; DefaultParameterValue(SampleVariants.None)>] variants : SampleVariants) =
        let args =
            arguments |> List.map string |> String.concat ", "

        if not <| comment.IsNullOrEmpty() then
            line "/// %s" comment

        if not <| attributes.IsNullOrEmpty() then
            line "%s" attributes

        line "member x.%s(%s) : %s = onlyInShaderCode \"%s\"" name args returnType name
        line ""

        if (variants &&& SampleVariants.Bias) <> SampleVariants.None then
            line "/// %s with lod-bias" comment
            line "member x.%s(%s, lodBias : float) : %s = onlyInShaderCode \"%s\"" name args returnType name
            line ""

    static member MemberFunction(name : string, arguments : list<string * string>, returnType : string,
                                [<Optional; DefaultParameterValue(null : string)>] comment : string,
                                [<Optional; DefaultParameterValue(null : string)>] attributes : string,
                                [<Optional; DefaultParameterValue(SampleVariants.None)>] variants : SampleVariants) =
        let arguments = arguments |> List.map (fun (n, t) -> Argument.simple n t)
        Write.MemberFunction(name, arguments, returnType, comment, attributes, variants)

    static member MemberFunction(name : string, arguments : list<string * string * Option<string>>, returnType : string,
                                [<Optional; DefaultParameterValue(null : string)>] comment : string,
                                [<Optional; DefaultParameterValue(null : string)>] attributes : string,
                                [<Optional; DefaultParameterValue(SampleVariants.None)>] variants : SampleVariants) =
        let arguments = arguments |> List.map (fun (n, t, d) -> Argument.create n t d)
        Write.MemberFunction(name, arguments, returnType, comment, attributes, variants)


    static member Property(name : string, arguments : list<Argument>, returnType : string,
                           [<Optional; DefaultParameterValue(null : string)>] setter : string,
                           [<Optional; DefaultParameterValue(null : string)>] comment : string) =
        let args =
            arguments |> List.map string |> String.concat ", "

        if not <| comment.IsNullOrEmpty() then
            line "/// %s" comment

        line "member x.Item"
        line "    with get (%s) : %s = onlyInShaderCode \"%s\"" args returnType name

        if not <| setter.IsNullOrEmpty() then
            line "    and set (%s) (data : %s) : unit = onlyInShaderCode \"%s\"" args returnType setter

        line ""

    static member Property(name : string, arguments : list<string * string>, returnType : string,
                           [<Optional; DefaultParameterValue(null : string)>] setter : string,
                           [<Optional; DefaultParameterValue(null : string)>] comment : string) =
        let arguments = arguments |> List.map (fun (n, t) -> Argument.simple n t)
        Write.Property(name, arguments, returnType, setter, comment)

    static member Property(name : string, arguments : list<string * string * Option<string>>, returnType : string,
                           [<Optional; DefaultParameterValue(null : string)>] setter : string,
                           [<Optional; DefaultParameterValue(null : string)>] comment : string) =
        let arguments = arguments |> List.map (fun (n, t, d) -> Argument.create n t d)
        Write.Property(name, arguments, returnType, setter, comment)


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

        if not m then
            line  "/// the number of mip-map levels of the texture bound to the sampler"
            line  "member x.MipMapLevels : int = onlyInShaderCode \"MipMapLevels\""
            line  ""

            line  "/// the level size for the sampler"
            line  "member x.GetSize (level : int) : %s = onlyInShaderCode \"GetSize\"" sizeType
            line  ""

        line  "/// the size for the sampler"
        line  "member x.Size : %s = onlyInShaderCode \"Size\"" sizeType
        line  ""

        if m then
            line "/// the number of samples of the texture bound to the sampler"
            line "member x.Samples : int = onlyInShaderCode \"Samples\""
            line ""

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

            Write.MemberFunction(
                "Sample",
                (["coord", coordType] @ additionalArgs),
                returnType,
                comment = "regular sampled texture-lookup",
                variants = variant
            )

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureOffset.xhtml
        if d <> SamplerDimension.SamplerCube && not m then
            Write.MemberFunction(
                "SampleOffset",
                (["coord", coordType] @ additionalArgs @ ["offset", texelCoordType]),
                returnType,
                comment = "regular sampled texture-lookup with offset",
                variants = SampleVariants.Bias
            )

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureProj.xhtml
        if d <> SamplerDimension.SamplerCube && not m && not a then
            Write.MemberFunction(
                "SampleProj",
                (["coord", projCoordType] @ additionalArgs),
                returnType,
                comment ="projective sampled texture-lookup",
                variants = SampleVariants.Bias
            )

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureLod.xhtml
        if not m && not (s && d = SamplerDimension.SamplerCube) && not (s && a && d = SamplerDimension.Sampler2d) then
            Write.MemberFunction(
                "SampleLevel",
                (["coord", coordType] @ additionalArgs @ ["level", "float"]),
                returnType,
                comment = "sampled texture-lookup with given level"
            )

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureLodOffset.xhtml
        if not m && d <> SamplerDimension.SamplerCube && not (s && a && d = SamplerDimension.Sampler2d) then
            Write.MemberFunction(
                "SampleLevelOffset",
                (["coord", coordType] @ additionalArgs @ ["level", "float"; "offset", texelCoordType]),
                returnType,
                comment = "sampled texture-lookup with given level and offset"
            )

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureGrad.xhtml
        if not m then
            Write.MemberFunction(
                "SampleGrad",
                (["coord", coordType] @ additionalArgs @ ["dTdx", coordType; "dTdy", coordType]),
                returnType,
                comment = "sampled texture-lookup with explicit gradients"
            )

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureQueryLod.xhtml
        if not m then
            Write.MemberFunction(
                "QueryLod",
                ["coord", coordType],
                "V2d",
                comment = "query lod levels"
            )

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureGather.xhtml
        if (d = SamplerDimension.Sampler2d || d = SamplerDimension.SamplerCube) && not m then

            let arguments =
                if s then
                    ["coord", coordType, None] @ additionalDefaultArgs
                else
                    ["coord", coordType, None] @ additionalDefaultArgs  @ ["comp", "int", Some "0"]

            Write.MemberFunction(
                "Gather",
                arguments,
                gatherReturnType,
                comment ="gathers one component for the neighbouring 4 texels"
            )

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/textureGatherOffset.xhtml
        if d = SamplerDimension.Sampler2d && not m then

            let arguments =
                if s then
                    ["coord", coordType, None] @ additionalDefaultArgs @ ["offset", texelCoordType, None]
                else
                    ["coord", coordType, None] @ additionalDefaultArgs @ ["offset", texelCoordType, None; "comp", "int", Some "0"]

            Write.MemberFunction(
                "GatherOffset",
                arguments,
                gatherReturnType,
                comment = "gathers one component for the neighbouring 4 texels"
            )

        // https://registry.khronos.org/OpenGL-Refpages/gl4/html/texelFetch.xhtml
        if d <> SamplerDimension.SamplerCube && not s then

            let arguments =
                if m then
                    ["coord", texelCoordType, None] @ additionalDefaultArgs @ ["sample", "int", Some "0"]
                else
                    ["coord", texelCoordType, None] @ additionalDefaultArgs @ ["lod", "int", Some "0"]

            Write.MemberFunction(
                "Read",
                arguments,
                returnType,
                comment = "non-sampled texture read"
            )

            Write.Property(
                "Fetch",
                arguments,
                returnType,
                comment = "non-sampled texture read"
            )

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
                | SamplerDimension.SamplerCube -> 2
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
        line  ""

        if m then
            line "/// the number of samples of the image"
            line "member x.Samples : int = onlyInShaderCode \"Samples\""
            line ""

        let args =
            [
                yield "coord", coordType
                if d = SamplerDimension.SamplerCube then
                    if a then
                        yield "layerFace", "int"
                    else
                        yield "face", "int"
                elif a then
                    yield "slice", "int"
                if m then yield "sample", "int"
            ]

        Write.MemberFunction(
            "Load",
            args,
            returnType,
            comment = "load single texel from image"
        )

        Write.MemberFunction(
            "Store",
            (args @ ["data", returnType]),
            "unit",
            comment = "write single texel into image",
            attributes = "[<FShade.Imperative.KeepCall>]"
        )

        Write.Property(
            "Load",
            args,
            returnType,
            setter = "Store",
            comment = "access single texel of image"
        )

        if t = SamplerType.Int then

            Write.MemberFunction(
                "AtomicAdd",
                args @ ["data", "int"],
                "int",
                comment = "atomically add a value to an existing value in memory and return the original value"
            )

            Write.MemberFunction(
                "AtomicMin",
                args @ ["data", "int"],
                "int",
                comment = "atomically compute the minimum of a value with an existing value in memory, store that value and return the original value"
            )

            Write.MemberFunction(
                "AtomicMax",
                args @ ["data", "int"],
                "int",
                comment = "atomically compute the maximum of a value with an existing value in memory, store that value and return the original value"
            )

            Write.MemberFunction(
                "AtomicAnd",
                args @ ["data", "int"],
                "int",
                comment = "atomically compute the logical AND of a value with an existing value in memory, store that value and return the original value"
            )

            Write.MemberFunction(
                "AtomicOr",
                args @ ["data", "int"],
                "int",
                comment = "atomically compute the logical OR of a value with an existing value in memory, store that value and return the original value"
            )

            Write.MemberFunction(
                "AtomicXor",
                args @ ["data", "int"],
                "int",
                comment = "atomically compute the logical exclusive OR of a value with an existing value in memory, store that value and return the original value"
            )

            Write.MemberFunction(
                "AtomicExchange",
                args @ ["data", "int"],
                "int",
                comment = "atomically store supplied data into memory and return the original value from memory"
            )

            Write.MemberFunction(
                "AtomicCompareExchange",
                args @ ["cmp", "int"; "data", "int"],
                "int",
                comment = "atomically compare supplied data with that in memory and store it to memory, if the original value was equal to cmp."
            )

        stop()
        ()


    let str = builder.ToString()
    let fileName = Path.Combine(__SOURCE_DIRECTORY__, "Samplers.fs")
    File.WriteAllText(fileName, str)

    ()

run()
exit 0