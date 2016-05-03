#if INTERACTIVE
#r "..\\..\\Bin\\Debug\\Aardvark.Base.dll"
#r "..\\..\\Bin\\Debug\\FShade.Compiler.dll"
#r "..\\..\\Bin\\Debug\\FShade.dll"
open FShade
#else
namespace FShade
#endif

open FShade.Utils

module SamplerCodeGen =
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
                                yield (t,d,a,m,s)
        ]

    let template = """
type __name__(tex : ISemanticValue, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension = SamplerDimension.__dim__
    static member ValueType = typeof<__valueType__>
    static member CoordType = typeof<__coordType__>
    static member IsArray = __isArray__
    static member IsShadow = __isShadow__
    static member IsMultisampled = __isMS__

    member x.Sample(__sampleArgs__) : __valueType__ = failwith ""
    member x.SampleLevel(__sampleArgs__, level : float) : __valueType__ = failwith ""
"""
    
    let createType (t : SamplerType) (d : SamplerDimension) (a : bool) (m : bool) (s : bool) =
        let code = template
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


        let name = sprintf "%sSampler%s%s%s%s" prefix dim ms arr shadow

        let returnType =
            match t with
                | SamplerType.Float -> "V4d"
                | SamplerType.Int -> "V4i"
                | _ -> failwith "unknown sampler baseType"

        let coordType, texelCoordType =
            match d with
                | SamplerDimension.Sampler1d -> "float", "int"
                | SamplerDimension.Sampler2d -> "V2d", "V2i"
                | SamplerDimension.Sampler3d -> "V3d", "V3i"
                | SamplerDimension.SamplerCube -> "V3d", "V3i"
                | _ -> failwith "unsupported sampler-kind"

        let samplerArgs =
            if a then
                sprintf "coord : %s, slice : int" coordType
            else
                sprintf "coord : %s" coordType

        code.Replace("__name__", name).Replace("__valueType__", returnType).Replace("__coordType__", coordType)
            .Replace("__dim__", sprintf "%A" d).Replace("__isArray__", if a then "true" else "false")
            .Replace("__isShadow__", if s then "true" else "false").Replace("__isMS__", if m then "true" else "false")
            .Replace("__sampleArgs__", samplerArgs)

    let private lineBreak = System.Text.RegularExpressions.Regex("\n")

    /// <summary>
    /// indents a string with the given level-count. uses 4 spaces per level.
    /// </summary>
    let indent (n : int) (str : string) =
        let lines = lineBreak.Split str
        let prefix = System.String(' ', 4*n)
        lines |> Seq.map (fun l -> if l.Length > 0 then prefix + l else l) |> String.concat "\r\n"

    let generate() =
        let code = allCombinations |> Seq.map (fun (a,b,c,d,e) -> createType a b c d e) |> String.concat "\r\n"

        let file = sprintf "namespace FShade\r\n\r\nopen Aardvark.Base\r\n\r\n[<AutoOpen>]\r\nmodule SamplerTypes = \r\n%s" (indent 1 code)

        printfn "%s" file