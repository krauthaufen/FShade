namespace FShade

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative

type ShaderCompiler private() =
    
    static member compile (prev : Option<ShaderType>, s : Shader, next : Option<ShaderType>) : EntryPoint =

        let inputs = 
            s.shaderInputs |> List.map (fun i -> 
                { 
                    paramName = i.paramSemantic
                    paramSemantic = i.paramSemantic
                    paramType = i.paramType
                    paramDecorations = Set.ofList [ParameterDecoration.Interpolation i.paramInterpolation]
                }
            )

        let outputs = 
            s.shaderOutptus |> List.map (fun i -> 
                { 
                    paramName = i.paramSemantic
                    paramSemantic = i.paramSemantic
                    paramType = i.paramType
                    paramDecorations = Set.empty
                }
            )

        let uniforms =
            s.shaderUniforms |> List.map (fun u ->
                let uniformBuffer = 
                    match u.uniformValue with
                        | Attribute(scope, name) -> Some scope.FullName
                        | _ -> None
                { 
                    uniformName = u.uniformName
                    uniformType = u.uniformType
                    uniformBuffer = uniformBuffer
                }
            )

        {
            conditional = s.shaderType |> string |> Some
            entryName   = "main"
            inputs      = inputs
            outputs     = outputs
            uniforms    = uniforms
            arguments   = []
            body        = s.shaderBody
            decorations = 
                List.concat [
                    [ EntryDecoration.Stages { prev = prev; self = s.shaderType; next = next } ]
                    s.shaderInputTopology |> Option.map EntryDecoration.InputTopology |> Option.toList
                    s.shaderOutputTopology |> Option.map EntryDecoration.OutputTopology |> Option.toList
                ]
        }
       
    static member compile (s : Shader) : EntryPoint =
        ShaderCompiler.compile(None, s, None) 

    static member compile (e : Effect) : Module =
        let rec compileAll (prev : Option<ShaderType>) (list : list<Shader>) =
            match list with
                | [] -> []
                | [last] -> [ ShaderCompiler.compile(prev, last, None) ]
                | current :: next :: rest ->
                    ShaderCompiler.compile(prev, current, Some next.shaderType) ::
                    compileAll (Some current.shaderType) (next :: rest)

        let entries = compileAll None e.shaders

        { entries = entries }
