namespace FShade

open System
open FSharp.Quotations
open FShade.Imperative

type RaytracingShader internal (shader : Shader, ?definition : SourceDefinition) =
    static do Serializer.Init()
    do assert (ShaderStage.isRaytracing shader.shaderStage)

    let id = Expr.ComputeHash shader.shaderBody
    let mutable shader = shader

    member x.Id = id
    member x.Shader = shader
    member x.SourceDefinition = definition

    member x.Body
        with get() = shader.shaderBody
        and internal set(e) = shader <- { shader with shaderBody = e }

    member inline x.Stage =
        x.Shader.shaderStage

    member inline x.Uniforms =
        x.Shader.shaderUniforms

    member inline x.RayTypes =
        x.Shader.shaderRayTypes

    member inline x.MissShaders =
        x.Shader.shaderMissShaders

    member inline x.CallableShaders =
        x.Shader.shaderCallableShaders

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RaytracingShader =

    let ofShader (shader : Shader) =
        RaytracingShader shader

    let ofExpr (inputTypes : Type list) (expr : Expr) =
        let shaders = expr |> Shader.ofExpr inputTypes
        let definition = expr |> SourceDefinition.ofExpr inputTypes
        RaytracingShader(shaders.Head, definition)

    let ofFunction (shaderFunction : 'a -> Expr<'b>) =
        match Shader.Utils.tryExtractExpr shaderFunction with
        | Some (expr, types) ->
            let shader = expr |> Shader.ofExpr types |> List.head
            let definition = expr |> SourceDefinition.create types shaderFunction
            RaytracingShader(shader, definition)
        | _ ->
            failwithf "[FShade] cannot create raytracing shader using function: %A" shaderFunction

    let toEntryPoint (slot : ShaderSlot) (s : RaytracingShader) =
        let ofMap kind =
            Map.toList >> List.map (fun (name, (typ, location)) ->
                {
                    rtdataName = name
                    rtdataType = typ
                    rtdataKind = kind location
                }
            )

        let ofOption kind =
            Option.toList >> List.map (fun (name, typ) ->
                {
                    rtdataName = name
                    rtdataType = typ
                    rtdataKind = kind
                }
            )

        let raytracingData =
            let payloads       = s.Shader.shaderPayloads       |> ofMap    RaytracingDataKind.RayPayload
            let payloadIn      = s.Shader.shaderPayloadIn      |> ofOption RaytracingDataKind.RayPayloadIn
            let callableData   = s.Shader.shaderCallableData   |> ofMap    RaytracingDataKind.CallableData
            let callableDataIn = s.Shader.shaderCallableDataIn |> ofOption RaytracingDataKind.CallableDataIn
            let hitAttribute   = s.Shader.shaderHitAttribute   |> ofOption RaytracingDataKind.HitAttribute
            payloads @ payloadIn @ callableData @ callableDataIn @ hitAttribute

        let uniforms =
            s.Shader.shaderUniforms |> Map.toList |> List.map (fun (n, u) ->
                let uniformBuffer =
                    match u.uniformValue with
                        | Attribute(scope, name) -> Some scope.FullName
                        | _ -> None

                let textureInfos =
                    match u.uniformValue with
                        | UniformValue.Sampler (n,s) -> [n,s :> obj]
                        | UniformValue.SamplerArray arr -> Array.toList arr |> List.map (fun (n,s) -> n, s :> obj)
                        | _ -> []

                {
                    uniformName = u.uniformName
                    uniformType = u.uniformType
                    uniformBuffer = uniformBuffer
                    uniformDecorations = u.decorations
                    uniformTextureInfo = textureInfos
                }
            )

        {
            conditional    = Some slot.Conditional
            entryName      = "main"
            inputs         = List.empty
            outputs        = List.empty
            uniforms       = uniforms
            raytracingData = raytracingData
            arguments      = []
            body           = s.Shader.shaderBody
            decorations =
                [
                    yield EntryDecoration.Stages (ShaderStageDescription.Raytracing slot)
                    yield EntryDecoration.Invocations s.Shader.shaderInvocations
                ]
        }