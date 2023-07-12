namespace FShade

open System
open FSharp.Quotations
open FShade.Imperative

type RaytracingShader internal (id : string, shader : Shader, ?definition : SourceDefinition) =
    do assert (ShaderStage.isRaytracing shader.shaderStage)

    member x.Id = id
    member x.Shader = shader
    member x.SourceDefinition = definition

    member inline x.Body =
        x.Shader.shaderBody

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
    open System.Collections.Concurrent

    let private cache = ConcurrentDictionary<string, RaytracingShader>()

    let ofShader (shader : Shader) =
        Serializer.Init()
        let hash = Expr.ComputeHash shader.shaderBody

        cache.GetOrAdd(hash, fun hash ->
            RaytracingShader(hash, shader)
        )

    let ofExpr (inputTypes : Type list) (expr : Expr) =
        Serializer.Init()
        let hash = Expr.ComputeHash expr

        cache.GetOrAdd(hash, fun hash ->
            let shaders = expr |> Shader.ofExpr inputTypes
            let definition = expr |> SourceDefinition.ofExpr inputTypes
            RaytracingShader(hash, shaders.Head, definition)
        )

    let ofFunction (shaderFunction : 'a -> Expr<'b>) =
        match Shader.Utils.tryExtractExpr shaderFunction with
        | Some (expr, types) ->
            Serializer.Init()
            let hash = Expr.ComputeHash expr

            cache.GetOrAdd(hash, fun hash ->
                let shader = expr |> Shader.ofExpr types |> List.head
                let definition = expr |> SourceDefinition.create types shaderFunction
                RaytracingShader(hash, shader, definition)
            )
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