namespace FShade.Debug

open FShade
open Aardvark.Base
open FSharp.Data.Adaptive

open System
open System.Threading
open System.Collections.Generic
open System.Reflection

module ShaderDebugger =

    type private Project =
        {
            // Metadata about the project
            Data : ProjectData

            // Original assembly of the project
            Assembly : Assembly

            // Callbacks to be invoked when the project is rebuilt
            Callbacks : List<Assembly -> bool>
        }

    type private SourceLocation =
        { File   : string
          Line   : int
          Column : int }

        override x.ToString() =
            $"{x.File} ({x.Line},{x.Column})"

    type private ShaderDefinition =
        {
            // The project in which the shader is defined
            Project : string

            // The name of the declaring type
            TypeName : string

            // The method of the shader
            MethodName : string

            // Location of the original shader
            Location : SourceLocation

            // Input types of the shader function
            // Note: Compute shaders may have multiple inputs.
            Inputs : Type list

            // Arguments of the shader function
            Arguments : obj list
        }

        member x.InputCount =
            // If there is a single unit input, the respective function will have no parameters -> treat as zero inputs
            if x.Inputs = [typeof<unit>] then 0
            else x.Inputs.Length

        override x.ToString() =
            $"{x.TypeName}.{x.MethodName}"

    // Contains all found projects stored with project file path as key.
    // Only projects that reference FShade are regarded.
    let private projects = Dictionary<string, Project>()


    module private ShaderDefinition =
        open FSharp.Quotations

        let tryResolve (assembly : Assembly) (definition : ShaderDefinition) =
            match assembly |> Assembly.tryGetMethod definition.TypeName definition.MethodName with
            | Some mi ->
                let parameters = mi.GetParameters()
                let argumentCount = parameters.Length - definition.InputCount

                if not <| typeof<Expr>.IsAssignableFrom mi.ReturnType then
                    Result.Error $"Shader {definition} has an invalid return type of {mi.ReturnType}."

                elif argumentCount < 0 then
                    Result.Error $"Shader {definition} has {parameters.Length} parameters but {definition.Inputs} inputs are expected."

                elif argumentCount > definition.Arguments.Length then
                    Result.Error $"Shader {definition} expects {argumentCount} arguments but only {definition.Arguments.Length} are known."

                else
                    let inputTypes =
                        parameters
                        |> Array.skip argumentCount
                        |> Array.map (fun p -> p.ParameterType)
                        |> List.ofArray

                    let arguments =
                        let args = definition.Arguments |> List.take argumentCount
                        let inputs = List.replicate inputTypes.Length null
                        args @ inputs |> Array.ofList

                    try
                        let expr = mi.Invoke(null, arguments) |> unbox<Expr>
                        Result.Ok (expr, inputTypes, mi)

                    with exn ->
                        Result.Error $"Failed to resolve shader {definition}: {exn.Message}"
            | _ ->
                Result.Error $"Failed to resolve shader {definition}."

        let tryResolveEffect (assembly : Assembly) (definition : ShaderDefinition) =
            definition
            |> tryResolve assembly
            |> Result.map (fun (expr, inputTypes, _) ->
                expr |> Effect.ofExpr inputTypes.Head
            )

        let tryResolveRaytracingShader (assembly : Assembly) (definition : ShaderDefinition) =
            definition
            |> tryResolve assembly
            |> Result.map (fun (expr, inputTypes, _) ->
                expr |> RaytracingShader.ofExpr inputTypes
            )

        let tryResolveComputeShader (assembly : Assembly) (definition : ShaderDefinition) =
            definition
            |> tryResolve assembly
            |> Result.bind (fun (expr, _, mi) ->
                match Seq.tryHead <| mi.GetCustomAttributes<LocalSizeAttribute>() with
                | Some att ->
                    let localSize = V3i(att.X, att.Y, att.Z)
                    Result.Ok <| ComputeShader.ofExpr localSize expr

                | _ ->
                    Result.Error $"Could not determine local size for {definition}."
            )

        let ofSourceDefinition =
            let cache = Dictionary<string, ShaderDefinition>()

            fun (id : string) (definition : SourceDefinition) ->
                match cache.TryGetValue id with
                | (true, def) -> Result.Ok def
                | _ ->
                    match definition.Expression.DebugRange with
                    | Some (srcFile, startLine, startCol, _endLine, _endCol) ->
                        let location = { File = srcFile; Line = startLine; Column = startCol }

                        let result =
                            projects.Values |> Seq.tryPick (fun p ->
                                if p.Data.Files |> List.contains srcFile then
                                    match p.Assembly |> Assembly.tryGetMethodName srcFile startLine startCol with
                                    | Some (typeName, methodName) ->
                                        Some {
                                            Project = p.Data.Path
                                            TypeName = typeName
                                            MethodName = methodName
                                            Location = location
                                            Inputs = definition.Inputs
                                            Arguments = definition.Arguments
                                        }

                                    | None ->
                                        None
                                else
                                    None
                            )

                        match result with
                        | Some definition ->
                            cache.[id] <- definition
                            Result.Ok definition
                        | _ ->
                            Result.Error $"Cannot find shader definition for {location}."

                    | None ->
                        Result.Error $"Shader {id} is missing debug information."

        let ofEffect (effect : Effect) =
            match effect.SourceDefinition with
            | Some def -> def |> ofSourceDefinition effect.Id
            | _ -> Result.Error $"Effect {id} is missing a source definition."

        let ofRaytracingShader (shader : RaytracingShader) =
            match shader.SourceDefinition with
            | Some def -> def |> ofSourceDefinition shader.Id
            | _ -> Result.Error $"Raytracing shader {id} is missing a source definition."

        let ofComputeShader (shader : ComputeShader) =
            shader.csSourceDefinition |> ofSourceDefinition shader.csId


    let private tryRegister<'Effect, 'Shader, 'Slot when 'Slot : comparison>
                            (description : string)
                            (getEffectId : 'Effect -> string)
                            (getShaderId : 'Shader -> string)
                            (getShaders : 'Effect -> Map<'Slot,'Shader>)
                            (getDefinition : 'Shader -> Result<ShaderDefinition, string>)
                            (tryResolveDefinition : Assembly -> ShaderDefinition -> Result<'Shader, string>)
                            (compose : Map<'Slot, 'Shader> -> 'Effect) =
        let shaderCache = Dictionary<string, ShaderDefinition * aval<'Shader>>()

        let tryHook (effect : 'Effect) =
            let mutable success = false

            Log.startTimed $"Registering {description} for debugging"

            let shaders =
                getShaders effect
                |> Map.map (fun _ shader ->
                    let id = getShaderId shader

                    match shaderCache.TryGetValue id with
                    | (true, (definition, effect)) ->
                        Report.Line(2, $"{definition} - {definition.Location}")
                        success <- true
                        string definition, effect

                    | _ ->
                        match getDefinition shader with
                        | Result.Ok definition ->
                            let assembly = projects.[definition.Project].Assembly

                            match definition |> tryResolveDefinition assembly with
                            | Result.Ok _ ->
                                let cval = cval shader

                                let update (assembly : Assembly) =
                                    match definition |> tryResolveDefinition assembly with
                                    | Result.Ok shader ->
                                        cval.Value <- shader
                                        true

                                    | Result.Error error ->
                                        Report.ErrorNoPrefix error
                                        false

                                Report.Line(2, $"{definition} - {definition.Location}")

                                success <- true
                                let project = projects.[definition.Project]
                                project.Callbacks.Add update
                                FileWatchers.activate project.Data
                                shaderCache.[id] <- (definition, cval)
                                string definition, cval

                            // Failed to resolve definition in original assembly, use shader as-is with definition as name
                            | Result.Error error ->
                                Report.Warn $"{error} ({definition.Location})"
                                string definition, AVal.constant shader

                        // Failed to get definition, just use shader as-is with id as name
                        | Result.Error error ->
                            Report.Warn error
                            id, AVal.constant shader
                )

            if success then
                Log.stop(": success")

                let names =
                    Map.values shaders
                    |> Seq.map fst
                    |> String.concat "; "

                Some <| AVal.custom (fun t ->
                    Report.Line(2, $"Updating [{names}].")

                    try
                        shaders |> Map.map (fun _ (_, s) -> s.GetValue t) |> compose
                    with exn ->
                        Log.error "%s" exn.Message
                        effect
                )
            else
                Log.stop(": failed")
                None

        // Cache results for overall effects as well
        let effectCache = Dictionary<string, aval<'Effect> option>()

        fun (effect : 'Effect) ->
            let id = getEffectId effect

            match effectCache.TryGetValue id with
            | (true, result) -> result
            | _ ->
                let result = tryHook effect
                effectCache.[id] <- result
                result


    // Tries to register an effect to be updated automatically when the corresponding project changes.
    // Note: Only the individual shaders may change, the composition is fixed.
    let private tryRegisterEffect : Effect -> aval<Effect> option =
        let rec getLeaves (e : FShade.Effect) =
            match e.ComposedOf with
            | [] -> [e]
            | [e] -> getLeaves e
            | many -> List.collect getLeaves many

        tryRegister<Effect, Effect, int>
                    "effect"
                    Effect.id
                    Effect.id
                    (getLeaves >> List.indexed >> Map.ofList) // Use indices as map keys to retain order
                    ShaderDefinition.ofEffect
                    ShaderDefinition.tryResolveEffect
                    (Map.values >> Effect.compose)

    let private tryRegisterRaytracingEffect : RaytracingEffect -> aval<RaytracingEffect> option =
        tryRegister<RaytracingEffect, RaytracingShader, ShaderSlot>
                    "raytracing effect"
                    (fun effect -> effect.Id)
                    (fun shader -> shader.Id)
                    (fun effect -> effect.ShadersWithStubs)
                    ShaderDefinition.ofRaytracingShader
                    ShaderDefinition.tryResolveRaytracingShader
                    RaytracingEffect.ofShaders

    let private tryRegisterComputeShader : ComputeShader -> aval<ComputeShader> option =
        tryRegister<ComputeShader, ComputeShader, int>
                    "compute shader"
                    (fun shader -> shader.csId)
                    (fun shader -> shader.csId)
                    (fun shader -> Map.ofList [0, shader])
                    ShaderDefinition.ofComputeShader
                    ShaderDefinition.tryResolveComputeShader
                    (Map.values >> Seq.head)

    let attach() =
        ShaderDebugger.initialize (fun _ ->
            Log.startTimed "Resolving projects for shader debugger"

            let coreLib = typeof<FShade.Effect>.Assembly.GetName().Name

            for assembly in Introspection.AllAssemblies do
                let refs = assembly.GetReferencedAssemblies()

                if refs |> Array.exists (fun r -> r.Name = coreLib) then
                    match Assembly.tryGetProjectData assembly with
                    | Some data ->
                        projects.[data.Path] <- { Data = data; Assembly = assembly; Callbacks = List() }
                        Log.line "Found %s" data.Name

                    | None ->
                        ()

            Log.stop()

            // Spin up thread that recompiles all projects marked as dirty
            let dirty = HashSet<string>()

            let worker =
                if projects.Count > 0 then
                    let mutable running = true

                    let recompile() =
                        let pending =
                            lock dirty (fun () ->
                                let d = dirty.ToArray(dirty.Count)
                                dirty.Clear()
                                d
                            )

                        for projectFile in pending do
                            let project = projects.[projectFile]

                            Log.startTimed "Recompiling shaders of %s" project.Data.Name

                            try
                                let result = Compiler.build project.Data

                                match result with
                                | Result.Ok output ->
                                    let assembly = Assembly.LoadFile output

                                    let success =
                                        transact (fun () ->
                                            let mutable success = true

                                            for update in project.Callbacks do
                                                let ret = update assembly
                                                success <- success && ret

                                            success
                                        )

                                    if success then
                                        Log.stop(": success")
                                    else
                                        Log.stop(": failed")

                                | Result.Error error ->
                                    Report.ErrorNoPrefix error
                                    Log.stop(": failed")

                            with exn ->
                                Report.ErrorNoPrefix(string exn)
                                Log.stop(": failed")

                    let run() =
                        while running do
                            lock dirty (fun () ->
                                while running && dirty.Count = 0 do
                                    Monitor.Wait dirty |> ignore
                            )

                            if running then
                                Thread.Sleep(500)
                                recompile()

                    let thread = startThread run

                    { new IDisposable with
                        member x.Dispose() =
                            running <- false
                            lock dirty (fun _ -> Monitor.PulseAll dirty)
                            thread.Join()
                    }
                else
                    Disposable.empty

            // Install file watchers
            for p in projects.Values do
                p.Data |> FileWatchers.install (fun _ ->
                    lock dirty (fun () ->
                        if p.Callbacks.Count > 0 && dirty.Add p.Data.Path then
                            Monitor.PulseAll dirty
                    )
                )

            { new ShaderDebugger.IShaderDebugger with
                member x.TryRegisterEffect(effect) = tryRegisterEffect effect
                member x.TryRegisterRaytracingEffect(effect) = tryRegisterRaytracingEffect effect
                member x.TryRegisterComputeShader(shader) = tryRegisterComputeShader shader
                member x.Dispose() =
                    FileWatchers.dispose()
                    worker.Dispose()
            }
        )