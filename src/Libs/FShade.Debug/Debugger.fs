namespace FShade.Debug

open FShade
open Aardvark.Base
open FSharp.Data.Adaptive
open ProjectInfo

open System
open System.IO
open System.Threading
open System.Collections.Generic
open System.Reflection

module ShaderDebugger =

    type private Project =
        {
            // Info about the project
            Info : ProjectInfo

            // Callbacks to be invoked when the project is rebuilt
            Callbacks : List<Assembly -> unit>
        }

    type private EffectDefinition =
        {
            // The project in which the effect is defined
            Project : string

            // The name of the declaring type
            TypeName : string

            // The method of the effect
            MethodName : string
        }

    // Contains all found projects stored with project file path as key.
    // Only projects that reference FShade are regarded.
    let private projects = Dictionary<string, Project>()

    // Utilities for probing assemblies
    module private Assembly =
        open System.Reflection.Metadata
        open System.Reflection.PortableExecutable

        [<AutoOpen>]
        module private ProjectHelpers =

            let rec private tryFindProject (dir : string) =
                let project =
                    Directory.GetFiles dir
                    |> Array.tryFind (fun f ->
                        Path.GetExtension(f).ToLower() = ".fsproj"
                    )

                if project.IsNone then
                    let parent = Path.GetDirectoryName dir
                    if not (isNull parent) then
                        tryFindProject parent
                    else
                        None
                else
                    project

            let tryFindProjectLocation (assemblyPath : string) : option<string> =
                let pdbPath = Path.ChangeExtension(assemblyPath, ".pdb")

                if File.Exists pdbPath then
                    try
                        use file = File.OpenRead pdbPath
                        use provider = MetadataReaderProvider.FromPortablePdbStream(file, MetadataStreamOptions.PrefetchMetadata)
                        let reader = provider.GetMetadataReader()

                        reader.Documents |> Seq.tryPick (fun d ->
                            if d.IsNil then None
                            else
                                try
                                    let doc = reader.GetDocument d

                                    if doc.Name.IsNil then None
                                    else
                                        let name = reader.GetString doc.Name

                                        if File.Exists name then
                                            tryFindProject (Path.GetDirectoryName name)
                                        else
                                            None
                                with _ ->
                                    None
                        )
                    with _ ->
                        None
                else
                    None

        let tryGetProjectInfo =
            let cache = Dictionary<string, ProjectInfo option>()

            fun (assembly : Assembly) ->
                let location =
                    try Path.GetFullPath assembly.Location |> Some
                    with _ -> None

                match location with
                | Some location ->
                    match cache.TryGetValue location with
                    | (true, info) -> info
                    | _ ->
                        match tryFindProjectLocation location with
                        | Some projectFile ->
                            let result =
                                match projectFile |> ProjectInfo.tryOfProject ["Configuration", "Debug"] with
                                | Result.Ok info -> Some info
                                | Result.Error errors ->
                                    for err in errors do Log.warn "%s: %s" projectFile err
                                    None

                            cache.[location] <- result
                            result

                        | None ->
                            None
                | None ->
                    None

        let tryGetMethodName (sourceFile : string) (startLine : int) (startColumn : int) (assemblyPath : string) =
            let pdbPath = Path.ChangeExtension(assemblyPath, ".pdb")

            if File.Exists pdbPath then
                try
                    use stream = File.OpenRead pdbPath
                    use provider = MetadataReaderProvider.FromPortablePdbStream(stream, MetadataStreamOptions.PrefetchMetadata)
                    let pdb = provider.GetMetadataReader()

                    use rd = new PEReader(File.OpenRead assemblyPath, PEStreamOptions.PrefetchMetadata)
                    let dll =
                        let m = rd.GetMetadata()
                        MetadataReader(m.Pointer, m.Length)

                    let rec getFullName (t : TypeDefinition) =
                          let name = dll.GetString t.Name

                          if t.IsNested then
                              let p = dll.GetTypeDefinition(t.GetDeclaringType())
                              sprintf "%s+%s" (getFullName p) name
                          else
                              let ns = dll.GetString t.Namespace
                              if String.IsNullOrWhiteSpace ns then name
                              else sprintf "%s.%s" ns name

                    pdb.MethodDebugInformation |> Seq.tryPick (fun m ->
                        try
                            if m.IsNil then
                                None
                            else
                                let info = pdb.GetMethodDebugInformation(m)

                                if info.Document.IsNil then
                                    None
                                else
                                    let doc = pdb.GetDocument(info.Document)

                                    if doc.Name.IsNil then
                                        None
                                    else
                                        if pdb.GetString doc.Name = sourceFile && not info.SequencePointsBlob.IsNil then
                                            info.GetSequencePoints() |> Seq.tryPick (fun p ->
                                                let dy = p.StartLine - startLine
                                                let dx = p.StartColumn - startColumn

                                                if dy = 0 && abs dx <= 3 then
                                                    let def = dll.GetMethodDefinition(m.ToDefinitionHandle())
                                                    let name = dll.GetString def.Name
                                                    let parent = dll.GetTypeDefinition(def.GetDeclaringType())
                                                    Some(getFullName parent, name)
                                                else
                                                    None
                                            )
                                        else
                                            None
                        with _ ->
                            None
                    )
                with _ ->
                    None
            else
                None

        let tryGetMethod (typeName : string) (methodName : string) (assembly : Assembly) =
            let typ = assembly.GetType typeName
            if isNull typ then None
            else
                let mi = typ.GetMethod(methodName, BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
                if isNull mi then None
                else Some mi

    module private Compiler =
        open FSharp.Compiler.CodeAnalysis
        open FSharp.Compiler.Diagnostics

        let private checker = FSharpChecker.Create()

        let compile (info : ProjectInfo) =
            let args = List.toArray ("fsc.exe" :: ProjectInfo.toFscArgs info)
            let output, _ = checker.Compile args |> Async.RunSynchronously

            output |> Array.choose (fun e ->
                if e.Severity = FSharpDiagnosticSeverity.Error then
                    Some <| sprintf "%A" e
                else
                    None
            )

    module private FileWatchers =

        let private watchers = Dictionary<string, FileSystemWatcher>()

        let install (callback : string -> unit) (info : ProjectInfo) =
            let filesToWatch =
                info.files
                |> List.groupBy Path.GetDirectoryName
                |> List.map (fun (dir, files) -> dir, Set.ofList files)
                |> HashMap.ofList

            for (dir, files) in filesToWatch do
                let watcher =
                    match watchers.TryGetValue dir with
                    | (true, w) -> w
                    | _ ->
                        let w = new FileSystemWatcher(dir)
                        w.EnableRaisingEvents <- true
                        watchers.[dir] <- w
                        w

                watcher.Changed.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)
                watcher.Renamed.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)
                watcher.Created.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)

        let dispose() =
            for w in watchers.Values do w.Dispose()
            watchers.Clear()

    module private EffectDefinition =
        open FSharp.Quotations

        let tryResolve (assembly : Assembly) (definition : EffectDefinition) =
            match assembly |> Assembly.tryGetMethod definition.TypeName definition.MethodName with
            | Some mi ->
                let parameters = mi.GetParameters()

                if not <| typeof<Expr>.IsAssignableFrom mi.ReturnType then
                    Result.Error $"method has an invalid return type of {mi.ReturnType}"

                elif parameters.Length <> 1 then
                    Result.Error $"effect must have a single parameter (but has {parameters.Length})"

                else
                    let paramType = parameters.[0].ParameterType

                    try
                        let expr = mi.Invoke(null, [| null |]) |> unbox<Expr>
                        let effect = expr |> Effect.ofExpr paramType
                        Result.Ok effect

                    with exn ->
                        Result.Error (string exn)
            | _ ->
                Result.Error $"failed to resolve effect method {definition.TypeName}.{definition.MethodName}"

        let ofEffect =
            let cache = Dictionary<string, EffectDefinition>()

            fun (effect : FShade.Effect) ->
                match cache.TryGetValue effect.Id with
                | (true, def) -> Some def
                | _ ->
                    let debugRange =
                        match effect.SourceDefintion with
                        | Some (expr, _) -> expr.DebugRange
                        | _ -> None

                    match debugRange with
                    | Some (srcFile, startLine, startCol, _endLine, _endCol) ->
                        projects.Values |> Seq.tryPick (fun p ->
                            if p.Info.files |> List.contains srcFile then
                                match p.Info.output.Value |> Assembly.tryGetMethodName srcFile startLine startCol with
                                | Some (typeName, methodName) ->
                                    Some {
                                        Project = p.Info.project
                                        TypeName = typeName
                                        MethodName = methodName
                                    }

                                | None ->
                                    None
                            else
                                None
                        )
                    | None ->
                        None

    // Tries to register an effect to be updated automatically when the corresponding project changes.
    // Note: Only the individual shaders may change, the composition is fixed.
    let private tryRegisterEffect =
        let cache = Dictionary<string, cval<Effect>>()

        let rec getLeaves (e : FShade.Effect) =
            match e.ComposedOf with
            | [] -> [e]
            | [e] -> getLeaves e
            | many -> List.collect getLeaves many

        fun (effect : FShade.Effect) ->
            let mutable success = true

            let leaves =
                getLeaves effect
                |> List.choose (fun effect ->
                    let id = effect.Id

                    match cache.TryGetValue id with
                    | (true, effect) -> Some effect
                    | _ ->
                        match EffectDefinition.ofEffect effect with
                        | Some def ->
                            let cval = cval effect

                            let update (assembly : Assembly) =
                                match def |> EffectDefinition.tryResolve assembly with
                                | Result.Ok effect ->
                                    cval.Value <- effect

                                | Result.Error error ->
                                    Report.ErrorNoPrefix($"{error} (effect {id})")

                            projects.[def.Project].Callbacks.Add update
                            cache.[id] <- cval
                            Some cval

                        | None ->
                            success <- false
                            None
                )

            if success then
                Some <| AVal.custom (fun t ->
                    try
                        leaves |> List.map (fun r -> r.GetValue t) |> Effect.compose
                    with exn ->
                        Log.error "[ShaderDebugger] failed to compose effect %s: %A" effect.Id exn
                        effect
                )
            else
                Log.warn "[ShaderDebugger] failed to register effect %s" effect.Id
                None

    let attach() =
        ShaderDebugSystem.initialize (fun _ ->
            Log.startTimed "resolving projects for shader debugger"

            let coreLib = typeof<FShade.Effect>.Assembly.GetName().Name

            for assembly in Introspection.AllAssemblies do
                let refs = assembly.GetReferencedAssemblies()

                if refs |> Array.exists (fun r -> r.Name = coreLib) then
                    match Assembly.tryGetProjectInfo assembly with
                    | Some info when info.output.IsSome ->
                        projects.[info.project] <- { Info = info; Callbacks = List() }
                        Log.line "found %s" (Path.GetFileName info.project)

                    | Some info ->
                        Report.WarnNoPrefix($"{info.project}: could not determine output file")

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
                            let projectName = Path.GetFileName projectFile

                            Log.startTimed "[ShaderDebugger] rebuild %s" projectName

                            let tmp = Path.ChangeExtension(Path.GetTempFileName(), Path.GetExtension project.Info.output.Value)
                            let errors = Compiler.compile { project.Info with output = Some tmp }

                            if errors.Length = 0 && File.Exists tmp then
                                let assembly = Assembly.LoadFile tmp

                                try
                                    transact (fun () ->
                                        for update in project.Callbacks do
                                            update assembly
                                    )
                                with exn ->
                                    Report.ErrorNoPrefix(string exn)
                            else
                                for e in errors do
                                    Report.ErrorNoPrefix e

                            Log.stop()

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
                p.Info |> FileWatchers.install (fun _ ->
                    lock dirty (fun () ->
                        if p.Callbacks.Count > 0 && dirty.Add p.Info.project then
                            Monitor.PulseAll dirty
                    )
                )

            { new ShaderDebugSystem.IShaderDebugger with
                member x.TryRegisterEffect(effect) = tryRegisterEffect effect
                member x.Dispose() =
                    FileWatchers.dispose()
                    worker.Dispose()
            }
        )