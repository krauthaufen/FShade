namespace FShade

open System.IO
open Aardvark.Base
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Data.Adaptive
open FShade.Debug.ProjectInfo

module EffectDebugger =
    open System
    open System.Collections.Generic

    open System.Reflection
    open System.Reflection.PortableExecutable
    open System.Reflection.Metadata
    open System.Threading

    [<AutoOpen>]
    module private Helpers =
        let projectFileExtensions =
            Set.ofList [
                ".fsproj"
                ".csproj"
                ".vbproj"
            ]

        let assemblyRedirects = ConcurrentDict<string, string>(Dict())
        let projectAssemblies = ConcurrentDict<string, Assembly>(Dict())
        let checker = FSharpChecker.Create()
        let assemblyProjects = System.Collections.Concurrent.ConcurrentDictionary<string, option<ProjectInfo>>()
        let parsedFile = System.Collections.Concurrent.ConcurrentDictionary<string, System.Threading.Tasks.Task<FSharpParseFileResults>>()

        let rec tryFindProject (dir : string) =
            let projects = Directory.GetFiles(dir) |> Array.filter (fun f -> Set.contains (Path.GetExtension(f).ToLower()) projectFileExtensions)
            if projects.Length > 0 then
                Some projects.[0]
            else
                let parent = Path.GetDirectoryName dir
                if not (isNull parent) then
                    tryFindProject parent
                else
                    None

        let tryGetProjectLocation (assemblyPath : string) : option<string> =
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

        let tryGetMethod (assemblyPath : string) (sourceFile : string) (startLine : int) (startCol : int) =
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
                                                let dy = abs (p.StartLine - startLine)
                                                let dx = abs (p.StartColumn - startCol)

                                                if dx <= 3 && dy = 0 then
                                                    let def = dll.GetMethodDefinition(m.ToDefinitionHandle())
                                                    let name = dll.GetString def.Name
                                                    let parent = dll.GetTypeDefinition(def.GetDeclaringType())

                                                    let rec getFullName (t : TypeDefinition) =
                                                        let name = dll.GetString t.Name

                                                        if t.IsNested then
                                                            let p = dll.GetTypeDefinition(t.GetDeclaringType())
                                                            sprintf "%s+%s" (getFullName p) name
                                                        else
                                                            let ns = dll.GetString t.Namespace
                                                            if String.IsNullOrWhiteSpace ns then name
                                                            else sprintf "%s.%s" ns name

                                                    let tname = getFullName parent

                                                    Some(tname, name)

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

        let compile (info : ProjectInfo) =

            let references =
                info.references |> List.map (fun r ->
                    match assemblyRedirects.TryGetValue r with
                    | (true, n) ->
                        Log.line "override %s: %s" (Path.GetFileName r) n
                        n
                    | _ -> r
                )

            let ninfo = { info with references = references }

            let args = List.toArray ("fsc.exe" :: ProjectInfo.toFscArgs ninfo)
            checker.Compile(args)

        let tryGetProjectForAssembly (a : Assembly) =
            let location =
                try Path.GetFullPath a.Location |> Some
                with _ -> None

            match location with
            | Some location ->
                assemblyProjects.GetOrAdd(location, fun location ->
                    match tryGetProjectLocation location with
                    | Some file ->
                        match ProjectInfo.tryOfProject ["Configuration", "Debug"] file with
                        | Result.Ok info ->
                            Some info
                        | Result.Error err ->
                            None
                    | None ->
                        None
                )
            | None ->
                None

        let installWatcher (callback : string -> unit) (info : ProjectInfo) =
            let dir =
                Path.GetDirectoryName(info.project)

            let filesToWatch =
                info.files
                |> Seq.groupBy Path.GetDirectoryName
                |> Seq.map (fun (dir, files) -> dir, Set.ofSeq files)
                |> HashMap.ofSeq
                |> HashMap.alter dir (Option.defaultValue Set.empty >> Set.add info.project >> Some)

            let watchers = List<FileSystemWatcher>()

            for (dir, files) in filesToWatch do
                let w = new FileSystemWatcher(dir)
                w.Changed.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)
                w.Renamed.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)
                w.Created.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)
                w.EnableRaisingEvents <- true
                watchers.Add w

            { new IDisposable with
                member x.Dispose() =
                    for w in watchers do w.Dispose()
            }

        let start () =
            let coreLib = typeof<FShade.Effect>.Assembly.GetName().Name

            let allAssemblies =
                Introspection.AllAssemblies
                |> Seq.filter (fun a -> a.GetReferencedAssemblies() |> Array.exists (fun r -> r.Name = coreLib))
                |> Seq.toArray

            Log.startTimed "resolving referenced projects"

            let assemblyLocations = Dict<Assembly, string>()
            let projectForAssembly = Dict<string, ProjectInfo>()
            let projectForProj = Dict<string, ProjectInfo>()

            for a in allAssemblies do
                match tryGetProjectForAssembly a with
                | Some info ->
                    projectAssemblies.[info.project] <- a
                    match info.output with
                    | Some o ->
                        projectForAssembly.[o] <- info
                        assemblyLocations.[a] <- o
                    | None ->
                        projectForAssembly.[a.Location] <- info
                        assemblyLocations.[a] <- a.Location

                    projectForProj.[info.project] <- info
                    Log.line "found %s" (Path.GetFileName info.project)
                | None ->
                    ()

            Log.stop()

            let dependent, buildOrder =
                let mutable dependent = HashMap.empty
                let mutable dependencies = HashMap.empty
                for KeyValue(path, project) in projectForAssembly do
                    for r in project.projects do
                        match projectForProj.TryGetValue r with
                        | (true, otherProject) ->
                            dependent <-
                                dependent |> HashMap.alter otherProject.project (Option.defaultValue Set.empty >> Set.add project.project >> Some)

                            dependencies <-
                                dependencies |> HashMap.alter project.project (Option.defaultValue Set.empty >> Set.add otherProject.project >> Some)

                        | _ ->
                            ()
                let dependencies = dependencies
                let dependent = dependent

                let buildOrder =
                    let res = List<Set<string>>()

                    let mutable all = Set.ofSeq projectForProj.Keys
                    let mutable dependencies = dependencies
                    let nodependencies () =
                        all |> Set.filter (fun p -> not (HashMap.containsKey p dependencies))

                    while not (Set.isEmpty all) do
                        let current = nodependencies()
                        res.Add current
                        dependencies <-
                            dependencies |> HashMap.choose (fun _ d ->
                                let n = Set.difference d current
                                if Set.isEmpty n then None
                                else Some n
                            )
                        all <- Set.difference all current

                    Seq.toArray res

                dependent, buildOrder

            let getAffected(projs : #seq<string>) =
                let rec all (p : string) =
                    match HashMap.tryFind p dependent with
                    | Some d ->
                        d |> Seq.map all |> Set.unionMany |> Set.add p
                    | None ->
                        Set.singleton p

                let all = projs |> Seq.collect all |>  Set.ofSeq
                buildOrder |> Array.choose (fun s ->
                    let r = Set.intersect s all
                    if Set.isEmpty r then None
                    else Some r
                )

            let tryGetEffectProject (e : FShade.Effect) =
                match e.SourceDefintion with
                | Some (d, _) ->
                    match d.DebugRange with
                    | Some (srcFile, startLine, startCol, endLine, endCol) ->
                        projectForProj |> Seq.tryPick (fun (KeyValue(file, info)) ->
                            if List.exists (fun f -> f = srcFile) info.files then

                                match tryGetMethod info.output.Value srcFile startLine startCol with
                                | Some (typeName, methName) ->

                                    let resolve (ass : Assembly) =
                                        let typ = ass.GetType typeName
                                        if isNull typ then None
                                        else
                                            let m = typ.GetMethod(methName, BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
                                            if isNull m then None
                                            else Some m

                                    Some(file, resolve)


                                | None ->
                                    None
                            else
                                None
                        )
                    | None ->
                        None
                | None ->
                    None

            let leafCache = ConcurrentDict<string, cval<Effect> * (Assembly -> unit)>(Dict())
            let registered = ConcurrentDict<string, List<Assembly -> unit>>(Dict())

            let tryRegister (effect : FShade.Effect) =
                let rec getLeaves (e : FShade.Effect) =
                    match e.ComposedOf with
                    | [] -> [e]
                    | [e] -> getLeaves e
                    | many -> List.collect getLeaves many

                let leaves = getLeaves effect

                let result =
                    leaves |> List.map (fun effect ->
                        match tryGetEffectProject effect with
                        | Some(projFile, resolve) ->
                            let effect =
                                match resolve projectAssemblies.[projFile] with
                                | Some mi ->
                                    try
                                        let p = mi.GetParameters().[0].ParameterType
                                        let test = mi.Invoke(null, [| null |]) |> unbox<_>
                                        let effect = Effect.ofExpr p test
                                        effect
                                    with e ->
                                        Log.warn "failed: %A" e
                                        effect
                                | None ->
                                    effect


                            let cell, change =
                                leafCache.GetOrCreate(effect.Id, fun _ ->
                                    let c = cval effect
                                    let change (a : Assembly) =
                                        match resolve a with
                                        | Some mi ->
                                            try
                                                let p = mi.GetParameters().[0].ParameterType
                                                let test = mi.Invoke(null, [| null |]) |> unbox<_>
                                                let effect = Effect.ofExpr p test
                                                c.Value <- effect
                                            with e ->
                                                Log.warn "failed: %A" e
                                        | None ->
                                            Log.warn "effect %s missing" effect.Id

                                    c, change
                                )

                            lock registered (fun () ->
                                let l = registered.GetOrCreate(projFile, fun _ -> List())
                                l.Add change
                            )
                            cell :> aval<_>
                        | None ->
                            AVal.constant effect
                    )

                AVal.custom (fun t ->
                    try
                        result |> List.map (fun r -> r.GetValue t) |> Effect.compose
                    with _ ->
                        effect
                )


            let lockObj = obj()
            let mutable dirty = Set.empty
            let thread =
                let tick () =
                    lock lockObj (fun () ->
                        while Set.isEmpty dirty do
                            Monitor.Wait lockObj |> ignore
                    )
                    Thread.Sleep(500)
                    let dirty =
                        lock lockObj (fun () ->
                            let d = dirty
                            dirty <- Set.empty
                            d
                        )

                    if not (Set.isEmpty dirty) then
                        let affected = dirty |> getAffected
                        Log.startTimed "rebuild [%s]" (affected |> Seq.collect (Seq.map Path.GetFileName) |> String.concat ", ")
                        for par in affected do
                            for a in par do
                                match projectForProj.TryGetValue a with
                                | (true, info) ->
                                    let outFile =
                                        match info.output with
                                        | Some o -> o
                                        | _ -> failwithf "no output file for %A" info.project

                                    Log.startTimed "rebuild %s" (Path.GetFileName a)
                                    let tmp = Path.ChangeExtension(Path.GetTempFileName(), Path.GetExtension info.output.Value)
                                    let info = { info with output = Some tmp }
                                    let (errors, _ret) = compile info |> Async.RunSynchronously

                                    let critical = errors |> Array.filter (fun e -> e.Severity = FSharpDiagnosticSeverity.Error)

                                    if critical.Length = 0 && File.Exists tmp then
                                        Report.End(": success") |> ignore

                                        assemblyRedirects.[outFile] <- tmp
                                        match registered.TryGetValue info.project with
                                        | (true, reg) when reg.Count > 0 ->
                                            let ass = Assembly.LoadFile tmp
                                            transact (fun () ->
                                                for r in reg do r ass
                                            )
                                        | _ ->
                                            ()
                                    else
                                        for e in critical do
                                            Report.ErrorNoPrefix(sprintf "%A" e)
                                        Log.stop()
                                | _ ->
                                    ()

                        Log.stop()

                startThread <| fun () ->
                    while true do
                        tick()



            let callback  (project : ProjectInfo) (file : string) =
                Log.line "%s/%s changed" (Path.GetFileName project.project) (Path.GetFileName file)

                lock lockObj (fun () ->
                    dirty <- Set.add project.project dirty
                    Monitor.PulseAll lockObj
                )

            let subscriptions =
                projectForAssembly
                |> Seq.map (fun (KeyValue(a, p)) -> installWatcher (fun f -> callback p f) p)
                |> Seq.toArray

            tryRegister, subscriptions

    let attach() =
        match EffectDebugger.registerFun with
        | Some _ -> Disposable.empty
        | None ->
            let register, watchers = start()
            EffectDebugger.registerFun <- Some (fun e -> register e :> obj)
            EffectDebugger.isAttached <- true
            EffectDebugger.saveCode <- fun _ _ -> ()

            { new IDisposable with
                member x.Dispose() =
                    for w in watchers do w.Dispose()
                    EffectDebugger.registerFun <- None
                    EffectDebugger.isAttached <- false
            }