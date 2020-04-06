module internal FShade.Debug.ProjectInfo


open System.IO
open Aardvark.Base
open FSharp.Compiler.SourceCodeServices
open Dotnet.ProjInfo
open Dotnet.ProjInfo.Inspect
open Dotnet.ProjInfo.Workspace
open FSharp.Data.Adaptive


[<RequireQualifiedAccess>]
type Target =   
    | Exe
    | Library
    | WinExe
    | Module

[<RequireQualifiedAccess>]
type DebugType =
    | Off
    | Full
    | PdbOnly
    | Portable

type ProjectInfo =
    {
        project     : string
        isNewStyle  : bool
        references  : list<string>
        projects    : list<string>
        files       : list<string>
        defines     : list<string>
        target      : Target
        output      : Option<string>
        additional  : list<string>
        debug       : DebugType
    }


module ProjectInfo =

    let ofFscArgs (isNewStyle : bool) (path : string) (projects : list<string>) (args : list<string>) =
        let mutable parsed = Set.empty
        let path = Path.GetFullPath path
        let dir = Path.GetDirectoryName path

        let full (str : string) =
            if Path.IsPathRooted str then str
            else Path.Combine(dir, str)


        let removeArg (a : string) = parsed <- Set.add a parsed

        let references = 
            args |> List.choose (fun a ->
                if a.StartsWith "-r:" then removeArg a; Some (full (a.Substring 3))
                elif a.StartsWith "--reference:" then removeArg a; Some (full (a.Substring 12))
                else None
            )

        let files =
            args |> List.choose (fun a -> 
                if not (a.StartsWith "-") then
                    let isAssemblyInfo = (Path.GetFileName(a).ToLower().EndsWith "assemblyinfo.fs")
                    removeArg a
                    if not isAssemblyInfo then
                        Some (full a)
                    else
                        None
                else
                    None
            ) 

        let output =
            args |> List.tryPick (fun a ->
                if a.StartsWith "-o:" then removeArg a; Some (full (a.Substring 3))
                elif a.StartsWith "--out:" then removeArg a; Some (full (a.Substring 6))
                else None
            )

        let target =
            args |> List.tryPick (fun a ->
                if a.StartsWith "--target:" then
                    removeArg a
                    let target = a.Substring(9).Trim().ToLower()
                    match target with
                    | "exe" -> Some Target.Exe
                    | "library" -> Some Target.Library
                    | "winexe" -> Some Target.WinExe
                    | "module" -> Some Target.Module
                    | _ -> None
                else
                    None
            )

        let defines =
            args |> List.choose (fun a ->
                if a.StartsWith "-d:" then removeArg a; Some (a.Substring 3)
                elif a.StartsWith "--define:" then removeArg a; Some (a.Substring 9)
                else None
            )

        let hasDebug =
            args |> List.tryPick (fun a ->
                let rest = 
                    if a.StartsWith "-g" then Some (a.Substring(2).Replace(" ", ""))
                    elif a.StartsWith "--debug" then Some (a.Substring(7).Replace(" ", ""))
                    else None
                        
                match rest with
                | Some "" | Some "+" -> removeArg a; Some true
                | Some "-" -> removeArg a; Some false
                | _ -> None
            )

        let debugType =
            args |> List.tryPick (fun a ->
                let rest = 
                    if a.StartsWith "-g" then Some (a.Substring(2).Replace(" ", ""))
                    elif a.StartsWith "--debug" then Some (a.Substring(7).Replace(" ", ""))
                    else None
                        
                match rest with
                | Some ":full" -> removeArg a; Some DebugType.Full
                | Some ":pdbonly" -> removeArg a; Some DebugType.PdbOnly
                | Some ":portable" -> removeArg a; Some DebugType.Portable
                | _ -> None
            )

        let additional =
            args |> List.filter (fun a -> not (Set.contains a parsed))

        let debug =
            match hasDebug with
            | Some true -> defaultArg debugType DebugType.Full
            | Some false -> DebugType.Off
            | None -> defaultArg debugType DebugType.Full 

        {
            isNewStyle  = isNewStyle
            project     = path
            projects    = projects
            //fscArgs     = args
            references  = references
            files       = files
            target      = defaultArg target Target.Library
            defines     = defines
            additional  = additional
            output      = output
            debug       = debug
        }

    let toFscArgs (info : ProjectInfo) =
        [
            match info.output with
            | Some o -> yield sprintf "-o:%s" o
            | None -> ()

            match info.debug with
            | DebugType.Off ->
                ()
            | DebugType.Full -> 
                yield "-g"
                yield "--debug:full"
            | DebugType.Portable -> 
                yield "-g"
                yield "--debug:portable"
            | DebugType.PdbOnly -> 
                yield "-g"
                yield "--debug:pdbonly"
                
            match info.target with
            | Target.Exe -> yield "--target:exe"
            | Target.Library -> yield "--target:library"
            | Target.WinExe -> yield "--target:winexe"
            | Target.Module -> yield "--target:module"

            for d in info.defines do
                yield sprintf "-d:%s" d

            yield "--nowin32manifest"
            for a in info.additional do
                yield a
                
            for r in info.references do
                yield sprintf "-r:%s" r

            for f in info.files do
                yield f

        ]

    let normalize (info : ProjectInfo) =
        let path = Path.GetFullPath info.project
        let dir = Path.GetDirectoryName path

        let full (str : string) =
            if Path.IsPathRooted str then str
            else Path.Combine(dir, str)

        { info with
            project = path
            files = info.files |> List.map full
            references = info.references |> List.map full
            output = info.output |> Option.map full
        }

    let computeHash (info : ProjectInfo) =
        use ms = new MemoryStream()
        use w = new StreamWriter(ms)
        let info = normalize info

        let projDir = Path.GetDirectoryName info.project
        let relativePath (name : string) =
            let dirFull = Path.GetFullPath projDir
            let nameFull = Path.GetFullPath name
            if nameFull.StartsWith dirFull then
                nameFull.Substring(dirFull.Length).TrimStart [| System.IO.Path.DirectorySeparatorChar; System.IO.Path.AltDirectorySeparatorChar |]
            else
                name

        w.WriteLine(Path.GetFileName info.project)
        w.WriteLine(
            if info.isNewStyle then "newstyle"
            else "oldstyle"
        )
        w.WriteLine "references"
        for r in List.sort info.references do 
            let fileInfo = FileInfo r
            if fileInfo.Exists then
                w.WriteLine(fileInfo.LastWriteTimeUtc.ToString("o"))
            w.WriteLine(relativePath r)

        w.WriteLine "files"
        for f in info.files do 
            w.WriteLine(relativePath f)

        w.WriteLine "defines"
        for f in List.sort info.defines do w.WriteLine f
        w.WriteLine(
            match info.target with
            | Target.Exe -> "exe"
            | Target.WinExe -> "winexe"
            | Target.Library -> "library"
            | Target.Module -> "module"
        )
        w.WriteLine "additional"
        for a in List.sort info.additional do w.WriteLine a
        w.WriteLine(
            match info.debug with
            | DebugType.Full -> "full"
            | DebugType.Off -> "off"
            | DebugType.PdbOnly -> "pdbonly"
            | DebugType.Portable -> "portable"
        )

        let data = ms.ToArray()
        let md5 = System.Security.Cryptography.MD5.Create()
        let hash = md5.ComputeHash(data)
        System.Guid hash |> string

    [<AutoOpen>]
    module private PickleHelpers = 
        let pickleTarget (t : Target) =
            match t with
            | Target.Exe -> 0
            | Target.Library -> 1
            | Target.Module -> 2
            | Target.WinExe -> 3
        let pickleDebugType (t : DebugType) =
            match t with
            | DebugType.Off -> 0
            | DebugType.PdbOnly -> 1
            | DebugType.Portable -> 2
            | DebugType.Full -> 3

        let unpickleTarget (v : int) =
            match v with
            | 0 -> Target.Exe
            | 2 -> Target.Module
            | 3 -> Target.WinExe
            | _ -> Target.Library

        let unpickleDebugType (v : int) =
            match v with
            | 1 -> DebugType.PdbOnly
            | 2 -> DebugType.Portable
            | 3 -> DebugType.Full
            | _ -> DebugType.Off




    let tryUnpickleOf (stream : Stream) =
        let p = stream.Position
        try
            use r = new BinaryReader(stream, System.Text.Encoding.UTF8, true)
        
            let project = r.ReadString()
            let isNewStyle = r.ReadBoolean()

            let c = r.ReadInt32()
            let projects = List.init c (fun _ -> r.ReadString())

            let c = r.ReadInt32()
            let references = List.init c (fun _ -> r.ReadString())
        
            let c = r.ReadInt32()
            let files = List.init c (fun _ -> r.ReadString())
        
            let c = r.ReadInt32()
            let defines = List.init c (fun _ -> r.ReadString())

            let target = r.ReadInt32() |> unpickleTarget

            let has = r.ReadBoolean()
            let output = if has then r.ReadString() |> Some else None
        
            let c = r.ReadInt32()
            let additional = List.init c (fun _ -> r.ReadString())

            let debug = r.ReadInt32() |> unpickleDebugType

            let project = 
                normalize {
                    isNewStyle  = isNewStyle
                    project     = project
                    projects    = projects
                    references  = references
                    files       = files
                    target      = target
                    defines     = defines
                    additional  = additional
                    output      = output
                    debug       = debug
                }
            Some project
        with _ ->
            stream.Position <- p
            None

    let pickleTo (stream : Stream) (info : ProjectInfo) =
        use w = new BinaryWriter(stream, System.Text.Encoding.UTF8, true)

        w.Write info.project
        w.Write info.isNewStyle

        w.Write (List.length info.projects)
        for p in info.projects do w.Write p

        w.Write (List.length info.references)
        for r in info.references do w.Write r

        w.Write (List.length info.files)
        for f in info.files do w.Write f

        w.Write (List.length info.defines)
        for f in info.defines do w.Write f

        w.Write (pickleTarget info.target)

        match info.output with
        | Some o -> 
            w.Write(true)
            w.Write(o)
        | None ->
            w.Write(false)

        w.Write (List.length info.additional)
        for f in info.additional do w.Write f

        w.Write(pickleDebugType info.debug)


    let pickle (info : ProjectInfo) =
        use ms = new System.IO.MemoryStream()
        pickleTo ms info
        ms.ToArray()

    let tryUnpickle (arr : byte[]) =
        use ms = new System.IO.MemoryStream(arr)
        tryUnpickleOf ms

    let rec private projInfo additionalMSBuildProps (file : string) =

        let projDir = Path.GetDirectoryName file
        let runCmd exePath args = Utils.runProcess ignore projDir exePath (args |> String.concat " ")
    
        let netcore =
            match file with
            | ProjectRecognizer.NetCoreSdk -> true
            | _ -> false
    
        let projectAssetsJsonPath = Path.Combine(projDir, "obj", "project.assets.json")
        if netcore && not(File.Exists(projectAssetsJsonPath)) then
            let (s, a) = runCmd "dotnet" ["restore"; sprintf "\"%s\"" file]
            if s <> 0 then 
                failwithf "Cannot find restored info for project %s" file
    
        let getFscArgs = 
            
            if netcore then
                Dotnet.ProjInfo.Inspect.getFscArgs
            else
                let asFscArgs props =
                    let fsc = Microsoft.FSharp.Build.Fsc()
                    Dotnet.ProjInfo.FakeMsbuildTasks.getResponseFileFromTask props fsc
                Dotnet.ProjInfo.Inspect.getFscArgsOldSdk (asFscArgs >> Ok)

        let results =
            let msbuildExec =
                let msbuildPath =
                    if netcore then Dotnet.ProjInfo.Inspect.MSBuildExePath.DotnetMsbuild "dotnet"
                    else 
                        let all = 
                            BlackFox.VsWhere.VsInstances.getWithPackage "Microsoft.Component.MSBuild" true

                        let probes =
                            [
                                @"MSBuild\Current\Bin\MSBuild.exe"
                                @"MSBuild\15.0\Bin\MSBuild.exe"
                            ]

                        let msbuild =
                            all |> List.tryPick (fun i ->
                                probes |> List.tryPick (fun p ->
                                    let path = Path.Combine(i.InstallationPath, p)
                                    if File.Exists path then Some path
                                    else None
                                )
                            )

                        match msbuild with
                        | Some msbuild -> Dotnet.ProjInfo.Inspect.MSBuildExePath.Path msbuild
                        | None -> failwith "no msbuild"
                Dotnet.ProjInfo.Inspect.msbuild msbuildPath runCmd

            let additionalArgs = additionalMSBuildProps |> List.map (Dotnet.ProjInfo.Inspect.MSBuild.MSbuildCli.Property)

            let log = ignore

            let projs = Dotnet.ProjInfo.Inspect.getResolvedP2PRefs
            let outputPath() = Dotnet.ProjInfo.Inspect.getProperties ["OutputPath"]
            file
            |> Inspect.getProjectInfos log msbuildExec [projs; getFscArgs; outputPath] additionalArgs

        netcore, results

    let tryOfProject (additionalMSBuildProps : list<string * string>) (file : string) =
        let (netcore, info) = projInfo additionalMSBuildProps file

        match info with
        | Result.Ok info ->
            let mutable errors = []

            let projects =
                info |> List.collect (fun res ->
                    match res with
                    | Result.Ok res ->
                        match res with
                        | GetResult.ResolvedP2PRefs args -> args
                        | _ -> []
                    | Result.Error err ->
                        errors <- err :: errors
                        []
                    
                )

            let projects =
                projects |> List.map (fun p -> p.ProjectReferenceFullPath)

            let outputPath =
                info |> List.tryPick (fun res ->
                    match res with
                    | Result.Ok res ->
                        match res with
                        | GetResult.Properties args -> 
                            args |> List.tryPick (function (k,v) when k = "OutputPath" -> Some v | _ -> None)
                        | _ -> None
                    | Result.Error _err ->
                        None
                )

            let fscArgs = 
                info |> List.tryPick (fun res ->
                    match res with
                    | Result.Ok res ->
                        match res with
                        | GetResult.FscArgs args -> Some (Ok args)
                        | _ -> None
                    | Result.Error _err ->
                        None
                )

            match fscArgs with
            | Some args -> 
                match args with
                | Result.Ok args -> 
                    let res = ofFscArgs netcore file projects args
                    match outputPath with
                    | Some path ->
                        let dllName =
                            match res.output with
                            | Some o -> Path.GetFileName o
                            | None -> 
                                let ext =
                                    match res.target with
                                    | Target.Library -> ".dll"
                                    | Target.Exe | Target.WinExe -> ".exe"
                                    | Target.Module -> ".dll"

                                Path.ChangeExtension(Path.GetFileName(file), ext)
                        let realPath = Path.Combine(Path.GetDirectoryName file, path, dllName) |> Path.GetFullPath
                        Result.Ok { res with output = Some realPath }
                    | None -> 
                        Result.Ok res
                | Result.Error e -> Result.Error [sprintf "%A" e]
            | None -> 
                let errors = 
                    errors |> List.map (fun e ->
                        match e with
                        | MSBuildFailed (code, err) ->
                            sprintf "msbuild error %d: %A" code err
                        | MSBuildSkippedTarget ->
                            sprintf "msbuild skipped target"
                        | UnexpectedMSBuildResult res ->
                            sprintf "msbuild error: %s" res
                    )
                Result.Error errors
        | Result.Error e ->
            match e with
            | MSBuildFailed (code, err) ->
                Result.Error [sprintf "msbuild error %d: %A" code err]
            | MSBuildSkippedTarget ->
                Result.Error [sprintf "msbuild skipped target"]
            | UnexpectedMSBuildResult res ->
                Result.Error [sprintf "msbuild error: %s" res]
