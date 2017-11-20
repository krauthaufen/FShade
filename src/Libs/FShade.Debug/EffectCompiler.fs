namespace FShade.Debug

open System
open System.Diagnostics
open System.Reflection
open System.IO
open System.Text
open Aardvark.Base
open FShade
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Interactive.Shell

[<AutoOpen>]
module internal GitHelpers =
    open System.Diagnostics
    let exec (folder : string) (cmd : string) (args : list<string>) =
        let args = String.concat " " args
        let info = ProcessStartInfo(cmd, args)
        info.UseShellExecute <- false
        info.RedirectStandardOutput <- true
        info.RedirectStandardError <- true
        info.WorkingDirectory <- folder

        let proc = Process.Start(info)
        proc.WaitForExit()

        if proc.ExitCode <> 0 then
            let out = proc.StandardOutput.ReadToEnd()
            let err = proc.StandardError.ReadToEnd()

            printfn "%s %s failed: %s" cmd args err
            failwithf "%s" err
        else
            ()

    let tryExec (folder : string) (cmd : string) (args : list<string>) =
        let args = String.concat " " args
        let info = ProcessStartInfo(cmd, args)
        info.UseShellExecute <- false
        info.RedirectStandardOutput <- true
        info.RedirectStandardError <- true
        info.WorkingDirectory <- folder

        let proc = Process.Start(info)
        proc.WaitForExit()
        if proc.ExitCode = 0 then
            Some (proc.StandardOutput.ReadToEnd())
        else
            None


    module Git = 
        let private lineBreak = System.Text.RegularExpressions.Regex("(\r\n)|(\n)")

        type FileStatus =
            | NoStatus
            | Modified
            | Deleted
            | Added
            | Renamed
            | Copied
            | Unmerged
            | Untracked

        
        type Status =
            | NotARepository
            | Clean
            | Dirty of Map<string, FileStatus * FileStatus>

        let private fileStatus c =
            match c with
                | ' ' -> NoStatus
                | 'M' -> Modified
                | 'D' -> Deleted
                | 'A' -> Added
                | 'R' -> Renamed
                | 'C' -> Copied
                | 'U' -> Unmerged
                | '?' -> Untracked
                | _ -> NoStatus

        let status (folder : string) =
            match tryExec folder "git" ["status"; "-s"] with
                | Some str when System.String.IsNullOrWhiteSpace str -> Clean
                | Some str -> 
                    let lines = lineBreak.Split(str) |> Array.toList

                    let files = 
                        lines |> List.choose (fun line ->
                            if line.Length > 2 then
                                let x = fileStatus line.[0]
                                let y = fileStatus line.[1]
                                let rest = line.Substring 3

                                Some (rest, (x,y))

                            else
                                None
                        )
                    Dirty (Map.ofList files)
                | None -> 
                    NotARepository

        let add (folder : string) (pattern : string) =
            exec folder "git" ["add"; pattern]
            
        let commit (folder : string) (message : string) =
            exec folder "git" ["commit"; "-m"; "\"" + message + "\""]
               
        let amend (folder : string) =
            exec folder "git" ["commit"; "--amend"; "--no-edit"; "--allow-empty"]

        let init (folder : string)  =
            exec folder "git" ["init"]


module EffectCompiler =
    
    let err = new StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(new StringBuilder())
    let errStream = new StringWriter(err)

    let assemblies =
        [
            "Aardvark.Base"
            "Aardvark.Base.TypeProviders"
            "Aardvark.Base.FSharp"
            "FShade.Imperative"
            "FShade.Core"
        ]

    let asmPaths =
        assemblies |> List.choose (fun name ->
            let a = Assembly.Load name
            try Some a.Location
            with _ -> None
        )


    let private composeCode =
        String.concat "\r\n" [
            "open Microsoft.FSharp.Quotations"
            "open FShade"
             
            "let mutable currentEffect = None"
             
            "type ComposeBuilder() ="
            "   member x.Bind(f : 'a -> Expr<'b>, g : unit -> list<Effect>) = (Effect.ofFunction f) :: g()"
            "   member x.Return (()) = []"
            "   member x.Zero() = []"
            "   member x.Delay(f : unit -> list<Effect>) = f"
            "   member x.Combine(l : list<Effect>, r : unit -> list<Effect>) = l @ r()"
            "   member x.Run(r : unit -> list<Effect>) = currentEffect <- Some (Effect.compose (r()))"
             
            "let compose = ComposeBuilder()"
        ]

    let private bootCode =
        let loads = asmPaths |> Seq.map (sprintf "#r @\"%s\"") |> String.concat "\r\n"
        String.concat "\r\n" [
            loads
            "module Setup = "
            String.indent 1 composeCode
        ]

    let private setupCode =
        let loads = asmPaths |> Seq.map (sprintf "#r @\"%s\"") |> String.concat "\r\n"
        String.concat "\r\n" [
            "#if FSHADEDEBUG"
            "#else"
            loads
            composeCode
            "#endif"
        ]

    let private fsi =
        let argv = [| "FsiAnyCPU.exe";  "--noninteractive"; "--define:FSHADEDEBUG" |]
        let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
        FsiEvaluationSession.Create(fsiConfig, argv, inStream, outStream, errStream) 

    let private currentProp = 
        match fsi.EvalInteractionNonThrowing bootCode with
            | Choice1Of2 (), _ -> 

                let rec all (t : Type) =
                    let all = t.GetNestedTypes() |> Array.collect all
                    Array.append [| t |] all

                let allTypes = fsi.DynamicAssembly.GetTypes() |> Array.collect all
                allTypes |> Array.tryPick (fun t ->
                    let prop = t.GetProperty("currentEffect")
                    if isNull prop || prop.PropertyType <> typeof<Option<Effect>> then
                        None
                    else
                        Some prop
                )


            | _, err -> 
                None
            
    do File.WriteAllText(Path.Combine(System.Environment.CurrentDirectory, "Setup.fsx"), setupCode)

    let appName =
        try Path.GetFileNameWithoutExtension(Assembly.GetEntryAssembly().Location)
        with _ -> Guid.NewGuid() |> string

    let debugDir =
        let desktop = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)
        let dir = Path.Combine(desktop, appName)
        if Directory.Exists dir then
            
            let files = 
                Directory.GetFiles(dir) 
                    |> Seq.map Path.GetFileName
                    |> Set.ofSeq
                    
            match Git.status dir with
                | Git.Clean ->
                    ()

                | Git.Dirty files ->
                    Git.add dir "."
                    Git.commit dir "backup"

                | Git.NotARepository ->
                    Git.init dir
                    if not (Set.isEmpty files) then
                        Git.add dir "."
                        Git.commit dir "backup"

            for f in files do
                let p = Path.Combine(dir, f)
                File.Delete p

            let compDir = Path.Combine(dir, "compositions")
            if Directory.Exists compDir then
                Directory.Delete(compDir, true)
            
            let outDir = Path.Combine(dir, "glsl")
            if Directory.Exists outDir then
                Directory.Delete(outDir, true)


            File.WriteAllText(Path.Combine(dir, ".gitignore"), "/compositions\r\n/glsl")
            File.WriteAllText(Path.Combine(dir, "Setup.fsx"), setupCode)
            
            match Git.status dir with
                | Git.Dirty _ ->
                    Git.add dir "."
                    Git.commit dir "import"
                | _ ->
                    ()

        else
            Directory.CreateDirectory dir |> ignore
            File.WriteAllText(Path.Combine(dir, "Setup.fsx"), setupCode)
            File.WriteAllText(Path.Combine(dir, ".gitignore"), "/compositions\r\n/glsl")
            Git.init dir
            Git.add dir "."
            Git.commit dir "import"
        
        dir

    let compDir =
        let dir = Path.Combine(debugDir, "compositions")
        if not (Directory.Exists dir) then
            Directory.CreateDirectory(dir) |> ignore
        dir

    let outDir =
        let dir = Path.Combine(debugDir, "glsl")
        if not (Directory.Exists dir) then
            Directory.CreateDirectory(dir) |> ignore
        dir


        
                    
    let private toPascalCase (str : string) =
        if str.Length > 0 then
            if str.[0] >= 'a' || str.[0] <= 'Z' then
                str.Substring(0, 1).ToUpper() + str.Substring(1)
            else
                str
        else
            str

    let tryCompile fileName  =
        lock fsi (fun () ->
            let value, errors = fsi.EvalScriptNonThrowing fileName
            match value with
                | Choice1Of2 () ->
                    match currentProp.Value.GetValue(null) |> unbox<Option<Effect>> with
                        | Some e -> Choice1Of2 e
                        | None -> Choice2Of2 errors
                | _ -> 
                    Choice2Of2 errors
        )

    let private dummyShader =
        String.concat "\r\n" [
            "open Aardvark.Base"
            "open FShade"
            "open Setup"
            ""
            "type Vertex = { [<Position>] pos : V4d }"
            ""
            "let shader(v : Vertex) ="
            "    vertex {"
            "        return { v with pos = 3.0 * v.pos }"
            "    }"
            ""
            "compose {"
            "    do! shader"
            "}"
        ]

    let init() =
        if not (Directory.Exists compDir) then
            failwith "init failed"

        let temp = Path.GetTempFileName() + ".fsx"
        File.WriteAllText(temp, dummyShader)
        match tryCompile temp with
            | Choice1Of2 _ -> ()
            | _ -> Log.warn "bad"
        File.Delete temp
