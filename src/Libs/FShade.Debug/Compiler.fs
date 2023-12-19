namespace FShade.Debug

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open Aardvark.Base

module internal Compiler =

    module private Path =

        let getTempDirectory =
            let root = Path.Combine(Path.GetTempPath(), "fshade-debug")

            try if Directory.Exists root then Directory.Delete(root, true)
            with _ -> ()

            let get() =
                Path.Combine(root, Path.GetRandomFileName())

            fun () ->
                let mutable dir = get()
                while Directory.Exists dir do
                    dir <- get()

                Directory.CreateDirectory dir |> ignore
                dir

    module private Process =

        let run (path : string) (arguments : string list) =
            let arguments = arguments |> String.concat " "

            // Set working directory to root in order to escape global.json
            let workingDir =
                try Directory.GetDirectoryRoot path
                with _ -> null

            try
                use p = new Process()
                p.StartInfo.FileName <- path
                p.StartInfo.Arguments <- arguments
                p.StartInfo.RedirectStandardOutput <- true
                p.StartInfo.RedirectStandardError <- true
                p.StartInfo.UseShellExecute <- false
                p.StartInfo.CreateNoWindow <- true

                if workingDir <> null then
                    p.StartInfo.WorkingDirectory <- workingDir

                Log.line "%s %s" path arguments

                let output = List<string>()

                p.OutputDataReceived.Add (fun args ->
                    if not <| String.IsNullOrWhiteSpace args.Data then
                        lock output (fun _ -> output.Add args.Data)
                )
                p.ErrorDataReceived.Add ignore

                p.Start() |> ignore
                p.BeginOutputReadLine()
                p.BeginErrorReadLine()
                p.WaitForExit()

                if p.ExitCode = 0 then
                    Result.Ok ()
                else
                    let output =
                        output
                        |> String.concat Environment.NewLine
                        |> String.indent 1

                    Report.Line(2, Environment.NewLine + output)
                    Result.Error $"Process exited with code {p.ExitCode}."

            with exn ->
                Result.Error exn.Message

    let private extensions =
        [| "dll"; "exe" |]

    let build (data : ProjectData) =
        let outputDir = Path.getTempDirectory()

        for ref in data.References do
            let name = Path.GetFileName ref
            let mutable found = false

            for ext in extensions do
                let srcFile = Path.ChangeExtension(name, ext)
                let dstFile = Path.Combine(outputDir, srcFile)

                if File.Exists srcFile then
                    File.Copy(srcFile, dstFile, true)
                    found <- true

            if not found then
                Log.warn "Cannot find assembly for %s" name

        let buildResult =
            Process.run "dotnet" [
                "build"
                $"\"{data.Path}\""
                $"--framework {data.Framework}"
                $"--output \"{outputDir}\""
                "--no-dependencies"
                "--no-restore"
                "--no-self-contained"
                "-property:DebugSymbols=false"
                "-property:DebugType=None"
                "-property:SatelliteResourceLanguages=en"
                "-property:GenerateDocumentationFile=false"
            ]

        match buildResult with
        | Result.Ok _ ->
            let output = Path.Combine(outputDir, data.Name)

            let artifactPath =
                extensions |> Array.tryPick (fun ext ->
                    let file = output + "." + ext

                    if File.Exists file then
                        Some file
                    else
                        None
                )

            match artifactPath with
            | Some path ->
                try
                    for dir in Directory.GetDirectories outputDir do
                        Directory.Delete(dir, true)

                    for file in Directory.GetFiles outputDir do
                        if Path.GetFileName file <> Path.GetFileName path then
                            File.Delete file
                with _ ->
                    ()

                Result.Ok path
            | _ ->
                Result.Error $"Could not find build artifact in '{outputDir}'."

        | Result.Error error ->
            Result.Error error