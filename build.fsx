#load @"paket-files/build/aardvark-platform/aardvark.fake/DefaultSetup.fsx"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/FShade.sln"]

Target "SourceLink.Test" (fun _ ->
    !! "bin/*.symbols.nupkg" 
    |> Seq.iter (fun nupkg ->
        DotNetCli.RunCommand
            (fun p -> { p with WorkingDir = __SOURCE_DIRECTORY__ @@ "src" @@ "Tests" @@ "FShade.Core.Tests" } )
            (sprintf "sourcelink test %s" nupkg)
    )
)

"CreatePackage" ==> "SourceLink.Test"
"SourceLink.Test" ==> "Push"

#if DEBUG
do System.Diagnostics.Debugger.Launch() |> ignore
#endif


entry()