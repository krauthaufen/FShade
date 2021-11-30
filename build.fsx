

#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"
#load @"paket-files/build/aardvark-platform/aardvark.fake/DefaultSetup.fsx"

open System
open System.IO
open System.Diagnostics
open Aardvark.Fake
open Fake.Core
open Fake.DotNet
open Fake.Core.TargetOperators


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/FShade.sln"]

Target.create "PackFShadeAOT" (fun _ ->
    let dir = Path.Combine(__SOURCE_DIRECTORY__, "src", "FShade.Preprocessor")
    let proj = Path.Combine(__SOURCE_DIRECTORY__, "src", "FShade.Preprocessor", "fshadeaot.fsproj")
    proj |> DotNet.pack (fun o -> 
        let cfg = if config.debug then DotNet.BuildConfiguration.Debug else DotNet.BuildConfiguration.Release
        let output = if config.debug then Path.Combine(__SOURCE_DIRECTORY__, "bin", "Debug") else Path.Combine(__SOURCE_DIRECTORY__, "bin", "Release")
        let version = getGitTag()

        { o with 
            //NoBuild = true
            //NoRestore = true
            Configuration = cfg
            //OutputPath = Some output 
            MSBuildParams = { o.MSBuildParams with DisableInternalBinLog = true; Properties = ["Version", version] }
            Common = { o.Common with WorkingDirectory = dir }
        }
    )
    ()
)

"Compile" ==> "PackFShadeAOT" ==> "CreatePackage"


#if DEBUG
do System.Diagnostics.Debugger.Launch() |> ignore
#endif


entry()