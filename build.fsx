#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"

open Fake
open System
open System.IO

let core = ["src/Libs/FShade.Compiler/FShade.Compiler.fsproj"; "src/Libs/FShade/FShade.fsproj"; "src/Libs/FShade.Debug/FShade.Debug.fsproj"];
let demo = ["src/Apps/FShade.DemoRenderer/FShade.DemoRenderer.fsproj"; "src/Apps/FShade.Demo/FShade.Demo.fsproj"];
let apps = ["src/Apps/FSCC/FSCC.fsproj"; "src/Apps/FSCC.Service/FSCC.Service.fsproj"]

Target "Restore" (fun () ->

    let packageConfigs = !!"src/**/packages.config" |> Seq.toList

    let defaultNuGetSources = RestorePackageHelper.RestorePackageDefaults.Sources
    for pc in packageConfigs do
        RestorePackage (fun p -> { p with OutputPath = "Packages" }) pc


)

Target "Clean" (fun () ->
    CleanDir "Bin"
)

Target "Core" (fun () ->
    MSBuildRelease "Bin/Release" "Build" core |> ignore
)

Target "Demo" (fun () ->
    MSBuildRelease "Bin/Release" "Build" demo |> ignore
)

Target "Apps" (fun () ->
    MSBuildRelease "Bin/Release" "Build" apps |> ignore
)


Target "Compile" (fun () -> ())
Target "Default" (fun () -> ())
Target "Rebuild" (fun () -> ())
Target "Build" (fun () -> ())


"Restore" ==> "Core"
"Core" ==> "Demo"
"Core" ==> "Apps"

"Core" ==>
    "Demo" ==>
    "Apps" ==>
    "Compile"

"Restore" ==> 
    "Compile" ==>
    "Default"


"Clean" ==> "Rebuild"
"Compile" ==> "Rebuild"
"Compile" ==> "Build"


Target "CreatePackage" (fun () ->
    let branch = Fake.Git.Information.getBranchName "."
    let releaseNotes = Fake.Git.Information.getCurrentHash()

    if branch = "master" then
        let tag = Fake.Git.Information.getLastTag()
        NuGetPack (fun p -> { p with Title = "FShade"; Project = "FShade"; OutputPath = "Bin"; Version = tag; ReleaseNotes = releaseNotes }) "NuGet/FShade.nuspec"
    else 
        traceError (sprintf "cannot create package for branch: %A" branch)
)

Target "Deploy" (fun () ->

    let accessKeyPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".ssh", "nuget.key")
    let accessKey =
        if File.Exists accessKeyPath then Some (File.ReadAllText accessKeyPath)
        else None

    let branch = Fake.Git.Information.getBranchName "."
    let releaseNotes = Fake.Git.Information.getCurrentHash()
    if branch = "master" then
        let tag = Fake.Git.Information.getLastTag()
        match accessKey with
            | Some accessKey ->
                try
                    NuGet (fun p -> let r = { p with Title = "FShade"; Project = "FShade"; OutputPath = "Bin"; AccessKey = accessKey; Publish = true; Version = tag; ReleaseNotes = releaseNotes } in printfn "%A" r; r) "NuGet/FShade.nuspec"
                with e ->
                    ()
            | None ->
                ()
     else 
        traceError (sprintf "cannot deploy branch: %A" branch)
)

"Core" ==> "CreatePackage"
"CreatePackage" ==> "Deploy"

// start build
RunTargetOrDefault "Default"

