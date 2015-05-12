#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"

open Fake
open System
open System.IO

let core = ["src/Libs/FShade.Compiler/FShade.Compiler.fsproj"; "src/Libs/FShade/FShade.fsproj"; "src/Libs/FShade.Debug/FShade.Debug.fsproj"];
let demo = ["src/Apps/FShade.DemoRenderer/FShade.DemoRenderer.fsproj"; "src/Apps/FShade.Demo/FShade.Demo.fsproj"];
let apps = ["src/Apps/FSCC/FSCC.fsproj"; "src/Apps/FSCC.Service/FSCC.Service.fsproj"]

let packageRx = System.Text.RegularExpressions.Regex @"(?<name>.*?)\.(?<version>([0-9]+\.)*[0-9]+)\.nupkg$"

let updatePackages (sources : list<string>) (projectFiles : #seq<string>) =
    let packages = 
        sources |> List.collect (fun source ->  
            Directory.GetFiles(source,"*.nupkg") 
                |> Array.choose (fun s -> 
                    let m = Path.GetFileName s |> packageRx.Match in if m.Success then m.Groups.["name"].Value |> Some else None
                ) 
                |> Array.toList
        ) |> Set.ofList

    if Set.count packages <> 0 then
        for project in projectFiles do
            project |> Fake.NuGet.Update.NugetUpdate (fun p ->  
                { p with 
                    Ids = packages |> Set.intersect (Set.ofList p.Ids) |> Set.toList
                    //ToolPath = @"E:\Development\aardvark-2015\tools\NuGet\nuget.exe"
                    RepositoryPath = "packages"
                    Sources = sources @ Fake.NuGet.Install.NugetInstallDefaults.Sources
                    //Prerelease = true
                } 
            )

Target "Restore" (fun () ->

    let packageConfigs = !!"src/**/packages.config" |> Seq.toList

    

    let addtionalSources = (environVarOrDefault "AdditionalNugetSources" "").Split([|";"|],StringSplitOptions.RemoveEmptyEntries) |> Array.toList
    let defaultNuGetSources = RestorePackageHelper.RestorePackageDefaults.Sources @ ["https://www.nuget.org/api/v2/" ]

    for pc in packageConfigs do
        RestorePackage (fun p -> { p with OutputPath = "packages"
                                          Sources = addtionalSources @ defaultNuGetSources  
                                 }) pc
    updatePackages addtionalSources  (!!"src/**/*.csproj" ++ "src/**/*.fsproj")


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

