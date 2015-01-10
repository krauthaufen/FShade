#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"

open Fake


let core = ["Libs/FShade.Compiler/FShade.Compiler.fsproj"; "Libs/FShade/FShade.fsproj"; "Libs/FShade.Debug/FShade.Debug.fsproj"];
let demo = ["Apps/FShade.DemoRenderer/FShade.DemoRenderer.fsproj"; "Apps/FShade.Demo/FShade.Demo.fsproj"];
let apps = ["Apps/FSCC/FSCC.fsproj"; "Apps/FSCC.Service/FSCC.Service.fsproj"]

Target "Restore" (fun () ->
    RestorePackages()
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

// start build
RunTargetOrDefault "Default"

