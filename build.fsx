#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"

open Fake


let core = ["src/Libs/FShade.Compiler/FShade.Compiler.fsproj"; "src/Libs/FShade/FShade.fsproj"; "src/Libs/FShade.Debug/FShade.Debug.fsproj"];
let demo = ["src/Apps/FShade.DemoRenderer/FShade.DemoRenderer.fsproj"; "src/Apps/FShade.Demo/FShade.Demo.fsproj"];
let apps = ["src/Apps/FSCC/FSCC.fsproj"; "src/Apps/FSCC.Service/FSCC.Service.fsproj"]

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

