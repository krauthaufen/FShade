#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"

open Fake


Target "Restore" (fun () ->
    RestorePackages()
)

Target "Clean" (fun () ->
    CleanDir "Bin"
)


Target "Compile" (fun () ->
    MSBuildRelease "Bin/Release" "Build" ["FShade.sln"] |> ignore
)


Target "Default" (fun () -> ())
Target "Rebuild" (fun () -> ())


"Restore" ==> 
    "Compile" ==>
    "Default"


"Clean" ==>
    "Restore"
    "Compile" ==>
    "Rebuild"


// start build
RunTargetOrDefault "Default"

