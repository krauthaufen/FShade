#load @"paket-files/build/vrvis/Aardvark.Fake/DefaultSetup.fsx"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/FShade.sln"]

Target "Merge" (fun () ->
    let tempDir = @"bin\Release\MergeTemp"
    
    let input = @"bin\Release\FShade.Imperative.dll"
    let temp = Path.Combine(tempDir, Path.GetFileName input)
    let inputPdb = Path.ChangeExtension(input, ".pdb")
    let tempPdb = Path.ChangeExtension(temp, ".pdb")
    
    let pickler = @"bin\Release\FsPickler.dll"

    if not (Directory.Exists tempDir) then 
        Directory.CreateDirectory tempDir |> ignore


    input |> ILMerge (fun p ->  
        { p with TargetKind = TargetKind.Library; Internalize = InternalizeTypes.NoInternalize; Libraries = [pickler]; SearchDirectories = [".."] }
    ) temp

    File.Delete input
    File.Move(temp, input)
    if File.Exists inputPdb then
        File.Delete inputPdb
        File.Move(tempPdb, inputPdb)

    Directory.Delete(tempDir, true)

)

"Merge" ==> "CreatePackage"



#if DEBUG
do System.Diagnostics.Debugger.Launch() |> ignore
#endif


entry()