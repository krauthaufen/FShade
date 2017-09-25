#load @"paket-files/build/vrvis/Aardvark.Fake/DefaultSetup.fsx"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/FShade.sln"]

Target "Merge" (fun () ->
    
    let compiled = @"bin\Release\FShade.Imperative.Compiled.dll"
    let result = @"bin\Release\FShade.Imperative.dll"
    
    let pickler = @"bin\Release\FsPickler.dll"

    compiled |> ILMerge (fun p -> 
        { p with Internalize = InternalizeTypes.Internalize; Libraries = [pickler] }
    ) result
)

"Merge" ==> "CreatePackage"



#if DEBUG
do System.Diagnostics.Debugger.Launch() |> ignore
#endif


entry()