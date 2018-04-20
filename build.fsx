#load @"paket-files/build/aardvark-platform/Aardvark.Fake/DefaultSetup.fsx"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/FShade.sln"]


#if DEBUG
do System.Diagnostics.Debugger.Launch() |> ignore
#endif


entry()