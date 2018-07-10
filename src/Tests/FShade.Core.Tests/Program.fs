// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    Optimizer.``[Hoist] inline function``()
    0 // return an integer exit code
