open System.Reflection
open FShade.Compiler.Service
open Microsoft.FSharp.Reflection
open Aardvark.Base
open FShade

let code =
    """
open Aardvark.Base
open FShade

module Test =
    type Vertex =
        {
            [<Position>]
            pos : V4d
        }

    let shader(v : Vertex) =
        vertex {
            return { pos = v.pos * 2.0 }
        }    
    
"""


[<EntryPoint>]
let main args =
    let res = Parser.test code
    printfn "%A" res
    0