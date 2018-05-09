namespace FShade.Tests

open Aardvark.Base
open FShade
open NUnit.Framework
open FsUnit
open FShade.Tests

module ``Simple Tests`` =
    
    type Vertex =
        {
            [<Position>] pos : V4d
            [<Color>] c : V4d
        }
        
    [<Test>]
    let ``Color Fragment Shader`` =
        
        let frag (v : Vertex) =
            fragment {
                return v.c
            }


        let res = GLSL.compile [ Effect.ofFunction frag ]

        for r in res do
            match r with
                | Success -> ()
                | Warning w -> ()
                | Error e -> failwithf "ERROR: %A" e



 



