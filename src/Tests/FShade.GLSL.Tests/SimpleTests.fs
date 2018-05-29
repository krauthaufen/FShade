module SingleEffects

open System
open Aardvark.Base
open FShade
open Xunit
open FsUnit
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
    }
        

    
[<Fact>]
let ``Broken GLSL Shader``() =
    Setup.Run()

    let code = 
        String.concat "\r\n" [
            "#version 430"
            "layout(location = 0) in vec4 Position;"
            "void main() {"
            "    gl_Position = Postion;"
            "}"
        ]

    let res = GLSL.glslang ShaderStage.Vertex code
    Console.WriteLine("{0}: {1}", ShaderStage.Vertex, sprintf "%A" res)
    match res with
        | Success | Warning _ -> failwith "compiler should not succeed"
        | Error e -> ()



[<Fact>]
let ``Simple fragment shader``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c
        }
            
    GLSL.shouldCompile [ Effect.ofFunction frag ]
        

[<Fact>]
let ``Helper with duplicate names``() =
    Setup.Run()

    let vert (v : Vertex) =
        vertex {
            if v.pos.X > 0.0 then
                let Positions = v.pos.W + v.pos.X
                let (a,b) = 
                    let v = 2.0 * v.pos
                    (v.X, v.Y + 1.0 + Positions)
                return { v with pos = V4d(a,b,b,a) }
            else
                return { v with pos = V4d.Zero }
        }
            
    GLSL.shouldCompile [ Effect.ofFunction vert ]

  
type PointSizeVertex =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
        [<PointSize>] s : float
    }
        

[<Fact>]
let ``PointSize shader``() =
    Setup.Run()

    let vert (v : PointSizeVertex) =
        vertex {
            if v.pos.X > 0.0 then
                let Positions = v.pos.W + v.pos.X
                let (a,b) = 
                    let v = 2.0 * v.pos
                    (v.X, v.Y + 1.0 + Positions)
                return { v with pos = V4d(a,b,b,a); s = 10.0 }
            else
                return { v with pos = V4d.Zero; s = 5.0 }
        }
            
    GLSL.shouldCompile [ Effect.ofFunction vert ]



