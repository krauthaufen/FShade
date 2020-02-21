module ConstantFolding


open System
open Aardvark.Base
open FShade
open NUnit.Framework
open FsUnit
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
    }
        
type Computation = WantA=1 | WantB=2 | WantC=3

[<Test>]
let ``Simple``() =
    Setup.Run()
        
    let frag (comp : Computation) (v : Vertex) =
        fragment {
            let result = match comp with
                         | Computation.WantA -> V4d(Constant.Pi)
                         | Computation.WantB -> v.pos * Constant.E
                         | Computation.WantC -> v.c * 2.0
                         | _ -> V4d.OOOO

            return result
        }

    let shaderA = frag (Computation.WantA) |> Effect.ofFunction
    let shaderB = frag (Computation.WantB) |> Effect.ofFunction
    let shaderC = frag (Computation.WantC) |> Effect.ofFunction

    let codeA, _ = GLSL.compile [ shaderA ]
    let codeB, _ = GLSL.compile [ shaderB ]
    let codeC, _ = GLSL.compile [ shaderC ]

    if (codeA.code.IndexOf "3.14159265") < 0 then failwith "does not contain computation A"
    if (codeA.code.IndexOf "2.7182") >= 0 then failwith "does also contain computation B"
    if (codeA.code.IndexOf "fs_Colors") >= 0 then failwith "does also contain computation C"

    if (codeB.code.IndexOf "2.7182") < 0 then failwith "does not contain computation B"
    if (codeB.code.IndexOf "3.14159265") >= 0 then failwith "does also contain computation A"
    if (codeB.code.IndexOf "fs_Colors") >= 0 then failwith "does also contain computation C"

    if (codeC.code.IndexOf "fs_Colors") < 0 then failwith "does not contain computation C"
    if (codeC.code.IndexOf "3.14159265") >= 0 then failwith "does also contain computation A"
    if (codeC.code.IndexOf "2.7182") >= 0 then failwith "does also contain computation B"


[<Test>]
let ``Complex``() =
    Setup.Run()
        
    let frag (comp : Computation) (v : Vertex) =
        fragment {
            let result = if v.pos.W <> 0.0 then
                            match comp with
                             | Computation.WantA -> 
                                let temp = v.pos.X * v.pos.Y * v.pos.Z + v.pos.W
                                2.0 * (sin temp) * (cos temp)
                             | Computation.WantB -> 
                                let temp = (v.pos.X + v.pos.Y) * (v.pos.Z + v.pos.W)
                                log ((abs temp) + 1.0)
                             | Computation.WantC -> 
                                let temp = Vec.cross (v.c.XYZ) (v.pos.XYZ)
                                temp |> Vec.length
                             | _ -> 0.0
                         else 
                            0.0

            return V4d(result)
        }

    let shaderA = frag (Computation.WantA) |> Effect.ofFunction
    let shaderB = frag (Computation.WantB) |> Effect.ofFunction
    let shaderC = frag (Computation.WantC) |> Effect.ofFunction

    let codeA, _ = GLSL.compile [ shaderA ]
    let codeB, _ = GLSL.compile [ shaderB ]
    let codeC, _ = GLSL.compile [ shaderC ]

    if (codeA.code.IndexOf "sin") < 0 then failwith "does not contain computation A"
    if (codeA.code.IndexOf "log") >= 0 then failwith "does also contain computation B"
    if (codeA.code.IndexOf "length") >= 0 then failwith "does also contain computation C"

    if (codeB.code.IndexOf "log") < 0 then failwith "does not contain computation B"
    if (codeB.code.IndexOf "sin") >= 0 then failwith "does also contain computation A"
    if (codeB.code.IndexOf "length") >= 0 then failwith "does also contain computation C"

    if (codeC.code.IndexOf "length") < 0 then failwith "does not contain computation C"
    if (codeC.code.IndexOf "sin") >= 0 then failwith "does also contain computation A"
    if (codeC.code.IndexOf "log") >= 0 then failwith "does also contain computation B"


[<ReflectedDefinition>] [<Inline>]
let computer (comp : Computation, pos : V4d, c : V4d) =
    match comp with
    | Computation.WantA -> 
        let temp = pos.X * pos.Y * pos.Z + pos.W
        2.0 * (sin temp) * (cos temp)
    | Computation.WantB -> 
        let temp = (pos.X + pos.Y) * (pos.Z + pos.W)
        log ((abs temp) + 1.0)
    | Computation.WantC -> 
        let temp = Vec.cross (c.XYZ) (pos.XYZ)
        temp |> Vec.length
    | _ -> 0.0

[<Test>]
let ``Subroutine``() =
    Setup.Run()
        
    let frag (comp : Computation) (v : Vertex) =
        fragment {
            let result = computer(comp, v.pos, v.c)

            return V4d(result)
        }

    let shaderA = frag (Computation.WantA) |> Effect.ofFunction
    let shaderB = frag (Computation.WantB) |> Effect.ofFunction
    let shaderC = frag (Computation.WantC) |> Effect.ofFunction

    let codeA, _ = GLSL.compile [ shaderA ]
    let codeB, _ = GLSL.compile [ shaderB ]
    let codeC, _ = GLSL.compile [ shaderC ]

    if (codeA.code.IndexOf "sin") < 0 then failwith "does not contain computation A"
    if (codeA.code.IndexOf "log") >= 0 then failwith "does also contain computation B"
    if (codeA.code.IndexOf "length") >= 0 then failwith "does also contain computation C"

    if (codeB.code.IndexOf "log") < 0 then failwith "does not contain computation B"
    if (codeB.code.IndexOf "sin") >= 0 then failwith "does also contain computation A"
    if (codeB.code.IndexOf "length") >= 0 then failwith "does also contain computation C"

    if (codeC.code.IndexOf "length") < 0 then failwith "does not contain computation C"
    if (codeC.code.IndexOf "sin") >= 0 then failwith "does also contain computation A"
    if (codeC.code.IndexOf "log") >= 0 then failwith "does also contain computation B"


[<ReflectedDefinition>]
let computeA (pos : V3d) =
    let temp = pos.X * pos.Y + pos.Z
    2.0 * (sin temp) * (cos temp)

[<ReflectedDefinition>]
let computeB (pos : V3d) =
    let someUni = uniform?SOME
    let temp = (pos.X + pos.Y) * (pos.Z + someUni)
    log ((abs temp) + 1.0)

[<ReflectedDefinition>]
let computeC (pos : V3d) =
    let temp = Vec.cross (pos) (pos)
    temp |> Vec.length
   

let Foo : V3d = V3d(0.299, 0.587, 0.114); 

[<ReflectedDefinition>] [<Inline>]
let computerXXX (color : V4d, comp : Computation, mb : bool) =
    
        let exposureValue = 1.0

        let exposureValue = if uniform?ExposureOffset <> 0.0 then
                                exp ((log (max exposureValue 0.0001)) - uniform?ExposureOffset)
                            else
                                exposureValue

        let exposedColor = color * exposureValue

        let exposedColor = uniform?WhiteShift * exposedColor.XYZ

        let tmColor = 
            if mb then
                let lum = Vec.Dot(exposedColor, Foo) 
                    
                if lum > 1e-7 then
                    let lumTm = match comp with
                                    | Computation.WantA -> computeA(V3d(lum))
                                    | Computation.WantB -> computeB(V3d(lum))
                                    | Computation.WantC -> computeC(V3d(lum))
                                    | _ -> lum
                 
                    lumTm * (exposedColor / lum)
                else
                    V3d.OOO
            else
                match comp with
                | Computation.WantA -> V3d(computeA(exposedColor))
                | Computation.WantB -> V3d(computeB(exposedColor))
                | Computation.WantC -> V3d(computeC(exposedColor))
                | _ -> exposedColor
                
        tmColor

[<Test>]
let ``Hilite``() =
    Setup.Run()
        
    let frag2 (comp : Computation) (myBool : bool) (v : Vertex) =
        fragment {

            let mutable result = V3d.OOO
            let result = computerXXX(v.pos, comp, myBool) 
            return V4d(result, 1.0)
        }

    let frag (comp : Computation) (myBool : bool) (v : Vertex) =
        fragment {

            let mutable result = V3d.OOO
            for i in 0..3 do
                
                if v.pos.W <> 0.0 then 
                    result <- result + computerXXX((V4d.IIII * v.pos), comp, (if myBool then true else false)) 
                else 
                    result <- result + v.pos.XYZ
                
            result <- result * 2.0
            return V4d(result, 1.0)
        }

    let theBool = false
    let shaderA = frag2 (Computation.WantA) (not theBool) |> Effect.ofFunction
    let shaderB = frag (Computation.WantB) (not theBool) |> Effect.ofFunction
    let shaderC = frag (Computation.WantC) (not theBool) |> Effect.ofFunction

    let config =
        EffectConfig.ofList [
            "Colors", typeof<V4d>, 0
        ]

    let md1 = shaderA |> Effect.toModule config
    let md2 = shaderB |> Effect.toModule config
    let md3 = shaderC |> Effect.toModule config

    let il = EffectInputLayout.ofModules [ md1; md2; md3]

    let shaderAUni = md1 |> EffectInputLayout.apply il
    let shaderBUni = md2 |> EffectInputLayout.apply il
    let shaderCUni = md3 |> EffectInputLayout.apply il

    let codeA = shaderAUni |> ModuleCompiler.compileGLSL430
    let codeB = shaderBUni |> ModuleCompiler.compileGLSL430
    let codeC = shaderCUni |> ModuleCompiler.compileGLSL430

    if (codeA.code.IndexOf "sin") < 0 then failwith "does not contain computation A"
    if (codeA.code.IndexOf "log") >= 0 then failwith "does also contain computation B"
    if (codeA.code.IndexOf "length") >= 0 then failwith "does also contain computation C"

    if (codeB.code.IndexOf "log") < 0 then failwith "does not contain computation B"
    if (codeB.code.IndexOf "sin") >= 0 then failwith "does also contain computation A"
    if (codeB.code.IndexOf "length") >= 0 then failwith "does also contain computation C"

    if (codeC.code.IndexOf "length") < 0 then failwith "does not contain computation C"
    if (codeC.code.IndexOf "sin") >= 0 then failwith "does also contain computation A"
    if (codeC.code.IndexOf "log") >= 0 then failwith "does also contain computation B"
    
    printfn "%s" codeC.code

[<ReflectedDefinition; Inline>]
let util (a : float) (b : float) (c : float) =
    a + b * c

[<Test>]
let ``Broken``() =
    Setup.Run()
    let frag2 (v : Vertex) =
        fragment {
            return V4d(V3d.III, util (sin v.pos.X) (cos v.pos.Y) (tan v.pos.Z))
        }

    GLSL.shouldCompile [Effect.ofFunction frag2]
