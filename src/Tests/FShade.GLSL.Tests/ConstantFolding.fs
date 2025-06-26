module ConstantFolding


open System
open Aardvark.Base
open FShade
open NUnit.Framework
open FsUnit
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
    }
        
type Computation = WantA=1 | WantB=2 | WantC=3

[<Test>]
let ``Simple``() =
    Setup.Run()
        
    let frag (comp : Computation) (v : Vertex) =
        fragment {
            let result = match comp with
                         | Computation.WantA -> V4f(Constant.Pi)
                         | Computation.WantB -> v.pos * ConstantF.E
                         | Computation.WantC -> v.c * 2.0f
                         | _ -> V4f.OOOO

            return result
        }

    let shaderA = frag (Computation.WantA) |> Effect.ofFunction
    let shaderB = frag (Computation.WantB) |> Effect.ofFunction
    let shaderC = frag (Computation.WantC) |> Effect.ofFunction

    let codeA, _ = GLSL.compile [ shaderA ]
    let codeB, _ = GLSL.compile [ shaderB ]
    let codeC, _ = GLSL.compile [ shaderC ]

    if (codeA.code.IndexOf "3.141592") < 0 then failwith "does not contain computation A"
    if (codeA.code.IndexOf "2.7182") >= 0 then failwith "does also contain computation B"
    if (codeA.code.IndexOf "fs_Colors") >= 0 then failwith "does also contain computation C"

    if (codeB.code.IndexOf "2.7182") < 0 then failwith "does not contain computation B"
    if (codeB.code.IndexOf "3.141592") >= 0 then failwith "does also contain computation A"
    if (codeB.code.IndexOf "fs_Colors") >= 0 then failwith "does also contain computation C"

    if (codeC.code.IndexOf "fs_Colors") < 0 then failwith "does not contain computation C"
    if (codeC.code.IndexOf "3.141592") >= 0 then failwith "does also contain computation A"
    if (codeC.code.IndexOf "2.7182") >= 0 then failwith "does also contain computation B"


[<Test>]
let ``Complex``() =
    Setup.Run()
        
    let frag (comp : Computation) (v : Vertex) =
        fragment {
            let result = if v.pos.W <> 0.0f then
                            match comp with
                             | Computation.WantA -> 
                                let temp = v.pos.X * v.pos.Y * v.pos.Z + v.pos.W
                                2.0f * (sin temp) * (cos temp)
                             | Computation.WantB -> 
                                let temp = (v.pos.X + v.pos.Y) * (v.pos.Z + v.pos.W)
                                log ((abs temp) + 1.0f)
                             | Computation.WantC -> 
                                let temp = Vec.cross (v.c.XYZ) (v.pos.XYZ)
                                temp |> Vec.length
                             | _ -> 0.0f
                         else 
                            0.0f

            return V4f(result)
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
let computer (comp : Computation, pos : V4f, c : V4f) =
    match comp with
    | Computation.WantA -> 
        let temp = pos.X * pos.Y * pos.Z + pos.W
        2.0f * (sin temp) * (cos temp)
    | Computation.WantB -> 
        let temp = (pos.X + pos.Y) * (pos.Z + pos.W)
        log ((abs temp) + 1.0f)
    | Computation.WantC -> 
        let temp = Vec.cross (c.XYZ) (pos.XYZ)
        temp |> Vec.length
    | _ -> 0.0f

[<Test>]
let ``Subroutine``() =
    Setup.Run()
        
    let frag (comp : Computation) (v : Vertex) =
        fragment {
            let result = computer(comp, v.pos, v.c)

            return V4f(result)
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
let computeA (pos : V3f) =
    let temp = pos.X * pos.Y + pos.Z
    2.0f * (sin temp) * (cos temp)

[<ReflectedDefinition>]
let computeB (pos : V3f) =
    let someUni = uniform?SOME
    let temp = (pos.X + pos.Y) * (pos.Z + someUni)
    log ((abs temp) + 1.0f)

[<ReflectedDefinition>]
let computeC (pos : V3f) =
    let temp = Vec.cross (pos) (pos)
    temp |> Vec.length
   

let Foo : V3f = V3f(0.299, 0.587, 0.114); 

[<ReflectedDefinition>] [<Inline>]
let computerXXX (color : V4f, comp : Computation, mb : bool) =
    
        let exposureValue = 1.0f

        let exposureValue = if uniform?ExposureOffset <> 0.0f then
                                exp ((log (max exposureValue 0.0001f)) - uniform?ExposureOffset)
                            else
                                exposureValue

        let exposedColor = color * exposureValue

        let exposedColor = uniform?WhiteShift * exposedColor.XYZ

        let tmColor = 
            if mb then
                let lum = Vec.Dot(exposedColor, Foo) 
                    
                if lum > 1e-7f then
                    let lumTm = match comp with
                                    | Computation.WantA -> computeA(V3f(lum))
                                    | Computation.WantB -> computeB(V3f(lum))
                                    | Computation.WantC -> computeC(V3f(lum))
                                    | _ -> lum
                 
                    lumTm * (exposedColor / lum)
                else
                    V3f.OOO
            else
                match comp with
                | Computation.WantA -> V3f(computeA(exposedColor))
                | Computation.WantB -> V3f(computeB(exposedColor))
                | Computation.WantC -> V3f(computeC(exposedColor))
                | _ -> exposedColor
                
        tmColor

[<Test>]
let ``Hilite``() =
    Setup.Run()
        
    let frag2 (comp : Computation) (myBool : bool) (v : Vertex) =
        fragment {

            let mutable result = V3f.OOO
            let result = computerXXX(v.pos, comp, myBool) 
            return V4f(result, 1.0f)
        }

    let frag (comp : Computation) (myBool : bool) (v : Vertex) =
        fragment {

            let mutable result = V3f.OOO
            for i in 0..3 do
                
                if v.pos.W <> 0.0f then
                    result <- result + computerXXX((V4f.IIII * v.pos), comp, (if myBool then true else false)) 
                else 
                    result <- result + v.pos.XYZ
                
            result <- result * 2.0f
            return V4f(result, 1.0f)
        }

    let theBool = false
    let shaderA = frag2 (Computation.WantA) (not theBool) |> Effect.ofFunction
    let shaderB = frag (Computation.WantB) (not theBool) |> Effect.ofFunction
    let shaderC = frag (Computation.WantC) (not theBool) |> Effect.ofFunction

    let config =
        EffectConfig.ofList [
            "Colors", typeof<V4f>, 0
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

    printfn "%s" codeA.code

    if (codeA.code.IndexOf "sin") < 0 then failwith "codeA does not contain computation A"
    if (codeA.code.IndexOf "abs") >= 0 then failwith "codeA does also contain computation B"
    if (codeA.code.IndexOf "length") >= 0 then failwith "codeA does also contain computation C"

    printfn "%s" codeB.code

    if (codeB.code.IndexOf "abs") < 0 then failwith "codeB does not contain computation B"
    if (codeB.code.IndexOf "sin") >= 0 then failwith "codeB does also contain computation A"
    if (codeB.code.IndexOf "length") >= 0 then failwith "codeB does also contain computation C"

    printfn "%s" codeC.code

    if (codeC.code.IndexOf "length") < 0 then failwith "codeC does not contain computation C"
    if (codeC.code.IndexOf "sin") >= 0 then failwith "codeC does also contain computation A"
    if (codeC.code.IndexOf "abs") >= 0 then failwith "codeC does also contain computation B"

[<ReflectedDefinition; Inline>]
let util (a : float32) (b : float32) (c : float32) =
    a + b * c

[<Test>]
let ``Broken``() =
    Setup.Run()
    let frag2 (v : Vertex) =
        fragment {
            let z : V3f = Fun.Sqrt (v.c.XYZ)
            return V4f(z, util (sin v.pos.X) (cos v.pos.Y) (tan v.pos.Z))
        }

    GLSL.shouldCompile [Effect.ofFunction frag2]
