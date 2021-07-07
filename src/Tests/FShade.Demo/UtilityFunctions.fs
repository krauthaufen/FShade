namespace Examples

open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open System.Reflection

module UtiliyFunctions =
    
    type UniformScope with
        member x.A : float = x?Bla?A
        member x.B : float = x?Bla?B
        member x.AB : float = x?Bla?AB

        member x.X : M33d = x?Buffy?X
        member x.Y : M44d = x?Buffy?Y

        member x.Heinz : Arr<8 N, float> = x?Buffy?Heinz

    type Vertex = 
        { 
            [<SourceVertexIndex>] i : int
            [<Position>] pos : V4d
            [<Semantic("Hugo"); Interpolation(InterpolationMode.Centroid)>] hugo: V3d 
        }
 
    type Vertex1 = 
        { 
            [<Position>] pos : V4d
            [<Semantic("Hugo")>] hugo: V3d 
        }
 
    type DummyVertex =
        {
            [<Semantic("Bla")>] c : V3d
        }

    let tess (v : Triangle<DummyVertex>) =
        tessellation {
            let! c = tessellateTriangle 1.0 (1.0, 1.0, 1.0)
            return { c = c }
        }
       
    [<ReflectedDefinition; AutoOpen>]
    module Helpers =
        open Aardvark.Base.Ag

        [<Inline>]
        let monster (a : float) =
            if a < 10.0 then
                discard()

        [<Inline>]
        let f (a : float) (b : float) =
            let x = 10.0 * FShade.Imperative.ExpressionExtensions.ShaderIO.ReadInput<float>(FShade.Imperative.ParameterKind.Input, "SomeInput", None)
            a + b * uniform.A + x
  
        //[<Inline>]
        let g (a : float) (b : float) =
            let arr = Arr<8 N, int> [1..8]

            for i in 0 .. 7 do
                arr.[i] <- i

            
            let mutable a = a
            for i in 0 .. 7 do
                a <- a + float arr.[i]

            f a a + uniform.A + uniform.B

    let vs (v : Vertex) =
        vertex {
            //let m = M44d.Identity * v.pos

            let m = uniform.X  * uniform.Y.TransformDir v.pos.XYZ * uniform.Heinz.[0]

            let test = g 1.0 2.9

            return { v with pos = V4d(m, 1.0) }
        }

    let sammy =
        sampler3d {
            texture uniform?Blau
            filter Filter.Anisotropic
            addressU WrapMode.Clamp
            addressV WrapMode.Wrap
            addressW WrapMode.Mirror
        }

    let eric =
        sampler3d {
            textureArray uniform?Blau 4
            filter Filter.Anisotropic
            addressU WrapMode.Clamp
            addressV WrapMode.Wrap
            addressW WrapMode.Mirror
        }

    [<ReflectedDefinition; Inline>]
    let funny v =
        eric.[0].Sample v + eric.[1].Sample v
        //sammy.Sample v

    type UniformScope with
        member x.Stacy : float[] = uniform?StorageBuffer?Stacy 
        member x.Ingolf : Image2d<Formats.r32f> = uniform?Ingolf
    

    let shader (v : Vertex1) =
        fragment {
            // should be removed
            monster 12.0


            let a = 10 // * int v.pos.X
            let b = a * 2
            let a = 100 //* int v.pos.Y
            let b = b + a * 2
            let a1 = 1000 //* int v.pos.Z


            let c = b + a1

            let asdv = uniform.Ingolf.[V2i.Zero].X

            return funny (V3d(a,a,a)) + V4d.IIII * g v.hugo.X v.hugo.Y * float c + uniform.Stacy.[0] * asdv
        }

    let gs0 (v : Triangle<Vertex>) =
        triangle {

            let a = 3.0 * v.P0.pos * v.P0.hugo.X
            let b = 3.0 * v.P1.pos * v.P1.hugo.X
            let c = 3.0 * v.P2.pos * v.P2.hugo.X

            yield { v.P0 with pos = a + b; hugo = v.P0.pos.XYZ + v.P0.hugo }
            yield { v.P1 with pos = b + c; hugo = v.P1.pos.XYZ + v.P1.hugo }
            yield { v.P2 with pos = c + a; hugo = v.P2.pos.XYZ + v.P2.hugo }
        }


    let gs1 (v : Triangle<Vertex>) =
        triangle {
            yield { v.P0 with pos = 2.0 * v.P0.pos }
            yield { v.P1 with pos = 2.0 * v.P1.pos }
            yield { v.P2 with pos = 2.0 * v.P2.pos }
            restartStrip()
            yield { v.P0 with pos = 3.0 * v.P0.pos }
            yield { v.P1 with pos = 3.0 * v.P1.pos }
            yield { v.P2 with pos = 3.0 * v.P2.pos }

        }

    let print (add : list<string * System.Type>) (effect : Effect) =
        match effect.LastShader with
            | Some shader ->
                let mutable index = 0
                let id () =
                    let i = index
                    index <- i + 1
                    i

                let existing = shader.shaderOutputs |> Map.remove Intrinsics.SourceVertexIndex |> Map.map (fun name desc -> desc.paramType, id())
                let additional = add |> Map.ofList |> Map.map (fun name t -> t, id())

                let config =
                    {
                        depthRange      = Range1d(-1.0, 1.0)
                        flipHandedness  = false
                        lastStage       = shader.shaderStage
                        outputs         = Map.union existing additional 
                    }

                let glsl410 =
                    GLSL.Backend.Create {
                        version                 = GLSL.GLSLVersion(4,1,0)
                        enabledExtensions       = Set.ofList [ ]
                        createUniformBuffers    = true
                        bindingMode             = GLSL.BindingMode.PerKind
                        createDescriptorSets    = true
                        stepDescriptorSets      = false
                        createInputLocations    = true
                        createOutputLocations   = true
                        createPassingLocations  = true
                        createPerStageUniforms  = false
                        reverseMatrixLogic      = true
                        depthWriteMode          = false
                        useInOut                = true
                    }

                let glsl = 
                    effect
                        // compile the thing
                        |> Effect.toModule config
                        |> ModuleCompiler.compileGLSL glsl410

                printfn "%s" glsl.code

            | None ->
                ()


    let logLines (code : string) =
        let lineCount = String.lineCount code
        let lineColumns = 1 + int (Fun.Log10 lineCount)
        let lineFormatLen = lineColumns + 3
        let sb = new System.Text.StringBuilder(code.Length + lineFormatLen * lineCount + 10)
            
        let fmtStr = "{0:" + lineColumns.ToString() + "} : "
        let mutable lineEnd = code.IndexOf('\n')
        let mutable lineStart = 0
        let mutable lineCnt = 1
        while (lineEnd >= 0) do
            sb.Clear() |> ignore
            let line = code.Substring(lineStart, lineEnd - lineStart)
            sb.Append(lineCnt.ToString().PadLeft(lineColumns)) |> ignore
            sb.Append(": ")  |> ignore
            sb.Append(line) |> ignore
            Report.Line("{0}", sb.ToString())
            lineStart <- lineEnd + 1
            lineCnt <- lineCnt + 1
            lineEnd <- code.IndexOf('\n', lineStart)
            ()

        let lastLine = code.Substring(lineStart)
        if lastLine.Length > 0 then
            sb.Clear() |> ignore
            sb.Append(lineCnt.ToString()) |> ignore
            sb.Append(": ")  |> ignore
            sb.Append(lastLine) |> ignore
            Report.Line("{0}", sb.ToString())

    let run() =
        //EffectDebugger.attach()

        let effect = 
            Effect.compose [
                Effect.ofFunction vs
                Effect.ofFunction tess
                Effect.ofFunction shader
            ]
        let glsl =
            effect
                |> Effect.toModule { EffectConfig.empty with outputs = Map.ofList ["Colors", (typeof<V4d>, 0)] }
                |> ModuleCompiler.compileGLSL410

        logLines glsl.code 


//        let changeable = 
//            match EffectDebugger.register effect with
//                | Some o -> o |> unbox<IMod<Effect>>
//                | None -> Mod.constant effect

//        let subsription = 
//            changeable |> Mod.unsafeRegisterCallbackKeepDisposable (fun replacement ->
//                Log.start "updated %A" effect.Id
//                try
//                    let glsl =
//                        replacement
//                            |> Effect.toModule { EffectConfig.empty with outputs = Map.ofList ["Colors", (typeof<V4d>, 0)] }
//                            |> ModuleCompiler.compileGLSL410
//
//
//                    logLines glsl.code 
//                finally 
//                    Log.stop()
//            )
//
//
//        System.Console.ReadLine() |> ignore
//        subsription.Dispose()

        System.Environment.Exit 0
        
        

//
//        let effect =
//            Effect.compose [
//                Effect.ofFunction gs0
//                //Effect.ofFunction gs1
//            ]
//
//        effect.GeometryShader.Value.shaderBody |> Optimizer.CSE.findIndexedCommonSubExpressions
//
//        //print [ "Heinz", typeof<int> ] effect
//        System.Environment.Exit 0

        let effect = 
            Effect.compose [
//                Effect.ofFunction vs
//                Effect.ofFunction gs0
                Effect.ofFunction shader
            ]

        effect
//            // decompose derived uniforms here
//            |> Effect.substituteUniforms (fun name typ index ->
//                if name = "AB" then
//                    let a = Expr.ReadInput<float>(ParameterKind.Uniform, "A")
//                    let b = Expr.ReadInput<float>(ParameterKind.Uniform, "B")
//                    Some <@@ (%a) * (%b) @@>
//                else
//                    None
//                )
            // one of the uniforms gets layered
            //|> Effect.toLayeredEffect 2 (Map.ofList [ "A", "As" ]) InputTopology.Triangle
                
            // compile the thing
            |> print []
////
//        let e =
//            <@
//                let thing =
//                    if true then
//                        let mutable a = 10
//                        for i in 0 .. 10 do
//                            a <- a + i
//                        a
//                    else
//                        10
//
//                thing
//            @>
//
//        Optimizer.DeExpr.deExpr e |> string |> printfn "%s"


        ()


