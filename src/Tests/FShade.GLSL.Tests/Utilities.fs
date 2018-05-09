namespace FShade.Tests

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open Aardvark.Base
open GLSLang
open NUnit.Framework
open FShade
open FShade.GLSL

module Code =
    let private whiteSpace = Regex @"[ \t\r\n]+"
    
    let equal (a : string) (b : string) =
        let a = whiteSpace.Replace(a, "")
        let b = whiteSpace.Replace(b, "")
        a = b

type CompilerResult =
    | Warning of string
    | Error of string
    | Success

module GLSL =

    let private toGLSLangStage =
        LookupTable.lookupTable [
            ShaderStage.Vertex, GLSLang.ShaderStage.Vertex
            ShaderStage.TessControl, GLSLang.ShaderStage.TessControl
            ShaderStage.TessEval, GLSLang.ShaderStage.TessEvaluation
            ShaderStage.Geometry, GLSLang.ShaderStage.Geometry
            ShaderStage.Fragment, GLSLang.ShaderStage.Fragment
            ShaderStage.Compute, GLSLang.ShaderStage.Compute
        ]



    let compile (e : list<Effect>) =
        let e = Effect.compose e

        let outputs, lastStage = 
            e.LastShader 
                |> Option.map (fun s -> (s.shaderOutputs |> Map.map (fun k v -> v.paramType) |> Map.toList), s.shaderStage) 
                |> Option.defaultValue (["Colors", typeof<V4d>], ShaderStage.Fragment)
            
        let module_ = 
            e |> Effect.toModule { 
                depthRange = Range1d(-1.0, 1.0)
                flipHandedness = false;
                lastStage = lastStage;
                outputs = outputs |> List.mapi (fun i (n,t) -> n, (t,i)) |> Map.ofList
            }

        let glsl = ModuleCompiler.compileGLSL430 module_
        
        module_.entries |> List.map (fun e ->
            let stage = e.decorations |> List.tryPick (function FShade.Imperative.EntryDecoration.Stages { self = s } -> Some s | _ -> None) |> Option.get
            
            let defines = [sprintf "%A" stage]
            match GLSLang.tryCompile (toGLSLangStage stage) "main" defines glsl.code with
                | Some _, warn -> 
                    if String.IsNullOrWhiteSpace warn then Success
                    else Warning warn
                | None, err ->
                    Error err

        )

[<SetUpFixture>]
type Setup() =
    [<OneTimeSetUp>]
    static member Setup() =
        Environment.CurrentDirectory <- Path.GetDirectoryName(typeof<Setup>.Assembly.Location)
        Aardvark.Init()