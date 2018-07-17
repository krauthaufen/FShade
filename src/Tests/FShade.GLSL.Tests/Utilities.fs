namespace FShade.Tests

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open Aardvark.Base
open GLSLang
open FShade
open FShade.GLSL

#nowarn "4321"

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


    let glslang (stage : ShaderStage) (code : string) =
        let defines = [sprintf "%A" stage]
        let res = 
            match GLSLang.tryCompile (toGLSLangStage stage) "main" defines code with
                | Some _, warn -> 
                    if String.IsNullOrWhiteSpace warn then Success
                    else Warning warn
                | None, err ->
                    Error err
        res

    let compile (e : list<Effect>) =
        let e = Effect.compose e
        Console.WriteLine("COMPILE {0}", e.Id)
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
        
        glsl,
        module_.entries |> List.map (fun e ->
            let stage = e.decorations |> List.tryPick (function FShade.Imperative.EntryDecoration.Stages { self = s } -> Some s | _ -> None) |> Option.get
            
            let res = glslang stage glsl.code
            
            stage, res
        )

    let shouldCompile (e : list<Effect>) =
        let glsl, res = compile e

        Console.WriteLine("====================== CODE ======================")
        Console.WriteLine(glsl.code)
        Console.WriteLine("====================== CODE ======================")
        
        Console.WriteLine("======================= IO =======================")
        GLSLProgramInterface.print glsl.iface
        Console.WriteLine("======================= IO =======================")

        
        for (stage, r) in res do
            Console.WriteLine("{0}: {1}", stage, sprintf "%A" r)
            match r with
                | Success -> ()
                | Warning w -> ()
                | Error e -> failwithf "ERROR: %A" e
                

    let shouldContainRegex (shader : GLSLShader) (regexList : list<string>) = 

        let notContained = regexList |> List.filter (fun s ->
                if Regex.Match(shader.code, s).Success then
                    false
                else 
                    true
            )

        if List.length notContained = 0 then
            ()
        else 
            failwithf "ERROR: Compiled shader did not contain %A" notContained

    let shouldCompileAndContainRegex (e : list<Effect>) (s : list<string>) =
        let glsl, res = compile e
        
        Console.WriteLine("{0}", glsl.code)
        for (stage, r) in res do
            Console.WriteLine("{0}: {1}", stage, sprintf "%A" r)
            match r with
                | Success -> shouldContainRegex glsl s
                | Warning w -> shouldContainRegex glsl s
                | Error e -> failwithf "ERROR: %A" e

type Setup() =
    static let initialized = ref false

    static member Run() =
        lock initialized (fun () ->
            if not !initialized then
                Environment.CurrentDirectory <- Path.GetDirectoryName(typeof<Setup>.Assembly.Location)
                Aardvark.Init()
                initialized := true
            Effect.clearCaches()
        )