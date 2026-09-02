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
        LookupTable.lookup [
            ShaderStage.Vertex, GLSLang.ShaderStage.Vertex
            ShaderStage.TessControl, GLSLang.ShaderStage.TessControl
            ShaderStage.TessEval, GLSLang.ShaderStage.TessEvaluation
            ShaderStage.Geometry, GLSLang.ShaderStage.Geometry
            ShaderStage.Fragment, GLSLang.ShaderStage.Fragment
            ShaderStage.Compute, GLSLang.ShaderStage.Compute
            ShaderStage.RayGeneration, GLSLang.ShaderStage.RayGen
            ShaderStage.Intersection, GLSLang.ShaderStage.Intersect
            ShaderStage.AnyHit, GLSLang.ShaderStage.AnyHit
            ShaderStage.ClosestHit, GLSLang.ShaderStage.ClosestHit
            ShaderStage.Miss, GLSLang.ShaderStage.Miss
            ShaderStage.Callable, GLSLang.ShaderStage.Callable
        ]

    let glslangWithTarget (target : GLSLang.Target) (stage : ShaderStage) (defines : List<string>) (code : string) =
        let res = 
            match GLSLang.tryCompileWithTarget target (toGLSLangStage stage) "main" false defines code with
            | Some _, warn -> 
                if String.IsNullOrWhiteSpace warn then Success
                else Warning warn
            | None, err ->
                Error err
        res

    let glslangWithDefines (stage : ShaderStage) (defines : List<string>) (code : string) =
        let res = 
            match GLSLang.tryCompile (toGLSLangStage stage) "main" false defines code with
            | Some _, warn -> 
                if String.IsNullOrWhiteSpace warn then Success
                else Warning warn
            | None, err ->
                Error err
        res

    let glslang (stage : ShaderStage) (code : string) =
        glslangWithDefines stage [sprintf "%A" stage] code     

    let compileRaytracing (effect : RaytracingEffect) =
        let module_ = effect |> RaytracingEffect.toModule
        let glsl = module_ |> ModuleCompiler.compileGLSLVulkan

        let compile (slot : ShaderSlot) (shader : RaytracingShader) =
            let result = glslangWithTarget GLSLang.Target.SPIRV_1_4 shader.Stage [slot.Conditional] glsl.code
            shader.Stage, result

        let results =
            effect.Shaders
            |> Map.toList
            |> List.map (fun (slot, shader) -> compile slot shader)

        glsl, results

    let compile' (backend : Backend) (e : list<Effect>) =
        let e = Effect.compose e
        Console.WriteLine("COMPILE {0}", e.Id)
        let outputs, lastStage =
            e.LastShader
                |> Option.map (fun s -> (s.shaderOutputs |> Map.map (fun k v -> v.paramType) |> Map.toList), s.shaderStage)
                |> Option.defaultValue (["Colors", typeof<V4f>], ShaderStage.Fragment)

        let module_ =
            e |> Effect.toModule {
                depthRange = Range1f(-1.0f, 1.0f)
                flipHandedness = false;
                lastStage = lastStage;
                outputs = outputs |> List.mapi (fun i (n,t) -> n, (t,i)) |> Map.ofList
            }

        let glsl = ModuleCompiler.compileGLSL backend module_

        glsl,
        module_.Entries |> List.map (fun e ->
            let stage = e.decorations |> List.tryPick (function FShade.Imperative.EntryDecoration.Stages s -> Some s.Stage | _ -> None) |> Option.get

            let res = glslang stage glsl.code

            stage, res
        )

    let compileCompute' (backend : Backend) (c : ComputeShader) =
        Console.WriteLine("COMPILE {0}", c.csId)
        let module_ =
            c |> ComputeShader.toModule

        let glsl = ModuleCompiler.compileGLSL backend module_

        glsl,
        module_.Entries |> List.map (fun e ->
            let stage = e.decorations |> List.tryPick (function FShade.Imperative.EntryDecoration.Stages s -> Some s.Stage | _ -> None) |> Option.get

            let res = glslang stage glsl.code

            stage, res
        )

    let compileCompute (c : ComputeShader) =
        compileCompute' glslVulkan c

    let compile (e : list<Effect>) =
        compile' glsl430 e

    let private printResults (rtx: RaytracingEffect option) (res : List<ShaderStage * CompilerResult>) (glsl : GLSLShader) =
        Console.WriteLine("====================== CODE ======================")
        Console.WriteLine(String.withLineNumbers glsl.code)
        Console.WriteLine("====================== CODE ======================")

        Console.WriteLine("======================= IO =======================")
        GLSLProgramInterface.print glsl.iface
        Console.WriteLine("======================= IO =======================")

        rtx |> Option.iter (fun e ->
            let sbt = e.ShaderBindingTableLayout
            let sym (s: Symbol) = if s.IsEmpty then "<default>" else s.ToString()

            Console.WriteLine("======================= SBT =======================")

            Console.WriteLine("Ray offsets:")
            for id, index in sbt.RayOffsets |> Map.toList |> List.sortBy snd do
                Console.WriteLine($"    {index}: {sym id}")

            Console.WriteLine("Miss indices:")
            for id, index in sbt.MissIndices |> Map.toList |> List.sortBy snd do
                Console.WriteLine($"    {index}: {sym id}")

            Console.WriteLine("Callable indices:")
            for id, index in sbt.CallableIndices |> Map.toList |> List.sortBy snd do
                Console.WriteLine($"    {index}: {sym id}")

            Console.WriteLine("======================= SBT =======================")
        )

        for (stage, r) in res do
            Console.WriteLine("{0}: {1}", stage, sprintf "%A" r)
            match r with
                | Success -> ()
                | Warning w -> ()
                | Error e -> failwithf "ERROR: %A" e

    let shouldCompile' (backend : Backend) (e : list<Effect>) =
        let glsl, res = compile' backend e
        printResults None res glsl

    let rec shouldCompile (e : list<Effect>) =
        shouldCompile' glsl430 e

    let shouldCompileCompute (c : ComputeShader) =
        let glsl, res = compileCompute c
        printResults None res glsl

    let shouldCompileRaytracing (e : RaytracingEffect) =
        let glsl, res = compileRaytracing e
        printResults (Some e) res glsl

    let shouldContainRegex (shader : GLSLShader) (regexList : list<string * Option<int>>) =
        regexList |> List.iter (fun (regex, count) ->
            match count with
            | Some n ->
                let actual = Regex.Matches(shader.code, regex, RegexOptions.Multiline).Count

                if n <> actual then
                    failwithf "ERROR: Expected compiled shader to contain '%s' %d times (encountered %d times)" regex n actual

            | _ ->
                if not <| Regex.Match(shader.code, regex, RegexOptions.Multiline).Success then
                    failwithf "ERROR: Compiled shader did not contain '%s'" regex
        )

    let shouldCompileAndContainRegex' (backend : Backend) (e : list<Effect>) (s : list<string>) =
        let glsl, res = compile' backend e
        printResults None res glsl
        s |> List.map (fun s -> s, None) |> shouldContainRegex glsl

    let shouldCompileAndContainRegex (e : list<Effect>) (s : list<string>) =
        shouldCompileAndContainRegex' glsl430 e s

    let shouldCompileAndContainRegexWithCount' (backend : Backend) (e : list<Effect>) (s : list<string * int>) =
        let glsl, res = compile' backend e
        printResults None res glsl
        s |> List.map (fun (s, n) -> s, Some n) |> shouldContainRegex glsl

    let shouldCompileAndContainRegexWithCount (e : list<Effect>) (s : list<string * int>) =
        shouldCompileAndContainRegexWithCount' glsl430 e s

    let shouldCompileRaytracingAndContainRegex (e : RaytracingEffect) (s : list<string>) =
        let glsl, res = compileRaytracing e
        printResults (Some e) res glsl
        s |> List.map (fun s -> s, None) |> shouldContainRegex glsl

    let shouldCompileComputeAndContainRegex' (backend : Backend) (c : ComputeShader) (s : list<string>) =
        let s = s |> List.map (fun s -> s, None)
        let glsl, res = compileCompute' backend c
        printResults None res glsl
        shouldContainRegex glsl s

    let shouldCompileComputeAndContainRegex (c : ComputeShader) (s : list<string>) =
        shouldCompileComputeAndContainRegex' glslVulkan c s

type Setup() =
    static let initialized = ref false

    static member Run() =
        lock initialized (fun () ->
            if not !initialized then
                Aardvark.Init()
                let glslang = typeof<GLSLang.Optimization>.Assembly
                let ptr = Aardvark.LoadLibrary(glslang, "GLSLangNative")
                if ptr = 0n then failwith "could not load glslangnative"
                //Environment.CurrentDirectory <- Path.GetDirectoryName(typeof<Setup>.Assembly.Location)
                initialized := true
            Effect.clearCaches()
        )