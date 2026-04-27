module FShade.Aot.Tests.Runner.Program

open System
open System.IO
open System.Reflection
open Aardvark.Base
open FShade
open FShade.GLSL

type Vertex =
    {
        [<Position>] pos : V4d
        [<Color>]    col : V4d
    }

/// Build effect by directly invoking the shader function with placeholder input,
/// extracting Expr, and feeding to Effect.ofExpr.
let buildEffect (mi : MethodInfo) (args : obj[]) : Effect =
    let allParams = mi.GetParameters()
    let inputType = allParams.[allParams.Length - 1].ParameterType
    let placeholder =
        if inputType.IsValueType then Activator.CreateInstance(inputType)
        else null
    let fullArgs = Array.append args [| placeholder |]
    let exprObj = mi.Invoke(null, fullArgs)
    let expr = exprObj :?> Microsoft.FSharp.Quotations.Expr
    Effect.ofExpr inputType expr

let toGlsl (effect : Effect) : string =
    let cfg = EffectConfig.ofMap (Map.ofList [ "Colors", (typeof<V4d>, 0) ])
    let module' = Effect.toModule cfg effect
    let glsl = ModuleCompiler.compileGLSL410 module'
    glsl.code

let runOne (ass : Assembly) (typeName : string) (methodName : string) (args : obj[]) : (string * string) =
    let t = ass.GetType(typeName)
    if isNull t then failwithf "type %s not found in %s" typeName ass.FullName
    let mi =
        t.GetMethods(BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.NonPublic)
        |> Array.find (fun m -> m.Name = methodName)
    let effect = buildEffect mi args
    effect.Id, toGlsl effect

/// For closures (stored FSharpFunc values), we don't know their MethodInfo statically;
/// access them via the static module-level field, then invoke via FSharpFunc.Invoke.
let runClosure (ass : Assembly) (typeName : string) (fieldName : string) (inputType : Type) : (string * string) =
    let t = ass.GetType(typeName)
    let fn = t.GetProperty(fieldName).GetValue(null)
    // Find Invoke(input) via reflection.
    let mi = fn.GetType().GetMethod("Invoke", [| inputType |])
    if isNull mi then failwithf "no Invoke(%s) on %s" inputType.Name (fn.GetType().FullName)
    let placeholder =
        if inputType.IsValueType then Activator.CreateInstance(inputType)
        else null
    let exprObj = mi.Invoke(fn, [| placeholder |])
    let expr = exprObj :?> Microsoft.FSharp.Quotations.Expr
    let effect = Effect.ofExpr inputType expr
    effect.Id, toGlsl effect

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "usage: Runner <shaders-dir>"
        exit 1
    let shadersDir = argv.[0]

    // Make the default ALC resolve from the given dir (so we pick up the right Shaders.dll).
    System.Runtime.Loader.AssemblyLoadContext.Default.add_Resolving(fun ctx name ->
        let p = Path.Combine(shadersDir, name.Name + ".dll")
        if File.Exists p then ctx.LoadFromAssemblyPath p
        else null)

    let ass =
        System.Runtime.Loader.AssemblyLoadContext.Default.LoadFromAssemblyPath(
            Path.Combine(shadersDir, "FShade.Aot.Tests.Shaders.dll"))

    let cases : (string * obj[]) list = [
        "frag_constantColor", [| box (V4d(0.4, 0.5, 0.6, 1.0)) |]
        "frag_constantColor", [| box (V4d(0.1, 0.2, 0.3, 1.0)) |]
        "frag_scaled",        [| box (V4d.IIII); box 2.5 |]
        "frag_scaled",        [| box (V4d.IIII); box 7.0 |]
        "frag_many",          [| box 1.0; box 2.0; box 3.0; box 4.0; box 5.0; box 6.0 |]
        "frag_int_arg",       [| box 7 |]
        "makeCapturedFrag",   [| box 3.5; box (V4d(0.4, 0.5, 0.6, 1.0)) |]
        "makeCapturedFrag",   [| box 7.0; box (V4d(0.1, 0.2, 0.3, 1.0)) |]
        "constantColorLike",  [| box (C4b(255uy, 128uy, 64uy, 255uy)) |]
        "constantColorLike",  [| box (C4b(10uy, 20uy, 30uy, 255uy)) |]
        "frag_passthrough",   [| |]
        "frag_swizzle",       [| |]
        // vert_scale skipped: shader writes to V4f Position from V4d which the
        // test's input layout doesn't match — that's an EffectConfig issue unrelated to AOT.
    ]

    let inputType = ass.GetType("FShade.Aot.Tests.Shaders.Definitions+Vertex")

    use w = new StringWriter()

    let mutable allOk = true
    for (mn, args) in cases do
        try
            let (id, glsl) = runOne ass "FShade.Aot.Tests.Shaders.Definitions" mn args
            w.WriteLine("=== " + mn + " args=" + string args.Length + " ===")
            w.WriteLine("id: " + id)
            w.WriteLine(glsl)
            w.WriteLine()
        with e ->
            allOk <- false
            let rec unwrap (e : exn) =
                match e with
                | :? TargetInvocationException as t when not (isNull t.InnerException) -> unwrap t.InnerException
                | _ -> e
            let real = unwrap e
            w.WriteLine("=== " + mn + " args=" + string args.Length + " ===")
            w.WriteLine("FAILED: " + real.GetType().Name + ": " + real.Message)

    // Closure (stored FSharpFunc value) cases — exercise instance-method shader rewrites.
    for fieldName in [ "closure_with_capture"; "nested_closure" ] do
        try
            let (id, glsl) = runClosure ass "FShade.Aot.Tests.Shaders.Definitions" fieldName inputType
            w.WriteLine("=== " + fieldName + " (closure) ===")
            w.WriteLine("id: " + id)
            w.WriteLine(glsl)
            w.WriteLine()
        with e ->
            allOk <- false
            let rec unwrap (e : exn) =
                match e with
                | :? TargetInvocationException as t when not (isNull t.InnerException) -> unwrap t.InnerException
                | _ -> e
            let real = unwrap e
            w.WriteLine("=== " + fieldName + " (closure) ===")
            w.WriteLine("FAILED: " + real.GetType().Name + ": " + real.Message)

    printf "%s" (w.ToString())
    printfn "AotMarkerInvocations: %d" Aot.markerInvocations
    printfn "AotMarkerPrecomputedInvocations: %d" Aot.markerPrecomputedInvocations
    if allOk then 0 else 1
