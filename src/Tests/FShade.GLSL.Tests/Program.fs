module Program

open System
open System.Reflection
open System.IO
open Aardvark.Base
open FShade
open FShade.Imperative
open Microsoft.FSharp.Quotations
open FShade.GLSL


type Preprocessor() =
    member x.Run (file : string) =
        let test = Assembly.LoadFile @"C:\Users\Schorsch\Development\MapNew\MapNew\bin\Debug\netcoreapp3.1\Expecto.dll"
        printfn "%A" test
        let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static

        let a = Assembly.LoadFile file

        let rec allTypes (t : Type) =
            let nested = t.GetNestedTypes flags
            Seq.append (Seq.singleton t) (nested |> Seq.collect allTypes)

        a.DefinedTypes
        |> Seq.collect allTypes
        |> Seq.collect (fun t ->
            t.GetMethods(flags)
            |> Seq.choose (fun mi ->
                if typeof<Expr>.IsAssignableFrom mi.ReturnType && mi.GetParameters().Length = 1 then
                    let pars = mi.GetParameters()
                    let inputType = pars.[0].ParameterType
                    try 
                        let value =
                            if inputType.IsValueType then Activator.CreateInstance(inputType)
                            else null

                        let res = mi.Invoke(null, [| value |]) :?> Expr
                        Log.startTimed "compiling %s.%s" mi.DeclaringType.FullName mi.Name
                        try
                            try 
                                let effect = Effect.ofExpr inputType res
                                Some(mi, effect)
                            with _ ->
                                Log.warn "failed"
                                None
                        finally
                            Log.stop()
                    with _ ->
                        None
                else
                    None
            )
        )
        |> Seq.toArray


[<ReflectedDefinition>]
let foo(a : float) =
    3.0 * a

type Rec = { a : float; b : int }

let thing1 = { a = 10.0; b = -4 }
let thing2 = { a = 3.0; b = -4 }

let vert (v : SingleEffects.Vertex) =
    vertex {
        if v.pos.X > 0.0 then
            //let a = v.pos.X
            //let b = v.pos.Y + 1.0
            let bla = v.pos.W + v.pos.X |> foo
            //let va = 2.0 * v.pos
            //let a = va.X
            //let b = v.pos.Y + v.pos.W * v.pos.Z + bla

            let test =
                if bla > 0.2 then thing1
                else thing2

            let (a,b) =
                let v = test.a * v.pos
                (v.X, v.Y + 1.0 + bla)
            return { v with pos = V4d(a,b,b,a) }
        else
            return { v with pos = V4d.Zero }
    }

open System.Security.Cryptography
open System.IO

let hans = "asdjnasjdkmsajkdmkasd"

[<EntryPoint>]
let main args = 
    Aardvark.Base.Aardvark.Init()
    //FShade.Serializer.Init()

    let sw = System.Diagnostics.Stopwatch.StartNew()
    let a = Effect.ofFunction vert
    let b = a
    
    let cleanName (file : string) =
        let bytes = System.Text.Encoding.UTF8.GetBytes(file)
        let builder = System.Text.StringBuilder()
        for b in bytes do builder.AppendFormat("{0:X2}", b) |> ignore
        builder.ToString()

        
    let cache = sprintf @"C:\Users\Schorsch\Desktop\%s.txt" (cleanName b.Id)

    let code = 
        let create() =
            let glsl = 
                b
                |> Effect.toModule { depthRange = Range1d(-1.0, 1.0); flipHandedness = false; lastStage = ShaderStage.Fragment; outputs = Map.ofList ["Colors", (typeof<V4d>, 0)] }
                |> ModuleCompiler.compileGLSL430

            File.WriteAllBytes(cache, GLSLShader.pickle glsl)
            glsl
 
        if File.Exists cache then  
            match GLSLShader.tryUnpickle (File.ReadAllBytes cache) with
            | Some glsl -> glsl
            | None -> create()
        else
            create()

    sw.Stop()
    printfn "%s" (string code)

    printfn "took: %A" sw.MicroTime

    0