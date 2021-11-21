module Program

open System
open System.Reflection
open System.IO
open Aardvark.Base
open FShade
open FShade.Imperative
open Microsoft.FSharp.Quotations


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
    2.0 * a

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

[<EntryPoint>]
let main args = 
    Aardvark.Base.Aardvark.Init()

    let e = Effect.ofFunction vert

    let e1 = 
        use ms = new System.IO.MemoryStream()
        e |> Effect.serialize ms
        ms.Position <- 0L
        Effect.deserialize ms





    let body = e.VertexShader.Value.shaderBody

    use ms = new MemoryStream()
    Expr.serialize ms body

    ms.Position <- 0L
    let res = Expr.deserialize ms

    let h1 = Expr.computeHash (vert Unchecked.defaultof<_>)
    let h2 = Expr.computeHash body
    let h3 = Expr.computeHash res

    
    printfn "%A" h1
    printfn "%A" h2
    printfn "%A" h3

    //ConstantFolding.Broken()
    0