namespace FShade.Debug

open System.IO
open System.Text
open FShade
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Interactive.Shell

module EffectCompiler =
    
    let err = new StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(new StringBuilder())
    let errStream = new StringWriter(err)

    let private startupCode =
        String.concat "\r\n" [
            sprintf "System.Environment.CurrentDirectory <- @\"%s\"" System.Environment.CurrentDirectory 
            "#r @\"Aardvark.Base.dll\" "
            "#r @\"Aardvark.Base.TypeProviders.dll\" "
            "#r @\"Aardvark.Base.FSharp.dll\" "
            "#r @\"FShade.Imperative.dll\" "
            "#r @\"FShade.Core.dll\" "
            "#r @\"FShade.GLSL.dll\" "
            
            "open Microsoft.FSharp.Quotations"
            "open FShade"

            "let mutable currentEffect = None"

            "type ComposeBuilder() ="
            "   member x.Bind(f : 'a -> Expr<'b>, g : unit -> list<Effect>) = (Effect.ofFunction f) :: g()"
            "   member x.Return (()) = []"
            "   member x.Zero() = []"
            "   member x.Delay(f : unit -> list<Effect>) = f"
            "   member x.Combine(l : list<Effect>, r : unit -> list<Effect>) = l @ r()"
            "   member x.Run(r : unit -> list<Effect>) = currentEffect <- Some (Effect.compose (r()))"

            "let compose = ComposeBuilder()"
        ]

    let private fsi =
        let argv = [| "FsiAnyCPU.exe";  "--noninteractive" |]
        let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
        FsiEvaluationSession.Create(fsiConfig, argv, inStream, outStream, errStream) 

    open System

    let currentProp = 
        match fsi.EvalInteractionNonThrowing startupCode with
            | Choice1Of2 (), _ -> 

                let rec all (t : Type) =
                    let all = t.GetNestedTypes() |> Array.collect all
                    Array.append [| t |] all

                let allTypes = fsi.DynamicAssembly.GetTypes() |> Array.collect all
                allTypes |> Array.tryPick (fun t ->
                    let prop = t.GetProperty("currentEffect")
                    if isNull prop || prop.PropertyType <> typeof<Option<Effect>> then
                        None
                    else
                        Some prop
                )


            | _, err -> 
                None

    let mutable private index = 2
                                  
    let private toPascalCase (str : string) =
        if str.Length > 0 then
            if str.[0] >= 'a' || str.[0] <= 'Z' then
                str.Substring(0, 1).ToUpper() + str.Substring(1)
            else
                str
        else
            str

    let evalScript fileName  =
        lock fsi (fun () ->
            let value, errors = fsi.EvalScriptNonThrowing fileName
            match value with
                | Choice1Of2 () ->
                    match currentProp.Value.GetValue(null) |> unbox<Option<Effect>> with
                        | Some e -> Choice1Of2 e
                        | None -> Choice2Of2 errors
                | _ -> 
                    Choice2Of2 errors
        )