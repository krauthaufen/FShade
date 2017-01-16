namespace FShade.Imperative

open System
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Aardvark.Base


type Uniform =
    | Buffer of name : string * fields : list<string * Type>
    | Global of name : string * t : Type

type EntryPoint =
    {
        conditional : Option<string>
        entryName   : string
        inputs      : list<Var>
        outputs     : list<Var>
        uniforms    : list<Uniform>
        arguments   : list<Var>
        body        : Expr
    }

type Module = { entries : list<EntryPoint> } with
    member x.uniforms =
        let globalTable = Dictionary.empty
        let bufferTable : Dictionary<string, Dictionary<string, Type>> = Dictionary.empty
        for e in x.entries do
            for u in e.uniforms do
                match u with
                    | Global(n, t) -> 
                        match globalTable.TryGetValue n with
                            | (true, ot) ->
                                if ot <> t then failwithf "[FShade] conflicting uniform type for %A (%A vs %A)" n ot t
                            | _ ->
                                globalTable.[n] <- t

                    | Buffer(n, fields) ->
                        match bufferTable.TryGetValue n with
                            | (true, existing) -> 
                                for (n, t) in fields do
                                    match existing.TryGetValue n with
                                        | (true, ot) ->
                                            if ot <> t then failwithf "[FShade] conflicting uniform type for %A (%A vs %A)" n ot t
                                        | _ ->
                                           existing.[n] <- t 
                                    ()

                            | _ ->
                                bufferTable.[n] <- Dictionary.ofList fields

        List.concat [
            globalTable |> Dictionary.toList |> List.map Global
            bufferTable |> Dictionary.toList |> List.map (fun (n,fs) -> Buffer(n, Dictionary.toList fs))
        ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EntryPoint =

    let ofLambda (name : string) (e : Expr) =
        match e with
            | Lambdas(args, body) ->
                let args = 
                    match List.concat args with
                        | [v] when v.Type = typeof<unit> -> []
                        | a -> a
                {
                    conditional = None
                    entryName = name
                    inputs = []
                    outputs = []
                    uniforms = []
                    arguments = args
                    body = body
                }
            | e ->
                {
                    conditional = None
                    entryName = name
                    inputs = []
                    outputs = []
                    uniforms = []
                    arguments = []
                    body = e
                }
                
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Module =

    let ofLambda (name : string) (e : Expr) =
        { entries = [EntryPoint.ofLambda name e] }

    let ofLambdas (l : list<string * Expr>) =
        { entries = l |> List.map (uncurry EntryPoint.ofLambda) }
