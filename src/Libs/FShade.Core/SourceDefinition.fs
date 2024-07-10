namespace FShade

open System
open System.Reflection
open FSharp.Quotations
open Aardvark.Base

type SourceDefinition =
    {
        // Original expression
        Expression : Expr

        // Input types of the function
        Inputs : Type list

        // Applied arguments of the function
        Arguments : obj list
    }

module SourceDefinition =

    [<AutoOpen>]
    module private ClosureUtilities =

        let private closureTypes =
            [| typedefof<OptimizedClosures.FSharpFunc<_, _, _>>
               typedefof<OptimizedClosures.FSharpFunc<_, _, _, _>>
               typedefof<OptimizedClosures.FSharpFunc<_, _, _, _, _>>
               typedefof<OptimizedClosures.FSharpFunc<_, _, _, _, _>>
               typedefof<OptimizedClosures.FSharpFunc<_, _, _, _, _, _>> |]

        [<return: Struct>]
        let (|Closure|_|) ((field, value) : FieldInfo * obj) =
            let ft = field.Type

            if ft.IsGenericType then
                let def = ft.GetGenericTypeDefinition()
                if closureTypes |> Array.contains def then
                    ValueSome value
                else
                    ValueNone
            else
                ValueNone

        [<return: Struct>]
        let (|Self|_|) (field : FieldInfo) =
            if field.Name = "self@" then ValueSome ()
            else ValueNone

    // Gets the applied arguments from a shader function with parameters
    let private getArguments (shaderFunction : 'a -> 'b) =

        let rec get (accum : obj list) (func : obj) =
            let typ = func.GetType()

            let arguments =
                typ.GetFields(BindingFlags.Public ||| BindingFlags.Instance)
                |> List.ofArray
                |> List.map (fun f ->
                    f, f.GetValue func
                )

            match arguments with
            | [Closure c; (_, arg)] ->
                get (arg :: accum) c

            | (Self, _) :: args | args ->
                (args |> List.map snd) @ accum

        try
            get [] shaderFunction
        with
        | exn ->
            Log.warn "[FShade] Failed to retrieve arguments of shader function: %s" exn.Message
            []

    let create (inputs : Type list) (shaderFunc : 'a -> 'b) (expr : Expr) =
        let arguments = getArguments shaderFunc
        { Expression = expr; Inputs = inputs; Arguments = arguments }

    let ofExpr (inputs : Type list) (expr : Expr) =
        { Expression = expr; Inputs = inputs; Arguments = [] }