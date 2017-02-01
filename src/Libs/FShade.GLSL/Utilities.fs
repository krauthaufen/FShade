namespace FShade.GLSL.Utilities


open System
open System.Reflection
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape


open FShade
open FShade.Imperative

module internal MethodTable =
    let rec tryGetMethod (e : Expr) =
        match e with
            | Call(_,mi,_) -> Some mi
            | PropertyGet(_,pi,_) -> Some pi.GetMethod

            | ShapeVar _ -> None
            | ShapeLambda(_,b) -> tryGetMethod b
            | ShapeCombination(_,args) -> args |> List.tryPick tryGetMethod

    let getMethod (e : Expr) =
        e |> tryGetMethod |> Option.get

    let ofList (list : list<'a * list<MethodInfo>>) =
        let store = Dictionary<MethodInfo, 'a>()

        for (value, mis) in list do
            for mi in mis do
                store.[mi] <- value
            
        fun (mi : MethodInfo) ->
            match store.TryGetValue mi with
                | (true, v) -> 
                    Some v

                | _ ->
                    if mi.IsGenericMethod then
                        match store.TryGetValue (mi.GetGenericMethodDefinition()) with
                            | (true, v) -> Some v
                            | _ -> None
                    else
                        None

[<AutoOpen>]
module internal Operators =
    let exactly (e : Expr) =
        MethodTable.getMethod e

    let generic (e : Expr) =
        let mi = MethodTable.getMethod e
        if mi.IsGenericMethod then mi.GetGenericMethodDefinition()
        else mi

module internal String =
    let private lineBreak = System.Text.RegularExpressions.Regex("\r\n")

    let indent (str : string) =
        let lines = lineBreak.Split str
        let prefix = "    "
        lines |> Seq.map (fun l -> if l.Length > 0 then prefix + l else l) |> String.concat "\r\n"
