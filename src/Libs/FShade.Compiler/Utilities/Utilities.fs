namespace FShade.Utils

open System
open System.Threading
open System.Runtime.CompilerServices

module String =
    let private lineBreak = System.Text.RegularExpressions.Regex("\r\n")

    /// <summary>
    /// indents a string with the given level-count. uses 4 spaces per level.
    /// </summary>
    let indent (n : int) (str : string) =
        let lines = lineBreak.Split str
        let prefix = System.String(' ', 4*n)
        lines |> Seq.map (fun l -> if l.Length > 0 then prefix + l else l) |> String.concat "\r\n"

    /// <summary>
    /// indents a string with the given level-count. uses 4 spaces per level.
    /// </summary>
    let linePrefix (prefix : string) (str : string) =
        let lines = lineBreak.Split str
        lines |> Seq.map (fun l -> if l.Length > 0 then prefix + l else l) |> String.concat "\r\n"


    /// <summary>
    /// counts the lines in a string and returns the total count.
    /// </summary>
    let lineCount (str : string) =
        let lines = lineBreak.Split str
        (lines |> Array.filter(fun l -> l.Length > 0)).Length

[<AutoOpen>]
module MyFSharpType =
    open System.Reflection
    open Microsoft.FSharp.Reflection

    let private anyFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance

    module FSharpTypeExt = 

        let IsTuple(t : Type) =
            FSharpType.IsTuple(t)

        let GetTupleElements(t : Type) =
            FSharpType.GetTupleElements(t)

        let MakeTupleType(types) =
            FSharpType.MakeTupleType types


        let IsFunction(t : Type) =
            FSharpType.IsTuple(t)

        let GetFunctionElements(t : Type) =
            FSharpType.GetFunctionElements(t)

        let MakeFunctionType(ta, tr) =
            FSharpType.MakeFunctionType(ta, tr)


        let IsRecord(t : Type) =
            FSharpType.IsRecord(t, anyFlags)

        let GetRecordFields(t : Type) =
            FSharpType.GetRecordFields(t, anyFlags)

        let IsUnion(t : Type) =
            FSharpType.IsUnion(t, anyFlags)

        let GetUnionCases(t : Type) =
            FSharpType.GetUnionCases(t, anyFlags)