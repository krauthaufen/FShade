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