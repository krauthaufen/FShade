namespace FShade

open System
open System.Reflection
open Aardvark.Base
open Aardvark.Base.TypeInfo.Patterns

[<AutoOpen>]
module ReflectionPatterns =
    let (|VectorSwizzle|_|) (p : MemberInfo) =
        match p.DeclaringType with
            | Vector -> let mutable success = true
                        for c in p.Name.ToCharArray() do
                            if not <| Set.contains c Aardvark.Base.TypeInfo.VectorFields then
                                success <- false
                        if success then
                            VectorSwizzle(p.Name) |> Some
                        else
                            None
            | _ -> None


    let matrixElementRx = System.Text.RegularExpressions.Regex("M(?<x>[0-3])(?<y>[0-3])")
    let matrixColRx = System.Text.RegularExpressions.Regex("C(?<n>[0-3])")
    let matrixRowRx = System.Text.RegularExpressions.Regex("R(?<n>[0-3])")

    let (|MatrixElement|_|) (p : MemberInfo) =
        match p.DeclaringType with
            | Matrix -> let m = matrixElementRx.Match p.Name
                        if m.Success then
                            MatrixElement(m.Groups.["x"].Value |> Int32.Parse, m.Groups.["y"].Value |> Int32.Parse) |> Some
                        else
                            None
            | _ -> None

    let (|MatrixCol|_|) (p : MemberInfo) =
        match p.DeclaringType with
            | MatrixOf (dim, bt) -> 
                let m = matrixColRx.Match p.Name
                if m.Success then
                    let ci = m.Groups.["n"].Value |> Int32.Parse
                    Some(bt, dim, ci)
                else
                    None
            | _ -> None  

    let (|MatrixRow|_|) (p : MemberInfo) =
        match p.DeclaringType with
            | MatrixOf (dim, bt) -> 
                let m = matrixRowRx.Match p.Name
                if m.Success then
                    let ri = m.Groups.["n"].Value |> Int32.Parse
                    Some(bt, dim, ri)
                else
                    None
            | _ -> None  
