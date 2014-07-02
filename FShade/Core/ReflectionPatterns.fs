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
    let (|MatrixElement|_|) (p : MemberInfo) =
        match p.DeclaringType with
            | Matrix -> let m = matrixElementRx.Match p.Name
                        if m.Success then
                            MatrixElement(m.Groups.["x"].Value |> Int32.Parse, m.Groups.["y"].Value |> Int32.Parse) |> Some
                        else
                            None
            | _ -> None

