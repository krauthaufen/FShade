module Loops

open Aardvark.Base
open FShade
open FShade.Tests
open System.Text.RegularExpressions
open NUnit.Framework

type Vertex =
    { [<Position>] pos: V4f }

[<Test>]
let ``For loop int32``() =
    Setup.Run()

    let frag () =
        fragment {
            let mutable res = 0
            for i0 = 0 to 1 do        res <- res + i0
            for i1 = 1 downto 0 do    res <- res + i1
            for i2 in 0 .. 1 do       res <- res + i2
            for i3 in 0 .. 2 .. 4 do  res <- res + i3
            for i4 in 4 .. -2 .. 0 do res <- res + i4
            return res
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        Regex.Escape "for(int i0 = 0; (i0 < 2); i0++)"
        Regex.Escape "for(int i1 = 1; (i1 >= 0); i1--)"
        Regex.Escape "for(int i2 = 0; (i2 < 2); i2++)"
        Regex.Escape "for(int i3 = 0; (i3 < 5); i3 = (i3 + 2))"
        Regex.Escape "for(int i4 = 4; (i4 >= 0); i4 = (i4 + -2))"
    ]

[<Test>]
let ``For loop unrolled int32``() =
    Setup.Run()

    let frag () =
        fragment {
            let mutable res = 0
            for i0 = unroll 0 to 1 do        res <- res + i0
            for i1 = 1 downto unroll 0 do    res <- res + i1
            for i2 in unroll 0 .. 1 do       res <- res + i2
            for i3 in unroll 0 .. 2 .. 4 do  res <- res + i3
            for i4 in unroll 4 .. -2 .. 0 do res <- res + i4
            return res
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        [
            "res = (res + 0);"
            "res = (res + 1);"
            "res = (res + 1);"
            "res = (res + 0);"
            "res = (res + 0);"
            "res = (res + 1);"
            "res = (res + 0);"
            "res = (res + 2);"
            "res = (res + 4);"
            "res = (res + 4);"
            "res = (res + 2);"
            "res = (res + 0);"
        ]
        |> List.map Regex.Escape
        |> String.concat @"\s*"
    ]

[<Test>]
let ``For loop int32 unroll with match`` () =
    Setup.Run()

    let frag (switch : int) (v : Vertex) =
        fragment {
            let mutable res = V3f.OOO

            for i in unroll 0 .. 5 do
                let p =
                    match i with
                    | 0 -> v.pos.XYZ
                    | 1 -> v.pos.XZY
                    | 2 -> v.pos.YZX
                    | 3 -> v.pos.YXZ
                    | 4 -> v.pos.ZYX
                    | _ -> v.pos.ZXY
                //let p = if i = 0 then v.pos.XYZ
                //        else if i = 1 then v.pos.XZY
                //        else if i = 2 then v.pos.YZX
                //        else if i = 3 then v.pos.YXZ
                //        else if i = 4 then v.pos.ZYX
                //        else v.pos.ZXY
                res <- res + p
            return V4f(res, 1.0f)
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction (frag 1) ] [
        @"res = \(res \+ fs_Positions\.xyz\);\s*" +
        @"res = \(res \+ fs_Positions\.xzy\);\s*" +
        @"res = \(res \+ fs_Positions\.yzx\);\s*" +
        @"res = \(res \+ fs_Positions\.yxz\);\s*" +
        @"res = \(res \+ fs_Positions\.zyx\);\s*" +
        @"res = \(res \+ fs_Positions\.zxy\);"
    ]

[<Test>]
let ``For loop float32``() =
    Setup.Run()

    let frag () =
        fragment {
            let mutable res = 0.0f
            for i0 = 0.0f to 1.0f do           res <- res + i0
            for i1 in 0.0f .. 1.0f do          res <- res + i1
            for i2 in 0.0f .. 0.5f .. 2.0f do  res <- res + i2
            for i3 in 2.0f .. -0.5f .. 0.0f do res <- res + i3
            return res
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        Regex.Escape "for(float i0 = 0.0; (i0 <= 1.0); i0++)"
        Regex.Escape "for(float i1 = 0.0; (i1 <= 1.0); i1++)"
        Regex.Escape "for(float i2 = 0.0; (i2 <= 2.0); i2 = (i2 + 0.5))"
        Regex.Escape "for(float i3 = 2.0; (i3 >= 0.0); i3 = (i3 + -0.5))"
    ]

[<Test>]
let ``For loop unrolled float32``() =
    Setup.Run()

    let frag () =
        fragment {
            let mutable res = 0.0f
            for i0 = unroll 0.0f to 1.0f do           res <- res + i0
            for i1 in 0.0f .. unroll 1.0f do          res <- res + i1
            for i2 in 0.0f .. unroll 0.5f .. 2.0f do  res <- res + i2
            for i3 in unroll 2.0f .. -0.5f .. 0.0f do res <- res + i3
            return res
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        [
            "res = (res + 0.0);"
            "res = (res + 1.0);"
            "res = (res + 0.0);"
            "res = (res + 1.0);"
            "res = (res + 0.0);"
            "res = (res + 0.5);"
            "res = (res + 1.0);"
            "res = (res + 1.5);"
            "res = (res + 2.0);"
            "res = (res + 2.0);"
            "res = (res + 1.5);"
            "res = (res + 1.0);"
            "res = (res + 0.5);"
            "res = (res + 0.0);"
        ]
        |> List.map Regex.Escape
        |> String.concat @"\s*"
    ]

[<Test>]
let ``For loop uint32``() =
    Setup.Run()

    let frag () =
        fragment {
            let mutable res = 0u
            for i0 = 0u to 1u do        res <- res + i0
            for i1 in 0u .. 1u do       res <- res + i1
            for i2 in 0u .. 2u .. 4u do res <- res + i2
            return res
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        Regex.Escape "for(uint i0 = 0u; (i0 < 2u); i0++)"
        Regex.Escape "for(uint i1 = 0u; (i1 < 2u); i1++)"
        Regex.Escape "for(uint i2 = 0u; (i2 < 5u); i2 = (i2 + 2u))"
    ]

[<Test>]
let ``For loop unrolled uint32``() =
    Setup.Run()

    let frag () =
        fragment {
            let mutable res = 0u
            for i0 = 0u to unroll 1u do        res <- res + i0
            for i1 in unroll 0u .. 1u do       res <- res + i1
            for i2 in 0u .. 2u .. unroll 4u do res <- res + i2
            return res
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        [
            "res = (res + 0u);"
            "res = (res + 1u);"
            "res = (res + 0u);"
            "res = (res + 1u);"
            "res = (res + 0u);"
            "res = (res + 2u);"
            "res = (res + 4u);"
        ]
        |> List.map Regex.Escape
        |> String.concat @"\s*"
    ]