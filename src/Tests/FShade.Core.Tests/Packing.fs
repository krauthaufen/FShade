module Packing

// The pack/unpack helpers are GLSL intrinsics in shaders; their F# bodies run on the CPU and must
// produce the same bit patterns as GLSL (first component in the least significant bits, snorm as
// two's complement scaled by 32767 / 127). See: https://github.com/krauthaufen/FShade/issues/40

open FsUnit
open NUnit.Framework
open Aardvark.Base
open FShade

let private shouldBeHex (expected : uint32) (actual : uint32) =
    if expected <> actual then
        failwithf "expected 0x%08X but got 0x%08X" expected actual

[<Test>]
let ``packUnorm2x16 matches GLSL``() =
    packUnorm2x16 (V2f(1.0f, 0.0f)) |> shouldBeHex 0x0000FFFFu
    packUnorm2x16 (V2f(0.0f, 1.0f)) |> shouldBeHex 0xFFFF0000u
    packUnorm2x16 (V2f(0.25f, 0.75f)) |> shouldBeHex 0xBFFF4000u
    packUnorm2x16 (V2f(-3.0f, 7.0f)) |> shouldBeHex 0xFFFF0000u // clamped

[<Test>]
let ``packSnorm2x16 matches GLSL``() =
    packSnorm2x16 (V2f(-1.0f, 1.0f)) |> shouldBeHex 0x7FFF8001u
    packSnorm2x16 (V2f(0.25f, -0.75f)) |> shouldBeHex 0xA0012000u
    packSnorm2x16 (V2f(0.0f, 0.0f)) |> shouldBeHex 0x00000000u
    packSnorm2x16 (V2f(2.0f, -2.0f)) |> shouldBeHex 0x80017FFFu // clamped

[<Test>]
let ``packUnorm4x8 matches GLSL``() =
    packUnorm4x8 (V4f(1.0f, 0.0f, 0.0f, 0.0f)) |> shouldBeHex 0x000000FFu
    packUnorm4x8 (V4f(0.0f, 0.0f, 0.0f, 1.0f)) |> shouldBeHex 0xFF000000u
    packUnorm4x8 (V4f(0.25f, 0.75f, 0.2f, 0.6f)) |> shouldBeHex 0x9933BF40u

[<Test>]
let ``packSnorm4x8 matches GLSL``() =
    packSnorm4x8 (V4f(1.0f, -1.0f, 0.0f, 0.25f)) |> shouldBeHex 0x2000817Fu
    packSnorm4x8 (V4f(-2.0f, 2.0f, 0.0f, 0.0f)) |> shouldBeHex 0x00007F81u // clamped

[<Test>]
let ``unpackUnorm2x16 matches GLSL``() =
    unpackUnorm2x16 0x0000FFFFu |> should equal (V2f(1.0f, 0.0f))
    unpackUnorm2x16 0xFFFF0000u |> should equal (V2f(0.0f, 1.0f))
    unpackUnorm2x16 0xBFFF4000u |> should equal (V2f(16384.0f / 65535.0f, 49151.0f / 65535.0f))

[<Test>]
let ``unpackSnorm2x16 matches GLSL``() =
    unpackSnorm2x16 0x7FFF8001u |> should equal (V2f(-1.0f, 1.0f))
    unpackSnorm2x16 0xA0012000u |> should equal (V2f(8192.0f / 32767.0f, -24575.0f / 32767.0f))
    unpackSnorm2x16 0x80008000u |> should equal (V2f(-1.0f, -1.0f)) // -32768 is clamped

[<Test>]
let ``unpackUnorm4x8 matches GLSL``() =
    unpackUnorm4x8 0x000000FFu |> should equal (V4f(1.0f, 0.0f, 0.0f, 0.0f))
    unpackUnorm4x8 0xFF000000u |> should equal (V4f(0.0f, 0.0f, 0.0f, 1.0f))
    unpackUnorm4x8 0x9933BF40u |> should equal (V4f(64.0f / 255.0f, 191.0f / 255.0f, 51.0f / 255.0f, 153.0f / 255.0f))

[<Test>]
let ``unpackSnorm4x8 matches GLSL``() =
    unpackSnorm4x8 0x2000817Fu |> should equal (V4f(1.0f, -1.0f, 0.0f, 32.0f / 127.0f))
    unpackSnorm4x8 0x80808080u |> should equal (V4f(-1.0f, -1.0f, -1.0f, -1.0f)) // -128 is clamped

[<Test>]
let ``Round trips``() =
    let rand = RandomSystem(42)

    for _ in 1 .. 1000 do
        let u2 = V2f(rand.UniformFloat(), rand.UniformFloat())
        let s2 = u2 * 2.0f - 1.0f
        let u4 = V4f(rand.UniformFloat(), rand.UniformFloat(), rand.UniformFloat(), rand.UniformFloat())
        let s4 = u4 * 2.0f - 1.0f

        Vec.Distance(unpackUnorm2x16 (packUnorm2x16 u2), u2) |> should be (lessThanOrEqualTo (1.0f / 65535.0f))
        Vec.Distance(unpackSnorm2x16 (packSnorm2x16 s2), s2) |> should be (lessThanOrEqualTo (1.0f / 32767.0f))
        Vec.Distance(unpackUnorm4x8 (packUnorm4x8 u4), u4) |> should be (lessThanOrEqualTo (1.0f / 255.0f))
        Vec.Distance(unpackSnorm4x8 (packSnorm4x8 s4), s4) |> should be (lessThanOrEqualTo (1.0f / 127.0f))
