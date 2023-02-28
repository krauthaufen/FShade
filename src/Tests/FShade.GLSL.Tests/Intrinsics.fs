module Intrinsics

open System.Text.RegularExpressions
open Aardvark.Base
open FShade
open NUnit.Framework
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
        foo : V4d
        what : V4i
        whatu : V4ui
    }

type UniformScope with
    member x.SomeUniform : V3d = uniform?SomeUniform

[<ReflectedDefinition>]
let getVec() =
    V4d(uniform.SomeUniform, 1.0)

[<ReflectedDefinition>]
let getVeci() =
    V4i(V3i uniform.SomeUniform, 1)

[<ReflectedDefinition>]
let assertV2d (v : V2d) = v

[<ReflectedDefinition>]
let assertV3d (v : V3d) = v

[<ReflectedDefinition>]
let assertV4d (v : V4d) = v

[<ReflectedDefinition>]
let assertDbl (d : float) = d

[<ReflectedDefinition>]
let assertInt (i : int) = i

[<Test>]
let ``Matrix constructors``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m33 = M33d(v.pos.X)
            let m44 = M44d(m33)
            let _ = M33d(m44)
            return v.pos
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Matrix elements``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m22 = M22d(v.pos.X)
            let m23 = M23d(v.pos.X)
            let m33 = M33d(v.pos.X)
            let m34 = M34d(v.pos.X)
            let m44 = M44d(m33)
            let _ = m22.[v.what.X, v.what.Y]
            let _ = m23.[v.what.X, v.what.Y]
            let _ = m33.[v.what.X, v.what.Y]
            let _ = m34.[v.what.X, v.what.Y]
            let _ = m44.[v.what.X, v.what.Y]
            let _ = m44.M03
            let _ = m44.M13
            let _ = m44.M23
            return v.pos
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Matrix columns / rows``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m33 = M33d(v.pos.X)
            let m34 = M34d(v.pos.X)
            let m44 = M44d(m33)
            let _ = assertV3d <| m33.C0
            let _ = assertV3d <| m33.Column(v.what.X)
            let _ = assertV3d <| m34.C1
            let _ = assertV3d <| m34.Column(v.what.X)
            let _ = assertV4d <| m34.R2
            let _ = assertV4d <| m34.Row(v.what.X)
            let _ = assertV4d <| m44.R3
            let _ = assertV4d <| m44.Row(v.what.X)
            return v.pos
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Vector constructors``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = V3d(v.pos.X)
            let _ = V4d(v.pos.XY, v.pos.ZW)
            let _ = V4d(v.pos.X, v.pos.YZW)
            let _ = V4d(v.pos.XYZ)
            let _ = V4d(v.pos.XY)
            let _ = V2d(v.pos.XYZ)
            return v.pos
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Vector conversion``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = v2d v.what
            let _ = v3d v.what
            let _ = v4d v.what
            let _ = V4d.op_Explicit v.what
            let _ = v4d v.c
            let _ = v2i v.c
            let _ = v3i v.c
            let _ = v4i v.c
            return v.c
        }

    GLSL.shouldCompile[Effect.ofFunction fs]

[<Test>]
let ``Inverse lerp``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let mutable a = 0.0
            a <- a + Fun.InvLerp(int8  v.c.X, int8  v.c.Y, int8  v.c.Z)
            a <- a + Fun.InvLerp(int16 v.c.X, int16 v.c.Y, int16 v.c.Z)
            a <- a + Fun.InvLerp(int32 v.c.X, int32 v.c.Y, int32 v.c.Z)
            a <- a + Fun.InvLerp(v.c.X - v.c.Y, v.c.Y, v.c.Z)
            a <- a + invLerp (int8  v.c.Y) (int8  v.c.Z) (int8  v.c.X)
            a <- a + invLerp (int16 v.c.Y) (int16 v.c.Z) (int16 v.c.X)
            a <- a + invLerp (int32 v.c.Y) (int32 v.c.Z) (int32 v.c.X)
            a <- a + invLerp v.c.Y v.c.Z (v.c.X - v.c.Y)
            let _ = a

            let mutable a = V2d.Zero
            a <- a + Fun.InvLerp(V2i v.c.XY, V2i v.c.YZ, V2i v.c.XZ)
            a <- a + Fun.InvLerp(V2d v.c.XY, V2d v.c.YZ, V2d v.c.XZ)
            a <- a + invLerp (V2i v.c.YZ) (V2i v.c.XZ) (V2i v.c.XY)
            a <- a + invLerp (V2d v.c.YZ) (V2d v.c.XZ) (V2d v.c.XY)
            let _ = a

            let mutable a = V3d.Zero
            a <- a + Fun.InvLerp(V3i v.c.XYZ, V3i v.pos.XYZ, V3i v.foo.XYZ)
            a <- a + Fun.InvLerp(V3d v.c.XYZ, V3d v.pos.XYZ, V3d v.foo.XYZ)
            a <- a + invLerp (V3i v.pos.XYZ) (V3i v.foo.XYZ) (V3i v.c.XYZ)
            a <- a + invLerp (V3d v.pos.XYZ) (V3d v.foo.XYZ) (V3d v.c.XYZ)
            let _ = a

            let mutable a = V4d.Zero
            a <- a + Fun.InvLerp(V4i v.c, V4i v.pos, V4i v.foo)
            a <- a + Fun.InvLerp(v.c, v.pos, v.foo)
            a <- a + invLerp (V4i v.pos) (V4i v.foo) (V4i v.c)
            a <- a + invLerp v.pos v.foo v.c
            let _ = a

            let mutable a = 0.0f
            a <- a + Fun.InvLerp(float32 v.c.X, float32 v.c.Y, float32 v.c.Z)
            a <- a + invLerp (float32 v.c.Y) (float32 v.c.Z) (float32 v.c.X)
            let _ = a

            let mutable a = V2f.Zero
            a <- a + Fun.InvLerp(V2f v.c.XY, V2f v.pos.XY, V2f v.foo.XY)
            a <- a + invLerp (V2f v.pos.XY) (V2f v.foo.XY) (V2f v.c.XY)
            let _ = a

            let mutable a = V3f.Zero
            a <- a + Fun.InvLerp(V3f v.c.XYZ, V3f v.pos.XYZ, V3f v.foo.XYZ)
            a <- a + invLerp (V3f v.pos.XYZ) (V3f v.foo.XYZ) (V3f v.c.XYZ)
            let _ = a

            let mutable a = V4f.Zero
            a <- a + Fun.InvLerp(V4f v.c, V4f v.pos, V4f v.foo)
            a <- a + invLerp (V4f v.pos) (V4f v.foo) (V4f v.c)
            let _ = a

            let _ = invLerp (getVec()) V4d.Zero V4d.One
            let _ = invLerp (getVec().XYZ) V3d.Zero V3d.One
            let _ = invLerp (getVec().XY) V2d.Zero V2d.One
            let _ = invLerp (getVec().X) 0.0 1.0

            let _ = Fun.InvLerp(V4d.One, getVec(), V4d.Zero)
            let _ = Fun.InvLerp(V3d.One, getVec().XYZ, V3d.Zero)
            let _ = Fun.InvLerp(V2d.One, getVec().XY, V2d.Zero)
            let _ = Fun.InvLerp(1.0, getVec().X, 0.0)

            let _ = invLerp (V4i (getVec())) V4i.Zero V4i.One
            let _ = invLerp (V3i (getVec().XYZ)) V3i.Zero V3i.One
            let _ = invLerp (V2i (getVec().XY)) V2i.Zero V2i.One
            let _ = invLerp (int <| getVec().X) 0 1

            let _ = Fun.InvLerp(V4i.One, V4i (getVec()), V4i.Zero)
            let _ = Fun.InvLerp(V3i.One, V3i (getVec().XYZ), V3i.Zero)
            let _ = Fun.InvLerp(V2i.One, V2i (getVec().XY), V2i.Zero)
            let x = Fun.InvLerp(1, int <| getVec().X, 0)

            return lerp v.c V4d.Zero x
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 17]

[<Test>]
let ``Inverse lerp constant``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = V2d(1.5) |> invLerp (V2d(1.0)) (V2d(2.0))
            let _ = V2i(2) |> invLerp (V2i(1)) (V2i(3))
            let _ = 1.5 |> invLerp 1.0 2.0
            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] [
        Regex.Escape "vec2(0.5, 0.5);", 2
        Regex.Escape "0.5;", 1
    ]

[<Test>]
let ``Lerp integer overloads``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let mutable a = int8 0
            a <- a + Fun.Lerp(v.c.X, int8 v.c.X, int8 v.c.X)
            a <- a + Fun.Lerp(float32 v.c.X, int8 v.c.X, int8 v.c.X)
            a <- a + lerp (int8 v.c.X) (int8 v.c.X) v.c.X
            a <- a + lerp (int8 v.c.X) (int8 v.c.X) (float32 v.c.X)
            let _ = a

            let mutable a = int16 0
            a <- a + Fun.Lerp(v.c.X, int16 v.c.X, int16 v.c.X)
            a <- a + Fun.Lerp(float32 v.c.X, int16 v.c.X, int16 v.c.X)
            a <- a + lerp (int16 v.c.X) (int16 v.c.X) v.c.X
            a <- a + lerp (int16 v.c.X) (int16 v.c.X) (float32 v.c.X)
            let _ = a

            let mutable a = int32 0
            a <- a + Fun.Lerp(v.c.X, int32 v.c.X, int32 v.c.X)
            a <- a + Fun.Lerp(float32 v.c.X, int32 v.c.X, int32 v.c.X)
            a <- a + lerp (int32 v.c.X) (int32 v.c.X) v.c.X
            a <- a + lerp (int32 v.c.X) (int32 v.c.X) (float32 v.c.X)
            let _ = a

            let mutable a = uint8 0
            a <- a + Fun.Lerp(v.c.X, uint8 v.c.X, uint8 v.c.X)
            a <- a + Fun.Lerp(float32 v.c.X, uint8 v.c.X, uint8 v.c.X)
            a <- a + lerp (uint8 v.c.X) (uint8 v.c.X) v.c.X
            a <- a + lerp (uint8 v.c.X) (uint8 v.c.X) (float32 v.c.X)
            let _ = a

            let mutable a = uint16 0
            a <- a + Fun.Lerp(v.c.X, uint16 v.c.X, uint16 v.c.X)
            a <- a + Fun.Lerp(float32 v.c.X, uint16 v.c.X, uint16 v.c.X)
            a <- a + lerp (uint16 v.c.X) (uint16 v.c.X) v.c.X
            a <- a + lerp (uint16 v.c.X) (uint16 v.c.X) (float32 v.c.X)
            let _ = a

            let mutable a = uint32 0
            a <- a + Fun.Lerp(v.c.X, uint32 v.c.X, uint32 v.c.X)
            a <- a + Fun.Lerp(float32 v.c.X, uint32 v.c.X, uint32 v.c.X)
            a <- a + lerp (uint32 v.c.X) (uint32 v.c.X) v.c.X
            a <- a + lerp (uint32 v.c.X) (uint32 v.c.X) (float32 v.c.X)
            let _ = a

            let mutable a = V2i.Zero
            a <- a + Fun.Lerp(v.c.X, V2i(v.c.XY), V2i(v.c.XY))
            a <- a + Fun.Lerp(float32 v.c.X, V2i(v.c.XY), V2i(v.c.XY))
            a <- a + Fun.Lerp(v.c.XY, V2i(v.c.XY), V2i(v.c.XY))
            a <- a + Fun.Lerp(V2f(v.c.XY), V2i(v.c.XY), V2i(v.c.XY))
            a <- a + lerp (V2i(v.c.XY)) (V2i(v.c.XY)) v.c.X
            a <- a + lerp (V2i(v.c.XY)) (V2i(v.c.XY)) (float32 v.c.X)
            a <- a + lerp (V2i(v.c.XY)) (V2i(v.c.XY)) v.c.XY
            a <- a + lerp (V2i(v.c.XY)) (V2i(v.c.XY)) (V2f(v.c.XY))
            let _ = a

            let mutable a = V3i.Zero
            a <- a + Fun.Lerp(v.c.X, V3i(v.c.XYZ), V3i(v.c.XYZ))
            a <- a + Fun.Lerp(float32 v.c.X, V3i(v.c.XYZ), V3i(v.c.XYZ))
            a <- a + Fun.Lerp(v.c.XYZ, V3i(v.c.XYZ), V3i(v.c.XYZ))
            a <- a + Fun.Lerp(V3f(v.c.XYZ), V3i(v.c.XYZ), V3i(v.c.XYZ))
            a <- a + lerp (V3i(v.c.XYZ)) (V3i(v.c.XYZ)) v.c.X
            a <- a + lerp (V3i(v.c.XYZ)) (V3i(v.c.XYZ)) (float32 v.c.X)
            a <- a + lerp (V3i(v.c.XYZ)) (V3i(v.c.XYZ)) v.c.XYZ
            a <- a + lerp (V3i(v.c.XYZ)) (V3i(v.c.XYZ)) (V3f(v.c.XYZ))
            let _ = a

            let mutable a = V4i.Zero
            a <- a + Fun.Lerp(v.c.X, V4i(v.c), V4i(v.c))
            a <- a + Fun.Lerp(float32 v.c.X, V4i(v.c), V4i(v.c))
            a <- a + Fun.Lerp(v.c, V4i(v.c), V4i(v.c))
            a <- a + Fun.Lerp(V4f(v.c), V4i(v.c), V4i(v.c))
            a <- a + lerp (V4i(v.c)) (V4i(v.c)) v.c.X
            a <- a + lerp (V4i(v.c)) (V4i(v.c)) (float32 v.c.X)
            a <- a + lerp (V4i(v.c)) (V4i(v.c)) v.c
            a <- a + lerp (V4i(v.c)) (V4i(v.c)) (V4f(v.c))
            let _ = a

            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["mix"; "round"; "\+ 0\.5"]

[<Test>]
let ``Matrix conversion``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = m22d <| M33d(v.c.X)
            let _ = m23d <| M33d(v.c.X)
            let _ = m33d <| M33d(v.c.X)
            let _ = m34d <| M33d(v.c.X)
            let _ = m44d <| M33d(v.c.X)
            let _ = m22f <| M33d(v.c.X)
            let _ = m23f <| M33d(v.c.X)
            let _ = m33f <| M33d(v.c.X)
            let _ = m34f <| M33d(v.c.X)
            let _ = m44f <| M33d(v.c.X)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["mat2x3"]

[<Test>]
let ``Lerp``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Fun.Lerp(float32 v.c.X, V2f.Zero, V2f.One)
            let _ = Fun.Lerp(v.c.X, V2d.Zero, V2d.One)
            let _ = Fun.Lerp(float32 v.c.X, V2f.Zero, V2f.One)
            let _ = Fun.Lerp(float32 v.c.X, V3f.Zero, V3f.One)
            let _ = Fun.Lerp(v.c.X, V3d.Zero, V3d.One)
            let _ = Fun.Lerp(float32 v.c.X, V3f.Zero, V3f.One)
            let _ = Fun.Lerp(float32 v.c.X, V4f.Zero, V4f.One)
            let _ = Fun.Lerp(v.c.X, V4d.Zero, V4d.One)
            let _ = Fun.Lerp(float32 v.c.X, V4f.Zero, V4f.One)
            let _ = lerp V2i.Zero V2i.One v.c.XY
            let _ = Fun.Lerp(v.c.X, 0, 1)
            let _ = Fun.Lerp(v.c, V4i(v.c), V4i(v.c))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["mix"]

[<Test>]
let ``Exp``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Fun.Exp(v.c.XY)
            let _ = Fun.Exp(V2f(v.c.XY))
            let _ = Fun.Exp(v.c.XYZ)
            let _ = Fun.Exp(V3f(v.c.XYZ))
            let _ = Fun.Exp(v.c)
            let _ = Fun.Exp(V4f(v.c))
            let _ = exp v.c
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["exp"]

[<Test>]
let ``Exp2``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = exp2 v.c.X
            let _ = exp2 v.c.XY
            let _ = exp2 v.c.XYZ
            let _ = exp2 v.c
            let _ = exp2 (float32 v.c.X)
            let _ = exp2 (v2f v.c.XY)
            let _ = exp2 (v3f v.c.XYZ)
            let _ = exp2 (v4f v.c)
            let _ = Fun.PowerOfTwo v.c.X
            let _ = Fun.PowerOfTwo v.c.XY
            let _ = Fun.PowerOfTwo v.c.XYZ
            let _ = Fun.PowerOfTwo v.c
            let _ = Fun.PowerOfTwo (float32 v.c.X)
            let _ = Fun.PowerOfTwo (v2f v.c.XY)
            let _ = Fun.PowerOfTwo (v3f v.c.XYZ)
            let _ = Fun.PowerOfTwo (v4f v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["exp2"]

[<Test>]
let ``Pow``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Fun.Pow(v.c.XY, V2d.II)
            let _ = Fun.Pow(V2f(v.c.XY), V2f.II)
            let _ = Fun.Pow(v.c.XY, v.c.XY)
            let _ = Fun.Pow(V2f(v.c.XY), V2f(v.c.XY))
            let _ = Fun.Pow(v.c.XYZ, V3d.III)
            let _ = Fun.Pow(V3f(v.c.XYZ), V3f.III)
            let _ = Fun.Pow(v.c.XYZ, v.c.XYZ)
            let _ = Fun.Pow(V3f(v.c.XYZ), V3f(v.c.XYZ))
            let _ = Fun.Pow(v.c, V4d.IIII)
            let _ = Fun.Pow(V4f(v.c), V4f.IIII)
            let _ = Fun.Pow(v.c, v.c)
            let _ = Fun.Pow(V4f(v.c), V4f(v.c))
            let _ = Fun.Pow(v.c, 1.0);
            let _ = Fun.Pow(1.0, v.c);
            let _ = Fun.Pown(v.c.X, int32 v.c.Y);
            let _ = Fun.Pown(1, V4i(v.c));
            let _ = Fun.Pown(V2f(v.c.XY), 1);
            let _ = Fun.Pown(V2i(v.c.XY), 1);
            let _ = Fun.Pow(V2i(v.c.XY), V2f(v.c.XY))
            let _ = pow v.c 2.0
            let _ = pow v.c v.c
            let _ = v.c ** 2.0
            let _ = v.c ** v.c
            let _ = pown v.c 2
            let _ = pown v.c (V4i(v.c))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["pow"]

[<Test>]
let ``Log``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Fun.Log(v.c.XY)
            let _ = Fun.Log(V2f(v.c.XY))
            let _ = Fun.Log(v.c.XYZ)
            let _ = Fun.Log(V3f(v.c.XYZ))
            let _ = Fun.Log(v.c)
            let _ = Fun.Log(V4f(v.c))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["log"]

[<Test>]
let ``Sqrt``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = sqrt v.c
            let _ = Fun.Sqrt (V2i(v.c.XY))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["sqrt"]

[<Test>]
let ``Cbrt``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = cbrt v.c
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["pow"]

[<Test>]
let ``Sqr``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = sqr v.c
            let _ = sqr (V4i v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["pow"]

[<Test>]
let ``Signum``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = signum v.c
            let _ = signumi v.c
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["sign"]

[<Test>]
let ``Rounding``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = v.c |> floor |> truncate |> ceil |> round
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["floor"; "trunc"; "ceil"; "round"]

[<Test>]
let ``Abs``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = v.c.Abs()
            let _ = abs v.c
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["abs"]

[<Test>]
let ``Min / max``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Fun.Min(v.c, 1.0)
            let _ = Fun.Min(1.0, v.c)
            let _ = v.c |> min 1.0
            let _ = v.c |> min v.c
            let _ = Fun.Max(v.c, 1.0)
            let _ = Fun.Max(1.0, v.c)
            let _ = v.c |> max 1.0
            let _ = v.c |> max v.c
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["min"; "max"]

[<Test>]
let ``Clamp``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = v.c |> clamp 1.0 v.c
            let _ = v.c.XYZ |> clamp v.c.XYZ 1.0
            let _ = Fun.Clamp(v.c.XY, 0.0, 1.0)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["clamp"]

[<Test>]
let ``Step``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = step 0.5 v.c.X
            let _ = step 0.5 v.c.XY
            let _ = step 0.5 v.c.XYZ
            let _ = step 0.5 v.c
            let _ = step (V2d(0.5)) v.c.XY
            let _ = step (V3d(0.5)) v.c.XYZ
            let _ = step (V4d(0.5)) v.c
            let _ = step 0.5f (float32 v.c.X)
            let _ = step 0.5f (v2f v.c.XY)
            let _ = step 0.5f (v3f v.c.XYZ)
            let _ = step 0.5f (v4f v.c)
            let _ = step (V2f(0.5f)) (v2f v.c.XY)
            let _ = step (V3f(0.5f)) (v3f v.c.XYZ)
            let _ = step (V4f(0.5f)) (v4f v.c)
            let _ = Fun.Step(v.c.X, 0.5)
            let _ = Fun.Step(v.c.XY, 0.5)
            let _ = Fun.Step(v.c.XYZ, 0.5)
            let _ = Fun.Step(v.c, 0.5)
            let _ = Fun.Step(v.c.XY, V2d(0.5))
            let _ = Fun.Step(v.c.XYZ, V3d(0.5))
            let _ = Fun.Step(v.c, V4d(0.5))
            let _ = Fun.Step((float32 v.c.X), 0.5f)
            let _ = Fun.Step((v2f v.c.XY), 0.5f)
            let _ = Fun.Step((v3f v.c.XYZ), 0.5f)
            let _ = Fun.Step((v4f v.c), 0.5f)
            let _ = Fun.Step((v2f v.c.XY), V2f(0.5f))
            let _ = Fun.Step((v3f v.c.XYZ), V3f(0.5f))
            let _ = Fun.Step((v4f v.c), V4f(0.5f))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["step"]

[<Test>]
let ``Linearstep``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = linearstep 0.5 1.0 v.c.X
            let _ = linearstep (V2d(0.5)) (V2d(1.0)) v.c.XY
            let _ = linearstep (V3d(0.5)) (V3d(1.0)) v.c.XYZ
            let _ = linearstep (getVec()) (V4d(1.0)) v.c
            let _ = linearstep 0.5f 1.0f (float32 v.c.X)
            let _ = linearstep (V2f(0.5f)) (V2f(1.0f)) (v2f v.c.XY)
            let _ = linearstep (V3f(0.5f)) (V3f(1.0f)) (v3f v.c.XYZ)
            let _ = linearstep (V4f(0.5f)) (V4f(1.0f)) (v4f v.c)
            let _ = Fun.Linearstep(v.c.X, 0.5, 1.0)
            let _ = Fun.Linearstep(v.c.XY, V2d(0.5), V2d(1.0))
            let _ = Fun.Linearstep(v.c.XYZ, V3d(0.5), V3d(1.0))
            let _ = Fun.Linearstep(v.c, V4d(0.5), V4d(1.0))
            let _ = Fun.Linearstep((float32 v.c.X), 0.5f, 1.0f)
            let _ = Fun.Linearstep((v2f v.c.XY), V2f(0.5f), V2f(1.0f))
            let _ = Fun.Linearstep((v3f v.c.XYZ), V3f(0.5f), V3f(1.0f))
            let _ = Fun.Linearstep((v4f v.c), V4f(0.5f), V4f(1.0f))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["clamp", 16; "getVec", 2]

[<Test>]
let ``Smoothstep``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = v.c |> smoothstep v.c v.c
            let _ = v.c |> smoothstep 0.0 1.0
            let _ = Fun.Smoothstep(v.c, 0.0, 1.0)
            let _ = Fun.Smoothstep(v.c, v.c, v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["smoothstep"]

[<Test>]
let ``Asinh``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = asinh (V4f(v.c))
            let _ = Fun.Asinh (V4f(v.c))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["asinh"]

[<Test>]
let ``MultiplyAdd``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = madd v.c v.c v.c
            let _ = madd v.c v.c.X v.c
            let _ = madd v.c.X v.c.X v.c.X
            let _ = madd (V4i(v.c)) 2 (V4i(v.c))
            let _ = Fun.MultiplyAdd(v.c.X, v.c, v.c)
            let _ = Fun.MultiplyAdd(v.c, v.c.X, v.c)
            let _ = Fun.MultiplyAdd(v.c, v.c, v.c)
            let _ = Fun.MultiplyAdd(v.c.X, v.c.Y, v.c.Z)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["fma"]

[<Test>]
let ``Degrees / radians``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = degrees v.c
            let _ = degrees v.c.X
            let _ = v.c.DegreesFromRadians()
            let _ = v.c.X.DegreesFromRadians()
            let _ = radians v.c
            let _ = radians v.c.X
            let _ = v.c.RadiansFromDegrees()
            let _ = v.c.X.RadiansFromDegrees()
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["degrees"; "radians"]


[<Test>]
let ``Length``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.length v.c
            let _ = Vec.Length v.c
            let _ = Vec.Length v.c.XYZ
            let _ = Vec.Length v.c.XY
            let _ = Vec.Length v.what
            let _ = Vec.Length v.what.XYZ
            let _ = Vec.Length v.what.XY

            let _ = v.c.Length
            let _ = v.c.XYZ.Length
            let _ = v.c.XY.Length
            let _ = v.what.Length
            let _ = v.what.XYZ.Length
            let _ = v.what.XY.Length

            let _ = v.c.Norm2
            let _ = v.c.XYZ.Norm2
            let _ = v.c.XY.Norm2
            let _ = v.what.Norm2
            let _ = v.what.XYZ.Norm2
            let _ = v.what.XY.Norm2

            let _ = Vec.Norm2 v.c
            let _ = Vec.Norm2 v.c.XYZ
            let _ = Vec.Norm2 v.c.XY
            let _ = Vec.Norm2 v.what
            let _ = Vec.Norm2 v.what.XYZ
            let _ = Vec.Norm2 v.what.XY

            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["length"]

[<Test>]
let ``LengthSquared``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertDbl <| Vec.lengthSquared v.c
            let _ = assertDbl <| Vec.LengthSquared v.c
            let _ = assertDbl <| Vec.LengthSquared v.c.XYZ
            let _ = assertDbl <| Vec.LengthSquared v.c.XY
            let _ = assertDbl <| Vec.LengthSquared v.what
            let _ = assertDbl <| Vec.LengthSquared v.what.XYZ
            let _ = assertDbl <| Vec.LengthSquared v.what.XY

            let _ = assertDbl <| Vec.lengthSquared (getVec())
            let _ = assertDbl <| Vec.LengthSquared(getVec())
            let _ = assertDbl <| Vec.LengthSquared(getVec().XYZ)
            let _ = assertDbl <| Vec.LengthSquared(getVec().XY)
            let _ = assertDbl <| getVec().LengthSquared
            let _ = assertDbl <| getVec().XYZ.LengthSquared
            let _ = assertDbl <| getVec().XY.LengthSquared

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 8]

[<Test>]
let ``DistanceSquared``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertDbl <| Vec.distanceSquared v.c v.c
            let _ = assertDbl <| Vec.DistanceSquared(v.c, v.c)
            let _ = assertDbl <| Vec.DistanceSquared(v.c.XYZ, v.c.XYZ)
            let _ = assertDbl <| Vec.DistanceSquared(v.c.XY, v.c.XY)
            let _ = assertInt <| Vec.DistanceSquared(v.what, v.what)
            let _ = assertInt <| Vec.DistanceSquared(v.what.XYZ, v.what.XYZ)
            let _ = assertInt <| Vec.DistanceSquared(v.what.XY, v.what.XY)

            let _ = assertDbl <| Vec.distanceSquared (getVec()) (getVec())
            let _ = assertDbl <| Vec.DistanceSquared(getVec(), getVec())
            let _ = assertDbl <| Vec.DistanceSquared(getVec().XYZ, getVec().XYZ)
            let _ = assertDbl <| Vec.DistanceSquared(getVec().XY, getVec().XY)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 9]

[<Test>]
let ``Distance1``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertDbl <| Vec.Distance1(v.c, v.c)
            let _ = assertDbl <| Vec.Distance1(v.c.XYZ, v.c.XYZ)
            let _ = assertDbl <| Vec.Distance1(v.c.XY, v.c.XY)
            let _ = assertInt <| Vec.Distance1(v.what, v.what)
            let _ = assertInt <| Vec.Distance1(v.what.XYZ, v.what.XYZ)
            let _ = assertInt <| Vec.Distance1(v.what.XY, v.what.XY)

            let _ = assertDbl <| Vec.Distance1(getVec(), getVec())
            let _ = assertDbl <| Vec.Distance1(getVec().XYZ, getVec().XYZ)
            let _ = assertDbl <| Vec.Distance1(getVec().XY, getVec().XY)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader]
        [
            "getVec", 7
            "\(\(tmp[0-9]*\.x \+ tmp[0-9]*\.y\) \+ tmp[0-9]*\.z\) \+ tmp[0-9]*\.w\)", 3
        ]

[<Test>]
let ``Norm1``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertInt <| v.what.Norm1
            let _ = assertInt <| Vec.Norm1(v.what)
            let _ = assertDbl <| getVec().Norm1
            let _ = assertDbl <| Vec.Norm1(getVec())

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] [ "getVec", 3 ]

[<Test>]
let ``DistanceMin /-Max``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertDbl <| Vec.DistanceMin(v.c, v.c)
            let _ = assertDbl <| Vec.DistanceMin(v.c.XYZ, v.c.XYZ)
            let _ = assertDbl <| Vec.DistanceMin(v.c.XY, v.c.XY)
            let _ = assertInt <| Vec.DistanceMin(v.what, v.what)
            let _ = assertInt <| Vec.DistanceMin(v.what.XYZ, v.what.XYZ)
            let _ = assertInt <| Vec.DistanceMin(v.what.XY, v.what.XY)

            let _ = assertDbl <| Vec.DistanceMin(getVec(), getVec())
            let _ = assertDbl <| Vec.DistanceMin(getVec().XYZ, getVec().XYZ)
            let _ = assertDbl <| Vec.DistanceMin(getVec().XY, getVec().XY)

            let _ = assertDbl <| Vec.DistanceMax(v.c, v.c)
            let _ = assertDbl <| Vec.DistanceMax(v.c.XYZ, v.c.XYZ)
            let _ = assertDbl <| Vec.DistanceMax(v.c.XY, v.c.XY)
            let _ = assertInt <| Vec.DistanceMax(v.what, v.what)
            let _ = assertInt <| Vec.DistanceMax(v.what.XYZ, v.what.XYZ)
            let _ = assertInt <| Vec.DistanceMax(v.what.XY, v.what.XY)

            let _ = assertDbl <| Vec.DistanceMax(getVec(), getVec())
            let _ = assertDbl <| Vec.DistanceMax(getVec().XYZ, getVec().XYZ)
            let _ = assertDbl <| Vec.DistanceMax(getVec().XY, getVec().XY)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader]
        [
            "getVec", 13
            "abs\(.*\)", 18
            "min\(.*\)", 9
            "max\(.*\)", 9
        ]

[<Test>]
let ``NormMin / -Max``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertInt <| v.what.NormMin
            let _ = assertInt <| Vec.NormMin(v.what)
            let _ = assertDbl <| getVec().NormMin
            let _ = assertDbl <| Vec.NormMin(getVec())

            let _ = assertInt <| v.what.NormMax
            let _ = assertInt <| Vec.NormMax(v.what)
            let _ = assertDbl <| getVec().NormMax
            let _ = assertDbl <| Vec.NormMax(getVec())

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] [ "getVec", 5 ]

[<Test>]
let ``Reflect / refract``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.reflect v.c v.c
            let _ = Vec.refract 0.5 v.c v.c
            let _ = Vec.Reflect(v.c, v.c)
            let _ = Vec.Refract(v.c, v.c, 0.5)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["reflect"; "refract"]

[<Test>]
let ``Dot``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertDbl <| Vec.dot v.c v.c
            let _ = assertDbl <| Vec.Dot(v.c, v.c)
            let _ = assertDbl <| Vec.Dot(v.c.XYZ, v.c.XYZ)
            let _ = assertDbl <| Vec.Dot(v.c.XY, v.c.XY)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["dot"]

[<Test>]
let ``Dot (int)``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertInt <| Vec.dot (getVeci()) v.what
            let _ = assertInt <| Vec.Dot(getVeci(), v.what)
            let _ = assertInt <| Vec.Dot(getVeci().XYZ, v.what.XYZ)
            let _ = assertInt <| Vec.Dot(getVeci().XY, v.what.XY)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVeci", 5; "\*", 13]

[<Test>]
let ``Cross``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.cross v.c.XYZ v.c.XYZ
            let _ = Vec.Cross(v.c.XYZ, v.c.XYZ)
            let _ = Vec.Cross(v.what.XYZ, v.what.XYZ)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["cross"]


[<Test>]
let ``Transpose``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.transpose <| M33d(v.c.X)
            let _ = Mat.Transposed(M44d(v.c.X))
            let _ = Mat.Transposed(M33d(v.c.X))
            let _ = Mat.Transposed(M22d(v.c.X))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["transpose"]

[<Test>]
let ``Transform 2x2``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertV2d <| Mat.transform (M22d(v.c.X)) V2d.Zero
            let _ = assertV2d <| Mat.Transform(M22d(v.c.X), V2d.Zero)
            let _ = assertV2d <| Mat.TransposedTransform(M22d(v.c.X), V2d.Zero)

            return 0.0
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Transform 2x3``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertV2d <| Mat.transform (M23d(v.c.X)) V3d.Zero
            let _ = assertV2d <| Mat.Transform(M23d(v.c.X), V3d.Zero)
            let _ = assertV3d <| Mat.TransposedTransform(M23d(v.c.X), V2d.Zero)

            let _ = assertV2d <| Mat.transformDir (M23d(v.c.X)) v.c.XY
            let _ = assertV2d <| Mat.TransformDir(M23d(v.c.X), v.c.XY)

            let _ = assertV2d <| Mat.transformPos (M23d(v.c.X)) v.c.XY
            let _ = assertV2d <| Mat.TransformPos(M23d(v.c.X), v.c.XY)

            return 0.0
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Transform 3x3``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertV3d <| Mat.transform (M33d(v.c.X)) V3d.Zero
            let _ = assertV3d <| Mat.Transform(M33d(v.c.X), V3d.Zero)
            let _ = assertV3d <| Mat.TransposedTransform(M33d(v.c.X), V3d.Zero)

            let _ = assertV2d <| Mat.transformDir (M33d(v.c.X)) v.c.XY
            let _ = assertV2d <| Mat.TransformDir(M33d(v.c.X), v.c.XY)
            let _ = assertV2d <| Mat.TransposedTransformDir(M33d(v.c.X), v.c.XY)

            let _ = assertV2d <| Mat.transformPos (M33d(v.c.X)) v.c.XY
            let _ = assertV2d <| Mat.TransformPos(M33d(v.c.X), v.c.XY)
            let _ = assertV2d <| Mat.TransposedTransformPos(M33d(v.c.X), v.c.XY)

            let _ = assertV2d <| Mat.TransformPosProj(M33d(v.c.X), v.c.XY)
            let _ = assertV3d <| Mat.TransformPosProjFull(M33d(v.c.X), v.c.XY)
            let _ = assertV2d <| Mat.TransposedTransformProj(M33d(v.c.X), v.c.XY)
            let _ = assertV3d <| Mat.TransposedTransformProjFull(M33d(v.c.X), v.c.XY)

            return 0.0
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Transform 3x4``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertV3d <| Mat.transform (M34d(v.c.X)) V4d.Zero
            let _ = assertV3d <| Mat.Transform(M34d(v.c.X), V4d.Zero)
            let _ = assertV4d <| Mat.TransposedTransform(M34d(v.c.X), V3d.Zero)

            let _ = assertV3d <| Mat.transformDir (M34d(v.c.X)) v.c.XYZ
            let _ = assertV3d <| Mat.TransformDir(M34d(v.c.X), v.c.XYZ)

            let _ = assertV3d <| Mat.transformPos (M34d(v.c.X)) v.c.XYZ
            let _ = assertV3d <| Mat.TransformPos(M34d(v.c.X), v.c.XYZ)

            return 0.0
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Transform 4x4``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertV4d <| Mat.transform (M44d(v.c.X)) V4d.Zero
            let _ = assertV4d <| Mat.Transform(M44d(v.c.X), V4d.Zero)
            let _ = assertV4d <| Mat.TransposedTransform(M44d(v.c.X), V4d.Zero)

            let _ = assertV3d <| Mat.transformDir (M44d(v.c.X)) v.c.XYZ
            let _ = assertV3d <| Mat.TransformDir(M44d(v.c.X), v.c.XYZ)
            let _ = assertV3d <| Mat.TransposedTransformDir(M44d(v.c.X), v.c.XYZ)

            let _ = assertV3d <| Mat.transformPos (M44d(v.c.X)) v.c.XYZ
            let _ = assertV3d <| Mat.TransformPos(M44d(v.c.X), v.c.XYZ)
            let _ = assertV3d <| Mat.TransposedTransformPos(M44d(v.c.X), v.c.XYZ)

            let _ = assertV3d <| Mat.TransformPosProj(M44d(v.c.X), v.c.XYZ)
            let _ = assertV4d <| Mat.TransformPosProjFull(M44d(v.c.X), v.c.XYZ)
            let _ = assertV3d <| Mat.TransposedTransformProj(M44d(v.c.X), v.c.XYZ)
            let _ = assertV4d <| Mat.TransposedTransformProjFull(M44d(v.c.X), v.c.XYZ)

            return 0.0
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Determinant``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.det <| M44d(v.c.X)
            let _ = Mat.Determinant(M44d(v.c.X))
            let _ = Mat.Determinant(M33d(v.c.X))
            let _ = Mat.Determinant(M22d(v.c.X))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["determinant"]


[<Test>]
let ``Min- / MaxElement``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.MinElement(v.c.XY)
            let _ = Vec.MinElement(v.c.XYZ)
            let _ = Vec.MinElement(v.c)
            let _ = Vec.MinElement(v.what.XY)
            let _ = Vec.MinElement(v.what.XYZ)
            let _ = Vec.MinElement(v.what)
            let _ = v.c.XY.MinElement
            let _ = v.c.XYZ.MinElement
            let _ = v.c.MinElement
            let _ = v.what.XY.MinElement
            let _ = v.what.XYZ.MinElement
            let _ = v.what.MinElement
            let _ = Vec.MaxElement(v.c.XY)
            let _ = Vec.MaxElement(v.c.XYZ)
            let _ = Vec.MaxElement(v.c)
            let _ = Vec.MaxElement(v.what.XY)
            let _ = Vec.MaxElement(v.what.XYZ)
            let _ = Vec.MaxElement(v.what)
            let _ = v.c.XY.MaxElement
            let _ = v.c.XYZ.MaxElement
            let _ = v.c.MaxElement
            let _ = v.what.XY.MaxElement
            let _ = v.what.XYZ.MaxElement
            let _ = v.what.MaxElement

            let _ = Vec.MinElement(getVec().XY)
            let _ = Vec.MinElement(getVec().XYZ)
            let _ = Vec.MinElement(getVec())
            let _ = getVec().XY.MinElement
            let _ = getVec().XYZ.MinElement
            let _ = getVec().MinElement

            let _ = Vec.MaxElement(getVec().XY)
            let _ = Vec.MaxElement(getVec().XYZ)
            let _ = Vec.MaxElement(getVec())
            let _ = getVec().XY.MaxElement
            let _ = getVec().XYZ.MaxElement
            let _ = getVec().MaxElement

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 13]

[<Test>]
let ``Normalize``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let normalized = Vec.Normalized (V4i(v.c))
            let added = normalized + (Vec.normalize V4d.Half)
            return added
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["normalize"]

[<Test>]
let ``Constant swizzles``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = getVec().XYOI
            let _ = getVec().OXXX
            let _ = getVec().ZIXO
            let _ = getVec().ZYXN
            let _ = getVec().OXOX
            let _ = getVec().XOOO
            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 7]

[<Test>]
let ``Vector swizzles``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.x v.c
            let _ = Vec.y v.c.XYZ
            let _ = Vec.z v.c
            let _ = Vec.w v.c
            let _ = Vec.xy v.c
            let _ = Vec.yz v.c.XYZ
            let _ = Vec.zw v.c
            let _ = Vec.xyz v.c
            let _ = Vec.yzw v.c
            return v
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Vector AnyEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AnyEqual(v.c, v.c)
            let _ = v.c.AnyEqual(v.c)
            let _ = Vec.AnyEqual(v.c, 0.0)
            let _ = Vec.AnyEqual(0.0, v.c)
            let _ = Vec.anyEqual v.c v.c
            let _ = Vec.anyEqual v.c 0.0
            let _ = Vec.anyEqual 0.0 v.c

            let _ = Vec.AnyEqual(v.what, v.what)
            let _ = v.what.AnyEqual(v.what)
            let _ = Vec.AnyEqual(v.what, 0)
            let _ = Vec.AnyEqual(0, v.what)
            let _ = Vec.anyEqual v.what v.what
            let _ = Vec.anyEqual v.what 0
            let _ = Vec.anyEqual 0 v.what

            let _ = Vec.AnyEqual(v.whatu, v.whatu)
            let _ = v.whatu.AnyEqual(v.whatu)
            let _ = Vec.AnyEqual(v.whatu, 0u)
            let _ = Vec.AnyEqual(0u, v.whatu)
            let _ = Vec.anyEqual v.whatu v.whatu
            let _ = Vec.anyEqual v.whatu 0u
            let _ = Vec.anyEqual 0u v.whatu

            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["equal"; "any"]

[<Test>]
let ``Vector AllEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AllEqual(v.c, v.c)
            let _ = v.c.AllEqual(v.c)
            let _ = Vec.AllEqual(v.c, 0.0)
            let _ = Vec.AllEqual(0.0, v.c)
            let _ = Vec.allEqual v.c v.c
            let _ = Vec.allEqual v.c 0.0
            let _ = Vec.allEqual 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["=="]

[<Test>]
let ``Vector AnyDifferent``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AnyDifferent(v.c, v.c)
            let _ = v.c.AnyDifferent(v.c)
            let _ = Vec.AnyDifferent(v.c, 0.0)
            let _ = Vec.AnyDifferent(0.0, v.c)
            let _ = Vec.anyDifferent v.c v.c
            let _ = Vec.anyDifferent v.c 0.0
            let _ = Vec.anyDifferent 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["!="]

[<Test>]
let ``Vector AllDifferent``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AllDifferent(v.c, v.c)
            let _ = v.c.AllDifferent(v.c)
            let _ = Vec.AllDifferent(v.c, 0.0)
            let _ = Vec.AllDifferent(0.0, v.c)
            let _ = Vec.allDifferent v.c v.c
            let _ = Vec.allDifferent v.c 0.0
            let _ = Vec.allDifferent 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "notEqual"]

[<Test>]
let ``Vector AnySmaller``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AnySmaller(v.c, v.c)
            let _ = v.c.AnySmaller(v.c)
            let _ = Vec.AnySmaller(v.c, 0.0)
            let _ = Vec.AnySmaller(0.0, v.c)
            let _ = Vec.anySmaller v.c v.c
            let _ = Vec.anySmaller v.c 0.0
            let _ = Vec.anySmaller 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "lessThan"]

[<Test>]
let ``Vector AllSmaller``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AllSmaller(v.c, v.c)
            let _ = v.c.AllSmaller(v.c)
            let _ = Vec.AllSmaller(v.c, 0.0)
            let _ = Vec.AllSmaller(0.0, v.c)
            let _ = Vec.allSmaller v.c v.c
            let _ = Vec.allSmaller v.c 0.0
            let _ = Vec.allSmaller 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "lessThan"]

[<Test>]
let ``Vector AnySmallerOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AnySmallerOrEqual(v.c, v.c)
            let _ = v.c.AnySmallerOrEqual(v.c)
            let _ = Vec.AnySmallerOrEqual(v.c, 0.0)
            let _ = Vec.AnySmallerOrEqual(0.0, v.c)
            let _ = Vec.anySmallerOrEqual v.c v.c
            let _ = Vec.anySmallerOrEqual v.c 0.0
            let _ = Vec.anySmallerOrEqual 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "lessThanEqual"]

[<Test>]
let ``Vector AllSmallerOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AllSmallerOrEqual(v.c, v.c)
            let _ = v.c.AllSmallerOrEqual(v.c)
            let _ = Vec.AllSmallerOrEqual(v.c, 0.0)
            let _ = Vec.AllSmallerOrEqual(0.0, v.c)
            let _ = Vec.allSmallerOrEqual v.c v.c
            let _ = Vec.allSmallerOrEqual v.c 0.0
            let _ = Vec.allSmallerOrEqual 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "lessThanEqual"]

[<Test>]
let ``Vector AnyGreater``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AnyGreater(v.c, v.c)
            let _ = v.c.AnyGreater(v.c)
            let _ = Vec.AnyGreater(v.c, 0.0)
            let _ = Vec.AnyGreater(0.0, v.c)
            let _ = Vec.anyGreater v.c v.c
            let _ = Vec.anyGreater v.c 0.0
            let _ = Vec.anyGreater 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "greaterThan"]

[<Test>]
let ``Vector AllGreater``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AllGreater(v.c, v.c)
            let _ = v.c.AllGreater(v.c)
            let _ = Vec.AllGreater(v.c, 0.0)
            let _ = Vec.AllGreater(0.0, v.c)
            let _ = Vec.allGreater v.c v.c
            let _ = Vec.allGreater v.c 0.0
            let _ = Vec.allGreater 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "greaterThan"]

[<Test>]
let ``Vector AnyGreaterOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AnyGreaterOrEqual(v.c, v.c)
            let _ = v.c.AnyGreaterOrEqual(v.c)
            let _ = Vec.AnyGreaterOrEqual(v.c, 0.0)
            let _ = Vec.AnyGreaterOrEqual(0.0, v.c)
            let _ = Vec.anyGreaterOrEqual v.c v.c
            let _ = Vec.anyGreaterOrEqual v.c 0.0
            let _ = Vec.anyGreaterOrEqual 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "greaterThanEqual"]

[<Test>]
let ``Vector AllGreaterOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Vec.AllGreaterOrEqual(v.c, v.c)
            let _ = v.c.AllGreaterOrEqual(v.c)
            let _ = Vec.AllGreaterOrEqual(v.c, 0.0)
            let _ = Vec.AllGreaterOrEqual(0.0, v.c.XY)
            let _ = Vec.allGreaterOrEqual v.c v.c
            let _ = Vec.allGreaterOrEqual v.c 0.0
            let _ = Vec.allGreaterOrEqual 0.0 v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "greaterThanEqual"]

[<Test>]
let ``Vector AnyInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = v.IsInfinity
            let _ = v.AnyInfinity
            let _ = isInfinity v
            let _ = Fun.IsInfinity v
            let _ = Vec.AnyInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "any"]

[<Test>]
let ``Vector AllInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = v.AllInfinity
            let _ = Vec.AllInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "all"]

[<Test>]
let ``Vector AnyPositiveInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = v.AnyPositiveInfinity
            let _ = Vec.AnyPositiveInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Vector AllPositiveInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = v.AllPositiveInfinity
            let _ = Vec.AllPositiveInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "all"]

[<Test>]
let ``Vector AnyNegativeInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = v.AnyNegativeInfinity
            let _ = Vec.AnyNegativeInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Vector AllNegativeInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = v.AllNegativeInfinity
            let _ = Vec.AllNegativeInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "all"]

[<Test>]
let ``Vector AnyFinite``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = v.IsFinite
            let _ = v.AnyFinite
            let _ = isFinite v
            let _ = Fun.IsFinite v
            let _ = Vec.AnyFinite v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Vector AllFinite``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = v.AllFinite
            let _ = Vec.AllFinite v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "any"]

[<Test>]
let ``Vector AnyNaN``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = v.IsNaN
            let _ = v.AnyNaN
            let _ = isNaN v
            let _ = Fun.IsNaN v
            let _ = Vec.AnyNaN v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isnan"; "any"]

[<Test>]
let ``Vector AllNaN``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = v.AllNaN
            let _ = Vec.AllNaN v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isnan"; "all"]

[<Test>]
let ``Matrix AnyEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AnyEqual((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AnyEqual((M34d(v.c.X)))
            let _ = Mat.AnyEqual((M34d(v.c.X)), 0.0)
            let _ = Mat.AnyEqual(0.0, (M34d(v.c.X)))
            let _ = Mat.anyEqual (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.anyEqual (M34d(v.c.X)) 0.0
            let _ = Mat.anyEqual 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["equal"; "any"]

[<Test>]
let ``Matrix AllEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AllEqual((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AllEqual((M34d(v.c.X)))
            let _ = Mat.AllEqual((M34d(v.c.X)), 0.0)
            let _ = Mat.AllEqual(0.0, (M34d(v.c.X)))
            let _ = Mat.allEqual (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.allEqual (M34d(v.c.X)) 0.0
            let _ = Mat.allEqual 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["=="]

[<Test>]
let ``Matrix AnyDifferent``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AnyDifferent((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AnyDifferent((M34d(v.c.X)))
            let _ = Mat.AnyDifferent((M34d(v.c.X)), 0.0)
            let _ = Mat.AnyDifferent(0.0, (M34d(v.c.X)))
            let _ = Mat.anyDifferent (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.anyDifferent (M34d(v.c.X)) 0.0
            let _ = Mat.anyDifferent 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["!="]

[<Test>]
let ``Matrix AllDifferent``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AllDifferent((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AllDifferent((M34d(v.c.X)))
            let _ = Mat.AllDifferent((M34d(v.c.X)), 0.0)
            let _ = Mat.AllDifferent(0.0, (M34d(v.c.X)))
            let _ = Mat.allDifferent (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.allDifferent (M34d(v.c.X)) 0.0
            let _ = Mat.allDifferent 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "notEqual"]

[<Test>]
let ``Matrix AnySmaller``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AnySmaller((M34d(v.c.X)), (M34d(v.c.Y)))
            let _ = (M34d(v.c.X)).AnySmaller((M34d(v.c.Y)))
            let _ = Mat.AnySmaller((M34d(v.c.X)), 0.0)
            let _ = Mat.AnySmaller(0.0, (M34d(v.c.X)))
            let _ = Mat.anySmaller (M34d(v.c.X)) (M34d(v.c.Y))
            let _ = Mat.anySmaller (M34d(v.c.X)) 0.0
            let _ = Mat.anySmaller 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "lessThan"]

[<Test>]
let ``Matrix AllSmaller``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AllSmaller((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AllSmaller((M34d(v.c.X)))
            let _ = Mat.AllSmaller((M34d(v.c.X)), 0.0)
            let _ = Mat.AllSmaller(0.0, (M34d(v.c.X)))
            let _ = Mat.allSmaller (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.allSmaller (M34d(v.c.X)) 0.0
            let _ = Mat.allSmaller 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "lessThan"]

[<Test>]
let ``Matrix AnySmallerOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AnySmallerOrEqual((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AnySmallerOrEqual((M34d(v.c.X)))
            let _ = Mat.AnySmallerOrEqual((M34d(v.c.X)), 0.0)
            let _ = Mat.AnySmallerOrEqual(0.0, (M34d(v.c.X)))
            let _ = Mat.anySmallerOrEqual (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.anySmallerOrEqual (M34d(v.c.X)) 0.0
            let _ = Mat.anySmallerOrEqual 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "lessThanEqual"]

[<Test>]
let ``Matrix AllSmallerOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AllSmallerOrEqual((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AllSmallerOrEqual((M34d(v.c.X)))
            let _ = Mat.AllSmallerOrEqual((M34d(v.c.X)), 0.0)
            let _ = Mat.AllSmallerOrEqual(0.0, (M34d(v.c.X)))
            let _ = Mat.allSmallerOrEqual (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.allSmallerOrEqual (M34d(v.c.X)) 0.0
            let _ = Mat.allSmallerOrEqual 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "lessThanEqual"]

[<Test>]
let ``Matrix AnyGreater``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AnyGreater((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AnyGreater((M34d(v.c.X)))
            let _ = Mat.AnyGreater((M34d(v.c.X)), 0.0)
            let _ = Mat.AnyGreater(0.0, (M34d(v.c.X)))
            let _ = Mat.anyGreater (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.anyGreater (M34d(v.c.X)) 0.0
            let _ = Mat.anyGreater 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "greaterThan"]

[<Test>]
let ``Matrix AllGreater``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AllGreater((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AllGreater((M34d(v.c.X)))
            let _ = Mat.AllGreater((M34d(v.c.X)), 0.0)
            let _ = Mat.AllGreater(0.0, (M34d(v.c.X)))
            let _ = Mat.allGreater (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.allGreater (M34d(v.c.X)) 0.0
            let _ = Mat.allGreater 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "greaterThan"]

[<Test>]
let ``Matrix AnyGreaterOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AnyGreaterOrEqual((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AnyGreaterOrEqual((M34d(v.c.X)))
            let _ = Mat.AnyGreaterOrEqual((M34d(v.c.X)), 0.0)
            let _ = Mat.AnyGreaterOrEqual(0.0, (M34d(v.c.X)))
            let _ = Mat.anyGreaterOrEqual (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.anyGreaterOrEqual (M34d(v.c.X)) 0.0
            let _ = Mat.anyGreaterOrEqual 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "greaterThanEqual"]

[<Test>]
let ``Matrix AllGreaterOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = Mat.AllGreaterOrEqual((M34d(v.c.X)), (M34d(v.c.X)))
            let _ = (M34d(v.c.X)).AllGreaterOrEqual((M34d(v.c.X)))
            let _ = Mat.AllGreaterOrEqual((M34d(v.c.X)), 0.0)
            let _ = Mat.AllGreaterOrEqual(0.0, (M34d(v.c.X)))
            let _ = Mat.allGreaterOrEqual (M34d(v.c.X)) (M34d(v.c.X))
            let _ = Mat.allGreaterOrEqual (M34d(v.c.X)) 0.0
            let _ = Mat.allGreaterOrEqual 0.0 (M34d(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "greaterThanEqual"]

[<Test>]
let ``Matrix AnyInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34d(v.c.X)
            let _ = m.IsInfinity
            let _ = m.AnyInfinity
            let _ = isInfinity m
            let _ = Fun.IsInfinity m
            let _ = Mat.AnyInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"; "any"]

[<Test>]
let ``Matrix AllInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34d(v.c.X)
            let _ = m.AllInfinity
            let _ = Mat.AllInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "&&"; "all"]

[<Test>]
let ``Matrix AnyPositiveInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34d(v.c.X)
            let _ = m.AnyPositiveInfinity
            let _ = Mat.AnyPositiveInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Matrix AllPositiveInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34d(v.c.X)
            let _ = m.AllPositiveInfinity
            let _ = Mat.AllPositiveInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "&&"; "all"]

[<Test>]
let ``Matrix AnyNegativeInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34d(v.c.X)
            let _ = m.AnyNegativeInfinity
            let _ = Mat.AnyNegativeInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Matrix AllNegativeInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34d(v.c.X)
            let _ = m.AllNegativeInfinity
            let _ = Mat.AllNegativeInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "all"]

[<Test>]
let ``Matrix AnyFinite``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34d(v.c.X)
            let _ = m.IsFinite
            let _ = m.AnyFinite
            let _ = isFinite m
            let _ = Fun.IsFinite m
            let _ = Mat.AnyFinite m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Matrix AllFinite``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34d(v.c.X)
            let _ = m.AllFinite
            let _ = Mat.AllFinite m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "&&"; "\|\|"; "any"]

[<Test>]
let ``Matrix AnyNaN``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34d(v.c.X)
            let _ = m.IsNaN
            let _ = m.AnyNaN
            let _ = isNaN m
            let _ = Fun.IsNaN m
            let _ = Mat.AnyNaN m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isnan"; "\|\|"; "any"]

[<Test>]
let ``Matrix AllNaN``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34d(v.c.X)
            let _ = m.AllNaN
            let _ = Mat.AllNaN m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isnan"; "&&"; "all"]

[<Test>]
let ``IsInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = isInfinity v.c
            let _ = isInfinity v.c.X
            let _ = Fun.IsInfinity v.c
            let _ = Fun.IsInfinity v.c.X
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"]

[<Test>]
let ``IsInfinity (signed)``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = isPositiveInfinity (getVec().X)
            let _ = isNegativeInfinity (getVec().X)
            let _ = Fun.IsPositiveInfinity (getVec().X)
            let _ = Fun.IsNegativeInfinity (getVec().X)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 5]

[<Test>]
let ``IsNaN``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = isNaN v.c
            let _ = isNaN v.c.X
            let _ = Fun.IsNaN v.c
            let _ = Fun.IsNaN v.c.X
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isnan"]