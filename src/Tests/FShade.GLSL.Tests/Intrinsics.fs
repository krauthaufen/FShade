module Intrinsics

open System.Text.RegularExpressions
open Aardvark.Base
open FShade
open NUnit.Framework
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
        foo : V4f
        what : V4i
        what2 : V4i
        whatl : V4l
        whatl2 : V4l
        whatu : V4ui
        whatu2 : V4ui
    }

type UniformScope with
    member x.SomeUniform : V3f = uniform?SomeUniform
    member x.Storage : int[] = uniform?StorageBuffer?Storage
[<ReflectedDefinition>]
let getVec() =
    V4f(uniform.SomeUniform, 1.0f)

[<ReflectedDefinition>]
let getVeci() =
    V4i(V3i uniform.SomeUniform, 1)

[<ReflectedDefinition>]
let getVecu() =
    V4ui(V3ui uniform.SomeUniform, 1u)

[<ReflectedDefinition>]
let assertT<'T> (v : 'T) = v

[<Test>]
let ``Matrix constructors``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m33 = assertT <| M33f(v.pos.X)
            let m44 = assertT <| M44f(m33)
            let _ = assertT <| M33f(m44)
            return v.pos
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Matrix elements``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m22 = assertT <| M22f(v.pos.X)
            let m23 = assertT <| M23f(v.pos.X)
            let m33 = assertT <| M33f(v.pos.X)
            let m34 = assertT <| M34f(v.pos.X)
            let m44 = assertT <| M44f(m33)
            let _ = assertT <| m22.[v.what.X, v.what.Y]
            let _ = assertT <| m23.[v.what.X, v.what.Y]
            let _ = assertT <| m33.[v.what.X, v.what.Y]
            let _ = assertT <| m34.[v.what.X, v.what.Y]
            let _ = assertT <| m44.[v.what.X, v.what.Y]
            let _ = assertT <| m44.M03
            let _ = assertT <| m44.M13
            let _ = assertT <| m44.M23
            return v.pos
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Matrix columns / rows``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m33 = M33f(v.pos.X)
            let m34 = M34f(v.pos.X)
            let m44 = M44f(m33)
            let _ = assertT <| m33.C0
            let _ = assertT <| m33.Column(v.what.X)
            let _ = assertT <| m34.C1
            let _ = assertT <| m34.Column(v.what.X)
            let _ = assertT <| m34.R2
            let _ = assertT <| m34.Row(v.what.X)
            let _ = assertT <| m44.R3
            let _ = assertT <| m44.Row(v.what.X)
            return v.pos
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Vector constructors``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| V3f(v.pos.X)
            let _ = assertT <| V4f(v.pos.XY, v.pos.ZW)
            let _ = assertT <| V4f(v.pos.X, v.pos.YZW)
            let _ = assertT <| V4f(v.pos.XYZ)
            let _ = assertT <| V4f(v.pos.XY)
            let _ = assertT <| V2f(v.pos.XYZ)
            return v.pos
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Vector conversion``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = assertT <| v2d v.what
            let _ = assertT <| v3d v.what
            let _ = assertT <| v3d v.what.XY
            let _ = assertT <| v4d v.what
            let _ = assertT <| v4d v.what.XY
            let _ = assertT <| v4d v.what.XYZ
            let _ = assertT <| V4f.op_Explicit v.what
            let _ = assertT <| v4d v.c
            let _ = assertT <| v2i v.c
            let _ = assertT <| v3i v.c
            let _ = assertT <| v4i v.c
            let _ = assertT <| v2i v.c
            let _ = assertT <| v3i v.c.XY
            let _ = assertT <| v4i v.c.XY
            let _ = assertT <| V4i.op_Explicit v.what.XY
            let _ = assertT <| v2ui v.c
            let _ = assertT <| v3ui v.c.XY
            let _ = assertT <| v4ui v.c.XY
            let _ = assertT <| V4ui.op_Explicit v.what.XY
            return v.c
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Inverse lerp``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let mutable a = 0.0f
            a <- a + float32 (Fun.InvLerp(int8  v.c.X, int8  v.c.Y, int8  v.c.Z))
            a <- a + float32 (Fun.InvLerp(uint8  v.c.X, uint8  v.c.Y, uint8  v.c.Z))
            a <- a + float32 (Fun.InvLerp(int16 v.c.X, int16 v.c.Y, int16 v.c.Z))
            a <- a + float32 (Fun.InvLerp(uint16 v.c.X, uint16 v.c.Y, uint16 v.c.Z))
            a <- a + float32 (Fun.InvLerp(int32 v.c.X, int32 v.c.Y, int32 v.c.Z))
            a <- a + float32 (Fun.InvLerp(uint32 v.c.X, uint32 v.c.Y, uint32 v.c.Z))
            a <- a + Fun.InvLerp(v.c.X - v.c.Y, v.c.Y, v.c.Z)
            a <- a + float32 (invLerp (int8  v.c.Y) (int8  v.c.Z) (int8  v.c.X))
            a <- a + float32 (invLerp (uint8  v.c.Y) (uint8  v.c.Z) (uint8  v.c.X))
            a <- a + float32 (invLerp (int16 v.c.Y) (int16 v.c.Z) (int16 v.c.X))
            a <- a + float32 (invLerp (uint16 v.c.Y) (uint16 v.c.Z) (uint16 v.c.X))
            a <- a + float32 (invLerp (int32 v.c.Y) (int32 v.c.Z) (int32 v.c.X))
            a <- a + float32 (invLerp (uint32 v.c.Y) (uint32 v.c.Z) (uint32 v.c.X))
            a <- a + invLerp v.c.Y v.c.Z (v.c.X - v.c.Y)
            let _ = a

            let mutable a = V2f.Zero
            a <- a + v2f (Fun.InvLerp(V2i v.c.XY, V2i v.c.YZ, V2i v.c.XZ))
            a <- a + Fun.InvLerp(V2f v.c.XY, V2f v.c.YZ, V2f v.c.XZ)
            a <- a + v2f (invLerp (V2i v.c.YZ) (V2i v.c.XZ) (V2i v.c.XY))
            a <- a + invLerp (V2f v.c.YZ) (V2f v.c.XZ) (V2f v.c.XY)
            let _ = a

            let mutable a = V3f.Zero
            a <- a + v3f (Fun.InvLerp(V3i v.c.XYZ, V3i v.pos.XYZ, V3i v.foo.XYZ))
            a <- a + Fun.InvLerp(V3f v.c.XYZ, V3f v.pos.XYZ, V3f v.foo.XYZ)
            a <- a + v3f (invLerp (V3i v.pos.XYZ) (V3i v.foo.XYZ) (V3i v.c.XYZ))
            a <- a + invLerp (V3f v.pos.XYZ) (V3f v.foo.XYZ) (V3f v.c.XYZ)
            let _ = a

            let mutable a = V4f.Zero
            a <- a + v4f (Fun.InvLerp(V4i v.c, V4i v.pos, V4i v.foo))
            a <- a + Fun.InvLerp(v.c, v.pos, v.foo)
            a <- a + v4f (invLerp (V4i v.pos) (V4i v.foo) (V4i v.c))
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

            let _ = invLerp (getVec()) V4f.Zero V4f.One
            let _ = invLerp (getVec().XYZ) V3f.Zero V3f.One
            let _ = invLerp (getVec().XY) V2f.Zero V2f.One
            let _ = invLerp (getVec().X) 0.0f 1.0f

            let _ = Fun.InvLerp(V4f.One, getVec(), V4f.Zero)
            let _ = Fun.InvLerp(V3f.One, getVec().XYZ, V3f.Zero)
            let _ = Fun.InvLerp(V2f.One, getVec().XY, V2f.Zero)
            let _ = Fun.InvLerp(1.0f, getVec().X, 0.0f)

            let _ = invLerp (V4i (getVec())) V4i.Zero V4i.One
            let _ = invLerp (V3i (getVec().XYZ)) V3i.Zero V3i.One
            let _ = invLerp (V2i (getVec().XY)) V2i.Zero V2i.One
            let _ = invLerp (int <| getVec().X) 0 1

            let _ = Fun.InvLerp(V4i.One, V4i (getVec()), V4i.Zero)
            let _ = Fun.InvLerp(V3i.One, V3i (getVec().XYZ), V3i.Zero)
            let _ = Fun.InvLerp(V2i.One, V2i (getVec().XY), V2i.Zero)
            let x = float32 <| Fun.InvLerp(1, int <| getVec().X, 0)

            let _ = invLerp (V4ui (getVec())) V4ui.Zero V4ui.One
            let _ = invLerp (V3ui (getVec().XYZ)) V3ui.Zero V3ui.One
            let _ = invLerp (V2ui (getVec().XY)) V2ui.Zero V2ui.One
            let _ = invLerp (uint <| getVec().X) 0u 1u

            let _ = Fun.InvLerp(V4ui.One, V4ui (getVec()), V4ui.Zero)
            let _ = Fun.InvLerp(V3ui.One, V3ui (getVec().XYZ), V3ui.Zero)
            let _ = Fun.InvLerp(V2ui.One, V2ui (getVec().XY), V2ui.Zero)
            let x = float32 <| Fun.InvLerp(1u, uint <| getVec().X, 0u)

            return lerp v.c V4f.Zero x
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 24]

[<Test>]
let ``Inverse lerp constant``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = V2f(1.5) |> invLerp (V2f(1.0f)) (V2f(2.0f))
            let _ = V2i(2) |> invLerp (V2i(1)) (V2i(3))
            let _ = 1.5f |> invLerp 1.0f 2.0f
            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] [
        Regex.Escape "vec2(0.5, 0.5);", 1
        Regex.Escape "dvec2(0.5lf, 0.5lf);", 1
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

            let mutable a = V2ui.Zero
            a <- a + Fun.Lerp(v.c.X, V2ui(v.c.XY), V2ui(v.c.XY))
            a <- a + Fun.Lerp(float32 v.c.X, V2ui(v.c.XY), V2ui(v.c.XY))
            a <- a + Fun.Lerp(v.c.XY, V2ui(v.c.XY), V2ui(v.c.XY))
            a <- a + Fun.Lerp(V2f(v.c.XY), V2ui(v.c.XY), V2ui(v.c.XY))
            a <- a + lerp (V2ui(v.c.XY)) (V2ui(v.c.XY)) v.c.X
            a <- a + lerp (V2ui(v.c.XY)) (V2ui(v.c.XY)) (float32 v.c.X)
            a <- a + lerp (V2ui(v.c.XY)) (V2ui(v.c.XY)) v.c.XY
            a <- a + lerp (V2ui(v.c.XY)) (V2ui(v.c.XY)) (V2f(v.c.XY))
            let _ = a

            let mutable a = V3ui.Zero
            a <- a + Fun.Lerp(v.c.X, V3ui(v.c.XYZ), V3ui(v.c.XYZ))
            a <- a + Fun.Lerp(float32 v.c.X, V3ui(v.c.XYZ), V3ui(v.c.XYZ))
            a <- a + Fun.Lerp(v.c.XYZ, V3ui(v.c.XYZ), V3ui(v.c.XYZ))
            a <- a + Fun.Lerp(V3f(v.c.XYZ), V3ui(v.c.XYZ), V3ui(v.c.XYZ))
            a <- a + lerp (V3ui(v.c.XYZ)) (V3ui(v.c.XYZ)) v.c.X
            a <- a + lerp (V3ui(v.c.XYZ)) (V3ui(v.c.XYZ)) (float32 v.c.X)
            a <- a + lerp (V3ui(v.c.XYZ)) (V3ui(v.c.XYZ)) v.c.XYZ
            a <- a + lerp (V3ui(v.c.XYZ)) (V3ui(v.c.XYZ)) (V3f(v.c.XYZ))
            let _ = a

            let mutable a = V4ui.Zero
            a <- a + Fun.Lerp(v.c.X, V4ui(v.c), V4ui(v.c))
            a <- a + Fun.Lerp(float32 v.c.X, V4ui(v.c), V4ui(v.c))
            a <- a + Fun.Lerp(v.c, V4ui(v.c), V4ui(v.c))
            a <- a + Fun.Lerp(V4f(v.c), V4ui(v.c), V4ui(v.c))
            a <- a + lerp (V4ui(v.c)) (V4ui(v.c)) v.c.X
            a <- a + lerp (V4ui(v.c)) (V4ui(v.c)) (float32 v.c.X)
            a <- a + lerp (V4ui(v.c)) (V4ui(v.c)) v.c
            a <- a + lerp (V4ui(v.c)) (V4ui(v.c)) (V4f(v.c))
            let _ = a
            
            let mutable a = V2l.Zero
            a <- a + Fun.Lerp(v.c.X, V2l(v.c.XY), V2l(v.c.XY))
            a <- a + Fun.Lerp(float32 v.c.X, V2l(v.c.XY), V2l(v.c.XY))
            a <- a + Fun.Lerp(v.c.XY, V2l(v.c.XY), V2l(v.c.XY))
            a <- a + Fun.Lerp(V2f(v.c.XY), V2l(v.c.XY), V2l(v.c.XY))
            a <- a + lerp (V2l(v.c.XY)) (V2l(v.c.XY)) v.c.X
            a <- a + lerp (V2l(v.c.XY)) (V2l(v.c.XY)) (float32 v.c.X)
            a <- a + lerp (V2l(v.c.XY)) (V2l(v.c.XY)) v.c.XY
            a <- a + lerp (V2l(v.c.XY)) (V2l(v.c.XY)) (V2f(v.c.XY))
            let _ = a

            let mutable a = V3l.Zero
            a <- a + Fun.Lerp(v.c.X, V3l(v.c.XYZ), V3l(v.c.XYZ))
            a <- a + Fun.Lerp(float32 v.c.X, V3l(v.c.XYZ), V3l(v.c.XYZ))
            a <- a + Fun.Lerp(v.c.XYZ, V3l(v.c.XYZ), V3l(v.c.XYZ))
            a <- a + Fun.Lerp(V3f(v.c.XYZ), V3l(v.c.XYZ), V3l(v.c.XYZ))
            a <- a + lerp (V3l(v.c.XYZ)) (V3l(v.c.XYZ)) v.c.X
            a <- a + lerp (V3l(v.c.XYZ)) (V3l(v.c.XYZ)) (float32 v.c.X)
            a <- a + lerp (V3l(v.c.XYZ)) (V3l(v.c.XYZ)) v.c.XYZ
            a <- a + lerp (V3l(v.c.XYZ)) (V3l(v.c.XYZ)) (V3f(v.c.XYZ))
            let _ = a

            let mutable a = V4l.Zero
            a <- a + Fun.Lerp(v.c.X, V4l(v.c), V4l(v.c))
            a <- a + Fun.Lerp(float32 v.c.X, V4l(v.c), V4l(v.c))
            a <- a + Fun.Lerp(v.c, V4l(v.c), V4l(v.c))
            a <- a + Fun.Lerp(V4f(v.c), V4l(v.c), V4l(v.c))
            a <- a + lerp (V4l(v.c)) (V4l(v.c)) v.c.X
            a <- a + lerp (V4l(v.c)) (V4l(v.c)) (float32 v.c.X)
            a <- a + lerp (V4l(v.c)) (V4l(v.c)) v.c
            a <- a + lerp (V4l(v.c)) (V4l(v.c)) (V4f(v.c))
            let _ = a

            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["mix"; "round"; "\+ 0\.5"]

[<Test>]
let ``Matrix conversion``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = m22d <| M33f(v.c.X)
            let _ = m23d <| M33f(v.c.X)
            let _ = m33d <| M33f(v.c.X)
            let _ = m34d <| M33f(v.c.X)
            let _ = m44d <| M33f(v.c.X)
            let _ = m22f <| M33f(v.c.X)
            let _ = m23f <| M33f(v.c.X)
            let _ = m33f <| M33f(v.c.X)
            let _ = m34f <| M33f(v.c.X)
            let _ = m44f <| M33f(v.c.X)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["mat2x3"]

[<Test>]
let ``Lerp``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Lerp(v.c.X,       float32 v.c.X, float32 v.c.X)
            let _ = assertT <| Fun.Lerp(v.c.X,       V2f v.c.XY,    V2f v.c.XY)
            let _ = assertT <| Fun.Lerp(v.c.X,       V3f v.c.XYZ,   V3f v.c.XYZ)
            let _ = assertT <| Fun.Lerp(v.c.X,       V4f v.c,       V4f v.c)
            let _ = assertT <| Fun.Lerp(V2f v.c.XY,  V2f v.c.XY,    V2f v.c.XY)
            let _ = assertT <| Fun.Lerp(V3f v.c.XYZ, V3f v.c.XYZ,   V3f v.c.XYZ)
            let _ = assertT <| Fun.Lerp(V4f v.c,     V4f v.c,       V4f v.c)
            let _ = assertT <| Fun.Lerp(float v.c.X, float v.c.X,   float v.c.X)
            let _ = assertT <| Fun.Lerp(float v.c.X, V2d v.c.XY,    V2d v.c.XY)
            let _ = assertT <| Fun.Lerp(float v.c.X, V3d v.c.XYZ,   V3d v.c.XYZ)
            let _ = assertT <| Fun.Lerp(float v.c.X, V4d v.c,       V4d v.c)
            let _ = assertT <| Fun.Lerp(V2d v.c.XY,  V2d v.c.XY,    V2d v.c.XY)
            let _ = assertT <| Fun.Lerp(V3d v.c.XYZ, V3d v.c.XYZ,   V3d v.c.XYZ)
            let _ = assertT <| Fun.Lerp(V4d v.c,     V4d v.c,       V4d v.c)
            let _ = assertT <| lerp (float32 v.c.X) (float32 v.c.X) (v.c.X)
            let _ = assertT <| lerp (V2f v.c.XY)    (V2f v.c.XY)    (v.c.X)
            let _ = assertT <| lerp (V3f v.c.XYZ)   (V3f v.c.XYZ)   (v.c.X)
            let _ = assertT <| lerp (V4f v.c)       (V4f v.c)       (v.c.X)
            let _ = assertT <| lerp (V2f v.c.XY)    (V2f v.c.XY)    (V2f v.c.XY)
            let _ = assertT <| lerp (V3f v.c.XYZ)   (V3f v.c.XYZ)   (V3f v.c.XYZ)
            let _ = assertT <| lerp (V4f v.c)       (V4f v.c)       (V4f v.c)
            let _ = assertT <| lerp (float v.c.X)   (float v.c.X)   (float v.c.X)
            let _ = assertT <| lerp (V2d v.c.XY)    (V2d v.c.XY)    (float v.c.X)
            let _ = assertT <| lerp (V3d v.c.XYZ)   (V3d v.c.XYZ)   (float v.c.X)
            let _ = assertT <| lerp (V4d v.c)       (V4d v.c)       (float v.c.X)
            let _ = assertT <| lerp (V2d v.c.XY)    (V2d v.c.XY)    (V2d v.c.XY)
            let _ = assertT <| lerp (V3d v.c.XYZ)   (V3d v.c.XYZ)   (V3d v.c.XYZ)
            let _ = assertT <| lerp (V4d v.c)       (V4d v.c)       (V4d v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["mix"]

[<Test>]
let ``Exp``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Exp(int8 v.c.X)
            let _ = assertT <| Fun.Exp(uint8 v.c.X)
            let _ = assertT <| Fun.Exp(int16 v.c.X)
            let _ = assertT <| Fun.Exp(uint16 v.c.X)
            let _ = assertT <| Fun.Exp(int32 v.c.X)
            let _ = assertT <| Fun.Exp(uint32 v.c.X)
            let _ = assertT <| Fun.Exp(v.c.XY)
            let _ = assertT <| Fun.Exp(v.c.XYZ)
            let _ = assertT <| Fun.Exp(v.c)
            let _ = assertT <| exp v.c.XY
            let _ = assertT <| exp v.c.XYZ
            let _ = assertT <| exp v.c
            let _ = assertT <| Fun.Exp(V2i v.c.XY)
            let _ = assertT <| Fun.Exp(V3i v.c.XYZ)
            let _ = assertT <| Fun.Exp(V4i v.c)
            let _ = assertT <| Fun.Exp(V2ui v.c.XY)
            let _ = assertT <| Fun.Exp(V3ui v.c.XYZ)
            let _ = assertT <| Fun.Exp(V4ui v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["exp"]

[<Test>]
let ``Exp2``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| exp2 v.c.X
            let _ = assertT <| exp2 v.c.XY
            let _ = assertT <| exp2 v.c.XYZ
            let _ = assertT <| exp2 v.c
            let _ = assertT <| exp2 (float32 v.c.X)
            let _ = assertT <| exp2 (v2f v.c.XY)
            let _ = assertT <| exp2 (v3f v.c.XYZ)
            let _ = assertT <| exp2 (v4f v.c)
            let _ = assertT <| Fun.PowerOfTwo v.c.X
            let _ = assertT <| Fun.PowerOfTwo v.c.XY
            let _ = assertT <| Fun.PowerOfTwo v.c.XYZ
            let _ = assertT <| Fun.PowerOfTwo v.c
            let _ = assertT <| Fun.PowerOfTwo (float32 v.c.X)
            let _ = assertT <| Fun.PowerOfTwo (v2f v.c.XY)
            let _ = assertT <| Fun.PowerOfTwo (v3f v.c.XYZ)
            let _ = assertT <| Fun.PowerOfTwo (v4f v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["exp2"]

[<Test>]
let ``Pow``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Pow (1.0f, v.c.X)
            let _ = assertT <| Fun.Pow (1y,   v.c.X)
            let _ = assertT <| Fun.Pow (1uy,  v.c.X)
            let _ = assertT <| Fun.Pow (1s,   v.c.X)
            let _ = assertT <| Fun.Pow (1us,  v.c.X)
            let _ = assertT <| Fun.Pow (1,    v.c.X)
            let _ = assertT <| Fun.Pow (1u,   v.c.X)
            let _ = assertT <| pow 1.0f v.c.X

            let _ = assertT <| Fun.Pow(V2f v.c.X, v.c.XY)
            let _ = assertT <| Fun.Pow(V3f v.c.X, v.c.XYZ)
            let _ = assertT <| Fun.Pow(V4f v.c.X, v.c)
            let _ = assertT <| Fun.Pow(V2i v.c.X, v.c.XY)
            let _ = assertT <| Fun.Pow(V3i v.c.X, v.c.XYZ)
            let _ = assertT <| Fun.Pow(V4i v.c.X, v.c)
            let _ = assertT <| Fun.Pow(V2ui v.c.X, v.c.XY)
            let _ = assertT <| Fun.Pow(V3ui v.c.X, v.c.XYZ)
            let _ = assertT <| Fun.Pow(V4ui v.c.X, v.c)
            let _ = assertT <| pow (V2f v.c.X) v.c.XY
            let _ = assertT <| pow (V3f v.c.X) v.c.XYZ
            let _ = assertT <| pow (V4f v.c.X) v.c

            let _ = assertT <| Fun.Pow(V2f v.c.X, v.c.X)
            let _ = assertT <| Fun.Pow(V3f v.c.X, v.c.X)
            let _ = assertT <| Fun.Pow(V4f v.c.X, v.c.X)
            let _ = assertT <| Fun.Pow(V2i v.c.X, v.c.X)
            let _ = assertT <| Fun.Pow(V3i v.c.X, v.c.X)
            let _ = assertT <| Fun.Pow(V4i v.c.X, v.c.X)
            let _ = assertT <| Fun.Pow(V2ui v.c.X, v.c.X)
            let _ = assertT <| Fun.Pow(V3ui v.c.X, v.c.X)
            let _ = assertT <| Fun.Pow(V4ui v.c.X, v.c.X)
            let _ = assertT <| pow (V2f v.c.X) v.c.X
            let _ = assertT <| pow (V3f v.c.X) v.c.X
            let _ = assertT <| pow (V4f v.c.X) v.c.X

            let _ = assertT <| Fun.Pow(v.c.X, V2f v.c.X)
            let _ = assertT <| Fun.Pow(v.c.X, V3f v.c.X)
            let _ = assertT <| Fun.Pow(v.c.X, V4f v.c.X)
            let _ = assertT <| Fun.Pow(int v.c.X, V2f v.c.X)
            let _ = assertT <| Fun.Pow(int v.c.X, V3f v.c.X)
            let _ = assertT <| Fun.Pow(int v.c.X, V4f v.c.X)
            let _ = assertT <| Fun.Pow(uint32 v.c.X, V2f v.c.X)
            let _ = assertT <| Fun.Pow(uint32 v.c.X, V3f v.c.X)
            let _ = assertT <| Fun.Pow(uint32 v.c.X, V4f v.c.X)

            let _ = assertT <| Fun.Pown(1y,  int v.c.X)
            let _ = assertT <| Fun.Pown(1uy, int v.c.X)
            let _ = assertT <| Fun.Pown(1s,  int v.c.X)
            let _ = assertT <| Fun.Pown(1us, int v.c.X)
            let _ = assertT <| Fun.Pown(1,   int v.c.X)
            let _ = assertT <| Fun.Pown(1u,  int v.c.X)
            let _ = assertT <| pown 1y  (int v.c.X)
            let _ = assertT <| pown 1uy (int v.c.X)
            let _ = assertT <| pown 1s  (int v.c.X)
            let _ = assertT <| pown 1us (int v.c.X)
            let _ = assertT <| pown 1   (int v.c.X)
            let _ = assertT <| pown 1u  (int v.c.X)

            let _ = assertT <| Fun.Pown(1y,  int8 v.c.X)
            let _ = assertT <| Fun.Pown(1uy, uint8 v.c.X)
            let _ = assertT <| Fun.Pown(1s,  int16 v.c.X)
            let _ = assertT <| Fun.Pown(1us, uint16 v.c.X)
            let _ = assertT <| Fun.Pown(1,   int32 v.c.X)
            let _ = assertT <| Fun.Pown(1u,  uint32 v.c.X)
            let _ = assertT <| pown 1y  (int8 v.c.X)
            let _ = assertT <| pown 1uy (uint8 v.c.X)
            let _ = assertT <| pown 1s  (int16 v.c.X)
            let _ = assertT <| pown 1us (uint16 v.c.X)
            let _ = assertT <| pown 1   (int32 v.c.X)
            let _ = assertT <| pown 1u  (uint32 v.c.X)

            let _ = assertT <| Fun.Pown(V2i v.c.X, V2i v.c.X)
            let _ = assertT <| Fun.Pown(V3i v.c.X, V3i v.c.X)
            let _ = assertT <| Fun.Pown(V4i v.c.X, V4i v.c.X)
            let _ = assertT <| pown (V2i v.c.X) (V2i v.c.X)
            let _ = assertT <| pown (V3i v.c.X) (V3i v.c.X)
            let _ = assertT <| pown (V4i v.c.X) (V4i v.c.X)

            let _ = assertT <| Fun.Pown(V2ui v.c.X, V2ui v.c.X)
            let _ = assertT <| Fun.Pown(V3ui v.c.X, V3ui v.c.X)
            let _ = assertT <| Fun.Pown(V4ui v.c.X, V4ui v.c.X)
            let _ = assertT <| Fun.Pown(V2ui v.c.X, V2i v.c.X)
            let _ = assertT <| Fun.Pown(V3ui v.c.X, V3i v.c.X)
            let _ = assertT <| Fun.Pown(V4ui v.c.X, V4i v.c.X)
            let _ = assertT <| pown (V2ui v.c.X) (V2ui v.c.X)
            let _ = assertT <| pown (V3ui v.c.X) (V3ui v.c.X)
            let _ = assertT <| pown (V4ui v.c.X) (V4ui v.c.X)
            let _ = assertT <| pown (V2ui v.c.X) (V2i v.c.X)
            let _ = assertT <| pown (V3ui v.c.X) (V3i v.c.X)
            let _ = assertT <| pown (V4ui v.c.X) (V4i v.c.X)

            let _ = assertT <| Fun.Pown(V2f v.c.X, int v.c.X)
            let _ = assertT <| Fun.Pown(V3f v.c.X, int v.c.X)
            let _ = assertT <| Fun.Pown(V4f v.c.X, int v.c.X)
            let _ = assertT <| Fun.Pown(V2i v.c.X, int v.c.X)
            let _ = assertT <| Fun.Pown(V3i v.c.X, int v.c.X)
            let _ = assertT <| Fun.Pown(V4i v.c.X, int v.c.X)
            let _ = assertT <| Fun.Pown(V2ui v.c.X, int v.c.X)
            let _ = assertT <| Fun.Pown(V2ui v.c.X, uint v.c.X)
            let _ = assertT <| Fun.Pown(V3ui v.c.X, int v.c.X)
            let _ = assertT <| Fun.Pown(V3ui v.c.X, uint v.c.X)
            let _ = assertT <| Fun.Pown(V4ui v.c.X, int v.c.X)
            let _ = assertT <| Fun.Pown(V4ui v.c.X, uint v.c.X)
            let _ = assertT <| pown (V2f v.c.X) (int v.c.X)
            let _ = assertT <| pown (V3f v.c.X) (int v.c.X)
            let _ = assertT <| pown (V4f v.c.X) (int v.c.X)

            let _ = assertT <| Fun.Pown(v.c.X, V2i v.c.X)
            let _ = assertT <| Fun.Pown(v.c.X, V3i v.c.X)
            let _ = assertT <| Fun.Pown(v.c.X, V4i v.c.X)
            let _ = assertT <| Fun.Pown(int v.c.X, V2i v.c.X)
            let _ = assertT <| Fun.Pown(int v.c.X, V3i v.c.X)
            let _ = assertT <| Fun.Pown(int v.c.X, V4i v.c.X)
            let _ = assertT <| Fun.Pown(uint32 v.c.X, V2i v.c.X)
            let _ = assertT <| Fun.Pown(uint32 v.c.X, V3i v.c.X)
            let _ = assertT <| Fun.Pown(uint32 v.c.X, V4i v.c.X)
            let _ = assertT <| Fun.Pown(uint32 v.c.X, V2ui v.c.X)
            let _ = assertT <| Fun.Pown(uint32 v.c.X, V3ui v.c.X)
            let _ = assertT <| Fun.Pown(uint32 v.c.X, V4ui v.c.X)

            let _ = assertT <| Fun.Pow(v.c.XY, V2f.II)
            let _ = assertT <| Fun.Pow(V2f(v.c.XY), V2f.II)
            let _ = assertT <| Fun.Pow(v.c.XY, v.c.XY)
            let _ = assertT <| Fun.Pow(V2f(v.c.XY), V2f(v.c.XY))
            let _ = assertT <| Fun.Pow(v.c.XYZ, V3f.III)
            let _ = assertT <| Fun.Pow(V3f(v.c.XYZ), V3f.III)
            let _ = assertT <| Fun.Pow(v.c.XYZ, v.c.XYZ)
            let _ = assertT <| Fun.Pow(V3f(v.c.XYZ), V3f(v.c.XYZ))
            let _ = assertT <| Fun.Pow(v.c, V4f.IIII)
            let _ = assertT <| Fun.Pow(V4f(v.c), V4f.IIII)
            let _ = assertT <| Fun.Pow(v.c, v.c)
            let _ = assertT <| Fun.Pow(V4f(v.c), V4f(v.c))
            let _ = assertT <| Fun.Pow(v.c, 1.0f)
            let _ = assertT <| Fun.Pow(1.0f, v.c)
            let _ = assertT <| Fun.Pow(1, v.c)
            let _ = assertT <| Fun.Pow(1u, v.c)
            let _ = assertT <| Fun.Pown(1, V4i v.c)
            let _ = assertT <| Fun.Pown(1u, V4i v.c)
            let _ = assertT <| Fun.Pown(1u, V4ui v.c)
            let _ = assertT <| Fun.Pown(v.c.X, int32 v.c.Y)
            let _ = assertT <| Fun.Pown(1, V4i(v.c))
            let _ = assertT <| Fun.Pown(V2f(v.c.XY), 1)
            let _ = assertT <| Fun.Pown(V2i(v.c.XY), 1)
            let _ = assertT <| Fun.Pow(V2i(v.c.XY), V2f(v.c.XY))
            let _ = assertT <| pow v.c 2.0f
            let _ = assertT <| pow v.c v.c
            let _ = assertT <| v.c ** 2.0f
            let _ = assertT <| v.c ** v.c
            let _ = assertT <| pown v.c 2
            let _ = assertT <| pown v.c (V4i(v.c))

            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["pow"]

[<Test>]
let ``Log``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Log(int8 v.c.X)
            let _ = assertT <| Fun.Log(uint8 v.c.X)
            let _ = assertT <| Fun.Log(int16 v.c.X)
            let _ = assertT <| Fun.Log(uint16 v.c.X)
            let _ = assertT <| Fun.Log(int32 v.c.X)
            let _ = assertT <| Fun.Log(uint32 v.c.X)
            let _ = assertT <| Fun.Log(v.c.X)
            let _ = assertT <| Fun.Log(v.c.XY)
            let _ = assertT <| Fun.Log(v.c.XYZ)
            let _ = assertT <| Fun.Log(v.c)
            let _ = assertT <| log v.c.X
            let _ = assertT <| log v.c.XY
            let _ = assertT <| log v.c.XYZ
            let _ = assertT <| log v.c
            let _ = assertT <| Fun.Log(V2i v.c.XY)
            let _ = assertT <| Fun.Log(V3i v.c.XYZ)
            let _ = assertT <| Fun.Log(V4i v.c)
            let _ = assertT <| Fun.Log(V2ui v.c.XY)
            let _ = assertT <| Fun.Log(V3ui v.c.XYZ)
            let _ = assertT <| Fun.Log(V4ui v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["log"]

[<Test>]
let ``Log2``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Log2(int8 v.c.X)
            let _ = assertT <| Fun.Log2(uint8 v.c.X)
            let _ = assertT <| Fun.Log2(int16 v.c.X)
            let _ = assertT <| Fun.Log2(uint16 v.c.X)
            let _ = assertT <| Fun.Log2(int32 v.c.X)
            let _ = assertT <| Fun.Log2(uint32 v.c.X)
            let _ = assertT <| Fun.Log2(v.c.X)
            let _ = assertT <| Fun.Log2(v.c.XY)
            let _ = assertT <| Fun.Log2(v.c.XYZ)
            let _ = assertT <| Fun.Log2(v.c)
            let _ = assertT <| log2 v.c.X
            let _ = assertT <| log2 v.c.XY
            let _ = assertT <| log2 v.c.XYZ
            let _ = assertT <| log2 v.c
            let _ = assertT <| Fun.Log2(V2i v.c.XY)
            let _ = assertT <| Fun.Log2(V3i v.c.XYZ)
            let _ = assertT <| Fun.Log2(V4i v.c)
            let _ = assertT <| Fun.Log2(V2ui v.c.XY)
            let _ = assertT <| Fun.Log2(V3ui v.c.XYZ)
            let _ = assertT <| Fun.Log2(V4ui v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["log2"]

[<Test>]
let ``Sqrt``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Sqrt(int8 v.c.X)
            let _ = assertT <| Fun.Sqrt(uint8 v.c.X)
            let _ = assertT <| Fun.Sqrt(int16 v.c.X)
            let _ = assertT <| Fun.Sqrt(uint16 v.c.X)
            let _ = assertT <| Fun.Sqrt(int32 v.c.X)
            let _ = assertT <| Fun.Sqrt(uint32 v.c.X)
            let _ = assertT <| Fun.Sqrt(int64 v.c.X)
            let _ = assertT <| Fun.Sqrt(uint64 v.c.X)
            let _ = assertT <| Fun.Sqrt(float v.c.X)
            let _ = assertT <| Fun.Sqrt(v.c.X)
            let _ = assertT <| Fun.Sqrt(v.c.XY)
            let _ = assertT <| Fun.Sqrt(v.c.XYZ)
            let _ = assertT <| Fun.Sqrt(v.c)
            let _ = assertT <| Fun.Sqrt(V2d v.c.XY)
            let _ = assertT <| Fun.Sqrt(V3d v.c.XYZ)
            let _ = assertT <| Fun.Sqrt(V4d v.c)
            let _ = assertT <| sqrt (float v.c.X)
            let _ = assertT <| sqrt v.c.X
            let _ = assertT <| sqrt v.c.XY
            let _ = assertT <| sqrt v.c.XYZ
            let _ = assertT <| sqrt v.c
            let _ = assertT <| sqrt (V2d v.c.XY)
            let _ = assertT <| sqrt (V3d v.c.XYZ)
            let _ = assertT <| sqrt (V4d v.c)
            let _ = assertT <| Fun.Sqrt(V2i v.c.XY)
            let _ = assertT <| Fun.Sqrt(V3i v.c.XYZ)
            let _ = assertT <| Fun.Sqrt(V4i v.c)
            let _ = assertT <| Fun.Sqrt(V2l v.c.XY)
            let _ = assertT <| Fun.Sqrt(V3l v.c.XYZ)
            let _ = assertT <| Fun.Sqrt(V4l v.c)
            let _ = assertT <| Fun.Sqrt(V2ui v.c.XY)
            let _ = assertT <| Fun.Sqrt(V3ui v.c.XYZ)
            let _ = assertT <| Fun.Sqrt(V4ui v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["sqrt"]

[<Test>]
let ``Cbrt``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Cbrt(int8 v.c.X)
            let _ = assertT <| Fun.Cbrt(uint8 v.c.X)
            let _ = assertT <| Fun.Cbrt(int16 v.c.X)
            let _ = assertT <| Fun.Cbrt(uint16 v.c.X)
            let _ = assertT <| Fun.Cbrt(int32 v.c.X)
            let _ = assertT <| Fun.Cbrt(uint32 v.c.X)
            let _ = assertT <| Fun.Cbrt(v.c.X)
            let _ = assertT <| Fun.Cbrt(v.c.XY)
            let _ = assertT <| Fun.Cbrt(v.c.XYZ)
            let _ = assertT <| Fun.Cbrt(v.c)
            let _ = assertT <| cbrt v.c.X
            let _ = assertT <| cbrt v.c.XY
            let _ = assertT <| cbrt v.c.XYZ
            let _ = assertT <| cbrt v.c
            let _ = assertT <| Fun.Cbrt(V2i v.c.XY)
            let _ = assertT <| Fun.Cbrt(V3i v.c.XYZ)
            let _ = assertT <| Fun.Cbrt(V4i v.c)
            let _ = assertT <| Fun.Cbrt(V2ui v.c.XY)
            let _ = assertT <| Fun.Cbrt(V3ui v.c.XYZ)
            let _ = assertT <| Fun.Cbrt(V4ui v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["pow"]

[<Test>]
let ``Sqr``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Square(int8 v.c.X)
            let _ = assertT <| Fun.Square(uint8 v.c.X)
            let _ = assertT <| Fun.Square(int16 v.c.X)
            let _ = assertT <| Fun.Square(uint16 v.c.X)
            let _ = assertT <| Fun.Square(int32 v.c.X)
            let _ = assertT <| Fun.Square(uint32 v.c.X)
            let _ = assertT <| Fun.Square(int64 v.c.X)
            let _ = assertT <| Fun.Square(uint64 v.c.X)
            let _ = assertT <| Fun.Square(float v.c.X)
            let _ = assertT <| Fun.Square(v.c.X)
            let _ = assertT <| Fun.Square(v.c.XY)
            let _ = assertT <| Fun.Square(v.c.XYZ)
            let _ = assertT <| Fun.Square(v.c)
            let _ = assertT <| Fun.Square(V2d v.c.XY)
            let _ = assertT <| Fun.Square(V3d v.c.XYZ)
            let _ = assertT <| Fun.Square(V4d v.c)
            let _ = assertT <| Fun.Square(V2i v.c.XY)
            let _ = assertT <| Fun.Square(V3i v.c.XYZ)
            let _ = assertT <| Fun.Square(V4i v.c)
            let _ = assertT <| Fun.Square(V2l v.c.XY)
            let _ = assertT <| Fun.Square(V3l v.c.XYZ)
            let _ = assertT <| Fun.Square(V4l v.c)
            let _ = assertT <| Fun.Square(V2ui v.c.XY)
            let _ = assertT <| Fun.Square(V3ui v.c.XYZ)
            let _ = assertT <| Fun.Square(V4ui v.c)
            let _ = assertT <| sqr (int8 v.c.X)
            let _ = assertT <| sqr (uint8 v.c.X)
            let _ = assertT <| sqr (int16 v.c.X)
            let _ = assertT <| sqr (uint16 v.c.X)
            let _ = assertT <| sqr (int32 v.c.X)
            let _ = assertT <| sqr (uint32 v.c.X)
            let _ = assertT <| sqr (int64 v.c.X)
            let _ = assertT <| sqr (uint64 v.c.X)
            let _ = assertT <| sqr (float v.c.X)
            let _ = assertT <| sqr v.c.X
            let _ = assertT <| sqr v.c.XY
            let _ = assertT <| sqr v.c.XYZ
            let _ = assertT <| sqr v.c
            let _ = assertT <| sqr (V2d v.c.XY)
            let _ = assertT <| sqr (V3d v.c.XYZ)
            let _ = assertT <| sqr (V4d v.c)
            let _ = assertT <| sqr (V2i v.c.XY)
            let _ = assertT <| sqr (V3i v.c.XYZ)
            let _ = assertT <| sqr (V4i v.c)
            let _ = assertT <| sqr (V2l v.c.XY)
            let _ = assertT <| sqr (V3l v.c.XYZ)
            let _ = assertT <| sqr (V4l v.c)
            let _ = assertT <| sqr (V2ui v.c.XY)
            let _ = assertT <| sqr (V3ui v.c.XYZ)
            let _ = assertT <| sqr (V4ui v.c)
            return v
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Signum``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Sign(int8 v.c.X)
            let _ = assertT <| Fun.Sign(int16 v.c.X)
            let _ = assertT <| Fun.Sign(int32 v.c.X)
            let _ = assertT <| Fun.Sign(int64 v.c.X)
            let _ = assertT <| Fun.Sign(float v.c.X)
            let _ = assertT <| Fun.Sign(v.c.X)
            let _ = assertT <| Fun.Sign(v.c.XY)
            let _ = assertT <| Fun.Sign(v.c.XYZ)
            let _ = assertT <| Fun.Sign(v.c)
            let _ = assertT <| Fun.Sign(V2d v.c.XY)
            let _ = assertT <| Fun.Sign(V3d v.c.XYZ)
            let _ = assertT <| Fun.Sign(V4d v.c)
            let _ = assertT <| Fun.Sign(V2i v.c.XY)
            let _ = assertT <| Fun.Sign(V3i v.c.XYZ)
            let _ = assertT <| Fun.Sign(V4i v.c)
            let _ = assertT <| Fun.Sign(V2l v.c.XY)
            let _ = assertT <| Fun.Sign(V3l v.c.XYZ)
            let _ = assertT <| Fun.Sign(V4l v.c)

            let _ = assertT <| sign (int8 v.c.X)
            let _ = assertT <| sign (int16 v.c.X)
            let _ = assertT <| sign (int32 v.c.X)
            let _ = assertT <| sign (int64 v.c.X)

            let _ = assertT <| Fun.Signum(int8 v.c.X)
            let _ = assertT <| Fun.Signum(int16 v.c.X)
            let _ = assertT <| Fun.Signum(int32 v.c.X)
            let _ = assertT <| Fun.Signum(int64 v.c.X)
            let _ = assertT <| Fun.Signum(float v.c.X)
            let _ = assertT <| Fun.Signum(v.c.X)
            let _ = assertT <| Fun.Signum(v.c.XY)
            let _ = assertT <| Fun.Signum(v.c.XYZ)
            let _ = assertT <| Fun.Signum(v.c)
            let _ = assertT <| Fun.Signum(V2d v.c.XY)
            let _ = assertT <| Fun.Signum(V3d v.c.XYZ)
            let _ = assertT <| Fun.Signum(V4d v.c)
            let _ = assertT <| Fun.Signum(V2i v.c.XY)
            let _ = assertT <| Fun.Signum(V3i v.c.XYZ)
            let _ = assertT <| Fun.Signum(V4i v.c)
            let _ = assertT <| Fun.Signum(V2l v.c.XY)
            let _ = assertT <| Fun.Signum(V3l v.c.XYZ)
            let _ = assertT <| Fun.Signum(V4l v.c)

            let _ = assertT <| signum (int8 v.c.X)
            let _ = assertT <| signum (int16 v.c.X)
            let _ = assertT <| signum (int32 v.c.X)
            let _ = assertT <| signum (int64 v.c.X)
            let _ = assertT <| signum (float v.c.X)
            let _ = assertT <| signum v.c.X
            let _ = assertT <| signum v.c.XY
            let _ = assertT <| signum v.c.XYZ
            let _ = assertT <| signum v.c
            let _ = assertT <| signum (V2d v.c.XY)
            let _ = assertT <| signum (V3d v.c.XYZ)
            let _ = assertT <| signum (V4d v.c)
            let _ = assertT <| signum (V2i v.c.XY)
            let _ = assertT <| signum (V3i v.c.XYZ)
            let _ = assertT <| signum (V4i v.c)
            let _ = assertT <| signum (V2l v.c.XY)
            let _ = assertT <| signum (V3l v.c.XYZ)
            let _ = assertT <| signum (V4l v.c)

            let _ = assertT <| Fun.Signumi(int8 v.c.X)
            let _ = assertT <| Fun.Signumi(int16 v.c.X)
            let _ = assertT <| Fun.Signumi(int32 v.c.X)
            let _ = assertT <| Fun.Signumi(int64 v.c.X)
            let _ = assertT <| Fun.Signumi(float v.c.X)
            let _ = assertT <| Fun.Signumi(v.c.X)
            let _ = assertT <| Fun.Signumi(v.c.XY)
            let _ = assertT <| Fun.Signumi(v.c.XYZ)
            let _ = assertT <| Fun.Signumi(v.c)
            let _ = assertT <| Fun.Signumi(V2d v.c.XY)
            let _ = assertT <| Fun.Signumi(V3d v.c.XYZ)
            let _ = assertT <| Fun.Signumi(V4d v.c)
            let _ = assertT <| Fun.Signumi(V2i v.c.XY)
            let _ = assertT <| Fun.Signumi(V3i v.c.XYZ)
            let _ = assertT <| Fun.Signumi(V4i v.c)
            let _ = assertT <| Fun.Signumi(V2l v.c.XY)
            let _ = assertT <| Fun.Signumi(V3l v.c.XYZ)
            let _ = assertT <| Fun.Signumi(V4l v.c)

            let _ = assertT <| signumi (int8 v.c.X)
            let _ = assertT <| signumi (int16 v.c.X)
            let _ = assertT <| signumi (int32 v.c.X)
            let _ = assertT <| signumi (int64 v.c.X)
            let _ = assertT <| signumi (float v.c.X)
            let _ = assertT <| signumi v.c.X
            let _ = assertT <| signumi v.c.XY
            let _ = assertT <| signumi v.c.XYZ
            let _ = assertT <| signumi v.c
            let _ = assertT <| signumi (V2d v.c.XY)
            let _ = assertT <| signumi (V3d v.c.XYZ)
            let _ = assertT <| signumi (V4d v.c)
            let _ = assertT <| signumi (V2i v.c.XY)
            let _ = assertT <| signumi (V3i v.c.XYZ)
            let _ = assertT <| signumi (V4i v.c)
            let _ = assertT <| signumi (V2l v.c.XY)
            let _ = assertT <| signumi (V3l v.c.XYZ)
            let _ = assertT <| signumi (V4l v.c)

            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["sign"]

[<Test>]
let ``Rounding``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Round(v.c.X)
            let _ = assertT <| Fun.Round(float v.c.X)
            let _ = assertT <| Fun.Round(V2d v.c.XY)
            let _ = assertT <| Fun.Round(V3d v.c.XYZ)
            let _ = assertT <| Fun.Round(V4d v.c)
            let _ = assertT <| Fun.Round(v.c.XY)
            let _ = assertT <| Fun.Round(v.c.XYZ)
            let _ = assertT <| Fun.Round(v.c)
            let _ = assertT <| round v.c.X
            let _ = assertT <| round (float v.c.X)
            let _ = assertT <| round (V2d v.c.XY)
            let _ = assertT <| round (V3d v.c.XYZ)
            let _ = assertT <| round (V4d v.c)
            let _ = assertT <| round v.c.XY
            let _ = assertT <| round v.c.XYZ
            let _ = assertT <| round v.c

            let _ = assertT <| Fun.Floor(v.c.X)
            let _ = assertT <| Fun.Floor(float v.c.X)
            let _ = assertT <| Fun.Floor(V2d v.c.XY)
            let _ = assertT <| Fun.Floor(V3d v.c.XYZ)
            let _ = assertT <| Fun.Floor(V4d v.c)
            let _ = assertT <| Fun.Floor(v.c.XY)
            let _ = assertT <| Fun.Floor(v.c.XYZ)
            let _ = assertT <| Fun.Floor(v.c)
            let _ = assertT <| floor v.c.X
            let _ = assertT <| floor (float v.c.X)
            let _ = assertT <| floor (V2d v.c.XY)
            let _ = assertT <| floor (V3d v.c.XYZ)
            let _ = assertT <| floor (V4d v.c)
            let _ = assertT <| floor v.c.XY
            let _ = assertT <| floor v.c.XYZ
            let _ = assertT <| floor v.c

            let _ = assertT <| Fun.Ceiling(v.c.X)
            let _ = assertT <| Fun.Ceiling(float v.c.X)
            let _ = assertT <| Fun.Ceiling(V2d v.c.XY)
            let _ = assertT <| Fun.Ceiling(V3d v.c.XYZ)
            let _ = assertT <| Fun.Ceiling(V4d v.c)
            let _ = assertT <| Fun.Ceiling(v.c.XY)
            let _ = assertT <| Fun.Ceiling(v.c.XYZ)
            let _ = assertT <| Fun.Ceiling(v.c)
            let _ = assertT <| ceil v.c.X
            let _ = assertT <| ceil (float v.c.X)
            let _ = assertT <| ceil (V2d v.c.XY)
            let _ = assertT <| ceil (V3d v.c.XYZ)
            let _ = assertT <| ceil (V4d v.c)
            let _ = assertT <| ceil v.c.XY
            let _ = assertT <| ceil v.c.XYZ
            let _ = assertT <| ceil v.c

            let _ = assertT <| Fun.Truncate(v.c.X)
            let _ = assertT <| Fun.Truncate(float v.c.X)
            let _ = assertT <| Fun.Truncate(V2d v.c.XY)
            let _ = assertT <| Fun.Truncate(V3d v.c.XYZ)
            let _ = assertT <| Fun.Truncate(V4d v.c)
            let _ = assertT <| Fun.Truncate(v.c.XY)
            let _ = assertT <| Fun.Truncate(v.c.XYZ)
            let _ = assertT <| Fun.Truncate(v.c)
            let _ = assertT <| truncate v.c.X
            let _ = assertT <| truncate (float v.c.X)
            let _ = assertT <| truncate (V2d v.c.XY)
            let _ = assertT <| truncate (V3d v.c.XYZ)
            let _ = assertT <| truncate (V4d v.c)
            let _ = assertT <| truncate v.c.XY
            let _ = assertT <| truncate v.c.XYZ
            let _ = assertT <| truncate v.c

            let _ = assertT <| Fun.Frac(v.c.X)
            let _ = assertT <| Fun.Frac(float v.c.X)
            let _ = assertT <| Fun.Frac(V2d v.c.XY)
            let _ = assertT <| Fun.Frac(V3d v.c.XYZ)
            let _ = assertT <| Fun.Frac(V4d v.c)
            let _ = assertT <| Fun.Frac(v.c.XY)
            let _ = assertT <| Fun.Frac(v.c.XYZ)
            let _ = assertT <| Fun.Frac(v.c)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["floor"; "trunc"; "ceil"; "round"; "fract"]

[<GLSLIntrinsic("atomicAdd({0}, {1})")>]
let atomicAdd (a : ref<int>) (b : int) : int = onlyInShaderCode "atomicAdd"

[<Test>]
let ``Atomics on Argument-Storage``() =
    Setup.Run()
    
    let shader (a : int[]) =
        compute {
            atomicAdd &&a.[0] 1 |> ignore
        }
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i(1024, 1024, 1024)) shader) [@"\)[ \t\r\n]*buffer aBuffer"]

[<Test>]
let ``Atomics on Uniform-Storage``() =
    Setup.Run()
    
    let shader (a : int[]) =
        compute {
            atomicAdd &&uniform.Storage.[0] 1 |> ignore
        }
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i(1024, 1024, 1024)) shader) [@"\)[ \t\r\n]*buffer StorageBuffer"]

[<Test>]
let ``Write on Argument-Storage``() =
    Setup.Run()
    
    let shader (a : int[]) =
        compute {
            a.[0] <- 1
        }
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i(1024, 1024, 1024)) shader) [@"\)[ \t\r\n]*writeonly buffer aBuffer"]

[<Test>]
let ``Write on Uniform-Storage``() =
    Setup.Run()
    
    let shader (a : int[]) =
        compute {
            uniform.Storage.[0] <- 1
        }
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i(1024, 1024, 1024)) shader) [@"\)[ \t\r\n]*writeonly buffer StorageBuffer"]

[<Test>]
let ``Read on Argument-Storage``() =
    Setup.Run()
    
    let shader (a : int[]) (x : int[]) =
        compute {
            x.[0] <- a.[0]
        }
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i(1024, 1024, 1024)) shader) [@"\)[ \t\r\n]*readonly buffer aBuffer"]

[<Test>]
let ``Read on Uniform-Storage``() =
    Setup.Run()
    
    let shader (x : int[]) =
        compute {
            x.[0] <- uniform.Storage.[0]
        }
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i(1024, 1024, 1024)) shader) [@"\)[ \t\r\n]*readonly buffer StorageBuffer"]

[<Test>]
let ``Abs``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| (int8    v.c.X).Abs()
            let _ = assertT <| (int16   v.c.X).Abs()
            let _ = assertT <| (int32   v.c.X).Abs()
            let _ = assertT <| (int64   v.c.X).Abs()
            let _ = assertT <| (float32 v.c.X).Abs()
            let _ = assertT <| (float   v.c.X).Abs()
            let _ = assertT <| (V2f v.c).Abs()
            let _ = assertT <| (V3f v.c).Abs()
            let _ = assertT <| (V4f v.c).Abs()
            let _ = assertT <| (V2d v.c).Abs()
            let _ = assertT <| (V3d v.c).Abs()
            let _ = assertT <| (V4d v.c).Abs()
            let _ = assertT <| (V2i v.c).Abs()
            let _ = assertT <| (V3i v.c).Abs()
            let _ = assertT <| (V4i v.c).Abs()
            let _ = assertT <| (V2l v.c).Abs()
            let _ = assertT <| (V3l v.c).Abs()
            let _ = assertT <| (V4l v.c).Abs()

            let _ = assertT <| abs (int8    v.c.X)
            let _ = assertT <| abs (int16   v.c.X)
            let _ = assertT <| abs (int32   v.c.X)
            let _ = assertT <| abs (int64   v.c.X)
            let _ = assertT <| abs (float32 v.c.X)
            let _ = assertT <| abs (float   v.c.X)
            let _ = assertT <| abs (V2f v.c)
            let _ = assertT <| abs (V3f v.c)
            let _ = assertT <| abs (V4f v.c)
            let _ = assertT <| abs (V2d v.c)
            let _ = assertT <| abs (V3d v.c)
            let _ = assertT <| abs (V4d v.c)
            let _ = assertT <| abs (V2i v.c)
            let _ = assertT <| abs (V3i v.c)
            let _ = assertT <| abs (V4i v.c)
            let _ = assertT <| abs (V2l v.c)
            let _ = assertT <| abs (V3l v.c)
            let _ = assertT <| abs (V4l v.c)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["abs"]

[<Test>]
let ``Min``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Min(int8 v.c.X,      int8 v.c.X)
            let _ = assertT <| Fun.Min(int16 v.c.X,     int16 v.c.X)
            let _ = assertT <| Fun.Min(int32 v.c.X,     int32 v.c.X)
            let _ = assertT <| Fun.Min(int64 v.c.X,     int64 v.c.X)
            let _ = assertT <| Fun.Min(uint8 v.c.X,     uint8 v.c.X)
            let _ = assertT <| Fun.Min(uint16 v.c.X,    uint16 v.c.X)
            let _ = assertT <| Fun.Min(uint32 v.c.X,    uint32 v.c.X)
            let _ = assertT <| Fun.Min(uint64 v.c.X,    uint64 v.c.X)
            let _ = assertT <| Fun.Min(float v.c.X,     float v.c.X)
            let _ = assertT <| Fun.Min(v.c.X,           v.c.X)
            let _ = assertT <| Fun.Min(v.c.XY,          v.c.XY)
            let _ = assertT <| Fun.Min(v.c.XYZ,         v.c.XYZ)
            let _ = assertT <| Fun.Min(v.c,             v.c)
            let _ = assertT <| Fun.Min(V2d v.c.XY,      V2d v.c.XY)
            let _ = assertT <| Fun.Min(V3d v.c.XYZ,     V3d v.c.XYZ)
            let _ = assertT <| Fun.Min(V4d v.c,         V4d v.c)
            let _ = assertT <| Fun.Min(V2i v.c.XY,      V2i v.c.XY)
            let _ = assertT <| Fun.Min(V3i v.c.XYZ,     V3i v.c.XYZ)
            let _ = assertT <| Fun.Min(V4i v.c,         V4i v.c)
            let _ = assertT <| Fun.Min(V2ui v.c.XY,     V2ui v.c.XY)
            let _ = assertT <| Fun.Min(V3ui v.c.XYZ,    V3ui v.c.XYZ)
            let _ = assertT <| Fun.Min(V4ui v.c,        V4ui v.c)
            let _ = assertT <| Fun.Min(V2l v.c.XY,      V2l v.c.XY)
            let _ = assertT <| Fun.Min(V3l v.c.XYZ,     V3l v.c.XYZ)
            let _ = assertT <| Fun.Min(V4l v.c,         V4l v.c)
            let _ = assertT <| Fun.Min(v.c.X,           v.c.XY)
            let _ = assertT <| Fun.Min(v.c.X,           v.c.XYZ)
            let _ = assertT <| Fun.Min(v.c.X,           v.c)
            let _ = assertT <| Fun.Min(float v.c.X,     V2d v.c.XY)
            let _ = assertT <| Fun.Min(float v.c.X,     V3d v.c.XYZ)
            let _ = assertT <| Fun.Min(float v.c.X,     V4d v.c)
            let _ = assertT <| Fun.Min(int v.c.X,       V2i v.c.XY)
            let _ = assertT <| Fun.Min(int v.c.X,       V3i v.c.XYZ)
            let _ = assertT <| Fun.Min(int v.c.X,       V4i v.c)
            let _ = assertT <| Fun.Min(uint32 v.c.X,    V2ui v.c.XY)
            let _ = assertT <| Fun.Min(uint32 v.c.X,    V3ui v.c.XYZ)
            let _ = assertT <| Fun.Min(uint32 v.c.X,    V4ui v.c)
            let _ = assertT <| Fun.Min(int64 v.c.X,     V2l v.c.XY)
            let _ = assertT <| Fun.Min(int64 v.c.X,     V3l v.c.XYZ)
            let _ = assertT <| Fun.Min(int64 v.c.X,     V4l v.c)
            let _ = assertT <| Fun.Min(v.c.XY,          v.c.X)
            let _ = assertT <| Fun.Min(v.c.XYZ,         v.c.X)
            let _ = assertT <| Fun.Min(v.c,             v.c.X)
            let _ = assertT <| Fun.Min(V2d v.c.XY,      float v.c.X)
            let _ = assertT <| Fun.Min(V3d v.c.XYZ,     float v.c.X)
            let _ = assertT <| Fun.Min(V4d v.c,         float v.c.X)
            let _ = assertT <| Fun.Min(V2i v.c.XY,      int v.c.X)
            let _ = assertT <| Fun.Min(V3i v.c.XYZ,     int v.c.X)
            let _ = assertT <| Fun.Min(V4i v.c,         int v.c.X)
            let _ = assertT <| Fun.Min(V2ui v.c.XY,     uint32 v.c.X)
            let _ = assertT <| Fun.Min(V3ui v.c.XYZ,    uint32 v.c.X)
            let _ = assertT <| Fun.Min(V4ui v.c,        uint32 v.c.X)
            let _ = assertT <| Fun.Min(V2l v.c.XY,      int64 v.c.X)
            let _ = assertT <| Fun.Min(V3l v.c.XYZ,     int64 v.c.X)
            let _ = assertT <| Fun.Min(V4l v.c,         int64 v.c.X)
            let _ = assertT <| min (int8 v.c.X)      (int8 v.c.X)
            let _ = assertT <| min (int16 v.c.X)     (int16 v.c.X)
            let _ = assertT <| min (int32 v.c.X)     (int32 v.c.X)
            let _ = assertT <| min (int64 v.c.X)     (int64 v.c.X)
            let _ = assertT <| min (uint8 v.c.X)     (uint8 v.c.X)
            let _ = assertT <| min (uint16 v.c.X)    (uint16 v.c.X)
            let _ = assertT <| min (uint32 v.c.X)    (uint32 v.c.X)
            let _ = assertT <| min (uint64 v.c.X)    (uint64 v.c.X)
            let _ = assertT <| min (float v.c.X)     (float v.c.X)
            let _ = assertT <| min (v.c.X)           (v.c.X)
            let _ = assertT <| min (v.c.XY)          (v.c.XY)
            let _ = assertT <| min (v.c.XYZ)         (v.c.XYZ)
            let _ = assertT <| min (v.c)             (v.c)
            let _ = assertT <| min (V2d v.c.XY)      (V2d v.c.XY)
            let _ = assertT <| min (V3d v.c.XYZ)     (V3d v.c.XYZ)
            let _ = assertT <| min (V4d v.c)         (V4d v.c)
            let _ = assertT <| min (V2i v.c.XY)      (V2i v.c.XY)
            let _ = assertT <| min (V3i v.c.XYZ)     (V3i v.c.XYZ)
            let _ = assertT <| min (V4i v.c)         (V4i v.c)
            let _ = assertT <| min (V2ui v.c.XY)     (V2ui v.c.XY)
            let _ = assertT <| min (V3ui v.c.XYZ)    (V3ui v.c.XYZ)
            let _ = assertT <| min (V4ui v.c)        (V4ui v.c)
            let _ = assertT <| min (V2l v.c.XY)      (V2l v.c.XY)
            let _ = assertT <| min (V3l v.c.XYZ)     (V3l v.c.XYZ)
            let _ = assertT <| min (V4l v.c)         (V4l v.c)
            let _ = assertT <| min (v.c.X)           (v.c.XY)
            let _ = assertT <| min (v.c.X)           (v.c.XYZ)
            let _ = assertT <| min (v.c.X)           (v.c)
            let _ = assertT <| min (float v.c.X)     (V2d v.c.XY)
            let _ = assertT <| min (float v.c.X)     (V3d v.c.XYZ)
            let _ = assertT <| min (float v.c.X)     (V4d v.c)
            let _ = assertT <| min (int v.c.X)       (V2i v.c.XY)
            let _ = assertT <| min (int v.c.X)       (V3i v.c.XYZ)
            let _ = assertT <| min (int v.c.X)       (V4i v.c)
            let _ = assertT <| min (uint v.c.X)      (V2ui v.c.XY)
            let _ = assertT <| min (uint v.c.X)      (V3ui v.c.XYZ)
            let _ = assertT <| min (uint v.c.X)      (V4ui v.c)
            let _ = assertT <| min (int64 v.c.X)     (V2l v.c.XY)
            let _ = assertT <| min (int64 v.c.X)     (V3l v.c.XYZ)
            let _ = assertT <| min (int64 v.c.X)     (V4l v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["min"]

[<Test>]
let ``Max``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Max(int8 v.c.X,      int8 v.c.X)
            let _ = assertT <| Fun.Max(int16 v.c.X,     int16 v.c.X)
            let _ = assertT <| Fun.Max(int32 v.c.X,     int32 v.c.X)
            let _ = assertT <| Fun.Max(int64 v.c.X,     int64 v.c.X)
            let _ = assertT <| Fun.Max(uint8 v.c.X,     uint8 v.c.X)
            let _ = assertT <| Fun.Max(uint16 v.c.X,    uint16 v.c.X)
            let _ = assertT <| Fun.Max(uint32 v.c.X,    uint32 v.c.X)
            let _ = assertT <| Fun.Max(uint64 v.c.X,    uint64 v.c.X)
            let _ = assertT <| Fun.Max(float v.c.X,     float v.c.X)
            let _ = assertT <| Fun.Max(v.c.X,           v.c.X)
            let _ = assertT <| Fun.Max(v.c.XY,          v.c.XY)
            let _ = assertT <| Fun.Max(v.c.XYZ,         v.c.XYZ)
            let _ = assertT <| Fun.Max(v.c,             v.c)
            let _ = assertT <| Fun.Max(V2d v.c.XY,      V2d v.c.XY)
            let _ = assertT <| Fun.Max(V3d v.c.XYZ,     V3d v.c.XYZ)
            let _ = assertT <| Fun.Max(V4d v.c,         V4d v.c)
            let _ = assertT <| Fun.Max(V2i v.c.XY,      V2i v.c.XY)
            let _ = assertT <| Fun.Max(V3i v.c.XYZ,     V3i v.c.XYZ)
            let _ = assertT <| Fun.Max(V4i v.c,         V4i v.c)
            let _ = assertT <| Fun.Max(V2ui v.c.XY,     V2ui v.c.XY)
            let _ = assertT <| Fun.Max(V3ui v.c.XYZ,    V3ui v.c.XYZ)
            let _ = assertT <| Fun.Max(V4ui v.c,        V4ui v.c)
            let _ = assertT <| Fun.Max(V2l v.c.XY,      V2l v.c.XY)
            let _ = assertT <| Fun.Max(V3l v.c.XYZ,     V3l v.c.XYZ)
            let _ = assertT <| Fun.Max(V4l v.c,         V4l v.c)
            let _ = assertT <| Fun.Max(v.c.X,           v.c.XY)
            let _ = assertT <| Fun.Max(v.c.X,           v.c.XYZ)
            let _ = assertT <| Fun.Max(v.c.X,           v.c)
            let _ = assertT <| Fun.Max(float v.c.X,     V2d v.c.XY)
            let _ = assertT <| Fun.Max(float v.c.X,     V3d v.c.XYZ)
            let _ = assertT <| Fun.Max(float v.c.X,     V4d v.c)
            let _ = assertT <| Fun.Max(int v.c.X,       V2i v.c.XY)
            let _ = assertT <| Fun.Max(int v.c.X,       V3i v.c.XYZ)
            let _ = assertT <| Fun.Max(int v.c.X,       V4i v.c)
            let _ = assertT <| Fun.Max(uint32 v.c.X,    V2ui v.c.XY)
            let _ = assertT <| Fun.Max(uint32 v.c.X,    V3ui v.c.XYZ)
            let _ = assertT <| Fun.Max(uint32 v.c.X,    V4ui v.c)
            let _ = assertT <| Fun.Max(int64 v.c.X,     V2l v.c.XY)
            let _ = assertT <| Fun.Max(int64 v.c.X,     V3l v.c.XYZ)
            let _ = assertT <| Fun.Max(int64 v.c.X,     V4l v.c)
            let _ = assertT <| Fun.Max(v.c.XY,          v.c.X)
            let _ = assertT <| Fun.Max(v.c.XYZ,         v.c.X)
            let _ = assertT <| Fun.Max(v.c,             v.c.X)
            let _ = assertT <| Fun.Max(V2d v.c.XY,      float v.c.X)
            let _ = assertT <| Fun.Max(V3d v.c.XYZ,     float v.c.X)
            let _ = assertT <| Fun.Max(V4d v.c,         float v.c.X)
            let _ = assertT <| Fun.Max(V2i v.c.XY,      int v.c.X)
            let _ = assertT <| Fun.Max(V3i v.c.XYZ,     int v.c.X)
            let _ = assertT <| Fun.Max(V4i v.c,         int v.c.X)
            let _ = assertT <| Fun.Max(V2ui v.c.XY,     uint32 v.c.X)
            let _ = assertT <| Fun.Max(V3ui v.c.XYZ,    uint32 v.c.X)
            let _ = assertT <| Fun.Max(V4ui v.c,        uint32 v.c.X)
            let _ = assertT <| Fun.Max(V2l v.c.XY,      int64 v.c.X)
            let _ = assertT <| Fun.Max(V3l v.c.XYZ,     int64 v.c.X)
            let _ = assertT <| Fun.Max(V4l v.c,         int64 v.c.X)
            let _ = assertT <| max (int8 v.c.X)      (int8 v.c.X)
            let _ = assertT <| max (int16 v.c.X)     (int16 v.c.X)
            let _ = assertT <| max (int32 v.c.X)     (int32 v.c.X)
            let _ = assertT <| max (int64 v.c.X)     (int64 v.c.X)
            let _ = assertT <| max (uint8 v.c.X)     (uint8 v.c.X)
            let _ = assertT <| max (uint16 v.c.X)    (uint16 v.c.X)
            let _ = assertT <| max (uint32 v.c.X)    (uint32 v.c.X)
            let _ = assertT <| max (uint64 v.c.X)    (uint64 v.c.X)
            let _ = assertT <| max (float v.c.X)     (float v.c.X)
            let _ = assertT <| max (v.c.X)           (v.c.X)
            let _ = assertT <| max (v.c.XY)          (v.c.XY)
            let _ = assertT <| max (v.c.XYZ)         (v.c.XYZ)
            let _ = assertT <| max (v.c)             (v.c)
            let _ = assertT <| max (V2d v.c.XY)      (V2d v.c.XY)
            let _ = assertT <| max (V3d v.c.XYZ)     (V3d v.c.XYZ)
            let _ = assertT <| max (V4d v.c)         (V4d v.c)
            let _ = assertT <| max (V2i v.c.XY)      (V2i v.c.XY)
            let _ = assertT <| max (V3i v.c.XYZ)     (V3i v.c.XYZ)
            let _ = assertT <| max (V4i v.c)         (V4i v.c)
            let _ = assertT <| max (V2ui v.c.XY)     (V2ui v.c.XY)
            let _ = assertT <| max (V3ui v.c.XYZ)    (V3ui v.c.XYZ)
            let _ = assertT <| max (V4ui v.c)        (V4ui v.c)
            let _ = assertT <| max (V2l v.c.XY)      (V2l v.c.XY)
            let _ = assertT <| max (V3l v.c.XYZ)     (V3l v.c.XYZ)
            let _ = assertT <| max (V4l v.c)         (V4l v.c)
            let _ = assertT <| max (v.c.X)           (v.c.XY)
            let _ = assertT <| max (v.c.X)           (v.c.XYZ)
            let _ = assertT <| max (v.c.X)           (v.c)
            let _ = assertT <| max (float v.c.X)     (V2d v.c.XY)
            let _ = assertT <| max (float v.c.X)     (V3d v.c.XYZ)
            let _ = assertT <| max (float v.c.X)     (V4d v.c)
            let _ = assertT <| max (int v.c.X)       (V2i v.c.XY)
            let _ = assertT <| max (int v.c.X)       (V3i v.c.XYZ)
            let _ = assertT <| max (int v.c.X)       (V4i v.c)
            let _ = assertT <| max (uint v.c.X)      (V2ui v.c.XY)
            let _ = assertT <| max (uint v.c.X)      (V3ui v.c.XYZ)
            let _ = assertT <| max (uint v.c.X)      (V4ui v.c)
            let _ = assertT <| max (int64 v.c.X)     (V2l v.c.XY)
            let _ = assertT <| max (int64 v.c.X)     (V3l v.c.XYZ)
            let _ = assertT <| max (int64 v.c.X)     (V4l v.c)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["max"]

[<Test>]
let ``Clamp``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Clamp(int8 v.c.X,   int8 v.c.X,   int8 v.c.X)
            let _ = assertT <| Fun.Clamp(int16 v.c.X,  int16 v.c.X,  int16 v.c.X)
            let _ = assertT <| Fun.Clamp(int32 v.c.X,  int32 v.c.X,  int32 v.c.X)
            let _ = assertT <| Fun.Clamp(int64 v.c.X,  int64 v.c.X,  int64 v.c.X)
            let _ = assertT <| Fun.Clamp(uint8 v.c.X,  uint8 v.c.X,  uint8 v.c.X)
            let _ = assertT <| Fun.Clamp(uint16 v.c.X, uint16 v.c.X, uint16 v.c.X)
            let _ = assertT <| Fun.Clamp(uint32 v.c.X, uint32 v.c.X, uint32 v.c.X)
            let _ = assertT <| Fun.Clamp(uint64 v.c.X, uint64 v.c.X, uint64 v.c.X)
            let _ = assertT <| Fun.Clamp(float v.c.X,  float v.c.X,  float v.c.X)
            let _ = assertT <| Fun.Clamp(v.c.X,        v.c.X,        v.c.X)

            let _ = assertT <| Fun.Clamp(V4f v.c,      V4f v.c,      V4f v.c)
            let _ = assertT <| Fun.Clamp(V4f v.c,      v.c.X,        v.c.X)
            let _ = assertT <| Fun.Clamp(V3f v.c,      V3f v.c,      V3f v.c)
            let _ = assertT <| Fun.Clamp(V3f v.c,      v.c.X,        v.c.X)
            let _ = assertT <| Fun.Clamp(V2f v.c,      V2f v.c,      V2f v.c)
            let _ = assertT <| Fun.Clamp(V2f v.c,      v.c.X,        v.c.X)

            let _ = assertT <| Fun.Clamp(V4d v.c,      V4d v.c,      V4d v.c)
            let _ = assertT <| Fun.Clamp(V4d v.c,      float v.c.X,  float v.c.X)
            let _ = assertT <| Fun.Clamp(V3d v.c,      V3d v.c,      V3d v.c)
            let _ = assertT <| Fun.Clamp(V3d v.c,      float v.c.X,  float v.c.X)
            let _ = assertT <| Fun.Clamp(V2d v.c,      V2d v.c,      V2d v.c)
            let _ = assertT <| Fun.Clamp(V2d v.c,      float v.c.X,  float v.c.X)

            let _ = assertT <| Fun.Clamp(V4i v.c,      V4i v.c,      V4i v.c)
            let _ = assertT <| Fun.Clamp(V4i v.c,      int v.c.X,    int v.c.X)
            let _ = assertT <| Fun.Clamp(V3i v.c,      V3i v.c,      V3i v.c)
            let _ = assertT <| Fun.Clamp(V3i v.c,      int v.c.X,    int v.c.X)
            let _ = assertT <| Fun.Clamp(V2i v.c,      V2i v.c,      V2i v.c)
            let _ = assertT <| Fun.Clamp(V2i v.c,      int v.c.X,    int v.c.X)

            let _ = assertT <| Fun.Clamp(V4ui v.c,     V4ui v.c,     V4ui v.c)
            let _ = assertT <| Fun.Clamp(V4ui v.c,     uint v.c.X,   uint v.c.X)
            let _ = assertT <| Fun.Clamp(V3ui v.c,     V3ui v.c,     V3ui v.c)
            let _ = assertT <| Fun.Clamp(V3ui v.c,     uint v.c.X,   uint v.c.X)
            let _ = assertT <| Fun.Clamp(V2ui v.c,     V2ui v.c,     V2ui v.c)
            let _ = assertT <| Fun.Clamp(V2ui v.c,     uint v.c.X,   uint v.c.X)

            let _ = assertT <| Fun.Clamp(V4l v.c,      V4l v.c,      V4l v.c)
            let _ = assertT <| Fun.Clamp(V4l v.c,      int64 v.c.X,  int64 v.c.X)
            let _ = assertT <| Fun.Clamp(V3l v.c,      V3l v.c,      V3l v.c)
            let _ = assertT <| Fun.Clamp(V3l v.c,      int64 v.c.X,  int64 v.c.X)
            let _ = assertT <| Fun.Clamp(V2l v.c,      V2l v.c,      V2l v.c)
            let _ = assertT <| Fun.Clamp(V2l v.c,      int64 v.c.X,  int64 v.c.X)

            let _ = assertT <| clamp (int8 v.c.X)   (int8 v.c.X)   (int8 v.c.X)
            let _ = assertT <| clamp (int16 v.c.X)  (int16 v.c.X)  (int16 v.c.X)
            let _ = assertT <| clamp (int32 v.c.X)  (int32 v.c.X)  (int32 v.c.X)
            let _ = assertT <| clamp (int64 v.c.X)  (int64 v.c.X)  (int64 v.c.X)
            let _ = assertT <| clamp (uint8 v.c.X)  (uint8 v.c.X)  (uint8 v.c.X)
            let _ = assertT <| clamp (uint16 v.c.X) (uint16 v.c.X) (uint16 v.c.X)
            let _ = assertT <| clamp (uint32 v.c.X) (uint32 v.c.X) (uint32 v.c.X)
            let _ = assertT <| clamp (uint64 v.c.X) (uint64 v.c.X) (uint64 v.c.X)
            let _ = assertT <| clamp (float v.c.X)  (float v.c.X)  (float v.c.X)
            let _ = assertT <| clamp (v.c.X)        (v.c.X)        (v.c.X)

            let _ = assertT <| clamp (V4f v.c)      (V4f v.c)      (V4f v.c)
            let _ = assertT <| clamp (v.c.X)        (V4f v.c)      (V4f v.c)
            let _ = assertT <| clamp (V4f v.c)      (v.c.X)        (V4f v.c)
            let _ = assertT <| clamp (v.c.X)        (v.c.X)        (V4f v.c)
            let _ = assertT <| clamp (V3f v.c)      (V3f v.c)      (V3f v.c)
            let _ = assertT <| clamp (v.c.X)        (V3f v.c)      (V3f v.c)
            let _ = assertT <| clamp (V3f v.c)      (v.c.X)        (V3f v.c)
            let _ = assertT <| clamp (v.c.X)        (v.c.X)        (V3f v.c)
            let _ = assertT <| clamp (V2f v.c)      (V2f v.c)      (V2f v.c)
            let _ = assertT <| clamp (v.c.X)        (V2f v.c)      (V2f v.c)
            let _ = assertT <| clamp (V2f v.c)      (v.c.X)        (V2f v.c)
            let _ = assertT <| clamp (v.c.X)        (v.c.X)        (V2f v.c)

            let _ = assertT <| clamp (V4d v.c)      (V4d v.c)      (V4d v.c)
            let _ = assertT <| clamp (float v.c.X)  (V4d v.c)      (V4d v.c)
            let _ = assertT <| clamp (V4d v.c)      (float v.c.X)  (V4d v.c)
            let _ = assertT <| clamp (float v.c.X)  (float v.c.X)  (V4d v.c)
            let _ = assertT <| clamp (V3d v.c)      (V3d v.c)      (V3d v.c)
            let _ = assertT <| clamp (float v.c.X)  (V3d v.c)      (V3d v.c)
            let _ = assertT <| clamp (V3d v.c)      (float v.c.X)  (V3d v.c)
            let _ = assertT <| clamp (float v.c.X)  (float v.c.X)  (V3d v.c)
            let _ = assertT <| clamp (V2d v.c)      (V2d v.c)      (V2d v.c)
            let _ = assertT <| clamp (float v.c.X)  (V2d v.c)      (V2d v.c)
            let _ = assertT <| clamp (V2d v.c)      (float v.c.X)  (V2d v.c)
            let _ = assertT <| clamp (float v.c.X)  (float v.c.X)  (V2d v.c)

            let _ = assertT <| clamp (V4i v.c)      (V4i v.c)      (V4i v.c)
            let _ = assertT <| clamp (int v.c.X)    (V4i v.c)      (V4i v.c)
            let _ = assertT <| clamp (V4i v.c)      (int v.c.X)    (V4i v.c)
            let _ = assertT <| clamp (int v.c.X)    (int v.c.X)    (V4i v.c)
            let _ = assertT <| clamp (V3i v.c)      (V3i v.c)      (V3i v.c)
            let _ = assertT <| clamp (int v.c.X)    (V3i v.c)      (V3i v.c)
            let _ = assertT <| clamp (V3i v.c)      (int v.c.X)    (V3i v.c)
            let _ = assertT <| clamp (int v.c.X)    (int v.c.X)    (V3i v.c)
            let _ = assertT <| clamp (V2i v.c)      (V2i v.c)      (V2i v.c)
            let _ = assertT <| clamp (int v.c.X)    (V2i v.c)      (V2i v.c)
            let _ = assertT <| clamp (V2i v.c)      (int v.c.X)    (V2i v.c)
            let _ = assertT <| clamp (int v.c.X)    (int v.c.X)    (V2i v.c)

            let _ = assertT <| clamp (V4ui v.c)     (V4ui v.c)     (V4ui v.c)
            let _ = assertT <| clamp (V3ui v.c)     (V3ui v.c)     (V3ui v.c)
            let _ = assertT <| clamp (V2ui v.c)     (V2ui v.c)     (V2ui v.c)

            let _ = assertT <| clamp (V4l v.c)      (V4l v.c)      (V4l v.c)
            let _ = assertT <| clamp (int64 v.c.X)  (V4l v.c)      (V4l v.c)
            let _ = assertT <| clamp (V4l v.c)      (int64 v.c.X)  (V4l v.c)
            let _ = assertT <| clamp (int64 v.c.X)  (int64 v.c.X)  (V4l v.c)
            let _ = assertT <| clamp (V3l v.c)      (V3l v.c)      (V3l v.c)
            let _ = assertT <| clamp (int64 v.c.X)  (V3l v.c)      (V3l v.c)
            let _ = assertT <| clamp (V3l v.c)      (int64 v.c.X)  (V3l v.c)
            let _ = assertT <| clamp (int64 v.c.X)  (int64 v.c.X)  (V3l v.c)
            let _ = assertT <| clamp (V2l v.c)      (V2l v.c)      (V2l v.c)
            let _ = assertT <| clamp (int64 v.c.X)  (V2l v.c)      (V2l v.c)
            let _ = assertT <| clamp (V2l v.c)      (int64 v.c.X)  (V2l v.c)
            let _ = assertT <| clamp (int64 v.c.X)  (int64 v.c.X)  (V2l v.c)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["clamp"]

[<Test>]
let ``Saturate``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Fun.Saturate(int8 v.c.X)
            let _ = assertT <| Fun.Saturate(int16 v.c.X)
            let _ = assertT <| Fun.Saturate(int32 v.c.X)
            let _ = assertT <| Fun.Saturate(int64 v.c.X)
            let _ = assertT <| Fun.Saturate(uint8 v.c.X)
            let _ = assertT <| Fun.Saturate(uint16 v.c.X)
            let _ = assertT <| Fun.Saturate(uint32 v.c.X)
            let _ = assertT <| Fun.Saturate(uint64 v.c.X)
            let _ = assertT <| Fun.Saturate(float v.c.X)
            let _ = assertT <| Fun.Saturate(v.c.X)
            let _ = assertT <| Fun.Saturate(V4f v.c)
            let _ = assertT <| Fun.Saturate(V3f v.c)
            let _ = assertT <| Fun.Saturate(V2f v.c)
            let _ = assertT <| Fun.Saturate(V4d v.c)
            let _ = assertT <| Fun.Saturate(V3d v.c)
            let _ = assertT <| Fun.Saturate(V2d v.c)
            let _ = assertT <| Fun.Saturate(V4i v.c)
            let _ = assertT <| Fun.Saturate(V3i v.c)
            let _ = assertT <| Fun.Saturate(V2i v.c)
            let _ = assertT <| Fun.Saturate(V4ui v.c)
            let _ = assertT <| Fun.Saturate(V3ui v.c)
            let _ = assertT <| Fun.Saturate(V2ui v.c)
            let _ = assertT <| Fun.Saturate(V4l v.c)
            let _ = assertT <| Fun.Saturate(V3l v.c)
            let _ = assertT <| Fun.Saturate(V2l v.c)

            let _ = assertT <| saturate (int8 v.c.X)
            let _ = assertT <| saturate (int16 v.c.X)
            let _ = assertT <| saturate (int32 v.c.X)
            let _ = assertT <| saturate (int64 v.c.X)
            let _ = assertT <| saturate (uint8 v.c.X)
            let _ = assertT <| saturate (uint16 v.c.X)
            let _ = assertT <| saturate (uint32 v.c.X)
            let _ = assertT <| saturate (uint64 v.c.X)
            let _ = assertT <| saturate (float v.c.X)
            let _ = assertT <| saturate (v.c.X)
            let _ = assertT <| saturate (V4f v.c)
            let _ = assertT <| saturate (V3f v.c)
            let _ = assertT <| saturate (V2f v.c)
            let _ = assertT <| saturate (V4d v.c)
            let _ = assertT <| saturate (V3d v.c)
            let _ = assertT <| saturate (V2d v.c)
            let _ = assertT <| saturate (V4i v.c)
            let _ = assertT <| saturate (V3i v.c)
            let _ = assertT <| saturate (V2i v.c)
            let _ = assertT <| saturate (V4ui v.c)
            let _ = assertT <| saturate (V3ui v.c)
            let _ = assertT <| saturate (V2ui v.c)
            let _ = assertT <| saturate (V4l v.c)
            let _ = assertT <| saturate (V3l v.c)
            let _ = assertT <| saturate (V2l v.c)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["clamp"]

[<Test>]
let ``Step``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| step 0.5f v.c.X
            let _ = assertT <| step 0.5f v.c.XY
            let _ = assertT <| step 0.5f v.c.XYZ
            let _ = assertT <| step 0.5f v.c
            let _ = assertT <| step (V2f(0.5)) v.c.XY
            let _ = assertT <| step (V3f(0.5)) v.c.XYZ
            let _ = assertT <| step (V4f(0.5)) v.c
            let _ = assertT <| step 0.5 (float v.c.X)
            let _ = assertT <| step 0.5 (v2d v.c.XY)
            let _ = assertT <| step 0.5 (v3d v.c.XYZ)
            let _ = assertT <| step 0.5 (v4d v.c)
            let _ = assertT <| step (V2d(0.5)) (v2d v.c.XY)
            let _ = assertT <| step (V3d(0.5)) (v3d v.c.XYZ)
            let _ = assertT <| step (V4d(0.5)) (v4d v.c)
            let _ = assertT <| Fun.Step(v.c.X, 0.5f)
            let _ = assertT <| Fun.Step(v.c.XY, 0.5f)
            let _ = assertT <| Fun.Step(v.c.XYZ, 0.5f)
            let _ = assertT <| Fun.Step(v.c, 0.5f)
            let _ = assertT <| Fun.Step(v.c.XY, V2f(0.5))
            let _ = assertT <| Fun.Step(v.c.XYZ, V3f(0.5))
            let _ = assertT <| Fun.Step(v.c, V4f(0.5))
            let _ = assertT <| Fun.Step((float v.c.X), 0.5)
            let _ = assertT <| Fun.Step((v2d v.c.XY), 0.5)
            let _ = assertT <| Fun.Step((v3d v.c.XYZ), 0.5)
            let _ = assertT <| Fun.Step((v4d v.c), 0.5)
            let _ = assertT <| Fun.Step((v2d v.c.XY), V2d(0.5))
            let _ = assertT <| Fun.Step((v3d v.c.XYZ), V3d(0.5))
            let _ = assertT <| Fun.Step((v4d v.c), V4d(0.5))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["step"]

[<Test>]
let ``Linearstep``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = linearstep 0.5f 1.0f v.c.X
            let _ = linearstep (V2f(0.5)) (V2f(1.0f)) v.c.XY
            let _ = linearstep (V3f(0.5)) (V3f(1.0f)) v.c.XYZ
            let _ = linearstep (getVec()) (V4f(1.0f)) v.c
            let _ = linearstep 0.5f 1.0f (float32 v.c.X)
            let _ = linearstep (V2f(0.5f)) (V2f(1.0f)) (v2f v.c.XY)
            let _ = linearstep (V3f(0.5f)) (V3f(1.0f)) (v3f v.c.XYZ)
            let _ = linearstep (V4f(0.5f)) (V4f(1.0f)) (v4f v.c)
            let _ = Fun.Linearstep(v.c.X, 0.5f, 1.0f)
            let _ = Fun.Linearstep(v.c.XY, V2f(0.5), V2f(1.0f))
            let _ = Fun.Linearstep(v.c.XYZ, V3f(0.5), V3f(1.0f))
            let _ = Fun.Linearstep(v.c, V4f(0.5), V4f(1.0f))
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
            let _ = assertT <| smoothstep 0.5f 0.5f v.c.X
            let _ = assertT <| smoothstep 0.5f 0.5f v.c.XY
            let _ = assertT <| smoothstep 0.5f 0.5f v.c.XYZ
            let _ = assertT <| smoothstep 0.5f 0.5f v.c
            let _ = assertT <| smoothstep (V2f(0.5)) (V2f(0.5)) v.c.XY
            let _ = assertT <| smoothstep (V3f(0.5)) (V3f(0.5)) v.c.XYZ
            let _ = assertT <| smoothstep (V4f(0.5)) (V4f(0.5)) v.c
            let _ = assertT <| smoothstep 0.5 0.5 (float v.c.X)
            let _ = assertT <| smoothstep 0.5 0.5 (v2d v.c.XY)
            let _ = assertT <| smoothstep 0.5 0.5 (v3d v.c.XYZ)
            let _ = assertT <| smoothstep 0.5 0.5 (v4d v.c)
            let _ = assertT <| smoothstep (V2d(0.5)) (V2d(0.5)) (v2d v.c.XY)
            let _ = assertT <| smoothstep (V3d(0.5)) (V3d(0.5)) (v3d v.c.XYZ)
            let _ = assertT <| smoothstep (V4d(0.5)) (V4d(0.5)) (v4d v.c)
            let _ = assertT <| Fun.Smoothstep(v.c.X, 0.5f, 0.5f)
            let _ = assertT <| Fun.Smoothstep(v.c.XY, 0.5f, 0.5f)
            let _ = assertT <| Fun.Smoothstep(v.c.XYZ, 0.5f, 0.5f)
            let _ = assertT <| Fun.Smoothstep(v.c, 0.5f, 0.5f)
            let _ = assertT <| Fun.Smoothstep(v.c.XY, V2f(0.5), V2f(0.5))
            let _ = assertT <| Fun.Smoothstep(v.c.XYZ, V3f(0.5), V3f(0.5))
            let _ = assertT <| Fun.Smoothstep(v.c, V4f(0.5), V4f(0.5))
            let _ = assertT <| Fun.Smoothstep((float v.c.X), 0.5, 0.5)
            let _ = assertT <| Fun.Smoothstep((v2d v.c.XY), 0.5, 0.5)
            let _ = assertT <| Fun.Smoothstep((v3d v.c.XYZ), 0.5, 0.5)
            let _ = assertT <| Fun.Smoothstep((v4d v.c), 0.5, 0.5)
            let _ = assertT <| Fun.Smoothstep((v2d v.c.XY), V2d(0.5), V2d(0.5))
            let _ = assertT <| Fun.Smoothstep((v3d v.c.XYZ), V3d(0.5), V3d(0.5))
            let _ = assertT <| Fun.Smoothstep((v4d v.c), V4d(0.5), V4d(0.5))
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
            let _ = assertT <| Fun.MultiplyAdd(int8 v.c.X,   int8 v.c.X,   int8 v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(int16 v.c.X,  int16 v.c.X,  int16 v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(int32 v.c.X,  int32 v.c.X,  int32 v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(int64 v.c.X,  int64 v.c.X,  int64 v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(uint8 v.c.X,  uint8 v.c.X,  uint8 v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(uint16 v.c.X, uint16 v.c.X, uint16 v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(uint32 v.c.X, uint32 v.c.X, uint32 v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(uint64 v.c.X, uint64 v.c.X, uint64 v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(float v.c.X,  float v.c.X,  float v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(v.c.X,        v.c.X,        v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V4f v.c,      V4f v.c,      V4f v.c)
            let _ = assertT <| Fun.MultiplyAdd(V4f v.c,      v.c.X,        V4f v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(v.c.X,        V4f v.c.X,    V4f v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V3f v.c,      V3f v.c,      V3f v.c)
            let _ = assertT <| Fun.MultiplyAdd(V3f v.c,      v.c.X,        V3f v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(v.c.X,        V3f v.c.X,    V3f v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V2f v.c,      V2f v.c,      V2f v.c)
            let _ = assertT <| Fun.MultiplyAdd(V2f v.c,      v.c.X,        V2f v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(v.c.X,        V2f v.c.X,    V2f v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V4d v.c,      V4d v.c,      V4d v.c)
            let _ = assertT <| Fun.MultiplyAdd(V4d v.c,      float v.c.X,  V4d v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(float v.c.X,  V4d v.c.X,    V4d v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V3d v.c,      V3d v.c,      V3d v.c)
            let _ = assertT <| Fun.MultiplyAdd(V3d v.c,      float v.c.X,  V3d v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(float v.c.X,  V3d v.c.X,    V3d v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V2d v.c,      V2d v.c,      V2d v.c)
            let _ = assertT <| Fun.MultiplyAdd(V2d v.c,      float v.c.X,  V2d v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(float v.c.X,  V2d v.c.X,    V2d v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V4i v.c,      V4i v.c,      V4i v.c)
            let _ = assertT <| Fun.MultiplyAdd(V4i v.c,      int32 v.c.X,  V4i v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(int32 v.c.X,  V4i v.c.X,    V4i v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V3i v.c,      V3i v.c,      V3i v.c)
            let _ = assertT <| Fun.MultiplyAdd(V3i v.c,      int32 v.c.X,  V3i v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(int32 v.c.X,  V3i v.c.X,    V3i v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V2i v.c,      V2i v.c,      V2i v.c)
            let _ = assertT <| Fun.MultiplyAdd(V2i v.c,      int32 v.c.X,  V2i v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(int32 v.c.X,  V2i v.c.X,    V2i v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V4ui v.c,     V4ui v.c,     V4ui v.c)
            let _ = assertT <| Fun.MultiplyAdd(V4ui v.c,     uint32 v.c.X, V4ui v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(uint32 v.c.X, V4ui v.c.X,   V4ui v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V3ui v.c,     V3ui v.c,     V3ui v.c)
            let _ = assertT <| Fun.MultiplyAdd(V3ui v.c,     uint32 v.c.X, V3ui v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(uint32 v.c.X, V3ui v.c.X,   V3ui v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V2ui v.c,     V2ui v.c,     V2ui v.c)
            let _ = assertT <| Fun.MultiplyAdd(V2ui v.c,     uint32 v.c.X, V2ui v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(uint32 v.c.X, V2ui v.c.X,   V2ui v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V4l v.c,      V4l v.c,      V4l v.c)
            let _ = assertT <| Fun.MultiplyAdd(V4l v.c,      int64 v.c.X,  V4l v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(int64 v.c.X,  V4l v.c.X,    V4l v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V3l v.c,      V3l v.c,      V3l v.c)
            let _ = assertT <| Fun.MultiplyAdd(V3l v.c,      int64 v.c.X,  V3l v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(int64 v.c.X,  V3l v.c.X,    V3l v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(V2l v.c,      V2l v.c,      V2l v.c)
            let _ = assertT <| Fun.MultiplyAdd(V2l v.c,      int64 v.c.X,  V2l v.c.X)
            let _ = assertT <| Fun.MultiplyAdd(int64 v.c.X,  V2l v.c.X,    V2l v.c.X)
            
            let _ = assertT <| madd (int8 v.c.X)   (int8 v.c.X)   (int8 v.c.X)
            let _ = assertT <| madd (int16 v.c.X)  (int16 v.c.X)  (int16 v.c.X)
            let _ = assertT <| madd (int32 v.c.X)  (int32 v.c.X)  (int32 v.c.X)
            let _ = assertT <| madd (int64 v.c.X)  (int64 v.c.X)  (int64 v.c.X)
            let _ = assertT <| madd (uint8 v.c.X)  (uint8 v.c.X)  (uint8 v.c.X)
            let _ = assertT <| madd (uint16 v.c.X) (uint16 v.c.X) (uint16 v.c.X)
            let _ = assertT <| madd (uint32 v.c.X) (uint32 v.c.X) (uint32 v.c.X)
            let _ = assertT <| madd (uint64 v.c.X) (uint64 v.c.X) (uint64 v.c.X)
            let _ = assertT <| madd (float v.c.X)  (float v.c.X)  (float v.c.X)
            let _ = assertT <| madd (v.c.X)        (v.c.X)        (v.c.X)
            let _ = assertT <| madd (V4f v.c)      (V4f v.c)      (V4f v.c)
            let _ = assertT <| madd (V4f v.c)      (v.c.X)        (V4f v.c.X)
            let _ = assertT <| madd (V3f v.c)      (V3f v.c)      (V3f v.c)
            let _ = assertT <| madd (V3f v.c)      (v.c.X)        (V3f v.c.X)
            let _ = assertT <| madd (V2f v.c)      (V2f v.c)      (V2f v.c)
            let _ = assertT <| madd (V2f v.c)      (v.c.X)        (V2f v.c.X)
            let _ = assertT <| madd (V4d v.c)      (V4d v.c)      (V4d v.c)
            let _ = assertT <| madd (V4d v.c)      (float v.c.X)  (V4d v.c.X)
            let _ = assertT <| madd (V3d v.c)      (V3d v.c)      (V3d v.c)
            let _ = assertT <| madd (V3d v.c)      (float v.c.X)  (V3d v.c.X)
            let _ = assertT <| madd (V2d v.c)      (V2d v.c)      (V2d v.c)
            let _ = assertT <| madd (V2d v.c)      (float v.c.X)  (V2d v.c.X)
            let _ = assertT <| madd (V4i v.c)      (V4i v.c)      (V4i v.c)
            let _ = assertT <| madd (V4i v.c)      (int32 v.c.X)  (V4i v.c.X)
            let _ = assertT <| madd (V3i v.c)      (V3i v.c)      (V3i v.c)
            let _ = assertT <| madd (V3i v.c)      (int32 v.c.X)  (V3i v.c.X)
            let _ = assertT <| madd (V2i v.c)      (V2i v.c)      (V2i v.c)
            let _ = assertT <| madd (V2i v.c)      (int32 v.c.X)  (V2i v.c.X)
            let _ = assertT <| madd (V4ui v.c)     (V4ui v.c)     (V4ui v.c)
            let _ = assertT <| madd (V4ui v.c)     (uint32 v.c.X) (V4ui v.c.X)
            let _ = assertT <| madd (V3ui v.c)     (V3ui v.c)     (V3ui v.c)
            let _ = assertT <| madd (V3ui v.c)     (uint32 v.c.X) (V3ui v.c.X)
            let _ = assertT <| madd (V2ui v.c)     (V2ui v.c)     (V2ui v.c)
            let _ = assertT <| madd (V2ui v.c)     (uint32 v.c.X) (V2ui v.c.X)
            let _ = assertT <| madd (V4l v.c)      (V4l v.c)      (V4l v.c)
            let _ = assertT <| madd (V4l v.c)      (int64 v.c.X)  (V4l v.c.X)
            let _ = assertT <| madd (V3l v.c)      (V3l v.c)      (V3l v.c)
            let _ = assertT <| madd (V3l v.c)      (int64 v.c.X)  (V3l v.c.X)
            let _ = assertT <| madd (V2l v.c)      (V2l v.c)      (V2l v.c)
            let _ = assertT <| madd (V2l v.c)      (int64 v.c.X)  (V2l v.c.X)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["fma"]

[<Test>]
let ``Degrees / radians``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| degrees v.c
            let _ = assertT <| degrees v.c.X
            let _ = assertT <| v.c.DegreesFromRadians()
            let _ = assertT <| v.c.X.DegreesFromRadians()
            let _ = assertT <| radians v.c
            let _ = assertT <| radians v.c.X
            let _ = assertT <| v.c.RadiansFromDegrees()
            let _ = assertT <| v.c.X.RadiansFromDegrees()
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["degrees"; "radians"]


[<Test>]
let ``Length``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.length v.c
            let _ = assertT <| Vec.Length v.c
            let _ = assertT <| Vec.Length v.c.XYZ
            let _ = assertT <| Vec.Length v.c.XY
            let _ = assertT <| Vec.length (V4d v.c)
            let _ = assertT <| Vec.Length (V4d v.c)
            let _ = assertT <| Vec.Length (V3d v.c.XYZ)
            let _ = assertT <| Vec.Length (V2d v.c.XY)
            let _ = assertT <| Vec.Length v.what
            let _ = assertT <| Vec.Length v.what.XYZ
            let _ = assertT <| Vec.Length v.what.XY
            let _ = assertT <| Vec.Length v.whatu
            let _ = assertT <| Vec.Length v.whatu.XYZ
            let _ = assertT <| Vec.Length v.whatu.XY
            let _ = assertT <| Vec.Length v.whatl
            let _ = assertT <| Vec.Length v.whatl.XYZ
            let _ = assertT <| Vec.Length v.whatl.XY

            let _ = assertT <| v.c.Length
            let _ = assertT <| v.c.XYZ.Length
            let _ = assertT <| v.c.XY.Length
            let _ = assertT <| (V4d v.c).Length
            let _ = assertT <| (V3d v.c.XYZ).Length
            let _ = assertT <| (V2d v.c.XY).Length
            let _ = assertT <| v.what.Length
            let _ = assertT <| v.what.XYZ.Length
            let _ = assertT <| v.what.XY.Length
            let _ = assertT <| v.whatu.Length
            let _ = assertT <| v.whatu.XYZ.Length
            let _ = assertT <| v.whatu.XY.Length
            let _ = assertT <| v.whatl.Length
            let _ = assertT <| v.whatl.XYZ.Length
            let _ = assertT <| v.whatl.XY.Length

            let _ = assertT <| (V4d v.c).Norm2
            let _ = assertT <| (V3d v.c.XYZ).Norm2
            let _ = assertT <| (V2d v.c.XY).Norm2
            let _ = assertT <| v.what.Norm2
            let _ = assertT <| v.what.XYZ.Norm2
            let _ = assertT <| v.what.XY.Norm2
            let _ = assertT <| v.whatu.Norm2
            let _ = assertT <| v.whatu.XYZ.Norm2
            let _ = assertT <| v.whatu.XY.Norm2
            let _ = assertT <| v.whatl.Norm2
            let _ = assertT <| v.whatl.XYZ.Norm2
            let _ = assertT <| v.whatl.XY.Norm2

            let _ = assertT <| Vec.Norm2 v.c
            let _ = assertT <| Vec.Norm2 v.c.XYZ
            let _ = assertT <| Vec.Norm2 v.c.XY
            let _ = assertT <| Vec.Norm2 (V4d v.c)
            let _ = assertT <| Vec.Norm2 (V3d v.c.XYZ)
            let _ = assertT <| Vec.Norm2 (V2d v.c.XY)
            let _ = assertT <| Vec.Norm2 v.what
            let _ = assertT <| Vec.Norm2 v.what.XYZ
            let _ = assertT <| Vec.Norm2 v.what.XY
            let _ = assertT <| Vec.Norm2 v.whatu
            let _ = assertT <| Vec.Norm2 v.whatu.XYZ
            let _ = assertT <| Vec.Norm2 v.whatu.XY
            let _ = assertT <| Vec.Norm2 v.whatl
            let _ = assertT <| Vec.Norm2 v.whatl.XYZ
            let _ = assertT <| Vec.Norm2 v.whatl.XY

            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["length"]

[<Test>]
let ``LengthSquared``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.lengthSquared v.c
            let _ = assertT <| Vec.LengthSquared v.c
            let _ = assertT <| Vec.LengthSquared v.c.XYZ
            let _ = assertT <| Vec.LengthSquared v.c.XY
            let _ = assertT <| Vec.LengthSquared v.what
            let _ = assertT <| Vec.LengthSquared v.what.XYZ
            let _ = assertT <| Vec.LengthSquared v.what.XY
            let _ = assertT <| Vec.LengthSquared v.whatu
            let _ = assertT <| Vec.LengthSquared v.whatu.XYZ
            let _ = assertT <| Vec.LengthSquared v.whatu.XY
            let _ = assertT <| Vec.LengthSquared v.whatl
            let _ = assertT <| Vec.LengthSquared v.whatl.XYZ
            let _ = assertT <| Vec.LengthSquared v.whatl.XY

            let _ = assertT <| Vec.lengthSquared (getVec())
            let _ = assertT <| Vec.LengthSquared(getVec())
            let _ = assertT <| Vec.LengthSquared(getVec().XYZ)
            let _ = assertT <| Vec.LengthSquared(getVec().XY)
            let _ = assertT <| getVec().LengthSquared
            let _ = assertT <| getVec().XYZ.LengthSquared
            let _ = assertT <| getVec().XY.LengthSquared

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 8]

[<Test>]
let ``DistanceSquared``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.distanceSquared v.c v.c
            let _ = assertT <| Vec.DistanceSquared(v.c,         v.c)
            let _ = assertT <| Vec.DistanceSquared(v.c.XYZ,     v.c.XYZ)
            let _ = assertT <| Vec.DistanceSquared(v.c.XY,      v.c.XY)
            let _ = assertT <| Vec.DistanceSquared(v.what,      v.what)
            let _ = assertT <| Vec.DistanceSquared(v.what.XYZ,  v.what.XYZ)
            let _ = assertT <| Vec.DistanceSquared(v.what.XY,   v.what.XY)
            let _ = assertT <| Vec.DistanceSquared(v.whatu,     v.whatu)
            let _ = assertT <| Vec.DistanceSquared(v.whatu.XYZ, v.whatu.XYZ)
            let _ = assertT <| Vec.DistanceSquared(v.whatu.XY,  v.whatu.XY)
            let _ = assertT <| Vec.DistanceSquared(v.whatl,     v.whatl)
            let _ = assertT <| Vec.DistanceSquared(v.whatl.XYZ, v.whatl.XYZ)
            let _ = assertT <| Vec.DistanceSquared(v.whatl.XY,  v.whatl.XY)

            let _ = assertT <| Vec.distanceSquared (getVec()) (getVec())
            let _ = assertT <| Vec.DistanceSquared(getVec(), getVec())
            let _ = assertT <| Vec.DistanceSquared(getVec().XYZ, getVec().XYZ)
            let _ = assertT <| Vec.DistanceSquared(getVec().XY, getVec().XY)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 9]

[<Test>]
let ``Distance1``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.Distance1(v.c, v.c)
            let _ = assertT <| Vec.Distance1(v.c.XYZ,       v.c.XYZ)
            let _ = assertT <| Vec.Distance1(v.c.XY,        v.c.XY)
            let _ = assertT <| Vec.Distance1(v.what,        v.what2)
            let _ = assertT <| Vec.Distance1(v.what.XYZ,    v.what2.XYZ)
            let _ = assertT <| Vec.Distance1(v.what.XY,     v.what2.XY)
            let _ = assertT <| Vec.Distance1(getVecu(),     v.whatu2)
            let _ = assertT <| Vec.Distance1(getVecu().XYZ, v.whatu2.XYZ)
            let _ = assertT <| Vec.Distance1(getVecu().XY,  v.whatu2.XY)
            let _ = assertT <| Vec.Distance1(v.whatl,       v.whatl2)
            let _ = assertT <| Vec.Distance1(v.whatl.XYZ,   v.whatl2.XYZ)
            let _ = assertT <| Vec.Distance1(v.whatl.XY,    v.whatl2.XY)

            let _ = assertT  <| Vec.Distance1(getVec(), getVec())
            let _ = assertT  <| Vec.Distance1(getVec().XYZ, getVec().XYZ)
            let _ = assertT  <| Vec.Distance1(getVec().XY, getVec().XY)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 11]

[<Test>]
let ``Norm1``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| v.what.Norm1
            let _ = assertT <| v.whatu.Norm1
            let _ = assertT <| v.whatl.Norm1
            let _ = assertT <| Vec.Norm1(v.what)
            let _ = assertT <| Vec.Norm1(v.whatu)
            let _ = assertT <| Vec.Norm1(v.whatl)
            let _ = assertT <| getVec().Norm1
            let _ = assertT <| Vec.Norm1(getVec())

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] [ "getVec", 3 ]

[<Test>]
let ``DistanceMin /-Max``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.DistanceMin(v.c,           v.c)
            let _ = assertT <| Vec.DistanceMin(v.c.XYZ,       v.c.XYZ)
            let _ = assertT <| Vec.DistanceMin(v.c.XY,        v.c.XY)
            let _ = assertT <| Vec.DistanceMin(v.what,        v.what2)
            let _ = assertT <| Vec.DistanceMin(v.what.XYZ,    v.what2.XYZ)
            let _ = assertT <| Vec.DistanceMin(v.what.XY,     v.what2.XY)
            let _ = assertT <| Vec.DistanceMin(getVecu(),     v.whatu2)
            let _ = assertT <| Vec.DistanceMin(getVecu().XYZ, v.whatu2.XYZ)
            let _ = assertT <| Vec.DistanceMin(getVecu().XY,  v.whatu2.XY)
            let _ = assertT <| Vec.DistanceMin(v.whatl,       v.whatl2)
            let _ = assertT <| Vec.DistanceMin(v.whatl.XYZ,   v.whatl2.XYZ)
            let _ = assertT <| Vec.DistanceMin(v.whatl.XY,    v.whatl2.XY)

            let _ = assertT <| Vec.DistanceMin(getVec(), getVec())
            let _ = assertT <| Vec.DistanceMin(getVec().XYZ, getVec().XYZ)
            let _ = assertT <| Vec.DistanceMin(getVec().XY, getVec().XY)

            let _ = assertT <| Vec.DistanceMax(v.c,           v.c)
            let _ = assertT <| Vec.DistanceMax(v.c.XYZ,       v.c.XYZ)
            let _ = assertT <| Vec.DistanceMax(v.c.XY,        v.c.XY)
            let _ = assertT <| Vec.DistanceMax(v.what,        v.what2)
            let _ = assertT <| Vec.DistanceMax(v.what.XYZ,    v.what2.XYZ)
            let _ = assertT <| Vec.DistanceMax(v.what.XY,     v.what2.XY)
            let _ = assertT <| Vec.DistanceMax(getVecu(),     v.whatu2)
            let _ = assertT <| Vec.DistanceMax(getVecu().XYZ, v.whatu2.XYZ)
            let _ = assertT <| Vec.DistanceMax(getVecu().XY,  v.whatu2.XY)
            let _ = assertT <| Vec.DistanceMax(v.whatl,       v.whatl2)
            let _ = assertT <| Vec.DistanceMax(v.whatl.XYZ,   v.whatl2.XYZ)
            let _ = assertT <| Vec.DistanceMax(v.whatl.XY,    v.whatl2.XY)

            let _ = assertT <| Vec.DistanceMax(getVec(), getVec())
            let _ = assertT <| Vec.DistanceMax(getVec().XYZ, getVec().XYZ)
            let _ = assertT <| Vec.DistanceMax(getVec().XY, getVec().XY)

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader]
        [
            "getVec", 20
            "abs\(.*\)", 12
            "min\(.*\)", 15
            "max\(.*\)", 15
        ]

[<Test>]
let ``NormMin / -Max``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| v.what.NormMin
            let _ = assertT <| v.whatu.NormMin
            let _ = assertT <| v.whatl.NormMin
            let _ = assertT <| Vec.NormMin(v.what)
            let _ = assertT <| Vec.NormMin(v.whatu)
            let _ = assertT <| Vec.NormMin(v.whatl)
            let _ = assertT <| getVec().NormMin
            let _ = assertT <| Vec.NormMin(getVec())

            let _ = assertT <| v.what.NormMax
            let _ = assertT <| v.whatu.NormMax
            let _ = assertT <| v.whatl.NormMax
            let _ = assertT <| Vec.NormMax(v.what)
            let _ = assertT <| Vec.NormMax(v.whatu)
            let _ = assertT <| Vec.NormMax(v.whatl)
            let _ = assertT <| getVec().NormMax
            let _ = assertT <| Vec.NormMax(getVec())

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] [ "getVec", 5 ]

[<Test>]
let ``Reflect / refract``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.reflect v.c v.c
            let _ = assertT <| Vec.refract 0.5f v.c v.c
            let _ = assertT <| Vec.Reflect(v.c, v.c)
            let _ = assertT <| Vec.Refract(v.c, v.c, 0.5f)
            let _ = assertT <| Vec.reflect (V4d v.c) (V4d v.c)
            let _ = assertT <| Vec.refract 0.5 (V4d v.c) (V4d v.c)
            let _ = assertT <| Vec.Reflect(V4d v.c, V4d v.c)
            let _ = assertT <| Vec.Refract(V4d v.c, V4d v.c, 0.5)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["reflect"; "refract"]

[<Test>]
let ``Dot``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.dot v.c v.c
            let _ = assertT <| Vec.Dot(v.c, v.c)
            let _ = assertT <| Vec.Dot(v.c.XYZ, v.c.XYZ)
            let _ = assertT <| Vec.Dot(v.c.XY, v.c.XY)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["dot"]

[<Test>]
let ``Dot (int)``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.dot (getVeci()) v.what
            let _ = assertT <| Vec.Dot(getVeci(), v.what)
            let _ = assertT <| Vec.Dot(getVeci().XYZ, v.what.XYZ)
            let _ = assertT <| Vec.Dot(getVeci().XY, v.what.XY)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVeci", 5; "\*", 13]

[<Test>]
let ``Cross``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.cross v.c.XYZ v.c.XYZ
            let _ = assertT <| Vec.Cross(v.c.XYZ, v.c.XYZ)
            let _ = assertT <| Vec.cross v.what.XYZ v.what.XYZ
            let _ = assertT <| Vec.Cross(v.what.XYZ, v.what.XYZ)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["cross"]


[<Test>]
let ``Transpose``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.transpose (M33f(v.c.X))
            let _ = assertT <| Mat.Transposed(M44f(v.c.X))
            let _ = assertT <| Mat.Transposed(M33f(v.c.X))
            let _ = assertT <| Mat.Transposed(M22f(v.c.X))
            let _ = assertT <| Mat.transpose (M33d(float v.c.X))
            let _ = assertT <| Mat.Transposed(M44d(float v.c.X))
            let _ = assertT <| Mat.Transposed(M33d(float v.c.X))
            let _ = assertT <| Mat.Transposed(M22d(float v.c.X))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["transpose"]

[<Test>]
let ``Transform 2x2``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.transform (M22f(v.c.X)) V2f.Zero
            let _ = assertT <| Mat.Transform(M22f(v.c.X), V2f.Zero)
            let _ = assertT <| Mat.TransposedTransform(M22f(v.c.X), V2f.Zero)
            let _ = assertT <| Mat.transform (M22d(float v.c.X)) V2d.Zero
            let _ = assertT <| Mat.Transform(M22d(float v.c.X), V2d.Zero)
            let _ = assertT <| Mat.TransposedTransform(M22d(float v.c.X), V2d.Zero)

            return 0.0f
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Transform 2x3``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.transform (M23f(v.c.X)) V3f.Zero
            let _ = assertT <| Mat.Transform(M23f(v.c.X), V3f.Zero)
            let _ = assertT <| Mat.TransposedTransform(M23f(v.c.X), V2f.Zero)

            let _ = assertT <| Mat.transformDir (M23f(v.c.X)) v.c.XY
            let _ = assertT <| Mat.TransformDir(M23f(v.c.X), v.c.XY)

            let _ = assertT <| Mat.transformPos (M23f(v.c.X)) v.c.XY
            let _ = assertT <| Mat.TransformPos(M23f(v.c.X), v.c.XY)

            let _ = assertT <| Mat.transform (M23d(float v.c.X)) V3d.Zero
            let _ = assertT <| Mat.Transform(M23d(float v.c.X), V3d.Zero)
            let _ = assertT <| Mat.TransposedTransform(M23d(float v.c.X), V2d.Zero)

            let _ = assertT <| Mat.transformDir (M23d(float v.c.X)) (V2d v.c.XY)
            let _ = assertT <| Mat.TransformDir(M23d(float v.c.X), V2d v.c.XY)

            let _ = assertT <| Mat.transformPos (M23d(float v.c.X)) (V2d v.c.XY)
            let _ = assertT <| Mat.TransformPos(M23d(float v.c.X), V2d v.c.XY)

            return 0.0f
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Transform 3x3``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.transform (M33f(v.c.X)) V3f.Zero
            let _ = assertT <| Mat.Transform(M33f(v.c.X), V3f.Zero)
            let _ = assertT <| Mat.TransposedTransform(M33f(v.c.X), V3f.Zero)

            let _ = assertT <| Mat.transformDir (M33f(v.c.X)) v.c.XY
            let _ = assertT <| Mat.TransformDir(M33f(v.c.X), v.c.XY)
            let _ = assertT <| Mat.TransposedTransformDir(M33f(v.c.X), v.c.XY)

            let _ = assertT <| Mat.transformPos (M33f(v.c.X)) v.c.XY
            let _ = assertT <| Mat.TransformPos(M33f(v.c.X), v.c.XY)
            let _ = assertT <| Mat.TransposedTransformPos(M33f(v.c.X), v.c.XY)

            let _ = assertT <| Mat.TransformPosProj(M33f(v.c.X), v.c.XY)
            let _ = assertT <| Mat.TransformPosProjFull(M33f(v.c.X), v.c.XY)
            let _ = assertT <| Mat.TransposedTransformPosProj(M33f(v.c.X), v.c.XY)
            let _ = assertT <| Mat.TransposedTransformPosProjFull(M33f(v.c.X), v.c.XY)

            let _ = assertT <| Mat.transform (M33d(float v.c.X)) V3d.Zero
            let _ = assertT <| Mat.Transform(M33d(float v.c.X), V3d.Zero)
            let _ = assertT <| Mat.TransposedTransform(M33d(float v.c.X), V3d.Zero)

            let _ = assertT <| Mat.transformDir (M33d(float v.c.X)) V2d.Zero
            let _ = assertT <| Mat.TransformDir(M33d(float v.c.X), V2d.Zero)
            let _ = assertT <| Mat.TransposedTransformDir(M33d(float v.c.X), V2d.Zero)

            let _ = assertT <| Mat.transformPos (M33d(float v.c.X)) V2d.Zero
            let _ = assertT <| Mat.TransformPos(M33d(float v.c.X), V2d.Zero)
            let _ = assertT <| Mat.TransposedTransformPos(M33d(float v.c.X), V2d.Zero)

            let _ = assertT <| Mat.TransformPosProj(M33d(float v.c.X), V2d.Zero)
            let _ = assertT <| Mat.TransformPosProjFull(M33d(float v.c.X), V2d.Zero)
            let _ = assertT <| Mat.TransposedTransformPosProj(M33d(float v.c.X), V2d.Zero)
            let _ = assertT <| Mat.TransposedTransformPosProjFull(M33d(float v.c.X), V2d.Zero)

            return 0.0f
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Transform 3x4``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.transform (M34f(v.c.X)) V4f.Zero
            let _ = assertT <| Mat.Transform(M34f(v.c.X), V4f.Zero)
            let _ = assertT <| Mat.TransposedTransform(M34f(v.c.X), V3f.Zero)

            let _ = assertT <| Mat.transformDir (M34f(v.c.X)) v.c.XYZ
            let _ = assertT <| Mat.TransformDir(M34f(v.c.X), v.c.XYZ)

            let _ = assertT <| Mat.transformPos (M34f(v.c.X)) v.c.XYZ
            let _ = assertT <| Mat.TransformPos(M34f(v.c.X), v.c.XYZ)
            
            let _ = assertT <| Mat.transform (M34d(float v.c.X)) V4d.Zero
            let _ = assertT <| Mat.Transform(M34d(float v.c.X), V4d.Zero)
            let _ = assertT <| Mat.TransposedTransform(M34d(float v.c.X), V3d.Zero)

            let _ = assertT <| Mat.transformDir (M34d(float v.c.X)) V3d.Zero
            let _ = assertT <| Mat.TransformDir(M34d(float v.c.X), V3d.Zero)

            let _ = assertT <| Mat.transformPos (M34d(float v.c.X)) V3d.Zero
            let _ = assertT <| Mat.TransformPos(M34d(float v.c.X), V3d.Zero)

            return 0.0f
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Transform 4x4``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.transform (M44f(v.c.X)) V4f.Zero
            let _ = assertT <| Mat.Transform(M44f(v.c.X), V4f.Zero)
            let _ = assertT <| Mat.TransposedTransform(M44f(v.c.X), V4f.Zero)

            let _ = assertT <| Mat.transformDir (M44f(v.c.X)) v.c.XYZ
            let _ = assertT <| Mat.TransformDir(M44f(v.c.X), v.c.XYZ)
            let _ = assertT <| Mat.TransposedTransformDir(M44f(v.c.X), v.c.XYZ)

            let _ = assertT <| Mat.transformPos (M44f(v.c.X)) v.c.XYZ
            let _ = assertT <| Mat.TransformPos(M44f(v.c.X), v.c.XYZ)
            let _ = assertT <| Mat.TransposedTransformPos(M44f(v.c.X), v.c.XYZ)

            let _ = assertT <| Mat.TransformPosProj(M44f(v.c.X), v.c.XYZ)
            let _ = assertT <| Mat.TransformPosProjFull(M44f(v.c.X), v.c.XYZ)
            let _ = assertT <| Mat.TransposedTransformPosProj(M44f(v.c.X), v.c.XYZ)
            let _ = assertT <| Mat.TransposedTransformPosProjFull(M44f(v.c.X), v.c.XYZ)
            
            let _ = assertT <| Mat.transform (M44d(float v.c.X)) V4d.Zero
            let _ = assertT <| Mat.Transform(M44d(float v.c.X), V4d.Zero)
            let _ = assertT <| Mat.TransposedTransform(M44d(float v.c.X), V4d.Zero)

            let _ = assertT <| Mat.transformDir (M44d(float v.c.X)) V3d.Zero
            let _ = assertT <| Mat.TransformDir(M44d(float v.c.X), V3d.Zero)
            let _ = assertT <| Mat.TransposedTransformDir(M44d(float v.c.X), V3d.Zero)

            let _ = assertT <| Mat.transformPos (M44d(float v.c.X)) V3d.Zero
            let _ = assertT <| Mat.TransformPos(M44d(float v.c.X), V3d.Zero)
            let _ = assertT <| Mat.TransposedTransformPos(M44d(float v.c.X), V3d.Zero)

            let _ = assertT <| Mat.TransformPosProj(M44d(float v.c.X), V3d.Zero)
            let _ = assertT <| Mat.TransformPosProjFull(M44d(float v.c.X), V3d.Zero)
            let _ = assertT <| Mat.TransposedTransformPosProj(M44d(float v.c.X), V3d.Zero)
            let _ = assertT <| Mat.TransposedTransformPosProjFull(M44d(float v.c.X), V3d.Zero)

            return 0.0f
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Determinant``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.det (M44f(v.c.X))
            let _ = assertT <| M44f(v.c.X).Determinant
            let _ = assertT <| Mat.Determinant(M44f(v.c.X))
            let _ = assertT <| Mat.Determinant(M33f(v.c.X))
            let _ = assertT <| Mat.Determinant(M22f(v.c.X))
            let _ = assertT <| Mat.det (M44d(float v.c.X))
            let _ = assertT <| M44d(float v.c.X).Determinant
            let _ = assertT <| Mat.Determinant(M44d(float v.c.X))
            let _ = assertT <| Mat.Determinant(M33d(float v.c.X))
            let _ = assertT <| Mat.Determinant(M22d(float v.c.X))
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["determinant"]


[<Test>]
let ``Min- / MaxElement``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.MinElement(v.c.XY)
            let _ = assertT <| Vec.MinElement(v.c.XYZ)
            let _ = assertT <| Vec.MinElement(v.c)
            let _ = assertT <| Vec.MinElement(v.what.XY)
            let _ = assertT <| Vec.MinElement(v.what.XYZ)
            let _ = assertT <| Vec.MinElement(v.what)
            let _ = assertT <| v.c.XY.MinElement
            let _ = assertT <| v.c.XYZ.MinElement
            let _ = assertT <| v.c.MinElement
            let _ = assertT <| v.what.XY.MinElement
            let _ = assertT <| v.what.XYZ.MinElement
            let _ = assertT <| v.what.MinElement
            let _ = assertT <| Vec.MaxElement(v.c.XY)
            let _ = assertT <| Vec.MaxElement(v.c.XYZ)
            let _ = assertT <| Vec.MaxElement(v.c)
            let _ = assertT <| Vec.MaxElement(v.what.XY)
            let _ = assertT <| Vec.MaxElement(v.what.XYZ)
            let _ = assertT <| Vec.MaxElement(v.what)
            let _ = assertT <| v.c.XY.MaxElement
            let _ = assertT <| v.c.XYZ.MaxElement
            let _ = assertT <| v.c.MaxElement
            let _ = assertT <| v.what.XY.MaxElement
            let _ = assertT <| v.what.XYZ.MaxElement
            let _ = assertT <| v.what.MaxElement

            let _ = assertT <| Vec.MinElement(getVec().XY)
            let _ = assertT <| Vec.MinElement(getVec().XYZ)
            let _ = assertT <| Vec.MinElement(getVec())
            let _ = assertT <| getVec().XY.MinElement
            let _ = assertT <| getVec().XYZ.MinElement
            let _ = assertT <| getVec().MinElement

            let _ = assertT <| Vec.MaxElement(getVec().XY)
            let _ = assertT <| Vec.MaxElement(getVec().XYZ)
            let _ = assertT <| Vec.MaxElement(getVec())
            let _ = assertT <| getVec().XY.MaxElement
            let _ = assertT <| getVec().XYZ.MaxElement
            let _ = assertT <| getVec().MaxElement

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 13]

[<Test>]
let ``Normalize``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.Normalized (V4f v.c)
            let _ = assertT <| Vec.normalize  (V4f v.c)
            let _ = assertT <| (V4f v.c).Normalized
            let _ = assertT <| Vec.Normalized (V3f v.c)
            let _ = assertT <| Vec.normalize  (V3f v.c)
            let _ = assertT <| (V3f v.c).Normalized
            let _ = assertT <| Vec.Normalized (V2f v.c)
            let _ = assertT <| Vec.normalize  (V2f v.c)
            let _ = assertT <| (V2f v.c).Normalized

            let _ = assertT <| Vec.Normalized (V4d v.c)
            let _ = assertT <| Vec.normalize  (V4d v.c)
            let _ = assertT <| (V4d v.c).Normalized
            let _ = assertT <| Vec.Normalized (V3d v.c)
            let _ = assertT <| Vec.normalize  (V3d v.c)
            let _ = assertT <| (V3d v.c).Normalized
            let _ = assertT <| Vec.Normalized (V2d v.c)
            let _ = assertT <| Vec.normalize  (V2d v.c)
            let _ = assertT <| (V2d v.c).Normalized

            let _ = assertT <| Vec.Normalized (V4i v.c)
            let _ = assertT <| Vec.normalize  (V4i v.c)
            let _ = assertT <| (V4i v.c).Normalized
            let _ = assertT <| Vec.Normalized (V3i v.c)
            let _ = assertT <| Vec.normalize  (V3i v.c)
            let _ = assertT <| (V3i v.c).Normalized
            let _ = assertT <| Vec.Normalized (V2i v.c)
            let _ = assertT <| Vec.normalize  (V2i v.c)
            let _ = assertT <| (V2i v.c).Normalized

            let _ = assertT <| Vec.Normalized (V4ui v.c)
            let _ = assertT <| Vec.normalize  (V4ui v.c)
            let _ = assertT <| (V4ui v.c).Normalized
            let _ = assertT <| Vec.Normalized (V3ui v.c)
            let _ = assertT <| Vec.normalize  (V3ui v.c)
            let _ = assertT <| (V3ui v.c).Normalized
            let _ = assertT <| Vec.Normalized (V2ui v.c)
            let _ = assertT <| Vec.normalize  (V2ui v.c)
            let _ = assertT <| (V2ui v.c).Normalized

            let _ = assertT <| Vec.Normalized (V4l v.c)
            let _ = assertT <| Vec.normalize  (V4l v.c)
            let _ = assertT <| (V4l v.c).Normalized
            let _ = assertT <| Vec.Normalized (V3l v.c)
            let _ = assertT <| Vec.normalize  (V3l v.c)
            let _ = assertT <| (V3l v.c).Normalized
            let _ = assertT <| Vec.Normalized (V2l v.c)
            let _ = assertT <| Vec.normalize  (V2l v.c)
            let _ = assertT <| (V2l v.c).Normalized

            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["normalize"]

[<Test>]
let ``Constant swizzles``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| getVec().XYOI
            let _ = assertT <| getVec().OXXX
            let _ = assertT <| getVec().ZIXO
            let _ = assertT <| getVec().ZYXN
            let _ = assertT <| getVec().OXOX
            let _ = assertT <| getVec().XOOO

            let _ = assertT <| V3ui(v.whatu.X, 2u, 3u).XYOI
            let _ = assertT <| V3ui(v.whatu.X, 2u, 3u).OXXX
            let _ = assertT <| V3ui(v.whatu.X, 2u, 3u).ZIXO
            let _ = assertT <| V3ui(v.whatu.X, 2u, 3u).OXOX
            let _ = assertT <| V3ui(v.whatu.X, 2u, 3u).XOOO

            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 7]

[<Test>]
let ``Vector swizzles``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.x v.c
            let _ = assertT <| Vec.y v.c.XYZ
            let _ = assertT <| Vec.z v.c
            let _ = assertT <| Vec.w v.c
            let _ = assertT <| Vec.xy v.c
            let _ = assertT <| Vec.yz v.c.XYZ
            let _ = assertT <| Vec.zw v.c
            let _ = assertT <| Vec.xyz v.c
            let _ = assertT <| Vec.yzw v.c
            return v
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Vector AnyEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AnyEqual(v.c, v.c)
            let _ = assertT <| v.c.AnyEqual(v.c)
            let _ = assertT <| Vec.AnyEqual(v.c, 0.0f)
            let _ = assertT <| Vec.AnyEqual(0.0f, v.c)
            let _ = assertT <| Vec.anyEqual v.c v.c
            let _ = assertT <| Vec.anyEqual v.c 0.0f
            let _ = assertT <| Vec.anyEqual 0.0f v.c

            let _ = assertT <| Vec.AnyEqual(v.what, v.what)
            let _ = assertT <| v.what.AnyEqual(v.what)
            let _ = assertT <| Vec.AnyEqual(v.what, 0)
            let _ = assertT <| Vec.AnyEqual(0, v.what)
            let _ = assertT <| Vec.anyEqual v.what v.what
            let _ = assertT <| Vec.anyEqual v.what 0
            let _ = assertT <| Vec.anyEqual 0 v.what

            let _ = assertT <| Vec.AnyEqual(v.whatu, v.whatu)
            let _ = assertT <| v.whatu.AnyEqual(v.whatu)
            let _ = assertT <| Vec.AnyEqual(v.whatu, 0u)
            let _ = assertT <| Vec.AnyEqual(0u, v.whatu)
            let _ = assertT <| Vec.anyEqual v.whatu v.whatu
            let _ = assertT <| Vec.anyEqual v.whatu 0u
            let _ = assertT <| Vec.anyEqual 0u v.whatu

            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["equal"; "any"]

[<Test>]
let ``Vector AllEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AllEqual(v.c, v.c)
            let _ = assertT <| v.c.AllEqual(v.c)
            let _ = assertT <| Vec.AllEqual(v.c, 0.0f)
            let _ = assertT <| Vec.AllEqual(0.0f, v.c)
            let _ = assertT <| Vec.allEqual v.c v.c
            let _ = assertT <| Vec.allEqual v.c 0.0f
            let _ = assertT <| Vec.allEqual 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["=="]

[<Test>]
let ``Vector AnyDifferent``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AnyDifferent(v.c, v.c)
            let _ = assertT <| v.c.AnyDifferent(v.c)
            let _ = assertT <| Vec.AnyDifferent(v.c, 0.0f)
            let _ = assertT <| Vec.AnyDifferent(0.0f, v.c)
            let _ = assertT <| Vec.anyDifferent v.c v.c
            let _ = assertT <| Vec.anyDifferent v.c 0.0f
            let _ = assertT <| Vec.anyDifferent 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["!="]

[<Test>]
let ``Vector AllDifferent``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AllDifferent(v.c, v.c)
            let _ = assertT <| v.c.AllDifferent(v.c)
            let _ = assertT <| Vec.AllDifferent(v.c, 0.0f)
            let _ = assertT <| Vec.AllDifferent(0.0f, v.c)
            let _ = assertT <| Vec.allDifferent v.c v.c
            let _ = assertT <| Vec.allDifferent v.c 0.0f
            let _ = assertT <| Vec.allDifferent 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "notEqual"]

[<Test>]
let ``Vector AnySmaller``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AnySmaller(v.c, v.c)
            let _ = assertT <| v.c.AnySmaller(v.c)
            let _ = assertT <| Vec.AnySmaller(v.c, 0.0f)
            let _ = assertT <| Vec.AnySmaller(0.0f, v.c)
            let _ = assertT <| Vec.anySmaller v.c v.c
            let _ = assertT <| Vec.anySmaller v.c 0.0f
            let _ = assertT <| Vec.anySmaller 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "lessThan"]

[<Test>]
let ``Vector AllSmaller``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AllSmaller(v.c, v.c)
            let _ = assertT <| v.c.AllSmaller(v.c)
            let _ = assertT <| Vec.AllSmaller(v.c, 0.0f)
            let _ = assertT <| Vec.AllSmaller(0.0f, v.c)
            let _ = assertT <| Vec.allSmaller v.c v.c
            let _ = assertT <| Vec.allSmaller v.c 0.0f
            let _ = assertT <| Vec.allSmaller 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "lessThan"]

[<Test>]
let ``Vector AnySmallerOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AnySmallerOrEqual(v.c, v.c)
            let _ = assertT <| v.c.AnySmallerOrEqual(v.c)
            let _ = assertT <| Vec.AnySmallerOrEqual(v.c, 0.0f)
            let _ = assertT <| Vec.AnySmallerOrEqual(0.0f, v.c)
            let _ = assertT <| Vec.anySmallerOrEqual v.c v.c
            let _ = assertT <| Vec.anySmallerOrEqual v.c 0.0f
            let _ = assertT <| Vec.anySmallerOrEqual 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "lessThanEqual"]

[<Test>]
let ``Vector AllSmallerOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AllSmallerOrEqual(v.c, v.c)
            let _ = assertT <| v.c.AllSmallerOrEqual(v.c)
            let _ = assertT <| Vec.AllSmallerOrEqual(v.c, 0.0f)
            let _ = assertT <| Vec.AllSmallerOrEqual(0.0f, v.c)
            let _ = assertT <| Vec.allSmallerOrEqual v.c v.c
            let _ = assertT <| Vec.allSmallerOrEqual v.c 0.0f
            let _ = assertT <| Vec.allSmallerOrEqual 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "lessThanEqual"]

[<Test>]
let ``Vector AnyGreater``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AnyGreater(v.c, v.c)
            let _ = assertT <| v.c.AnyGreater(v.c)
            let _ = assertT <| Vec.AnyGreater(v.c, 0.0f)
            let _ = assertT <| Vec.AnyGreater(0.0f, v.c)
            let _ = assertT <| Vec.anyGreater v.c v.c
            let _ = assertT <| Vec.anyGreater v.c 0.0f
            let _ = assertT <| Vec.anyGreater 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "greaterThan"]

[<Test>]
let ``Vector AllGreater``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AllGreater(v.c, v.c)
            let _ = assertT <| v.c.AllGreater(v.c)
            let _ = assertT <| Vec.AllGreater(v.c, 0.0f)
            let _ = assertT <| Vec.AllGreater(0.0f, v.c)
            let _ = assertT <| Vec.allGreater v.c v.c
            let _ = assertT <| Vec.allGreater v.c 0.0f
            let _ = assertT <| Vec.allGreater 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "greaterThan"]

[<Test>]
let ``Vector AnyGreaterOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AnyGreaterOrEqual(v.c, v.c)
            let _ = assertT <| v.c.AnyGreaterOrEqual(v.c)
            let _ = assertT <| Vec.AnyGreaterOrEqual(v.c, 0.0f)
            let _ = assertT <| Vec.AnyGreaterOrEqual(0.0f, v.c)
            let _ = assertT <| Vec.anyGreaterOrEqual v.c v.c
            let _ = assertT <| Vec.anyGreaterOrEqual v.c 0.0f
            let _ = assertT <| Vec.anyGreaterOrEqual 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "greaterThanEqual"]

[<Test>]
let ``Vector AllGreaterOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Vec.AllGreaterOrEqual(v.c, v.c)
            let _ = assertT <| v.c.AllGreaterOrEqual(v.c)
            let _ = assertT <| Vec.AllGreaterOrEqual(v.c, 0.0f)
            let _ = assertT <| Vec.AllGreaterOrEqual(0.0f, v.c.XY)
            let _ = assertT <| Vec.allGreaterOrEqual v.c v.c
            let _ = assertT <| Vec.allGreaterOrEqual v.c 0.0f
            let _ = assertT <| Vec.allGreaterOrEqual 0.0f v.c
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "greaterThanEqual"]

[<Test>]
let ``Vector AnyInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = assertT <| v.IsInfinity
            let _ = assertT <| v.AnyInfinity
            let _ = assertT <| isInfinity v
            let _ = assertT <| Fun.IsInfinity v
            let _ = assertT <| Vec.AnyInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "any"]

[<Test>]
let ``Vector AllInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = assertT <| v.AllInfinity
            let _ = assertT <| Vec.AllInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "all"]

[<Test>]
let ``Vector AnyPositiveInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = assertT <| v.AnyPositiveInfinity
            let _ = assertT <| Vec.AnyPositiveInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Vector AllPositiveInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = assertT <| v.AllPositiveInfinity
            let _ = assertT <| Vec.AllPositiveInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "all"]

[<Test>]
let ``Vector AnyNegativeInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = assertT <| v.AnyNegativeInfinity
            let _ = assertT <| Vec.AnyNegativeInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Vector AllNegativeInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = assertT <| v.AllNegativeInfinity
            let _ = assertT <| Vec.AllNegativeInfinity v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "all"]

[<Test>]
let ``Vector AnyFinite``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = assertT <| v.IsFinite
            let _ = assertT <| v.AnyFinite
            let _ = assertT <| isFinite v
            let _ = assertT <| Fun.IsFinite v
            let _ = assertT <| Vec.AnyFinite v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Vector AllFinite``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = assertT <| v.AllFinite
            let _ = assertT <| Vec.AllFinite v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "any"]

[<Test>]
let ``Vector AnyNaN``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = assertT <| v.IsNaN
            let _ = assertT <| v.AnyNaN
            let _ = assertT <| isNaN v
            let _ = assertT <| Fun.IsNaN v
            let _ = assertT <| Vec.AnyNaN v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isnan"; "any"]

[<Test>]
let ``Vector AllNaN``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let v = v.c
            let _ = assertT <| v.AllNaN
            let _ = assertT <| Vec.AllNaN v
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isnan"; "all"]

[<Test>]
let ``Matrix AnyEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AnyEqual((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AnyEqual((M34f(v.c.X)))
            let _ = assertT <| Mat.AnyEqual((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AnyEqual(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.anyEqual (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.anyEqual (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.anyEqual 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["equal"; "any"]

[<Test>]
let ``Matrix AllEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AllEqual((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AllEqual((M34f(v.c.X)))
            let _ = assertT <| Mat.AllEqual((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AllEqual(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.allEqual (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.allEqual (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.allEqual 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["=="]

[<Test>]
let ``Matrix AnyDifferent``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AnyDifferent((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AnyDifferent((M34f(v.c.X)))
            let _ = assertT <| Mat.AnyDifferent((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AnyDifferent(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.anyDifferent (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.anyDifferent (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.anyDifferent 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["!="]

[<Test>]
let ``Matrix AllDifferent``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AllDifferent((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AllDifferent((M34f(v.c.X)))
            let _ = assertT <| Mat.AllDifferent((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AllDifferent(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.allDifferent (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.allDifferent (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.allDifferent 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "notEqual"]

[<Test>]
let ``Matrix AnySmaller``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AnySmaller((M34f(v.c.X)), (M34f(v.c.Y)))
            let _ = assertT <| (M34f(v.c.X)).AnySmaller((M34f(v.c.Y)))
            let _ = assertT <| Mat.AnySmaller((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AnySmaller(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.anySmaller (M34f(v.c.X)) (M34f(v.c.Y))
            let _ = assertT <| Mat.anySmaller (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.anySmaller 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "lessThan"]

[<Test>]
let ``Matrix AllSmaller``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AllSmaller((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AllSmaller((M34f(v.c.X)))
            let _ = assertT <| Mat.AllSmaller((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AllSmaller(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.allSmaller (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.allSmaller (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.allSmaller 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "lessThan"]

[<Test>]
let ``Matrix AnySmallerOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AnySmallerOrEqual((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AnySmallerOrEqual((M34f(v.c.X)))
            let _ = assertT <| Mat.AnySmallerOrEqual((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AnySmallerOrEqual(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.anySmallerOrEqual (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.anySmallerOrEqual (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.anySmallerOrEqual 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "lessThanEqual"]

[<Test>]
let ``Matrix AllSmallerOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AllSmallerOrEqual((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AllSmallerOrEqual((M34f(v.c.X)))
            let _ = assertT <| Mat.AllSmallerOrEqual((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AllSmallerOrEqual(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.allSmallerOrEqual (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.allSmallerOrEqual (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.allSmallerOrEqual 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "lessThanEqual"]

[<Test>]
let ``Matrix AnyGreater``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AnyGreater((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AnyGreater((M34f(v.c.X)))
            let _ = assertT <| Mat.AnyGreater((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AnyGreater(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.anyGreater (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.anyGreater (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.anyGreater 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "greaterThan"]

[<Test>]
let ``Matrix AllGreater``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AllGreater((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AllGreater((M34f(v.c.X)))
            let _ = assertT <| Mat.AllGreater((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AllGreater(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.allGreater (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.allGreater (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.allGreater 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "greaterThan"]

[<Test>]
let ``Matrix AnyGreaterOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AnyGreaterOrEqual((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AnyGreaterOrEqual((M34f(v.c.X)))
            let _ = assertT <| Mat.AnyGreaterOrEqual((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AnyGreaterOrEqual(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.anyGreaterOrEqual (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.anyGreaterOrEqual (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.anyGreaterOrEqual 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["any"; "greaterThanEqual"]

[<Test>]
let ``Matrix AllGreaterOrEqual``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| Mat.AllGreaterOrEqual((M34f(v.c.X)), (M34f(v.c.X)))
            let _ = assertT <| (M34f(v.c.X)).AllGreaterOrEqual((M34f(v.c.X)))
            let _ = assertT <| Mat.AllGreaterOrEqual((M34f(v.c.X)), 0.0f)
            let _ = assertT <| Mat.AllGreaterOrEqual(0.0f, (M34f(v.c.X)))
            let _ = assertT <| Mat.allGreaterOrEqual (M34f(v.c.X)) (M34f(v.c.X))
            let _ = assertT <| Mat.allGreaterOrEqual (M34f(v.c.X)) 0.0f
            let _ = assertT <| Mat.allGreaterOrEqual 0.0f (M34f(v.c.X))
            return v
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["all"; "greaterThanEqual"]

[<Test>]
let ``Matrix AnyInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = assertT <| M34f(v.c.X)
            let _ = assertT <| m.IsInfinity
            let _ = assertT <| m.AnyInfinity
            let _ = assertT <| isInfinity m
            let _ = assertT <| Fun.IsInfinity m
            let _ = assertT <| Mat.AnyInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"; "any"]

[<Test>]
let ``Matrix AllInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34f(v.c.X)
            let _ = assertT <| m.AllInfinity
            let _ = assertT <| Mat.AllInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "&&"; "all"]

[<Test>]
let ``Matrix AnyPositiveInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34f(v.c.X)
            let _ = assertT <| m.AnyPositiveInfinity
            let _ = assertT <| Mat.AnyPositiveInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Matrix AllPositiveInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34f(v.c.X)
            let _ = assertT <| m.AllPositiveInfinity
            let _ = assertT <| Mat.AllPositiveInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "&&"; "all"]

[<Test>]
let ``Matrix AnyNegativeInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34f(v.c.X)
            let _ = assertT <| m.AnyNegativeInfinity
            let _ = assertT <| Mat.AnyNegativeInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Matrix AllNegativeInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34f(v.c.X)
            let _ = assertT <| m.AllNegativeInfinity
            let _ = assertT <| Mat.AllNegativeInfinity m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "all"]

[<Test>]
let ``Matrix AnyFinite``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34f(v.c.X)
            let _ = assertT <| m.IsFinite
            let _ = assertT <| m.AnyFinite
            let _ = assertT <| isFinite m
            let _ = assertT <| Fun.IsFinite m
            let _ = assertT <| Mat.AnyFinite m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "\|\|"]

[<Test>]
let ``Matrix AllFinite``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34f(v.c.X)
            let _ = assertT <| m.AllFinite
            let _ = assertT <| Mat.AllFinite m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"; "&&"; "\|\|"; "any"]

[<Test>]
let ``Matrix AnyNaN``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34f(v.c.X)
            let _ = assertT <| m.IsNaN
            let _ = assertT <| m.AnyNaN
            let _ = assertT <| isNaN m
            let _ = assertT <| Fun.IsNaN m
            let _ = assertT <| Mat.AnyNaN m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isnan"; "\|\|"; "any"]

[<Test>]
let ``Matrix AllNaN``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let m = M34f(v.c.X)
            let _ = assertT <| m.AllNaN
            let _ = assertT <| Mat.AllNaN m
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isnan"; "&&"; "all"]

[<Test>]
let ``IsInfinity``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| isInfinity v.c
            let _ = assertT <| isInfinity v.c.X
            let _ = assertT <| Fun.IsInfinity v.c
            let _ = assertT <| Fun.IsInfinity v.c.X
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isinf"]

[<Test>]
let ``IsInfinity (signed)``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| isPositiveInfinity (getVec().X)
            let _ = assertT <| isNegativeInfinity (getVec().X)
            let _ = assertT <| Fun.IsPositiveInfinity (getVec().X)
            let _ = assertT <| Fun.IsNegativeInfinity (getVec().X)
            return v.pos
        }

    GLSL.shouldCompileAndContainRegexWithCount [Effect.ofFunction shader] ["getVec", 5]

[<Test>]
let ``IsNaN``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let _ = assertT <| isNaN v.c
            let _ = assertT <| isNaN v.c.X
            let _ = assertT <| Fun.IsNaN v.c
            let _ = assertT <| Fun.IsNaN v.c.X
            return v.pos
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["isnan"]