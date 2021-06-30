module SingleEffects

open System
open Aardvark.Base
open FShade
open NUnit.Framework
open FsUnit
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
        foo : V4d
    }


[<Test>]
let ``Reserved Names``() =
    Setup.Run()

    let shader (v : Vertex) =
        vertex {
            let mutable input = v.pos
            input <- V4d.Zero

            let mutable output = input
            output <- V4d.Zero

            let mutable this = output
            this <- V4d.Zero

            let mutable union = this
            union <- V4d.Zero

            return union
        }

    GLSL.shouldCompile [Effect.ofFunction shader]

[<Test>]
let ``Constructors Matrix``() =
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
let ``Constructors Vector``() =
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

            return v.pos
        }

    GLSL.shouldCompile [Effect.ofFunction shader]


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
let ``New Intrinsics``() =
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
            let _ = Fun.Exp(v.c.XY)
            let _ = Fun.Exp(V2f(v.c.XY))
            let _ = Fun.Exp(v.c.XYZ)
            let _ = Fun.Exp(V3f(v.c.XYZ))
            let _ = Fun.Exp(v.c)
            let _ = Fun.Exp(V4f(v.c))
            let _ = exp v.c
            let _ = Fun.Log(v.c.XY)
            let _ = Fun.Log(V2f(v.c.XY))
            let _ = Fun.Log(v.c.XYZ)
            let _ = Fun.Log(V3f(v.c.XYZ))
            let _ = Fun.Log(v.c)
            let _ = Fun.Log(V4f(v.c))
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
            let _ = sqrt v.c
            let _ = Fun.Sqrt (V2i(v.c.XY))
            let _ = signum v.c
            let _ = signumi v.c
            let _ = v.c |> floor |> truncate |> ceil |> round
            let _ = sqr v.c
            let _ = sqr (V4i v.c)
            let _ = cbrt v.c
            let _ = v.c.Abs()
            let _ = Fun.Min(v.c, 1.0)
            let _ = Fun.Min(1.0, v.c)
            let _ = v.c |> min 1.0
            let _ = v.c |> min v.c
            let _ = v.c |> clamp 1.0 v.c
            let _ = v.c.XYZ |> clamp v.c.XYZ 1.0
            let _ = Fun.Clamp(v.c.XY, 0.0, 1.0)
            let _ = v.c |> smoothstep v.c v.c
            let _ = v.c |> smoothstep 0.0 1.0
            let _ = Fun.Smoothstep(v.c, 0.0, 1.0)
            let _ = Fun.Smoothstep(v.c, v.c, v.c)
            let _ = lerp V2i.Zero V2i.One v.c.XY
            let _ = Fun.Lerp(v.c.X, 0, 1)
            let _ = Fun.Lerp(v.c, V4i(v.c), V4i(v.c))
            let _ = asinh (V4f(v.c))
            let _ = madd v.c v.c v.c
            let _ = madd v.c v.c.X v.c
            let _ = madd v.c.X v.c.X v.c.X
            let _ = madd (V4i(v.c)) 2 (V4i(v.c))
            let _ = Fun.MultiplyAdd(v.c.X, v.c, v.c)
            let _ = Fun.MultiplyAdd(v.c, v.c.X, v.c)
            let _ = Fun.MultiplyAdd(v.c, v.c, v.c)
            let _ = Fun.MultiplyAdd(v.c.X, v.c.Y, v.c.Z)
            let _ = degrees v.c
            let _ = degrees v.c.X
            let _ = v.c.DegreesFromRadians()
            let _ = isInfinity v.c
            let _ = isInfinity v.c.X
            let _ = Vec.reflect v.c v.c
            let _ = Vec.refract 0.5 v.c v.c
            let _ = v.c.X.DegreesFromRadians()
            let _ = Vec.length v.c
            let _ = Vec.Length v.c
            let _ = Vec.lengthSquared v.c
            let _ = Vec.LengthSquared v.c
            let _ = Vec.dot v.c v.c
            let _ = Vec.Dot(v.c, v.c)
            let _ = Vec.cross v.c.XYZ v.c.XYZ
            let _ = Vec.Cross(v.c.XYZ, v.c.XYZ)
            let _ = Mat.transpose <| M33d(v.c.X)
            let _ = Mat.Transposed(M33d(v.c.X))
            let _ = Mat.transformDir (M44d(v.c.X)) v.c.XYZ
            let _ = Mat.TransformDir(M44d(v.c.X), v.c.XYZ)
            let _ = Mat.transformPos (M44d(v.c.X)) v.c.XYZ
            let _ = Mat.TransformPos(M44d(v.c.X), v.c.XYZ)
            let _ = Mat.TransposedTransformDir(M44d(v.c.X), v.c.XYZ)
            let _ = Mat.TransposedTransformPos(M44d(v.c.X), v.c.XYZ)
            let _ = Mat.det <| M44d(v.c.X)
            let _ = Mat.Determinant(M44d(v.c.X))
            let _ = Vec.MinElement(v.c.XY)
            let _ = Vec.MinElement(v.c.XYZ)
            let _ = Vec.MinElement(v.c)
            let _ = v.c.XY.MinElement
            let _ = v.c.XYZ.MinElement
            let _ = v.c.MinElement
            let _ = Vec.MaxElement(v.c.XY)
            let _ = Vec.MaxElement(v.c.XYZ)
            let _ = Vec.MaxElement(v.c)
            let _ = v.c.XY.MaxElement
            let _ = v.c.XYZ.MaxElement
            let _ = v.c.MaxElement
            let normalized = Vec.Normalized (V4i(v.c))
            let added = normalized + (Vec.normalize V4d.Half)

            return added
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction shader] ["mix"; "exp"; "log"; "pow"; "sign"; "sqrt"; "length"]

[<Test>]
let ``Broken GLSL Shader``() =
    Setup.Run()

    let code =
        String.concat "\r\n" [
            "#version 430"
            "layout(location = 0) in vec4 Position;"
            "void main() {"
            "    gl_Position = Postion;"
            "}"
        ]

    let res = GLSL.glslang ShaderStage.Vertex code
    Console.WriteLine("{0}: {1}", ShaderStage.Vertex, sprintf "%A" res)
    match res with
        | Success | Warning _ -> failwith "compiler should not succeed"
        | Error e -> ()



[<Test>]
let ``Simple fragment shader``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c
        }

    GLSL.shouldCompile [ Effect.ofFunction frag ]


[<Test>]
let ``Helper with duplicate names``() =
    Setup.Run()

    let vert (v : Vertex) =
        vertex {
            if v.pos.X > 0.0 then
                let Positions = v.pos.W + v.pos.X
                let (a,b) =
                    let v = 2.0 * v.pos
                    (v.X, v.Y + 1.0 + Positions)
                return { v with pos = V4d(a,b,b,a) }
            else
                return { v with pos = V4d.Zero }
        }

    GLSL.shouldCompile [ Effect.ofFunction vert ]


type PointSizeVertex =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
        [<PointSize>] s : float
    }


[<Test>]
let ``PointSize shader``() =
    Setup.Run()

    let vert (v : PointSizeVertex) =
        vertex {
            if v.pos.X > 0.0 then
                let Positions = v.pos.W + v.pos.X
                let (a,b) =
                    let v = 2.0 * v.pos
                    (v.X, v.Y + 1.0 + Positions)
                return { v with pos = V4d(a,b,b,a); s = 10.0 }
            else
                return { v with pos = V4d.Zero; s = 5.0 }
        }

    GLSL.shouldCompile [ Effect.ofFunction vert ]




[<ReflectedDefinition>]
let fillArray (array : Arr<N<10>, V4d>) denom =
    for i in 0 .. 9 do
        array.[i] <- V4d(float i / denom)


[<Test>]
let ``Fill Array with Function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let array = Arr<N<10>, V4d>()

            10.0 |> fillArray array

            return array.[uniform?color]
        }

    let expectedFillArrayGLSL =
        sprintf "
void .*_fillArray_.*\(vec4 array\[10], float denom\)
{
    for\(int i = 0; \(i < 10\); i\+\+\)
    {
        array\[i] = vec4\(\(float\(i\) \/ denom\)\);
    }
}"

    let expectedMainGLSL =
        sprintf "
    vec4 array\[10\];
    .*_fillArray_.*\(array, 10\.0\);
    ColorsOut = array\[color\]"


    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [ expectedFillArrayGLSL; expectedMainGLSL ]


[<ReflectedDefinition>]
let fillArrayReturn (array : Arr<N<10>, V4d>) denom =
    for i in 0 .. 9 do
        array.[i] <- V4d(float i / denom)

    array


[<Test>]
let ``Fill Array with Return Function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let array = Arr<N<10>, V4d>()


            let array1 = fillArrayReturn array 10.0

            return array1.[uniform?color]
        }

    let expectedFillArrayGLSL =
        sprintf "
vec4\[10\] .*_fillArrayReturn_.*\(vec4 array\[10], float denom\)
{
    for\(int i = 0; \(i < 10\); i\+\+\)
    {
        array\[i] = vec4\(\(float\(i\) \/ denom\)\);
    }
    return array;
}"

    let expectedMainGLSL =
        sprintf "
    vec4 array\[10\];
    vec4 array1\[10\] = .*_fillArrayReturn_.*\(array, 10\.0\);
    ColorsOut = array1\[color\]"


    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [ expectedFillArrayGLSL; expectedMainGLSL ]

[<ReflectedDefinition>][<Inline>]
let fillArrayInline (array : Arr<N<10>, V4d>) denom =
    for i in 0 .. 9 do
        array.[i] <- V4d(float i / denom)


[<Test>]
let ``Fill Array with Inline Function``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let array = Arr<N<10>, V4d>()

            10.0 |> fillArrayInline array

            // using this instead works
            // fillArrayInline array 10.0

            return array.[uniform?color]
        }

    let expectedGLSL =
        sprintf "
    vec4 array\[10];
    for\(int i = 0; \(i < 10\); i\+\+\)
    {
        array\[i\] = vec4\(\(float\(i\) \/ 10\.0\)\);
    }
    ColorsOut = array\[color\];"

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [ expectedGLSL ]



[<ReflectedDefinition>][<Inline>]
let valueIdentity v = v

[<ReflectedDefinition>][<Inline>]
let condTimesTwo (v : float) =
    if v > 0.5 then
        valueIdentity (v * 2.0)
    else
        valueIdentity v

[<Test>]
let ``Nested Double Inline``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {

            let v = condTimesTwo (uniform?value)

            return V4d(v)
        }

    GLSL.shouldCompile [ Effect.ofFunction frag ]

[<ReflectedDefinition;AutoOpen>]
module Helper =
    let b (h : float) (s : float) (v : float) =
        let s = clamp 0.0 1.0 s
        let v = clamp 0.0 uniform?Blubb?MaxValue v

        let h = h % 1.0
        let h = if h < 0.0 then h + 1.0 else h
        let hi = floor ( h * 6.0 ) |> int
        let f = h * 6.0 - float hi
        let p = v * (1.0 - s)
        let q = v * (1.0 - s * f)
        let t = v * (1.0 - s * ( 1.0 - f ))
        match hi with
            | 1 -> V3d(q,v,p)
            | 2 -> V3d(p,v,t)
            | 3 -> V3d(p,q,v)
            | 4 -> V3d(t,p,v)
            | 5 -> V3d(v,p,q)
            | _ -> V3d(v,t,p)

    let a (h : float) (s : float) (v : float) =
        if h < 0.0 then discard()
        b h s v

    let hsv2rgb (h : float) (s : float) (v : float)  =
        a h s v

    type UniformScope with
        member x.RegionBuffer : array<int> = uniform?StorageBuffer?RegionBuffer

    let sampleOffsets16 =
        [|
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
            V3d(0.0,0.0,0.0)
        |]

[<Test>]
let ``Bad Helpers``() =
    Setup.Run()

    let c () =
        fragment {
            let id : V3i = uniform?Bla
            let urdar =
                match id.X with
                    | 0 ->
                        let a = uniform.RegionBuffer.[id.X]
                        hsv2rgb (float a) 1.0 1.0
                    | _ ->
                        let b = uniform.RegionBuffer.[0]
                        V3d.III
            uniform.RegionBuffer.[id.X] <- int sampleOffsets16.[int urdar.X].X
            return V4d(sampleOffsets16.[int urdar.X], 1.0)
        }

    //let a = ComputeShader.ofFunction V3i.III c |> ComputeShader.toModule
    //let s = ModuleCompiler.compileGLSLVulkan a
    //printfn "%A" s.code
    GLSL.shouldCompile [ Effect.ofFunction c ]

[<ReflectedDefinition>]
let util (a : ref<float>) =
    a := !a + 1.0

[<Test>]
let ``Ref translated to inout``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let a = ref v.pos.X
            util a
            return !a * v.c
        }

    let rx =
        String.concat "" [
            "[ \t\r\n]*void[ \t]+(.*?)_util_(.*?)\(inout[ \t]+float[ \t]+a\)"
            "[ \t\r\n]*\{"
            "[ \t\r\n]*a[ \t]+\=[ \t]+\(a[ \t]+\+[ \t]+1\.0\);"
            "[ \t\r\n]*\}"
        ]

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [rx]


[<GLSLIntrinsic("atomicAdd({0}, {1})")>]
let atomicAdd (r : ref<int>) (v : int) = onlyInShaderCode "atomicAdd"

type UniformScope with
    member x.Test : int[] = uniform?StorageBuffer?Test

[<Test>]
let ``Ref storage buffer modification``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            atomicAdd &&uniform.Test.[0] 19
            return float uniform.Test.[0] * v.c
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [ "atomicAdd" ]




type Shader private () =

    static member Sampler =
        sampler2d {
            texture uniform?texture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    [<LocalSize(X = 8, Y = 8)>]
    static member shader (v : V4d[]) =
        compute {
            let id = getGlobalId().XY
            let a =  Shader.Sampler.Sample(V2d id)
            v.[id.X] <- a
        }

[<Test>]
let ``[Compute] includes samplerInfo``() =
    let shader = ComputeShader.ofFunction (V3i(128,128,128)) Shader.shader

    let glsl =
        ComputeShader.toModule shader
            |> ModuleCompiler.compileGLSLVulkan

    let sammy =
        glsl.iface.samplers.["Sampler"].samplerTextures

    let state =
        samplerState {
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    sammy |> should equal ["texture", state ]



type UniformScope with
    member x.Count : int = uniform?Count
    member x.Trafo : M34d = uniform?Hugo

type MyVertex =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
        [<Semantic("AAATrafo")>] trafo : M34d
    }

[<Test>]
let ``Compose variables correct``() =
    let frag (v : MyVertex) =
        fragment {
            let a = v.trafo * v.pos
            let mutable value = a.X
            for i in 0 .. uniform.Count do
                value <- sin(float i) * cos(float i) * value
            return (1.0 + value) * v.c
        }

    GLSL.shouldCompile [ Effect.ofFunction frag; Effect.ofFunction frag]


[<ReflectedDefinition>]
let clampPointToPolygon (vc : int) (p : V3d) =

    if vc > 0 then
        (p, vc, -1, -1)
    else
        (p * 2.0, vc + 1, -1, -1)

[<Test>]
let ``Struct declaration`` () =
    Setup.Run()

    let frag1 (v : Vertex) =
        fragment {

            let (x, y, z, w) = clampPointToPolygon ((int)v.pos.X) (v.pos.XYZ)
            return V4d(x, float (y + z + w))
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag1 ] [ "struct tup_Aardvark_Base_V3d_int32_int32_int32" ]

    let frag2 (v : Vertex) =
        fragment {

            let (x, y, _, _) = clampPointToPolygon ((int)v.pos.X) (v.pos.XYZ)
            return V4d(x, float y)
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag2 ] [ "struct tup_Aardvark_Base_V3d_int32_int32_int32" ]

    let frag3 (v : Vertex) =
        fragment {

            let (x, _, _, _) = clampPointToPolygon ((int)v.pos.X) (v.pos.XYZ)
            return V4d(x, 1.0)
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag3 ] [ "struct tup_Aardvark_Base_V3d_int32_int32_int32" ]


type UniformScope with
    member x.SomeUniform : V3d = uniform?SomeUniform
    member x.SomeUniformArr : Arr<N<3>,V3d> = uniform?SomeUniformArr


[<ReflectedDefinition>] [<Inline>]
let inlineFun1 (forward : V3d) (up : V3d) =
    Vec.Cross(up, -forward) + up - forward


[<ReflectedDefinition>] [<Inline>]
let inlineFun2 (p : V3d) =

    let i = int p.X

    let n = Vec.dot p uniform.SomeUniformArr.[i]

    let irr = inlineFun1 uniform.SomeUniformArr.[i] (uniform.SomeUniform * n)

    irr * p.Z


[<Test>]
let ``Inline Inline`` () =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let x = inlineFun2 (v.pos.XYZ)
            return V4d(x.X, x.Y, x.Z, 1.0)
        }

    GLSL.shouldCompile [ Effect.ofFunction frag ]

let private textureArraySampler =
    sampler2d {
        textureArray uniform?TextureArray 12
        filter Filter.MinMagLinear
        addressU WrapMode.Clamp
        addressV WrapMode.Clamp
    }

[<ReflectedDefinition>] [<Inline>]
let GetSample(switch : int, lc : V2d, sam : Sampler2d) : V3d =
    match switch with
    | 1 -> sam.Sample(lc).XYZ
    | 2 -> sam.Sample(lc * 2.0).XYZ
    | _ -> V3d.OOO

[<Test>]
let ``Sampler Loop Inline`` () =
    Setup.Run()

    let frag (switch : int) (v : Vertex) =
        fragment {
            let lc = v.pos.XY
            let mutable sum = V3d.Zero
            for i in 0..uniform?TextureCount-1 do
                let layer = GetSample(switch, lc, textureArraySampler.[i])
                sum <- sum + layer

            return V4d(sum, 1.0)
        }

    GLSL.shouldCompile [ Effect.ofFunction (frag 1) ]

[<Test>]
let ``Unroll Match`` () =
    Setup.Run()

    let frag (switch : int) (v : Vertex) =
        fragment {
            let mutable res = V3d.OOO
            Preprocessor.unroll()
            for i in 0..5 do
                let p = match i with
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
            return V4d(res, 1.0)
        }

    GLSL.shouldCompile [ Effect.ofFunction (frag 1) ] // should not contain "0 == 0", "0 == 1", "0 == 2", ...

let private arraySampler =
    sampler2dArray {
        texture uniform?TextureArray
        filter Filter.MinMagLinear
        addressU WrapMode.Clamp
        addressV WrapMode.Clamp
    }

let private simpleSampler =
    sampler2d {
        texture uniform?Simple
        filter Filter.MinMagLinear
        addressU WrapMode.Clamp
        addressV WrapMode.Clamp
    }

[<Test>]
let ``Array Samplers`` () =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let lc = v.pos.XY
            let mutable sum = V4d.Zero
            for i in 0..uniform?TextureCount-1 do
                let layer = arraySampler.Read(V2i(int lc.X, int lc.Y), i, 0)
                sum <- sum + layer

            return sum
        }

    GLSL.shouldCompile [ Effect.ofFunction (frag) ]

[<Test>]
let ``Simple Fetch`` () =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let a = simpleSampler.[V2i.IO]
            let b = simpleSampler.[V2i.OI, 1]
            let c = simpleSampler.Read(V2i.II, 7)

            return a + b + c
        }

    GLSL.shouldCompile [ Effect.ofFunction (frag) ]

type Fragment =
    {
        [<Color>] c : V4d
        [<Depth(DepthWriteMode.OnlyLess)>] d : float
    }

let ``Depth Only Less``() =
    Setup.Run()

    let fraggy (v : Vertex) =
        fragment {
            return {
                c = V4d.IIII
                d = 0.5
            }
        }

    GLSL.shouldCompile [ Effect.ofFunction (fraggy) ]
[<ReflectedDefinition>] [<Inline>]
let createTuple(x : V3d) : (Arr<N<5>,V3d> * int) =
    let arr = Arr<N<5>, V3d>()
    let mutable cnt = 0
    for i in 0..4 do
        if x.Length < float i * 0.4 then
            arr.[cnt] <- x
            cnt <- cnt + 1

    (arr, cnt)

let ``Tuple Inline`` () =
    Setup.Run()

    let frag (v : Vertex) =

        fragment {

            let (va, vc) = createTuple v.pos.XYZ

            let mutable col = V3d.OOO
            for i in 0..vc-1 do
                col <- col + (va.[i]) * (float i)

            return V4d(col, 1.0)
        }

    GLSL.shouldCompile [ Effect.ofFunction (frag) ]

[<ReflectedDefinition>] [<Inline>]
let computeSome va vb vc =
    let e1 = vb - va
    let e2 = vc - va
    Vec.cross e1 e2 |> Vec.length


[<Test>]
let ``Variable Declaration`` () =
    Setup.Run()

    let frag (v : Vertex) =

        fragment {

            let va = Unchecked.defaultof<Arr<N<5>,V3d>>

            let tmp = v.pos * 2.0
            let mutable value = 0.0
            for i in 0..3 do
                let v = if v.pos.W > 0.0 then 1.0 else computeSome tmp.XYZ va.[i] va.[i+1]
                value <- value + v

            return V4d(value)
        }

    GLSL.shouldCompile [ Effect.ofFunction (frag) ]

type VertexClip =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
        [<ClipDistance>] cd : float[]
    }

[<Test>]
let ``ClipDistance Pass-Through`` () =
    Setup.Run()

    let vs (v : Vertex) =
        vertex {

            let plane : V4d = uniform?ClipPlane
            let cd = Vec.dot v.pos plane

            return { pos = v.pos; c = v.c ; cd = [| cd |] }
        }

    let gs (t : Triangle<VertexClip>) =

        triangle {
            yield t.P0
            yield t.P1
            yield t.P2
        }

    let frag (v : Vertex) =

        fragment {
            return V4d(1, 1, 1, 1)
        }

    GLSL.shouldCompile [ Effect.ofFunction vs; Effect.ofFunction gs; Effect.ofFunction frag ]


type VertexWithPid =
    {
        [<Position>] pos : V4d
        [<PrimitiveId>] pid : uint32
    }

[<Test>]
let ``GS PrimitiveId`` () =
    Setup.Run()

    let gs (t : Triangle<VertexWithPid>) =

        triangle {
            let pid = t.P0.pid
            if pid &&& 1u = 0u then
                yield t.P0
                yield t.P1
                yield t.P2
        }

    let frag (v : Vertex) =

        fragment {
            return V4d(1, 1, 1, 1)
        }

    GLSL.shouldCompile [ Effect.ofFunction gs; Effect.ofFunction frag ]

type VertexLayer =
    {
        [<Position>] pos : V4d
        [<Layer>] l : int
    }

[<Test>]
let ``GS Composition with Layer`` () =
    Setup.Run()

    let gs1 (t : Triangle<VertexLayer>) =

        triangle {
            let layer = int t.P0.pos.X
            yield { t.P0 with l = layer }
            yield { t.P1 with l = layer }
            yield { t.P2 with l = layer }
        }

    let gs2 (t : Triangle<VertexLayer>) =

        triangle {
            yield t.P0
            yield t.P1
            yield t.P2

            restartStrip()

            yield t.P0
            yield t.P2
            yield t.P1
        }

    let frag (v : Vertex) =

        fragment {
            return V4d(1, 1, 1, 1)
        }

    GLSL.shouldCompile [ Effect.ofFunction gs1; Effect.ofFunction gs2; Effect.ofFunction frag ]

type VertexLayerSid =
    {
        [<Position>] pos : V4d
        [<Layer>] l : int
        [<SourceVertexIndex>] sid : int
    }

[<Test>]
let ``GS Composition with Layer2`` () =
    Setup.Run()

    let gs1 (t : Triangle<VertexLayerSid>) =

        triangle {
            let layer = int t.P0.pos.X
            yield { t.P0 with l = layer; sid = 0 }
            yield { t.P1 with l = layer; sid = 1 }
            yield { t.P2 with l = layer; sid = 2 }
        }

    let gs2 (t : Triangle<Vertex>) =

        triangle {
            if t.P0.pos.X > 1.0 then
                yield t.P0
                yield t.P1
                yield t.P2
        }

    let frag (v : Vertex) =

        fragment {
            return V4d(1, 1, 1, 1)
        }

    GLSL.shouldCompile [ Effect.ofFunction gs1; Effect.ofFunction gs2; Effect.ofFunction frag ]

let intergerSampler =
    intSampler2d {
        texture uniform?IntTexture
    }

[<Test>]
let ``IntSampler`` () =
    Setup.Run()

    let ps (v : Vertex) =
        fragment {
            let value = intergerSampler.Sample(v.pos.XY).X
            return V4d(value, 1, 1, 1)
        }

    GLSL.shouldCompile [ Effect.ofFunction ps; ]

[<Test>]
let ``GLSLTypesToString`` () =

    Setup.Run()

    let ps (v : Vertex) =
        fragment {
            return v.c
        }

    let glsl, res = FShade.Tests.GLSL.compile [ Effect.ofFunction ps; ]

    FShade.GLSL.GLSLProgramInterface.print glsl.iface

    Console.WriteLine(glsl.iface.ToString())

    Console.WriteLine(glsl.ToString())

    glsl.iface.shaders |> MapExt.iter (fun k v ->
        Console.WriteLine(v.ToString()))

    FShade.GLSL.GLSLProgramInterface.log glsl.iface

    ()

let sampler1 =
    sampler2d {
        texture uniform?Texture
    }

let sampler2 =
    sampler2d {
        texture uniform?Texture
    }

let sampler3 =
    sampler2d {
        texture uniform?OtherTexture
    }

[<Test>]
let ``DuplicateId`` () =
    Setup.Run()

    let psOther (v : Vertex) =
        fragment {
            let value = sampler3.Sample(v.pos.XY).X
            return V4d(value, 1.0, 1.0, 1.0)
        }

    let fxOther = Effect.ofFunction psOther

    let ps1 (v : Vertex) =
        fragment {
            let value = sampler1.Sample(v.pos.XY).X
            return V4d(value, 1.0, 1.0, 1.0)
        }

    let fx1 = Effect.ofFunction ps1

    // different texture name and sampler name -> passes
    if fxOther.Id = fx1.Id then
        failwith "FAIL"

    let ps2 (v : Vertex) =
        fragment {
            let value = sampler2.Sample(v.pos.XY).X
            return V4d(value, 1.0, 1.0, 1.0)
        }

    let fx2 = Effect.ofFunction ps2

    // same texture name, but different sampler name -> will have same Id
    if fx1.Id = fx2.Id then
        failwith "duplicate id"


[<ReflectedDefinition>]
let helper (v : V4d) =
    // without this uniform query (e.g. replacing it by a constant) the helper will be generated in the global block
    match uniform?CRASH with
    | 1 -> v * 2.0
    | _ -> v

[<Test>]
let ``VS/TS shared helper`` () =
    Setup.Run()

    let vs(v : Vertex) =
        vertex {
            let pp = helper v.pos
            return { v with pos = pp }
        }

    let ts (t : Patch<3 N, Vertex>) =

        tessellation {

            let! coord = tessellateTriangle 2.0 (2.0, 2.0, 2.0)

            let v = t.[0].pos * coord.X + t.[1].pos * coord.Y + t.[2].pos * coord.Z

            let pp = helper v

            return { t.[0] with pos = pp }
        }

    let frag (v : Vertex) =
        fragment {
            return V4d(1, 1, 1, 1)
        }

    GLSL.shouldCompile [ Effect.ofFunction vs; Effect.ofFunction ts; Effect.ofFunction frag ]

[<ReflectedDefinition>]
let foo () =
    V3d.Zero

[<Test>]
let ``Reflected Function``() =
    Setup.Run()

    let vs (v : Vertex) =
        vertex {
            return foo()
        }

    let fs (v : Vertex) =
        fragment {
            return V4d(foo(), 1.0)
        }

    GLSL.shouldCompile [Effect.ofFunction vs; Effect.ofFunction fs]

[<Test>]
let ``Non-static sampler``() =
    Setup.Run()

    let textureDiffuse =
        sampler2d {
            texture uniform?DiffuseTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let fs (v : Vertex) =
        fragment {
            return textureDiffuse.Sample(V2d.Zero)
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

//[<EntryPoint>]
//let main args =
//    ``New Intrinsics``()
    //``GS Composition with Layer2``()
    //``GS Composition with Layer``()
    //``GS PrimitiveId``()
    //``DuplicateId``()
    //``ClipDistance Pass-Through``()
    //``Variable Declaration``()
    //``Fill Array with Inline Function``()
    //``Tuple Inline``()
    //``Helper with duplicate names``()
    //``Bad Helpers``()
    //``Helper with duplicate names``()
    //``Unroll Match``()
    //``Array Samplers``()
