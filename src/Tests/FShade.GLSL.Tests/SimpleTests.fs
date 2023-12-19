module SingleEffects

open System
open Aardvark.Base
open FShade
open NUnit.Framework
open FsUnit
open FShade.Tests

type MyEnum =
    | A = 1234us
    | B = 4321us

type Vertex =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
        [<Interpolation(InterpolationMode.NoPerspective ||| InterpolationMode.Sample)>] hugo : V3d
        [<Interpolation(InterpolationMode.Flat ||| InterpolationMode.PerPatch)>] hugo2 : V3d
        foo : V4d
        what : V4i
        [<Semantic("Id")>] id : int
        [<PrimitiveId>] primId : int
        [<Semantic("uid")>] uid : uint
        myenum : MyEnum
    }

type F32Vertex =
    {
        [<Color>] c : V4f
        whatever : float32
    }

type IntVertex =
    {
        [<Color>] c : int
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
    
type MyVertex2 =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
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

    glsl.iface.shaders |> GLSL.GLSLProgramShaders.iter (fun v ->
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

module ReflectedFunctionTest = 

    let constantArray =
        [| V4d.Zero |]

    [<ReflectedDefinition>]
    let foo () =
        V3d.Zero

    [<ReflectedDefinition>]
    let get (tc : float) =
        constantArray.[int tc]

[<Test>]
let ``Reflected function``() =
    Setup.Run()

    let vs (v : Vertex) =
        vertex {
            return ReflectedFunctionTest.foo()
        }

    let fs (v : Vertex) =
        fragment {
            return V4d(ReflectedFunctionTest.foo(), 1.0)
        }

    GLSL.shouldCompile [Effect.ofFunction vs; Effect.ofFunction fs]

[<Test>]
let ``Reflected function with constant array``() =
    Setup.Run()

    let function0 (v : MyVertex2) =
         vertex {
            let h = ReflectedFunctionTest.get (float ((v.pos.X**0.15)*0.8))
            return { v with c = h }
         }

    let function1 (v : MyVertex2) =
        vertex { 
            let h = ReflectedFunctionTest.get (float v.pos.Y / 6.0)
            return { v with pos = h }
        }

    let function2 (v : MyVertex2) =
        fragment {
            let h = ReflectedFunctionTest.get (v.pos.X / 8.0)
            return { v with pos = h }
        }

    GLSL.shouldCompile [
        Effect.ofFunction function0
        Effect.ofFunction function1
        Effect.ofFunction function2
    ]

[<Test>]
let ``Multiple interpolation qualifiers``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return v.hugo + v.hugo2
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction fs] ["flat"; "noperspective sample"]

[<Test>]
let ``Integer with implicit flat interpolation``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return V3d v.what
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Integer with implicit flat interpolation 2``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let _ = v.what
            let _ = v.uid
            return v.c
        }

    GLSL.shouldCompile [Effect.ofFunction fs]

[<Test>]
let ``Integer fragment output``() =
    Setup.Run()

    let fs (v : IntVertex) =
        fragment {
            return V2i(v.c + 1, 0)
        }

    let fs2 (v : IntVertex) =
        fragment {
            return v.c + 2
        }

    GLSL.shouldCompile [Effect.ofFunction fs; Effect.ofFunction fs2]

[<Test>]
let ``Integer vertex field output``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return { v with c = v.c + 1.0 }
        }

    let fs2 (v : Vertex) =
        fragment {
            return { v with id = int v.id; uid = uint v.id}
        }

    GLSL.shouldCompile [Effect.ofFunction fs; Effect.ofFunction fs2]

[<Test>]
let ``Output type conversions``() =
    Setup.Run()

    let fs1 (v : Vertex) =
        fragment {
            return v.c + 1.0
        }

    let fs2 (v : Vertex) =
        fragment {
            return v.c.ZXY + 2.0
        }

    let fs3 (v : Vertex) =
        fragment {
            return v.c.WX + 3.0
        }

    GLSL.shouldCompile [
        Effect.ofFunction fs1
        Effect.ofFunction fs2
        Effect.ofFunction fs3
    ]

[<Test>]
let ``Primitive id``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return v3ui <| V4i(v.primId)
        }

    GLSL.shouldCompile [
        Effect.ofFunction fs
    ]

[<Test>]
let ``Debug output``() =
    Setup.Run()

    let formatStr = "This is a format string"

    let fs (v : Vertex) =
        fragment {
            Debug.Printf("Hello" + " my " +  "World!")
            Debug.Printf(formatStr)
            Debug.Printf("Hello, look at my float: %f", 0.0)
            Debug.Printf("Hello, look at my float: %f", 0.0)
            Debug.Printf("Hello, look at my float: %f", v.c.X)
            Debug.Printf("Hello, look at my vector: %v4f", v.c)

            Debug.Printfn("Hello" + " my " +  "World!")
            Debug.Printfn(formatStr)
            Debug.Printfn("Hello, look at my float: %f", 0.0)
            Debug.Printfn("Hello, look at my float: %f", 0.0)
            Debug.Printfn("Hello, look at my float: %f", v.c.X)
            Debug.Printfn("Hello, look at my vector: %v4f", v.c)

            return V4i.Zero
        }

    GLSL.shouldCompile [
        Effect.ofFunction fs
    ]

[<Test>]
let ``UInt32 literals``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return V3ui(0xFFFFFFFFu)
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] [ $"{UInt32.MaxValue}u" ]

[<Test>]
let ``Float32 vertex types``() =
    Setup.Run()

    let fs1 (v : F32Vertex) =
        fragment {
            return v.c + V4f.XAxis
        }

    let fs2 (v : Vertex) =
        fragment {
            return v.c.YXZW + (V4d.YAxis * 2.0)
        }

    let fs3 (v : F32Vertex) =
        fragment {
            return {| whatever = float (v.c.XYZ + 2.0f).Y |}
        }

    let fs4 (v : F32Vertex) =
        fragment {
            return v.whatever - 1.0f
        }

    GLSL.shouldCompile [
        Effect.ofFunction fs1
        Effect.ofFunction fs2
        Effect.ofFunction fs3
        Effect.ofFunction fs4
    ]

[<Test>]
let ``Enum with non-int32 underlying type``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return V2ui(uint32 v.myenum, uint32 MyEnum.A)
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] [ "uint fs_myenum"; "1234u" ]

type UniformScope with
    member x.SomeVector : V3d = x?Foo?Bar?MyVector
    member x.SomeVector2 : V3d = x?Foo?Bar?MyVector
    member x.BadVector : float = x?Foo?Bar?MyVector
    member x.BadVector2 : V3d = x?MyBuff?Yea?MyVector

[<Test>]
let ``Uniform alias``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return uniform.SomeVector + uniform.SomeVector2
        }

    GLSL.shouldCompile [ Effect.ofFunction fs ]

[<Test>]
let ``Uniform alias with mismatching type``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return uniform.SomeVector + V3d(uniform.BadVector, 0.0, 0.0)
        }

    let exn =
        Assert.Throws(fun () ->
            let e = Effect.ofFunction fs
            e.Shaders |> ignore
        )

    Assert.That(exn.Message, Does.Contain "has conflicting types")

[<Test>]
let ``Uniform alias with mismatching scope``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return uniform.SomeVector + uniform.BadVector2
        }

    let exn =
        Assert.Throws(fun () ->
            let e = Effect.ofFunction fs
            e.Shaders |> ignore
        )

    Assert.That(exn.Message, Does.Contain "has conflicting values")

[<Test>]
let ``Uniform and storage buffers use std140 and std430 layout``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return uniform.RegionBuffer.[0]
        }

    let fsu (v : Vertex) =
        fragment {
            return uniform.SomeUniformArr.[0]
        }

    let cs (mybuffer : int[]) =
        compute {
            let id = getGlobalId().XY
            mybuffer.[id.X] <- id.Y
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fs ] [ "std430" ]
    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction fsu ] [ "std140" ]

    let glsl =
        cs
        |> ComputeShader.ofFunction (V3i(128,128,128))
        |> ComputeShader.toModule
        |> ModuleCompiler.compileGLSL430

    GLSL.shouldContainRegex glsl [ "std430", None ]