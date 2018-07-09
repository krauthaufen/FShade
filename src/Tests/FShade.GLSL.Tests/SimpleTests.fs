module SingleEffects

open System
open Aardvark.Base
open FShade
open Xunit
open FsUnit
open FShade.Tests

type Vertex =
    {
        [<Position>] pos : V4d
        [<Color>] c : V4d
    }
        

    
[<Fact>]
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



[<Fact>]
let ``Simple fragment shader``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c
        }
            
    GLSL.shouldCompile [ Effect.ofFunction frag ]
        

[<Fact>]
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
        

[<Fact>]
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
        

[<Fact>]
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
    

[<Fact>]
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
    

[<Fact>]
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
        
[<Fact>]
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
        let v = clamp 0.0 1.0 v

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

[<Fact>]
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

[<Fact>]
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

[<Fact>]
let ``Ref storage buffer modification``() =
    Setup.Run()
    
    let frag (v : Vertex) =
        fragment {
            atomicAdd &&uniform.Test.[0] 19
            return float uniform.Test.[0] * v.c 
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [ "atomicAdd" ]
    


[<EntryPoint>]
let main args =
    //``Helper with duplicate names``()
    ``Bad Helpers``()
    0