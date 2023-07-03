module Hashes

open FsUnit
open NUnit.Framework
open Aardvark.Base
open FShade
open System.IO

type Vertex = 
    {
        [<Position>] pos : V4d
        [<Semantic("TexCoord")>] tc : V2d
    }

type Effect0 private () =

    [<GLSLIntrinsic("someFun({0})")>]
    static let sepp (a : V2d) : V4d =
        onlyInShaderCode "sepp"

    static member vertexShader (v : Vertex) =
        vertex {
            return { v with pos = sepp v.tc }
        }

    static member fragmentShader (v : Vertex) =
        fragment {
            return V4d.Zero
        }

type Shader1 private () =

    static member Sampler =
        sampler2d {
            texture uniform?texture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
            
    static member shader (v : Vertex) =
        fragment {
            return Shader1.Sampler.Sample(v.tc)
        }
            
type Shader2 private () =

    static member Sampler =
        sampler2d {
            texture uniform?texture
            filter Filter.MinMagPoint
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
            
    static member shader (v : Vertex) =
        fragment {
            return Shader2.Sampler.Sample(v.tc)
        }
            
type Shader3 private () =

    static member Sampler =
        sampler2d {
            texture uniform?texture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
            
    static member shader (v : Vertex) =
        fragment {
            return Shader3.Sampler.Sample(v.tc)
        }
                       
type Shader4 private() =

    [<GLSLIntrinsic("someFun({0})")>]
    static let sepp (a : V2d) : V4d =
        onlyInShaderCode "sepp"

    static member shader (v : Vertex) =
        fragment {
            return sepp v.tc
        }
            
type Shader5 private() =

    [<GLSLIntrinsic("someFun({0})")>]
    static let heinz (a : V2d) : V4d =
        onlyInShaderCode "sepp"

    static member shader (v : Vertex) =
        fragment {
            return heinz v.tc
        }

type Shader6 private () =

    static member shader (v : Vertex) =
        fragment {
            let a : V4d = uniform?Blubber?Value
            return a
        }


[<AutoOpen>]
module private Utilities =
    open FSharp.Quotations

    let roundtripExpr (input : Expr) =
        use memory = new MemoryStream()

        let inputHash = Serializer.Expr.computeHash input
        input |> Serializer.Expr.serialize memory

        memory.Position <- 0L
        let output = Serializer.Expr.deserialize memory
        let outputHash = Serializer.Expr.computeHash output

        outputHash |> should equal inputHash

    let roundtrip (func : 'T -> Expr<'U>) =
        let expr = func Unchecked.defaultof<_>
        roundtripExpr expr

    let roundtripEffect (input : Effect) =
        use memory = new MemoryStream()

        input |> Effect.serialize memory
        
        memory.Position <- 0L
        let output = Effect.deserialize memory

        input.Id |> should equal output.Id
        input.Inputs |> should equal output.Inputs
        input.Outputs |> should equal output.Outputs
        //input.Shaders |> should equal output.Shaders // NOTE: cannot be compared this way -> Exrp.Var does not have suitable equality (always false)
        input.Uniforms |> should equal output.Uniforms
        


[<Test>]
let ``[Serializer] instance field get``() =

    let bla (v : Vertex) =
        fragment {
            return V4d(v.pos.X)
        }

    bla |> roundtrip

[<Test>]
let ``[Serializer] instance field set and get``() =

    let bla (v : Vertex) =
        fragment {
            let mutable res = v.pos
            res.X <- v.pos.Z
            return res
        }

    bla |> roundtrip

type SomeFuncs() =

    [<ReflectedDefinition>]
    static member Hehe (input : V4d) =
        input.XYZ

    [<ReflectedDefinition>]
    static member Hehe<'T> (input : 'T) =
        input

    [<ReflectedDefinition>]
    static member Hehe<'T> (input : 'T, foo : int) =
        input

    [<ReflectedDefinition>]
    static member Hehe<'T1, 'T2> (i1 : 'T1, i2 : 'T2, foo : int) =
        i1

    static member Haha (input : V4d) =
        input.XYZ

    static member Haha<'T> (input : 'T) =
        input

    static member Haha<'T> (input : 'T, foo : int) =
        input

    static member Haha<'T1, 'T2> (i1 : 'T1, i2 : 'T2, foo : int) =
        i1

    member x.Hihi (input : V4d) =
        input.XYZ

    member x.Hihi<'T> (input : 'T) =
        input

    member x.Hihi<'T> (input : 'T, foo : int) =
        input

    member x.Hihi<'T1, 'T2> (i1 : 'T1, i2 : 'T2, foo : int) =
        i1


[<Test>]
let ``[Serializer] reflected function``() =

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Hehe v.pos, 0.0)
        }

    bla |> roundtrip

[<Test>]
let ``[Serializer] reflected generic function``() =

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Hehe(v.pos.XYZ, 3) + SomeFuncs.Hehe(v.pos.XYZ, v.tc, 3), 0.0)
        }

    bla |> roundtrip

[<Test>]
let ``[Serializer] static call``() =

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Haha v.pos, 0.0)
        }

    bla |> roundtrip

[<Test>]
let ``[Serializer] static generic call``() =

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Haha(v.pos.XYZ, 3) + SomeFuncs.Haha(v.pos.XYZ, v.tc, 3), 0.0)
        }

    bla |> roundtrip

[<Test>]
let ``[Serializer] instance call``() =

    let bla (v : Vertex) =
        fragment {
            let funcs = SomeFuncs()
            return V4d(funcs.Hihi v.pos, 0.0)
        }

    bla |> roundtrip

[<Test>]
let ``[Serializer] instance generic call``() =

    let bla (v : Vertex) =
        fragment {
            let funcs = Unchecked.defaultof<SomeFuncs>
            return V4d(funcs.Hihi(v.pos.XYZ, 3) + funcs.Hihi(v.pos.XYZ, v.tc, 3), 0.0)
        }

    bla |> roundtrip


[<Test>]
let ``[Serializer] utility function call``() =
    Serializer.Init()

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Hehe v.pos, 1.0)
        }

    let shader = Shader.ofFunction bla |> List.head
    shader.shaderBody |> roundtripExpr

[<Test>]
let ``[Serializer] utility function generic call``() =
    Serializer.Init()

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Hehe v.pos.XYZ, 1.0)
        }

    let shader = Shader.ofFunction bla |> List.head
    shader.shaderBody |> roundtripExpr

[<Test>]
let ``[Serializer] sampler arrays``() =

    let samplerArray = 
        sampler2d {
            textureArray uniform?MyTextures 12
        }

    let shader (v : Vertex) =
        fragment {
            let mutable color = V4d.Zero
            let cnt : int = uniform?TextureCount
            for i in 0..cnt-1 do
                color <- color + samplerArray.[i].Sample(v.tc)
            return color
        }

    let fx = shader |> Shader.ofFunction |> List.head
    fx.shaderBody |> roundtripExpr

[<Test>]
let ``[Serializer] storage buffers``() =

    let shader (v : Vertex) =
        fragment {
            let mutable color = V4d.Zero
            let buf : V4d[] = uniform?StorageBuffer?buffy
            let cnt : int = uniform?BufferLength
            for i in 0..cnt-1 do
                color <- color + buf.[i]
            return color
        }

    let fx = shader |> Effect.ofFunction
    fx |> roundtripEffect

type EnumUint8  = | A = 255uy
type EnumInt8   = | A = -128y
type EnumUint16 = | A = 65535us
type EnumInt16  = | A = -32768s
type EnumUint32 = | A = 4294967295u
type EnumInt32  = | A = -2147483648
type EnumUint64 = | A = 18446744073709551615UL
type EnumInt64  = | A = -9223372036854775808L

type UniformScope with
    member x.EnumUint8  : EnumUint8 = x?Foo
    member x.EnumInt8   : EnumInt8 = x?Foo
    member x.EnumUint16 : EnumUint16 = x?Foo
    member x.EnumInt16  : EnumInt16 = x?Foo
    member x.EnumUint32 : EnumUint32 = x?Foo
    member x.EnumInt32  : EnumInt32 = x?Foo
    member x.EnumUint64 : EnumUint64 = x?Foo
    member x.EnumInt64  : EnumInt64 = x?Foo

[<Test>]
let ``[Serializer] enum uint8``() =
    let shader (v : Vertex) =
        fragment { return int (uniform.EnumUint8 + EnumUint8.A) }

    let shader = Shader.ofFunction shader |> List.head
    shader.shaderBody |> roundtripExpr

[<Test>]
let ``[Serializer] enum int8``() =
    let shader (v : Vertex) =
        fragment { return int (uniform.EnumInt8 + EnumInt8.A) }

    let shader = Shader.ofFunction shader |> List.head
    shader.shaderBody |> roundtripExpr

[<Test>]
let ``[Serializer] enum uint16``() =
    let shader (v : Vertex) =
        fragment { return int (uniform.EnumUint16 + EnumUint16.A) }

    let shader = Shader.ofFunction shader |> List.head
    shader.shaderBody |> roundtripExpr

[<Test>]
let ``[Serializer] enum int16``() =
    let shader (v : Vertex) =
        fragment { return int (uniform.EnumInt16 + EnumInt16.A) }

    let shader = Shader.ofFunction shader |> List.head
    shader.shaderBody |> roundtripExpr

[<Test>]
let ``[Serializer] enum uint32``() =
    let shader (v : Vertex) =
        fragment { return int (uniform.EnumUint32 + EnumUint32.A) }

    let shader = Shader.ofFunction shader |> List.head
    shader.shaderBody |> roundtripExpr

[<Test>]
let ``[Serializer] enum int32``() =
    let shader (v : Vertex) =
        fragment { return int (uniform.EnumInt32 + EnumInt32.A) }

    let shader = Shader.ofFunction shader |> List.head
    shader.shaderBody |> roundtripExpr

[<Test>]
let ``[Serializer] enum uint64``() =
    let shader (v : Vertex) =
        fragment { return int (uniform.EnumUint64 + EnumUint64.A) }

    let shader = Shader.ofFunction shader |> List.head
    shader.shaderBody |> roundtripExpr

[<Test>]
let ``[Serializer] enum int64``() =
    let shader (v : Vertex) =
        fragment { return int (uniform.EnumInt64 + EnumInt64.A) }

    let shader = Shader.ofFunction shader |> List.head
    shader.shaderBody |> roundtripExpr

[<Test>]
let ``[Hashing] includes SamplerState``() =

    let e1 = Effect.ofFunction (Shader1.shader)
    let e2 = Effect.ofFunction (Shader2.shader)

    e1.Id |> should not' (equal e2.Id)

[<Test>]
let ``[Hashing] SamplerState hash works``() =

    let e1 = Effect.ofFunction (Shader1.shader)
    let e2 = Effect.ofFunction (Shader1.shader)

    e1.Id |> should equal e2.Id

[<Test>]
let ``[Hashing] SamplerState hash is structural``() =

    let e1 = Effect.ofFunction (Shader1.shader)
    let e2 = Effect.ofFunction (Shader3.shader)

    e1.Id |> should equal e2.Id

module SamplerShader =
    let private sampler =
        sampler2d {
            texture uniform?texture
        }

    let shader (v : Vertex) =
        fragment {
            return sampler.Sample(v.tc)
        }

module SamplerShader2 =
    let private sampler =
        sampler2d {
            texture uniform?texture2
        }

    let shader (v : Vertex) =
        fragment {
            return sampler.Sample(v.tc)
        }

[<Test>]
let ``[Hashing] sampler includes texture name``() =
    let e1 = Effect.ofFunction SamplerShader.shader
    let e2 = Effect.ofFunction SamplerShader2.shader

    e1.Id |> should not' (equal e2.Id)

type UniformScope with
    member x.Img  : Image1d<Formats.rgba8>   = x?Img
    member x.Img2 : Image1d<Formats.rgba8>   = x?Img
    member x.Img3 : Image1d<Formats.rgba32f> = x?Img
    member x.Foo  : V3d                      = x?Foo
    member x.Foo2 : V3d                      = x?Foo
    member x.Foo3 : V3d                      = x?Foo3
    member x.Foo4 : V2d                      = x?Foo

[<Test>]
let ``[Hashing] includes image format``() =
    let s1 (v : Vertex) =
        fragment {
            return uniform.Img.Load 0
        }

    let s2 (v : Vertex) =
        fragment {
            return uniform.Img2.Load 0
        }

    let s3 (v : Vertex) =
        fragment {
            return uniform.Img3.Load 0
        }

    let e1 = Effect.ofFunction s1
    let e2 = Effect.ofFunction s2
    let e3 = Effect.ofFunction s3

    e1.Id |> should equal e2.Id
    e1.Id |> should not' (equal e3.Id)

[<Test>]
let ``[Hashing] includes uniform semantic``() =
    let s1 (v : Vertex) =
        fragment {
            let _ = uniform.Foo
            return V3d.Zero
        }

    let s2 (v : Vertex) =
        fragment {
            let _ = uniform.Foo2
            return V3d.Zero
        }

    let s3 (v : Vertex) =
        fragment {
            let _ = uniform.Foo3
            return V3d.Zero
        }

    let e1 = Effect.ofFunction s1
    let e2 = Effect.ofFunction s2
    let e3 = Effect.ofFunction s3

    e1.Id |> should equal e2.Id
    e1.Id |> should not' (equal e3.Id)

[<Test>]
let ``[Hashing] includes uniform type``() =
    let s1 (v : Vertex) =
        fragment {
            let _ = uniform.Foo
            return V3d.Zero
        }

    let s2 (v : Vertex) =
        fragment {
            let _ = uniform.Foo2
            return V3d.Zero
        }

    let s3 (v : Vertex) =
        fragment {
            let _ = uniform.Foo4
            return V3d.Zero
        }

    let e1 = Effect.ofFunction s1
    let e2 = Effect.ofFunction s2
    let e3 = Effect.ofFunction s3

    e1.Id |> should equal e2.Id
    e1.Id |> should not' (equal e3.Id)

[<Test>]
let ``[Hashing] intrinsic hashes on target-function``() =

    let e1 = Effect.ofFunction Shader4.shader
    let e2 = Effect.ofFunction Shader5.shader

    e1.Id |> should equal e2.Id


// Note: Hashes depend on assembly version
// -> unit tests containing hard coded hashes must be fixed for each new major version or when the serializer is modified
[<Test>]
let ``[Hashing] deterministic uniforms``() =
    let subScopes = [ "Bla"; "Blurg"; "Blubber" ].RandomOrder() |> Seq.toList
    for s in subScopes do uniform.GetChildScope s |> ignore

    let e1 = Effect.ofFunction Shader6.shader

    e1.Id |> should equal "I28tKjpOedsPUgwmki84VX722d0="


[<Test>]
let ``[ComputeHashing] equal => equal hash``() =
    let s = V3i(128,128,128)
    let a (dst : int[]) (src : int[]) =
        compute {
            let id = getGlobalId().X
            dst.[id] <- src.[id] * 2
        }

    let b (dst : int[]) (src : int[]) =
        compute {
            let id = getGlobalId().X
            dst.[id] <- src.[id] * 2
        }
    let sa = ComputeShader.ofFunction s a
    let sb = ComputeShader.ofFunction s b

    sa.csId |> should equal sb.csId
    
[<Test>]
let ``[ComputeHashing] different => different hash``() =
    let s = V3i(128,128,128)
    let a (dst : int[]) (src : int[]) =
        compute {
            let id = getGlobalId().X
            let v : int = uniform?Bla
            dst.[id] <- src.[id] * v
        }

    let b (dst : int[]) (src : int[]) =
        compute {
            let id = getGlobalId().X
            let v : int = uniform?Bla
            dst.[id] <- src.[id] * 3
        }
    let sa = ComputeShader.ofFunction s a
    let sb = ComputeShader.ofFunction s b

    sa.csId |> should not' (equal sb.csId)

[<LocalSize(X = 16)>]
let a (dst : int[]) (src : int[]) =
    compute {
        let id = getGlobalId().X
        let v : int = uniform?Bla
        dst.[id] <- src.[id] * v
    }
    
[<LocalSize(X = 32)>]
let b (dst : int[]) (src : int[]) =
    compute {
        let id = getGlobalId().X
        let v : int = uniform?Bla
        dst.[id] <- src.[id] * v
    }
[<Test>]
let ``[ComputeHashing] including localSize``() =
    let s = V3i(128,128,128)
    let sa = ComputeShader.ofFunction s a
    let sb = ComputeShader.ofFunction s b

    sa.csId |> should not' (equal sb.csId)