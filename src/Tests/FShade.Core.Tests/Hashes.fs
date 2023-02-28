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

    let roundtripExpr (expected : string) (input : Expr) =
        use memory = new MemoryStream()

        let inputHash = Serializer.Expr.computeHash input
        inputHash |> should equal expected
        input |> Serializer.Expr.serialize memory

        memory.Position <- 0L
        let output = Serializer.Expr.deserialize memory
        let outputHash = Serializer.Expr.computeHash output

        outputHash |> should equal inputHash

    let roundtrip (expected : string) (func : 'T -> Expr<'U>) =
        let expr = func Unchecked.defaultof<_>
        roundtripExpr expected expr

[<Test>]
let ``[Serializer] instance field get``() =

    let bla (v : Vertex) =
        fragment {
            return V4d(v.pos.X)
        }

    bla |> roundtrip "DCv1azx4RLpvpq2wjhVDMT8+Zns="

[<Test>]
let ``[Serializer] instance field set and get``() =

    let bla (v : Vertex) =
        fragment {
            let mutable res = v.pos
            res.X <- v.pos.Z
            return res
        }

    bla |> roundtrip "m6EfMsKRtQIaYL3oKes/a2lQoPg="

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

    bla |> roundtrip "HSV9Bs7pYBI7eCCVwRZqiRZ05tk="

[<Test>]
let ``[Serializer] reflected generic function``() =

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Hehe(v.pos.XYZ, 3) + SomeFuncs.Hehe(v.pos.XYZ, v.tc, 3), 0.0)
        }

    bla |> roundtrip "kXS5ZykIckcdE7Mo0n/iODYl0W8="

[<Test>]
let ``[Serializer] static call``() =

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Haha v.pos, 0.0)
        }

    bla |> roundtrip "j/im0yqw4G9YnJV5Xa8vRKpaFh4="

[<Test>]
let ``[Serializer] static generic call``() =

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Haha(v.pos.XYZ, 3) + SomeFuncs.Haha(v.pos.XYZ, v.tc, 3), 0.0)
        }

    bla |> roundtrip "jZHIIvwGiXBWGX2/rkBc645hXPA="

[<Test>]
let ``[Serializer] instance call``() =

    let bla (v : Vertex) =
        fragment {
            let funcs = SomeFuncs()
            return V4d(funcs.Hihi v.pos, 0.0)
        }

    bla |> roundtrip "LdQZKI0BV19HadpFKBz48vOFS7Q="

[<Test>]
let ``[Serializer] instance generic call``() =

    let bla (v : Vertex) =
        fragment {
            let funcs = Unchecked.defaultof<SomeFuncs>
            return V4d(funcs.Hihi(v.pos.XYZ, 3) + funcs.Hihi(v.pos.XYZ, v.tc, 3), 0.0)
        }

    bla |> roundtrip "yr+7sLte0QF1u77SC8tSXa+R6hU="


[<Test>]
let ``[Serializer] utility function call``() =
    Serializer.Init()

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Hehe v.pos, 1.0)
        }

    let shader = Shader.ofFunction bla |> List.head
    shader.shaderBody |> roundtripExpr "HjiiX8umznSdGrQ03JUZq2b4nAc="

[<Test>]
let ``[Serializer] utility function generic call``() =
    Serializer.Init()

    let bla (v : Vertex) =
        fragment {
            return V4d(SomeFuncs.Hehe v.pos.XYZ, 1.0)
        }

    let shader = Shader.ofFunction bla |> List.head
    shader.shaderBody |> roundtripExpr "cH0b4Q0FVt0FkN1wVQpQQ+6CPqI="

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

[<Test>]
let ``[Hashing] intrinsic hashes on target-function``() =

    let e1 = Effect.ofFunction Shader4.shader
    let e2 = Effect.ofFunction Shader5.shader

    e1.Id |> should equal e2.Id


// Note: Hashes depend on assembly version
// -> unit tests containing hard coded hashes must be fixed for each new major version
[<Test>]
let ``[Hashing] deterministic uniforms``() =
    let subScopes = [ "Bla"; "Blurg"; "Blubber" ].RandomOrder() |> Seq.toList
    for s in subScopes do uniform.GetChildScope s |> ignore

    let e1 = Effect.ofFunction Shader6.shader

    e1.Id |> should equal "0lhtS6x7oVav/FgQUaj9lVV6Djs="


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