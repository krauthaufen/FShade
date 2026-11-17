module Interlock

open System
open System.Text.RegularExpressions
open Aardvark.Base
open FShade
open FShade.GLSL
open FShade.Tests
open NUnit.Framework

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
    }

type UniformScope with
    member x.Counter : int[] = uniform?StorageBuffer?Counter
    member x.Output : Image2d<Formats.rgba32f> = uniform?Output

// glslang accepts the interlock layout qualifiers only for GLSL >= 4.50 (the ARB extension is written against 4.50).
let private glsl450 = Backend.Create { glsl430.Config with version = GLSLVersion(4, 5, 0) }

let private extensionRx = Regex.Escape "#extension GL_ARB_fragment_shader_interlock : enable"
let private beginRx = Regex.Escape "beginInvocationInterlockARB();"
let private endRx = Regex.Escape "endInvocationInterlockARB();"

let private interlocked (mode : FragmentInterlockMode) (v : Vertex) =
    fragment {
        beginInvocationInterlock mode
        let old = uniform.Output.[V2i v.pos.XY]
        uniform.Output.[V2i v.pos.XY] <- old + v.c
        endInvocationInterlock()
        return v.c
    }

let private plain (v : Vertex) =
    fragment {
        return v.c * 0.5f
    }

let private compileFails (effects : list<Effect>) (expected : string) =
    let exn = Assert.Throws(fun () -> GLSL.compile' glsl450 effects |> ignore)
    Assert.That(exn.Message, Does.Contain expected)

[<Test>]
let ``Pixel ordered``() =
    Setup.Run()

    let expected = [
        extensionRx
        Regex.Escape "layout(pixel_interlock_ordered) in;"
        beginRx
        endRx
    ]

    GLSL.shouldCompileAndContainRegex' glsl450 [ Effect.ofFunction (interlocked FragmentInterlockMode.PixelOrdered) ] expected
    GLSL.shouldCompileAndContainRegex' glslVulkan [ Effect.ofFunction (interlocked FragmentInterlockMode.PixelOrdered) ] expected

[<Test>]
let ``Modes``() =
    Setup.Run()

    let check (mode : FragmentInterlockMode) (qualifier : string) =
        GLSL.shouldCompileAndContainRegex' glsl450 [ Effect.ofFunction (interlocked mode) ] [
            Regex.Escape (sprintf "layout(%s) in;" qualifier)
        ]

    check FragmentInterlockMode.PixelUnordered "pixel_interlock_unordered"
    check FragmentInterlockMode.SampleOrdered "sample_interlock_ordered"
    check FragmentInterlockMode.SampleUnordered "sample_interlock_unordered"

[<Test>]
let ``Calls are kept and emitted once``() =
    Setup.Run()

    GLSL.shouldCompileAndContainRegexWithCount' glsl450 [ Effect.ofFunction (interlocked FragmentInterlockMode.PixelOrdered) ] [
        beginRx, 1
        endRx, 1
        Regex.Escape "layout(pixel_interlock_ordered) in;", 1
    ]

[<Test>]
let ``Composition with plain fragment shader``() =
    Setup.Run()

    let expected = [
        beginRx, 1
        endRx, 1
        Regex.Escape "layout(pixel_interlock_ordered) in;", 1
    ]

    // interlocked shader first: the plain shader runs after the critical section
    GLSL.shouldCompileAndContainRegexWithCount' glsl450 [
        Effect.ofFunction (interlocked FragmentInterlockMode.PixelOrdered)
        Effect.ofFunction plain
    ] expected

    // interlocked shader last
    GLSL.shouldCompileAndContainRegexWithCount' glsl450 [
        Effect.ofFunction plain
        Effect.ofFunction (interlocked FragmentInterlockMode.PixelOrdered)
    ] expected

[<Test>]
let ``Composition with vertex shader``() =
    Setup.Run()

    let vs (v : Vertex) =
        vertex {
            return { v with pos = v.pos * 2.0f }
        }

    GLSL.shouldCompileAndContainRegexWithCount' glsl450 [
        Effect.ofFunction vs
        Effect.ofFunction (interlocked FragmentInterlockMode.PixelOrdered)
    ] [ beginRx, 1; endRx, 1 ]

[<Test>]
let ``Two interlocked shaders cannot be composed``() =
    Setup.Run()

    compileFails [
        Effect.ofFunction (interlocked FragmentInterlockMode.PixelOrdered)
        Effect.ofFunction (interlocked FragmentInterlockMode.PixelOrdered)
    ] "both use invocation interlock"

[<Test>]
let ``Call inside control flow fails``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            if v.pos.X > 0.0f then
                beginInvocationInterlock FragmentInterlockMode.PixelOrdered
                uniform.Counter.[0] <- uniform.Counter.[0] + 1
                endInvocationInterlock()
            return v.c
        }

    compileFails [ Effect.ofFunction fs ] "top level of the fragment shader"

[<ReflectedDefinition>]
let private criticalHelper (i : int) =
    beginInvocationInterlock FragmentInterlockMode.PixelOrdered
    uniform.Counter.[i] <- uniform.Counter.[i] + 1
    endInvocationInterlock()

[<Test>]
let ``Call inside utility function fails``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            criticalHelper (int v.pos.X)
            return v.c
        }

    compileFails [ Effect.ofFunction fs ] "top level of the fragment shader"

[<Test>]
let ``Missing end fails``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            beginInvocationInterlock FragmentInterlockMode.PixelOrdered
            uniform.Counter.[0] <- uniform.Counter.[0] + 1
            return v.c
        }

    compileFails [ Effect.ofFunction fs ] "exactly once"

[<Test>]
let ``End before begin fails``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            endInvocationInterlock()
            uniform.Counter.[0] <- uniform.Counter.[0] + 1
            beginInvocationInterlock FragmentInterlockMode.PixelOrdered
            return v.c
        }

    compileFails [ Effect.ofFunction fs ] "must be called after"

[<Test>]
let ``Use in vertex shader fails``() =
    Setup.Run()

    let vs (v : Vertex) =
        vertex {
            beginInvocationInterlock FragmentInterlockMode.PixelOrdered
            endInvocationInterlock()
            return v
        }

    compileFails [ Effect.ofFunction vs; Effect.ofFunction plain ] "only be used in fragment shaders"

[<Test>]
let ``Interface reports interlock mode``() =
    Setup.Run()

    let glsl, _ = GLSL.compile' glsl450 [ Effect.ofFunction (interlocked FragmentInterlockMode.SampleOrdered) ]

    let decorations = glsl.iface.shaders.[ShaderSlot.Fragment].shaderDecorations
    Assert.That(decorations |> List.contains (GLSLFragmentInterlock FragmentInterlockMode.SampleOrdered), Is.True)

    // round trip through the GLSLShader serializer keeps the decoration
    let roundtrip = glsl |> GLSLShader.pickle |> GLSLShader.unpickle
    Assert.That(roundtrip, Is.EqualTo glsl)

[<Test>]
let ``Non-constant mode fails``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            let mode =
                if v.pos.X > 0.0f then FragmentInterlockMode.PixelOrdered
                else FragmentInterlockMode.SampleOrdered

            beginInvocationInterlock mode
            uniform.Counter.[0] <- uniform.Counter.[0] + 1
            endInvocationInterlock()
            return v.c
        }

    compileFails [ Effect.ofFunction fs ] "must be a constant"

[<Test>]
let ``Mode from uniform fails``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            beginInvocationInterlock uniform?InterlockMode
            uniform.Counter.[0] <- uniform.Counter.[0] + 1
            endInvocationInterlock()
            return v.c
        }

    compileFails [ Effect.ofFunction fs ] "must be a constant"

[<Test>]
let ``Unknown mode value fails``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            beginInvocationInterlock (unbox<FragmentInterlockMode> 42)
            uniform.Counter.[0] <- uniform.Counter.[0] + 1
            endInvocationInterlock()
            return v.c
        }

    compileFails [ Effect.ofFunction fs ] "unknown fragment interlock mode"
