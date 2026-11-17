module EarlyFragmentTests

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

type Fragment =
    {
        [<Color>] c : V4f
        [<Depth>] d : float32
    }

type UniformScope with
    member x.Counter : int[] = uniform?StorageBuffer?Counter

let private qualifierRx = Regex.Escape "layout(early_fragment_tests) in;"

let private withMarker (v : Vertex) =
    fragment {
        earlyFragmentTests()
        uniform.Counter.[0] <- uniform.Counter.[0] + 1
        return v.c
    }

[<EarlyFragmentTests>]
let private withAttribute (v : Vertex) =
    fragment {
        uniform.Counter.[0] <- uniform.Counter.[0] + 1
        return v.c
    }

let private plain (v : Vertex) =
    fragment {
        return v.c * 0.5f
    }

let private compileFails (effects : list<Effect>) (expected : string) =
    let exn = Assert.Throws(fun () -> GLSL.compile effects |> ignore)
    Assert.That(exn.Message, Does.Contain expected)

[<Test>]
let ``Marker call``() =
    Setup.Run()

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction withMarker ] [ qualifierRx, 1 ]
    GLSL.shouldCompileAndContainRegexWithCount' glslVulkan [ Effect.ofFunction withMarker ] [ qualifierRx, 1 ]

    // the marker has no representation in the generated code
    let glsl, _ = GLSL.compile [ Effect.ofFunction withMarker ]
    Assert.That(glsl.code, Does.Not.Contain "earlyFragmentTests")

[<Test>]
let ``Attribute``() =
    Setup.Run()

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction withAttribute ] [ qualifierRx, 1 ]

[<Test>]
let ``Attribute affects effect id``() =
    Setup.Run()

    let a = Effect.ofFunction withAttribute
    let b = Effect.ofFunction plain
    Assert.That(a.Id, Is.Not.EqualTo b.Id)

[<Test>]
let ``Composition``() =
    Setup.Run()

    // with a plain shader before and after, and two requesting shaders
    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction withMarker; Effect.ofFunction plain ] [ qualifierRx, 1 ]
    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction plain; Effect.ofFunction withAttribute ] [ qualifierRx, 1 ]
    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction withMarker; Effect.ofFunction withAttribute ] [ qualifierRx, 1 ]

[<Test>]
let ``Together with interlock``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            earlyFragmentTests()
            beginInvocationInterlock FragmentInterlockMode.PixelOrdered
            uniform.Counter.[0] <- uniform.Counter.[0] + 1
            endInvocationInterlock()
            return v.c
        }

    GLSL.shouldCompileAndContainRegexWithCount' glslVulkan [ Effect.ofFunction fs ] [
        qualifierRx, 1
        Regex.Escape "layout(pixel_interlock_ordered) in;", 1
    ]

[<Test>]
let ``Extension for old GLSL versions``() =
    Setup.Run()

    let glsl410 = Backend.Create { glsl430.Config with version = GLSLVersion(4, 1, 0) }
    let (glsl : GLSLShader) = Effect.ofFunction withMarker |> Effect.toModule { EffectConfig.empty with outputs = Map.ofList [ ("Colors", (typeof<V4f>, 0)) ] } |> ModuleCompiler.compileGLSL glsl410

    GLSL.shouldContainRegex glsl [
        Regex.Escape "#extension GL_ARB_shader_image_load_store : enable", None
        qualifierRx, None
    ]

[<Test>]
let ``Depth output only warns``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            earlyFragmentTests()
            return { c = v.c; d = 0.5f }
        }

    // allowed (a warning is logged), the qualifier is still emitted
    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction fs ] [ qualifierRx, 1 ]

[<Test>]
let ``Interface reports early fragment tests``() =
    Setup.Run()

    let (glsl : GLSLShader), _ = GLSL.compile [ Effect.ofFunction withMarker ]
    let decorations = glsl.iface.shaders.[ShaderSlot.Fragment].shaderDecorations
    Assert.That(decorations |> List.contains GLSLEarlyFragmentTests, Is.True)

    let roundtrip = glsl |> GLSLShader.pickle |> GLSLShader.unpickle
    Assert.That(roundtrip, Is.EqualTo glsl)

let private branching (v : Vertex) =
    fragment {
        if v.pos.X > 0.0f then
            return v.c
        else
            return V4f.Zero
    }

[<ReflectedDefinition>]
let private helperWithMarker (c : V4f) =
    earlyFragmentTests()
    c * 2.0f

[<Test>]
let ``Composition after shader with branching returns``() =
    Setup.Run()

    // the body of the second shader is inlined into both branches of the first one;
    // the request must still be honoured and the markers must not leak into the generated code
    let glsl, _ = GLSL.compile [ Effect.ofFunction branching; Effect.ofFunction withMarker ]
    Assert.That(glsl.code, Does.Not.Contain "earlyFragmentTests")

    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction branching; Effect.ofFunction withMarker ] [ qualifierRx, 1 ]
    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction branching; Effect.ofFunction withAttribute ] [ qualifierRx, 1 ]

[<Test>]
let ``Call inside control flow``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            if v.pos.X > 0.0f then
                earlyFragmentTests()
            return v.c
        }

    let glsl, _ = GLSL.compile [ Effect.ofFunction fs ]
    Assert.That(glsl.code, Does.Not.Contain "earlyFragmentTests")
    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction fs ] [ qualifierRx, 1 ]

[<Test>]
let ``Call inside utility function``() =
    Setup.Run()

    let fs (v : Vertex) =
        fragment {
            return helperWithMarker v.c
        }

    let glsl, _ = GLSL.compile [ Effect.ofFunction fs ]
    Assert.That(glsl.code, Does.Not.Contain "earlyFragmentTests")
    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction fs ] [ qualifierRx, 1 ]

[<Test>]
let ``Use in vertex shader fails``() =
    Setup.Run()

    let vs (v : Vertex) =
        vertex {
            earlyFragmentTests()
            return v
        }

    compileFails [ Effect.ofFunction vs; Effect.ofFunction plain ] "only be requested in fragment shaders"
