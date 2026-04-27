module Effect

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open FsUnit
open NUnit.Framework
open Aardvark.Base
open Aardvark.Base.Monads.State

open FShade
open FShade.Imperative

#nowarn "4321"

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] color : V4f
    }

let shader0 (v : Vertex) =
    vertex {
        return { v with pos = V4f.IIII + v.pos }
    }

let shader1 (offset : V4f) (v : Vertex) =
    vertex {
        return { v with pos = offset + v.pos }
    }

let shader3 (a : V4f) (b : V4f) (v : Vertex) =
    vertex {
        return { v with pos = a + b * v.pos }
    }
    
let setup() =
    Effect.clearCaches()

[<Test>]
let ``[OfFunction] static``() =
    setup()
    let e0 = Effect.ofFunction shader0
    let e1 = Effect.ofFunction shader0
    e0 |> should equal e1

[<Test>]
let ``[OfFunction] static with closure``() =
    setup()
    let e0 = Effect.ofFunction (shader1 V4f.OIOI)
    let e1 = Effect.ofFunction (shader1 V4f.OIOI)
    e0 |> should equal e1
    let e2 = Effect.ofFunction (shader1 V4f.IOIO)
    e2 |> should not' (equal e1)

[<Test>]
let ``[OfFunction] local``() =
    setup()
    let shader2 (v : Vertex) =
        vertex {
            return { v with pos = V4f.IIII }
        } 
    let e0 = Effect.ofFunction shader2
    let e1 = Effect.ofFunction shader2
    e0 |> should equal e1

[<Test>]
let ``[OfFunction] local with closure value``() =
    setup()
    let aaaa = 2.0f
    let shader213 (p : V4f) (v : Vertex) =
        vertex {
            return {  pos = aaaa * p; color = v.color }
        } 

    let e0 = Effect.ofFunction (shader213 V4f.OIOI)
    let e1 = Effect.ofFunction (shader213 V4f.OIOI)
    e0 |> should equal e1
    let e2 = Effect.ofFunction (shader213 V4f.IOIO)
    e2 |> should not' (equal e1)

[<Test>] 
let ``[OfFunction] static curried closure``() =
    setup()
    let t0 = shader3 V4f.Zero
    let t1 = t0 V4f.IIII

    let e0 = Effect.ofFunction t1
    let e1 = Effect.ofFunction (fun a -> shader3 V4f.Zero V4f.IIII a)
    let e2 = Effect.ofFunction (fun a -> t0 V4f.IIII a)
    e0 |> should equal e1
    e0 |> should equal e2
    let e3 = Effect.ofFunction (fun a -> shader3 V4f.IIII V4f.Zero a)
    e3 |> should not' (equal e0)

[<Test>] 
let ``[OfFunction] local curried closure``() =
    setup()
    let shader2 (a : V4f) (b : V4f) (v : Vertex) =
        vertex {
            return { v with pos = a + b + v.pos }
        }


    let t0 = shader2 V4f.Zero
    let t1 = t0 V4f.IIII

    let e0 = Effect.ofFunction t1
    let e1 = Effect.ofFunction (fun a -> shader2 V4f.Zero V4f.IIII a)
    let e2 = Effect.ofFunction (fun a -> t0 V4f.IIII a)
    e0 |> should equal e1
    e0 |> should equal e2
    let e3 = Effect.ofFunction (fun a -> shader2 V4f.IIII V4f.Zero a)
    e3 |> should not' (equal e0)




[<Test>]
let ``[Compose] associativity``() =
    setup()
    let a = Effect.ofFunction shader0
    let b = Effect.ofFunction (shader1 V4f.IIII)
    let c = Effect.ofFunction (shader3 V4f.IIII V4f.IIII)

    let r = Effect.compose [ a; Effect.compose [ b; c ] ]
    let l = Effect.compose [ Effect.compose [ a; b ]; c ]

    l |> should equal r

[<Test>]
let ``[Compose] neutral element``() =
    setup()
    let z = Effect.empty
    let a = Effect.ofFunction shader0

    Effect.compose [ z; a ] |> should equal a
    Effect.compose [ a; z ] |> should equal a

[<Test>]
let ``[Compose] caching``() =
    setup()
    let a = Effect.ofFunction shader0
    let b = Effect.ofFunction (shader1 V4f.IIII)
    let c = Effect.ofFunction (shader3 V4f.IIII V4f.IIII)

    let e = Effect.compose [ a; b; c ]
    Effect.compose [ a; b; c ] |> should equal e


// ----------------------------------------------------------------------------
// Effect.Dependencies — per-output input/uniform tracking
// ----------------------------------------------------------------------------

type FragIn =
    {
        [<Color>] color : V4f
        [<Semantic("PickPartIndex")>] pi : int
    }

type FragOut =
    {
        [<Color>] color : V4f
        [<Semantic("PickId")>] pid : V4f
    }

let pickFrag (v : FragIn) =
    fragment {
        return { color = v.color; pid = V4f(float32 v.pi, 0.0f, 0.0f, 1.0f) }
    }

[<Test>]
let ``[Deps] leaf effect carries non-empty Dependencies``() =
    setup()
    let e = Effect.ofFunction shader0
    let deps = e.Dependencies
    // shader0 is a vertex shader: vertex output (pos) depends on input (pos).
    deps.Primitive |> Map.containsKey "Positions" |> should equal true
    let posDep = Map.find "Positions" deps.Primitive
    posDep.Inputs |> Map.containsKey "Positions" |> should equal true

[<Test>]
let ``[Deps] fragment effect tracks pi as input``() =
    setup()
    let e = Effect.ofFunction pickFrag
    let deps = e.Dependencies
    let pidDep = Map.find "PickId" deps.Fragment
    pidDep.Inputs |> Map.containsKey "PickPartIndex" |> should equal true

[<Test>]
let ``[Deps] resolveTop on fragment-only effect surfaces inputs``() =
    setup()
    let e = Effect.ofFunction pickFrag
    let resolved = EffectDeps.resolveTop e.Dependencies
    let pidDep = Map.find "PickId" resolved
    // No primitive stage → the fragment input passes through to the resolved view as-is.
    pidDep.Inputs |> Map.containsKey "PickPartIndex" |> should equal true

[<Test>]
let ``[Deps] composeStage substitutes upstream outputs``() =
    setup()
    // Manually craft two stage maps: l produces "X" depending on "A"; r reads
    // "X" and produces "Y". Composed: Y depends on A.
    let l : Map<string, OutputDeps> =
        Map.ofList [
            "X", ({ Inputs = Map.ofList ["A", typeof<float32>]; Uniforms = Map.empty } : OutputDeps)
        ]
    let r : Map<string, OutputDeps> =
        Map.ofList [
            "Y", ({ Inputs = Map.ofList ["X", typeof<float32>]; Uniforms = Map.empty } : OutputDeps)
        ]
    let composed = EffectDeps.composeStage l r
    let yDep = Map.find "Y" composed
    yDep.Inputs |> Map.containsKey "A" |> should equal true
    yDep.Inputs |> Map.containsKey "X" |> should equal false  // X was substituted away
    // X (l's output that r doesn't replace) passes through.
    composed |> Map.containsKey "X" |> should equal true

[<Test>]
let ``[Deps] composeStage carries uniforms across the join``() =
    setup()
    let l : Map<string, OutputDeps> =
        Map.ofList [
            "X", ({ Inputs = Map.ofList ["A", typeof<float32>]
                    Uniforms = Map.ofList ["UniformOnLeft", typeof<float32>] } : OutputDeps)
        ]
    let r : Map<string, OutputDeps> =
        Map.ofList [
            "Y", ({ Inputs = Map.ofList ["X", typeof<float32>]
                    Uniforms = Map.ofList ["UniformOnRight", typeof<float32>] } : OutputDeps)
        ]
    let composed = EffectDeps.composeStage l r
    let yDep = Map.find "Y" composed
    yDep.Uniforms |> Map.containsKey "UniformOnLeft" |> should equal true
    yDep.Uniforms |> Map.containsKey "UniformOnRight" |> should equal true

[<Test>]
let ``[Deps] Effect.compose composes deps without forcing shaders``() =
    setup()
    let a = Effect.ofFunction shader0
    let b = Effect.ofFunction shader0
    let composed = Effect.compose [ a; b ]
    // Both leaf and composed effects should expose Dependencies; the composed
    // map is derived from the inputs' maps without re-analyzing shaders.
    composed.Dependencies.Primitive |> Map.isEmpty |> should equal false

[<Test>]
let ``[Deps] serialization round-trip preserves Dependencies``() =
    setup()
    let original = Effect.ofFunction pickFrag
    let blob = Effect.pickle original
    use ms = new System.IO.MemoryStream(blob)
    let loaded = Effect.deserialize ms
    loaded.Id |> should equal original.Id
    let pidOriginal = Map.find "PickId" original.Dependencies.Fragment
    let pidLoaded   = Map.find "PickId" loaded.Dependencies.Fragment
    pidLoaded.Inputs |> Map.containsKey "PickPartIndex" |> should equal true
    Set.ofSeq (Map.keys pidLoaded.Inputs) |> should equal (Set.ofSeq (Map.keys pidOriginal.Inputs))

[<Test>]
let ``[Deps] read accesses Dependencies without forcing shaders``() =
    setup()
    let original = Effect.ofFunction pickFrag
    let blob = Effect.pickle original
    let loaded = Effect.read blob
    // Touch Dependencies but never Shaders; should not throw and returns the right map.
    let pidDep = Map.find "PickId" loaded.Dependencies.Fragment
    pidDep.Inputs |> Map.containsKey "PickPartIndex" |> should equal true

// ----------------------------------------------------------------------------
// [Deps] cross-check against actual Effect.toModule linking
// ----------------------------------------------------------------------------
//
// The deps map promises that for any subset S of fragment outputs, the union
// of `Dependencies.Fragment[o].Inputs` over o ∈ S — once resolved through the
// Primitive map — equals what FShade's linker would actually demand as the
// vertex shader's required inputs if you asked it to keep just S.
//
// These tests exercise that invariant by building an effect with multiple
// distinct outputs that read different combinations of vertex attributes,
// then linking with each subset and comparing to the deps-map prediction.

type GbufVertex =
    {
        [<Position>] pos : V4f
        [<Semantic("Normals")>] n : V3f
        [<Color>] c : V4f
        [<Semantic("DiffuseColorCoordinates")>] tc : V2f
        [<Semantic("MyExtra")>] extra : float32
    }

type GbufFrag =
    {
        [<Color>] color : V4f
        [<Semantic("NormalOut")>] normalOut : V4f
        [<Semantic("TexCoordOut")>] tcOut : V2f
        [<Semantic("ExtraOut")>] extraOut : float32
    }

let gbufferShader (v : GbufVertex) =
    fragment {
        return {
            color    = v.c                                            // depends on Colors (only)
            normalOut = V4f(v.n.X, v.n.Y, v.n.Z, 1.0f)                // depends on Normals (only)
            tcOut    = v.tc                                           // depends on DiffuseColorCoordinates (only)
            extraOut = v.extra                                        // depends on MyExtra (only)
        }
    }

/// Get the linked vertex shader's REQUIRED inputs (paramSemantic set), with
/// builtins like gl_Position filtered out, when only the given outputs are
/// requested via EffectConfig.
let private linkedVertexInputs (effect : Effect) (desiredOutputs : list<string * Type>) =
    let config =
        EffectConfig.ofList (desiredOutputs |> List.mapi (fun i (n, t) -> n, t, i))
    let m = Effect.toModule config effect
    let vertexEntry =
        m.Entries
        |> List.tryFind (fun e ->
            e.decorations
            |> List.exists (function
                | EntryDecoration.Stages (ShaderStageDescription.Graphics g) when g.self = ShaderStage.Vertex -> true
                | _ -> false))
    match vertexEntry with
    | Some ep ->
        ep.inputs
        |> List.map (fun p -> p.paramSemantic)
        |> List.filter (fun s ->
            // Filter built-ins that aren't real vertex attributes.
            not (s.StartsWith "gl_") &&
            s <> Intrinsics.VertexId &&
            s <> Intrinsics.InstanceId)
        |> Set.ofList
    | None ->
        Set.empty

/// Predict required vertex inputs from the deps map for a subset of outputs.
let private predictedVertexInputs (effect : Effect) (outputNames : seq<string>) =
    let resolved = EffectDeps.resolveTop effect.Dependencies
    outputNames
    |> Seq.collect (fun o ->
        match Map.tryFind o resolved with
        | Some d -> Map.toSeq d.Inputs |> Seq.map fst
        | None   -> Seq.empty)
    |> Set.ofSeq

[<Test>]
let ``[Deps] linker matches deps for single-output requests``() =
    setup()
    let effect = Effect.ofFunction gbufferShader

    // Each fragment output reads exactly one vertex semantic. Verify the deps
    // map's per-output Inputs match what the linker actually demands.
    let cases = [
        "Colors",      typeof<V4f>,    "Colors"
        "NormalOut",   typeof<V4f>,    "Normals"
        "TexCoordOut", typeof<V2f>,    "DiffuseColorCoordinates"
        "ExtraOut",    typeof<float32>,"MyExtra"
    ]

    for outputName, outputType, expectedInput in cases do
        let predicted = predictedVertexInputs effect [outputName]
        let actual    = linkedVertexInputs effect [outputName, outputType]
        // Equality, not just subset. The deps map should be exact.
        actual
        |> should equal predicted
        // And both should contain the one expected input semantic.
        actual.Contains expectedInput |> should equal true

[<Test>]
let ``[Deps] linker matches deps for multi-output subsets``() =
    setup()
    let effect = Effect.ofFunction gbufferShader

    // All non-empty subsets of the 4 outputs (15 total).
    let allOutputs = [
        "Colors",      typeof<V4f>
        "NormalOut",   typeof<V4f>
        "TexCoordOut", typeof<V2f>
        "ExtraOut",    typeof<float32>
    ]
    let rec subsets l =
        match l with
        | [] -> [[]]
        | h :: t ->
            let rest = subsets t
            rest @ (rest |> List.map (fun s -> h :: s))
    let nonempty =
        subsets allOutputs
        |> List.filter (fun s -> not (List.isEmpty s))

    for subset in nonempty do
        let names = subset |> List.map fst
        let predicted = predictedVertexInputs effect names
        let actual    = linkedVertexInputs effect subset
        if predicted <> actual then
            failwithf
                "subset %A: deps predicted %A, linker demanded %A (diff: predicted-only=%A, linker-only=%A)"
                names predicted actual
                (Set.difference predicted actual)
                (Set.difference actual predicted)

[<Test>]
let ``[Deps] composed effect: prediction matches linker``() =
    setup()
    // Two effects composed: the leaf gbufferShader (fragment) plus a vertex
    // shader that overrides Position. The deps for "Colors" output should
    // still be {Colors} on the vertex side, since the vertex shader doesn't
    // touch Color. Composing through compose2 should preserve that invariant.
    let extra =
        Effect.ofFunction (fun (v : GbufVertex) ->
            vertex {
                return { v with pos = v.pos + V4f.IIII }
            })
    let composed = Effect.compose [extra; Effect.ofFunction gbufferShader]
    let predicted = predictedVertexInputs composed ["Colors"]
    let actual    = linkedVertexInputs composed [("Colors", typeof<V4f>)]
    actual |> should equal predicted
    actual.Contains "Colors" |> should equal true
