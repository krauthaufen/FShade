module HeapSpike

// Phase-0 spike for the Vulkan "heap" render path.
//
// Goal: prove that an *unmodified* user effect (which reads its uniforms
// by name in the normal way) can be mechanically rewritten so that every
// uniform read instead gathers its value out of a shared storage-buffer
// "arena", indexed by the per-draw slot id (gl_InstanceIndex, routed via
// firstInstance). No user shader changes; the rewrite is a pure
// Effect -> Effect transform built from FShade's public substitution API.
//
// If this compiles through glslang, the linchpin of the heap design (§3
// of the plan) is validated on the real FShade pipeline.

open Aardvark.Base
open FShade
open FShade.Imperative
open NUnit.Framework
open FShade.Tests
open Microsoft.FSharp.Quotations

// ── The arena, as FShade storage buffers ────────────────────────────────
// One u32/i32 "headers" buffer (per-draw offsets) + one f32 "data" buffer
// (the packed uniform values). Real impl aliases several typed views over
// one VkBuffer; the spike uses two buffers for clarity.
type UniformScope with
    member x.HeapHeaders : int[]     = uniform?StorageBuffer?HeapHeaders
    member x.HeapData    : float32[] = uniform?StorageBuffer?HeapData

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>]    c   : V4f
    }

// ── A perfectly ordinary effect — reads two uniforms by name ────────────
let normalVertex (v : Vertex) =
    vertex {
        let mvp : M44f = uniform?ModelViewProjTrafo
        let col : V4f  = uniform?Color
        return { pos = mvp * v.pos; c = col }
    }

let normalFragment (v : Vertex) =
    fragment {
        return v.c
    }

// ── The rewrite ─────────────────────────────────────────────────────────
// Per-bucket layout: each uniform name maps to a field slot in the header
// row. HeapHeaders.[drawSlot * stride + fieldIdx] = element offset into
// HeapData where that uniform's value starts.
let private cint (v : int) : Expr<int> = Expr.Value v |> Expr.Cast

let buildHeapScene () =
    let stride = 2
    let layout = Map.ofList [ "ModelViewProjTrafo", 0; "Color", 1 ]

    // slot id = gl_InstanceIndex (firstInstance routing). Read as a raw
    // input builtin so we don't have to touch the user's vertex record.
    let iid : Expr<int> = Expr.ReadInput<int>(ParameterKind.Input, Intrinsics.InstanceId)

    let substitute (name : string) (typ : System.Type) (_idx : Expr option) (_slot : ShaderSlot option) : Expr option =
        match Map.tryFind name layout with
        | None -> None
        | Some fieldIdx ->
            let fi = cint fieldIdx
            let st = cint stride
            // base element offset of this uniform's value in HeapData
            let off : Expr<int> = <@ uniform.HeapHeaders.[ %iid * %st + %fi ] @>
            if typ = typeof<M44f> then
                let e =
                    <@
                        let o = %off
                        M44f(uniform.HeapData.[o+0],  uniform.HeapData.[o+1],  uniform.HeapData.[o+2],  uniform.HeapData.[o+3],
                             uniform.HeapData.[o+4],  uniform.HeapData.[o+5],  uniform.HeapData.[o+6],  uniform.HeapData.[o+7],
                             uniform.HeapData.[o+8],  uniform.HeapData.[o+9],  uniform.HeapData.[o+10], uniform.HeapData.[o+11],
                             uniform.HeapData.[o+12], uniform.HeapData.[o+13], uniform.HeapData.[o+14], uniform.HeapData.[o+15])
                    @>
                Some e.Raw
            elif typ = typeof<V4f> then
                let e =
                    <@
                        let o = %off
                        V4f(uniform.HeapData.[o+0], uniform.HeapData.[o+1], uniform.HeapData.[o+2], uniform.HeapData.[o+3])
                    @>
                Some e.Raw
            else
                None

    Effect.compose [ Effect.ofFunction normalVertex; Effect.ofFunction normalFragment ]
    |> Effect.substituteUniforms substitute

// FShade's GLSL backend emits `gl_InstanceID` for the InstanceId builtin
// regardless of backend; aardvark patches it to `gl_InstanceIndex` before
// glslang (Aardvark.Rendering.Vulkan ShaderProgram.fs). We mirror that
// patch here so the spike validates the exact source aardvark feeds to
// glslang. (A cleaner long-term fix is to teach the glslVulkan backend to
// emit gl_InstanceIndex directly — a candidate FShade heap-branch change.)
let private aardvarkVulkanPatch (code : string) =
    code.Replace("gl_InstanceID", "gl_InstanceIndex").Replace("gl_VertexID", "gl_VertexIndex")

[<Test>]
let ``Heap rewrite: uniforms become arena gathers``() =
    Setup.Run()
    let heap = buildHeapScene ()
    let glsl, _ = GLSL.compile' glslVulkan [ heap ]
    let code = aardvarkVulkanPatch glsl.code

    // 1) the rewrite removed the UBOs and emitted arena gathers
    let mustContain (s : string) =
        if not (code.Contains s) then failwithf "generated GLSL missing %A:\n%s" s code
    mustContain "buffer HeapDataBuffer"
    mustContain "buffer HeapHeadersBuffer"
    mustContain "gl_InstanceIndex"
    if code.Contains "uniform " then failwithf "expected no UBO uniforms, got:\n%s" code

    // 2) the patched source actually compiles to SPIR-V (Vulkan) per stage
    for stage in [ ShaderStage.Vertex; ShaderStage.Fragment ] do
        match GLSL.glslang stage code with
        | Error e -> failwithf "glslang rejected %A stage: %s" stage e
        | _ -> ()

// ── Bindless: non-uniform descriptor indexing ───────────────────────────
// A DYNAMIC index into a sampler array must be wrapped in nonuniformEXT (+
// the GL_EXT_nonuniform_qualifier extension); a CONSTANT index must not.
type TexVertex =
    { [<Position>]                                                pos : V4f
      [<Semantic("Tc")>]                                         tc  : V2f
      [<Semantic("TexId"); Interpolation(InterpolationMode.Flat)>] tid : int }

let private bindlessTextures =
    sampler2d {
        textureArray uniform?Textures 16
        filter Filter.MinMagMipLinear
    }

[<Test>]
let ``Bindless: dynamic sampler-array index uses nonuniformEXT``() =
    Setup.Run()
    let frag (v : TexVertex) = fragment { return bindlessTextures.[v.tid].Sample(v.tc) }   // dynamic index
    let glsl, _ = GLSL.compile' glslVulkan [ Effect.ofFunction frag ]
    let code = glsl.code
    if not (code.Contains "nonuniformEXT") then failwithf "expected nonuniformEXT for dynamic sampler index:\n%s" code
    if not (code.Contains "GL_EXT_nonuniform_qualifier") then failwithf "expected the extension:\n%s" code
    match GLSL.glslang ShaderStage.Fragment code with
    | Error e -> failwithf "glslang rejected bindless fragment: %s" e
    | _ -> ()

[<Test>]
let ``Bindless: constant sampler-array index stays plain``() =
    Setup.Run()
    let frag (v : TexVertex) = fragment { return bindlessTextures.[2].Sample(v.tc) }        // constant index
    let glsl, _ = GLSL.compile' glslVulkan [ Effect.ofFunction frag ]
    if glsl.code.Contains "nonuniformEXT" then failwithf "constant index must NOT use nonuniformEXT:\n%s" glsl.code
