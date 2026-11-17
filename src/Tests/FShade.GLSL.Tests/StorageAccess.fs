module StorageAccess

open Aardvark.Base
open FShade
open NUnit.Framework
open FShade.Tests

type Data =
    struct
        val mutable Foo : V3f
        val mutable Bar : V3f
        val mutable Huh : V3i
    end

type UniformScope with
    member _.StructInput : Data[] = uniform?StorageBuffer?StructInput
    member _.StructOutput : Data[] = uniform?StorageBuffer?StructOutput
    member _.StructScratch : Data[] = uniform?StorageBuffer?StructScratch
    member _.StructScratchArr : Arr<N<4>, Data> = uniform?StorageBuffer?StructScratch
    member _.FloatInput : float32[] = uniform?StorageBuffer?FloatInput
    member _.FloatOutput : float32[] = uniform?StorageBuffer?FloatOutput
    member _.FloatScratch : float32[] = uniform?StorageBuffer?FloatScratch
    member _.V3fInput : V3f[] = uniform?StorageBuffer?V3fInput
    member _.V3fOutput : V3f[] = uniform?StorageBuffer?V3fOutput
    member _.V3fScratch : V3f[] = uniform?StorageBuffer?V3fScratch
    member _.IntInput : int[] = uniform?StorageBuffer?IntInput
    member _.IntInputArr : Arr<N<4>, int> = uniform?StorageBuffer?IntInput
    member _.IntOutput : int[] = uniform?StorageBuffer?IntOutput
    member _.IntScratch : int[] = uniform?StorageBuffer?IntScratch
    member _.V3iInput : V3i[] = uniform?StorageBuffer?V3iInput
    member _.V3iOutput : V3i[] = uniform?StorageBuffer?V3iOutput
    member _.V3iScratch : V3i[] = uniform?StorageBuffer?V3iScratch

[<Test>]
let ``Storage buffer read and write``() =
    Setup.Run()

    let fs () =
        fragment {
            uniform.FloatOutput.[0] <- 0.0f
            uniform.FloatScratch.[0] <- 0.0f
            uniform.V3fOutput.[0].X <- 0.0f
            uniform.V3fScratch.[1].Y <- 0.0f
            uniform.StructOutput.[0].Foo.X <- 0.0f
            uniform.StructScratch.[0].Bar.Z <- 0.0f
            let a = uniform.FloatInput.[0] + uniform.FloatScratch.[1]
            let b = uniform.V3fInput.[0].Y + uniform.V3fScratch.[2].Z
            let c = uniform.StructInput.[0].Foo.Y + uniform.StructScratch.[2].Bar.X
            return a + b + c
        }

    let cs (floatScratch: float32[]) (floatInput: float32[]) (floatOutput: float32[])
           (v3fScratch: V3f[]) (v3fInput: V3f[]) (v3fOutput: V3f[])
           (structScratch: Data[]) (structInput: Data[]) (structOutput: Data[]) =
        compute {
            let id = getGlobalId().X
            floatScratch.[id] <- floatInput.[id]
            floatOutput.[id] <- floatInput.[id] + floatScratch.[id]
            v3fScratch.[id].Y <- v3fInput.[id].Y
            v3fOutput.[id].X <- v3fInput.[id].Y + v3fScratch.[id].Z
            structScratch.[id].Foo.X <- structInput.[id].Bar.Y
            structOutput.[id].Foo.X <- structInput.[id].Bar.Y + structScratch.[id].Bar.Z
        }

    let expected = [
        "^readonly buffer [fF]loatInputBuffer {"
        "^writeonly buffer [fF]loatOutputBuffer {"
        "^buffer [fF]loatScratchBuffer {"
        "^readonly buffer [vV]3fInputBuffer {"
        "^writeonly buffer [vV]3fOutputBuffer {"
        "^buffer [vV]3fScratchBuffer {"
        "^readonly buffer [sS]tructInputBuffer {"
        "^writeonly buffer [sS]tructOutputBuffer {"
        "^buffer [sS]tructScratchBuffer {"
    ]

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction fs] expected
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i(128)) cs) expected

[<Test>]
let ``Storage buffer as by-ref argument of intrinsic``() =
    Setup.Run()

    let fs () =
        fragment {
            let unused = Atomic.Add(&&uniform.IntScratch.[0], 1)
            let unused = Atomic.Add(&&uniform.V3iScratch.[0].X, 1)
            let unused = Atomic.Add(&&uniform.StructScratch.[0].Huh.X, 1)
            return V3f.Zero
        }

    let cs (intScratch: int[]) (v3iScratch: V3i[]) (structScratch: Data[]) =
        compute {
            let id = getGlobalId().X
            let unused = Atomic.Add(&&intScratch.[0], 1)
            let unused = Atomic.Add(&&v3iScratch.[0].X, 1)
            let unused = Atomic.Add(&&structScratch.[0].Huh.Y, 1)
            ()
        }

    let expected = [
        "^buffer [iI]ntScratchBuffer {"
        "^buffer [vV]3iScratchBuffer {"
        "^buffer [sS]tructScratchBuffer {"
        "atomicAdd"
    ]

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction fs] expected
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i(128)) cs) expected

[<ReflectedDefinition>]
let doSomethingInner (input: Arr<N<4>, int>) (scratch: int ref) =
    scratch.Value <- input.[0]
    uniform.V3iOutput.[0].X <- input.[1] + scratch.Value

[<ReflectedDefinition>]
let doSomething (input: Arr<N<4>, int>) (scratch: int ref) =
    doSomethingInner input scratch

[<Test>]
let ``Storage buffer read and write in utility function``() =
    Setup.Run()

    let fs () =
        fragment {
            doSomething uniform.IntInputArr &&uniform.StructScratchArr.[0].Huh.X
            return V3f.Zero
        }

    let cs (structScratch: Data[]) =
        compute {
            doSomething uniform.IntInputArr &&structScratch.[0].Huh.X
        }

    let expected = [
        "^readonly buffer [iI]ntInputBuffer {"
        "^writeonly buffer [vV]3iOutputBuffer {"
        "^buffer [sS]tructScratchBuffer {"
        "doSomething"
        "doSomethingInner"
    ]

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction fs] expected
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i(128)) cs) expected

type Vertex =
    {
        [<Position>] pos : V4f
    }

[<Test>]
let ``Storage buffer access is merged when composing fragment shaders``() =
    Setup.Run()

    let writer (v : Vertex) =
        fragment {
            uniform.FloatScratch.[0] <- 1.0f
            return V3f.Zero
        }

    let reader (v : Vertex) =
        fragment {
            return V3f(uniform.FloatScratch.[1])
        }

    // writeonly in one shader, readonly in the other: the composed shader must declare the buffer read-write
    let expected = [ "^buffer [fF]loatScratchBuffer {" ]

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction writer; Effect.ofFunction reader ] expected
    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction reader; Effect.ofFunction writer ] expected

[<Test>]
let ``Storage buffer access is merged across stages``() =
    Setup.Run()

    let vs (v : Vertex) =
        vertex {
            uniform.FloatScratch.[0] <- v.pos.X
            return v
        }

    let fs (v : Vertex) =
        fragment {
            return V3f(uniform.FloatScratch.[1])
        }

    // the declaration is shared by all stages of the program: vertex writes, fragment reads
    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction vs; Effect.ofFunction fs ] [
        "^buffer [fF]loatScratchBuffer {"
    ]

type UniformScope with
    member _.OutImg : IntImage2d<Formats.r32i> = uniform?OutImg

[<ReflectedDefinition>]
let writeOutput (value : int) =
    uniform.IntOutput.[0] <- value

[<ReflectedDefinition>]
let storeImage (value : int) =
    uniform.OutImg.[V2i.Zero] <- V4i(value)

[<Test>]
let ``Storage buffer write in utility function is kept``() =
    Setup.Run()

    let fs () =
        fragment {
            writeOutput 1
            return V3f.Zero
        }

    // the call has no result and no by-ref argument; the write alone must keep it alive
    GLSL.shouldCompileAndContainRegex [Effect.ofFunction fs] [
        "writeOutput"
        "^writeonly buffer [iI]ntOutputBuffer"
    ]

[<Test>]
let ``Image store in utility function is kept``() =
    Setup.Run()

    let fs () =
        fragment {
            storeImage 1
            return V3f.Zero
        }

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction fs] [
        "storeImage"
        "imageStore\(OutImg"
    ]
