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
        "^readonly buffer (cs_)?[fF]loatInputBuffer {"
        "^writeonly buffer (cs_)?[fF]loatOutputBuffer {"
        "^buffer (cs_)?[fF]loatScratchBuffer {"
        "^readonly buffer (cs_)?[vV]3fInputBuffer {"
        "^writeonly buffer (cs_)?[vV]3fOutputBuffer {"
        "^buffer (cs_)?[vV]3fScratchBuffer {"
        "^readonly buffer (cs_)?[sS]tructInputBuffer {"
        "^writeonly buffer (cs_)?[sS]tructOutputBuffer {"
        "^buffer (cs_)?[sS]tructScratchBuffer {"
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
        "^buffer (cs_)?[iI]ntScratchBuffer {"
        "^buffer (cs_)?[vV]3iScratchBuffer {"
        "^buffer (cs_)?[sS]tructScratchBuffer {"
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
        "^readonly buffer (cs_)?[iI]ntInputBuffer {"
        "^writeonly buffer (cs_)?[vV]3iOutputBuffer {"
        "^buffer (cs_)?[sS]tructScratchBuffer {"
        "doSomething"
        "doSomethingInner"
    ]

    GLSL.shouldCompileAndContainRegex [Effect.ofFunction fs] expected
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i(128)) cs) expected