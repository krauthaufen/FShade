module MemoryOrdering

// Values read from mutable memory (refs, storage buffers, images, shared memory) must not be
// inlined past a write, call or barrier that can change the memory they were read from.

open Aardvark.Base
open FShade
open FShade.Tests
open NUnit.Framework

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
    }

type UniformScope with
    member x.Data : int[] = x?StorageBuffer?Data
    member x.Img : IntImage2d<Formats.r32i> = x?Img

[<ReflectedDefinition>]
let incAndRet (x : int ref) =
    x.Value <- x.Value + 1
    x.Value

[<ReflectedDefinition>]
let getAndInc (var : int ref) =
    let value = var.Value
    incAndRet var |> ignore
    value

[<ReflectedDefinition>]
let incTwice (var : int ref) =
    incAndRet var |> ignore
    incAndRet var |> ignore

[<ReflectedDefinition>]
let getAndIncNested (var : int ref) =
    let value = var.Value
    incTwice var
    value

[<ReflectedDefinition>]
let getAndSet (var : int ref) =
    let old = var.Value
    var.Value <- 5
    old

[<ReflectedDefinition>]
let readThenInc (x : int) =
    let b = x
    incAndRet &&x |> ignore
    b + x

[<ReflectedDefinition>]
let getPlusOne (var : int ref) =
    let value = var.Value
    value + 1

let private helper = @"[A-Za-z0-9_]+"

[<Test>]
let ``Ref read before mutating call``() =
    Setup.Run()

    let frag () =
        fragment {
            let v = 0
            return getAndInc &&v
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        @"int value = var;\s*" + helper + @"_incAndRet_" + helper + @"\(var\);\s*return value;"
    ]

[<Test>]
let ``Ref read before nested mutating call``() =
    Setup.Run()

    let frag () =
        fragment {
            let v = 0
            return getAndIncNested &&v
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        @"int value = var;\s*" + helper + @"_incTwice_" + helper + @"\(var\);\s*return value;"
    ]

[<Test>]
let ``Ref read before set``() =
    Setup.Run()

    let frag () =
        fragment {
            let v = 0
            return getAndSet &&v
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        @"int old = var;\s*var = 5;\s*return old;"
    ]

[<Test>]
let ``Storage buffer read before write``() =
    Setup.Run()

    let frag () =
        fragment {
            let a = uniform.Data.[0]
            uniform.Data.[0] <- a + 1
            return V4f(float32 a)
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        @"int a = Data\[0\];\s*Data\[0\] = \(a \+ 1\);"
    ]

[<Test>]
let ``Storage buffer read before atomic``() =
    Setup.Run()

    let frag () =
        fragment {
            let old = uniform.Data.[0]
            Atomic.Add(&&uniform.Data.[0], 1) |> ignore
            return V4f(float32 old)
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        @"int old = Data\[0\];\s*atomicAdd\(Data\[0\], 1\);"
    ]

[<Test>]
let ``Image load before store``() =
    Setup.Run()

    let frag () =
        fragment {
            let c = uniform.Img.[V2i.Zero]
            uniform.Img.[V2i.Zero] <- c + V4i.IIII
            return V4f c
        }

    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        @"ivec4 c = imageLoad\(Img, [^;]*;\s*imageStore\(Img, "
    ]

[<Test>]
let ``Shared memory read before barrier``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let mem : int[] = allocateShared 64
            let id = getLocalId().X
            mem.[id] <- data.[id]
            barrier()
            let x = mem.[63 - id]
            barrier()
            mem.[id] <- x
            barrier()
            data.[id] <- mem.[id]
        }

    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i 64) cs) [
        @"int x = " + helper + @"\[\(63 - id\)\];\s*barrier\(\);"
    ]

[<Test>]
let ``Parameter read before write through its address``() =
    Setup.Run()

    let frag () =
        fragment {
            return readThenInc 1
        }

    // the parameter stays by-value (no inout); the read of b must happen before the call
    GLSL.shouldCompileAndContainRegex [ Effect.ofFunction frag ] [
        @"int " + helper + @"_readThenInc_" + helper + @"\(int x\)"
        @"int b = " + helper + @";\s*" + helper + @"_incAndRet_" + helper + @"\(" + helper + @"\);\s*return \(b \+ " + helper + @"\);"
    ]

[<Test>]
let ``Ref read without intervening mutation is still inlined``() =
    Setup.Run()

    let frag () =
        fragment {
            let v = 0
            return getPlusOne &&v
        }

    // a single use of a value read through a ref that is not passed on is inlined
    GLSL.shouldCompileAndContainRegexWithCount [ Effect.ofFunction frag ] [
        @"int value = ", 0
        @"return \(var \+ 1\);", 1
    ]
