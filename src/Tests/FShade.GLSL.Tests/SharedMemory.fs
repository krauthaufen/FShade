module SharedMemory

// Shared arrays are allocated with allocateShared and indexed; non-array shared variables are allocated with
// allocateSharedValue and accessed through the returned ref. A shared variable is identified by its F# name and
// type (and size), so re-declaring it in a utility function refers to the same variable.

open Aardvark.Base
open FShade
open FShade.Tests
open NUnit.Framework

[<ReflectedDefinition>]
let addTo (target : int ref) (value : int) =
    target.Value <- target.Value + value

[<ReflectedDefinition; Inline>]
let resetInline (target : int ref) =
    target.Value <- 0

[<ReflectedDefinition>]
let fillShared (value : int) =
    let mem : int[] = allocateShared 64
    mem.[getLocalIndex()] <- value

[<ReflectedDefinition>]
let fillSharedHalf (value : int) =
    let mem : int[] = allocateShared 32
    mem.[getLocalIndex()] <- value

[<ReflectedDefinition>]
let incShared () =
    let sum = allocateSharedValue<int>()
    Atomic.Add(sum, 1) |> ignore

let private helper = @"[A-Za-z0-9_]+"

[<Test>]
let ``Scalar shared variable``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let sum = allocateSharedValue<int>()
            let id = getLocalIndex()

            if id = 0 then sum.Value <- 0
            barrier()
            Atomic.Add(sum, data.[id]) |> ignore
            barrier()
            if id = 0 then data.[0] <- sum.Value
        }

    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i 64) cs) [
        @"^shared int " + helper + @";"
        helper + @" = 0;"
        @"atomicAdd\(" + helper + @", data\[id\]\);"
        @"data\[0\] = " + helper + @";"
    ]

[<Test>]
let ``Vector shared variable``() =
    Setup.Run()

    let cs (data : V3f[]) =
        compute {
            let sum = allocateSharedValue<V3f>()
            let id = getLocalIndex()

            if id = 0 then sum.Value <- V3f.Zero
            barrier()
            data.[id] <- sum.Value + data.[id]
        }

    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i 64) cs) [
        @"^shared vec3 " + helper + @";"
        helper + @" = vec3\("
        @"data\[id\] = \(" + helper + @" \+ data\[id\]\);"
    ]

[<Test>]
let ``Shared variable passed by reference``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let sum = allocateSharedValue<int>()
            let id = getLocalIndex()

            if id = 0 then resetInline sum
            barrier()
            addTo sum data.[id]
            barrier()
            if id = 0 then data.[0] <- sum.Value
        }

    // the inline function writes the shared variable directly, the other one receives it as inout parameter
    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i 64) cs) [
        @"^shared int " + helper + @";"
        helper + @" = 0;"
        @"void " + helper + @"_addTo_" + helper + @"\(inout int target, int value\)"
        @"_addTo_" + helper + @"\(" + helper + @", data\[id\]\);"
    ]

[<Test>]
let ``Shared variable read before barrier``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let sum = allocateSharedValue<int>()
            let id = getLocalIndex()

            if id = 0 then sum.Value <- data.[0]
            barrier()
            let x = sum.Value
            barrier()
            if id = 0 then sum.Value <- x + 1
            barrier()
            data.[id] <- sum.Value
        }

    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i 64) cs) [
        @"int x = " + helper + @";\s*barrier\(\);"
    ]

[<Test>]
let ``Shared variable must be bound with let``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            data.[0] <- allocateSharedValue<int>().Value
        }

    let exn = Assert.Throws(fun () -> GLSL.compileCompute (ComputeShader.ofFunction (V3i 64) cs) |> ignore)
    Assert.That(exn.Message, Does.Contain "allocateSharedValue")

[<Test>]
let ``Shared array``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let mem : int[] = allocateShared 64
            let id = getLocalIndex()

            mem.[id] <- data.[id]
            barrier()
            data.[id] <- mem.[63 - id]
        }

    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i 64) cs) [
        @"^shared int " + helper + @"\[64\];"
        helper + @"\[id\] = data\[id\];"
        @"data\[id\] = " + helper + @"\[\(63 - id\)\];"
    ]

[<Test>]
let ``Vector shared array``() =
    Setup.Run()

    let cs (data : V3f[]) =
        compute {
            let mem : V3f[] = allocateShared 16
            let id = getLocalIndex()

            mem.[id] <- data.[id]
            barrier()
            data.[id] <- mem.[id] + mem.[15 - id]
        }

    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i 16) cs) [
        @"^shared vec3 " + helper + @"\[16\];"
        @"data\[id\] = \(" + helper + @"\[id\] \+ " + helper + @"\[\(15 - id\)\]\);"
    ]

[<Test>]
let ``Shared array element passed by reference``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let mem : int[] = allocateShared 64
            let id = getLocalIndex()

            resetInline &&mem.[id]
            barrier()
            Atomic.Add(&&mem.[id], data.[id]) |> ignore
            addTo &&mem.[63 - id] 1
            barrier()
            data.[id] <- mem.[id]
        }

    GLSL.shouldCompileComputeAndContainRegex (ComputeShader.ofFunction (V3i 64) cs) [
        helper + @"\[id\] = 0;"
        @"atomicAdd\(" + helper + @"\[id\], data\[id\]\);"
        @"_addTo_" + helper + @"\(" + helper + @"\[\(63 - id\)\], 1\);"
    ]

[<Test>]
let ``Shared array must be bound with let``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            data.[0] <- (allocateShared 64 : int[]).[0]
        }

    let exn = Assert.Throws(fun () -> GLSL.compileCompute (ComputeShader.ofFunction (V3i 64) cs) |> ignore)
    Assert.That(exn.Message, Does.Contain "allocateShared")

[<Test>]
let ``Shared array size must be constant``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let mem : int[] = allocateShared data.[0]
            mem.[0] <- 1
        }

    let exn = Assert.Throws(fun () -> GLSL.compileCompute (ComputeShader.ofFunction (V3i 64) cs) |> ignore)
    Assert.That(exn.Message, Does.Contain "allocateShared")

[<Test>]
let ``Shared array cannot be used as value``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let mem : int[] = allocateShared 64
            let copy = mem
            data.[0] <- copy.[0]
        }

    let exn = Assert.Throws(fun () -> GLSL.compileCompute (ComputeShader.ofFunction (V3i 64) cs) |> ignore)
    Assert.That(exn.Message, Does.Contain "shared memory")

[<Test>]
let ``Shared array re-declared in utility function``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let mem : int[] = allocateShared 64
            let id = getLocalIndex()

            fillShared data.[id]
            barrier()
            data.[id] <- mem.[63 - id]
        }

    // same name, type and size: the utility function refers to the same shared array
    GLSL.shouldCompileComputeAndContainRegexWithCount (ComputeShader.ofFunction (V3i 64) cs) [
        @"^shared int ", 1
        @"^shared int " + helper + @"\[64\];", 1
    ]

[<Test>]
let ``Shared array with different size is a different variable``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let mem : int[] = allocateShared 64
            let id = getLocalIndex()

            fillSharedHalf data.[id]
            barrier()
            data.[id] <- mem.[63 - id]
        }

    GLSL.shouldCompileComputeAndContainRegexWithCount (ComputeShader.ofFunction (V3i 64) cs) [
        @"^shared int ", 2
        @"^shared int " + helper + @"\[64\];", 1
        @"^shared int " + helper + @"\[32\];", 1
    ]

[<Test>]
let ``Shared variable re-declared in utility function``() =
    Setup.Run()

    let cs (data : int[]) =
        compute {
            let sum = allocateSharedValue<int>()
            let id = getLocalIndex()

            if id = 0 then sum.Value <- 0
            barrier()
            incShared()
            barrier()
            if id = 0 then data.[0] <- sum.Value
        }

    GLSL.shouldCompileComputeAndContainRegexWithCount (ComputeShader.ofFunction (V3i 64) cs) [
        @"^shared int ", 1
        @"atomicAdd\(" + helper + @", 1\);", 1
    ]
