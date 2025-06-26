module DataTypes

open System
open Aardvark.Base
open FShade
open NUnit.Framework
open FsUnit
open FShade.Tests

type UniformScope with
    member x.Data8ui  : uint8[]   = x?StorageBuffer?Data1
    member x.Data8i   : int8[]    = x?StorageBuffer?Data2
    member x.Data16ui : uint16[]  = x?StorageBuffer?Data3
    member x.Data16i  : int16[]   = x?StorageBuffer?Data4
    member x.Data16f  : float16[] = x?StorageBuffer?Data5
    member x.Data64ui  : uint64[] = x?StorageBuffer?Data6
    member x.Data64i  : int64[]   = x?StorageBuffer?Data7
    member x.DataV3l  : V3l[]     = x?StorageBuffer?Data8
    member x.Array    : int[]     = x?StorageBuffer?Array
    member x.Arr : Arr<N<8>, int> = x?Arr

type Vertex =
    {
        Data8ui  : uint8
        Data8i   : int8
        Data16ui : uint16
        Data16i  : int16
        Data16f  : float16
        Data64ui : uint64
        Data64i  : int64
        DataV3l  : V3l
    }

[<Test>]
let ``Array index unsigned``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { return uniform.Array.Get(5) + uniform.Array.Get(uint v.Data16ui) }
    GLSL.shouldCompile [ Effect.ofFunction shader ]

[<Test>]
let ``Arr index unsigned``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { return uniform.Arr.Get(5) + uniform.Arr.Get(7u) }
    GLSL.shouldCompile [ Effect.ofFunction shader ]

[<Test>]
let ``Attributes 8ui``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { return v.Data8ui + v.Data8ui }
    GLSL.shouldCompileAndContainRegex' glslVulkan [ Effect.ofFunction shader ] ["uint8_t"]

[<Test>]
let ``Attributes 8i``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { return v.Data8i + v.Data8i }
    GLSL.shouldCompileAndContainRegex' glslVulkan [ Effect.ofFunction shader ] ["int8_t"]

[<Test>]
let ``Attributes 16ui``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { return v.Data16ui + v.Data16ui }
    GLSL.shouldCompileAndContainRegex' glslVulkan [ Effect.ofFunction shader ] ["uint16_t"]

[<Test>]
let ``Attributes 16i``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { return v.Data16i + v.Data16i }
    GLSL.shouldCompileAndContainRegex' glslVulkan [ Effect.ofFunction shader ] ["int16_t"]

[<Test>]
let ``Attributes 16f``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { return v.Data16f + v.Data16f }
    GLSL.shouldCompileAndContainRegex' glslVulkan [ Effect.ofFunction shader ] ["float16_t"]

[<Test>]
let ``Attributes 64ui``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { return uint (v.Data64ui + v.Data64ui) }
    GLSL.shouldCompileAndContainRegex' glslVulkan [ Effect.ofFunction shader ] ["uint64_t"]

[<Test>]
let ``Attributes 64i``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { return int (v.Data64i + v.DataV3l.Z) }
    GLSL.shouldCompileAndContainRegex' glslVulkan [ Effect.ofFunction shader ] ["int64_t"; "i64vec3"]

[<Test>]
let ``Storage buffers 8ui``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { uniform.Data8ui.[1] <- 42uy; return uint uniform.Data8ui.[0] }
    GLSL.shouldCompileAndContainRegex' glsl430 [ Effect.ofFunction shader ] ["uint8_t"]

[<Test>]
let ``Storage buffers 8i``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { uniform.Data8i.[1] <- 42y; return int uniform.Data8i.[0] }
    GLSL.shouldCompileAndContainRegex' glsl430 [ Effect.ofFunction shader ] ["int8_t"]

[<Test>]
let ``Storage buffers 16ui``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { uniform.Data16ui.[1] <- 42us; return uint uniform.Data16ui.[0] }
    GLSL.shouldCompileAndContainRegex' glsl430 [ Effect.ofFunction shader ] ["uint16_t"]

[<Test>]
let ``Storage buffers 16i``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { uniform.Data16i.[1] <- 42s; return int uniform.Data16i.[0] }
    GLSL.shouldCompileAndContainRegex' glsl430 [ Effect.ofFunction shader ] ["int16_t"]

[<Test>]
let ``Storage buffers 16f``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { uniform.Data16f.[1] <- float16 42.0f; return float32 uniform.Data16f.[0] }
    GLSL.shouldCompileAndContainRegex' glsl430 [ Effect.ofFunction shader ] ["float16_t"]

[<Test>]
let ``Storage buffers 64ui``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { uniform.Data64ui.[1] <- 42UL; return uint uniform.Data64ui.[0] }
    GLSL.shouldCompileAndContainRegex' glsl430 [ Effect.ofFunction shader ] ["uint64_t"]

[<Test>]
let ``Storage buffers 64i``() =
    Setup.Run()
    let shader (v: Vertex) = fragment { uniform.Data64i.[1] <- 42L; uniform.DataV3l.[1] <- V3l(42L); return int (uniform.Data64i.[0] + uniform.DataV3l.[0].X) }
    GLSL.shouldCompileAndContainRegex' glsl430 [ Effect.ofFunction shader ] ["int64_t"; "i64vec3"]

