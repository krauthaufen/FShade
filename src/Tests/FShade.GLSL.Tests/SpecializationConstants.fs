module SpecializationConstants

// Members of the magic `SpecializationConstant` uniform scope become Vulkan specialization constants
// (`layout(constant_id = N) const T name = 0;`) with name-keyed ids; backends without the feature
// emit an ordinary uniform buffer.

open Aardvark.Base
open FSharp.Data.Adaptive
open FShade
open FShade.GLSL
open FShade.Tests
open NUnit.Framework
open FsUnit

type Vertex =
    {
        [<Position>] pos : V4f
        [<Color>] c : V4f
    }

type Mode =
    | Fast = 0
    | Precise = 1

type UniformScope with
    member x.IntConst : int = x?SpecializationConstant?IntConst
    member x.UIntConst : uint32 = x?SpecializationConstant?UIntConst
    member x.LongConst : int64 = x?SpecializationConstant?LongConst
    member x.ULongConst : uint64 = x?SpecializationConstant?ULongConst
    member x.FloatConst : float32 = x?SpecializationConstant?FloatConst
    member x.DoubleConst : float = x?SpecializationConstant?DoubleConst
    member x.BoolConst : bool = x?SpecializationConstant?BoolConst
    member x.ModeConst : Mode = x?SpecializationConstant?ModeConst
    member x.VecConst : V3f = x?SpecializationConstant?VecConst

let private compileVulkan (effects : list<Effect>) =
    let glsl, res = GLSL.compile' glslVulkan effects
    GLSL.printResults None res glsl
    glsl

[<Test>]
let ``Scalar types``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            let a = float32 uniform.IntConst + float32 uniform.UIntConst
            let b = float32 uniform.LongConst + float32 uniform.ULongConst
            let c = uniform.FloatConst + float32 uniform.DoubleConst
            let d = if uniform.BoolConst then 1.0f else 0.0f
            let e = if uniform.ModeConst = Mode.Precise then 1.0f else 0.5f
            return v.c * (a + b + c + d + e)
        }

    let glsl = compileVulkan [ Effect.ofFunction frag ]

    GLSL.shouldContainRegex glsl [
        @"layout\(constant_id = [0-9]+\) const int IntConst = 0;", Some 1
        @"layout\(constant_id = [0-9]+\) const uint UIntConst = 0u;", Some 1
        @"layout\(constant_id = [0-9]+\) const int64_t LongConst = 0l;", Some 1
        @"layout\(constant_id = [0-9]+\) const uint64_t ULongConst = 0ul;", Some 1
        @"layout\(constant_id = [0-9]+\) const float FloatConst = 0\.0;", Some 1
        @"layout\(constant_id = [0-9]+\) const double DoubleConst = 0\.0lf;", Some 1
        @"layout\(constant_id = [0-9]+\) const bool BoolConst = false;", Some 1
        @"layout\(constant_id = [0-9]+\) const int ModeConst = 0;", Some 1
        @"uniform SpecializationConstant", Some 0
    ]

    for name in [ "IntConst"; "UIntConst"; "LongConst"; "ULongConst"; "FloatConst"; "DoubleConst"; "BoolConst"; "ModeConst" ] do
        glsl.iface.specializationConstants |> MapExt.containsKey name |> should be True

[<Test>]
let ``Ids are sorted by name and stable across stages``() =
    Setup.Run()

    let vert (v : Vertex) =
        vertex {
            return { v with pos = v.pos * uniform.FloatConst }
        }

    let frag (v : Vertex) =
        fragment {
            return v.c * (uniform.FloatConst + float32 uniform.IntConst)
        }

    let glsl = compileVulkan [ Effect.ofFunction vert; Effect.ofFunction frag ]

    let floatId = glsl.iface.specializationConstants.["FloatConst"].specId
    let intId = glsl.iface.specializationConstants.["IntConst"].specId
    floatId |> should equal 0
    intId |> should equal 1

    // the constant used by both stages is declared once in the shared section with that id
    GLSL.shouldContainRegex glsl [
        sprintf @"layout\(constant_id = %d\) const float FloatConst = 0\.0;" floatId, Some 1
        sprintf @"layout\(constant_id = %d\) const int IntConst = 0;" intId, Some 1
        @"constant_id", Some 2
    ]

[<Test>]
let ``Interface entry``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c * float32 uniform.IntConst
        }

    let glsl = compileVulkan [ Effect.ofFunction frag ]
    let sc = glsl.iface.specializationConstants.["IntConst"]

    sc.specName |> should equal "IntConst"
    sc.specType |> should equal (GLSLType.Int(true, 32))

[<Test>]
let ``Backends without specialization constants use a uniform buffer``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return v.c * float32 uniform.IntConst
        }

    let glsl, res = GLSL.compile' glsl430 [ Effect.ofFunction frag ]
    GLSL.printResults None res glsl

    GLSL.shouldContainRegex glsl [
        @"constant_id", Some 0
        @"uniform SpecializationConstant", Some 1
    ]

    glsl.iface.specializationConstants |> MapExt.isEmpty |> should be True

[<Test>]
let ``Non-scalar types are rejected``() =
    Setup.Run()

    let frag (v : Vertex) =
        fragment {
            return V4f(uniform.VecConst, 1.0f)
        }

    let exn = Assert.Throws(fun () -> GLSL.compile' glslVulkan [ Effect.ofFunction frag ] |> ignore)
    Assert.That(exn.Message, Does.Contain "specialization constants must be scalar")
