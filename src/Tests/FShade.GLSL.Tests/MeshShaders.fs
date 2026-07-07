module MeshShaders

open Aardvark.Base
open FShade
open NUnit.Framework
open FsUnit
open FShade.Tests

type MeshVertex =
    {
        [<Position>] pos : V4f
        [<Semantic("Normal")>] n : V3f
        [<Color>] color : V4f
    }

type Payload =
    {
        offset : int
        scale : float32
    }

type FragmentInput =
    {
        [<Color>] fragColor : V4f
    }

let fragColor (v : FragmentInput) =
    fragment {
        return v.fragColor
    }

let fragNormal (v : {| n : V3f |}) =
    fragment {
        return V4f(v.n, 1.0f)
    }


[<LocalSize(X = 32); MeshOutputs(MaxVertices = 3, MaxPrimitives = 1)>]
let simpleMesh (_ : MeshInput<unit>) =
    meshTriangle {
        setMeshOutputs 3 1

        let id = getLocalId().X
        if id < 3 then
            let pos = V4f(float32 id, 0.0f, 0.0f, 1.0f)
            writeVertex id { pos = pos; n = V3f.OOI; color = V4f.IIII }

        if id = 0 then
            writeTriangle 0 (V3i(0, 1, 2))
    }

[<Test>]
let ``Simple MeshShader``() =
    Setup.Run()
    GLSL.shouldCompile' glslVulkan [
        Effect.ofFunction simpleMesh
        Effect.ofFunction fragColor
    ]

[<Test>]
let ``MeshShader code``() =
    Setup.Run()
    let glsl, res = GLSL.compile' glslVulkan [ Effect.ofFunction simpleMesh; Effect.ofFunction fragColor ]
    for (_, r) in res do
        match r with
        | Error e -> failwithf "%s" e
        | _ -> ()

    [
        "GL_EXT_mesh_shader", None
        @"layout\(local_size_x = 32, local_size_y = 1, local_size_z = 1\) in;", None
        @"layout\(triangles, max_vertices = 3, max_primitives = 1\) out;", None
        "SetMeshOutputsEXT", None
        @"gl_MeshVerticesEXT\[.*\]\.gl_Position", None
        @"gl_PrimitiveTriangleIndicesEXT\[.*\] = uvec3", None
    ]
    |> GLSL.shouldContainRegex glsl


[<LocalSize(X = 1)>]
let cullTask (_ : TaskInput) =
    task {
        let cnt : int = uniform?MeshletCount
        emitMeshTasks (V3i(cnt, 1, 1)) { offset = 42; scale = 2.0f }
    }

[<LocalSize(X = 32); MeshOutputs(MaxVertices = 3, MaxPrimitives = 1)>]
let payloadMesh (input : MeshInput<Payload>) =
    meshTriangle {
        setMeshOutputs 3 1

        let id = getLocalId().X
        if id < 3 then
            let pos = V4f(float32 (id + input.Payload.offset) * input.Payload.scale, 0.0f, 0.0f, 1.0f)
            writeVertex id { pos = pos; n = V3f.OOI; color = V4f.IIII }

        if id = 0 then
            writeTriangle 0 (V3i(0, 1, 2))
    }

[<Test>]
let ``Task and MeshShader with payload``() =
    Setup.Run()
    let glsl, res = GLSL.compile' glslVulkan [ Effect.ofFunction cullTask; Effect.ofFunction payloadMesh; Effect.ofFunction fragColor ]
    for (_, r) in res do
        match r with
        | Error e -> failwithf "%s" e
        | _ -> ()

    [
        "taskPayloadSharedEXT", None
        "EmitMeshTasksEXT", None
        @"_payload\.offset", None
        @"_payload\.scale", None
    ]
    |> GLSL.shouldContainRegex glsl

[<LocalSize(X = 32); MeshOutputs(MaxVertices = 3, MaxPrimitives = 1)>]
let offsetOnlyMesh (input : MeshInput<Payload>) =
    meshTriangle {
        setMeshOutputs 3 1

        let id = getLocalId().X
        if id < 3 then
            let pos = V4f(float32 (id + input.Payload.offset), 0.0f, 0.0f, 1.0f)
            writeVertex id { pos = pos; n = V3f.OOI; color = V4f.IIII }

        if id = 0 then
            writeTriangle 0 (V3i(0, 1, 2))
    }

[<Test>]
let ``Payload fields are pruned by linking``() =
    Setup.Run()
    // the mesh-shader only reads payload.offset -> 'scale' must vanish from the
    // payload block and from the task-shader
    let glsl, res = GLSL.compile' glslVulkan [ Effect.ofFunction cullTask; Effect.ofFunction offsetOnlyMesh; Effect.ofFunction fragColor ]
    for (_, r) in res do
        match r with
        | Error e -> failwithf "%s" e
        | _ -> ()

    [ @"_payload\.offset", None ] |> GLSL.shouldContainRegex glsl

    if glsl.code.Contains "scale" then
        failwithf "unused payload field 'scale' was not removed:\n%s" glsl.code


type TrafoVertex =
    {
        [<Position>] tpos : V4f
    }

let trafo (v : TrafoVertex) =
    vertex {
        let m : M44f = uniform?ModelTrafo
        return { v with tpos = m * v.tpos }
    }

[<Test>]
let ``MeshShader composed with VertexShader``() =
    Setup.Run()
    let glsl, res = GLSL.compile' glslVulkan [ Effect.ofFunction simpleMesh; Effect.ofFunction trafo; Effect.ofFunction fragColor ]
    for (_, r) in res do
        match r with
        | Error e -> failwithf "%s" e
        | _ -> ()

    [
        "ModelTrafo", None
        @"gl_MeshVerticesEXT\[.*\]\.gl_Position", None
    ]
    |> GLSL.shouldContainRegex glsl

[<Test>]
let ``MeshShader effect serialization round-trip``() =
    Setup.Run()
    let original = Effect.compose [ Effect.ofFunction cullTask; Effect.ofFunction payloadMesh; Effect.ofFunction fragColor ]
    let blob = Effect.pickle original
    use ms = new System.IO.MemoryStream(blob)
    let loaded = Effect.deserialize ms
    loaded.Id |> should equal original.Id

    let ms = loaded.MeshShader |> Option.get
    ms.shaderLocalSize |> should equal (Some (V3i(32, 1, 1)))
    ms.shaderOutputPrimitives |> should equal (Some 1)
    (loaded.TaskShader |> Option.get).shaderStage |> should equal ShaderStage.Task

    // the deserialized effect must compile to the same GLSL
    let glsl1, _ = GLSL.compile' glslVulkan [ original ]
    let glsl2, _ = GLSL.compile' glslVulkan [ loaded ]
    glsl1.code |> should equal glsl2.code

[<Test>]
let ``MeshShader linking drops unused outputs``() =
    Setup.Run()
    // fragNormal reads only the normal -> Colors output (and its writes) must disappear
    let glsl, res = GLSL.compile' glslVulkan [ Effect.ofFunction simpleMesh; Effect.ofFunction fragNormal ]
    for (_, r) in res do
        match r with
        | Error e -> failwithf "%s" e
        | _ -> ()

    if glsl.code.Contains "Colors[" then
        failwithf "unused mesh output 'Colors' was not removed:\n%s" glsl.code
