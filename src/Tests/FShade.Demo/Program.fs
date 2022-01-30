
open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open FShade.Tests
open System.Reflection
open System.Threading
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open System
open System.Runtime.InteropServices
open FSharp.Data.Adaptive

#nowarn "9"

[<RequireQualifiedAccess>]
type SegmentMergeMode =
    | ValueToAvg
    | AvgToAvg
    | TTest


[<StructLayout(LayoutKind.Sequential); StructuredFormatDisplay("{AsString}")>]
type RegionStats =
    struct
        val mutable public Count : int
        val mutable public SurfaceCount : int
        val mutable public Id : int
        val mutable public Average : float32
        val mutable public StdDev : float32
        val mutable public TwoLargeNeighbours : int

        member private x.AsString = x.ToString()

        [<ReflectedDefinition>]
        new(count, surface, id, avg, dev) = { Count = count; SurfaceCount = surface; Id = id; Average = avg; StdDev = dev; TwoLargeNeighbours = 0 }

        override x.ToString() =
            sprintf "{ Count = %A; SurfaceCount = %A; Average = %A; StdDev = %A; Id = %A; Two = %A }" x.Count x.SurfaceCount x.Average x.StdDev x.Id (x.TwoLargeNeighbours <> 0)

    end

module Shader =
    open FShade


    type UniformScope with
        member x.RegionBuffer : RegionStats[] = uniform?StorageBuffer?RegionStats

    type RegionVertex =
        {
            [<Semantic("RegionId")>]
            id : int

            [<Semantic("Average")>]
            avg : float

            [<Semantic("StdDev")>]
            stddev : float

            [<Semantic("Surface")>]
            surface : int

            [<Semantic("Count")>]
            count : int
        }

    [<GLSLIntrinsic("gl_VertexIndex")>]
    let getVertexId() : int = onlyInShaderCode "getVertexId"

    let regionVertex (v : RegionVertex) =
        vertex {
            let id = getVertexId()
            return {
                id = ~~~uniform.RegionBuffer.[id].Id
                avg = float uniform.RegionBuffer.[id].Average
                stddev = float uniform.RegionBuffer.[id].StdDev
                surface = uniform.RegionBuffer.[id].SurfaceCount
                count = uniform.RegionBuffer.[id].Count
            }
        }



let compileEffect (e : list<Effect>) =
    let e = Effect.compose e
    
    let lastShader = e.LastShader

    let lastStage, outputs =
        match lastShader with
            | Some shader ->
                let mutable id = 0
                let newId() = Interlocked.Increment(&id)
                let outputs = shader.shaderOutputs |> Map.map (fun name p -> p.paramType, newId())
                shader.shaderStage, outputs
            | None ->
                ShaderStage.Fragment, Map.ofList ["Colors", (typeof<V4d>, 0)]

    let glsl =
        e |> Effect.toModule { EffectConfig.empty with lastStage = lastStage; outputs = outputs } |> ModuleCompiler.compileGLSL410
    
    
    
    glsl


type Vertex = { [<Position>] pos : V4d }

let shaderA (v : Vertex) =
    fragment {
        return v.pos
    }
    


let funny (v : Vertex) =
    vertex {
        if v.pos.X > 0.0 then
            let Positions = v.pos.W + v.pos.X
            let (a,b) = 
                let v = 2.0 * v.pos
                (v.X, v.Y + 1.0 + Positions)
            return { v with pos = V4d(a,b,b,a) }
        else
            return { pos = V4d.Zero }
    }


type Fragment = 
    {
        [<Color>] c : V4d
        [<Depth(DepthWriteMode.OnlyLess)>] d : float
    }

let fraggy (v : Vertex) =
    fragment {
        return {
            c = V4d.IIII
            d = 0.5
        }
    }

//type Payload =
//    {
//        hitCount : int
//    }

//type Result =
//    {
//        value : V4d
//    }

//type Ray =
//    { origin : V3d; direction : V3d }

////type Scene =
////    | Self
////    | Named of string

//[<AutoOpen>]
//module Intrinsics = 


//    type ShaderTable =
//        {
//            miss        : int
//            offset      : int
//            stride      : int
//        }

//        static member Default = { miss = 0; offset = 1; stride = 1 }

//    let trace<'a, 's> (table : ShaderTable) (state : RayQuery<'s, 'a>) : 'a = failwith ""

//    let buffers = UniformScope.Global

//    type UniformScope with
//        member x.Index : int[][] = uniform?StorageBuffer?Index
//        member x.Positions : V4d[][] = uniform?StorageBuffer?Positions
//        member x.Colors : V4d[][] = uniform?StorageBuffer?Colors
//        member x.TexCoords : V2d[][] = uniform?StorageBuffer?TexCoords

//let sammy =
//    sampler2d {
//        texture uniform?MyTexture
//        filter Filter.MinMagMipLinear
//        addressU WrapMode.Wrap
//        addressV WrapMode.Wrap

//    }

//type Other =
//    { ishit : bool }

//let test (state : RayHit<Payload, Result>) =
//    rayhit {
//        let gi = state.instanceIndex
//        let i0 = buffers.Index.[gi].[3 * state.primitiveId + 0]
//        let i1 = buffers.Index.[gi].[3 * state.primitiveId + 1]
//        let i2 = buffers.Index.[gi].[3 * state.primitiveId + 2]

//        let c = V3d(state.coord.X, state.coord.Y, 1.0 - state.coord.X - state.coord.Y)
//        let tc = c.X * buffers.TexCoords.[gi].[i0] + c.Y * buffers.TexCoords.[gi].[i1] + c.Z * buffers.TexCoords.[gi].[i2]

//        let hitty = state.query.origin + state.rayT * state.query.direction

//        let res = 
//            RayQuery.trace {
//                tmin = 666.0
//                scene = state.query.scene
//                origin = hitty
//                direction = V3d.OOI
//                payload = state.query.payload
//                tmax = 1337.0
//            }
            
//        let res2 : Other = 
//            RayQuery.trace {
//                tmin = 666.0
//                scene = RayScene.RayScene "Hugo"
//                origin = hitty
//                direction = V3d.OOI
//                payload = state.query.payload
//                tmax = 1337.0
//            }

//        return { 
//            value = 
//                if res2.ishit then sammy.Sample(tc) * res.value
//                else V4d.IIII
//        }
//    }

module SpirVPrint =
    open GLSLang.SpirV

    let line (color : ConsoleColor) fmt =
        fmt |> Printf.kprintf (fun str ->
            let prefix = System.String(' ', Report.JobReporter.m_indent)
            if Console.CursorLeft = 0 then Console.Write(" 0: {0}", prefix)
            let o = Console.ForegroundColor
            Console.ForegroundColor <- color
            Console.WriteLine("{0}", str)
            Console.ForegroundColor <- o
        )
        
    let write (color : ConsoleColor) fmt =
        fmt |> Printf.kprintf (fun str ->
            let prefix = System.String(' ', Report.JobReporter.m_indent)
            if Console.CursorLeft = 0 then Console.Write(" 0: {0}", prefix)
            let o = Console.ForegroundColor
            Console.ForegroundColor <- color
            Console.Write str
            Console.ForegroundColor <- o
        )
         
    let write' fmt =
        fmt |> Printf.kprintf (fun str ->
            let prefix = System.String(' ', Report.JobReporter.m_indent)
            if Console.CursorLeft = 0 then Console.Write(" 0: {0}", prefix)
            Console.Write str
        )

    open FSharp.Reflection

    let desctruct =
        fun (i : Instruction) ->
            let case, fields = FSharpValue.GetUnionFields(i, typeof<Instruction>, true)
            case.Name, fields

    let print (m : Module) =
        line ConsoleColor.DarkGray "; SPIR-V"
        line ConsoleColor.DarkGray "; Version: %d" m.version
        line ConsoleColor.DarkGray "; Generator: 0x%08X" m.generator
        line ConsoleColor.DarkGray "; Bound: %d" m.bound

        let mutable variables = Set.empty
        let mutable names = Map.empty
        let mutable memberNames = Map.empty
        for i in m.instructions do
            match Instruction.tryGetId i with
            | Some id -> variables <- Set.add id variables
            | None -> ()

            match i with
            | Instruction.Name(dst, name) -> names <- Map.add dst (sprintf "%%%s" name) names
            | Instruction.MemberName(dst, mem, name) -> memberNames <- Map.add (dst, mem) name memberNames
            | _ -> ()

        names <- Map.add System.UInt32.MaxValue (sprintf "%%%d" m.bound) names
        let maxNameLength = names |> Map.toSeq |> Seq.map (fun (_,n) -> n.Length) |> Seq.max

        let getName(id : uint32) =
            if Set.contains id variables then
                match Map.tryFind id names with
                | Some n -> ConsoleColor.DarkYellow, n
                | None -> ConsoleColor.DarkYellow, sprintf "%%%d" id
            else
                ConsoleColor.Red, string id

        let getPaddedName (id : uint32) =
            let _, n = getName id
            if n.Length < maxNameLength then System.String(' ', maxNameLength - n.Length) + n
            else n

        let noName = System.String(' ', maxNameLength + 3)
        for i in m.instructions do
            let typ = Instruction.tryGetResultTypeId i
            let result = Instruction.tryGetId i

            let c, args = desctruct i

            let rec printValue (pi : int) (a : obj) =
                match a with
                | :? uint32 as a ->
                    [| getName a |]
                | :? Array as a ->
                    if a.Length <= 0 then 
                        [||]
                    else
                        [|
                            yield ConsoleColor.White, "["
                            for i in 0 .. a.Length - 1 do
                                if i > 0 then yield ConsoleColor.White, ", "
                                let v = a.GetValue i
                                yield! printValue 100 v
                            
                            yield ConsoleColor.White, "]"
                        |]
                | :? string as str ->
                    let str = System.Text.RegularExpressions.Regex.Replace(str, @"\p{C}+", "");
                    [| ConsoleColor.White, "\""; ConsoleColor.Green, str; ConsoleColor.White, "\""  |]

                | _ ->
                    [| ConsoleColor.Red, string a |]

            let args =
                args |> Array.mapi printValue

            match result with
            | Some r ->
                let name = getPaddedName r
                write ConsoleColor.Blue "%s" name
                write' " = "
                write ConsoleColor.White "Op%s" c
                for part in args do
                    write' " "
                    for (c, a) in part do
                        write c "%s" a

                line ConsoleColor.White ""
            | None ->
                write' "%s" noName
                write ConsoleColor.White "Op%s" c
                for part in args do
                    write' " "
                    for (c, a) in part do
                        write c "%s" a

                line ConsoleColor.White ""

        ()


[<AutoOpen>]
module RaytracingTest =

    type UniformScope with
        member x.OutputBuffer : Image2d<Formats.rgba32f> = uniform?OutputBuffer

    type Payload =
        {
            foo : float
            flag : bool
        }

    type CallableData =
        {
            flag : bool
            value : float
        }

    let scene =
        scene { accelerationStructure uniform?RaytracingScene }

    let callableShader (input : RayCallableInput<CallableData>) =
        callable {
            if input.data.flag then
                return { value = 0.5; flag = false }
            else
                return { value = 1.0; flag = false }
        }

    let intersectionShader (input : RayIntersectionInput) =
        intersection {
            Intersection.Report(0.5, true, HitKind.FrontFacingTriangle) |> ignore
        }

    let missShader (input : RayMissInput<Payload>) =
        miss {
            if input.payload.flag then
                return { foo = 1.0; flag = false }
            else
                return { foo = 0.0; flag = false }
        }

    let anyHitShader (input : RayHitInput<Payload>) =
        anyHit {
            if input.ray.direction = V3d.Zero then
                ignoreIntersection()
            elif input.hit.attribute.X = 0.0 then
                terminateRay()
        }

    let closestHitShader (input : RayHitInput<Payload>) =
        closestHit {
            let whatever = scene.TraceRay(V3d.Zero, V3d.XAxis)
            return { foo = input.hit.attribute.X; flag = whatever }
        }

    let raygenShader (input : RayGenerationInput) =
        let secondaryRayFlags = RayFlags.Opaque ||| RayFlags.SkipClosestHitShader
        let rayType = Sym.ofString "rayMain"
        let missType = Sym.ofString "missMain"

        raygen {
            let whatever = Callable.Execute({ flag = true; value = 0.0 })
            let result = scene.TraceRay<Payload>(scene.TraceRay<V3d>(V3d.Zero, V3d.ZAxis, V3d.One), V3d.YAxis, miss = missType, ray = rayType, flags = secondaryRayFlags)
            uniform.OutputBuffer.[input.work.id.XY] <- V4d(result.foo + whatever.value)
        }
[<AutoOpen>]
module IOExtensions =
    open System.Runtime.CompilerServices
    open System.IO
    open Microsoft.FSharp.NativeInterop
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators

    [<AbstractClass; Sealed; Extension>]
    type SpanExtensions private() =
        [<Extension>]
        static member inline Cast<'a, 'b when 'a : unmanaged and 'b : unmanaged>(x : Span<'a>) : Span<'b> =
            Span<'b>(
                NativePtr.toVoidPtr (&&x.GetPinnableReference()),
                int ((int64 x.Length * int64 sizeof<'a>) / int64 sizeof<'b>)
            )
        [<Extension>]
        static member inline Cast<'a, 'b when 'a : unmanaged and 'b : unmanaged>(x : ReadOnlySpan<'a>) : ReadOnlySpan<'b> =
            ReadOnlySpan<'b>(
                NativePtr.toVoidPtr (&&x.GetPinnableReference()),
                int ((int64 x.Length * int64 sizeof<'a>) / int64 sizeof<'b>)
            )

    type Stream with

        member x.Write(data : Span<'a>) =
            x.Write(Span.op_Implicit (data.Cast<_,byte>()))

        member x.Write(data : ReadOnlySpan<'a>) =
            x.Write(data.Cast<_,byte>())

        member x.Write(data : 'a[], index : int, count : int) =
            x.Write(Span(data, index, count))

        member x.ReadSafe(span : Span<'a>) = 
            let mutable dst = span.Cast<_, byte>()
            let mutable rem = dst.Length
            while rem > 0 do
                let r = x.Read(dst)
                if r = 0 then failwith "stream ended"
                rem <- rem - r
                dst <- dst.Slice r

        member x.ReadSafe(buffer : 'a[], index : int, count : int) =
            x.ReadSafe(Span(buffer, index, count))


[<EntryPoint>]
let main args =
    Aardvark.Init()

    let f = System.IO.Path.GetTempFileName()
    do
        use f = System.IO.File.OpenWrite f
        f.Write (Span [|V3d.III; V3d.OIO; V3d.IIO|])


    do 
        use f = System.IO.File.OpenRead f
        let arr = Array.zeroCreate<V3d> (int (f.Length / int64 sizeof<V3d>))
        f.ReadSafe(Span arr)
        printfn "%A" arr

    exit 0


    let effect =
        let defaultHitGroup =
            hitgroup {
                anyHit anyHitShader
                closestHit closestHitShader
                intersection intersectionShader
            }

        raytracingEffect {
            raygen raygenShader
            hitgroup defaultHitGroup
            miss missShader
            callable callableShader
        }

    let glsl =
        effect
        |> RaytracingEffect.toModule
        |> ModuleCompiler.compileGLSLRaytracing

    Log.start "GLSL"
    let lines = glsl.code.Split("\r\n") |> Array.indexed

    let digits = log10 (float lines.Length) |> ceil |> int
    for (i, l) in lines do
        let i = 
            let v = string (i + 1)
            if v.Length < digits then System.String(' ', digits - v.Length) + v
            else v
        Log.line "%s: %s" i l
    Log.stop()


    let compile (name : Option<string>) (shader : Shader) =
        let def =
            match name with
            | Some n -> sprintf "%A_%s" shader.shaderStage n
            | _ -> sprintf "%A" shader.shaderStage

        Log.start "Compiling %s"def
        let res = GLSL.glslangWithTarget GLSLang.Target.SPIRV_1_4 shader.shaderStage [def] glsl.code

        match res with
        | Warning w ->
            Log.warn "%s" w
        | Error e ->
            Log.error "%s" e
        | _ -> ()

        Log.stop()

    compile None effect.RayGenerationShader

    for (KeyValue(name, shader)) in effect.MissShaders do
        compile (name |> string |> Some) shader

    for (KeyValue(name, shader)) in effect.CallableShaders do
        compile (name |> string |> Some) shader

    for (KeyValue(groupName, hitgroup)) in effect.HitGroups do
        for (KeyValue(rayName, entry)) in hitgroup.PerRayType do
            let name = Some <| sprintf "%A_%A" groupName rayName
            entry.AnyHit |> Option.iter (compile name)
            entry.ClosestHit |> Option.iter (compile name)
            entry.Intersection |> Option.iter (compile name)

    0

