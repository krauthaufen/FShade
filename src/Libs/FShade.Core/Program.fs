
open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open System.Reflection

let sink(a,b) = ()

type GLSLBackend private() =
    inherit Compiler.Backend()
    static let instance = GLSLBackend() :> Compiler.Backend

    static member Instance = instance

    override x.TryGetIntrinsicMethod (c : MethodInfo) =
        match c with
            | MethodQuote <@ sink @> _          -> CIntrinsic.simple "sink" |> Some
            | MethodQuote <@ sin @> _           -> CIntrinsic.simple "sin" |> Some
            | MethodQuote <@ cos @> _           -> CIntrinsic.simple "cos" |> Some
            | MethodQuote <@ clamp @> _         -> CIntrinsic.custom "clamp" [2;0;1] |> Some

            | MethodQuote <@ discard @> _         -> CIntrinsic.simple "discard" |> Some
            | MethodQuote <@ emitVertex @> _      -> CIntrinsic.simple "EmitVertex" |> Some
            | MethodQuote <@ restartStrip @> _    -> CIntrinsic.simple "EndPrimitive" |> Some
            | MethodQuote <@ endPrimitive @> _    -> CIntrinsic.simple "EndPrimitive" |> Some

            | _ -> None
    
    override x.TryGetIntrinsicCtor (c : ConstructorInfo) =
        None

type Vertex =
    {
        [<Position>] pos : V4d
        [<Semantic("TexCoord")>] tc : V2d
    }

type Fragment =
    {
        [<Color>] color : V4d
        [<Semantic("TexCoord")>] tc : V2d
    }

type UniformScope with
    member x.Trafo : M44d= x?PerModel?Trafo

[<ReflectedDefinition>]
module Bla =
    let test (a : Arr<4 N, int>) =
        for i in 0 .. a.Length - 1 do
            a.[i] <- i
        a.Length

let effectTest() =

    let vert (v : Vertex) =
        vertex {
            return {
                pos = uniform.Trafo * v.pos
                tc = v.tc
            }
        }

    let frag (v : Vertex) =
        fragment {
            return {
                color = V4d.IIII
                tc = v.tc
            }
        }

    let vert = Shader.ofFunction vert
    let frag = Shader.ofFunction frag
    
    let effect = Effect.ofList (vert @ frag)

    Effect.empty
        |> Effect.link ShaderStage.Fragment (Map.ofList ["Colors", typeof<V4d>; "Bla", typeof<V2d>])
        |> Effect.toModule
        |> Linker.compileAndLink GLSLBackend.Instance
        |> GLSL.CModule.glsl  { 
            GLSL.Config.version = System.Version(4,1,0)
            GLSL.Config.locations = false
            GLSL.Config.perStageUniforms = true
            GLSL.Config.uniformBuffers = true 
        }
        |> printfn "%s"

    effect
        |> Effect.link ShaderStage.Fragment (Map.ofList ["Colors", typeof<V4d>])
        |> Effect.toModule
        |> Linker.compileAndLink GLSLBackend.Instance
        |> GLSL.CModule.glsl  { 
            GLSL.Config.version = System.Version(4,1,0)
            GLSL.Config.locations = false
            GLSL.Config.perStageUniforms = true
            GLSL.Config.uniformBuffers = true 
        }
        |> printfn "%s"

type Vertex1 =
    {
        [<Position>] p1 : V4d
        [<SourceVertexIndex>] i : int
    }

type Vertex2 =
    {
        [<Position>] p2 : V4d
        [<Semantic("Coord")>] c2 : V2d
        [<Semantic("Coord2")>] c3 : V2d
    }

let composeTest() =
    let a (tri : Triangle<Vertex1>) =
        triangle {
            for vi in 0 .. 2 do
                yield {
                    p1 = 1.0 * uniform.Trafo * tri.[vi].p1
                    i = vi
                }
        }

    let b (v : Vertex2) =
        vertex {
            if 1 + 3 < 10 then
                return {
                    p2 = 10.0 * v.p2 + V4d(v.c3.X, 0.0, 0.0, 0.0)
                    c2 = V2d(1.0, 4.0) + v.c2 + v.c3
                    c3 = v.c3
                }
            else
                return {
                    p2 = 3.0 * v.p2
                    c2 = V2d.II + v.c2
                    c3 = V2d.II
                }
        }

    let c (v : Vertex2) =
        fragment {
            return V4d.IOOI
        }

    let sa = Effect.ofFunction a
    let sb = Effect.ofFunction b
    let sc = Effect.ofFunction c



    Effect.compose [sa; sb]
        |> Effect.link ShaderStage.Fragment (Map.ofList [Intrinsics.Color, typeof<V4d>])
        |> Effect.toModule
        |> Linker.compileAndLink GLSLBackend.Instance
        |> GLSL.CModule.glsl  { 
            GLSL.Config.version = System.Version(4,1,0)
            GLSL.Config.locations = true
            GLSL.Config.perStageUniforms = false
            GLSL.Config.uniformBuffers = true 
        }
        |> printfn "%s"




module Crazyness = 
    type Test =
        struct
            val mutable public Value : int
            member x.Bla(a : int) = x.Value <- a
            member x.Blubb(a : int) = x.Value + a

            member x.Sepp
                with get() = x.Value
                and set v = x.Value <- v

            new(v) = { Value = v }
        end

    [<ReflectedDefinition>]
    let test1() =
        let mutable t = Test(100)
        t.Bla(1)
        t.Blubb(2)

    [<ReflectedDefinition>]
    let test2() =
        let t = Test(100)
        t.Bla(1)
        t.Blubb(2)

    let run() =
        printfn "test1: %A" (test1())   // 'test1: 3'
        printfn "test2: %A" (test2())   // 'test2: 102'

        let def1 = getMethodInfo <@ test1 @> |> Expr.TryGetReflectedDefinition |> Option.get
        let def2 = getMethodInfo <@ test2 @> |> Expr.TryGetReflectedDefinition |> Option.get

        printfn "def1: %A" def1
        printfn "def2: %A" def2


type TessBuilder() =
    inherit BaseBuilder()
    member x.Bind(t : TessCoord<'c>, f : 'c -> 'a) : 'a =
        failwith ""

    member x.Return(v) = v

    member x.Quote() = ()

    interface IShaderBuilder with
        member x.ShaderStage = ShaderStage.TessControl
        member x.OutputTopology = None

let tessellation = TessBuilder()

module TessDeconstruct = 
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.ExprShape

    let test (quad : Patch4<Vertex>) =
        tessellation {
            let p0 = quad.P0
            let p1 = quad.P1
            let p2 = quad.P2
            let p3 = quad.P3

            let centroid = (p0.pos + p1.pos + p2.pos + p3.pos) / 4.0
            let level = 1.0 / centroid.Z

            let! coord = tessellateQuad (level, level) (level, level, level, level)
            
            let px = coord.X * quad.P0.pos + (1.0 - coord.X) * quad.P1.pos
            let py = coord.X * quad.P2.pos + (1.0 - coord.X) * quad.P3.pos
            let p = coord.Y * px + (1.0 - coord.Y) * py

            return {
                pos = p + V4d(0.0, level, 0.0, 0.0)
                tc = coord
            }
        }

    let run() =
        let e = Effect.ofFunction test
        //let (tcs, tcsState), (tev, tevState) = test Unchecked.defaultof<_> |> Preprocessor.removeTessEval typeof<Patch4<Vertex>>

        e   |> Effect.link ShaderStage.Fragment (Map.ofList [Intrinsics.Color, typeof<V4d>])
            |> Effect.toModule
            |> Linker.compileAndLink GLSLBackend.Instance
            |> GLSL.CModule.glsl  { 
                GLSL.Config.version = System.Version(4,1,0)
                GLSL.Config.locations = false
                GLSL.Config.perStageUniforms = true
                GLSL.Config.uniformBuffers = true 
            }
            |> printfn "%s"



open FShade.Imperative
let expected() =
    let tcs = 
        <@ fun () ->
            let id = ShaderIO.ReadInput<int>(ParameterKind.Input, Intrinsics.InvocationId, 0)
            if id = 0 then
                let p0 = ShaderIO.ReadInput<V4d>(ParameterKind.Input, Intrinsics.Position, 0)
                let p1 = ShaderIO.ReadInput<V4d>(ParameterKind.Input, Intrinsics.Position, 1)
                let p2 = ShaderIO.ReadInput<V4d>(ParameterKind.Input, Intrinsics.Position, 2)
                let p3 = ShaderIO.ReadInput<V4d>(ParameterKind.Input, Intrinsics.Position, 3)

                
                let centroid = (p0 + p1 + p2) / 3.0
                let level = 1.0 / centroid.Z

                ShaderIO.WriteOutputs [|
                    Intrinsics.TessLevelInner, -1, [| level |] :> obj
                    Intrinsics.TessLevelOuter, -1, [| level; level; level |] :> obj
                    "level", -1, level :> obj
                |]



//            ShaderIO.WriteOutputs [|
//                Intrinsics.Position, ShaderIO.ReadInput<V4d>(ParameterKind.Input, Intrinsics.Position, id) :> obj
//            |]

            ()
        @>
    
    let tev =
        <@
            let coord = ShaderIO.ReadInput<V3d>(ParameterKind.Input, Intrinsics.TessCoord)
            
            let level = ShaderIO.ReadInput<V3d>(ParameterKind.Input, "level")
            let p0 = ShaderIO.ReadInput<V4d>(ParameterKind.Input, Intrinsics.Position, 0)
            let p1 = ShaderIO.ReadInput<V4d>(ParameterKind.Input, Intrinsics.Position, 1)
            let p2 = ShaderIO.ReadInput<V4d>(ParameterKind.Input, Intrinsics.Position, 2)
            let p3 = ShaderIO.ReadInput<V4d>(ParameterKind.Input, Intrinsics.Position, 3)

            ()
        @> 

    ()



[<EntryPoint>]
let main args =
    TessDeconstruct.run()
    System.Environment.Exit 0

    composeTest()
    System.Environment.Exit 0


    let optimized = 
        Optimizer.eliminateDeadCode 
            <@
                // long dependency chain test
                // needs 4 iterations in fixpoint search when a is used
                // a <- b <- c <- d <- a ...
                let mutable a = 0
                let mutable b = 0
                let mutable c = 0
                let mutable d = 0
                for i in 0 .. 2 .. 10 do
                    a <- a + b
                    b <- b + c
                    c <- c + d
                    d <- d + a
                sink(a,a)

                // unused y should be remove but side-effect 'z <- z + 1' should remain
                let mutable y = 0 
                let mutable z = 1
                if (z <- z + 1; y < 10) then
                    y <- 10
                else
                    y <- 100
                sink(z,z)


                // since t is unused it should be removed
                let mutable t = 0
                let mutable s = 0
                while (t <- t + 1; s < 10) do
                    s <- s + 1
                sink(s,s)

                // fun fact: do-while looks like:
                let mutable t = 0
                while (
                        t <- t + 1
                        t < 10
                ) do ()

                // array should remain
                let mutable x = 0
                let arr = [| V2i.Zero; V2i.Zero; V2i.Zero; V2i.Zero |]
                for i in 3 .. -1 .. 0 do
                    arr.[i].X <- i

                sink(arr, arr)

                let mutable bla = Arr<4 N, int> [| 1;2;3;4 |]
                let cnt = Bla.test(bla)
                sink(bla, bla)


                // Dot could modify r here (since this is always byref)
                let mutable r = V2d.II
                let test = r.Dot(r)
                sink(r,r)

            @>
    
    let entry =
        Expr.Lambda(Var("unitVar", typeof<unit>), optimized)
            |> Module.ofLambda "test"

//    let entry =
//        Module.ofLambda "test" <@ fun () ->
//            let mutable a = 0
//            let mutable b = 0
//            for i in 8 .. -1 .. 0 do
//                a <- a + 1
//                b <- b + 1
//            a
//        @>

    let entry =
        entry
            |> Linker.compileAndLink GLSLBackend.Instance
            |> GLSL.CModule.glsl  { 
                GLSL.Config.version = System.Version(4,1,0)
                GLSL.Config.locations = false
                GLSL.Config.perStageUniforms = true
                GLSL.Config.uniformBuffers = true 
            }

    printfn "%s" entry
    0

