
open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open System.Reflection

let sink(a,b) = ()

type Vertex =
    {
        [<Position>] pos : V4d
        [<Semantic("TexCoord")>] tc : V2d
        [<SourceVertexIndex>] vi : int
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

    let hugo (v : V4d) =
        let a : float = uniform?A?B
        v * a

let private texSampler =
    sampler2d {
        texture uniform?ColorTexture
        filter Filter.Anisotropic
        addressU WrapMode.Wrap
        addressV WrapMode.Wrap
    }


type MyEnum =
    | A = 1
    | B = 2



let effectTest() =

    

    let vert (v : Vertex) =
        vertex {
            return {
                pos = Bla.hugo (uniform.Trafo * v.pos) 
                tc = v.tc
                vi = 0
            }
        }

    let frag (v : Vertex) =
        fragment {
        
            let asd : MyEnum = uniform?ASD

            let color = 
                if asd = MyEnum.A then
                    let x = int v.tc.X
                    let color =
                        if x > 0 then
                            let color = 4 * x
                            color * 3

                        else
                            0

                    color * 7
                else
                    0

            return {
                color = texSampler.Sample(v.tc) * float color
                tc = v.tc
            }
        }

    let vert = Effect.ofFunction vert
    let frag = Effect.ofFunction frag
    
    let effect = Effect.compose [vert;frag]

    let config =
        EffectConfig.ofList [
            Intrinsics.Color, typeof<V4d>, 1
            "Bla", typeof<V2d>, 0
        ]

    effect
        |> Effect.toModule config
        |> ModuleCompiler.compileGLSL glsl410
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


    let config =
        EffectConfig.ofList [
            Intrinsics.Color, typeof<V4d>, 0
        ]
    Effect.compose [sa; sb] 
        |> Effect.toModule config
        |> ModuleCompiler.compileGLSL glsl410
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


module TessDeconstruct = 
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.ExprShape
    
    module Sketch = 
        type Bla = Bla

        let patchOut (v : 'a) = Bla
        type TessControlBuilder() =
            inherit BaseBuilder()

            member x.Quote() = ()
            member x.Delay f = f()
            member x.Bind(levels : Bla, f : unit -> Bla) : unit =
                ()

            member x.Return(u : unit) = Bla

            member x.Yield(v : 'a) = v

            interface IShaderBuilder with
                member x.ShaderStage = ShaderStage.TessControl
                member x.OutputTopology = None

        let tessControl = TessControlBuilder()
    
        let testTCS (quad : Patch<3 N, Vertex>) =
            tessControl {
                if quad.InvocationId = 0 then
                    do! patchOut { pos = V4d.IIII; tc = V2d.II; vi = 0 }


                yield quad.[quad.InvocationId]
            }


    [<GLSLIntrinsic("mix({1},{2},{0})")>]
    let inline lerp (t : float) (a : 'a) (b : 'a)  = 
        onlyInShaderCode<'a> "mix"


    let test (quad : Patch<3 N, Vertex>) =
        tessellation {
            let p0 = quad.[0].pos
            let p1 = quad.[1].pos
            let p2 = quad.[2].pos

            let centroid = (p0 + p1 + p2) / 3.0
            
            let level = 
                let a = 1.0 / centroid.Z
                2.0 * a

            let! coord = tessellateTriangle (level) (level, level, level)
            
            let test = sinh coord.X * p0.Abs + coord.Y * p1 + coord.Z * p2

            let p = lerp 0.5 p0 test

            return {
                pos = p + V4d(0.0, level, 0.0, 0.0)
                tc = coord.XY
                vi = 0
            }
        }

    let geometry (tri : Triangle<Vertex>) =
        triangle {
            let scale : float = uniform?SomeScope?Scale

            let center = (tri.P0.pos + tri.P1.pos + tri.P2.pos) / 3.0

            yield { tri.P0 with pos = center + (tri.P0.pos - center) * scale }
            yield { tri.P1 with pos = center + (tri.P1.pos - center) * scale }
            yield { tri.P2 with pos = center + (tri.P2.pos - center) * scale }
        }
       

    let sam = 
        sampler2d {
            texture uniform?SomeTexture
        }

    let frag (v : Vertex)  =
        fragment {
            let col : V4d = uniform?PerView?AdditionalColor
            let s = V2d(sam.Size)
            return v.pos + col + V4d(s.X, s.Y, 0.0, 0.0)
        }

    let loopUnroll (v : Vertex) =
        fragment {
            let mutable a = 0.0

            Preprocessor.unroll()
            for i in 0 .. 2 .. 6 do
                let x = 2 * i
                a <- a + float x


            return V4d(a, 0.0, 0.0, 1.0)
        }

    let signatureTest() =
        let someFunction (a : V4d) (b : V4d) (c : V4d) = a + b * c
        let test2 = someFunction V4d.IIII
        let test3 = test2 V4d.IIII
        let s0 = FunctionSignature.ofFunction test3
        let s1 = FunctionSignature.ofFunction (fun a -> someFunction V4d.IIII V4d.IIII a)
        printfn "equal: %A" (s0 = s1)


    let run() =
//
//        let test =
//            Effect.ofFunction loopUnroll
//                |> Effect.toModule (Map.ofList ["Colors", 0])
//                |> ModuleCompiler.compileSpirV
//
//        for i in test.instructions do
//            printfn "%A" i
//        System.Environment.Exit 0



        let config =
            EffectConfig.ofList [
                Intrinsics.Color, typeof<V4d>, 0
                "Blubb", typeof<float>, 2
            ]

        let cModule = 
            Effect.compose [Effect.ofFunction test]
                |> Effect.toModule config
                |> ModuleCompiler.compile glsl410

        cModule
            |> GLSL.Assembler.assemble glsl410
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
                let level = 
                    let a = 1.0 / centroid.Z
                    2.0 * a

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

[<Literal>]
let local = 32

[<LocalSize(X = local)>]
let computer (f : Expr<float -> float -> float>) (a : float[]) (b : float[]) (c : Image2d<Formats.r32f>) =
    compute {
        let temp = allocateShared<float> local

        let id = getGlobalId()
        let i = id.X

        let l = getLocalIndex()
        temp.[l] <- c.[V2i(l,0)].X
        barrier()

        b.[i] <- (%f) a.[i] temp.[l]

    }


[<LocalSize(X = 256)>]
let scan (add : Expr<'a -> 'a -> 'a>) (zero : Expr<'a>) (input : 'a[]) (output : 'a[]) =
    compute {
        let temp = allocateShared<'a> 256

        let gid = getGlobalId().X
        let lid = getLocalId().X

        if gid < input.Length then
            temp.[lid] <- input.[gid]
        else
            temp.[lid] <- %zero

        barrier()

        let mutable d = 2
        let mutable s = 1
        while s < 256 do
            if lid % d = 0 && lid >= s then
                temp.[lid] <- (%add) temp.[lid] temp.[lid - s]

            barrier()
            s <- s * 2
            d <- d * 2


        s <- s / 2
        d <- d / 2
        while s >= 1 do
            if lid % d = 0 && lid + s < 256 then
                temp.[lid + s] <- (%add) temp.[lid + s] temp.[lid]

            barrier()
            s <- s / 2
            d <- d / 2

        if gid < input.Length then
            output.[gid] <- temp.[lid]


    }

[<EntryPoint>]
let main args =


    let test = ComputeShader.ofFunction (scan <@ fun a b -> a + b @> <@ 0.0 @>)
    let m = ComputeShader.toModule test
    let code = ModuleCompiler.compileGLSL410 m
    printfn "%s" code
    System.Environment.Exit 0


    effectTest()
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
        entry |> ModuleCompiler.compileGLSL glsl410

    printfn "%s" entry
    0

