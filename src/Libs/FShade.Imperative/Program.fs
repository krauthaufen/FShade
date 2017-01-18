
open System
open System.Reflection
open Aardvark.Base
open FShade.Imperative
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open FShade

type GLSLBackend private() =
    inherit Compiler.Backend()
    static let instance = GLSLBackend() :> Compiler.Backend

    static member Instance = instance

    override x.TryGetIntrinsicMethod (c : MethodInfo) =
        match c with
            | MethodQuote <@ sin @> _           -> CIntrinsic.simple "sin" |> Some
            | MethodQuote <@ cos @> _           -> CIntrinsic.simple "cos" |> Some
            | MethodQuote <@ clamp @> _         -> CIntrinsic.custom "clamp" [2;0;1] |> Some
            | _ -> None
    
    override x.TryGetIntrinsicCtor (c : ConstructorInfo) =
        None

type MyRecord =
    { a : int; b : int }

type MyUnion =
    | Case1 of MyRecord
    | Case2 of float
    | Case3 of int * float


let lambdaTest() =
    let arr = Arr<4 N, int> [|1;2;3;4|]
    let arr2 = Arr<1 N, MyUnion> [|Case2 1.0|]
    let v = V4d(1.0, 3.0, 2.0, 4.0)
    let testModule = 
        Module.ofLambdas [
            "sepp", <@@ fun (m : M44d) (a : V4d) ->
                if m.M00 > 1.0 then
                    let x = Case3(arr.[2],2.0)
                    m * a + v
                else
                    arr.[0] <- 10
                    let y = Case1 { a = 10; b = int a.Y }
                    match y with
                        | Case1 v ->
                            V4d(-a.X |> clamp 0.0 1.0, cos 0.0, float v.a, 0.0)
                        | Case2 a ->
                            V4d(1.0, 0.0, 0.0, 0.0)
                        | Case3 _ ->
                            V4d(1.0, 0.0, 0.0, 0.0)
            @@>

            "heinz", <@@ fun () ->
                let mutable a = 0
                for i in 10 .. -1 .. 0 do
                    a <- a + 1
                arr2.[a]
            @@>
        ]

    let glslCode = 
        testModule
            |> Linker.compileAndLink GLSLBackend.Instance
            |> GLSL.CModule.glsl { 
                GLSL.Config.version = Version(4,1,0)
                GLSL.Config.locations = true
                GLSL.Config.perStageUniforms = false
                GLSL.Config.uniformBuffers = true 
            }

    printfn "%s" glslCode

let entryTest() =
    let data = Arr<4 N, float> [| 1.0; 2.0; 3.0; 4.0 |]

    let testCode = 
        <@ fun (p : V4d) (modelTrafo : float) (viewProjTrafo : float) ->
            let x = Case1 { a = 1; b = 3 }
            let w = 
                let test = modelTrafo * p
                data.[1] * test
            data.[1] * viewProjTrafo * w
        @>

    let seppCode = 
        <@ fun (tc : V2d) (modelTrafo : float) (seppy : float) ->
            let x = Case1 { a = 1; b = 3 }
            data.[1] * modelTrafo * (seppy * tc)
        @>


    let testEntry = 
        let entry = EntryPoint.ofLambda "main" testCode

        let inputs, uniforms = entry.arguments |> List.splitAt 1

        { entry with
            conditional = Some "Vertex"
            inputs = inputs
            uniforms = uniforms |> List.map (fun p -> { uniformName = p.paramName; uniformType = p.paramType; uniformBuffer = Some "BlaBuffer" })
            arguments = []
            decorations = [ EntryDecoration.Stages { prev = None; self = ShaderType.Fragment; next = Some ShaderType.Fragment } ]
        }

    let seppEntry = 
        let entry = EntryPoint.ofLambda "main" seppCode

        let inputs, uniforms = entry.arguments |> List.splitAt 1

        { entry with
            conditional = Some "Pixel"
            inputs = inputs
            uniforms = uniforms |> List.map (fun p -> { uniformName = p.paramName; uniformType = p.paramType; uniformBuffer = Some "BlaBuffer" })
            arguments = []
            decorations = [ EntryDecoration.Stages { prev = Some ShaderType.Vertex; self = ShaderType.Fragment; next = None } ]
        }

    // define a simple module consisting of two entry-points
    let mainModule = { entries = [testEntry; seppEntry] }

    // compile and link the defined module to a CModule.
    // then print the resulting module as GLSL using the given config
    let glslCode = 
        mainModule
            |> Linker.compileAndLink GLSLBackend.Instance
            |> GLSL.CModule.glsl { 
                GLSL.Config.version = Version(4,1,0)
                GLSL.Config.locations = false
                GLSL.Config.perStageUniforms = false
                GLSL.Config.uniformBuffers = true 
            }

    printfn "%s" glslCode

let forLoopTest() =
    let expression =    
        <@ 
            let mutable a = 0
            for i in 10 .. -1 .. 0 do
                a <- a + 1
        @>

    match expression with
        | Let(_,_,ForInteger(v, first, step, last, body)) ->
            printfn "%A .. %A .. %A" first step last
        | _ -> 
            printfn "ERROR: %A" expression

[<EntryPoint>]
let main args =
    //forLoopTest()
    lambdaTest()
    //entryTest()

    0