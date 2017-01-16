
open System
open System.Reflection
open Aardvark.Base
open FShade.Imperative
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

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

[<EntryPoint>]
let main args =
    let arr = Arr<4 N, int> [|1;2;3;4|]
    let arr2 = Arr<1 N, MyUnion> [|Case2 1.0|]
    let v = V4d(1.0, 3.0, 2.0, 4.0)
    let test = 
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
                arr2.[0]
            @@>
        ]

    let testCode = 
        <@ fun (p : V4d) (ModelTrafo : float, ViewProjTrafo : float) ->
            let w = ModelTrafo * p
            ViewProjTrafo * w
        @>

    let seppCode = 
        <@ fun (tc : V2d) (ModelTrafo : float, Seppy : float) ->
            ModelTrafo * (Seppy * tc)
        @>

    let rec removeUselessLets (e : Expr) = 
        match e with
            | Let(vn, Var vo, e) when vn.Name = vo.Name -> removeUselessLets e
            | _ -> e
        


    let testEntry = 
        match testCode with
            | Lambdas([inputs; uniforms], body) -> 
                {
                    conditional = Some "Vertex"
                    entryName   = "main"
                    inputs      = inputs
                    outputs     = []
                    uniforms    = [ Buffer("BlaBuffer", uniforms |> List.map (fun v -> (v.Name, v.Type))) ]
                    arguments   = [] 
                    body        = removeUselessLets body
                }
            | _ ->
                failwith ""

    let seppEntry = 
        match seppCode with
            | Lambdas([inputs; uniforms], body) ->
                {
                    conditional = Some "Pixel"
                    entryName   = "main"
                    inputs      = inputs
                    outputs     = []
                    uniforms    = [ Buffer("BlaBuffer", uniforms |> List.map (fun v -> (v.Name, v.Type))) ]
                    arguments   = [] 
                    body        = removeUselessLets body
                }
            | _ ->
                failwith ""


    let compiled = Compiler.compile GLSLBackend.Instance { entries = [testEntry; seppEntry] }
    let config = { GLSL.Config.locations = true; GLSL.Config.perStageUniforms = false }
    let str = FShade.Imperative.GLSL.CModule.glsl config compiled
    printfn "%s" str
//    let m = Compiler.compile GLSLBackend.Instance test
//    let config = { GLSL.Config.locations = true; GLSL.Config.perStageUniforms = true }
//    let str = FShade.Imperative.GLSL.CModule.glsl config m
//    printfn "%s" str


    0