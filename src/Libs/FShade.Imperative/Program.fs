
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
    
    let v = V4d(1.0, 3.0, 2.0, 4.0)
    let test = 
        Module.ofLambdas [
            "sepp", <@@ fun (m : M44d) (a : V4d) ->
                if m.M00 > 1.0 then
                    let x = Case3(1,2.0)
                    m * a + v
                else
                    let y = Case1 { a = 10; b = int a.Y }
                    match y with
                        | Case1 v ->
                            V4d(-a.X |> clamp 0.0 1.0, cos 0.0, float v.a, 0.0)
                        | Case2 a ->
                            V4d(1.0, 0.0, 0.0, 0.0)
                        | Case3 _ ->
                            V4d(1.0, 0.0, 0.0, 0.0)
            @@>
        ]

    let m = Compiler.compile GLSLBackend.Instance test
    let str = FShade.Imperative.GLSL.CModule.glsl m
    printfn "%s" str


    0