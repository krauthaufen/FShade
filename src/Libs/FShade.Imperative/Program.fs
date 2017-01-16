
open System
open System.Reflection
open Aardvark.Base
open FShade.Imperative
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

type MyUnion =
    | Case1 of int
    | Case2 of float
    | Case3 of int * float

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

[<EntryPoint>]
let main args =
    
    let v = V4d(1.0, 3.0, 2.0, 4.0)
    let test = 
        <@
            fun (m : M44d) (a : V4d) ->
                if m.M00 > 1.0 then
                    let x = Case3(1,2.0)
                    m * a + v
                else
                    V4d(a.X |> clamp 0.0 1.0, cos 0.0, 0.0, 0.0)
        @>

    match test with
        | Lambdas(v,b) ->
            let s = Compiler.toCStatement true b
            let mutable state = Compiler.emptyState GLSLBackend.Instance
            let s = s.Run(&state)

            let str = FShade.Imperative.GLSL.CStatement.glsl s

            printfn "%s" str
        | _ ->
            printfn "ERROR"

    0