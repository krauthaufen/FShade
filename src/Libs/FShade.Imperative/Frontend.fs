namespace FShade.Imperative

open System
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Aardvark.Base
open FShade

[<RequireQualifiedAccess>]
type ParameterDecoration =
    | Interpolation of InterpolationMode
    | Memory of MemoryType
    | Const

type Uniform =
    {
        uniformType         : Type
        uniformName         : string
        uniformBuffer       : Option<string>
    }

type EntryParameter =
    {
        paramType           : Type
        paramName           : string
        paramSemantic       : string
        paramDecorations    : Set<ParameterDecoration>
    }

type ShaderStageDescription =
    {
        prev : Option<ShaderType>
        self : ShaderType
        next : Option<ShaderType>
    }

[<RequireQualifiedAccess>]
type EntryDecoration =
    | Stages of ShaderStageDescription
    | InputTopology of int


type EntryPoint =
    {
        conditional : Option<string>
        entryName   : string
        inputs      : list<EntryParameter>
        outputs     : list<EntryParameter>
        uniforms    : list<Uniform>
        arguments   : list<EntryParameter>
        body        : Expr
        decorations : list<EntryDecoration>
    }

[<AutoOpen>]
module ExpressionExtensions =
    open System.Reflection

    type private IO private() =
        static let allMethods = typeof<IO>.GetMethods(BindingFlags.NonPublic ||| BindingFlags.Static)

        static let find (name : string) (types : array<Type>) =
            allMethods |> Array.find (fun mi -> 
                if mi.Name = name then
                    let p = mi.GetParameters()
                    if p.Length = types.Length then
                        Array.forall2 (fun (p : ParameterInfo) (t : Type) -> isNull t || p.ParameterType.IsAssignableFrom t) p types
                    else
                        false
                else
                    false
            )

        static let readInput        = find "ReadInput" [| typeof<string> |]
        static let readInputIndex   = find "ReadInput" [| typeof<string>; typeof<int> |]
        static let writeOutput      = find "WriteOutput" [| typeof<string>; null |]
        static let writeOutputIndex = find "WriteOutput" [| typeof<string>; typeof<int>; null |]

        static member internal ReadInputMeth = readInput
        static member internal ReadInputIndexedMeth = readInputIndex
        static member internal WriteOutputMeth = writeOutput
        static member internal WriteOutputIndexedMeth = writeOutputIndex


        static member ReadInput<'a>(name : string) : 'a =
            failwith "[FShade] cannot read inputs in host-code"

        static member ReadInput<'a>(name : string, index : int) : 'a =
            failwith "[FShade] cannot read inputs in host-code"

        static member WriteOutput<'a>(name : string, value : 'a) : unit =
            failwith "[FShade] cannot write outputs in host-code"

        static member WriteOutput<'a>(name : string, index : int, value : 'a) : unit =
            failwith "[FShade] cannot write outputs in host-code"

    type Expr with
        static member ReadInput<'a>(name : string) : Expr<'a> =
            let mi = IO.ReadInputMeth.MakeGenericMethod [| typeof<'a> |]
            Expr.Call(mi, [ Expr.Value(name) ]) |> Expr.Cast

        static member ReadInput<'a>(name : string, index : Expr) : Expr<'a> =
            let mi = IO.ReadInputIndexedMeth.MakeGenericMethod [| typeof<'a> |]
            Expr.Call(mi, [ Expr.Value(name); index ]) |> Expr.Cast

        static member ReadInput(t : Type, name : string) =
            let mi = IO.ReadInputMeth.MakeGenericMethod [| t |]
            Expr.Call(mi, [ Expr.Value(name) ])

        static member ReadInput(t : Type, name : string, index : Expr) =
            let mi = IO.ReadInputIndexedMeth.MakeGenericMethod [| t |]
            Expr.Call(mi, [ Expr.Value(name); index ])

        static member WriteOutput(name : string, value : Expr) =
            let mi = IO.WriteOutputMeth.MakeGenericMethod [| value.Type |]
            Expr.Call(mi, [ Expr.Value(name); value ])

        static member WriteOutput(name : string, index : Expr, value : Expr) =
            let mi = IO.WriteOutputIndexedMeth.MakeGenericMethod [| value.Type |]
            Expr.Call(mi, [ Expr.Value(name); index; value ])

    let (|ReadInput|_|) (e : Expr) =
        match e with
            | Call(None, mi, [ Value((:? string as name),_) ]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = IO.ReadInputMeth ->
                Some(name, None)
            | Call(None, mi, [ Value((:? string as name),_); index ]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = IO.ReadInputIndexedMeth ->
                Some(name, Some index)
            | _ ->
                None

    let (|WriteOutput|_|) (e : Expr) =
        match e with
            | Call(None, mi, [ Value((:? string as name),_); value ]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = IO.WriteOutputMeth ->
                Some(name, None, value)
            | Call(None, mi, [ Value((:? string as name),_); index; value ]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = IO.WriteOutputIndexedMeth ->
                Some(name, Some index, value)
            | _ ->
                None

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EntryPoint =
    open Aardvark.Base.Monads.State
    open Microsoft.FSharp.Quotations.ExprShape
    
    let rec private substituteVarsWithReads (args : Set<Var>) (e : Expr) =
        state {
            match e with
                | ShapeVar(v) ->

                    if Set.contains v args then
                        let! s = State.get
                        
                        match Map.tryFind v s with
                            | Some p ->
                                return Expr.ReadInput(e.Type, p.paramName)
                            | None ->
                                let parameter =
                                    {
                                        paramType = v.Type
                                        paramName = v.Name
                                        paramSemantic = v.Name
                                        paramDecorations = Set.empty
                                    }
                                do! State.put (Map.add v parameter s)
                                return Expr.ReadInput(e.Type, parameter.paramName)

                    else
                        return e

                | ShapeLambda(v,b) ->
                    let! b = substituteVarsWithReads args b
                    return Expr.Lambda(v, b)
            
                | ShapeCombination(o, c) ->
                    let! c = c |> List.mapS (substituteVarsWithReads args)
                    return RebuildShapeCombination(o, c)
        }

    let ofLambda (name : string) (e : Expr) =
        let args, body = 
            match e with
                | Lambdas(args, body) ->
                    let args = 
                        match List.concat args with
                            | [v] when v.Type = typeof<unit> -> []
                            | a -> a

                    let parameters, body = substituteVarsWithReads (Set.ofList args) body |> State.run Map.empty
                    let args = 
                        args |> List.map (fun a -> 
                            match Map.tryFind a parameters with
                                | Some p -> p
                                | None -> { paramType = a.Type; paramName = a.Name; paramSemantic = a.Name; paramDecorations = Set.empty }
                        )
                    args, body
                | e ->
                    [], e

        {
            conditional = None
            entryName = name
            inputs = []
            outputs = []
            uniforms = []
            arguments = args
            body = body
            decorations = []
        }
      

type Module = { entries : list<EntryPoint> }
       
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Module =

    let ofLambda (name : string) (e : Expr) =
        { entries = [EntryPoint.ofLambda name e] }

    let ofLambdas (l : list<string * Expr>) =
        { entries = l |> List.map (uncurry EntryPoint.ofLambda) }
