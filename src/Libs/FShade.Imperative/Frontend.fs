namespace FShade.Imperative

open System
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection

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

type ParameterKind =
    | Input         = 0
    | Output        = 1
    | Uniform       = 2
    | Argument      = 3

type EntryParameter =
    {
        paramType           : Type
        paramName           : string
        paramSemantic       : string
        paramDecorations    : Set<ParameterDecoration>
    }

type ShaderStageDescription =
    {
        prev : Option<ShaderStage>
        self : ShaderStage
        next : Option<ShaderStage>
    }

[<RequireQualifiedAccess>]
type EntryDecoration =
    | Stages of ShaderStageDescription
    | InputTopology of InputTopology
    | OutputTopology of OutputTopology


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

        static let readInput        = find "ReadInput" [| typeof<ParameterKind>; typeof<string> |]
        static let readInputIndex   = find "ReadInput" [| typeof<ParameterKind>; typeof<string>; typeof<int> |]
        static let writeOutputs     = find "WriteOutputs" [| typeof<array<string * obj>> |]

        static member internal ReadInputMeth = readInput
        static member internal ReadInputIndexedMeth = readInputIndex
        static member internal WriteOutputsMeth = writeOutputs

        static member ReadInput<'a>(kind : ParameterKind, name : string) : 'a =
            failwith "[FShade] cannot read inputs in host-code"

        static member ReadInput<'a>(kind : ParameterKind, name : string, index : int) : 'a =
            failwith "[FShade] cannot read inputs in host-code"

        static member WriteOutputs(values : array<string * obj>) : unit =
            failwith "[FShade] cannot write outputs in host-code"

    let private noneCtor, someCtor = 
        let t = typeof<Option<int>>
        let cases = FSharpType.GetUnionCases(t, true)
        let none = cases |> Array.find (fun c -> c.Name = "None")
        let some = cases |> Array.find (fun c -> c.Name = "Some")
        none, some


    type Expr with
        static member ReadInput<'a>(kind : ParameterKind, name : string) : Expr<'a> =
            let mi = IO.ReadInputMeth.MakeGenericMethod [| typeof<'a> |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name) ]) |> Expr.Cast

        static member ReadInput<'a>(kind : ParameterKind, name : string, index : Expr) : Expr<'a> =
            let mi = IO.ReadInputIndexedMeth.MakeGenericMethod [| typeof<'a> |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name); index ]) |> Expr.Cast

        static member ReadInput(kind : ParameterKind, t : Type, name : string) =
            let mi = IO.ReadInputMeth.MakeGenericMethod [| t |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name) ])

        static member ReadInput(kind : ParameterKind, t : Type, name : string, index : Expr) =
            let mi = IO.ReadInputIndexedMeth.MakeGenericMethod [| t |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name); index ])

        static member WriteOutputs(values : Map<string, Expr>) =
            let values =
                values 
                    |> Map.toList
                    |> List.map (fun (name, value) -> 
                        Expr.NewTuple [ Expr.Value name; Expr.Coerce(value, typeof<obj>) ]
                    )

            Expr.Call(
                IO.WriteOutputsMeth,
                [ Expr.NewArray(typeof<string * obj>, values) ]
            )

        static member WriteOutputs (outputs : list<string * Expr>) =
            let mutable map = Map.empty
            for (name, value) in outputs do
                match Map.tryFind name map with
                    | Some old ->
                        failwithf "[FShade] conflicting output-writes for semantic %s (%A vs. %A)" name old value
                    | _ ->
                        map <- Map.add name value map

            Expr.WriteOutputs map

    let (|ReadInput|_|) (e : Expr) =
        match e with
            | Call(None, mi, [ Value((:? ParameterKind as kind),_); Value((:? string as name),_) ]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = IO.ReadInputMeth ->
                Some(kind, name, None)

            | Call(None, mi, [ Value((:? ParameterKind as kind),_); Value((:? string as name),_); index ]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = IO.ReadInputIndexedMeth ->
                Some(kind, name, Some index)

            | _ ->
                None

    let (|WriteOutputs|_|) (e : Expr) =
        match e with
            | Call(none, mi, [NewArray(_,args)]) when mi = IO.WriteOutputsMeth ->
                let args =
                    args |> List.map (fun a ->
                        match a with
                            | NewTuple [String name;Coerce(value, _) ] ->
                                name, value
                            | _ ->  
                                failwithf "[FShade] ill-formed WriteOutputs argument: %A" a    
                    )

                Some (Map.ofList args)
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
                                return Expr.ReadInput(ParameterKind.Argument, e.Type, p.paramName)
                            | None ->
                                let parameter =
                                    {
                                        paramType = v.Type
                                        paramName = v.Name
                                        paramSemantic = v.Name
                                        paramDecorations = Set.empty
                                    }
                                do! State.put (Map.add v parameter s)
                                return Expr.ReadInput(ParameterKind.Argument, e.Type, parameter.paramName)

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
