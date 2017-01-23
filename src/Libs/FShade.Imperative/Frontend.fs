namespace FShade.Imperative

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.CompilerServices

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
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
    | OutputTopology of OutputTopology * int
    | TessControlPassThru of list<EntryParameter>

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
    

    type ShaderIO private() =
        static let allMethods = typeof<ShaderIO>.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)

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
        static let writeOutputs     = find "WriteOutputs" [| typeof<array<string * int * obj>> |]

        static member internal ReadInputMeth = readInput
        static member internal ReadInputIndexedMeth = readInputIndex
        static member internal WriteOutputsMeth = writeOutputs

        static member ReadInput<'a>(kind : ParameterKind, name : string) : 'a =
            failwith "[FShade] cannot read inputs in host-code"

        static member ReadInput<'a>(kind : ParameterKind, name : string, index : int) : 'a =
            failwith "[FShade] cannot read inputs in host-code"

        static member WriteOutputs(values : array<string * int * obj>) : unit =
            failwith "[FShade] cannot write outputs in host-code"


  
    type Expr with
        static member ReadInput<'a>(kind : ParameterKind, name : string) : Expr<'a> =
            let mi = ShaderIO.ReadInputMeth.MakeGenericMethod [| typeof<'a> |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name) ]) |> Expr.Cast

        static member ReadInput<'a>(kind : ParameterKind, name : string, index : Expr) : Expr<'a> =
            let mi = ShaderIO.ReadInputIndexedMeth.MakeGenericMethod [| typeof<'a> |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name); index ]) |> Expr.Cast

        static member ReadInput(kind : ParameterKind, t : Type, name : string) =
            let mi = ShaderIO.ReadInputMeth.MakeGenericMethod [| t |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name) ])

        static member ReadInput(kind : ParameterKind, t : Type, name : string, index : Expr) =
            let mi = ShaderIO.ReadInputIndexedMeth.MakeGenericMethod [| t |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name); index ])

        static member WriteOutputs(values : Map<string, Option<Expr> * Expr>) =
            let values =
                values 
                    |> Map.toList
                    |> List.map (fun (name, (index, value)) -> 
                        let index = index |> Option.defaultValue (Expr.Value -1)
                        if value.Type = typeof<obj> then
                            Expr.NewTuple [ Expr.Value name; index; value ]
                        else 
                            Expr.NewTuple [ Expr.Value name; index; Expr.Coerce(value, typeof<obj>) ]
                    )

            Expr.Call(
                ShaderIO.WriteOutputsMeth,
                [ Expr.NewArray(typeof<string * int * obj>, values) ]
            )

        static member WriteOutputs (outputs : list<string * Option<Expr> * Expr>) =
            let mutable map = Map.empty
            for (name, index, value) in outputs do
                match Map.tryFind name map with
                    | Some old ->
                        failwithf "[FShade] conflicting output-writes for semantic %s (%A vs. %A)" name old value
                    | _ ->
                        map <- Map.add name (index, value) map

            Expr.WriteOutputs map

    let (|ReadInput|_|) (e : Expr) =
        match e with
            | Call(None, mi, [ Value((:? ParameterKind as kind),_); Value((:? string as name),_) ]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = ShaderIO.ReadInputMeth ->
                Some(kind, name, None)

            | Call(None, mi, [ Value((:? ParameterKind as kind),_); Value((:? string as name),_); index ]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = ShaderIO.ReadInputIndexedMeth ->
                Some(kind, name, Some index)

            | _ ->
                None

    let (|WriteOutputs|_|) (e : Expr) =
        match e with
            | Call(none, mi, [NewArray(_,args)]) when mi = ShaderIO.WriteOutputsMeth ->
                let args =
                    args |> List.map (fun a ->
                        match a with
                            | NewTuple [String name; index; Coerce(value, _) ] ->
                                match index with
                                    | Int32 -1 -> name, (None, value)
                                    | _ -> name, (Some index, value)
                            | _ ->  
                                failwithf "[FShade] ill-formed WriteOutputs argument: %A" a    
                    )

                Some (Map.ofList args)
            | _ -> 
                None

    type MethodInfo with
        static member WriteOutputs = ShaderIO.WriteOutputsMeth
        static member ReadInput = ShaderIO.ReadInputMeth
        static member ReadInputIndexed = ShaderIO.ReadInputIndexedMeth


module private Affected =
    open Aardvark.Base.Monads.State
    
    type State = { dependencies : Map<Var, Set<string>>; affected : Map<string, Set<string>> }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        let add (v : Var) (deps : Set<string>) = 
            State.modify (fun s -> 
                match Map.tryFind v s.dependencies with
                    | Some old -> { s with dependencies = Map.add v (Set.union old deps) s.dependencies }
                    | None -> { s with dependencies = Map.add v deps s.dependencies }
            )

        let affects (o : string) (deps : Set<string>) = 
            State.modify (fun s -> 
                match Map.tryFind o s.affected with
                    | Some old -> { s with affected = Map.add o (Set.union old deps) s.affected }
                    | None -> { s with affected = Map.add o deps s.affected }
            )

        let dependencies (v : Var) =
            State.get |> State.map (fun s -> 
                match Map.tryFind v s.dependencies with
                    | Some d -> d
                    | _ -> Set.empty
            )
       
    let rec usedInputsS (e : Expr) : State<State, Set<string>> =
        state {
            match e with
                | ReadInput(ParameterKind.Input, name, idx) ->
                    match idx with
                        | Some idx -> 
                            let! used = usedInputsS idx
                            return Set.add name used
                        | None ->
                            return Set.singleton name

                | WriteOutputs values ->
                    let! values = 
                        values |> Map.toList |> List.mapS (fun (name, (index, value)) ->
                            state {
                                let! vUsed = usedInputsS value
                                do! State.affects name vUsed
                                match index with
                                    | Some index -> 
                                        let! iUsed = usedInputsS index
                                        do! State.affects name iUsed
                                        return Set.union iUsed vUsed
                                    | _ ->
                                        return vUsed
                            }
                        )   
                    return Set.unionMany values

                | VarSet(v, e) ->
                    let! eUsed = usedInputsS e
                    do! State.add v eUsed
                    return eUsed
                    
                | Let(v, e, b) ->
                    let! eUsed = usedInputsS e
                    do! State.add v eUsed
                    let! bUsed = usedInputsS b
                    return Set.union eUsed bUsed

                | ShapeVar v -> 
                    return! State.dependencies v

                | ShapeLambda(v, b) ->
                    return! usedInputsS b

                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapS usedInputsS
                    return Set.unionMany args

        }

    let getAffectedOutputsMap (e : Expr) =
        let s, _ = usedInputsS e |> State.run { dependencies = Map.empty; affected = Map.empty }
        s.affected

[<AbstractClass; Sealed; Extension>]
type ExpressionSubstitutionExtensions private() =
    static let zero = Range1i(0,0)

    static let (<+>) (l : Range1i) (r : Range1i) =
        Range1i(l.Min + r.Min, l.Max + r.Max)

    static let (<|>) (l : Range1i) (r : Range1i) =
        Range1i(min l.Min r.Min, max l.Max r.Max)

    static let (<*>) (l : Range1i) (r : int) =
        Range1i(l.Min * r, l.Max * r)

    static let rec numberOfCalls (mi : MethodInfo) (e : Expr) =
        match e with
            | Call(t, m, args) ->
                if m = mi || (m.IsGenericMethod && m.GetGenericMethodDefinition() = mi) then
                    Range1i(1,1)
                else
                    let args = 
                        match t with
                            | Some t -> (t :: args)
                            | None -> args
                    args |> List.fold (fun r e -> r <+> numberOfCalls mi e) zero

            | Sequential(l, r) ->
                numberOfCalls mi l <+> numberOfCalls mi r

            | IfThenElse(cond, i, e) ->
                numberOfCalls mi cond <+> (numberOfCalls mi i <|> numberOfCalls mi e)

            | ForInteger(v, first, step, last, body) ->
                let minimal = 
                    numberOfCalls mi first <+>
                    numberOfCalls mi step <+>
                    numberOfCalls mi last

                let inner = numberOfCalls mi body

                match first, step, last with
                    | Int32 first, Int32 step, Int32 last -> 
                        let cnt = [first .. step .. last] |> List.length
                        minimal <+> inner <*> cnt
                    | _ ->
                        if inner.Max = 0 then
                            minimal
                        else
                            Range1i(minimal.Min, Int32.MaxValue)

            | WhileLoop(guard, body) ->
                let minimal = numberOfCalls mi guard
                let inner = numberOfCalls mi body
                if inner.Max = 0 && minimal.Max = 0 then
                    Range1i(0,0)
                else
                    Range1i(0, Int32.MaxValue)

            | ShapeVar v -> zero
            | ShapeLambda(v, b) -> zero
            | ShapeCombination(o, args) -> args |> List.fold (fun r e -> r <+> numberOfCalls mi e) zero

    static let rec substituteReads (substitute : ParameterKind -> Type -> string -> Option<Expr> -> Option<Expr>) (e : Expr) =
        match e with
            | ReadInput(kind, name, index) ->
                let index = index |> Option.map (substituteReads substitute)
                match substitute kind e.Type name index with
                    | Some e -> e
                    | None -> 
                        match index with
                            | Some index -> Expr.ReadInput(kind, e.Type, name, index)
                            | None -> e

            | ShapeLambda(v,b) -> Expr.Lambda(v, substituteReads substitute b)
            | ShapeVar _ -> e
            | ShapeCombination(o, args) ->
                RebuildShapeCombination(o, args |> List.map (substituteReads substitute))

    static let rec substituteWrites (substitute : Map<string, Option<Expr> * Expr> -> Option<Expr>) (e : Expr) =
        match e with
            | WriteOutputs values ->
                match substitute values with
                    | Some e -> e
                    | None ->
                        let newValues = 
                            values |> Map.map (fun _ (index, value) -> 
                                let value = substituteWrites substitute value
                                let index = index |> Option.map (substituteWrites substitute)
                                index, value
                            )
                        Expr.WriteOutputs newValues

            | ShapeLambda(v,b) -> Expr.Lambda(v, substituteWrites substitute b)
            | ShapeVar _ -> e
            | ShapeCombination(o, args) ->
                RebuildShapeCombination(o, args |> List.map (substituteWrites substitute))
 
        
    [<Extension>]
    static member SubstituteReads (e : Expr, substitute : ParameterKind -> Type -> string -> Option<Expr> -> Option<Expr>) =
        substituteReads substitute e
        
    [<Extension>]
    static member SubstituteWrites (e : Expr, substitute : Map<string, Option<Expr> * Expr> -> Option<Expr>) =
        substituteWrites substitute e

    [<Extension>]
    static member ComputeCallCount (e : Expr, mi : MethodInfo) =
        numberOfCalls mi e


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Expr =
    let inline substitute (f : Var -> Option<Expr>) (e : Expr) = e.Substitute f
    let inline substituteReads (f : ParameterKind -> Type -> string -> Option<Expr> -> Option<Expr>) (e : Expr) = e.SubstituteReads f
    let inline substituteWrites (f : Map<string, Option<Expr> * Expr> -> Option<Expr>) (e : Expr) = e.SubstituteWrites f
    let getAffectedOutputsMap (e : Expr) = Affected.getAffectedOutputsMap e
    let computeCallCount (mi : MethodInfo) (e : Expr) = e.ComputeCallCount(mi)

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
