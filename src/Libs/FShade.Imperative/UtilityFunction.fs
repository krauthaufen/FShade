namespace FShade.Imperative

open System
open System.Reflection
open System.Runtime.CompilerServices
open FShade.Imperative
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Aardvark.Base
open FShade

[<AutoOpen>]
module private UtilityFunctionHelpers =
    let cleanHash (str : string) =
        let str : string = str.Replace("+", "_")
        let str : string = str.Replace("/", "00")
        let str : string = str.Replace("=", "")
        str

[<CustomEquality; NoComparison>]
type UtilityFunction =
    {
        functionId          : string
        functionName        : string
        functionArguments   : list<Var>
        functionBody        : Expr
        functionMethod      : Option<MethodBase>
        functionTag         : obj
        functionIsInline    : bool
    }
    member x.uniqueName = x.functionName + "_" + cleanHash x.functionId
    member x.returnType = x.functionBody.Type
    
    override x.GetHashCode() = x.uniqueName.GetHashCode()
    override x.Equals(o) =
        match o with
            | :? UtilityFunction as o ->
                x.functionId = o.functionId &&
                x.functionName = o.functionName &&
                List.forall2 (fun (l : Var) (r : Var) -> l.Name = r.Name && l.Type = r.Type && l.IsMutable = r.IsMutable) x.functionArguments o.functionArguments
            | _ ->
                false

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UtilityFunction =
    let tryCreate (m : MethodBase) =
        let intrinsic = m.GetCustomAttributes<IntrinsicAttribute>(true) |> Seq.isEmpty |> not
        if intrinsic then
            None
        else
            match ExprWorkardound.TryGetReflectedDefinition m with
                | Some e ->
                    let isInline = m.GetCustomAttributes(typeof<InlineAttribute>, true) |> Seq.isEmpty |> not

                    match e with
                        | Lambdas(args, body) ->
                            let args = List.concat args

                            let args, body = 
                                if m.IsStatic then 
                                    args, body
                                else
                                    match args with
                                        | this :: args ->
                                            let mThis = Var(this.Name, this.Type, true)
                                            let body = body.Substitute(fun vi -> if vi = this then Some (Expr.Var mThis) else None)
                                            mThis :: args, body
                                        | _ ->
                                            args, body

                            let args =
                                args |> List.filter (fun a -> a.Type <> typeof<unit>)

                            Some {
                                functionId = Expr.ComputeHash e
                                functionName = methodName m
                                functionArguments = args
                                functionBody = body
                                functionMethod = Some m
                                functionTag = null
                                functionIsInline = isInline
                            }
                        | _ ->
                            None
                | None ->
                    None

    let ofMethodBase (m : MethodBase) =
        match tryCreate m with
            | Some f -> f
            | None -> failwithf "[FShade] utility function %A is not reflectable" m

    let map (mapping : Expr -> Expr) (f : UtilityFunction) =
        let b = mapping f.functionBody

        let lam = Expr.Lambdas(f.functionArguments, b)

        { f with functionBody = b; functionId = Expr.ComputeHash lam }

   
[<AutoOpen>]
module UtilityFunctionExpressionExtensions =
    
    type Expr with

        static member CallFunction(f : UtilityFunction, args : list<Expr>) =
            assert ( List.forall2 (fun (v : Var) (e : Expr) -> v.Type.IsAssignableFrom e.Type) f.functionArguments args )

            let args =
                match args with
                    | [] -> Expr.Unit
                    | args -> Expr.NewTuple args

            Expr.Coerce(
                Expr.NewTuple [Expr.Value "__FUNCTIONCALL__"; Expr.Value f; args ], 
                f.returnType
            )

    [<return: Struct>]
    let (|CallFunction|_|) (e : Expr) =
        match e with
        | Coerce(NewTuple [ String "__FUNCTIONCALL__"; Value((:? UtilityFunction as f), _); args], t) when t = f.returnType ->
            match args with
            | Unit -> ValueSome(f,[])
            | NewTuple args -> ValueSome(f, args)
            | _ -> ValueNone
        | _ ->
            ValueNone



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

            | Let(v, e, b) ->
                numberOfCalls mi e <+> numberOfCalls mi b

            | ShapeVar v -> zero
            | ShapeLambda(v, b) -> numberOfCalls mi b
            | ShapeCombination(o, args) -> args |> List.fold (fun r e -> r <+> numberOfCalls mi e) zero

    static let rec substituteReads (substitute : ParameterKind -> Type -> string -> Option<Expr> -> Option<ShaderSlot> -> Option<Expr>) (e : Expr) =
        match e with
            | ReadInputOrRaytracingData(kind, name, index, slot) ->
                let index = index |> Option.map (substituteReads substitute)
                match substitute kind e.Type name index slot with
                    | Some e -> e
                    | None -> 
                        match index with
                            | Some index -> Expr.ReadInput(kind, e.Type, name, index, slot)
                            | None -> e

            | CallFunction(utility, args) ->
                let args = args |> List.map (substituteReads substitute)
                let utility = utility |> UtilityFunction.map (substituteReads substitute)
                Expr.CallFunction(utility, args)

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
    static member SubstituteReads (e : Expr, substitute : ParameterKind -> Type -> string -> Option<Expr> -> Option<ShaderSlot> -> Option<Expr>) =
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
    let inline substituteReads f (e : Expr) = e.SubstituteReads f
    let inline substituteWrites (f : Map<string, Option<Expr> * Expr> -> Option<Expr>) (e : Expr) = e.SubstituteWrites f
    let getAffectedOutputsMap (e : Expr) = Affected.getAffectedOutputsMap e
    let computeCallCount (mi : MethodInfo) (e : Expr) = e.ComputeCallCount(mi)
