namespace FShade

open Aardvark.Base
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open System.Reflection
type Preprocessor private() =
    static member unroll() = ()

module ExprUtilities =

    let private (.+) (l : Range1i) (r : Range1i) =
        let max =
            if l.Max < 0 || r.Max < 0 then -1
            else l.Max + r.Max

        let min =
            if l.Min < 0 || r.Min < 0 then -1
            else l.Min + r.Min

        Range1i(min, max)

    let private (.|) (l : Range1i) (r : Range1i) =
        let max =
            if l.Max < 0 || r.Max < 0 then -1
            else max l.Max r.Max

        let min =
            if l.Min < 0 || r.Min < 0 then -1
            else min l.Min r.Min

        Range1i(min, max)


    let private isValue (e : Expr) =
        match e with
            | Value _ -> true
            | _ -> false

    let private getValue(e : Expr) =
        match e with
            | Value(o,_) -> o
            | _ -> failwith "not a constant"

    let rec evaluateConstants (constants : Map<Var, obj>) (e : Expr) =
        match e with
            | Sequential(Sequential(a, (Call(None, MethodQuote <@ Preprocessor.unroll : unit -> unit @> _, []) as b)), c) ->
                Expr.Sequential(a, Expr.Sequential(b,c)) |> evaluateConstants constants

            | Sequential((Call(None, MethodQuote <@ Preprocessor.unroll : unit -> unit @> _, []) as a), Sequential(b,c)) ->
                Expr.Sequential(Expr.Sequential(a,b), c) |> evaluateConstants constants

            // a value remains a value
            | Value(v, t) -> 
                e

            // a variable is constant here iff we know a constant value
            | Var(v) ->
                match Map.tryFind v constants with
                    | Some value -> Expr.Value(value, v.Type)
                    | None -> e

            // remove let-expressions whenever the value is constant
            // and the variable is not mutable
            | Let(v, expr, body) ->
                let expr = evaluateConstants constants expr
                match expr with
                    | Value(o,t) -> 
                        if v.IsMutable then Expr.Let(v, expr, evaluateConstants constants body)
                        else evaluateConstants (Map.add v o constants) body
                    | _ -> 
                        Expr.Let(v, expr, evaluateConstants constants body)

            // calls are constant iff all arguments are constant
            | Call(target, mi, args) ->
                let target =
                    match target with
                        | Some t -> Some (evaluateConstants constants t)
                        | None -> None

                let args = args |> List.map (evaluateConstants constants)

                let allConstant = 
                    match target with
                        | Some t -> t :: args |> List.forall isValue
                        | None -> args |> List.forall isValue
                        
                // only evaluate operators (TODO: find a good heuristic)
                if allConstant && mi.Name.StartsWith "op_" then
                    try
                        let result = 
                            match target with
                                | Some t -> mi.Invoke(getValue t, args |> List.map getValue |> List.toArray)
                                | None -> mi.Invoke(null, args |> List.map getValue |> List.toArray)
                        Expr.Value(result, e.Type)

                    with _ ->
                        match target with
                            | Some t -> Expr.Call(t, mi, args)
                            | None -> Expr.Call(mi, args)
                else
                    match target with
                        | Some t -> Expr.Call(t, mi, args)
                        | None -> Expr.Call(mi, args)


            // if then else is a little tricky
            | IfThenElse(cond, i, e) ->
                match evaluateConstants constants cond with

                    | Call(None, Method("op_Equality",_), ([Value(value,_);Var(v)]|[Var(v);Value(value,_)])) when not v.IsMutable ->
                        Expr.IfThenElse(cond, evaluateConstants (Map.add v value constants) i, evaluateConstants constants e)

                    | Call(None, Method("op_Inequality",_), ([Value(value,_);Var(v)]|[Var(v);Value(value,_)])) when not v.IsMutable ->
                        Expr.IfThenElse(cond, evaluateConstants constants i, evaluateConstants (Map.add v value constants) e)

                    | Value(v,_) -> 
                        if unbox v then evaluateConstants constants i
                        else evaluateConstants constants e

                    | cond ->
                        Expr.IfThenElse(cond, evaluateConstants constants i, evaluateConstants constants e)

            | WhileLoop(guard, body) ->
                let guard = evaluateConstants constants guard
                match guard with
                    | Value(v,_) ->
                        if unbox v then failwith "nontermination detected"
                        else Expr.Value(())
                    | guard ->
                        Expr.WhileLoop(guard, evaluateConstants constants body)

            | Sequential(Call(None, MethodQuote <@ Preprocessor.unroll : unit -> unit @> _, []), ForIntegerRangeLoop(v, s, e, body)) ->
                let s = evaluateConstants constants s
                let e = evaluateConstants constants e

                match s, e with
                    | Value(s,_), Value(e,_) ->
                        let s = unbox<int> s
                        let e = unbox<int> e

                        if e < s then 
                            Expr.Value(())
                        else
                            let mutable res = evaluateConstants (Map.add v (s :> obj) constants) body
                            for i in s + 1 .. e do
                                res <- Expr.Sequential(res, evaluateConstants (Map.add v (i :> obj) constants) body)

                            res
                    | _ ->
                        Log.warn "[FShade] could not unroll loop"
                        Expr.ForIntegerRangeLoop(v, s, e, evaluateConstants constants body)

            | Sequential(a,b) ->
                let a = evaluateConstants constants a
                let b = evaluateConstants constants b
                Expr.Sequential(a, b)
  

            | ForIntegerRangeLoop(v, s, e, body) ->
                let s = evaluateConstants constants s
                let e = evaluateConstants constants e
                Expr.ForIntegerRangeLoop(v, s, e, evaluateConstants constants body)

            | ExprShape.ShapeCombination(o, args) ->
                let args = args |> List.map (evaluateConstants constants)
                ExprShape.RebuildShapeCombination(o, args)

            | ExprShape.ShapeLambda(v, b) ->
                Expr.Lambda(v, evaluateConstants constants b)

            | ExprShape.ShapeVar(v) ->
                match Map.tryFind v constants with
                    | Some value -> Expr.Value(value, v.Type)
                    | None -> e

                

    let rec estimateNumberOfCallsTo (mi : MethodInfo) (e : Expr) =
        
        match e with
            | Call(t, m, args) ->
                let t = match t with | Some t -> estimateNumberOfCallsTo mi t | _ -> Range1i(0,0)
                let r = t .+ (args |> List.fold (fun s e -> s .+ estimateNumberOfCallsTo mi e) (Range1i(0,0)))
                if m = mi then Range1i(1 + r.Min, 1 + r.Max)
                else r

            | IfThenElse(cond, i, e) ->
                let i' = estimateNumberOfCallsTo mi i
                let e' = estimateNumberOfCallsTo mi e
                let c' = estimateNumberOfCallsTo mi cond

                c' .+ (i' .| e')


            | ForIntegerRangeLoop(v, Value((:? int as min), _), Value((:? int as max), _), body) ->
                let inner = estimateNumberOfCallsTo mi body

                let cnt = 1 + max - min
                Range1i(cnt * inner.Min, cnt * inner.Max)

            | ForIntegerRangeLoop(v, min, max, body) ->
                let min' = estimateNumberOfCallsTo mi min
                let max' = estimateNumberOfCallsTo mi max
                let body' = estimateNumberOfCallsTo mi body

                if body'.Min = 0 && body'.Max = 0 then min' .+ max'
                else Range1i(min'.Min + max'.Min, -1)


            | WhileLoop(cond, body) ->
                let cond = estimateNumberOfCallsTo mi cond
                let inner = estimateNumberOfCallsTo mi body

                if cond.Min = 0 && cond.Max = 0 then
                    if inner.Min = 0 && inner.Max = 0 then Range1i(0,0)
                    else Range1i(0,-1)
                else
                    Range1i(cond.Min, -1)

            | Sequential(l, r) ->
                let l' = estimateNumberOfCallsTo mi l
                let r' = estimateNumberOfCallsTo mi r
                l' .+ r'

            | Let(v,e,b) ->
                let e' = estimateNumberOfCallsTo mi e
                let b' = estimateNumberOfCallsTo mi b
                e' .+ b'

            | NewObject(_, args) | NewRecord(_, args) | NewTuple(args) | NewUnionCase(_, args) | NewArray(_, args) ->
                args |> List.fold (fun s e -> s .+ estimateNumberOfCallsTo mi e) (Range1i(0,0))

            | VarSet(_, e) | FieldGet(Some e, _) | UnionCaseTest(e, _) | TupleGet(e, _) ->
                estimateNumberOfCallsTo mi e

            | PropertyGet(target, _, args) ->
                let args' = args |> List.fold (fun s e -> s .+ estimateNumberOfCallsTo mi e) (Range1i(0,0))
                let target' = match target with | Some t -> estimateNumberOfCallsTo mi t | None -> Range1i(0,0)
                args' .+ target'
                
            | PropertySet(target, _, args, value) ->
                let args' = args |> List.fold (fun s e -> s .+ estimateNumberOfCallsTo mi e) (Range1i(0,0))
                let target' = match target with | Some t -> estimateNumberOfCallsTo mi t | None -> Range1i(0,0)
                let value' = estimateNumberOfCallsTo mi value
                args' .+ target' .+ value'

            | Value(_,_) | Var(_) | Application(_,_) | Lambda(_,_) ->
                Range1i(0,0)

            | Coerce(a,b) -> estimateNumberOfCallsTo mi a

            | _ ->
                Range1i(-1,-1)