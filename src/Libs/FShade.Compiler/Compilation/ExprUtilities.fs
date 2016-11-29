namespace FShade

open Aardvark.Base
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

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

//
//    let rec evaluateConstants (constants : Map<Var, obj>) (e : Expr) =
//        match e with
//            | Value(v, t) -> 
//                e
//
//            | Var(v) ->
//                match Map.tryFind v constants with
//                    | Some value -> Expr.Value(value, v.Type)
//                    | None -> e
//
//            | Let(v, expr, body) ->
//                let expr = evaluateConstants constants expr
//                match expr with
//                    | Value(o,t) -> 
//                        if v.IsMutable then evaluateConstants constants body
//                        else evaluateConstants (Map.add v o constants) body
//
//                    | _ -> 
//                        Expr.Let(v, expr, evaluateConstants constants body)
//
//            | IfThenElse(cond, i, e) ->
//                match evaluateConstants constants cond with
//                    | Call(None, Method("op_Equality",_), ([Value(value,_);Var(v)]|[Var(v);Value(value,_)])) when not v.IsMutable ->
//                        Expr.IfThenElse(cond, evaluateConstants (Map.add v value constants) i, evaluateConstants constants e)
//
//                    | Call(None, Method("op_Inequality",_), ([Value(value,_);Var(v)]|[Var(v);Value(value,_)])) when not v.IsMutable ->
//                        Expr.IfThenElse(cond, evaluateConstants constants i, evaluateConstants (Map.add v value constants) e)
//
//                    | Value(v,_) -> 
//                        if unbox v then evaluateConstants constants i
//                        else evaluateConstants constants e
//
//                    | cond ->
//                        Expr.IfThenElse(cond, evaluateConstants constants i, evaluateConstants constants e)
//
//            | WhileLoop(guard, body) ->
//                let guard = evaluateConstants constants guard
//                match guard with
//                    | Value(v,_) ->
//                        if unbox v then failwith "nontermination detected"
//                        else Expr.Value(())
//                    | guard ->
//                        Expr.WhileLoop(guard, evaluateConstants constants body)
//
//            | Sequential(Call(None, Method("unroll",_), []), ForIntegerRangeLoop(v, s, e, body)) ->
//                let s = evaluateConstants constants s
//                let e = evaluateConstants constants e
//                
//
//            | ForIntegerRangeLoop(v, s, e, body) ->
//                let s = evaluateConstants constants s
//                let e = evaluateConstants constants e
//                Expr.ForIntegerRangeLoop(v, s, e, evaluateConstants constants body)
//

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

                let cnt = max - min
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