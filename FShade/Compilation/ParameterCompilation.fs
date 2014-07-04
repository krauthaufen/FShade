namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Aardvark.Base
open FShade.Utils
open FShade.Compiler


[<AutoOpen>]
module ParameterCompilation =

    let rec substituteUniforms (e : Expr) =
        transform {
            match e with
                | Uniform(u) -> let! v = getUniform u
                                return Expr.Var v

                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapC substituteUniforms
                    return RebuildShapeCombination(o, args)

                | ShapeLambda(v,b) -> 
                    let! b = substituteUniforms b
                    return Expr.Lambda(v, b)

                | _ -> return e
        }

    let rec substituteInputs (inputType : Type) (index : Option<Expr>) (e : Expr) =
        transform {
            let inputName (f : MemberInfo) =
                let att = f.GetCustomAttributes<SemanticAttribute>(true) |> Seq.toList
                match att with
                    | x::_ -> x.Semantic
                    | _ -> f.Name

            match e with
                | Input inputType (t,sem) -> match index with
                                                | None -> let! v = getInput t sem
                                                          return Expr.Var(v)
                                                | Some i -> let! v = getInput (t.MakeArrayType()) sem
                                                            return Expr.ArrayAccess(Expr.Var(v), i)

                | ShapeCombination(o, args) ->
                    
                    let! args = args |> List.mapC (substituteInputs inputType index)
                    return RebuildShapeCombination(o, args)


                | ShapeLambda(v,b) -> 
                    let! b = substituteInputs inputType index b
                    return Expr.Lambda(v, b)

                | _ -> return e
            }

    let rec substituteOutputs (outputType : Type) (e : Expr) =
        transform {
            let getVar name t m =
                match Map.tryFind name m with
                    | Some(v) -> (m, v)
                    | None -> let v = Var(name + "Out", t)
                              (Map.add name v m, v)
            match e with
                | Output outputType (args) -> 

                    let! outputs = args |> List.mapC (fun ((t,n),e) -> 
                        transform { 
                            let! v = getOutput e.Type t n
                            return (v,e)
                        })


                    let result = outputs |> List.fold (fun a (v,e) -> Expr.Sequential(Expr.VarSet(v,e), a)) (Expr.Value(()))
                    return result

                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapC (substituteOutputs outputType)
                    return RebuildShapeCombination(o, args)

                | ShapeLambda(v,b) -> 
                    let! b = substituteOutputs outputType b
                    return Expr.Lambda(v, b)

                | _ -> return e
        }
