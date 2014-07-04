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

    [<AutoOpen>]
    module Uniforms =
        open FShade.Parameters.Uniforms
        open Aardvark.Base.TypeInfo.Patterns

        let (|Uniform|_|) (e : Expr) =
            match detectUniform e with
                | Some u -> Uniform(u) |> Some
                | None -> 
                    match e with

                        | PropertyGet(None, pi, []) ->
                            match pi.Type with
                                | SamplerType(_) ->
                                    match Expr.tryEval e with
                                        | Some sam ->
                                            match sam with
                                                | :? Sampler2d as sam ->
                                                    let tex : ShaderTexture2D = sam.Texture
                                                    let textureUniform = Attribute(tex.Scope, typeof<ShaderTexture2D>, tex.Semantic)
                                                    Uniform(textureUniform) |> Some
                                                | _ -> None
                                        | None -> None
                                | _ -> None

                        | Call(None, Method("op_Dynamic", [UniformScopeType; String]), [scope; Value(s,_)]) ->
                            match Expr.tryEval scope with
                                | Some scope ->
                                    let scope = scope |> unbox
                                    Uniform(Attribute(scope, e.Type, s |> unbox<string>)) |> Some
                                | None ->
                                    None

                        | PropertyGet(Some scope, p, []) when scope.Type = typeof<UniformScope> ->
                            try
                                match Expr.tryEval scope with
                                    | Some scope ->
                                        let result = p.GetValue(scope, [||])
                                        match tryExtractUniformFromValue result with
                                            | Some uniform ->
                                                Uniform(uniform) |> Some
                                            | _ -> None
                                    | None ->
                                        None
                            with :? TargetInvocationException as ex ->
                                match ex.InnerException with
                                    | :? SemanticException as s -> Uniform(Attribute(s.Scope, p.PropertyType, s.Semantic)) |> Some
                                    | _ -> None

                        | Call(None, m, [scope]) when scope.Type = typeof<UniformScope> ->
                            try
                                match Expr.tryEval scope with
                                    | Some scope ->
                                        m.Invoke(null, [| scope |]) |> ignore
                                        None
                                    | None ->
                                        None
                            with :? TargetInvocationException as ex ->
                                match ex.InnerException with
                                    | :? SemanticException as s -> Uniform(Attribute(s.Scope, m.ReturnType, s.Semantic)) |> Some
                                    | _ -> None

                        | _ -> None

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
