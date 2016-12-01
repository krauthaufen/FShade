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

    open FShade.Parameters.Uniforms
    open Aardvark.Base.TypeInfo.Patterns

    module private Peano = 
        let private peanoTypes =
            let s = typedefof<S<_>>
            Seq.initInfinite id 
                |> Seq.scan (fun last _ -> s.MakeGenericType [|last|]) typeof<Z>
                |> Seq.cache

        let getPeanoType (i : int) =
            Seq.item i peanoTypes

        let getArrayType (i : int) (content : Type) =
            typedefof<Arr<_,_>>.MakeGenericType [| getPeanoType i; content |]
           



    let (|ArrayOf|_|) (t : Type) =
        if t.IsArray then Some(t.GetElementType())
        else None


    let (|ArrOf|_|) (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Arr<_,_>> then Some (t.GetGenericArguments().[1])
        else None

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
                                        let sam = sam |> unbox<ISampler>
                                        let tex = sam.Texture
                                        let textureUniform = SamplerUniform(sam.GetType(), tex.Semantic, pi.Name, sam.State)
                                        Uniform(textureUniform) |> Some
                                    | None -> None

                            | ArrayOf((SamplerType _ as t)) ->
                                match Expr.tryEval e with
                                    | Some sam ->
                                        let arr = sam |> unbox<Array>
                                        let samplers = 
                                            List.init arr.Length (fun i -> 
                                                let sam1 = arr.GetValue i |> unbox<ISampler>
                                                let tex = sam1.Texture
                                                tex.Semantic, sam1.State
                                            )

                                        let t = Peano.getArrayType arr.Length t
                                        Some (SamplerArray(t, arr.Length, pi.Name, samplers))
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

    let rec substituteUniforms' (f : Uniform -> Compiled<Expr, _>) (e : Expr) =
        transform {
            match e with
                | Uniform(u) -> 
                    let! v = f u //getUniform u
                    return v

                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapC (substituteUniforms' f)
                    return RebuildShapeCombination(o, args)

                | ShapeLambda(v,b) -> 
                    let! b = substituteUniforms' f b
                    return Expr.Lambda(v, b)

                | _ -> return e
        }

    let rec substituteUniforms (e : Expr) =
        e |> substituteUniforms' (fun u ->
            transform {
                let! u = getUniform u
                return Expr.Var u
            }
        )

    let rec substituteInputs (inputType : Type) (index : Option<Expr>) (e : Expr) =
        transform {
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

    let rec substituteInputAccess (v : Var) (index : Var) (e : Expr) =
        transform {
            match e with
                | MemberFieldGet(Var vi, fi) when vi = v ->
                    let! v = getInput (fi.Type.MakeArrayType()) fi.Semantic
                    return Expr.ArrayAccess(Expr.Var(v), Expr.Var(index))

                | ShapeCombination(o, args) ->
                    
                    let! args = args |> List.mapC (substituteInputAccess v index)
                    return RebuildShapeCombination(o, args)


                | ShapeLambda(vl,b) -> 
                    let! b = substituteInputAccess v index b
                    return Expr.Lambda(vl, b)

                | ShapeVar(v) ->
                    return e

            }

    let rec substituteOutputs (outputType : Type) (e : Expr) =
        transform {

            match e with
                | Output outputType (args) -> 

                    let! outputs = args |> List.collectC (fun ((sem,t),e) -> 
                        transform { 
                            let! v = getOutput e.Type sem t

                            if v.Type.IsArray then
                                match e with
                                    | NewArray(t,args) -> 
                                          let set = getMethodInfo <@ LanguagePrimitives.IntrinsicFunctions.SetArray @>
                                          let set = set.MakeGenericMethod [|t|]
                                          return args |> List.mapi (fun i ei -> 
                                            Expr.Call(set, [Expr.Var v; Expr.Value i; ei])
                                          )
                                    | _ -> return []
                            else
                                if sem = "Positions" then
                                    let! vi = getOutput e.Type "_Positions_" t
                                    return [
                                        Expr.VarSet (vi, Expr.Var(v))
                                        Expr.VarSet (v, e)
                                    ]
                                else
                                    return [Expr.VarSet (v,e)]
                        })


                    let result = outputs |> List.fold (fun a e -> Expr.Sequential(e, a)) (Expr.Value(()))
                    return result

                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapC (substituteOutputs outputType)
                    return RebuildShapeCombination(o, args)

                | ShapeLambda(v,b) -> 
                    let! b = substituteOutputs outputType b
                    return Expr.Lambda(v, b)

                | _ -> return e
        }
