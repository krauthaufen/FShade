namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection
open Aardvark.Base
open FShade.Utils
open FShade.Compiler
open Aardvark.Base.TypeInfo.Patterns

[<AutoOpen>]
module SequentialComposition =


    let private checkShaderConsistency (s : Shader) =
        let free = s.body.GetFreeVars() |> Set.ofSeq
        let uniformSet = s.uniforms |> Seq.map(fun (_,u) -> u) |> Set.ofSeq

        let mapValue (v : Var) (m : Map<string, ShaderInput>) =
            match Map.tryFindKey (fun _ vi -> vi.var = v) m with
                | Some _ -> true
                | None -> false

        let mapValueOutput (v : Var) (m : Map<string, Option<string> * Var>) =
            match Map.tryFindKey (fun _ (_,vi) -> vi = v) m with
                | Some _ -> true
                | None -> false

        let notFound = free |> Seq.toList |> List.choose (fun v -> if not (mapValue v s.inputs || mapValueOutput v s.outputs || Set.contains v uniformSet) then Some v else None)

        if notFound.Length > 0 then
            failwithf "shader contains free variables which are no in-/outputs %A" notFound

        s

    let rec private outputsToVariables (mapping : Map<Var, Var>) (hidden : Set<Var>) (cont : Expr) (e : Expr) =
        match e with

            | Sequential(Sequential(s0,s1),s2) ->
                outputsToVariables mapping  hidden cont (Expr.Sequential(s0, Expr.Sequential(s1,s2)))

            | Sequential(VarSet(v,value), Value(_, Unit)) ->
                match Map.tryFind v mapping with
                    | Some r -> Expr.Let(r, value, cont)
                    | None -> 
                        if Set.contains v hidden then
                            cont
                        else 
                            Expr.Sequential(Expr.VarSet(v,value), cont)

            | Sequential(VarSet(v,value), b) ->
                let b = outputsToVariables mapping hidden cont b
                match Map.tryFind v mapping with
                    | Some r -> Expr.Let(r, value, b)
                    | None -> 
                        if Set.contains v hidden then
                            b
                        else 
                            Expr.Sequential(Expr.VarSet(v,value), b)

            | VarSet(v,value) ->
                match Map.tryFind v mapping with
                    | Some r -> Expr.Let(r, value, cont)
                    | None -> 
                        if Set.contains v hidden then
                            cont
                        else 
                            Expr.Sequential(Expr.VarSet(v,value), cont)

            | Sequential(l, r) ->
                Expr.Sequential(l, outputsToVariables mapping hidden cont r)


            | ShapeLambda(v,b) -> Expr.Lambda(v, outputsToVariables mapping hidden cont b)
            | ShapeCombination(o, args) -> RebuildShapeCombination(o, args |> List.map (outputsToVariables mapping hidden cont))
            | _ -> e

    let private composeBinarySimple (l : Shader) (r : Shader) : Compiled<Shader, 'a> =
        let b0 = l.body
        let mutable b1 = r.body

        let mutable mapping = Map.empty
        let mutable unmatched = Map.empty
        let mutable hidden = Set.empty

        //all outputs that are written by right and left get pruned from left
        //this ensures, that those outputs are only written once.
        //NOTE: if right uses them as input they will be contained in mapping and
        //      will therefore be treated seperately
        //NOTE: hidden contains variables originating from left
        for KeyValue(n,o) in r.outputs do
            match Map.tryFind n l.outputs with
                | Some (_,leftOutput) -> 
                    hidden <- Set.add leftOutput hidden
                | _ -> ()

        //All inputs of right that are outputs of left are converted to local variables
        //Since right needs to use the newly declared variables we need to substitute all 
        //occurrences of the corresponding inputs in right
        //NOTE: mapping maps from variables originating from left to variables contained in right
        for KeyValue(n,i) in r.inputs do
            match Map.tryFind n l.outputs with
                | Some (_,o) -> let v = Var(n + "C", o.Type)
                                mapping <- Map.add o v mapping
                                b1 <- b1.Substitute(fun vi -> if vi = i.var then v |> Expr.Var |> Some else None)
                | None -> unmatched <- Map.add n i unmatched

        //hide hidden and mapping since mutables cannot be captured
        //by lambdas
        let mapping = mapping
        let hidden = hidden

        //do the real substitution here
        let b0 = outputsToVariables mapping hidden b1 b0

        //the input set consists of all inputs of left and all unmatched inputs of right
        let inputs = [l.inputs |> Map.toSeq; unmatched |> Map.toSeq] |> Seq.concat |> Map.ofSeq

        //the output set consists of all outputs of right and all outputs of left which are not hidden by right
        let outputs = [r.outputs |> Map.toSeq; l.outputs |> Map.toSeq |> Seq.filter(fun (_,(_,v)) -> not <| Set.contains v hidden) |> Seq.map (fun (k,(_,v)) -> k,(None, v))] |> Seq.concat |> Map.ofSeq

        //the uniform set is simply merged
        let uniforms = [l.uniforms; r.uniforms] |> List.concat
        let uniformMap = uniforms |> List.map (fun (a,b) -> b.Name, b) |> Map.ofList


        let uniformMap =
            (l.uniforms |> List.map (fun (u,v) -> v, uniformMap.[v.Name])) @
            (r.uniforms |> List.map (fun (u,v) -> v, uniformMap.[v.Name]))
            |> Map.ofList


        let b0 = 
            b0.Substitute(fun vi -> 
                match Map.tryFind vi uniformMap with
                    | Some n -> Some (Expr.Var n)
                    | _ -> 
                        match Map.tryFind vi.Name l.inputs with
                            | Some i -> 
                                match Map.tryFind vi.Name inputs with
                                    | Some v -> v.var |> Expr.Var |> Some
                                    | _ -> None
                            | None -> None
            )
//
//        let b0 = b0.Substitute(fun vi -> 
//                    match Map.tryFind vi.Name outputs with
//                        | Some (_,v) -> v |> Expr.Var |> Some
//                        | _ -> None
//                )
//
//        let b0 = b0.Substitute(fun vi -> 
//                    match Map.tryFind vi.Name uniformMap with
//                        | Some v -> v |> Expr.Var |> Some
//                        | _ -> None
//                )

        transform {
            return { shaderType = l.shaderType; inputs = inputs; outputs = outputs; uniforms = uniforms; body = b0; inputTopology = l.inputTopology; debugInfo = None }
        }


    let private composeBinaryGeometryVertex (l : Shader * OutputTopology) (r : Shader) : Compiled<Shader * OutputTopology, 'a> =
        failwith "trying to compose geometryshader with vertex shader"

    let private composeBinaryGeometry (l : Shader * OutputTopology) (r : Shader * OutputTopology) : Compiled<Shader * OutputTopology, 'a> =
        failwith "geometry composition not possible atm"

        

    let private composeBinary (l : Compiled<Effect, 'a>) (r : Compiled<Effect, 'a>) : Compiled<Effect, 'a> =
        transform {
            let! l = l
            let! r = r

            let! vs,gs = transform {
                            match l.vertexShader, l.geometryShader, r.vertexShader, r.geometryShader with
                                //00** | **00
                                | (None, None, l, r)|(l, r, None, None) -> 
                                    return (l, r)
                                
                                //*00*
                                | l, None, None, r -> 
                                    return (l, r)
                                
                                //101*
                                | Some lvs, None, Some rvs, r -> 
                                    let! vs = composeBinarySimple lvs rvs
                                    return (Some vs, r)
                                
                                //*110
                                | lvs, Some lgs, Some rvs, None ->
                                    let! gs = composeBinaryGeometryVertex lgs rvs
                                    return (lvs, Some gs)

                                //*1*1
                                | l, Some lgs, r, Some rgs ->
                                    let! lgs = match r with
                                                | Some r -> composeBinaryGeometryVertex lgs r
                                                | _ -> transform { return lgs }

                                    let! gs = composeBinaryGeometry lgs rgs
                                    return (l, Some gs)
                            }

            let! tcs = transform {
                            match l.tessControlShader, r.tessControlShader with
                                | Some lcs, None -> return Some lcs
                                | None, Some rcs -> return Some rcs
                                | None, None -> return None
                                | _ -> return! error "invalid tessellation-pipeline in composition"
                        }

            let! tev = transform {
                            match l.tessEvalShader, r.tessEvalShader with
                                | Some lev, None -> return Some lev
                                | None, Some rev -> return Some rev
                                | None, None -> return None
                                | _ -> return! error "invalid tessellation-pipeline in composition"
                        }

            let! fs = match l.fragmentShader, r.fragmentShader with
                        | Some l, None -> transform { return Some l }
                        | None, Some r -> transform { return Some r }
                        | Some l, Some r -> transform { let! fs = composeBinarySimple l r in return Some fs }
                        | None, None -> transform { return None }


            return { vertexShader = vs; geometryShader = gs; tessControlShader = tcs; tessEvalShader = tev; fragmentShader = fs; originals = List.concat [l.originals; r.originals] }
        }

    let rec compose (l : #seq<Compiled<Effect, 'a>>) =
        let mutable result = compile { return { vertexShader = None; geometryShader = None; tessControlShader = None; tessEvalShader = None; fragmentShader = None; originals = [] } }
        for e in l do
            result <- composeBinary result e

        result
