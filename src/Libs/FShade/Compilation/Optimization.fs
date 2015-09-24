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
module Optimization =
    let rec removeAssigns (remVariables : Set<Var>) (e : Expr) =
        match e with
            | Sequential(VarSet(var, value), r) when Set.contains var remVariables ->
                removeAssigns remVariables r

            | VarSet(var, value) when Set.contains var remVariables ->
                Expr.Value(())

            | ShapeCombination(o, args) ->
                RebuildShapeCombination(o, args |> List.map (removeAssigns remVariables))

            | ShapeLambda(v,b) ->
                Expr.Lambda(v, removeAssigns remVariables b)

            | _ -> e

    type private EliminationState = { used : Set<Var>; found : bool }

    let private found = modifyCompilerState(fun e -> if e.found then e else { used = e.used; found = true })

    let private isFound = { runCompile = fun s -> Success(s,s.userState.found) }
    let private putFound f = modifyCompilerState(fun e -> { used = e.used; found = f })

    let private addUsed v = modifyCompilerState(fun e -> { used = Set.add v e.used; found = e.found })
    let private isUsed v = 
        compile {
            let! s = compilerState
            return Set.contains v s.used
        }

    let rec private eliminateDeadCodeInternal (e : Expr) =
        transform {
            match e with
                | Let(var,value,cont) -> 
                    
                    let! cont = eliminateDeadCodeInternal cont
                    let! value = eliminateDeadCodeInternal value

                    let! used = isUsed var
                    if used then
                        return Expr.Let(var, value, cont)
                    else
                        do! found
                        return cont

                | VarSet(var,value) -> 
                    let! used = isUsed var
                    let! value = eliminateDeadCodeInternal value

                    if used then
                        return Expr.VarSet(var, value)
                    else
                        do! found
                        return Expr.Value(())

                | IfThenElse(_, Value(_, Unit), Value(_, Unit)) ->
                    do! found
                    return Expr.Value(())

                | IfThenElse(c, i, e) ->
                    let! i = eliminateDeadCodeInternal i
                    let! e = eliminateDeadCodeInternal e
                    let! c = eliminateDeadCodeInternal c
                    return Expr.IfThenElse(c,i,e)

                | ForIntegerRangeLoop(v,s,e,Value(_,Unit)) ->
                    do! found
                    return Expr.Value(())

                | ForIntegerRangeLoop(v,s,e,b) ->
                    
                    //traverse the body once since it needs to maintain all used varaibles
                    let! f = isFound
                    let! _ = eliminateDeadCodeInternal b
                    do! putFound f

                    let! b = eliminateDeadCodeInternal b
                    let! e = eliminateDeadCodeInternal e
                    let! s = eliminateDeadCodeInternal s

                    return Expr.ForIntegerRangeLoop(v,s,e,b)

                | Sequential(Value(_,Unit), c) | Sequential(c, Value(_,Unit)) ->
                    do! found
                    return! eliminateDeadCodeInternal c

                | ShapeCombination(o, args) ->
                    let! args = args |> List.rev |> List.mapC eliminateDeadCodeInternal
                    return RebuildShapeCombination(o, args |> List.rev)

                | ShapeLambda(v,b) -> 
                    let! b = eliminateDeadCodeInternal b
                    return Expr.Lambda(v, b)

                | Var v -> 
                    do! addUsed v
                    return e

                | _ -> return e
        }
                
    let rec eliminateDeadCode (e : Expr) =
        let free = e.GetFreeVars() |> Set.ofSeq
        let mutable result = e
        let r = ref true
        while !r do
            r := false
            let comp = eliminateDeadCodeInternal result
            let state = { compiler = Unchecked.defaultof<ICompiler<EliminationState>>; types = PersistentHashSet.empty; functions = PersistentHashSet.empty; functionId = 0; constantId = 0; constants = HashMap.empty; lambdaId = 0; lambdas = Map.empty; uniformId = 0; uniforms = HashMap.empty; defines = Map.empty; bound = Set.empty; userState = { used = free; found = false }}
            match comp.runCompile state with
                | Success(s,v) -> r := s.userState.found
                                  result <- v
                | Error e -> printfn "ERROR: %A" e
                             r := false


        result

    let rec removeOutputs (rem : Set<Var>) (s : Shader) =
        let b = removeAssigns rem s.body
        let b = eliminateDeadCode b

        let free = b.GetFreeVars() |> Set.ofSeq
        let inputs = s.inputs |> Map.filter(fun k v -> Set.contains v free)
        let outputs = s.outputs |> Map.filter(fun k (_,v) -> Set.contains v free)
        let uniforms = s.uniforms |> List.filter(fun (k,v) -> Set.contains v free)

        { inputs = inputs; outputs = outputs; uniforms = uniforms; body = b; shaderType = s.shaderType; inputTopology = s.inputTopology; debugInfo = s.debugInfo }

    type TessellationMethods =

        //< ^a, ^b when ^a : (static member (*) : float -> ^a -> ^a) and ^a : (static member (+) : ^a -> ^a -> ^a)>
        static member inline interpolate3 (a : Expr) (b : Expr) (c : Expr) (coord : Expr) =
            let ta = a.Type
            match ta with
                | Fractional|VectorOf(_,Fractional)|MatrixOf(_,Fractional) ->
                    let mul = getMethodInfo <@ (*) @>
                    let add = getMethodInfo <@ (+) @>

                    let mul = mul.MakeGenericMethod [| typeof<float>; ta; ta |]
                    let add = add.MakeGenericMethod [| ta; ta; ta |]

                    if mul = null || add = null then
                        a
                    else
                        let mul a b = Expr.Call(mul, [a; b])
                        let add a b = Expr.Call(add, [a;b])

                        let a = mul <@@ (%%coord : V3d).X @@> a
                        let b = mul <@@ (%%coord : V3d).Y @@> b
                        let c = mul <@@ (%%coord : V3d).Z @@> c


                        let e = add a (add b c)
                        e
                | _ -> a

        static member inline interpolate4 (p0 : Expr) (p1 : Expr) (p2 : Expr) (p3 : Expr) (coord : Expr) =
            let ta = p0.Type
            match ta with
                | Fractional|VectorOf(_,Fractional)|MatrixOf(_,Fractional) ->
                    let mul = getMethodInfo <@ (*) @>
                    let add = getMethodInfo <@ (+) @>

                    let mul = mul.MakeGenericMethod [| typeof<float>; ta; ta |]
                    let add = add.MakeGenericMethod [| ta; ta; ta |]

                    if mul = null || add = null then
                        p0
                    else
                        let mul a b = Expr.Call(mul, [a; b])
                        let add a b = Expr.Call(add, [a; b])
                        let lerp a b c = add (mul <@@ 1.0 - %%c @@> a) (mul c a)

                        let a = lerp p0 p1 <@@ (%%coord : V3d).X @@>
                        let b = lerp p2 p3 <@@ (%%coord : V3d).X @@>

                        let e = lerp a b <@@ (%%coord : V3d).Y @@>
                        e
                | _ -> p0

        static member defaultEvalInterpolation (v : Var) (arity : int) : Expr =
            let t = v.Type.GetElementType()

            if arity = 3 then
                let mi = typeof<TessellationMethods>.GetMethod("interpolate3")
                mi.Invoke(null, [|Expr.ArrayAccess(Expr.Var v, Expr.Value 0) :> obj
                                  Expr.ArrayAccess(Expr.Var v, Expr.Value 1) :> obj
                                  Expr.ArrayAccess(Expr.Var v, Expr.Value 2) :> obj
                                  Expr.Var (Var("gl_TessCoord", typeof<V3d>)) :> obj         |]) |> unbox

            elif arity = 4 then
                let mi = typeof<TessellationMethods>.GetMethod("interpolate4")
                mi.Invoke(null, [|Expr.ArrayAccess(Expr.Var v, Expr.Value 0) :> obj
                                  Expr.ArrayAccess(Expr.Var v, Expr.Value 1) :> obj
                                  Expr.ArrayAccess(Expr.Var v, Expr.Value 2) :> obj
                                  Expr.ArrayAccess(Expr.Var v, Expr.Value 3) :> obj
                                  Expr.Var (Var("gl_TessCoord", typeof<V3d>)) :> obj         |]) |> unbox
            else
                failwith ""

        static member pass (stage : ShaderType) (v : Var) (arity : int) : Expr =
            if stage = ShaderType.TessControl then
                Expr.ArrayAccess(Expr.Var v, Expr.Var (Var("gl_InvocationID", typeof<int>)))
            else
                TessellationMethods.defaultEvalInterpolation v arity
                
    let getInputName (outputName : string) (stage : ShaderType) =
        if outputName = "_Positions_" then "Positions"
        else outputName

    let rec addOutputs (wanted : Map<string, Type>) (s : Shader) : Shader =
        if wanted.Count > 0 then
            match s.shaderType with
                | Geometry _ -> failwith "geometry passing not implemented"
                | TessEval|TessControl ->
                    match s.inputTopology with
                        | Some(Patch n) ->
                                
                            let b = s.body
                            let inputs = wanted |> Seq.map(fun (KeyValue(k,v)) -> 
                                                    let k = getInputName k s.shaderType

                                                    match Map.tryFind k s.inputs with
                                                        | Some var -> k,var
                                                        | None -> k,Var(k, v.MakeArrayType())
                                                    ) |> Seq.toList

                            let outputs = wanted |> Seq.map(fun (KeyValue(k,v)) -> k,(None, Var(k + "Out", v))) |> Seq.toList
                            let b = List.zip inputs outputs |> List.fold (fun b ((_,i),(_,(_,o))) -> Expr.Sequential(Expr.VarSet(o, TessellationMethods.pass s.shaderType i n), b)) b
                            let newInputs = seq { yield! s.inputs |> Map.toSeq; yield! inputs } |> Map.ofSeq
                            let newOutputs = seq { yield! s.outputs |> Map.toSeq; yield! outputs } |> Map.ofSeq

                            { inputs = newInputs; outputs = newOutputs; uniforms = s.uniforms; shaderType = s.shaderType; body = b; inputTopology = s.inputTopology; debugInfo = s.debugInfo }
                        | _ -> failwith "invalid input-topology for tessellation shader"

                | _ -> let b = s.body

                       let inputs = wanted |> Seq.map(fun (KeyValue(k,v)) -> 
                                                let k = getInputName k s.shaderType

                                                match Map.tryFind k s.inputs with
                                                    | Some var -> k,var
                                                    | None -> k,Var(k, v)
                                              ) |> Seq.toList

                       let outputs = wanted |> Seq.map(fun (KeyValue(k,v)) -> k,(None, Var(k + "Out", v))) |> Seq.toList

                       let b = List.zip inputs outputs |> List.fold (fun b ((_,i),(_,(_,o))) -> Expr.Sequential(Expr.VarSet(o, Expr.Var(i)), b)) b

                       let newInputs = seq { yield! s.inputs |> Map.toSeq; yield! inputs } |> Map.ofSeq
                       let newOutputs = seq { yield! s.outputs |> Map.toSeq; yield! outputs } |> Map.ofSeq

                       { inputs = newInputs; outputs = newOutputs; uniforms = s.uniforms; shaderType = s.shaderType; body = b; inputTopology = s.inputTopology; debugInfo = s.debugInfo }
        else
            s