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

[<AutoOpen>]
module EffectCompilation =

    let toShader (inputTopology : Option<InputTopology>) (e : Expr) =
        transform {
            match e with
                | MethodLambda(args, body) -> 
                    do! resetCompilerState

                    //eliminate useless builder calls and get the shaderType
                    let! body = removeBuilderCalls body

                    //replace all uniforms with variables and return a map
                    let! body = substituteUniforms body
                    
                    //replace all input-accesses with variables and return a map
                    let a = List.head args
                    let! body = substituteInputs a.Type None body
                
                    //replace all outputs with variables and return a map 
                    let outputType = body.Type

                    let! body = substituteOutputs outputType body

                    let! state = compilerState

                    let uniforms  = state.uniforms |> HashMap.toList

                    let builder = match state.builder with
                                        | Some b -> 
                                            match Expr.tryEval b with
                                                | Some b -> b|> unbox<IShaderBuilder>
                                                | None -> failwith "could not evaluate IShaderBuilder"
                                        | None -> failwith ""


                    return { shaderType = builder.ShaderType; uniforms = uniforms; inputs = state.inputs; outputs = state.outputs; body = body; inputTopology = inputTopology; debugInfo = None }

                | _ -> return! error "not a function"
        }

    let createPassingShader (prim : Type) (id : Var)=
        compile {
            let vertexType = prim.GetGenericArguments().[0]
            let fields = FSharpTypeExt.GetRecordFields(vertexType)

            let input = Var("input", prim)
//            let item = prim.Type.GetProperty("Item")
//            let inputAtIndex = Expr.PropertyGet(Expr.Var input, item, [Expr.Var id])

            let! fields = fields |> Array.toList |> List.mapC (fun m -> 
                            compile {
                                let item = prim.GetProperty("Item")

                                let v = Expr.PropertyGet(Expr.Var input, item, [Expr.Var id])
                                return Expr.PropertyGet(v, m)
                            } )



            let ret = typeof<VertexBuilder>.GetMethod("Return")
            let ret = ret.MakeGenericMethod [|vertexType|]
            let value = Expr.NewRecord(vertexType, fields)
            let shaderCode = Expr.Call(Expr.Value(vertex), ret, [value])

            let shaderCode = Expr.Lambda(input, shaderCode)
            let top = prim.GetProperty("InputTopology").GetValue(null) |> unbox<InputTopology> |> Some

            
            return! toShader top shaderCode
        }

    

    let toShader' (f : 'a -> Expr<'b>) =
        
        let outputType = typeof<'b>

        let e = Unchecked.defaultof<'a> |> f
        let debugInfo = ShaderDebug.tryGetShaderInfo f e
        let e = Expr.Lambda(Var("input", typeof<'a>), e)

        let prim = typeof<'a>.GetInterface("Primitive`1")
        let itop = 
            if prim = null then 
                None 
            else 
                typeof<'a>.GetProperty("InputTopology").GetValue(null) |> unbox<InputTopology> |> Some

        compile {
            let! s = toShader itop e

            if s.shaderType = ShaderType.TessControl then

                let _,inner = s.outputs.["TessLevelInner"]
                let _,outer = s.outputs.["TessLevelOuter"]

                let rec padTessLevels (e : Expr) =
                    match e with
                        | VarSet(v, NewArray(t,values)) when v = inner || v = outer ->
                            let count = if v = inner then 2 else 4
                            
                            let argCount = values.Length
                            if argCount < count then
                                let values = List.concat [values; List.init (count - argCount) (fun _ -> Expr.Value(1.0))]
                                Expr.VarSet(v, Expr.NewArray(t, values))
                            elif argCount > count then
                                let values = values |> Seq.take count |> Seq.toList
                                Expr.VarSet(v, Expr.NewArray(t, values))
                            else
                                e

                        | ShapeLambda(v, b) -> Expr.Lambda(v, padTessLevels b)
                        | ShapeCombination(o, args) -> RebuildShapeCombination(o, args |> List.map padTessLevels)
                        | _ -> e

                let id = Var("InvocationId", typeof<int>)
                let mi = getMethodInfo <@ (=) : int -> int -> bool @>
                let mi = mi.MakeGenericMethod [| typeof<int> |]

                let newBody = Expr.IfThenElse(Expr.Call(mi, [Expr.Var id; Expr.Value(0)]),
                                padTessLevels s.body,
                                Expr.Value(())
                               )

                return [{ s with body = newBody; 
                                 inputs = Map.add "InvocationId" id s.inputs
                                 debugInfo = debugInfo }]
            else
                return [{ s with debugInfo = debugInfo }]
        }

    let private toEffectInternalCache = 
        MemoCache (fun (s : Shader) ->
            match s.shaderType with
                | Vertex -> { vertexShader = Some s; geometryShader = None; tessControlShader = None; tessEvalShader = None; fragmentShader = None; originals = [s] }
                | Fragment -> { vertexShader = None; geometryShader = None; tessControlShader = None; tessEvalShader = None; fragmentShader = Some s; originals = [s] }
                | Geometry(maxVertices, t) -> { vertexShader = None; geometryShader = Some(s,t); tessControlShader = None; tessEvalShader = None; fragmentShader = None; originals = [s] } 
                | TessControl -> { vertexShader = None; geometryShader = None; tessControlShader = Some s; tessEvalShader = None; fragmentShader = None; originals = [s] }
                | TessEval -> { vertexShader = None; geometryShader = None; tessControlShader = None; tessEvalShader = Some s; fragmentShader = None; originals = [s] }
        )

    let toEffectInternal (s : Shader) =
        toEffectInternalCache.Invoke s

    let private toEffectCache = GenericMemoCache()
    let private createEffect (f : 'a -> Expr<'b>) =
        transform {
            let! shaders = toShader' f
            let mutable result = { vertexShader = None; geometryShader = None; tessControlShader = None; tessEvalShader = None; fragmentShader = None; originals = shaders }
            do for s in shaders do
                match s.shaderType with
                    | Vertex -> result <- { result with vertexShader = Some s }
                    | Fragment ->  result <- { result with fragmentShader = Some s }
                    | Geometry(maxVertices, t) ->  result <- { result with geometryShader = Some(s, t) } 
                    | TessControl ->  result <- { result with tessControlShader = Some s }
                    | TessEval ->  result <- { result with tessEvalShader = Some s }
            return result
        }

    let toEffect (f : 'a -> Expr<'b>) =
        toEffectCache.Invoke(createEffect, f)
        
    let private shaderWithDebugInfo (s : Option<Shader>) (info : ShaderDebugInfo) =
        match s with
            | Some s -> Some { s with debugInfo = Some info }
            | None -> None

    let withDebugInfo (newEffect : Effect) (info : ShaderDebugInfo) =
        { vertexShader = shaderWithDebugInfo newEffect.vertexShader info
          tessControlShader = shaderWithDebugInfo newEffect.tessControlShader info
          tessEvalShader = shaderWithDebugInfo newEffect.tessEvalShader info
          geometryShader = match newEffect.geometryShader with | Some (gs,t) -> Some ({ gs with debugInfo = Some info }, t) | _ -> None
          fragmentShader = shaderWithDebugInfo newEffect.fragmentShader info
          originals = newEffect.originals }
