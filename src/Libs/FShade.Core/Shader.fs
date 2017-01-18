namespace FShade

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative


type Shader =
    {
        shaderType              : ShaderType
        shaderInputs            : list<IOParameter>
        shaderOutptus           : list<IOParameter>
        shaderUniforms          : list<UniformParameter>
        shaderInputTopology     : Option<InputTopology>
        shaderOutputTopology    : Option<OutputTopology>
        shaderBody              : Expr
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Shader =
    open System.Collections.Generic
    open Aardvark.Base.ReflectionHelpers

    type private PreprocessorState =
        {
            inputTopology : Option<InputTopology>
            vertexType : Type
            inputs : Dictionary<string, IOParameter>
            outputs : Dictionary<string, IOParameter>
            uniforms : HashSet<UniformParameter>
            mutable builder : Expr
        }

        member x.AddInput(p : IOParameter) =
            match x.inputs.TryGetValue p.paramSemantic with
                | (true, old) ->
                    if old.paramType = p.paramType then ()
                    else failwithf "[FShade] conflicting input-types for %s (%s vs %s)" p.paramSemantic (getPrettyName old.paramType) (getPrettyName p.paramType)
                | _ ->
                    x.inputs.[p.paramSemantic] <- p

        member x.AddOutput(p : IOParameter) =
            match x.outputs.TryGetValue p.paramSemantic with
                | (true, old) ->
                    if old.paramType = p.paramType then ()
                    else failwithf "[FShade] conflicting outputs-types for %s (%s vs %s)" p.paramSemantic (getPrettyName old.paramType) (getPrettyName p.paramType)
                | _ ->
                    x.outputs.[p.paramSemantic] <- p
            
        member x.AddUniform(u : UniformParameter) =
            x.uniforms.Add u |> ignore

    module private Preprocessor =
        open System.Reflection
        open System.Collections.Generic
        open Aardvark.Base.ReflectionHelpers

        [<AutoOpen>]
        module private Helpers = 
            let (|VertexLoad|_|) (vertexType : Type) (e : Expr) =
                match e with
                    | PropertyGet(Some e, p, []) when e.Type = vertexType ->
                        let att = p.GetCustomAttributes<SemanticAttribute>(true) |> Seq.toList
                        match att with
                            | x::_ -> Some (e, x.Semantic)
                            | _ -> Some (e, p.Name)
                    | _ ->
                        None

            let (|Primitive|_|) (e : Option<Expr>) =
                match e with
                    | Some e -> 
                        if e.Type.GetInterface("Primitive`1") <> null then
                            match e with
                                | Value _ | Var _ | PropertyGet(None, _, []) -> Some e
                                | _ -> None
                        else
                            None
                    | None ->
                        None

        let rec private preprocessInternal (state : PreprocessorState) (vertexIndices : Map<Var, Expr>) (e : Expr) =
            match e with
                | BuilderCall(b, mi, [Lambda(v,body)]) when mi.Name = "Delay" ->
                    state.builder <- b
                    preprocessInternal state vertexIndices body

                | BuilderCall(b, Method("For",_), [ExprOf(FixedArrayType(d,_)) as seq; Lambda(v,Let(vi,Var(vo),body))]) ->  
                    state.builder <- b   
                    let body = preprocessInternal state vertexIndices body
                    let seq = preprocessInternal state vertexIndices seq            
                    let result = Expr.ForEach(vi, seq, body)
                    result

                | BuilderCall(b, Method("For",_), [Call(None, Method("op_Range",_), [s; e]); Lambda(v,Let(vi,Var(vo),body))]) ->
                    state.builder <- b
                    let body = preprocessInternal state vertexIndices body
                    let s = preprocessInternal state vertexIndices s  
                    let e = preprocessInternal state vertexIndices e
                    Expr.ForIntegerRangeLoop(vi, s, e, body)

                | BuilderCall(b, Method("Combine",_), [l;r]) ->
                    state.builder <- b
                    let l = preprocessInternal state vertexIndices l
                    let r = preprocessInternal state vertexIndices r
                    Expr.Sequential(l,r)

                | BuilderCall(b, Method("Zero",_), []) ->
                    state.builder <- b
                    Expr.Value(())


                // for v in primitive do
                | BuilderCall(b, Method("For",_) , [Coerce(primitive, t); Lambda(v,Let(vi,Var(vo),body))]) 
                  when b.Type = typeof<GeometryBuilder> || b.Type = typeof<TessControlBuilder> || b.Type = typeof<TessEvalBuilder> ->
                    state.builder <- b

                    let count = primitive.Type.GetProperty("VertexCount", BindingFlags.Static ||| BindingFlags.Public).GetValue(null) |> unbox<int>
                
                    let index = Var("i", typeof<int>)
                    let vertexIndices = Map.add vi (Expr.Var index) vertexIndices
                    let body = preprocessInternal state vertexIndices body

                    Expr.ForIntegerRangeLoop(index, Expr.Value(0), Expr.Value(count - 1),  body)


                // let v = primitive.P0 || let v = primitive.VertexCount
                | Let(v, PropertyGet(Primitive p, pi, []), body) ->
                    match pi.PrimitiveIndex with
                        | Some index -> 
                            let i = Var(v.Name, typeof<int>)
                            let vertexIndices = Map.add v (Expr.Var i) vertexIndices
                        
                            let body = preprocessInternal state vertexIndices body

                            Expr.Let(i, Expr.Value(index), body)

                        | _ ->               
                            let body = preprocessInternal state vertexIndices body
                            Expr.Let(v, Expr.PropertyGet(p, pi, []), body)

                // primitive.P0.pos || primitive.VertexCount.Abs
                | MemberFieldGet(PropertyGet(Primitive p, pi, []), m) ->
                    match pi.PrimitiveIndex with
                        | Some index ->
                            let n = m.Semantic
                            state.AddInput { paramSemantic = n; paramType = e.Type.MakeArrayType(); paramInterpolation = InterpolationMode.Default }
                            Expr.ReadInput(e.Type, n, Expr.Value index)
                        | None ->
                            e

                // primitive.[i].pos
                | MemberFieldGet(PropertyGet(Some p, pi, [index]), m) when pi.Name = "Item" ->
                    let sem = m.Semantic
                    state.AddInput { paramSemantic = sem; paramType = e.Type.MakeArrayType(); paramInterpolation = pi.Interpolation }
                    Expr.ReadInput(e.Type, sem, index)

                | VertexLoad state.vertexType (ve, sem) ->
                    match ve with
                        | Value(null,_) -> 
                            state.AddInput { paramSemantic = sem; paramType = e.Type; paramInterpolation = InterpolationMode.Default }
                            Expr.ReadInput(e.Type, sem)
                        | Var v ->
                            match Map.tryFind v vertexIndices with
                                | Some i -> 
                                    state.AddInput { paramSemantic = sem; paramType = e.Type.MakeArrayType(); paramInterpolation = InterpolationMode.Default }
                                    Expr.ReadInput(e.Type, sem, i)
                                | _ -> 
                                    failwithf "[FShade] accessing vertex input with unknown index"

                        | e ->
                            failwithf "[FShade] complex vertex expressions not supported: %A" ve

                // yield (let a = x+y in f(a))   ==>    yield f(x+y)
                | BuilderCall(b, mi, [Let(v, e, inner)]) when mi.Name = "Yield" ->
                    state.builder <- b
                    let inner = inner.Substitute (fun vi -> if vi = v then Some e else None)
                    Expr.Call(b, mi, [inner]) |> preprocessInternal state vertexIndices

                // yield { a = 1; b = x + y }
                | BuilderCall(b, mi, [NewRecord(t, fields)]) when mi.Name = "Yield" ->
                    state.builder <- b
                    let semantics = FSharpType.GetRecordFields(t, true) |> Seq.map (fun m -> m.Semantic) |> Seq.toList

                    let stores =
                        List.zip semantics fields 
                            |> List.collect (fun (name, value) ->
                                let value = preprocessInternal state vertexIndices value

                                if value.Type.IsArray then
                                    match value with
                                        | NewArray(t, args) ->
                                            state.AddOutput { paramSemantic = name; paramType = value.Type; paramInterpolation = InterpolationMode.Default }
                                            //state.AddOutput(name, value.Type)
                                            args |> List.mapi (fun i v -> Expr.WriteOutput(name, Expr.Value i, v))
                                        | _ ->
                                            Log.warn "[FShade] bad array output"
                                            []


                                else
                                    state.AddOutput { paramSemantic = name; paramType = value.Type; paramInterpolation = InterpolationMode.Default }
                                    [Expr.WriteOutput(name, value)]
                            )

                    let emit = Expr.Call(getMethodInfo <@ emitVertex @>, [])
                    stores |> List.fold (fun a e -> Expr.Sequential(e, a)) emit

                // yield V4d.IOOI
                | BuilderCall(b, mi, [value]) when mi.Name = "Yield" ->
                    state.builder <- b
                    let value = preprocessInternal state vertexIndices value
                    state.AddOutput { paramSemantic = Intrinsics.Position; paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default }
                    let store = 
                        if value.Type = typeof<V4d> then 
                            Expr.WriteOutput(Intrinsics.Position, value)

                        elif value.Type = typeof<V3d> then 
                            let ctor = typeof<V4d>.GetConstructor [|typeof<V3d>; typeof<float> |]
                            Expr.WriteOutput(Intrinsics.Position, Expr.NewObject(ctor, [value; Expr.Value 1.0]))

                        else 
                            failwithf "[FShade] cannot yield %A" value
                    
                    Expr.Sequential(store, Expr.Call(getMethodInfo <@ emitVertex @>, []))

                // return (let a = x+y in f(a))   ==>    return f(x+y)
                | BuilderCall(b, mi, [Let(v, e, inner)]) when mi.Name = "Return" ->
                    state.builder <- b
                    let inner = inner.Substitute (fun vi -> if vi = v then Some e else None)
                    Expr.Call(b, mi, [inner]) |> preprocessInternal state vertexIndices


                | BuilderCall(b, mi, [NewRecord(t, fields)]) when mi.Name = "Return" ->
                    state.builder <- b
                    let semantics = FSharpType.GetRecordFields(t, true) |> Seq.map (fun m -> m.Semantic) |> Seq.toList

                    let stores =
                        List.zip semantics fields 
                            |> List.collect (fun (name, value) ->
                                let value = preprocessInternal state vertexIndices value


                                if value.Type.IsArray then
                                    match value with
                                        | NewArray(t, args) ->
                                            state.AddOutput { paramSemantic = name; paramType = value.Type.MakeArrayType(); paramInterpolation = InterpolationMode.Default }
                                            args |> List.mapi (fun i v -> Expr.WriteOutput(name, Expr.Value i, v))
                                        | _ ->
                                            Log.warn "[FShade] bad array output"
                                            []


                                else
                                    state.AddOutput { paramSemantic = name; paramType = value.Type; paramInterpolation = InterpolationMode.Default }
                                    [Expr.WriteOutput(name, value)]
                            )

                    match stores with
                        | [] -> Expr.Value(())
                        | fst :: rest ->
                            List.fold (fun e rest -> Expr.Sequential(e, rest)) fst rest

                // return V4d.IOOI
                | BuilderCall(b, mi, [value]) when mi.Name = "Return" ->
                    state.builder <- b
                    let value = preprocessInternal state vertexIndices value
                    let sem = 
                        if b.Type = typeof<FragmentBuilder> then Intrinsics.Color
                        else Intrinsics.Position
                        
                    state.AddOutput { paramSemantic = sem; paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default }
                    if value.Type = typeof<V4d> then
                        Expr.WriteOutput(sem, value)
                    elif value.Type = typeof<V3d> then
                        let ctor = typeof<V4d>.GetConstructor [|typeof<V3d>; typeof<float> |]
                        Expr.WriteOutput(sem, Expr.NewObject(ctor, [value; Expr.Value 1.0]))
                    else
                        failwithf "[FShade] cannot return %A" value
                       
                
                | Uniform u ->
                    state.AddUniform u
                    Expr.ReadInput(u.uniformType, u.uniformName)
                       
                        
                | ShapeCombination(o, args) ->
                    let args = args |> List.map (preprocessInternal state vertexIndices)
                    RebuildShapeCombination(o, args)

                | ShapeLambda(v,b) -> 
                    Expr.Lambda(v, preprocessInternal state vertexIndices b)

                | ShapeVar _ -> 
                    e

        /// preprocess removes all builder calls from the given expression, 
        /// replaces all input-accesses with Expr.Load and all writes with Expr.Store.
        /// additionaly the used builder 
        let preprocess (inputType : Type) (e : Expr) =

            // figure out the vertex-type and the (optional) input-topology
            let vertexType, inputTopology =
                let p = inputType.GetInterface("Primitive`1")
                if isNull p then inputType, None
                else 
                    let vertexType = p.GetGenericArguments().[0]
                    let top = inputType.GetProperty("InputTopology", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public).GetValue(null) |> unbox<InputTopology>
                    vertexType, Some top

            // the builder can be inlined (TODO: may be nested)
            let input = 
                match e with
                    | Application(Lambda(var, b), value) -> b.Substitute(fun vi -> if vi = var then Some value else None)
                    | e -> e

            // run the real processing
            let state = 
                {
                    inputTopology = inputTopology
                    vertexType = vertexType
                    inputs = Dictionary()
                    outputs = Dictionary()
                    uniforms = HashSet()
                    builder = Expr.Value(())
                }

            let clean = input |> preprocessInternal state Map.empty


            clean, state

    let inline shaderType (s : Shader) = s.shaderType
    let inline uniforms (s : Shader) = s.shaderUniforms
    let inline inputs (s : Shader) = s.shaderInputs
    let inline outputs (s : Shader) = s.shaderOutptus
    let inline body (s : Shader) = s.shaderBody
    let inline inputTopology (s : Shader) = s.shaderInputTopology
    let inline outputTopology (s : Shader) = s.shaderOutputTopology
        
        
    let ofExpr (inputType : Type) (e : Expr) =
        let body, state = Preprocessor.preprocess inputType e

        // figure out the used builder-type
        let builder = 
            match Expr.TryEval state.builder with
                | Some (:? IShaderBuilder as v) -> v
                | _ -> failwithf "[FShade] could not evaluate shader-builder %A" state.builder

        { 
            shaderType              = builder.ShaderType
            shaderInputs            = state.inputs.Values |> Seq.toList
            shaderOutptus           = state.inputs.Values |> Seq.toList
            shaderUniforms          = state.uniforms |> Seq.toList
            shaderInputTopology     = state.inputTopology
            shaderOutputTopology    = builder.OutputTopology
            shaderBody              = body
        }

    let ofFunction (f : 'a -> Expr<'b>) =
        let e = f Unchecked.defaultof<'a>
        ofExpr typeof<'a> e



[<NoComparison>]
type Effect = 
    { 
        vertex        : Option<Shader>
        tessControl   : Option<Shader>
        tessEval      : Option<Shader>
        geometry      : Option<Shader>
        fragment      : Option<Shader>
    }

    member x.hasVertex = Option.isSome x.vertex
    member x.hasTessControl = Option.isSome x.tessControl
    member x.hasTessEval = Option.isSome x.tessEval
    member x.hasGeometry = Option.isSome x.geometry
    member x.hasFragment = Option.isSome x.fragment

    member x.shaders =
        List.concat [
            Option.toList x.vertex
            Option.toList x.tessControl
            Option.toList x.tessEval
            Option.toList x.geometry
            Option.toList x.fragment
        ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Effect =
    let inline vertex (e : Effect) = e.vertex
    let inline tessControl (e : Effect) = e.tessControl
    let inline tessEval (e : Effect) = e.tessEval
    let inline geometry (e : Effect) = e.geometry
    let inline fragment (e : Effect) = e.fragment
    let inline hasVertex (e : Effect) = e.hasVertex
    let inline hasTessControl (e : Effect) = e.hasTessControl
    let inline hasTessEval (e : Effect) = e.hasTessEval
    let inline hasGeometry (e : Effect) = e.hasGeometry
    let inline hasFragment (e : Effect) = e.hasFragment
    let inline shaders (e : Effect) = e.shaders

    let empty = { vertex = None; tessControl = None; tessEval = None; geometry = None; fragment = None }

    let isEmpty (e : Effect) =
        Option.isNone e.vertex &&
        Option.isNone e.tessControl &&
        Option.isNone e.tessEval &&
        Option.isNone e.geometry &&
        Option.isNone e.fragment

    let ofShader (shader : Shader) =
        match shader.shaderType with
            | ShaderType.Vertex         -> { empty with vertex = Some shader }
            | ShaderType.TessControl    -> { empty with tessControl = Some shader }
            | ShaderType.TessEval       -> { empty with tessEval = Some shader }
            | ShaderType.Geometry       -> { empty with geometry = Some shader }
            | ShaderType.Fragment       -> { empty with fragment = Some shader }
            | _                         -> failwithf "[FShade] unknown shader-type %A" shader.shaderType

    let inline ofExpr (inputType : Type) (e : Expr) =
        Shader.ofExpr inputType e |> ofShader

    let inline ofFunction (f : 'a -> Expr<'b>) =
        Shader.ofFunction f |> ofShader


