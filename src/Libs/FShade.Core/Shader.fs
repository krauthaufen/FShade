namespace FShade

open System
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative


open System.Collections.Generic
open Aardvark.Base.ReflectionHelpers

type Shader =
    {
        shaderStage             : ShaderStage
        shaderInputs            : Map<string, ParameterDescription>
        shaderOutputs           : Map<string, ParameterDescription>
        shaderUniforms          : Map<string, UniformParameter>
        shaderInputTopology     : Option<InputTopology>
        shaderOutputTopology    : Option<OutputTopology * int>
        shaderBody              : Expr
    }

module private Preprocessor =
    open System.Reflection
    open System.Collections.Generic
    open Aardvark.Base.ReflectionHelpers
    open Aardvark.Base.Monads.State

    [<AutoOpen>] 
    module BuilderPatterns = 

        let (|BuilderCombine|_|) (e : Expr) =
            match e with
                | BuilderCall(b, Method("Combine",_), [l;r]) ->
                    Some(b, l, r)
                | _ ->
                    None

        let (|BuilderDelay|_|) (e : Expr) =
            match e with
                | BuilderCall(b, Method("Delay",_), [Lambda(v,body)]) when v.Type = typeof<unit> ->
                    Some(b, body)
                | _ ->
                    None

        let (|BuilderRun|_|) (e : Expr) =
            match e with
                | BuilderCall(b, Method("Run",_), [e]) ->
                    Some(b, e)
                | _ ->
                    None
            
        let (|BuilderZero|_|) (e : Expr) =
            match e with
                | BuilderCall(b, Method("Zero",_), []) ->
                    Some(b)
                | _ ->
                    None

        let (|BuilderFor|_|) (e : Expr) =
            match e with
                | BuilderCall(b, Method("For",_) , [sequence; Lambda(v,body)]) ->
                    match body with
                        | Let(vi,Var(vo),body) when vo = v ->
                            Some(b, vi, sequence, body)
                        | _ ->
                            Some(b, v, sequence, body)
                            
                | _ ->
                    None

        let (|BuilderWhile|_|) (e : Expr) =
            match e with
                | BuilderCall(b, Method("While",_), [guard; body]) ->
                    Some(b, guard, body)
                | _ ->
                    None

        let (|BuilderYield|_|) (e : Expr) =
            match e with
                | BuilderCall(b, mi, [v]) when mi.Name = "Yield" ->
                    Some(b, mi, v)
                | _ ->
                    None

        let (|BuilderYieldFrom|_|) (e : Expr) =
            match e with
                | BuilderCall(b, mi, [v]) when mi.Name = "YieldFrom" ->
                    Some(b, mi, v)
                | _ ->
                    None
            
        let (|BuilderReturn|_|) (e : Expr) =
            match e with
                | BuilderCall(b, mi, [v]) when mi.Name = "Return" ->
                    Some(b, mi, v)
                | _ ->
                    None

        let (|BuilderReturnFrom|_|) (e : Expr) =
            match e with
                | BuilderCall(b, mi, [v]) when mi.Name = "ReturnFrom" ->
                    Some(b, mi, v)
                | _ ->
                    None

        let (|BuilderBind|_|) (e : Expr) =
            match e with
                | BuilderCall(b, Method("Bind",_), [e; Lambda(vv, c)]) ->
                    match c with
                        | Let(vi, Var ve, c) -> 
                            Some(b, vi, e, c)
                        | _ ->
                            Some(b, vv, e, c)
                | _ ->
                    None
        
        let (|BuilderUsing|_|) (e : Expr) =
            match e with
                | BuilderCall(b, Method("Using",_), [e; Lambda(v,body)]) ->
                    match body with
                        | Let(vi, Var vo, body) when vo = v -> Some(b, vi, e, b)
                        | _ -> Some(b, v, e, body)
                | _ ->
                    None

    [<AutoOpen>] 
    module OtherPatterns =

        let (|Primitive|_|) (e : Expr) =
            let iface = e.Type.GetInterface("Primitive`1")
            if isNull iface then
                None
            else
                let countProp = e.Type.GetProperty("VertexCount", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                let vertices = countProp.GetValue(null) |> unbox<int>
                match e with
                    | Value _ | Var _ | PropertyGet(None, _, []) -> Some(e, vertices)
                    | _ -> None

        let (|PrimitiveVertexGet|_|) (e : Expr) =
            match e with
                | PropertyGet(Some (Primitive(p,_)), pi, []) ->
                    match pi.PrimitiveIndex with
                        | Some index -> Some(p, Expr.Value index)
                        | _ -> failwithf "[FShade] cannot get primitive-property %A" pi

                | PropertyGet(Some (Primitive(p,_)), pi, [index]) when pi.Name = "Item" ->
                    Some (p, index)

                | _ ->
                    None

        let (|InputRead|_|) (vertexType : Type) (e : Expr) =
            match e with
                | PropertyGet(Some vertex, field, []) when vertex.Type = vertexType ->
                    let isRecordField = FSharpType.GetRecordFields(vertexType, true) |> Array.exists (fun pi -> pi = field)
                    if isRecordField then 
                        let parameter = { paramType = field.PropertyType; paramInterpolation = field.Interpolation }
                        Some(vertex, field.Semantic, parameter)
                    else
                        None
                | _ ->
                    None

        let (|TessellateCall|_|) (e : Expr) =
            match e with
                | Call(None, MethodQuote <@ tessellateTriangle @> _, [li; l01;l12;l20]) ->
                    Some(3, [li], [l01;l12;l20])
                | Call(None, MethodQuote <@ tessellateQuad @> _, [lx;ly; l01;l12;l23;l30]) ->
                    Some(4, [lx; ly], [l01;l12;l23;l30])

                | _ ->
                    None

    type State =
        {
            inputType       : Type
            inputTopology   : Option<InputTopology>
            vertexType      : Type
            builder         : Option<Expr>
            inputs          : Map<string, ParameterDescription>
            outputs         : Map<string, ParameterDescription>
            uniforms        : Map<string, UniformParameter>
            vertexIndex     : Map<Var, Expr>
            shaders         : list<Shader>
        }
        
    type Preprocess<'a> = State<State, 'a>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        type private NoInput = { x : int }

        let empty =
            {
                inputType       = typeof<NoInput>
                inputTopology   = None
                vertexType      = typeof<NoInput>
                builder         = None
                inputs          = Map.empty
                outputs         = Map.empty
                uniforms        = Map.empty
                vertexIndex     = Map.empty
                shaders         = []
            }

        let ofInputType (t : Type) =
            let iface = t.GetInterface(typedefof<Primitive<_>>.Name)
            let vertexType, topology = 
                if isNull iface then 
                    t, None
                else 
                    let top = t.GetProperty("InputTopology", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public).GetValue(null) |> unbox<InputTopology>
                    let t = iface.GetGenericArguments().[0]
                    t, Some top

            { empty with
                inputType = t
                inputTopology = topology
                vertexType = vertexType
            }

                

        let inputType = State.get |> State.map (fun s -> s.inputType)
        let vertexType = State.get |> State.map (fun s -> s.vertexType)
        let builder = State.get |> State.map (fun s -> s.builder)

        let readInput (name : string) (desc : ParameterDescription) = 
            State.modify (fun s ->
                { s with State.inputs = Map.add name desc s.inputs }
            )

        let readUniform (p : UniformParameter) = 
            State.modify (fun s ->
                { s with State.uniforms = Map.add p.uniformName p s.uniforms }
            )

        let writeOutput (name : string) (desc : ParameterDescription) = 
            State.modify (fun s ->
                { s with State.outputs = Map.add name desc s.outputs }
            )

        let setBuilder (b : Expr) =
            State.modify (fun s ->
                { s with State.builder = Some b }
            )

        let setVertexIndex (v : Var) (index : Expr) =
            State.modify (fun s ->
                { s with vertexIndex = Map.add v index s.vertexIndex }
            )

        let tryGetVertexIndex (v : Var) =
            State.get |> State.map (fun s -> Map.tryFind v s.vertexIndex)

    let rec preprocessS (e : Expr) : Preprocess<Expr> =
        state {
            let! vertexType = State.vertexType

            match e with
                | ReadInput(kind, name, idx) ->
                    let! idx = idx |> Option.mapS preprocessS

                    let paramType =
                        match idx with
                            | Some _ -> e.Type.MakeArrayType()
                            | _ -> e.Type

                    match kind with
                        | ParameterKind.Input -> 
                            do! State.readInput name { paramType = paramType; paramInterpolation = InterpolationMode.Default }
                        | _ ->
                            do! State.readUniform { uniformType = paramType; uniformName = name; uniformValue = UniformValue.Attribute(uniform, name) }

                    match idx with
                        | Some idx -> return Expr.ReadInput(kind, e.Type, name, idx)
                        | None -> return Expr.ReadInput(kind, e.Type, name)


                | BuilderRun(b, body)
                | BuilderDelay(b, body) ->
                    do! State.setBuilder b
                    return! preprocessS body

                | BuilderCombine(b, l, r) ->
                    do! State.setBuilder b
                    let! l = preprocessS l
                    let! r = preprocessS r
                    return Expr.Seq [l;r]

                | BuilderZero(b) ->
                    do! State.setBuilder b
                    return Expr.Unit


                | BuilderBind(b, var, TessellateCall(dim, inner, outer), tev) ->
                    do! State.setBuilder b

                    let coord = 
                        let c = Expr.ReadInput(ParameterKind.Input, typeof<V3d>, Intrinsics.TessCoord)
                        if var.Type = c.Type then c
                        else <@@ (%%c : V3d).XY @@>

                    let tev = Expr.Let(var, coord, tev)
                    let free = tev.GetFreeVars() |> Set.ofSeq |> Set.toList

                    for f in free do
                        do! State.writeOutput f.Name { paramType = f.Type; paramInterpolation = InterpolationMode.Default }

                    let rec wrap (free : list<Var>) (b : Expr) =
                        match free with
                            | [] -> b
                            | h :: rest ->
                                Expr.Let(h, Expr.ReadInput(ParameterKind.Input, h.Type, h.Name), wrap rest b)
                        
                    let! inputType = State.inputType
                    let tev = wrap free tev |> toShaders inputType
                    match tev with
                        | [tev] ->
                            do! State.modify (fun s -> { s with shaders = [ { tev with shaderStage = ShaderStage.TessEval } ] })
                        | _ ->
                            failwithf "[FShade] invalid shader(s) after tessellate-call: %A" tev

                    do! State.writeOutput Intrinsics.TessLevelInner { paramType = typeof<float[]>; paramInterpolation = InterpolationMode.Default }
                    do! State.writeOutput Intrinsics.TessLevelOuter { paramType = typeof<float[]>; paramInterpolation = InterpolationMode.Default }

                    return 
                        Expr.WriteOutputs [
                            yield Intrinsics.TessLevelInner, Expr.NewArray(typeof<float>, inner)
                            yield Intrinsics.TessLevelOuter, Expr.NewArray(typeof<float>, outer)
                            yield! free |> List.map (fun v -> v.Name, Expr.Var v)
                        ]


                | BuilderUsing(b, var, value, body)
                | BuilderBind(b, var, value, body) ->
                    do! State.setBuilder b
                    let! value = preprocessS value
                    let! body = preprocessS body
                    if var.Type <> typeof<unit> then
                        return Expr.Let(var, value, body)
                    else
                        match value with
                            | Unit -> return body
                            | _ -> return Expr.Sequential(value, body)

                | BuilderFor(b, var, RangeSequence(first, step, last), body) ->
                    do! State.setBuilder b
                    let! first = preprocessS first
                    let! step = preprocessS step
                    let! last = preprocessS last
                    let! body = preprocessS body
                    return Expr.ForInteger(var, first, step, last, body)
                    
                | BuilderFor(b, var, Coerce(Primitive(primitive, vertexCount), _), body) ->
                    do! State.setBuilder b
                    let iVar = Var(var.Name + "Index", typeof<int>)
                    let prop = primitive.Type.GetProperty("Item", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                    let replacement = Expr.PropertyGet(primitive, prop, [Expr.Var iVar])
                    
                    do! State.setVertexIndex var (Expr.Var iVar)
                    let! body = preprocessS body

                    return Expr.ForIntegerRangeLoop(iVar, Expr.Value 0, Expr.Value (vertexCount - 1), body)

                | BuilderFor(b, var, sequence, body) ->
                    do! State.setBuilder b
                    let! sequence = preprocessS sequence
                    let! body = preprocessS body
                    return Expr.ForEach(var, sequence, body)

                | BuilderWhile(b, guard, body) ->
                    do! State.setBuilder b
                    let! guard = preprocessS guard
                    let! body = preprocessS body
                    return Expr.WhileLoop(guard, body)
                    

                | Uniform u ->
                    do! State.readUniform u
                    return Expr.ReadInput(ParameterKind.Uniform, e.Type, u.uniformName)

                // let p0 = tri.P0 in <body>
                // store (p0 -> 0) and preprocess <body>
                | Let(var, PrimitiveVertexGet(p, index), body) when var.Type = vertexType ->
                    let! index = preprocessS index
                    do! State.setVertexIndex var index
                    return! preprocessS body

                // tri.P0.pos -> ReadInput(pos, 0)
                | InputRead vertexType (PrimitiveVertexGet(p, index), semantic, parameter) ->
                    do! State.readInput semantic { parameter with paramType = parameter.paramType.MakeArrayType() }
                    return Expr.ReadInput(ParameterKind.Input, e.Type, semantic, index)

                // real vertex-read needed
                | PrimitiveVertexGet(p, index) ->
                    let! index = preprocessS index
                    let fields = FSharpType.GetRecordFields(e.Type, true) |> Array.toList

                    let! args =
                        fields |> List.mapS (fun f ->
                            state {
                                let interpolation = f.Interpolation
                                let semantic = f.Semantic 
                                let parameter = { paramType = f.PropertyType; paramInterpolation = interpolation }
                                do! State.readInput semantic { parameter with paramType = parameter.paramType.MakeArrayType() }
                                return Expr.ReadInput(ParameterKind.Input, f.PropertyType, semantic, index)
                            }
                        )
                        
                    return Expr.NewRecord(e.Type, args)

                // vertex.pos -> ReadInput(pos)
                | InputRead vertexType (vertex, semantic, parameter) ->
                    let! index =
                        match vertex with
                            | Var v -> State.tryGetVertexIndex v
                            | Value _ -> State.value None
                            
                            | _ -> failwithf "[FShade] found non-primitive vertex-expression: %A" vertex

                    match index with
                        | Some index -> 
                            do! State.readInput semantic { parameter with paramType = parameter.paramType.MakeArrayType() }
                            return Expr.ReadInput(ParameterKind.Input, parameter.paramType, semantic, index)
                        | _ ->
                            do! State.readInput semantic parameter
                            return Expr.ReadInput(ParameterKind.Input, parameter.paramType, semantic)
                    
                    
                | BuilderYield(b, mi, Let(var, value, body)) ->
                    return! preprocessS (Expr.Let(var, value, Expr.Call(b, mi, [body])))       

                | BuilderReturn(b, mi, Let(var, value, body)) ->
                    return! preprocessS (Expr.Let(var, value, Expr.Call(b, mi, [body])))   
                    
                | BuilderYield(b, _, value) ->
                    do! State.setBuilder b
                    let! value = preprocessS value
                    let defaultSem = 
                        if b.Type = typeof<FragmentBuilder> then Intrinsics.Color
                        else Intrinsics.Position

                    let! values = getOutputValues defaultSem value

                    return 
                        Expr.Sequential(
                            Expr.WriteOutputs values,
                            <@ emitVertex() @>
                        )

                | BuilderReturn(b, _, value) ->
                    do! State.setBuilder b
                    let! value = preprocessS value
                    let defaultSem = 
                        if b.Type = typeof<FragmentBuilder> then Intrinsics.Color
                        else Intrinsics.Position

                    let! values = getOutputValues defaultSem value

                    return Expr.WriteOutputs values

                | Sequential(l, r) ->
                    let! l = preprocessS l
                    let! r = preprocessS r
                    return Expr.Seq [l;r]
                    
                | IfThenElse(cond, i, e) ->
                    let! cond = preprocessS cond
                    let! i = preprocessS i
                    let! e = preprocessS e
                    return Expr.IfThenElse(cond, i, e)

                | Let(v, e, b) ->
                    let! e = preprocessS e
                    let! b = preprocessS b
                    return Expr.Let(v, e, b)

                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapS preprocessS
                    return RebuildShapeCombination(o, args)

                | ShapeVar _ ->
                    return e

                | ShapeLambda(v, b) ->
                    let! b = preprocessS b
                    return Expr.Lambda(v, b)
        }

    and getOutputValues (sem : string) (value : Expr) : Preprocess<list<string * Expr>> =
        state {
            if FSharpType.IsRecord(value.Type, true) then
                let fields = FSharpType.GetRecordFields(value.Type, true) |> Array.toList

                let! values = 
                    match value with
                        | NewRecord(_,args) ->
                            List.zip fields args |> List.mapS (fun (f,v) ->
                                state {
                                    let sem = f.Semantic
                                    let i = f.Interpolation
                                    let p = { paramType = f.PropertyType; paramInterpolation = i }
                                    do! State.writeOutput sem p

                                    let! real = preprocessS v
                                    return sem, real
                                }
                            )
                        | _ -> 
                            fields |> List.mapS (fun f ->
                                state {
                                    let sem = f.Semantic
                                    let i = f.Interpolation
                                    let p = { paramType = f.PropertyType; paramInterpolation = i }
                                    do! State.writeOutput sem p

                                    let! real = preprocessS (Expr.PropertyGet(value, f))
                                    return sem, real
                                }
                            )

                return values

            elif value.Type = typeof<V3d> then
                let! value = preprocessS value
                do! State.writeOutput sem { paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default }
                return [sem, <@@ V4d((%%value : V3d), 1.0) @@>]


            elif value.Type = typeof<V4d> then
                let! value = preprocessS value
                do! State.writeOutput sem { paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default }
                return [sem, value]

            else
                return failwithf "[FShade] invalid vertex-type: %A" value.Type

        }

    and toShaders (inputType : Type) (e : Expr) =
        let run = preprocessS e
        let mutable state = State.ofInputType inputType
        let body = run.Run(&state)

        // figure out the used builder-type
        let builder = 
            match state.builder with
                | Some builder -> 
                    match Expr.TryEval builder with
                        | Some (:? IShaderBuilder as v) -> v
                        | _ -> failwithf "[FShade] could not evaluate shader-builder %A" builder
                | _ ->
                    failwithf "[FShade] could not evaluate shader-builder %A" state.builder

        let shader = 
            { 
                shaderStage             = builder.ShaderStage
                shaderInputs            = state.inputs
                shaderOutputs           = state.outputs
                shaderUniforms          = state.uniforms
                shaderInputTopology     = state.inputTopology
                shaderOutputTopology    = builder.OutputTopology |> Option.map (fun t -> t, Int32.MaxValue)
                shaderBody              = body
            }

        shader :: state.shaders

    let preprocess (e : Expr) =
        let mutable state = State.empty
        let e = preprocessS(e).Run(&state)
        e, state

    let rec usedInputs (e : Expr) =
        match e with
            | ReadInput(kind,n,idx) ->
                match idx with
                    | Some idx -> usedInputs idx |> Map.add n (kind, e.Type)
                    | None -> Map.ofList [n, (kind, e.Type)]

            | ShapeCombination(o, args) ->
                args |> List.fold (fun m e -> Map.union m (usedInputs e)) Map.empty

            | ShapeLambda(_,b) ->
                usedInputs b

            | ShapeVar _ ->
                Map.empty


type ShaderOutputValue(mode : InterpolationMode, value : Expr) =
    let value, inputs, uniforms =
        let value, state = Preprocessor.preprocess value

        let inputs, uniforms =
            value
                |> Preprocessor.usedInputs 
                |> Map.partition (fun l (kind, t) -> kind = ParameterKind.Input)

        let inputs = 
            inputs |> Map.map (fun name (_,t) ->
                match Map.tryFind name state.inputs with
                    | Some p -> p
                    | None -> { paramType = t; paramInterpolation = InterpolationMode.Default }
            )
            
        let uniforms = 
            uniforms |> Map.map (fun name (_,t) ->
                match Map.tryFind name state.uniforms with
                    | Some p -> p
                    | None -> { uniformType = t; uniformName = name; uniformValue = UniformValue.Attribute(uniform, name) }
            )

        value, inputs, uniforms

    member x.Type = value.Type
    member x.Value = value
    member x.Interpolation = mode
    member x.UsedInputs = inputs
    member x.UsedUniforms = uniforms

    new(value : Expr) = ShaderOutputValue(InterpolationMode.Default, value)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderOutputValue =
    let inline value (o : ShaderOutputValue) = o.Value
    let inline interpolation (o : ShaderOutputValue) = o.Interpolation
    let inline outputType (o : ShaderOutputValue) = o.Type
    let inline usedInputs (o : ShaderOutputValue) = o.UsedInputs
    let inline usedUniforms (o : ShaderOutputValue) = o.UsedUniforms

    let toParameterDescription (o : ShaderOutputValue) =
        { paramType = o.Type; paramInterpolation = o.Interpolation }

    let ofParameterDescription (p : ParameterDescription) (value : Expr) =
        match value with
            | Coerce(value,_) -> 
                ShaderOutputValue(p.paramInterpolation, value)
            | value ->
                ShaderOutputValue(p.paramInterpolation, value)
                

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Shader =

    let private builtInInputs =
        Dictionary.ofList [
            ShaderStage.Vertex,
                Map.ofList [
                    Intrinsics.VertexId, typeof<int>
                    Intrinsics.InstanceId, typeof<int>
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.PatchVertices, typeof<int>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.InvocationId, typeof<int>
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.TessCoord, typeof<V3d>
                    Intrinsics.PatchVertices, typeof<int>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.TessLevelInner, typeof<Arr<2 N, float>>
                    Intrinsics.TessLevelOuter, typeof<Arr<4 N, float>>
                ]
                
            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.InvocationId, typeof<int>
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.FragCoord, typeof<V4d>
                    Intrinsics.PointCoord, typeof<V2d>
                    Intrinsics.FrontFacing, typeof<bool>
                    Intrinsics.SampleId, typeof<int>
                    Intrinsics.SamplePosition, typeof<V2d>
                    Intrinsics.SampleMask, typeof<int[]>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.Layer, typeof<int>
                    Intrinsics.ViewportIndex, typeof<int>
                ]

        ]

    let private builtInOutputs =
        Dictionary.ofList [
            ShaderStage.Vertex,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.TessLevelInner, typeof<Arr<2 N, float>>
                    Intrinsics.TessLevelOuter, typeof<Arr<4 N, float>>
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                ]

            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.Layer, typeof<int>
                    Intrinsics.ViewportIndex, typeof<int>
                    Intrinsics.SourceVertexIndex, typeof<int>
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.Depth, typeof<float>
                    Intrinsics.SampleMask, typeof<float[]>
                ]

        ]

    let private sideEffects =
        Dictionary.ofList [
            ShaderStage.Vertex, 
                HashSet.empty

            ShaderStage.TessControl, 
                HashSet.empty

            ShaderStage.TessEval, 
                HashSet.empty

            ShaderStage.Geometry, 
                HashSet.ofList [
                    getMethodInfo <@ emitVertex @>
                    getMethodInfo <@ restartStrip @>
                    getMethodInfo <@ endPrimitive @>
                ]

            ShaderStage.Fragment, 
                HashSet.ofList [
                    getMethodInfo <@ discard @>
                ]
        ]

    let private converter (inType : Type) (outType : Type) : Expr -> Expr =
        if inType <> outType then
            failwithf "[FShade] cannot convert value from %A to %A" inType outType
        else
            id
        
    let private tryGetSourceVertexIndex (shader : Shader) (values : Map<string, Expr>) =
        match shader.shaderInputTopology with
            | Some InputTopology.Point -> Expr.Value 0 |> Some
            | _ -> Map.tryFind Intrinsics.SourceVertexIndex values

    let private tryGetSourceVertexIndexValue (shader : Shader) (values : Map<string, ShaderOutputValue>) =
        match shader.shaderInputTopology with
            | Some InputTopology.Point -> ShaderOutputValue(Expr.Value 0) |> Some
            | _ -> Map.tryFind Intrinsics.SourceVertexIndex values

    let private emitVertexMeth = getMethodInfo <@ emitVertex @>

    let systemInputs (shader : Shader) =
        let builtIn = builtInInputs.[shader.shaderStage]
        shader.shaderInputs |> Map.choose (fun name p ->
            match Map.tryFind name builtIn with
                | Some neededType ->
                    let typesValid =
                        match neededType, p.paramType with
                            | (ArrOf(_, nt) | ArrayOf nt), (ArrOf(_, pt) | ArrayOf pt) -> nt = pt
                            | n, p -> n = p

                    if not typesValid then
                        failwithf "[FShade] invalid system-input type %A (should be %A)" p.paramType neededType

                    Some p.paramType
                | None ->
                    None
        )

    let systemOutputs (shader : Shader) =
        let builtIn = builtInOutputs.[shader.shaderStage]
        shader.shaderOutputs |> Map.choose (fun name p ->
            match Map.tryFind name builtIn with
                | Some neededType ->
                    let typesValid =
                        match neededType, p.paramType with
                            | (ArrOf(_, nt) | ArrayOf nt), (ArrOf(_, pt) | ArrayOf pt) -> nt = pt
                            | n, p -> n = p

                    if not typesValid then
                        failwithf "[FShade] invalid system-output type %A (should be %A)" p.paramType neededType

                    Some p.paramType
                | None ->
                    None
        )

    let inline stage (s : Shader) = s.shaderStage
    let inline uniforms (s : Shader) = s.shaderUniforms
    let inline body (s : Shader) = s.shaderBody
    let inline inputTopology (s : Shader) = s.shaderInputTopology
    let inline outputTopology (s : Shader) = s.shaderOutputTopology

    /// optimizes the shader by
    ///    1) evaluating constant expressions
    ///    2) removing useless expressions/variables
    ///    3) unrolling loops where annotated
    ///
    /// and might in the future:
    ///    3) inline copy variables
    ///    4) inline functions where possible
    let optimize (shader : Shader) =
        let sideEffects = sideEffects.[shader.shaderStage]
        let newBody = 
            shader.shaderBody
                |> Optimizer.evaluateConstants' sideEffects.Contains
                |> Optimizer.eliminateDeadCode' sideEffects.Contains

        let inputs = Preprocessor.usedInputs newBody

        let newOutputTopology =
            match shader.shaderStage with
                | ShaderStage.Geometry -> 
                    let top, _ = Option.get shader.shaderOutputTopology
                    let range = Expr.computeCallCount emitVertexMeth newBody
                    let maxVertices =
                        if range.Max = Int32.MaxValue then
                            Log.warn "[FShade] could not determine max-vertex-count (using 32)"
                            32
                        else
                            range.Max
                    Some (top, range.Max)
                | _ ->
                    None

        { shader with
            shaderInputs = shader.shaderInputs |> Map.filter (fun n _ -> inputs.ContainsKey n)
            shaderUniforms = shader.shaderUniforms |> Map.filter (fun n _ -> inputs.ContainsKey n)
            shaderOutputTopology = newOutputTopology
            shaderBody = newBody
        }

    let ofExpr (inputType : Type) (e : Expr) =
        Preprocessor.toShaders inputType e |> List.map optimize

    let ofFunction (shaderFunction : 'a -> Expr<'b>) =
        let expression = 
            try shaderFunction Unchecked.defaultof<'a>
            with _ -> failwith "[FShade] shader functions may not access their vertex-input statically"
        ofExpr typeof<'a> expression




    /// creates a new shader by modifying all output-writes.
    /// this can be used for adding/removing outputs for shaders
    let mapOutputs (f : Map<string, ShaderOutputValue> -> Map<string, ShaderOutputValue>) (shader : Shader) =
        let mutable newOutputs = Map.empty
        let mutable newInputs = shader.shaderInputs
        let mutable newUniforms = shader.shaderUniforms

        let addValue (name : string) (value : ShaderOutputValue) =
            newOutputs <- Map.add name (ShaderOutputValue.toParameterDescription value) newOutputs

            for (n, pd) in Map.toSeq value.UsedInputs do
                let inputParameter = 
                    match Map.tryFind n newInputs with
                        | Some i -> i
                        | None -> pd

                newInputs <- Map.add name inputParameter newInputs

            for (n, up) in Map.toSeq value.UsedUniforms do
                let uniformParameter =
                    match Map.tryFind n newUniforms with
                        | Some u -> u
                        | None -> up

                newUniforms <- Map.add n uniformParameter newUniforms

            value.Value

        let newBody =
            shader.shaderBody.SubstituteWrites (fun values ->
                values
                    |> Map.map (fun n -> ShaderOutputValue.ofParameterDescription shader.shaderOutputs.[n])
                    |> f
                    |> Map.map addValue
                    |> Expr.WriteOutputs
                    |> Some
            )

        let newShader =
            { shader with
                shaderUniforms          = newUniforms
                shaderInputs            = newInputs
                shaderOutputs           = newOutputs
                shaderBody              = newBody
            }

        let outputsRemoved = 
            shader.shaderOutputs |> Map.keys |> Seq.exists (fun n -> not (Map.containsKey n newOutputs))

        optimize newShader

    /// creates a new shader having exactly the the given outputs by:
    ///     1) removing the ones that are not needed
    ///     2) adding the ones that are not existing
    ///     3) changing the type (using value-conversions) for the ones with bad types
    /// NOTE: by passing typeof<obj> for a semantic the function does not 
    ///       change existing types but can obviously not add a new output for it.
    let withOutputs (outputs : Map<string, Type>) (shader : Shader) =
        let current = shader.shaderOutputs |> Map.map (fun n p -> p.paramType)

        if current = outputs then
            shader
        else
            shader |> mapOutputs (fun values ->
                outputs 
                    |> Map.map (fun n t ->
                        match Map.tryFind n values with
                            | Some value ->
                                if not (t.IsAssignableFrom value.Type) then
                                    ShaderOutputValue(value.Interpolation, converter value.Type t value.Value)
                                else
                                    value
                            | None ->
                                if t = typeof<obj> then
                                    failwithf "[FShade] cannot add output %A with object type" n
                                    
                                match shader.shaderStage with
                                    | ShaderStage.Vertex | ShaderStage.Fragment ->
                                        ShaderOutputValue(Expr.ReadInput(ParameterKind.Input, t, n))

                                    | ShaderStage.Geometry ->
                                        match tryGetSourceVertexIndexValue shader values with
                                            | Some index -> 
                                                ShaderOutputValue(Expr.ReadInput(ParameterKind.Input, t, n, index.Value))
                                            | None -> 
                                                failwithf "[FShade] cannot add output %A to GeometryShader since no SourceVertexIndex was available" n
                                    
                                    | _ ->
                                        failwith "[FShade] passing for tessellation not implemented"
                                                
                    )
            )

    let removeOutputs (semantics : Set<string>) (shader : Shader) =
        let desired = 
            shader.shaderOutputs |> Map.choose (fun name p ->
                if Set.contains name semantics then None
                else Some p.paramType
            )

        withOutputs desired shader

    /// translates a shader to an EntryPoint which can be used for compiling
    /// the shader to a CAst
    let toEntryPoint (prev : Option<Shader>) (s : Shader) (next : Option<Shader>) =
        let inputs = 
            s.shaderInputs |> Map.toList |> List.map (fun (n,i) -> 
                { 
                    paramName = n
                    paramSemantic = n
                    paramType = i.paramType
                    paramDecorations = Set.ofList [ParameterDecoration.Interpolation i.paramInterpolation]
                }
            )

        let outputs = 
            s.shaderOutputs |> Map.toList |> List.map (fun (n, i) -> 
                { 
                    paramName = n
                    paramSemantic = n
                    paramType = i.paramType
                    paramDecorations = Set.empty
                }
            )

        let uniforms =
            s.shaderUniforms |> Map.toList |> List.map (fun (n, u) ->
                let uniformBuffer = 
                    match u.uniformValue with
                        | Attribute(scope, name) -> Some scope.FullName
                        | _ -> None
                { 
                    uniformName = u.uniformName
                    uniformType = u.uniformType
                    uniformBuffer = uniformBuffer
                }
            )

        let prevStage = prev |> Option.map stage
        let nextStage = next |> Option.map stage

        let tessControlDecoration =
            match next with
                | Some tev when tev.shaderStage = ShaderStage.TessEval ->
                    let tevInputs = 
                        tev.shaderInputs |> Map.toList |> List.choose (fun (n,i) ->
                            if builtInInputs.[ShaderStage.TessEval].ContainsKey n || s.shaderOutputs.ContainsKey n then
                                None
                            else
                                Some { 
                                    paramName = n
                                    paramSemantic = n
                                    paramType = i.paramType
                                    paramDecorations = Set.ofList [ParameterDecoration.Interpolation i.paramInterpolation]
                                }
                        )

                    [ EntryDecoration.TessControlPassThru tevInputs ]
                | _ ->
                    []

        {
            conditional = s.shaderStage |> string |> Some
            entryName   = "main"
            inputs      = inputs
            outputs     = outputs
            uniforms    = uniforms
            arguments   = []
            body        = s.shaderBody
            decorations = 
                List.concat [
                    [ EntryDecoration.Stages { prev = prevStage; self = s.shaderStage; next = nextStage } ]
                    tessControlDecoration
                    s.shaderInputTopology |> Option.map EntryDecoration.InputTopology |> Option.toList
                    s.shaderOutputTopology |> Option.map EntryDecoration.OutputTopology |> Option.toList
                ]
        }

    /// creates a shader "passing-thru" all supplied attributes
    let passing (attributes : Map<string, Type>) (stage : ShaderStage) =
        match stage with
            | ShaderStage.Vertex | ShaderStage.Fragment ->
                let parameters = attributes |> Map.map (fun _ -> ParameterDescription.ofType)
                {
                    shaderStage             = stage
                    shaderInputs            = parameters
                    shaderOutputs           = parameters
                    shaderUniforms          = Map.empty
                    shaderInputTopology     = None
                    shaderOutputTopology    = None
                    shaderBody =
                        attributes
                            |> Map.map (fun n t -> Expr.ReadInput(ParameterKind.Input, t, n))
                            |> Expr.WriteOutputs
            
                }
            | _ ->
                failwith "[FShade] not implemented"
    
    /// creates a shader (for the given stage) with no in-/outputs
    let empty (stage : ShaderStage) = 
        passing Map.empty stage

    let inputs (shader : Shader) =
        let builtIn = builtInInputs.[shader.shaderStage]
        let inputs = 
            match shader.shaderStage with
                | ShaderStage.Fragment ->
                    shader.shaderInputs
                        |> Map.map (fun _ p -> p.paramType)
                        |> Map.add Intrinsics.Position typeof<V4d>

                | ShaderStage.Geometry ->
                    shader.shaderInputs
                        |> Map.map (fun _ p ->
                            match p.paramType with
                                | ArrOf(_,t) | ArrayOf t -> t
                                | t -> t
                        )

                | _ ->
                    shader.shaderInputs
                        |> Map.map (fun _ p -> p.paramType)

        Map.difference inputs builtIn    

    let outputs (shader : Shader) =
        match shader.shaderStage with
            | ShaderStage.Fragment ->
                shader.shaderOutputs
                    |> Map.map (fun _ p -> p.paramType)
                    |> Map.add Intrinsics.Depth typeof<float>
            | _ ->
                shader.shaderOutputs
                    |> Map.map (fun _ p -> p.paramType)
        

    module private Composition = 
        let simple (l : Shader) (r : Shader) =
            let needed  = Map.intersect l.shaderOutputs r.shaderInputs
            let passed  = Map.difference l.shaderOutputs r.shaderOutputs

            let lBody =
                l.shaderBody.SubstituteWrites (fun values ->
                    let variables =
                        needed |> Map.map (fun name (lv, rv) -> 
                            let variable = Var(name + "C", rv.paramType)
                            let converter = converter lv.paramType rv.paramType
                            variable, converter
                        )

                    let rBody =
                        r.shaderBody
                            |> Expr.substituteReads (fun kind t name idx ->
                                match kind with
                                    | ParameterKind.Input -> 
                                        match Map.tryFind name variables with
                                            | Some(v,_) -> Expr.Var v |> Some
                                            | _ -> None
                                    | _ ->
                                        None
                            )

                    let rBody = 
                        variables |> Map.fold (fun b name (var, convert) ->
                            Expr.Let(var, convert (Map.find name values), b)
                        ) rBody

                    let passedValues =
                        passed |> Map.map (fun name p ->
                            Map.find name values
                        )

                    if Map.isEmpty passedValues then
                        rBody |> Some
                    else
                        rBody
                            |> Expr.substituteWrites (fun rValues ->
                                Map.union passedValues rValues
                                    |> Expr.WriteOutputs
                                    |> Some
                            )
                            |> Some
                )

            optimize 
                { l with
                    shaderInputs = Map.union r.shaderInputs l.shaderInputs
                    shaderOutputs = Map.union l.shaderOutputs r.shaderOutputs
                    shaderUniforms = Map.union l.shaderUniforms r.shaderUniforms
                    shaderBody = lBody
                }

        let gsvs (lShader : Shader) (rShader : Shader) =
            // matched values (left-out <-> right-in)
            let needed  = Map.intersect lShader.shaderOutputs rShader.shaderInputs

            // pass-thru (left-out <-> not right-out)
            let passed  = Map.difference lShader.shaderOutputs rShader.shaderOutputs

            // unknown (right-in <-> not left-out)
            let unknown = Map.difference rShader.shaderInputs lShader.shaderOutputs |> Map.keys |> Set.ofSeq

            // prepare a set containing all valid outputs from right (which will be mutated in the substitution)
            let mutable finalOutputsFromRight = rShader.shaderOutputs |> Map.keys |> Set.ofSeq
            
            // modify the left-shader body including the computations from the right one
            let lBody =
                lShader.shaderBody.SubstituteWrites (fun values ->
                    let variables =
                        needed |> Map.map (fun name (lv, rv) -> 
                            let variable = Var(name + "C", rv.paramType)
                            let converter = converter lv.paramType rv.paramType
                            variable, converter
                        )

                    // try to find the unique vertex-index for this write
                    let vertexIndex = tryGetSourceVertexIndex lShader values

                    // preprocess the right shader
                    let rShader =
                        match vertexIndex with
                            | Some _ -> rShader
                            | None when Set.isEmpty unknown -> rShader
                            | None ->
                                // all outputs affected by unknown inputs cannot be written by the composed shader
                                let invalidOutputs = 
                                    let affected = Expr.getAffectedOutputsMap rShader.shaderBody
                                    unknown 
                                        |> Seq.map (fun u -> match Map.tryFind u affected with | Some a -> a | _ -> Set.empty) 
                                        |> Set.unionMany

                                // remove all invalid outputs from the final ones
                                finalOutputsFromRight <- Set.difference finalOutputsFromRight invalidOutputs

                                // remove all invalid outputs from the right shader
                                rShader |> removeOutputs invalidOutputs
                            
                    // wrap the right shader with let-bindings for all used inputs
                    let rBody = 
                        rShader.shaderBody
                            |> Expr.substituteReads (fun kind t name idx ->
                                match kind with
                                    | ParameterKind.Input -> 
                                        match Map.tryFind name variables with
                                            | Some(v,_) -> 
                                                Expr.Var v |> Some
                                            | None -> 
                                                match vertexIndex with
                                                    | Some vertexIndex -> 
                                                        match idx with
                                                            | None -> Some (Expr.ReadInput(kind, t, name, vertexIndex))
                                                            | Some i -> failwithf "[FShade] vertex shader reading indexed input %A not supported" name
                                                    | None ->
                                                        failwithf "[FShade] internal error in gsvs"
                                    | _ -> 
                                        None
                            )
                             
                    let rBody = 
                        variables |> Map.fold (fun b name (var, convert) ->
                            Expr.Let(var, convert (Map.find name values), b)
                        ) rBody

        
                    // if no outputs from left are "passed" then simply inline the right-shader
                    if Map.isEmpty passed then
                        rBody |> Some
                    
                    // otherwise modify it to include the "passed" outputs
                    else
                       
                        let passedValues = 
                            passed |> Map.map (fun name p -> Map.find name values)

                        rBody
                            |> Expr.substituteWrites (fun rValues ->
                                Map.union passedValues rValues
                                    |> Expr.WriteOutputs
                                    |> Some
                               )
                            |> Some
                )

            // determine the inputs used by the right-shader (may not be used)
            let rInputs = 
                rShader.shaderInputs |> Map.choose (fun name p ->
                    if Set.contains name unknown then
                        Some { p with paramType = p.paramType.MakeArrayType() }
                    else
                        None
                )

            // determine the outputs for the inlined right-shader
            let rOutputs =
                rShader.shaderOutputs |> Map.filter (fun name p -> Set.contains name finalOutputsFromRight)

            // return the new shader (possibly including unused inputs)
            // and optimize it (removing those)
            optimize 
                { lShader with
                    shaderInputs = Map.union rInputs lShader.shaderInputs
                    shaderOutputs = Map.union lShader.shaderOutputs rOutputs
                    shaderUniforms = Map.union lShader.shaderUniforms rShader.shaderUniforms
                    shaderBody = lBody
                }

    let compose2 (l : Shader) (r : Shader) =
        match l.shaderStage, r.shaderStage with

            // simple case: both are vertex/fragment
            | ShaderStage.Vertex, ShaderStage.Vertex
            | ShaderStage.Fragment, ShaderStage.Fragment ->
                Composition.simple l r

            // harder case: left is geometry and right is vertex
            | ShaderStage.Geometry, ShaderStage.Vertex ->
                Composition.gsvs l r


            | _ ->
                failwithf "[FShade] cannot compose %AShader with %AShader" l.shaderStage r.shaderStage

    let compose (l : #seq<Shader>) =
        use e = l.GetEnumerator()
        if e.MoveNext() then
            let mutable res = e.Current
            while e.MoveNext() do
                res <- compose2 res e.Current
            res
        else
            failwith "[FShade] cannot compose empty shader-sequence"      

