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


[<RequireQualifiedAccess>]
type ShaderOutputVertices =
    | Unknown
    | Computed of int
    | UserGiven of int

/// Shader encapsulates all information about a specific shader.
type Shader =
    {
        /// the shader's stage
        shaderStage : ShaderStage
        /// the used inputs for the shader
        shaderInputs : Map<string, ParameterDescription>
        /// the provided outputs written by the shader
        shaderOutputs : Map<string, ParameterDescription>
        /// the used uniforms for the shader
        shaderUniforms : Map<string, UniformParameter>
        /// the optional input-topology for the shader
        shaderInputTopology : Option<InputTopology>
        /// the optional output-topology for the shader (including the maximal vertex-count)
        shaderOutputTopology : Option<OutputTopology>
        /// the optional maximal vertex-count for the shader
        shaderOutputVertices : ShaderOutputVertices
        /// the body for the shader
        shaderBody : Expr
        /// the shader's source info (if any)
        shaderDebugRange : Option<DebugRange>
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

        let rec (|TrivialInput|_|) (e : Expr) =
            match e with
                | Value _
                | TupleGet(TrivialInput, _)
                | PropertyGet((None | Some TrivialInput), _, [])
                | ReadInput(_,_,(None | Some TrivialInput))
                | FieldGet((None | Some TrivialInput), _) ->
                    Some ()
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
            variableValues  : Map<Var, Expr>
            shaders         : list<Shader>
            localSize       : V3i
        }
        
    let shaderUtilityFunctions = System.Collections.Concurrent.ConcurrentDictionary<V3i * MethodBase, Option<Expr * State>>()
    

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
                variableValues  = Map.empty
                shaders         = []
                localSize       = V3i.Zero
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
                match Map.tryFind p.uniformName s.uniforms with
                    | Some u -> s
                    | None -> { s with State.uniforms = Map.add p.uniformName p s.uniforms }
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

        let setVariableValue (v : Var) (value : Expr) =
            State.modify (fun s ->
                { s with variableValues = Map.add v value s.variableValues }
            )
            


        let tryGetVertexIndex (v : Var) =
            State.get |> State.map (fun s -> Map.tryFind v s.vertexIndex)

    let rec preprocessComputeS (e : Expr) : Preprocess<Expr> =
        state {
            match e with
                | GetArray(ValueWithName(v, t, name), i) ->
                    let! i = preprocessComputeS i
                    match t with
                        | ArrOf(_,t) | ArrayOf t ->
                            return Expr.ReadInput(ParameterKind.Input, t, name, i)
                        | _ ->
                            return e

                | SetArray(ValueWithName(v, t, name), i, e) ->
                    let! i = preprocessComputeS i
                    let! e = preprocessComputeS e
                    return Expr.WriteOutputsRaw([name, Some i, e])

                | PropertyGet(Some (ValueWithName(v, t, name)), prop, []) when t.IsArray && (prop.Name = "Length" || prop.Name = "LongLength") ->
                    return Expr.ReadInput(ParameterKind.Uniform, typeof<int>, "cs_" + name + "_length")
                    
                | ValueWithName(v,t,name) ->
                    return Expr.ReadInput(ParameterKind.Uniform, t, "cs_" + name)

                | PropertyGet(None, pi, []) when pi.Name = "LocalSize" ->
                    let! s = State.get
                    return Expr.Value(s.localSize)

                | FieldGet(None, pi) when pi.Name = "LocalSize" ->
                    let! s = State.get
                    return Expr.Value(s.localSize)

                | Call(None, mi, [size]) when mi.Name = "allocateShared" ->
                    return failwith "[FShade] non-static call to allocateShared"

                | Let(v, Call(None, mi, [size]),b) when mi.Name = "allocateShared" ->
                    let! size = preprocessComputeS size
                    match Expr.TryEval size with
                        | Some (:? int as size) ->
                            let et = mi.ReturnType.GetElementType()
                            let t = Peano.getArrayType size et


                            let name = sprintf "%s_%sx%d" v.Name et.PrettyName size

                            let rep = Expr.ReadInput(ParameterKind.Uniform, t, name)
                            let rec substitute (e : Expr) =
                                match e with

                                    | GetArray(Var vv, index) when vv = v ->
                                        let index = substitute index
                                        Peano.getItem rep index

                                    | SetArray(Var vv, index, value) when vv = v ->
                                        let index = substitute index
                                        let value = substitute value
                                        Peano.setItem rep index value

                                    

                                    | ShapeCombination(o, args) -> RebuildShapeCombination(o, List.map substitute args)
                                    | ShapeLambda(v,b) -> Expr.Lambda(v, substitute b)
                                    | ShapeVar vv -> 
                                        if vv = v then failwith "[FShade] cannot use shared memory as value"
                                        Expr.Var vv


                        //    {
                        //        uniformName         : string
                        //        uniformType         : Type
                        //        uniformValue        : UniformValue
                        //    }


                            let b = substitute b
                            do! State.readUniform { uniformName = name; uniformType = t; uniformValue = UniformValue.Attribute(uniform?SharedMemory, name) }

                            return! preprocessComputeS b

                        | _ ->
                            return failwith "[FShade] could not evaluate size for allocateShared"

                | ShapeLambda(v, b) ->
                    let! b = preprocessComputeS b
                    return Expr.Lambda(v, b)

                | ShapeVar(v) ->
                    return e

                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapS preprocessComputeS
                    return RebuildShapeCombination(o, args)
          
        }

    let rec preprocessNormalS (e : Expr) : Preprocess<Expr> =
        state {
            let! vertexType = State.vertexType

            match e with
                | ReadInput(kind, name, idx) ->
                    let! idx = idx |> Option.mapS preprocessNormalS

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

                | WriteOutputs(map) ->
                    let! map = 
                        map |> Map.mapS (fun name (idx, value) ->
                            state {
                                let! idx = idx |> Option.mapS preprocessNormalS
                                let! value = preprocessNormalS value
                                return idx, value
                            }
                        )
                    for (name, (idx, value)) in Map.toSeq map do
                        do! State.modify (fun s ->
                                if Map.containsKey name s.outputs then 
                                    s
                                else 
                                    let typ =
                                        match idx with
                                            | None -> value.Type
                                            | Some _ -> value.Type.MakeArrayType()
                                    { s with State.outputs = Map.add name { paramType = typ; paramInterpolation = InterpolationMode.Default } s.outputs }
                            ) 


                    return Expr.WriteOutputs(map)

                | BuilderRun(b, body)
                | BuilderDelay(b, body) ->
                    do! State.setBuilder b
                    return! preprocessNormalS body

                | BuilderCombine(b, l, r) ->
                    do! State.setBuilder b
                    let! l = preprocessNormalS l
                    let! r = preprocessNormalS r
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

                    let! s = State.get
                    let! bindings, free = 
                        tev.GetFreeVars() 
                            |> Seq.toList 
                            |> List.choose2S (fun v ->
                                state {
                                    match Map.tryFind v s.variableValues with
                                        | Some (ReadInput(ParameterKind.Input, name, Some TrivialInput) as e) ->
                                            return Choice1Of2 (v, e)
                                        | _ -> 
                                            do! State.writeOutput v.Name { paramType = v.Type; paramInterpolation = InterpolationMode.Default }
                                            return Choice2Of2(v, Expr.ReadInput(ParameterKind.Input, v.Type, v.Name))
                                }
                            )

                    let rec wrap (bindings : list<Var * Expr>) (b : Expr) =
                        match bindings with
                            | [] -> b
                            | (h, he) :: rest ->
                                Expr.Let(h, he, wrap rest b)
                        
                    let inputType = s.inputType
                    let tev = wrap (bindings @ free) tev |> toShaders inputType s.vertexIndex
                    match tev with
                        | [tev] ->
                            do! State.modify (fun s -> { s with shaders = [ { tev with shaderStage = ShaderStage.TessEval } ] })
                        | _ ->
                            failwithf "[FShade] invalid shader(s) after tessellate-call: %A" tev

                    do! State.writeOutput Intrinsics.TessLevelInner { paramType = typeof<float[]>; paramInterpolation = InterpolationMode.Default }
                    do! State.writeOutput Intrinsics.TessLevelOuter { paramType = typeof<float[]>; paramInterpolation = InterpolationMode.Default }

                    return 
                        Expr.WriteOutputs [
                            yield Intrinsics.TessLevelInner, None, Expr.NewArray(typeof<float>, inner)
                            yield Intrinsics.TessLevelOuter, None, Expr.NewArray(typeof<float>, outer)
                            yield! free |> List.map (fun (v,_) -> v.Name, None, Expr.Var v)
                        ]


                | BuilderUsing(b, var, value, body)
                | BuilderBind(b, var, value, body) ->
                    do! State.setBuilder b
                    let! value = preprocessNormalS value
                    let! body = preprocessNormalS body
                    if var.Type <> typeof<unit> then
                        return Expr.Let(var, value, body)
                    else
                        match value with
                            | Unit -> return body
                            | _ -> return Expr.Sequential(value, body)

                | BuilderFor(b, var, RangeSequence(first, step, last), body) ->
                    do! State.setBuilder b
                    let! first = preprocessNormalS first
                    let! step = preprocessNormalS step
                    let! last = preprocessNormalS last
                    let! body = preprocessNormalS body
                    return Expr.ForInteger(var, first, step, last, body)
                    
                | BuilderFor(b, var, Coerce(Primitive(primitive, vertexCount), _), body) ->
                    do! State.setBuilder b
                    let iVar = Var(var.Name + "Index", typeof<int>)
                    let prop = primitive.Type.GetProperty("Item", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                    let replacement = Expr.PropertyGet(primitive, prop, [Expr.Var iVar])
                    
                    do! State.setVertexIndex var (Expr.Var iVar)
                    let! body = preprocessNormalS body

                    return Expr.ForIntegerRangeLoop(iVar, Expr.Value 0, Expr.Value (vertexCount - 1), body)

                | BuilderFor(b, var, sequence, body) ->
                    do! State.setBuilder b
                    let! sequence = preprocessNormalS sequence
                    let! body = preprocessNormalS body
                    return Expr.ForEach(var, sequence, body)

                | BuilderWhile(b, Lambda(unitVar, guard), body) ->
                    do! State.setBuilder b
                    let! guard = preprocessNormalS guard
                    let! body = preprocessNormalS body
                    return Expr.WhileLoop(guard, body)
                    

                | Uniform u ->
                    do! State.readUniform u
                    return Expr.ReadInput(ParameterKind.Uniform, e.Type, u.uniformName)

                // let p0 = tri.P0 in <body>
                // store (p0 -> 0) and preprocess <body>
                | Let(var, PrimitiveVertexGet(p, index), body) when var.Type = vertexType ->
                    let! index = preprocessNormalS index
                    do! State.setVertexIndex var index
                    return! preprocessNormalS body


                | Let(var, e, body) ->
                    let! e = preprocessNormalS e
                    do! State.setVariableValue var e
                    let! body = preprocessNormalS body
                    return Expr.Let(var, e, body)

                // tri.P0.pos -> ReadInput(pos, 0)
                | InputRead vertexType (PrimitiveVertexGet(p, index), semantic, parameter) ->
                    if semantic = Intrinsics.SourceVertexIndex then
                        let! index = preprocessNormalS index
                        return index
                    else
                        let! index = preprocessNormalS index
                        do! State.readInput semantic { parameter with paramType = parameter.paramType.MakeArrayType() }
                        return Expr.ReadInput(ParameterKind.Input, e.Type, semantic, index)

                // real vertex-read needed
                | PrimitiveVertexGet(p, index) ->
                    let! index = preprocessNormalS index
                    let fields = FSharpType.GetRecordFields(e.Type, true) |> Array.toList

                    let! args =
                        fields |> List.mapS (fun f ->
                            state {
                                let interpolation = f.Interpolation
                                let semantic = f.Semantic 
                                if semantic = Intrinsics.SourceVertexIndex then
                                    return index
                                else
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
                            if semantic = Intrinsics.SourceVertexIndex then
                                return index
                            else
                                do! State.readInput semantic { parameter with paramType = parameter.paramType.MakeArrayType() }
                                return Expr.ReadInput(ParameterKind.Input, parameter.paramType, semantic, index)
                        | _ ->
                            do! State.readInput semantic parameter
                            return Expr.ReadInput(ParameterKind.Input, parameter.paramType, semantic)
                    
                    
                | BuilderYield(b, mi, Let(var, value, body)) | BuilderReturn(b, mi, Let(var, value, body)) ->
                    let mutable used = 0
                    let newBody =
                        body.Substitute (fun vi -> 
                            if vi = var then 
                                used <- used + 1
                                Some value 
                            else 
                                None
                        )

                    let real = 
                        if used <= 1 then Expr.Call(b, mi, [newBody])
                        else Expr.Let(var, value, Expr.Call(b, mi, [body]))

                    return! preprocessNormalS real

                    
                | BuilderYield(b, _, value) ->
                    do! State.setBuilder b
                    let! value = preprocessNormalS value
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
                    let! value = preprocessNormalS value
                    let defaultSem = 
                        if b.Type = typeof<FragmentBuilder> then Intrinsics.Color
                        else Intrinsics.Position

                    let! values = getOutputValues defaultSem value

                    return Expr.WriteOutputs values

                | Sequential(l, r) ->
                    let! l = preprocessNormalS l
                    let! r = preprocessNormalS r
                    return Expr.Seq [l;r]
                    
                | IfThenElse(cond, i, e) ->
                    let! cond = preprocessNormalS cond
                    let! i = preprocessNormalS i
                    let! e = preprocessNormalS e
                    return Expr.IfThenElse(cond, i, e)

                | Let(v, e, b) ->
                    let! e = preprocessNormalS e
                    let! b = preprocessNormalS b
                    return Expr.Let(v, e, b)

                | CallFunction(utility, args) ->
                    let! args = args |> List.mapS preprocessNormalS
                    match utility.functionTag with
                        | :? State as innerState ->
                            do! State.modify (fun s -> { s with uniforms = Map.union s.uniforms innerState.uniforms })
                            return Expr.CallFunction(utility, args)

                        | _ ->
                            
                            let mutable innerState = State.empty
                            let processedF =
                                utility |> UtilityFunction.map (fun b -> 
                                    let run : Preprocess<Expr> = preprocessS b
                                    run.Run(&innerState)
                                )
                                

                            let processedF = { processedF with functionTag = innerState }
                            do! State.modify (fun s -> { s with uniforms = Map.union s.uniforms innerState.uniforms })

                            return Expr.CallFunction(processedF, args)

                | Call(t, mi, args) ->
                    let! args = args |> List.mapS preprocessNormalS
                    let! t = t |> Option.mapS preprocessNormalS

                    match UtilityFunction.tryCreate mi with
                        | Some utility ->

                            let mutable innerState = State.empty
                            let processedF =
                                utility |> UtilityFunction.map (fun b -> 
                                    let run : Preprocess<Expr> = preprocessS b
                                    run.Run(&innerState)
                                )

                            let processedF = { processedF with functionTag = innerState }
                            do! State.modify (fun s -> { s with uniforms = Map.union s.uniforms innerState.uniforms })

                            match t with    
                                | Some t -> return Expr.CallFunction(processedF, t :: args)
                                | None -> return Expr.CallFunction(processedF, args)

                        | None -> 
                            match t with
                                | Some t -> return Expr.Call(t, mi, args)
                                | None -> return Expr.Call(mi, args)
//                            
//                    let! s = State.get
//                    match preprocessMethod s.localSize mi with
//                        | Some(_, innerState) ->
//                            do! State.modify (fun s -> { s with uniforms = Map.union s.uniforms innerState.uniforms })
//                        | None ->
//                            ()
//
//                    let! args = args |> List.mapS preprocessNormalS
//                    let! t = t |> Option.mapS preprocessNormalS
//
//                    match t with
//                        | Some t -> return Expr.Call(t, mi, args)
//                        | None -> return Expr.Call(mi, args)
                            


                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapS preprocessNormalS
                    return RebuildShapeCombination(o, args)

                | ShapeVar _ ->
                    return e

                | ShapeLambda(v, b) ->
                    let! b = preprocessNormalS b
                    return Expr.Lambda(v, b)
        }

    and preprocessS (e : Expr) =
        state {
            let! s = State.get
            let! e0 = 
                if s.localSize <> V3i.Zero then preprocessComputeS e
                else State.value e

            let! e1 = preprocessNormalS e0
            return e1
        }

    and getOutputValues (sem : string) (value : Expr) : Preprocess<list<string * Option<Expr> * Expr>> =
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
                                    return sem, None, real
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
                                    return sem, None, real
                                }
                            )

                return values

            elif value.Type = typeof<V3d> then
                let! value = preprocessS value
                do! State.writeOutput sem { paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default }
                return [sem, None, <@@ V4d((%%value : V3d), 1.0) @@>]


            elif value.Type = typeof<V4d> then
                let! value = preprocessS value
                do! State.writeOutput sem { paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default }
                return [sem, None, value]

            else
                return failwithf "[FShade] invalid vertex-type: %A" value.Type

        }

    and toShaders (inputType : Type) (vertexIndex : Map<Var, Expr>) (e : Expr) =
        let run = preprocessS e
        let mutable state = { State.ofInputType inputType with vertexIndex = vertexIndex }
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

        let body, outputs =
            match builder.ShaderStage, state.shaders with
                | ShaderStage.TessControl, _ :: _ ->
                    let newBody =
                        let invocationId = Expr.ReadInput<int>(ParameterKind.Input, Intrinsics.InvocationId)
                        Expr.Sequential(
                            Expr.IfThenElse(
                                <@ %invocationId = 0 @>,
                                body,
                                Expr.Unit
                            ),
                            Expr.WriteOutputs Map.empty
                        )

                    let newOutputs =
                        state.outputs |> Map.map (fun _ p -> 
                            { p with paramInterpolation = InterpolationMode.PerPatch }
                        )
                        

                    newBody, newOutputs
                | _ -> 
                    body, state.outputs

        let outputVertices =
            match builder with
                | :? GeometryBuilder as b -> 
                    match b.Size with
                        | Some s -> ShaderOutputVertices.UserGiven s
                        | None -> ShaderOutputVertices.Unknown
                | _ ->
                    ShaderOutputVertices.Unknown

        let shader = 
            { 
                shaderStage             = builder.ShaderStage
                shaderInputs            = state.inputs
                shaderOutputs           = outputs
                shaderUniforms          = state.uniforms
                shaderInputTopology     = state.inputTopology
                shaderOutputTopology    = builder.OutputTopology
                shaderOutputVertices    = outputVertices
                shaderBody              = body
                shaderDebugRange        = None
            }

        shader :: state.shaders

    and preprocess (localSize : V3i) (e : Expr) =
        let mutable state = { State.empty with localSize = localSize }
        let e = preprocessS(e).Run(&state)
        e, state

    and preprocessMethod (localSize : V3i) (mi : MethodBase) =    
        shaderUtilityFunctions.GetOrAdd((localSize, mi), fun (localSize, mi) ->
            match Expr.TryGetReflectedDefinition mi with
                | Some expr ->
                    preprocess localSize expr |> Some
                | None ->
                    None
        )

    let rec computeIO (localSize : V3i) (e : Expr) =
        match e with
            | ReadInput(kind,n,idx) ->
                match idx with
                    | Some idx -> computeIO localSize idx |> Map.add (n, kind) e.Type
                    | None -> Map.ofList [(n, kind), e.Type]

            | WriteOutputs values ->
                let used = 
                    values |> Map.fold (fun m _ (i,v) ->
                        let v = computeIO localSize v
                        let i = i |> Option.map (computeIO localSize) |> Option.defaultValue Map.empty
                        Map.union m (Map.union i v)
                    ) Map.empty

                let written =
                    values |> Map.toSeq |> Seq.map (fun (name, (i,v)) -> 
                        match i with
                            | Some _ -> (name, ParameterKind.Output), v.Type.MakeArrayType()
                            | _ -> (name, ParameterKind.Output), v.Type
                    ) |> Map.ofSeq

                Map.union used written

            
            | Call(t,mi,args) ->
                let inner = 
                    match preprocessMethod localSize mi with
                        | Some(e, _) -> computeIO localSize e
                        | None -> Map.empty

                Option.toList t @ args |> List.fold (fun s a -> Map.union s (computeIO localSize a)) inner


            | ShapeCombination(o, args) ->
                args |> List.fold (fun m e -> Map.union m (computeIO localSize e)) Map.empty

            | ShapeLambda(_,b) ->
                computeIO localSize b

            | ShapeVar _ ->
                Map.empty

    let rec usedInputs (localSize : V3i) (e : Expr) =
        match e with
            | ReadInput(kind,n,idx) ->
                match idx with
                    | Some idx -> usedInputs localSize idx |> Map.add n (kind, e.Type)
                    | None -> Map.ofList [n, (kind, e.Type)]
            
            | Call(t,mi,args) ->
                let inner = 
                    match preprocessMethod localSize mi with
                        | Some(e, _) -> usedInputs localSize e
                        | None -> Map.empty

                Option.toList t @ args |> List.fold (fun s a -> Map.union s (usedInputs localSize a)) inner


            | ShapeCombination(o, args) ->
                args |> List.fold (fun m e -> Map.union m (usedInputs localSize e)) Map.empty

            | ShapeLambda(_,b) ->
                usedInputs localSize b

            | ShapeVar _ ->
                Map.empty



type ShaderOutputValue (mode : InterpolationMode, index : Option<Expr>, value : Expr) =
    let value, inputs, uniforms =
        let value, state = Preprocessor.preprocess V3i.Zero value

        let inputs, uniforms =
            value
                |> Preprocessor.usedInputs V3i.Zero
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
    member x.Index = index
    member x.Value = value
    member x.Interpolation = mode
    member x.UsedInputs = inputs
    member x.UsedUniforms = uniforms

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderOutputValue =
    let inline value (o : ShaderOutputValue) = o.Value
    let inline interpolation (o : ShaderOutputValue) = o.Interpolation
    let inline outputType (o : ShaderOutputValue) = o.Type
    let inline usedInputs (o : ShaderOutputValue) = o.UsedInputs
    let inline usedUniforms (o : ShaderOutputValue) = o.UsedUniforms

    let toParameterDescription (o : ShaderOutputValue) =
        match o.Index with
            | Some i -> 
                { paramType = o.Type.MakeArrayType(); paramInterpolation = o.Interpolation }
            | _ ->
                { paramType = o.Type; paramInterpolation = o.Interpolation }

    let ofParameterDescription (p : ParameterDescription) (value : Expr) =
        match value with
            | Coerce(value,_) -> 
                ShaderOutputValue(p.paramInterpolation, None, value)
            | value ->
                ShaderOutputValue(p.paramInterpolation, None, value)
       
    let ofValue (value : Expr) =
        ShaderOutputValue(InterpolationMode.Default, None, value)

    let withValue (value : Expr) (o : ShaderOutputValue) =
        ShaderOutputValue(o.Interpolation, o.Index, value)  

[<AutoOpen>]
module ShaderOutputValueExtensions = 
     type Expr with
        static member WriteOutputs (values : Map<string, ShaderOutputValue>) =
            values
                |> Map.map (fun _ v -> v.Index, v.Value)
                |> Expr.WriteOutputs

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Shader =

    let tryGetOverrideCode (localSize : V3i) (mi : MethodBase) =
        match Preprocessor.preprocessMethod localSize mi with
            | Some (e,_) -> Some e
            | None -> None
             
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
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.Depth, typeof<float>
                    Intrinsics.SampleMask, typeof<int[]>
                ]

        ]

    let private sideEffects =
        Dictionary.ofList [
            ShaderStage.Vertex, 
                HashSet.ofList [
                    getMethodInfo <@ barrier @>
                ]

            ShaderStage.TessControl, 
                HashSet.ofList [
                    getMethodInfo <@ barrier @>
                ]

            ShaderStage.TessEval, 
                HashSet.ofList [
                    getMethodInfo <@ barrier @>
                ]

            ShaderStage.Geometry, 
                HashSet.ofList [
                    getMethodInfo <@ emitVertex @>
                    getMethodInfo <@ restartStrip @>
                    getMethodInfo <@ endPrimitive @>
                    getMethodInfo <@ barrier @>
                ]

            ShaderStage.Fragment, 
                HashSet.ofList [
                    getMethodInfo <@ discard @>
                    getMethodInfo <@ barrier @>
                ]

            ShaderStage.Compute, 
                HashSet.ofList [
                    getMethodInfo <@ barrier @>
                ]
        ]

    let private converter (inType : Type) (outType : Type) : Expr -> Expr =
        if inType <> outType then
            failwithf "[FShade] cannot convert value from %A to %A" inType outType
        else
            id
        
    let private tryGetSourceVertexIndex (shader : Shader) (values : Map<string, Option<Expr> * Expr>) =
        match shader.shaderInputTopology with
            | Some InputTopology.Point -> Expr.Value 0 |> Some
            | _ -> Map.tryFind Intrinsics.SourceVertexIndex values |> Option.map snd

    let private tryGetSourceVertexIndexValue (shader : Shader) (values : Map<string, ShaderOutputValue>) =
        match shader.shaderInputTopology with
            | Some InputTopology.Point -> Expr.Value 0 |> Some
            | _ -> Map.tryFind Intrinsics.SourceVertexIndex values |> Option.map ShaderOutputValue.value

    let private emitVertexMeth = getMethodInfo <@ emitVertex @>

    let private interpolate3 (coord : Expr) (p0 : Expr) (p1 : Expr) (p2 : Expr) =
        let coord : Expr<V3d> = Expr.Cast coord
        if p0.Type = typeof<float> then
            <@@ (%coord).X * (%%p0 : float) + (%coord).Y * (%%p1 : float) + (%coord).Z * (%%p2 : float)  @@>
        elif p0.Type = typeof<V2d> then
            <@@ (%coord).X * (%%p0 : V2d) + (%coord).Y * (%%p1 : V2d) + (%coord).Z * (%%p2 : V2d)  @@>
        elif p0.Type = typeof<V3d> then
            <@@ (%coord).X * (%%p0 : V3d) + (%coord).Y * (%%p1 : V3d) + (%coord).Z * (%%p2 : V3d)  @@>
        elif p0.Type = typeof<V4d> then
            <@@ (%coord).X * (%%p0 : V4d) + (%coord).Y * (%%p1 : V4d) + (%coord).Z * (%%p2 : V4d)  @@>
        else
            failwithf "[FShade] cannot interpolate %A" p0.Type

    let private interpolate4 (coord : Expr) (p0 : Expr) (p1 : Expr) (p2 : Expr) (p3 : Expr) =
        let coord : Expr<V3d> = Expr.Cast coord
        
        if p0.Type = typeof<float> then
            <@@ 
                (1.0 - (%coord).Y) * ((1.0 - (%coord).X) * (%%p0 : float) + (%coord).X * (%%p1 : float)) +
                (%coord).Y * ((1.0 - (%coord).X) * (%%p2 : float) + (%coord).X * (%%p3 : float)) 
            @@>
        elif p0.Type = typeof<V2d> then
            <@@ 
                (1.0 - (%coord).Y) * ((1.0 - (%coord).X) * (%%p0 : V2d) + (%coord).X * (%%p1 : V2d)) +
                (%coord).Y * ((1.0 - (%coord).X) * (%%p2 : V2d) + (%coord).X * (%%p3 : V2d)) 
            @@>
        elif p0.Type = typeof<V3d> then
            <@@ 
                (1.0 - (%coord).Y) * ((1.0 - (%coord).X) * (%%p0 : V3d) + (%coord).X * (%%p1 : V3d)) +
                (%coord).Y * ((1.0 - (%coord).X) * (%%p2 : V3d) + (%coord).X * (%%p3 : V3d)) 
            @@>
        elif p0.Type = typeof<V4d> then
            <@@ 
                (1.0 - (%coord).Y) * ((1.0 - (%coord).X) * (%%p0 : V4d) + (%coord).X * (%%p1 : V4d)) +
                (%coord).Y * ((1.0 - (%coord).X) * (%%p2 : V4d) + (%coord).X * (%%p3 : V4d)) 
            @@>
        else
            failwithf "[FShade] cannot interpolate %A" p0.Type


    let inline stage (s : Shader) = s.shaderStage
    let inline uniforms (s : Shader) = s.shaderUniforms
    let inline body (s : Shader) = s.shaderBody
    let inline inputTopology (s : Shader) = s.shaderInputTopology
    let inline outputTopology (s : Shader) = s.shaderOutputTopology

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
        let newBody, state = 
            shader.shaderBody
                |> Optimizer.inlining
                |> Optimizer.hoistImperativeConstructs
                |> Optimizer.evaluateConstants' sideEffects.Contains
                |> Optimizer.eliminateDeadCode' sideEffects.Contains
                |> Optimizer.evaluateConstants' sideEffects.Contains
                |> Optimizer.liftInputs
                |> Preprocessor.preprocess V3i.Zero

        let newOutputVertices =
            match shader.shaderStage with
                | ShaderStage.Geometry ->
                    match shader.shaderOutputVertices with
                        | ShaderOutputVertices.Computed _ | ShaderOutputVertices.Unknown ->
                            let range = Expr.computeCallCount emitVertexMeth newBody
                            let maxVertices =
                                if range.Max = Int32.MaxValue then
                                    Log.warn "[FShade] could not determine max-vertex-count (using 32)"
                                    32
                                else
                                    range.Max
                            ShaderOutputVertices.Computed range.Max
                        | ov ->
                            ov
                | _ ->
                    shader.shaderOutputVertices

        { shader with
            shaderInputs = state.inputs |> Map.map (fun name desc -> match Map.tryFind name shader.shaderInputs with | Some od when od.paramType = desc.paramType -> od | _ -> desc) //shader.shaderInputs |> Map.filter (fun n _ -> state.inputs.ContainsKey n)
            shaderUniforms = state.uniforms |> Map.map (fun name u -> match Map.tryFind name shader.shaderUniforms with Some ou when ou.uniformType = u.uniformType -> ou | _ -> u) //shader.shaderUniforms |> Map.filter (fun n _ -> state.uniforms.ContainsKey n)
            shaderOutputVertices = newOutputVertices
            shaderBody = newBody
        }

    let ofExpr (inputType : Type) (e : Expr) =
        let debugRange = e.CustomAttributes |> List.tryPick (function DebugRange r -> Some r | _ -> None)
        Preprocessor.toShaders inputType Map.empty e 
            |> List.map optimize
            |> List.map (fun shader -> { shader with shaderDebugRange = debugRange })

    let ofFunction (shaderFunction : 'a -> Expr<'b>) =
        let expression = 
            try shaderFunction Unchecked.defaultof<'a>
            with _ -> failwith "[FShade] shader functions may not access their vertex-input statically"

        ofExpr typeof<'a> expression

    let withBody (newBody : Expr) (shader : Shader) =
        let newBody, state = newBody |> Preprocessor.preprocess V3i.Zero
        let io = Preprocessor.computeIO V3i.Zero newBody

        let filterIO (desiredKind : ParameterKind) (build : string -> Type -> 'a) =

            io  |> Map.toSeq 
                |> Seq.choose (fun ((name, kind), t) -> 
                    if kind = desiredKind then 
                        Some (name, build name t)
                    else 
                        None
                )
                |> Map.ofSeq

        let newInputs = 
            filterIO ParameterKind.Input (fun name t ->
                match Map.tryFind name shader.shaderInputs with
                    | Some p -> p
                    | _ -> 
                        match Map.tryFind name state.inputs with
                            | Some p -> p
                            | None -> ParameterDescription.ofType t
            )

        let newOutputs = 
            filterIO ParameterKind.Output (fun name t ->
                match Map.tryFind name shader.shaderOutputs with
                    | Some p -> p
                    | _ -> 
                        match Map.tryFind name state.outputs with
                            | Some p -> p
                            | None -> ParameterDescription.ofType t
            )

        let newUniforms =
            filterIO ParameterKind.Uniform (fun name t ->
                match Map.tryFind name shader.shaderUniforms with
                    | Some p -> p
                    | None -> 
                        match Map.tryFind name state.uniforms with
                            | Some p -> p
                            | None -> { uniformName = name; uniformType = t; uniformValue = UniformValue.Attribute(uniform, name) }
            )

        optimize
            { shader with
                shaderUniforms          = newUniforms
                shaderInputs            = newInputs
                shaderOutputs           = newOutputs
                shaderBody              = newBody
            }


    /// creates a new shader by substituting output-writes with an epxression.
    /// when None is returned the respective write-expression will remain untouched
    let substituteWrites (f : Map<string, ShaderOutputValue> -> Option<Expr>) (shader : Shader) =
        let newBody =
            shader.shaderBody.SubstituteWrites (fun values ->
                values
                    |> Map.map (fun n (index, value) -> ShaderOutputValue(shader.shaderOutputs.[n].paramInterpolation, index, value))
                    |> f
            )

        shader |> withBody newBody
  
    let substituteReads (f : ParameterKind -> Type -> string -> Option<Expr> -> Option<Expr>) (shader : Shader) =
        let newBody =
            shader.shaderBody.SubstituteReads (fun k t n i ->
                f k t n i
            )

        shader |> withBody newBody

    let uniformsToInputs (semantics : Set<string>) (shader : Shader) =
        let needed = semantics |> Set.exists (fun n -> Map.containsKey n shader.shaderUniforms)
        if needed then
            shader |> substituteReads (fun kind inputType name index ->
                match kind with
                    | ParameterKind.Uniform when Set.contains name semantics ->
                        match index with
                            | Some index ->
                                failwith "[FShade] encountered indexed uniform-read" 
                            | None ->
                                match shader.shaderStage with
                                    | ShaderStage.Vertex | ShaderStage.Fragment ->
                                        Expr.ReadInput(ParameterKind.Input, inputType, name) |> Some

                                    | ShaderStage.Geometry | ShaderStage.TessControl | ShaderStage.TessEval ->
                                        Log.warn "[FShade] potentially bad uniform->input conversion (using vertex 0)"
                                        Expr.ReadInput(ParameterKind.Input, inputType, name, Expr.Value 0) |> Some

                                    | stage ->
                                        failwithf "[FShade] unknown ShaderStage %A" stage
                    | _ ->
                        None
            )
        else
            shader

    let inputsToUniforms (scopes : Map<string, UniformScope>) (shader : Shader) =
        let newBody = shader.shaderBody

        // replace all input-reads (on the given semantics) with uniform-reads
        let newBody = 
            if scopes |> Map.exists (fun n _ -> Map.containsKey n shader.shaderInputs) then
                let semantics = scopes |> Map.keys |> Set.ofSeq
                newBody |> Expr.substituteReads (fun kind inputType name index ->
                    match kind with
                        | ParameterKind.Input when Set.contains name semantics ->
                            match index with
                                | Some index -> 
                                    let inputType = 
                                        if inputType.IsArray then inputType.GetElementType()
                                        else inputType

                                    Expr.ReadInput(ParameterKind.Uniform, inputType, name) |> Some
                                | None ->
                                    Expr.ReadInput(ParameterKind.Uniform, inputType, name) |> Some
                        | _ ->
                            None
                )
            else
                newBody

        // remove all writes to the given semantics
        let newBody =
            if scopes |> Map.exists (fun n _ -> Map.containsKey n shader.shaderOutputs) then 
                newBody |> Expr.substituteWrites (fun values ->
                    Map.difference values scopes
                        |> Expr.WriteOutputs
                        |> Some
                )
            else
                newBody

        // if the body was untouched simply return the old shader
        if newBody = shader.shaderBody then 
            shader
        else 
            let newShader = withBody newBody shader
            { newShader with
                shaderUniforms = 
                    newShader.shaderUniforms |> Map.map (fun name u ->
                        match Map.tryFind name scopes with
                            | Some scope -> { u with uniformValue = UniformValue.Attribute(scope, name) }
                            | _ -> u
                    )
            }

    /// creates a new shader having exactly the the given outputs by:
    ///     1) removing the ones that are not needed
    ///     2) adding the ones that are not existing
    ///     3) changing the type (using value-conversions) for the ones with bad types
    /// NOTE: by passing typeof<obj> for a semantic the function does not 
    ///       change existing types but can obviously not add a new output for it.
    let withOutputs (outputs : Map<string, Type>) (shader : Shader) =
        let current = shader.shaderOutputs |> Map.map (fun n p -> p.paramType)

        let allWrites (shader : Shader) =
            let rec allWrites (e : Expr) =
                match e with
                    | WriteOutputs values       -> Seq.singleton values
                    | ShapeLambda(_,b)          -> allWrites b
                    | ShapeVar _                -> Seq.empty
                    | ShapeCombination(_,args)  -> args |> Seq.collect allWrites

            allWrites shader.shaderBody

        if current = outputs then
            shader
        else
            let patchOutputs =
                if shader.shaderStage = ShaderStage.TessControl then
                    shader.shaderOutputs |> Map.filter (fun name p ->
                        p.paramInterpolation = InterpolationMode.PerPatch
                    )
                else
                    Map.empty

            shader |> substituteWrites (fun values ->
                let isPatchOutput = 
                    if shader.shaderStage = ShaderStage.TessControl then
                        values |> Map.exists (fun _ v -> v.Interpolation = InterpolationMode.PerPatch)
                    else
                        false

                if isPatchOutput then
                    let newValues = 
                        values |> Map.choose (fun n v ->
                            match Map.tryFind n outputs with
                                | Some t ->
                                    if t = v.Type then Some v
                                    else v |> ShaderOutputValue.withValue (converter v.Type t v.Value) |> Some
                                | None ->
                                    None
                        ) 

                    newValues |> Expr.WriteOutputs |> Some

                else
                    let outputs = Map.difference outputs patchOutputs

                    let write = 
                        if Map.containsKey Intrinsics.FragmentPosition outputs then
                            fun (m : Map<string, ShaderOutputValue>) ->
                                match Map.tryFind Intrinsics.Position m with
                                    | Some value ->
                                        match value.Value with
                                            | Trivial -> 
                                                Expr.WriteOutputs (Map.add Intrinsics.FragmentPosition value m)
                                                    |> Some
                                            | nonTrivial ->
                                                let v = Var("position", nonTrivial.Type)
                                                let ve = Expr.Var v
                                                Expr.Let(v, value.Value,
                                                    Expr.WriteOutputs(
                                                        m |> Map.add Intrinsics.Position (value |> ShaderOutputValue.withValue ve)
                                                          |> Map.add Intrinsics.FragmentPosition (value |> ShaderOutputValue.withValue ve)
                                                    )
                                                ) 
                                                |> Some

                                    | None ->
                                        failwith "[FShade] positions not written before fragment shader"
                        else
                            Expr.WriteOutputs >> Some

                    Map.difference outputs patchOutputs
                        |> Map.map (fun n t ->
                            match Map.tryFind n values with
                                | Some value ->
                                    if not (t.IsAssignableFrom value.Type) then
                                        value |> ShaderOutputValue.withValue (converter value.Type t value.Value)
                                    else
                                        value
                                | None ->
                                    if t = typeof<obj> then
                                        failwithf "[FShade] cannot add output %A with object type" n
                                    
                                    match shader.shaderStage with
                                        | ShaderStage.Vertex | ShaderStage.Fragment ->
                                            Expr.ReadInput(ParameterKind.Input, t, n)
                                                |> ShaderOutputValue.ofValue

                                        | ShaderStage.Geometry ->
                                            match tryGetSourceVertexIndexValue shader values with
                                                | Some index -> 
                                                    Expr.ReadInput(ParameterKind.Input, t, n, index)
                                                        |> ShaderOutputValue.ofValue
                                                | None -> 
                                                    failwithf "[FShade] cannot add output %A to GeometryShader since no SourceVertexIndex was available" n
                                    
                                        | ShaderStage.TessControl ->
                                            let invocationId = Expr.ReadInput<int>(ParameterKind.Input, Intrinsics.InvocationId).Raw
                                            let t = 
                                                match t with
                                                    | ArrayOf t -> t
                                                    | _ -> t
                                            ShaderOutputValue(
                                                InterpolationMode.Default, 
                                                Some invocationId, 
                                                Expr.ReadInput(ParameterKind.Input, t, n, invocationId)
                                            )

                                        | ShaderStage.TessEval ->
                                            match shader.shaderInputTopology.Value with
                                                | InputTopology.Patch 3 | InputTopology.Triangle ->
                                                    let coord = Expr.ReadInput<V3d>(ParameterKind.Input, Intrinsics.TessCoord)
                                                    let p0 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 0)
                                                    let p1 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 1)
                                                    let p2 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 2)
                                                    interpolate3 coord p0 p1 p2 |> ShaderOutputValue.ofValue

                                                | InputTopology.Patch 4 ->
                                                    let coord = Expr.ReadInput<V3d>(ParameterKind.Input, Intrinsics.TessCoord)
                                                    let p0 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 0)
                                                    let p1 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 1)
                                                    let p2 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 2)
                                                    let p3 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 3)
                                                    interpolate4 coord p0 p1 p2 p3 |> ShaderOutputValue.ofValue
                                                | _ ->
                                                    failwith "[FShade] cannot pass for n-ary tess-eval shader"

                                        | _ ->
                                            failwith "[FShade] passing for tessellation not implemented"
                                                
                        )
                        |> write
            )

    /// creates a new shader by removing all outputs given in semantics
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
                    paramDecorations = Set.ofList [ParameterDecoration.Interpolation i.paramInterpolation]
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
                    uniformDecorations = u.decorations
                }
            )

        let prevStage = prev |> Option.map stage
        let nextStage = next |> Option.map stage

  
        {
            conditional = s.shaderStage |> string |> Some
            entryName   = "main"
            inputs      = inputs
            outputs     = outputs
            uniforms    = uniforms
            arguments   = []
            body        = s.shaderBody
            decorations = 
                [
                    yield EntryDecoration.Stages { 
                        prev = prevStage
                        self = s.shaderStage
                        next = nextStage 
                    }

                    match s.shaderInputTopology with
                        | Some t -> yield EntryDecoration.InputTopology t
                        | None -> ()

                    match s.shaderOutputTopology with
                        | Some t -> yield EntryDecoration.OutputTopology t
                        | _ -> ()

                    match s.shaderOutputVertices with
                        | ShaderOutputVertices.Unknown -> ()
                        | ShaderOutputVertices.Computed v | ShaderOutputVertices.UserGiven v ->
                            yield EntryDecoration.OutputVertices v
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
                    shaderOutputVertices    = ShaderOutputVertices.Unknown
                    shaderBody =
                        attributes
                            |> Map.map (fun n t -> None, Expr.ReadInput(ParameterKind.Input, t, n))
                            |> Expr.WriteOutputs
                    shaderDebugRange = None
                }
            | _ ->
                failwith "[FShade] not implemented"
    
    /// creates a shader (for the given stage) with no in-/outputs
    let empty (stage : ShaderStage) = 
        passing Map.empty stage

    /// gets the needed inputs for the given shader
    let neededInputs (shader : Shader) =
        let builtIn = builtInInputs.[shader.shaderStage]
        let inputs = 
            match shader.shaderStage with
                | ShaderStage.Fragment ->
                    let inputs =
                        shader.shaderInputs
                            |> Map.map (fun _ p -> p.paramType)

                    match Map.tryFind Intrinsics.Position inputs with
                        | Some p ->
                            inputs |> Map.add Intrinsics.FragmentPosition typeof<V4d>
                        | None ->
                            inputs |> Map.add Intrinsics.Position typeof<V4d>

                | ShaderStage.Geometry | ShaderStage.TessControl ->
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
        
    /// gets the provided outputs for the given shader
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
                    // cannot compose to TessControl Shader
                    let values = values |> Map.map (fun _ (_,v) -> v)

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
                            None, Map.find name values
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
                    shaderDebugRange = None
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
                             
                    // cannot compose to TessControl Shader
                    let values = values |> Map.map (fun _ (_,v) -> v)

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
                            passed |> Map.map (fun name p -> None, Map.find name values)

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
                    shaderDebugRange = None
                }

        let private topologyCompatible (lOutput : Option<OutputTopology>) (rInput : Option<InputTopology>) =
            match lOutput, rInput with
                | Some OutputTopology.Points, Some InputTopology.Point
                | Some OutputTopology.LineStrip, Some InputTopology.Line 
                | Some OutputTopology.TriangleStrip, Some InputTopology.Triangle ->
                    true
                | _ -> 
                    false

        [<ReflectedDefinition>]
        type TriangleStream<'d when 'd :> INatural> =
            {
                indices                     : Arr<'d, int>
                mutable triangleCount       : int

                mutable p0                  : int
                mutable p1                  : int
                mutable p2                  : int
                mutable vs                  : int
            }

            static member Create() =
                { 
                    indices = Arr<'d, int>()
                    triangleCount = 0
                    p0 = -1
                    p1 = -1
                    p2 = -1
                    vs = 0
                }

            member x.EmitVertex(vi : int) =
                match x.vs with
                    | 0 -> 
                        x.p0 <- vi
                        x.vs <- 1
                    | 1 ->
                        x.p1 <- vi
                        x.vs <- 2
                    | 2 ->
                        x.p2 <- vi
                        x.vs <- 3

                        let bi = 3 * x.triangleCount
                        x.indices.[bi + 0] <- x.p0
                        x.indices.[bi + 1] <- x.p1
                        x.indices.[bi + 2] <- x.p2
                        
                        // 0 1 2   2 1 3   2 3 4
                        if x.triangleCount % 2 = 0 then
                            x.p0 <- x.p2
                            x.vs <- 2
                        else
                            x.p1 <- x.p2
                            x.vs <- 2
                        
                        x.triangleCount <- x.triangleCount + 1

                    | _ ->
                        ()

            [<Inline>]
            member x.Reset() =
                x.triangleCount <- 0
                x.vs <- 0
                x.p0 <- -1
                x.p1 <- -1
                x.p2 <- -1

            [<Inline>]
            member x.EndPrimitive() =
                x.vs <- 0
                
            [<Inline>]
            member x.GetIndex(pi : int, fvi : int) =
                x.indices.[3 * pi + fvi]


        let gsgs (lShader : Shader) (rShader : Shader) =
            if not (topologyCompatible lShader.shaderOutputTopology rShader.shaderInputTopology) then
                failwithf "[FShade] cannot compose geometryshaders with mismatching topologies: %A vs %A" lShader.shaderOutputTopology rShader.shaderInputTopology

            match lShader.shaderOutputVertices with
                | ShaderOutputVertices.Computed count | ShaderOutputVertices.UserGiven count ->
                    // pass all needed inputs along
                    let lShader = lShader |> withOutputs (rShader.shaderInputs |> Map.map (fun _ d -> d.paramType.GetElementType()))

                    // determine the maximal number of indices needed
                    let lOutputTopology = lShader.shaderOutputTopology.Value
                    let indexCount =
                        match lOutputTopology with
                            | OutputTopology.Points -> count
                            | OutputTopology.LineStrip -> 2 * (count - 1)
                            | OutputTopology.TriangleStrip -> 3 * (count - 2)

                    // introduce variables for all composition semantics
                    let composeVars = 
                        lShader.shaderOutputs |> Map.map (fun name desc ->
                            let arrType = Peano.getArrayType count desc.paramType
                            Var(name, arrType) 
                        )

                    // maintain an index 
                    let currentVertex = Var("currentVertex", typeof<int>, true)

                    let lBody =
                        lShader.shaderBody.SubstituteWrites (fun outputs ->
                            let result =
                                Expr.Seq [
                                    for (name, (i,o)) in Map.toSeq outputs do
                                        if Option.isSome i then failwithf "[FShade] indexed output-write not possible atm."

                                        let v = composeVars.[name]
                                        yield Expr.ArraySet(Expr.Var v, Expr.Var currentVertex, o)
                                ]

                            Some result
                        )

                    let streamType = typedefof<TriangleStream<_>>.MakeGenericType [| Peano.getPeanoType indexCount |]
                    let stream = Var("stream", streamType, true)
                    let streamCreate = streamType.GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
                    let streamEmit = streamType.GetMethod("EmitVertex", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                    let streamRestart = streamType.GetMethod("EndPrimitive", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                    let streamGetIndex = streamType.GetMethod("GetIndex", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                    let streamReset = streamType.GetMethod("Reset", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)

                    let streamCount = streamType.GetProperty("triangleCount", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                    let streamIndices = streamType.GetProperty("indices", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)


                    let lBody =
                        let rec replace (e : Expr) =
                            match e with
                                | SpecificCall <@ restartStrip @> _
                                | SpecificCall <@ endPrimitive @> _ ->
                                    Expr.Call(Expr.Var stream, streamRestart, [])
                                
                                | SpecificCall <@ emitVertex @> _ ->
                                    Expr.Seq [
                                        Expr.Call(Expr.Var stream, streamEmit, [Expr.Var currentVertex])
                                        Expr.VarSet(currentVertex, <@ (%%(Expr.Var(currentVertex)) : int) + 1 @>)
                                    ]

                                | ShapeLambda(v,b) -> Expr.Lambda(v, replace b)
                                | ShapeCombination(o, args) -> RebuildShapeCombination(o, args |> List.map replace)
                                | ShapeVar v -> Expr.Var v

                        replace lBody

                    let rec bind (l : list<Var * Option<Expr>>) (body : Expr) =
                        match l with
                            | [] -> body
                            | (v,e) :: rest ->
                                match e with
                                    | Some e -> Expr.Let(v, e, bind rest body)
                                    | None -> Expr.Let(v, Expr.DefaultValue v.Type, bind rest body) 

                    let variables =
                        [
                            for v in Map.values composeVars do
                                yield v, None
                            yield currentVertex, Some (Expr.Value 0)
                            yield stream, None //Some (Expr.Call(streamCreate, []))
                        ]   


                    let rInputCount =
                        match rShader.shaderInputTopology.Value with
                            | InputTopology.Point -> 1
                            | InputTopology.Line -> 2
                            | InputTopology.Triangle -> 3
                            | t -> failwithf "[FShade] bad input topology %A" t

                    let rPrimitiveId = Var("primitiveId", typeof<int>)
                    let rIndices = Var("indices", Peano.getArrayType rInputCount typeof<int>)

                    let rBody =
                        let rBody =
                            rShader.shaderBody.SubstituteReads (fun kind typ name index ->
                                match kind with
                                    | ParameterKind.Input ->
                                        match index with
                                            | Some index ->
                                                let index = Expr.ArrayAccess(Expr.Var rIndices, index) 
                                                let v = composeVars.[name]
                                                Some (Expr.ArrayAccess(Expr.Var v, index))

                                            | None ->
                                                None
                                                //failwith "[FShade] GeometryShader cannot read non-indexed input"
                                    | _ ->
                                        None
                            )


                        let getIndex (pi : Expr) (i : int) =
                            Expr.ArrayAccess(
                                Expr.PropertyGet(Expr.Var stream, streamIndices, []),
                                <@@ 3 * (%%pi : int) + i @@>
                            )

                        let primtiveCount = Expr.PropertyGet(Expr.Var stream, streamCount)
                        Expr.ForIntegerRangeLoop(
                            rPrimitiveId, Expr.Value 0, <@@ (%%primtiveCount : int) - 1 @@>, 
                            Expr.Let(rIndices, Expr.DefaultValue rIndices.Type,
                                Expr.Seq [
                                    for i in 0 .. rInputCount - 1 do
                                        yield Expr.ArraySet(Expr.Var rIndices, Expr.Value i, getIndex (Expr.Var rPrimitiveId) i)
                                    yield rBody
                                    yield <@@ restartStrip() @@>
                                ]
                            )
                        )


                    let body =
                        bind variables (
                            Expr.Seq [
                                Expr.Call(Expr.Var stream, streamReset, [])
                                lBody
                                rBody
                            ]
                        )

                    let outputVerices =
                        match rShader.shaderOutputVertices with
                            | ShaderOutputVertices.UserGiven rc | ShaderOutputVertices.Computed rc ->
                                let maxPrimitiveCount = indexCount / rInputCount

                                ShaderOutputVertices.UserGiven (maxPrimitiveCount * rc)

                            | _ ->
                                ShaderOutputVertices.Unknown

                    optimize 
                        { rShader with
                            shaderBody = Preprocessor.preprocess V3i.Zero body |> fst
                            shaderInputTopology = lShader.shaderInputTopology
                            shaderUniforms = Map.union lShader.shaderUniforms rShader.shaderUniforms
                            shaderInputs = lShader.shaderInputs
                            shaderOutputVertices = outputVerices
                        }

                    //rShader |> withBody body

                | _ ->
                    failwithf "[FShade] cannot compose GeometryShader without vertex-count"




    /// composes two shaders respecting their stages.
    ///     - implemented: { Vertex->Vertex; Geometry->Vertex; Fragment->Fragment }
    ///     - future:           { Tessellation->Vertex; Geometry->Geometry }
    ///     - impossible/hard:  { Tessellation->Tessellation; Geometry->Tessellation }
    ///     - effect:           { Vertex->Tessellation; Vertex->Geometry; Vertex->Fragment; Tessellation->Geometry; Tessellation->Fragment; Geometry->Fragment }
    let compose2 (l : Shader) (r : Shader) =
        match l.shaderStage, r.shaderStage with

            // simple case: both are vertex/fragment
            | ShaderStage.Vertex, ShaderStage.Vertex
            | ShaderStage.Fragment, ShaderStage.Fragment ->
                Composition.simple l r

            // harder case: left is geometry and right is vertex
            | ShaderStage.Geometry, ShaderStage.Vertex ->
                Composition.gsvs l r

            | ShaderStage.Geometry, ShaderStage.Geometry ->
                Composition.gsgs l r

            | _ ->
                failwithf "[FShade] cannot compose %AShader with %AShader" l.shaderStage r.shaderStage

    /// composes many shaders respecting their stages.
    /// NOTE: the sequence cannot be empty
    let compose (l : #seq<Shader>) =
        use e = l.GetEnumerator()
        if e.MoveNext() then
            let mutable res = e.Current
            while e.MoveNext() do
                res <- compose2 res e.Current
            res
        else
            failwith "[FShade] cannot compose empty shader-sequence"      

