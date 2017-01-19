namespace FShade

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative


type Shader =
    {
        shaderType              : ShaderType
        shaderInputs            : Map<string, IOParameter>
        shaderOutputs           : Map<string, IOParameter>
        shaderUniforms          : Map<string, UniformParameter>
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
            uniforms : Dictionary<string, UniformParameter>
            mutable builder : Expr
        }

        member x.AddInput(semantic : string, p : IOParameter) =
            match x.inputs.TryGetValue semantic with
                | (true, old) ->
                    if old.paramType = p.paramType then ()
                    else failwithf "[FShade] conflicting input-types for %s (%s vs %s)" semantic (getPrettyName old.paramType) (getPrettyName p.paramType)
                | _ ->
                    x.inputs.[semantic] <- p


        member x.AddOutput(semantic : string, p : IOParameter) =
            match x.outputs.TryGetValue semantic with
                | (true, old) ->
                    if old.paramType = p.paramType then ()
                    else failwithf "[FShade] conflicting outputs-types for %s (%s vs %s)" semantic (getPrettyName old.paramType) (getPrettyName p.paramType)
                | _ ->
                    x.outputs.[semantic] <- p
            
        member x.AddUniform( u : UniformParameter) =
            match x.uniforms.TryGetValue u.uniformName with
                | (true, old) ->
                    if old.uniformType = u.uniformType then ()
                    else failwithf "[FShade] conflicting uniform-types for %s (%s vs %s)" u.uniformName (getPrettyName old.uniformType) (getPrettyName u.uniformType)
                | _ ->
                    x.uniforms.[u.uniformName] <- u

    module private Preprocessor =
        open System.Reflection
        open System.Collections.Generic
        open Aardvark.Base.ReflectionHelpers

        [<AutoOpen>]
        module private Helpers = 
            let (|VertexLoad|_|) (vertexType : Type) (e : Expr) =
                match e with
                    | PropertyGet(Some e, p, []) when e.Type = vertexType ->
                        let isRecordField = FSharpType.GetRecordFields(vertexType, true) |> Array.exists (fun pi -> pi = p)
                        if isRecordField then
                            let att = p.GetCustomAttributes<SemanticAttribute>(true) |> Seq.toList
                            match att with
                                | x::_ -> Some (e, x.Semantic, p)
                                | _ -> Some (e, p.Name, p)
                        else
                            failwith "[FShade] cannot access properties on vertex-input-type"
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

        let private deconstructRecord (e : Expr) =
            match e with
                | NewRecord(t, args) -> 
                    args

                | e ->
                    let fields = FSharpType.GetRecordFields(e.Type, true)
                    fields |> Array.toList |> List.map (fun f -> Expr.PropertyGet(e, f))

        let rec private writeOutput (state : PreprocessorState) (vertexIndices : Map<Var, Expr>) (vertex : Expr) =
            let fields = FSharpType.GetRecordFields(vertex.Type, true) |> Array.toList
            let values = deconstructRecord vertex |> List.map (preprocessInternal state vertexIndices)

            List.zip fields values
                |> List.collect (fun (f, v) ->
                    let v = preprocessInternal state vertexIndices v
                    let sem = f.Semantic
                    match v with
                        | NewFixedArray(len, et, args) ->
                            state.AddOutput(sem, { paramType = v.Type; paramInterpolation = InterpolationMode.Default })
                            args |> List.mapi (fun i a ->
                                Expr.WriteOutput(sem, Expr.Value i, a)
                            )
                            
                        | NewArray(et, args) ->
                            let outputType = Peano.getArrayType (List.length args) et
                            state.AddOutput(sem, { paramType = outputType; paramInterpolation = InterpolationMode.Default })
                            args |> List.mapi (fun i a ->
                                Expr.WriteOutput(sem, Expr.Value i, a)
                            )
                        | _ -> 
                            state.AddOutput(sem, { paramType = v.Type; paramInterpolation = InterpolationMode.Default })
                            [ Expr.WriteOutput(sem, v) ]
                   )
                |> Expr.Seq

        and private preprocessInternal (state : PreprocessorState) (vertexIndices : Map<Var, Expr>) (e : Expr) =
            match e with
                | BuilderCall(b, mi, [Lambda(v,body)]) when mi.Name = "Delay" ->
                    state.builder <- b
                    preprocessInternal state vertexIndices body

                | BuilderCall(b, Method("For",_), [seq; Lambda(v,Let(vi,Var(vo),body))]) ->  
                    state.builder <- b   
                    let body = preprocessInternal state vertexIndices body
                    let seq = preprocessInternal state vertexIndices seq            
                    let result = Expr.ForEach(vi, seq, body)
                    result

                | BuilderCall(b, Method("Combine",_), [l;r]) ->
                    state.builder <- b
                    let l = preprocessInternal state vertexIndices l
                    let r = preprocessInternal state vertexIndices r
                    Expr.Seq [l; r]

                | BuilderCall(b, Method("Zero",_), []) ->
                    state.builder <- b
                    Expr.Unit

                // for v in primitive do
                | BuilderCall(b, Method("For",_) , [Coerce(primitive, t); Lambda(v,Let(vi,Var(vo),body))]) 
                  when b.Type = typeof<GeometryBuilder> || b.Type = typeof<TessControlBuilder> || b.Type = typeof<TessEvalBuilder> ->
                    state.builder <- b

                    let count = primitive.Type.GetProperty("VertexCount", BindingFlags.Static ||| BindingFlags.Public).GetValue(null) |> unbox<int>
                
                    let index = Var("i", typeof<int>)
                    let vertexIndices = Map.add vi (Expr.Var index) vertexIndices
                    let body = preprocessInternal state vertexIndices body
                    Expr.ForIntegerRangeLoop(index, <@@ 0 @@>, <@@ count - 1 @@>,  body)


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
                            state.AddInput(n, { paramType = e.Type.MakeArrayType(); paramInterpolation = InterpolationMode.Default })
                            Expr.ReadInput(e.Type, n, Expr.Value index)
                        | None ->
                            e

                // primitive.[i].pos
                | MemberFieldGet(PropertyGet(Some p, pi, [index]), m) when pi.Name = "Item" ->
                    let sem = m.Semantic
                    state.AddInput(sem, { paramType = e.Type.MakeArrayType(); paramInterpolation = pi.Interpolation })
                    Expr.ReadInput(e.Type, sem, index)

                | VertexLoad state.vertexType (ve, sem, pi) ->
                    match ve with
                        | Value(null,_) -> 
                            state.AddInput(sem, { paramType = e.Type; paramInterpolation = InterpolationMode.Default })
                            Expr.ReadInput(e.Type, sem)

                        | Var v ->
                            match Map.tryFind v vertexIndices with
                                | Some i -> 
                                    state.AddInput(sem, { paramType = e.Type.MakeArrayType(); paramInterpolation = InterpolationMode.Default })
                                    Expr.ReadInput(e.Type, sem, i)
                                | _ -> 
                                    Expr.PropertyGet(ve, pi)
                                    //failwithf "[FShade] accessing vertex input with unknown index"

                        | e ->
                            Expr.PropertyGet(ve, pi)
                            //failwithf "[FShade] complex vertex expressions not supported: %A" ve

                // yield (let a = x+y in f(a))   ==>    let a = x + y in yield f(a)
                | BuilderCall(b, mi, [Let(v, e, inner)]) when mi.Name = "Yield" ->
                    state.builder <- b
                    Expr.Let(v, e, Expr.Call(b, mi, [inner])) |> preprocessInternal state vertexIndices


                // yield { a = 1; b = x + y }
                | BuilderCall(b, mi, [vertex]) when mi.Name = "Yield" && FSharpType.IsRecord vertex.Type ->
                    state.builder <- b
                    Expr.Seq [
                        writeOutput state vertexIndices vertex
                        <@ emitVertex() @>
                    ]

                // yield V4d.IOOI
                | BuilderCall(b, mi, [value]) when mi.Name = "Yield" ->
                    state.builder <- b
                    let value = preprocessInternal state vertexIndices value
                    state.AddOutput(Intrinsics.Position, { paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default })
                    let store = 
                        if value.Type = typeof<V4d> then 
                            Expr.WriteOutput(Intrinsics.Position, value)

                        elif value.Type = typeof<V3d> then 
                            let ctor = typeof<V4d>.GetConstructor [|typeof<V3d>; typeof<float> |]
                            Expr.WriteOutput(Intrinsics.Position, Expr.NewObject(ctor, [value; Expr.Value 1.0]))

                        else 
                            failwithf "[FShade] cannot yield %A" value
                    
                    Expr.Sequential(store, Expr.Call(getMethodInfo <@ emitVertex @>, []))

                // return (let a = x+y in f(a))   ==>    let a = x + y in return f(a)
                | BuilderCall(b, mi, [Let(v, e, inner)]) when mi.Name = "Return" ->
                    state.builder <- b
                    Expr.Let(v, e, Expr.Call(b, mi, [inner])) |> preprocessInternal state vertexIndices

                | BuilderCall(b, mi, [vertex]) when mi.Name = "Return" && FSharpType.IsRecord vertex.Type ->
                    state.builder <- b
                    writeOutput state vertexIndices vertex

                // return V4d.IOOI
                | BuilderCall(b, mi, [value]) when mi.Name = "Return" ->
                    state.builder <- b
                    let value = preprocessInternal state vertexIndices value
                    let sem = 
                        if b.Type = typeof<FragmentBuilder> then Intrinsics.Color
                        else Intrinsics.Position
                        
                    state.AddOutput(sem, { paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default })
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
                    uniforms = Dictionary()
                    builder = Expr.Value(())
                }

            let clean = input |> preprocessInternal state Map.empty


            clean, state


    


    let inline shaderType (s : Shader) = s.shaderType
    let inline uniforms (s : Shader) = s.shaderUniforms
    let inline inputs (s : Shader) = s.shaderInputs
    let inline outputs (s : Shader) = s.shaderOutputs
    let inline body (s : Shader) = s.shaderBody
    let inline inputTopology (s : Shader) = s.shaderInputTopology
    let inline outputTopology (s : Shader) = s.shaderOutputTopology
      
    [<AutoOpen>]
    module private Utilities =   
        module Map =
            let keys (m : Map<'a, 'b>) = m |> Map.toSeq |> Seq.map fst
            let values (m : Map<'a, 'b>) = m |> Map.toSeq |> Seq.map snd

        let usedInputs (e : Expr) =
            let rec visit (used : HashSet<string>) (e : Expr) =
                match e with
                    | ReadInput(name, idx) ->
                        idx |> Option.iter (visit used)
                        used.Add name |> ignore

                    | ShapeVar v ->
                        ()

                    | ShapeLambda(v,b) ->
                        visit used b

                    | ShapeCombination(o, args) ->
                        for a in args do
                            visit used a

            let used = HashSet<string>()
            visit used e
            used
        
        let rec append (writes : list<Expr>) (e : Expr) =
            match e with
                | Let(v,e,b) -> Expr.Let(v, e, append writes b)
                | Sequential(l, r) -> Expr.Sequential(l, append writes r)
                | IfThenElse(c, i, e) -> Expr.IfThenElse(c, append writes i, append writes e)


                | WriteOutput _ -> Expr.Seq (e :: writes)

                | _ -> failwith ""

    let ofExpr (inputType : Type) (e : Expr) =
        let body, state = Preprocessor.preprocess inputType e

        // figure out the used builder-type
        let builder = 
            match Expr.TryEval state.builder with
                | Some (:? IShaderBuilder as v) -> v
                | _ -> failwithf "[FShade] could not evaluate shader-builder %A" state.builder

        { 
            shaderType              = builder.ShaderType
            shaderInputs            = state.inputs |> Dictionary.toMap
            shaderOutputs           = state.outputs |> Dictionary.toMap
            shaderUniforms          = state.uniforms |> Dictionary.toMap
            shaderInputTopology     = state.inputTopology
            shaderOutputTopology    = builder.OutputTopology
            shaderBody              = body
        }

    let ofFunction (f : 'a -> Expr<'b>) =
        let e = f Unchecked.defaultof<'a>
        ofExpr typeof<'a> e

    let optimize (shader : Shader) =
        let newBody = Optimizer.eliminateDeadCode shader.shaderBody
        let used = usedInputs newBody

        { shader with
            shaderInputs = shader.shaderInputs |> Map.filter (fun n _ -> used.Contains n)
            shaderUniforms = shader.shaderUniforms |> Map.filter (fun n _ -> used.Contains n)
            shaderBody = newBody
        }

    let removeOutputs (semantics : Set<string>) (shader : Shader) =
        let rec remove (semantics : Set<string>) (e : Expr) =
            match e with
                | WriteOutput(name, idx, value) ->
                    if Set.contains name semantics then
                        let value = Optimizer.withoutValue value
                        match idx with
                            | Some idx -> 
                                let idx = Optimizer.withoutValue idx
                                Expr.Seq [idx; value]
                            | None ->
                                value
                    else
                        e

                | Sequential(l, r) ->
                    let l = remove semantics l
                    let r = remove semantics r
                    match l, r with
                        | Unit, r -> r
                        | l, Unit -> l
                        | l, r -> Expr.Sequential(l, r)

                | ShapeVar v ->
                    e

                | ShapeLambda(v,b) ->
                    Expr.Lambda(v, remove semantics b)

                | ShapeCombination(o, args) ->
                    RebuildShapeCombination(o, args |> List.map (remove semantics))

        optimize 
            { shader with
                shaderOutputs           = shader.shaderOutputs |> Map.filter (fun n _ -> not (Set.contains n semantics))
                shaderBody              = remove semantics shader.shaderBody
            }

    let addOutputs (outputs : Map<string, Type>) (shader : Shader) =
        match shader.shaderType with
            | ShaderType.Geometry | ShaderType.TessControl | ShaderType.TessEval -> 
                failwithf "[FShade] pass-thru not implemented for %A" shader.shaderType

            | _ ->
                let newOutputs =  outputs |> Map.filter (fun sem _ -> not (Map.containsKey sem shader.shaderOutputs))
                if Map.isEmpty newOutputs then
                    shader
                else
                    let writes = newOutputs |> Map.toList |> List.map (fun (n,t) -> Expr.WriteOutput(n, Expr.ReadInput(t, n)))
                    let newBody = shader.shaderBody |> append writes
                    let added = newOutputs |> Map.map (fun n t -> { paramType = t; paramInterpolation = InterpolationMode.Default })

                    { shader with
                        shaderOutputs           = Map.union shader.shaderOutputs added
                        shaderInputs            = Map.union shader.shaderInputs added
                        shaderBody              = newBody
                    }

    let setOutputs (outputs : Map<string, Type>) (shader : Shader) =
        let toRemove = shader.shaderOutputs |> Map.keys |> Seq.filter (fun n -> not (Map.containsKey n outputs)) |> Set.ofSeq
        let toAdd = outputs |> Map.filter (fun n _ -> not (Map.containsKey n shader.shaderOutputs))

        match Map.isEmpty toAdd, Set.isEmpty toRemove with
            | true, true -> 
                shader

            | true, false -> 
                shader |> removeOutputs toRemove

            | false, true -> 
                shader |> addOutputs toAdd

            | false, false ->
                shader 
                    |> addOutputs toAdd
                    |> removeOutputs toRemove

    let toEntryPoint (prev : Option<ShaderType>) (s : Shader) (next : Option<ShaderType>) =
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

        {
            conditional = s.shaderType |> string |> Some
            entryName   = "main"
            inputs      = inputs
            outputs     = outputs
            uniforms    = uniforms
            arguments   = []
            body        = s.shaderBody
            decorations = 
                List.concat [
                    [ EntryDecoration.Stages { prev = prev; self = s.shaderType; next = next } ]
                    s.shaderInputTopology |> Option.map EntryDecoration.InputTopology |> Option.toList
                    s.shaderOutputTopology |> Option.map EntryDecoration.OutputTopology |> Option.toList
                ]
        }



    let passing (stage : ShaderType) (attributes : Map<string, IOParameter>) =
        match stage with
            | ShaderType.Vertex | ShaderType.Fragment ->
                {
                    shaderType              = stage
                    shaderInputs            = attributes
                    shaderOutputs           = attributes
                    shaderUniforms          = Map.empty
                    shaderInputTopology     = None
                    shaderOutputTopology    = None
                    shaderBody =
                        attributes
                            |> Map.toList
                            |> List.map (fun (n, p) -> Expr.WriteOutput(n, Expr.ReadInput(p.paramType, n)))
                            |> Expr.Seq
            
                }
            | _ ->
                failwith "[FShade] not implemented"
    
    let colorFragment = 
        passing ShaderType.Fragment (Map.ofList [ Intrinsics.Color, { paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default }] )

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

    let ofShaders (shaders : list<Shader>) =
        let addShader (shader : Shader) (e : Effect) =
            let trySet (old : Option<Shader>) (n : Shader) =
                match old with
                    | None -> Some n
                    | Some o -> failwithf "[FShade] duplicate shader for stage %A" o.shaderType

            match shader.shaderType with
                | ShaderType.Vertex         -> { e with vertex = trySet e.vertex shader }
                | ShaderType.TessControl    -> { e with tessControl = trySet e.tessControl shader }
                | ShaderType.TessEval       -> { e with tessEval = trySet e.tessEval shader }
                | ShaderType.Geometry       -> { e with geometry = trySet e.geometry shader }
                | ShaderType.Fragment       -> { e with fragment = trySet e.fragment shader }
                | _                         -> failwithf "[FShade] unknown shader-type %A" shader.shaderType

        shaders |> List.fold (fun e s -> addShader s e) empty

    let inline ofExpr (inputType : Type) (e : Expr) =
        Shader.ofExpr inputType e |> ofShader

    let inline ofFunction (f : 'a -> Expr<'b>) =
        Shader.ofFunction f |> ofShader

    let toModule (e : Effect) =
        let rec compileAll (prev : Option<ShaderType>) (list : list<Shader>) =
            match list with
                | [] -> []
                | [last] -> [ Shader.toEntryPoint prev last None ]
                | current :: next :: rest ->
                    (Shader.toEntryPoint prev current (Some next.shaderType)) ::
                    compileAll (Some current.shaderType) (next :: rest)

        let entries = compileAll None e.shaders

        { entries = entries }

    let map (f : Shader -> Shader) (e : Effect) =
        {
            vertex        = e.vertex        |> Option.map f
            tessControl   = e.tessControl   |> Option.map f
            tessEval      = e.tessEval      |> Option.map f
            geometry      = e.geometry      |> Option.map f
            fragment      = e.fragment      |> Option.map f
        }

    let setOutputs (wanted : Map<string, Type>) (e : Effect) =
        let rec traverse (wanted : Map<string, Type>) (s : list<Shader>) =
            match s with
                | [] -> []

                | s :: rest ->
                    let newShader = Shader.setOutputs wanted s
                    let wanted = newShader.shaderInputs |> Map.map (fun _ p -> p.paramType)
                
                    let newWanted =
                        if s.shaderType = ShaderType.Fragment then 
                            Map.add Intrinsics.Position typeof<V4d> wanted
                        else 
                            wanted

                    newShader :: traverse newWanted rest

        e.shaders 
            |> List.rev
            |> traverse wanted
            |> ofShaders

    let nextShader (t : ShaderType) (e : Effect) =
        e.shaders 
            |> List.tryFind (fun s -> s.shaderType > t)

    let prevShader (t : ShaderType) (e : Effect) =
        e.shaders 
            |> List.rev
            |> List.tryFind (fun s -> s.shaderType < t)

    let link (effect : Effect) =
        let next = effect |> nextShader ShaderType.Vertex
        match next with
            | Some next -> 
                match effect.vertex with
                    | None -> { effect with vertex = Some (Shader.passing ShaderType.Vertex next.shaderInputs) }
                    | _ -> effect
            | None ->
                effect