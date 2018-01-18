namespace FShade

open System
open System.Runtime.CompilerServices

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative


/// Effect encapsulates a set of shaders for the various ShaderStages defined by FShade.
type Effect internal(id : string, shaders : Map<ShaderStage, Shader>, composedOf : list<Effect>) =
    let mutable sourceDefintion : Option<Expr * Type> = None

    let inputToplogy =
        lazy (
            shaders |> Map.toSeq |> Seq.tryPick (fun (_,s) ->
                s.shaderInputTopology
            )
        )

    let first =
        lazy (
            shaders |> Map.toSeq |> Seq.map snd |> Seq.tryHead
        )

    let last =
        lazy (
            shaders |> Map.toSeq |> Seq.map snd |> Seq.tryLast
        )

    let inputs =
        lazy (
            match first.Value with
                | Some shader -> Shader.neededInputs shader
                | None -> Map.empty
        )

    let outputs =
        lazy (
            match last.Value with
                | Some shader -> Shader.outputs shader
                | None -> Map.empty
        )

    let uniforms =
        lazy (
            shaders 
                |> Map.toSeq 
                |> Seq.map (fun (_,s) -> Shader.uniforms s) 
                |> Seq.fold Map.union Map.empty
        )
        
    let lastPrimitive =
        lazy (
            shaders |> Map.values |> Seq.filter (fun s -> s.shaderStage < ShaderStage.Fragment) |> Seq.tryLast
        )

    static member internal NewId() =
        Guid.NewGuid().ToByteArray() |> Convert.ToBase64String

    member x.SourceDefintion
        with get() = sourceDefintion
        and internal set d = sourceDefintion <- d
      
    member x.IsEmpty            = id = ""

    /// gets a unique id for this effect
    member x.Id                 = id

    /// returns the effect of which the current one was composed (empty if leaf)
    member x.ComposedOf = composedOf

    /// gets the optionally required InputTopology for the effect.
    /// returns None when the effect is agnostic
    member x.InputToplogy       = inputToplogy.Value
    /// gets the optional first Shader for the effect (in execution-order).
    member x.FirstShader        = first.Value
    /// gets the optional last Shader for the effect (in execution-order).
    member x.LastShader         = last.Value
    /// gets the optional last Shader operating on primitives for the effect (in execution-order).
    member x.LastPrimShader     = lastPrimitive.Value 
    /// gets the required inputs for the first Shader in the effect (in execution-order).
    member x.Inputs             = inputs.Value
    /// gets the provided outputs for the last Shader in the effect (in execution-order).
    member x.Outputs            = outputs.Value
    /// gets all required uniforms for the effect (from all shaders).
    member x.Uniforms           = uniforms.Value
    /// gets a Map<ShaderStage, Shader> for the effect containing all Shaders.
    member x.Shaders            = shaders
    /// gets the optional VertexShader for the effect.
    member x.VertexShader       = shaders |> Map.tryFind ShaderStage.Vertex
    /// gets the optional TessControlShader for the effect.
    member x.TessControlShader  = shaders |> Map.tryFind ShaderStage.TessControl
    /// gets the optional TessEvalShader for the effect.
    member x.TessEvalShader     = shaders |> Map.tryFind ShaderStage.TessEval
    /// gets the optional GeometryShader for the effect.
    member x.GeometryShader     = shaders |> Map.tryFind ShaderStage.Geometry
    /// gets the optional FragmentShader for the effect.
    member x.FragmentShader     = shaders |> Map.tryFind ShaderStage.Fragment

    new(m, o) = Effect(Effect.NewId(), m, o)
    new(m) = Effect(Effect.NewId(), m, [])


type EffectConfig =
    {
        depthRange          : Range1d
        flipHandedness      : bool
        lastStage           : ShaderStage
        outputs             : Map<string, Type * int>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EffectConfig =
    let empty =
        {
            depthRange      = Range1d(-1.0, 1.0)
            flipHandedness  = false
            lastStage       = ShaderStage.Fragment
            outputs         = Map.empty
        }
      
    let ofMap (m : Map<string, Type * int>) =
        { empty with outputs = m }

    let ofSeq (m : seq<string * Type * int>) =
        m |> Seq.map (fun (n,t,i) -> n,(t,i)) |> Map.ofSeq |> ofMap

    let ofList (m : list<string * Type * int>) =
        ofSeq m

    let ofArray (m : array<string * Type * int>) =
        ofSeq m

/// the Effect module provides functions for accessing, creating and modifying effects.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Effect =
    let private effectCache = System.Collections.Concurrent.ConcurrentDictionary<string, Effect>()
    let private composeCache = System.Collections.Concurrent.ConcurrentDictionary<string, Effect>()

    [<CompilerMessage("clearCaches is considered harmful", 4321, IsError=false, IsHidden=true)>]
    let clearCaches() =
        effectCache.Clear()
        composeCache.Clear()

    let empty = Effect("", Map.empty, [])
    
    /// gets a unique id for this effect
    let inline id (effect : Effect) = effect.Id

    /// gets the optional VertexShader for the effect.
    let inline vertexShader (effect : Effect) = effect.VertexShader

    /// gets the optional TessControlShader for the effect.
    let inline tessControlShader (effect : Effect) = effect.TessControlShader

    /// gets the optional TessEvalShader for the effect.
    let inline tessEvalShader (effect : Effect) = effect.TessEvalShader

    /// gets the optional GeometryShader for the effect.
    let inline geometryShader (effect : Effect) = effect.GeometryShader

    /// gets the optional FragmentShader for the effect.
    let inline fragmentShader (effect : Effect) = effect.FragmentShader
    
    /// gets the optionally required InputTopology for the effect.
    /// returns None when the effect is agnostic
    let inline inputTopology (effect : Effect) = effect.InputToplogy

    /// gets the optional first Shader for the effect (in execution-order).
    let inline firstShader (effect : Effect) = effect.FirstShader
    
    /// gets the optional last Shader for the effect (in execution-order).
    let inline lastShader (effect : Effect) = effect.LastShader

    /// gets the optional last Shader operating on primitives for the effect (in execution-order).
    let inline lastPrimShader (effect : Effect) = effect.LastPrimShader

    /// gets the optional first ShaderStage for the effect (in execution-order).
    let inline firstStage (effect : Effect) = effect.FirstShader |> Option.map Shader.stage

    /// gets the optional last ShaderStage for the effect (in execution-order).
    let inline lastStage (effect : Effect) = effect.LastShader |> Option.map Shader.stage

    /// gets the required inputs for the first Shader in the effect (in execution-order).
    let inline inputs (effect : Effect) = effect.Inputs

    /// gets the provided outputs for the last Shader in the effect (in execution-order).
    let inline outputs (effect : Effect) = effect.Outputs
    
    /// gets all required uniforms for the effect (from all shaders).
    let inline uniforms (effect : Effect) = effect.Uniforms

    /// creates an effect from a Map<ShaderStage,Shader>.
    let ofMap (shaders : Map<ShaderStage, Shader>) =
        for (stage, shader) in Map.toSeq shaders do
            if stage <> shader.shaderStage then 
                failwithf "[FShade] inconsistent shader-map: %A claims to be %A" shader.shaderStage stage

        Effect shaders
        
    /// creates an effect from a sequence of shaders
    let ofSeq (shaders : seq<Shader>) =
        let mutable map = Map.empty
        for shader in shaders do
            match Map.tryFind shader.shaderStage map with
                | Some prev -> 
                    failwithf "[FShade] conflicting shaders for stage: %A" shader.shaderStage
                | None ->
                    map <- Map.add shader.shaderStage shader map

        Effect map
           

    /// creates an effect from a list of shaders
    let inline ofList (shaders : list<Shader>) = 
        ofSeq shaders
        
    /// creates an effect from an array of shaders
    let inline ofArray (shaders : Shader[]) = 
        ofSeq shaders
        
    /// creates an effect from a single shaders
    let ofShader (shader : Shader) =
        Effect (Map.ofList [shader.shaderStage, shader])
        
    /// creates an effect from an expression (assuming expressions as returned by shader-functions)
    let ofExpr (inputType : Type) (e : Expr) =
        let effect = Shader.ofExpr inputType e |> ofList
        effect.SourceDefintion <- Some (e, inputType)
        effect

    /// creates an effect from a shader-function
    let ofFunction (shaderFunction : 'a -> Expr<'b>) =
        let expression = 
            try shaderFunction Unchecked.defaultof<'a>
            with 
                | :? System.NullReferenceException as n -> 
                    failwithf "[FShade] shader functions may not access their vertex-input statically (inner cause - NullReferenceException: %A)" n.StackTrace
                | e -> 
                    failwithf "[FShade] failed to execute shader function.\nInner cause: %A at\n%A" e e.StackTrace
            
        let hash = Expr.ComputeHash expression
        effectCache.GetOrAdd(hash, fun _ ->
            let range = expression.DebugRange
            let effect = Shader.ofExpr typeof<'a> expression |> ofList
            effect.SourceDefintion <- Some (expression.Raw, typeof<'a>)
            effect
        )

    /// gets a Map<ShaderStage, Shader> for the effect containing all Shaders.
    let inline toMap (effect : Effect) = effect.Shaders

    /// gets a sequence of Shaders for the effect
    let toSeq (effect : Effect) =
        effect.Shaders |> Map.toSeq |> Seq.map snd
        
    /// gets a list of Shaders for the effect
    let toList (effect : Effect) =
        effect.Shaders |> Map.toList |> List.map snd
        
    /// gets an array of Shaders for the effect
    let toArray (effect : Effect) =
        effect.Shaders |> Map.toSeq |> Seq.map snd |> Seq.toArray
        
    /// determines whether the effect is empty (has no Shaders).
    let isEmpty (effect : Effect) = 
        Map.isEmpty effect.Shaders
            
    /// gets the optional Shader for the given stage from the effect.
    let tryFindShader (stage : ShaderStage) (effect : Effect) =
        Map.tryFind stage effect.Shaders
        
    /// determines whether the effect includes the given ShaderStage.s
    let hasStage (stage : ShaderStage) (effect : Effect) =
        Map.containsKey stage effect.Shaders

    /// creates a new effect with the given shader addded (replacing the current if it exists).
    let add (shader : Shader) (effect : Effect) =
        Effect (Map.add shader.shaderStage shader effect.Shaders)
        
    /// creates a new effect with the given ShaderStage removed.
    let remove (stage : ShaderStage) (effect : Effect) =
        Effect (Map.remove stage effect.Shaders)
        
    /// returns all DebugRanges used by the effect's shaders
    let rec debugRanges (effect : Effect) =
        match effect.ComposedOf with
            | [] -> 
                effect.Shaders 
                    |> Map.values 
                    |> Seq.choose (fun shader -> shader.shaderDebugRange)
                    |> Seq.distinct
                    |> Seq.toList
            | effects ->
                effects |> List.collect debugRanges

    /// creates a new effect by applying the given update-function to
    /// the respective (optional) shader for the given stage.
    /// When update returns Some it will be added/replaced and None will cause the shader to get removed.
    let alter (stage : ShaderStage) (update : Option<Shader> -> Option<Shader>) (effect : Effect) =
        match update (tryFindShader stage effect) with
            | Some n -> 
                if stage <> n.shaderStage then
                    failwithf "[FShade] cannot change shader-stage in Effect.alter from %A to %A" stage n.shaderStage

                add n effect

            | None -> 
                remove stage effect

    /// creates a new effect by adding a shader for the given stage (if none was present).
    let addIfNotPresent (stage : ShaderStage) (create : ShaderStage -> Shader) (effect : Effect) =
        alter stage (function Some o -> Some o | None -> Some (create stage)) effect

    /// creates a new effect which will contain exactly the outputs given by <outputs> at the specified
    /// stage.
    let private link (stage : ShaderStage) (outputs : Map<string, Type>) (effect : Effect) =
        let rec linkShaders (needed : Map<string, Type>) (l : list<Shader>) =
            match l with
                | [] -> 
                    []

                | current :: before ->
                    
                    let desired =
                        needed |> Map.union (Shader.systemOutputs current)

                    let newCurrent = 
                        Shader.withOutputs desired current

                    let newBefore = 
                        linkShaders (Shader.neededInputs newCurrent) before

                    newCurrent :: newBefore
                             

        effect 
            // add the final desired stage passing all desired
            // outputs (if not yet present)
            |> addIfNotPresent stage (Shader.passing outputs)

            // add an empty vertex shader when none is present
            |> addIfNotPresent ShaderStage.Vertex (Shader.passing Map.empty)

            // link all shaders (backward)
            |> toList
            |> List.rev
            |> linkShaders outputs        
                
            // and create the new effect                              
            |> ofList
          
    let map (f : Shader -> Shader) (effect : Effect) =
        effect.Shaders
            |> Map.map (fun _ -> f)
            |> ofMap

    let private withDepthRange (flipHandedness : bool) (range : Range1d) (effect : Effect) =
        if flipHandedness || range.Min <> -1.0 || range.Max <> 1.0 then
            let convertValue (v : ShaderOutputValue) =
                let ie = v.Value

                let newZ =
                    if range.Min <> -1.0 || range.Max <> 1.0 then
                        //   newZ = a * z + b * w
                        // due to projective division:
                        //   max = (a * w + b * w) / w
                        //   min = (a * -w + b * w) / w
                        // therefore:
                        //   (1) max = a + b
                        //   (2) min = b - a
                        //   (1) + (2) => max + min = 2 * b
                        //   (1) - (2) => max - min = 2 * a
                        // so finally we get:
                        //   a = (max - min) / 2
                        //   b = (max + min) / 2
                        let a = range.Size / 2.0
                        let b = (range.Min + range.Max) / 2.0
                        if a = b then <@@ a * ((%%ie : V4d).Z + (%%ie : V4d).W) @@>
                        else <@@ a * (%%ie : V4d).Z + b * (%%ie : V4d).W @@>
                    else  
                        <@@ (%%ie : V4d).Z @@>

                if flipHandedness then
                    v |> ShaderOutputValue.withValue <@@ V4d((%%ie : V4d).X, -(%%ie : V4d).Y, %%newZ, (%%ie : V4d).W)  @@>
                else
                    v |> ShaderOutputValue.withValue <@@ V4d((%%ie : V4d).X, (%%ie : V4d).Y, %%newZ, (%%ie : V4d).W)  @@>

            match lastPrimShader effect with
                | Some shader ->
                    let newLastPrim = 
                        shader |> Shader.substituteWrites (fun values ->
                            match Map.tryFind Intrinsics.Position values with
                                | Some pos ->
                                    match pos.Value with
                                        | Trivial ->
                                            Expr.WriteOutputs(Map.add Intrinsics.Position (convertValue pos) values)
                                                |> Some
                                        | _ -> 
                                            let v = Var("_pos", pos.Type)
                                            let newValue = pos |> ShaderOutputValue.withValue (Expr.Var v)
                                            Expr.Let(
                                                v, pos.Value,
                                                Expr.WriteOutputs(Map.add Intrinsics.Position (convertValue newValue) values)
                                               )
                                            |> Some

                                | _ ->
                                    None
                        )

                    effect |> add newLastPrim

                | None -> 
                    failwith "[FShade] cannot adjust depth-range for effect without primitive shaders"
        else
            effect

    /// creates a Module for the given effect which can be used for compilation.              
    let toModule (config : EffectConfig) (effect : Effect) =
        
        let effect =
            effect
                |> link config.lastStage (Map.map (fun _ -> fst) config.outputs)
                |> withDepthRange config.flipHandedness config.depthRange
                

        let rec entryPoints (last : Option<Shader>) (lastStage : Option<Shader>) (shaders : list<Shader>) =
            match shaders with
                | [] -> 
                    []

                | [shader] -> 
                    
                    let inputs =
                        match last with
                            | Some last -> 
                                shader.shaderInputs |> Map.map (fun sem i ->
                                    if i.paramInterpolation = InterpolationMode.Default then
                                        match Map.tryFind sem last.shaderOutputs with
                                            | Some { paramInterpolation = outMode } ->
                                                { i with paramInterpolation = outMode }
                                            | _ ->
                                                i
                                    else
                                        i
                                )
                            | None ->
                                shader.shaderInputs

                    let entry = Shader.toEntryPoint lastStage { shader with shaderInputs = inputs } None

                    let mutable free = Set.ofList (entry.outputs |> List.mapi (fun i _ -> i))

                    let outputs = 
                        entry.outputs
                            |> List.map (fun o -> 
                                let slot = 
                                    match Map.tryFind o.paramName config.outputs with
                                        | Some(_,slot) -> slot
                                        | None -> Seq.head free
                                let o = { o with paramDecorations = Set.add (ParameterDecoration.Slot slot) o.paramDecorations }
                                free <- Set.remove slot free
                                (slot, o)
                            )
                            |> List.sortBy fst
                            |> List.map snd
                            
                    [ { entry with outputs = outputs } ]

                | shader :: next :: after ->

                    let outputs =
                        shader.shaderOutputs |> Map.map (fun sem o ->
                            if o.paramInterpolation = InterpolationMode.Default then
                                match Map.tryFind sem next.shaderInputs with
                                    | Some { paramInterpolation = inMode } ->
                                        { o with paramInterpolation = inMode }
                                    | _ ->
                                        o
                            else
                                o
                        )
                    let shader = { shader with shaderOutputs = outputs }

                    let shaderEntry = Shader.toEntryPoint lastStage shader (Some next) 
                    shaderEntry :: entryPoints (Some shader) (Some shader) (next :: after)

        let shaders = toList effect

        {
            hash = effect.Id
            userData = effect
            entries = entryPoints None None shaders
            tryGetOverrideCode = Shader.tryGetOverrideCode V3i.Zero 
        }

    let inputsToUniforms (scopes : Map<string, UniformScope>) (effect : Effect) =
        effect |> map (Shader.inputsToUniforms scopes)

    let uniformsToInputs (semantics : Set<string>) (effect : Effect) =
        effect |> map (Shader.uniformsToInputs semantics)

    /// composes two effects using sequential semantics for 'abstract' stages.
    /// these 'abstract' stages consist of the following 'real' stages:
    ///     - Primitive: [Vertex; TessControl; TessEval; Geometry]
    ///     - Fragment:  [Fragment]
    /// effects are composed sequentially per 'abstract' stage. 
    /// e.g. r's VertexShader will be appended to l's GeometryShader (if present)
    let compose2 (l : Effect) (r : Effect) =
        if l.IsEmpty then r
        elif r.IsEmpty then l
        else
            let resultId = l.Id + r.Id
            composeCache.GetOrAdd(resultId, fun resultId ->
                let geometryLeft = l |> toList |> List.filter (fun s -> s.shaderStage < ShaderStage.Fragment)
                let geometryRight = r |> toList |> List.filter (fun s -> s.shaderStage < ShaderStage.Fragment)

                let rec composeToLast (l : list<Shader>) (r : list<Shader>) =
                    match l with
                        | [] -> r
                        | [l] ->
                            match r with
                                | [] -> [l]
                                | rh :: _ ->
                                    if l.shaderStage < rh.shaderStage then
                                        l :: r
                                    else
                                        let mutable res = l
                                        for r in r do res <- Shader.compose2 res r
                                        [ res ]
                        | h :: rest ->
                            h :: composeToLast rest r

                let shaders = 
                    composeToLast geometryLeft geometryRight

                let shaders = 
                    match l.FragmentShader, r.FragmentShader with
                        | Some l, Some r -> (Shader.compose2 l r) :: shaders
                        | None, Some r -> r :: shaders
                        | Some l, None -> l :: shaders
                        | None, None -> shaders
        
                let shaderMap =                 
                    shaders |> Seq.map (fun s -> s.shaderStage, s) |> Map.ofSeq

                Effect(resultId, shaderMap, [l;r])
            )

    /// composes many effects using the sequential semantics defined in compose2.
    let compose (effects : #seq<Effect>) =
        effects |> Seq.fold compose2 empty


    module private Helpers =
        type Vertex = { [<SourceVertexIndex>] i : int }

        let nopPoint (p : Point<Vertex>) =
            point {
                yield p.Value
            }

        let nopLine (p : Line<Vertex>) =
            line {
                for i in 0 .. 1 do
                    yield p.[i]
            }

        let nopTriangle (p : Triangle<Vertex>) =
            triangle {
                for i in 0 .. 2 do
                    yield p.[i]
            }

        let nopGS =
            Map.ofList [
                InputTopology.Point, ofFunction(nopPoint).GeometryShader.Value
                InputTopology.Line, ofFunction(nopLine).GeometryShader.Value
                InputTopology.Triangle, ofFunction(nopTriangle).GeometryShader.Value
            ]

    let substituteUniforms (substitute : string -> Type -> Option<Expr> -> Option<Expr>) (effect : Effect) =
        effect |> map (fun shader ->
            shader |> Shader.substituteReads (fun kind typ name index ->
                match kind with
                    | ParameterKind.Uniform ->
                        substitute name typ index
                    | _ ->
                        None
            )
        )

    module private LayerHelpers = 

        let withLayeredUniforms (uniforms : Map<string, string>) (layerCount : int) (layer : Expr) (shader : Shader) =
            if Map.isEmpty uniforms then
                shader
            else
                let mutable perLayerUniforms = Map.empty
                let mutable removed = Map.empty

                let newBody = 
                    shader.shaderBody.SubstituteReads(fun kind typ oldName index ->
                        match kind with
                            | ParameterKind.Uniform ->
                                match Map.tryFind oldName uniforms with
                                    | Some newName ->
                                        match index with
                                            | Some index ->
                                                let old = shader.shaderUniforms.[oldName]
                                                let typ = Peano.getArrayType layerCount old.uniformType
                                                perLayerUniforms <- Map.add newName { old with uniformName = newName; uniformType = typ } perLayerUniforms
                                                removed <- Map.add oldName () removed
                                                let arrInput = Expr.ReadInput(kind, typ, newName)
                                                let layerItem = Expr.ArrayAccess(arrInput, layer) //Expr.PropertyGet(arrInput, arrInput.Type.GetProperty("Item"), [layer])
                                                let realItem = Expr.ArrayAccess(arrInput, index) //Expr.PropertyGet(layerItem, layerItem.Type.GetProperty("Item"), [index])

                                                realItem |> Some
                                    
                                            | None ->
                                                let old = shader.shaderUniforms.[oldName]
                                                let typ = Peano.getArrayType layerCount old.uniformType
                                                perLayerUniforms <- Map.add newName { old with uniformName = newName; uniformType = typ } perLayerUniforms
                                                removed <- Map.add oldName () removed
                                                let arrInput = Expr.ReadInput(kind, typ, newName) 
                                                let realItem = Expr.ArrayAccess(arrInput, layer) //Expr.PropertyGet(arrInput, arrInput.Type.GetProperty("Item"), [layer])
                                                realItem |> Some
                                    | _ ->
                                        None
                            | _ ->
                                None
                    )


                let uniforms = Map.union (Map.difference shader.shaderUniforms removed) perLayerUniforms
                    

                Shader.withBody newBody { shader with shaderUniforms = uniforms }



    let toLayeredEffect' (layerSemantic : string) (layers : int) (uniforms : Map<string, string>) (topology : InputTopology) (effect : Effect) = 
        if effect.TessControlShader.IsSome || effect.TessEvalShader.IsSome then
            failwithf "[FShade] effects containing tessellation shaders cannot be layered automatically"

        let geometryShader =
            match effect.VertexShader, effect.GeometryShader with
                | None, None ->
                    match Map.tryFind topology Helpers.nopGS with
                        | Some gs ->
                            gs
                        | None ->
                            failwithf "[FShade] bad topology for layered shader: %A" topology

                | Some vs, None ->
                    match Map.tryFind topology Helpers.nopGS with
                        | Some gs ->
                            Shader.compose2 gs vs
                        | None ->
                            failwithf "[FShade] bad topology for layered shader: %A" topology
                    
                | None, Some gs ->
                    gs

                | Some vs, Some gs ->
                    Shader.Composition.vsgs vs gs
//                    match Map.tryFind gs.shaderInputTopology.Value Helpers.nopGS with
//                        | Some nop ->
//                            let vs = Shader.compose2 nop vs
//                            Shader.compose2 vs gs
//                        | None ->
//                            failwithf "[FShade] bad topology for layered shader: %A" gs.shaderInputTopology.Value

        let geometryShader =
            let layer = Expr.ReadInput(ParameterKind.Input, typeof<int>, Intrinsics.InvocationId)
            let gs = LayerHelpers.withLayeredUniforms uniforms layers layer geometryShader

            let newBody =
                gs.shaderBody.SubstituteWrites (fun outputs ->
                    let o = Map.add layerSemantic (None, layer) outputs
                    Expr.WriteOutputs o |> Some
                )

            { gs with shaderInvocations = layers } |> Shader.withBody newBody

        let fragmentShader =
            match effect.FragmentShader with
                | Some fragmentShader ->
                    let layer = Expr.ReadInput(ParameterKind.Input, typeof<int>, layerSemantic)
                    fragmentShader
                        |> LayerHelpers.withLayeredUniforms uniforms layers layer 
                        |> Some
                | None ->
                    None

        match fragmentShader with
            | Some fs ->
                Effect(Map.ofList [ShaderStage.Geometry, geometryShader; ShaderStage.Fragment, fs])
            | None ->
                ofList [ geometryShader ]

    let toLayeredEffect (layers : int) (uniforms : Map<string, string>) (topology : InputTopology) (effect : Effect) = 
        toLayeredEffect' Intrinsics.Layer layers uniforms topology effect

    let toMultiViewportEffect (viewports : int) (uniforms : Map<string, string>) (topology : InputTopology) (effect : Effect) = 
        toLayeredEffect' Intrinsics.ViewportIndex viewports uniforms topology effect