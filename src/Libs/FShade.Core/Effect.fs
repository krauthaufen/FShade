namespace FShade

open System
open System.Reflection
open System.Runtime.CompilerServices

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative

#nowarn "1337"

[<AttributeUsage(AttributeTargets.Method)>]
type WithEffectAttribute() =
    inherit System.Attribute()


/// Effect encapsulates a set of shaders for the various ShaderStages defined by FShade.
type Effect(id : string, shaders : Lazy<Map<ShaderStage, Shader>>, composedOf : list<Effect>) =
    let mutable sourceDefinition : Option<Expr * List<obj>> = None

    let inputToplogy =
        lazy (
            shaders.Value |> Map.toSeq |> Seq.tryPick (fun (_,s) ->
                s.shaderInputTopology
            )
        )

    let first =
        lazy (
            shaders.Value |> Map.toSeq |> Seq.map snd |> Seq.tryHead
        )

    let last =
        lazy (
            shaders.Value |> Map.toSeq |> Seq.map snd |> Seq.tryLast
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
            shaders.Value
                |> Map.toSeq 
                |> Seq.map (fun (_,s) -> Shader.uniforms s) 
                |> Seq.fold Map.union Map.empty
        )
        
    let lastPrimitive =
        lazy (
            shaders.Value |> Map.values |> Seq.filter (fun s -> s.shaderStage < ShaderStage.Fragment) |> Seq.tryLast
        )

    static member internal NewId() =
        Guid.NewGuid().ToByteArray() |> Convert.ToBase64String

    member x.SourceDefinition
        with get() = sourceDefinition
        and internal set d = sourceDefinition <- d
      
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
    member x.Shaders            = shaders.Value
    /// gets the optional VertexShader for the effect.
    member x.VertexShader       = shaders.Value |> Map.tryFind ShaderStage.Vertex
    /// gets the optional TessControlShader for the effect.
    member x.TessControlShader  = shaders.Value |> Map.tryFind ShaderStage.TessControl
    /// gets the optional TessEvalShader for the effect.
    member x.TessEvalShader     = shaders.Value |> Map.tryFind ShaderStage.TessEval
    /// gets the optional GeometryShader for the effect.
    member x.GeometryShader     = shaders.Value |> Map.tryFind ShaderStage.Geometry
    /// gets the optional FragmentShader for the effect.
    member x.FragmentShader     = shaders.Value |> Map.tryFind ShaderStage.Fragment

    new(m, o) = Effect(Effect.NewId(), m, o)
    new(m) = Effect(Effect.NewId(), m, [])


type EffectConfig =
    {
        /// Target depth range for optional depth range mapping.
        /// The final depth will be mapped from [-1, 1] to the target range.
        /// If the target range is [-1, 1] no mapping will be performed.
        depthRange          : Range1d

        /// Indicates whether the y-component of vertex clip cooridnates is to be inverted.
        flipHandedness      : bool

        /// Stage of the last shader in the effect.
        lastStage           : ShaderStage

        /// Type and slot for each output.
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

//type IPersistentStorage =
//    abstract IsNone : bool
//    abstract TryRead : file : string -> option<byte[]>
//    abstract Write : file : string * data : byte[] -> unit
//    abstract Delete : file : string -> bool
//    abstract Exists : file : string -> bool
//    abstract List : unit -> seq<string>

//module PersistentStorage =
//    open System
//    open System.IO

//    let none =
//        { new IPersistentStorage with
//            member x.IsNone = true
//            member x.TryRead _ = None
//            member x.Write(_,_) = ()
//            member x.Delete _ = false
//            member x.Exists _ = false
//            member x.List() = Seq.empty
//        }

//    let directory (dir : string) =
//        let dir = Path.GetFullPath dir
//        if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore

//        let relativePath (file : string) =
//            let name =
//                let bytes = System.Text.Encoding.UTF8.GetBytes(file)
//                let builder = System.Text.StringBuilder()
//                for b in bytes do builder.AppendFormat("{0:X2}", b) |> ignore
//                builder.ToString()

//            Path.Combine(dir, name)

//        { new IPersistentStorage with
//            member x.IsNone = false
//            member x.TryRead(file) =
//                try
//                    let p = relativePath file
//                    if File.Exists p then File.ReadAllBytes p |> Some
//                    else None
//                with _ ->
//                    None
//            member x.Write(file, data) =
//                try
//                    let p = relativePath file
//                    File.WriteAllBytes(p, data)
//                with _ ->
//                    ()

//            member x.Exists(file) =
//                try
//                    let p = relativePath file
//                    File.Exists p
//                with _ ->
//                    false
                
//            member x.Delete(file) =
//                try
//                    let p = relativePath file
//                    if File.Exists p then
//                        File.Delete p
//                        true
//                    else
//                        false
//                with _ ->
//                    false

//            member x.List() =
//                let rec run (dir : string) =
                    
//                    let files =
//                        try Directory.EnumerateFiles dir
//                        with _ -> Seq.empty

//                    let dirs = 
//                        try Directory.EnumerateDirectories dir
//                        with _ -> Seq.empty

//                    Seq.append
//                        files
//                        (dirs |> Seq.collect run)

//                let makeRelative (path : string) =
//                    let path = Path.GetFullPath path
//                    if path.StartsWith dir then 
//                        if path.Length > dir.Length && path.[dir.Length] = Path.DirectorySeparatorChar then path.Substring(dir.Length + 1)
//                        else path.Substring(dir.Length)
//                    else
//                        path

//                run dir |> Seq.map makeRelative
//        }

//    let mutable current =
//        try
//            let path = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "FShade")
//            directory path
//        with _ ->
//            none



/// the Effect module provides functions for accessing, creating and modifying effects.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Effect =
    let private effectCache = System.Collections.Concurrent.ConcurrentDictionary<string, Effect>()
    let private composeCache = System.Collections.Concurrent.ConcurrentDictionary<string, Effect>()



    [<CompilerMessage("clearCaches is considered harmful", 4321, IsError=false, IsHidden=true)>]
    let clearCaches() =
        effectCache.Clear()
        composeCache.Clear()



    open System.IO

    let internal serializeInternal (state : Shader.SerializerState) (dst : BinaryWriter) (effect : Effect) =
        dst.Write effect.Id
        let shaders = effect.Shaders
        dst.Write (Map.count shaders)
        for KeyValue(stage, shader) in shaders do
            dst.Write (int stage)
            Shader.serializeInternal state dst shader
        
    let internal deserializeInternal (state : Shader.DeserializerState) (src : BinaryReader) =
        let id = src.ReadString()
        let shaders =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->
                let stage = src.ReadInt32() |> unbox<ShaderStage>
                let shader = Shader.deserializeInternal state src
                stage, shader
            )
            |> Map.ofList

        Effect(id, lazy shaders, [])


    let serialize (dst : Stream) (e : Effect) =
        use w = new BinaryWriter(dst, System.Text.Encoding.UTF8, true)
        serializeInternal (Shader.SerializerState()) w e
        
    let deserialize (src : Stream) =
        use r = new BinaryReader(src, System.Text.Encoding.UTF8, true)
        deserializeInternal (Shader.DeserializerState()) r
         
    let read (src : byte[]) =
        let id = 
            use ms = new MemoryStream(src)
            use r = new BinaryReader(ms, System.Text.Encoding.UTF8, true)
            r.ReadString()

        let shaders =
            lazy (
                try
                    use ms = new MemoryStream(src)
                    use src = new BinaryReader(ms, System.Text.Encoding.UTF8, true)
                    let state = Shader.DeserializerState()
                    let _id = src.ReadString()
                    let cnt = src.ReadInt32()
                    List.init cnt (fun _ ->
                        let stage = src.ReadInt32() |> unbox<ShaderStage>
                        let shader = Shader.deserializeInternal state src
                        stage, shader
                    )
                    |> Map.ofList
                with _ ->
                    let b64 = System.Convert.ToBase64String src
                    failwithf "Failed to read effect base64: \"%s\"" b64
            )

        Effect(id, shaders, [])

    let unpickleWithId (id : string) (data : string) =
        let shaders =
            lazy (
                try
                    let binary = System.Convert.FromBase64String data
                    use ms = new MemoryStream(binary)
                    use src = new BinaryReader(ms, System.Text.Encoding.UTF8, true)
                    let state = Shader.DeserializerState()
                    let _id = src.ReadString()
                    let cnt = src.ReadInt32()
                    List.init cnt (fun _ ->
                        let stage = src.ReadInt32() |> unbox<ShaderStage>
                        let shader = Shader.deserializeInternal state src
                        stage, shader
                    )
                    |> Map.ofList
                with e ->
                    failwithf "Failed to read effect base64: \"%s\"" data
            )

        Effect(id, shaders, [])

    let pickle (e : Effect) =
        use ms = new MemoryStream()
        serialize ms e
        ms.ToArray()

    let tryDeserialize (src : Stream) =
        try Some (deserialize src)
        with _ -> None


    let empty = Effect("", Lazy<Map<ShaderStage, Shader>>.CreateFromValue(Map.empty), [])
    
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
        Serializer.Init()
        for (stage, shader) in Map.toSeq shaders do
            if stage <> shader.shaderStage then 
                failwithf "[FShade] inconsistent shader-map: %A claims to be %A" shader.shaderStage stage

        Effect (Lazy<Map<ShaderStage, Shader>>.CreateFromValue(shaders))
        
    /// creates an effect from a sequence of shaders
    let ofSeq (shaders : seq<Shader>) =
        Serializer.Init()
        let map =
            lazy (
                let mutable map = Map.empty
                for shader in shaders do
                    match Map.tryFind shader.shaderStage map with
                        | Some prev -> 
                            failwithf "[FShade] conflicting shaders for stage: %A" shader.shaderStage
                        | None ->
                            map <- Map.add shader.shaderStage shader map
                map
            )
        Effect map
           

    /// creates an effect from a list of shaders
    let inline ofList (shaders : list<Shader>) = 
        ofSeq shaders
        
    /// creates an effect from an array of shaders
    let inline ofArray (shaders : Shader[]) = 
        ofSeq shaders
        
    /// creates an effect from a single shaders
    let ofShader (shader : Shader) =
        Serializer.Init()
        Effect (Lazy<Map<ShaderStage, Shader>>.CreateFromValue(Map.ofList [shader.shaderStage, shader]))
        
    
    [<CompilerMessage("internal use", 1337, IsHidden = true)>]
    let mutable UsePrecompiled = true
    

    type private EffectKey =
        {
            inputSemantics : Map<string, InterpolationMode>
            outputSemantics : Map<string, InterpolationMode * DepthWriteMode>
            body : Expr
        }

    module private EffectKey =
        open System.IO
        open System.Security.Cryptography

        let computeHash (e : EffectKey) =  
            use hash = System.Security.Cryptography.SHA1.Create()
            use ms = new MemoryStream()
            use h = new CryptoStream(ms, hash, CryptoStreamMode.Write)
            use w = new BinaryWriter(h, System.Text.Encoding.UTF8, true)
            Expr.serializeInternal (Expr.SerializerState(true)) w e.body
            w.Write (Map.count e.inputSemantics)
            for KeyValue(name, mode) in e.inputSemantics do
                w.Write name; w.Write (int mode)
            w.Write (Map.count e.outputSemantics)
            for KeyValue(name, (a, b)) in e.outputSemantics do
                w.Write name; w.Write (int a); w.Write (int b)
            h.FlushFinalBlock()
            hash.Hash |> Convert.ToBase64String

    let rec private getRecordFields (t : Type) =
        if FSharpType.IsRecord(t, true) then 
            FSharpType.GetRecordFields(t, true)
        elif t.IsGenericType then
            match t.GetGenericArguments() with
                | [| t |] -> getRecordFields t
                | _ -> [||]
        else 
            [||]

    /// creates an effect from an expression (assuming expressions as returned by shader-functions)
    let ofExpr (inputType : Type) (e : Expr) =
        match e with
        | Patterns.WithValue(:? Effect as effect, _, _) when UsePrecompiled ->
            effect.SourceDefinition <- Some (e, [])
            effect
        | _ ->
            let e =
                match e with
                | Patterns.WithValue(_,_,e) -> e
                | e -> e

            
            let key = 
                let inputFields = getRecordFields inputType
                let outputFields = getRecordFields e.Type
            
                {
                    inputSemantics = inputFields |> Seq.map (fun f -> f.Semantic, f.Interpolation) |> Map.ofSeq
                    outputSemantics = outputFields |> Seq.map (fun f -> f.Semantic, (f.Interpolation, f.DepthWriteMode)) |> Map.ofSeq
                    body = e.WithAttributes []
                }
         
            Serializer.Init()
            let hash = EffectKey.computeHash key

            effectCache.GetOrAdd(hash, fun hash ->
                let map = lazy (Shader.ofExpr [inputType] e |> List.map (fun s -> s.shaderStage, s) |> Map.ofList)
                let effect = Effect(hash, map, [])
                effect.SourceDefinition <- Some (e, [])
                effect
            )

    [<AutoOpen>]
    module private ClosureUtilities =

        let private closureTypes =
            [| typedefof<OptimizedClosures.FSharpFunc<_, _, _>>
               typedefof<OptimizedClosures.FSharpFunc<_, _, _, _>>
               typedefof<OptimizedClosures.FSharpFunc<_, _, _, _, _>>
               typedefof<OptimizedClosures.FSharpFunc<_, _, _, _, _>>
               typedefof<OptimizedClosures.FSharpFunc<_, _, _, _, _, _>> |]

        let (|Closure|_|) ((field, value) : FieldInfo * obj) =
            let ft = field.Type

            if ft.IsGenericType then
                let def = ft.GetGenericTypeDefinition()
                if closureTypes |> Array.contains def then
                    Some value
                else
                    None
            else
                None

        let (|Self|_|) (field : FieldInfo) =
            if field.Name = "self@" then Some ()
            else None

    // Gets the applied arguments from a shader function with parameters
    let private getArguments (shaderFunction : 'a -> Expr<'b>) =

        let rec get (accum : obj list) (func : obj) =
            let typ = func.GetType()

            let arguments =
                typ.GetFields(BindingFlags.Public ||| BindingFlags.Instance)
                |> List.ofArray
                |> List.map (fun f ->
                    f, f.GetValue func
                )

            match arguments with
            | [Closure c; (_, arg)] ->
                get (arg :: accum) c

            | (Self, _) :: args | args ->
                (args |> List.map snd) @ accum

        try
            get [] shaderFunction
        with
        | exn ->
            Log.warn "[FShade] Failed to retrieve arguments of shader function: %s" exn.Message
            []

    /// creates an effect from a shader-function
    let ofFunction (shaderFunction : 'a -> Expr<'b>) =
        let realEx = 
            try shaderFunction Unchecked.defaultof<'a>
            with 
                | :? System.NullReferenceException as n -> 
                    failwithf "[FShade] shader functions may not access their vertex-input statically (inner cause - NullReferenceException: %A)" n.StackTrace
                | e -> 
                    failwithf "[FShade] failed to execute shader function.\nInner cause: %A at\n%A" e e.StackTrace
         
        match realEx with
        | Patterns.WithValue(:? Effect as e,_,_) when UsePrecompiled ->
            e
        | _ ->
            let realEx =
                match realEx with
                | Patterns.WithValue(_, _, e) -> e
                | e -> e

            let expression = Expr.InlineSplices realEx

            let key = 
                let inputFields = getRecordFields typeof<'a>
                let outputFields = getRecordFields typeof<'b>
            
                {
                    inputSemantics = inputFields |> Seq.map (fun f -> f.Semantic, f.Interpolation) |> Map.ofSeq
                    outputSemantics = outputFields |> Seq.map (fun f -> f.Semantic, (f.Interpolation, f.DepthWriteMode)) |> Map.ofSeq
                    body = expression.WithAttributes []
                }
         
            Serializer.Init()
            let hash = EffectKey.computeHash key


            effectCache.GetOrAdd(hash, fun _ ->
                let map =
                    lazy (
                        let shader = Shader.ofExpr [typeof<'a>] expression
                        shader |> List.map (fun s -> s.shaderStage, s) |> Map.ofList
                    )

                let effect = Effect(hash, map, [])
                effect.SourceDefinition <- Some (expression, getArguments shaderFunction)
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
        Effect (lazy (Map.add shader.shaderStage shader effect.Shaders))
        
    /// creates a new effect with the given ShaderStage removed.
    let remove (stage : ShaderStage) (effect : Effect) =
        Effect (lazy (Map.remove stage effect.Shaders))
        
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
        Serializer.Init()
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
        Serializer.Init()
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
        Serializer.Init()
        effect.Shaders
            |> Map.map (fun _ -> f)
            |> ofMap

    let private withDepthRange (flipHandedness : bool) (range : Range1d) (effect : Effect) =
        Serializer.Init()
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
        Serializer.Init()
        
        let entries = 
            lazy (
                let effectLinked =
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

                let shaders = toList effectLinked

                entryPoints None None shaders
            )

        Module(effect.Id, effect, entries, Shader.tryGetOverrideCode V3i.Zero)

    let inputsToUniforms (scopes : Map<string, UniformScope>) (effect : Effect) =
        Serializer.Init()

        let newShaders =
            lazy (
                effect.Shaders
                |> Map.map (fun _ -> Shader.inputsToUniforms scopes)
            )

        Effect(effect.Id + "IU", newShaders, [])

    let uniformsToInputs (semantics : Set<string>) (effect : Effect) =
        Serializer.Init()
        let newShaders =
            lazy (
                effect.Shaders
                |> Map.map (fun _ -> Shader.uniformsToInputs semantics)
            )
            
        Effect(effect.Id + "UI", newShaders, [])

    /// composes two effects using sequential semantics for 'abstract' stages.
    /// these 'abstract' stages consist of the following 'real' stages:
    ///     - Primitive: [Vertex; TessControl; TessEval; Geometry]
    ///     - Fragment:  [Fragment]
    /// effects are composed sequentially per 'abstract' stage. 
    /// e.g. r's VertexShader will be appended to l's GeometryShader (if present)
    let compose2 (l : Effect) (r : Effect) =
        Serializer.Init()
        if l.IsEmpty then r
        elif r.IsEmpty then l
        else
            let resultId = l.Id + r.Id
            composeCache.GetOrAdd(resultId, fun resultId ->
                let shaders =
                    lazy (
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
                        
                        shaders |> Seq.map (fun s -> s.shaderStage, s) |> Map.ofSeq
                    )
                Effect(resultId, shaders, [l;r])
             
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
                InputTopology.Point, lazy ofFunction(nopPoint).GeometryShader.Value
                InputTopology.Line, lazy ofFunction(nopLine).GeometryShader.Value
                InputTopology.Triangle, lazy ofFunction(nopTriangle).GeometryShader.Value
            ]

    let substituteUniforms (substitute : string -> Type -> Option<Expr> -> Option<ShaderSlot> -> Option<Expr>) (effect : Effect) =
        Serializer.Init()
        effect |> map (fun shader ->
            shader |> Shader.substituteReads (fun kind typ name index slot ->
                match kind with
                    | ParameterKind.Uniform ->
                        substitute name typ index slot
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
                    shader.shaderBody.SubstituteReads(fun kind typ oldName index slot ->
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
                                                let arrInput = Expr.ReadInput(kind, typ, newName, slot)
                                                let layerItem = Expr.ArrayAccess(arrInput, layer) //Expr.PropertyGet(arrInput, arrInput.Type.GetProperty("Item"), [layer])
                                                let realItem = Expr.ArrayAccess(arrInput, index) //Expr.PropertyGet(layerItem, layerItem.Type.GetProperty("Item"), [index])

                                                realItem |> Some
                                    
                                            | None ->
                                                let old = shader.shaderUniforms.[oldName]
                                                let typ = Peano.getArrayType layerCount old.uniformType
                                                perLayerUniforms <- Map.add newName { old with uniformName = newName; uniformType = typ } perLayerUniforms
                                                removed <- Map.add oldName () removed
                                                let arrInput = Expr.ReadInput(kind, typ, newName, slot) 
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
        Serializer.Init()
        if effect.TessControlShader.IsSome || effect.TessEvalShader.IsSome then
            failwithf "[FShade] effects containing tessellation shaders cannot be layered automatically"

        let geometryShader =
            match effect.VertexShader, effect.GeometryShader with
                | None, None ->
                    match Map.tryFind topology Helpers.nopGS with
                        | Some gs ->
                            gs.Value
                        | None ->
                            failwithf "[FShade] bad topology for layered shader: %A" topology

                | Some vs, None ->
                    match Map.tryFind topology Helpers.nopGS with
                        | Some gs ->
                            Shader.compose2 gs.Value vs
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
                Effect(Map.ofList [ShaderStage.Geometry, geometryShader; ShaderStage.Fragment, fs] |> Lazy<Map<ShaderStage, Shader>>.CreateFromValue)
            | None ->
                ofList [ geometryShader ]

    let toLayeredEffect (layers : int) (uniforms : Map<string, string>) (topology : InputTopology) (effect : Effect) = 
        toLayeredEffect' Intrinsics.Layer layers uniforms topology effect

    let toMultiViewportEffect (viewports : int) (uniforms : Map<string, string>) (topology : InputTopology) (effect : Effect) = 
        toLayeredEffect' Intrinsics.ViewportIndex viewports uniforms topology effect



    module private GeometryToVertex =
        open Aardvark.Base.Monads.State

        type State =
            {
                emitIndex : int
            }

        

        let rec substituteEmits (e : Expr) =
            state {
                match e with
                | RestartStrip ->
                    return failwith "restartStrip not supported"

                | Sequential(WriteOutputs op, EmitVertex) ->
                    let vertexId = Expr.ReadInput<int>(ParameterKind.Input, Intrinsics.VertexId)
                    let! s = State.get
                    let i = s.emitIndex
                    do! State.put { s with emitIndex = i + 1 }
                    return
                        Expr.IfThenElse(
                            <@ %vertexId = i @>,
                            Expr.WriteOutputs op,
                            Expr.Unit
                        )
                | ShapeLambda(v, b) ->
                    let! b = substituteEmits b
                    return Expr.Lambda(v, b)
                | ShapeVar v ->
                    return e
                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapS substituteEmits
                    return RebuildShapeCombination(o, args)

            }

        let toVertexShader (gs : Shader) =
            let mutable inputs = Map.empty

            let newBody = 
                gs.shaderBody.SubstituteReads(fun kind typ name index _ ->
                    match kind with
                    | ParameterKind.Input ->
                        match index with
                        | Some idx ->
                            match idx with
                            | Value((:? int as i), _) ->
                                let name = sprintf "%s_%d" name i
                                let interp =
                                    Map.tryFind name inputs
                                    |> Option.map ParameterDescription.paramInterpolation
                                    |> Option.defaultValue InterpolationMode.Default

                                inputs <- Map.add name { paramType = typ; paramInterpolation = interp ||| InterpolationMode.Flat } inputs
                                Some (Expr.ReadInput(kind, typ, name))
                            | _ ->
                                failwith "non-constant input access"
                                None
                        | None -> 
                            None
                    | _ ->
                        None
                )
                |> substituteEmits

            let mutable state = { emitIndex = 0 }
            let newBody = newBody.Run(&state)

            { 
                shaderStage = ShaderStage.Vertex
                shaderBody = newBody
                shaderInputs = inputs
                shaderOutputs = gs.shaderOutputs
                shaderUniforms = gs.shaderUniforms
                shaderInputTopology = None
                shaderOutputTopology = None
                shaderOutputVertices = ShaderOutputVertices.Unknown
                shaderOutputPrimitives = None
                shaderInvocations = 1
                shaderDebugRange = None
                shaderPayloads = Map.empty
                shaderPayloadIn = None
                shaderCallableData = Map.empty
                shaderCallableDataIn = None
                shaderHitAttribute = None
                shaderRayTypes = Set.empty
                shaderMissShaders = Set.empty
                shaderCallableShaders = Set.empty
                shaderDepthWriteMode = DepthWriteMode.None
            }
            
    let tryReplaceGeometry (e : Effect) =
        if Option.isSome e.TessControlShader || Option.isSome e.TessEvalShader then
            None
        else
            
            try
                let renameIO (mapping : string -> string) (vs : Shader) =
                    let body = 
                        vs.shaderBody.SubstituteReads(fun kind typ name _ _->
                            match kind with
                            | ParameterKind.Input ->
                                Expr.ReadInput(kind, typ, mapping name) |> Some
                            | _ ->
                                None
                        )
                    let body =
                        body.SubstituteWrites(fun map ->
                            map 
                            |> Map.toList 
                            |> List.map (fun (name, (idx, t)) -> mapping name, idx, t)
                            |> Expr.WriteOutputsRaw
                            |> Some
                        )
                    { vs with 
                        shaderBody = body
                        shaderInputs = vs.shaderInputs |> Map.toList |> List.map (fun (name, t) -> mapping name, t) |> Map.ofList
                        shaderOutputs = vs.shaderOutputs |> Map.toList |> List.map (fun (name, t) -> mapping name, t) |> Map.ofList
                    }

                match e.GeometryShader with
                | Some ({ shaderOutputVertices = (ShaderOutputVertices.Computed ov | ShaderOutputVertices.UserGiven ov) } as gs0)  ->
                    let newShaders =
                        lazy (
                            let newVertex = 
                                let mutable shader = GeometryToVertex.toVertexShader gs0
                                match e.VertexShader with
                                | Some vs ->
                                    let inputs =
                                        match gs0.shaderInputTopology with
                                        | Some InputTopology.Point -> 1
                                        | Some InputTopology.Line -> 2
                                        | Some InputTopology.LineAdjacency -> 4
                                        | Some InputTopology.Triangle -> 3
                                        | Some InputTopology.TriangleAdjacency -> 6
                                        | Some (InputTopology.Patch n) -> n
                                        | None -> 0

                                    for i in 0 .. inputs - 1 do
                                        shader <- Shader.compose2 (renameIO (fun n -> sprintf "%s_%d" n i) vs) shader
                                    shader
                                | None ->
                                    shader
                


                            e.Shaders |> Map.remove ShaderStage.Geometry |> Map.add ShaderStage.Vertex newVertex
                        )

                    Effect(e.Id + "VSS", newShaders, []) |> Some
                | _ ->
                    e |> Some
            with _ ->
                None



