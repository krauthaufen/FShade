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

#nowarn "4321"


type ComputeBuffer =
    {
        contentType : Type
        access : StorageAccess
    }

type ComputeBuffer2 =
    {
        arrayType : Type
        access : StorageAccess
    }
    member this.contentType = match this.arrayType with ArrayOf ct | ArrOf(_, ct) | ct -> ct

module internal ComputeBuffer2 =
    let toComputeBuffer (b: ComputeBuffer2) =
        match b.arrayType with
        | ArrayOf ct | ArrOf(_, ct) -> { contentType = ct; access = b.access }
        | _ -> { contentType = b.arrayType; access = b.access }

type ComputeImage =
    {
        imageType : Type
        formatType : Type
        dimension : SamplerDimension
        isArray : bool
        isMS : bool
        contentType : Type
    }


type internal ComputeShaderData =
    {
        csBuffers       : Map<string, ComputeBuffer2>
        csImages        : Map<string, ComputeImage>
        csSamplerStates : Map<string * int, SamplerState>
        csTextureNames  : Map<string * int, string>
        csUniforms      : Map<string, UniformParameter>
        csShared        : Map<string, Type * int>
        csBody          : Expr
    }

type ComputeShader internal(id : string, method : MethodBase, localSize : V3i, data : Lazy<ComputeShaderData>, definition : SourceDefinition) =
    member x.csId = id
    member x.csMethod = method
    member x.csLocalSize = localSize
    member x.csBuffers = data.Value.csBuffers |> Map.map (fun _ -> ComputeBuffer2.toComputeBuffer)
    member x.csBuffers2 = data.Value.csBuffers
    member x.csImages = data.Value.csImages
    member x.csSamplerStates = data.Value.csSamplerStates
    member x.csTextureNames = data.Value.csTextureNames
    member x.csUniforms = data.Value.csUniforms
    member x.csShared = data.Value.csShared
    member x.csBody = data.Value.csBody
    member x.csSourceDefinition = definition

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ComputeShader =

    let private sideEffects =
        HashSet.ofList [
            getMethodInfo <@ barrier @>
        ]

    let private isSideEffect (m : MethodInfo) =
        if sideEffects.Contains m then
            true
        else
            m.GetCustomAttributes<KeepCallAttribute>()
            |> Seq.isEmpty
            |> not

    let private ofExprInternal (meth : MethodBase) (hash : string) (localSize : V3i) (definition : SourceDefinition) (body : Expr) =
        let data =
            lazy (
                let body, state = body |> Preprocessor.preprocess localSize

                let bodyOpt, stateOpt =
                    body
                    |> Optimizer.inlining isSideEffect
                    |> Optimizer.hoistImperativeConstructs
                    |> Optimizer.evaluateConstants' isSideEffect
                    |> Optimizer.eliminateDeadCode' isSideEffect
                    |> Optimizer.evaluateConstants' isSideEffect
                    |> Optimizer.inlining isSideEffect
                    |> Optimizer.liftInputs
                    |> Preprocessor.preprocess localSize

                let bodyOpt =
                    if bodyOpt.Type <> typeof<unit> then
                        Expr.Ignore bodyOpt
                    else
                        bodyOpt

                let state =
                    { state with uniforms = state.uniforms |> Map.filter (fun n _ -> stateOpt.uniforms.ContainsKey n) }

                let mutable buffers = Map.empty<string, ComputeBuffer2>
                let mutable images = Map.empty
                let mutable uniforms = Map.empty
                let mutable samplerStates = Map.empty
                let mutable textureNames = Map.empty

                let addImage (fmt : Type) (name : string) (t : Type) (dim : SamplerDimension) (isArray : bool) (isMS : bool) (contentType : Type) =
                    match Map.tryFind name images with
                        | Some oi ->
                            ()
                        | None ->
                            let img =
                                {
                                    imageType = t
                                    formatType = fmt
                                    dimension = dim
                                    isArray = isArray
                                    isMS = isMS
                                    contentType = contentType
                                }
                            images <- Map.add name img images

                let addBuffer (name : string) (arrayType : Type) (access : StorageAccess) =
                    match Map.tryFind name buffers with
                        | Some b ->
                            buffers <- Map.add name { b with access = b.access ||| access } buffers
                        | None ->
                            buffers <- Map.add name { arrayType = arrayType; access = access } buffers

                let setSamplerState (name : string) (index : int) (state : SamplerState) =
                    match Map.tryFind (name, index) samplerStates with
                        | Some _ -> ()
                        | None ->
                            samplerStates <- Map.add (name, index) state samplerStates

                let setTextureName (name : string) (index : int) (textureName : string) =
                    match Map.tryFind (name, index) textureNames with
                        | Some _ -> ()
                        | None ->
                            textureNames <- Map.add (name, index) textureName textureNames

                let isArgument =
                    let args = meth.GetParameters() |> Array.map (fun p -> p.Name, p.ParameterType) |> Map.ofArray
                    fun name typ ->
                        match args |> Map.tryFindV name with
                        | ValueSome t -> t = typ
                        | _ -> false

                for name, p in Map.toSeq state.uniforms do
                    let isArgument =
                        isArgument name p.uniformType ||
                        (name.StartsWith "cs_" && isArgument (name.Substring(3)) p.uniformType)

                    match p.uniformType, p.uniformValue with
                    | ImageType(fmt, dim, isArr, isMS, valueType), _ ->
                        addImage fmt name p.uniformType dim isArr isMS valueType

                    // Fixed-size array can either be a storage buffer or uniform
                    // Only treat as storage buffer if the scope correct -> fixed-size array as compute argument will always be treated as uniform
                    | ArrayOf _, UniformValue.Attribute _
                    | ArrOf _, UniformValue.Attribute(StorageBufferScope, _) ->
                        match Map.tryFind name state.storageBufferAccess with
                        | Some access ->
                            addBuffer name p.uniformType access
                        | None ->
                            Log.warn "unknown array uniform: %A" name
                            uniforms <- Map.add name p uniforms

                    | _ ->
                        match p.uniformValue with
                        | UniformValue.Sampler(texName, state) ->
                            setSamplerState name 0 state
                            setTextureName name 0 texName

                        | UniformValue.SamplerArray arr ->
                            for i in 0 .. arr.Length - 1 do
                                let texName, state = arr.[i]
                                setSamplerState name i state
                                setTextureName name i texName

                        | _ ->
                            ()

                        if isArgument then
                            uniforms <- Map.add name { p with uniformValue = UniformValue.Attribute(uniform?Arguments, name) } uniforms
                        else
                            uniforms <- Map.add name p uniforms

                {
                    csBuffers       = buffers
                    csImages        = images
                    csSamplerStates = samplerStates
                    csTextureNames  = textureNames
                    csUniforms      = uniforms
                    csBody          = bodyOpt
                    csShared        = Map.empty
                }
            )

        ComputeShader(hash, meth, localSize, data, definition)

    let private cache = System.Collections.Concurrent.ConcurrentDictionary<string, ComputeShader>()
    
    let ofExpr (localSize : V3i) (body : Expr) =
        Serializer.Init()
        let hash = $"{Expr.ComputeHash body}{localSize}"

        cache.GetOrAdd(hash, fun _ ->
            let body = Expr.InlineSplices body

            let meth =
                match body.Method with
                | Some mb -> mb
                | None -> null

            let definition = body |> SourceDefinition.ofExpr []
            ofExprInternal meth hash localSize definition body
        )

    let ofFunction (maxLocalSize : V3i) (f : 'a -> 'b) : ComputeShader =
        match Shader.Utils.tryExtractExpr f with
        | Some (body, inputs) ->
            Serializer.Init()

            let localSize, meth =
                match body.Method with
                | Some mb ->
                    match mb.GetCustomAttributes<LocalSizeAttribute>() |> Seq.tryHead with
                    | Some att ->
                        V3i(
                            (if att.X = MaxLocalSize then maxLocalSize.X else att.X),
                            (if att.Y = MaxLocalSize then maxLocalSize.Y else att.Y),
                            (if att.Z = MaxLocalSize then maxLocalSize.Z else att.Z)
                        ), mb
                    | _ ->
                        Log.warn "[FShade] compute shader without local-size"
                        V3i.One, mb
                | None ->
                    Log.warn "[FShade] compute shader without local-size"
                    V3i.One, null

            let hash = $"{Expr.ComputeHash body}{localSize}"

            cache.GetOrAdd(hash, fun _ ->
                let body = Expr.InlineSplices body
                let definition = body |> SourceDefinition.create inputs f
                ofExprInternal meth hash localSize definition body
            )
        | None ->
            failwithf "[FShade] cannot create compute shader using function: %A" f

    let toEntryPoint (s : ComputeShader) =
        let bufferArguments = 
            s.csBuffers2 |> Map.toList |> List.map (fun (n,i) ->
                { 
                    uniformName = n
                    uniformType = i.arrayType
                    uniformBuffer = Some "StorageBuffer"
                    uniformDecorations = [UniformDecoration.BufferAccess i.access] 
                    uniformTextureInfo = []
                }
            )

        let sharedArguments =
            s.csShared |> Map.toList |> List.map (fun (n,(t,s)) ->
                { 
                    paramName = n
                    paramSemantic = n
                    paramType = Peano.getArrayType s t
                    paramDecorations = Set.ofList [ParameterDecoration.Shared]
                }
            )

        let imageArguments =
            s.csImages |> Map.toList |> List.map (fun (n,u) ->
                let decorations =
                    match u.imageType with
                        | ImageType(fmt,_,_,_,_) -> [ Imperative.UniformDecoration.Format fmt ]
                        | _ -> []
                { 
                    uniformName = n
                    uniformType = u.imageType
                    uniformBuffer = None
                    uniformDecorations = decorations
                    uniformTextureInfo = []
                }
            )


        let uniforms =
            s.csUniforms |> Map.toList |> List.map (fun (n, u) ->
                let uniformBuffer = 
                    match u.uniformValue with
                        | Attribute(scope, name) -> Some scope.FullName
                        | _ -> None


                let textureInfos =
                    match u.uniformValue with
                        | UniformValue.Sampler (n,s) -> [n,s :> obj]
                        | UniformValue.SamplerArray arr -> Array.toList arr |> List.map (fun (n,s) -> n, s :> obj)
                        | _ -> []

                { 
                    uniformName = u.uniformName
                    uniformType = u.uniformType
                    uniformBuffer = uniformBuffer
                    uniformDecorations = u.decorations
                    uniformTextureInfo = textureInfos
                }
            )

        {
            conditional    = None
            entryName      = "main"
            inputs         = []
            outputs        = []
            uniforms       = imageArguments @ uniforms @ bufferArguments
            arguments      = sharedArguments
            raytracingData = []
            body           = s.csBody
            decorations = 
                [
                    EntryDecoration.Stages ShaderStageDescription.Compute
                    EntryDecoration.LocalSize s.csLocalSize
                ]
        }

    let toModule (shader : ComputeShader) : Module =
        let entries = lazy ([ toEntryPoint shader ])
        Module(shader.csId, shader, entries, Shader.tryGetOverrideCode shader.csLocalSize)