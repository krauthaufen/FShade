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
        read : bool
        write : bool
    }

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
        csBuffers       : Map<string, ComputeBuffer>
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
    member x.csBuffers = data.Value.csBuffers
    member x.csImages = data.Value.csImages
    member x.csSamplerStates = data.Value.csSamplerStates
    member x.csTextureNames = data.Value.csTextureNames
    member x.csUniforms = data.Value.csUniforms
    member x.csShared = data.Value.csShared
    member x.csBody = data.Value.csBody
    member x.csSourceDefinition = definition

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ComputeShader =

    let private ofExprInternal (meth : MethodBase) (hash : string) (localSize : V3i) (definition : SourceDefinition) (body0 : Expr) =
        let data =
            lazy (
                let body1, state = Preprocessor.preprocess localSize body0
                let body2 = Optimizer.ConstantFolding.evaluateConstants'' (fun m -> m.DeclaringType.FullName = "FShade.Primitives") body1
                let body2 = Optimizer.liftInputs body2

                let mutable buffers = Map.empty
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

                let addBuffer (name : string) (arrayType : Type) (read : bool) (write : bool) =
                    match Map.tryFind name buffers with
                        | Some b ->
                            buffers <- Map.add name { b with ComputeBuffer.read = b.read || read } buffers
                        | None ->
                            buffers <- Map.add name { ComputeBuffer.contentType = arrayType.GetElementType(); ComputeBuffer.read = read; ComputeBuffer.write = write } buffers
            
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


                for (name, p) in Map.toSeq state.inputs do
                    match p.paramType with
                        | ImageType(fmt, dim, isArr, isMS, valueType) ->
                            addImage fmt name p.paramType dim isArr isMS valueType
                        | t -> 
                            addBuffer name t true false

                for (name, p) in Map.toSeq state.outputs do
                    addBuffer name p.paramType false true



                for (name, p) in Map.toSeq state.uniforms do
                    let isArgument, name = 
                        if name.StartsWith "cs_" then true, name
                        else false, name

                    match p.uniformType with
                        | ImageType(fmt, dim, isArr, isMS, valueType) ->
                            addImage fmt name p.uniformType dim isArr isMS valueType

                        | SamplerType(dim, isArray, isShadow, isMS, valueType) ->
                            match p.uniformValue with
                                | UniformValue.Sampler(texName, state) ->
                                    setSamplerState name 0 state
                                    setTextureName name 0 texName

                                | UniformValue.SamplerArray arr ->
                                    for i in 0 .. arr.Length - 1 do
                                        let (texName, state) = arr.[i]
                                        setSamplerState name i state
                                        setTextureName name i texName

                                | _ ->
                                    ()

                            uniforms <- Map.add name p uniforms

                        | _ ->
                            if isArgument then
                                uniforms <- Map.add name { p with uniformValue = UniformValue.Attribute(uniform?Arguments, name) } uniforms
                            else
                                uniforms <- Map.add name p uniforms
            
                    ()

                {
                    csBuffers       = buffers
                    csImages        = images
                    csSamplerStates = samplerStates
                    csTextureNames  = textureNames
                    csUniforms      = uniforms
                    csBody          = body2
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
            s.csBuffers |> Map.toList |> List.map (fun (n,i) -> 
                { 
                    paramName = n
                    paramSemantic = n
                    paramType = i.contentType.MakeArrayType()
                    paramDecorations = Set.ofList [ParameterDecoration.StorageBuffer(i.read, i.write)]
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
            uniforms       = imageArguments @ uniforms
            arguments      = bufferArguments @ sharedArguments
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