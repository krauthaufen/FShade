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

type ComputeShader internal(id : string, method : MethodBase, localSize : V3i, data : Lazy<ComputeShaderData>) =
    
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ComputeShader =

    let private ofExprInternal (meth : MethodBase) (hash : string) (localSize : V3i) (body0 : Expr) =
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
        

        ComputeShader(hash, meth, localSize, data)

    let private cache = System.Collections.Concurrent.ConcurrentDictionary<string * V3i, ComputeShader>()
    
    let ofExpr (localSize : V3i) (body : Expr) =
        Pickler.ExprPicklerFunctions.Init()
        let body = Expr.InlineSplices body
        let hash = Expr.ComputeHash body
        cache.GetOrAdd((hash, localSize), fun (signature, localSize) ->
            let meth =
                match body.Method with
                    | Some mb -> mb
                    | None -> null
            ofExprInternal meth hash localSize body
        )

    let ofFunction (maxLocalSize : V3i) (f : 'a -> 'b) : ComputeShader =
        match Shader.Utils.tryExtractExpr f with
            | Some (body, _) ->
                Pickler.ExprPicklerFunctions.Init()

                let localSize, meth =
                    match body.Method with
                        | Some mb -> 
                            match mb.GetCustomAttributes<LocalSizeAttribute>() |> Seq.tryHead with
                                | Some att -> V3i(att.X, att.Y, att.Z), mb
                                | _ -> 
                                    Log.warn "[FShade] compute shader without local-size"
                                    V3i(1,1,1), mb
                        | None ->
                            Log.warn "[FShade] compute shader without local-size"
                            V3i(1,1,1), null

                let hash = Pickler.pickler.ComputeHash ((body, localSize))
                let hash = hash.Hash |> System.Convert.ToBase64String

                cache.GetOrAdd((hash, localSize), fun (signature, localSize) ->
                    let body = Expr.InlineSplices body

                    let localSize =
                        V3i(
                            (if localSize.X = MaxLocalSize then maxLocalSize.X else localSize.X),
                            (if localSize.Y = MaxLocalSize then maxLocalSize.Y else localSize.Y),
                            (if localSize.Z = MaxLocalSize then maxLocalSize.Z else localSize.Z)
                        )

                    ofExprInternal meth hash localSize body
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
                    EntryDecoration.Stages { 
                        prev = None
                        self = ShaderStage.Compute
                        next = None 
                    }

                    EntryDecoration.LocalSize s.csLocalSize
                ]
        }

    let toModule (shader : ComputeShader) : Module =
        {
            hash = shader.csId
            userData = shader
            entries = [ toEntryPoint shader ]
            tryGetOverrideCode = Shader.tryGetOverrideCode shader.csLocalSize
        }