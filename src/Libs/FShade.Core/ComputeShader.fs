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


type ComputeShader =
    {
        csLocalSize    : V3i
        csBuffers      : Map<string, ComputeBuffer>
        csImages       : Map<string, ComputeImage>
        csUniforms     : Map<string, UniformParameter>
        csShared       : Map<string, Type * int>
        csBody         : Expr
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ComputeShader =
    
    [<AutoOpen>]
    module private Utils =
        
        let rec (|FunctionType|_|) (t : Type) =
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ -> _> then
                let args = t.GetGenericArguments()
                let invoke = t.GetMethod("Invoke",BindingFlags.Public ||| BindingFlags.Instance, Type.DefaultBinder, CallingConventions.Any, [|args.[0]|], null)
                Some(args.[0], args.[1], invoke)
            else
                match t.BaseType with
                    | null -> None
                    | FunctionType(a, r, m) -> Some(a, r, m)
                    | _ -> None

        let rec tryExtractExpr (f : obj) =
            match f with
                | null -> None
                | :? Expr as e -> Some e
                | _ ->
                    match f.GetType() with
                        | FunctionType(a,_,invoke) ->
                            let argument =
                                if a.IsValueType then Activator.CreateInstance(a)
                                else null

                            let r = invoke.Invoke(f, [|argument|])
                            tryExtractExpr r
                        | _ ->
                            None

        let rec preprocessCompute (localSize : V3i) (sizes : Dictionary<string, int>) (e : Expr) =
            match e with
                | GetArray(ValueWithName(v, t, name), i) ->
                    let i = preprocessCompute localSize sizes i
                    match t with
                        | ArrOf(_,t) | ArrayOf t ->
                            Expr.ReadInput(ParameterKind.Input, t, name, i)
                        | _ ->
                            e

                | SetArray(ValueWithName(v, t, name), i, e) ->
                    let i = preprocessCompute localSize sizes i
                    let e = preprocessCompute localSize sizes e
                    Expr.WriteOutputsRaw([name, Some i, e])

                | PropertyGet(Some (ValueWithName(v, t, name)), prop, []) when t.IsArray && (prop.Name = "Length" || prop.Name = "LongLength") ->
                    Expr.ReadInput(ParameterKind.Uniform, typeof<int>, "cs_" + name + "_length")
                    
                | ValueWithName(v,t,name) ->
                    Expr.ReadInput(ParameterKind.Uniform, t, "cs_" + name)

                | PropertyGet(None, pi, []) when pi.Name = "LocalSize" ->
                    Expr.Value(localSize)

                | FieldGet(None, pi) when pi.Name = "LocalSize" ->
                    Expr.Value(localSize)

                | Let(v, Call(None, mi, [size]),b) when mi.Name = "allocateShared" ->
                    let size = preprocessCompute localSize sizes size
                    match Expr.TryEval size with
                        | Some (:? int as size) ->
                            sizes.[v.Name] <- size

                            let b = 
                                let e = Expr.ValueWithName(null, v.Type, v.Name) //Expr.ReadInput(ParameterKind.Input, v.Type, v.Name)
                                b.Substitute(fun vi ->
                                    if vi = v then Some e
                                    else None
                                )

                            preprocessCompute localSize sizes b

                        | _ ->
                            failwith "[FShade] could not evaluate size for allocateShared"

                | ShapeLambda(v, b) ->
                    Expr.Lambda(v, preprocessCompute localSize sizes b)

                | ShapeVar(v) ->
                    e

                | ShapeCombination(o, args) ->
                    RebuildShapeCombination(o, List.map (preprocessCompute localSize sizes) args)
          
        let rec liftAllocs (localSize : V3i) (sizes : Dictionary<string, int>) (e : Expr) =
            match e with
                | Let(v, Call(None, mi, [size]),b) when mi.Name = "allocateShared" ->
                    let size = liftAllocs localSize sizes size
                    match Expr.TryEval size with
                        | Some (:? int as size) ->
                            sizes.[v.Name] <- size

                            let b = 
                                let e = Expr.ValueWithName(null, v.Type, v.Name) //Expr.ReadInput(ParameterKind.Input, v.Type, v.Name)
                                b.Substitute(fun vi ->
                                    if vi = v then Some e
                                    else None
                                )

                            liftAllocs localSize sizes b

                        | _ ->
                            failwith "[FShade] could not evaluate size for allocateShared"

                | ShapeLambda(v, b) ->
                    Expr.Lambda(v, preprocessCompute localSize sizes b)

                | ShapeVar(v) ->
                    e

                | ShapeCombination(o, args) ->
                    RebuildShapeCombination(o, List.map (preprocessCompute localSize sizes) args)
                


    let ofExpr (localSize : V3i) (body : Expr) =
        let sizes = Dictionary()
        let body = preprocessCompute localSize sizes body
        let body = Optimizer.ConstantFolding.evaluateConstants'' (fun m -> m.DeclaringType.FullName = "FShade.Primitives") body
        let body, state = Preprocessor.preprocess body


        let mutable buffers = Map.empty
        let mutable images = Map.empty
        let mutable uniforms = Map.empty
        let mutable shared = Map.empty

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
            
        let addShared (name : string) (arrayType : Type) (size : int) =
            shared <- Map.add name (arrayType.GetElementType(), size) shared

        for (name, p) in Map.toSeq state.inputs do
            match sizes.TryGetValue name with
                | (true, s) ->
                    addShared name p.paramType s
                | _ -> 
                    match p.paramType with
                        | ImageType(fmt, dim, isArr, isMS, valueType) ->
                            addImage fmt name p.paramType dim isArr isMS valueType
                        | t -> 
                            addBuffer name t true false

        for (name, p) in Map.toSeq state.outputs do
            match sizes.TryGetValue name with
                | (true, s) -> addShared name (p.paramType.MakeArrayType()) s
                | _ -> addBuffer name (p.paramType.MakeArrayType()) false true

        for (name, p) in Map.toSeq state.uniforms do
            let isArgument, name = 
                if name.StartsWith "cs_" then true, name
                else false, name

            match p.uniformType with
                | ImageType(fmt, dim, isArr, isMS, valueType) ->
                    addImage fmt name p.uniformType dim isArr isMS valueType
                | _ ->
                    if isArgument then
                        uniforms <- Map.add name { p with uniformValue = UniformValue.Attribute(uniform?Arguments, name) } uniforms

                    else
                        uniforms <- Map.add name p uniforms
            
            ()
            

        {
            csLocalSize    = localSize
            csBuffers      = buffers
            csImages       = images
            csUniforms     = uniforms
            csBody         = body
            csShared       = shared
        }

    let private cache = System.Collections.Concurrent.ConcurrentDictionary<IFunctionSignature * V3i, ComputeShader>()

    let ofFunction (maxLocalSize : V3i) (f : 'a -> 'b) : ComputeShader =
        let signature = FunctionSignature.ofFunction f

        cache.GetOrAdd((signature, maxLocalSize), fun (signature, maxLocalSize) ->
            let localSize = 
                match FunctionSignature.tryGetAttribute<LocalSizeAttribute> signature with
                    | Some size -> V3i(size.X, size.Y, size.Z)
                    | _ -> V3i(0, 0, 0)

            let localSize =
                V3i(
                    (if localSize.X = MaxLocalSize then maxLocalSize.X else localSize.X),
                    (if localSize.Y = MaxLocalSize then maxLocalSize.Y else localSize.Y),
                    (if localSize.Z = MaxLocalSize then maxLocalSize.Z else localSize.Z)
                )

            match tryExtractExpr f with
                | Some body ->
                    ofExpr localSize body
                | _ ->
                    failwithf "[FShade] cannot create compute shader using function: %A" f
        )

    let toEntryPoint (s : ComputeShader) =
        let bufferArguments = 
            s.csBuffers |> Map.toList |> List.map (fun (n,i) -> 
                { 
                    paramName = n
                    paramSemantic = n
                    paramType = i.contentType.MakeArrayType()
                    paramDecorations = Set.ofList [ParameterDecoration.StorageBuffer]
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
                }
            )


        let uniforms =
            s.csUniforms |> Map.toList |> List.map (fun (n, u) ->
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

        {
            conditional = None
            entryName   = "main"
            inputs      = []
            outputs     = []
            uniforms    = imageArguments @ uniforms
            arguments   = bufferArguments @ sharedArguments
            body        = s.csBody
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
            entries = [ toEntryPoint shader ]
            tryGetOverrideCode = Shader.tryGetOverrideCode
        }