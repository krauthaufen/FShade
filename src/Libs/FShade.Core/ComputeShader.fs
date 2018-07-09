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


type ComputeShader =
    {
        csId            : string
        csMethod        : MethodBase
        csLocalSize     : V3i
        csBuffers       : Map<string, ComputeBuffer>
        csImages        : Map<string, ComputeImage>
        csSamplerStates : Map<string * int, SamplerState>
        csTextureNames  : Map<string * int, string>
        csUniforms      : Map<string, UniformParameter>
        csShared        : Map<string, Type * int>
        csBody          : Expr
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ComputeShader =
    
    [<AutoOpen>]
    module private Utils =
        open System.Reflection.Emit

        type Invoker<'a, 'b>() =
            static let invoke =
                let t = typeof<'a>

                let rec decompose (t : Type) =
                    if FSharpType.IsFunction t then
                        let (a, r) = FSharpType.GetFunctionElements t

                        match decompose r with
                            | Some (args, ret) ->
                                Some ((a, t) :: args, ret)
                            | None ->
                                None
                    elif typeof<'b>.IsAssignableFrom t then
                        Some ([], t)
                    else
                        None

                match decompose t with
                    | Some (args, ret) ->
                        match args with
                            | [] -> 
                                fun (v : 'a) -> unbox<'b> v
                            | _ ->
                                let dyn = DynamicMethod("invoker", MethodAttributes.Static ||| MethodAttributes.Public, CallingConventions.Standard, typeof<'b>, [| t |], t, true)

                                let il = dyn.GetILGenerator()

                                il.Emit(OpCodes.Ldarg_0)
                                for a, fType in args do
                                    if a.IsValueType then 
                                        let l = il.DeclareLocal(a)
                                        il.Emit(OpCodes.Ldloc, l)
                                    else 
                                        il.Emit(OpCodes.Ldnull)

                                    let m = fType.GetMethod("Invoke", [| a |])
                                    il.EmitCall(OpCodes.Callvirt, m, null)

                                il.Emit(OpCodes.Ret)


                                let d = dyn.CreateDelegate(typeof<Func<'a, 'b>>) |> unbox<Func<'a, 'b>>


                                d.Invoke
                    | None ->
                        fun (v : 'a) -> failwithf "[FShade] cannot invoke type %A" t
                        
            static member Invoke(f : 'a) =
                invoke f 

        let rec tryExtractExpr (f : 'a) =
            match f :> obj with
                | null -> None
                | :? Expr as e -> Some e
                | _ ->
                    try Invoker<'a, Expr>.Invoke f |> Some
                    with _ -> None

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

                | Call(None, mi, [size]) when mi.Name = "allocateShared" ->
                    failwith "[FShade] non-static call to allocateShared"

                | Let(v, Call(None, mi, [size]),b) when mi.Name = "allocateShared" ->
                    let size = preprocessCompute localSize sizes size
                    match Expr.TryEval size with
                        | Some (:? int as size) ->
                            //sizes.[v.Name] <- size

                            let et = mi.ReturnType.GetElementType()
                            let t = Peano.getArrayType size et

                            let rep = Expr.ReadInput(ParameterKind.Uniform, t, v.Name)
                            let rec substitute (e : Expr) =
                                match e with

                                    | GetArray(Var vv, index) when vv = v ->
                                        Peano.getItem rep index
                                        //Expr.ReadInput(ParameterKind.Argument, et, v.Name, index)

                                    | SetArray(Var vv, index, value) when vv = v ->
                                        Peano.setItem rep index value
                                        //Expr.WriteOutputs [v.Name, Some index, value]
                                        //failwith ""

                                    

                                    | ShapeCombination(o, args) -> RebuildShapeCombination(o, List.map substitute args)
                                    | ShapeLambda(v,b) -> Expr.Lambda(v, substitute b)
                                    | ShapeVar vv -> 
                                        if vv = v then failwith "[FShade] cannot use shared memory as value"
                                        Expr.Var vv


                            let b = substitute b
//                                let e = Expr.ReadInput(ParameterKind.Argument, v.Type, v.Name)
//                                b.Substitute(fun vi ->
//                                    if vi = v then Some e
//                                    else None
//                                )

                            preprocessCompute localSize sizes b

                        | _ ->
                            failwith "[FShade] could not evaluate size for allocateShared"

                | ShapeLambda(v, b) ->
                    Expr.Lambda(v, preprocessCompute localSize sizes b)

                | ShapeVar(v) ->
                    e

                | ShapeCombination(o, args) ->
                    RebuildShapeCombination(o, List.map (preprocessCompute localSize sizes) args)


        



    let private ofExprInternal (meth : MethodBase) (hash : string) (localSize : V3i) (body0 : Expr) =
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
            csId            = hash
            csMethod        = meth
            csLocalSize     = localSize
            csBuffers       = buffers
            csImages        = images
            csSamplerStates = samplerStates
            csTextureNames  = textureNames
            csUniforms      = uniforms
            csBody          = body2
            csShared        = Map.empty
        }

    let private cache = System.Collections.Concurrent.ConcurrentDictionary<string * V3i, ComputeShader>()
    
    let ofExpr (localSize : V3i) (body : Expr) =
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
        match tryExtractExpr f with
            | Some body ->
                let body = Expr.InlineSplices body
                let hash = Expr.ComputeHash body

                cache.GetOrAdd((hash, maxLocalSize), fun (signature, maxLocalSize) ->
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
//
//
//        let signature = FunctionSignature.ofFunction f
//
//        cache.GetOrAdd((signature, maxLocalSize), fun (signature, maxLocalSize) ->
//            let localSize = 
//                match FunctionSignature.tryGetAttribute<LocalSizeAttribute> signature with
//                    | Some size -> V3i(size.X, size.Y, size.Z)
//                    | _ -> V3i(0, 0, 0)
//
//            let localSize =
//                V3i(
//                    (if localSize.X = MaxLocalSize then maxLocalSize.X else localSize.X),
//                    (if localSize.Y = MaxLocalSize then maxLocalSize.Y else localSize.Y),
//                    (if localSize.Z = MaxLocalSize then maxLocalSize.Z else localSize.Z)
//                )
//
//            match tryExtractExpr f with
//                | Some body ->
//                    ofExpr localSize body
//                | _ ->
//                    failwithf "[FShade] cannot create compute shader using function: %A" f
//        )

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
            hash = shader.csId
            userData = shader
            entries = [ toEntryPoint shader ]
            tryGetOverrideCode = Shader.tryGetOverrideCode shader.csLocalSize
        }