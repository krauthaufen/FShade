namespace FShade

//open System
//open System.Reflection

//open Microsoft.FSharp.Quotations
//open Microsoft.FSharp.Quotations.Patterns
//open Microsoft.FSharp.Quotations.DerivedPatterns
//open Microsoft.FSharp.Quotations.ExprShape
//open Microsoft.FSharp.Reflection

//open Aardvark.Base
//open FShade.Imperative

//open System.Collections.Generic
//open Aardvark.Base.ReflectionHelpers
//open FSharp.Data.Adaptive

//#nowarn "4321"

//type WorkDimensions =
//    {
//        [<LaunchId>] id : V2i
//        [<LaunchSize>] size : V2i
//    }

//type GeometryInstance =
//    {
//        [<PrimitiveId>] primitiveId : int
//        [<InstanceId>] instanceId : int
//        [<InstanceCustomIndex>] instanceCustomIndex : int
//        [<GeometryIndex>] geometryIndex : int
//    }

//type RayParameters =
//    {
//        [<WorldRayOrigin>] origin : V3d
//        [<WorldRayDirection>] direction : V3d
//        [<RayTmin>] minT : float
//        [<RayTmax>] maxT : float
//        [<IncomingRayFlags>] flags : uint32
//    }

//type ObjectSpace =
//    {
//        [<ObjectRayOrigin>] rayOrigin : V3d
//        [<ObjectRayDirection>] rayDirection : V3d
//        [<ObjectToWorld>] objectToWorld : M44d
//        [<WorldToObject>] worldToObject : M44d
//    }

//type RayHit =
//    {
//        [<HitT>] t : float
//        [<HitKind>] kind : uint32
//    }


//type RayHitInfo =
//    {
//        work : WorkDimensions
//        geometry : GeometryInstance
//        ray : RayParameters
//        hit : RayHit
//        objectSpace : ObjectSpace
//    }

//type RayIntersection =
//    {
//        work : WorkDimensions
//        geometry : GeometryInstance
//        ray : RayParameters
//        objectSpace : ObjectSpace
//    }

//type RayMiss =
//    {
//        work : WorkDimensions
//        ray : RayParameters
//    }


//type RayScene<'a> =
//    | Self
//    | RayScene of name : string

//type RayEffect =
//    {
//        rayFlags : int
//        cullMask : int
//        sbtRecordOffset : int
//        sbtRecordStride : int
//        missIndex : int
//    }


//type RayQuery<'s, 'a> =
//    {
//        scene       : RayScene<'a>
//        origin      : V3d
//        direction   : V3d
//        tmin        : float
//        tmax        : float
//        payload     : 's
//    }

//module RayQuery =
//    let trace<'s, 'a> (query : RayQuery<'s, 'a>) : 'a = onlyInShaderCode "trace"


////type RayHit<'s, 'a> =
////    {
////        scene       : RayScene<'a>
////        query       : RayQuery<'s, 'a>
////        rayT        : float
////        coord       : V2d
////        instanceId  : int
////        instanceIndex : int
////        primitiveId : int
////    }

//type SamplerInfo =
//    {
//        samplerType     : Type
//        textureName     : string
//        samplerState    : SamplerState
//        dimension       : SamplerDimension
//        isArray         : bool
//        isShadow        : bool
//        isMS            : bool
//        valueType       : Type
//    }

//type RayHitInfo =
//    {
//        neededPayloads : HashSet<Type * Type>
//        neededScenes   : Set<string>
//        neededUniforms : Map<string, Type>
//        neededSamplers : Map<string, SamplerInfo>
//        neededBuffers  : Map<string, int * Type>
//        payloadInType  : Type
//        payloadOutType : Type
//    }


//type RayHitInterface =
//    {
//        uniformBuffers      : Map<int * int, string * list<string * Type>>
//        samplers            : Map<int * int, string * SamplerInfo>
//        buffers             : Map<int * int, string * int * Type>
//        scenes              : Map<int * int, string>
//        payloadLocations    : HashMap<Type * Type, int>
//        payloadInLocation   : int
//        payloadOutLocation  : int
//    }

//type RayHitShader =
//    {
//        rayHitHash              : string
//        rayPayloadInType        : Type
//        rayPayloadOutType       : Type
//        rayHitUniforms          : Map<string, UniformParameter>
//        rayHitInputs            : Map<string, Type>
//        rayHitBody              : Expr
//        rayHitUseInOuts         : HashMap<Type * Type, Var>
//    }

//module RayHitShader =

//    [<KeepCall; GLSLIntrinsic("traceNV({0}, {1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10})")>]
//    let realTrace (scene : AccelerationStructure) (rayFlags : int) (cullMask : int) (sbtRecordOffset : int) (sbtRecordStride : int) (missIndex : int) (origin : V3d) (tmin : float) (dir : V3d) (tmax : float)  (payload : int) : unit = onlyInShaderCode ""

//    let realTraceMeth = getMethodInfo <@ realTrace @>

//    [<GLSLIntrinsic("{0}")>]
//    let identity (v : 'a) : 'a = onlyInShaderCode "identity"

//    let identityMeth = getMethodInfo <@ identity @>

//    module Preprocess =
//        open Aardvark.Base.Monads.State
//        open FShade.Preprocessor.BuilderPatterns

//        [<AutoOpen>]
//        module Patterns =

//            let rec tryGetPropertyInfo (e : Expr) =
//                match e with
//                | PropertyGet(_, prop, _) -> Some prop
//                | ShapeCombination(_, args) -> args |> List.tryPick tryGetPropertyInfo
//                | ShapeLambda(_,b) -> tryGetPropertyInfo b
//                | ShapeVar _ -> None

//            let (|RemoveBuilder|_|) (e : Expr) =
//                match e with
//                | BuilderCombine(_, l, r) -> Expr.Seq [l;r] |> Some
//                | BuilderReturn(_, _, e) -> e |> Some
//                | BuilderReturnFrom(_, _, e) -> e |> Some
//                | BuilderRun(_, e) -> e |> Some
//                | BuilderDelay(_, e) -> e |> Some
//                | BuilderZero _ -> Expr.Unit |> Some
//                | BuilderFor(_, var, RangeSequence(first, step, last), body) -> Expr.ForInteger(var, first, step, last, body) |> Some
//                | BuilderFor(_, var, sequence, body) -> Expr.ForEach(var, sequence, body) |> Some
//                | BuilderWhile(b, Lambda(unitVar, guard), body) -> Expr.WhileLoop(guard, body) |> Some
//                | _ -> None

//            let private propertiesEqual (l : PropertyInfo) (r : PropertyInfo) =
//                if l.Name = r.Name then
//                    let ld =
//                        if l.DeclaringType.IsGenericType then l.DeclaringType.GetGenericTypeDefinition()
//                        else l.DeclaringType

//                    let rd =
//                        if r.DeclaringType.IsGenericType then r.DeclaringType.GetGenericTypeDefinition()
//                        else r.DeclaringType

//                    ld = rd
//                else
//                    false

//            let (|SpecificProperty|_|) (expr : Expr<'a>) =
//                match tryGetPropertyInfo expr with
//                | Some prop ->
//                    fun (e : Expr) ->
//                        match e with
//                        | PropertyGet(t, p, i) when propertiesEqual p prop ->
//                            Some i
//                        | _ ->
//                            None
//                | None ->
//                    fun _ -> None

//            let propertyPath (template : Expr<'a>) =
//                match template with
//                | Lambda(vv, b) ->
//                    let rec pattern (template : Expr) =
//                        match template with
//                        | Var v when v = vv ->
//                            Some
//                        | PropertyGet(Some tt, tp, []) ->
//                            let inner = pattern tt
//                            fun (e : Expr) ->
//                                match e with
//                                | PropertyGet(Some rt, rp, []) when propertiesEqual tp rp ->
//                                    match inner rt with
//                                    | Some r -> Some r
//                                    | None -> None
//                                | _ ->
//                                    None
//                        | _ ->
//                            fun _ -> None
//                    pattern b
//                | _ ->
//                    fun _ -> None


//            let (|RayT|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.rayT @>
//            let (|Coord|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.coord @>
//            let (|InstanceId|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.instanceId @>
//            let (|InstanceIndex|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.instanceIndex @>
//            let (|PrimitiveId|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.primitiveId @>
//            let (|RayDirection|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.query.direction @>
//            let (|RayOrigin|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.query.origin @>
//            let (|RayPayloadIn|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.query.payload @>
//            let (|InputScene|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.query.scene @>

//            let (|SceneValue|_|) (e : Expr) =
//                if e.Type.IsGenericType && e.Type.GetGenericTypeDefinition() = typedefof<RayScene<_>> then
//                    match e with
//                    | NewUnionCase(_, []) -> Some Intrinsics.SelfScene
//                    | NewUnionCase(_, [Value((:? string as v), _)]) -> Some v
//                    | NewUnionCase _ -> failwith "non-constant scene"
//                    | _ -> None
//                else
//                    None

//            let private traceMeth = getMethodInfo <@ RayQuery.trace @>
//            let (|Trace|_|) (e : Expr) =
//                match e with
//                | Call(None, mi, [query]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = traceMeth ->
//                    Some (query)
//                | _ ->
//                    None

//            let (|NewQuery|_|) (e : Expr) =
//                match e with
//                | NewRecord(t, [scene; origin; dir; tmin; tmax; payload]) when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<RayQuery<_,_>> ->
//                    Some (scene, origin, dir, tmin, tmax, payload)
//                | _ ->
//                    None


//        type State =
//            {
//                inType  : Type
//                outType : Type
//                inputs  : Map<string, Type>
//                uniforms : Map<string, UniformParameter>
//                usedInOuts : HashMap<Type * Type, Var * Expr * Expr>
//            }

//        let emptyState (inType : Type) (outType : Type) =
//            {
//                inType = inType
//                outType = outType
//                inputs = Map.empty
//                uniforms = Map.empty
//                usedInOuts = HashMap.empty

//            }

//        module State =

//            let useInOut (inType : Type) (outType : Type) =
//                State.custom (fun (s : State) ->
//                    match HashMap.tryFind (inType, outType) s.usedInOuts with
//                    | Some v -> s, v
//                    | None ->
//                        let id = s.usedInOuts.Count
//                        let v = Var(sprintf "tempPayload%d" id, typeof<int>)
//                        let inName = sprintf "tempPayload%dIn" id
//                        let outName = sprintf "tempPayload%dOut" id
//                        let readIn = Expr.ReadInput(ParameterKind.Uniform, inType, inName)
//                        let readOut = Expr.ReadInput(ParameterKind.Uniform, outType, outName)
//                        let newState =
//                            { s with
//                                usedInOuts = HashMap.add (inType, outType) (v, readIn, readOut) s.usedInOuts
//                            }
//                        newState, (v, readIn, readOut)
//                )

//            let read (typ : Type) (name : string) =
//                State.custom (fun (s : State) ->
//                    let s' = { s with inputs = Map.add name typ s.inputs }
//                    s', Expr.ReadInput(ParameterKind.Input, typ, name)
//                )

//            let readUniform (p : UniformParameter) =
//                State.custom (fun s ->
//                    match Map.tryFind p.uniformName s.uniforms with
//                        | Some u -> s, Expr.ReadInput(ParameterKind.Uniform, u.uniformType, u.uniformName)
//                        | None ->
//                            let s' = { s with State.uniforms = Map.add p.uniformName p s.uniforms }
//                            s', Expr.ReadInput(ParameterKind.Uniform, p.uniformType, p.uniformName)
//                )

//        let private getFields (t : Type) =
//            if FSharpType.IsRecord t then
//                FSharpType.GetRecordFields t
//                |> Array.toList
//                |> List.map (fun f ->
//                    f.Name, f.PropertyType
//                )
//            else
//                ["value", t]





//        let rec preprocess (e : Expr) : State<_, Expr> =
//            state {
//                match e with

//                | Let(v, Var o, b) when not v.IsMutable && not o.IsMutable ->
//                    let b = b.Substitute(fun vi -> if vi = v then Some (Expr.Var o) else None)
//                    return! preprocess b

//                //| Let(v, e, b) when not v.IsMutable ->
//                //    let mutable cnt = 0
//                //    let test = b.Substitute (fun vi -> if vi = v then cnt <- cnt + 1; Some e else None)
//                //    if cnt <= 1 then
//                //        return! preprocess test
//                //    else
//                //        let! e = preprocess e
//                //        let! b = preprocess b
//                //        return Expr.Let(v, e, b)

//                | Trace(query) ->
//                    let rec inlineAll (e : Expr) =
//                        match e with
//                        | Let (v, e, b) -> inlineAll (b.Substitute (fun vi -> if vi = v then Some e else None))
//                        | _ -> e
//                    let better = inlineAll query
//                    match better with
//                    | NewQuery(scene, origin, dir, tmin, tmax, payload) ->
//                        let! scene = preprocess scene
//                        let! origin = preprocess origin
//                        let! dir = preprocess dir
//                        let! tmin = preprocess tmin
//                        let! tmax = preprocess tmax
//                        let! payload = preprocess payload


//                        let sceneName =
//                            match scene with
//                            | ReadInput(ParameterKind.Uniform, name, None) ->
//                                name
//                            | _ ->
//                                failwith "explode"

//                        let! effect =
//                            let name = sprintf "%sEffect" sceneName
//                            State.readUniform {
//                                uniformType = typeof<RayEffect>
//                                uniformName = name
//                                uniformValue = UniformValue.Attribute(UniformScope.Global, name)
//                            }

//                        let cullMask = <@ (%%effect : RayEffect).cullMask @>
//                        let sbtRecordOffset = <@ (%%effect : RayEffect).sbtRecordOffset @>
//                        let sbtRecordStride = <@ (%%effect : RayEffect).sbtRecordStride @>
//                        let missIndex = <@ (%%effect : RayEffect).missIndex @>
//                        let rayFlags = <@ (%%effect : RayEffect).rayFlags @>

//                        let! (v, input, output) = State.useInOut payload.Type e.Type

//                        //let input = Expr.ReadInput(ParameterKind.Uniform, payload.Type, "RayPayloadIn")
//                        //let output = Expr.ReadInput(ParameterKind.Uniform, e.Type, "RayPayloadOut")
//                        //let v = Var("result", output.Type, true)

//                        let outputVars =
//                            FSharpType.GetRecordFields(output.Type, true)
//                            |> Array.toList
//                            |> List.map (fun prop ->
//                                Var(prop.Name, prop.PropertyType, true),
//                                Var(prop.Name, prop.PropertyType, false),
//                                Expr.Call(identityMeth.MakeGenericMethod [|prop.PropertyType|], [Expr.PropertyGet(output, prop)])
//                            )

//                        let rec wrap (bindings : list<Var * Var * Expr>) (body : Expr) =
//                            match bindings with
//                            | [] -> body
//                            | (vm, vi, e) :: rest -> Expr.Let(vm, e, Expr.Let(vi, Expr.Var vm, wrap rest body))


//                        return
//                            Expr.Seq [
//                                for f in FSharpType.GetRecordFields(input.Type, true) do
//                                    Expr.UnsafePropertySet(input, f, Expr.PropertyGet(payload, f))

//                                Expr.Call(realTraceMeth, [scene; rayFlags; cullMask; sbtRecordOffset; sbtRecordStride; missIndex; origin; tmin; dir; tmax; Expr.Var v])

//                                wrap outputVars (
//                                    Expr.NewRecord(output.Type,
//                                        outputVars
//                                        |> List.map (fun (_,v,_) -> Expr.Var v)
//                                    )
//                                )
//                            ]
//                        | _ ->
//                            return failwith "bad query"

//                 | InputScene _ ->
//                    return! State.readUniform {
//                        uniformName = Intrinsics.SelfScene
//                        uniformType = typeof<AccelerationStructure>
//                        uniformValue = UniformValue.Attribute(UniformScope.Global, Intrinsics.SelfScene)
//                    }

//                | SceneValue name ->
//                    return! State.readUniform {
//                        uniformName = name
//                        uniformType = typeof<AccelerationStructure>
//                        uniformValue = UniformValue.Attribute(UniformScope.Global, name)
//                    }

//                | RayT _ ->
//                    return! State.read typeof<float> Intrinsics.HitT

//                | Coord _ ->
//                    return! State.read typeof<V2d> Intrinsics.HitCoord

//                | InstanceId _ ->
//                    return! State.read typeof<int> Intrinsics.InstanceId

//                | InstanceIndex _ ->
//                    return! State.read typeof<int> Intrinsics.InstanceCustomIndex

//                | PrimitiveId _ ->
//                    return! State.read typeof<int> Intrinsics.PrimitiveId

//                | RayOrigin _ ->
//                    return! State.read typeof<V3d> Intrinsics.WorldRayOrigin

//                | RayDirection _ ->
//                    return! State.read typeof<V3d> Intrinsics.WorldRayDirection

//                | RayPayloadIn _ ->
//                    let! s = State.get
//                    return! State.read s.inType Intrinsics.RayPayloadIn


//                | Uniform u ->
//                    return! State.readUniform u

//                | BuilderReturn(_, _, e) ->
//                    let! e = preprocess e

//                    if FSharpType.IsRecord e.Type then
//                        let props = FSharpType.GetRecordFields e.Type |> Array.toList

//                        let inputs =
//                            match e with
//                            | NewRecord(_, fields) -> fields
//                            | _ -> props |> List.map (fun p -> Expr.PropertyGet(e, p))

//                        return
//                            Expr.Seq [
//                                let input = Expr.ReadInput(ParameterKind.Output, e.Type, Intrinsics.RayPayloadOut)
//                                for (p, i) in List.zip props inputs do

//                                    yield Expr.UnsafePropertySet(input, p, i)
//                            ]
//                    else
//                        return Expr.WriteOutputs([Intrinsics.RayPayloadOut, None, e])


//                | RemoveBuilder e ->
//                    return! preprocess e


//                | ShapeLambda(v, b) ->
//                    let! b1 = preprocess b
//                    return Expr.Lambda(v, b1)

//                | ShapeCombination(o, args) ->
//                    let! args1 = args |> List.mapS preprocess
//                    return RebuildShapeCombination(o, args1)

//                | ShapeVar v ->
//                    return Expr.Var v
//            }

//    let private getInfo (s : RayHitShader) =

//        let scenes =
//            s.rayHitUniforms |> Map.toSeq |> Seq.choose (fun (_,u) ->
//                match u.uniformType with
//                | AccelerationStructure -> Some u.uniformName
//                | _ -> None
//            )

//        let rayHitUniorms =
//            s.rayHitUniforms |> Map.choose (fun _ u ->
//                match u.uniformType with
//                | AccelerationStructure -> None
//                | _ -> Some u
//            )

//        let buffers =
//            rayHitUniorms |> Map.choose (fun _ u ->
//                match u.uniformType with
//                | AccelerationStructure -> None
//                | _ ->
//                    match u.uniformValue with
//                    | UniformValue.Attribute(scope, _) when scope = uniform?StorageBuffer ->
//                        if u.uniformType.IsArray then
//                            let et = u.uniformType.GetElementType()
//                            if et.IsArray then
//                                Some (2, et.GetElementType())
//                            else
//                                Some (1, et)
//                        else
//                            None
//                    | _ ->
//                        None
//            )

//        let uniforms =
//            rayHitUniorms |> Map.choose (fun _ u ->
//                match u.uniformValue with
//                | UniformValue.Sampler _
//                | UniformValue.SamplerArray _ ->
//                    None
//                | UniformValue.Attribute(scope, _) when scope = uniform?StorageBuffer ->
//                    None
//                | _ ->
//                    Some u.uniformType
//            )

//        let samplers =
//            rayHitUniorms |> Map.choose (fun _ u ->
//                match u.uniformType with
//                | ArrayOf (SamplerType(dim, isArray, isShadow, isMS, valueType))
//                | SamplerType(dim, isArray, isShadow, isMS, valueType) ->
//                    match u.uniformValue with
//                    | UniformValue.Sampler(textureName, samplerState) ->
//                        Some {
//                            samplerType = u.uniformType
//                            textureName = textureName
//                            dimension = dim
//                            isArray = isArray
//                            isShadow = isShadow
//                            isMS = isMS
//                            valueType = valueType
//                            samplerState = samplerState
//                        }
//                    | _ ->
//                        None
//                | _ ->
//                    None
//            )

//        let free =
//            s.rayHitBody.GetFreeVars()

//        {
//            payloadInType = s.rayPayloadInType
//            payloadOutType = s.rayPayloadOutType
//            neededUniforms = uniforms
//            neededBuffers = buffers
//            neededSamplers = samplers
//            neededScenes = Set.ofSeq scenes
//            neededPayloads = HashMap.keys s.rayHitUseInOuts
//        }

//    let ofFunction (shader : RayHit<'s, 'a> -> Expr<'a>) =
//        let rawExpr = shader Unchecked.defaultof<_>
//        let hash = Expr.ComputeHash rawExpr


//        let inType = typeof<'s>
//        let outType = typeof<'a>


//        let state, expr =
//            rawExpr
//            |> Preprocess.preprocess
//            |> Aardvark.Base.Monads.State.State.run (Preprocess.emptyState inType outType)

//        let isSideEffect (m : MethodInfo) =
//            m.GetCustomAttributes<KeepCallAttribute>()
//            |> Seq.isEmpty
//            |> not

//        let expr =
//            expr
//            |> Optimizer.StatementHoisting.hoistImperative
//            |> Optimizer.inlining isSideEffect
//            |> Optimizer.evaluateConstants' isSideEffect
//            |> Optimizer.eliminateDeadCode' isSideEffect
//            |> Optimizer.evaluateConstants' isSideEffect
//            |> Optimizer.inlining isSideEffect
//        {
//            rayHitHash = hash
//            rayPayloadInType = inType
//            rayPayloadOutType = outType
//            rayHitUniforms = state.uniforms
//            rayHitInputs = state.inputs
//            rayHitBody = expr
//            rayHitUseInOuts = state.usedInOuts |> HashMap.map (fun _ (v,_,_) -> v)
//        }

//    let toModule (assign : RayHitInfo -> RayHitInterface) (s : RayHitShader) =
//        let info = getInfo s
//        let iface = assign info

//        let uniforms =
//            iface.uniformBuffers
//            |> Map.toList
//            |> List.collect (fun ((set, binding), (bufferName, fields)) ->
//                fields |> List.mapi (fun i (n, t) ->
//                    {
//                        uniformName = n
//                        uniformType = t
//                        uniformBuffer = Some bufferName
//                        uniformDecorations = [UniformDecoration.FieldIndex i; UniformDecoration.BufferBinding binding; UniformDecoration.BufferDescriptorSet set]
//                        uniformTextureInfo = []
//                    }
//                )
//            )

//        let buffers =
//            iface.buffers
//            |> Map.toList
//            |> List.map (fun ((set, binding), (bufferName, rank, typ)) ->
//                let rec mk (n : int) (t : Type) =
//                    if n <= 0 then t
//                    else mk (n - 1) (t.MakeArrayType())

//                {
//                    uniformName = bufferName
//                    uniformType = mk rank typ
//                    uniformBuffer = Some "StorageBuffer"
//                    uniformDecorations = [UniformDecoration.BufferBinding binding; UniformDecoration.BufferDescriptorSet set]
//                    uniformTextureInfo = []
//                }
//            )

//        let scenes =
//            iface.scenes
//            |> Map.toList
//            |> List.map (fun ((set, binding), name) ->
//                {
//                    uniformName = name
//                    uniformType = typeof<AccelerationStructure>
//                    uniformBuffer = None
//                    uniformDecorations = [UniformDecoration.BufferBinding binding; UniformDecoration.BufferDescriptorSet set]
//                    uniformTextureInfo = []
//                }

//            )

//        let samplers =
//            iface.samplers
//            |> Map.toList
//            |> List.map (fun ((set, binding), (name, info)) ->
//                {
//                    uniformName = name
//                    uniformType = info.samplerType
//                    uniformBuffer = None
//                    uniformDecorations = [UniformDecoration.BufferBinding binding; UniformDecoration.BufferDescriptorSet set]
//                    uniformTextureInfo = [info.textureName, info.samplerState :> obj]
//                }
//            )

//        let inputs =
//            s.rayHitInputs
//            |> Map.toList
//            |> List.map (fun (name, typ) ->
//                {
//                    paramType = typ
//                    paramName = name
//                    paramSemantic = name
//                    paramDecorations =
//                        if typ = s.rayPayloadInType then
//                            Set.ofList [ParameterDecoration.Slot iface.payloadInLocation]
//                        else
//                            Set.empty
//                }
//            )

//        let outputs =
//            [
//                {
//                    paramType = s.rayPayloadOutType
//                    paramName = Intrinsics.RayPayloadOut
//                    paramSemantic = Intrinsics.RayPayloadOut
//                    paramDecorations = Set.ofList [ParameterDecoration.Slot iface.payloadOutLocation]
//                }
//            ]

//        let mapping =
//            s.rayHitUseInOuts
//            |> HashMap.toSeq
//            |> Seq.map (fun ((i,o), v) ->
//                match HashMap.tryFind (i,o) iface.payloadLocations with
//                | Some loc -> v, loc
//                | None -> failwith "missing assignment for inout"
//            )
//            |> Map.ofSeq

//        let newBody =
//            s.rayHitBody.Substitute(fun v ->
//                match Map.tryFind v mapping with
//                | Some loc -> Expr.Value loc |> Some
//                | None -> None
//            )

//        let tempPayloads =
//            s.rayHitUseInOuts
//            |> HashMap.toList
//            |> List.collect (fun ((i,o), v) ->
//                match HashMap.tryFind (i,o) iface.payloadLocations with
//                | Some loc ->
//                    let inName = v.Name + "In"
//                    let outName =   v.Name + "Out"
//                    [
//                        {
//                            paramType = i
//                            paramName = inName
//                            paramSemantic = inName
//                            paramDecorations = Set.ofList [ParameterDecoration.Slot loc; ParameterDecoration.Raypayload true]
//                        }
//                        {
//                            paramType = o
//                            paramName = outName
//                            paramSemantic = outName
//                            paramDecorations = Set.ofList [ParameterDecoration.Slot loc; ParameterDecoration.Raypayload false]
//                        }
//                    ]
//                | None ->
//                    []
//            )


//        let entry =
//            {
//                conditional = None
//                entryName   = "main"
//                inputs      = inputs @ tempPayloads
//                outputs     = outputs
//                uniforms    = uniforms @ buffers @ samplers @ scenes
//                arguments   = []
//                body        = newBody
//                decorations = [ EntryDecoration.Stages { prev = None; self = ShaderStage.RayHitShader; next = None } ]
//            }
//        {
//            hash = s.rayHitHash
//            userData = s
//            entries = [ entry ]
//            tryGetOverrideCode = Shader.tryGetOverrideCode V3i.Zero
//        }

