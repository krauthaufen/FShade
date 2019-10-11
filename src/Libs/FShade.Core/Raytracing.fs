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

type RayScene = RayScene of string

type RayQuery<'s, 'a> =
    {
        scene       : RayScene
        origin      : V3d
        direction   : V3d
        payload     : 's
    }

type RayHit<'s, 'a> =
    {
        query       : RayQuery<'s, 'a>
        rayT        : float
        coord       : V2d
        instanceId  : int
        instanceIndex : int
        primitiveId : int
        hitPoint    : V3d
    }

type SamplerInfo =
    {
        samplerType     : Type
        textureName     : string
        samplerState    : SamplerState
        dimension       : SamplerDimension
        isArray         : bool
        isShadow        : bool
        isMS            : bool
        valueType       : Type
    }

type RayHitInfo =
    {
        neededUniforms : Map<string, Type>
        neededSamplers : Map<string, SamplerInfo>
        neededBuffers  : Map<string, int * Type>
        payloadInType  : Type
        payloadOutType : Type
    }


type RayHitInterface =
    {
        uniformBuffers      : Map<int * int, string * list<string * Type>>
        samplers            : Map<int * int, string * SamplerInfo>
        buffers             : Map<int * int, string * int * Type>
        payloadInLocation   : int
        payloadOutLocation  : int
    }



type RayHitShader =
    {
        rayHitHash              : string
        rayPayloadInType        : Type
        rayPayloadOutType       : Type
        rayHitUniforms          : Map<string, UniformParameter>
        rayHitInputs            : Map<string, Type>
        rayHitBody              : Expr
    }

module RayHitShader =

    module Preprocess =
        open Aardvark.Base.Monads.State
        open FShade.Preprocessor.BuilderPatterns

        [<AutoOpen>]
        module Patterns =

            let rec tryGetPropertyInfo (e : Expr) =
                match e with
                | PropertyGet(_, prop, _) -> Some prop
                | ShapeCombination(_, args) -> args |> List.tryPick tryGetPropertyInfo
                | ShapeLambda(_,b) -> tryGetPropertyInfo b
                | ShapeVar _ -> None

            let (|RemoveBuilder|_|) (e : Expr) =
                match e with
                | BuilderCombine(_, l, r) -> Expr.Seq [l;r] |> Some
                | BuilderReturn(_, _, e) -> e |> Some
                | BuilderReturnFrom(_, _, e) -> e |> Some
                | BuilderRun(_, e) -> e |> Some
                | BuilderDelay(_, e) -> e |> Some
                | BuilderZero _ -> Expr.Unit |> Some
                | BuilderFor(_, var, RangeSequence(first, step, last), body) -> Expr.ForInteger(var, first, step, last, body) |> Some
                | BuilderFor(_, var, sequence, body) -> Expr.ForEach(var, sequence, body) |> Some
                | BuilderWhile(b, Lambda(unitVar, guard), body) -> Expr.WhileLoop(guard, body) |> Some
                | _ -> None
            
            let private propertiesEqual (l : PropertyInfo) (r : PropertyInfo) =
                if l.Name = r.Name then
                    let ld = 
                        if l.DeclaringType.IsGenericType then l.DeclaringType.GetGenericTypeDefinition()
                        else l.DeclaringType
                    
                    let rd = 
                        if r.DeclaringType.IsGenericType then r.DeclaringType.GetGenericTypeDefinition()
                        else r.DeclaringType

                    ld = rd
                else
                    false

            let (|SpecificProperty|_|) (expr : Expr<'a>) =
                match tryGetPropertyInfo expr with
                | Some prop ->  
                    fun (e : Expr) -> 
                        match e with
                        | PropertyGet(t, p, i) when propertiesEqual p prop -> 
                            Some i
                        | _ ->
                            None
                | None ->
                    fun _ -> None
                  
            let propertyPath (template : Expr<'a>) =
                match template with
                | Lambda(vv, b) ->
                    let rec pattern (template : Expr) =
                        match template with
                        | Var v when v = vv ->
                            Some
                        | PropertyGet(Some tt, tp, []) ->
                            let inner = pattern tt
                            fun (e : Expr) ->
                                match e with
                                | PropertyGet(Some rt, rp, []) when propertiesEqual tp rp ->
                                    match inner rt with
                                    | Some r -> Some r
                                    | None -> None
                                | _ ->
                                    None
                        | _ ->
                            fun _ -> None
                    pattern b
                | _ ->
                    fun _ -> None


            let (|RayT|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.rayT @>
            let (|Coord|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.coord @>
            let (|InstanceId|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.instanceId @>
            let (|InstanceIndex|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.instanceIndex @>
            let (|PrimitiveId|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.primitiveId @>
            let (|RayDirection|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.query.direction @>
            let (|RayOrigin|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.query.origin @>
            let (|RayPayloadIn|_|) = propertyPath <@ fun (v : RayHit<_,_>) -> v.query.payload @>
      
        type State =
            {
                inType  : Type
                outType : Type
                inputs  : Map<string, Type>
                uniforms : Map<string, UniformParameter>
            }

        let emptyState (inType : Type) (outType : Type) =
            {
                inType = inType
                outType = outType
                inputs = Map.empty
                uniforms = Map.empty
            }

        module State =
            let read (typ : Type) (name : string) =
                State.custom (fun (s : State) ->
                    let s' = { s with inputs = Map.add name typ s.inputs }
                    s', Expr.ReadInput(ParameterKind.Input, typ, name)
                )

            let readUniform (p : UniformParameter) = 
                State.modify (fun s ->
                    match Map.tryFind p.uniformName s.uniforms with
                        | Some u -> s
                        | None -> { s with State.uniforms = Map.add p.uniformName p s.uniforms }
                )

        let private getFields (t : Type) =
            if FSharpType.IsRecord t then 
                FSharpType.GetRecordFields t
                |> Array.toList
                |> List.map (fun f ->
                    f.Name, f.PropertyType
                )
            else
                ["value", t]
        


        let rec preprocess (e : Expr) =
            state {
                match e with

                | RayT _ ->
                    return! State.read typeof<float> Intrinsics.HitT

                | Coord _ ->
                    return! State.read typeof<V2d> Intrinsics.HitCoord

                | InstanceId _ ->
                    return! State.read typeof<int> Intrinsics.InstanceId

                | InstanceIndex _ ->
                    return! State.read typeof<int> Intrinsics.InstanceCustomIndex

                | PrimitiveId _ ->
                    return! State.read typeof<int> Intrinsics.PrimitiveId
                    
                | RayOrigin _ ->
                    return! State.read typeof<V3d> Intrinsics.WorldRayOrigin

                | RayDirection _ ->
                    return! State.read typeof<V3d> Intrinsics.WorldRayDirection

                | RayPayloadIn _ ->
                    let! s = State.get
                    return! State.read s.inType Intrinsics.RayPayloadIn
       

                | Uniform u ->
                    do! State.readUniform u
                    return Expr.ReadInput(ParameterKind.Uniform, e.Type, u.uniformName)

                | BuilderReturn(_, _, e) ->
                    let! e = preprocess e

                    if FSharpType.IsRecord e.Type then
                        let props = FSharpType.GetRecordFields e.Type |> Array.toList

                        let inputs =
                            match e with
                            | NewRecord(_, fields) -> fields
                            | _ -> props |> List.map (fun p -> Expr.PropertyGet(e, p))

                        return 
                            Expr.Seq [
                                let input = Expr.ReadInput(ParameterKind.Output, e.Type, Intrinsics.RayPayloadOut)
                                for (p, i) in List.zip props inputs do
                                    
                                    yield Expr.UnsafePropertySet(input, p, i)
                            ]
                    else 
                        return Expr.WriteOutputs([Intrinsics.RayPayloadOut, None, e])

                | RemoveBuilder e ->
                    return! preprocess e
                   
                | ShapeLambda(v, b) ->  
                    let! b1 = preprocess b
                    return Expr.Lambda(v, b1)

                | ShapeCombination(o, args) ->  
                    let! args1 = args |> List.mapS preprocess
                    return RebuildShapeCombination(o, args1)

                | ShapeVar v ->
                    return Expr.Var v
            }

    let private getInfo (s : RayHitShader) =

        let buffers =
            s.rayHitUniforms |> Map.choose (fun _ u ->
                match u.uniformValue with
                | UniformValue.Attribute(scope, _) when scope = uniform?StorageBuffer ->
                    if u.uniformType.IsArray then
                        let et = u.uniformType.GetElementType()
                        if et.IsArray then
                            Some (2, et.GetElementType())
                        else
                            Some (1, et)
                    else
                        None
                | _ ->
                    None
            )

        let uniforms =
            s.rayHitUniforms |> Map.choose (fun _ u ->
                match u.uniformValue with
                | UniformValue.Sampler _
                | UniformValue.SamplerArray _ -> 
                    None
                | UniformValue.Attribute(scope, _) when scope = uniform?StorageBuffer -> 
                    None
                | _ ->
                    Some u.uniformType
            )
            
        let samplers =
            s.rayHitUniforms |> Map.choose (fun _ u ->
                match u.uniformType with
                | ArrayOf (SamplerType(dim, isArray, isShadow, isMS, valueType))
                | SamplerType(dim, isArray, isShadow, isMS, valueType) ->
                    match u.uniformValue with
                    | UniformValue.Sampler(textureName, samplerState) ->
                        Some {
                            samplerType = u.uniformType
                            textureName = textureName
                            dimension = dim
                            isArray = isArray
                            isShadow = isShadow
                            isMS = isMS
                            valueType = valueType
                            samplerState = samplerState
                        }
                    | _ ->
                        None
                | _ ->
                    None
            )

        {
            payloadInType = s.rayPayloadInType
            payloadOutType = s.rayPayloadOutType
            neededUniforms = uniforms
            neededBuffers = buffers
            neededSamplers = samplers
        }

    let ofFunction (shader : RayHit<'s, 'a> -> Expr<'a>) =
        let rawExpr = shader Unchecked.defaultof<_>
        let hash = Expr.ComputeHash rawExpr


        let inType = typeof<'s>
        let outType = typeof<'a>


        let state, expr = 
            rawExpr
            |> Preprocess.preprocess
            |> Aardvark.Base.Monads.State.State.run (Preprocess.emptyState inType outType)

        {
            rayHitHash = hash
            rayPayloadInType = inType
            rayPayloadOutType = outType
            rayHitUniforms = state.uniforms
            rayHitInputs = state.inputs
            rayHitBody = expr
        }

    let toModule (assign : RayHitInfo -> RayHitInterface) (s : RayHitShader) =
        let info = getInfo s
        let iface = assign info

        let uniforms = 
            iface.uniformBuffers
            |> Map.toList
            |> List.collect (fun ((set, binding), (bufferName, fields)) ->
                fields |> List.mapi (fun i (n, t) ->
                    { 
                        uniformName = n
                        uniformType = t
                        uniformBuffer = Some bufferName
                        uniformDecorations = [UniformDecoration.FieldIndex i; UniformDecoration.BufferBinding binding; UniformDecoration.BufferDescriptorSet set]
                        uniformTextureInfo = []
                    }
                )
            )
            
        let buffers = 
            iface.buffers
            |> Map.toList
            |> List.map (fun ((set, binding), (bufferName, rank, typ)) ->
                let rec mk (n : int) (t : Type) =
                    if n <= 0 then t
                    else mk (n - 1) (t.MakeArrayType())

                { 
                    uniformName = bufferName
                    uniformType = mk rank typ
                    uniformBuffer = Some "StorageBuffer"
                    uniformDecorations = [UniformDecoration.BufferBinding binding; UniformDecoration.BufferDescriptorSet set]
                    uniformTextureInfo = []
                }
            )



        let samplers =
            iface.samplers
            |> Map.toList
            |> List.map (fun ((set, binding), (name, info)) ->
                { 
                    uniformName = name
                    uniformType = info.samplerType
                    uniformBuffer = None
                    uniformDecorations = [UniformDecoration.BufferBinding binding; UniformDecoration.BufferDescriptorSet set]
                    uniformTextureInfo = [info.textureName, info.samplerState :> obj]
                }
            )
            

        let inputs =
            s.rayHitInputs
            |> Map.toList
            |> List.map (fun (name, typ) ->
                { 
                    paramType = typ
                    paramName = name
                    paramSemantic = name
                    paramDecorations = 
                        if typ = s.rayPayloadInType then 
                            Set.ofList [ParameterDecoration.Slot iface.payloadInLocation]
                        else 
                            Set.empty 
                }
            )

        let outputs =
            [
                {
                    paramType = s.rayPayloadOutType
                    paramName = Intrinsics.RayPayloadOut
                    paramSemantic = Intrinsics.RayPayloadOut
                    paramDecorations = Set.ofList [ParameterDecoration.Slot iface.payloadOutLocation]
                }
            ]

        let entry = 
            { 
                conditional = None
                entryName   = "main"
                inputs      = inputs
                outputs     = outputs
                uniforms    = uniforms @ buffers @ samplers
                arguments   = []
                body        = s.rayHitBody
                decorations = [ EntryDecoration.Stages { prev = None; self = ShaderStage.RayHitShader; next = None } ]
            }
        {
            hash = s.rayHitHash
            userData = s
            entries = [ entry ]
            tryGetOverrideCode = Shader.tryGetOverrideCode V3i.Zero
        }

