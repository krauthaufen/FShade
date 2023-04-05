namespace FShade

#nowarn "4321"

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Aardvark.Base
open FShade.Imperative

module RaytracingIntrinsics =

    [<KeepCall>]
    let traceRay (accelerationStructure : IAccelerationStructure)
                 (rayFlags : RayFlags) (cullMask : int)
                 (sbtRecordOffset : int) (sbtRecordStride : int) (missIndex : int)
                 (origin : V3d) (minT : float)
                 (direction : V3d) (maxT : float)
                 (payload : int) : unit =
        onlyInShaderCode "traceRay"

    let traceRayMeth = getMethodInfo <@ traceRay @>


    [<KeepCall>]
    let executeCallable (index : int) (callable : int) : unit =
        onlyInShaderCode "executeCallable"

    let executeCallableMeth = getMethodInfo <@ executeCallable @>


[<AutoOpen>]
module private RaytracingUtilities =

    // Raytracing data is not global but scoped by the shader slot. Encoding
    // the slot explicitly leads to the creation of per-slot copies of reflected functions
    // calling traceRay or example.
    let rec private setShaderSlotForReads (slot : ShaderSlot) (e : Expr) =
        match e with
        | ReadRaytracingData(name, _) ->
            Expr.ReadRaytracingData(e.Type, name, slot)

        | CallFunction(f, args) ->
            let f = f |> UtilityFunction.map (setShaderSlotForReads slot)
            Expr.CallFunction(f, args |> List.map (setShaderSlotForReads slot))

        | ShapeLambda(v, b) -> Expr.Lambda(v, setShaderSlotForReads slot b)
        | ShapeVar(_) -> e
        | ShapeCombination(o, args) ->
            let args = args |> List.map (setShaderSlotForReads slot)
            RebuildShapeCombination(o, args)

    let rec private substituteStubs (sbt : ShaderBindingTableLayout) (e : Expr) =
        match e with
        | Call(None, mi, args) when mi = Preprocessor.Stubs.traceRayMeth ->
            match args with
            | [accel; cullMask; flags;
               String rayId; String missId;
               origin; minT; direction; maxT; payload] ->
                let sbtRecordOffset = Expr.Value <| sbt.GetRayOffset(Sym.ofString rayId)
                let sbtRecordStride = Expr.Value sbt.RayStride
                let missIndex = Expr.Value <| sbt.GetMissIndex(Sym.ofString missId)

                Expr.Call(
                    RaytracingIntrinsics.traceRayMeth,
                    [accel; flags; cullMask;
                    sbtRecordOffset; sbtRecordStride; missIndex;
                    origin; minT; direction; maxT; payload]
                )

            | _ ->
                failwithf "[FShade] Unexpected arguments when substituting traceRay stub: %A" args

        | Call(None, mi, args) when mi = Preprocessor.Stubs.executeCallableMeth ->
            match args with
            | [String id; callable] ->
                let callableIndex = Expr.Value <| sbt.GetCallableIndex(Sym.ofString id)
                Expr.Call(RaytracingIntrinsics.executeCallableMeth, [callableIndex; callable])

            | _ ->
                failwithf "[FShade] Unexpected arguments when substituting executeCallable stub: %A" args

        | CallFunction(f, args) ->
            let f = f |> UtilityFunction.map (substituteStubs sbt)
            Expr.CallFunction(f, args |> List.map (substituteStubs sbt))

        | ShapeLambda(v, b) -> Expr.Lambda(v, substituteStubs sbt b)
        | ShapeVar(_) -> e
        | ShapeCombination(o, args) ->
            let args = args |> List.map (substituteStubs sbt)
            RebuildShapeCombination(o, args)

    let prepareShader (sbt : ShaderBindingTableLayout) (slot : ShaderSlot) (shader : Shader) =
        let prepared =
            shader.shaderBody
            |> substituteStubs sbt
            |> setShaderSlotForReads slot

        { shader with shaderBody = prepared }


type HitGroupEntry =
    {
        AnyHit       : Shader option
        ClosestHit   : Shader option
        Intersection : Shader option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal HitGroupEntry =

    let empty =
        { AnyHit = None; ClosestHit = None; Intersection = None }

    let map (name : Symbol) (ray : Symbol) (mapping : ShaderSlot -> Shader -> Shader) (e : HitGroupEntry) =
        { AnyHit       = e.AnyHit |> Option.map (mapping <| ShaderSlot.AnyHit(name, ray))
          ClosestHit   = e.ClosestHit |> Option.map (mapping <| ShaderSlot.ClosestHit(name, ray))
          Intersection = e.Intersection |> Option.map (mapping <| ShaderSlot.Intersection(name, ray)) }


type HitGroup =
    {
        PerRayType : Map<Symbol, HitGroupEntry>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal HitGroup =

    let empty =
        { PerRayType = Map.empty }

    let map (name : Symbol) (mapping : ShaderSlot -> Shader -> Shader) (g : HitGroup) =
        { PerRayType = g.PerRayType |> Map.map (fun ray -> HitGroupEntry.map name ray mapping) }


type RaytracingEffectState =
    {
        RaygenShader    : Shader
        MissShaders     : Map<Symbol, Shader>
        CallableShaders : Map<Symbol, Shader>
        HitGroups       : Map<Symbol, HitGroup>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal RaytracingEffectState =
    open System
    open System.IO
    open System.Security.Cryptography

    let empty (raygen : Shader) =
        {
            RaygenShader    = raygen
            MissShaders     = Map.empty
            CallableShaders = Map.empty
            HitGroups       = Map.empty
        }

    let shaders (s : RaytracingEffectState) = 
        [|
            yield s.RaygenShader
            yield! s.MissShaders |> Map.values
            yield! s.CallableShaders |> Map.values

            for g in Map.values s.HitGroups do
                for e in Map.values g.PerRayType do
                    yield! [e.AnyHit; e.ClosestHit; e.Intersection] |> List.choose id
        |]

    let map (mapping : ShaderSlot -> Shader -> Shader) (s : RaytracingEffectState) =
        { RaygenShader    = s.RaygenShader |> mapping ShaderSlot.RayGeneration
          MissShaders     = s.MissShaders |> Map.map (fun n -> mapping (ShaderSlot.Miss n))
          CallableShaders = s.CallableShaders |> Map.map (fun n -> mapping (ShaderSlot.Callable n))
          HitGroups       = s.HitGroups |> Map.map (fun n -> HitGroup.map n mapping) }

    let computeHash (s : RaytracingEffectState) =
        use hash = SHA1.Create()
        use ms = new MemoryStream()
        use h = new CryptoStream(ms, hash, CryptoStreamMode.Write)
        use w = new BinaryWriter(h, System.Text.Encoding.UTF8, true)
        let state = Expr.SerializerState true

        let serializeName (name : Symbol) =
            name |> string |> Value.serialize w typeof<string>

        let serializeShader (s : Shader) =
            s.shaderStage |> int |> Value.serialize w typeof<int>
            s.shaderBody |> Expr.serializeInternal state w

        serializeShader s.RaygenShader

        for (KeyValue(n, s)) in s.MissShaders do
            serializeName n
            serializeShader s

        for (KeyValue(n, s)) in s.CallableShaders do
            serializeName n
            serializeShader s

        for (KeyValue(n, g)) in s.HitGroups do
            serializeName n

            for (KeyValue(n, s)) in g.PerRayType do
                serializeName n
                s.AnyHit |> Option.iter serializeShader
                s.ClosestHit |> Option.iter serializeShader
                s.Intersection |> Option.iter serializeShader

        h.FlushFinalBlock()
        hash.Hash |> Convert.ToBase64String

type RaytracingEffect internal(state : RaytracingEffectState) =

    let shaderBindingTableLayout =
        lazy (
            let shaders = RaytracingEffectState.shaders state
            ShaderBindingTableLayout.generate shaders
        )

    let uniforms =
        lazy (
            RaytracingEffectState.shaders state
            |> Array.map Shader.uniforms
            |> Array.fold Map.union Map.empty
        )

    let state =
        lazy (
            state |> RaytracingEffectState.map (prepareShader shaderBindingTableLayout.Value)
        )

    let id =
        lazy (
            state.Value |> RaytracingEffectState.computeHash
        )

    member x.Id = id.Value

    member x.ShaderBindingTableLayout =
        shaderBindingTableLayout.Value

    member x.RayGenerationShader =
        state.Value.RaygenShader

    member x.HitGroups =
        state.Value.HitGroups

    member x.MissShaders =
        state.Value.MissShaders

    member x.CallableShaders =
        state.Value.CallableShaders

    member x.Uniforms =
        uniforms.Value


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RaytracingEffect =

    let toModule (effect : RaytracingEffect) =
        Serializer.Init()

        let entryPoints = 
            lazy (
            
                let toEntryPoints (shaders : List<ShaderSlot * Shader>) =
                    shaders |> List.map (fun (slot, shader) ->
                        Shader.toEntryPointRaytracing slot shader
                    )

                let hitGroups =
                    effect.HitGroups |> Map.toList |> List.collect (fun (name, group) ->
                        group.PerRayType |> Map.toList |> List.collect (fun (ray, entry) ->
                            let select slot = Option.map (fun s -> slot (name, ray), s)

                            [ entry.AnyHit |> select ShaderSlot.AnyHit
                              entry.ClosestHit |> select ShaderSlot.ClosestHit
                              entry.Intersection |> select ShaderSlot.Intersection ]
                            |> List.choose id
                        )
                    )

                let toList (slot : Symbol -> ShaderSlot) (map : Map<Symbol, Shader>) =
                    map |> Map.toList |> List.map (fun (name, shader) ->
                        slot name, shader
                    )

                [ toEntryPoints [ShaderSlot.RayGeneration, effect.RayGenerationShader]
                  toEntryPoints (effect.MissShaders |> toList ShaderSlot.Miss)
                  toEntryPoints (effect.CallableShaders |> toList ShaderSlot.Callable)
                  toEntryPoints hitGroups ]
                |> List.concat
            )

        {
            hash = effect.Id
            userData = effect
            entries = entryPoints
            tryGetOverrideCode = Shader.tryGetOverrideCode V3i.Zero
        }


[<AutoOpen>]
module RaytracingBuilders =
    open System

    type HitGroupBuilder() =
        do Serializer.Init()

        member x.Yield(_) = HitGroup.empty

        member private x.UpdateEntry(s : HitGroup, rayId : Symbol, f : HitGroupEntry -> HitGroupEntry) =
            let entry = s.PerRayType |> Map.tryFind rayId |> Option.defaultValue HitGroupEntry.empty
            { s with PerRayType = s.PerRayType |> Map.add rayId (f entry)}

        [<CustomOperation("anyHit")>]
        member x.AnyHit(s : HitGroup, rayId : Symbol, shader : Shader) =
            match shader.shaderStage with
            | ShaderStage.AnyHit ->
                x.UpdateEntry(s, rayId, fun e -> { e with AnyHit = Some shader })
            | _ ->
                failwithf "[FShade] Expected any hit shader but got %A" shader.shaderStage

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(s : HitGroup, rayId : Symbol, e : Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.AnyHit(s, rayId, shaders.Head)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(s : HitGroup, rayId : Symbol, f : ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.AnyHit(s, rayId, shaders.Head)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(s : HitGroup, rayId : string, shader : Shader) =
            x.AnyHit(s, Sym.ofString rayId, shader)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(s : HitGroup, rayId : string, e : Expr<'a>) =
            x.AnyHit(s, Sym.ofString rayId, e)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(s : HitGroup, rayId : string, f : ('a -> 'b)) =
            x.AnyHit(s, Sym.ofString rayId, f)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member x.AnyHit(s : HitGroup, (rayId, shader) : Symbol * Shader) =
            x.AnyHit(s, rayId, shader)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.AnyHit(s : HitGroup, (rayId, e) : Symbol * Expr<'a>) =
            x.AnyHit(s, rayId, e)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.AnyHit(s : HitGroup, (rayId, f) : Symbol * ('a -> 'b)) =
            x.AnyHit(s, rayId, f)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.AnyHit(s : HitGroup, (rayId, shader) : string * Shader) =
            x.AnyHit(s, rayId, shader)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.AnyHit(s : HitGroup, (rayId, e) : string * Expr<'a>) =
            x.AnyHit(s, rayId, e)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.AnyHit(s : HitGroup, (rayId, f) : string * ('a -> 'b)) =
            x.AnyHit(s, rayId, f)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(s : HitGroup, shader : Shader) =
            x.AnyHit(s, Identifier.Default, shader)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(s : HitGroup, e : Expr<'a>) =
            x.AnyHit(s, Identifier.Default, e)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(s : HitGroup, f : 'a -> 'b) =
            x.AnyHit(s, Identifier.Default, f)


        [<CustomOperation("closestHit")>]
        member x.ClosestHit(s : HitGroup, rayId : Symbol, shader : Shader) =
            match shader.shaderStage with
            | ShaderStage.ClosestHit ->
                x.UpdateEntry(s, rayId, fun e -> { e with ClosestHit = Some shader })
            | _ ->
                failwithf "[FShade] Expected closest hit shader but got %A" shader.shaderStage

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(s : HitGroup, rayId : Symbol, e : Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.ClosestHit(s, rayId, shaders.Head)

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(s : HitGroup, rayId : Symbol, f : ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.ClosestHit(s, rayId, shaders.Head)

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(s : HitGroup, rayId : string, shader : Shader) =
            x.ClosestHit(s, Sym.ofString rayId, shader)

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(s : HitGroup, rayId : string, e : Expr<'a>) =
            x.ClosestHit(s, Sym.ofString rayId, e)

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(s : HitGroup, rayId : string, f : ('a -> 'b)) =
            x.ClosestHit(s, Sym.ofString rayId, f)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(s : HitGroup, (rayId, shader) : Symbol * Shader) =
            x.ClosestHit(s, rayId, shader)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(s : HitGroup, (rayId, e) : Symbol * Expr<'a>) =
            x.ClosestHit(s, rayId, e)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(s : HitGroup, (rayId, f) : Symbol * ('a -> 'b)) =
            x.ClosestHit(s, rayId, f)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(s : HitGroup, (rayId, shader) : string * Shader) =
            x.ClosestHit(s, rayId, shader)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(s : HitGroup, (rayId, e) : string * Expr<'a>) =
            x.ClosestHit(s, rayId, e)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(s : HitGroup, (rayId, f) : string * ('a -> 'b)) =
            x.ClosestHit(s, rayId, f)

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(s : HitGroup, shader : Shader) =
            x.ClosestHit(s, Identifier.Default, shader)

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(s : HitGroup, e : Expr<'a>) =
            x.ClosestHit(s, Identifier.Default, e)

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(s : HitGroup, f : 'a -> 'b) =
            x.ClosestHit(s, Identifier.Default, f)


        [<CustomOperation("intersection")>]
        member x.Intersection(s : HitGroup, rayId : Symbol, shader : Shader) =
            match shader.shaderStage with
            | ShaderStage.Intersection ->
                x.UpdateEntry(s, rayId, fun e -> { e with Intersection = Some shader })
            | _ ->
                failwithf "[FShade] Expected intersection shader but got %A" shader.shaderStage

        [<CustomOperation("intersection")>]
        member inline x.Intersection(s : HitGroup, rayId : Symbol, e : Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Intersection(s, rayId, shaders.Head)

        [<CustomOperation("intersection")>]
        member inline x.Intersection(s : HitGroup, rayId : Symbol, f : ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Intersection(s, rayId, shaders.Head)

        [<CustomOperation("intersection")>]
        member inline x.Intersection(s : HitGroup, rayId : string, shader : Shader) =
            x.Intersection(s, Sym.ofString rayId, shader)

        [<CustomOperation("intersection")>]
        member inline x.Intersection(s : HitGroup, rayId : string, e : Expr<'a>) =
            x.Intersection(s, Sym.ofString rayId, e)

        [<CustomOperation("intersection")>]
        member inline x.Intersection(s : HitGroup, rayId : string, f : ('a -> 'b)) =
            x.Intersection(s, Sym.ofString rayId, f)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(s : HitGroup, (rayId, shader) : Symbol * Shader) =
            x.Intersection(s, rayId, shader)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(s : HitGroup, (rayId, e) : Symbol * Expr<'a>) =
            x.Intersection(s, rayId, e)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(s : HitGroup, (rayId, f) : Symbol * ('a -> 'b)) =
            x.Intersection(s, rayId, f)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(s : HitGroup, (rayId, shader) : string * Shader) =
            x.Intersection(s, rayId, shader)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(s : HitGroup, (rayId, e) : string * Expr<'a>) =
            x.Intersection(s, rayId, e)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(s : HitGroup, (rayId, f) : string * ('a -> 'b)) =
            x.Intersection(s, rayId, f)

        [<CustomOperation("intersection")>]
        member inline x.Intersection(s : HitGroup, shader : Shader) =
            x.Intersection(s, Identifier.Default, shader)

        [<CustomOperation("intersection")>]
        member inline x.Intersection(s : HitGroup, e : Expr<'a>) =
            x.Intersection(s, Identifier.Default, e)

        [<CustomOperation("intersection")>]
        member inline x.Intersection(s : HitGroup, f : 'a -> 'b) =
            x.Intersection(s, Identifier.Default, f)

    type RayGenerationShaderMustBeSpecified = RayGenerationShaderMustBeSpecified

    type RaytracingEffectBuilder() =
        do Serializer.Init()

        member inline x.Yield(_) = RayGenerationShaderMustBeSpecified

        member inline x.Delay f = f()

        member x.Run(s : RaytracingEffectState) =
            RaytracingEffect(s)

        [<CustomOperation("raygen")>]
        member x.Raygen(_ : RayGenerationShaderMustBeSpecified, shader : Shader) =
            match shader.shaderStage with
            | ShaderStage.RayGeneration ->
                RaytracingEffectState.empty shader
            | _ ->
                failwithf "[FShade] Expected ray generation shader but got %A" shader.shaderStage

        [<CustomOperation("raygen")>]
        member inline x.Raygen(s : RayGenerationShaderMustBeSpecified, e : Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Raygen(s, shaders.Head)

        [<CustomOperation("raygen")>]
        member inline x.Raygen(s : RayGenerationShaderMustBeSpecified, f : ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Raygen(s, shaders.Head)


        [<CustomOperation("miss")>]
        member inline x.Miss(s : RaytracingEffectState, name : Symbol, shader : Shader) =
            match shader.shaderStage with
            | ShaderStage.Miss ->
                { s with MissShaders = s.MissShaders |> Map.add name shader }
            | _ ->
                failwithf "[FShade] Expected miss shader but got %A" shader.shaderStage

        [<CustomOperation("miss")>]
        member inline x.Miss(s : RaytracingEffectState, name : Symbol, e : Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Miss(s, name, shaders.Head)

        [<CustomOperation("miss")>]
        member inline x.Miss(s : RaytracingEffectState, name : Symbol, f : ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Miss(s, name, shaders.Head)

        [<CustomOperation("miss")>]
        member inline x.Miss(s : RaytracingEffectState, name : string, shader : Shader) =
            x.Miss(s, Sym.ofString name, shader)

        [<CustomOperation("miss")>]
        member inline x.Miss(s : RaytracingEffectState, name : string, e : Expr<'a>) =
            x.Miss(s, Sym.ofString name, e)

        [<CustomOperation("miss")>]
        member inline x.Miss(s : RaytracingEffectState, name : string, f : ('a -> 'b)) =
            x.Miss(s, Sym.ofString name, f)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(s : RaytracingEffectState, (name, shader) : Symbol * Shader) =
            x.Miss(s, name, shader)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(s : RaytracingEffectState, (name, e) : Symbol * Expr<'a>) =
            x.Miss(s, name, e)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(s : RaytracingEffectState, (name, f) : Symbol * ('a -> 'b)) =
            x.Miss(s, name, f)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(s : RaytracingEffectState, (name, shader) : string * Shader) =
           x.Miss(s, name, shader)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(s : RaytracingEffectState, (name, e) : string * Expr<'a>) =
            x.Miss(s, name, e)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(s : RaytracingEffectState, (name, f) : string * ('a -> 'b)) =
            x.Miss(s, name, f)

        [<CustomOperation("miss")>]
        member inline x.Miss(s : RaytracingEffectState, shader : Shader) =
            x.Miss(s, Identifier.Default, shader)

        [<CustomOperation("miss")>]
        member inline x.Miss(s : RaytracingEffectState, e : Expr<'a>) =
            x.Miss(s, Identifier.Default, e)

        [<CustomOperation("miss")>]
        member inline x.Miss(s : RaytracingEffectState, f : 'a -> 'b) =
            x.Miss(s, Identifier.Default, f)


        [<CustomOperation("callable")>]
        member inline x.Callable(s : RaytracingEffectState, name : Symbol, shader : Shader) =
            match shader.shaderStage with
            | ShaderStage.Callable ->
                { s with CallableShaders = s.CallableShaders |> Map.add name shader }
            | _ ->
                failwithf "[FShade] Expected callable shader but got %A" shader.shaderStage

        [<CustomOperation("callable")>]
        member inline x.Callable(s : RaytracingEffectState, name : Symbol, e : Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Callable(s, name, shaders.Head)

        [<CustomOperation("callable")>]
        member inline x.Callable(s : RaytracingEffectState, name : Symbol, f : ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Callable(s, name, shaders.Head)

        [<CustomOperation("callable")>]
        member inline x.Callable(s : RaytracingEffectState, name : string, shader : Shader) =
            x.Callable(s, Sym.ofString name, shader)

        [<CustomOperation("callable")>]
        member inline x.Callable(s : RaytracingEffectState, name : string, e : Expr<'a>) =
            x.Callable(s, Sym.ofString name, e)

        [<CustomOperation("callable")>]
        member inline x.Callable(s : RaytracingEffectState, name : string , f : ('a -> 'b)) =
            x.Callable(s, Sym.ofString name, f)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(s : RaytracingEffectState, (name, shader) : Symbol * Shader) =
            x.Callable(s, name, shader)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(s : RaytracingEffectState, (name, e) : Symbol * Expr<'a>) =
            x.Callable(s, name, e)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(s : RaytracingEffectState, (name, f) : Symbol * ('a -> 'b)) =
            x.Callable(s, name, f)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(s : RaytracingEffectState, (name, shader) : string * Shader) =
            x.Callable(s, name, shader)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(s : RaytracingEffectState, (name, e) : string * Expr<'a>) =
            x.Callable(s, name, e)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(s : RaytracingEffectState, (name, f) : string * ('a -> 'b)) =
            x.Callable(s, name, f)

        [<CustomOperation("callable")>]
        member inline x.Callable(s : RaytracingEffectState, shader : Shader) =
            x.Callable(s, Identifier.Default, shader)

        [<CustomOperation("callable")>]
        member inline x.Callable(s : RaytracingEffectState, e : Expr<'a>) =
            x.Callable(s, Identifier.Default, e)

        [<CustomOperation("callable")>]
        member inline x.Callable(s : RaytracingEffectState, f : 'a -> 'b) =
            x.Callable(s, Identifier.Default, f)


        [<CustomOperation("hitgroup")>]
        member inline x.HitGroup(s : RaytracingEffectState, name : Symbol, hitGroup : HitGroup) =
            { s with HitGroups = s.HitGroups |> Map.add name hitGroup }

        [<CustomOperation("hitgroup")>]
        member inline x.HitGroup(s : RaytracingEffectState, name : string, hitGroup : HitGroup) =
            x.HitGroup(s, Sym.ofString name, hitGroup)

        [<CustomOperation("hitgroup"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.HitGroup(s : RaytracingEffectState, (name, hitGroup) : Symbol * HitGroup) =
            x.HitGroup(s, name, hitGroup)

        [<CustomOperation("hitgroup"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.HitGroup(s : RaytracingEffectState, (name, hitGroup) : string * HitGroup) =
            x.HitGroup(s, name, hitGroup)


    let hitgroup = HitGroupBuilder()
    let raytracingEffect = RaytracingEffectBuilder()