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

    let resolveIndices (sbt : ShaderBindingTableLayout) (shader : Shader) =
        { shader with  shaderBody = substituteStubs sbt shader.shaderBody }


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

    let map (mapping : Shader -> Shader) (e : HitGroupEntry) =
        { AnyHit       = e.AnyHit |> Option.map mapping
          ClosestHit   = e.ClosestHit |> Option.map mapping
          Intersection = e.Intersection |> Option.map mapping }


type HitGroup =
    {
        PerRayType : Map<Symbol, HitGroupEntry>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal HitGroup =

    let empty =
        { PerRayType = Map.empty }

    let map (mapping : Shader -> Shader) (g : HitGroup) =
        { PerRayType = g.PerRayType |> Map.map (fun _ -> HitGroupEntry.map mapping) }


type RaytracingEffectState =
    {
        RaygenShader    : Shader
        MissShaders     : Map<Symbol, Shader>
        CallableShaders : Map<Symbol, Shader>
        HitGroups       : Map<Symbol, HitGroup>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal RaytracingEffectState =

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

    let map (mapping : Shader -> Shader) (s : RaytracingEffectState) =
        { RaygenShader    = s.RaygenShader |> mapping
          MissShaders     = s.MissShaders |> Map.map (fun _ -> mapping)
          CallableShaders = s.CallableShaders |> Map.map (fun _ -> mapping)
          HitGroups       = s.HitGroups |> Map.map (fun _ -> HitGroup.map mapping) }

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
            state |> RaytracingEffectState.map (resolveIndices shaderBindingTableLayout.Value)
        )

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

        let toEntryPoints (shaders : List<RaytracingStageDescription * Shader>) =
            shaders |> List.map (fun (stage, shader) ->
                Shader.toEntryPointRaytracing stage shader
            )

        let hitGroups =
            effect.HitGroups |> Map.toList |> List.collect (fun (name, group) ->
                group.PerRayType |> Map.toList |> List.collect (fun (ray, entry) ->
                    let select stage = Option.map (fun s -> stage (name, ray), s)

                    [ entry.AnyHit |> select RaytracingStageDescription.AnyHit
                      entry.ClosestHit |> select RaytracingStageDescription.ClosestHit
                      entry.Intersection |> select RaytracingStageDescription.Intersection ]
                    |> List.choose id
                )
            )

        let toList (stage : Symbol -> RaytracingStageDescription) (map : Map<Symbol, Shader>) =
            map |> Map.toList |> List.map (fun (name, shader) ->
                stage name, shader
            )

        let entryPoints =
            [ toEntryPoints [RaytracingStageDescription.RayGeneration, effect.RayGenerationShader]
              toEntryPoints (effect.MissShaders |> toList RaytracingStageDescription.Miss)
              toEntryPoints (effect.CallableShaders |> toList RaytracingStageDescription.Callable)
              toEntryPoints hitGroups ]
            |> List.concat

        {
            hash = effect |> hash |> string
            userData = effect
            entries = entryPoints
            tryGetOverrideCode = Shader.tryGetOverrideCode V3i.Zero
        }


[<AutoOpen>]
module RaytracingBuilders =

    type HitGroupBuilder() =
        member x.Yield(_) = HitGroup.empty

        member private x.UpdateEntry(s : HitGroup, rayId : Symbol, f : HitGroupEntry -> HitGroupEntry) =
            let entry = s.PerRayType |> Map.tryFind rayId |> Option.defaultValue HitGroupEntry.empty
            { s with PerRayType = s.PerRayType |> Map.add rayId (f entry)}

        [<CustomOperation("anyhit")>]
        member x.AnyHit(s : HitGroup, (rayId, shader) : Symbol * Shader) =
            match shader.shaderStage with
            | ShaderStage.AnyHit ->
                x.UpdateEntry(s, rayId, fun e -> { e with AnyHit = Some shader })
            | _ ->
                failwithf "[FShade] Expected any hit shader but got %A" shader.shaderStage

        member x.AnyHit(s : HitGroup, (rayId, e) : Symbol * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.AnyHit(s, (rayId, shaders.Head))

        member x.AnyHit(s : HitGroup, (rayId, f) : Symbol * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.AnyHit(s, (rayId, shaders.Head))

        member x.AnyHit(s : HitGroup, (rayId, shader) : string * Shader) =
             x.AnyHit(s, (Sym.ofString rayId, shader))

         member x.AnyHit(s : HitGroup, (rayId, e) : string * Expr<'a>) =
             x.AnyHit(s, (Sym.ofString rayId, e))

         member x.AnyHit(s : HitGroup, (rayId, f) : string * ('a -> 'b)) =
             x.AnyHit(s, (Sym.ofString rayId, f))

        member x.AnyHit(s : HitGroup, shader : Shader) =
            x.AnyHit(s, (Identifier.Default, shader))

        member x.AnyHit(s : HitGroup, e : Expr<'a>) =
            x.AnyHit(s, (Identifier.Default, e))

        member x.AnyHit(s : HitGroup, f : 'a -> 'b) =
            x.AnyHit(s, (Identifier.Default, f))


        [<CustomOperation("closesthit")>]
        member x.ClosestHit(s : HitGroup, (rayId, shader) : Symbol * Shader) =
            match shader.shaderStage with
            | ShaderStage.ClosestHit ->
                x.UpdateEntry(s, rayId, fun e -> { e with ClosestHit = Some shader })
            | _ ->
                failwithf "[FShade] Expected closest hit shader but got %A" shader.shaderStage

        member x.ClosestHit(s : HitGroup, (rayId, e) : Symbol * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.ClosestHit(s, (rayId, shaders.Head))

        member x.ClosestHit(s : HitGroup, (rayId, f) : Symbol * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.ClosestHit(s, (rayId, shaders.Head))

        member x.ClosestHit(s : HitGroup, (rayId, shader) : string * Shader) =
            x.ClosestHit(s, (Sym.ofString rayId, shader))

        member x.ClosestHit(s : HitGroup, (rayId, e) : string * Expr<'a>) =
            x.ClosestHit(s, (Sym.ofString rayId, e))

        member x.ClosestHit(s : HitGroup, (rayId, f) : string * ('a -> 'b)) =
            x.ClosestHit(s, (Sym.ofString rayId, f))

        member x.ClosestHit(s : HitGroup, shader : Shader) =
            x.ClosestHit(s, (Identifier.Default, shader))

        member x.ClosestHit(s : HitGroup, e : Expr<'a>) =
            x.ClosestHit(s, (Identifier.Default, e))

        member x.ClosestHit(s : HitGroup, f : 'a -> 'b) =
            x.ClosestHit(s, (Identifier.Default, f))


        [<CustomOperation("intersection")>]
        member x.Intersection(s : HitGroup, (rayId, shader) : Symbol * Shader) =
            match shader.shaderStage with
            | ShaderStage.Intersection ->
                x.UpdateEntry(s, rayId, fun e -> { e with Intersection = Some shader })
            | _ ->
                failwithf "[FShade] Expected intersection shader but got %A" shader.shaderStage

        member x.Intersection(s : HitGroup, (rayId, e) : Symbol * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Intersection(s, (rayId, shaders.Head))

        member x.Intersection(s : HitGroup, (rayId, f) : Symbol * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Intersection(s, (rayId, shaders.Head))

        member x.Intersection(s : HitGroup, (rayId, shader) : string * Shader) =
            x.Intersection(s, (Sym.ofString rayId, shader))

        member x.Intersection(s : HitGroup, (rayId, e) : string * Expr<'a>) =
            x.Intersection(s, (Sym.ofString rayId, e))

        member x.Intersection(s : HitGroup, (rayId, f) : string * ('a -> 'b)) =
            x.Intersection(s, (Sym.ofString rayId, f))

        member x.Intersection(s : HitGroup, shader : Shader) =
            x.Intersection(s, (Identifier.Default, shader))

        member x.Intersection(s : HitGroup, e : Expr<'a>) =
            x.Intersection(s, (Identifier.Default, e))

        member x.Intersection(s : HitGroup, f : 'a -> 'b) =
            x.Intersection(s, (Identifier.Default, f))

    type RayGenerationShaderMustBeSpecified = RayGenerationShaderMustBeSpecified

    type RaytracingBuilder() =
        member x.Yield(_) = RayGenerationShaderMustBeSpecified

        member x.Delay f = f()

        member x.Run(s : RaytracingEffectState) =
            RaytracingEffect(s)

        [<CustomOperation("raygen")>]
        member x.Raygen(_ : RayGenerationShaderMustBeSpecified, shader : Shader) =
            match shader.shaderStage with
            | ShaderStage.RayGeneration ->
                RaytracingEffectState.empty shader
            | _ ->
                failwithf "[FShade] Expected ray generation shader but got %A" shader.shaderStage

        member x.Raygen(s : RayGenerationShaderMustBeSpecified, e : Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Raygen(s, shaders.Head)

        member x.Raygen(s : RayGenerationShaderMustBeSpecified, f : ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Raygen(s, shaders.Head)


        [<CustomOperation("miss")>]
        member x.Miss(s : RaytracingEffectState, (name, shader) : Symbol * Shader) =
            match shader.shaderStage with
            | ShaderStage.Miss ->
                { s with MissShaders = s.MissShaders |> Map.add name shader }
            | _ ->
                failwithf "[FShade] Expected miss shader but got %A" shader.shaderStage

        member x.Miss(s : RaytracingEffectState, (name, e) : Symbol * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Miss(s, (name, shaders.Head))

        member x.Miss(s : RaytracingEffectState, (name, f) : Symbol * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Miss(s, (name, shaders.Head))

        member x.Miss(s : RaytracingEffectState, (name, shader) : string * Shader) =
            x.Miss(s, (Sym.ofString name, shader))

        member x.Miss(s : RaytracingEffectState, (name, e) : string * Expr<'a>) =
            x.Miss(s, (Sym.ofString name, e))

        member x.Miss(s : RaytracingEffectState, (name, f) : string * ('a -> 'b)) =
            x.Miss(s, (Sym.ofString name, f))

        member x.Miss(s : RaytracingEffectState, shader : Shader) =
            x.Miss(s, (Identifier.Default, shader))

        member x.Miss(s : RaytracingEffectState, e : Expr<'a>) =
            x.Miss(s, (Identifier.Default, e))

        member x.Miss(s : RaytracingEffectState, f : 'a -> 'b) =
            x.Miss(s, (Identifier.Default, f))


        [<CustomOperation("callable")>]
        member x.Callable(s : RaytracingEffectState, (name, shader) : Symbol * Shader) =
            match shader.shaderStage with
            | ShaderStage.Callable ->
                { s with CallableShaders = s.CallableShaders |> Map.add name shader }
            | _ ->
                failwithf "[FShade] Expected callable shader but got %A" shader.shaderStage

        member x.Callable(s : RaytracingEffectState, (name, e) : Symbol * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Callable(s, (name, shaders.Head))

        member x.Callable(s : RaytracingEffectState, (name, f) : Symbol * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Callable(s, (name, shaders.Head))

        member x.Callable(s : RaytracingEffectState, (name, shader) : string * Shader) =
            x.Callable(s, (Sym.ofString name, shader))

        member x.Callable(s : RaytracingEffectState, (name, e) : string * Expr<'a>) =
            x.Callable(s, (Sym.ofString name, e))

        member x.Callable(s : RaytracingEffectState, (name, f) : string * ('a -> 'b)) =
            x.Callable(s, (Sym.ofString name, f))

        member x.Callable(s : RaytracingEffectState, shader : Shader) =
            x.Callable(s, (Identifier.Default, shader))

        member x.Callable(s : RaytracingEffectState, e : Expr<'a>) =
            x.Callable(s, (Identifier.Default, e))

        member x.Callable(s : RaytracingEffectState, f : 'a -> 'b) =
            x.Callable(s, (Identifier.Default, f))


        [<CustomOperation("hitgroup")>]
        member x.HitGroup(s : RaytracingEffectState, (name, hitGroup) : Symbol * HitGroup) =
            { s with HitGroups = s.HitGroups |> Map.add name hitGroup}

        member x.HitGroup(s : RaytracingEffectState, (name, hitGroup) : string * HitGroup) =
            x.HitGroup(s, (Sym.ofString name, hitGroup))

        member x.HitGroup(s : RaytracingEffectState, hitGroup : HitGroup) =
            x.HitGroup(s, (Identifier.Default, hitGroup))


    let hitgroup = HitGroupBuilder()
    let raytracing = RaytracingBuilder()