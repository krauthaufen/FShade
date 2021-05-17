namespace FShade

#nowarn "4321"

open System
open System.Runtime.CompilerServices

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

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


[<AutoOpen>]
module private RaytracingUtilities =

    let rec private substituteStub (sbt : ShaderBindingTableLayout) (e : Expr) =
        match e with
        | Call(None, mi, args) when mi = Preprocessor.traceRayStubMeth ->
            match args with
            | [accel; cullMask; flags;
               String rayId; String missId;
               origin; minT; direction; maxT; payload] ->
                    let sbtRecordOffset = Expr.Value <| sbt.GetRayOffset(rayId)
                    let sbtRecordStride = Expr.Value sbt.RayStride
                    let missIndex = Expr.Value <| sbt.GetMissIndex(missId)

                    Expr.Call(
                        RaytracingIntrinsics.traceRayMeth,
                        [accel; flags; cullMask;
                        sbtRecordOffset; sbtRecordStride; missIndex;
                        origin; minT; direction; maxT; payload]
                    )

            | _ ->
                failwithf "[FShade] Unexpected arguments when substituting traceRay stub: %A" args

        | ShapeLambda(v, b) -> Expr.Lambda(v, substituteStub sbt b)
        | ShapeVar(_) -> e
        | ShapeCombination(o, args) ->
            let args = args |> List.map (substituteStub sbt)
            RebuildShapeCombination(o, args)

    let resolveIndices (sbt : ShaderBindingTableLayout) (shader : Shader) =
        { shader with  shaderBody = substituteStub sbt shader.shaderBody }


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
        PerRayType : Map<string, HitGroupEntry>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal HitGroup =

    let empty =
        { PerRayType = Map.empty }

    let map (mapping : Shader -> Shader) (g : HitGroup) =
        { PerRayType = g.PerRayType |> Map.map (fun _ -> HitGroupEntry.map mapping) }


type RaytracingEffectState =
    {
        RaygenShaders   : Map<string, Shader>
        MissShaders     : Map<string, Shader>
        CallableShaders : Map<string, Shader>
        HitGroups       : Map<string, HitGroup>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal RaytracingEffectState =

    let empty =
        {
            RaygenShaders   = Map.empty
            MissShaders     = Map.empty
            CallableShaders = Map.empty
            HitGroups       = Map.empty
        }

    let shaders (s : RaytracingEffectState) = 
        [|
            yield! s.RaygenShaders |> Map.values
            yield! s.MissShaders |> Map.values
            yield! s.CallableShaders |> Map.values

            for g in Map.values s.HitGroups do
                for e in Map.values g.PerRayType do
                    yield! [e.AnyHit; e.ClosestHit; e.Intersection] |> List.choose id
        |]

    let map (mapping : Shader -> Shader) (s : RaytracingEffectState) =
        { RaygenShaders   = s.RaygenShaders |> Map.map (fun _ -> mapping)
          MissShaders     = s.MissShaders |> Map.map (fun _ -> mapping)
          CallableShaders = s.CallableShaders |> Map.map (fun _ -> mapping)
          HitGroups       = s.HitGroups |> Map.map (fun _ -> HitGroup.map mapping) }

type RaytracingEffect internal(state : RaytracingEffectState) =

    let shaderBindingTableLayout =
        lazy (
            let shaders = RaytracingEffectState.shaders state
            let hitGroups = state.HitGroups |> Map.keys |> Set.ofSeq
            ShaderBindingTableLayout.generate shaders hitGroups
        )

    let state =
        lazy (
            state |> RaytracingEffectState.map (resolveIndices shaderBindingTableLayout.Value)
        )

    member x.ShaderBindingTableLayout =
        shaderBindingTableLayout.Value

    member x.RayGenerationShaders =
        state.Value.RaygenShaders

    member x.HitGroups =
        state.Value.HitGroups

    member x.MissShaders =
        state.Value.MissShaders

    member x.CallableShaders =
        state.Value.CallableShaders


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RaytracingEffect =

    let toModule (effect : RaytracingEffect) =

        let toEntryPoints (shaders : Map<string, Shader>) =
            shaders |> Map.toList |> List.map (fun (name, shader) ->
                Shader.toEntryPoint' (Some name) None shader None
            )

        let hitGroups =
            effect.HitGroups |> Map.toList |> List.collect (fun (name, group) ->
                group.PerRayType |> Map.toList |> List.collect (fun (ray, entry) ->
                    let cond = sprintf "%s_%s" name ray
                    let select = Option.map (fun s -> cond, s)

                    [ select entry.AnyHit
                      select entry.ClosestHit
                      select entry.Intersection ]
                    |> List.choose id
                )
            )
            |> Map.ofList

        let entryPoints =
            [ toEntryPoints effect.RayGenerationShaders
              toEntryPoints effect.MissShaders
              toEntryPoints effect.CallableShaders
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

        member private x.UpdateEntry(s : HitGroup, rayId : string, f : HitGroupEntry -> HitGroupEntry) =
            let entry = s.PerRayType |> Map.tryFind rayId |> Option.defaultValue HitGroupEntry.empty
            { s with PerRayType = s.PerRayType |> Map.add rayId (f entry)}

        [<CustomOperation("anyhit")>]
        member x.AnyHit(s : HitGroup, (rayId, shader) : string * Shader) =
            match shader.shaderStage with
            | ShaderStage.AnyHit ->
                x.UpdateEntry(s, rayId, fun e -> { e with AnyHit = Some shader })
            | _ ->
                failwithf "[FShade] Expected any hit shader but got %A" shader.shaderStage

        member x.AnyHit(s : HitGroup, (rayId, e) : string * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.AnyHit(s, (rayId, shaders.Head))

        member x.AnyHit(s : HitGroup, (rayId, f) : string * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.AnyHit(s, (rayId, shaders.Head))

        member x.AnyHit(s : HitGroup, shader : Shader) =
            x.AnyHit(s, (Identifier.Default, shader))

        member x.AnyHit(s : HitGroup, e : Expr<'a>) =
            x.AnyHit(s, (Identifier.Default, e))

        member x.AnyHit(s : HitGroup, f : 'a -> 'b) =
            x.AnyHit(s, (Identifier.Default, f))


        [<CustomOperation("closesthit")>]
        member x.ClosestHit(s : HitGroup, (rayId, shader) : string * Shader) =
            match shader.shaderStage with
            | ShaderStage.ClosestHit ->
                x.UpdateEntry(s, rayId, fun e -> { e with ClosestHit = Some shader })
            | _ ->
                failwithf "[FShade] Expected closest hit shader but got %A" shader.shaderStage

        member x.ClosestHit(s : HitGroup, (rayId, e) : string * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.ClosestHit(s, (rayId, shaders.Head))

        member x.ClosestHit(s : HitGroup, (rayId, f) : string * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.ClosestHit(s, (rayId, shaders.Head))

        member x.ClosestHit(s : HitGroup, shader : Shader) =
            x.ClosestHit(s, (Identifier.Default, shader))

        member x.ClosestHit(s : HitGroup, e : Expr<'a>) =
            x.ClosestHit(s, (Identifier.Default, e))

        member x.ClosestHit(s : HitGroup, f : 'a -> 'b) =
            x.ClosestHit(s, (Identifier.Default, f))


        [<CustomOperation("intersection")>]
        member x.Intersection(s : HitGroup, (rayId, shader) : string * Shader) =
            match shader.shaderStage with
            | ShaderStage.Intersection ->
                x.UpdateEntry(s, rayId, fun e -> { e with Intersection = Some shader })
            | _ ->
                failwithf "[FShade] Expected intersection shader but got %A" shader.shaderStage

        member x.Intersection(s : HitGroup, (rayId, e) : string * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Intersection(s, (rayId, shaders.Head))

        member x.Intersection(s : HitGroup, (rayId, f) : string * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Intersection(s, (rayId, shaders.Head))

        member x.Intersection(s : HitGroup, shader : Shader) =
            x.Intersection(s, (Identifier.Default, shader))

        member x.Intersection(s : HitGroup, e : Expr<'a>) =
            x.Intersection(s, (Identifier.Default, e))

        member x.Intersection(s : HitGroup, f : 'a -> 'b) =
            x.Intersection(s, (Identifier.Default, f))


    type RaytracingBuilder() =
        member x.Yield(_) = RaytracingEffectState.empty

        member x.Delay f = f()

        member x.Run(s : RaytracingEffectState) =
            RaytracingEffect(s)

        [<CustomOperation("raygen")>]
        member x.Raygen(s : RaytracingEffectState, (name, shader) : string * Shader) =
            match shader.shaderStage with
            | ShaderStage.RayGeneration ->
                { s with RaygenShaders = s.RaygenShaders |> Map.add name shader }
            | _ ->
                failwithf "[FShade] Expected ray generation shader but got %A" shader.shaderStage

        member x.Raygen(s : RaytracingEffectState, (name, e) : string * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Raygen(s, (name, shaders.Head))

        member x.Raygen(s : RaytracingEffectState, (name, f) : string * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Raygen(s, (name, shaders.Head))

        member x.Raygen(s : RaytracingEffectState, shader : Shader) =
            x.Raygen(s, (Identifier.Default, shader))

        member x.Raygen(s : RaytracingEffectState, e : Expr<'a>) =
            x.Raygen(s, (Identifier.Default, e))

        member x.Raygen(s : RaytracingEffectState, f : 'a -> 'b) =
            x.Raygen(s, (Identifier.Default, f))


        [<CustomOperation("miss")>]
        member x.Miss(s : RaytracingEffectState, (name, shader) : string * Shader) =
            match shader.shaderStage with
            | ShaderStage.Miss ->
                { s with MissShaders = s.MissShaders |> Map.add name shader }
            | _ ->
                failwithf "[FShade] Expected miss shader but got %A" shader.shaderStage

        member x.Miss(s : RaytracingEffectState, (name, e) : string * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Miss(s, (name, shaders.Head))

        member x.Miss(s : RaytracingEffectState, (name, f) : string * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Miss(s, (name, shaders.Head))

        member x.Miss(s : RaytracingEffectState, shader : Shader) =
            x.Miss(s, (Identifier.Default, shader))

        member x.Miss(s : RaytracingEffectState, e : Expr<'a>) =
            x.Miss(s, (Identifier.Default, e))

        member x.Miss(s : RaytracingEffectState, f : 'a -> 'b) =
            x.Miss(s, (Identifier.Default, f))


        [<CustomOperation("callable")>]
        member x.Callable(s : RaytracingEffectState, (name, shader) : string * Shader) =
            match shader.shaderStage with
            | ShaderStage.Callable ->
                { s with CallableShaders = s.CallableShaders |> Map.add name shader }
            | _ ->
                failwithf "[FShade] Expected callable shader but got %A" shader.shaderStage

        member x.Callable(s : RaytracingEffectState, (name, e) : string * Expr<'a>) =
            let shaders = Shader.ofExpr [] e
            x.Callable(s, (name, shaders.Head))

        member x.Callable(s : RaytracingEffectState, (name, f) : string * ('a -> 'b)) =
            let shaders = Shader.ofRaytracingFunction f
            x.Callable(s, (name, shaders.Head))

        member x.Callable(s : RaytracingEffectState, shader : Shader) =
            x.Callable(s, (Identifier.Default, shader))

        member x.Callable(s : RaytracingEffectState, e : Expr<'a>) =
            x.Callable(s, (Identifier.Default, e))

        member x.Callable(s : RaytracingEffectState, f : 'a -> 'b) =
            x.Callable(s, (Identifier.Default, f))


        [<CustomOperation("hitgroup")>]
        member x.HitGroup(s : RaytracingEffectState, (name, hitGroup) : string * HitGroup) =
            { s with HitGroups = s.HitGroups |> Map.add name hitGroup}

        member x.HitGroup(s : RaytracingEffectState, hitGroup : HitGroup) =
            { s with HitGroups = s.HitGroups |> Map.add Identifier.Default hitGroup}


    let hitgroup = HitGroupBuilder()
    let raytracing = RaytracingBuilder()