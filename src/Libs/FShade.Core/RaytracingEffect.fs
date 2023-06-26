namespace FShade

#nowarn "4321"

open System
open System.IO
open System.Security.Cryptography

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

    let prepareShader (sbt : ShaderBindingTableLayout) (slot : ShaderSlot) (shader : RaytracingShader) =
        let prepared =
            shader.Body
            |> substituteStubs sbt
            |> setShaderSlotForReads slot

        shader.Body <- prepared

    let computeHash (shaders : Map<ShaderSlot, RaytracingShader>) =
        use hash = SHA1.Create()
        use ms = new MemoryStream()
        use h = new CryptoStream(ms, hash, CryptoStreamMode.Write)
        use w = new BinaryWriter(h, System.Text.Encoding.UTF8, true)

        let serializeName (name : Symbol) =
            w.Write (string name)

        for (KeyValue(slot, shader)) in shaders do
            w.Write (int slot.Stage)

            match slot with
            | ShaderSlot.Miss name
            | ShaderSlot.Callable name ->
                serializeName name

            | ShaderSlot.AnyHit (name, ray)
            | ShaderSlot.ClosestHit (name, ray)
            | ShaderSlot.Intersection (name, ray) ->
                serializeName name
                serializeName ray

            | _ -> ()

            w.Write shader.Id

        h.FlushFinalBlock()
        hash.Hash |> Convert.ToBase64String


type RaytracingEffect(shaders : Map<ShaderSlot, RaytracingShader>) =
    do for KeyValue(slot, shader) in shaders do
        if shader.Stage <> slot.Stage then raise <| ArgumentException($"Invalid {slot.Stage} shader in slot {slot}.")

    let id = computeHash shaders

    let shaderBindingTableLayout =
        lazy (
            let shaders = shaders |> Map.values |> Array.ofSeq
            ShaderBindingTableLayout.generate shaders
        )

    let uniforms =
        lazy (
            shaders |> Map.values |> Array.ofSeq
            |> Array.map (fun s -> s.Uniforms)
            |> Array.fold Map.union Map.empty
        )

    let shaders =
        lazy (
            let sbt = shaderBindingTableLayout.Value

            for KeyValue(slot, shader) in shaders do
                shader |> prepareShader sbt slot

            shaders
        )

    member x.Id = id

    member x.ShaderBindingTableLayout =
        shaderBindingTableLayout.Value

    member x.Shaders =
        shaders.Value

    member x.Uniforms =
        uniforms.Value

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RaytracingEffect =

    let toModule (effect : RaytracingEffect) =
        Serializer.Init()

        let entryPoints =
            lazy (
                effect.Shaders
                |> Map.toList
                |> List.map (fun (slot, shader) -> RaytracingShader.toEntryPoint slot shader)
            )

        Module(effect.Id, effect, entryPoints, Shader.tryGetOverrideCode V3i.Zero)


[<AutoOpen>]
module RaytracingBuilders =

    type HitGroup =
        {
            AnyHit       : Map<Symbol, RaytracingShader>
            ClosestHit   : Map<Symbol, RaytracingShader>
            Intersection : Map<Symbol, RaytracingShader>
        }

    type HitGroupBuilder() =
        do Serializer.Init()

        member x.Yield(_) = { AnyHit = Map.empty; ClosestHit = Map.empty; Intersection = Map.empty }

        member private x.SetShader(group : HitGroup, rayType : Symbol, stage : ShaderStage, shader : RaytracingShader) =
            if shader.Stage <> stage then
                failwithf "[FShade] Expected %A shader but got %A shader." stage shader.Stage

            match stage with
            | ShaderStage.AnyHit     -> { group with AnyHit       = group.AnyHit       |> Map.add rayType shader }
            | ShaderStage.ClosestHit -> { group with ClosestHit   = group.ClosestHit   |> Map.add rayType shader }
            | _                      -> { group with Intersection = group.Intersection |> Map.add rayType shader }


        [<CustomOperation("anyHit")>]
        member x.AnyHit(group : HitGroup, rayType : Symbol, shader : Shader) =
            x.SetShader(group, rayType, ShaderStage.AnyHit, RaytracingShader.ofShader shader)

        [<CustomOperation("anyHit")>]
        member x.AnyHit(group : HitGroup, rayType : Symbol, e : Expr<'a>) =
            x.SetShader(group, rayType, ShaderStage.AnyHit, RaytracingShader.ofExpr [] e)

        [<CustomOperation("anyHit")>]
        member x.AnyHit(group : HitGroup, rayType : Symbol, f : 'a -> Expr<'b>) =
            x.SetShader(group, rayType, ShaderStage.AnyHit, RaytracingShader.ofFunction f)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(group : HitGroup, rayType : string, shader : Shader) =
            x.AnyHit(group, Sym.ofString rayType, shader)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(group : HitGroup, rayType : string, e : Expr<'a>) =
            x.AnyHit(group, Sym.ofString rayType, e)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(group : HitGroup, rayType : string, f : 'a -> Expr<'b>) =
            x.AnyHit(group, Sym.ofString rayType, f)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member x.AnyHit(group : HitGroup, (rayType, shader) : Symbol * Shader) =
            x.AnyHit(group, rayType, shader)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.AnyHit(group : HitGroup, (rayType, e) : Symbol * Expr<'a>) =
            x.AnyHit(group, rayType, e)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.AnyHit(group : HitGroup, (rayType, f) : Symbol * ('a -> Expr<'b>)) =
            x.AnyHit(group, rayType, f)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.AnyHit(group : HitGroup, (rayType, shader) : string * Shader) =
            x.AnyHit(group, rayType, shader)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.AnyHit(group : HitGroup, (rayType, e) : string * Expr<'a>) =
            x.AnyHit(group, rayType, e)

        [<CustomOperation("anyHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.AnyHit(group : HitGroup, (rayType, f) : string * ('a -> Expr<'b>)) =
            x.AnyHit(group, rayType, f)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(group : HitGroup, shader : Shader) =
            x.AnyHit(group, Identifier.Default, shader)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(group : HitGroup, e : Expr<'a>) =
            x.AnyHit(group, Identifier.Default, e)

        [<CustomOperation("anyHit")>]
        member inline x.AnyHit(group : HitGroup, f : 'a -> Expr<'b>) =
            x.AnyHit(group, Identifier.Default, f)


        [<CustomOperation("closestHit")>]
        member x.ClosestHit(group : HitGroup, rayType : Symbol, shader : Shader) =
            x.SetShader(group, rayType, ShaderStage.ClosestHit, RaytracingShader.ofShader shader)

        [<CustomOperation("closestHit")>]
        member x.ClosestHit(group : HitGroup, rayType : Symbol, e : Expr<'a>) =
            x.SetShader(group, rayType, ShaderStage.ClosestHit, RaytracingShader.ofExpr [] e)

        [<CustomOperation("closestHit")>]
        member x.ClosestHit(group : HitGroup, rayType : Symbol, f : 'a -> Expr<'b>) =
            x.SetShader(group, rayType, ShaderStage.ClosestHit, RaytracingShader.ofFunction f)

        [<CustomOperation("closestHit")>]
        member x.ClosestHit(group : HitGroup, rayType : string, shader : Shader) =
            x.ClosestHit(group, Sym.ofString rayType, shader)

        [<CustomOperation("closestHit")>]
        member x.ClosestHit(group : HitGroup, rayType : string, e : Expr<'a>) =
           x.ClosestHit(group, Sym.ofString rayType, e)

        [<CustomOperation("closestHit")>]
        member x.ClosestHit(group : HitGroup, rayType : string, f : 'a -> Expr<'b>) =
            x.ClosestHit(group, Sym.ofString rayType, f)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member x.ClosestHit(group : HitGroup, (rayType, shader) : Symbol * Shader) =
            x.ClosestHit(group, rayType, shader)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(group : HitGroup, (rayType, e) : Symbol * Expr<'a>) =
            x.ClosestHit(group, rayType, e)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(group : HitGroup, (rayType, f) : Symbol * ('a -> Expr<'b>)) =
            x.ClosestHit(group, rayType, f)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(group : HitGroup, (rayType, shader) : string * Shader) =
            x.ClosestHit(group, rayType, shader)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(group : HitGroup, (rayType, e) : string * Expr<'a>) =
            x.ClosestHit(group, rayType, e)

        [<CustomOperation("closestHit"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.ClosestHit(group : HitGroup, (rayType, f) : string * ('a -> Expr<'b>)) =
            x.ClosestHit(group, rayType, f)

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(group : HitGroup, shader : Shader) =
            x.ClosestHit(group, Identifier.Default, shader)

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(group : HitGroup, e : Expr<'a>) =
            x.ClosestHit(group, Identifier.Default, e)

        [<CustomOperation("closestHit")>]
        member inline x.ClosestHit(group : HitGroup, f : 'a -> Expr<'b>) =
            x.ClosestHit(group, Identifier.Default, f)


        [<CustomOperation("intersection")>]
        member x.Intersection(group : HitGroup, rayType : Symbol, shader : Shader) =
            x.SetShader(group, rayType, ShaderStage.Intersection, RaytracingShader.ofShader shader)

        [<CustomOperation("intersection")>]
        member x.Intersection(group : HitGroup, rayType : Symbol, e : Expr<'a>) =
            x.SetShader(group, rayType, ShaderStage.Intersection, RaytracingShader.ofExpr [] e)

        [<CustomOperation("intersection")>]
        member x.Intersection(group : HitGroup, rayType : Symbol, f : 'a -> Expr<'b>) =
            x.SetShader(group, rayType, ShaderStage.Intersection, RaytracingShader.ofFunction f)

        [<CustomOperation("intersection")>]
        member x.Intersection(group : HitGroup, rayType : string, shader : Shader) =
            x.Intersection(group, Sym.ofString rayType, shader)

        [<CustomOperation("intersection")>]
        member x.Intersection(group : HitGroup, rayType : string, e : Expr<'a>) =
           x.Intersection(group, Sym.ofString rayType, e)

        [<CustomOperation("intersection")>]
        member x.Intersection(group : HitGroup, rayType : string, f : 'a -> Expr<'b>) =
            x.Intersection(group, Sym.ofString rayType, f)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member x.Intersection(group : HitGroup, (rayType, shader) : Symbol * Shader) =
            x.Intersection(group, rayType, shader)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(group : HitGroup, (rayType, e) : Symbol * Expr<'a>) =
            x.Intersection(group, rayType, e)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(group : HitGroup, (rayType, f) : Symbol * ('a -> Expr<'b>)) =
            x.Intersection(group, rayType, f)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(group : HitGroup, (rayType, shader) : string * Shader) =
            x.Intersection(group, rayType, shader)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(group : HitGroup, (rayType, e) : string * Expr<'a>) =
            x.Intersection(group, rayType, e)

        [<CustomOperation("intersection"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Intersection(group : HitGroup, (rayType, f) : string * ('a -> Expr<'b>)) =
            x.Intersection(group, rayType, f)

        [<CustomOperation("intersection")>]
        member inline x.Intersection(group : HitGroup, shader : Shader) =
            x.Intersection(group, Identifier.Default, shader)

        [<CustomOperation("intersection")>]
        member inline x.Intersection(group : HitGroup, e : Expr<'a>) =
            x.Intersection(group, Identifier.Default, e)

        [<CustomOperation("intersection")>]
        member inline x.Intersection(group : HitGroup, f : 'a -> Expr<'b>) =
            x.Intersection(group, Identifier.Default, f)


    type RayGenerationShaderMustBeSpecified = RayGenerationShaderMustBeSpecified

    type RaytracingEffectBuilder() =
        do Serializer.Init()

        member inline x.Yield(_) = RayGenerationShaderMustBeSpecified

        member inline x.Delay f = f()

        member x.Run(shaders : Map<ShaderSlot, RaytracingShader>) =
            RaytracingEffect(shaders)


        member private x.SetRaygen(shader : RaytracingShader) =
            if shader.Stage <> ShaderStage.RayGeneration then
                failwithf "[FShade] Expected ray generation shader but got %A shader." shader.Stage

            Map.ofList [ ShaderSlot.RayGeneration, shader ]

        member private x.SetShader(shaders : Map<ShaderSlot, RaytracingShader>, slot : ShaderSlot, shader : RaytracingShader) =
            let stage = slot.Stage

            if shader.Stage <> stage then
                failwithf "[FShade] Expected %A shader but got %A shader." stage shader.Stage

            shaders |> Map.add slot shader

        [<CustomOperation("raygen")>]
        member x.Raygen(_ : RayGenerationShaderMustBeSpecified, shader : Shader) =
            x.SetRaygen(RaytracingShader.ofShader shader)

        [<CustomOperation("raygen")>]
        member x.Raygen(_ : RayGenerationShaderMustBeSpecified, e : Expr<'a>) =
            x.SetRaygen(RaytracingShader.ofExpr [] e)

        [<CustomOperation("raygen")>]
        member x.Raygen(_ : RayGenerationShaderMustBeSpecified, f : 'a -> Expr<'b>) =
            x.SetRaygen(RaytracingShader.ofFunction f)


        [<CustomOperation("miss")>]
        member x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, name : Symbol, shader : Shader) =
            x.SetShader(shaders, ShaderSlot.Miss name, RaytracingShader.ofShader shader)

        [<CustomOperation("miss")>]
        member x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, name : Symbol, e : Expr<'a>) =
            x.SetShader(shaders, ShaderSlot.Miss name, RaytracingShader.ofExpr [] e)

        [<CustomOperation("miss")>]
        member x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, name : Symbol, f : 'a -> Expr<'b>) =
            x.SetShader(shaders, ShaderSlot.Miss name, RaytracingShader.ofFunction f)

        [<CustomOperation("miss")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, name : string, shader : Shader) =
            x.Miss(shaders, Sym.ofString name, shader)

        [<CustomOperation("miss")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, name : string, e : Expr<'a>) =
            x.Miss(shaders, Sym.ofString name, e)

        [<CustomOperation("miss")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, name : string, f : 'a -> Expr<'b>) =
            x.Miss(shaders, Sym.ofString name, f)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, (name, shader) : Symbol * Shader) =
            x.Miss(shaders, name, shader)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, (name, e) : Symbol * Expr<'a>) =
            x.Miss(shaders, name, e)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, (name, f) : Symbol * ('a -> Expr<'b>)) =
            x.Miss(shaders, name, f)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, (name, shader) : string * Shader) =
           x.Miss(shaders, name, shader)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, (name, e) : string * Expr<'a>) =
            x.Miss(shaders, name, e)

        [<CustomOperation("miss"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, (name, f) : string * ('a -> Expr<'b>)) =
            x.Miss(shaders, name, f)

        [<CustomOperation("miss")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, shader : Shader) =
            x.Miss(shaders, Identifier.Default, shader)

        [<CustomOperation("miss")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, e : Expr<'a>) =
            x.Miss(shaders, Identifier.Default, e)

        [<CustomOperation("miss")>]
        member inline x.Miss(shaders : Map<ShaderSlot, RaytracingShader>, f : 'a -> Expr<'b>) =
            x.Miss(shaders, Identifier.Default, f)


        [<CustomOperation("callable")>]
        member x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, name : Symbol, shader : Shader) =
            x.SetShader(shaders, ShaderSlot.Callable name, RaytracingShader.ofShader shader)

        [<CustomOperation("callable")>]
        member x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, name : Symbol, e : Expr<'a>) =
            x.SetShader(shaders, ShaderSlot.Callable name, RaytracingShader.ofExpr [] e)

        [<CustomOperation("callable")>]
        member x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, name : Symbol, f : 'a -> Expr<'b>) =
            x.SetShader(shaders, ShaderSlot.Callable name, RaytracingShader.ofFunction f)

        [<CustomOperation("callable")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, name : string, shader : Shader) =
            x.Callable(shaders, Sym.ofString name, shader)

        [<CustomOperation("callable")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, name : string, e : Expr<'a>) =
            x.Callable(shaders, Sym.ofString name, e)

        [<CustomOperation("callable")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, name : string , f : 'a -> Expr<'b>) =
            x.Callable(shaders, Sym.ofString name, f)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, (name, shader) : Symbol * Shader) =
            x.Callable(shaders, name, shader)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, (name, e) : Symbol * Expr<'a>) =
            x.Callable(shaders, name, e)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, (name, f) : Symbol * ('a -> Expr<'b>)) =
            x.Callable(shaders, name, f)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, (name, shader) : string * Shader) =
            x.Callable(shaders, name, shader)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, (name, e) : string * Expr<'a>) =
            x.Callable(shaders, name, e)

        [<CustomOperation("callable"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, (name, f) : string * ('a -> Expr<'b>)) =
            x.Callable(shaders, name, f)

        [<CustomOperation("callable")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, shader : Shader) =
            x.Callable(shaders, Identifier.Default, shader)

        [<CustomOperation("callable")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, e : Expr<'a>) =
            x.Callable(shaders, Identifier.Default, e)

        [<CustomOperation("callable")>]
        member inline x.Callable(shaders : Map<ShaderSlot, RaytracingShader>, f : 'a -> Expr<'b>) =
            x.Callable(shaders, Identifier.Default, f)


        [<CustomOperation("hitgroup")>]
        member x.HitGroup(shaders : Map<ShaderSlot, RaytracingShader>, name : Symbol, group : HitGroup) =
            let mutable shaders = shaders

            for KeyValue(ray, shader) in group.AnyHit do
                shaders <- shaders |> Map.add (ShaderSlot.AnyHit (name, ray)) shader

            for KeyValue(ray, shader) in group.ClosestHit do
                shaders <- shaders |> Map.add (ShaderSlot.ClosestHit (name, ray)) shader

            for KeyValue(ray, shader) in group.Intersection do
                shaders <- shaders |> Map.add (ShaderSlot.Intersection (name, ray)) shader

            shaders

        [<CustomOperation("hitgroup")>]
        member inline x.HitGroup(shaders : Map<ShaderSlot, RaytracingShader>, name : string, group : HitGroup) =
            x.HitGroup(shaders, Sym.ofString name, group)

        [<CustomOperation("hitgroup"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.HitGroup(shaders : Map<ShaderSlot, RaytracingShader>, (name, group) : Symbol * HitGroup) =
            x.HitGroup(shaders, name, group)

        [<CustomOperation("hitgroup"); Obsolete("Use overload with untupled arguments.")>]
        member inline x.HitGroup(shaders : Map<ShaderSlot, RaytracingShader>, (name, group) : string * HitGroup) =
            x.HitGroup(shaders, name, group)


    let hitgroup = HitGroupBuilder()
    let raytracingEffect = RaytracingEffectBuilder()