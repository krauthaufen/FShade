namespace FShade

open System
open System.IO
open System.Reflection
open System.Security.Cryptography
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Aardvark.Base
open FShade.Imperative

/// Ahead-of-time support API used by fshadeaot-rewritten shader functions.
/// Public so the rewrite can target it; users normally do not call this directly.
module Aot =

    /// Counter incremented every time a marker (deferred or precomputed) is called —
    /// useful for tests to verify AOT path. markerInvocations counts deferred markers,
    /// markerPrecomputedInvocations counts precomputed (cold-fast) markers.
    let mutable markerInvocations : int = 0
    let mutable markerPrecomputedInvocations : int = 0

    /// Computes the CANONICAL Effect id for a shader function body. Mirrors what
    /// `Effect.ofExpr`/`Effect.ofFunction` produces — includes input/output semantics
    /// (`[<Semantic>]`, `[<Interpolation>]`, `[<DepthWrite>]` attrs from the I/O records).
    /// Two shaders with same body but different I/O attributes get distinct ids.
    let hashBody (inputType : Type) (body : Expr) : string =
        Serializer.Init()
        Effect.computeEffectIdFromBody inputType body

    /// Walk an Expr; replace any ValueWithName(_, t, name) where (name, t) appears in
    /// `carriers` with a fresh Var. Used by fshadeaot at build-time so the body hash
    /// is stable regardless of the placeholder values used to produce the Expr.
    let normalizeCarriers (carriers : (string * Type)[]) (e : Expr) : Expr =
        let map =
            carriers
            |> Array.map (fun (n, t) -> n, Var("$p_" + n, t))
            |> Map.ofArray
        let rec go (e : Expr) =
            match e with
            | Patterns.ValueWithName(_, t, n) ->
                match Map.tryFind n map with
                | Some v when v.Type = t -> Expr.Var v
                | _ -> e
            | ExprShape.ShapeVar _ -> e
            | ExprShape.ShapeLambda(v, b) -> Expr.Lambda(v, go b)
            | ExprShape.ShapeCombination(o, args) ->
                ExprShape.RebuildShapeCombination(o, args |> List.map go)
        go e

    /// Convenience for fshadeaot: takes the Expr boxed as obj (cross-ALC safe).
    /// Normalizes and hashes in one shot.
    let normalizeAndHash (carriers : (string * Type)[]) (inputType : Type) (rawExpr : obj) : string =
        let e =
            match rawExpr with
            | :? Expr as e -> e
            | _ -> failwithf "normalizeAndHash: expected Expr, got %s" (rawExpr.GetType().FullName)
        hashBody inputType (normalizeCarriers carriers e)

    /// Strips outer WithValue wrappers from an Expr — used when the user invoked the
    /// shader function during build to recover the raw body before serialization.
    let private peelWithValue (e : Expr) : Expr =
        match e with
        | Patterns.WithValue(_, _, b) -> b
        | _ -> e

    /// Build-time helper for fshadeaot: reflectively invoke a shader function with
    /// placeholder args, then run the full FShade preprocess pipeline (Shader.ofExpr)
    /// to produce the final Map<ShaderStage, Shader>. Returns (id, serializedBlob).
    /// Used for ZERO-arg shaders so the runtime cold path skips the optimizer.
    let precomputeShader (inputType : Type) (rawExpr : obj) : string * byte[] =
        Serializer.Init()
        let body = peelWithValue (rawExpr :?> Expr)
        let id = Effect.computeEffectIdFromBody inputType body
        let shaders =
            Shader.ofExpr [inputType] body
            |> List.map (fun s -> s.shaderStage, s)
            |> Map.ofList
        let effect = Effect(id, lazy shaders, [])
        use ms = new MemoryStream()
        Effect.serialize ms effect
        id, ms.ToArray()

    /// Computes the runtime id of a deferred marker call: SHA1(serialize(args...) || bodyHash).
    /// Different from hashBody because here we only need stability across runs of the same args,
    /// not equivalence with the canonical Effect id.
    let computeId (bodyHash : string) (args : obj[]) : string =
        Serializer.Init()
        use sha = SHA1.Create()
        use ms  = new MemoryStream()
        use cs  = new CryptoStream(ms, sha, CryptoStreamMode.Write)
        use bw  = new BinaryWriter(cs, System.Text.Encoding.UTF8, true)
        let state = Expr.SerializerState(true)
        bw.Write (args.Length)
        for a in args do
            let t =
                if isNull a then typeof<obj>
                else a.GetType()
            Expr.serializeInternal state bw (Expr.Value(a, t))
        bw.Write bodyHash
        cs.FlushFinalBlock()
        sha.Hash |> Convert.ToBase64String

    /// Builds the precomputed Effect for a marker call without forcing the lazy body.
    /// Shaders are produced lazily by invoking lazyBody on first access.
    let private buildEffect (inputType : Type) (id : string) (lazyBody : Lazy<Expr>) : Effect =
        Effect.effectCache.GetOrAdd(id, fun id ->
            let shaders =
                lazy (
                    let body = peelWithValue lazyBody.Value
                    Shader.ofExpr [inputType] body
                    |> List.map (fun s -> s.shaderStage, s)
                    |> Map.ofList
                )
            Effect(id, shaders, [])
        )

    /// Marker for shaders WITH runtime args. Returns an Expr that Effect.ofExpr /
    /// Effect.ofFunction recognize via WithValue and short-circuit on, without
    /// forcing lazyBody (unless the backend cache misses and asks for .Shaders,
    /// at which point the original shader function runs and Shader.ofExpr fires).
    let marker
        (inputType  : Type)
        (returnType : Type)
        (bodyHash   : string)
        (args       : obj[])
        (lazyBody   : Lazy<Expr>) : Expr =
        System.Threading.Interlocked.Increment(&markerInvocations) |> ignore
        let id = computeId bodyHash args
        let effect = buildEffect inputType id lazyBody
        let dummyDef = Expr.Value(null, returnType)
        Expr.WithValue(box effect, returnType, dummyDef)

    /// Marker for ZERO-arg shaders (no runtime variability). The Effect is fully
    /// determined at build time, so fshadeaot serializes the precomputed
    /// Map<ShaderStage, Shader> as an assembly resource and we deserialize it
    /// directly — skipping Shader.ofExpr / the optimizer entirely on cold start.
    let markerPrecomputed
        (returnType    : Type)
        (id            : string)
        (assemblyName  : string)
        (resourceName  : string) : Expr =
        System.Threading.Interlocked.Increment(&markerPrecomputedInvocations) |> ignore
        let effect = Effect.effectCache.GetOrAdd(id, fun _ ->
            let shaders =
                lazy (
                    let asm = Assembly.Load(assemblyName)
                    use stream = asm.GetManifestResourceStream(resourceName)
                    if isNull stream then
                        failwithf "[FShade.Aot] missing resource %s in %s" resourceName assemblyName
                    use br = new BinaryReader(stream, System.Text.Encoding.UTF8, true)
                    // serialized format starts with the effect id, then shader count, then shaders.
                    let _id = br.ReadString()
                    let cnt = br.ReadInt32()
                    let state = Shader.DeserializerState()
                    List.init cnt (fun _ ->
                        let stage = br.ReadInt32() |> unbox<ShaderStage>
                        let shader = Shader.deserializeInternal state br
                        stage, shader)
                    |> Map.ofList
                )
            Effect(id, shaders, []))
        let dummyDef = Expr.Value(null, returnType)
        Expr.WithValue(box effect, returnType, dummyDef)
