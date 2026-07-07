namespace FShade

open System
open Aardvark.Base

type DepthWriteMode =
    | None          = 0
    | Any           = 1
    | Equal         = 2
    | OnlyGreater   = 3
    | OnlyLess      = 4

[<Flags>]
type InterpolationMode =
    | Default           = 0
    | NoPerspective     = 1
    | Flat              = 2
    | Centroid          = 4
    | Sample            = 8
    | PerPatch          = 16

type MemoryType =
    | None = 0
    | Global = 1
    | Local = 2

/// <summary>
/// Bitmask flags that control ray traversal, hit reporting and shader invocation.
/// These mirror the commonly used ray flags in Vulkan (KHR) and DirectX Raytracing (DXR)
/// and are suppliedwhen invoking Scene.TraceRay or HitObject.TraceRay.
/// </summary>
[<Flags>]
type RayFlags =
    /// No special behavior; default traversal and shading.
    | None                          = 0

    /// <summary>
    /// Treat all geometry as opaque regardless of build-time opacity state.
    /// Often used to bypass any-hit shaders and alpha-testing for faster traversal when transparency is not needed.
    /// </summary>
    /// <remarks>
    /// Mutually exclusive with NoOpaque, CullOpaque, and CullNoOpaque.
    /// </remarks>
    | Opaque                        = 1

    /// <summary>
    /// Treat all geometry as non-opaque regardless of build-time opacity state.
    /// </summary>
    /// <remarks>
    /// Mutually exclusive with Opaque, CullOpaque, and CullNoOpaque.
    /// </remarks>
    | NoOpaque                      = 2

    /// <summary>
    /// Do not sort hit candidates to determine the closest intersection; traversal may terminate
    /// as soon as any hit is found and the first reported intersection is used as the final hit.
    /// </summary>
    | TerminateOnFirstHit           = 4

    /// Do not execute the closest-hit shader even if a closest intersection is found.
    | SkipClosestHitShader          = 8

    /// <summary>
    /// Ignore intersections with back-facing triangles.
    /// </summary>
    /// <remarks>
    /// Mutually exclusive with CullFrontFacingTriangles.
    /// </remarks>
    | CullBackFacingTriangles       = 16

    /// <summary>
    /// Ignore intersections with front-facing triangles.
    /// </summary>
    /// <remarks>
    /// Mutually exclusive with CullBackFacingTriangles.
    /// </remarks>
    | CullFrontFacingTriangles      = 32

    /// <summary>
    /// Ignore intersections with geometry that is considered opaque.
    /// </summary>
    /// <remarks>
    /// Mutually exclusive with Opaque, NoOpaque, and CullNoOpaque.
    /// </remarks>
    | CullOpaque                    = 64

    /// <summary>
    /// Ignore intersections with geometry that is considered non-opaque.
    /// </summary>
    /// <remarks>
    /// Mutually exclusive with Opaque, NoOpaque, and CullOpaque.
    /// </remarks>
    | CullNoOpaque                  = 128

    /// <summary>
    /// Force opacity micromaps intersected by this ray to be evaluated in two-state mode.
    /// </summary>
    /// <remarks>
    /// Requires GL_EXT_opacity_micromap.
    /// </remarks>
    | ForceOpacityMicromapsTwoState = 1024

type RayHitKind =
    | Default             = 0

    /// Intersection is with a front-facing triangle.
    | FrontFacingTriangle = 0xFE

    /// Intersection is with a back-facing triangle.
    | BackFacingTriangle  = 0xFF

type IRaytracingId =
    abstract member Index : int

type ShaderStage =
    | Vertex        = 0
    | TessControl   = 1
    | TessEval      = 2
    | Geometry      = 3
    | Fragment      = 4
    | Compute       = -1
    | RayGeneration = -2
    | Intersection  = -3
    | AnyHit        = -4
    | ClosestHit    = -5
    | Miss          = -6
    | Callable      = -7
    // Task < Mesh so map-/composition-ordering matches the pipeline order
    | Task          = -9
    | Mesh          = -8

module ShaderStage =
    let isCompute (stage : ShaderStage) =
        stage = ShaderStage.Compute

    let isTask (stage : ShaderStage) =
        stage = ShaderStage.Task

    let isMesh (stage : ShaderStage) =
        stage = ShaderStage.Mesh

    /// stages executing as workgroups (compute-like intrinsics apply)
    let isWorkgroup (stage : ShaderStage) =
        match stage with
        | ShaderStage.Compute | ShaderStage.Task | ShaderStage.Mesh -> true
        | _ -> false

    let isRaytracing (stage : ShaderStage) =
        match stage with
        | ShaderStage.RayGeneration
        | ShaderStage.Intersection
        | ShaderStage.AnyHit
        | ShaderStage.ClosestHit
        | ShaderStage.Miss
        | ShaderStage.Callable ->
            true
        | _ ->
            false

    let supportsTraceRay = function
        | ShaderStage.RayGeneration | ShaderStage.ClosestHit | ShaderStage.Miss -> true
        | _ -> false

    let supportsExecuteCallable = function
        | ShaderStage.RayGeneration | ShaderStage.ClosestHit | ShaderStage.Miss | ShaderStage.Callable -> true
        | _ -> false

    let supportsPayloadIn = function
        | ShaderStage.AnyHit | ShaderStage.ClosestHit | ShaderStage.Miss -> true
        | _ -> false

    let supportsHitAttributes = function
        | ShaderStage.Intersection | ShaderStage.AnyHit | ShaderStage.ClosestHit -> true
        | _ -> false

    let prefix =
        LookupTable.lookup [
            ShaderStage.Vertex,         "vs"
            ShaderStage.TessControl,    "tc"
            ShaderStage.TessEval,       "te"
            ShaderStage.Geometry,       "gs"
            ShaderStage.Fragment,       "fs"
            ShaderStage.Compute,        "cs"
            ShaderStage.RayGeneration,  "rgen"
            ShaderStage.Intersection,   "rint"
            ShaderStage.AnyHit,         "rahit"
            ShaderStage.ClosestHit,     "rchit"
            ShaderStage.Miss,           "rmiss"
            ShaderStage.Callable,       "rcall"
            ShaderStage.Task,           "ts"
            ShaderStage.Mesh,           "ms"
        ]

[<RequireQualifiedAccess>]
type ShaderSlot =
    | Vertex
    | TessControl
    | TessEval
    | Geometry
    | Fragment
    | Compute
    | Task
    | Mesh
    | RayGeneration
    | Miss          of name: Symbol
    | Callable      of name: Symbol
    | AnyHit        of name: Symbol * rayType: Symbol
    | ClosestHit    of name: Symbol * rayType: Symbol
    | Intersection  of name: Symbol * rayType: Symbol

    member x.Stage =
        match x with
        | Vertex         -> ShaderStage.Vertex
        | TessControl    -> ShaderStage.TessControl
        | TessEval       -> ShaderStage.TessEval
        | Geometry       -> ShaderStage.Geometry
        | Fragment       -> ShaderStage.Fragment
        | Compute        -> ShaderStage.Compute
        | Task           -> ShaderStage.Task
        | Mesh           -> ShaderStage.Mesh
        | RayGeneration  -> ShaderStage.RayGeneration
        | Miss _         -> ShaderStage.Miss
        | Callable _     -> ShaderStage.Callable
        | AnyHit _       -> ShaderStage.AnyHit
        | ClosestHit _   -> ShaderStage.ClosestHit
        | Intersection _ -> ShaderStage.Intersection

    member x.Conditional =
        match x with
        | Miss n | Callable n -> $"{x.Stage}_{n}"
        | AnyHit (n, r) | ClosestHit (n, r) | Intersection (n, r) -> $"{x.Stage}_{n}_{r}"
        | _ -> $"{x.Stage}"

[<RequireQualifiedAccess>]
type OutputTopology = 
    | Points 
    | LineStrip 
    | TriangleStrip

[<RequireQualifiedAccess>]
type InputTopology = 
    | Point 
    | Line 
    | LineAdjacency 
    | Triangle 
    | TriangleAdjacency 
    | Patch of int

type StorageAccess =
    | None          = 0x00
    | Read          = 0x01
    | Write         = 0x02
    | ReadWrite     = 0x03
    | Atomic        = 0x04

    
type Preprocessor private() =
    [<Obsolete("Use 'unroll' instead. E.g. for i in unroll 0 .. 5 do")>]
    static member unroll() = ()

[<AutoOpen>]
module Operators = 
    let (~&&) (v : 'a) = ref v
