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

[<Flags>]
type RayFlags =
    | None                     = 0
    | Opaque                   = 1
    | NoOpaque                 = 2
    | TerminateOnFirstHit      = 4
    | SkipClosestHitShader     = 8
    | CullBackFacingTriangles  = 16
    | CullFrontFacingTriangles = 32
    | CullOpaque               = 64
    | CullNoOpaque             = 128

type RayHitKind =
    | Default             = 0
    | FrontFacingTriangle = 0xFE
    | BackFacingTriangle  = 0xFF

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

module ShaderStage =
    let isCompute (stage : ShaderStage) =
        stage = ShaderStage.Compute

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
        LookupTable.lookupTable [
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
        ]

[<RequireQualifiedAccess>]
type ShaderSlot =
    | Vertex
    | TessControl
    | TessEval
    | Geometry
    | Fragment
    | Compute
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

    
type Preprocessor private() =
    static member unroll() = ()

[<AutoOpen>]
module Operators = 
    let (~&&) (v : 'a) = ref v
