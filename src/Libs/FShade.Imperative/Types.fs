namespace FShade

open Aardvark.Base

type DepthWriteMode =
    | None          = 0
    | Any           = 1
    | Equal         = 2
    | OnlyGreater   = 3
    | OnlyLess      = 4

type InterpolationMode =
    | Default           = 0
    | Perspective       = 1
    | NoPerspective     = 2
    | Flat              = 3
    | Centroid          = 4
    | Sample            = 5
    | PerPatch          = 6

type MemoryType =
    | None = 0
    | Global = 1
    | Local = 2

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
