namespace FShade

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

type ShaderStage = 
    | Vertex  = 0
    | TessControl = 1
    | TessEval = 2
    | Geometry = 3 
    | Fragment = 4
    | Compute = -1

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
