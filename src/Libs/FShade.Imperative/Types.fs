namespace FShade

type InterpolationMode =
    | Default           = 0
    | Perspective       = 1
    | NoPerspective     = 2
    | Flat              = 3
    | Centroid          = 4
    | Sample            = 5

type MemoryType =
    | None = 0
    | Global = 1
    | Local = 2

type ShaderType = 
    | Vertex  = 0
    | Geometry = 1 
    | TessControl = 2
    | TessEval = 3
    | Fragment = 4
    | Compute = 5 

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