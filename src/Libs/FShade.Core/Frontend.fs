namespace FShade

open System
open System.Diagnostics
open System.Reflection

open Aardvark.Base

open FShade

type InlineAttribute = FShade.Imperative.InlineAttribute
type KeepCallAttribute = FShade.Imperative.KeepCallAttribute

[<AutoOpen>]
module Primitives =
    type Primitive<'a> = interface end

    type Point<'a> = Point of 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(0)>]
        member x.Value : 'a = onlyInShaderCode "read"

        static member VertexCount = 1
        static member InputTopology = InputTopology.Point

    type Line<'a> = Line of 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = onlyInShaderCode "read"

        static member VertexCount = 2
        static member InputTopology = InputTopology.Line
        member x.Interpolate(coord : float32) : 'v = onlyInShaderCode "read"

    type Triangle<'a> = Triangle of 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = onlyInShaderCode "read"

        static member VertexCount = 3
        static member InputTopology = InputTopology.Triangle
        member x.Interpolate(coord : V3f) : 'v = onlyInShaderCode "read"

    type LineAdjacency<'a> = Triangle of 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"
            
            
        [<PrimitiveIndex(1)>]
        member x.P0 : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(2)>]
        member x.P1 : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(0)>]
        member x.Left : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(3)>]
        member x.Right : 'a = onlyInShaderCode "read"

        static member VertexCount = 4
        static member InputTopology = InputTopology.LineAdjacency

    type TriangleAdjacency<'a> = Triangle of 'a * 'a * 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(2)>]
        member x.P1 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(4)>]
        member x.P2 : 'a = onlyInShaderCode "read"
        
        [<PrimitiveIndex(1)>]
        member x.N01 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(3)>]
        member x.N12 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(5)>]
        member x.N20 : 'a = onlyInShaderCode "read"

        static member VertexCount = 6
        static member InputTopology = InputTopology.TriangleAdjacency

    type Patch3<'a> = Patch3 of 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"

        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = onlyInShaderCode "read"


        member x.TessCoord : V3f = onlyInShaderCode "read"

        static member VertexCount = 3
        static member InputTopology = InputTopology.Patch 3
        member x.Interpolate(coord : V3f) : 'v = onlyInShaderCode "read"

    type Patch4<'a> = Patch4 of 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"


        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = onlyInShaderCode "read"
        [<PrimitiveIndex(3)>]
        member x.P3 : 'a = onlyInShaderCode "read"

        member x.TessCoord : V3f = onlyInShaderCode "read"

        static member VertexCount = 4
        static member InputTopology = InputTopology.Patch 4
        member x.Interpolate(coord : V2f) : 'v = onlyInShaderCode "read"

    type Patch<'d, 'a when 'd :> INatural>() =
        static let dim = Peano.getSize typeof<'d>

        interface Primitive<'a>

        member x.InvocationId : int = onlyInShaderCode "read"

        member x.Item
            with get (i : int) : 'a = onlyInShaderCode "read"
            
        static member VertexCount = dim
        static member InputTopology = InputTopology.Patch dim
        

    let emitVertex() = ()

    let endPrimitive() = ()
    let restartStrip() = ()

    let private emitVertexMeth = getMethodInfo <@ emitVertex() @>
    let private restartStripMeth = getMethodInfo <@ restartStrip() @>
    let private endPrimitiveMeth = getMethodInfo <@ endPrimitive() @>

    [<return: Struct>]
    let (|EmitVertex|_|) (e : Microsoft.FSharp.Quotations.Expr) =
        match e with
        | Microsoft.FSharp.Quotations.Patterns.Call(None, mi, []) when mi = emitVertexMeth ->
            ValueSome ()
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|RestartStrip|_|) (e : Microsoft.FSharp.Quotations.Expr) =
        match e with
        | Microsoft.FSharp.Quotations.Patterns.Call(None, mi, []) when mi = restartStripMeth || mi = endPrimitiveMeth ->
            ValueSome ()
        | _ ->
            ValueNone



    let inline ddx< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ddx"
    let inline ddy< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ddy"
    let inline ddxFine< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ddxFine"
    let inline ddyFine< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ddyFine"
    let inline ddxCoarse< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ddxCoarse"
    let inline ddyCoarse< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = onlyInShaderCode "ffyCoarse"
    let discard () : unit = onlyInShaderCode "discard"

    /// <summary>
    /// Signals to the optimizer that the given for-loop should be unrolled if possible.
    /// </summary>
    /// <example>
    /// Usage:
    /// <code>for i = unroll 0 to 5 do</code> or <code>for i in unroll 0 .. 5 do</code>
    /// </example>
    let unroll (loop: 'T) : 'T = onlyInShaderCode "unroll"

    // The pack/unpack functions are GLSL intrinsics in shaders. The bodies below are only used on the
    // CPU and follow the GLSL specification: the first component is stored in the least significant bits,
    // unorm values are scaled by 65535 / 255, snorm values by 32767 / 127 as two's complement integers.
    let inline private packUnorm16 (x : float32) = (clamp 0.0f 1.0f x) * 65535.0f |> round |> uint32
    let inline private packSnorm16 (x : float32) = uint32 ((clamp -1.0f 1.0f x) * 32767.0f |> round |> int32) &&& 0xFFFFu
    let inline private packUnorm8 (x : float32) = (clamp 0.0f 1.0f x) * 255.0f |> round |> uint32
    let inline private packSnorm8 (x : float32) = uint32 ((clamp -1.0f 1.0f x) * 127.0f |> round |> int32) &&& 0xFFu

    let inline private unpackUnorm16 (v : uint32) = float32 (v &&& 0xFFFFu) / 65535.0f
    let inline private unpackSnorm16 (v : uint32) = clamp -1.0f 1.0f (float32 (int16 (v &&& 0xFFFFu)) / 32767.0f)
    let inline private unpackUnorm8 (v : uint32) = float32 (v &&& 0xFFu) / 255.0f
    let inline private unpackSnorm8 (v : uint32) = clamp -1.0f 1.0f (float32 (sbyte (v &&& 0xFFu)) / 127.0f)

    /// <summary>Packs two normalized floats in [0, 1] into 16 bits each (x in the least significant bits).</summary>
    /// <remarks>Can be used on the host and produces the same result as the GLSL function
    /// (for values that scale to an exact .5 the rounding direction is implementation-defined in GLSL).</remarks>
    let packUnorm2x16 (v : V2f) : uint32 =
        packUnorm16 v.X ||| (packUnorm16 v.Y <<< 16)

    /// <summary>Packs two normalized floats in [-1, 1] into 16 bits each (x in the least significant bits).</summary>
    /// <remarks>Can be used on the host and produces the same result as the GLSL function
    /// (for values that scale to an exact .5 the rounding direction is implementation-defined in GLSL).</remarks>
    let packSnorm2x16 (v : V2f) : uint32 =
        packSnorm16 v.X ||| (packSnorm16 v.Y <<< 16)

    /// <summary>Packs four normalized floats in [0, 1] into 8 bits each (x in the least significant bits).</summary>
    /// <remarks>Can be used on the host and produces the same result as the GLSL function
    /// (for values that scale to an exact .5 the rounding direction is implementation-defined in GLSL).</remarks>
    let packUnorm4x8 (v : V4f) : uint32 =
        packUnorm8 v.X ||| (packUnorm8 v.Y <<< 8) ||| (packUnorm8 v.Z <<< 16) ||| (packUnorm8 v.W <<< 24)

    /// <summary>Packs four normalized floats in [-1, 1] into 8 bits each (x in the least significant bits).</summary>
    /// <remarks>Can be used on the host and produces the same result as the GLSL function
    /// (for values that scale to an exact .5 the rounding direction is implementation-defined in GLSL).</remarks>
    let packSnorm4x8 (v : V4f) : uint32 =
        packSnorm8 v.X ||| (packSnorm8 v.Y <<< 8) ||| (packSnorm8 v.Z <<< 16) ||| (packSnorm8 v.W <<< 24)

    /// <summary>Unpacks two normalized floats in [0, 1] from 16 bits each (inverse of packUnorm2x16).</summary>
    /// <remarks>Can be used on the host and produces the same result as the GLSL function
    /// (for values that scale to an exact .5 the rounding direction is implementation-defined in GLSL).</remarks>
    let unpackUnorm2x16 (v : uint32) : V2f =
        V2f(unpackUnorm16 v, unpackUnorm16 (v >>> 16))

    /// <summary>Unpacks two normalized floats in [-1, 1] from 16 bits each (inverse of packSnorm2x16).</summary>
    /// <remarks>Can be used on the host and produces the same result as the GLSL function
    /// (for values that scale to an exact .5 the rounding direction is implementation-defined in GLSL).</remarks>
    let unpackSnorm2x16 (v : uint32) : V2f =
        V2f(unpackSnorm16 v, unpackSnorm16 (v >>> 16))

    /// <summary>Unpacks four normalized floats in [0, 1] from 8 bits each (inverse of packUnorm4x8).</summary>
    /// <remarks>Can be used on the host and produces the same result as the GLSL function
    /// (for values that scale to an exact .5 the rounding direction is implementation-defined in GLSL).</remarks>
    let unpackUnorm4x8 (v : uint32) : V4f =
        V4f(unpackUnorm8 v, unpackUnorm8 (v >>> 8), unpackUnorm8 (v >>> 16), unpackUnorm8 (v >>> 24))

    /// <summary>Unpacks four normalized floats in [-1, 1] from 8 bits each (inverse of packSnorm4x8).</summary>
    /// <remarks>Can be used on the host and produces the same result as the GLSL function
    /// (for values that scale to an exact .5 the rounding direction is implementation-defined in GLSL).</remarks>
    let unpackSnorm4x8 (v : uint32) : V4f =
        V4f(unpackSnorm8 v, unpackSnorm8 (v >>> 8), unpackSnorm8 (v >>> 16), unpackSnorm8 (v >>> 24))

    [<AbstractClass; Sealed>]
    type Atomic =
        static member Add(mem: int ref, data: int) : int    = onlyInShaderCode "Atomic.Add"
        static member Add(mem: uint ref, data: uint) : uint = onlyInShaderCode "Atomic.Add"

        static member Min(mem: int ref, data: int) : int    = onlyInShaderCode "Atomic.Min"
        static member Min(mem: uint ref, data: uint) : uint = onlyInShaderCode "Atomic.Min"

        static member Max(mem: int ref, data: int) : int    = onlyInShaderCode "Atomic.Max"
        static member Max(mem: uint ref, data: uint) : uint = onlyInShaderCode "Atomic.Max"

        static member And(mem: int ref, data: int) : int    = onlyInShaderCode "Atomic.And"
        static member And(mem: uint ref, data: uint) : uint = onlyInShaderCode "Atomic.And"

        static member Or(mem: int ref, data: int) : int     = onlyInShaderCode "Atomic.Or"
        static member Or(mem: uint ref, data: uint) : uint  = onlyInShaderCode "Atomic.Or"

        static member Xor(mem: int ref, data: int) : int    = onlyInShaderCode "Atomic.Xor"
        static member Xor(mem: uint ref, data: uint) : uint = onlyInShaderCode "Atomic.Xor"

        static member Exchange(mem: int ref, data: int) : int    = onlyInShaderCode "Atomic.Exchange"
        static member Exchange(mem: uint ref, data: uint) : uint = onlyInShaderCode "Atomic.Exchange"

        static member CompareExchange(mem: int ref, compare: int, data: int) : int    = onlyInShaderCode "Atomic.CompareExchange"
        static member CompareExchange(mem: uint ref, compare: uint, data: uint) : uint = onlyInShaderCode "Atomic.CompareExchange"

    [<AbstractClass; Sealed>]
    type Bitwise private() = 
        static member BitFieldExtract (v : uint32, offset : int, bits : int) : uint32 = onlyInShaderCode "BitFieldExtract"
        static member BitFieldExtract (v : int, offset : int, bits : int) : int = onlyInShaderCode "BitFieldExtract"
        static member BitFieldExtract (v : uint64, offset : int, bits : int) : uint64 = onlyInShaderCode "BitFieldExtract"
        static member BitFieldExtract (v : int64, offset : int, bits : int) : int64 = onlyInShaderCode "BitFieldExtract"

        static member BitFieldInsert (v : uint32, insert : uint32, offset : int, bits : int) : uint32 = onlyInShaderCode "BitFieldInsert"
        static member BitFieldInsert (v : int, insert : int, offset : int, bits : int) : int = onlyInShaderCode "BitFieldInsert"
        static member BitFieldInsert (v : uint64, insert : uint64, offset : int, bits : int) : uint64 = onlyInShaderCode "BitFieldInsert"
        static member BitFieldInsert (v : int64, insert : int64, offset : int, bits : int) : int64 = onlyInShaderCode "BitFieldInsert"

        static member BitFieldReverse (v : uint32) : uint32 = onlyInShaderCode "BitFieldReverse"
        static member BitFieldReverse (v : int) : int = onlyInShaderCode "BitFieldReverse"
        static member BitFieldReverse (v : uint64) : uint64 = onlyInShaderCode "BitFieldReverse"
        static member BitFieldReverse (v : int64) : int64 = onlyInShaderCode "BitFieldReverse"

        static member BitCount (v : uint32) : int = onlyInShaderCode "BitCount"
        static member BitCount (v : int) : int = onlyInShaderCode "BitCount"
        static member BitCount (v : uint64) : int = onlyInShaderCode "BitCount"
        static member BitCount (v : int64) : int = onlyInShaderCode "BitCount"

        static member MSB(v : uint32) : int = onlyInShaderCode "MSB"
        static member MSB(v : int) : int = onlyInShaderCode "MSB"
        static member MSB(v : uint64) : int = onlyInShaderCode "MSB"
        static member MSB(v : int64) : int = onlyInShaderCode "MSB"

        static member LSB(v : uint32) : int = onlyInShaderCode "LSB"
        static member LSB(v : int) : int = onlyInShaderCode "LSB"
        static member LSB(v : uint64) : int = onlyInShaderCode "LSB"
        static member LSB(v : int64) : int = onlyInShaderCode "LSB"


        static member FloatBitsToInt(v : float32) : int = onlyInShaderCode "FloatBitsToInt"
        static member FloatBitsToUInt(v : float32) : uint32 = onlyInShaderCode "FloatBitsToUInt"
        static member IntBitsToFloat(v : int) : float32 = onlyInShaderCode "IntBitsToFloat"
        static member UIntBitsToFloat(v : uint32) : float32 = onlyInShaderCode "UIntBitsToFloat"



    let getGlobalId() : V3i = onlyInShaderCode "getGlobalId"
    let getWorkGroupId() : V3i = onlyInShaderCode "getWorkGroupId"
    let getLocalId() : V3i = onlyInShaderCode "getLocalId"
    let getLocalIndex() : int = onlyInShaderCode "getLocalIndex"
    let getWorkGroupSize() : V3i = onlyInShaderCode "getWorkGroupSize"
    let getWorkGroupCount() : V3i = onlyInShaderCode "getWorkGroupCount"
    let barrier() : unit = onlyInShaderCode "barrier"

    /// Declares a shared array of the given constant size (compute shaders only). The result must be
    /// bound with let and only used for element reads and writes (it cannot be passed as a whole).
    /// The array is identified by the name of the bound variable, its element type and size: declaring
    /// it again (e.g. in a utility function) refers to the same array.
    let allocateShared<'a when 'a : unmanaged> (size : int) : 'a[] = onlyInShaderCode "allocateShared"

    /// Declares a single shared variable (compute shaders only). The result must be bound with let;
    /// the variable is read and written through the returned ref. The variable is identified by the
    /// name of the bound variable and its type: declaring it again (e.g. in a utility function) refers
    /// to the same variable.
    let allocateSharedValue<'a when 'a : unmanaged> () : 'a ref = onlyInShaderCode "allocateSharedValue"


    [<Sealed; AbstractClass>]
    type Debug private() =

        /// Print to the debug output (only Vulkan with validation layers)
        // See: https://github.com/KhronosGroup/Vulkan-ValidationLayers/blob/master/docs/debug_printf.md
        [<KeepCall>]
        static member Printf(format : string, [<ParamArray>] values : obj[]) : unit =
            onlyInShaderCode "debugPrintf"

        /// Print to the debug output (only Vulkan with validation layers)
        [<KeepCall>]
        static member Printfn(format : string, [<ParamArray>] values : obj[]) : unit =
            onlyInShaderCode "debugPrintfn"


    [<Literal>]
    let MaxLocalSize = 2147483647

    let LocalSize = V3i(2147483647, 2147483647, 2147483647)

    type LocalSizeAttribute() = 
        inherit System.Attribute()

        let mutable x = 1
        let mutable y = 1
        let mutable z = 1

        member __.X
            with get() = x
            and set v = x <- v

        member __.Y
            with get () = y
            and set v = y <- v

        member __.Z
            with get () = z
            and set v = z <- v

        override __.ToString() =
            sprintf "LocalSize(X=%d, Y=%d, Z=%d)" x y z

    type TessCoord<'a> = class end

    let tessellateTriangle (li : float32) (l01 : float32, l12 : float32, l20 : float32) : TessCoord<V3f> =
        onlyInShaderCode "tessellateTriangle"
        
    /// lu:  Inner tessellation level in horizontal (u) direction
    /// li:  Inner tessellation level in vertical (v) direction
    /// l01: Outer tessellation level for edge defined by u=0 (i.e. edge 0-1)
    /// l03: Outer tessellation level for edge defined by v=0 (i.e. edge 3-0)
    /// l23: Outer tessellation level for edge defined by u=1 (i.e. edge 2-3)
    /// l30: Outer tessellation level for edge defined by v=1 (i.e. edge 1-2)
    let tessellateQuad (lu : float32, lv : float32) (l01 : float32, l30 : float32, l23 : float32, l12 : float32) : TessCoord<V2f> =
        onlyInShaderCode "tessellateQuad"


[<AutoOpen>]
module ShaderBuilders =
    open Microsoft.FSharp.Quotations

    type BaseBuilder() =
        member x.For(a : Arr<'d, 'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.For(a : seq<'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.While(guard : unit -> bool, b : unit) =
            ()

        member x.Combine(l : unit, r : 'a) = r

        member x.Zero() = ()
        member x.Delay f = f()

    type VertexBuilder() =
        inherit BaseBuilder()
        member x.Return(v) = v

        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Vertex
            member x.OutputTopology = None


    type FragmentBuilder() =
        inherit BaseBuilder()
        member x.Return(v) = v
        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Fragment
            member x.OutputTopology = None

    type GeometryBuilder(size : Option<int>, top : OutputTopology) =
        member x.For(a : Arr<'d, 'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            a |> Seq.collect f

        member x.For(a : seq<'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            a |> Seq.collect f

        member x.Size = size

        member x.Yield(v : 'a) : seq<Primitive<'a>> =
            raise <| FShadeOnlyInShaderCodeException ""

        member x.For(p : Primitive<'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            raise <| FShadeOnlyInShaderCodeException ""

        member x.Delay(f : unit -> 'a) = f()

        member x.Quote() = ()

        member x.Combine(l : seq<Primitive<'a>>, r : seq<Primitive<'a>>) : seq<Primitive<'a>> =
            raise <| FShadeOnlyInShaderCodeException ""

        member x.Zero() : seq<Primitive<'a>> = 
            Seq.empty

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Geometry
            member x.OutputTopology = Some top


    type TessBuilder() =
        inherit BaseBuilder()
        member x.Bind(t : TessCoord<'c>, f : 'c -> 'a) : 'a =
            raise <| FShadeOnlyInShaderCodeException ""

        member x.Return(v) = v

        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.TessControl
            member x.OutputTopology = None

    type ComputeBuilder() =
        member x.For(a : Arr<'d, 'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.For(a : seq<'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.While(guard : unit -> bool, b : unit) =
            ()

        member x.Combine(l : unit, r : 'a) = r

        member x.Zero() = ()
        member x.Delay f = f()

        member x.Quote() = ()

        member inline x.Run(e : Expr<'a>) : Expr<'a> =
            try
                let mb = MethodBase.GetCurrentMethod()
                if isNull mb then
                    Log.warn "[FShade] Cannot determine function of compute shader"
                    e
                else
                    // .NET SDK 10.0.401 compiler does not inline for Debug builds
                    let mb =
                        if mb.DeclaringType = typeof<ComputeBuilder> && mb.Name = "Run" then
                            let frame = StackFrame(1, false)
                            frame.GetMethod()
                        else
                            mb

                    e.WithAttributes(Expr.NewTuple [ Expr.Value "Method"; Expr.Value mb] :: e.CustomAttributes) |> Expr.Cast
            with exn ->
                Log.error "[FShade] Failed to determine function of compute shader: %A" exn
                e

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Compute
            member x.OutputTopology = None

    type RayGenerationBuilder() =
        inherit BaseBuilder()

        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.RayGeneration
            member x.OutputTopology = None

    type RayMissBuilder() =
        inherit BaseBuilder()

        member x.Return(v) = v
        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Miss
            member x.OutputTopology = None

    type RayAnyHitBuilder() =
        inherit BaseBuilder()

        member x.Return(v) = v
        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.AnyHit
            member x.OutputTopology = None

    type RayClosestHitBuilder() =
        inherit BaseBuilder()

        member x.Return(v) = v
        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.ClosestHit
            member x.OutputTopology = None

    type RayCallableBuilder() =
        inherit BaseBuilder()

        member x.Return(v) = v
        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Callable
            member x.OutputTopology = None

    type RayIntersectionBuilder() =
        inherit BaseBuilder()

        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderStage = ShaderStage.Intersection
            member x.OutputTopology = None

    let compute = ComputeBuilder()
    let vertex = VertexBuilder()
    let tessellation = TessBuilder()
    let fragment = FragmentBuilder()

    let raygen = RayGenerationBuilder()
    let miss = RayMissBuilder()
    let anyHit = RayAnyHitBuilder()
    let closestHit = RayClosestHitBuilder()
    let callable = RayCallableBuilder()
    let intersection = RayIntersectionBuilder()

    let triangle = GeometryBuilder(None, OutputTopology.TriangleStrip)
    let line = GeometryBuilder(None, OutputTopology.LineStrip)
    let point = GeometryBuilder(None, OutputTopology.Points)

    let _triangle<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.TriangleStrip)
    let _line<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.LineStrip)
    let _point<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.Points)
