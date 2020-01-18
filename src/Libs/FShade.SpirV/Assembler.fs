namespace FShade.SpirV

open System
open System.Reflection

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open FSharp.Data.Adaptive


type SpirVIntrinsicType = { compileType : SpirV<uint32> }

type SpirVIntrinsicFunction = { compileFunction : uint32 -> uint32[] -> SpirV<uint32> }

[<AutoOpen>]
module private IntrinsicFunctions =

    [<Literal>]
    let glsl410 = "GLSL.std.450"

    module CIntrinsic = 
        let create (create : IdResultType -> IdRef[] -> SpirV<uint32>) =
            CIntrinsic.tagged { compileFunction = create }

        let ofFunction (create : IdResultType -> IdRef[] -> SpirV<uint32>) =
            CIntrinsic.tagged { compileFunction = create } |> Some

        let instr0 (code : Instruction) = 
            CIntrinsic.tagged
                { compileFunction = fun t args ->
                    spirv {
                        yield code
                        return 0u
                    }
                }
        let instr1 (code : IdResultType * IdResult * IdRef -> Instruction) = 
            CIntrinsic.tagged
                { compileFunction = fun t args ->
                    spirv {
                        let! id = SpirV.id
                        yield code(t, id, args.[0])
                        return id
                    }
                }

        let instr2 (code : IdResultType * IdResult * IdRef * IdRef -> Instruction) = 
            CIntrinsic.tagged
                { compileFunction = fun t args ->
                    spirv {
                        let! id = SpirV.id
                        yield code(t, id, args.[0], args.[1])
                        return id
                    }
                }
                

        let instr3 (code : IdResultType * IdResult * IdRef * IdRef * IdRef -> Instruction) = 
            CIntrinsic.tagged
                { compileFunction = fun t args ->
                    spirv {
                        let! id = SpirV.id
                        yield code(t, id, args.[0], args.[1], args.[2])
                        return id
                    }
                }

        let simple (code : GLSLExtInstruction) = 
            CIntrinsic.tagged
                { compileFunction = fun t args ->
                    spirv {
                        let! ext = SpirV.import glsl410
                        let! id = SpirV.id
                        yield Instruction.OpExtInst(t, id, ext, int code, args)
                        return id
                    }
                }
        let custom (code : GLSLExtInstruction) (perm : list<int>) = 
            CIntrinsic.tagged
                { compileFunction = fun t args ->
                    spirv {
                        let! ext = SpirV.import glsl410
                        let! id = SpirV.id
                        yield Instruction.OpExtInst(t, id, ext, int code, perm |> List.map (fun i -> args.[i]) |> List.toArray)
                        return id
                    }
                }
            
    let (|InstrinsicFunction|_|) : MethodInfo -> Option<CIntrinsic> =
        MethodTable.ofList [
            // ==========================================================================
            // TRIGONOMETRIC
            // ==========================================================================

            // sin
            CIntrinsic.simple GLSLExtInstruction.Sin, [
                exactly <@ Math.Sin @>
                exactly <@ Fun.Sin : float -> float @>
                exactly <@ Fun.Sin : float32 -> float32 @>
                generic <@ sin @> 
            ]

            // cos
            CIntrinsic.simple GLSLExtInstruction.Cos, [
                exactly <@ Math.Cos @>
                exactly <@ Fun.Cos : float -> float @>
                exactly <@ Fun.Cos : float32 -> float32 @>
                generic <@ cos @> 
            ]

            // tan
            CIntrinsic.simple GLSLExtInstruction.Tan, [
                exactly <@ Math.Tan @>
                exactly <@ Fun.Tan : float -> float @>
                exactly <@ Fun.Tan : float32 -> float32 @>
                generic <@ tan @> 
            ]

            // asin
            CIntrinsic.simple GLSLExtInstruction.Asin, [
                exactly <@ Math.Asin @>
                exactly <@ Fun.Asin : float -> float @>
                exactly <@ Fun.Asin : float32 -> float32 @>
                generic <@ asin @> 
            ]

            // acos
            CIntrinsic.simple GLSLExtInstruction.Acos, [
                exactly <@ Math.Acos @>
                exactly <@ Fun.Acos : float -> float @>
                exactly <@ Fun.Acos : float32 -> float32 @>
                generic <@ acos @> 
            ]

            // atan
            CIntrinsic.simple GLSLExtInstruction.Atan, [
                exactly <@ Math.Atan @>
                exactly <@ Fun.Atan : float -> float @>
                exactly <@ Fun.Atan : float32 -> float32 @>
                generic <@ atan @> 
            ]

            // atan2
            CIntrinsic.simple GLSLExtInstruction.Atan2, [
                exactly <@ Math.Atan2 @>
                exactly <@ Fun.Atan2 : float * float -> float @>
                exactly <@ Fun.Atan2 : float32 * float32 -> float32 @>
                generic <@ atan2 @> 
            ]

            // sinh
            CIntrinsic.simple GLSLExtInstruction.Sinh, [
                exactly <@ Math.Sinh @>
                exactly <@ Fun.Sinh : float -> float @>
                exactly <@ Fun.Sinh : float32 -> float32 @>
                generic <@ sinh @> 
            ]

            // cosh
            CIntrinsic.simple GLSLExtInstruction.Cosh, [
                exactly <@ Math.Cosh @>
                exactly <@ Fun.Cosh : float -> float @>
                exactly <@ Fun.Cosh : float32 -> float32 @>
                generic <@ cosh @> 
            ]

            // tanh
            CIntrinsic.simple GLSLExtInstruction.Tanh, [
                exactly <@ Math.Tanh @>
                exactly <@ Fun.Tanh : float -> float @>
                exactly <@ Fun.Tanh : float32 -> float32 @>
                generic <@ tanh @> 
            ]

            // asinh
            CIntrinsic.simple GLSLExtInstruction.Asinh, [
                exactly <@ Fun.Asinh : float -> float @>
                exactly <@ Fun.Asinh : float32 -> float32 @>
            ]

            // acosh
            CIntrinsic.simple GLSLExtInstruction.Acosh, [
                exactly <@ Fun.Acosh : float -> float @>
                exactly <@ Fun.Acosh : float32 -> float32 @>
            ]

            // atanh
            CIntrinsic.simple GLSLExtInstruction.Atanh, [
                exactly <@ Fun.Atanh : float -> float @>
                exactly <@ Fun.Atanh : float32 -> float32 @>
            ]

            // ==========================================================================
            // EXPONENTIAL
            // ==========================================================================

            // pow
            CIntrinsic.simple GLSLExtInstruction.Pow, [
                exactly <@ Fun.Pow : float * float -> float @>
                exactly <@ Fun.Pow : float32 * float32 -> float32 @>
                exactly <@ Math.Pow @>
                generic <@ ( ** ) : float -> float -> float @>
                generic <@ pow : float -> float -> float @>
                generic <@ pown @>
            ]

            // exp
            CIntrinsic.simple GLSLExtInstruction.Exp, [
                exactly <@ Math.Exp @> 
                exactly <@ Fun.Exp : float32 -> float32 @>
                exactly <@ Fun.Exp : float -> float @>
                generic <@ exp @>
            ]

            // log
            CIntrinsic.simple GLSLExtInstruction.Log, [
                exactly <@ Math.Log @> 
                exactly <@ Fun.Log : float32 -> float32 @>
                exactly <@ Fun.Log : float -> float @>
                generic <@ log @>
            ]

            // log2
            CIntrinsic.simple GLSLExtInstruction.Log2, [
                exactly <@ Fun.Log2 : float32 -> float32 @>
                exactly <@ Fun.Log2 : float -> float @>
            ]

            // sqrt
            CIntrinsic.simple GLSLExtInstruction.Sqrt, [
                exactly <@ Math.Sqrt @>
                exactly <@ Fun.Sqrt : float32 -> float32 @>
                exactly <@ Fun.Sqrt : float -> float @>
                generic <@ sqrt @>
            ]


            // ==========================================================================
            // COMMON
            // ==========================================================================
            
            // abs
            CIntrinsic.simple GLSLExtInstruction.SAbs, [
                exactly <@ Math.Abs : int8 -> _ @>
                exactly <@ Math.Abs : int16 -> _ @>
                exactly <@ Math.Abs : int32 -> _ @>
                exactly <@ Math.Abs : int64 -> _ @>

                exactly <@ Fun.Abs : int8 -> _ @>
                exactly <@ Fun.Abs : int16 -> _ @>
                exactly <@ Fun.Abs : int32 -> _ @>
                exactly <@ Fun.Abs : int64 -> _ @>
                
                exactly <@ fun (v : int8) -> v.Abs() @>
                exactly <@ fun (v : int16) -> v.Abs() @>
                exactly <@ fun (v : int32) -> v.Abs() @>
                exactly <@ fun (v : int64) -> v.Abs() @>

                exactly <@ fun (v : V2i) -> v.Abs() @>
                exactly <@ fun (v : V3i) -> v.Abs() @>
                exactly <@ fun (v : V4i) -> v.Abs() @>
                exactly <@ fun (v : V2l) -> v.Abs() @>
                exactly <@ fun (v : V3l) -> v.Abs() @>
                exactly <@ fun (v : V4l) -> v.Abs() @>
                
                exactly <@ abs : int8 -> int8 @>
                exactly <@ abs : int16 -> int16 @>
                exactly <@ abs : int32 -> int32 @>
                exactly <@ abs : int64 -> int64 @>

            ]

            // fabs
            CIntrinsic.simple GLSLExtInstruction.FAbs, [
                exactly <@ Math.Abs : float32 -> _ @>
                exactly <@ Math.Abs : float -> _ @>
                exactly <@ Math.Abs : decimal -> _ @>

                exactly <@ Fun.Abs : float32 -> _ @>
                exactly <@ Fun.Abs : float -> _ @>
                exactly <@ Fun.Abs : decimal -> _ @>

                exactly <@ fun (v : float32) -> v.Abs() @>
                exactly <@ fun (v : float) -> v.Abs() @>
                exactly <@ fun (v : decimal) -> v.Abs() @>

                exactly <@ fun (v : V2f) -> v.Abs() @>
                exactly <@ fun (v : V3f) -> v.Abs() @>
                exactly <@ fun (v : V4f) -> v.Abs() @>
                exactly <@ fun (v : V2d) -> v.Abs() @>
                exactly <@ fun (v : V3d) -> v.Abs() @>
                exactly <@ fun (v : V4d) -> v.Abs() @>

                exactly <@ abs : float32 -> float32 @>
                exactly <@ abs : float -> float @>
            ]

            // sign
            CIntrinsic.simple GLSLExtInstruction.SSign, [
                exactly <@ Math.Sign : int8 -> _ @>
                exactly <@ Math.Sign : int16 -> _ @>
                exactly <@ Math.Sign : int32 -> _ @>
                exactly <@ Math.Sign : int64 -> _ @>

                exactly <@ Fun.Sign : int8 -> _ @>
                exactly <@ Fun.Sign : int16 -> _ @>
                exactly <@ Fun.Sign : int32 -> _ @>
                exactly <@ Fun.Sign : int64 -> _ @>
                
                exactly <@ fun (v : int8) -> v.Sign() @>
                exactly <@ fun (v : int16) -> v.Sign() @>
                exactly <@ fun (v : int32) -> v.Sign() @>
                exactly <@ fun (v : int64) -> v.Sign() @>

                exactly <@ sign : int8 -> int @>
                exactly <@ sign : int16 -> int @>
                exactly <@ sign : int32 -> int @>
                exactly <@ sign : int64 -> int @>
            ]

            // sign
            CIntrinsic.simple GLSLExtInstruction.FSign, [
                exactly <@ Math.Sign : float32 -> _ @>
                exactly <@ Math.Sign : float -> _ @>
                exactly <@ Math.Sign : decimal -> _ @>

                exactly <@ Fun.Sign : float32 -> _ @>
                exactly <@ Fun.Sign : float -> _ @>
                exactly <@ Fun.Sign : decimal -> _ @>

                exactly <@ fun (v : float32) -> v.Sign() @>
                exactly <@ fun (v : float) -> v.Sign() @>
                exactly <@ fun (v : decimal) -> v.Sign() @>

                exactly <@ sign : float32 -> int @>
                exactly <@ sign : float -> int @>
            ]
            
            // floor
            CIntrinsic.simple GLSLExtInstruction.Floor, [
                exactly <@ Math.Floor : float -> _ @>
                exactly <@ Math.Floor : decimal -> _ @>
                exactly <@ Fun.Floor : float -> _ @>
                exactly <@ Fun.Floor : float32 -> _ @>
                generic <@ floor @>
            ]
            
            // trunc
            CIntrinsic.simple GLSLExtInstruction.Trunc, [
                exactly <@ Math.Truncate : float -> _ @>
                exactly <@ Math.Truncate : decimal -> _ @>
                generic <@ truncate @>
            ]

            // round
            CIntrinsic.simple GLSLExtInstruction.Round, [
                exactly <@ Math.Round : float -> _ @>
                exactly <@ Math.Round : decimal -> _ @>
                exactly <@ Fun.Round : float -> _ @>
                exactly <@ Fun.Round : float32 -> _ @>
                generic <@ round @>
            ]

            // ceil
            CIntrinsic.simple GLSLExtInstruction.Ceil, [
                exactly <@ Math.Ceiling : float -> _ @>
                exactly <@ Math.Ceiling : decimal -> _ @>
                exactly <@ Fun.Ceiling : float -> _ @>
                exactly <@ Fun.Ceiling : float32 -> _ @>
                generic <@ ceil @>
            ]

            // fract
            CIntrinsic.simple GLSLExtInstruction.Fract, [
                exactly <@ Fun.Frac : float -> _ @>
                exactly <@ Fun.Frac : float32 -> _ @>
            ]

            // min(int)
            CIntrinsic.simple GLSLExtInstruction.SMin, [
                exactly <@ Math.Min : int8 * int8 -> _ @>
                exactly <@ Math.Min : int16 * int16 -> _ @>
                exactly <@ Math.Min : int32 * int32 -> _ @>
                exactly <@ Math.Min : int64 * int64 -> _ @>
                
                exactly <@ Fun.Min : int8 * int8 -> _ @>
                exactly <@ Fun.Min : int16 * int16 -> _ @>
                exactly <@ Fun.Min : int32 * int32 -> _ @>
                exactly <@ Fun.Min : int64 * int64 -> _ @>
                
                exactly <@ V2i.Min : V2i * V2i -> V2i @>
                exactly <@ V3i.Min : V3i * V3i -> V3i @>
                exactly <@ V4i.Min : V4i * V4i -> V4i @>
                exactly <@ V2l.Min : V2l * V2l -> V2l @>
                exactly <@ V3l.Min : V3l * V3l -> V3l @>
                exactly <@ V4l.Min : V4l * V4l -> V4l @>

                
                exactly <@ min : int8 -> int8 -> int8 @>
                exactly <@ min : int16 -> int16 -> int16 @>
                exactly <@ min : int32 -> int32 -> int32 @>
            ]

            // min(uint)
            CIntrinsic.simple GLSLExtInstruction.UMin, [
                exactly <@ Math.Min : uint8 * uint8 -> _ @>
                exactly <@ Math.Min : uint16 * uint16 -> _ @>
                exactly <@ Math.Min : uint32 * uint32 -> _ @>
                exactly <@ Math.Min : uint64 * uint64 -> _ @>

                exactly <@ Fun.Min : uint8 * uint8 -> _ @>
                exactly <@ Fun.Min : uint16 * uint16 -> _ @>
                exactly <@ Fun.Min : uint32 * uint32 -> _ @>
                exactly <@ Fun.Min : uint64 * uint64 -> _ @>

                
                exactly <@ min : uint8 -> uint8 -> uint8 @>
                exactly <@ min : uint16 -> uint16 -> uint16 @>
                exactly <@ min : uint32 -> uint32 -> uint32 @>
            ]

            // min(float)
            CIntrinsic.simple GLSLExtInstruction.FMin, [
                exactly <@ Math.Min : float32 * float32 -> _ @>
                exactly <@ Math.Min : float * float -> _ @>
                exactly <@ Math.Min : decimal * decimal -> _ @>

                exactly <@ Fun.Min : float32 * float32 -> _ @>
                exactly <@ Fun.Min : float * float -> _ @>

                exactly <@ V2f.Min : V2f * V2f -> V2f @>
                exactly <@ V3f.Min : V3f * V3f -> V3f @>
                exactly <@ V4f.Min : V4f * V4f -> V4f @>
                exactly <@ V2d.Min : V2d * V2d -> V2d @>
                exactly <@ V3d.Min : V3d * V3d -> V3d @>
                exactly <@ V4d.Min : V4d * V4d -> V4d @>

                exactly <@ min : float32 -> float32 -> float32 @> 
                exactly <@ min : float -> float -> float @> 
            ]




            // max(int)
            CIntrinsic.simple GLSLExtInstruction.SMax, [
                exactly <@ Math.Max : int8 * int8 -> _ @>
                exactly <@ Math.Max : int16 * int16 -> _ @>
                exactly <@ Math.Max : int32 * int32 -> _ @>
                exactly <@ Math.Max : int64 * int64 -> _ @>
                
                exactly <@ Fun.Max : int8 * int8 -> _ @>
                exactly <@ Fun.Max : int16 * int16 -> _ @>
                exactly <@ Fun.Max : int32 * int32 -> _ @>
                exactly <@ Fun.Max : int64 * int64 -> _ @>
                
                exactly <@ V2i.Max : V2i * V2i -> V2i @>
                exactly <@ V3i.Max : V3i * V3i -> V3i @>
                exactly <@ V4i.Max : V4i * V4i -> V4i @>
                exactly <@ V2l.Max : V2l * V2l -> V2l @>
                exactly <@ V3l.Max : V3l * V3l -> V3l @>
                exactly <@ V4l.Max : V4l * V4l -> V4l @>

                
                exactly <@ max : int8 -> int8 -> int8 @>
                exactly <@ max : int16 -> int16 -> int16 @>
                exactly <@ max : int32 -> int32 -> int32 @>
            ]

            // max(uint)
            CIntrinsic.simple GLSLExtInstruction.UMax, [
                exactly <@ Math.Max : uint8 * uint8 -> _ @>
                exactly <@ Math.Max : uint16 * uint16 -> _ @>
                exactly <@ Math.Max : uint32 * uint32 -> _ @>
                exactly <@ Math.Max : uint64 * uint64 -> _ @>

                exactly <@ Fun.Max : uint8 * uint8 -> _ @>
                exactly <@ Fun.Max : uint16 * uint16 -> _ @>
                exactly <@ Fun.Max : uint32 * uint32 -> _ @>
                exactly <@ Fun.Max : uint64 * uint64 -> _ @>

                
                exactly <@ max : uint8 -> uint8 -> uint8 @>
                exactly <@ max : uint16 -> uint16 -> uint16 @>
                exactly <@ max : uint32 -> uint32 -> uint32 @>
            ]

            // max(float)
            CIntrinsic.simple GLSLExtInstruction.FMin, [
                exactly <@ Math.Max : float32 * float32 -> _ @>
                exactly <@ Math.Max : float * float -> _ @>
                exactly <@ Math.Max : decimal * decimal -> _ @>

                exactly <@ Fun.Max : float32 * float32 -> _ @>
                exactly <@ Fun.Max : float * float -> _ @>

                exactly <@ V2f.Max : V2f * V2f -> V2f @>
                exactly <@ V3f.Max : V3f * V3f -> V3f @>
                exactly <@ V4f.Max : V4f * V4f -> V4f @>
                exactly <@ V2d.Max : V2d * V2d -> V2d @>
                exactly <@ V3d.Max : V3d * V3d -> V3d @>
                exactly <@ V4d.Max : V4d * V4d -> V4d @>

                exactly <@ max : float32 -> float32 -> float32 @> 
                exactly <@ max : float -> float -> float @> 
            ]
            
            // clamp(int)
            CIntrinsic.custom GLSLExtInstruction.SClamp [2; 0; 1], [
                exactly <@ clamp : int8 -> _ -> _ @>
                exactly <@ clamp : int16 -> _ -> _ @>
                exactly <@ clamp : int32 -> _ -> _ @>
                exactly <@ clamp : int64 -> _ -> _ @>
            ]

            // clamp(uint)
            CIntrinsic.custom GLSLExtInstruction.UClamp [2; 0; 1], [
                exactly <@ clamp : uint8 -> _ -> _ @>
                exactly <@ clamp : uint16 -> _ -> _ @>
                exactly <@ clamp : uint32 -> _ -> _ @>
                exactly <@ clamp : uint64 -> _ -> _ @>
            ]

            // clamp(float)
            CIntrinsic.custom GLSLExtInstruction.FClamp [2; 0; 1], [
                exactly <@ clamp : float32 -> _ -> _ @>
                exactly <@ clamp : float -> _ -> _ @>
            ]

            // mix
            CIntrinsic.simple GLSLExtInstruction.FMix, [
                exactly <@ Fun.Lerp : float32 * float32 * float32 -> _ @>
                exactly <@ Fun.Lerp : float * float * float -> _ @>
            ]
            
            
            // ==========================================================================
            // GEOMETRIC
            // ==========================================================================
            
            // distance
            CIntrinsic.simple GLSLExtInstruction.Distance, [
                exactly <@ V2i.Distance : _ * _ -> _ @>
                exactly <@ V3i.Distance : _ * _ -> _ @>
                exactly <@ V4i.Distance : _ * _ -> _ @>
                exactly <@ V2l.Distance : _ * _ -> _ @>
                exactly <@ V3l.Distance : _ * _ -> _ @>
                exactly <@ V4l.Distance : _ * _ -> _ @>
                exactly <@ V2f.Distance : _ * _ -> _ @>
                exactly <@ V3f.Distance : _ * _ -> _ @>
                exactly <@ V4f.Distance : _ * _ -> _ @>
                exactly <@ V2d.Distance : _ * _ -> _ @>
                exactly <@ V3d.Distance : _ * _ -> _ @>
                exactly <@ V4d.Distance : _ * _ -> _ @>
            ]

            // normalize
            CIntrinsic.simple GLSLExtInstruction.Normalize, [
                exactly <@ fun (v : V2f) -> v.Normalized @>
                exactly <@ fun (v : V3f) -> v.Normalized @>
                exactly <@ fun (v : V4f) -> v.Normalized @>
                exactly <@ fun (v : V2d) -> v.Normalized @>
                exactly <@ fun (v : V3d) -> v.Normalized @>
                exactly <@ fun (v : V4d) -> v.Normalized @>
                generic <@ Vec.normalize : V3d -> V3d @>
            ]

            // reflect
            CIntrinsic.simple GLSLExtInstruction.Reflect, [
                exactly <@ Vec.reflect : V3d -> V3d -> V3d @> 
            ]

            // refract
            CIntrinsic.simple GLSLExtInstruction.Refract, [
                exactly <@ Vec.refract : V3d -> V3d -> float -> V3d @> 
            ]
            
            // ==========================================================================
            // MATRIX
            // ==========================================================================

            // transpose
            CIntrinsic.instr1 Instruction.OpTranspose, [
                exactly <@ fun (v : M22f) -> v.Transposed @>
                exactly <@ fun (v : M33f) -> v.Transposed @>
                exactly <@ fun (v : M44f) -> v.Transposed @>
                exactly <@ fun (v : M22d) -> v.Transposed @>
                exactly <@ fun (v : M33d) -> v.Transposed @>
                exactly <@ fun (v : M44d) -> v.Transposed @>
                generic <@ Mat.transpose : M22d -> M22d @>
            ]

            // det
            CIntrinsic.simple GLSLExtInstruction.Determinant, [
                exactly <@ fun (v : M22f) -> v.Determinant @>
                exactly <@ fun (v : M33f) -> v.Determinant @>
                exactly <@ fun (v : M44f) -> v.Determinant @>
                exactly <@ fun (v : M22d) -> v.Determinant @>
                exactly <@ fun (v : M33d) -> v.Determinant @>
                exactly <@ fun (v : M44d) -> v.Determinant @>
                generic <@ Mat.det : M22d -> float @>
            ]

            // inverse
            CIntrinsic.simple GLSLExtInstruction.MatrixInverse, [
                exactly <@ fun (v : M22f) -> v.Inverse @>
                exactly <@ fun (v : M33f) -> v.Inverse @>
                exactly <@ fun (v : M44f) -> v.Inverse @>
                exactly <@ fun (v : M22d) -> v.Inverse @>
                exactly <@ fun (v : M33d) -> v.Inverse @>
                exactly <@ fun (v : M44d) -> v.Inverse @>
                generic <@ Mat.inverse : M22d -> M22d @>
            ]
            

            // ==========================================================================
            // GLOBAL
            // ==========================================================================
            CIntrinsic.instr1 Instruction.OpDPdx, [ generic <@ ddx : float -> _ @> ]
            CIntrinsic.instr1 Instruction.OpDPdy, [ generic <@ ddy : float -> _ @> ]
            CIntrinsic.instr1 Instruction.OpDPdxFine, [ generic <@ ddxFine : float -> _ @> ]
            CIntrinsic.instr1 Instruction.OpDPdyFine, [ generic <@ ddyFine : float -> _ @> ]
            CIntrinsic.instr1 Instruction.OpDPdxCoarse, [ generic <@ ddxCoarse : float -> _ @> ]
            CIntrinsic.instr1 Instruction.OpDPdyCoarse, [ generic <@ ddyCoarse : float -> _ @> ]
            CIntrinsic.instr0 Instruction.OpKill, [ exactly <@ discard @> ]
            CIntrinsic.instr0 Instruction.OpEmitVertex, [ exactly <@ emitVertex @> ]
            CIntrinsic.instr0 Instruction.OpEndPrimitive, [ 
                exactly <@ restartStrip @>
                exactly <@ endPrimitive @>
            ]

            // compute stuff
//            CIntrinsic.tagged "ivec3(gl_NumWorkGroups)", [ exactly <@ getWorkGroupCount @> ]
//            CIntrinsic.tagged "ivec3(gl_WorkGroupID)", [ exactly <@ getWorkGroupId @> ]
//            CIntrinsic.tagged "ivec3(gl_LocalInvocationID)", [ exactly <@ getLocalId @> ]
//            CIntrinsic.tagged "ivec3(gl_GlobalInvocationID)", [ exactly <@ getGlobalId @> ]
//            CIntrinsic.tagged "int(gl_LocalInvocationIndex)", [ exactly <@ getLocalIndex @> ]
//            CIntrinsic.tagged "ivec3(gl_WorkGroupSize)", [ exactly <@ getWorkGroupSize @> ]
//            CIntrinsic.tagged "barrier()", [ exactly <@ barrier @> ]

        ]



module Assembler =

    let rec assembleType (t : CType) =
        SpirV.cached t {
            match t with
                | CType.CVoid -> 
                    let! id = SpirV.id
                    yield OpTypeVoid id
                    return id

                | CType.CBool ->
                    let! id = SpirV.id
                    yield OpTypeBool id
                    return id
                
                | CType.CInt(s,w) ->
                    let! id = SpirV.id
                    yield OpTypeInt(id, w, (if s then 1 else 0))
                    return id

                | CType.CFloat(32 | 64) ->
                    let! id = SpirV.id
                    yield OpTypeFloat(id, 32)
                    return id

                | CType.CFloat(w) ->
                    let! id = SpirV.id
                    yield OpTypeFloat(id, w)
                    return id

                | CType.CVector(e,d) ->
                    let! e = assembleType e
                    let! id = SpirV.id
                    yield OpTypeVector(id, e, d)
                    return id

                | CType.CMatrix(e,r,c) ->
                    let! ev = assembleType (CVector(e, r))
                    let! id = SpirV.id
                    yield OpTypeMatrix(id, ev, c)
                    return id
                    
                | CType.CArray(e,l) ->
                    let! e = assembleType e
                    let! id = SpirV.id
                    yield OpTypeArray(id, e, uint32 l)
                    return id
                    
                | CType.CPointer(_,e) ->
                    let! e = assembleType e
                    let! id = SpirV.id
                    yield OpTypeRuntimeArray(id, e)
                    return id
                    
                | CType.CStruct(name, fields, _) ->
                    let fields = List.toArray fields
                    let! fieldTypes = fields |> Array.mapS (fst >> assembleType)

                    let! id = SpirV.id
                    yield OpTypeStruct(id, fieldTypes)

                    yield OpName(id, name)
                    for i in 0 .. fields.Length - 1 do
                        let (_,n) = fields.[i]
                        yield OpMemberName(id, i, n)
                        do! SpirV.setFieldId t n i

                    return id

                | CType.CIntrinsic it ->
                    return! assembleIntrinsicType it
                    
        }

    and assemblePtrType (clazz : StorageClass) (t : CType) =
        SpirV.cached (clazz, t) {
            let! t = assembleType t
            let! id = SpirV.id
            yield OpTypePointer(id, clazz, t)
            return id
        }

    and assembleIntrinsicType (t : CIntrinsicType) : SpirV<uint32> =
        spirv {
            match t.tag with
                | :? SpirVIntrinsicType as t ->
                    return! t.compileType
                | _ ->
                    return failwithf "[FShade] cannot compile type %A" t
        }

    let rec assembleLiteral (t : CType) (v : CLiteral) =
        SpirV.cached (t, v) {
            let! tid = assembleType t
            let! id = SpirV.id
            match t, v with
                | CInt(_, 8), CIntegral v ->
                    yield OpConstant(tid, id, [| int (int8 v) |])
                | CInt(_, 16), CIntegral v ->
                    yield OpConstant(tid, id, [| int (int16 v) |])
                | CInt(_, 32), CIntegral v ->
                    yield OpConstant(tid, id, [| int (int32 v) |])
                | CInt(_, 64), CIntegral v ->
                    yield OpConstant(tid, id, [| int (v >>> 32); int v |])

                | CFloat(32 | 64), CFractional v ->
                    let value = v |> float32 |> BitConverter.GetBytes
                    yield OpConstant(tid, id, [| BitConverter.ToInt32(value, 0) |])
                    
                | CFloat(16), CFractional v ->
                    let mutable value = float16()
                    value.Float32 <- float32 v
                    yield OpConstant(tid, id, [| int value.UInt16 |])

                | _, CBool v ->
                    if v then yield OpConstantTrue(tid, id)
                    else yield OpConstantFalse(tid, id)

                | _, Null ->
                    yield OpConstantNull(tid, id)

                | _ ->
                    failwithf "[FShade] unsupported literal value %A : %A" v t

            return id
        }

    [<AutoOpen>]
    module Patterns = 
        let rec (|Floating|Signed|Unsigned|Other|) (t : CType) =
            match t with
                | CFloat _ -> Floating
                | CInt(signed, _) ->
                    if signed then Signed
                    else Unsigned

                | CMatrix(t, _, _) -> (|Floating|Signed|Unsigned|Other|) t
                | CVector(t, _) -> (|Floating|Signed|Unsigned|Other|) t

                | _ ->
                    Other

        let (|Scalar|Vector|Matrix|NonNumeric|) (t : CType) =
            match t with
                | CInt _ 
                | CFloat _ ->
                    Scalar
                | CVector(et,_) ->
                    Vector et
                | CMatrix(et, _, _) ->
                    Matrix et
                | _ ->
                    NonNumeric

    let private newVector (et : CType) (d : int) (vid : uint32) =
        spirv {
            let! vType = assembleType (CVector(et, d))
            let! tid = SpirV.id
            yield OpCompositeConstruct(vType, tid, Array.create d vid)
            return tid
        }

    let rec assembleExpr (e : CExpr) : SpirV<uint32> =
        spirv {
            let! tid = assembleType e.ctype
            match e with
                | CVar v ->
                    let! vid = SpirV.getId v.name
                    let! id = SpirV.id
                    yield OpLoad(tid, id, vid, None)
                    return id

                | CValue(t,v) -> 
                    return! assembleLiteral t v

                | CCall(f, args) ->
                    let! fid = SpirV.getId f
                    let! id = SpirV.id
                    let! args = args |> Array.mapS assembleExpr
                    yield OpFunctionCall(tid, id, fid, args)
                    return id

                | CCallIntrinsic(_,f,args) ->
                    let! args = args |> Array.mapS assembleExpr
                    match f.tag with
                        | :? SpirVIntrinsicFunction as f ->
                            return! f.compileFunction tid args
                        | _ ->
                            return failwith ""

                | CConditional(_, cond, i, e) ->
                    let! cond = assembleExpr cond

                    let! lTrue = SpirV.id
                    let! lFalse = SpirV.id
                    let! lEnd = SpirV.id

                    yield OpBranchConditional(cond, lTrue, lFalse, [||])
                    yield OpLabel(lTrue)
                    let! vTrue = assembleExpr i
                    yield OpBranch(lEnd)
                    yield OpLabel(lFalse)
                    let! vFalse = assembleExpr e
                    yield OpLabel(lEnd)

                    let! id = SpirV.id
                    yield OpPhi(tid, id, [| PairIdRefIdRef(vTrue, lTrue); PairIdRefIdRef(vFalse, lFalse) |])
                    return id

                | CReadInput(ParameterKind.Uniform, t, name, None) ->
                    let! vid, path = SpirV.getUniformId name
                    match path with
                        | [] -> 
                            let! id = SpirV.id
                            yield OpLoad(tid, id, vid, None)
                            return id

                        | p ->  
                            let! ptrType = assemblePtrType StorageClass.Uniform t
                            let! ptr = SpirV.id
                            yield OpInBoundsAccessChain(ptrType, ptr, vid, List.toArray p)
                            let! id = SpirV.id
                            yield OpLoad(tid, id, ptr, None)
                            return id
                            
                | CReadInput(kind, t, name, index) ->
                    let! vid = SpirV.getId (kind, name)
                    match index with
                        | None ->
                            let! id = SpirV.id
                            yield OpLoad(tid, id, vid, None)
                            return id
                        | Some index ->
                            let! index = assembleExpr index
                            let clazz = StorageClass.Input

                            let! ptrType = assemblePtrType clazz (CPointer(CPointerModifier.None, t))
                            let! ptr = SpirV.id
                            yield OpAccessChain(ptrType, ptr, vid, [| index |])
                            let! id = SpirV.id
                            yield OpLoad(tid, id, ptr, None)
                            return id

                | CNeg(t,e) ->
                    let! eid = assembleExpr e
                    let! id = SpirV.id
                    match t with
                        | Signed -> yield OpSNegate(tid, id, eid)
                        | Floating -> yield OpFNegate(tid, id, eid)
                        | _ -> failwithf "[FShade] cannot negate %A" t

                    return id

                | CNot(t,e) ->
                    let! eid = assembleExpr e
                    let! id = SpirV.id
                    yield OpLogicalNot(tid, id, eid)
                    return id

                | CAdd(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match t with
                        | Signed | Unsigned -> yield OpIAdd(tid, id, lid, rid)
                        | Floating -> yield OpFAdd(tid, id, lid, rid)
                        | _ -> failwithf "[FShade] cannot add %A" t
                    return id

                | CSub(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match t with
                        | Signed | Unsigned -> yield OpISub(tid, id, lid, rid)
                        | Floating -> yield OpFSub(tid, id, lid, rid)
                        | _ -> failwithf "[FShade] cannot add %A" t
                    return id
                    
                | CMul(t, l, r) | CMulMatVec(t, l, r) | CMulMatMat(t, l, r) | CMulVecMat(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id

                    let reverse = true

                    match l.ctype, r.ctype with
                        | Scalar, Vector _          -> yield OpVectorTimesScalar(tid, id, rid, lid)
                        | Vector _, Scalar          -> yield OpVectorTimesScalar(tid, id, lid, rid)
                        | Scalar, Matrix _          -> yield OpMatrixTimesScalar(tid, id, rid, lid)
                        | Matrix _, Scalar          -> yield OpMatrixTimesScalar(tid, id, lid, rid)
                        | Floating, Floating        -> yield OpFMul(tid, id, lid, rid)

                        | (Signed | Unsigned), (Signed | Unsigned) -> 
                            yield OpIMul(tid, id, lid, rid)

                        | Matrix _, Vector _ -> 
                            if reverse then yield OpVectorTimesMatrix(tid, id, rid, lid)
                            else yield OpMatrixTimesVector(tid, id, lid, rid)

                        | Vector _, Matrix _ -> 
                            if reverse then yield OpMatrixTimesVector(tid, id, rid, lid)
                            else yield OpVectorTimesMatrix(tid, id, lid, rid)

                        | Matrix _, Matrix _ -> 
                            if reverse then yield OpMatrixTimesMatrix(tid, id, rid, lid)
                            else yield OpMatrixTimesMatrix(tid, id, lid, rid)

                        | l, r ->
                            failwithf "[FShade] cannot multiply %A and %A" l r

                    return id

                | CDiv(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let lType = l.ctype
                    let rType = r.ctype
                    match lType, rType with
                        | CVector(et,d), Scalar ->
                            let! vid = newVector rType d rid
                            let! id = SpirV.id
                            match et with
                                | Floating -> yield OpFDiv(tid, id, lid, vid)
                                | Signed -> yield OpSDiv(tid, id, lid, vid)
                                | Unsigned -> yield OpUDiv(tid, id, lid, vid)
                                | _ -> failwith ""

                            return id

                        | Scalar, CVector(et, d) ->
                            let! vid = newVector lType d lid
                            let! id = SpirV.id
                            match et with
                                | Floating -> yield OpFDiv(tid, id, vid, rid)
                                | Signed -> yield OpSDiv(tid, id, vid, rid)
                                | Unsigned -> yield OpUDiv(tid, id, vid, rid)
                                | _ -> failwith ""

                            return id
                            
                        | Signed, Signed ->
                            let! id = SpirV.id
                            yield OpSDiv(tid, id, lid, rid)
                            return id

                        | Unsigned, Unsigned ->
                            let! id = SpirV.id
                            yield OpUDiv(tid, id, lid, rid)
                            return id

                        | Floating, Floating ->
                            let! id = SpirV.id
                            yield OpFDiv(tid, id, lid, rid)
                            return id

                        | _ -> 
                            return failwith "not implemented"

                | CMod(t, l, r) ->
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = SpirV.id
                    match t with
                        | Floating -> yield OpFMod(tid, id, l, r)
                        | Signed -> yield OpSMod(tid, id, l, r)
                        | Unsigned -> yield OpUMod(tid, id, l, r)
                        | _ -> failwithf "[SpirV] bad type in modulus %A" t
                    return id

                | CTranspose(t, v) ->
                    let! v = assembleExpr v
                    let! id = SpirV.id
                    yield OpTranspose(tid, id, v)
                    return id

                | CDot(t, l, r) ->
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = SpirV.id
                    yield OpDot(tid, id, l, r)
                    return id
                    
                | CCross(t, l, r) ->
                    let! glsl = SpirV.import glsl410
                    let! l = assembleExpr l
                    let! r = assembleExpr r
                    let! id = SpirV.id
                    yield OpExtInst(tid, id, 1u, int GLSLExtInstruction.Cross, [| l; r |])
                    return id

                | CVecSwizzle(t, v, fields) ->
                    let! v = assembleExpr v
                    let! id = SpirV.id
                    let comp = fields |> List.map int |> List.toArray
                    yield OpVectorShuffle(tid, id, v, v, comp)
                    return id

                | CVecItem(t, v, i) ->
                    let! v = assembleExpr v
                    let! i = assembleExpr i
                    let! id = SpirV.id
                    yield OpVectorExtractDynamic(tid, id, v, i)
                    return id

                | CNewVector(t, d, args) ->
                    let! args = args |> List.mapS assembleExpr
                    let! id = SpirV.id
                    yield OpCompositeConstruct(tid, id, List.toArray args)
                    return id

                | CVecLength(t, v) ->
                    let! v = assembleExpr v
                    let! glsl = SpirV.import glsl410
                    let! id = SpirV.id
                    yield OpExtInst(tid, id, 1u, int GLSLExtInstruction.Length, [| v |])
                    return id

                | CMatrixElement _ | CMatrixFromCols _ | CMatrixFromRows _ | CNewMatrix _ | CMatrixRow _ | CMatrixCol _ | CConvertMatrix _ ->
                    return failwith "not implemented"
                    
                | CConvert(t, e) ->
                    let! eid = assembleExpr e
                    let! id = SpirV.id
                    match e.ctype, t with
                        | Floating, Floating    -> yield OpFConvert(tid, id, eid)
                        | Floating, Signed      -> yield OpConvertFToS(tid, id, eid)
                        | Floating, Unsigned    -> yield OpConvertFToU(tid, id, eid)
                        | Signed, Floating      -> yield OpConvertSToF(tid, id, eid)
                        | Signed, Signed        -> yield OpSConvert(tid, id, eid)
                        | Signed, Unsigned      -> yield OpSatConvertSToU(tid, id, eid)
                        | Unsigned, Floating    -> yield OpConvertUToF(tid, id, eid)
                        | Unsigned, Signed      -> yield OpSatConvertUToS(tid, id, eid)
                        | Unsigned, Unsigned    -> yield OpUConvert(tid, id, eid)
                        | s, t                  -> failwithf "[SpirV] unknown conversion from %A to %A" s t

                    return id

                | CLess(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdLessThan(tid, id, lid, rid)
                        | Signed -> yield OpSLessThan(tid, id, lid, rid)
                        | Unsigned -> yield OpULessThan(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CLequal(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdLessThanEqual(tid, id, lid, rid)
                        | Signed -> yield OpSLessThanEqual(tid, id, lid, rid)
                        | Unsigned -> yield OpULessThanEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CGreater(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdGreaterThan(tid, id, lid, rid)
                        | Signed -> yield OpSGreaterThan(tid, id, lid, rid)
                        | Unsigned -> yield OpUGreaterThan(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CGequal(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdGreaterThanEqual(tid, id, lid, rid)
                        | Signed -> yield OpSGreaterThanEqual(tid, id, lid, rid)
                        | Unsigned -> yield OpUGreaterThanEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CEqual(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdEqual(tid, id, lid, rid)
                        | Signed | Unsigned -> yield OpIEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CNotEqual(l, r) -> 
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    match l.ctype with
                        | Floating -> yield OpFOrdNotEqual(tid, id, lid, rid)
                        | Signed | Unsigned -> yield OpINotEqual(tid, id, lid, rid)
                        | t -> failwithf "[SpirV] cannot compare %A" t
                    return id

                | CAnd(l, r) ->
                    return! assembleExpr (CConditional(CType.CBool, l, r, CValue(CType.CBool, CBool false)))

                | COr(l, r) ->
                    return! assembleExpr (CConditional(CType.CBool, l,  CValue(CType.CBool, CBool true), r))

                | CBitAnd(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    yield OpBitwiseAnd(tid, id, lid, rid)
                    return id

                | CBitOr(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    yield OpBitwiseOr(tid, id, lid, rid)
                    return id

                | CBitXor(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    yield OpBitwiseXor(tid, id, lid, rid)
                    return id

                | CBitNot(t, v) ->
                    let! vid = assembleExpr v
                    let! id = SpirV.id
                    yield OpNot(tid, id, vid)
                    return id

                | CLeftShift(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    yield OpShiftLeftLogical(tid, id, lid, rid)
                    return id

                | CRightShift(t, l, r) ->
                    let! lid = assembleExpr l
                    let! rid = assembleExpr r
                    let! id = SpirV.id
                    yield OpShiftRightLogical(tid, id, lid, rid)
                    return id

                | CAddressOf _ ->
                    return failwith "not implemented"

                | CField(t, e, f) ->
                    let! eid = assembleExpr e
                    let! fid = SpirV.tryGetFieldId e.ctype f
                    match fid with
                        | Some fid ->
                            let! id = SpirV.id
                            //let! iid = assembleLiteral (CType.CInt(true, 32)) (CIntegral (int64 fid))
                            yield OpCompositeExtract(tid, id, eid, [|int fid|])
                            return id
                        | None ->
                            return failwithf "could not get field-id for %s on %A" f e.ctype

                | CItem(t, e, i) ->
                    let! eid = assembleExpr e
                    let! iid = assembleExpr i
                    let! id = SpirV.id
                    failwith "not implemented"
                    return id

        }

    let rec assembleRExpr (e : CRExpr) : SpirV<uint32> =
        spirv {
            match e with
                | CRExpr e -> 
                    return! assembleExpr e

                | CRArray(t, args) ->
                    let! tid = assembleType t
                    let! args = args |> List.mapS assembleExpr
                    let! id = SpirV.id
                    yield OpCompositeConstruct(tid, id, List.toArray args)
                    return id
        }

    let rec assembleLExpr (e : CLExpr) : SpirV<uint32> =
        spirv {
            let! tid = assemblePtrType StorageClass.Private e.ctype
            match e with
                | CLVar v ->
                    return! SpirV.getId v.name

                | CLExpr.CLField(_, e, f) ->
                    let! eid = assembleLExpr e
                    let! fid = SpirV.tryGetFieldId e.ctype f
                    match fid with
                        | Some fid ->
                            let! fid = assembleLiteral (CType.CInt(true, 32)) (CIntegral (int64 fid))
                            let! id = SpirV.id
                            yield OpInBoundsAccessChain(tid, id, eid, [| fid |])
                            return id
                        | None ->
                            return failwithf "[FShade] unknown field %A" f

                | CLExpr.CLItem(_, e, i) ->
                    let! eid = assembleExpr e
                    let! iid = assembleExpr i
                    let! id = SpirV.id
                    yield OpInBoundsAccessChain(tid, id, eid, [| iid |])
                    return id

                | _ ->
                    return failwith "not implemented"
                    
        }
   
    let rec assembleStatement (lBreak : Option<uint32>) (lCont : Option<uint32>) (s : CStatement) : SpirV<unit> =
        spirv {
            match s with
                | CNop ->
                    ()

                | CDo e ->
                    let! id = assembleExpr e
                    ()

                | CDeclare(v, r) ->
                    let! t = assemblePtrType StorageClass.Private v.ctype
                    let! vid = SpirV.id

                    match r with
                        | Some r ->
                            let! rid = assembleRExpr r
                            yield OpVariable(t, vid, StorageClass.Private, Some rid)
                        | None ->
                            yield OpVariable(t, vid, StorageClass.Private, None)

                    do! SpirV.setId v.name vid

                | CWrite(l, v) ->
                    let! lid = assembleLExpr l
                    let! vid = assembleExpr v
                    yield OpStore(lid, vid, None)

                | CIncrement(_, v) ->
                    let t = v.ctype
                    let! tid = assembleType t
                    let! v = assembleLExpr v
                    let! c1 = assembleLiteral t (CIntegral 1L)
                    let! id0 = SpirV.id
                    let! id1 = SpirV.id
                    yield OpLoad(tid, id0, v, None)
                    yield OpIAdd(tid, id1, id0, c1)
                    yield OpStore(v, id1, None)

                | CDecrement(_, v) ->
                    let t = v.ctype
                    let! tid = assembleType t
                    let! v = assembleLExpr v
                    let! c1 = assembleLiteral t (CIntegral 1L)
                    let! id0 = SpirV.id
                    let! id1 = SpirV.id
                    yield OpLoad(tid, id0, v, None)
                    yield OpISub(tid, id1, id0, c1)
                    yield OpStore(v, id1, None)

                | CSequential ss | CIsolated ss ->
                    for s in ss do 
                        do! assembleStatement lBreak lCont s

                | CReturnValue v ->
                    let! v = assembleExpr v
                    yield OpReturnValue v

                | CReturn ->
                    yield OpReturn

                | CBreak ->
                    match lBreak with
                        | Some lBreak -> yield OpBranch lBreak
                        | _ -> failwith "break outside loop"

                | CContinue ->
                    match lCont with
                        | Some lCont -> yield OpBranch lCont
                        | _ -> failwith "continue outside loop"

                | CWriteOutput(name, index, value) ->
                    let! vid = assembleRExpr value
                    match index with
                        | Some index -> 
                            let! id = SpirV.getId (ParameterKind.Output, name)

                            let! indexId = assembleExpr index
                            let! vt = assemblePtrType StorageClass.Output value.ctype
                            let! ptrId = SpirV.id
                            yield OpPtrAccessChain(vt, ptrId, vt, indexId , [||])

                            yield OpStore(ptrId, vid, None)
                        | None ->
                            let! id = SpirV.getId (ParameterKind.Output, name)
                            yield OpStore(id, vid, None)

                | CFor(init, cond, step, body) ->
                    do! assembleStatement lBreak lCont init

                    let! lStart = SpirV.id
                    let! lBody = SpirV.id
                    let! lEnd = SpirV.id
                    let! lStep = SpirV.id

                    yield OpLabel lStart
                    let! c = assembleExpr cond
                    yield OpBranchConditional(c, lBody, lEnd, [||])
                    yield OpLabel lBody

                    do! assembleStatement (Some lEnd) (Some lStep) body

                    yield OpLabel lStep
                    do! assembleStatement lBreak lCont step
                    yield OpBranch lStart

                    yield OpLabel lEnd

                | CWhile(guard, body) ->
                    let! lStart = SpirV.id
                    let! lBody = SpirV.id
                    let! lEnd = SpirV.id

                    yield OpLabel lStart
                    let! v = assembleExpr guard
                    yield OpBranchConditional(v, lBody, lEnd, [||])
                    yield OpLabel lBody
                    do! assembleStatement (Some lEnd) (Some lStart) body
                    yield OpBranch lStart
                    yield OpLabel lEnd

                | CDoWhile(guard, body) ->
                    let! lStart = SpirV.id
                    let! lEnd = SpirV.id

                    yield OpLabel lStart
                    do! assembleStatement (Some lEnd) (Some lStart) body
                    let! v = assembleExpr guard
                    yield OpBranchConditional(v, lStart, lEnd, [||])
                    yield OpLabel lEnd
                    
                | CIfThenElse(guard, bTrue, bFalse) ->
                    let! g = assembleExpr guard
                    let! lTrue = SpirV.id
                    let! lFalse = SpirV.id
                    let! lEnd = SpirV.id

                    yield OpBranchConditional(g, lTrue, lFalse, [||])
                    yield OpLabel lTrue
                    do! assembleStatement lBreak lCont bTrue
                    yield OpBranch lEnd

                    yield OpLabel lFalse
                    do! assembleStatement lBreak lCont bFalse
                    yield OpLabel lEnd

                | CSwitch _ ->
                    return failwith "not implemented"
        }

    let tryGetBuiltIn (kind : ParameterKind) (stage : ShaderStageDescription) (name : string) : Option<BuiltIn> =
        Log.warn "not implemented"
        None


    let assembleFunctionType (args : array<uint32>) (ret : uint32) =
        SpirV.cached ("fun", args, ret) {  
            let! id = SpirV.id
            yield OpTypeFunction(id, ret, args)
            return id
        }

    let rec assembleConstantExpr (e : CExpr) =
        spirv {
            match e with
                | CValue(t,v) ->
                    return! assembleLiteral t v

                | CNewVector(t, d, comp) ->
                    let! tid = assembleType t
                    let! comp = comp |> List.mapS assembleConstantExpr
                    let! id = SpirV.id
                    yield OpConstantComposite(tid, id, List.toArray comp)
                    return id


                | _ ->
                    return failwithf "[FShade] not a constant expression %A" e
        }

    let assembleConstantRExpr (e : CRExpr) =
        spirv {
            match e with
                | CRExpr e -> 
                    return! assembleConstantExpr e

                | CRArray(t,args) ->
                    let! t = assembleType t
                    let! args = args |> List.mapS assembleConstantExpr
                    let! id = SpirV.id
                    yield OpConstantComposite(t, id, List.toArray args)
                    return id
        }


    let assembleEntryDef (cond : Option<string>) (e : CEntryDef) =
        spirv {
            let args = e.cArguments |> List.toArray

            let! ret = assembleType e.cReturnType
            let! argTypes = args |> Array.mapS (fun a -> a.cParamType |> assembleType)
            let! fType = assembleFunctionType argTypes ret

            let mutable inputLocation = 0
            let mutable outputLocation = 0

            let stages = e.cDecorations |> List.pick (function (EntryDecoration.Stages s) -> Some s | _ -> None)
            let model =
                match stages.self with
                    | ShaderStage.Vertex -> ExecutionModel.Vertex
                    | ShaderStage.TessControl -> ExecutionModel.TessellationControl
                    | ShaderStage.TessEval -> ExecutionModel.TessellationEvaluation
                    | ShaderStage.Geometry -> ExecutionModel.Geometry
                    | ShaderStage.Fragment -> ExecutionModel.Fragment
                    | _ -> ExecutionModel.GLCompute

            let iface = System.Collections.Generic.List<uint32>()

            for i in e.cInputs do
                let! t = assembleType i.cParamType
                let! id = SpirV.id
                yield OpVariable(t, id, StorageClass.Input, None)
                yield OpName(id, i.cParamName)
                match tryGetBuiltIn ParameterKind.Input stages i.cParamName with
                    | Some b ->
                        yield OpDecorate(id, Decoration.BuiltIn, [| int b |])
                    | None ->   
                        yield OpDecorate(id, Decoration.Location, [| inputLocation |])
                        inc &inputLocation

                do! SpirV.setId (ParameterKind.Input, i.cParamName) id
                iface.Add id

            for i in e.cOutputs do
                let! t = assembleType i.cParamType
                let! id = SpirV.id
                yield OpVariable(t, id, StorageClass.Output, None)
                yield OpName(id, i.cParamName)
                match tryGetBuiltIn ParameterKind.Output stages i.cParamName with
                    | Some b ->
                        yield OpDecorate(id, Decoration.BuiltIn, [| int b |])
                    | None ->   
                        yield OpDecorate(id, Decoration.Location, [| outputLocation |])
                        inc &outputLocation

                do! SpirV.setId (ParameterKind.Output, i.cParamName) id
                iface.Add id

            let entryName =
                match cond with
                    | Some c -> c
                    | None -> e.cEntryName

            let! eid = SpirV.id

            yield OpEntryPoint(model, eid, entryName, iface.ToArray())
            yield OpFunction(ret, eid, FunctionControl.None, fType)
            yield OpName(eid, entryName)

            for ai in 0 .. argTypes.Length - 1 do
                let name = args.[ai].cParamName
                let tid = argTypes.[ai]
                let! id = SpirV.id
                yield OpFunctionParameter(tid, id)
                yield OpName(id, name)
                do! SpirV.setId (ParameterKind.Argument, name) id


            do! assembleStatement None None e.cBody

            yield OpFunctionEnd

        }

    let rec assembleValueDef (cond : Option<string>) (d : CValueDef) : SpirV<unit> =
        spirv {
            match d with
                | CValueDef.CConstant(t, name, value) ->
                    let! value = assembleConstantRExpr value
                    yield OpName(value, name)
                    do! SpirV.setId name value
                    
                | CValueDef.CUniformDef uniforms ->
                    let buffers =
                        uniforms 
                            |> List.groupBy (fun u -> u.cUniformBuffer)

                    let! set = SpirV.newSet
                    for (name, fields) in buffers do
                        match name with
                            | Some name ->
                                let! binding = SpirV.newBinding

                                let t = CStruct(name, fields |> List.map (fun f -> f.cUniformType, f.cUniformName), None)
                                let! tid = assemblePtrType StorageClass.Uniform t

                                yield OpDecorate(tid, Decoration.GLSLPacked, [||])
                                yield OpDecorate(tid, Decoration.BufferBlock, [||])

                                let! id = SpirV.id
                                yield OpVariable(tid, id, StorageClass.Uniform, None)
                                yield OpDecorate(id, Decoration.DescriptorSet, [| int set |])
                                yield OpDecorate(id, Decoration.Binding, [| int binding |])

                                let fields = List.toArray fields
                                for i in 0 .. fields.Length - 1 do
                                    let u = fields.[i]
                                    do! SpirV.setUniformId u.cUniformName id [uint32 i]


                                ()

                            | None ->
                                for f in fields do
                                    let! tid = assemblePtrType StorageClass.Uniform f.cUniformType
                                    let! binding = SpirV.newBinding
                                    let! id = SpirV.id
                                    yield OpVariable(tid, id, StorageClass.Uniform, None)
                                    yield OpDecorate(id, Decoration.DescriptorSet, [| int set |])
                                    yield OpDecorate(id, Decoration.Binding, [| int binding |])
                                    do! SpirV.setUniformId f.cUniformName id []
        
                | CValueDef.CConditionalDef(cond, inner) ->
                    for v in inner do
                        do! assembleValueDef (Some cond) v

                | CValueDef.CFunctionDef(signature, body) ->
                    let! argTypes = signature.parameters |> Array.mapS (fun p -> assembleType p.ctype)
                    let! ret = assembleType signature.returnType
                    let! tFun = assembleFunctionType argTypes ret

                    let! id = SpirV.id
                    yield OpFunction(ret, id, FunctionControl.None, tFun)
                    for i in 0 .. argTypes.Length - 1 do
                        let p = signature.parameters.[i]
                        let! id = SpirV.id
                        yield OpFunctionParameter(argTypes.[i], id)
                        do! SpirV.setId p.name id


                    do! assembleStatement None None body

                    yield OpFunctionEnd
                    do! SpirV.setId signature id

                    ()

                | CValueDef.CEntryDef e ->
                    do! assembleEntryDef cond e
        }

    let assembleModuleS (m : CModule) =
        spirv {
            let usage = CModule.usageInfo m

            // assemble all used types
            for t in usage.usedTypes do
                let! _ = assembleType t
                ()

            for v in m.values do
                do! assembleValueDef None v


        }

    let assembleModule (m : CModule) : SpirV.Module =
        let mutable state = 
            {
                currentId           = 0u
                valueIds            = HashMap.empty
                uniformIds          = Map.empty
                fieldIds            = HashMap.empty
                reversedInstuctions = []
                currentBinding      = 0u
                currentSet          = 0u
                imports             = Map.empty
            }
        assembleModuleS(m).Run(&state)
        {
            magic = 0x07230203u
            version = Version(1,0)
            generatorMagic = (0xFADEu <<< 16) ||| 1u
            bound = state.currentId
            reserved = 0u
            instructions = state.reversedInstuctions |> List.rev
        }


[<AutoOpen>]
module private TextureFunctions =
    open Aardvark.Base.TypeInfo.Patterns

    let (|TextureLookup|_|) (mi : MethodInfo) : Option<CIntrinsic> =
        match mi with
            | Method(name, ((ImageType(_, dim, isArray, isMS, valueType)::args))) ->
                Log.warn "image functions not implemented"
                None    

            | Method(name, ((SamplerType(dim, isArray, isShadow, isMS, valueType)::args))) ->
                let argCount = List.length args

                let coordDim =
                    match dim with
                        | SamplerDimension.Sampler1d -> 1
                        | SamplerDimension.Sampler2d -> 2
                        | SamplerDimension.Sampler3d -> 3
                        | SamplerDimension.SamplerCube -> 3
                        | _ -> failwithf "unknown sampler dimension %A" dim

                let (|Coord|_|) =
                    if coordDim = 1 then fun (t : Type) -> Some t
                    else 
                        fun (t : Type) ->
                            match t with
                                | VectorOf(d, t) when d = coordDim -> Some t
                                | _ -> None

                let (|CoordProj|_|) (t : Type) =
                    match t with
                        | VectorOf(d, t) when d = coordDim + 1 -> Some t
                        | _ -> None

                match name, args with
                    // Size
                    | "get_Size", [] -> 
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageQuerySize(tid, id, args.[0])
                                return id
                            }
                            
                    // GetSize(level)
                    | "GetSize", [ Int32 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageQuerySizeLod(tid, id, args.[0], args.[1])
                                return id
                            }
                        
                    // Read(coord)
                    | "get_Item", [ Coord Int32 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageRead(tid, id, args.[0], args.[1], None, [||])
                                return id
                            }
                            
                    // Read(coord, level)
                    | ("get_Item" | "Read"), [ Coord Int32; Int32 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageRead(tid, id, args.[0], args.[1], Some ImageOperands.Lod, [| args.[2] |])
                                return id
                            }
                        
                    // Read(coord, slice, level)
                    | ("get_Item" | "Read"), [ Coord Int32; Int32; Int32 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! cid = SpirV.id
                                let! realCoordType = Assembler.assembleType (CVector(CInt(true, 32), coordDim + 1))
                                yield OpCompositeConstruct(realCoordType, cid, [| args.[1]; args.[2] |])

                                let! id = SpirV.id
                                yield OpImageRead(tid, id, args.[0], cid, Some ImageOperands.Lod, [| args.[3] |])
                                return id
                            }
                        
                    // MipMapLevels
                    | "get_MipMapLevels", [] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageQueryLevels(tid, id, args.[0])
                                return id
                            }
                        
                    
                    // Sample(coord)
                    | "Sample", [ Coord Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageSampleImplicitLod(tid, id, args.[0], args.[1], None, [||])
                                return id
                            }
                        
                    // Sample(coord, bias)
                    | "Sample", [ Coord Float64; Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageSampleImplicitLod(tid, id, args.[0], args.[1], Some ImageOperands.Bias, [| args.[2] |])
                                return id
                            }

                    // Sample(coord, slice)
                    | "Sample", [ Coord Float64; Int32 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! cid = SpirV.id
                                let! realCoordType = Assembler.assembleType (CVector(CFloat 64, coordDim + 1))
                                yield OpCompositeConstruct(realCoordType, cid, [| args.[1]; args.[2] |])

                                let! id = SpirV.id
                                yield OpImageSampleImplicitLod(tid, id, args.[0], cid, None, [||])
                                return id
                            }
                            
                    // Sample(coord, slice, bias)
                    | "Sample", [ Coord Float64; Int32; Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! cid = SpirV.id
                                let! realCoordType = Assembler.assembleType (CVector(CFloat 64, coordDim + 1))
                                yield OpCompositeConstruct(realCoordType, cid, [| args.[1]; args.[2] |])

                                let! id = SpirV.id
                                yield OpImageSampleImplicitLod(tid, id, args.[0], cid, Some ImageOperands.Bias, [| args.[3] |])
                                return id
                            }

                    // SampleOffset(coord, offset)
                    | "SampleOffset", [ Coord Float64; Coord Int32 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageSampleImplicitLod(tid, id, args.[0], args.[1], Some ImageOperands.Offset, [| args.[2] |])
                                return id
                            }
                            
                    // SampleOffset(coord, offset, bias)
                    | "SampleOffset", [ Coord Float64; Coord Int32; Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageSampleImplicitLod(tid, id, args.[0], args.[1], Some (ImageOperands.Bias ||| ImageOperands.Offset), [| args.[3]; args.[2] |])
                                return id
                            }
                        
                    // SampleOffset(coord, slice, offset)
                    | "SampleOffset", [ Coord Float64; Int32; Coord Int32 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! cid = SpirV.id
                                let! realCoordType = Assembler.assembleType (CVector(CFloat 64, coordDim + 1))
                                yield OpCompositeConstruct(realCoordType, cid, [| args.[1]; args.[2] |])

                                let! id = SpirV.id
                                yield OpImageSampleImplicitLod(tid, id, args.[0], cid, Some ImageOperands.Offset, [| args.[3] |])
                                return id
                            }

                    // SampleOffset(coord, slice, offset, bias)
                    | "SampleOffset", [ Coord Float64; Int32; Coord Int32; Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! cid = SpirV.id
                                let! realCoordType = Assembler.assembleType (CVector(CFloat 64, coordDim + 1))
                                yield OpCompositeConstruct(realCoordType, cid, [| args.[1]; args.[2] |])

                                let! id = SpirV.id
                                yield OpImageSampleImplicitLod(tid, id, args.[0], cid, Some (ImageOperands.Bias ||| ImageOperands.Offset), [| args.[4]; args.[3] |])
                                return id
                            }
                           
                    // SampleProj(coord) 
                    | "SampleProj", [ CoordProj Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageSampleProjImplicitLod(tid, id, args.[0], args.[1], None, [||])
                                return id
                            }
                        
                    // SampleProj(coord, bias) 
                    | "SampleProj", [ CoordProj Float64; Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageSampleProjImplicitLod(tid, id, args.[0], args.[1], Some ImageOperands.Bias, [| args.[2] |])
                                return id
                            }

                    // SampleLevel(coord, level)
                    | "SampleLevel", [ Coord Float64; Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageSampleExplicitLod(tid, id, args.[0], args.[1], ImageOperands.Lod, [| args.[2] |])
                                return id
                            }

                    // SampleLevel(coord, slice, level)
                    | "SampleLevel", [ Coord Float64; Int32; Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! cid = SpirV.id
                                let! realCoordType = Assembler.assembleType (CVector(CFloat 64, coordDim + 1))
                                yield OpCompositeConstruct(realCoordType, cid, [| args.[1]; args.[2] |])

                                let! id = SpirV.id
                                yield OpImageSampleExplicitLod(tid, id, args.[0], cid, ImageOperands.Lod, [| args.[3] |])
                                return id
                            }

                    // SampleGrad(coord, dx, dy)
                    | "SampleGrad", [ Coord Float64; Coord Float64; Coord Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageSampleExplicitLod(tid, id, args.[0], args.[1], ImageOperands.Grad, [| args.[2]; args.[3] |])
                                return id
                            }

                    // SampleGrad(coord, slice, dx, dy)
                    | "SampleGrad", [ Coord Float64; Int32; Coord Float64; Coord Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! cid = SpirV.id
                                let! realCoordType = Assembler.assembleType (CVector(CFloat 64, coordDim + 1))
                                yield OpCompositeConstruct(realCoordType, cid, [| args.[1]; args.[2] |])

                                let! id = SpirV.id
                                yield OpImageSampleExplicitLod(tid, id, args.[0], cid, ImageOperands.Grad, [| args.[3]; args.[4] |])
                                return id
                            }

                    // QueryLod(coord)
                    | "QueryLod", [ Coord Float64 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageQueryLod(tid, id, args.[0], args.[1])
                                return id
                            }

                    // Gather(coord, comp)
                    | "Gather", [ Coord Float64; Int32 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageGather(tid, id, args.[0], args.[1], args.[2], None, [||])
                                return id
                            }

                    // Gather(coord, slice, comp)
                    | "Gather", [ Coord Float64; Int32; Int32 ] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! cid = SpirV.id
                                let! realCoordType = Assembler.assembleType (CVector(CFloat 64, coordDim + 1))
                                yield OpCompositeConstruct(realCoordType, cid, [| args.[1]; args.[2] |])

                                let! id = SpirV.id
                                yield OpImageGather(tid, id, args.[0], cid, args.[3], None, [||])
                                return id
                            }
                            
                    // GatherOffset(coord, offset, comp)
                    | "GatherOffset", [ Coord Float64; Coord Int32; Int32] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! id = SpirV.id
                                yield OpImageGather(tid, id, args.[0], args.[1], args.[3], Some ImageOperands.Offset, [| args.[2] |])
                                return id
                            }  

                    // GatherOffset(coord, slice, offset, comp)
                    | "GatherOffset", [ Coord Float64; Int32; Coord Int32; Int32] ->
                        CIntrinsic.ofFunction <| fun tid args ->
                            spirv {
                                let! cid = SpirV.id
                                let! realCoordType = Assembler.assembleType (CVector(CFloat 64, coordDim + 1))
                                yield OpCompositeConstruct(realCoordType, cid, [| args.[1]; args.[2] |])

                                let! id = SpirV.id
                                yield OpImageGather(tid, id, args.[0], cid, args.[4], Some ImageOperands.Offset, [| args.[3] |])
                                return id
                            }
                        

                    | _ ->
                        None

            | _ ->
                None
   



open Aardvark.Base.TypeInfo.Patterns

type Backend private() =
    inherit Compiler.Backend()
    static let instance = Backend()

    static let rec compileSimpleType (t : Type) =
        match t with
            | VectorOf(d,et) -> 
                let et = compileSimpleType et
                CVector(et, d)

            | Float32 | Float64 -> CFloat(32)
            | Int32 -> CInt(true, 32)


            | _ ->
                failwith "unexpected sampled type"

    static member Instance = instance

    override x.TryGetIntrinsicMethod (c : MethodInfo) =
        match c with
            | InstrinsicFunction ci -> Some ci
            | TextureLookup ci -> Some ci
            | _ -> None

    override x.TryGetIntrinsicCtor (c : ConstructorInfo) =
        None

    override x.TryGetIntrinsicType (t : Type) =
        match t with
            | ImageType(fmt, dim, arr, ms, valueType) -> 
                failwith ""

            | SamplerType(dim, arr, shadow, ms, valueType) -> 
                let valueType = compileSimpleType valueType
                let compile = 
                    SpirV.cached t {
                        let! st = Assembler.assembleType valueType

                        let dim =
                            match dim with
                                | SamplerDimension.Sampler1d -> Dim.Dim1D
                                | SamplerDimension.Sampler2d -> Dim.Dim2D
                                | SamplerDimension.Sampler3d -> Dim.Dim3D
                                | SamplerDimension.SamplerCube -> Dim.Cube
                                | _ -> failwithf "unknown texture dimension: %A" dim

                        let! iid = SpirV.id
                        yield OpTypeImage(iid, st, dim, (if shadow then 1 else 0), (if arr then 1 else 0), (if ms then 1 else 0), 1, ImageFormat.Unknown, None)

                        let! sid = SpirV.id
                        yield OpTypeSampledImage(sid, iid)

                        return sid
                    }
                CIntrinsicType.tagged { compileType = compile } |> Some
            | _ ->
                None