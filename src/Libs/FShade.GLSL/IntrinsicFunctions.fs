namespace FShade.GLSL.Utilities


open System
open System.Reflection

open Aardvark.Base

open FShade
open FShade.Imperative


[<AutoOpen>]
module IntrinsicFunctions =
    let (|IntrinsicFunction|_|) : MethodInfo -> Option<CIntrinsic> =
        MethodTable.ofList [
            // ==========================================================================
            // ANGLES & TRIGONOMETRIC
            // ==========================================================================

            CIntrinsic.simple "degrees", [
                exactly <@ Conversion.DegreesFromRadians : float -> _ @>
                exactly <@ Conversion.DegreesFromRadians : float32 -> _ @>
                exactly <@ Conversion.DegreesFromRadians : V2f -> _ @>
                exactly <@ Conversion.DegreesFromRadians : V3f -> _ @>
                exactly <@ Conversion.DegreesFromRadians : V4f -> _ @>
                exactly <@ Conversion.DegreesFromRadians : V2d -> _ @>
                exactly <@ Conversion.DegreesFromRadians : V3d -> _ @>
                exactly <@ Conversion.DegreesFromRadians : V4d -> _ @>
                generic <@ degrees : float -> _ @>
            ]

            CIntrinsic.simple "radians", [
                exactly <@ Conversion.RadiansFromDegrees : float -> _ @>
                exactly <@ Conversion.RadiansFromDegrees : float32 -> _ @>
                exactly <@ Conversion.RadiansFromDegrees : V2f -> _ @>
                exactly <@ Conversion.RadiansFromDegrees : V3f -> _ @>
                exactly <@ Conversion.RadiansFromDegrees : V4f -> _ @>
                exactly <@ Conversion.RadiansFromDegrees : V2d -> _ @>
                exactly <@ Conversion.RadiansFromDegrees : V3d -> _ @>
                exactly <@ Conversion.RadiansFromDegrees : V4d -> _ @>
                generic <@ radians : float -> _ @>
            ]

            CIntrinsic.simple "sin", [
                exactly <@ Math.Sin @>
                exactly <@ Fun.Sin : float -> float @>
                exactly <@ Fun.Sin : float32 -> float32 @>
                exactly <@ Fun.Sin : V2f -> V2f @>
                exactly <@ Fun.Sin : V3f -> V3f @>
                exactly <@ Fun.Sin : V4f -> V4f @>
                exactly <@ Fun.Sin : V2d -> V2d @>
                exactly <@ Fun.Sin : V3d -> V3d @>
                exactly <@ Fun.Sin : V4d -> V4d @>
                generic <@ sin @>
            ]

            CIntrinsic.simple "cos", [
                exactly <@ Math.Cos @>
                exactly <@ Fun.Cos : float -> float @>
                exactly <@ Fun.Cos : float32 -> float32 @>
                exactly <@ Fun.Cos : V2f -> V2f @>
                exactly <@ Fun.Cos : V3f -> V3f @>
                exactly <@ Fun.Cos : V4f -> V4f @>
                exactly <@ Fun.Cos : V2d -> V2d @>
                exactly <@ Fun.Cos : V3d -> V3d @>
                exactly <@ Fun.Cos : V4d -> V4d @>
                generic <@ cos @>
            ]

            CIntrinsic.simple "tan", [
                exactly <@ Math.Tan @>
                exactly <@ Fun.Tan : float -> float @>
                exactly <@ Fun.Tan : float32 -> float32 @>
                exactly <@ Fun.Tan : V2f -> V2f @>
                exactly <@ Fun.Tan : V3f -> V3f @>
                exactly <@ Fun.Tan : V4f -> V4f @>
                exactly <@ Fun.Tan : V2d -> V2d @>
                exactly <@ Fun.Tan : V3d -> V3d @>
                exactly <@ Fun.Tan : V4d -> V4d @>
                generic <@ tan @>
            ]

            CIntrinsic.simple "asin", [
                exactly <@ Math.Asin @>
                exactly <@ Fun.Asin : float -> float @>
                exactly <@ Fun.Asin : float32 -> float32 @>
                exactly <@ Fun.Asin : V2f -> V2f @>
                exactly <@ Fun.Asin : V3f -> V3f @>
                exactly <@ Fun.Asin : V4f -> V4f @>
                exactly <@ Fun.Asin : V2d -> V2d @>
                exactly <@ Fun.Asin : V3d -> V3d @>
                exactly <@ Fun.Asin : V4d -> V4d @>
                generic <@ asin @>
            ]

            CIntrinsic.simple "acos", [
                exactly <@ Math.Acos @>
                exactly <@ Fun.Acos : float -> float @>
                exactly <@ Fun.Acos : float32 -> float32 @>
                exactly <@ Fun.Acos : V2f -> V2f @>
                exactly <@ Fun.Acos : V3f -> V3f @>
                exactly <@ Fun.Acos : V4f -> V4f @>
                exactly <@ Fun.Acos : V2d -> V2d @>
                exactly <@ Fun.Acos : V3d -> V3d @>
                exactly <@ Fun.Acos : V4d -> V4d @>
                generic <@ acos @>
            ]

            CIntrinsic.simple "atan", [
                exactly <@ Math.Atan @>
                exactly <@ Fun.Atan : float -> float @>
                exactly <@ Fun.Atan : float32 -> float32 @>
                exactly <@ Fun.Atan : V2f -> V2f @>
                exactly <@ Fun.Atan : V3f -> V3f @>
                exactly <@ Fun.Atan : V4f -> V4f @>
                exactly <@ Fun.Atan : V2d -> V2d @>
                exactly <@ Fun.Atan : V3d -> V3d @>
                exactly <@ Fun.Atan : V4d -> V4d @>
                generic <@ atan @>

                exactly <@ Math.Atan2 @>
                exactly <@ Fun.Atan2 : float * float -> float @>
                exactly <@ Fun.Atan2 : float32 * float32 -> float32 @>
                exactly <@ Fun.Atan2 : V2f * V2f -> V2f @>
                exactly <@ Fun.Atan2 : V3f * V3f -> V3f @>
                exactly <@ Fun.Atan2 : V4f * V4f -> V4f @>
                exactly <@ Fun.Atan2 : V2d * V2d -> V2d @>
                exactly <@ Fun.Atan2 : V3d * V3d -> V3d @>
                exactly <@ Fun.Atan2 : V4d * V4d -> V4d @>
                generic <@ atan2 @>
            ]

            CIntrinsic.simple "sinh", [
                exactly <@ Math.Sinh @>
                exactly <@ Fun.Sinh : float -> float @>
                exactly <@ Fun.Sinh : float32 -> float32 @>
                exactly <@ Fun.Sinh : V2f -> V2f @>
                exactly <@ Fun.Sinh : V3f -> V3f @>
                exactly <@ Fun.Sinh : V4f -> V4f @>
                exactly <@ Fun.Sinh : V2d -> V2d @>
                exactly <@ Fun.Sinh : V3d -> V3d @>
                exactly <@ Fun.Sinh : V4d -> V4d @>
                generic <@ sinh @>
            ]

            CIntrinsic.simple "cosh", [
                exactly <@ Math.Cosh @>
                exactly <@ Fun.Cosh : float -> float @>
                exactly <@ Fun.Cosh : float32 -> float32 @>
                exactly <@ Fun.Cosh : V2f -> V2f @>
                exactly <@ Fun.Cosh : V3f -> V3f @>
                exactly <@ Fun.Cosh : V4f -> V4f @>
                exactly <@ Fun.Cosh : V2d -> V2d @>
                exactly <@ Fun.Cosh : V3d -> V3d @>
                exactly <@ Fun.Cosh : V4d -> V4d @>
                generic <@ cosh @>
            ]

            CIntrinsic.simple "tanh", [
                exactly <@ Math.Tanh @>
                exactly <@ Fun.Tanh : float -> float @>
                exactly <@ Fun.Tanh : float32 -> float32 @>
                exactly <@ Fun.Tanh : V2f -> V2f @>
                exactly <@ Fun.Tanh : V3f -> V3f @>
                exactly <@ Fun.Tanh : V4f -> V4f @>
                exactly <@ Fun.Tanh : V2d -> V2d @>
                exactly <@ Fun.Tanh : V3d -> V3d @>
                exactly <@ Fun.Tanh : V4d -> V4d @>
                generic <@ tanh @>
            ]

            CIntrinsic.simple "asinh", [
                exactly <@ Fun.Asinh : float -> float @>
                exactly <@ Fun.Asinh : float32 -> float32 @>
                exactly <@ Fun.Asinh : V2f -> V2f @>
                exactly <@ Fun.Asinh : V3f -> V3f @>
                exactly <@ Fun.Asinh : V4f -> V4f @>
                exactly <@ Fun.Asinh : V2d -> V2d @>
                exactly <@ Fun.Asinh : V3d -> V3d @>
                exactly <@ Fun.Asinh : V4d -> V4d @>
                generic <@ asinh : float -> float @>
            ]

            CIntrinsic.simple "acosh", [
                exactly <@ Fun.Acosh : float -> float @>
                exactly <@ Fun.Acosh : float32 -> float32 @>
                exactly <@ Fun.Acosh : V2f -> V2f @>
                exactly <@ Fun.Acosh : V3f -> V3f @>
                exactly <@ Fun.Acosh : V4f -> V4f @>
                exactly <@ Fun.Acosh : V2d -> V2d @>
                exactly <@ Fun.Acosh : V3d -> V3d @>
                exactly <@ Fun.Acosh : V4d -> V4d @>
                generic <@ acosh : float -> float @>
            ]

            CIntrinsic.simple "atanh", [
                exactly <@ Fun.Atanh : float -> float @>
                exactly <@ Fun.Atanh : float32 -> float32 @>
                exactly <@ Fun.Atanh : V2f -> V2f @>
                exactly <@ Fun.Atanh : V3f -> V3f @>
                exactly <@ Fun.Atanh : V4f -> V4f @>
                exactly <@ Fun.Atanh : V2d -> V2d @>
                exactly <@ Fun.Atanh : V3d -> V3d @>
                exactly <@ Fun.Atanh : V4d -> V4d @>
                generic <@ atanh : float -> float @>
            ]

            // ==========================================================================
            // EXPONENTIAL
            // ==========================================================================
            CIntrinsic.simple "pow", [
                exactly <@ Math.Pow @>

                exactly <@ Fun.Pow : float  * float -> float @>
                exactly <@ Fun.Pow : int8   * float -> float @>
                exactly <@ Fun.Pow : int16  * float -> float @>
                exactly <@ Fun.Pow : int32  * float -> float @>
                exactly <@ Fun.Pow : uint8  * float -> float @>
                exactly <@ Fun.Pow : uint16 * float -> float @>
                exactly <@ Fun.Pow : uint32 * float -> float @>
                exactly <@ Fun.Pow : float32 * float32 -> float32 @>
                exactly <@ Fun.Pow : int8    * float32 -> float32 @>
                exactly <@ Fun.Pow : int16   * float32 -> float32 @>
                exactly <@ Fun.Pow : int32   * float32 -> float32 @>
                exactly <@ Fun.Pow : uint8   * float32 -> float32 @>
                exactly <@ Fun.Pow : uint16  * float32 -> float32 @>
                exactly <@ Fun.Pow : uint32  * float32 -> float32 @>

                exactly <@ Fun.Pow : V2i * V2d -> V2d @>
                exactly <@ Fun.Pow : V3i * V3d -> V3d @>
                exactly <@ Fun.Pow : V4i * V4d -> V4d @>
                exactly <@ Fun.Pow : V2i * V2f -> V2f @>
                exactly <@ Fun.Pow : V3i * V3f -> V3f @>
                exactly <@ Fun.Pow : V4i * V4f -> V4f @>

                exactly <@ Fun.Pow : V2l * V2d -> V2d @>
                exactly <@ Fun.Pow : V3l * V3d -> V3d @>
                exactly <@ Fun.Pow : V4l * V4d -> V4d @>
                exactly <@ Fun.Pow : V2l * V2f -> V2f @>
                exactly <@ Fun.Pow : V3l * V3f -> V3f @>
                exactly <@ Fun.Pow : V4l * V4f -> V4f @>

                exactly <@ Fun.Pow : V2d * V2d -> V2d @>
                exactly <@ Fun.Pow : V2f * V2f -> V2f @>
                exactly <@ Fun.Pow : V3d * V3d -> V3d @>
                exactly <@ Fun.Pow : V3f * V3f -> V3f @>
                exactly <@ Fun.Pow : V4d * V4d -> V4d @>
                exactly <@ Fun.Pow : V4f * V4f -> V4f @>

                generic <@ Operators.( ** ) : float -> float -> float @>
                generic <@ ( ** ) : float -> float -> float @>
                generic <@ pow : float -> float -> float @>
            ]

            CIntrinsic.simple "pow", [
                exactly <@ Fun.Pown : int8    * int8   -> int8 @>
                exactly <@ Fun.Pown : int16   * int16  -> int16 @>
                exactly <@ Fun.Pown : int64   * int64  -> int64 @>
                exactly <@ Fun.Pown : uint8   * uint8  -> uint8 @>
                exactly <@ Fun.Pown : uint16  * uint16 -> uint16 @>
                exactly <@ Fun.Pown : uint32  * uint32 -> uint32 @>
                exactly <@ Fun.Pown : uint64  * uint64 -> uint64 @>
                exactly <@ Fun.Pown : int8    * int32  -> int8 @>
                exactly <@ Fun.Pown : int16   * int32  -> int16 @>
                exactly <@ Fun.Pown : int32   * int32  -> int32 @>
                exactly <@ Fun.Pown : int64   * int32  -> int64 @>
                exactly <@ Fun.Pown : uint8   * int32  -> uint8 @>
                exactly <@ Fun.Pown : uint16  * int32  -> uint16 @>
                exactly <@ Fun.Pown : uint32  * int32  -> uint32 @>
                exactly <@ Fun.Pown : uint64  * int32  -> uint64 @>
                exactly <@ Fun.Pown : float   * int32  -> float @>
                exactly <@ Fun.Pown : float32 * int32  -> float32 @>

                exactly <@ Fun.Pown : V2f * V2i  -> V2f @>
                exactly <@ Fun.Pown : V3f * V3i  -> V3f @>
                exactly <@ Fun.Pown : V4f * V4i  -> V4f @>
                exactly <@ Fun.Pown : V2d * V2i  -> V2d @>
                exactly <@ Fun.Pown : V3d * V3i  -> V3d @>
                exactly <@ Fun.Pown : V4d * V4i  -> V4d @>
                exactly <@ Fun.Pown : V2i * V2i  -> V2i @>
                exactly <@ Fun.Pown : V3i * V3i  -> V3i @>
                exactly <@ Fun.Pown : V4i * V4i  -> V4i @>
                exactly <@ Fun.Pown : V2l * V2i  -> V2l @>
                exactly <@ Fun.Pown : V3l * V3i  -> V3l @>
                exactly <@ Fun.Pown : V4l * V4i  -> V4l @>
                exactly <@ Fun.Pown : V2l * V2l  -> V2l @>
                exactly <@ Fun.Pown : V3l * V3l  -> V3l @>
                exactly <@ Fun.Pown : V4l * V4l  -> V4l @>

                generic <@ pown : int -> int -> _ @>
            ]

            CIntrinsic.tagged "pow({0}, vec2({1}))", [
                exactly <@ Fun.Pow  : V2f * float32 -> V2f @>
                exactly <@ Fun.Pow  : V2d * float   -> V2d @>
                exactly <@ Fun.Pow  : V2i * float   -> V2d @>
                exactly <@ Fun.Pow  : V2l * float   -> V2d @>
                exactly <@ Fun.Pow  : V2i * float32 -> V2f @>
                exactly <@ Fun.Pow  : V2l * float32 -> V2f @>

                exactly <@ Fun.Pown : V2f * int32   -> V2f @>
                exactly <@ Fun.Pown : V2d * int32   -> V2d @>
                exactly <@ Fun.Pown : V2i * int32   -> V2i @>
                exactly <@ Fun.Pown : V2l * int32   -> V2l @>
                exactly <@ Fun.Pown : V2l * int64   -> V2l @>

                exactly <@ pow    : V2f -> float32 -> V2f @>
                exactly <@ pow    : V2d -> float   -> V2d @>
                exactly <@ ( ** ) : V2f -> float32 -> V2f @>
                exactly <@ ( ** ) : V2d -> float   -> V2d @>

                exactly <@ pown : V2f -> int32   -> V2f @>
                exactly <@ pown : V2d -> int32   -> V2d @>
                exactly <@ pown : V2i -> int32   -> V2i @>
                exactly <@ pown : V2l -> int32   -> V2l @>
                exactly <@ pown : V2l -> int64   -> V2l @>
            ]

            CIntrinsic.tagged "pow({0}, vec3({1}))", [
                exactly <@ Fun.Pow  : V3f * float32 -> V3f @>
                exactly <@ Fun.Pow  : V3d * float   -> V3d @>
                exactly <@ Fun.Pow  : V3i * float   -> V3d @>
                exactly <@ Fun.Pow  : V3l * float   -> V3d @>
                exactly <@ Fun.Pow  : V3i * float32 -> V3f @>
                exactly <@ Fun.Pow  : V3l * float32 -> V3f @>

                exactly <@ Fun.Pown : V3f * int32   -> V3f @>
                exactly <@ Fun.Pown : V3d * int32   -> V3d @>
                exactly <@ Fun.Pown : V3i * int32   -> V3i @>
                exactly <@ Fun.Pown : V3l * int32   -> V3l @>
                exactly <@ Fun.Pown : V3l * int64   -> V3l @>

                exactly <@ pow    : V3f -> float32 -> _ @>
                exactly <@ pow    : V3d -> float   -> _ @>
                exactly <@ ( ** ) : V3f -> float32 -> _ @>
                exactly <@ ( ** ) : V3d -> float   -> _ @>

                exactly <@ pown : V3f -> int32   -> V3f @>
                exactly <@ pown : V3d -> int32   -> V3d @>
                exactly <@ pown : V3i -> int32   -> V3i @>
                exactly <@ pown : V3l -> int32   -> V3l @>
                exactly <@ pown : V3l -> int64   -> V3l @>
            ]

            CIntrinsic.tagged "pow({0}, vec4({1}))", [
                exactly <@ Fun.Pow  : V4f * float32 -> V4f @>
                exactly <@ Fun.Pow  : V4d * float   -> V4d @>
                exactly <@ Fun.Pow  : V4i * float   -> V4d @>
                exactly <@ Fun.Pow  : V4l * float   -> V4d @>
                exactly <@ Fun.Pow  : V4i * float32 -> V4f @>
                exactly <@ Fun.Pow  : V4l * float32 -> V4f @>

                exactly <@ Fun.Pown : V4f * int32   -> V4f @>
                exactly <@ Fun.Pown : V4d * int32   -> V4d @>
                exactly <@ Fun.Pown : V4i * int32   -> V4i @>
                exactly <@ Fun.Pown : V4l * int32   -> V4l @>
                exactly <@ Fun.Pown : V4l * int64   -> V4l @>

                exactly <@ pow    : V4f -> float32 -> _ @>
                exactly <@ pow    : V4d -> float   -> _ @>
                exactly <@ ( ** ) : V4f -> float32 -> _ @>
                exactly <@ ( ** ) : V4d -> float   -> _ @>

                exactly <@ pown : V4f -> int32   -> V4f @>
                exactly <@ pown : V4d -> int32   -> V4d @>
                exactly <@ pown : V4i -> int32   -> V4i @>
                exactly <@ pown : V4l -> int32   -> V4l @>
                exactly <@ pown : V4l -> int64   -> V4l @>
            ]

            CIntrinsic.tagged "pow(vec2({0}), {1})", [
                exactly <@ Fun.Pow  : float32 * V2f -> V2f @>
                exactly <@ Fun.Pow  : float   * V2d -> V2d @>
                exactly <@ Fun.Pow  : int32   * V2d -> V2d @>
                exactly <@ Fun.Pow  : int64   * V2d -> V2d @>

                exactly <@ Fun.Pown : float   * V2i -> V2d @>
                exactly <@ Fun.Pown : float32 * V2i -> V2f @>
                exactly <@ Fun.Pown : int32   * V2i -> V2i @>
                exactly <@ Fun.Pown : int64   * V2i -> V2l @>
                exactly <@ Fun.Pown : int64   * V2l -> V2l @>
            ]

            CIntrinsic.tagged "pow(vec3({0}), {1})", [
                exactly <@ Fun.Pow  : float32 * V3f -> V3f @>
                exactly <@ Fun.Pow  : float   * V3d -> V3d @>
                exactly <@ Fun.Pow  : int32   * V3d -> V3d @>
                exactly <@ Fun.Pow  : int64   * V3d -> V3d @>

                exactly <@ Fun.Pown : float   * V3i -> V3d @>
                exactly <@ Fun.Pown : float32 * V3i -> V3f @>
                exactly <@ Fun.Pown : int32   * V3i -> V3i @>
                exactly <@ Fun.Pown : int64   * V3i -> V3l @>
                exactly <@ Fun.Pown : int64   * V3l -> V3l @>
            ]

            CIntrinsic.tagged "pow(vec4({0}), {1})", [
                exactly <@ Fun.Pow  : float32 * V4f -> V4f @>
                exactly <@ Fun.Pow  : float   * V4d -> V4d @>
                exactly <@ Fun.Pow  : int32   * V4d -> V4d @>
                exactly <@ Fun.Pow  : int64   * V4d -> V4d @>

                exactly <@ Fun.Pown : float   * V4i -> V4d @>
                exactly <@ Fun.Pown : float32 * V4i -> V4f @>
                exactly <@ Fun.Pown : int32   * V4i -> V4i @>
                exactly <@ Fun.Pown : int64   * V4i -> V4l @>
                exactly <@ Fun.Pown : int64   * V4l -> V4l @>
            ]

            CIntrinsic.simple "exp2", [
                exactly <@ Fun.PowerOfTwo : float32 -> float32 @>
                exactly <@ Fun.PowerOfTwo : float   -> float @>
                exactly <@ Fun.PowerOfTwo : V2f -> V2f @>
                exactly <@ Fun.PowerOfTwo : V2d -> V2d @>
                exactly <@ Fun.PowerOfTwo : V3f -> V3f @>
                exactly <@ Fun.PowerOfTwo : V3d -> V3d @>
                exactly <@ Fun.PowerOfTwo : V4f -> V4f @>
                exactly <@ Fun.PowerOfTwo : V4d -> V4d @>
                generic <@ exp2 : float -> float @>
            ]

            CIntrinsic.simple "exp", [
                exactly <@ Math.Exp @>
                exactly <@ Fun.Exp : float32 -> float32 @>
                exactly <@ Fun.Exp : float   -> float @>
                exactly <@ Fun.Exp : int8    -> float @>
                exactly <@ Fun.Exp : int16   -> float @>
                exactly <@ Fun.Exp : int32   -> float @>
                exactly <@ Fun.Exp : int64   -> float @>
                exactly <@ Fun.Exp : uint8   -> float @>
                exactly <@ Fun.Exp : uint16  -> float @>
                exactly <@ Fun.Exp : uint32  -> float @>
                exactly <@ Fun.Exp : uint64  -> float @>
                exactly <@ Fun.Exp : V2f -> V2f @>
                exactly <@ Fun.Exp : V2d -> V2d @>
                exactly <@ Fun.Exp : V2i -> V2d @>
                exactly <@ Fun.Exp : V2l -> V2d @>
                exactly <@ Fun.Exp : V3f -> V3f @>
                exactly <@ Fun.Exp : V3d -> V3d @>
                exactly <@ Fun.Exp : V3i -> V3d @>
                exactly <@ Fun.Exp : V3l -> V3d @>
                exactly <@ Fun.Exp : V4f -> V4f @>
                exactly <@ Fun.Exp : V4d -> V4d @>
                exactly <@ Fun.Exp : V4i -> V4d @>
                exactly <@ Fun.Exp : V4l -> V4d @>
                generic <@ exp @>
            ]

            CIntrinsic.simple "log", [
                exactly <@ Math.Log @>
                exactly <@ Fun.Log : float32 -> float32 @>
                exactly <@ Fun.Log : float   -> float @>
                exactly <@ Fun.Log : int8    -> float @>
                exactly <@ Fun.Log : int16   -> float @>
                exactly <@ Fun.Log : int32   -> float @>
                exactly <@ Fun.Log : int64   -> float @>
                exactly <@ Fun.Log : uint8   -> float @>
                exactly <@ Fun.Log : uint16  -> float @>
                exactly <@ Fun.Log : uint32  -> float @>
                exactly <@ Fun.Log : uint64  -> float @>
                exactly <@ Fun.Log : V2f -> V2f @>
                exactly <@ Fun.Log : V2d -> V2d @>
                exactly <@ Fun.Log : V2i -> V2d @>
                exactly <@ Fun.Log : V2l -> V2d @>
                exactly <@ Fun.Log : V3f -> V3f @>
                exactly <@ Fun.Log : V3d -> V3d @>
                exactly <@ Fun.Log : V3i -> V3d @>
                exactly <@ Fun.Log : V3l -> V3d @>
                exactly <@ Fun.Log : V4f -> V4f @>
                exactly <@ Fun.Log : V4d -> V4d @>
                exactly <@ Fun.Log : V4i -> V4d @>
                exactly <@ Fun.Log : V4l -> V4d @>
                generic <@ log @>
            ]

            CIntrinsic.simple "log2", [
                exactly <@ Fun.Log2 : float32 -> float32 @>
                exactly <@ Fun.Log2 : float   -> float @>
                exactly <@ Fun.Log2 : int8    -> float @>
                exactly <@ Fun.Log2 : int16   -> float @>
                exactly <@ Fun.Log2 : int32   -> float @>
                exactly <@ Fun.Log2 : int64   -> float @>
                exactly <@ Fun.Log2 : uint8   -> float @>
                exactly <@ Fun.Log2 : uint16  -> float @>
                exactly <@ Fun.Log2 : uint32  -> float @>
                exactly <@ Fun.Log2 : uint64  -> float @>
                exactly <@ Fun.Log2 : V2f -> V2f @>
                exactly <@ Fun.Log2 : V2d -> V2d @>
                exactly <@ Fun.Log2 : V2i -> V2d @>
                exactly <@ Fun.Log2 : V2l -> V2d @>
                exactly <@ Fun.Log2 : V3f -> V3f @>
                exactly <@ Fun.Log2 : V3d -> V3d @>
                exactly <@ Fun.Log2 : V3i -> V3d @>
                exactly <@ Fun.Log2 : V3l -> V3d @>
                exactly <@ Fun.Log2 : V4f -> V4f @>
                exactly <@ Fun.Log2 : V4d -> V4d @>
                exactly <@ Fun.Log2 : V4i -> V4d @>
                exactly <@ Fun.Log2 : V4l -> V4d @>
                generic <@ log2 : float -> float @>
            ]

            CIntrinsic.simple "sqrt", [
                exactly <@ Math.Sqrt @>
                exactly <@ Fun.Sqrt : float32 -> float32 @>
                exactly <@ Fun.Sqrt : float   -> float @>
                exactly <@ Fun.Sqrt : int8    -> float @>
                exactly <@ Fun.Sqrt : int16   -> float @>
                exactly <@ Fun.Sqrt : int32   -> float @>
                exactly <@ Fun.Sqrt : int64   -> float @>
                exactly <@ Fun.Sqrt : uint8   -> float @>
                exactly <@ Fun.Sqrt : uint16  -> float @>
                exactly <@ Fun.Sqrt : uint32  -> float @>
                exactly <@ Fun.Sqrt : uint64  -> float @>
                exactly <@ Fun.Sqrt : V2f -> V2f @>
                exactly <@ Fun.Sqrt : V2d -> V2d @>
                exactly <@ Fun.Sqrt : V2i -> V2d @>
                exactly <@ Fun.Sqrt : V2l -> V2d @>
                exactly <@ Fun.Sqrt : V3f -> V3f @>
                exactly <@ Fun.Sqrt : V3d -> V3d @>
                exactly <@ Fun.Sqrt : V3i -> V3d @>
                exactly <@ Fun.Sqrt : V3l -> V3d @>
                exactly <@ Fun.Sqrt : V4f -> V4f @>
                exactly <@ Fun.Sqrt : V4d -> V4d @>
                exactly <@ Fun.Sqrt : V4i -> V4d @>
                exactly <@ Fun.Sqrt : V4l -> V4d @>
                exactly <@ V2f.Sqrt : V2f -> V2f @>
                exactly <@ V2d.Sqrt : V2d -> V2d @>
                exactly <@ V3f.Sqrt : V3f -> V3f @>
                exactly <@ V3d.Sqrt : V3d -> V3d @>
                exactly <@ V4f.Sqrt : V4f -> V4f @>
                exactly <@ V4d.Sqrt : V4d -> V4d @>
                generic <@ sqrt @>
            ]

            CIntrinsic.tagged "pow({0}, 0.333333333333)", [
                exactly <@ Fun.Cbrt : float32 -> _ @>
                exactly <@ Fun.Cbrt : float   -> _ @>
                exactly <@ Fun.Cbrt : int8    -> _ @>
                exactly <@ Fun.Cbrt : int16   -> _ @>
                exactly <@ Fun.Cbrt : int32   -> _ @>
                exactly <@ Fun.Cbrt : int64   -> _ @>
                exactly <@ Fun.Cbrt : uint8   -> _ @>
                exactly <@ Fun.Cbrt : uint16  -> _ @>
                exactly <@ Fun.Cbrt : uint32  -> _ @>
                exactly <@ Fun.Cbrt : uint64  -> _ @>
                generic <@ cbrt     : float   -> _ @>
            ]

            CIntrinsic.tagged "pow({0}, vec2(0.333333333333))", [
                exactly <@ Fun.Cbrt : V2f -> _ @>
                exactly <@ Fun.Cbrt : V2d -> _ @>
                exactly <@ Fun.Cbrt : V2i -> _ @>
                exactly <@ Fun.Cbrt : V2l -> _ @>
                exactly <@ cbrt     : V2f -> _ @>
                exactly <@ cbrt     : V2d -> _ @>
            ]

            CIntrinsic.tagged "pow({0}, vec3(0.333333333333))", [
                exactly <@ Fun.Cbrt : V3f -> _ @>
                exactly <@ Fun.Cbrt : V3d -> _ @>
                exactly <@ Fun.Cbrt : V3i -> _ @>
                exactly <@ Fun.Cbrt : V3l -> _ @>
                exactly <@ cbrt     : V3f -> _ @>
                exactly <@ cbrt     : V3d -> _ @>
            ]

            CIntrinsic.tagged "pow({0}, vec4(0.333333333333))", [
                exactly <@ Fun.Cbrt : V4f -> _ @>
                exactly <@ Fun.Cbrt : V4d -> _ @>
                exactly <@ Fun.Cbrt : V4i -> _ @>
                exactly <@ Fun.Cbrt : V4l -> _ @>
                exactly <@ cbrt     : V4f -> _ @>
                exactly <@ cbrt     : V4d -> _ @>
            ]

            CIntrinsic.tagged "pow({0}, 2)", [
                exactly <@ Fun.Square : float32 -> _ @>
                exactly <@ Fun.Square : float   -> _ @>
                exactly <@ Fun.Square : int8    -> _ @>
                exactly <@ Fun.Square : int16   -> _ @>
                exactly <@ Fun.Square : int32   -> _ @>
                exactly <@ Fun.Square : int64   -> _ @>
                exactly <@ Fun.Square : uint8   -> _ @>
                exactly <@ Fun.Square : uint16  -> _ @>
                exactly <@ Fun.Square : uint32  -> _ @>
                exactly <@ Fun.Square : uint64  -> _ @>
                generic <@ sqr        : float   -> _ @>
            ]

            CIntrinsic.tagged "pow({0}, vec2(2))", [
                exactly <@ Fun.Square : V2f -> _ @>
                exactly <@ Fun.Square : V2d -> _ @>
                exactly <@ Fun.Square : V2i -> _ @>
                exactly <@ Fun.Square : V2l -> _ @>
                exactly <@ sqr        : V2f -> _ @>
                exactly <@ sqr        : V2d -> _ @>
                exactly <@ sqr        : V2i -> _ @>
                exactly <@ sqr        : V2l -> _ @>
            ]

            CIntrinsic.tagged "pow({0}, vec3(2))", [
                exactly <@ Fun.Square : V3f -> _ @>
                exactly <@ Fun.Square : V3d -> _ @>
                exactly <@ Fun.Square : V3i -> _ @>
                exactly <@ Fun.Square : V3l -> _ @>
                exactly <@ sqr        : V3f -> _ @>
                exactly <@ sqr        : V3d -> _ @>
                exactly <@ sqr        : V3i -> _ @>
                exactly <@ sqr        : V3l -> _ @>
            ]

            CIntrinsic.tagged "pow({0}, vec4(2))", [
                exactly <@ Fun.Square : V4f -> _ @>
                exactly <@ Fun.Square : V4d -> _ @>
                exactly <@ Fun.Square : V4i -> _ @>
                exactly <@ Fun.Square : V4l -> _ @>
                exactly <@ sqr        : V4f -> _ @>
                exactly <@ sqr        : V4d -> _ @>
                exactly <@ sqr        : V4i -> _ @>
                exactly <@ sqr        : V4l -> _ @>
            ]

            // ==========================================================================
            // COMMON
            // ==========================================================================

            CIntrinsic.simple "abs", [
                exactly <@ Math.Abs : int8 -> _ @>
                exactly <@ Math.Abs : int16 -> _ @>
                exactly <@ Math.Abs : int32 -> _ @>
                exactly <@ Math.Abs : int64 -> _ @>
                exactly <@ Math.Abs : float32 -> _ @>
                exactly <@ Math.Abs : float -> _ @>
                exactly <@ Math.Abs : decimal -> _ @>

                exactly <@ Fun.Abs : int8 -> _ @>
                exactly <@ Fun.Abs : int16 -> _ @>
                exactly <@ Fun.Abs : int32 -> _ @>
                exactly <@ Fun.Abs : int64 -> _ @>
                exactly <@ Fun.Abs : float32 -> _ @>
                exactly <@ Fun.Abs : float -> _ @>
                exactly <@ Fun.Abs : decimal -> _ @>

                exactly <@ fun (v : int8) -> v.Abs() @>
                exactly <@ fun (v : int16) -> v.Abs() @>
                exactly <@ fun (v : int32) -> v.Abs() @>
                exactly <@ fun (v : int64) -> v.Abs() @>
                exactly <@ fun (v : float32) -> v.Abs() @>
                exactly <@ fun (v : float) -> v.Abs() @>
                exactly <@ fun (v : decimal) -> v.Abs() @>

                exactly <@ fun (v : V2i) -> v.Abs() @>
                exactly <@ fun (v : V3i) -> v.Abs() @>
                exactly <@ fun (v : V4i) -> v.Abs() @>
                exactly <@ fun (v : V2l) -> v.Abs() @>
                exactly <@ fun (v : V3l) -> v.Abs() @>
                exactly <@ fun (v : V4l) -> v.Abs() @>
                exactly <@ fun (v : V2f) -> v.Abs() @>
                exactly <@ fun (v : V3f) -> v.Abs() @>
                exactly <@ fun (v : V4f) -> v.Abs() @>
                exactly <@ fun (v : V2d) -> v.Abs() @>
                exactly <@ fun (v : V3d) -> v.Abs() @>
                exactly <@ fun (v : V4d) -> v.Abs() @>

                generic <@ abs @>
            ]

            CIntrinsic.tagged "int(sign({0}))", [
                exactly <@ Math.Sign : int8 -> _ @>
                exactly <@ Math.Sign : int16 -> _ @>
                exactly <@ Math.Sign : int32 -> _ @>
                exactly <@ Math.Sign : int64 -> _ @>
                exactly <@ Math.Sign : float32 -> _ @>
                exactly <@ Math.Sign : float -> _ @>
                exactly <@ Math.Sign : decimal -> _ @>

                exactly <@ Fun.Sign : int8 -> _ @>
                exactly <@ Fun.Sign : int16 -> _ @>
                exactly <@ Fun.Sign : int32 -> _ @>
                exactly <@ Fun.Sign : int64 -> _ @>
                exactly <@ Fun.Sign : float32 -> _ @>
                exactly <@ Fun.Sign : float -> _ @>
                exactly <@ Fun.Sign : decimal -> _ @>

                exactly <@ Fun.Signumi : int8 -> _ @>
                exactly <@ Fun.Signumi : int16 -> _ @>
                exactly <@ Fun.Signumi : int32 -> _ @>
                exactly <@ Fun.Signumi : int64 -> _ @>
                exactly <@ Fun.Signumi : float32 -> _ @>
                exactly <@ Fun.Signumi : float -> _ @>
                exactly <@ Fun.Signumi : decimal -> _ @>

                exactly <@ fun (v : int8) -> v.Sign() @>
                exactly <@ fun (v : int16) -> v.Sign() @>
                exactly <@ fun (v : int32) -> v.Sign() @>
                exactly <@ fun (v : int64) -> v.Sign() @>
                exactly <@ fun (v : float32) -> v.Sign() @>
                exactly <@ fun (v : float) -> v.Sign() @>
                exactly <@ fun (v : decimal) -> v.Sign() @>

                generic <@ sign @>
                generic <@ signumi : int -> _ @>
            ]

            CIntrinsic.tagged "ivec2(sign({0}))", [
                exactly <@ Fun.Sign     : V2f -> _ @>
                exactly <@ Fun.Sign     : V2d -> _ @>
                exactly <@ Fun.Sign     : V2i -> _ @>
                exactly <@ Fun.Sign     : V2l -> _ @>

                exactly <@ Fun.Signumi  : V2f -> _ @>
                exactly <@ Fun.Signumi  : V2d -> _ @>
                exactly <@ Fun.Signumi  : V2i -> _ @>
                exactly <@ Fun.Signumi  : V2l -> _ @>

                exactly <@ signumi      : V2f -> _ @>
                exactly <@ signumi      : V2d -> _ @>
                exactly <@ signumi      : V2i -> _ @>
                exactly <@ signumi      : V2l -> _ @>
            ]

            CIntrinsic.tagged "ivec3(sign({0}))", [
                exactly <@ Fun.Sign     : V3f -> _ @>
                exactly <@ Fun.Sign     : V3d -> _ @>
                exactly <@ Fun.Sign     : V3i -> _ @>
                exactly <@ Fun.Sign     : V3l -> _ @>

                exactly <@ Fun.Signumi  : V3f -> _ @>
                exactly <@ Fun.Signumi  : V3d -> _ @>
                exactly <@ Fun.Signumi  : V3i -> _ @>
                exactly <@ Fun.Signumi  : V3l -> _ @>

                exactly <@ signumi      : V3f -> _ @>
                exactly <@ signumi      : V3d -> _ @>
                exactly <@ signumi      : V3i -> _ @>
                exactly <@ signumi      : V3l -> _ @>
            ]

            CIntrinsic.tagged "ivec4(sign({0}))", [
                exactly <@ Fun.Sign     : V4f -> _ @>
                exactly <@ Fun.Sign     : V4d -> _ @>
                exactly <@ Fun.Sign     : V4i -> _ @>
                exactly <@ Fun.Sign     : V4l -> _ @>

                exactly <@ Fun.Signumi  : V4f -> _ @>
                exactly <@ Fun.Signumi  : V4d -> _ @>
                exactly <@ Fun.Signumi  : V4i -> _ @>
                exactly <@ Fun.Signumi  : V4l -> _ @>

                exactly <@ signumi      : V4f -> _ @>
                exactly <@ signumi      : V4d -> _ @>
                exactly <@ signumi      : V4i -> _ @>
                exactly <@ signumi      : V4l -> _ @>
            ]

            CIntrinsic.simple "sign", [
                exactly <@ Fun.Signum : int8 -> _ @>
                exactly <@ Fun.Signum : int16 -> _ @>
                exactly <@ Fun.Signum : int32 -> _ @>
                exactly <@ Fun.Signum : int64 -> _ @>
                exactly <@ Fun.Signum : float32 -> _ @>
                exactly <@ Fun.Signum : float -> _ @>
                exactly <@ Fun.Signum : decimal -> _ @>

                exactly <@ Fun.Signum : V2f -> _ @>
                exactly <@ Fun.Signum : V3f -> _ @>
                exactly <@ Fun.Signum : V4f -> _ @>
                exactly <@ Fun.Signum : V2d -> _ @>
                exactly <@ Fun.Signum : V3d -> _ @>
                exactly <@ Fun.Signum : V4d -> _ @>
                exactly <@ Fun.Signum : V2i -> _ @>
                exactly <@ Fun.Signum : V3i -> _ @>
                exactly <@ Fun.Signum : V4i -> _ @>
                exactly <@ Fun.Signum : V2l -> _ @>
                exactly <@ Fun.Signum : V3l -> _ @>
                exactly <@ Fun.Signum : V4l -> _ @>
                generic <@ signum : V4d -> V4d @>
            ]

            CIntrinsic.simple "floor", [
                exactly <@ Math.Floor : float -> _ @>
                exactly <@ Math.Floor : decimal -> _ @>
                exactly <@ Fun.Floor : float -> _ @>
                exactly <@ Fun.Floor : float32 -> _ @>
                exactly <@ Fun.Floor : V2f -> _ @>
                exactly <@ Fun.Floor : V2d -> _ @>
                exactly <@ Fun.Floor : V3f -> _ @>
                exactly <@ Fun.Floor : V3d -> _ @>
                exactly <@ Fun.Floor : V4f -> _ @>
                exactly <@ Fun.Floor : V4d -> _ @>
                generic <@ floor @>
            ]

            CIntrinsic.simple "trunc", [
                exactly <@ Math.Truncate : float -> _ @>
                exactly <@ Math.Truncate : decimal -> _ @>
                exactly <@ Fun.Truncate : float -> _ @>
                exactly <@ Fun.Truncate : decimal -> _ @>
                exactly <@ Fun.Truncate : float32 -> _ @>
                exactly <@ Fun.Truncate : V2f -> _ @>
                exactly <@ Fun.Truncate : V2d -> _ @>
                exactly <@ Fun.Truncate : V3f -> _ @>
                exactly <@ Fun.Truncate : V3d -> _ @>
                exactly <@ Fun.Truncate : V4f -> _ @>
                exactly <@ Fun.Truncate : V4d -> _ @>
                generic <@ truncate @>
            ]

            CIntrinsic.simple "round", [
                exactly <@ Math.Round : float -> _ @>
                exactly <@ Math.Round : decimal -> _ @>
                exactly <@ Fun.Round : float -> _ @>
                exactly <@ Fun.Round : decimal -> _ @>
                exactly <@ Fun.Round : float32 -> _ @>
                exactly <@ Fun.Round : V2f -> _ @>
                exactly <@ Fun.Round : V2d -> _ @>
                exactly <@ Fun.Round : V3f -> _ @>
                exactly <@ Fun.Round : V3d -> _ @>
                exactly <@ Fun.Round : V4f -> _ @>
                exactly <@ Fun.Round : V4d -> _ @>
                generic <@ round @>
            ]

            CIntrinsic.simple "ceil", [
                exactly <@ Math.Ceiling : float -> _ @>
                exactly <@ Math.Ceiling : decimal -> _ @>
                exactly <@ Fun.Ceiling : float -> _ @>
                exactly <@ Fun.Ceiling : float32 -> _ @>
                exactly <@ Fun.Ceiling : V2f -> _ @>
                exactly <@ Fun.Ceiling : V2d -> _ @>
                exactly <@ Fun.Ceiling : V3f -> _ @>
                exactly <@ Fun.Ceiling : V3d -> _ @>
                exactly <@ Fun.Ceiling : V4f -> _ @>
                exactly <@ Fun.Ceiling : V4d -> _ @>
                generic <@ ceil @>
            ]

            CIntrinsic.simple "fract", [
                exactly <@ Fun.Frac : float -> _ @>
                exactly <@ Fun.Frac : float32 -> _ @>
                exactly <@ Fun.Frac : V2f -> V2f @>
                exactly <@ Fun.Frac : V2d -> V2d @>
                exactly <@ Fun.Frac : V3f -> V3f @>
                exactly <@ Fun.Frac : V3d -> V3d @>
                exactly <@ Fun.Frac : V4f -> V4f @>
                exactly <@ Fun.Frac : V4d -> V4d @>
            ]

            CIntrinsic.simple "min", [
                exactly <@ Math.Min : int8 * int8 -> _ @>
                exactly <@ Math.Min : int16 * int16 -> _ @>
                exactly <@ Math.Min : int32 * int32 -> _ @>
                exactly <@ Math.Min : int64 * int64 -> _ @>
                exactly <@ Math.Min : uint8 * uint8 -> _ @>
                exactly <@ Math.Min : uint16 * uint16 -> _ @>
                exactly <@ Math.Min : uint32 * uint32 -> _ @>
                exactly <@ Math.Min : uint64 * uint64 -> _ @>
                exactly <@ Math.Min : float32 * float32 -> _ @>
                exactly <@ Math.Min : float * float -> _ @>
                exactly <@ Math.Min : decimal * decimal -> _ @>

                exactly <@ Fun.Min : int8    * int8 -> _ @>
                exactly <@ Fun.Min : int16   * int16 -> _ @>
                exactly <@ Fun.Min : int32   * int32 -> _ @>
                exactly <@ Fun.Min : int64   * int64 -> _ @>
                exactly <@ Fun.Min : uint8   * uint8 -> _ @>
                exactly <@ Fun.Min : uint16  * uint16 -> _ @>
                exactly <@ Fun.Min : uint32  * uint32 -> _ @>
                exactly <@ Fun.Min : uint64  * uint64 -> _ @>
                exactly <@ Fun.Min : float32 * float32 -> _ @>
                exactly <@ Fun.Min : float   * float -> _ @>
                exactly <@ Fun.Min : V2i * V2i -> V2i @>
                exactly <@ Fun.Min : V3i * V3i -> V3i @>
                exactly <@ Fun.Min : V4i * V4i -> V4i @>
                exactly <@ Fun.Min : V2l * V2l -> V2l @>
                exactly <@ Fun.Min : V3l * V3l -> V3l @>
                exactly <@ Fun.Min : V4l * V4l -> V4l @>
                exactly <@ Fun.Min : V2f * V2f -> V2f @>
                exactly <@ Fun.Min : V3f * V3f -> V3f @>
                exactly <@ Fun.Min : V4f * V4f -> V4f @>
                exactly <@ Fun.Min : V2d * V2d -> V2d @>
                exactly <@ Fun.Min : V3d * V3d -> V3d @>
                exactly <@ Fun.Min : V4d * V4d -> V4d @>
                exactly <@ Fun.Min : V2i * int32 -> V2i @>
                exactly <@ Fun.Min : V3i * int32 -> V3i @>
                exactly <@ Fun.Min : V4i * int32 -> V4i @>
                exactly <@ Fun.Min : V2l * int64 -> V2l @>
                exactly <@ Fun.Min : V3l * int64 -> V3l @>
                exactly <@ Fun.Min : V4l * int64 -> V4l @>
                exactly <@ Fun.Min : V2f * float32 -> V2f @>
                exactly <@ Fun.Min : V3f * float32 -> V3f @>
                exactly <@ Fun.Min : V4f * float32 -> V4f @>
                exactly <@ Fun.Min : V2d * float -> V2d @>
                exactly <@ Fun.Min : V3d * float -> V3d @>
                exactly <@ Fun.Min : V4d * float -> V4d @>

                exactly <@ V2i.Min : V2i * V2i -> V2i @>
                exactly <@ V3i.Min : V3i * V3i -> V3i @>
                exactly <@ V4i.Min : V4i * V4i -> V4i @>
                exactly <@ V2l.Min : V2l * V2l -> V2l @>
                exactly <@ V3l.Min : V3l * V3l -> V3l @>
                exactly <@ V4l.Min : V4l * V4l -> V4l @>
                exactly <@ V2f.Min : V2f * V2f -> V2f @>
                exactly <@ V3f.Min : V3f * V3f -> V3f @>
                exactly <@ V4f.Min : V4f * V4f -> V4f @>
                exactly <@ V2d.Min : V2d * V2d -> V2d @>
                exactly <@ V3d.Min : V3d * V3d -> V3d @>
                exactly <@ V4d.Min : V4d * V4d -> V4d @>

                generic <@ Operators.min : int -> int -> int @>
                generic <@ min : int -> int -> int @>
            ]

            CIntrinsic.custom "min" [1; 0], [
                exactly <@ Fun.Min : int32   * V2i -> V2i @>
                exactly <@ Fun.Min : int32   * V3i -> V3i @>
                exactly <@ Fun.Min : int32   * V4i -> V4i @>
                exactly <@ Fun.Min : int64   * V2l -> V2l @>
                exactly <@ Fun.Min : int64   * V3l -> V3l @>
                exactly <@ Fun.Min : int64   * V4l -> V4l @>
                exactly <@ Fun.Min : float32 * V2f -> V2f @>
                exactly <@ Fun.Min : float32 * V3f -> V3f @>
                exactly <@ Fun.Min : float32 * V4f -> V4f @>
                exactly <@ Fun.Min : float   * V2d -> V2d @>
                exactly <@ Fun.Min : float   * V3d -> V3d @>
                exactly <@ Fun.Min : float   * V4d -> V4d @>
                exactly <@ min : int32   -> V2i -> V2i @>
                exactly <@ min : int32   -> V3i -> V3i @>
                exactly <@ min : int32   -> V4i -> V4i @>
                exactly <@ min : int64   -> V2l -> V2l @>
                exactly <@ min : int64   -> V3l -> V3l @>
                exactly <@ min : int64   -> V4l -> V4l @>
                exactly <@ min : float32 -> V2f -> V2f @>
                exactly <@ min : float32 -> V3f -> V3f @>
                exactly <@ min : float32 -> V4f -> V4f @>
                exactly <@ min : float   -> V2d -> V2d @>
                exactly <@ min : float   -> V3d -> V3d @>
                exactly <@ min : float   -> V4d -> V4d @>
            ]

            CIntrinsic.simple "max", [
                exactly <@ Math.Max : int8 * int8 -> _ @>
                exactly <@ Math.Max : int16 * int16 -> _ @>
                exactly <@ Math.Max : int32 * int32 -> _ @>
                exactly <@ Math.Max : int64 * int64 -> _ @>
                exactly <@ Math.Max : uint8 * uint8 -> _ @>
                exactly <@ Math.Max : uint16 * uint16 -> _ @>
                exactly <@ Math.Max : uint32 * uint32 -> _ @>
                exactly <@ Math.Max : uint64 * uint64 -> _ @>
                exactly <@ Math.Max : float32 * float32 -> _ @>
                exactly <@ Math.Max : float * float -> _ @>
                exactly <@ Math.Max : decimal * decimal -> _ @>

                exactly <@ Fun.Max : int8    * int8 -> _ @>
                exactly <@ Fun.Max : int16   * int16 -> _ @>
                exactly <@ Fun.Max : int32   * int32 -> _ @>
                exactly <@ Fun.Max : int64   * int64 -> _ @>
                exactly <@ Fun.Max : uint8   * uint8 -> _ @>
                exactly <@ Fun.Max : uint16  * uint16 -> _ @>
                exactly <@ Fun.Max : uint32  * uint32 -> _ @>
                exactly <@ Fun.Max : uint64  * uint64 -> _ @>
                exactly <@ Fun.Max : float32 * float32 -> _ @>
                exactly <@ Fun.Max : float   * float -> _ @>
                exactly <@ Fun.Max : V2i * V2i -> V2i @>
                exactly <@ Fun.Max : V3i * V3i -> V3i @>
                exactly <@ Fun.Max : V4i * V4i -> V4i @>
                exactly <@ Fun.Max : V2l * V2l -> V2l @>
                exactly <@ Fun.Max : V3l * V3l -> V3l @>
                exactly <@ Fun.Max : V4l * V4l -> V4l @>
                exactly <@ Fun.Max : V2f * V2f -> V2f @>
                exactly <@ Fun.Max : V3f * V3f -> V3f @>
                exactly <@ Fun.Max : V4f * V4f -> V4f @>
                exactly <@ Fun.Max : V2d * V2d -> V2d @>
                exactly <@ Fun.Max : V3d * V3d -> V3d @>
                exactly <@ Fun.Max : V4d * V4d -> V4d @>
                exactly <@ Fun.Max : V2i * int32 -> V2i @>
                exactly <@ Fun.Max : V3i * int32 -> V3i @>
                exactly <@ Fun.Max : V4i * int32 -> V4i @>
                exactly <@ Fun.Max : V2l * int64 -> V2l @>
                exactly <@ Fun.Max : V3l * int64 -> V3l @>
                exactly <@ Fun.Max : V4l * int64 -> V4l @>
                exactly <@ Fun.Max : V2f * float32 -> V2f @>
                exactly <@ Fun.Max : V3f * float32 -> V3f @>
                exactly <@ Fun.Max : V4f * float32 -> V4f @>
                exactly <@ Fun.Max : V2d * float -> V2d @>
                exactly <@ Fun.Max : V3d * float -> V3d @>
                exactly <@ Fun.Max : V4d * float -> V4d @>

                exactly <@ V2i.Max : V2i * V2i -> V2i @>
                exactly <@ V3i.Max : V3i * V3i -> V3i @>
                exactly <@ V4i.Max : V4i * V4i -> V4i @>
                exactly <@ V2l.Max : V2l * V2l -> V2l @>
                exactly <@ V3l.Max : V3l * V3l -> V3l @>
                exactly <@ V4l.Max : V4l * V4l -> V4l @>
                exactly <@ V2f.Max : V2f * V2f -> V2f @>
                exactly <@ V3f.Max : V3f * V3f -> V3f @>
                exactly <@ V4f.Max : V4f * V4f -> V4f @>
                exactly <@ V2d.Max : V2d * V2d -> V2d @>
                exactly <@ V3d.Max : V3d * V3d -> V3d @>
                exactly <@ V4d.Max : V4d * V4d -> V4d @>

                generic <@ Operators.max : int -> int -> int @>
                generic <@ max : float -> float -> _ @>
            ]

            CIntrinsic.custom "max" [1; 0], [
                exactly <@ Fun.Max : int32   * V2i -> V2i @>
                exactly <@ Fun.Max : int32   * V3i -> V3i @>
                exactly <@ Fun.Max : int32   * V4i -> V4i @>
                exactly <@ Fun.Max : int64   * V2l -> V2l @>
                exactly <@ Fun.Max : int64   * V3l -> V3l @>
                exactly <@ Fun.Max : int64   * V4l -> V4l @>
                exactly <@ Fun.Max : float32 * V2f -> V2f @>
                exactly <@ Fun.Max : float32 * V3f -> V3f @>
                exactly <@ Fun.Max : float32 * V4f -> V4f @>
                exactly <@ Fun.Max : float   * V2d -> V2d @>
                exactly <@ Fun.Max : float   * V3d -> V3d @>
                exactly <@ Fun.Max : float   * V4d -> V4d @>
                exactly <@ max : int32   -> V2i -> V2i @>
                exactly <@ max : int32   -> V3i -> V3i @>
                exactly <@ max : int32   -> V4i -> V4i @>
                exactly <@ max : int64   -> V2l -> V2l @>
                exactly <@ max : int64   -> V3l -> V3l @>
                exactly <@ max : int64   -> V4l -> V4l @>
                exactly <@ max : float32 -> V2f -> V2f @>
                exactly <@ max : float32 -> V3f -> V3f @>
                exactly <@ max : float32 -> V4f -> V4f @>
                exactly <@ max : float   -> V2d -> V2d @>
                exactly <@ max : float   -> V3d -> V3d @>
                exactly <@ max : float   -> V4d -> V4d @>
            ]

            CIntrinsic.custom "clamp" [2; 0; 1], [
                generic <@ clamp : float -> float -> float -> _ @>
            ]

            CIntrinsic.tagged "clamp({2}, vec2({0}), {1})", [
                exactly <@ clamp : int32   -> V2i -> V2i -> _ @>
                exactly <@ clamp : int64   -> V2l -> V2l -> _ @>
                exactly <@ clamp : float32 -> V2f -> V2f -> _ @>
                exactly <@ clamp : float   -> V2d -> V2d -> _ @>
            ]

            CIntrinsic.tagged "clamp({2}, vec3({0}), {1})", [
                exactly <@ clamp : int32   -> V3i -> V3i -> _ @>
                exactly <@ clamp : int64   -> V3l -> V3l -> _ @>
                exactly <@ clamp : float32 -> V3f -> V3f -> _ @>
                exactly <@ clamp : float   -> V3d -> V3d -> _ @>
            ]

            CIntrinsic.tagged "clamp({2}, vec4({0}), {1})", [
                exactly <@ clamp : int32   -> V4i -> V4i -> _ @>
                exactly <@ clamp : int64   -> V4l -> V4l -> _ @>
                exactly <@ clamp : float32 -> V4f -> V4f -> _ @>
                exactly <@ clamp : float   -> V4d -> V4d -> _ @>
            ]

            CIntrinsic.tagged "clamp({2}, {0}, vec2({1}))", [
                exactly <@ clamp : V2i -> int32   -> V2i -> _ @>
                exactly <@ clamp : V2l -> int64   -> V2l -> _ @>
                exactly <@ clamp : V2f -> float32 -> V2f -> _ @>
                exactly <@ clamp : V2d -> float   -> V2d -> _ @>
            ]

            CIntrinsic.tagged "clamp({2}, {0}, vec3({1}))", [
                exactly <@ clamp : V3i -> int32   -> V3i -> _ @>
                exactly <@ clamp : V3l -> int64   -> V3l -> _ @>
                exactly <@ clamp : V3f -> float32 -> V3f -> _ @>
                exactly <@ clamp : V3d -> float   -> V3d -> _ @>
            ]

            CIntrinsic.tagged "clamp({2}, {0}, vec4({1}))", [
                exactly <@ clamp : V4i -> int32   -> V4i -> _ @>
                exactly <@ clamp : V4l -> int64   -> V4l -> _ @>
                exactly <@ clamp : V4f -> float32 -> V4f -> _ @>
                exactly <@ clamp : V4d -> float   -> V4d -> _ @>
            ]

            CIntrinsic.simple "clamp", [
                exactly <@ Fun.Clamp : int8    * int8    * int8 -> _ @>
                exactly <@ Fun.Clamp : int16   * int16   * int16 -> _ @>
                exactly <@ Fun.Clamp : int32   * int32   * int32 -> _ @>
                exactly <@ Fun.Clamp : int64   * int64   * int64 -> _ @>
                exactly <@ Fun.Clamp : uint8   * uint8   * uint8 -> _ @>
                exactly <@ Fun.Clamp : uint16  * uint16  * uint16 -> _ @>
                exactly <@ Fun.Clamp : uint32  * uint32  * uint32 -> _ @>
                exactly <@ Fun.Clamp : uint64  * uint64  * uint64 -> _ @>
                exactly <@ Fun.Clamp : float32 * float32 * float32 -> _ @>
                exactly <@ Fun.Clamp : float   * float   * float -> _ @>
                exactly <@ Fun.Clamp : decimal * decimal * decimal -> _ @>

                exactly <@ Fun.Clamp : V2i * V2i     * V2i -> _ @>
                exactly <@ Fun.Clamp : V2l * V2l     * V2l -> _ @>
                exactly <@ Fun.Clamp : V2f * V2f     * V2f -> _ @>
                exactly <@ Fun.Clamp : V2d * V2d     * V2d -> _ @>
                exactly <@ Fun.Clamp : V2i * int32   * int32 -> _ @>
                exactly <@ Fun.Clamp : V2l * int64   * int64 -> _ @>
                exactly <@ Fun.Clamp : V2f * float32 * float32 -> _ @>
                exactly <@ Fun.Clamp : V2d * float   * float -> _ @>

                exactly <@ Fun.Clamp : V3i * V3i     * V3i -> _ @>
                exactly <@ Fun.Clamp : V3l * V3l     * V3l -> _ @>
                exactly <@ Fun.Clamp : V3f * V3f     * V3f -> _ @>
                exactly <@ Fun.Clamp : V3d * V3d     * V3d -> _ @>
                exactly <@ Fun.Clamp : V3i * int32   * int32 -> _ @>
                exactly <@ Fun.Clamp : V3l * int64   * int64 -> _ @>
                exactly <@ Fun.Clamp : V3f * float32 * float32 -> _ @>
                exactly <@ Fun.Clamp : V3d * float   * float -> _ @>

                exactly <@ Fun.Clamp : V4i * V4i     * V4i -> _ @>
                exactly <@ Fun.Clamp : V4l * V4l     * V4l -> _ @>
                exactly <@ Fun.Clamp : V4f * V4f     * V4f -> _ @>
                exactly <@ Fun.Clamp : V4d * V4d     * V4d -> _ @>
                exactly <@ Fun.Clamp : V4i * int32   * int32 -> _ @>
                exactly <@ Fun.Clamp : V4l * int64   * int64 -> _ @>
                exactly <@ Fun.Clamp : V4f * float32 * float32 -> _ @>
                exactly <@ Fun.Clamp : V4d * float   * float -> _ @>
            ]

            CIntrinsic.tagged "clamp({0}, 0, 1)", [
                exactly <@ Fun.Saturate : int8 -> _ @>
                exactly <@ Fun.Saturate : int16 -> _ @>
                exactly <@ Fun.Saturate : int32 -> _ @>
                exactly <@ Fun.Saturate : int64 -> _ @>
                exactly <@ Fun.Saturate : uint8  -> _ @>
                exactly <@ Fun.Saturate : uint16 -> _ @>
                exactly <@ Fun.Saturate : uint32 -> _ @>
                exactly <@ Fun.Saturate : uint64 -> _ @>
                exactly <@ Fun.Saturate : float32 -> _ @>
                exactly <@ Fun.Saturate : float -> _ @>
                exactly <@ Fun.Saturate : decimal -> _ @>
                exactly <@ Fun.Saturate : V2i -> _ @>
                exactly <@ Fun.Saturate : V3i -> _ @>
                exactly <@ Fun.Saturate : V4i -> _ @>
                exactly <@ Fun.Saturate : V2l -> _ @>
                exactly <@ Fun.Saturate : V3l -> _ @>
                exactly <@ Fun.Saturate : V4l -> _ @>
                exactly <@ Fun.Saturate : V2f -> _ @>
                exactly <@ Fun.Saturate : V3f -> _ @>
                exactly <@ Fun.Saturate : V4f -> _ @>
                exactly <@ Fun.Saturate : V2d -> _ @>
                exactly <@ Fun.Saturate : V3d -> _ @>
                exactly <@ Fun.Saturate : V4d -> _ @>
                generic <@ saturate : V4d -> _ @>
            ]

            CIntrinsic.custom "step" [1; 0], [
                exactly <@ Fun.Step : float32 * float32 -> _ @>
                exactly <@ Fun.Step : float * float -> _ @>
                exactly <@ Fun.Step : V2f * V2f -> V2f @>
                exactly <@ Fun.Step : V2f * float32 -> V2f @>
                exactly <@ Fun.Step : V2d * V2d -> V2d @>
                exactly <@ Fun.Step : V2d * float -> V2d @>
                exactly <@ Fun.Step : V3f * V3f -> V3f @>
                exactly <@ Fun.Step : V3f * float32 -> V3f @>
                exactly <@ Fun.Step : V3d * V3d -> V3d @>
                exactly <@ Fun.Step : V3d * float -> V3d @>
                exactly <@ Fun.Step : V4f * V4f -> V4f @>
                exactly <@ Fun.Step : V4f * float32 -> V4f @>
                exactly <@ Fun.Step : V4d * V4d -> V4d @>
                exactly <@ Fun.Step : V4d * float -> V4d @>
            ]

            CIntrinsic.simple "step", [
                generic <@ step : float -> float -> _ @>
            ]

            CIntrinsic.custom "smoothstep" [1; 2; 0], [
                exactly <@ Fun.Smoothstep : float32 * float32 * float32 -> _ @>
                exactly <@ Fun.Smoothstep : float * float * float -> _ @>
                exactly <@ Fun.Smoothstep : V2f * V2f * V2f -> V2f @>
                exactly <@ Fun.Smoothstep : V2f * float32 * float32 -> V2f @>
                exactly <@ Fun.Smoothstep : V2d * V2d * V2d -> V2d @>
                exactly <@ Fun.Smoothstep : V2d * float * float -> V2d @>
                exactly <@ Fun.Smoothstep : V3f * V3f * V3f -> V3f @>
                exactly <@ Fun.Smoothstep : V3f * float32 * float32 -> V3f @>
                exactly <@ Fun.Smoothstep : V3d * V3d * V3d -> V3d @>
                exactly <@ Fun.Smoothstep : V3d * float * float -> V3d @>
                exactly <@ Fun.Smoothstep : V4f * V4f * V4f -> V4f @>
                exactly <@ Fun.Smoothstep : V4f * float32 * float32 -> V4f @>
                exactly <@ Fun.Smoothstep : V4d * V4d * V4d -> V4d @>
                exactly <@ Fun.Smoothstep : V4d * float * float -> V4d @>
            ]

            CIntrinsic.simple "smoothstep", [
                generic <@ smoothstep : float -> float -> V2d -> V2d @>
            ]


            CIntrinsic.tagged "int(round(mix({1}, {2}, {0})))", [
                exactly <@ Fun.Lerp : float32 * int8    * int8 -> _ @>
                exactly <@ Fun.Lerp : float32 * int16   * int16 -> _ @>
                exactly <@ Fun.Lerp : float32 * int32   * int32 -> _ @>
                exactly <@ Fun.Lerp : float32 * int64   * int64 -> _ @>
                exactly <@ Fun.Lerp : float   * int8    * int8 -> _ @>
                exactly <@ Fun.Lerp : float   * int16   * int16 -> _ @>
                exactly <@ Fun.Lerp : float   * int32   * int32 -> _ @>
                exactly <@ Fun.Lerp : float   * int64   * int64 -> _ @>
            ]

            CIntrinsic.tagged "int(round(mix({0}, {1}, {2})))", [
                exactly <@ lerp : int8  -> int8  -> float32 -> _ @>
                exactly <@ lerp : int16 -> int16 -> float32 -> _ @>
                exactly <@ lerp : int32 -> int32 -> float32 -> _ @>
                exactly <@ lerp : int64 -> int64 -> float32 -> _ @>
                exactly <@ lerp : int8  -> int8  -> float   -> _ @>
                exactly <@ lerp : int16 -> int16 -> float   -> _ @>
                exactly <@ lerp : int32 -> int32 -> float   -> _ @>
                exactly <@ lerp : int64 -> int64 -> float   -> _ @>
            ]

            CIntrinsic.tagged "uint(mix({1}, {2}, {0}) + 0.5)", [
                exactly <@ Fun.Lerp : float32 * uint8    * uint8 -> _ @>
                exactly <@ Fun.Lerp : float32 * uint16   * uint16 -> _ @>
                exactly <@ Fun.Lerp : float32 * uint32   * uint32 -> _ @>
                exactly <@ Fun.Lerp : float32 * uint64   * uint64 -> _ @>
                exactly <@ Fun.Lerp : float   * uint8    * uint8 -> _ @>
                exactly <@ Fun.Lerp : float   * uint16   * uint16 -> _ @>
                exactly <@ Fun.Lerp : float   * uint32   * uint32 -> _ @>
                exactly <@ Fun.Lerp : float   * uint64   * uint64 -> _ @>
            ]

            CIntrinsic.tagged "uint(mix({0}, {1}, {2}) + 0.5)", [
                exactly <@ lerp : uint8  -> uint8  -> float32 -> _ @>
                exactly <@ lerp : uint16 -> uint16 -> float32 -> _ @>
                exactly <@ lerp : uint32 -> uint32 -> float32 -> _ @>
                exactly <@ lerp : uint64 -> uint64 -> float32 -> _ @>
                exactly <@ lerp : uint8  -> uint8  -> float   -> _ @>
                exactly <@ lerp : uint16 -> uint16 -> float   -> _ @>
                exactly <@ lerp : uint32 -> uint32 -> float   -> _ @>
                exactly <@ lerp : uint64 -> uint64 -> float   -> _ @>
            ]

            CIntrinsic.tagged "ivec2(round(mix({1}, {2}, {0})))", [
                exactly <@ Fun.Lerp : float32 * V2i * V2i -> _ @>
                exactly <@ Fun.Lerp : float32 * V2l * V2l -> _ @>
                exactly <@ Fun.Lerp : float * V2i * V2i -> _ @>
                exactly <@ Fun.Lerp : float * V2l * V2l -> _ @>
                exactly <@ Fun.Lerp : V2f * V2i * V2i -> _ @>
                exactly <@ Fun.Lerp : V2f * V2l * V2l -> _ @>
                exactly <@ Fun.Lerp : V2d * V2i * V2i -> _ @>
                exactly <@ Fun.Lerp : V2d * V2l * V2l -> _ @>
            ]

            CIntrinsic.tagged "ivec2(round(mix({0}, {1}, {2})))", [
                exactly <@ lerp : V2i -> V2i -> float32 -> _ @>
                exactly <@ lerp : V2l -> V2l -> float32 -> _ @>
                exactly <@ lerp : V2i -> V2i -> float -> _ @>
                exactly <@ lerp : V2l -> V2l -> float -> _ @>
                exactly <@ lerp : V2i -> V2i -> V2f -> _ @>
                exactly <@ lerp : V2l -> V2l -> V2f -> _ @>
                exactly <@ lerp : V2i -> V2i -> V2d -> _ @>
                exactly <@ lerp : V2l -> V2l -> V2d -> _ @>
            ]

            CIntrinsic.tagged "ivec3(round(mix({1}, {2}, {0})))", [
                exactly <@ Fun.Lerp : float32 * V3i * V3i -> _ @>
                exactly <@ Fun.Lerp : float32 * V3l * V3l -> _ @>
                exactly <@ Fun.Lerp : float * V3i * V3i -> _ @>
                exactly <@ Fun.Lerp : float * V3l * V3l -> _ @>
                exactly <@ Fun.Lerp : V3f * V3i * V3i -> _ @>
                exactly <@ Fun.Lerp : V3f * V3l * V3l -> _ @>
                exactly <@ Fun.Lerp : V3d * V3i * V3i -> _ @>
                exactly <@ Fun.Lerp : V3d * V3l * V3l -> _ @>
            ]

            CIntrinsic.tagged "ivec3(round(mix({0}, {1}, {2})))", [
                exactly <@ lerp : V3i -> V3i -> float32 -> _ @>
                exactly <@ lerp : V3l -> V3l -> float32 -> _ @>
                exactly <@ lerp : V3i -> V3i -> float -> _ @>
                exactly <@ lerp : V3l -> V3l -> float -> _ @>
                exactly <@ lerp : V3i -> V3i -> V3f -> _ @>
                exactly <@ lerp : V3l -> V3l -> V3f -> _ @>
                exactly <@ lerp : V3i -> V3i -> V3d -> _ @>
                exactly <@ lerp : V3l -> V3l -> V3d -> _ @>
            ]

            CIntrinsic.tagged "ivec4(round(mix({1}, {2}, {0})))", [
                exactly <@ Fun.Lerp : float32 * V4i * V4i -> _ @>
                exactly <@ Fun.Lerp : float32 * V4l * V4l -> _ @>
                exactly <@ Fun.Lerp : float * V4i * V4i -> _ @>
                exactly <@ Fun.Lerp : float * V4l * V4l -> _ @>
                exactly <@ Fun.Lerp : V4f * V4i * V4i -> _ @>
                exactly <@ Fun.Lerp : V4f * V4l * V4l -> _ @>
                exactly <@ Fun.Lerp : V4d * V4i * V4i -> _ @>
                exactly <@ Fun.Lerp : V4d * V4l * V4l -> _ @>
            ]

            CIntrinsic.tagged "ivec4(round(mix({0}, {1}, {2})))", [
                exactly <@ lerp : V4i -> V4i -> float32 -> _ @>
                exactly <@ lerp : V4l -> V4l -> float32 -> _ @>
                exactly <@ lerp : V4i -> V4i -> float -> _ @>
                exactly <@ lerp : V4l -> V4l -> float -> _ @>
                exactly <@ lerp : V4i -> V4i -> V4f -> _ @>
                exactly <@ lerp : V4l -> V4l -> V4f -> _ @>
                exactly <@ lerp : V4i -> V4i -> V4d -> _ @>
                exactly <@ lerp : V4l -> V4l -> V4d -> _ @>
            ]

            CIntrinsic.custom "mix" [1; 2; 0], [
                exactly <@ Fun.Lerp : float32 * float32 * float32 -> _ @>
                exactly <@ Fun.Lerp : float   * float   * float -> _ @>

                exactly <@ Fun.Lerp : float32 * V2f * V2f -> _ @>
                exactly <@ Fun.Lerp : float32 * V3f * V3f -> _ @>
                exactly <@ Fun.Lerp : float32 * V4f * V4f -> _ @>

                exactly <@ Fun.Lerp : float * V2d * V2d -> _ @>
                exactly <@ Fun.Lerp : float * V3d * V3d -> _ @>
                exactly <@ Fun.Lerp : float * V4d * V4d -> _ @>

                exactly <@ Fun.Lerp : V2f * V2f * V2f -> _ @>
                exactly <@ Fun.Lerp : V3f * V3f * V3f -> _ @>
                exactly <@ Fun.Lerp : V4f * V4f * V4f -> _ @>

                exactly <@ Fun.Lerp : V2d * V2d * V2d -> _ @>
                exactly <@ Fun.Lerp : V3d * V3d * V3d -> _ @>
                exactly <@ Fun.Lerp : V4d * V4d * V4d -> _ @>
            ]

            CIntrinsic.simple "mix", [
                generic <@ lerp : V3d -> V3d -> float -> V3d @>
            ]

            CIntrinsic.simple "fma", [
                exactly <@ Fun.MultiplyAdd : float   * float   * float -> _ @>
                exactly <@ Fun.MultiplyAdd : float32 * float32 * float32 -> _ @>
                exactly <@ Fun.MultiplyAdd : V2f     * V2f     * V2f -> _ @>
                exactly <@ Fun.MultiplyAdd : V3f     * V3f     * V3f -> _ @>
                exactly <@ Fun.MultiplyAdd : V4f     * V4f     * V4f -> _ @>
                exactly <@ Fun.MultiplyAdd : V2d     * V2d     * V2d -> _ @>
                exactly <@ Fun.MultiplyAdd : V3d     * V3d     * V3d -> _ @>
                exactly <@ Fun.MultiplyAdd : V4d     * V4d     * V4d -> _ @>

                generic <@ madd : float -> float -> float -> float @>
            ]

            CIntrinsic.tagged "fma({0}, ivec2({1}), {2})", [
                exactly <@ madd : V2i -> int32 -> V2i -> _ @>
            ]

            CIntrinsic.tagged "fma({0}, ivec3({1}), {2})", [
                exactly <@ madd : V3i -> int32 -> V3i -> _ @>
            ]

            CIntrinsic.tagged "fma({0}, ivec4({1}), {2})", [
                exactly <@ madd : V4i -> int32 -> V4i -> _ @>
            ]

            CIntrinsic.tagged "fma({0}, vec2({1}), {2})", [
                exactly <@ Fun.MultiplyAdd : V2f * float32 * V2f -> _ @>
                exactly <@ Fun.MultiplyAdd : V2d * float   * V2d -> _ @>
                exactly <@ madd : V2f -> float32 -> V2f -> _ @>
                exactly <@ madd : V2d -> float   -> V2d -> _ @>
            ]

            CIntrinsic.tagged "fma({0}, vec3({1}), {2})", [
                exactly <@ Fun.MultiplyAdd : V3f * float32 * V3f -> _ @>
                exactly <@ Fun.MultiplyAdd : V3d * float   * V3d -> _ @>
                exactly <@ madd : V3f -> float32 -> V3f -> _ @>
                exactly <@ madd : V3d -> float   -> V3d -> _ @>
            ]

            CIntrinsic.tagged "fma({0}, vec4({1}), {2})", [
                exactly <@ Fun.MultiplyAdd : V4f * float32 * V4f -> _ @>
                exactly <@ Fun.MultiplyAdd : V4d * float   * V4d -> _ @>
                exactly <@ madd : V4f -> float32 -> V4f -> _ @>
                exactly <@ madd : V4d -> float   -> V4d -> _ @>
            ]

            CIntrinsic.tagged "fma(vec2({0}), {1}, {2})", [
                exactly <@ Fun.MultiplyAdd : float32 * V2f * V2f -> _ @>
                exactly <@ Fun.MultiplyAdd : float   * V2d * V2d -> _ @>
            ]

            CIntrinsic.tagged "fma(vec3({0}), {1}, {2})", [
                exactly <@ Fun.MultiplyAdd : float32 * V3f * V3f -> _ @>
                exactly <@ Fun.MultiplyAdd : float   * V3d * V3d -> _ @>
            ]

            CIntrinsic.tagged "fma(vec4({0}), {1}, {2})", [
                exactly <@ Fun.MultiplyAdd : float32 * V4f * V4f -> _ @>
                exactly <@ Fun.MultiplyAdd : float   * V4d * V4d -> _ @>
            ]

            CIntrinsic.simple "isnan", [
                exactly <@ Fun.IsNaN : float32 -> bool @>
                exactly <@ Fun.IsNaN : float -> bool @>
                exactly <@ isNaN : float32 -> bool @>
                exactly <@ isNaN : float -> bool @>
                exactly <@ Single.IsNaN @>
                exactly <@ Double.IsNaN @>
            ]

            CIntrinsic.tagged "any(isnan({0}))", [
                exactly <@ Fun.IsNaN : V2f -> bool @>
                exactly <@ Fun.IsNaN : V3f -> bool @>
                exactly <@ Fun.IsNaN : V4f -> bool @>
                exactly <@ Fun.IsNaN : V2d -> bool @>
                exactly <@ Fun.IsNaN : V3d -> bool @>
                exactly <@ Fun.IsNaN : V4d -> bool @>
                exactly <@ fun (v : V2f) -> v.IsNaN @>
                exactly <@ fun (v : V3f) -> v.IsNaN @>
                exactly <@ fun (v : V4f) -> v.IsNaN @>
                exactly <@ fun (v : V2d) -> v.IsNaN @>
                exactly <@ fun (v : V3d) -> v.IsNaN @>
                exactly <@ fun (v : V4d) -> v.IsNaN @>
                generic <@ isNaN : float -> _ @>
            ]

            CIntrinsic.simple "isinf", [
                exactly <@ Fun.IsInfinity : float32 -> bool @>
                exactly <@ Fun.IsInfinity : float -> bool @>
                exactly <@ isInfinity : float32 -> bool @>
                exactly <@ isInfinity : float -> bool @>
                exactly <@ Single.IsInfinity @>
                exactly <@ Double.IsInfinity @>
            ]

            CIntrinsic.tagged "any(isinf({0}))", [
                exactly <@ Fun.IsInfinity : V2f -> bool @>
                exactly <@ Fun.IsInfinity : V3f -> bool @>
                exactly <@ Fun.IsInfinity : V4f -> bool @>
                exactly <@ Fun.IsInfinity : V2d -> bool @>
                exactly <@ Fun.IsInfinity : V3d -> bool @>
                exactly <@ Fun.IsInfinity : V4d -> bool @>
                exactly <@ fun (v : V2f) -> v.IsInfinity @>
                exactly <@ fun (v : V3f) -> v.IsInfinity @>
                exactly <@ fun (v : V4f) -> v.IsInfinity @>
                exactly <@ fun (v : V2d) -> v.IsInfinity @>
                exactly <@ fun (v : V3d) -> v.IsInfinity @>
                exactly <@ fun (v : V4d) -> v.IsInfinity @>
                generic <@ isInfinity : float -> _ @>
            ]

            // ==========================================================================
            // VECTOR relations
            // ==========================================================================
            CIntrinsic.tagged "any(lessThan({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AnySmaller(v) @>
                exactly <@ Vec.AnySmaller : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AnySmaller(v) @>
                exactly <@ Vec.AnySmaller : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AnySmaller(v) @>
                exactly <@ Vec.AnySmaller : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AnySmaller(v) @>
                exactly <@ Vec.AnySmaller : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AnySmaller(v) @>
                exactly <@ Vec.AnySmaller : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AnySmaller(v) @>
                exactly <@ Vec.AnySmaller : V4i * V4i -> bool @>
                generic <@ Vec.anySmaller : V4i -> V4i -> bool @>
            ]

            CIntrinsic.tagged "all(lessThan({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AllSmaller(v) @>
                exactly <@ Vec.AllSmaller : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AllSmaller(v) @>
                exactly <@ Vec.AllSmaller : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AllSmaller(v) @>
                exactly <@ Vec.AllSmaller : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AllSmaller(v) @>
                exactly <@ Vec.AllSmaller : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AllSmaller(v) @>
                exactly <@ Vec.AllSmaller : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AllSmaller(v) @>
                exactly <@ Vec.AllSmaller : V4i * V4i -> bool @>
                generic <@ Vec.allSmaller : V4i -> V4i -> bool @>
            ]

            CIntrinsic.tagged "any(lessThanEqual({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AnySmallerOrEqual(v) @>
                exactly <@ Vec.AnySmallerOrEqual : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AnySmallerOrEqual(v) @>
                exactly <@ Vec.AnySmallerOrEqual : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AnySmallerOrEqual(v) @>
                exactly <@ Vec.AnySmallerOrEqual : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AnySmallerOrEqual(v) @>
                exactly <@ Vec.AnySmallerOrEqual : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AnySmallerOrEqual(v) @>
                exactly <@ Vec.AnySmallerOrEqual : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AnySmallerOrEqual(v) @>
                exactly <@ Vec.AnySmallerOrEqual : V4i * V4i -> bool @>
                generic <@ Vec.anySmallerOrEqual : V4i -> V4i -> bool @>
            ]

            CIntrinsic.tagged "all(lessThanEqual({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AllSmallerOrEqual(v) @>
                exactly <@ Vec.AllSmallerOrEqual : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AllSmallerOrEqual(v) @>
                exactly <@ Vec.AllSmallerOrEqual : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AllSmallerOrEqual(v) @>
                exactly <@ Vec.AllSmallerOrEqual : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AllSmallerOrEqual(v) @>
                exactly <@ Vec.AllSmallerOrEqual : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AllSmallerOrEqual(v) @>
                exactly <@ Vec.AllSmallerOrEqual : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AllSmallerOrEqual(v) @>
                exactly <@ Vec.AllSmallerOrEqual : V4i * V4i -> bool @>
                generic <@ Vec.allSmallerOrEqual : V4i -> V4i -> bool @>
            ]

            CIntrinsic.tagged "any(greaterThan({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AnyGreater(v) @>
                exactly <@ Vec.AnyGreater : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AnyGreater(v) @>
                exactly <@ Vec.AnyGreater : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AnyGreater(v) @>
                exactly <@ Vec.AnyGreater : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AnyGreater(v) @>
                exactly <@ Vec.AnyGreater : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AnyGreater(v) @>
                exactly <@ Vec.AnyGreater : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AnyGreater(v) @>
                exactly <@ Vec.AnyGreater : V4i * V4i -> bool @>
                generic <@ Vec.anyGreater : V4i -> V4i -> bool @>
            ]

            CIntrinsic.tagged "all(greaterThan({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AllGreater(v) @>
                exactly <@ Vec.AllGreater : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AllGreater(v) @>
                exactly <@ Vec.AllGreater : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AllGreater(v) @>
                exactly <@ Vec.AllGreater : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AllGreater(v) @>
                exactly <@ Vec.AllGreater : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AllGreater(v) @>
                exactly <@ Vec.AllGreater : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AllGreater(v) @>
                exactly <@ Vec.AllGreater : V4i * V4i -> bool @>
                generic <@ Vec.allGreater : V4i -> V4i -> bool @>
            ]

            CIntrinsic.tagged "any(greaterThanEqual({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AnyGreaterOrEqual(v) @>
                exactly <@ Vec.AnyGreaterOrEqual : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AnyGreaterOrEqual(v) @>
                exactly <@ Vec.AnyGreaterOrEqual : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AnyGreaterOrEqual(v) @>
                exactly <@ Vec.AnyGreaterOrEqual : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AnyGreaterOrEqual(v) @>
                exactly <@ Vec.AnyGreaterOrEqual : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AnyGreaterOrEqual(v) @>
                exactly <@ Vec.AnyGreaterOrEqual : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AnyGreaterOrEqual(v) @>
                exactly <@ Vec.AnyGreaterOrEqual : V4i * V4i -> bool @>
                generic <@ Vec.anyGreaterOrEqual : V4i -> V4i -> bool @>
            ]

            CIntrinsic.tagged "all(greaterThanEqual({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AllGreaterOrEqual(v) @>
                exactly <@ Vec.AllGreaterOrEqual : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AllGreaterOrEqual(v) @>
                exactly <@ Vec.AllGreaterOrEqual : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AllGreaterOrEqual(v) @>
                exactly <@ Vec.AllGreaterOrEqual : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AllGreaterOrEqual(v) @>
                exactly <@ Vec.AllGreaterOrEqual : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AllGreaterOrEqual(v) @>
                exactly <@ Vec.AllGreaterOrEqual : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AllGreaterOrEqual(v) @>
                exactly <@ Vec.AllGreaterOrEqual : V4i * V4i -> bool @>
                generic <@ Vec.allGreaterOrEqual : V4i -> V4i -> bool @>
            ]

            CIntrinsic.tagged "any(equal({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AnyEqual(v) @>
                exactly <@ Vec.AnyEqual : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AnyEqual(v) @>
                exactly <@ Vec.AnyEqual : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AnyEqual(v) @>
                exactly <@ Vec.AnyEqual : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AnyEqual(v) @>
                exactly <@ Vec.AnyEqual : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AnyEqual(v) @>
                exactly <@ Vec.AnyEqual : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AnyEqual(v) @>
                exactly <@ Vec.AnyEqual : V4i * V4i -> bool @>
            ]

            CIntrinsic.tagged "all(equal({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AllEqual(v) @>
                exactly <@ Vec.AllEqual : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AllEqual(v) @>
                exactly <@ Vec.AllEqual : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AllEqual(v) @>
                exactly <@ Vec.AllEqual : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AllEqual(v) @>
                exactly <@ Vec.AllEqual : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AllEqual(v) @>
                exactly <@ Vec.AllEqual : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AllEqual(v) @>
                exactly <@ Vec.AllEqual : V4i * V4i -> bool @>
            ]

            CIntrinsic.tagged "any(notEqual({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AnyDifferent(v) @>
                exactly <@ Vec.AnyDifferent : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AnyDifferent(v) @>
                exactly <@ Vec.AnyDifferent : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AnyDifferent(v) @>
                exactly <@ Vec.AnyDifferent : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AnyDifferent(v) @>
                exactly <@ Vec.AnyDifferent : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AnyDifferent(v) @>
                exactly <@ Vec.AnyDifferent : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AnyDifferent(v) @>
                exactly <@ Vec.AnyDifferent : V4i * V4i -> bool @>
            ]

            CIntrinsic.tagged "all(notEqual({0},{1}))", [
                exactly <@ fun (u : V2d) (v : V2d) -> u.AllDifferent(v) @>
                exactly <@ Vec.AllDifferent : V2d * V2d -> bool @>
                exactly <@ fun (u : V3d) (v : V3d) -> u.AllDifferent(v) @>
                exactly <@ Vec.AllDifferent : V3d * V3d -> bool @>
                exactly <@ fun (u : V4d) (v : V4d) -> u.AllDifferent(v) @>
                exactly <@ Vec.AllDifferent : V4d * V4d -> bool @>

                exactly <@ fun (u : V2i) (v : V2i) -> u.AllDifferent(v) @>
                exactly <@ Vec.AllDifferent : V2i * V2i -> bool @>
                exactly <@ fun (u : V3i) (v : V3i) -> u.AllDifferent(v) @>
                exactly <@ Vec.AllDifferent : V3i * V3i -> bool @>
                exactly <@ fun (u : V4i) (v : V4i) -> u.AllDifferent(v) @>
                exactly <@ Vec.AllDifferent : V4i * V4i -> bool @>
            ]

            // ==========================================================================
            // GEOMETRIC
            // ==========================================================================

            CIntrinsic.simple "distance", [
                exactly <@ Vec.Distance : V2i * V2i -> _ @>
                exactly <@ Vec.Distance : V3i * V3i -> _ @>
                exactly <@ Vec.Distance : V4i * V4i -> _ @>
                exactly <@ Vec.Distance : V2l * V2l -> _ @>
                exactly <@ Vec.Distance : V3l * V3l -> _ @>
                exactly <@ Vec.Distance : V4l * V4l -> _ @>
                exactly <@ Vec.Distance : V2f * V2f -> _ @>
                exactly <@ Vec.Distance : V3f * V3f -> _ @>
                exactly <@ Vec.Distance : V4f * V4f -> _ @>
                exactly <@ Vec.Distance : V2d * V2d -> _ @>
                exactly <@ Vec.Distance : V3d * V3d -> _ @>
                exactly <@ Vec.Distance : V4d * V4d -> _ @>
                generic <@ Vec.distance : V4d -> V4d -> _ @>
            ]

            CIntrinsic.simple "normalize", [
                exactly <@ Vec.Normalized : V2i -> _ @>
                exactly <@ Vec.Normalized : V2l -> _ @>
                exactly <@ Vec.Normalized : V2f -> _ @>
                exactly <@ Vec.Normalized : V2d -> _ @>
                exactly <@ Vec.Normalized : V3i -> _ @>
                exactly <@ Vec.Normalized : V3l -> _ @>
                exactly <@ Vec.Normalized : V3f -> _ @>
                exactly <@ Vec.Normalized : V3d -> _ @>
                exactly <@ Vec.Normalized : V4i -> _ @>
                exactly <@ Vec.Normalized : V4l -> _ @>
                exactly <@ Vec.Normalized : V4f -> _ @>
                exactly <@ Vec.Normalized : V4d -> _ @>
                exactly <@ fun (v : V2i) -> v.Normalized @>
                exactly <@ fun (v : V3i) -> v.Normalized @>
                exactly <@ fun (v : V4i) -> v.Normalized @>
                exactly <@ fun (v : V2l) -> v.Normalized @>
                exactly <@ fun (v : V3l) -> v.Normalized @>
                exactly <@ fun (v : V4l) -> v.Normalized @>
                exactly <@ fun (v : V2f) -> v.Normalized @>
                exactly <@ fun (v : V3f) -> v.Normalized @>
                exactly <@ fun (v : V4f) -> v.Normalized @>
                exactly <@ fun (v : V2d) -> v.Normalized @>
                exactly <@ fun (v : V3d) -> v.Normalized @>
                exactly <@ fun (v : V4d) -> v.Normalized @>
                generic <@ Vec.normalize : V3d -> V3d @>
            ]

            CIntrinsic.simple "reflect", [
                exactly <@ Vec.Reflect : V2f * V2f -> _ @>
                exactly <@ Vec.Reflect : V3f * V3f -> _ @>
                exactly <@ Vec.Reflect : V4f * V4f -> _ @>
                exactly <@ Vec.Reflect : V2d * V2d -> _ @>
                exactly <@ Vec.Reflect : V3d * V3d -> _ @>
                exactly <@ Vec.Reflect : V4d * V4d -> _ @>
            ]
            CIntrinsic.tagged "reflect({1}, {0})", [
                generic <@ Vec.reflect : V3d -> V3d -> V3d @>
            ]

            CIntrinsic.simple "refract", [
                exactly <@ Vec.Refract : V2f * V2f * _ -> _ @>
                exactly <@ Vec.Refract : V3f * V3f * _ -> _ @>
                exactly <@ Vec.Refract : V4f * V4f * _ -> _ @>
                exactly <@ Vec.Refract : V2d * V2d * _ -> _ @>
                exactly <@ Vec.Refract : V3d * V3d * _ -> _ @>
                exactly <@ Vec.Refract : V4d * V4d * _ -> _ @>
            ]

            CIntrinsic.tagged "refract({2},{1},{0})", [
                generic <@ Vec.refract : float -> V3d -> V3d -> V3d @>
            ]

            // ==========================================================================
            // MATRIX
            // ==========================================================================

            CIntrinsic.simple "determinant", [
                exactly <@ Mat.Determinant : M22f -> _ @>
                exactly <@ Mat.Determinant : M33f -> _ @>
                exactly <@ Mat.Determinant : M44f -> _ @>
                exactly <@ Mat.Determinant : M22d -> _ @>
                exactly <@ Mat.Determinant : M33d -> _ @>
                exactly <@ Mat.Determinant : M44d -> _ @>
                exactly <@ fun (v : M22f) -> v.Determinant @>
                exactly <@ fun (v : M33f) -> v.Determinant @>
                exactly <@ fun (v : M44f) -> v.Determinant @>
                exactly <@ fun (v : M22d) -> v.Determinant @>
                exactly <@ fun (v : M33d) -> v.Determinant @>
                exactly <@ fun (v : M44d) -> v.Determinant @>
                generic <@ Mat.det : M22d -> float @>
            ]

            CIntrinsic.simple "inverse", [
                exactly <@ Mat.Inverse : M22f -> _ @>
                exactly <@ Mat.Inverse : M33f -> _ @>
                exactly <@ Mat.Inverse : M44f -> _ @>
                exactly <@ Mat.Inverse : M22d -> _ @>
                exactly <@ Mat.Inverse : M33d -> _ @>
                exactly <@ Mat.Inverse : M44d -> _ @>
                exactly <@ fun (v : M22f) -> v.Inverse @>
                exactly <@ fun (v : M33f) -> v.Inverse @>
                exactly <@ fun (v : M44f) -> v.Inverse @>
                exactly <@ fun (v : M22d) -> v.Inverse @>
                exactly <@ fun (v : M33d) -> v.Inverse @>
                exactly <@ fun (v : M44d) -> v.Inverse @>
                generic <@ Mat.inverse : M22d -> M22d @>
            ]

            // ==========================================================================
            // BITWISE stuff
            // ==========================================================================
            CIntrinsic.simple "bitfieldExtract", [
                exactly <@ Bitwise.BitFieldExtract : uint32 * _ * _ -> _ @>
                exactly <@ Bitwise.BitFieldExtract : int * _ * _ -> _ @>
                exactly <@ Bitwise.BitFieldExtract : uint64 * _ * _ -> _ @>
                exactly <@ Bitwise.BitFieldExtract : int64 * _ * _ -> _ @>
            ]

            CIntrinsic.simple "bitfieldInsert", [
                exactly <@ Bitwise.BitFieldInsert : uint32 * _ * _ * _ -> _ @>
                exactly <@ Bitwise.BitFieldInsert : int * _ * _ * _ -> _ @>
                exactly <@ Bitwise.BitFieldInsert : uint64 * _ * _ * _ -> _ @>
                exactly <@ Bitwise.BitFieldInsert : int64 * _ * _ * _ -> _ @>
            ]

            CIntrinsic.simple "bitfieldReverse", [
                exactly <@ Bitwise.BitFieldReverse : uint32 -> _ @>
                exactly <@ Bitwise.BitFieldReverse : int -> _ @>
                exactly <@ Bitwise.BitFieldReverse : uint64 -> _ @>
                exactly <@ Bitwise.BitFieldReverse : int64 -> _ @>
            ]

            CIntrinsic.simple "bitCount", [
                exactly <@ Bitwise.BitCount : uint32 -> _ @>
                exactly <@ Bitwise.BitCount : int -> _ @>
                exactly <@ Bitwise.BitCount : uint64 -> _ @>
                exactly <@ Bitwise.BitCount : int64 -> _ @>
            ]

            CIntrinsic.simple "findMSB", [
                exactly <@ Bitwise.MSB : uint32 -> _ @>
                exactly <@ Bitwise.MSB : int -> _ @>
                exactly <@ Bitwise.MSB : uint64 -> _ @>
                exactly <@ Bitwise.MSB : int64 -> _ @>
            ]

            CIntrinsic.simple "findLSB", [
                exactly <@ Bitwise.LSB : uint32 -> _ @>
                exactly <@ Bitwise.LSB : int -> _ @>
                exactly <@ Bitwise.LSB : uint64 -> _ @>
                exactly <@ Bitwise.LSB : int64 -> _ @>
            ]

            CIntrinsic.simple "floatBitsToInt", [
                exactly <@ Fun.FloatToBits : float32 -> _ @>
                exactly <@ Bitwise.FloatBitsToInt @>
            ]

            CIntrinsic.simple "floatBitsToUint", [
                exactly <@ Bitwise.FloatBitsToUInt @>
            ]

            CIntrinsic.simple "intBitsToFloat", [
                exactly <@ Fun.FloatFromBits : int32 -> _ @>
                exactly <@ Bitwise.IntBitsToFloat @>
            ]

            CIntrinsic.simple "uintBitsToFloat", [
                exactly <@ Bitwise.UIntBitsToFloat @>
            ]

            // ==========================================================================
            // GLOBAL
            // ==========================================================================

            CIntrinsic.simple "packUnorm2x16", [ exactly <@ packUnorm2x16 @> ]
            CIntrinsic.simple "packSnorm2x16", [ exactly <@ packSnorm2x16 @> ]
            CIntrinsic.simple "packUnorm4x8", [ exactly <@ packUnorm4x8 @> ]
            CIntrinsic.simple "packSnorm4x8", [ exactly <@ packSnorm4x8 @> ]

            CIntrinsic.simple "unpackUnorm2x16", [ exactly <@ unpackUnorm2x16 @> ]
            CIntrinsic.simple "unpackSnorm2x16", [ exactly <@ unpackSnorm2x16 @> ]
            CIntrinsic.simple "unpackUnorm4x8", [ exactly <@ unpackUnorm4x8 @> ]
            CIntrinsic.simple "unpackSnorm4x8", [ exactly <@ unpackSnorm4x8 @> ]

            CIntrinsic.simple "dFdx", [ generic <@ ddx : float -> _ @> ]
            CIntrinsic.simple "dFdy", [ generic <@ ddy : float -> _ @> ]
            CIntrinsic.simple "dFdxFine", [ generic <@ ddxFine : float -> _ @> ]
            CIntrinsic.simple "dFdyFine", [ generic <@ ddyFine : float -> _ @> ]
            CIntrinsic.simple "dFdxCoarse", [ generic <@ ddxCoarse : float -> _ @> ]
            CIntrinsic.simple "dFdyCoarse", [ generic <@ ddyCoarse : float -> _ @> ]
            CIntrinsic.tagged "discard", [ exactly <@ discard @> ]
            CIntrinsic.simple "EmitVertex", [ exactly <@ emitVertex @> ]
            CIntrinsic.simple "EndPrimitive", [
                exactly <@ restartStrip @>
                exactly <@ endPrimitive @>
            ]

            CIntrinsic.tagged "ivec3(gl_NumWorkGroups)", [ exactly <@ getWorkGroupCount @> ]
            CIntrinsic.tagged "ivec3(gl_WorkGroupID)", [ exactly <@ getWorkGroupId @> ]
            CIntrinsic.tagged "ivec3(gl_LocalInvocationID)", [ exactly <@ getLocalId @> ]
            CIntrinsic.tagged "ivec3(gl_GlobalInvocationID)", [ exactly <@ getGlobalId @> ]
            CIntrinsic.tagged "int(gl_LocalInvocationIndex)", [ exactly <@ getLocalIndex @> ]
            CIntrinsic.tagged "ivec3(gl_WorkGroupSize)", [ exactly <@ getWorkGroupSize @> ]
            CIntrinsic.tagged "barrier()", [ exactly <@ barrier @> ]

            CIntrinsic.simple "traceRayEXT", [ RaytracingIntrinsics.traceRayMeth ]
            CIntrinsic.simple "executeCallableEXT", [ RaytracingIntrinsics.executeCallableMeth ]
            CIntrinsic.simple "reportIntersectionEXT", [ exactly <@ reportIntersection @> ]
            CIntrinsic.tagged "ignoreIntersectionEXT", [ exactly <@ ignoreIntersection @> ]
            CIntrinsic.tagged "terminateRayEXT", [ exactly <@ terminateRay @> ]
        ]

    let (|TextureLookup|_|) (mi : MethodInfo) =
        match mi with
        | Method(name, ((ImageType(_, dim, isArray, isMS, valueType)::_) as args)) ->

            let plainArgs(skip : int) =
                args |> List.skip skip |> List.mapi (fun i _ -> sprintf "{%d}" (skip + i)) |> String.concat ", "

            let coordComponents =
                match dim with
                | SamplerDimension.Sampler1d -> 1
                | SamplerDimension.Sampler2d -> 2
                | SamplerDimension.Sampler3d -> 3
                | SamplerDimension.SamplerCube -> 2
                | _ -> failwithf "unknown sampler dimension: %A" dim

            let loadStoreArgs() =
                let consumedArgs, reshapedArgs =
                    if isArray || dim = SamplerDimension.SamplerCube then
                        3, sprintf "{0}, ivec%d({1}, {2})" (coordComponents + 1)
                    else
                        1, "{0}"

                let args = List.skip consumedArgs args

                let rest =
                    match args with
                    | [] -> ""
                    | _ ->
                        args |> List.mapi (fun i _ -> sprintf "{%d}" (i + consumedArgs)) |> String.concat ", " |> sprintf ", %s"

                reshapedArgs + rest

            let functionName =
                match name with
                | "get_Size" -> "imageSize({0})"
                | "get_Samples" -> "imageSamples({0})"
                | "Load" | "get_Item" -> sprintf "imageLoad(%s)" (loadStoreArgs())
                | "Store" | "set_Item" -> sprintf "imageStore(%s)" (loadStoreArgs())
                | "AtomicAdd" -> sprintf "imageAtomicAdd(%s)" (loadStoreArgs())
                | "AtomicMin" -> sprintf "imageAtomicMin(%s)" (loadStoreArgs())
                | "AtomicMax" -> sprintf "imageAtomicMax(%s)" (loadStoreArgs())
                | "AtomicAnd" -> sprintf "imageAtomicAnd(%s)" (loadStoreArgs())
                | "AtomicOr" -> sprintf "imageAtomicOr(%s)" (loadStoreArgs())
                | "AtomicXor" -> sprintf "imageAtomicXor(%s)" (loadStoreArgs())
                | "AtomicExchange" -> sprintf "imageAtomicExchange(%s)" (loadStoreArgs())
                | "AtomicCompareExchange" -> sprintf "imageAtomicCompSwap(%s)" (loadStoreArgs())
                | _ -> failwithf "unknown image function %s" name

            let extensions =
                match name with
                | "get_Samples" -> ["GL_ARB_shader_texture_image_samples"]
                | _ -> []

            Some (functionName, Set.ofList extensions)

        | Method(name, ((SamplerType(dim, isArray, isShadow, isMS, valueType)::_) as args)) ->
            let coordComponents =
                match dim with
                    | SamplerDimension.Sampler1d -> 1
                    | SamplerDimension.Sampler2d -> 2
                    | SamplerDimension.Sampler3d -> 3
                    | SamplerDimension.SamplerCube -> 3
                    | _ -> failwithf "unknown sampler dimension: %A" dim

            let fetchArgs() =
                let consumedArgs, sampleArgs =
                    match isArray with
                        | true -> 3, sprintf "{0}, ivec%d({1}, {2})" (coordComponents + 1)
                        | false -> 2, "{0}, {1}"

                let args = List.skip consumedArgs args

                let rest =
                    match args with
                    | [] ->
                        ""
                    | _ ->
                        args |> List.mapi (fun i _ -> sprintf "{%d}" (i + consumedArgs)) |> String.concat ", " |> sprintf ", %s"


                sampleArgs + rest

            let sampleArgs (separateCmpArg : bool) =
                let consumedArgs, sampleArgs =
                    match isArray, isShadow && not separateCmpArg with
                    | true, true ->
                        if coordComponents = 3 then
                            4, "{0}, vec4({1}, {2}), {3}" // Cube array shadow sampler has separate cmp argument
                        else
                            4, sprintf "{0}, vec%d({1}, {2}, {3})" (coordComponents + 2)

                    | true, false ->
                        3, sprintf "{0}, vec%d({1}, {2})" (coordComponents + 1)

                    | false, true ->
                        if coordComponents = 1 then
                            3, "{0}, vec3({1}, 0, {2})" // 1D shadow sampler has unused 2nd component
                        else
                            3, sprintf "{0}, vec%d({1}, {2})" (coordComponents + 1)

                    | false, false ->
                        2, "{0}, {1}"

                let args = List.skip consumedArgs args

                let rest =
                    match args with
                        | [] -> ""
                        | _ ->
                            args |> List.mapi (fun i _ -> sprintf "{%d}" (i + consumedArgs)) |> String.concat ", " |> sprintf ", %s"

                sampleArgs + rest


            let projArgs() =
                let consumedArgs, sampleArgs =
                    if isShadow then
                        if coordComponents = 1 then
                            3, "{0}, vec4({1}, {2}, 0).xwzy"
                        else
                            3, "{0}, vec4({1}, {2}).xywz"
                    else
                        2, "{0}, {1}"

                let args = List.skip consumedArgs args

                let rest =
                    match args with
                        | [] -> ""
                        | _ ->
                            args |> List.mapi (fun i _ -> sprintf "{%d}" (i + consumedArgs)) |> String.concat ", " |> sprintf ", %s"

                sampleArgs + rest


            let plainArgs(skip : int) =
                args |> List.skip skip |> List.mapi (fun i _ -> sprintf "{%d}" (skip + i)) |> String.concat ", "

            let functionName =
                match name with
                | "get_Size" ->
                    if isMS then "textureSize({0})"
                    else "textureSize({0}, 0)"

                | "get_MipMapLevels" ->
                    if isMS then "1"
                    else "textureQueryLevels({0})"

                | "GetSize" ->
                    if isMS then "textureSize({0})"
                    else "textureSize({0}, {1})"

                | "get_Samples" ->
                    "textureSamples({0})"

                | "Sample" -> sprintf "texture(%s)" (sampleArgs false)
                | "SampleOffset" -> sprintf "textureOffset(%s)" (sampleArgs false)
                | "SampleProj" -> sprintf "textureProj(%s)" (projArgs())
                | "SampleLevel" -> sprintf "textureLod(%s)" (sampleArgs false)
                | "SampleLevelOffset" -> sprintf "textureLodOffset(%s)" (sampleArgs false)
                | "SampleGrad" -> sprintf "textureGrad(%s)" (sampleArgs false)
                | "Gather" -> sprintf "textureGather(%s)" (sampleArgs true)
                | "GatherOffset" -> sprintf "textureGatherOffset(%s)" (sampleArgs true)

                | "Read" -> sprintf "texelFetch(%s)" (fetchArgs())
                | "get_Item" -> sprintf "texelFetch(%s)" (fetchArgs())

                | "QueryLod" -> sprintf "textureQueryLod(%s)" (plainArgs 0)

                | _ -> failwithf "unknown sampler function %s" name

            let extensions =
                match name with
                | "get_Samples" -> ["GL_ARB_shader_texture_image_samples"]
                | _ -> []

            Some (functionName, Set.ofList extensions)
        | _ ->
            None

