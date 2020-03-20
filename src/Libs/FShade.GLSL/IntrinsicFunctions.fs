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
            // TRIGONOMETRIC
            // ==========================================================================

            CIntrinsic.simple "sin", [
                exactly <@ Math.Sin @>
                exactly <@ Fun.Sin : float -> float @>
                exactly <@ Fun.Sin : float32 -> float32 @>
                generic <@ sin @> 
            ]

            CIntrinsic.simple "cos", [
                exactly <@ Math.Cos @>
                exactly <@ Fun.Cos : float -> float @>
                exactly <@ Fun.Cos : float32 -> float32 @>
                generic <@ cos @> 
            ]

            CIntrinsic.simple "tan", [
                exactly <@ Math.Tan @>
                exactly <@ Fun.Tan : float -> float @>
                exactly <@ Fun.Tan : float32 -> float32 @>
                generic <@ tan @> 
            ]


            CIntrinsic.simple "asin", [
                exactly <@ Math.Asin @>
                exactly <@ Fun.Asin : float -> float @>
                exactly <@ Fun.Asin : float32 -> float32 @>
                generic <@ asin @> 
            ]

            CIntrinsic.simple "acos", [
                exactly <@ Math.Acos @>
                exactly <@ Fun.Acos : float -> float @>
                exactly <@ Fun.Acos : float32 -> float32 @>
                generic <@ acos @> 
            ]

            CIntrinsic.simple "atan", [
                exactly <@ Math.Atan @>
                exactly <@ Fun.Atan : float -> float @>
                exactly <@ Fun.Atan : float32 -> float32 @>
                generic <@ atan @> 

                exactly <@ Math.Atan2 @>
                exactly <@ Fun.Atan2 : float * float -> float @>
                exactly <@ Fun.Atan2 : float32 * float32 -> float32 @>
                generic <@ atan2 @> 
            ]

            CIntrinsic.simple "sinh", [
                exactly <@ Math.Sinh @>
                exactly <@ Fun.Sinh : float -> float @>
                exactly <@ Fun.Sinh : float32 -> float32 @>
                generic <@ sinh @> 
            ]

            CIntrinsic.simple "cosh", [
                exactly <@ Math.Cosh @>
                exactly <@ Fun.Cosh : float -> float @>
                exactly <@ Fun.Cosh : float32 -> float32 @>
                generic <@ cosh @> 
            ]

            CIntrinsic.simple "tanh", [
                exactly <@ Math.Tanh @>
                exactly <@ Fun.Tanh : float -> float @>
                exactly <@ Fun.Tanh : float32 -> float32 @>
                generic <@ tanh @> 
            ]

            CIntrinsic.simple "asinh", [
                exactly <@ Fun.Asinh : float -> float @>
                exactly <@ Fun.Asinh : float32 -> float32 @>
            ]

            CIntrinsic.simple "acosh", [
                exactly <@ Fun.Acosh : float -> float @>
                exactly <@ Fun.Acosh : float32 -> float32 @>
            ]

            CIntrinsic.simple "atanh", [
                exactly <@ Fun.Atanh : float -> float @>
                exactly <@ Fun.Atanh : float32 -> float32 @>
            ]

            // ==========================================================================
            // EXPONENTIAL
            // ==========================================================================
            CIntrinsic.simple "pow", [
                exactly <@ Fun.Pow : float * float -> float @>
                exactly <@ Fun.Pow : float32 * float32 -> float32 @>
                exactly <@ Math.Pow @>
                exactly <@ Fun.Pow : V2d * V2d -> V2d @>
                exactly <@ Fun.Pow : V2d * float -> V2d @>
                exactly <@ Fun.Pow : float * V2d -> V2d @>
                exactly <@ Fun.Pow : V2f * V2f -> V2f @>
                exactly <@ Fun.Pow : V2f * float32 -> V2f @>
                exactly <@ Fun.Pow : float32 * V2f -> V2f @>
                exactly <@ Fun.Pow : V3d * V3d -> V3d @>
                exactly <@ Fun.Pow : V3d * float -> V3d @>
                exactly <@ Fun.Pow : float * V3d -> V3d @>
                exactly <@ Fun.Pow : V3f * V3f -> V3f @>
                exactly <@ Fun.Pow : V3f * float32 -> V3f @>
                exactly <@ Fun.Pow : float32 * V3f -> V3f @>
                exactly <@ Fun.Pow : V4d * V4d -> V4d @>
                exactly <@ Fun.Pow : V4d * float -> V4d @>
                exactly <@ Fun.Pow : float * V4d -> V4d @>
                exactly <@ Fun.Pow : V4f * V4f -> V4f @>
                exactly <@ Fun.Pow : V4f * float32 -> V4f @>
                exactly <@ Fun.Pow : float32 * V4f -> V4f @>
                generic <@ ( ** ) : float -> float -> float @>
                generic <@ Operators.( ** ) : float -> float -> float @>
                generic <@ pow : float -> float -> float @>
                generic <@ pown : float -> int -> float @>
            ]

            // TODO: still existing???
            //CIntrinsic.tagged "pow({0}, vec2({1}))", [
            //    exactly <@ VecFun.Pow : V2f * float -> V2f @>
            //    exactly <@ Fun.Pow : V2d * float -> V2d @>
            //]

            //CIntrinsic.tagged "pow({0}, vec3({1}))", [
            //    exactly <@ VecFun.Pow : V3f * float -> V3f @>
            //    exactly <@ VecFun.Pow : V3d * float -> V3d @>
            //]

            //CIntrinsic.tagged "pow({0}, vec4({1}))", [
            //    exactly <@ VecFun.Pow : V4f * float -> V4f @>
            //    exactly <@ VecFun.Pow : V4d * float -> V4d @>
            //]

            CIntrinsic.simple "exp", [
                exactly <@ Math.Exp @> 
                exactly <@ Fun.Exp : float32 -> float32 @>
                exactly <@ Fun.Exp : float -> float @>
                exactly <@ Fun.Exp : V2f -> V2f @>
                exactly <@ Fun.Exp : V2d -> V2d @>
                exactly <@ Fun.Exp : V3f -> V3f @>
                exactly <@ Fun.Exp : V3d -> V3d @>
                exactly <@ Fun.Exp : V4f -> V4f @>
                exactly <@ Fun.Exp : V4d -> V4d @>
                generic <@ exp @>
            ]

            CIntrinsic.simple "log", [
                exactly <@ Math.Log @> 
                exactly <@ Fun.Log : float32 -> float32 @>
                exactly <@ Fun.Log : float -> float @>
                exactly <@ Fun.Log : V2f -> V2f @>
                exactly <@ Fun.Log : V2d -> V2d @>
                exactly <@ Fun.Log : V3f -> V3f @>
                exactly <@ Fun.Log : V3d -> V3d @>
                exactly <@ Fun.Log : V4f -> V4f @>
                exactly <@ Fun.Log : V4d -> V4d @>
                generic <@ log @>
            ]

            CIntrinsic.simple "log2", [
                exactly <@ Fun.Log2 : float32 -> float32 @>
                exactly <@ Fun.Log2 : float -> float @>
                exactly <@ Fun.Log2 : V2f -> V2f @>
                exactly <@ Fun.Log2 : V2d -> V2d @>
                exactly <@ Fun.Log2 : V3f -> V3f @>
                exactly <@ Fun.Log2 : V3d -> V3d @>
                exactly <@ Fun.Log2 : V4f -> V4f @>
                exactly <@ Fun.Log2 : V4d -> V4d @>
                generic <@ log2 : float -> float @>
            ]

            CIntrinsic.simple "sqrt", [
                exactly <@ Math.Sqrt @>
                exactly <@ Fun.Sqrt : float32 -> float32 @>
                exactly <@ Fun.Sqrt : float -> float @>
                exactly <@ Fun.Sqrt : V2f -> V2f @>
                exactly <@ Fun.Sqrt : V2d -> V2d @>
                exactly <@ Fun.Sqrt : V3f -> V3f @>
                exactly <@ Fun.Sqrt : V3d -> V3d @>
                exactly <@ Fun.Sqrt : V4f -> V4f @>
                exactly <@ Fun.Sqrt : V4d -> V4d @>
                exactly <@ V2f.Sqrt : V2f -> V2f @>
                exactly <@ V2d.Sqrt : V2d -> V2d @>
                exactly <@ V3f.Sqrt : V3f -> V3f @>
                exactly <@ V3d.Sqrt : V3d -> V3d @>
                exactly <@ V4f.Sqrt : V4f -> V4f @>
                exactly <@ V4d.Sqrt : V4d -> V4d @>
                exactly <@ V4d.Sqrt : V4d -> V4d @>
                generic <@ sqrt @>
            ]

            CIntrinsic.tagged "sqrt({0})", [
                exactly <@ Fun.Sqrt : int8 -> float @>
                exactly <@ Fun.Sqrt : int16 -> float @>
                exactly <@ Fun.Sqrt : int32 -> float @>
                exactly <@ Fun.Sqrt : int64 -> float @>
                exactly <@ Fun.Sqrt : uint8 -> float @>
                exactly <@ Fun.Sqrt : uint16 -> float @>
                exactly <@ Fun.Sqrt : uint32 -> float @>
                exactly <@ Fun.Sqrt : uint64 -> float @>
                exactly <@ Fun.Sqrt : V2i -> _ @>
                exactly <@ Fun.Sqrt : V3i -> _ @>
                exactly <@ Fun.Sqrt : V4i -> _ @>
                exactly <@ Fun.Sqrt : V2l -> _ @>
                exactly <@ Fun.Sqrt : V3l -> _ @>
                exactly <@ Fun.Sqrt : V4l -> _ @>
            ]
            
            CIntrinsic.tagged "pow({0}, 0.333333333333)", [
                exactly <@ Fun.Cbrt : int8 -> float @>
                exactly <@ Fun.Cbrt : int16 -> float @>
                exactly <@ Fun.Cbrt : int32 -> float @>
                exactly <@ Fun.Cbrt : int64 -> float @>
                exactly <@ Fun.Cbrt : uint8 -> float @>
                exactly <@ Fun.Cbrt : uint16 -> float @>
                exactly <@ Fun.Cbrt : uint32 -> float @>
                exactly <@ Fun.Cbrt : uint64 -> float @>
                exactly <@ Fun.Cbrt : V2f -> _ @>
                exactly <@ Fun.Cbrt : V3f -> _ @>
                exactly <@ Fun.Cbrt : V4f -> _ @>
                exactly <@ Fun.Cbrt : V2d -> _ @>
                exactly <@ Fun.Cbrt : V3d -> _ @>
                exactly <@ Fun.Cbrt : V4d -> _ @>
                exactly <@ Fun.Cbrt : V2i -> _ @>
                exactly <@ Fun.Cbrt : V3i -> _ @>
                exactly <@ Fun.Cbrt : V4i -> _ @>
                exactly <@ Fun.Cbrt : V2l -> _ @>
                exactly <@ Fun.Cbrt : V3l -> _ @>
                exactly <@ Fun.Cbrt : V4l -> _ @>
                generic <@ cbrt : float -> _ @>
            ]
            
            CIntrinsic.tagged "pow({0}, 2.0)", [
                exactly <@ Fun.Square : int8 -> _ @>
                exactly <@ Fun.Square : int16 -> _ @>
                exactly <@ Fun.Square : int32 -> _ @>
                exactly <@ Fun.Square : int64 -> _ @>
                exactly <@ Fun.Square : uint8 -> _ @>
                exactly <@ Fun.Square : uint16 -> _ @>
                exactly <@ Fun.Square : uint32 -> _ @>
                exactly <@ Fun.Square : uint64 -> _ @>
                exactly <@ Fun.Square : V2f -> _ @>
                exactly <@ Fun.Square : V3f -> _ @>
                exactly <@ Fun.Square : V4f -> _ @>
                exactly <@ Fun.Square : V2d -> _ @>
                exactly <@ Fun.Square : V3d -> _ @>
                exactly <@ Fun.Square : V4d -> _ @>
                exactly <@ Fun.Square : V2i -> _ @>
                exactly <@ Fun.Square : V3i -> _ @>
                exactly <@ Fun.Square : V4i -> _ @>
                exactly <@ Fun.Square : V2l -> _ @>
                exactly <@ Fun.Square : V3l -> _ @>
                exactly <@ Fun.Square : V4l -> _ @>
                generic <@ sqr : float -> _ @>
                
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
                
                exactly <@ fun (v : int8) -> v.Sign() @>
                exactly <@ fun (v : int16) -> v.Sign() @>
                exactly <@ fun (v : int32) -> v.Sign() @>
                exactly <@ fun (v : int64) -> v.Sign() @>
                exactly <@ fun (v : float32) -> v.Sign() @>
                exactly <@ fun (v : float) -> v.Sign() @>
                exactly <@ fun (v : decimal) -> v.Sign() @>

                generic <@ sign @>
            ]
            
            CIntrinsic.tagged "sign({0})", [
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
                
                exactly <@ Fun.Min : int8 * int8 -> _ @>
                exactly <@ Fun.Min : int16 * int16 -> _ @>
                exactly <@ Fun.Min : int32 * int32 -> _ @>
                exactly <@ Fun.Min : int64 * int64 -> _ @>
                exactly <@ Fun.Min : uint8 * uint8 -> _ @>
                exactly <@ Fun.Min : uint16 * uint16 -> _ @>
                exactly <@ Fun.Min : uint32 * uint32 -> _ @>
                exactly <@ Fun.Min : uint64 * uint64 -> _ @>
                exactly <@ Fun.Min : float32 * float32 -> _ @>
                exactly <@ Fun.Min : float * float -> _ @>
                
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
                
                exactly <@ Fun.Max : int8 * int8 -> _ @>
                exactly <@ Fun.Max : int16 * int16 -> _ @>
                exactly <@ Fun.Max : int32 * int32 -> _ @>
                exactly <@ Fun.Max : int64 * int64 -> _ @>
                exactly <@ Fun.Max : uint8 * uint8 -> _ @>
                exactly <@ Fun.Max : uint16 * uint16 -> _ @>
                exactly <@ Fun.Max : uint32 * uint32 -> _ @>
                exactly <@ Fun.Max : uint64 * uint64 -> _ @>
                exactly <@ Fun.Max : float32 * float32 -> _ @>
                exactly <@ Fun.Max : float * float -> _ @>
                
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

                generic <@ Operators.max @>
                generic <@ max : int -> int -> int @> 
            ]

            CIntrinsic.custom "clamp" [2; 0; 1], [
                generic <@ clamp : float -> float -> float -> float @>
            ]
            CIntrinsic.simple "clamp", [
                exactly <@ Fun.Clamp : int8 * int8 * int8 -> _ @>
                exactly <@ Fun.Clamp : int16 * int16 * int16 -> _ @>
                exactly <@ Fun.Clamp : int32 * int32 * int32 -> _ @>
                exactly <@ Fun.Clamp : int64 * int64 * int64 -> _ @>
                exactly <@ Fun.Clamp : uint8 * uint8 * uint8 -> _ @>
                exactly <@ Fun.Clamp : uint16 * uint16 * uint16 -> _ @>
                exactly <@ Fun.Clamp : uint32 * uint32 * uint32 -> _ @>
                exactly <@ Fun.Clamp : uint64 * uint64 * uint64 -> _ @>
                exactly <@ Fun.Clamp : float32 * float32 * float32 -> _ @>
                exactly <@ Fun.Clamp : float * float * float -> _ @>
                exactly <@ Fun.Clamp : decimal * decimal * decimal -> _ @>
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


            CIntrinsic.custom "smoothstep"[1; 2; 0], [
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

            CIntrinsic.custom "mix" [1; 2; 0], [
                exactly <@ Fun.Lerp : float32 * float32 * float32 -> _ @>
                exactly <@ Fun.Lerp : float * float * float -> _ @>
                exactly <@ Fun.Lerp : float32 * V2f * V2f -> V2f @>
                exactly <@ Fun.Lerp : float * V2d * V2d -> V2d @>
                exactly <@ Fun.Lerp : float32 * V2f * V2f -> V2f @>
                exactly <@ Fun.Lerp : float32 * V3f * V3f -> V3f @>
                exactly <@ Fun.Lerp : float * V3d * V3d -> V3d @>
                exactly <@ Fun.Lerp : float32 * V3f * V3f -> V3f @>
                exactly <@ Fun.Lerp : float32 * V4f * V4f -> V4f @>
                exactly <@ Fun.Lerp : float * V4d * V4d -> V4d @>
                exactly <@ Fun.Lerp : float32 * V4f * V4f -> V4f @>
            ]
            
            CIntrinsic.simple "mix", [
                generic <@ lerp : V3d -> V3d -> float -> V3d @>
            ]

            CIntrinsic.simple "isnan", [
                exactly <@ Fun.IsNaN : float32 -> bool @>
                exactly <@ Fun.IsNaN : float -> bool @>
                exactly <@ Single.IsNaN @>
                exactly <@ Double.IsNaN @>

                exactly <@ fun (v : V2f) -> v.IsNaN @>
                exactly <@ fun (v : V3f) -> v.IsNaN @>
                exactly <@ fun (v : V4f) -> v.IsNaN @>
                exactly <@ fun (v : V2d) -> v.IsNaN @>
                exactly <@ fun (v : V3d) -> v.IsNaN @>
                exactly <@ fun (v : V4d) -> v.IsNaN @>
            ]

            CIntrinsic.simple "isinf", [
                exactly <@ Fun.IsInfinity : float32 -> bool @>
                exactly <@ Fun.IsInfinity : float -> bool @>
                exactly <@ Single.IsInfinity @>
                exactly <@ Double.IsInfinity @>
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

            CIntrinsic.simple "transpose", [
                exactly <@ fun (v : M22f) -> v.Transposed @>
                exactly <@ fun (v : M33f) -> v.Transposed @>
                exactly <@ fun (v : M44f) -> v.Transposed @>
                exactly <@ fun (v : M22d) -> v.Transposed @>
                exactly <@ fun (v : M33d) -> v.Transposed @>
                exactly <@ fun (v : M44d) -> v.Transposed @>
                generic <@ Mat.transpose : M22d -> M22d @>
            ]

            CIntrinsic.simple "determinant", [
                exactly <@ fun (v : M22f) -> v.Determinant @>
                exactly <@ fun (v : M33f) -> v.Determinant @>
                exactly <@ fun (v : M44f) -> v.Determinant @>
                exactly <@ fun (v : M22d) -> v.Determinant @>
                exactly <@ fun (v : M33d) -> v.Determinant @>
                exactly <@ fun (v : M44d) -> v.Determinant @>
                //generic <@ Mat.det : M22d -> float @>
            ]

            CIntrinsic.simple "inverse", [
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
                exactly <@ Bitwise.FloatBitsToInt @>
            ]
            CIntrinsic.simple "floatBitsToUInt", [
                exactly <@ Bitwise.FloatBitsToUInt @>
            ]
            CIntrinsic.simple "intBitsToFloat", [
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


        ]

    let (|TextureLookup|_|) (mi : MethodInfo) =
        match mi with
            | Method(name, ((ImageType(_, dim, isArray, isMS, valueType)::_) as args)) ->
                
                let plainArgs(skip : int) =
                    args |> List.skip skip |> List.mapi (fun i _ -> sprintf "{%d}" (skip + i)) |> String.concat ", "
                        
                let argCount = List.length args - 1

                let functionName = 
                    match name with
                        | "get_Size" -> "imageSize({0})"
                        | "get_Item" when argCount = 1 -> sprintf "imageLoad(%s)" (plainArgs 0)
                        | "set_Item" when argCount = 2 -> sprintf "imageStore(%s)" (plainArgs 0)
                        | "get_Item" -> sprintf "imageLoad({0}, ivec%d(%s), 0)" argCount (plainArgs 1)
                        | "AtomicCompareExchange" -> sprintf "imageAtomicCompSwap(%s)" (plainArgs 0)
                        | "AtomicAdd" -> sprintf "imageAtomicAdd(%s)" (plainArgs 0)



                        | _ ->failwithf "unknown image function %A" name
                        
                Some functionName

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
                                ", 0"
                            | _ ->
                                args |> List.mapi (fun i _ -> sprintf "{%d}" (i + consumedArgs)) |> String.concat ", " |> sprintf ", %s"


                    sampleArgs + rest

                let sampleArgs() = 
                    let consumedArgs, sampleArgs =
                        match isArray, isShadow with
                            | true, true -> 4, sprintf "{0}, vec%d({1}, {2}, {3})" (coordComponents + 2)
                            | true, false -> 3, sprintf "{0}, vec%d({1}, {2})" (coordComponents + 1)
                            | false, true -> 3, sprintf "{0}, vec%d({1}, {2})" (coordComponents + 1)
                            | false, false -> 2, "{0}, {1}"

                    let args = List.skip consumedArgs args

                    let rest =
                        match args with
                            | [] -> ""
                            | _ ->
                                args |> List.mapi (fun i _ -> sprintf "{%d}" (i + consumedArgs)) |> String.concat ", " |> sprintf ", %s"


                    sampleArgs + rest
                let projArgs() =
                    let consumedArgs, sampleArgs =
                        match isArray, isShadow with
                            | true, true -> 4, sprintf "{0}, vec%d({1}, {2}, {3})" (coordComponents + 3)
                            | true, false -> 3, sprintf "{0}, vec%d({1}, {2})" (coordComponents + 2)
                            | false, true -> 3, sprintf "{0}, vec%d({1}, {2})" (coordComponents + 2)
                            | false, false -> 2, "{0}, {1}"

                    let args = List.skip consumedArgs args

                    let rest =
                        match args with
                            | [] -> ""
                            | _ ->
                                args |> List.mapi (fun i _ -> sprintf "{%d}" (i + consumedArgs)) |> String.concat ", " |> sprintf ", %s"


                    sampleArgs + rest

                let plainArgs(skip : int) =
                    args |> List.skip skip |> List.mapi (fun i _ -> sprintf "{%d}" (skip + i)) |> String.concat ", "
                        

                let argCount = List.length args - 1

                match name with
                    | "get_Size" -> 
                        if isMS then Some "textureSize({0})"
                        else Some "textureSize({0}, 0)"

                    | "get_MipMapLevels" -> 
                        if isMS then Some "1"
                        else Some "textureQueryLevels({0})"

                    | "GetSize" -> 
                        if isMS then Some "textureSize({0})"
                        else Some "textureSize({0}, {1})"


                    | "Sample" -> sprintf "texture(%s)" (sampleArgs()) |> Some
                    | "SampleOffset" -> sprintf "textureOffset(%s)" (sampleArgs()) |> Some
                    | "SampleProj" -> sprintf "textureProj(%s)" (projArgs()) |> Some
                    | "SampleLevel" -> sprintf "textureLod(%s)" (sampleArgs()) |> Some
                    | "SampleGrad" -> sprintf "textureGrad(%s)" (sampleArgs()) |> Some
                    | "Gather" -> sprintf "textureGather(%s)" (plainArgs 0) |> Some
                    | "GatherOffset" -> sprintf "textureGatherOffset(%s)" (plainArgs 0) |> Some
                    
                    | "Read" -> sprintf "texelFetch(%s)" (fetchArgs()) |> Some
                    | "get_Item" -> sprintf "texelFetch(%s)" (fetchArgs()) |> Some

                    | "QueryLod" -> sprintf "textureQueryLod(%s)" (plainArgs 0) |> Some

                    | name -> None
            | _ ->
                None

