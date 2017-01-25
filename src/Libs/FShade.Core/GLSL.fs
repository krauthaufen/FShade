namespace FShade

open System
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape

open Aardvark.Base
open FShade.Imperative


module MethodTable =
    let rec tryGetMethod (e : Expr) =
        match e with
            | Call(_,mi,_) -> Some mi
            | PropertyGet(_,pi,_) -> Some pi.GetMethod

            | ShapeVar _ -> None
            | ShapeLambda(_,b) -> tryGetMethod b
            | ShapeCombination(_,args) -> args |> List.tryPick tryGetMethod

    let getMethod (e : Expr) =
        e |> tryGetMethod |> Option.get

    let ofList (list : list<'a * list<MethodInfo>>) =
        let store = Dict<MethodInfo, 'a>()

        for (value, mis) in list do
            for mi in mis do
                store.[mi] <- value
            
        fun (mi : MethodInfo) ->
            match store.TryGetValue mi with
                | (true, v) -> 
                    Some v

                | _ ->
                    if mi.IsGenericMethod then
                        match store.TryGetValue (mi.GetGenericMethodDefinition()) with
                            | (true, v) -> Some v
                            | _ -> None
                    else
                        None

[<AutoOpen>]
module private Operators =
    let exactly (e : Expr) =
        let mi = MethodTable.getMethod e
        if mi.IsGenericMethod then mi.GetGenericMethodDefinition()
        else mi

    let generic (e : Expr) =
        MethodTable.getMethod e





type private GLSLBackend() =
    inherit Compiler.Backend()
    static let instance = GLSLBackend() :> Compiler.Backend

    static let (|Numeric|_|) = 
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
                generic <@ ( ** ) : float -> float -> float @>
                generic <@ pow : float -> float -> float @>
                generic <@ pown @>
            ]

            CIntrinsic.simple "exp", [
                exactly <@ Math.Exp @> 
                exactly <@ Fun.Exp : float32 -> float32 @>
                exactly <@ Fun.Exp : float -> float @>
                generic <@ exp @>
            ]

            CIntrinsic.simple "log", [
                exactly <@ Math.Log @> 
                exactly <@ Fun.Log : float32 -> float @>
                exactly <@ Fun.Log : float -> float @>
                generic <@ log @>
            ]

            CIntrinsic.simple "log2", [
                exactly <@ Fun.Log2 : float32 -> float @>
                exactly <@ Fun.Log2 : float -> float @>
            ]

            CIntrinsic.simple "sqrt", [
                exactly <@ Math.Sqrt @>
                exactly <@ Fun.Sqrt : float32 -> float32 @>
                exactly <@ Fun.Sqrt : float -> float @>
                generic <@ sqrt @>
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

                exactly <@ fun (v : V2i) -> v.Abs @>
                exactly <@ fun (v : V3i) -> v.Abs @>
                exactly <@ fun (v : V4i) -> v.Abs @>
                exactly <@ fun (v : V2l) -> v.Abs @>
                exactly <@ fun (v : V3l) -> v.Abs @>
                exactly <@ fun (v : V4l) -> v.Abs @>
                exactly <@ fun (v : V2f) -> v.Abs @>
                exactly <@ fun (v : V3f) -> v.Abs @>
                exactly <@ fun (v : V4f) -> v.Abs @>
                exactly <@ fun (v : V2d) -> v.Abs @>
                exactly <@ fun (v : V3d) -> v.Abs @>
                exactly <@ fun (v : V4d) -> v.Abs @>

                generic <@ abs @>
            ]

            CIntrinsic.simple "sign", [
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
                generic <@ truncate @>
            ]

            CIntrinsic.simple "round", [
                exactly <@ Math.Round : float -> _ @>
                exactly <@ Math.Round : decimal -> _ @>
                exactly <@ Fun.Round : float -> _ @>
                exactly <@ Fun.Round : float32 -> _ @>
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
                exactly <@ Fun.Frac : decimal -> _ @>
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
                
                exactly <@ V2i.Min @>
                exactly <@ V3i.Min @>
                exactly <@ V4i.Min @>
                exactly <@ V2l.Min @>
                exactly <@ V3l.Min @>
                exactly <@ V4l.Min @>
                exactly <@ V2f.Min @>
                exactly <@ V3f.Min @>
                exactly <@ V4f.Min @>
                exactly <@ V2d.Min @>
                exactly <@ V3d.Min @>
                exactly <@ V4d.Min @>

                generic <@ min @> 
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
                
                exactly <@ V2i.Max @>
                exactly <@ V3i.Max @>
                exactly <@ V4i.Max @>
                exactly <@ V2l.Max @>
                exactly <@ V3l.Max @>
                exactly <@ V4l.Max @>
                exactly <@ V2f.Max @>
                exactly <@ V3f.Max @>
                exactly <@ V4f.Max @>
                exactly <@ V2d.Max @>
                exactly <@ V3d.Max @>
                exactly <@ V4d.Max @>

                generic <@ max @> 
            ]

            CIntrinsic.custom "clamp" [2; 0; 1], [
                generic <@ clamp @>
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

            CIntrinsic.simple "mix", [
                exactly <@ Fun.Lerp : float32 * float32 * float32 -> _ @>
                exactly <@ Fun.Lerp : float * float * float -> _ @>
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
            // GEOMETRIC
            // ==========================================================================

            CIntrinsic.simple "distance", [
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
                exactly <@ Vec.reflect : V3d -> V3d -> V3d @> 
            ]

            CIntrinsic.simple "refract", [
                exactly <@ Vec.refract : V3d -> V3d -> float -> V3d @> 
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
                generic <@ Mat.det : M22d -> float @>
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

        ]

    static let (|TextureLookup|_|) (mi : MethodInfo) =
        match mi with
            | Method(name, ((SamplerType(dim, isArray, isShadow, isMS, valueType)::_) as args)) ->
                let coordComponents =
                    match dim with
                        | SamplerDimension.Sampler1d -> 1
                        | SamplerDimension.Sampler2d -> 2
                        | SamplerDimension.Sampler3d -> 3
                        | SamplerDimension.SamplerCube -> 3
                        | _ -> failwithf "unknown sampler dimension: %A" dim


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


                        | "Sample" -> sprintf "texture(%s)" (sampleArgs())
                        | "SampleOffset" -> sprintf "textureOffset(%s)" (sampleArgs())
                        | "SampleProj" -> sprintf "textureProj(%s)" (projArgs())
                        | "SampleLevel" -> sprintf "textureLod(%s)" (sampleArgs())
                        | "SampleGrad" -> sprintf "textureGrad(%s)" (sampleArgs())
                        | "Gather" -> sprintf "textureGather(%s)" (plainArgs 0)
                        | "GatherOffset" -> sprintf "textureGatherOffset(%s)" (plainArgs 0)
                        | "Read" -> sprintf "texelFetch(%s)" (plainArgs 0)

                        | "get_Item" when argCount = 1 -> sprintf "texelFetch(%s, 0)" (plainArgs 0)
                        | "get_Item" -> sprintf "texelFetch({0}, ivec%d(%s), 0)" argCount (plainArgs 1)
                        | name -> failwithf "unknown sampler function %A" name

                Some functionName
            | _ ->
                None

    static member Instance = instance


    override x.TryGetIntrinsicMethod (c : MethodInfo) =
        match c with
            | Numeric f -> Some f
            | TextureLookup fmt -> Some (CIntrinsic.tagged fmt)

            | MethodQuote <@ ddx : float -> _ @> _  -> CIntrinsic.simple "dFdx" |> Some
            | MethodQuote <@ ddy : float -> _ @> _  -> CIntrinsic.simple "dFdy" |> Some
            | MethodQuote <@ discard @> _           -> CIntrinsic.tagged "discard" |> Some
            | MethodQuote <@ emitVertex @> _        -> CIntrinsic.simple "EmitVertex" |> Some
            | MethodQuote <@ restartStrip @> _      -> CIntrinsic.simple "EndPrimitive" |> Some
            | MethodQuote <@ endPrimitive @> _      -> CIntrinsic.simple "EndPrimitive" |> Some

            | _ -> None
    
    override x.TryGetIntrinsicCtor (c : ConstructorInfo) =
        None

    override x.TryGetIntrinsicType (t : Type) =
        match t with
            | SamplerType(dim, arr, shadow, ms, valueType) -> 
                let dimStr =
                    match dim with
                        | SamplerDimension.Sampler1d -> "1D"
                        | SamplerDimension.Sampler2d -> "2D"
                        | SamplerDimension.Sampler3d -> "3D"
                        | SamplerDimension.SamplerCube -> "Cube"
                        | _ -> failwith "unsupported sampler dimension"

                let shadowSuffix = if shadow then "Shadow" else ""
                let msSuffix = if ms then "MS" else ""
                let typePrefix = 
                    match valueType.Name with
                        | "V4i" -> "i"
                        | _ -> ""

                let name = 
                    if arr then sprintf "%ssampler%s%sArray%s" typePrefix dimStr msSuffix shadowSuffix
                    else sprintf "%ssampler%s%s%s" typePrefix dimStr msSuffix shadowSuffix 

                CIntrinsicType.simple name |> Some

            | _ ->
                None


module GLSL =

    let backend = GLSLBackend.Instance

    module private String =
        let private lineBreak = System.Text.RegularExpressions.Regex("\r\n")

        let indent (str : string) =
            let lines = lineBreak.Split str
            let prefix = "    "
            lines |> Seq.map (fun l -> if l.Length > 0 then prefix + l else l) |> String.concat "\r\n"

//    type CompilerConfiguration =
//        {
//            languageVersion : Version
//            enabledExtensions : Set<string>
//            createUniformBuffers : bool
//            createGlobalUniforms : bool
//            createBindings : bool
//            createDescriptorSets : bool
//            createInputLocations : bool
//            expectRowMajorMatrices : bool
//
//            createPerStageUniforms : bool
//
//            flipHandedness : bool
//            depthRange : Range1d
//
//            treatUniformsAsInputs : bool
//
//        }

    type Config =
        {
            version                 : Version
            enabledExtensions       : Set<string>
            createUniformBuffers    : bool
            
//            createBindings          : bool
//            createDescriptorSets    : bool
            createInputLocations    : bool
            createPerStageUniforms  : bool
            
            reverseMatrixLogic      : bool
            flipHandedness          : bool
            depthRange              : Range1d
        }

    let glsl410 =
        {
            version                 = Version(4,1,0)
            enabledExtensions       = Set.empty
            createUniformBuffers    = true
            createInputLocations    = true
            createPerStageUniforms  = false
            
            reverseMatrixLogic      = false
            flipHandedness          = false
            depthRange              = Range1d(-1.0, 1.0)
        }

    let version120 = Version(1,2,0)

    type State =
        {
            config          : Config
            stages          : ShaderStageDescription
            inputs          : Set<string>
        }
 

    let private builtInInputs =
        Dictionary.ofList [
            ShaderStage.Vertex, 
                Map.ofList [
                    Intrinsics.VertexId, "gl_VertexID"
                    Intrinsics.InstanceId, "gl_InstanceID"
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.PatchVertices, "gl_PatchVertices"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.InvocationId, "gl_InvocationID"
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.TessCoord, "gl_TessCoord"
                    Intrinsics.PatchVertices, "gl_PatchVertices"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.TessLevelInner, "gl_TessLevelInner"
                    Intrinsics.TessLevelOuter, "gl_TessLevelOuter"
                ]
                
            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.InvocationId, "gl_InvocationID"
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.FragCoord, "gl_FragCoord"
                    Intrinsics.PointCoord, "gl_PointCoord"
                    Intrinsics.FrontFacing, "gl_FronFacing"
                    Intrinsics.SampleId, "gl_SampleID"
                    Intrinsics.SamplePosition, "gl_SamplePosition"
                    Intrinsics.SampleMask, "gl_SampleMask"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.Layer, "gl_Layer"
                    Intrinsics.ViewportIndex, "gl_ViewportIndex"
                ]
            ]

    let private builtInOutputs =
        Dictionary.ofList [
            ShaderStage.Vertex,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.TessLevelInner, "gl_TessLevelInner"
                    Intrinsics.TessLevelOuter, "gl_TessLevelOuter"
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                ]

            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.Layer, "gl_Layer"
                    Intrinsics.ViewportIndex, "gl_ViewportIndex"
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.Depth, "gl_FragDepth"
                    Intrinsics.SampleMask, "gl_SampleMask"
                ]

        ]

  
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        let ofConfig (c : Config) =
            {
                config = c
                stages = { prev = None; self = ShaderStage.Vertex; next = None }
                inputs = Set.empty
            }

        let prefixes =
            Dictionary.ofList [
                ShaderStage.Vertex,         "vs_"
                ShaderStage.TessControl,    "tc_"
                ShaderStage.TessEval,       "te_"
                ShaderStage.Geometry,       "gs_"
                ShaderStage.Fragment,       "fs_"
            ]

        let regularName (kind : ParameterKind) (name : string) (s : State) =
            let c = s.config

            if kind = ParameterKind.Output && name = Intrinsics.Position && s.stages.next = Some ShaderStage.Fragment then
                "gl_Position"

            else
                match kind, s.stages with
                    | ParameterKind.Input, { prev = None }              -> name
                    | ParameterKind.Input, { self = s }                 -> prefixes.[s] + name

                    | ParameterKind.Output, { next = Some n }           -> prefixes.[n] + name
                    | ParameterKind.Output, { next = None }             -> name + "Out"
                    | _                                                 -> name


        let parameterName (kind : ParameterKind) (name : string) (s : State) =
            let c = s.config

            match kind with
                | ParameterKind.Input -> 
                    match Map.tryFind name builtInInputs.[s.stages.self] with
                        | Some s -> s
                        | None -> regularName kind name s

                | ParameterKind.Output -> 
                    match Map.tryFind name builtInOutputs.[s.stages.self] with
                        | Some s -> s
                        | None -> regularName kind name s

                | _ ->
                    regularName kind name s



    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CType =
        let rec glsl (t : CType) =
            match t with
                | CType.CBool                               -> "bool"
                | CType.CVoid                               -> "void"
                | CType.CInt(true, (8 | 16 | 32 | 64))      -> "int"
                | CType.CInt(false, (8 | 16 | 32 | 64))     -> "uint"
                | CType.CFloat(16)                          -> "half"
                | CType.CFloat(32 | 64)                     -> "float"
                
                | CType.CVector(CType.CInt(true, (8 | 16 | 32 | 64)), d)   -> "ivec" + string d
                | CType.CVector(CType.CFloat(32 | 64), d)   -> "vec" + string d
                | CType.CMatrix(CType.CFloat(32 | 64), r,c) -> "mat" + string c + "x" + string r

                | CType.CArray(t, l)                        -> glsl t + "[" + string l + "]"
                | CType.CStruct(n,_,_)                      -> n

                | CType.CIntrinsic it                       -> it.intrinsicTypeName

                | _ -> failwithf "[GLSL] cannot compile type %A" t
                    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CParameterModifier =
        let glsl (m : CParameterModifier) =
            match m with
                | CParameterModifier.In -> ""
                | CParameterModifier.ByRef -> "inout"

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CParameter =
        let glsl (p : CParameter) =
            let m = CParameterModifier.glsl p.modifier
            match p.ctype with
                | CArray(et, l) ->
                    let t = CType.glsl et
                    if m = "" then sprintf "%s %s[%d]" t p.name l
                    else sprintf "%s %s %s[%d]" m t p.name l

                | pt ->
                    let t = CType.glsl pt
                    if m = "" then sprintf "%s %s" t p.name
                    else sprintf "%s %s %s" m t p.name

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CFunctionSignature =
        let glsl (s : CFunctionSignature) =
            let ret = s.returnType |> CType.glsl
            let args = s.parameters |> Seq.map CParameter.glsl |> String.concat ", "
            sprintf "%s %s(%s)" ret s.name args
            
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CLiteral =
        let glsl (l : CLiteral) =
            match l with
                | CLiteral.CBool v -> if v then "true" else "false"
                | CLiteral.Null -> "null"
                | CLiteral.CIntegral v -> string v
                | CLiteral.CFractional v -> 
                    let str = v.ToString(System.Globalization.CultureInfo.InvariantCulture)
                    if str.Contains "." then str
                    else str + ".0"

                | CLiteral.CString v -> "\"" + v + "\""            

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CExpr =

        let rec private componentNames =
            LookupTable.lookupTable [
                CVecComponent.X, "x"
                CVecComponent.Y, "y"
                CVecComponent.Z, "z"
                CVecComponent.W, "w"
            ]

        let swizzle (l : list<CVecComponent>) =
            l |> Seq.map componentNames |> String.concat ""

        let rec glsl (c : State) (e : CExpr) =
            let glsl = glsl c
            let reverse = c.config.reverseMatrixLogic
            match e with
                | CVar v -> 
                    v.name

                | CValue(_,v) -> 
                    CLiteral.glsl v

                | CCall(f, args) ->
                    let args = args |> Seq.map glsl |> String.concat ", "
                    sprintf "%s(%s)" f.name args

                | CReadInput(kind,_, name, idx) ->
                    let name = c |> State.parameterName kind name
                    match idx with
                        | Some idx -> sprintf "%s[%s]" name (glsl idx)
                        | None -> name

                | CCallIntrinsic(_, f, args) ->
                    match f.tag with
                        | null -> 
                            let args = args |> Seq.map glsl |> String.concat ", "
                            sprintf "%s(%s)" f.intrinsicName args

                        | :? string as format ->
                            let args = args |> Array.map (fun a -> a |> glsl :> obj)
                            String.Format(format, args)
                            
                        | _ ->
                            failwithf "[FShade] invalid GLSL intrinsic: %A" f

                | CConditional(_, c, i, e) ->
                    sprintf "(%s ? %s : %s)" (glsl c) (glsl i) (glsl e)

                | CNeg(_, v) ->
                    sprintf "(-%s)" (glsl v)

                | CNot(_, v) ->
                    sprintf "(!%s)" (glsl v)

                | CAdd(_, l, r) -> sprintf "(%s + %s)" (glsl l) (glsl r)
                | CSub(_, l, r) -> sprintf "(%s - %s)" (glsl l) (glsl r)
                | CMul(_, l, r) -> sprintf "(%s * %s)" (glsl l) (glsl r)
                | CDiv(_, l, r) -> sprintf "(%s / %s)" (glsl l) (glsl r)
                | CMod(_, l, r) -> sprintf "(%s %% %s)" (glsl l) (glsl r)

                | CMulMatMat(_, l, r) when reverse -> sprintf "(%s * %s)" (glsl r) (glsl l)
                | CMulMatVec(_, l, r) when reverse -> sprintf "(%s * %s)" (glsl r) (glsl l)
                | CMatrixElement(_, m, r, c) when reverse -> sprintf "%s[%d][%d]" (glsl m) r c

                | CMulMatMat(_, l, r) -> sprintf "(%s * %s)" (glsl l) (glsl r)
                | CMulMatVec(_, l, r) -> sprintf "(%s * %s)" (glsl l) (glsl r)
                | CMatrixElement(_, m, r, c) -> sprintf "%s[%d][%d]" (glsl m) c r

                | CTranspose(_,m) -> sprintf "transpose(%s)" (glsl m)
                | CDot(_, l, r) -> sprintf "dot(%s, %s)" (glsl l) (glsl r)
                | CCross(_, l, r) -> sprintf "cross(%s, %s)" (glsl l) (glsl r)
                | CVecSwizzle(_, v, s) -> sprintf "%s.%s" (glsl v) (swizzle s)
                | CNewVector(r, _, args) -> sprintf "%s(%s)" (CType.glsl r) (args |> List.map glsl |> String.concat ", ")
                | CVecLength(_, v) -> sprintf "length(%s)" (glsl v)

                | CMatrixFromRows _ | CMatrixFromCols _ -> string e
                
                | CConvert(t, v) -> sprintf "%s(%s)" (CType.glsl t) (glsl v)
                
                | CAnd(l, r) -> sprintf "(%s && %s)" (glsl l) (glsl r)    
                | COr(l, r) -> sprintf "(%s || %s)" (glsl l) (glsl r)    

                | CBitAnd(_, l, r) -> sprintf "(%s & %s)" (glsl l) (glsl r)
                | CBitOr(_, l, r) -> sprintf "(%s | %s)" (glsl l) (glsl r)
                | CBitXor(_, l, r) -> sprintf "(%s ^ %s)" (glsl l) (glsl r)

                
                | CLess(l, r) -> sprintf "(%s < %s)" (glsl l) (glsl r)
                | CLequal(l, r) -> sprintf "(%s <= %s)" (glsl l) (glsl r)
                | CGreater(l, r) -> sprintf "(%s > %s)" (glsl l) (glsl r)
                | CGequal(l, r) -> sprintf "(%s >= %s)" (glsl l) (glsl r)
                | CEqual(l, r) -> sprintf "(%s == %s)" (glsl l) (glsl r)
                | CNotEqual(l, r) -> sprintf "(%s != %s)" (glsl l) (glsl r)

                | CAddressOf(_, e) -> sprintf "&(%s)" (glsl e)
                | CField(_, e, f) -> sprintf "%s.%s" (glsl e) f
                | CItem(_, e, i) -> sprintf "%s[%s]" (glsl e) (glsl i)
                    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CLExpr = 
        let glsl (c : State) (e : CLExpr) = 
            e |> CLExpr.toExpr |> CExpr.glsl c
                     
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CRExpr =
        let glsl (c : State) (e : CRExpr) =
            match e with
                | CRArray(t, args) -> args |> List.map (CExpr.glsl c) |> String.concat ", " |> sprintf "{ %s }"
                | CRExpr e -> CExpr.glsl c e

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CStatement =
        let rec glsl (c : State) (s : CStatement) =
             match s with
                | CNop -> ""
                | CDo e -> CExpr.glsl c e
                | CDeclare(v, r) ->
                    match v.ctype with
                        | CArray(t, l) ->
                            match r with
                                | Some r -> sprintf "%s %s[%d] = %s" (CType.glsl t) v.name l (CRExpr.glsl c r)
                                | None -> sprintf "%s %s[%d]" (CType.glsl t) v.name l
                        | _ -> 
                            match r with
                                | Some r -> sprintf "%s %s = %s" (CType.glsl v.ctype) v.name (CRExpr.glsl c r)
                                | None -> sprintf "%s %s" (CType.glsl v.ctype) v.name

                | CWriteOutput(name, index, value) ->
                    let name = c |> State.parameterName ParameterKind.Output name

                    let name =
                        match index with
                            | Some idx -> sprintf "%s[%s]" name (CExpr.glsl c idx)
                            | _ -> name

                    match value with
                        | CRExpr.CRArray(_,values) ->
                            values |> Seq.map (CExpr.glsl c) |> Seq.mapi (sprintf "%s[%d] = %s" name) |> String.concat ";\r\n"

                        | CRExpr e -> 
                            sprintf "%s = %s" name (CExpr.glsl c e)
                            

                | CWrite(l, v) ->
                    sprintf "%s = %s" (CLExpr.glsl c l) (CExpr.glsl c v)

                | CIncrement(pre, l) ->
                    if pre then sprintf "++%s" (CLExpr.glsl c l)
                    else sprintf "%s++" (CLExpr.glsl c l)

                | CDecrement(pre, l) ->
                    if pre then sprintf "--%s" (CLExpr.glsl c l)
                    else sprintf "%s--" (CLExpr.glsl c l)

                | CSequential s ->
                    s |> List.map (glsl c) |> String.concat ";\r\n"

                | CReturn -> "return"
                | CBreak -> "break"
                | CContinue -> "continue"
                | CReturnValue v -> sprintf "return %s" (CExpr.glsl c v)

                | CFor(init, cond, step, body) ->
                    sprintf "for(%s; %s; %s)\r\n{\r\n%s;\r\n}" (glsl c init) (CExpr.glsl c cond) (glsl c step) (body |> glsl c |> String.indent)

                | CWhile(guard, body) ->
                    sprintf "while(%s)\r\n{\r\n%s\r\n}" (CExpr.glsl c guard) (body |> glsl c |> String.indent)

                | CDoWhile(guard, body) ->
                    sprintf "do\r\n{\r\n%s\r\n}\r\nwhile(%s)" (body |> glsl c |> String.indent) (CExpr.glsl c guard)

                | CIfThenElse(cond, i, CNop) ->
                    sprintf "if(%s)\r\n{\r\n%s;\r\n}" (CExpr.glsl c cond) (i |> glsl c |> String.indent)

                | CIfThenElse(cond, CNop, e) ->
                    sprintf "if(!(%s))\r\n{\r\n%s;\r\n}" (CExpr.glsl c cond) (e |> glsl c |> String.indent)

                | CIfThenElse(cond, i, e) ->
                    sprintf "if(%s)\r\n{\r\n%s;\r\n}\r\nelse\r\n{\r\n%s;\r\n}" (CExpr.glsl c cond) (i |> glsl c |> String.indent) (e |> glsl c |> String.indent)

                | CSwitch _ -> string s
               
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CUniform =
        let glsl (c : Config) (uniforms : list<CUniform>) =
            if c.createUniformBuffers then
                let buffers =
                    uniforms 
                        |> List.groupBy (fun u -> u.cUniformBuffer)

                let definitions = 
                    buffers |> List.map (fun (name, fields) ->
                        let fields = fields |> List.map (fun u -> sprintf "%s %s;" (CType.glsl u.cUniformType) u.cUniformName)

                        match name with
                            | None ->
                                fields |> List.map (sprintf "uniform %s") |> String.concat "\r\n"
                            | Some n ->
                                let fields = fields |> String.concat "\r\n"
                                sprintf "uniform %s\r\n{\r\n%s\r\n}" n (String.indent fields)


                    )

                String.concat "\r\n\r\n" definitions
            else
                uniforms
                    |> List.map (fun u -> sprintf "uniform %s %s;" (CType.glsl u.cUniformType) u.cUniformName)
                    |> String.concat "\r\n"


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CEntryParameter =
        let private regularDefinition (state : State) (kind : ParameterKind) (index : int) (p : CEntryParameter) =
            let c = state.config
            let decorations =
                p.cParamDecorations 
                |> Set.toList
                |> List.choose (fun d ->
                    match d with
                        | ParameterDecoration.Const -> Some "const"
                        | ParameterDecoration.Interpolation m ->
                            match m with
                                | InterpolationMode.Centroid -> Some "centroid"
                                | InterpolationMode.Flat -> Some "flat"
                                | InterpolationMode.NoPerspective -> Some "noperspective"
                                | InterpolationMode.Perspective -> Some "perspective"
                                | InterpolationMode.Sample -> Some "sample"
                                | InterpolationMode.PerPatch -> Some "patch"
                                | _ -> None
                        | ParameterDecoration.Memory _ ->
                            None

                )

            let decorations =
                match kind with
                    | ParameterKind.Input | ParameterKind.Output when c.createInputLocations && c.version > version120 ->
                        sprintf "layout(location = %d)" index :: decorations
                    | _ ->
                        decorations
                
            let decorations = 
                match decorations with
                    | [] -> ""
                    | _ -> String.concat " " decorations + " "

            let name = state |> State.parameterName kind p.cParamName


            let prefix =
                if c.version > version120 then
                    match kind with
                        | ParameterKind.Input -> "in "
                        | ParameterKind.Output -> "out "
                        | _ -> ""
                else
                    match kind with
                        | ParameterKind.Input -> "varying "
                        | ParameterKind.Output -> "varying "
                        | _ -> ""
                    

            match p.cParamType with
                | CPointer(_, t) ->
                    sprintf "%s%s%s %s[];" decorations prefix (CType.glsl t) name
                | _ -> 
                    sprintf "%s%s%s %s;" decorations prefix (CType.glsl p.cParamType) name

        let glsl (state : State) (kind : ParameterKind) (index : int) (p : CEntryParameter) =
            match kind with
                | ParameterKind.Input -> 
                    match Map.tryFind p.cParamName builtInInputs.[state.stages.self] with
                        | Some s -> None
                        | None -> regularDefinition state kind index p |> Some

                | ParameterKind.Output -> 
                    match state.stages.next, p.cParamName with
                        | Some ShaderStage.Fragment, "Positions" -> None
                        | _ -> 
                            match Map.tryFind p.cParamName builtInOutputs.[state.stages.self] with
                                | Some s -> None
                                | None -> regularDefinition state kind index p |> Some
                | _ ->
                    regularDefinition state kind index p |> Some

    
        let many (state : State) (kind : ParameterKind) (l : list<CEntryParameter>) =
            let definitions, _ = 
                l |> List.fold (fun (r,i) p ->
                    match glsl state kind i p with
                        | Some str ->
                            (r @ [str], i + 1)
                        | None ->
                            (r, i)
                ) ([], 0) 
            definitions
                

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CEntryDef =
        let glsl (c : Config) (e : CEntryDef) =
            let state =
                { 
                    config = c

                    inputs = e.cInputs |> List.map (fun p -> p.cParamName) |> Set.ofList

                    stages =
                        e.cDecorations 
                        |> List.tryPick (function EntryDecoration.Stages t -> Some t | _ -> None) 
                        |> Option.defaultValue {
                            prev = None
                            self = ShaderStage.Vertex
                            next = None
                        }
                }

            let prefix = 
                match state.stages.self with
                    | ShaderStage.Geometry -> 
                        let inputTopology = e.cDecorations |> List.tryPick (function EntryDecoration.InputTopology t -> Some t | _ -> None) |> Option.get
                        let outputTopology = e.cDecorations |> List.tryPick (function EntryDecoration.OutputTopology(t) -> Some(t) | _ -> None) |> Option.get
                        let vertexCount = e.cDecorations |> List.tryPick (function EntryDecoration.OutputVertices(t) -> Some(t) | _ -> None) |> Option.get
                        
                        let inputPrimitive =
                            match inputTopology with
                                | InputTopology.Point -> "points"
                                | InputTopology.Line -> "lines"
                                | InputTopology.LineAdjacency -> "lines_adjacency"
                                | InputTopology.Triangle -> "triangles"
                                | InputTopology.TriangleAdjacency -> "triangles_adjacency"
                                | InputTopology.Patch _ -> failwith "[FShade] GeometryShaders cannot use patches"

                        let outputPrimitive =
                            match outputTopology with
                                | OutputTopology.Points -> "points"
                                | OutputTopology.LineStrip -> "line_strip"
                                | OutputTopology.TriangleStrip -> "triangle_strip"

                        [
                            sprintf "layout(%s) in;" inputPrimitive
                            sprintf "layout(%s, max_vertices = %d) out;" outputPrimitive vertexCount
                        ]

                    | ShaderStage.TessControl ->
                        let inputTopology = e.cDecorations |> List.tryPick (function EntryDecoration.InputTopology t -> Some t | _ -> None) |> Option.get
                        
                        let vertices =
                            match inputTopology with
                                | InputTopology.Point -> 1
                                | InputTopology.Line -> 2
                                | InputTopology.LineAdjacency -> 4
                                | InputTopology.Triangle -> 3
                                | InputTopology.TriangleAdjacency -> 6
                                | InputTopology.Patch n -> n
                        
                        [
                            sprintf "layout(vertices = %d) out;" vertices
                        ]

                    | ShaderStage.TessEval ->
                        let inputTopology = e.cDecorations |> List.tryPick (function EntryDecoration.InputTopology t -> Some t | _ -> None) |> Option.get
                        
                        let inputTop =
                            match inputTopology with
                                | InputTopology.Line -> "isolines"
                                | InputTopology.Triangle -> "triangles"
                                | InputTopology.Patch 3 -> "triangles"
                                | InputTopology.Patch 4 -> "quads"
                                | t -> failwithf "[FShade] TessEvalShader cannot have %A inputs" t

                        [
                            sprintf "layout(%s, equal_spacing, ccw) in;" inputTop
                        ]
                    | _ ->
                        []


    
            let inputs,outputs, body  =
                e.cInputs, e.cOutputs, e.cBody

            let inputs = CEntryParameter.many state ParameterKind.Input inputs
            let outputs = CEntryParameter.many state ParameterKind.Output outputs
            let args = CEntryParameter.many state ParameterKind.Argument e.cArguments |> String.concat ", " 

            String.concat "\r\n" [
                yield! prefix
                yield! inputs
                yield! outputs
                yield sprintf "%s %s(%s)\r\n{\r\n%s;\r\n}" (CType.glsl e.cReturnType) e.cEntryName args (body |> CStatement.glsl state |> String.indent)
            ]

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CValueDef =
        let rec glsl (c : Config) (d : CValueDef) =
            match d with
                | CConditionalDef(d, inner) ->
                    let inner = inner |> List.map (glsl c) |> String.concat "\r\n"
                    sprintf "\r\n#ifdef %s\r\n%s\r\n\r\n#endif\r\n" d inner

                | CEntryDef e -> 
                    CEntryDef.glsl c e

                | CFunctionDef(signature, body) ->
                    sprintf "%s\r\n{\r\n%s;\r\n}\r\n" (CFunctionSignature.glsl signature) (body |> CStatement.glsl (State.ofConfig c) |> String.indent)

                | CConstant(t, n, init) ->
                    match t with
                        | CArray(t,l) -> sprintf "const %s %s[%d] = %s;" (CType.glsl t) n l (CRExpr.glsl (State.ofConfig c) init)
                        | _ -> sprintf "const %s %s = %s;" (CType.glsl t) n (CRExpr.glsl (State.ofConfig c) init)

                | CUniformDef us ->
                    if c.createPerStageUniforms then
                        CUniform.glsl c us
                    else
                        ""

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CTypeDef =
        let glsl (d : CTypeDef) =
            match d with
                | CStructDef(name, fields) ->
                    let fields = fields |> List.map (fun (t, n) -> sprintf "%s %s;" (CType.glsl t) n) |> String.concat "\r\n"
                    sprintf "struct %s\r\n{\r\n%s\r\n}" name (String.indent fields)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CModule =
        let glsl (c : Config) (m : CModule) =
            let definitions =
                List.concat [
                    yield [sprintf "#version %d%d0" c.version.Major c.version.Minor ]
                    yield c.enabledExtensions |> Seq.map (sprintf "#extension %s : enable;") |> Seq.toList

                    yield m.types |> List.map CTypeDef.glsl

                    if not c.createPerStageUniforms then
                        let uniforms = m.uniforms
                        yield [ CUniform.glsl c uniforms ]

                    yield m.values |> List.map (CValueDef.glsl c)
                ]

            definitions |> String.concat "\r\n\r\n"

        
    let compile (config : Config) (outputs : Map<string, Type>) (effect : Effect) =

        let adjustDepthRange =
            config.depthRange.Min <> -1.0 || config.depthRange.Max <> 1.0 || config.flipHandedness

        let effect =
            if adjustDepthRange then
                effect |> Effect.withDepthRange config.flipHandedness config.depthRange
            else
                effect

        effect
            |> Effect.link ShaderStage.Fragment outputs
            |> Effect.toModule
            |> Linker.compileAndLink backend
            |> CModule.glsl config