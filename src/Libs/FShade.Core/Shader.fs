namespace FShade

open System
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative
open FSharp.Data.Adaptive

#nowarn "4321"

[<RequireQualifiedAccess>]
type ShaderOutputVertices =
    | Unknown
    | Computed of int
    | UserGiven of int

/// Shader encapsulates all information about a specific shader.
type Shader =
    {
        /// the shader's stage
        shaderStage : ShaderStage
        /// the used inputs for the shader
        shaderInputs : Map<string, ParameterDescription>
        /// the provided outputs written by the shader
        shaderOutputs : Map<string, ParameterDescription>
        /// the used uniforms for the shader
        shaderUniforms : Map<string, UniformParameter>
        /// the optional input-topology for the shader
        shaderInputTopology : Option<InputTopology>
        /// the optional output-topology for the shader (including the maximal vertex-count)
        shaderOutputTopology : Option<OutputTopology>
        /// the optional maximal vertex-count for the shader
        shaderOutputVertices : ShaderOutputVertices
        /// the optional maximal vertex-count for the shader
        shaderOutputPrimitives : Option<int>
        /// the number of shader invocations (only useful for some stages)
        shaderInvocations : int
        /// the body for the shader
        shaderBody : Expr
        /// the shader's source info (if any)
        shaderDebugRange : Option<DebugRange>
        /// the shader's payloads (only useful in some raytracing shaders)
        shaderPayloads : Map<string, Type * int>
        /// the shader's incoming payload (only useful in some raytracing shaders)
        shaderPayloadIn : Option<string * Type>
        /// the shader's callable data types (only useful in some raytracing shaders)
        shaderCallableData : Map<string, Type * int>
        /// the shader's incoming callable data (only useful in some callable shaders)
        shaderCallableDataIn : Option<string * Type>
        /// the shader's hit attribute (only useful in some raytracing shaders)
        shaderHitAttribute : Option<string * Type>
        /// the shader's referenced ray types (only useful in some raytracing shaders)
        shaderRayTypes : Set<Symbol>
        /// the shader's referenced miss shaders (only useful in some raytracing shaders)
        shaderMissShaders : Set<Symbol>
        /// the shader's referenced callable shaders (only useful in some raytracing shaders)
        shaderCallableShaders : Set<Symbol>
        shaderDepthWriteMode : DepthWriteMode
    }

[<CompilerMessage("Preprocessor should not be used directly", 4321, IsHidden = true)>]
module Preprocessor =
    open Aardvark.Base.Monads.State
    open Aardvark.Base.ReflectionHelpers

    [<AutoOpen>] 
    module BuilderPatterns = 

        [<return: Struct>]
        let (|BuilderCombine|_|) (e : Expr) =
            match e with
            | BuilderCall(b, Method("Combine",_), [l;r]) ->
                ValueSome(b, l, r)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderDelay|_|) (e : Expr) =
            match e with
            | BuilderCall(b, Method("Delay",_), [Lambda(v,body)]) when v.Type = typeof<unit> ->
                ValueSome(b, body)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderRun|_|) (e : Expr) =
            match e with
            | BuilderCall(b, Method("Run",_), [e]) ->
                ValueSome(b, e)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderZero|_|) (e : Expr) =
            match e with
            | BuilderCall(b, Method("Zero",_), []) ->
                ValueSome(b)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderFor|_|) (e : Expr) =
            match e with
            | BuilderCall(b, Method("For",_) , [sequence; Lambda(v,body)]) ->
                match body with
                | Let(vi,Var(vo),body) when vo = v ->
                    ValueSome(b, vi, sequence, body)
                | _ ->
                    ValueSome(b, v, sequence, body)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderWhile|_|) (e : Expr) =
            match e with
            | BuilderCall(b, Method("While",_), [guard; body]) ->
                ValueSome(b, guard, body)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderYield|_|) (e : Expr) =
            match e with
            | BuilderCall(b, mi, [v]) when mi.Name = "Yield" ->
                ValueSome(b, mi, v)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderYieldFrom|_|) (e : Expr) =
            match e with
            | BuilderCall(b, mi, [v]) when mi.Name = "YieldFrom" ->
                ValueSome(b, mi, v)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderReturn|_|) (e : Expr) =
            match e with
            | BuilderCall(b, mi, [v]) when mi.Name = "Return" ->
                ValueSome(b, mi, v)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderReturnFrom|_|) (e : Expr) =
            match e with
            | BuilderCall(b, mi, [v]) when mi.Name = "ReturnFrom" ->
                ValueSome(b, mi, v)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderBind|_|) (e : Expr) =
            match e with
            | BuilderCall(b, Method("Bind",_), [e; Lambda(vv, c)]) ->
                match c with
                | Let(vi, Var ve, c) -> 
                    ValueSome(b, vi, e, c)
                | _ ->
                    ValueSome(b, vv, e, c)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|BuilderUsing|_|) (e : Expr) =
            match e with
            | BuilderCall(b, Method("Using",_), [e; Lambda(v,body)]) ->
                match body with
                | Let(vi, Var vo, body) when vo = v -> ValueSome(b, vi, e, b)
                | _ -> ValueSome(b, v, e, body)
            | _ ->
                ValueNone

        [<return: Struct>]
        let rec private (|ShaderBuilder|_|) (e : Expr) =
            match e with
            | BuilderCall(b, _, _) ->
                match Expr.TryEval b with
                | Some (:? IShaderBuilder as sb) -> ValueSome sb
                | _ -> ValueNone

            | ShapeLambda (_, ShaderBuilder sb) ->
                ValueSome sb

            | ShapeCombination(_, exprList) ->
                exprList |> List.tryPick (function
                    | ShaderBuilder sb -> Some sb
                    | _ -> None
                ) |> Option.toValueOption

            | _ -> ValueNone

        [<return: Struct>]
        let (|ComputeBuilder|_|) (e : Expr) =
            match e with
            | ShaderBuilder b when ShaderStage.isCompute b.ShaderStage -> ValueSome b
            | _ -> ValueNone

        [<return: Struct>]
        let (|RaytracingBuilder|_|) (e : Expr) =
            match e with
            | ShaderBuilder b when ShaderStage.isRaytracing b.ShaderStage -> ValueSome b
            | _ -> ValueNone


    [<AutoOpen>] 
    module OtherPatterns =

        [<return: Struct>]
        let rec (|Integral|_|) (t : Type) =
            match t with
            | TypeInfo.Patterns.Enum
            | TypeInfo.Patterns.Integral
            | TypeInfo.Patterns.VectorOf(_, Integral) -> ValueSome ()
            | TypeInfo.Patterns.MatrixOf(_, Integral) -> ValueSome ()
            | _ -> ValueNone

        [<return: Struct>]
        let rec (|FloatingPoint|_|) (t : Type) =
            match t with
            | TypeInfo.Patterns.Float32
            | TypeInfo.Patterns.Float64
            | TypeInfo.Patterns.VectorOf(_, (TypeInfo.Patterns.Float32 | TypeInfo.Patterns.Float64)) -> ValueSome ()
            | TypeInfo.Patterns.MatrixOf(_, (TypeInfo.Patterns.Float32 | TypeInfo.Patterns.Float64)) -> ValueSome ()
            | _ -> ValueNone

        [<return: Struct>]
        let (|Primitive|_|) (e : Expr) =
            let iface = e.Type.GetInterface("Primitive`1")
            if isNull iface then
                ValueNone
            else
                let countProp = e.Type.GetProperty("VertexCount", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                let vertices = countProp.GetValue(null) |> unbox<int>
                match e with
                | Value _ | Var _ | PropertyGet(None, _, []) -> ValueSome(e, vertices)
                | _ -> ValueNone

        [<return: Struct>]
        let (|PrimitiveVertexGet|_|) (e : Expr) =
            match e with
            | PropertyGet(Some (Primitive(p,_)), pi, []) ->
                match pi.PrimitiveIndex with
                | ValueSome index -> ValueSome(p, Expr.Value index)
                | _ -> failwithf "[FShade] cannot get primitive-property %A" pi

            | PropertyGet(Some (Primitive(p,_)), pi, [index]) when pi.Name = "Item" ->
                ValueSome (p, index)

            | _ ->
                ValueNone

        [<return: Struct>]
        let (|InputRead|_|) (vertexType : Type) (e : Expr) =
            match e with
            | PropertyGet(Some vertex, field, []) when vertex.Type = vertexType ->
                let isRecordField = FSharpType.GetRecordFields(vertexType, true) |> Array.exists (fun pi -> pi = field)
                if isRecordField then 
                    let parameter = { paramType = field.PropertyType; paramInterpolation = field.Interpolation }
                    ValueSome(vertex, field.Semantic, parameter)
                else
                    ValueNone
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|TessellateCall|_|) (e : Expr) =
            match e with
            | Call(None, MethodQuote <@ tessellateTriangle @> _, [li; l01;l12;l20]) ->
                ValueSome(3, [li], [l01;l12;l20])
            | Call(None, MethodQuote <@ tessellateQuad @> _, [lu;lv; l01;l30;l23;l12]) ->
                ValueSome(4, [lu; lv], [l01;l30;l23;l12])

            | _ ->
                ValueNone

        [<return: Struct>]
        let rec (|TrivialInput|_|) (e : Expr) =
            match e with
            | Value _
            | TupleGet(TrivialInput, _)
            | PropertyGet((None | Some TrivialInput), _, [])
            | ReadInput(_,_,(None | Some TrivialInput))
            | FieldGet((None | Some TrivialInput), _) ->
                ValueSome ()
            | _ ->
                ValueNone

        let private (|Cons|Nil|Other|) (u : UnionCaseInfo) =
            if u.DeclaringType.IsGenericType && u.DeclaringType.GetGenericTypeDefinition() = typedefof<list<_>> then
                if u.GetFields().Length = 2 then
                    Cons
                else 
                    Nil
            else
                Other

        [<return: Struct>]
        let rec (|NewList|_|) (e : Expr) =
            match e with
            | NewUnionCase(Cons, [h; NewList r]) ->
                ValueSome (h :: r)
            | NewUnionCase(Nil, []) ->
                ValueSome []
            | _-> 
                ValueNone

        [<return: Struct>]
        let rec (|NewSeq|_|) (e : Expr) : ValueOption<list<Expr>> =
            match e with
            | Coerce(NewSeq args, _) -> ValueSome args
            | NewArray(_,args) -> ValueSome args
            | NewList(args) -> ValueSome args
            | RangeSequence(Int32 min, Int32 step, Int32 max) -> ValueSome (List.map Expr.Value [min .. step .. max ])
            | _ -> ValueNone

        [<return: Struct>]
        let private (|ArrCtor|_|) (c : ConstructorInfo) =
            match c.DeclaringType with
            | ArrOf(l,t) -> ValueSome (l,t)
            | _ -> ValueNone

        [<return: Struct>]
        let (|NewArr|_|) (e : Expr) =
            match e with
            | NewObject(ArrCtor(l,t), args) ->
                match args with
                | [] -> ValueSome(t,l,[])
                | [NewSeq args] -> ValueSome(t,l,args)
                | _ -> ValueNone

            | _ ->
                ValueNone

        [<return: Struct>]
        let (|RemoveBuilder|_|) (e : Expr) =
            match e with
            | BuilderCombine(_, l, r) -> Expr.Seq [l;r] |> ValueSome
            | BuilderReturn(_, _, e) -> e |> ValueSome
            | BuilderReturnFrom(_, _, e) -> e |> ValueSome
            | BuilderRun(_, e) -> e |> ValueSome
            | BuilderDelay(_, e) -> e |> ValueSome
            | BuilderZero _ -> Expr.Unit |> ValueSome
            | BuilderFor(_, var, RangeSequence(first, step, last), body) -> Expr.ForInteger(var, first, step, last, body) |> ValueSome
            | BuilderFor(_, var, sequence, body) -> Expr.ForEach(var, sequence, body) |> ValueSome
            | BuilderWhile(_, Lambda(_, guard), body) -> Expr.WhileLoop(guard, body) |> ValueSome
            | _ -> ValueNone

        [<return: Struct>]
        let (|Scene|_|) (e : Expr) =
            if e.Type = typeof<Scene> then
                match Expr.TryEval e with
                | Some (:? Scene as s) ->
                    ValueSome {
                        uniformName = s.AccelerationStructure.Semantic
                        uniformType = typeof<Scene>
                        uniformValue = AccelerationStructure s.AccelerationStructure.Semantic
                    }

                | _ ->
                    failwith "[FShade] Failed to evaluate scene expression"
            else
                ValueNone

        [<return: Struct>]
        let (|TraceRay|_|) (e : Expr) =
            match e with
            | Call(Some (Scene accel), Method("TraceRay", _), args) ->
                match args with
                | [origin; direction; ray; miss; minT; maxT; flags; cullMask] ->
                    ValueSome {| accelerationStructure = accel; origin = origin; direction = direction;
                                 payload = ValueNone; ray = ray; miss = miss; minT = minT; maxT = maxT;
                                 flags = flags; cullMask = cullMask |}

                | [origin; direction; payload; ray; miss; minT; maxT; flags; cullMask] ->
                    ValueSome {| accelerationStructure = accel; origin = origin; direction = direction;
                                 payload = ValueSome payload; ray = ray; miss = miss; minT = minT; maxT = maxT;
                                 flags = flags; cullMask = cullMask |}

                | _ ->
                    failwithf "[FShade] Illformed call to TraceRay with %d arguments" args.Length

            | _ ->
                ValueNone

        [<return: Struct>]
        let private (|StaticMethod|_|) (e : Expr) =
            match e with
            | Call(None, mi, args) when mi.IsStatic -> ValueSome (mi.DeclaringType, mi.Name, args)
            | _ -> ValueNone

        [<return: Struct>]
        let (|ExecuteCallable|_|) (e : Expr) =
            match e with
            | StaticMethod(t, "Execute", args) when t = typeof<Callable> ->
                match args with
                | [id] -> ValueSome (id, ValueNone)
                | [data; id] -> ValueSome (id, ValueSome data)
                | _ ->
                    failwithf "[FShade] Illformed call to Callable.Execute with %d arguments" args.Length
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|ReportIntersection|_|) (e : Expr) =
            match e with
            | StaticMethod(t, "Report", args) when t = typeof<Intersection> ->
                match args with
                | [t; kind] -> ValueSome (t, kind, ValueNone)
                | [t; attribute; kind] -> ValueSome (t, kind, ValueSome attribute)
                | _ ->
                    failwithf "[FShade] Illformed call to Intersection.Report with %d arguments" args.Length
            | _ ->
                ValueNone

        let private (|SemanticProperty|_|) (pi : PropertyInfo) =
            let attributes = pi.GetCustomAttributes<SemanticAttribute>(true) |> Seq.toList
            attributes |> List.tryHead |> Option.map (fun att -> att.Semantic)

        [<return: Struct>]
        let (|SemanticInput|_|) (e : Expr) =
            match e with
            | PropertyGet(Some t, pi, []) when FSharpType.IsRecord t.Type ->
                match pi with
                | SemanticProperty semantic ->
                    let parameter = { paramType = pi.PropertyType; paramInterpolation = InterpolationMode.Default }
                    ValueSome (semantic, parameter)
                | _ ->
                    ValueNone
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|RayPayloadIn|_|) (e : Expr) =
            match e with
            | SemanticInput(semantic, _) when semantic = Intrinsics.RayPayloadIn -> ValueSome e.Type
            | _ -> ValueNone

        [<return: Struct>]
        let (|CallableDataIn|_|) (e : Expr) =
            match e with
            | SemanticInput(semantic, _) when semantic = Intrinsics.CallableDataIn -> ValueSome e.Type
            | _ -> ValueNone

        [<return: Struct>]
        let (|HitAttribute|_|) (e : Expr) =
            match e with
            | SemanticInput(semantic, _) when semantic = Intrinsics.HitAttribute -> ValueSome e.Type
            | _ -> ValueNone

    [<AutoOpen>]
    module private ComplexIntrinsicPatterns =
        open TypeInfo.Patterns

        let private vectorFields =
            ["X"; "Y"; "Z"; "W"]

        let private zeroOneVecProperty =
            System.Text.RegularExpressions.Regex @"^([XYZW]*[OINP]+[XYZW]*)+$"

        let private quantifiedBooleanOperations =
            System.Text.RegularExpressions.Regex
                @"^([iI]s|[aA]ny|[aA]ll)(Infinity|PositiveInfinity|NegativeInfinity|NaN|Finite|Equal|Different|Smaller|Greater|SmallerOrEqual|GreaterOrEqual)$"

        // These require the input expression to be duplicated and must be handled differently.
        let private emulatedSpecialFloatingPointCheck =
            System.Text.RegularExpressions.Regex @"^([iI]s|Any|All)(PositiveInfinity|NegativeInfinity|Finite)$"

        [<return: Struct>]
        let private (|FloatingPointExpr|_|) (e : Expr) =
            match e.Type with
            | Float32 | Float64 -> ValueSome e
            | _ -> ValueNone

        [<return: Struct>]
        let private (|VectorExpr|_|) (e : Expr) =
            match e.Type with
            | VectorOf(d, t) -> ValueSome (e, d, t)
            | _ -> ValueNone

        [<return: Struct>]
        let private (|MatrixExpr|_|) (e : Expr) =
            match e.Type with
            | MatrixOf(d, t) -> ValueSome (e, d, t)
            | _ -> ValueNone

        [<return: Struct>]
        let private (|Property|_|) (pi : PropertyInfo) =
            ValueSome pi.Name

        [<return: Struct>]
        let (|ConstantSwizzle|_|) (e : Expr) =
            match e with
            | PropertyGet(Some (VectorExpr(v, _, baseType)), prop, []) when zeroOneVecProperty.IsMatch prop.Name ->
                ValueSome (v, prop, baseType)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|DotInt|_|) (e : Expr) =
            match e with
            | Call(_, MethodQuote <@ Vec.dot : V4d -> V4d -> float @> _, [VectorExpr (a, d, Integral); VectorExpr (b, _, _)])
            | Call(_, Method("Dot", _), [VectorExpr (a, d, Integral); VectorExpr (b, _, _)]) ->
                ValueSome (a, b, vectorFields |> List.take d)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|LengthSquared|_|) (e : Expr) =
            match e with
            | Call(_, MethodQuote <@ Vec.lengthSquared : V4d -> float @> _, [VectorExpr (v, d, ft)])
            | Call(_, Method("LengthSquared", _), [VectorExpr (v, d, ft)])
            | PropertyGet(Some (VectorExpr (v, d, ft)), Property "LengthSquared", _) ->
                ValueSome v
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|DistanceSquared|_|) (e : Expr) =
            match e with
            | Call(_, MethodQuote <@ Vec.distanceSquared : V4d -> V4d -> float @> _, [VectorExpr (a, _, aft); VectorExpr (b, _, bft)])
            | Call(_, Method("DistanceSquared", _), [VectorExpr (a, _, aft); VectorExpr (b, _, bft)]) when aft = bft ->
                ValueSome (a, b)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|Distance1|_|) (e : Expr) =
            match e with
            | Call(_, Method("Distance1", _), [VectorExpr (a, d, aft); VectorExpr (b, _, bft)]) when aft = bft ->
                ValueSome (a, b, vectorFields |> List.take d)
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|DistanceMinMax|_|) (e : Expr) =
            match e with
            | Call(_, Method("DistanceMin", _), [VectorExpr (a, d, aft); VectorExpr (b, _, bft)]) when aft = bft ->
                ValueSome (a, b, "Min")
            | Call(_, Method("DistanceMax", _), [VectorExpr (a, d, aft); VectorExpr (b, _, bft)]) when aft = bft ->
                ValueSome (a, b, "Max")
            | _ ->
                ValueNone

        [<return: Struct>]
        let (|Norm|_|) (e : Expr) =
            match e with
            | Call(_, Method("Norm1", _), [VectorExpr (v, _, _)])
            | PropertyGet(Some (VectorExpr (v, _, _)), Property "Norm1", _) ->
                ValueSome (v, "1")

            | Call(_, Method("NormMin", _), [VectorExpr (v, _, _)])
            | PropertyGet(Some (VectorExpr (v, _, _)), Property "NormMin", _) ->
                ValueSome (v, "Min")

            | Call(_, Method("NormMax", _), [VectorExpr (v, _, _)])
            | PropertyGet(Some (VectorExpr (v, _, _)), Property "NormMax", _) ->
                ValueSome (v, "Max")

            | _ ->
                ValueNone

        [<return: Struct>]
        let (|MinMaxElement|_|) (e : Expr) =
            match e with
            | Call(_, Method("MinElement", _), [VectorExpr (v, d, ft)])
            | PropertyGet(Some (VectorExpr (v, d, ft)), Property "MinElement", _) ->
                ValueSome (v, "Min", ft, vectorFields |> List.take d)

            | Call(_, Method("MaxElement", _), [VectorExpr (v, d, ft)])
            | PropertyGet(Some (VectorExpr (v, d, ft)), Property "MaxElement", _) ->
                ValueSome (v, "Max", ft, vectorFields |> List.take d)

            | _ ->
                ValueNone

        [<return: Struct>]
        let (|InvLerp|_|) (e : Expr) =
            match e with
            | Call(_, MethodQuote <@ invLerp : float -> float -> float -> float @> _, [a; b; y]) ->
                ValueSome [a; b; y]

            | Call(_, (Method("InvLerp", _) as mi), [y; a; b]) when mi.DeclaringType = typeof<Fun> ->
                ValueSome [a; b; y]

            | _ ->
                ValueNone

        [<return: Struct>]
        let (|LinearStep|_|) (e : Expr) =
            match e with
            | Call(_, MethodQuote <@ linearstep : float -> float -> float -> float @> _, [edge0; edge1; x]) ->
                ValueSome [edge0; edge1; x]

            | Call(_, (Method("Linearstep", _) as mi), [x; edge0; edge1]) when mi.DeclaringType = typeof<Fun> ->
                ValueSome [edge0; edge1; x]

            | _ ->
                ValueNone

        [<return: Struct>]
        let (|TransformPosProj|_|) (e : Expr) =
            match e with
            | Call(_, MethodQuote <@ Mat.transformPosProj : M44d -> V3d -> V3d @> _, [m; VectorExpr(v, d, _)]) ->
                ValueSome (d, false, [m; v])

            | Call(_, (Method("TransformPosProj", _) as mi), [m; VectorExpr(v, d, _)]) when mi.DeclaringType = typeof<Mat> ->
                ValueSome (d, false, [m; v])

            | Call(_, (Method("TransposedTransformProj", _) as mi), [m; VectorExpr(v, d, _)])
            | Call(_, (Method("TransposedTransformPosProj", _) as mi), [m; VectorExpr(v, d, _)]) when mi.DeclaringType = typeof<Mat> ->
                ValueSome (d, true, [m; v])

            | _ ->
                ValueNone

        // returns (arguments, typ, dimension, isAllQuantifier, name)
        [<return: Struct>]
        let (|MatrixQuantifiedBooleanOperation|_|) (e : Expr) =
            match e with
            | Call(_, Method(name, _), ([MatrixExpr(mat, dim, _)] as args))
            | Call(_, Method(name, _), ([MatrixExpr(mat, dim, _); _] as args))
            | Call(_, Method(name, _), ([_; MatrixExpr(mat, dim, _)] as args))
            | PropertyGet (Some (MatrixExpr(mat, dim, _)), Property(name), args) ->
                let m = quantifiedBooleanOperations.Match name
                if m.Success then
                    let all = (m.Groups.[1].Value.ToLower() = "all")
                    let name = m.Groups.[2].Value

                    match name, all with
                    | "Equal", true | "Different", false -> ValueNone  // trivial equality
                    | _ ->
                        let args =
                            if args.Length = 0 then [mat]
                            else args

                        ValueSome (args, mat.Type, dim, all, name)
                else
                    ValueNone
            | _ ->
                ValueNone

        // returns (expr, isAllQuantifier, propertyFunctionName)
        [<return: Struct>]
        let private (|EmulatedSpecialFloatingPointCheck|_|) (e : Expr) =
            match e with
            | Call(_, Method(name, _), [value])
            | PropertyGet (Some (value), Property(name), _) ->
                let m = emulatedSpecialFloatingPointCheck.Match name
                if m.Success then
                    let isAllQuantifier = (m.Groups.[1].Value = "All")
                    let propertyName = m.Groups.[2].Value
                    ValueSome (value, isAllQuantifier, propertyName)
                else
                    ValueNone
            | _ ->
                ValueNone

        // returns (vectoExpr, dimension, isAllQuantifier, propertyFunctionName)
        [<return: Struct>]
        let (|VectorEmulatedSpecialFloatingPointCheck|_|) (e : Expr) =
            match e with
            | EmulatedSpecialFloatingPointCheck (VectorExpr(v, d, _), all, name) -> ValueSome (v, d, all, name)
            | _ -> ValueNone

        // returns (expr, isVector, name)
        [<return: Struct>]
        let private (|ScalarOrVectorAllEmulatedSpecialFloatingPointCheck|_|) (e : Expr) =
            match e with
            | EmulatedSpecialFloatingPointCheck (VectorExpr(e, _, _), true, name) -> ValueSome (e, true, name)
            | EmulatedSpecialFloatingPointCheck (FloatingPointExpr e, _, name) -> ValueSome (e, false, name)
            | _ -> ValueNone

        // returns (expr, isVector, isPositive)
        [<return: Struct>]
        let (|ScalarOrVectorAllSignedInfinity|_|) (e : Expr) =
            match e with
            | ScalarOrVectorAllEmulatedSpecialFloatingPointCheck (e, isVector, name) ->
                match name with
                | "PositiveInfinity" -> ValueSome (e, isVector, true)
                | "NegativeInfinity" -> ValueSome (e, isVector, false)
                | _ -> ValueNone
            | _ ->
                ValueNone

        // returns (expr, isVector)
        [<return: Struct>]
        let (|ScalarOrVectorAllFinite|_|) (e : Expr) =
            match e with
            | ScalarOrVectorAllEmulatedSpecialFloatingPointCheck (e, isVector, "Finite") -> ValueSome (e, isVector)
            | _ -> ValueNone

    // Used to determine which preprocess function to call
    [<RequireQualifiedAccess>]
    type ShaderExpressionType =
        | Normal
        | Compute
        | Raytracing of ShaderStage

    type State =
        {
            depthWriteMode  : DepthWriteMode
            inputTypes      : List<Type>
            inputTopology   : Option<InputTopology>
            vertexType      : Type
            builder         : Option<Expr>
            inputs          : Map<string, ParameterDescription>
            outputs         : Map<string, ParameterDescription>
            uniforms        : Map<string, UniformParameter>
            vertexIndex     : Map<Var, Expr>
            variableValues  : Map<Var, Expr>
            shaders         : list<Shader>
            payloads        : HashMap<Type, string * int>
            payloadIn       : Option<string * Type>
            callableData    : HashMap<Type, string * int>
            callableDataIn  : Option<string * Type>
            hitAttribute    : Option<string * Type>
            rayTypes        : Set<string>
            missShaders     : Set<string>
            callableShaders : Set<string>
            localSize       : V3i
            expressionType  : ShaderExpressionType
        }
        
    let shaderUtilityFunctions = System.Collections.Concurrent.ConcurrentDictionary<V3i * MethodBase, Option<Expr * State>>()

    type Preprocess<'a> = State<State, 'a>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        type private NoInput = { x : int }

        let empty =
            {
                depthWriteMode  = DepthWriteMode.None
                inputTypes      = []
                inputTopology   = None
                vertexType      = typeof<NoInput>
                builder         = None
                inputs          = Map.empty
                outputs         = Map.empty
                uniforms        = Map.empty
                vertexIndex     = Map.empty
                variableValues  = Map.empty
                shaders         = []
                payloads        = HashMap.empty
                payloadIn       = None
                callableData    = HashMap.empty
                callableDataIn  = None
                hitAttribute    = None
                rayTypes        = Set.empty
                missShaders     = Set.empty
                callableShaders = Set.empty
                localSize       = V3i.Zero
                expressionType  = ShaderExpressionType.Normal
            }

        let createInner (parent : State) =
            { empty with
                payloads        = parent.payloads
                payloadIn       = parent.payloadIn
                callableData    = parent.callableData
                callableDataIn  = parent.callableDataIn
                hitAttribute    = parent.hitAttribute
                expressionType  = parent.expressionType
                uniforms        = parent.uniforms }

        let mergeInner (inner : State) =
            State.modify (fun s ->
                { s with
                    payloads        = inner.payloads
                    payloadIn       = inner.payloadIn
                    callableData    = inner.callableData
                    callableDataIn  = inner.callableDataIn
                    hitAttribute    = inner.hitAttribute
                    uniforms        = inner.uniforms
                    rayTypes        = Set.union s.rayTypes inner.rayTypes
                    missShaders     = Set.union s.missShaders inner.missShaders
                    callableShaders = Set.union s.callableShaders inner.callableShaders }
            )

        let ofInputTypes (types : List<Type>) =

            let vertexInput =
                types |> List.tryPick (fun t ->
                    let iface = t.GetInterface(typedefof<Primitive<_>>.Name)

                    if isNull iface then
                        None
                    else
                        let top = t.GetProperty("InputTopology", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public).GetValue(null) |> unbox<InputTopology>
                        let v = iface.GetGenericArguments().[0]
                        Some (v, top)
                )

            let vertexType =
                vertexInput |> Option.map fst |> Option.defaultWith (fun _ ->
                    types |> List.tryHead |> Option.defaultValue typeof<NoInput>
                )

            let inputTopology =
                vertexInput |> Option.map snd

            { empty with
                inputTypes = types
                inputTopology = inputTopology
                vertexType = vertexType }


        let inputTypes = State.get |> State.map (fun s -> s.inputTypes)
        let vertexType = State.get |> State.map (fun s -> s.vertexType)
        let builder = State.get |> State.map (fun s -> s.builder)

        let readInput (name : string) (desc : ParameterDescription) =
            let implicitInterp =
                match desc.paramType with
                | Integral -> InterpolationMode.Flat
                | _ -> InterpolationMode.Default

            State.modify (fun s ->
                match Map.tryFind name s.inputs with
                | Some odesc ->
                    if odesc.paramType = desc.paramType then
                        let interp = odesc.paramInterpolation ||| desc.paramInterpolation ||| implicitInterp
                        { s with State.inputs = Map.add name { desc with paramInterpolation = interp } s.inputs }
                    else
                        failwithf "[FShade] input '%s' has conflicting types %A and %A." name odesc.paramType desc.paramType
                | None ->
                    let desc = { desc with paramInterpolation = desc.paramInterpolation ||| implicitInterp }
                    { s with State.inputs = Map.add name desc s.inputs }
            )

        let readUniform (validateCompatibility : bool) (p : UniformParameter) =
            State.modify (fun s ->
                match Map.tryFind p.uniformName s.uniforms with
                | Some _ when not validateCompatibility ->
                    s
                | Some { uniformType = otype; uniformValue = ovalue } ->
                    if p.uniformType <> otype then
                        failwithf "[FShade] uniform '%s' has conflicting types %A and %A." p.uniformName otype p.uniformType
                    elif p.uniformValue <> ovalue then
                        failwithf "[FShade] uniform '%s' has conflicting values %A and %A." p.uniformName ovalue p.uniformValue
                    else
                        s
                | None ->
                    { s with State.uniforms = Map.add p.uniformName p s.uniforms }
            )

        let writeOutput (name : string) (desc : ParameterDescription) = 
            State.modify (fun s ->
                { s with State.outputs = Map.add name desc s.outputs }
            )

        let setDepthWriteMode (mode : DepthWriteMode) =
            State.modify (fun s ->
                { s with depthWriteMode = mode }
            )

        let setBuilder (b : Expr) =
            State.modify (fun s ->
                { s with State.builder = Some b }
            )

        let setVertexIndex (v : Var) (index : Expr) =
            State.modify (fun s ->
                { s with vertexIndex = Map.add v index s.vertexIndex }
            )

        let setVariableValue (v : Var) (value : Expr) =
            State.modify (fun s ->
                { s with variableValues = Map.add v value s.variableValues }
            )

        let setExpressionType (t : ShaderExpressionType) =
            State.modify (fun s ->
                { s with expressionType = t }
            )

        let tryGetVertexIndex (v : Var) =
            State.get |> State.map (fun s -> Map.tryFind v s.vertexIndex)

        let readAccelerationStructure (uniform : UniformParameter) =
            state {
                do! uniform |> readUniform true
                return Expr.ReadInput(ParameterKind.Uniform, typeof<Scene>, uniform.uniformName)
            }

        let usePayload (payload : Type) =
            State.custom (fun (s : State) ->
                match s.payloads |> HashMap.tryFind payload with
                | Some (var, index) -> s, (var, index)
                | None ->
                    let index = s.payloads.Count
                    let name = sprintf "rayPayload%d" index
                    let s = { s with payloads = s.payloads |> HashMap.add payload (name, index) }
                    s, (name, index)
            )

        let usePayloadIn (payload : Type) =
            State.custom (fun (s : State) ->
                match s.payloadIn with
                | Some (_, p) when payload <> p -> failwithf "[FShade] Can only use one type of incoming payload (got %A and %A)" p payload
                | _ ->
                    let name = "rayPayloadIn"
                    { s with payloadIn = Some (name, payload) }, name
            )

        let useCallableData (data : Type) =
            State.custom (fun (s : State) ->
                match s.callableData |> HashMap.tryFind data with
                | Some (var, index) -> s, (var, index)
                | None ->
                    let index = s.callableData.Count
                    let name = sprintf "callableData%d" index
                    let s = { s with callableData = s.callableData |> HashMap.add data (name, index) }
                    s, (name, index)
            )

        let useCallableDataIn (data : Type) =
            State.custom (fun (s : State) ->
                match s.callableDataIn with
                | Some (_, d) when data <> d -> failwithf "[FShade] Can only use one type of incoming callable data (got %A and %A)" d data
                | _ ->
                    let name = "callableDataIn"
                    { s with callableDataIn = Some (name, data) }, name
            )

        let useHitAttribute (hitAttribute : Type) =
            State.custom (fun (s : State) ->
                match s.hitAttribute with
                | Some (_, h) when hitAttribute <> h -> failwithf "[FShade] Can only use one type of hit attribute (got %A and %A)" h hitAttribute
                | _ ->
                    let name = "rayHitAttribute"
                    { s with hitAttribute = Some (name, hitAttribute) }, name
            )

        [<return: Struct>]
        let private (|Name|_|) (value : obj) =
            match value with
            | :? string as name -> ValueSome name
            | :? Symbol as sym -> ValueSome (string sym)
            | _ -> ValueNone

        let useRayType (e : Expr) =
            State.custom (fun (s : State) ->
                match Expr.TryEval e with
                | Some (Name name) ->
                    { s with rayTypes = s.rayTypes |> Set.add name }, name
                | _ ->
                    failwithf "[FShade] Ray type name must be constant"
            )

        let useMissShader (e : Expr) =
            State.custom (fun (s : State) ->
                match Expr.TryEval e with
                | Some (Name name) ->
                    { s with missShaders = s.missShaders |> Set.add name }, name
                | _ ->
                    failwithf "[FShade] Miss shader name must be constant"
            )

        let useCallableShader (e : Expr) =
            State.custom (fun (s : State) ->
                match Expr.TryEval e with
                | (Some (Name name)) ->
                    { s with callableShaders = s.callableShaders |> Set.add name }, name
                | _ ->
                    failwithf "[FShade] Callable shader name must be constant"
            )

    module Stubs =
        [<KeepCall>]
        let private traceRay (scene : IAccelerationStructure)
                             (cullMask : int) (flags : RayFlags)
                             (rayId : string) (missId : string)
                             (origin : V3d) (minT : float)
                             (direction : V3d) (maxT : float)
                             (payload : int) : unit =
            onlyInShaderCode "traceRayStub"

        let traceRayMeth = getMethodInfo <@ traceRay @>


        [<KeepCall>]
        let private executeCallable (id : string) (callable : int) : unit =
            onlyInShaderCode "executeCallableStub"

        let executeCallableMeth = getMethodInfo <@ executeCallable @>

    module private MethodInfo =

        let op_addition    = getMethodInfo <@ (+) : float -> float -> float @>
        let op_subtraction = getMethodInfo <@ (-) : float -> float -> float @>
        let op_multiply    = getMethodInfo <@ (*) : float -> float -> float @>
        let op_division    = getMethodInfo <@ (/) : float -> float -> float @>
        let op_logicalAnd  = getMethodInfo <@ (&&) : bool -> bool -> bool @>
        let op_logicalOr   = getMethodInfo <@ (||) : bool -> bool -> bool @>
        let op_lessThan    = getMethodInfo <@ (<) : float -> float -> bool @>
        let op_greaterThan = getMethodInfo <@ (>) : float -> float -> bool @>

        let not = getMethodInfo <@ not : bool -> bool @>
        let defaultOf = getMethodInfo <@ Unchecked.defaultof<int> @>
        let reportIntersection = getMethodInfo <@ reportIntersection @>

    [<AutoOpen>]
    module private ExprExtensions =
        open TypeInfo.Patterns

        let private toFloat = getMethodInfo <@ float : int -> float @>

        let private floatVecType = [|
                typeof<V2d>
                typeof<V3d>
                typeof<V4d>
            |]

        [<return: Struct>]
        let private (|TensorOp|_|) (a : Expr, b : Expr) =
            match a.Type, b.Type with
            | (VectorOf(_, _) | MatrixOf(_, _)), _ -> ValueSome a.Type
            | _, (VectorOf(_, _) | MatrixOf(_, _)) -> ValueSome b.Type
            | _ -> ValueNone

        type Expr with
            static member Add(a : Expr, b : Expr) =
                match a, b with
                | TensorOp t ->
                    let mi = t.GetMethod("op_Addition", [| a.Type; b.Type |])
                    Expr.Call(mi, [a; b])
                | _ ->
                    let mi = MethodInfo.op_addition.MakeGenericMethod(a.Type, b.Type, b.Type)
                    Expr.Call(mi, [a; b])

            static member Subtract(a : Expr, b : Expr) =
                match a, b with
                | TensorOp t ->
                    let mi = t.GetMethod("op_Subtraction", [| a.Type; b.Type |])
                    Expr.Call(mi, [a; b])
                | _ ->
                    let mi = MethodInfo.op_subtraction.MakeGenericMethod(a.Type, b.Type, b.Type)
                    Expr.Call(mi, [a; b])

            static member Multiply(a : Expr, b : Expr) =
                match a, b with
                | TensorOp t ->
                    let mi = t.GetMethod("op_Multiply", [| a.Type; b.Type |])
                    Expr.Call(mi, [a; b])
                | _ ->
                    let mi = MethodInfo.op_multiply.MakeGenericMethod(a.Type, b.Type, b.Type)
                    Expr.Call(mi, [a; b])

            static member Division(a : Expr, b : Expr) =
                match a, b with
                | TensorOp t ->
                    let mi = t.GetMethod("op_Division", [| a.Type; b.Type |])
                    Expr.Call(mi, [a; b])
                | _ ->
                    let mi = MethodInfo.op_division.MakeGenericMethod(a.Type, b.Type, b.Type)
                    Expr.Call(mi, [a; b])

            static member ToFloat(expr : Expr) =
                match expr.Type with
                | FloatingPoint -> expr

                | VectorOf(dim, _) ->
                    let vdt = floatVecType.[dim - 2]
                    let ctor = vdt.GetConstructor [| expr.Type |]
                    Expr.NewObject(ctor, [expr])

                | _ ->
                    let tf = toFloat.MakeGenericMethod expr.Type
                    Expr.Call(tf, [expr])

            static member And(a : Expr, b : Expr) =
                Expr.Call(MethodInfo.op_logicalAnd, [a; b])

            static member Or(a : Expr, b : Expr) =
                Expr.Call(MethodInfo.op_logicalOr, [a; b])

            static member LessThan(a : Expr, b : Expr) =
                let mi = MethodInfo.op_lessThan.MakeGenericMethod(a.Type)
                Expr.Call(mi, [a; b])

            static member GreaterThan(a : Expr, b : Expr) =
                let mi = MethodInfo.op_greaterThan.MakeGenericMethod(a.Type)
                Expr.Call(mi, [a; b])

            static member Not(e : Expr) =
                Expr.Call(MethodInfo.not, [e])

            static member Zero(t : Type) =
                match t with
                | TypeInfo.Patterns.Int32 -> Expr.Value 0
                | TypeInfo.Patterns.UInt32 -> Expr.Value 0u
                | TypeInfo.Patterns.Float32 -> Expr.Value 0.0f
                | TypeInfo.Patterns.Float64 -> Expr.Value 0.0
                | TypeInfo.Patterns.Vector
                | TypeInfo.Patterns.Matrix ->
                    let pi = t.GetProperty("Zero", [||])
                    Expr.PropertyGet pi

                | _ ->
                    failwithf "[FShade] Expr.Zero not implemented for type %s" t.FullName

    let rec preprocessRaytracingS (stage : ShaderStage) (e : Expr) : Preprocess<Expr> =
        state {
            match e with
            | TraceRay _ when not (ShaderStage.supportsTraceRay stage) ->
                return failwithf "[FShade] Cannot invoke TraceRay in %A shaders" stage

            | TraceRay args ->
                let! origin = preprocessRaytracingS stage args.origin
                let! direction = preprocessRaytracingS stage args.direction
                let! minT = preprocessRaytracingS stage args.minT
                let! maxT = preprocessRaytracingS stage args.maxT
                let! flags = preprocessRaytracingS stage args.flags
                let! cullMask = preprocessRaytracingS stage args.cullMask

                let! payloadIn =
                    match args.payload with
                    | ValueSome p ->
                        preprocessRaytracingS stage p |> State.map Some
                    | ValueNone ->
                        State.value None

                let! payloadName, payloadIndex = State.usePayload e.Type
                let! ray = State.useRayType args.ray
                let! miss = State.useMissShader args.miss
                let! accel = State.readAccelerationStructure args.accelerationStructure

                let rayId = Expr.Value(ray)
                let missId = Expr.Value(miss)
                let payloadIndex = Expr.Value(payloadIndex)

                return Expr.Seq [
                    match payloadIn with
                    | Some p -> Expr.WriteRaytracingData(payloadName, p)
                    | _ -> ()

                    Expr.Call(
                        Stubs.traceRayMeth,
                        [accel; cullMask;
                        flags; rayId; missId;
                        origin; minT;
                        direction; maxT;
                        payloadIndex]
                    )

                    Expr.ReadRaytracingData(e.Type, payloadName)
                ]

            | RayPayloadIn _ when not (ShaderStage.supportsPayloadIn stage) ->
                return failwithf "[FShade] Cannot use incoming payloads in %A shaders" stage

            | RayPayloadIn payload ->
                let! name = State.usePayloadIn payload
                return Expr.ReadRaytracingData(payload, name)

            | ExecuteCallable(id, data) ->
                let! id = State.useCallableShader id

                let! callableDataIn =
                    match data with
                    | ValueSome d ->
                        preprocessRaytracingS stage d |> State.map Some
                    | ValueNone ->
                        State.value None

                let! callableDataName, callableDataIndex = State.useCallableData e.Type
                let callableDataIndex = Expr.Value(callableDataIndex)

                return Expr.Seq [
                    match callableDataIn with
                    | Some d -> Expr.WriteRaytracingData(callableDataName, d)
                    | _ -> ()

                    Expr.Call(Stubs.executeCallableMeth, [Expr.Value id; callableDataIndex])

                    Expr.ReadRaytracingData(e.Type, callableDataName)
                ]

            | CallableDataIn _ when stage <> ShaderStage.Callable ->
                return failwith "[FShade] Incmoing callable data can only be used in callable shaders"

            | CallableDataIn data ->
                let! name = State.useCallableDataIn data
                return Expr.ReadRaytracingData(data, name)

            | ReportIntersection _ when stage <> ShaderStage.Intersection ->
                return failwith "[FShade] Report.Intersection can only be invoked in intersection shaders"

            | ReportIntersection (t, kind, attribute) ->
                let! t = preprocessRaytracingS stage t
                let! kind = preprocessRaytracingS stage kind

                let! attribute =
                    match attribute with
                    | ValueSome attr ->
                        preprocessRaytracingS stage attr
                        |> State.bind (fun value ->
                            State.useHitAttribute value.Type |> State.map (fun name -> Some (name, value))
                        )
                    | ValueNone ->
                        State.value None

                return Expr.Seq [
                    match attribute with
                    | Some (name, value) ->
                        Expr.WriteRaytracingData(name, value)

                    | _ -> ()

                    Expr.Call(MethodInfo.reportIntersection, [t; kind])
                ]

            | HitAttribute _ when not (ShaderStage.supportsHitAttributes stage) ->
                return failwithf "[FShade] Cannot use hit attributes in %A shaders" stage

            | HitAttribute attr ->
                let! name = State.useHitAttribute attr
                return Expr.ReadRaytracingData(attr, name)

            | BuilderReturn(_, _, value) when ShaderStage.supportsPayloadIn stage ->
                let! name = State.usePayloadIn value.Type
                let! value = preprocessRaytracingS stage value
                return Expr.WriteRaytracingData(name, value)

            | BuilderReturn(_, _, value) when stage = ShaderStage.Callable ->
                let! name = State.useCallableDataIn value.Type
                let! value = preprocessRaytracingS stage value
                return Expr.WriteRaytracingData(name, value)

            | SemanticInput(semantic, parameter) ->
                do! State.readInput semantic parameter
                return Expr.ReadInput(ParameterKind.Input, e.Type, semantic)

            | RemoveBuilder e ->
                return! preprocessRaytracingS stage e

            | ShapeLambda(v, b) ->
                let! b = preprocessRaytracingS stage b
                return Expr.Lambda(v, b)

            | ShapeVar(v) ->
                return e

            | ShapeCombination(o, args) ->
                let! args = args |> List.mapS (preprocessRaytracingS stage)
                return RebuildShapeCombination(o, args)
        }

    let rec preprocessComputeS (e : Expr) : Preprocess<Expr> =
        state {
            match e with
                | GetArray(ValueWithName(v, t, name), i) ->
                    let! i = preprocessComputeS i
                    match t with
                        | ArrOf(_,t) | ArrayOf t ->
                            return Expr.ReadInput(ParameterKind.Input, t, name, i)
                        | _ ->
                            return e

                | SetArray(ValueWithName(v, t, name), i, e) ->
                    let! i = preprocessComputeS i
                    let! e = preprocessComputeS e
                    return Expr.WriteOutputsRaw([name, Some i, e])

                | PropertyGet(Some (ValueWithName(v, t, name)), prop, []) when t.IsArray && (prop.Name = "Length" || prop.Name = "LongLength") ->
                    return Expr.ReadInput(ParameterKind.Uniform, typeof<int>, "cs_" + name + "_length")

                | ValueWithName(v,t,name) ->
                    return Expr.ReadInput(ParameterKind.Uniform, t, "cs_" + name)

                | PropertyGet(None, pi, []) when pi.Name = "LocalSize" ->
                    let! s = State.get
                    return Expr.Value(s.localSize)

                | FieldGet(None, pi) when pi.Name = "LocalSize" ->
                    let! s = State.get
                    return Expr.Value(s.localSize)

                | Call(None, mi, [size]) when mi.Name = "allocateShared" ->
                    return failwith "[FShade] non-static call to allocateShared"

                | Let(v, Call(None, mi, [size]),b) when mi.Name = "allocateShared" ->
                    let! size = preprocessComputeS size
                    match Expr.TryEval size with
                        | Some (:? int as size) ->
                            let et = mi.ReturnType.GetElementType()
                            let t = Peano.getArrayType size et


                            let name = sprintf "%s_%sx%d" v.Name et.PrettyName size

                            let rep = Expr.ReadInput(ParameterKind.Uniform, t, name)
                            let rec substitute (e : Expr) =
                                match e with

                                    | GetArray(Var vv, index) when vv = v ->
                                        let index = substitute index
                                        Peano.getItem rep index

                                    | SetArray(Var vv, index, value) when vv = v ->
                                        let index = substitute index
                                        let value = substitute value
                                        Peano.setItem rep index value



                                    | ShapeCombination(o, args) -> RebuildShapeCombination(o, List.map substitute args)
                                    | ShapeLambda(v,b) -> Expr.Lambda(v, substitute b)
                                    | ShapeVar vv -> 
                                        if vv = v then failwith "[FShade] cannot use shared memory as value"
                                        Expr.Var vv


                        //    {
                        //        uniformName         : string
                        //        uniformType         : Type
                        //        uniformValue        : UniformValue
                        //    }


                            let b = substitute b
                            let u = { uniformName = name; uniformType = t; uniformValue = UniformValue.Attribute(uniform?SharedMemory, name) }
                            do! u |> State.readUniform true

                            return! preprocessComputeS b

                        | _ ->
                            return failwith "[FShade] could not evaluate size for allocateShared"



                | ShapeLambda(v, b) ->
                    let! b = preprocessComputeS b
                    return Expr.Lambda(v, b)

                | ShapeVar(v) ->
                    return e

                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapS preprocessComputeS
                    return RebuildShapeCombination(o, args)

        }

    let rec preprocessNormalS (e : Expr) : Preprocess<Expr> =
        state {
            let! vertexType = State.vertexType

            match e with
            | ConstantSwizzle(v, prop, baseType) ->
                let tmp = Var("tmp", v.Type)
                let components =
                    prop.Name |> Seq.toArray |> Array.map (function
                        | 'X' -> Expr.FieldGet(Expr.Var tmp, v.Type.GetField "X")
                        | 'Y' -> Expr.FieldGet(Expr.Var tmp, v.Type.GetField "Y")
                        | 'Z' -> Expr.FieldGet(Expr.Var tmp, v.Type.GetField "Z")
                        | 'W' -> Expr.FieldGet(Expr.Var tmp, v.Type.GetField "W")
                        | 'I' | 'P' ->
                            if baseType = typeof<int> then Expr.Value 1
                            elif baseType = typeof<uint32> then Expr.Value 1u
                            elif baseType = typeof<float32> then Expr.Value 1.0f
                            elif baseType = typeof<double> then Expr.Value 1.0
                            else failwith "not implemented"
                        | 'O' ->
                            if baseType = typeof<int> then Expr.Value 0
                            elif baseType = typeof<uint32> then Expr.Value 0u
                            elif baseType = typeof<float32> then Expr.Value 0.0f
                            elif baseType = typeof<double> then Expr.Value 0.0
                            else failwith "not implemented"
                        | 'N' ->
                            if baseType = typeof<int> then Expr.Value -1
                            elif baseType = typeof<float32> then Expr.Value -1.0f
                            elif baseType = typeof<double> then Expr.Value -1.0
                            else failwith "not implemented"
                        | c ->
                            failwithf "bad swizzle: %A" c
                    )

                let ctor = e.Type.GetConstructor(components |> Array.map (fun c -> c.Type))

                let! v = preprocessNormalS v
                return
                    Expr.Let(tmp, v,
                        Expr.NewObject(
                            ctor, Array.toList components
                        )
                    )

            // GLSL dot always returns floating point...
            | DotInt (a, b, fields) ->
                let ta = Var("ta", a.Type)
                let tb = Var("tb", b.Type)

                let expr =
                    fields |> List.map (fun f ->
                        Expr.Multiply(
                            Expr.FieldGet(Expr.Var ta, a.Type.GetField f),
                            Expr.FieldGet(Expr.Var tb, b.Type.GetField f)
                        )
                    )
                    |> List.reduce (fun x y -> Expr.Add(x, y))

                let! a = preprocessNormalS a
                let! b = preprocessNormalS b
                return Expr.Let(ta, a, Expr.Let(tb, b, expr))

            | LengthSquared v ->
                let dot = typeof<Vec>.GetMethod("Dot", [| v.Type; v.Type |])

                let tmp = Var("tmp", v.Type)
                let! v = preprocessNormalS v
                return Expr.Let(tmp, v,
                    Expr.Call(dot, [Expr.Var tmp; Expr.Var tmp])
                )

            | DistanceSquared (a, b) ->
                let dot = typeof<Vec>.GetMethod("Dot", [| a.Type; a.Type |])

                let tmp = Var("tmp", a.Type)
                let expr =
                    Expr.Let(tmp,
                        Expr.Subtract(a, b),
                        Expr.Call(dot, [Expr.Var tmp; Expr.Var tmp])
                    )

                return! preprocessNormalS expr

            | Distance1 (a, b, fields) ->
                let abs = typeof<Fun>.GetMethod("Abs", [| a.Type |])

                let tmp = Var("tmp", a.Type)

                let sumExpr =
                    fields |> List.map (fun f ->
                        Expr.FieldGet(Expr.Var tmp, a.Type.GetField f)
                    )
                    |> List.reduce (fun x y -> Expr.Add(x, y))

                let! a = preprocessNormalS a
                let! b = preprocessNormalS b
                return Expr.Let(tmp, Expr.Call(abs, [Expr.Subtract(a, b)]), sumExpr)

            | DistanceMinMax (a, b, name) ->
                let abs = typeof<Fun>.GetMethod("Abs", [| a.Type |])
                let minMax = typeof<Vec>.GetMethod(name + "Element", [| a.Type |])

                let expr =
                    Expr.Call(minMax, [
                        Expr.Call(abs, [Expr.Subtract(a, b)])
                    ])

                return! preprocessNormalS expr

            | Norm (v, name) ->
                let expr =
                    if name = "Min" || name = "Max" then
                        let abs = typeof<Fun>.GetMethod("Abs", [| v.Type |])
                        let minMax = typeof<Vec>.GetMethod(name + "Element", [| v.Type |])
                        Expr.Call(minMax, [Expr.Call(abs, [v])])
                    else
                        let dist = typeof<Vec>.GetMethod("Distance" + name, [| v.Type; v.Type |])
                        Expr.Call(dist, [v; Expr.Zero v.Type])

                return! preprocessNormalS expr

            | MinMaxElement(v, name, fieldType, fields) ->
                let mi = typeof<Fun>.GetMethod(name, [| fieldType; fieldType |])

                let tmp = Var("tmp", v.Type)

                let minMaxExpr =
                    fields
                    |> List.map (fun f ->
                        Expr.FieldGet(Expr.Var tmp, v.Type.GetField f)
                    )
                    |> List.reduce (fun a b -> Expr.Call(mi, [a; b]))

                let! v = preprocessNormalS v
                return Expr.Let(tmp, v, minMaxExpr)

            | InvLerp [a; b; y] ->
                // Convert to floating point if input is integral
                // E.g invLerp : int32 -> int32 -> int32 -> float
                let! a = preprocessNormalS (Expr.ToFloat a)
                let! b = preprocessNormalS (Expr.ToFloat b)
                let! y = preprocessNormalS (Expr.ToFloat y)

                let ta = Var("tmp", y.Type)

                return
                    Expr.Let(ta, a,
                        Expr.Division(
                            Expr.Subtract(Expr.Var ta, y),
                            Expr.Subtract(Expr.Var ta, b)
                        )
                    )

            | LinearStep [edge0; edge1; x] ->
                let invLerp = typeof<Fun>.GetMethod("InvLerp", [| x.Type; edge0.Type; edge1.Type |])
                let saturate = typeof<Fun>.GetMethod("Saturate", [| x.Type |])

                let expr =
                    Expr.Call(saturate, [
                        Expr.Call(invLerp, [x; edge0; edge1])
                    ])

                return! preprocessNormalS expr

            | TransformPosProj (dim, transposed, [m; v]) ->
                let! m = preprocessNormalS m
                let! v = preprocessNormalS v

                let transform =
                    if transposed then
                        typeof<Mat>.GetMethod("TransposedTransformPosProjFull", [| m.Type; v.Type |])
                    else
                        typeof<Mat>.GetMethod("TransformPosProjFull", [| m.Type; v.Type |])

                let tmp = Var("tmp", transform.ReturnType)

                let first, last =
                    if dim = 2 then tmp.Type.GetProperty("XY"), tmp.Type.GetField("Z")
                    elif dim = 3 then tmp.Type.GetProperty("XYZ"), tmp.Type.GetField("W")
                    else failwithf "[FShade] Encountered transform of %d-dimensional vector" dim

                // Multiply and then divide by last component
                return Expr.Let(
                    tmp, Expr.Call(transform, [m; v]),
                    Expr.Division(Expr.PropertyGet(Expr.Var tmp, first), Expr.FieldGet(Expr.Var tmp, last))
                )

            | MatrixQuantifiedBooleanOperation (args, typ, dim, all, name) ->
                // Column access is easier in GLSL but we usually have reverse matrix logic
                let getRow =
                    typeof<Mat>.GetMethod("Row", [| typ; typeof<int> |])

                let operation =
                    let parameterTypes =
                        args |> List.map (fun a ->
                            if a.Type = typ then getRow.ReturnType  // Matrix row
                            else a.Type                             // Scalar
                        )

                    typeof<Vec>.GetMethod(
                        (if all then "All" else "Any") + name,
                        List.toArray parameterTypes
                    )

                let tmps =
                    args |> List.mapi (fun i a ->
                        Var($"tmp{i}", a.Type)
                    )

                let body =
                    List.init dim.Y (fun row ->
                        let parameters =
                            args |> List.mapi (fun i a ->
                                if a.Type = typ then
                                    Expr.Call(getRow, [Expr.Var tmps.[i]; Expr.Value row])
                                else
                                    a
                            )

                        Expr.Call(operation, parameters)
                    )
                    |> List.reduce (fun l r ->
                        if all then
                            Expr.And(l, r)
                        else
                            Expr.Or(l, r)
                    )

                let expr =
                    let bindings =
                        (tmps, args) ||> List.zip

                    (bindings, body) ||> List.foldBack (fun (t, a) s ->
                        Expr.Let(t, a, s)
                    )

                return! preprocessNormalS expr

            // Non-native floating point checks for vectors with ANY quantifier have
            // to be transformed into ORed checks for each component.
            | VectorEmulatedSpecialFloatingPointCheck (v, d, false, name) ->
                let getElement =
                    v.Type.GetProperty("Item", [| typeof<int> |])

                let isSpecialFloatingPoint =
                    typeof<Fun>.GetMethod("Is" + name, [| getElement.Type |])

                let tmp = Var("tmp", v.Type)

                let expr =
                    List.init d (fun i ->
                        Expr.Call(
                            isSpecialFloatingPoint,
                            [ Expr.PropertyGet(Expr.Var tmp, getElement, [Expr.Value i]) ]
                        )
                    )
                    |> List.reduce (fun l r -> Expr.Or(l, r))

                return! preprocessNormalS <| Expr.Let(
                    tmp, v,
                    expr
                )

            | ScalarOrVectorAllSignedInfinity (e, isVector, isPositive) ->
                let! e = preprocessNormalS e

                let isInfinity =
                    if isVector then
                        typeof<Vec>.GetMethod("AllInfinity", [| e.Type |])
                    else
                        typeof<Fun>.GetMethod("IsInfinity", [| e.Type |])

                let compare (a : Expr) (b : Expr) =
                    if isVector then
                        if isPositive then
                            let mi = typeof<Vec>.GetMethod("AllGreater", [| a.Type; b.Type |] )
                            Expr.Call(mi, [a; b])
                        else
                            let mi = typeof<Vec>.GetMethod("AllSmaller", [| a.Type; b.Type |] )
                            Expr.Call(mi, [a; b])
                    else
                        if isPositive then
                            Expr.GreaterThan(a, b)
                        else
                            Expr.LessThan(a, b)

                let tmp = Var("tmp", e.Type)

                return Expr.Let(
                    tmp, e,
                    Expr.And(
                        Expr.Call(isInfinity, [Expr.Var tmp]),
                        compare (Expr.Var tmp) (Expr.Zero(tmp.Type))
                    )
                )

            | ScalarOrVectorAllFinite (e, isVector) ->
                let! e = preprocessNormalS e

                let isInfinity, isNaN =
                    if isVector then
                        typeof<Vec>.GetMethod("AnyInfinity", [| e.Type |]),
                        typeof<Vec>.GetMethod("AnyNaN", [| e.Type |])
                    else
                        typeof<Fun>.GetMethod("IsInfinity", [| e.Type |]),
                        typeof<Fun>.GetMethod("IsNaN", [| e.Type |])

                let tmp = Var("tmp", e.Type)

                return Expr.Let(
                    tmp, e,
                    Expr.Not(
                         Expr.Or(
                             Expr.Call(isInfinity, [Expr.Var tmp]),
                             Expr.Call(isNaN, [Expr.Var tmp])
                         )
                     )
                )

            | Call(None, mi, [ExprValue v]) when mi.Name = "op_Splice" || mi.Name = "op_SpliceUntyped" ->
                if v.Type = e.Type then
                    return! preprocessNormalS v
                else
                    return! preprocessNormalS (Expr.Coerce(v, e.Type))

            | Pipe(e) ->
                return! preprocessNormalS e

            | LetCopyOfStruct(e) ->
                return! preprocessNormalS e

            | ReadInputOrRaytracingData(kind, name, idx, slot) ->
                let! idx = idx |> Option.mapS preprocessNormalS

                let paramType =
                    match idx with
                        | Some _ -> e.Type.MakeArrayType()
                        | _ -> e.Type

                match kind with
                | ParameterKind.Input -> 
                    do! State.readInput name { paramType = paramType; paramInterpolation = InterpolationMode.Default }
                | ParameterKind.Uniform ->
                    let u = { uniformType = paramType; uniformName = name; uniformValue = UniformValue.Attribute(uniform, name) }
                    do! u |> State.readUniform false // optimizer uses this to see which uniforms are used, scope is potentially wrong here so don't validate
                | _ ->
                    ()

                match idx with
                    | Some idx -> return Expr.ReadInput(kind, e.Type, name, idx, slot)
                    | None -> return Expr.ReadInput(kind, e.Type, name, slot)

            | WriteOutputs(map) ->
                let! map = 
                    map |> Map.mapS (fun name (idx, value) ->
                        state {
                            let! idx = idx |> Option.mapS preprocessNormalS
                            let! value = preprocessNormalS value
                            return idx, value
                        }
                    )
                for (name, (idx, value)) in Map.toSeq map do
                    do! State.modify (fun s ->
                            if Map.containsKey name s.outputs then 
                                s
                            else 
                                let typ =
                                    match idx with
                                        | None -> value.Type
                                        | Some _ -> value.Type.MakeArrayType()
                                { s with State.outputs = Map.add name { paramType = typ; paramInterpolation = InterpolationMode.Default } s.outputs }
                        ) 


                return Expr.WriteOutputs(map)

            | BuilderRun(b, body)
            | BuilderDelay(b, body) ->
                do! State.setBuilder b
                return! preprocessNormalS body

            | BuilderCombine(b, l, r) ->
                do! State.setBuilder b
                let! l = preprocessNormalS l
                let! r = preprocessNormalS r
                return Expr.Seq [l;r]

            | BuilderZero(b) ->
                do! State.setBuilder b
                return Expr.Unit


            | BuilderBind(b, var, TessellateCall(dim, inner, outer), tev) ->
                do! State.setBuilder b

                let coord = 
                    let c = Expr.ReadInput(ParameterKind.Input, typeof<V3d>, Intrinsics.TessCoord)
                    if var.Type = c.Type then c
                    else <@@ (%%c : V3d).XY @@>

                let tev = Expr.Let(var, coord, tev)

                let! s = State.get
                let! bindings, free = 
                    tev.GetFreeVars() 
                        |> Seq.toList 
                        |> List.choose2S (fun v ->
                            state {
                                match Map.tryFind v s.variableValues with
                                    | Some (ReadInput(ParameterKind.Input, name, Some TrivialInput) as e) ->
                                        return Choice1Of2 (v, e)
                                    | _ -> 
                                        do! State.writeOutput v.Name { paramType = v.Type; paramInterpolation = InterpolationMode.Default }
                                        return Choice2Of2(v, Expr.ReadInput(ParameterKind.Input, v.Type, v.Name))
                            }
                        )

                let rec wrap (bindings : list<Var * Expr>) (b : Expr) =
                    match bindings with
                        | [] -> b
                        | (h, he) :: rest ->
                            Expr.Let(h, he, wrap rest b)
                        
                let tev = wrap (bindings @ free) tev |> toShaders s.inputTypes s.vertexIndex
                match tev with
                    | [tev] ->
                        do! State.modify (fun s -> { s with shaders = [ { tev with shaderStage = ShaderStage.TessEval } ] })
                    | _ ->
                        failwithf "[FShade] invalid shader(s) after tessellate-call: %A" tev

                do! State.writeOutput Intrinsics.TessLevelInner { paramType = typeof<float[]>; paramInterpolation = InterpolationMode.Default }
                do! State.writeOutput Intrinsics.TessLevelOuter { paramType = typeof<float[]>; paramInterpolation = InterpolationMode.Default }

                return 
                    Expr.WriteOutputs [
                        yield Intrinsics.TessLevelInner, None, Expr.NewArray(typeof<float>, inner)
                        yield Intrinsics.TessLevelOuter, None, Expr.NewArray(typeof<float>, outer)
                        yield! free |> List.map (fun (v,_) -> v.Name, None, Expr.Var v)
                    ]


            | BuilderUsing(b, var, value, body)
            | BuilderBind(b, var, value, body) ->
                do! State.setBuilder b
                let! value = preprocessNormalS value
                let! body = preprocessNormalS body
                if var.Type <> typeof<unit> then
                    return Expr.Let(var, value, body)
                else
                    match value with
                        | Unit -> return body
                        | _ -> return Expr.Sequential(value, body)

            | BuilderFor(b, var, RangeSequence(first, step, last), body) ->
                do! State.setBuilder b
                let! first = preprocessNormalS first
                let! step = preprocessNormalS step
                let! last = preprocessNormalS last
                let! body = preprocessNormalS body
                return Expr.ForInteger(var, first, step, last, body)
                    
            | BuilderFor(b, var, Coerce(Primitive(primitive, vertexCount), _), body) ->
                do! State.setBuilder b
                let iVar = Var(var.Name + "Index", typeof<int>)
                let prop = primitive.Type.GetProperty("Item", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                let replacement = Expr.PropertyGet(primitive, prop, [Expr.Var iVar])
                    
                do! State.setVertexIndex var (Expr.Var iVar)
                let! body = preprocessNormalS body

                return Expr.ForIntegerRangeLoop(iVar, Expr.Value 0, Expr.Value (vertexCount - 1), body)

            | BuilderFor(b, var, sequence, body) ->
                do! State.setBuilder b
                let! sequence = preprocessNormalS sequence
                let! body = preprocessNormalS body
                return Expr.ForEach(var, sequence, body)

            | BuilderWhile(b, Lambda(unitVar, guard), body) ->
                do! State.setBuilder b
                let! guard = preprocessNormalS guard
                let! body = preprocessNormalS body
                return Expr.WhileLoop(guard, body)
                    

            | Uniform u ->
                do! u |> State.readUniform true
                return Expr.ReadInput(ParameterKind.Uniform, e.Type, u.uniformName)

            | Let(var, Call(None, mi, []), b) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = MethodInfo.defaultOf ->
                let! b = preprocessNormalS b
                return Expr.Let(var, Expr.DefaultValue(mi.ReturnType), b)

            // let p0 = tri.P0 in <body>
            // store (p0 -> 0) and preprocess <body>
            | Let(var, PrimitiveVertexGet(p, index), body) when var.Type = vertexType ->
                let! index = preprocessNormalS index
                do! State.setVertexIndex var index
                return! preprocessNormalS body


            | Let(var, e, body) ->
                let! e = preprocessNormalS e
                do! State.setVariableValue var e
                let! body = preprocessNormalS body
                return Expr.Let(var, e, body)

            // tri.P0.pos -> ReadInput(pos, 0)
            | InputRead vertexType (PrimitiveVertexGet(p, index), semantic, parameter) ->
//                    if semantic = Intrinsics.SourceVertexIndex then
//                        let! index = preprocessNormalS index
//                        return index
//                    else
                let! index = preprocessNormalS index
                do! State.readInput semantic { parameter with paramType = parameter.paramType.MakeArrayType() }
                return Expr.ReadInput(ParameterKind.Input, e.Type, semantic, index)

            // real vertex-read needed
            | PrimitiveVertexGet(p, index) ->
                let! index = preprocessNormalS index
                let fields = FSharpType.GetRecordFields(e.Type, true) |> Array.toList

                let! args =
                    fields |> List.mapS (fun f ->
                        state {
                            let interpolation = f.Interpolation
                            let semantic = f.Semantic 
//                                if semantic = Intrinsics.SourceVertexIndex then
//                                    return index
//                                else
                            let parameter = { paramType = f.PropertyType; paramInterpolation = interpolation }
                            do! State.readInput semantic { parameter with paramType = parameter.paramType.MakeArrayType() }
                            return Expr.ReadInput(ParameterKind.Input, f.PropertyType, semantic, index)
                        }
                    )
                        
                return Expr.NewRecord(e.Type, args)


            // vertex.pos -> ReadInput(pos)
            | InputRead vertexType (vertex, semantic, parameter) ->
                let! index =
                    match vertex with
                        | Var v -> State.tryGetVertexIndex v
                        | Value _ -> State.value None
                            
                        | _ -> failwithf "[FShade] found non-primitive vertex-expression: %A" vertex

                match index with
                    | Some index -> 
//                            if semantic = Intrinsics.SourceVertexIndex then
//                                return index
//                            else
                        do! State.readInput semantic { parameter with paramType = parameter.paramType.MakeArrayType() }
                        return Expr.ReadInput(ParameterKind.Input, parameter.paramType, semantic, index)
                    | _ ->
                        do! State.readInput semantic parameter
                        return Expr.ReadInput(ParameterKind.Input, parameter.paramType, semantic)
                    
                    
            | BuilderYield(b, mi, Let(var, value, body)) | BuilderReturn(b, mi, Let(var, value, body)) ->
                let mutable used = 0
                let newBody =
                    body.Substitute (fun vi -> 
                        if vi = var then 
                            used <- used + 1
                            Some value 
                        else 
                            None
                    )

                let real = 
                    if used <= 1 then Expr.Call(b, mi, [newBody])
                    else Expr.Let(var, value, Expr.Call(b, mi, [body]))

                return! preprocessNormalS real

                    
            | BuilderYield(b, _, value) ->
                do! State.setBuilder b
                let! value = preprocessNormalS value
                let defaultSem = 
                    if b.Type = typeof<FragmentBuilder> then Intrinsics.Color
                    else Intrinsics.Position

                let! values = getOutputValues defaultSem value

                return 
                    Expr.Sequential(
                        Expr.WriteOutputs values,
                        <@ emitVertex() @>
                    )

            | BuilderReturn(b, _, value) ->
                do! State.setBuilder b
                let! value = preprocessNormalS value
                let defaultSem = 
                    if b.Type = typeof<FragmentBuilder> then Intrinsics.Color
                    else Intrinsics.Position

                let! values = getOutputValues defaultSem value

                return Expr.WriteOutputs values

            | Sequential(l, r) ->
                let! l = preprocessNormalS l
                let! r = preprocessNormalS r
                return Expr.Seq [l;r]
                    
            | IfThenElse(cond, i, e) ->
                let! cond = preprocessNormalS cond
                let! i = preprocessNormalS i
                let! e = preprocessNormalS e
                return Expr.IfThenElse(cond, i, e)

            | CallFunction(utility, args) ->
                let! args = args |> List.mapS preprocessNormalS
                match utility.functionTag with
                    | :? State as innerState ->
                        do! State.modify (fun s -> { s with uniforms = Map.union s.uniforms innerState.uniforms })
                        return Expr.CallFunction(utility, args)

                    | _ ->
                        let! state = State.get

                        let mutable innerState = State.createInner state
                        let processedF =
                            utility |> UtilityFunction.map (fun b -> 
                                let run : Preprocess<Expr> = preprocessByTypeS state.expressionType b
                                run.Run(&innerState)
                            )

                        let processedF = { processedF with functionTag = innerState }
                        do! State.mergeInner innerState

                        return Expr.CallFunction(processedF, args)

            | CallWithWitnesses(t, original, m, ws, args) ->
                let! args = args |> List.mapS preprocessNormalS
                let! t = t |> Option.mapS preprocessNormalS
                    
                match UtilityFunction.tryCreate original with
                | Some utility ->
                    let! state = State.get
                    let mutable innerState = State.createInner state
                    let processedF =
                        utility |> UtilityFunction.map (fun b -> 
                            let run : Preprocess<Expr> = preprocessByTypeS state.expressionType b
                            run.Run(&innerState)
                        )

                    let processedF = { processedF with functionTag = innerState }
                    do! State.mergeInner innerState

                    match t with    
                        | Some t -> return Expr.CallFunction(processedF, t :: args)
                        | None -> return Expr.CallFunction(processedF, args)

                | None -> 
                    match t with
                    | Some t -> return Expr.CallWithWitnesses(t, original, m, ws, args)
                    | None -> return Expr.CallWithWitnesses(original, m, ws, args)
                    

            | Call(t, mi, args) ->
                let! args = args |> List.mapS preprocessNormalS
                let! t = t |> Option.mapS preprocessNormalS

                match UtilityFunction.tryCreate mi with
                    | Some utility ->
                        let! state = State.get
                        let mutable innerState = State.createInner state
                        let processedF =
                            utility |> UtilityFunction.map (fun b -> 
                                let run : Preprocess<Expr> = preprocessByTypeS state.expressionType b
                                run.Run(&innerState)
                            )

                        let processedF = { processedF with functionTag = innerState }
                        do! State.mergeInner innerState

                        match t with    
                            | Some t -> return Expr.CallFunction(processedF, t :: args)
                            | None -> return Expr.CallFunction(processedF, args)

                    | None -> 
                        match t with
                            | Some t -> return Expr.Call(t, mi, args)
                            | None -> return Expr.Call(mi, args)
//                            
//                    let! s = State.get
//                    match preprocessMethod s.localSize mi with
//                        | Some(_, innerState) ->
//                            do! State.modify (fun s -> { s with uniforms = Map.union s.uniforms innerState.uniforms })
//                        | None ->
//                            ()
//
//                    let! args = args |> List.mapS preprocessNormalS
//                    let! t = t |> Option.mapS preprocessNormalS
//
//                    match t with
//                        | Some t -> return Expr.Call(t, mi, args)
//                        | None -> return Expr.Call(mi, args)

            | NewArr(t, l, []) ->
                return Expr.DefaultValue(e.Type)

            | NewArr(t, l, args) ->
                let! args = args |> List.mapS preprocessNormalS
                let t = Var("arr", e.Type, false)
                return Expr.Let(
                    t, Expr.DefaultValue(e.Type),
                    Expr.Seq [
                        args |> List.mapi (fun i a -> Expr.ArraySet(Expr.Var t, Expr.Value i, a)) |> Expr.Seq
                        Expr.Var t
                    ]
                )             
                                   
            | ShapeCombination(o, args) ->
                let! args = args |> List.mapS preprocessNormalS
                return RebuildShapeCombination(o, args)

            | ShapeVar _ ->
                return e

            | ShapeLambda(v, b) ->
                let! b = preprocessNormalS b
                return Expr.Lambda(v, b)
        }

    and preprocessByTypeS (typ : ShaderExpressionType) (e : Expr) =
        state {
            do! State.setExpressionType typ

            let! e0 =
                match typ with
                | ShaderExpressionType.Normal -> State.value e
                | ShaderExpressionType.Compute -> preprocessComputeS e
                | ShaderExpressionType.Raytracing s -> preprocessRaytracingS s e

            let! e1 = preprocessNormalS e0
            return Optimizer.hoistImperativeConstructs e1
        }

    and preprocessS (e : Expr) =
        let typ =
            match e with
            | ComputeBuilder _ -> ShaderExpressionType.Compute
            | RaytracingBuilder b -> ShaderExpressionType.Raytracing b.ShaderStage
            | _ -> ShaderExpressionType.Normal

        preprocessByTypeS typ e

    and getOutputValues (sem : string) (value : Expr) : Preprocess<list<string * Option<Expr> * Expr>> =
        state {
            match value.Type with
            | TypeInfo.Patterns.VectorOf (_, (TypeInfo.Patterns.Float32 | TypeInfo.Patterns.Float64 | TypeInfo.Patterns.Int32 | TypeInfo.Patterns.UInt32))
            | TypeInfo.Patterns.Float32 | TypeInfo.Patterns.Float64 | TypeInfo.Patterns.Int32 | TypeInfo.Patterns.UInt32 ->
                let! value = preprocessS value
                do! State.writeOutput sem { paramType = value.Type; paramInterpolation = InterpolationMode.Default }
                return [sem, None, value]

            | _ when FSharpType.IsRecord(value.Type, true) ->
                let fields = FSharpType.GetRecordFields(value.Type, true) |> Array.toList

                let! values = 
                    match value with
                    | NewRecord(_,args) ->
                        List.zip fields args |> List.mapS (fun (f,v) ->
                            state {
                                let sem = f.Semantic
                                let i = f.Interpolation
                                let p = { paramType = f.PropertyType; paramInterpolation = i }
                                do! State.writeOutput sem p

                                if sem = "Depth" then
                                    do! State.setDepthWriteMode f.DepthWriteMode

                                let! real = preprocessS v
                                return sem, None, real
                            }
                        )
                    | _ -> 
                        fields |> List.mapS (fun f ->
                            state {
                                let sem = f.Semantic
                                let i = f.Interpolation
                                let p = { paramType = f.PropertyType; paramInterpolation = i }
                                do! State.writeOutput sem p

                                let! real = preprocessS (Expr.PropertyGet(value, f))
                                return sem, None, real
                            }
                        )

                return values

            | _ ->
                return failwithf "[FShade] invalid vertex-type: %A" value.Type
        }

    and toShaders (inputTypes : List<Type>) (vertexIndex : Map<Var, Expr>) (e : Expr) =
        let run = preprocessS e
        let mutable state = { State.ofInputTypes inputTypes with vertexIndex = vertexIndex }
        let body = run.Run(&state)

        // figure out the used builder-type
        let builder =
            match e with
            | ComputeBuilder sb
            | RaytracingBuilder sb -> sb
            | _ ->
                match state.builder with
                | Some builder ->
                    match Expr.TryEval builder with
                    | Some (:? IShaderBuilder as v) -> v
                    | _ -> failwithf "[FShade] could not evaluate shader-builder %A" builder

                | _ ->
                    failwithf "[FShade] could not evaluate shader-builder %A" state.builder

        let body, outputs =
            match builder.ShaderStage, state.shaders with
                | ShaderStage.TessControl, _ :: _ ->
                    let newBody =
                        let invocationId = Expr.ReadInput<int>(ParameterKind.Input, Intrinsics.InvocationId)
                        Expr.Sequential(
                            Expr.IfThenElse(
                                <@ %invocationId = 0 @>,
                                body,
                                Expr.Unit
                            ),
                            Expr.WriteOutputs Map.empty
                        )

                    let newOutputs =
                        state.outputs |> Map.map (fun _ p -> 
                            { p with paramInterpolation = InterpolationMode.PerPatch }
                        )
                        

                    newBody, newOutputs
                | _ -> 
                    body, state.outputs

        let outputVertices =
            match builder with
                | :? GeometryBuilder as b -> 
                    match b.Size with
                        | Some s -> ShaderOutputVertices.UserGiven s
                        | None -> ShaderOutputVertices.Unknown
                | _ ->
                    ShaderOutputVertices.Unknown

        let inverseMap =
            Seq.map (fun (typ, (name, location)) ->
                name, (typ, location)
            ) >> Map.ofSeq

        let shader = 
            { 
                shaderStage             = builder.ShaderStage
                shaderInputs            = state.inputs
                shaderOutputs           = outputs
                shaderUniforms          = state.uniforms
                shaderInputTopology     = state.inputTopology
                shaderOutputTopology    = builder.OutputTopology
                shaderOutputVertices    = outputVertices
                shaderOutputPrimitives  = None
                shaderInvocations       = 1
                shaderBody              = body
                shaderDebugRange        = None
                shaderDepthWriteMode    = state.depthWriteMode
                shaderPayloads          = inverseMap state.payloads
                shaderPayloadIn         = state.payloadIn
                shaderCallableData      = inverseMap state.callableData
                shaderCallableDataIn    = state.callableDataIn
                shaderHitAttribute      = state.hitAttribute
                shaderRayTypes          = state.rayTypes |> Set.map Sym.ofString
                shaderMissShaders       = state.missShaders |> Set.map Sym.ofString
                shaderCallableShaders   = state.callableShaders |> Set.map Sym.ofString
            }

        shader :: state.shaders

    and preprocess (localSize : V3i) (e : Expr) =
        let mutable state = { State.empty with localSize = localSize }
        let e = preprocessS(e).Run(&state)
        e, state

    and preprocessMethod (localSize : V3i) (mi : MethodBase) =    
        shaderUtilityFunctions.GetOrAdd((localSize, mi), fun (localSize, mi) ->
            match ExprWorkardound.TryGetReflectedDefinition mi with
                | Some expr ->
                    preprocess localSize expr |> Some
                | None ->
                    None
        )

    let rec computeIO (localSize : V3i) (e : Expr) =
        match e with
            | ReadInputOrRaytracingData(kind,n,idx,_) ->
                match idx with
                    | Some idx -> computeIO localSize idx |> Map.add (n, kind) e.Type
                    | None -> Map.ofList [(n, kind), e.Type]

            | WriteOutputs values ->
                let used = 
                    values |> Map.fold (fun m _ (i,v) ->
                        let v = computeIO localSize v
                        let i = i |> Option.map (computeIO localSize) |> Option.defaultValue Map.empty
                        Map.union m (Map.union i v)
                    ) Map.empty

                let written =
                    values |> Map.toSeq |> Seq.map (fun (name, (i,v)) -> 
                        match i with
                            | Some _ -> (name, ParameterKind.Output), v.Type.MakeArrayType()
                            | _ -> (name, ParameterKind.Output), v.Type
                    ) |> Map.ofSeq

                Map.union used written

            
            | Call(t,mi,args) ->
                let inner = 
                    match preprocessMethod localSize mi with
                        | Some(e, _) -> computeIO localSize e
                        | None -> Map.empty

                Option.toList t @ args |> List.fold (fun s a -> Map.union s (computeIO localSize a)) inner

            | CallFunction(f, args) ->
                let used = args |> List.fold (fun m e -> Map.union m (computeIO localSize e)) Map.empty

                Map.union (computeIO localSize f.functionBody) used

            | ShapeCombination(o, args) ->
                args |> List.fold (fun m e -> Map.union m (computeIO localSize e)) Map.empty

            | ShapeLambda(_,b) ->
                computeIO localSize b

            | ShapeVar _ ->
                Map.empty

    let rec usedInputs (localSize : V3i) (e : Expr) =
        match e with
            | ReadInputOrRaytracingData(kind,n,idx,_) ->
                match idx with
                    | Some idx -> usedInputs localSize idx |> Map.add n (kind, e.Type)
                    | None -> Map.ofList [n, (kind, e.Type)]
            
            | Call(t,mi,args) ->
                let inner = 
                    match preprocessMethod localSize mi with
                        | Some(e, _) -> usedInputs localSize e
                        | None -> Map.empty

                Option.toList t @ args |> List.fold (fun s a -> Map.union s (usedInputs localSize a)) inner


            | ShapeCombination(o, args) ->
                args |> List.fold (fun m e -> Map.union m (usedInputs localSize e)) Map.empty

            | ShapeLambda(_,b) ->
                usedInputs localSize b

            | ShapeVar _ ->
                Map.empty



module private GeometryInfo =

    type Info =
        {
            maxVertices : int
            maxPrimitives : int
            maxVC : int
        } with

        static member Zero = { maxVertices = 0; maxPrimitives = 0; maxVC = 0 }

        static member Add (l : Info, r : Info) = 
            {
                maxVertices = l.maxVertices + r.maxVertices
                maxPrimitives = l.maxPrimitives + r.maxPrimitives
                maxVC = l.maxVC + r.maxVC
            }
        static member Merge (l : Info, r : Info) = 
            {
                maxVertices = max l.maxVertices r.maxVertices
                maxPrimitives = max l.maxPrimitives r.maxPrimitives
                maxVC = max l.maxVC r.maxVC
            }

    type Stats =
        {
            stripCount      : int
            info            : Option<Info>
        }

        static member Create (stripCount : int) =
            { 
                stripCount = stripCount
                info = Some Info.Zero
            }
           
        member x.RestartStrip() =
            match x.info with
                | Some info ->
                    { x with info = Some { info with maxVC = 0 } }
                | None ->
                    x
                    
        member l.EmitVertex() =
            match l.info with
                | Some i ->
                    {
                        stripCount = l.stripCount
                        info =
                            Some {
                                maxVertices = i.maxVertices + 1
                                maxPrimitives =
                                    if i.maxVC >= l.stripCount - 1 then
                                        (i.maxPrimitives + 1)
                                    else
                                        i.maxPrimitives
                                maxVC = min l.stripCount (i.maxVC + 1)
                            }
                    }
                | None ->
                    l

        static member Add (l : Stats, r : Stats) =
            {
                stripCount = min l.stripCount r.stripCount
                info = (match l.info, r.info with | Some l, Some r -> Some (Info.Add(l, r)) | _ -> None)
            }

        static member Merge (l : Stats, r : Stats) =
            {
                stripCount = min l.stripCount r.stripCount
                info = (match l.info, r.info with | Some l, Some r -> Some (Info.Merge(l,r)) | _ -> None)
            }

    let rec tryGetUpperBound (e : Expr) =
        match e with
            | Int32 v -> 
                Some v

            | SpecificCall <@ min : int -> int -> int @> (_,_,[l;r]) 
            | SpecificCall <@ Operators.min : int -> int -> int @> (_,_,[l;r]) ->
                match tryGetUpperBound l, tryGetUpperBound r with
                    | Some l, Some r -> Some (min l r)
                    | l, None -> l
                    | None, r -> r

            | SpecificCall <@ (+) @> (_,_,[l;r]) ->
                match tryGetUpperBound l, tryGetUpperBound r with
                    | Some l, Some r -> Some (l + r)
                    | _ -> None

            | SpecificCall <@ (-) @> (_,_,[l;r]) ->
                match tryGetUpperBound l, tryGetLowerBound r with
                    | Some l, Some r -> Some (l - r)
                    | _ -> None

            | SpecificCall <@ (*) @> (_,_,[l;r]) ->
                match tryGetUpperBound l, tryGetUpperBound r with
                    | Some l, Some r -> Some (l * r)
                    | _ -> None

            | SpecificCall <@ (/) @> (_,_,[l;r]) ->
                match tryGetUpperBound l, tryGetLowerBound r with
                    | Some l, Some r -> Some (l / r)
                    | _ -> None

            | IfThenElse(c, i, e) ->
                match c with
                    | Bool true -> tryGetUpperBound i
                    | Bool false -> tryGetUpperBound e
                    | _ ->
                        match tryGetUpperBound i, tryGetUpperBound e with
                            | Some i, Some e -> Some (max i e)
                            | _ -> None



            | _ -> None
                
    and tryGetLowerBound (e : Expr) =
        match e with    
            | Int32 v -> 
                Some v

            | SpecificCall <@ max : int -> int -> int @> (_,_,[l;r])
            | SpecificCall <@ Operators.max : int -> int -> int @> (_,_,[l;r]) ->
                match tryGetUpperBound l, tryGetUpperBound r with
                    | Some l, Some r -> Some (max l r)
                    | l, None -> l
                    | None, r -> r
                            
            | SpecificCall <@ (+) @> (_,_,[l;r]) ->
                match tryGetUpperBound l, tryGetUpperBound r with
                    | Some l, Some r -> Some (l + r)
                    | _ -> None

            | SpecificCall <@ (-) @> (_,_,[l;r]) ->
                match tryGetLowerBound l, tryGetUpperBound r with
                    | Some l, Some r -> Some (l - r)
                    | _ -> None

            | SpecificCall <@ (*) @> (_,_,[l;r]) ->
                match tryGetLowerBound l, tryGetLowerBound r with
                    | Some l, Some r -> Some (l * r)
                    | _ -> None
                            

            | SpecificCall <@ (/) @> (_,_,[l;r]) ->
                match tryGetLowerBound l, tryGetUpperBound r with
                    | Some l, Some r -> Some (l / r)
                    | _ -> None


            | IfThenElse(c, i, e) ->
                match c with
                    | Bool true -> tryGetLowerBound i
                    | Bool false -> tryGetLowerBound e
                    | _ ->
                        match tryGetLowerBound i, tryGetLowerBound e with
                            | Some i, Some e -> Some (max i e)
                            | _ -> None

            | _ ->
                None

    let rec geometryStats (s : Stats) (e : Expr) =
        match e with
            | SpecificCall <@ emitVertex() @> _ ->
                s.EmitVertex()

            | SpecificCall <@ restartStrip() @> _ | SpecificCall <@ endPrimitive() @> _ ->
                s.RestartStrip()

            | ForInteger(v, min, step, max, body) ->
                let bodyStats = geometryStats (Stats.Create s.stripCount) body
                match bodyStats.info with
                    | Some { maxVertices = 0 } ->
                        s
                            
                    | Some info ->
                        match tryGetLowerBound min, tryGetLowerBound step, tryGetUpperBound max with
                            | Some min, Some step, Some max ->
                                let mutable res = s
                                for i in [min .. step .. max] do
                                    res <- geometryStats res body

                                res
                            | _ ->
                                { s with info = None }

                    | None ->
                        { s with info = None }
                
            | Sequential(l,r) ->
                let ls = geometryStats s l
                let rs = geometryStats ls r
                rs

            | IfThenElse(c,i,e) ->
                let s = geometryStats s c

                let si = geometryStats s i
                let se = geometryStats s e

                Stats.Merge(si, se)

            | WhileLoop(guard, body) ->
                let s = geometryStats (Stats.Create s.stripCount) (Expr.Seq [body; guard])

                match s.info with
                    | Some { maxVertices = 0 } ->
                        s
                    | _ ->
                        { s with info = None }

            | ShapeCombination(o, args) ->
                let mutable res = s
                for a in args do
                    res <- geometryStats res a

                res

            | ShapeVar _ ->
                s

            | ShapeLambda(_,b) ->
                geometryStats s b
                



type ShaderOutputValue (mode : InterpolationMode, index : Option<Expr>, value : Expr) =
    let value, inputs, uniforms =
        let value, state = Preprocessor.preprocess V3i.Zero value

        let inputs, uniforms =
            value
                |> Preprocessor.usedInputs V3i.Zero
                |> Map.partition (fun l (kind, t) -> kind = ParameterKind.Input)

        let inputs =
            inputs |> Map.map (fun name (_,t) ->
                match Map.tryFind name state.inputs with
                    | Some p -> p
                    | None -> { paramType = t; paramInterpolation = InterpolationMode.Default }
            )
            
        let uniforms =
            uniforms |> Map.map (fun name (_,t) ->
                match Map.tryFind name state.uniforms with
                    | Some p -> p
                    | None -> { uniformType = t; uniformName = name; uniformValue = UniformValue.Attribute(uniform, name) }
            )

        value, inputs, uniforms


    member x.Type = value.Type
    member x.Index = index
    member x.Value = value
    member x.Interpolation = mode
    member x.UsedInputs = inputs
    member x.UsedUniforms = uniforms

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderOutputValue =
    let inline value (o : ShaderOutputValue) = o.Value
    let inline interpolation (o : ShaderOutputValue) = o.Interpolation
    let inline outputType (o : ShaderOutputValue) = o.Type
    let inline usedInputs (o : ShaderOutputValue) = o.UsedInputs
    let inline usedUniforms (o : ShaderOutputValue) = o.UsedUniforms

    let toParameterDescription (o : ShaderOutputValue) =
        match o.Index with
            | Some i -> 
                { paramType = o.Type.MakeArrayType(); paramInterpolation = o.Interpolation }
            | _ ->
                { paramType = o.Type; paramInterpolation = o.Interpolation }

    let ofParameterDescription (p : ParameterDescription) (value : Expr) =
        match value with
            | Coerce(value,_) -> 
                ShaderOutputValue(p.paramInterpolation, None, value)
            | value ->
                ShaderOutputValue(p.paramInterpolation, None, value)
       
    let ofValue (value : Expr) =
        ShaderOutputValue(InterpolationMode.Default, None, value)

    let withValue (value : Expr) (o : ShaderOutputValue) =
        ShaderOutputValue(o.Interpolation, o.Index, value)  

[<AutoOpen>]
module ShaderOutputValueExtensions = 
     type Expr with
        static member WriteOutputs (values : Map<string, ShaderOutputValue>) =
            values
                |> Map.map (fun _ v -> v.Index, v.Value)
                |> Expr.WriteOutputs

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Shader =

    module internal Utils =
        open System.Reflection.Emit

        type Invoker<'a, 'b>() =
            static let invoke =
                let t = typeof<'a>

                let rec decompose (t : Type) =
                    if FSharpType.IsFunction t then
                        let (a, r) = FSharpType.GetFunctionElements t

                        match decompose r with
                        | Some (args, ret) ->
                            Some ((a, t) :: args, ret)
                        | None ->
                            None
                    elif typeof<'b>.IsAssignableFrom t then
                        Some ([], t)
                    else
                        None

                match decompose t with
                | Some (args, ret) ->
                    match args with
                    | [] ->
                        fun (v : 'a) -> unbox<'b> v, []
                    | _ ->
                        let argumentTypes = args |> List.map fst
                        let dyn = DynamicMethod("invoker", MethodAttributes.Static ||| MethodAttributes.Public, CallingConventions.Standard, typeof<'b>, [| t |], t, true)

                        let il = dyn.GetILGenerator()

                        il.Emit(OpCodes.Ldarg_0)
                        for a, fType in args do
                            if a.IsValueType then
                                let l = il.DeclareLocal(a)
                                il.Emit(OpCodes.Ldloc, l)
                            else
                                il.Emit(OpCodes.Ldnull)

                            let m = fType.GetMethod("Invoke", [| a |])
                            il.EmitCall(OpCodes.Callvirt, m, null)

                        il.Emit(OpCodes.Ret)

                        let d = dyn.CreateDelegate(typeof<Func<'a, 'b>>) |> unbox<Func<'a, 'b>>

                        fun (v : 'a) -> d.Invoke(v), argumentTypes
                | None ->
                    fun (v : 'a) -> failwithf "[FShade] cannot invoke type %A" t

            static member Invoke(f : 'a) =
                invoke f

        let rec tryExtractExpr (f : 'a) =
            match f :> obj with
            | null -> None
            | :? Expr as e -> Some (e, [])
            | _ ->
                try Invoker<'a, Expr>.Invoke f |> Some
                with _ -> None

    let tryGetOverrideCode (localSize : V3i) (mi : MethodBase) =
        match Preprocessor.preprocessMethod localSize mi with
            | Some (e,_) -> Some e
            | None -> None

    let private builtInInputs =
        Dictionary.ofList [
            ShaderStage.Vertex,
                Map.ofList [
                    Intrinsics.VertexId, typeof<int>
                    Intrinsics.InstanceId, typeof<int>
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.PatchVertices, typeof<int>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.InvocationId, typeof<int>
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.TessCoord, typeof<V3d>
                    Intrinsics.PatchVertices, typeof<int>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.TessLevelInner, typeof<Arr<2 N, float>>
                    Intrinsics.TessLevelOuter, typeof<Arr<4 N, float>>
                ]
                
            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.SourceVertexIndex, typeof<int[]>
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.InvocationId, typeof<int>
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.FragCoord, typeof<V4d>
                    Intrinsics.PointCoord, typeof<V2d>
                    Intrinsics.FrontFacing, typeof<bool>
                    Intrinsics.SampleId, typeof<int>
                    Intrinsics.SamplePosition, typeof<V2d>
                    Intrinsics.SampleMask, typeof<int[]>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.Layer, typeof<int>
                    Intrinsics.ViewportIndex, typeof<int>
                ]

            ShaderStage.RayGeneration,
                Map.ofList [
                    Intrinsics.LaunchId, typeof<V3i>
                    Intrinsics.LaunchSize, typeof<V3i>
                ]

            ShaderStage.Intersection,
                Map.ofList [
                    Intrinsics.LaunchId, typeof<V3i>
                    Intrinsics.LaunchSize, typeof<V3i>

                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.InstanceId, typeof<int>
                    Intrinsics.InstanceCustomIndex, typeof<int>
                    Intrinsics.GeometryIndex, typeof<int>

                    Intrinsics.WorldRayOrigin, typeof<V3d>
                    Intrinsics.WorldRayDirection, typeof<V3d>
                    Intrinsics.ObjectRayOrigin, typeof<V3d>
                    Intrinsics.ObjectRayDirection, typeof<V3d>

                    Intrinsics.RayTmin, typeof<float>
                    Intrinsics.RayTmax, typeof<float>
                    Intrinsics.IncomingRayFlags, typeof<uint32>

                    Intrinsics.ObjectToWorld, typeof<M44d>
                    Intrinsics.WorldToObject, typeof<M44d>
                ]

            ShaderStage.AnyHit,
                Map.ofList [
                    Intrinsics.LaunchId, typeof<V3i>
                    Intrinsics.LaunchSize, typeof<V3i>

                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.InstanceId, typeof<int>
                    Intrinsics.InstanceCustomIndex, typeof<int>
                    Intrinsics.GeometryIndex, typeof<int>

                    Intrinsics.WorldRayOrigin, typeof<V3d>
                    Intrinsics.WorldRayDirection, typeof<V3d>
                    Intrinsics.ObjectRayOrigin, typeof<V3d>
                    Intrinsics.ObjectRayDirection, typeof<V3d>

                    Intrinsics.RayTmin, typeof<float>
                    Intrinsics.RayTmax, typeof<float>
                    Intrinsics.IncomingRayFlags, typeof<uint32>

                    Intrinsics.HitT, typeof<float>
                    Intrinsics.HitKind, typeof<uint32>

                    Intrinsics.ObjectToWorld, typeof<M44d>
                    Intrinsics.WorldToObject, typeof<M44d>
                ]

            ShaderStage.ClosestHit,
                Map.ofList [
                    Intrinsics.LaunchId, typeof<V3i>
                    Intrinsics.LaunchSize, typeof<V3i>

                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.InstanceId, typeof<int>
                    Intrinsics.InstanceCustomIndex, typeof<int>
                    Intrinsics.GeometryIndex, typeof<int>

                    Intrinsics.WorldRayOrigin, typeof<V3d>
                    Intrinsics.WorldRayDirection, typeof<V3d>
                    Intrinsics.ObjectRayOrigin, typeof<V3d>
                    Intrinsics.ObjectRayDirection, typeof<V3d>

                    Intrinsics.RayTmin, typeof<float>
                    Intrinsics.RayTmax, typeof<float>
                    Intrinsics.IncomingRayFlags, typeof<uint32>

                    Intrinsics.HitT, typeof<float>
                    Intrinsics.HitKind, typeof<uint32>

                    Intrinsics.ObjectToWorld, typeof<M44d>
                    Intrinsics.WorldToObject, typeof<M44d>
                ]

            ShaderStage.Miss,
                Map.ofList [
                    Intrinsics.LaunchId, typeof<V3i>
                    Intrinsics.LaunchSize, typeof<V3i>

                    Intrinsics.WorldRayOrigin, typeof<V3d>
                    Intrinsics.WorldRayDirection, typeof<V3d>

                    Intrinsics.RayTmin, typeof<float>
                    Intrinsics.RayTmax, typeof<float>
                    Intrinsics.IncomingRayFlags, typeof<uint32>
                ]

            ShaderStage.Callable,
                Map.ofList [
                    Intrinsics.LaunchId, typeof<V3i>
                    Intrinsics.LaunchSize, typeof<V3i>
                ]
        ]

    let private builtInOutputs =
        Dictionary.ofList [
            ShaderStage.Vertex,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.TessLevelInner, typeof<Arr<2 N, float>>
                    Intrinsics.TessLevelOuter, typeof<Arr<4 N, float>>
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                ]

            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.Layer, typeof<int>
                    Intrinsics.ViewportIndex, typeof<int>
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.Depth, typeof<float>
                    Intrinsics.SampleMask, typeof<int[]>
                ]

            ShaderStage.RayGeneration,
                Map.ofList [
                ]

            ShaderStage.Intersection,
                Map.ofList [
                ]

            ShaderStage.AnyHit,
                Map.ofList [
                ]

            ShaderStage.ClosestHit,
                Map.ofList [
                ]

            ShaderStage.Miss,
                Map.ofList [
                ]

            ShaderStage.Callable,
                Map.ofList [
                ]
        ]

    let private sideEffects =
        Dictionary.ofList [
            ShaderStage.Vertex, 
                HashSet.ofList [
                    getMethodInfo <@ barrier @>
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.TessControl, 
                HashSet.ofList [
                    getMethodInfo <@ barrier @>
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.TessEval, 
                HashSet.ofList [
                    getMethodInfo <@ barrier @>
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.Geometry, 
                HashSet.ofList [
                    getMethodInfo <@ emitVertex @>
                    getMethodInfo <@ restartStrip @>
                    getMethodInfo <@ endPrimitive @>
                    getMethodInfo <@ barrier @>
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.Fragment, 
                HashSet.ofList [
                    getMethodInfo <@ discard @>
                    getMethodInfo <@ barrier @>
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.Compute,
                HashSet.ofList [
                    getMethodInfo <@ barrier @>
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.RayGeneration,
                HashSet.ofList [
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.Intersection,
                HashSet.ofList [
                    getMethodInfo <@ reportIntersection @>
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.AnyHit,
                HashSet.ofList [
                    getMethodInfo <@ ignoreIntersection @>
                    getMethodInfo <@ terminateRay @>
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.ClosestHit,
                HashSet.ofList [
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.Miss,
                HashSet.ofList [
                    getMethodInfo <@ Debug.Printf @>
                ]

            ShaderStage.Callable,
                HashSet.ofList [
                    getMethodInfo <@ Debug.Printf @>
                ]
        ]

    let private typeConversions =
        LookupTable.lookupTable' [
            // Float32
            (typeof<V2f>, typeof<float32>), fun value -> <@@ (%%value : V2f).X @@>

            (typeof<V3f>, typeof<float32>), fun value -> <@@ (%%value : V3f).X @@>
            (typeof<V3f>, typeof<V2f>),     fun value -> <@@ (%%value : V3f).XY @@>
            (typeof<V3f>, typeof<V4f>),     fun value -> <@@ V4f((%%value : V3f), 1.0f) @@>

            (typeof<V4f>, typeof<float32>), fun value -> <@@ (%%value : V4f).X @@>
            (typeof<V4f>, typeof<V2f>),     fun value -> <@@ (%%value : V4f).XY @@>
            (typeof<V4f>, typeof<V3f>),     fun value -> <@@ (%%value : V4f).XYZ @@>

            // Float32 -> Float64
            (typeof<float32>, typeof<float>), fun value -> <@@ float (%%value : float32) @@>

            (typeof<V2f>, typeof<float>),     fun value -> <@@ float (%%value : V2f).X @@>
            (typeof<V2f>, typeof<V2d>),       fun value -> <@@ V2d (%%value : V2f) @@>

            (typeof<V3f>, typeof<float>),     fun value -> <@@ float (%%value : V3f).X @@>
            (typeof<V3f>, typeof<V2d>),       fun value -> <@@ V2d(%%value : V3f).XY @@>
            (typeof<V3f>, typeof<V3d>),       fun value -> <@@ V3d(%%value : V3f) @@>
            (typeof<V3f>, typeof<V4d>),       fun value -> <@@ V4d((%%value : V3f), 1.0f) @@>

            (typeof<V4f>, typeof<float>),     fun value -> <@@ float (%%value : V4f).X @@>
            (typeof<V4f>, typeof<V2d>),       fun value -> <@@ V2d(%%value : V4f).XY @@>
            (typeof<V4f>, typeof<V3d>),       fun value -> <@@ V3d(%%value : V4f).XYZ @@>
            (typeof<V4f>, typeof<V4d>),       fun value -> <@@ V4d(%%value : V4f) @@>

            // Float64
            (typeof<V2d>, typeof<float>), fun value -> <@@ (%%value : V2d).X @@>

            (typeof<V3d>, typeof<float>), fun value -> <@@ (%%value : V3d).X @@>
            (typeof<V3d>, typeof<V2d>),   fun value -> <@@ (%%value : V3d).XY @@>
            (typeof<V3d>, typeof<V4d>),   fun value -> <@@ V4d((%%value : V3d), 1.0) @@>

            (typeof<V4d>, typeof<float>), fun value -> <@@ (%%value : V4d).X @@>
            (typeof<V4d>, typeof<V2d>),   fun value -> <@@ (%%value : V4d).XY @@>
            (typeof<V4d>, typeof<V3d>),   fun value -> <@@ (%%value : V4d).XYZ @@>

            // Float64 -> Float32
            (typeof<float>, typeof<float32>), fun value -> <@@ float32 (%%value : float) @@>

            (typeof<V2d>, typeof<float32>),   fun value -> <@@ float32 (%%value : V2d).X @@>
            (typeof<V2d>, typeof<V2f>),       fun value -> <@@ V2f (%%value : V2d) @@>

            (typeof<V3d>, typeof<float32>),   fun value -> <@@ float32 (%%value : V3d).X @@>
            (typeof<V3d>, typeof<V2f>),       fun value -> <@@ V2f(%%value : V3d).XY @@>
            (typeof<V3d>, typeof<V3f>),       fun value -> <@@ V3f(%%value : V3d) @@>
            (typeof<V3d>, typeof<V4f>),       fun value -> <@@ V4f((%%value : V3d), 1.0) @@>

            (typeof<V4d>, typeof<float32>),   fun value -> <@@ float32 (%%value : V4d).X @@>
            (typeof<V4d>, typeof<V2f>),       fun value -> <@@ V2f(%%value : V4d).XY @@>
            (typeof<V4d>, typeof<V3f>),       fun value -> <@@ V3f(%%value : V4d).XYZ @@>
            (typeof<V4d>, typeof<V4f>),       fun value -> <@@ V4f(%%value : V4d) @@>

            // Int32
            (typeof<V2i>, typeof<int32>), fun value -> <@@ (%%value : V2i).X @@>

            (typeof<V3i>, typeof<int32>), fun value -> <@@ (%%value : V3i).X @@>
            (typeof<V3i>, typeof<V2i>),   fun value -> <@@ (%%value : V3i).XY @@>

            (typeof<V4i>, typeof<int32>), fun value -> <@@ (%%value : V4i).X @@>
            (typeof<V4i>, typeof<V2i>),   fun value -> <@@ (%%value : V4i).XY @@>
            (typeof<V4i>, typeof<V3i>),   fun value -> <@@ (%%value : V4i).XYZ @@>

            // UInt32
            (typeof<V2ui>, typeof<uint32>), fun value -> <@@ (%%value : V2ui).X @@>

            (typeof<V3ui>, typeof<uint32>), fun value -> <@@ (%%value : V3ui).X @@>
            (typeof<V3ui>, typeof<V2ui>),   fun value -> <@@ (%%value : V3ui).XY @@>

            (typeof<V4ui>, typeof<uint32>), fun value -> <@@ (%%value : V4ui).X @@>
            (typeof<V4ui>, typeof<V2ui>),   fun value -> <@@ (%%value : V4ui).XY @@>
            (typeof<V4ui>, typeof<V3ui>),   fun value -> <@@ (%%value : V4ui).XYZ @@>
        ]

    let private converter (semantic : string) (inType : Type) (outType : Type) : Expr -> Expr =
        if inType <> outType then
            match typeConversions (inType, outType) with
            | Some conv -> conv
            | _ -> failwithf "[FShade] cannot convert %s value from %A to %A" semantic inType outType
        else
            id

    let private tryGetSourceVertexIndex (shader : Shader) (values : Map<string, Option<Expr> * Expr>) =
        match shader.shaderInputTopology with
            | Some InputTopology.Point -> Expr.Value 0 |> Some
            | _ -> Map.tryFind Intrinsics.SourceVertexIndex values |> Option.map snd

    let private tryGetSourceVertexIndexValue (shader : Shader) (values : Map<string, ShaderOutputValue>) =
        match shader.shaderInputTopology with
            | Some InputTopology.Point -> Expr.Value 0 |> Some
            | _ -> Map.tryFind Intrinsics.SourceVertexIndex values |> Option.map ShaderOutputValue.value

    let private emitVertexMeth = getMethodInfo <@ emitVertex @>

    let private interpolate3 (coord : Expr) (p0 : Expr) (p1 : Expr) (p2 : Expr) =
        let coord : Expr<V3d> = Expr.Cast coord
        if p0.Type = typeof<float> then
            <@@ (%coord).X * (%%p0 : float) + (%coord).Y * (%%p1 : float) + (%coord).Z * (%%p2 : float)  @@>
        elif p0.Type = typeof<V2d> then
            <@@ (%coord).X * (%%p0 : V2d) + (%coord).Y * (%%p1 : V2d) + (%coord).Z * (%%p2 : V2d)  @@>
        elif p0.Type = typeof<V3d> then
            <@@ (%coord).X * (%%p0 : V3d) + (%coord).Y * (%%p1 : V3d) + (%coord).Z * (%%p2 : V3d)  @@>
        elif p0.Type = typeof<V4d> then
            <@@ (%coord).X * (%%p0 : V4d) + (%coord).Y * (%%p1 : V4d) + (%coord).Z * (%%p2 : V4d)  @@>
        else
            failwithf "[FShade] cannot interpolate %A" p0.Type

    let private interpolate4 (coord : Expr) (p0 : Expr) (p1 : Expr) (p2 : Expr) (p3 : Expr) =
        let coord : Expr<V3d> = Expr.Cast coord
        
        if p0.Type = typeof<float> then
            <@@ 
                (1.0 - (%coord).Y) * ((1.0 - (%coord).X) * (%%p0 : float) + (%coord).X * (%%p1 : float)) +
                (%coord).Y * ((1.0 - (%coord).X) * (%%p2 : float) + (%coord).X * (%%p3 : float)) 
            @@>
        elif p0.Type = typeof<V2d> then
            <@@ 
                (1.0 - (%coord).Y) * ((1.0 - (%coord).X) * (%%p0 : V2d) + (%coord).X * (%%p1 : V2d)) +
                (%coord).Y * ((1.0 - (%coord).X) * (%%p2 : V2d) + (%coord).X * (%%p3 : V2d)) 
            @@>
        elif p0.Type = typeof<V3d> then
            <@@ 
                (1.0 - (%coord).Y) * ((1.0 - (%coord).X) * (%%p0 : V3d) + (%coord).X * (%%p1 : V3d)) +
                (%coord).Y * ((1.0 - (%coord).X) * (%%p2 : V3d) + (%coord).X * (%%p3 : V3d)) 
            @@>
        elif p0.Type = typeof<V4d> then
            <@@ 
                (1.0 - (%coord).Y) * ((1.0 - (%coord).X) * (%%p0 : V4d) + (%coord).X * (%%p1 : V4d)) +
                (%coord).Y * ((1.0 - (%coord).X) * (%%p2 : V4d) + (%coord).X * (%%p3 : V4d)) 
            @@>
        else
            failwithf "[FShade] cannot interpolate %A" p0.Type


    let inline stage (s : Shader) = s.shaderStage
    let inline uniforms (s : Shader) = s.shaderUniforms
    let inline body (s : Shader) = s.shaderBody
    let inline inputTopology (s : Shader) = s.shaderInputTopology
    let inline outputTopology (s : Shader) = s.shaderOutputTopology

    let systemInputs (shader : Shader) =
        let builtIn = builtInInputs.[shader.shaderStage]
        shader.shaderInputs |> Map.choose (fun name p ->
            match Map.tryFind name builtIn with
                | Some neededType ->
                    let typesValid =
                        match neededType, p.paramType with
                            | (ArrOf(_, nt) | ArrayOf nt), (ArrOf(_, pt) | ArrayOf pt) -> nt = pt
                            | n, p -> n = p

                    if not typesValid then
                        failwithf "[FShade] invalid system-input type %A (should be %A)" p.paramType neededType

                    Some p.paramType
                | None ->
                    None
        )

    let systemOutputs (shader : Shader) =
        let builtIn = builtInOutputs.[shader.shaderStage]
        shader.shaderOutputs |> Map.choose (fun name p ->
            match Map.tryFind name builtIn with
                | Some neededType ->
                    let typesValid =
                        match neededType, p.paramType with
                            | (ArrOf(_, nt) | ArrayOf nt), (ArrOf(_, pt) | ArrayOf pt) -> nt = pt
                            | n, p -> n = p

                    if not typesValid then
                        failwithf "[FShade] invalid system-output type %A (should be %A)" p.paramType neededType

                    Some p.paramType
                | None ->
                    None
        )

    /// optimizes the shader by
    ///    1) evaluating constant expressions
    ///    2) removing useless expressions/variables
    ///    3) unrolling loops where annotated
    ///
    /// and might in the future:
    ///    3) inline copy variables
    ///    4) inline functions where possible
    let optimize (shader : Shader) =
        let sideEffects = sideEffects.[shader.shaderStage]

        let isSideEffect (m : MethodInfo) =
            if sideEffects.Contains m then
                true
            else
                m.GetCustomAttributes<KeepCallAttribute>()
                |> Seq.isEmpty
                |> not

        let newBody, state = 
            shader.shaderBody
                |> Optimizer.inlining isSideEffect
                |> Optimizer.hoistImperativeConstructs
                |> Optimizer.evaluateConstants' isSideEffect
                |> Optimizer.eliminateDeadCode' isSideEffect
                |> Optimizer.evaluateConstants' isSideEffect
                |> Optimizer.liftInputs
                |> Preprocessor.preprocess V3i.Zero

        let newOutputVertices, newOutputPrimitives =
            match shader.shaderStage with
                | ShaderStage.Geometry ->
                    match shader.shaderOutputVertices with
                        | ShaderOutputVertices.Computed _ | ShaderOutputVertices.Unknown ->
                            let outputVertices =
                                match shader.shaderOutputTopology.Value with
                                    | OutputTopology.Points -> 1
                                    | OutputTopology.LineStrip -> 2
                                    | OutputTopology.TriangleStrip -> 3

                            let stats = GeometryInfo.geometryStats (GeometryInfo.Stats.Create outputVertices) newBody

                            let maxVertices, maxPrimitives =
                                match stats.info with
                                    | Some { maxVertices = v; maxPrimitives = p } ->
                                        v, Some p
                                    | _ ->
                                        Log.warn "[FShade] could not determine max-vertex-count (using 32)"
                                        32, None

                            ShaderOutputVertices.Computed maxVertices, maxPrimitives
                        | ov ->
                            ov, shader.shaderOutputPrimitives
                | _ ->
                    shader.shaderOutputVertices, shader.shaderOutputPrimitives

        { shader with
            shaderInputs = state.inputs |> Map.map (fun name desc -> match Map.tryFind name shader.shaderInputs with | Some od when od.paramType = desc.paramType -> od | _ -> desc) //shader.shaderInputs |> Map.filter (fun n _ -> state.inputs.ContainsKey n)
            shaderUniforms = state.uniforms |> Map.map (fun name u -> match Map.tryFind name shader.shaderUniforms with Some ou -> ou | _ -> u) //shader.shaderUniforms |> Map.filter (fun n _ -> state.uniforms.ContainsKey n)
            shaderOutputVertices = newOutputVertices
            shaderOutputPrimitives = newOutputPrimitives
            shaderBody = newBody
        }

    let ofExpr (inputTypes : List<Type>) (e : Expr) =
        let debugRange = e.CustomAttributes |> List.tryPick (function DebugRange r -> Some r | _ -> None)
        Preprocessor.toShaders inputTypes Map.empty e 
            |> List.map optimize
            |> List.map (fun shader -> { shader with shaderDebugRange = debugRange })

    let ofFunction (shaderFunction : 'a -> Expr<'b>) =
        let expression = 
            try shaderFunction Unchecked.defaultof<'a>
            with 
                | :? System.NullReferenceException as n -> 
                    failwithf "[FShade] shader functions may not access their vertex-input statically (inner cause - NullReferenceException: %A)" n.StackTrace
                | e -> 
                    failwithf "[FShade] failed to execute shader function.\nInner cause: %A at\n%A" e e.StackTrace

        ofExpr [typeof<'a>] expression

    let withBody (newBody : Expr) (shader : Shader) =
        let newBody, state = newBody |> Preprocessor.preprocess V3i.Zero
        let io = Preprocessor.computeIO V3i.Zero newBody

        let filterIO (desiredKind : ParameterKind) (build : string -> Type -> 'a) =

            io  |> Map.toSeq 
                |> Seq.choose (fun ((name, kind), t) -> 
                    if kind = desiredKind then 
                        Some (name, build name t)
                    else 
                        None
                )
                |> Map.ofSeq

        let newInputs = 
            filterIO ParameterKind.Input (fun name t ->
                match Map.tryFind name shader.shaderInputs with
                    | Some p -> p
                    | _ -> 
                        match Map.tryFind name state.inputs with
                            | Some p -> p
                            | None -> ParameterDescription.ofType t
            )

        let newOutputs = 
            filterIO ParameterKind.Output (fun name t ->
                match Map.tryFind name shader.shaderOutputs with
                    | Some p -> p
                    | _ -> 
                        match Map.tryFind name state.outputs with
                            | Some p -> p
                            | None -> ParameterDescription.ofType t
            )

        let newUniforms =
            filterIO ParameterKind.Uniform (fun name t ->
                match Map.tryFind name shader.shaderUniforms with
                    | Some p -> p
                    | None -> 
                        match Map.tryFind name state.uniforms with
                            | Some p -> p
                            | None -> { uniformName = name; uniformType = t; uniformValue = UniformValue.Attribute(uniform, name) }
            )

        optimize
            { shader with
                shaderUniforms          = newUniforms
                shaderInputs            = newInputs
                shaderOutputs           = newOutputs
                shaderBody              = newBody
            }


    /// creates a new shader by substituting output-writes with an epxression.
    /// when None is returned the respective write-expression will remain untouched
    let substituteWrites (f : Map<string, ShaderOutputValue> -> Option<Expr>) (shader : Shader) =
        let newBody =
            shader.shaderBody.SubstituteWrites (fun values ->
                values
                    |> Map.map (fun n (index, value) -> ShaderOutputValue(shader.shaderOutputs.[n].paramInterpolation, index, value))
                    |> f
            )

        shader |> withBody newBody
  
    let substituteReads (f : ParameterKind -> Type -> string -> Option<Expr> -> Option<ShaderSlot> -> Option<Expr>) (shader : Shader) =
        let newBody =
            shader.shaderBody.SubstituteReads (fun k t n i s ->
                f k t n i s
            )

        shader |> withBody newBody

    let uniformsToInputs (semantics : Set<string>) (shader : Shader) =
        let needed = semantics |> Set.exists (fun n -> Map.containsKey n shader.shaderUniforms)
        if needed then
            shader |> substituteReads (fun kind inputType name index slot ->
                match kind with
                    | ParameterKind.Uniform when Set.contains name semantics ->
                        match index with
                            | Some index ->
                                failwith "[FShade] encountered indexed uniform-read" 
                            | None ->
                                match shader.shaderStage with
                                    | ShaderStage.Vertex | ShaderStage.Fragment ->
                                        Expr.ReadInput(ParameterKind.Input, inputType, name, slot) |> Some

                                    | ShaderStage.Geometry | ShaderStage.TessControl | ShaderStage.TessEval ->
                                        Log.warn "[FShade] potentially bad uniform->input conversion (using vertex 0)"
                                        Expr.ReadInput(ParameterKind.Input, inputType, name, Expr.Value 0, slot) |> Some

                                    | stage ->
                                        failwithf "[FShade] unknown ShaderStage %A" stage
                    | _ ->
                        None
            )
        else
            shader

    let inputsToUniforms (scopes : Map<string, UniformScope>) (shader : Shader) =
        let newBody = shader.shaderBody

        // replace all input-reads (on the given semantics) with uniform-reads
        let newBody = 
            if scopes |> Map.exists (fun n _ -> Map.containsKey n shader.shaderInputs) then
                let semantics = scopes |> Map.keys |> Set.ofSeq
                newBody |> Expr.substituteReads (fun kind inputType name index slot ->
                    match kind with
                        | ParameterKind.Input when Set.contains name semantics ->
                            match index with
                                | Some index -> 
                                    let inputType = 
                                        if inputType.IsArray then inputType.GetElementType()
                                        else inputType

                                    Expr.ReadInput(ParameterKind.Uniform, inputType, name, slot) |> Some
                                | None ->
                                    Expr.ReadInput(ParameterKind.Uniform, inputType, name, slot) |> Some
                        | _ ->
                            None
                )
            else
                newBody

        // remove all writes to the given semantics
        let newBody =
            if scopes |> Map.exists (fun n _ -> Map.containsKey n shader.shaderOutputs) then 
                newBody |> Expr.substituteWrites (fun values ->
                    Map.difference values scopes
                        |> Expr.WriteOutputs
                        |> Some
                )
            else
                newBody

        // if the body was untouched simply return the old shader
        if newBody = shader.shaderBody then 
            shader
        else 
            let newShader = withBody newBody shader
            { newShader with
                shaderUniforms = 
                    newShader.shaderUniforms |> Map.map (fun name u ->
                        match Map.tryFind name scopes with
                            | Some scope -> { u with uniformValue = UniformValue.Attribute(scope, name) }
                            | _ -> u
                    )
            }

    /// creates a new shader having exactly the the given outputs by:
    ///     1) removing the ones that are not needed
    ///     2) adding the ones that are not existing
    ///     3) changing the type (using value-conversions) for the ones with bad types
    /// NOTE: by passing typeof<obj> for a semantic the function does not 
    ///       change existing types but can obviously not add a new output for it.
    let withOutputs (outputs : Map<string, Type>) (shader : Shader) =
        let current = shader.shaderOutputs |> Map.map (fun n p -> p.paramType)

        let allWrites (shader : Shader) =
            let rec allWrites (e : Expr) =
                match e with
                    | WriteOutputs values       -> Seq.singleton values
                    | ShapeLambda(_,b)          -> allWrites b
                    | ShapeVar _                -> Seq.empty
                    | ShapeCombination(_,args)  -> args |> Seq.collect allWrites

            allWrites shader.shaderBody

        if current = outputs then
            shader
        else
            let patchOutputs =
                if shader.shaderStage = ShaderStage.TessControl then
                    shader.shaderOutputs |> Map.filter (fun name p ->
                        p.paramInterpolation.HasFlag InterpolationMode.PerPatch
                    )
                else
                    Map.empty

            let shader =
                shader |> substituteWrites (fun values ->
                    let isPatchOutput = 
                        if shader.shaderStage = ShaderStage.TessControl then
                            values |> Map.exists (fun _ v -> v.Interpolation.HasFlag InterpolationMode.PerPatch)
                        else
                            false

                    if isPatchOutput then
                        let newValues = 
                            values |> Map.choose (fun n v ->
                                match Map.tryFind n outputs with
                                | Some t ->
                                    if t = v.Type then Some v
                                    else v |> ShaderOutputValue.withValue (converter n v.Type t v.Value) |> Some
                                | None ->
                                    None
                            )

                        newValues |> Expr.WriteOutputs |> Some

                    else
                        let outputs = Map.difference outputs patchOutputs

                        let write =
                            if Map.containsKey Intrinsics.FragmentPosition outputs then
                                fun (m : Map<string, ShaderOutputValue>) ->
                                    match Map.tryFind Intrinsics.Position m with
                                    | Some value ->
                                        match value.Value with
                                        | Trivial ->
                                            Expr.WriteOutputs (Map.add Intrinsics.FragmentPosition value m)
                                                |> Some
                                        | nonTrivial ->
                                            let v = Var("position", nonTrivial.Type)
                                            let ve = Expr.Var v
                                            Expr.Let(v, value.Value,
                                                Expr.WriteOutputs(
                                                    m |> Map.add Intrinsics.Position (value |> ShaderOutputValue.withValue ve)
                                                        |> Map.add Intrinsics.FragmentPosition (value |> ShaderOutputValue.withValue ve)
                                                )
                                            )
                                            |> Some

                                    | None ->
                                        failwith "[FShade] positions not written before fragment shader"
                            else
                                Expr.WriteOutputs >> Some

                        Map.difference outputs patchOutputs
                            |> Map.map (fun n t ->
                                match Map.tryFind n values with
                                | Some value ->
                                    if not (t.IsAssignableFrom value.Type) then
                                        value |> ShaderOutputValue.withValue (converter n value.Type t value.Value)
                                    else
                                        value
                                | None ->
                                    if t = typeof<obj> then
                                        failwithf "[FShade] cannot add output %A with object type" n
                                    
                                    match shader.shaderStage with
                                    | ShaderStage.Vertex | ShaderStage.Fragment ->
                                        Expr.ReadInput(ParameterKind.Input, t, n)
                                            |> ShaderOutputValue.ofValue

                                    | ShaderStage.Geometry ->
                                        match tryGetSourceVertexIndexValue shader values with
                                        | Some index -> 
                                            Expr.ReadInput(ParameterKind.Input, t, n, index)
                                                |> ShaderOutputValue.ofValue
                                        | None -> 
                                            failwithf "[FShade] cannot add output %A to GeometryShader since no SourceVertexIndex was available" n
                                    
                                    | ShaderStage.TessControl ->
                                        let invocationId = Expr.ReadInput<int>(ParameterKind.Input, Intrinsics.InvocationId).Raw
                                        let t = 
                                            match t with
                                            | ArrayOf t -> t
                                            | _ -> t
                                        ShaderOutputValue(
                                            InterpolationMode.Default, 
                                            Some invocationId, 
                                            Expr.ReadInput(ParameterKind.Input, t, n, invocationId)
                                        )

                                    | ShaderStage.TessEval ->
                                        match shader.shaderInputTopology.Value with
                                        | InputTopology.Patch 3 | InputTopology.Triangle ->
                                            let coord = Expr.ReadInput<V3d>(ParameterKind.Input, Intrinsics.TessCoord)
                                            let p0 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 0)
                                            let p1 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 1)
                                            let p2 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 2)
                                            interpolate3 coord p0 p1 p2 |> ShaderOutputValue.ofValue

                                        | InputTopology.Patch 4 ->
                                            let coord = Expr.ReadInput<V3d>(ParameterKind.Input, Intrinsics.TessCoord)
                                            let p0 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 0)
                                            let p1 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 1)
                                            let p2 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 2)
                                            let p3 = Expr.ReadInput(ParameterKind.Input, t, n, Expr.Value 3)
                                            interpolate4 coord p0 p1 p2 p3 |> ShaderOutputValue.ofValue
                                        | _ ->
                                            failwith "[FShade] cannot pass for n-ary tess-eval shader"

                                    | _ ->
                                        failwith "[FShade] passing for tessellation not implemented"
                            )
                            |> write
                )

            { shader with
                shaderOutputs = outputs |> Map.map (fun _ -> ParameterDescription.ofType) }

    /// creates a new shader by removing all outputs given in semantics
    let removeOutputs (semantics : Set<string>) (shader : Shader) =
        let desired = 
            shader.shaderOutputs |> Map.choose (fun name p ->
                if Set.contains name semantics then None
                else Some p.paramType
            )

        withOutputs desired shader

    /// translates a shader to an EntryPoint which can be used for compiling
    /// the shader to a CAst
    let toEntryPoint (prev : Option<Shader>) (s : Shader) (next : Option<Shader>) =
        let mutable hasSourceVertexIndex = false
        let inputs = 
            s.shaderInputs |> Map.toList |> List.choose (fun (n,i) -> 
                if s.shaderStage = ShaderStage.Geometry && n = Intrinsics.SourceVertexIndex then
                    hasSourceVertexIndex <- true
                    None
                else
                    Some { 
                        paramName = n
                        paramSemantic = n
                        paramType = i.paramType
                        paramDecorations = Set.ofList [ParameterDecoration.Interpolation i.paramInterpolation]
                    }
            )

        let depthWriteMode =
            if s.shaderStage = ShaderStage.Fragment && s.shaderDepthWriteMode <> DepthWriteMode.None then
                Set.ofList [ ParameterDecoration.DepthWrite s.shaderDepthWriteMode ]
            else
                Set.empty

        let outputs = 
            s.shaderOutputs |> Map.toList |> List.map (fun (n, i) -> 
                let dec = Set.ofList [ParameterDecoration.Interpolation i.paramInterpolation]
                let dec = if n = "Depth" then Set.union depthWriteMode dec else dec
                { 
                    paramName = n
                    paramSemantic = n
                    paramType = i.paramType
                    paramDecorations = dec
                }
            )

        let uniforms =
            s.shaderUniforms |> Map.toList |> List.map (fun (n, u) ->
                let uniformBuffer = 
                    match u.uniformValue with
                        | Attribute(scope, name) -> Some scope.FullName
                        | _ -> None

                let textureInfos =
                    match u.uniformValue with
                        | UniformValue.Sampler (n,s) -> [n,s :> obj]
                        | UniformValue.SamplerArray arr -> Array.toList arr |> List.map (fun (n,s) -> n, s :> obj)
                        | _ -> []

                {
                    uniformName = u.uniformName
                    uniformType = u.uniformType
                    uniformBuffer = uniformBuffer
                    uniformDecorations = u.decorations
                    uniformTextureInfo = textureInfos
                }
            )

        let prevStage = prev |> Option.map stage
        let nextStage = next |> Option.map stage

        let body =
            if hasSourceVertexIndex then
                s.shaderBody.SubstituteReads(fun kind typ name index slot ->
                    match kind, index with
                        | ParameterKind.Input, Some i when name = Intrinsics.SourceVertexIndex ->
                            Some i
                        | _ ->
                            None
                
                )
            else
                s.shaderBody
  
        {
            conditional    = Some (string s.shaderStage)
            entryName      = "main"
            inputs         = inputs
            outputs        = outputs
            uniforms       = uniforms
            raytracingData = []
            arguments      = []
            body           = body
            decorations = 
                [
                    yield EntryDecoration.Stages <| ShaderStageDescription.Graphics { 
                        prev = prevStage
                        self = s.shaderStage
                        next = nextStage 
                    }

                    yield EntryDecoration.Invocations s.shaderInvocations

                    match s.shaderInputTopology with
                        | Some t -> yield EntryDecoration.InputTopology t
                        | None -> ()

                    match s.shaderOutputTopology with
                        | Some t -> yield EntryDecoration.OutputTopology t
                        | _ -> ()

                    match s.shaderOutputVertices with
                        | ShaderOutputVertices.Unknown -> ()
                        | ShaderOutputVertices.Computed v | ShaderOutputVertices.UserGiven v ->
                            yield EntryDecoration.OutputVertices v
                ]
        }

    /// creates a shader "passing-thru" all supplied attributes
    let passing (attributes : Map<string, Type>) (stage : ShaderStage) =
        match stage with
            | ShaderStage.Vertex | ShaderStage.Fragment ->
                let parameters = attributes |> Map.map (fun _ -> ParameterDescription.ofType)
                {
                    shaderStage             = stage
                    shaderInputs            = parameters
                    shaderOutputs           = parameters
                    shaderUniforms          = Map.empty
                    shaderInputTopology     = None
                    shaderOutputTopology    = None
                    shaderOutputVertices    = ShaderOutputVertices.Unknown
                    shaderOutputPrimitives  = None
                    shaderInvocations       = 1
                    shaderBody =
                        attributes
                            |> Map.map (fun n t -> None, Expr.ReadInput(ParameterKind.Input, t, n))
                            |> Expr.WriteOutputs
                    shaderDebugRange        = None
                    shaderDepthWriteMode    = DepthWriteMode.None
                    shaderPayloads          = Map.empty
                    shaderPayloadIn         = None
                    shaderCallableData      = Map.empty
                    shaderCallableDataIn    = None
                    shaderHitAttribute      = None
                    shaderRayTypes          = Set.empty
                    shaderMissShaders       = Set.empty
                    shaderCallableShaders   = Set.empty
                }
            | _ ->
                failwith "[FShade] not implemented"
    
    /// creates a shader (for the given stage) with no in-/outputs
    let empty (stage : ShaderStage) = 
        passing Map.empty stage

    /// gets the needed inputs for the given shader
    let neededInputs (shader : Shader) =
        let builtIn = builtInInputs.[shader.shaderStage]
        let inputs = 
            match shader.shaderStage with
                | ShaderStage.Fragment ->
                    let inputs =
                        shader.shaderInputs
                            |> Map.map (fun _ p -> p.paramType)

                    match Map.tryFind Intrinsics.Position inputs with
                        | Some p ->
                            inputs |> Map.add Intrinsics.FragmentPosition typeof<V4d>
                        | None ->
                            inputs |> Map.add Intrinsics.Position typeof<V4d>

                | ShaderStage.Geometry | ShaderStage.TessControl ->
                    shader.shaderInputs
                        |> Map.map (fun _ p ->
                            match p.paramType with
                                | ArrOf(_,t) | ArrayOf t -> t
                                | t -> t
                        )

                | _ ->
                    shader.shaderInputs
                        |> Map.map (fun _ p -> p.paramType)

        Map.difference inputs builtIn    
        
    /// gets the provided outputs for the given shader
    let outputs (shader : Shader) =
        match shader.shaderStage with
            | ShaderStage.Fragment ->
                shader.shaderOutputs
                    |> Map.map (fun _ p -> p.paramType)
                    |> Map.add Intrinsics.Depth typeof<float>
            | _ ->
                shader.shaderOutputs
                    |> Map.map (fun _ p -> p.paramType)
        

    module internal Composition = 
        let simple (l : Shader) (r : Shader) =
            let needed  = Map.intersect l.shaderOutputs r.shaderInputs
            let passed  = Map.difference l.shaderOutputs r.shaderOutputs

            let depthWrite =
                if r.shaderDepthWriteMode <> DepthWriteMode.None then r.shaderDepthWriteMode
                else l.shaderDepthWriteMode

            let lBody =
                l.shaderBody.SubstituteWrites (fun values ->
                    // cannot compose to TessControl Shader
                    let values = values |> Map.map (fun _ (_,v) -> v)

                    let variables =
                        needed |> Map.map (fun name (lv, rv) -> 
                            let variable = Var(name + "C", rv.paramType)
                            let converter = converter name lv.paramType rv.paramType
                            variable, converter
                        )

                    let rBody =
                        r.shaderBody
                            |> Expr.substituteReads (fun kind t name idx slot ->
                                match kind with
                                    | ParameterKind.Input -> 
                                        match Map.tryFind name variables with
                                            | Some(v,_) -> Expr.Var v |> Some
                                            | _ -> None
                                    | _ ->
                                        None
                            )

                    let rBody = 
                        variables |> Map.fold (fun b name (var, convert) ->
                            Expr.Let(var, convert (Map.find name values), b)
                        ) rBody

                    let passedValues =
                        passed |> Map.map (fun name p ->
                            None, Map.find name values
                        )

                    if Map.isEmpty passedValues then
                        rBody |> Some
                    else
                        rBody
                            |> Expr.substituteWrites (fun rValues ->
                                Map.union passedValues rValues
                                    |> Expr.WriteOutputs
                                    |> Some
                            )
                            |> Some
                )

            optimize 
                { l with
                    shaderInputs = Map.union r.shaderInputs l.shaderInputs
                    shaderOutputs = Map.union l.shaderOutputs r.shaderOutputs
                    shaderUniforms = Map.union l.shaderUniforms r.shaderUniforms
                    shaderBody = lBody
                    shaderDebugRange = None
                    shaderDepthWriteMode = depthWrite
                }

        let gsvs (lShader : Shader) (rShader : Shader) =
            // matched values (left-out <-> right-in)
            let needed  = Map.intersect lShader.shaderOutputs rShader.shaderInputs

            // pass-thru (left-out <-> not right-out)
            let passed  = Map.difference lShader.shaderOutputs rShader.shaderOutputs

            // unknown (right-in <-> not left-out)
            let unknown = Map.difference rShader.shaderInputs lShader.shaderOutputs |> Map.keys |> Set.ofSeq

            // prepare a set containing all valid outputs from right (which will be mutated in the substitution)
            let mutable finalOutputsFromRight = rShader.shaderOutputs |> Map.keys |> Set.ofSeq
            
            // modify the left-shader body including the computations from the right one
            let lBody =
                lShader.shaderBody.SubstituteWrites (fun values ->
                    let variables =
                        needed |> Map.map (fun name (lv, rv) -> 
                            let variable = Var(name + "C", rv.paramType)
                            let converter = converter name lv.paramType rv.paramType
                            variable, converter
                        )

                    // try to find the unique vertex-index for this write
                    let vertexIndex = tryGetSourceVertexIndex lShader values

                    // preprocess the right shader
                    let rShader =
                        match vertexIndex with
                            | Some _ -> rShader
                            | None when Set.isEmpty unknown -> rShader
                            | None ->
                                // all outputs affected by unknown inputs cannot be written by the composed shader
                                let invalidOutputs = 
                                    let affected = Expr.getAffectedOutputsMap rShader.shaderBody
                                    unknown 
                                        |> Seq.map (fun u -> match Map.tryFind u affected with | Some a -> a | _ -> Set.empty) 
                                        |> Set.unionMany

                                // remove all invalid outputs from the final ones
                                finalOutputsFromRight <- Set.difference finalOutputsFromRight invalidOutputs

                                // remove all invalid outputs from the right shader
                                rShader |> removeOutputs invalidOutputs
                            
                    // wrap the right shader with let-bindings for all used inputs
                    let rBody = 
                        rShader.shaderBody
                            |> Expr.substituteReads (fun kind t name idx slot ->
                                match kind with
                                    | ParameterKind.Input -> 
                                        match Map.tryFind name variables with
                                            | Some(v,_) -> 
                                                Expr.Var v |> Some
                                            | None -> 
                                                match vertexIndex with
                                                    | Some vertexIndex -> 
                                                        match idx with
                                                            | None -> Some (Expr.ReadInput(kind, t, name, vertexIndex, slot))
                                                            | Some i -> failwithf "[FShade] vertex shader reading indexed input %A not supported" name
                                                    | None ->
                                                        failwithf "[FShade] internal error in gsvs"
                                    | _ -> 
                                        None
                            )
                             
                    // cannot compose to TessControl Shader
                    let values = values |> Map.map (fun _ (_,v) -> v)

                    let rBody = 
                        variables |> Map.fold (fun b name (var, convert) ->
                            Expr.Let(var, convert (Map.find name values), b)
                        ) rBody

        
                    // if no outputs from left are "passed" then simply inline the right-shader
                    if Map.isEmpty passed then
                        rBody |> Some
                    
                    // otherwise modify it to include the "passed" outputs
                    else
                       
                        let passedValues = 
                            passed |> Map.map (fun name p -> None, Map.find name values)

                        rBody
                            |> Expr.substituteWrites (fun rValues ->
                                Map.union passedValues rValues
                                    |> Expr.WriteOutputs
                                    |> Some
                               )
                            |> Some
                )

            // determine the inputs used by the right-shader (may not be used)
            let rInputs = 
                rShader.shaderInputs |> Map.choose (fun name p ->
                    if Set.contains name unknown then
                        Some { p with paramType = p.paramType.MakeArrayType() }
                    else
                        None
                )

            // determine the outputs for the inlined right-shader
            let rOutputs =
                rShader.shaderOutputs |> Map.filter (fun name p -> Set.contains name finalOutputsFromRight)

            // return the new shader (possibly including unused inputs)
            // and optimize it (removing those)
            optimize 
                { lShader with
                    shaderInputs = Map.union rInputs lShader.shaderInputs
                    shaderOutputs = Map.union lShader.shaderOutputs rOutputs
                    shaderUniforms = Map.union lShader.shaderUniforms rShader.shaderUniforms
                    shaderBody = lBody
                    shaderDebugRange = None
                }

        [<AutoOpen>]
        module private GSCompositionHelpers =
            let topologyCompatible (lOutput : Option<OutputTopology>) (rInput : Option<InputTopology>) =
                match lOutput, rInput with
                    | Some OutputTopology.Points, Some InputTopology.Point
                    | Some OutputTopology.LineStrip, Some InputTopology.Line 
                    | Some OutputTopology.TriangleStrip, Some InputTopology.Triangle ->
                        true
                    | _ -> 
                        false

            [<ReflectedDefinition>]
            type TriangleStream<'d when 'd :> INatural> =
                {
                    indices                     : Arr<'d, int>
                    mutable count               : int

                    mutable p0                  : int
                    mutable p1                  : int
                    mutable p2                  : int
                    mutable vs                  : int
                }

                member x.EmitVertex(vi : int) =
                    match x.vs with
                        | 0 -> 
                            x.p0 <- vi
                            x.vs <- 1
                        | 1 ->
                            x.p1 <- vi
                            x.vs <- 2
                        | vs ->
                            x.p2 <- vi
                            x.vs <- vs + 1

                            let bi = 3 * x.count
                            x.indices.[bi + 0] <- x.p0
                            x.indices.[bi + 1] <- x.p1
                            x.indices.[bi + 2] <- x.p2
                        
                            let indexInStrip = vs - 3

                            // 0 1 2   2 1 3   2 3 4   4 3 5   4 5 6
                            if indexInStrip % 2 = 0 then x.p0 <- x.p2
                            else x.p1 <- x.p2
                            x.count <- x.count + 1
                            x.p2 <- -1
                        


                [<Inline>]
                member x.Reset() =
                    x.count <- 0
                    x.vs <- 0
                    x.p0 <- -1
                    x.p1 <- -1
                    x.p2 <- -1
                    for i in 0 .. Peano.typeSize<'d> - 1 do
                        x.indices.[i] <- -1

                [<Inline>]
                member x.EndPrimitive() =
                    x.vs <- 0
                    x.p0 <- -1
                    x.p1 <- -1
                    x.p2 <- -1
                
                [<Inline>]
                member x.GetIndex(pi : int, fvi : int) =
                    x.indices.[3 * pi + fvi]
                
            [<ReflectedDefinition>]
            type LineStream<'d when 'd :> INatural> =
                {
                    indices                     : Arr<'d, int>
                    mutable count               : int

                    mutable p0                  : int
                    mutable p1                  : int
                    mutable vs                  : int
                }

                [<Inline>]
                member x.Reset() =
                    x.count <- 0
                    x.vs <- 0
                    x.p0 <- -1
                    x.p1 <- -1
                    for i in 0 .. Peano.typeSize<'d> - 1 do
                        x.indices.[i] <- -1

                member x.EmitVertex(vi : int) =
                    match x.vs with
                        | 0 -> 
                            x.p0 <- vi
                            x.vs <- 1
                        | 1 ->
                            x.p1 <- vi
                            x.vs <- 2

                            let bi = 2 * x.count
                            x.indices.[bi + 0] <- x.p0
                            x.indices.[bi + 1] <- x.p1
                        
                            x.p0 <- x.p1
                            x.vs <- 1
                        
                            x.count <- x.count + 1

                        | _ ->
                            ()

                [<Inline>]
                member x.EndPrimitive() =
                    x.vs <- 0
                  
                [<Inline>]
                member x.GetIndex(pi : int, fvi : int) =
                    x.indices.[2 * pi + fvi]
                              
            [<ReflectedDefinition>]
            type PointStream<'d when 'd :> INatural> =
                {
                    indices                     : Arr<'d, int>
                    mutable count               : int
                }

                [<Inline>]
                member x.Reset() =
                    x.count <- 0
                    for i in 0 .. Peano.typeSize<'d> - 1 do
                        x.indices.[i] <- -1

                member x.EmitVertex(vi : int) =
                    x.indices.[x.count] <- vi
                    x.count <- x.count + 1
              
                [<Inline>]
                member x.EndPrimitive() =
                    ()
                  
                [<Inline>]
                member x.GetIndex(pi : int, fvi : int) =
                    x.indices.[pi]
                
            type ReflectedStream(t : OutputTopology, maxPrimitives : int) =
                
                let maxIndices =
                    match t with
                        | OutputTopology.Points -> maxPrimitives
                        | OutputTopology.LineStrip -> maxPrimitives * 2
                        | OutputTopology.TriangleStrip -> maxPrimitives * 3
                    
                let tStream =
                    match t with
                        | OutputTopology.Points -> typedefof<PointStream<_>>.MakeGenericType [| Peano.getPeanoType maxIndices |]
                        | OutputTopology.LineStrip -> typedefof<LineStream<_>>.MakeGenericType [| Peano.getPeanoType maxIndices |]
                        | OutputTopology.TriangleStrip -> typedefof<TriangleStream<_>>.MakeGenericType [| Peano.getPeanoType maxIndices |]
                        
                let mReset          = tStream.GetMethod("Reset", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                let mEmitVertex     = tStream.GetMethod("EmitVertex", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                let mEndPrimitive   = tStream.GetMethod("EndPrimitive", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                let mGetIndex       = tStream.GetMethod("GetIndex", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                let pCount          = tStream.GetProperty("count", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                

                let stream = Var("stream", tStream, true)

                member x.VerticesPerPrimitive = 
                    match t with
                        | OutputTopology.Points -> 1
                        | OutputTopology.LineStrip -> 2
                        | OutputTopology.TriangleStrip -> 3

                member x.Declare(body : Expr) =
                    Expr.Let(
                        stream, Expr.DefaultValue tStream,
                        Expr.Seq [
                            Expr.Call(Expr.Var stream, mReset, [])
                            body
                        ]
                    )

                member x.EmitVertex(index : Expr<int>) : Expr<unit> =
                    Expr.Call(Expr.Var stream, mEmitVertex, [index]) |> Expr.Cast
                    
                member x.EndPrimitive() : Expr<unit> =
                    Expr.Call(Expr.Var stream, mEndPrimitive, []) |> Expr.Cast

                member x.GetIndex(pi : Expr<int>, fvi : Expr<int>) : Expr<int> =
                    Expr.Call(Expr.Var stream, mGetIndex, [pi.Raw; fvi.Raw]) |> Expr.Cast

                member x.Count : Expr<int> = 
                    Expr.PropertyGet(Expr.Var stream, pCount) |> Expr.Cast 
            
            type Expr with
                static member Lets (vs : seq<Var * Option<Expr>>, body : Expr) =
                    let rec doit (l : list<Var * Option<Expr>>) =
                        match l with
                            | [] -> body
                            | (v,e) :: rest ->
                                let e = match e with | Some e -> e | None -> Expr.DefaultValue(v.Type)
                                Expr.Let(v, e, doit rest)

                    vs |> Seq.toList |> doit

                static member Repeat(n : int, body : Expr -> Expr) =
                    match n with
                        | 0 -> Expr.Unit
                        | 1 -> body (Expr.Value 0)
                        | n -> 
                            let i = Var("i", typeof<int>)
                            Expr.ForIntegerRangeLoop(i, Expr.Value 0, Expr.Value (n - 1), body (Expr.Var i))
                    

        let gsgs (lShader : Shader) (rShader : Shader) =
            if not (topologyCompatible lShader.shaderOutputTopology rShader.shaderInputTopology) then
                failwithf "[FShade] cannot compose geometryshaders with mismatching topologies: %A vs %A" lShader.shaderOutputTopology rShader.shaderInputTopology

            match lShader.shaderOutputVertices with
                | ShaderOutputVertices.Computed count | ShaderOutputVertices.UserGiven count ->

                    let lOutputTopology = lShader.shaderOutputTopology.Value


                    // pass all needed inputs along
                    let lShader = lShader |> withOutputs (rShader.shaderInputs |> Map.map (fun _ d -> d.paramType.GetElementType()))

                    // determine the maximal number of indices needed
                    let maxPrimitives =
                        match lShader.shaderOutputPrimitives with
                            | Some p -> p
                            | None ->
                                match lOutputTopology with
                                    | OutputTopology.Points -> count
                                    | OutputTopology.LineStrip -> count - 1
                                    | OutputTopology.TriangleStrip -> count - 2

                    // introduce variables for all composition semantics
                    let composeVars = 
                        lShader.shaderOutputs |> Map.map (fun name desc ->
                            let arrType = Peano.getArrayType count desc.paramType
                            Var(name, arrType) 
                        )

                    // maintain an index 
                    let currentVertex = Var("currentVertex", typeof<int>, true)
                    
                    let stream = ReflectedStream(lShader.shaderOutputTopology.Value, maxPrimitives)

                    let lBody =
                        lShader.shaderBody.SubstituteWrites (fun outputs ->
                            let result =
                                Expr.Seq [
                                    for (name, (i,o)) in Map.toSeq outputs do
                                        if Option.isSome i then failwithf "[FShade] indexed output-write not possible atm."

                                        let v = composeVars.[name]
                                        yield Expr.ArraySet(Expr.Var v, Expr.Var currentVertex, o)
                                ]

                            Some result
                        )

                    let lBody =
                        let rec replace (e : Expr) =
                            match e with
                                | SpecificCall <@ restartStrip @> _
                                | SpecificCall <@ endPrimitive @> _ ->
                                    stream.EndPrimitive() :> Expr

                                | SpecificCall <@ emitVertex @> _ ->
                                    Expr.Seq [
                                        stream.EmitVertex(Expr.Var(currentVertex) |> Expr.Cast) :> Expr
                                        Expr.VarSet(currentVertex, <@ (%%(Expr.Var(currentVertex)) : int) + 1 @>)
                                    ]

                                | ShapeLambda(v,b) -> Expr.Lambda(v, replace b)
                                | ShapeCombination(o, args) -> RebuildShapeCombination(o, args |> List.map replace)
                                | ShapeVar v -> Expr.Var v

                        replace lBody

                    let rec bind (l : list<Var * Option<Expr>>) (body : Expr) =
                        match l with
                            | [] -> body
                            | (v,e) :: rest ->
                                match e with
                                    | Some e -> Expr.Let(v, e, bind rest body)
                                    | None -> Expr.Let(v, Expr.DefaultValue v.Type, bind rest body) 

                    let variables =
                        [
                            for v in Map.values composeVars do
                                yield v, None
                            yield currentVertex, Some (Expr.Value 0)
                        ]   

                    let rInputCount = stream.VerticesPerPrimitive
                    let rPrimitiveId = Var("primitiveId", typeof<int>)
                    let rIndices = Var("indices", Peano.getArrayType rInputCount typeof<int>)

                    let rBody =
                        let rBody =
                            rShader.shaderBody.SubstituteReads (fun kind typ name index slot ->
                                match kind with
                                    | ParameterKind.Input ->
                                        match index with
                                            | Some index ->
                                                let index = Expr.ArrayAccess(Expr.Var rIndices, index) 
                                                let v = composeVars.[name]
                                                Some (Expr.ArrayAccess(Expr.Var v, index))

                                            | None ->
                                                None
                                                //failwith "[FShade] GeometryShader cannot read non-indexed input"
                                    | _ ->
                                        None
                            )


                        let getIndex (pi : Expr) (i : int) = stream.GetIndex(Expr.Cast pi, <@ i @>)

                        Expr.ForIntegerRangeLoop(
                            rPrimitiveId, Expr.Value 0, <@@ %(stream.Count) - 1 @@>, 
                            Expr.Let(rIndices, Expr.DefaultValue rIndices.Type,
                                Expr.Seq [
                                    for i in 0 .. rInputCount - 1 do
                                        yield Expr.ArraySet(Expr.Var rIndices, Expr.Value i, getIndex (Expr.Var rPrimitiveId) i)
                                    yield rBody
                                    yield <@@ restartStrip() @@>
                                ]
                            )
                        )


                    let body =
                        bind variables (
                            stream.Declare (
                                Expr.Seq [
                                    lBody
                                    rBody
                                ]
                            )
                        )

                    let outputVerices =
                        match rShader.shaderOutputVertices with
                            | ShaderOutputVertices.UserGiven rc | ShaderOutputVertices.Computed rc ->
                                ShaderOutputVertices.UserGiven (maxPrimitives * rc)

                            | _ ->
                                ShaderOutputVertices.Unknown

                    let outputPrimitives =
                        match rShader.shaderOutputPrimitives with
                            | Some rp -> Some (maxPrimitives * rp)
                            | None -> None

                    optimize 
                        { rShader with
                            shaderInvocations = lShader.shaderInvocations * rShader.shaderInvocations
                            shaderBody = Preprocessor.preprocess V3i.Zero body |> fst
                            shaderInputTopology = lShader.shaderInputTopology
                            shaderUniforms = Map.union lShader.shaderUniforms rShader.shaderUniforms
                            shaderInputs = lShader.shaderInputs
                            shaderOutputVertices = outputVerices
                            shaderOutputPrimitives = outputPrimitives
                        }


                | _ ->
                    failwithf "[FShade] cannot compose GeometryShader without vertex-count"

        let vsgs (lShader : Shader) (rShader : Shader) =
            
            let inputVertices =
                match rShader.shaderInputTopology.Value with
                    | InputTopology.Point -> 1
                    | InputTopology.Line -> 2
                    | InputTopology.Triangle -> 3
                    | InputTopology.LineAdjacency -> 4
                    | InputTopology.TriangleAdjacency -> 5
                    | InputTopology.Patch n -> n

            let vars =
                lShader.shaderOutputs |> Map.remove Intrinsics.SourceVertexIndex |> Map.map (fun name desc ->
                    if inputVertices = 1 then
                        Var(name, desc.paramType, true)
                    else
                        Var(name, Peano.getArrayType inputVertices desc.paramType)
                )

            let write (name : string) (index : Expr) (value : Expr) =
                let v = vars.[name]

                if v.IsMutable && v.Type = value.Type then
                    assert (match index with | Int32 0 -> true | _ -> false)
                    Expr.VarSet(v, value)

                else
                    Expr.ArraySet(Expr.Var v, index, value)
                    
            let tryRead (name : string) (index : Expr) =
                match Map.tryFind name vars with
                    | Some v ->
                        match v.Type with
                            | ArrOf _ | ArrayOf _ -> 
                                Some (Expr.ArrayAccess(Expr.Var v, index))
                            | _ -> 
                                assert ( match index with | Int32 0 -> true | _ -> false)
                                Some (Expr.Var v)
                    | None ->
                        None

            let rBody = 
                rShader.shaderBody.SubstituteReads (fun kind typ name index slot ->
                    match kind, index with
                        | ParameterKind.Input, Some index ->
                            tryRead name index
                        | _ ->
                            None
                )

            let rBody =
                Expr.Lets(
                    vars |> Map.values |> Seq.map (fun v -> v, None),
                    Expr.Seq [
                        Expr.Repeat(inputVertices, fun index -> 
                            let body = lShader.shaderBody

                            let body =
                                body.SubstituteReads(fun kind typ name ii slot ->
                                    assert (Option.isNone ii)
                                    match kind with
                                        | ParameterKind.Input ->
                                            Expr.ReadInput(kind, typ, name, index, slot) |> Some
                                        | _ ->
                                            None
                                )

                            let body = 
                                body.SubstituteWrites (fun outputs ->
                                    let values = outputs |> Map.remove Intrinsics.SourceVertexIndex |> Map.toList

                                    let writes = 
                                        values |> List.map (fun (name, (idx,value)) ->
                                            assert (Option.isNone idx)
                                            write name index value
                                        )

                                    Some (Expr.Seq writes)
                                )

                            body
                        )

                        rBody
                    ]
                )


            withBody rBody { rShader with shaderUniforms = Map.union lShader.shaderUniforms rShader.shaderUniforms }

    /// composes two shaders respecting their stages.
    ///     - implemented: { Vertex->Vertex; Geometry->Vertex; Fragment->Fragment }
    ///     - future:           { Tessellation->Vertex; Geometry->Geometry }
    ///     - impossible/hard:  { Tessellation->Tessellation; Geometry->Tessellation }
    ///     - effect:           { Vertex->Tessellation; Vertex->Geometry; Vertex->Fragment; Tessellation->Geometry; Tessellation->Fragment; Geometry->Fragment }
    let compose2 (l : Shader) (r : Shader) =
        match l.shaderStage, r.shaderStage with

            // simple case: both are vertex/fragment
            | ShaderStage.Vertex, ShaderStage.Vertex
            | ShaderStage.Fragment, ShaderStage.Fragment ->
                Composition.simple l r

            // harder case: left is geometry and right is vertex
            | ShaderStage.Geometry, ShaderStage.Vertex ->
                Composition.gsvs l r

            | ShaderStage.Geometry, ShaderStage.Geometry ->
                Composition.gsgs l r

            | ShaderStage.Vertex, ShaderStage.Geometry ->
                Composition.vsgs l r

            | _ ->
                failwithf "[FShade] cannot compose %AShader with %AShader" l.shaderStage r.shaderStage

    /// composes many shaders respecting their stages.
    /// NOTE: the sequence cannot be empty
    let compose (l : #seq<Shader>) =
        use e = l.GetEnumerator()
        if e.MoveNext() then
            let mutable res = e.Current
            while e.MoveNext() do
                res <- compose2 res e.Current
            res
        else
            failwith "[FShade] cannot compose empty shader-sequence"      

