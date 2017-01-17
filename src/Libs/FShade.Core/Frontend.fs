namespace FShade

open System
open System.Reflection

open Aardvark.Base

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type SemanticAttribute(s : string) =
    inherit Attribute()
    member x.Semantic = s

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type InterpolationAttribute(qualifier : InterpolationMode) =
    inherit Attribute()
    member x.Qualifier = qualifier

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type PrimitiveIndexAttribute(index : int) =
    inherit Attribute()
    member x.Index = index

type IShaderBuilder =
    abstract member ShaderType : ShaderType
    abstract member OutputTopology : Option<OutputTopology>

type IOParameter =
    {
        paramSemantic       : string
        paramType           : Type
        paramInterpolation  : InterpolationMode
    }

type UniformValue =
    | Attribute of scope : UniformScope * name : string
    | Sampler of textureName : string * SamplerState
    | SamplerArray of array<string * SamplerState>

type UniformParameter =
    {
        uniformName         : string
        uniformType         : Type
        uniformValue        : UniformValue
    }

[<AutoOpen>]
module ParameterExtensions = 
    type System.Reflection.MemberInfo with
        member x.Type =
            match x with
                | :? PropertyInfo as p -> p.PropertyType
                | :? FieldInfo as f -> f.FieldType
                | _ -> failwith "no member type could be determined"

        member x.Semantic =
            let att = x.GetCustomAttributes<SemanticAttribute>(true) |> Seq.toList
            match att with
                | x::_ -> x.Semantic
                | _ -> x.Name

        member x.Interpolation =
            let att = x.GetCustomAttributes<InterpolationAttribute>(true) |> Seq.toList
            match att with
                | x::_ -> x.Qualifier
                | _ -> InterpolationMode.Default

        member x.PrimitiveIndex =
            let att = x.GetCustomAttributes<PrimitiveIndexAttribute>(true) |> Seq.toList
            match att with
                | x::_ -> Some x.Index
                | _ -> None

[<AutoOpen>]
module BasicQuotationPatterns =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns

    module private Peano = 
        let private peanoTypes =
            let s = typedefof<S<_>>
            Seq.initInfinite id 
                |> Seq.scan (fun last _ -> s.MakeGenericType [|last|]) typeof<Z>
                |> Seq.cache

        let getPeanoType (i : int) =
            Seq.item i peanoTypes

        let getArrayType (i : int) (content : Type) =
            typedefof<Arr<_,_>>.MakeGenericType [| getPeanoType i; content |]
           


    //extracts the (optional) top-most method call from an expression
    let rec tryGetMethodInfo (e : Expr) =
        match e with
            | Patterns.Call(_,mi,_) -> 
                if mi.IsGenericMethod then mi.GetGenericMethodDefinition() |> Some
                else mi |> Some
            | ExprShape.ShapeCombination(_, args) -> 
                args |> List.tryPick tryGetMethodInfo
            | ExprShape.ShapeLambda(_,b) ->
                tryGetMethodInfo b
            | _ -> None


    /// <summary>
    /// extracts the top-most method-call from an expression.
    /// When no method-call is found the method will raise an exception
    /// </summary>
    /// <param name="e"></param>
    let getMethodInfo (e : Expr) =
        match tryGetMethodInfo e with
            | Some mi -> mi
            | None -> failwith "[FShade] could not find a method-call in expression"


    let private getArray = getMethodInfo <@ Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetArray @>

    /// <summary>
    /// Some missing Constructors and functions for F#'s Expr-type
    /// </summary>
    type Expr with
        /// <summary>
        /// Creates an array-indexing expression using the supplied arguments
        /// </summary>
        static member ArrayAccess(arr : Expr, index : Expr) =
            let get = getArray.MakeGenericMethod([| arr.Type.GetElementType() |])
            Expr.Call(get, [arr;index])

        /// <summary>
        /// Creates a ForEach-Expression using the standard layout as used by F#-quotations
        /// </summary>
        static member ForEach(v : Var, seq : Expr, body : Expr) =
            let sType = typeof<System.Collections.Generic.IEnumerable<obj>>.GetGenericTypeDefinition().MakeGenericType([|v.Type|])
            let eType = typeof<System.Collections.Generic.IEnumerator<obj>>.GetGenericTypeDefinition().MakeGenericType([|v.Type|])
            let e = Var("enumerator", eType)

            let intrinsics = getArray.DeclaringType
            let unboxDisposable = intrinsics.GetMethod("UnboxGeneric").MakeGenericMethod([|typeof<IDisposable>|])

            let getEnumerator = sType.GetMethod("GetEnumerator")
            let dispose = typeof<IDisposable>.GetMethod("Dispose")
            let moveNext = typeof<System.Collections.IEnumerator>.GetMethod("MoveNext")

            Expr.Let(e, Expr.Call(Expr.Coerce(seq, sType), getEnumerator, []),
                Expr.TryFinally(
                    Expr.WhileLoop(Expr.Call(Expr.Var e, moveNext, []),
                        Expr.Let(v, Expr.PropertyGet(Expr.Var e, eType.GetProperty("Current"), []),
                            body
                        )
                    ),
                    Expr.IfThenElse(Expr.TypeTest(Expr.Coerce(Expr.Var e, typeof<obj>), typeof<IDisposable>),
                        Expr.Call(Expr.Call(unboxDisposable, [Expr.Coerce(Expr.Var e, typeof<obj>)]), dispose, []),
                        Expr.Value(())
                    )
                )
            )

        /// <summary>
        /// Tries to evaluate an expression and returns its value when possible.
        /// Note that this implementation may be incomplete.
        /// </summary>
        static member tryEval (e : Expr) : Option<obj> =

            let reduce (a : Option<obj>[]) =
                match a |> Array.forall(fun o -> o.IsSome) with
                    | true -> a |> Array.map (fun o -> o.Value) |> Some
                    | false -> None

            match e with
                | Patterns.PropertyGet(t , p, args) ->
                    match t with
                        | Some(t) -> 
                            let args = args |> List.map Expr.tryEval |> List.toArray
                            match Expr.tryEval t, args |> reduce with
                                | Some t, Some args -> p.GetValue(t, args) |> Some
                                | _ -> None
                        | None -> 
                            let args = args |> List.map Expr.tryEval |> List.toArray
                            match args |> reduce with
                                | Some args -> p.GetValue(null, args) |> Some
                                | _ -> None

                | Patterns.FieldGet(t,f) ->
                    match t with
                        | Some(t) -> 
                            match Expr.tryEval t with
                                | Some t -> f.GetValue(t) |> Some
                                | None -> None
                        | None -> 
                            f.GetValue(null) |> Some
                | Patterns.Call(t, mi, args) ->
                    match t with
                        | Some(t) -> 
                            let args = args |> List.map Expr.tryEval |> List.toArray
                            let t = Expr.tryEval t
                            match t, args |> reduce with
                                | Some t, Some args -> mi.Invoke(t, args) |> Some
                                | _ -> None

                        | None -> 
                            let args = args |> List.map Expr.tryEval |> List.toArray
                            match args |> reduce with
                                | Some args -> mi.Invoke(null, args) |> Some
                                | _ -> None

                | Patterns.Let(var,value,body) ->
                    let value = Expr.tryEval value
                    match value with
                        | Some value ->
                            let body = body.Substitute(fun vi -> if vi = var then Expr.Value(value, vi.Type) |> Some else None)
                            Expr.tryEval body
                        | None -> None

                | Patterns.Value(v,_) ->
                    v |> Some
                | _ -> None

    let (|BuilderCall|_|) (e : Expr) =
        match e with
            | Call(Some t, mi, args) when typeof<IShaderBuilder>.IsAssignableFrom t.Type ->
                BuilderCall(t, mi, args) |> Some
            | _ -> None

    let (|UniformScopeType|_|) (t : Type) =
        if t = typeof<UniformScope> then UniformScopeType |> Some
        else None


    let (|ExprOf|) (e : Expr) =
        ExprOf(e.Type)

    let (|MemberFieldGet|_|) (e : Expr) =
        match e with
            | PropertyGet(Some t,p,[]) ->
                MemberFieldGet(t, p :> MemberInfo) |> Some
            | FieldGet(Some t, f) ->
                MemberFieldGet(t, f :> MemberInfo) |> Some
            | _ -> None

    let (|ArrayOf|_|) (t : Type) =
        if t.IsArray then Some(t.GetElementType())
        else None


    let (|ArrOf|_|) (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Arr<_,_>> then Some (t.GetGenericArguments().[1])
        else None

    let (|Uniform|_|) (e : Expr) =
        match e with

            | PropertyGet(None, pi, []) ->
                match pi.Type with
                    | SamplerType(_) ->
                        match Expr.tryEval e with
                            | Some sam ->
                                let sam = sam |> unbox<ISampler>
                                let tex = sam.Texture
                                Some { 
                                    uniformName = pi.Name
                                    uniformType = sam.GetType()
                                    uniformValue = Sampler(tex.Semantic, sam.State) 
                                } 

                            | None ->
                                None

                    | ArrayOf((SamplerType _ as t)) ->
                        match Expr.tryEval e with
                            | Some sam ->
                                let arr = sam |> unbox<Array>
                                let samplers = 
                                    List.init arr.Length (fun i -> 
                                        let sam1 = arr.GetValue i |> unbox<ISampler>
                                        let tex = sam1.Texture
                                        tex.Semantic, sam1.State
                                    )

                                let t = Peano.getArrayType arr.Length t
                                Some {
                                    uniformName = pi.Name
                                    uniformType = t
                                    uniformValue = SamplerArray(List.toArray samplers)
                                } 

                            | None -> 
                                None

                    | _ -> None

            | Call(None, Method("op_Dynamic", [UniformScopeType; _]), [scope; Value(s,_)]) ->
                match Expr.tryEval scope with
                    | Some scope ->
                        let scope = scope |> unbox
                        Some {
                            uniformName = unbox s
                            uniformType = e.Type
                            uniformValue = Attribute(scope, unbox s)
                        }

                    | None ->
                        None

            | PropertyGet(Some scope, p, []) when scope.Type = typeof<UniformScope> ->
                try
                    match Expr.tryEval scope with
                        | Some scope ->
                            let result = p.GetValue(scope, [||])
                            match result with
                                | :? ISemanticValue as v ->
                                    Some {
                                        uniformName = v.Semantic
                                        uniformType = v.GetType()
                                        uniformValue = Attribute(v.Scope, v.Semantic)
                                    }
                                | _ ->
                                    None
                        | None ->
                            None
                with :? TargetInvocationException as ex ->
                    match ex.InnerException with
                        | :? SemanticException as s -> 
                            Some {
                                uniformName = s.Semantic
                                uniformType = p.PropertyType
                                uniformValue = Attribute(s.Scope, s.Semantic)
                            }

                        | _ -> 
                            None

            | Call(None, m, [scope]) when scope.Type = typeof<UniformScope> ->
                try
                    match Expr.tryEval scope with
                        | Some scope ->
                            let result = m.Invoke(null, [| scope |])
                            match result with
                                | :? ISemanticValue as v ->
                                    Some {
                                        uniformName = v.Semantic
                                        uniformType = v.GetType()
                                        uniformValue = Attribute(v.Scope, v.Semantic)
                                    }
                                | _ ->
                                    None

                        | None ->
                            None
                with :? TargetInvocationException as ex ->
                    match ex.InnerException with
                        | :? SemanticException as s -> 
                            Some {
                                uniformName = s.Semantic
                                uniformType = m.ReturnType
                                uniformValue = Attribute(s.Scope, s.Semantic)
                            }
                        | _ -> 
                            None

            | _ -> None




module Intrinsics =
    
    [<Literal>] 
    let Position = "Positions"

    [<Literal>] 
    let FragCoord = "FragCoord"


    [<Literal>] 
    let VertexId = "VertexId"
    [<Literal>] 
    let InstanceId = "InstanceId"
    [<Literal>] 
    let PointSize = "PointSize"
    [<Literal>] 
    let ClipDistance = "ClipDistance"

    [<Literal>] 
    let PatchVertices = "PatchVertices"
    [<Literal>] 
    let PrimitiveId = "PrimitiveId"
    [<Literal>] 
    let InvocationId = "InvocationId"
    [<Literal>] 
    let TessCoord = "TessCoord"

    [<Literal>] 
    let Depth = "Depth"
    [<Literal>] 
    let Color = "Colors"
    [<Literal>] 
    let SecondaryColor = "Color2"

    [<Literal>] 
    let TessLevelInner = "TessLevelInner"
    [<Literal>] 
    let TessLevelOuter = "TessLevelOuter"

    [<Literal>] 
    let FrontFacing = "FrontFacing"
    [<Literal>] 
    let PointCoord = "PointCoord"
    [<Literal>] 
    let SampleId = "SampleId"
    [<Literal>] 
    let SamplePosition = "SamplePosition"
    [<Literal>] 
    let SampleMask = "SampleMask"

    [<Literal>] 
    let Layer = "Layer"
    [<Literal>] 
    let ViewportIndex = "ViewportIndex"

[<AutoOpen>]
module InstrinsicAttributes =
    type PositionAttribute() = inherit SemanticAttribute(Intrinsics.Position)
    type FragCoordAttribute() = inherit SemanticAttribute(Intrinsics.FragCoord)
    type VertexIdAttribute() = inherit SemanticAttribute(Intrinsics.VertexId)
    type InstanceIdAttribute() = inherit SemanticAttribute(Intrinsics.InstanceId)
    type PointSizeAttribute() = inherit SemanticAttribute(Intrinsics.PointSize)
    type ClipDistanceAttribute() = inherit SemanticAttribute(Intrinsics.ClipDistance)
    type PatchVerticesAttribute() = inherit SemanticAttribute(Intrinsics.PatchVertices)
    type PrimitiveIdAttribute() = inherit SemanticAttribute(Intrinsics.PrimitiveId)
    type InvocationIdAttribute() = inherit SemanticAttribute(Intrinsics.InvocationId)
    type TessCoordAttribute() = inherit SemanticAttribute(Intrinsics.TessCoord)
    type DepthAttribute() = inherit SemanticAttribute(Intrinsics.Depth)
    type ColorAttribute() = inherit SemanticAttribute(Intrinsics.Color)
    type SecondaryColorAttribute() = inherit SemanticAttribute(Intrinsics.SecondaryColor)
    type TessLevelInnerAttribute() = inherit SemanticAttribute(Intrinsics.TessLevelInner)
    type TessLevelOuterAttribute() = inherit SemanticAttribute(Intrinsics.TessLevelOuter)
    type FrontFacingAttribute() = inherit SemanticAttribute(Intrinsics.FrontFacing)
    type PointCoordAttribute() = inherit SemanticAttribute(Intrinsics.PointCoord)
    type SampleIdAttribute() = inherit SemanticAttribute(Intrinsics.SampleId)
    type SamplePositionAttribute() = inherit SemanticAttribute(Intrinsics.SamplePosition)
    type SampleMaskAttribute() = inherit SemanticAttribute(Intrinsics.SampleMask)
    type LayerAttribute() = inherit SemanticAttribute(Intrinsics.Layer)
    type ViewportIndexAttribute() = inherit SemanticAttribute(Intrinsics.ViewportIndex)



type TessLevels = 
    { 
        [<TessLevelInner>] innerLevel : float[]
        [<TessLevelOuter>] outerLevel : float[] 
    }

[<AutoOpen>]
module Primitives =
    let inline private shaderOnlyAccess() = failwith "[FShade] cannot execute shader-only function"

    type Primitive<'a> = interface end

    type Point<'a> = Point of 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(0)>]
        member x.Value : 'a = shaderOnlyAccess()

        static member VertexCount = 1
        static member InputTopology = InputTopology.Point

    type Line<'a> = Line of 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = shaderOnlyAccess()

        static member VertexCount = 2
        static member InputTopology = InputTopology.Line

    type Triangle<'a> = Triangle of 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = shaderOnlyAccess()

        static member VertexCount = 3
        static member InputTopology = InputTopology.Triangle

    type LineAdjacency<'a> = Triangle of 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()
            
            
        [<PrimitiveIndex(1)>]
        member x.P0 : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(2)>]
        member x.P1 : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(0)>]
        member x.Left : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(3)>]
        member x.Right : 'a = shaderOnlyAccess()

        static member VertexCount = 4
        static member InputTopology = InputTopology.LineAdjacency

    type TriangleAdjacency<'a> = Triangle of 'a * 'a * 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()
            
        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(2)>]
        member x.P1 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(4)>]
        member x.P2 : 'a = shaderOnlyAccess()
        
        [<PrimitiveIndex(1)>]
        member x.N01 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(3)>]
        member x.N12 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(5)>]
        member x.N20 : 'a = shaderOnlyAccess()

        static member VertexCount = 6
        static member InputTopology = InputTopology.TriangleAdjacency



    type Patch3<'a> = Patch3 of 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()

        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = shaderOnlyAccess()


        member x.TessCoord : V3d = shaderOnlyAccess()

        static member VertexCount = 3
        static member InputTopology = InputTopology.Patch 3

    type Patch4<'a> = Patch4 of 'a * 'a * 'a * 'a with
        interface Primitive<'a>

        member x.Item
            with get (i : int) : 'a = shaderOnlyAccess()


        [<PrimitiveIndex(0)>]
        member x.P0 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(1)>]
        member x.P1 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(2)>]
        member x.P2 : 'a = shaderOnlyAccess()
        [<PrimitiveIndex(3)>]
        member x.P3 : 'a = shaderOnlyAccess()

        member x.TessCoord : V3d = shaderOnlyAccess()

        static member VertexCount = 4
        static member InputTopology = InputTopology.Patch 4


    let emitVertex() = ()

    let endPrimitive() = ()
    let restartStrip() = ()

    let inline ddx< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = failwith "_"
    let inline ddy< ^a when ^a : (static member (-) : ^a -> ^a -> ^a) > (v : ^a) : ^a = failwith "_"
    let discard () : unit = failwith "_"


    let getInnerTessLevel (i : int) : float = failwith ""
    let getOuterTessLevel (i : int) : float = failwith ""




[<AutoOpen>]
module ShaderBuilders =
    type BaseBuilder() =
        member x.For(a : Arr<'d, 'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.For(a : seq<'a>, f : 'a -> unit) : unit =
            for i in a do f i

        member x.Combine(l : unit, r : 'a) = r

        member x.Zero() = ()
        member x.Delay f = f()

    type VertexBuilder() =
        inherit BaseBuilder()
        member x.Return(v) = v

        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderType = ShaderType.Vertex
            member x.OutputTopology = None
    type FragmentBuilder() =
        inherit BaseBuilder()
        member x.Return(v) = v
        member x.Quote() = ()

        interface IShaderBuilder with
            member x.ShaderType = ShaderType.Fragment
            member x.OutputTopology = None

    type GeometryBuilder(size : Option<int>, top : OutputTopology) =
        member x.For(a : Arr<'d, 'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            a |> Seq.collect f

        member x.For(a : seq<'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            a |> Seq.collect f

        member x.Size = size

        member x.Yield(v : 'a) : seq<Primitive<'a>> = 
            failwith ""

        member x.For(p : Primitive<'a>, f : 'a -> seq<Primitive<'b>>) : seq<Primitive<'b>> =
            failwith ""

        member x.Delay(f : unit -> 'a) = f()

        member x.Quote() = ()

        member x.Combine(l : seq<Primitive<'a>>, r : seq<Primitive<'a>>) : seq<Primitive<'a>> =
            failwith ""

        member x.Zero() : seq<Primitive<'a>> = 
            Seq.empty

        interface IShaderBuilder with
            member x.ShaderType = ShaderType.Geometry
            member x.OutputTopology = Some top


    
    type TessControlBuilder() =
        inherit BaseBuilder()

        member x.Quote() = ()
        member x.Delay f = f()
        member x.Return(levels : TessLevels) =
            levels

        interface IShaderBuilder with
            member x.ShaderType = ShaderType.TessControl
            member x.OutputTopology = None

    type TessEvalBuilder() =
        inherit BaseBuilder()

        member x.Quote() = ()
        member x.Delay f = f()
        member x.Return(v) = v
        interface IShaderBuilder with
            member x.ShaderType = ShaderType.TessEval
            member x.OutputTopology = None

    let vertex = VertexBuilder()
    let fragment = FragmentBuilder()

    let triangle = GeometryBuilder(None, OutputTopology.TriangleStrip)
    let line = GeometryBuilder(None, OutputTopology.LineStrip)
    let point = GeometryBuilder(None, OutputTopology.Points)

    let _triangle<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.TriangleStrip)
    let _line<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.LineStrip)
    let _point<'d> = GeometryBuilder(Some typeSize<'d>, OutputTopology.Points)


    let tessControl = TessControlBuilder()
    let tessEval = TessEvalBuilder()
