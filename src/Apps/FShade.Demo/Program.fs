// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open Aardvark.Base
open FShade
open FShade.Demo


module Simple =
    open Aardvark.Base
    open FShade
    
    type UniformScope with
        member x.CameraLocation : V3d = x?PerView?CameraLocation
        member x.LightLocation : V3d = x?PerLight?LightLocation

        member x.ModelTrafo : M44d = x?PerModel?ModelTrafo
        member x.ViewProjTrafo : M44d = x?PerView?ViewProjTrafo

    type V = { [<Semantic("Positions")>] p : V4d 
               [<Semantic("World")>] wp : V4d
               [<Semantic("Normals")>] n : V3d
               [<Semantic("Tangents")>] t : V3d
               [<Semantic("BiNormals")>] b : V3d
               [<Semantic("TexCoords")>] tc : V2d
               [<Semantic("Colors")>] color : V4d 
               [<PrimitiveId>] id : int
               [<ClipDistance>] cd : float[] 
              }

    let DiffuseColorTexture =
           sampler2d {
               texture uniform?DiffuseTexture
        }
                
    let NormalMap =
           sampler2d {
               texture uniform?NormalMap
        }

    let trafo(v : V) =
        vertex {
            let world = uniform.ModelTrafo * v.p
            return { v with p = uniform.ViewProjTrafo * world; wp = world }
        }

    let normals(v : V) =
        fragment {
            return V4d(0.5 * (v.n.Normalized + V3d.III), 1.0)
        }
        
    let bump (v : V) =
        fragment {
            let s = 2.0 * NormalMap.Sample(v.tc).XYZ - V3d.III
            let n = s.X * v.t + s.Y * v.b + s.Z * v.n
            return { v with n = n.Normalized }
        }
        
    let white (v : V) =
        fragment {
            let c : V4d = uniform?PerView?Color
            return c
        }
            
    let texture (v : V) =
           fragment {
               return DiffuseColorTexture.Sample(v.tc)
        }
            
    let pointSurface (size : V2d) (p : Point<V>) =
        let sx = size.X
        let sy = size.Y
        triangle {
            let v = p.Value

            match p.Value with
                | { p = pos; n = n } ->

                    let pxyz = pos.XYZ / pos.W
            
                    let p00 = V3d(pxyz + V3d( -sx, -sy, 0.0 ))
                    let p01 = V3d(pxyz + V3d( -sx,  sy, 0.0 ))
                    let p10 = V3d(pxyz + V3d(  sx, -sy, 0.0 ))
                    let p11 = V3d(pxyz + V3d(  sx,  sy, 0.0 ))

                    yield { p.Value with p = uniform.ViewProjTrafo * V4d(p00 * pos.W, pos.W); tc = V2d.OO; cd = [|1.0; 2.0|] } 
                    yield { p.Value with p = uniform.ViewProjTrafo * V4d(p10 * pos.W, pos.W); tc = V2d.IO; cd = [|1.0; 2.0|] }
                    yield { p.Value with p = uniform.ViewProjTrafo * V4d(p01 * pos.W, pos.W); tc = V2d.OI; cd = [|1.0; 2.0|] }
                    yield { p.Value with p = uniform.ViewProjTrafo * V4d(p11 * pos.W, pos.W); tc = V2d.II; cd = [|1.0; 2.0|] }

        }
            
    let light (v : V) =
        fragment {
            let n = v.n.Normalized

            let c = uniform.CameraLocation - v.wp.XYZ |> Vec.normalize
            let l = uniform.LightLocation - v.wp.XYZ |> Vec.normalize
            let r = -Vec.reflect c n |> Vec.normalize

            let d = Vec.dot l n |> clamp 0.0 1.0
            let s = Vec.dot r l |> clamp 0.0 1.0

            return  v.color.XYZ * (0.2 + 0.8 * d) + V3d.III * pow s 64.0
        }    
 
 
module Dead =
    type BillboardVertex =
        {
            [<Position>] position : V4d
            [<Color>] color : V4d
            [<SemanticAttribute("DiffuseColorCoordinate")>] texCoord : V2d
            [<SemanticAttribute("ViewPosition")>] viewPos : V4d
            [<PrimitiveId>] id : int
        }

    type UniformScope with
        member x.ModelViewTrafo : M44d = uniform?PerModel?ModelViewTrafo
        member x.ProjTrafo : M44d = uniform?PerView?ProjTrafo
        member x.UserSelected : bool = uniform?UserSelected

    let BillboardTrafo (v : BillboardVertex) =
        vertex {
            let vp = uniform.ModelViewTrafo * v.position
            let pp = uniform.ProjTrafo * vp
            return {
                position = pp
                texCoord = V2d(0,0)
                color = v.color
                viewPos = vp
                id = 0
            }
        }

    let BillboardGeometry (sizes: V2d) (distanceScalingFactor : float -> float) (v : Point<BillboardVertex>) =
        let targetRange = 5.0

        _triangle<4 N> {
            let s = sizes
            let offsetX = s.X
            let offsetY = s.Y

            let bv = v.Value
            let pos = bv.position
            let vp = bv.viewPos
            let c = bv.color

            let factor =
                let d = abs(vp.Z)
                let e = 
                    if d < targetRange then
                        1.0 - ( d / targetRange )
                    else
                        0.0
                1.0 + e
        
            let offsetX = offsetX * factor
            let offsetY = offsetY * factor

            let TopLeft =       V4d(vp.XYZ + V3d(   -offsetX,   -offsetY, 0.0),vp.W)
            let TopRight =      V4d(vp.XYZ + V3d(    offsetX,   -offsetY, 0.0),vp.W)
            let BottomLeft =    V4d(vp.XYZ + V3d(   -offsetX,    offsetY, 0.0),vp.W)
            let BottomRight =   V4d(vp.XYZ + V3d(    offsetX,    offsetY, 0.0),vp.W)

            let TLO =   uniform.ProjTrafo * TopLeft
            let TRO =   uniform.ProjTrafo * TopRight
            let BLO =   uniform.ProjTrafo * BottomLeft
            let BRO =   uniform.ProjTrafo * BottomRight
        
            yield { v.Value with position = TLO; color = c; texCoord = V2d(0, 1); viewPos = vp }
            yield { v.Value with position = TRO; color = c; texCoord = V2d(1, 1); viewPos = vp }
            yield { v.Value with position = BLO; color = c; texCoord = V2d(0, 0); viewPos = vp }
            yield { v.Value with position = BRO; color = c; texCoord = V2d(1, 0); viewPos = vp }
            }

    let BillboardFragment (color: V4d) (v : BillboardVertex) =
        fragment {
            let c = color
            let s = uniform.UserSelected
            let t = v.texCoord
            let comp = V2d(0.5, 0.5)
            let dist = t - comp
            let len = dist.Length
            if len < 0.5 then
                if s then
                    return c
                else
                    if v.id < 100 then
                        return v.color
                    else 
                        return V4d(0.0, 1.0, 0.0, 1.0)
            else
                discard()
                return V4d(0,0,0,0)
        } 
           
            
module NewStuff =
    open System.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.ExprShape
    open FShade.Compiler
    open FShade.Utils
    open System.Collections.Generic
    open Aardvark.Base.ReflectionHelpers

    [<AutoOpen>]
    module Utilities = 
        let inline err fmt = Printf.kprintf (fun str -> Error str) fmt
        let inline fail fmt = Printf.kprintf (fun str -> failwith ("[FShade] " + str)) fmt

    module Pretty =
        open System.Text
        open Microsoft.FSharp.Quotations.DerivedPatterns
        open Aardvark.Base.ReflectionHelpers

        module String = 
            let indent (str : string) =
                str.Split([|"\r\n"|], StringSplitOptions.None) |> Array.map (fun l -> "    " + l) |> String.concat "\r\n"

            let isSingleLine (str : string) =
                str.Contains "\r\n" |> not

        let rec print (e : Expr) =
            match e with
                | Value(v,_) -> 
                    string v

                | Var(v) -> 
                    v.Name

                | Let(v, e, b) -> 
                    let b = print b
                    let e = print e
                    if String.isSingleLine e then 
                        sprintf "let %s = %s\r\n%s" v.Name e b
                    else
                        sprintf "let %s =\r\n%s\r\n%s" v.Name (String.indent e) b

                | Sequential(l,r) -> 
                    sprintf "%s\r\n%s" (print l) (print r)

                | Lambdas(args, body) ->
                    let body = print body
                    let args = args |> List.map (fun g -> g |> List.map (fun v -> sprintf "%s : %s" v.Name (getPrettyName v.Type)) |> String.concat ", " |> sprintf "(%s)") |> String.concat " "
                    
                    if String.isSingleLine body then sprintf "fun %s ->\r\n%s" args body
                    else sprintf "fun %s ->\r\n%s" args (String.indent body)

                | IfThenElse(cond, i, e) ->
                    let cond = print cond
                    let i = print i
                    let e = print e

                    if String.isSingleLine i && String.isSingleLine e then
                        sprintf "if %s then %s else %s" cond i e
                    else
                        sprintf "if %s then\r\n%s\r\nelse\r\n%s" cond i e

                | Call(t, mi, args) ->

                    let argumentCounts =
                        let att = mi.GetCustomAttribute<CompilationArgumentCountsAttribute>()
                        if isNull (att :> obj) then [mi.GetParameters().Length]
                        else att.Counts |> Seq.toList

                    let args = List.toArray args
                    let mutable offset = 0

                    let _, argGroups = 
                        List.foldBack (fun groupSize (offset, groups) ->
                            let group = 
                                List.init groupSize (fun i -> args.[offset + i])
                                    |> List.map print
                                    |> String.concat ", "
                                    |> sprintf "(%s)"
                            (offset + groupSize, group :: groups)
                        ) argumentCounts (0, [])

                    let args = argGroups |> String.concat " "
                    match t with
                        | Some t -> sprintf "%s.%s %s" (print t) mi.Name args
                        | _ -> sprintf "%s.%s %s" (getPrettyName mi.DeclaringType) mi.Name args

                | PropertyGet(t, pi, args) ->
                    let t =
                        match t with
                            | Some t -> print t
                            | None -> getPrettyName pi.DeclaringType

                    let args = args |> List.map print

                    if pi.Name = "Item" then
                        sprintf "%s.[%s]" t (String.concat ", " args)
                    else 
                        match args with
                            | [] -> sprintf "%s.%s" t pi.Name
                            | args -> sprintf "%s.%s(%s)" t pi.Name (String.concat ", " args)

                | FieldGet(t, fi) ->
                    let t =
                        match t with
                            | Some t -> print t
                            | None -> getPrettyName fi.DeclaringType
                    sprintf "%s.%s" t fi.Name

                | NewObject(ctor, args) ->
                    args |> List.map print |> String.concat ", " |> sprintf "%s(%s)" (getPrettyName ctor.DeclaringType)

                | NewArray(t, args) ->
                    args |> List.map print |> String.concat "; " |> sprintf "[| %s |]" 

                | ShapeCombination(o, args) ->
                    args |> List.map print |> String.concat ", " |> sprintf "Combination(%s)"


                | _ ->
                    ""



    [<AutoOpen>]
    module IOExtensions =
        
        type private IO private() =
            static let methods = typeof<IO>.GetMethods(BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
            static let load1 = methods |> Array.find (fun mi -> mi.Name = "Load" && mi.GetParameters().Length = 1)
            static let load2 = methods |> Array.find (fun mi -> mi.Name = "Load" && mi.GetParameters().Length = 2)
            static let store1 = methods |> Array.find (fun mi -> mi.Name = "Store" && mi.GetParameters().Length = 2)
            static let store2 = methods |> Array.find (fun mi -> mi.Name = "Store" && mi.GetParameters().Length = 3)
            static let loadUniform = methods |> Array.find (fun mi -> mi.Name = "LoadUniform")

            static member Load<'a>(name : string) : 'a = fail "cannot load input %s" name
            static member Load<'a>(name : string, index : int) : 'a = fail "cannot load input %s[%d]" name index
            static member Store<'a>(name : string, value : 'a) : unit = fail "cannot store output %s" name
            static member Store<'a>(name : string, index : int, value : 'a) : unit = fail "cannot store output %s[%d]" name index

            static member LoadUniform<'a>(uniform : Uniform) : 'a = fail "cannot load uniform %A" uniform

            static member LoadMeth = load1
            static member LoadIndexedMeth = load2
            static member LoadUniformMeth = loadUniform
            static member StoreMeth = store1
            static member StoreIndexedMeth = store2

        type Expr with
            static member Load(name : string, t : Type) = Expr.Call(IO.LoadMeth.MakeGenericMethod [|t|], [Expr.Value name])
            static member Load(name : string, index : Expr, t : Type) = Expr.Call(IO.LoadIndexedMeth.MakeGenericMethod [|t|], [Expr.Value name; index])

            static member LoadUniform(uniform : Uniform) = Expr.Call(IO.LoadUniformMeth.MakeGenericMethod [|uniform.Type|], [Expr.Value uniform])

            static member Store(name : string, value : Expr) = Expr.Call(IO.StoreMeth.MakeGenericMethod [|value.Type|], [Expr.Value name; value])
            static member Store(name : string, index : Expr, value : Expr) = Expr.Call(IO.StoreIndexedMeth.MakeGenericMethod [|value.Type|], [Expr.Value name; index; value])


        let private (|GenericInstanceOf|_|) (test : MethodInfo) (mi : MethodInfo) =
            if mi.IsGenericMethod && mi.GetGenericMethodDefinition() = test then
                Some ()
            else
                None

        let (|Load|_|) (e : Expr) =
            match e with
                | Call(None, GenericInstanceOf IO.LoadMeth, [Value((:? string as name),_)]) ->
                    Some(name, None)

                | Call(None, GenericInstanceOf IO.LoadIndexedMeth, [Value((:? string as name),_); index]) ->
                    Some(name, Some index)

                | _ ->
                    None

        let (|Store|_|) (e : Expr) =
            match e with
                | Call(None, GenericInstanceOf IO.StoreMeth, [Value((:? string as name),_); value]) ->
                    Some (name, None, value)

                | Call(None, GenericInstanceOf IO.StoreIndexedMeth, [Value((:? string as name),_); index; value]) ->
                    Some (name, Some index, value)

                | _ ->
                    None
            
        let (|LoadUniform|_|) (e : Expr) =
            match e with
                | Call(None, GenericInstanceOf IO.LoadUniformMeth, [Value((:? UniformScope as s),_); Value((:? string as name),_)]) ->
                    Some(s, name)
                | _ ->
                    None

        type Expr with

            member x.SubstituteLoads(f : Type -> string -> Option<Expr> -> Option<Expr>) =
                match x with
                    | Load(name, index) -> 
                        match f x.Type name index with
                            | Some r -> r
                            | None -> x

                    | ShapeVar(v) -> 
                        x

                    | ShapeLambda(v,b) -> 
                        Expr.Lambda(v, b.SubstituteLoads f)

                    | ShapeCombination(o, args) ->
                        RebuildShapeCombination(o, args |> List.map (fun a -> a.SubstituteLoads f))

    type private PreprocessorState =
        {
            inputTopology : Option<InputTopology>
            vertexType : Type
            inputs : Dictionary<string, Type>
            outputs : Dictionary<string, Type>
            uniforms : HashSet<Uniform>
            mutable builder : Expr
        }

        member x.AddInput(name : string, t : Type) =
            match x.inputs.TryGetValue name with
                | (true, old) ->
                    if old = t then ()
                    else fail "conflicting input-types for %s (%s vs %s)" name (getPrettyName old) (getPrettyName t)
                | _ ->
                    x.inputs.[name] <- t

        member x.AddOutput(name : string, t : Type) =
            match x.outputs.TryGetValue name with
                | (true, old) ->
                    if old = t then ()
                    else fail "conflicting outputs-types for %s (%s vs %s)" name (getPrettyName old) (getPrettyName t)
                | _ ->
                    x.outputs.[name] <- t
            
        member x.AddUniform(u : Uniform) =
            x.uniforms.Add u |> ignore

    module private Preprocessor =
        open System.Collections.Generic
        open Aardvark.Base.ReflectionHelpers

        [<AutoOpen>]
        module private Helpers = 
            let (|VertexLoad|_|) (vertexType : Type) (e : Expr) =
                match e with
                    | PropertyGet(Some e, p, []) when e.Type = vertexType ->
                        let att = p.GetCustomAttributes<SemanticAttribute>(true) |> Seq.toList
                        match att with
                            | x::_ -> Some (e, x.Semantic)
                            | _ -> Some (e, p.Name)
                    | _ ->
                        None

            let (|Primitive|_|) (e : Option<Expr>) =
                match e with
                    | Some e -> 
                        if e.Type.GetInterface("Primitive`1") <> null then
                            match e with
                                | Value _ | Var _ | PropertyGet(None, _, []) -> Some e
                                | _ -> None
                        else
                            None
                    | None ->
                        None

            let tryGetPrimitiveVertexId (prop : PropertyInfo) =
                let att = prop.GetCustomAttribute<PrimitiveIndexAttribute>()
                if isNull att then 
                    None
                else
                    Some att.Index

        let rec private preprocessInternal (state : PreprocessorState) (vertexIndices : Map<Var, Expr>) (e : Expr) =
            match e with
                | BuilderCall(b, mi, [Lambda(v,body)]) when mi.Name = "Delay" ->
                    state.builder <- b
                    preprocessInternal state vertexIndices body

                | BuilderCall(b, Method("For",_), [ExprOf(FixedArrayType(d,_)) as seq; Lambda(v,Let(vi,Var(vo),body))]) ->  
                    state.builder <- b   
                    let body = preprocessInternal state vertexIndices body
                    let seq = preprocessInternal state vertexIndices seq            
                    let result = Expr.ForEach(vi, seq, body)
                    result

                | BuilderCall(b, Method("For",_), [Call(None, Method("op_Range",_), [s; e]); Lambda(v,Let(vi,Var(vo),body))]) ->
                    state.builder <- b
                    let body = preprocessInternal state vertexIndices body
                    let s = preprocessInternal state vertexIndices s  
                    let e = preprocessInternal state vertexIndices e
                    Expr.ForIntegerRangeLoop(vi, s, e, body)

                | BuilderCall(b, Method("Combine",_), [l;r]) ->
                    state.builder <- b
                    let l = preprocessInternal state vertexIndices l
                    let r = preprocessInternal state vertexIndices r
                    Expr.Sequential(l,r)

                | BuilderCall(b, Method("Zero",_), []) ->
                    state.builder <- b
                    Expr.Value(())


                // for v in primitive do
                | BuilderCall(b, Method("For",_) , [Coerce(primitive, t); Lambda(v,Let(vi,Var(vo),body))]) 
                  when b.Type = typeof<GeometryBuilder> || b.Type = typeof<TessControlBuilder> || b.Type = typeof<TessEvalBuilder> ->
                    state.builder <- b

                    let count = primitive.Type.GetProperty("VertexCount", BindingFlags.Static ||| BindingFlags.Public).GetValue(null) |> unbox<int>
                
                    let index = Var("i", typeof<int>)
                    let vertexIndices = Map.add vi (Expr.Var index) vertexIndices
                    let body = preprocessInternal state vertexIndices body

                    Expr.ForIntegerRangeLoop(index, Expr.Value(0), Expr.Value(count - 1),  body)


                // let v = primitive.P0 || let v = primitive.VertexCount
                | Let(v, PropertyGet(Primitive p, pi, []), body) ->
                    match tryGetPrimitiveVertexId pi with
                        | Some index -> 
                            let i = Var(v.Name, typeof<int>)
                            let vertexIndices = Map.add v (Expr.Var i) vertexIndices
                        
                            let body = preprocessInternal state vertexIndices body

                            Expr.Let(i, Expr.Value(index), body)

                        | _ ->               
                            let body = preprocessInternal state vertexIndices body
                            Expr.Let(v, Expr.PropertyGet(p, pi, []), body)

                // primitive.P0.pos || primitive.VertexCount.Abs
                | MemberFieldGet(PropertyGet(Primitive p, pi, []), m) ->
                    match tryGetPrimitiveVertexId pi with
                        | Some index ->
                            let n = m.Semantic
                            state.AddInput(n, e.Type.MakeArrayType())
                            Expr.Load(n, Expr.Value index, e.Type)
                        | None ->
                            e

                // primitive.[i].pos
                | MemberFieldGet(PropertyGet(Some p, pi, [index]), m) when pi.Name = "Item" ->
                    let sem = m.Semantic
                    state.AddInput(sem, e.Type.MakeArrayType())
                    Expr.Load(sem, index, e.Type)

                | VertexLoad state.vertexType (ve, sem) ->
                    match ve with
                        | Value(null,_) -> 
                            state.AddInput(sem, e.Type)
                            Expr.Load(sem, e.Type)
                        | Var v ->
                            match Map.tryFind v vertexIndices with
                                | Some i -> 
                                    state.AddInput(sem, e.Type.MakeArrayType())
                                    Expr.Load(sem, i, e.Type)
                                | _ -> 
                                    fail "accessing vertex input with unknown index"

                        | e ->
                            fail "complex vertex expressions not supported: %A" ve

                // yield (let a = x+y in f(a))   ==>    yield f(x+y)
                | BuilderCall(b, mi, [Let(v, e, inner)]) when mi.Name = "Yield" ->
                    state.builder <- b
                    let inner = inner.Substitute (fun vi -> if vi = v then Some e else None)
                    Expr.Call(b, mi, [inner]) |> preprocessInternal state vertexIndices

                // yield { a = 1; b = x + y }
                | BuilderCall(b, mi, [NewRecord(t, fields)]) when mi.Name = "Yield" ->
                    state.builder <- b
                    let semantics = FSharpTypeExt.GetRecordFields(t) |> Seq.map (fun m -> m.Semantic) |> Seq.toList

                    let stores =
                        List.zip semantics fields 
                            |> List.collect (fun (name, value) ->
                                let value = preprocessInternal state vertexIndices value

                                if value.Type.IsArray then
                                    match value with
                                        | NewArray(t, args) ->
                                            state.AddOutput(name, value.Type)
                                            args |> List.mapi (fun i v -> Expr.Store(name, Expr.Value i, v))
                                        | _ ->
                                            Log.warn "[FShade] bad array output"
                                            []


                                else
                                    state.AddOutput(name, value.Type)
                                    [Expr.Store(name, value)]
                            )

                    let emit = Expr.Call(getMethodInfo <@ emitVertex @>, [])
                    stores |> List.fold (fun a e -> Expr.Sequential(e, a)) emit

                // yield V4d.IOOI
                | BuilderCall(b, mi, [value]) when mi.Name = "Yield" ->
                    state.builder <- b
                    let value = preprocessInternal state vertexIndices value
                    state.AddOutput(Intrinsics.Position, typeof<V4d>)
                    let store = 
                        if value.Type = typeof<V4d> then 
                            Expr.Store(Intrinsics.Position, value)

                        elif value.Type = typeof<V3d> then 
                            let ctor = typeof<V4d>.GetConstructor [|typeof<V3d>; typeof<float> |]
                            Expr.Store(Intrinsics.Position, Expr.NewObject(ctor, [value; Expr.Value 1.0]))

                        else 
                            fail "cannot yield %A" value
                    
                    Expr.Sequential(store, Expr.Call(getMethodInfo <@ emitVertex @>, []))

                // return (let a = x+y in f(a))   ==>    return f(x+y)
                | BuilderCall(b, mi, [Let(v, e, inner)]) when mi.Name = "Return" ->
                    state.builder <- b
                    let inner = inner.Substitute (fun vi -> if vi = v then Some e else None)
                    Expr.Call(b, mi, [inner]) |> preprocessInternal state vertexIndices


                | BuilderCall(b, mi, [NewRecord(t, fields)]) when mi.Name = "Return" ->
                    state.builder <- b
                    let semantics = FSharpTypeExt.GetRecordFields(t) |> Seq.map (fun m -> m.Semantic) |> Seq.toList

                    let stores =
                        List.zip semantics fields 
                            |> List.collect (fun (name, value) ->
                                let value = preprocessInternal state vertexIndices value


                                if value.Type.IsArray then
                                    match value with
                                        | NewArray(t, args) ->
                                            state.AddOutput(name, value.Type)
                                            args |> List.mapi (fun i v -> Expr.Store(name, Expr.Value i, v))
                                        | _ ->
                                            Log.warn "[FShade] bad array output"
                                            []


                                else
                                    state.AddOutput(name, value.Type)
                                    [Expr.Store(name, value)]
                            )

                    match stores with
                        | [] -> Expr.Value(())
                        | fst :: rest ->
                            List.fold (fun e rest -> Expr.Sequential(e, rest)) fst rest

                // return V4d.IOOI
                | BuilderCall(b, mi, [value]) when mi.Name = "Return" ->
                    state.builder <- b
                    let value = preprocessInternal state vertexIndices value
                    let sem = 
                        if b.Type = typeof<FragmentBuilder> then Intrinsics.Color
                        else Intrinsics.Position

                    state.AddOutput(sem, typeof<V4d>)
                    if value.Type = typeof<V4d> then
                        Expr.Store(sem, value)
                    elif value.Type = typeof<V3d> then
                        let ctor = typeof<V4d>.GetConstructor [|typeof<V3d>; typeof<float> |]
                        Expr.Store(sem, Expr.NewObject(ctor, [value; Expr.Value 1.0]))
                    else
                        fail "cannot return %A" value
                       
                
                | Uniform u ->
                    state.AddUniform u
                    Expr.LoadUniform u
                       
                        
                | ShapeCombination(o, args) ->
                    let args = args |> List.map (preprocessInternal state vertexIndices)
                    RebuildShapeCombination(o, args)

                | ShapeLambda(v,b) -> 
                    Expr.Lambda(v, preprocessInternal state vertexIndices b)

                | ShapeVar _ -> 
                    e

        /// preprocess removes all builder calls from the given expression, 
        /// replaces all input-accesses with Expr.Load and all writes with Expr.Store.
        /// additionaly the used builder 
        let preprocess (inputType : Type) (e : Expr) =

            // figure out the vertex-type and the (optional) input-topology
            let vertexType, inputTopology =
                let p = inputType.GetInterface("Primitive`1")
                if isNull p then inputType, None
                else 
                    let vertexType = p.GetGenericArguments().[0]
                    let top = inputType.GetProperty("InputTopology", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public).GetValue(null) |> unbox<InputTopology>
                    vertexType, Some top

            // the builder can be inlined (TODO: may be nested)
            let input = 
                match e with
                    | Application(Lambda(var, b), value) -> b.Substitute(fun vi -> if vi = var then Some value else None)
                    | e -> e

            // run the real processing
            let state = 
                {
                    inputTopology = inputTopology
                    vertexType = vertexType
                    inputs = Dictionary()
                    outputs = Dictionary()
                    uniforms = HashSet()
                    builder = Expr.Value(())
                }

            let clean = input |> preprocessInternal state Map.empty


            clean, state


    [<NoComparison>]
    type Shader = 
        { 
            shaderType      : ShaderType
            uniforms        : pset<Uniform>
            inputs          : Map<string, Type>
            outputs         : Map<string, Type>
            body            : Expr
            inputTopology   : Option<InputTopology>
            debugInfo       : Option<ShaderDebugInfo>
        }

        member x.outputTopology =
            match x.shaderType with
                | Geometry(_,t) -> Some t
                | _ -> None

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Shader = 
        let inline shaderType (s : Shader) = s.shaderType
        let inline uniforms (s : Shader) = s.shaderType
        let inline inputs (s : Shader) = s.shaderType
        let inline outputs (s : Shader) = s.outputs
        let inline body (s : Shader) = s.body
        let inline inputTopology (s : Shader) = s.inputTopology
        let inline debugInfo (s : Shader) = s.debugInfo
        let inline outputTopology (s : Shader) = s.outputTopology
        
        
        let ofExpr (inputType : Type) (e : Expr) =
            let body, state = Preprocessor.preprocess inputType e

            // figure out the used builder-type
            let shaderType = 
                match Expr.tryEval state.builder with
                    | Some (:? IShaderBuilder as v) -> v.ShaderType
                    | _ -> fail "could not evaluate shader-builder %A" state.builder

            { 
                shaderType = shaderType
                uniforms = state.uniforms |> PSet.ofSeq
                inputs = state.inputs |> Dictionary.toMap
                outputs = state.outputs |> Dictionary.toMap
                body = body
                inputTopology = state.inputTopology
                debugInfo = None 
            }

        let ofFunction (f : 'a -> Expr<'b>) =
            let e = f Unchecked.defaultof<'a>
            ofExpr typeof<'a> e

       
    [<NoComparison>]
    type Effect = 
        { 
            vertex        : Option<Shader>
            tessControl   : Option<Shader>
            tessEval      : Option<Shader>
            geometry      : Option<Shader>
            fragment      : Option<Shader>
        }

        member x.hasVertex = Option.isSome x.vertex
        member x.hasTessControl = Option.isSome x.tessControl
        member x.hasTessEval = Option.isSome x.tessEval
        member x.hasGeometry = Option.isSome x.geometry
        member x.hasFragment = Option.isSome x.fragment

        member x.shaders =
            List.concat [
                Option.toList x.vertex
                Option.toList x.tessControl
                Option.toList x.tessEval
                Option.toList x.geometry
                Option.toList x.fragment
            ]

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Effect =
        let inline vertex (e : Effect) = e.vertex
        let inline tessControl (e : Effect) = e.tessControl
        let inline tessEval (e : Effect) = e.tessEval
        let inline geometry (e : Effect) = e.geometry
        let inline fragment (e : Effect) = e.fragment
        let inline hasVertex (e : Effect) = e.hasVertex
        let inline hasTessControl (e : Effect) = e.hasTessControl
        let inline hasTessEval (e : Effect) = e.hasTessEval
        let inline hasGeometry (e : Effect) = e.hasGeometry
        let inline hasFragment (e : Effect) = e.hasFragment
        let inline shaders (e : Effect) = e.shaders

        let empty = { vertex = None; tessControl = None; tessEval = None; geometry = None; fragment = None }

        let isEmpty (e : Effect) =
            Option.isNone e.vertex &&
            Option.isNone e.tessControl &&
            Option.isNone e.tessEval &&
            Option.isNone e.geometry &&
            Option.isNone e.fragment

        let ofShader (shader : Shader) =
            match shader.shaderType with
                | Vertex        -> { empty with vertex = Some shader }
                | TessControl   -> { empty with tessControl = Some shader }
                | TessEval      -> { empty with tessEval = Some shader }
                | Geometry _    -> { empty with geometry = Some shader }
                | Fragment      -> { empty with fragment = Some shader }

        let inline ofExpr (inputType : Type) (e : Expr) =
            Shader.ofExpr inputType e |> ofShader

        let inline ofFunction (f : 'a -> Expr<'b>) =
            Shader.ofFunction f |> ofShader


type Bla = { a : int; b : int }
type Blubb = Sepp of int | Heinz of float

[<EntryPoint>]
let main argv = 

//    let evilShader (tri : Triangle<Dead.BillboardVertex>) =
//        triangle {
//            for v in tri do
//                yield { v with position = v.position - tri.P0.position }
//            
//        }
//
//
//    let e = NewStuff.Effect.ofFunction (evilShader)
//    printfn "%A" e

    let testModule = 
        FShade.Compiler.CModule.ofLambda "test" <@ fun (a : int) -> 
            (Sepp a, float 1)
        @>
    printfn "%A" testModule

    let code = FShade.Compiler.GLSL.CModule.glsl testModule
    printfn "%s" code

    Environment.Exit 0



    let effect = [Dead.BillboardTrafo |> toEffect
                  Dead.BillboardGeometry (V2d(0.06, 0.08)) (fun a -> a) |> toEffect
                  Dead.BillboardFragment V4d.IIII |> toEffect
                  ] |> compose

    let res = GLSL.compileEffect GLSL.version410 (Map.ofList["Colors", typeof<V4d>]) effect
    match res with
        | Success (uniforms, code) ->
            for (name,getter) in Map.toSeq uniforms do
                match getter with
                    | AttributeGetter(name, t) -> printfn "%s : %A" name t
                    | _ -> ()
            printfn "%s" code
        | _ ->
            ()

    Environment.Exit 0
    let effect = [Simple.trafo   |> toEffect
                  Simple.normals |> toEffect
                  Simple.texture |> toEffect
                  Simple.light   |> toEffect
                  ] |> compose


    let w = new Window()

    let ps = [|V3f.OOO; V3f.IOO; V3f.IIO; V3f.OIO|] :> Array
    let tc = [|V2f.OO; V2f.IO; V2f.II; V2f.OI|] :> Array
    let n = [|V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI|] :> Array
    let b = [|V3f.IOO; V3f.IOO; V3f.IOO; V3f.IOO|] :> Array
    let t = [|V3f.OIO; V3f.OIO; V3f.OIO; V3f.OIO|] :> Array
    let indices = [|0;1;2; 0;2;3|] :> Array

    let sg = Sg.geometry (Some indices) (["Positions", ps; "TexCoords", tc; "Normals", n; "BiNormals", b; "Tangents", t] |> Map.ofList)
    let sg = Sg.shader "Main" effect sg


    FShade.Debug.EffectEditor.runTray()

    let sg = Sg.fileTexture "DiffuseTexture" @"C:\Aardwork\FLOOR2.jpg" sg
    let sg = Sg.fileTexture "NormalMap"      @"C:\Aardwork\brickwall_normal.jpg" sg

    let sg = Sg.uniform "Color" (V4f(1,1,1,1)) sg


    w.Scene <- sg

    w.Run()

    0
