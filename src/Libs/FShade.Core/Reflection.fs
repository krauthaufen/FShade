namespace FShade

open System
open System.Reflection

open Aardvark.Base

open FShade

[<AutoOpen>]
module ReflectionPatterns = 
    let private getProp<'a> (name : string) (t : Type) =
        t.GetProperty(name).GetValue(null) |> unbox<'a>


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
                
        member x.DepthWriteMode =
            let att = x.GetCustomAttributes<DepthAttribute>(true) |> Seq.toList
            match att with
                | x::_ -> x.Mode
                | _ -> DepthWriteMode.Any

        member x.PrimitiveIndex =
            let att = x.GetCustomAttributes<PrimitiveIndexAttribute>(true) |> Seq.toList
            match att with
                | x::_ -> ValueSome x.Index
                | _ -> ValueNone

    /// <summary>
    /// determines whether a given type is a Sampler and returns its properties if successful.
    /// The properties are given by SamplerType(dim, isArray, isShadow, isMS, valueType)
    /// </summary>
    [<return: Struct>]
    let (|SamplerType|_|) (t : Type) =
        if typeof<ISampler>.IsAssignableFrom(t) then
            let dim : SamplerDimension = getProp "Dimension" t
            let isArray : bool = getProp "IsArray" t
            let isShadow : bool = getProp "IsShadow" t
            let isMS : bool = getProp "IsMultisampled" t
            let valueType : Type = getProp "ValueType" t

            SamplerType(dim, isArray, isShadow, isMS, valueType) |> ValueSome

        else
            ValueNone

    /// <summary>
    /// determines whether a given type is an Image and returns its properties if successful.
    /// The properties are given by ImageType(dim, isArray, isMS, valueType)
    /// </summary>
    [<return: Struct>]
    let (|ImageType|_|) (t : Type) =
        if typeof<IImage>.IsAssignableFrom(t) then
            let dim : SamplerDimension = getProp "Dimension" t
            let isArray : bool = getProp "IsArray" t
            let isMS : bool = getProp "IsMultisampled" t
            let valueType : Type = getProp "ValueType" t
            let format : Type = getProp "FormatType" t
            ImageType(format, dim, isArray, isMS, valueType) |> ValueSome
        else
            ValueNone

    [<return: Struct>]
    let (|AccelerationStructure|_|) (t : Type) =
        if typeof<IAccelerationStructure>.IsAssignableFrom(t) then
            ValueSome ()
        else
            ValueNone

    type UniformParameter with
        member x.decorations =
            match x.uniformType with
                | ImageType(fmt,_,_,_,_) -> [Imperative.UniformDecoration.Format fmt]
                | _ -> []

type DebugRange =
    {
        file : string
        startLine : int
        startCol : int
        endLine : int
        endCol : int
    }


[<AutoOpen>]
module BasicQuotationPatterns =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns

    [<return: Struct>]
    let (|BuilderCall|_|) (e : Expr) =
        match e with
        | Call(Some t, mi, args) when typeof<IShaderBuilder>.IsAssignableFrom t.Type ->
            BuilderCall(t, mi, args) |> ValueSome
        | _ -> ValueNone

    [<return: Struct>]
    let (|UniformScopeType|_|) (t : Type) =
        if t = typeof<UniformScope> then UniformScopeType |> ValueSome
        else ValueNone

    [<return: Struct>]
    let (|Uniform|_|) (e : Expr) =
        match e with
        | ValueWithName(sam,t,name) ->
            match sam with
            | :? ISampler as sam ->
                let tex = sam.Texture
                ValueSome { 
                    uniformName = name
                    uniformType = sam.GetType()
                    uniformValue = Sampler(tex.Semantic, sam.State) 
                } 
            | _ ->
                match t with
                | ArrayOf(SamplerType _) ->
                    let arr = sam |> unbox<Array>
                    let samplers = 
                        List.init arr.Length (fun i -> 
                            let sam1 = arr.GetValue i |> unbox<ISampler>
                            let tex = sam1.Texture
                            tex.Semantic, sam1.State
                        )

                    let t = Peano.getArrayType arr.Length t
                    ValueSome {
                        uniformName = name
                        uniformType = t
                        uniformValue = SamplerArray(List.toArray samplers)
                    } 
                | _ ->
                    ValueNone

        | PropertyGet(None, pi, []) ->
            match pi.Type with
            | SamplerType(_) ->
                match Expr.TryEval e with
                | Some sam ->
                    let sam = sam |> unbox<ISampler>
                    let tex = sam.Texture
                    ValueSome { 
                        uniformName = pi.Name
                        uniformType = sam.GetType()
                        uniformValue = Sampler(tex.Semantic, sam.State) 
                    } 

                | None ->
                    ValueNone

            | ArrayOf((SamplerType _ as t)) ->
                match Expr.TryEval e with
                | Some sam ->
                    let arr = sam |> unbox<Array>
                    let samplers = 
                        List.init arr.Length (fun i -> 
                            let sam1 = arr.GetValue i |> unbox<ISampler>
                            let tex = sam1.Texture
                            tex.Semantic, sam1.State
                        )

                    let t = Peano.getArrayType arr.Length t
                    ValueSome {
                        uniformName = pi.Name
                        uniformType = t
                        uniformValue = SamplerArray(List.toArray samplers)
                    } 

                | None -> 
                    ValueNone

            | _ -> ValueNone

        | Call(None, Method("op_Dynamic", [UniformScopeType; _]), [scope; Value(s,_)]) ->
            match Expr.TryEval scope with
            | Some scope ->
                let scope = scope |> unbox
                ValueSome {
                    uniformName = unbox s
                    uniformType = e.Type
                    uniformValue = Attribute(scope, unbox s)
                }

            | None ->
                ValueNone

        | PropertyGet(Some scope, p, []) when scope.Type = typeof<UniformScope> ->
            match Expr.TryEval scope with
            | Some scope ->
                let old = UniformStuff.Push()
                let result = p.GetValue(scope, [||])
                match result with
                    | :? ISemanticValue as v ->
                        ValueSome {
                            uniformName = v.Semantic
                            uniformType = v.GetType()
                            uniformValue = Attribute(v.Scope, v.Semantic)
                        }
                    | _ ->
                        match UniformStuff.Pop old with
                        | Some (scope, name) -> 
                            ValueSome {
                                uniformName = name
                                uniformType = p.PropertyType
                                uniformValue = Attribute(scope, name)
                            }
                        | None -> 
                            ValueNone
            | None ->
                ValueNone
//                with :? TargetInvocationException as ex ->
//                    match ex.InnerException with
//                        | :? SemanticException as s -> 
//                            ValueSome {
//                                uniformName = s.Semantic
//                                uniformType = p.PropertyType
//                                uniformValue = Attribute(s.Scope, s.Semantic)
//                            }
//
//                        | _ -> 
//                            ValueNone

        | Call(None, m, [scope]) when scope.Type = typeof<UniformScope> ->
            match Expr.TryEval scope with
            | Some scope ->
                let old = UniformStuff.Push()
                let result = m.Invoke(null, [| scope |])
                match result with
                    | :? ISemanticValue as v ->
                        ValueSome {
                            uniformName = v.Semantic
                            uniformType = v.GetType()
                            uniformValue = Attribute(v.Scope, v.Semantic)
                        }
                    | _ ->
                        match UniformStuff.Pop old with
                            | Some (scope, name) -> 
                                ValueSome {
                                    uniformName = name
                                    uniformType = m.ReturnType
                                    uniformValue = Attribute(scope, name)
                                }
                            | None -> 
                                ValueNone

            | None ->
                ValueNone

        | _ -> ValueNone


    [<return: Struct>]
    let (|DebugRange|_|) (e : Expr) =
        match e with
            | NewTuple([String "DebugRange"; NewTuple [String file; Int32 startLine; Int32 startCol; Int32 endLine; Int32 endCol]]) -> 
                ValueSome { file = file; startLine = startLine; startCol = startCol; endLine = endLine; endCol = endCol }
            | _ -> ValueNone


    module Map =
        let choose (f : 'k -> 'v -> Option<'r>) (m : Map<'k, 'v>) =
            let mutable res = Map.empty
            for (k,v) in Map.toSeq m do
                match f k v with
                    | Some v -> res <- Map.add k v res
                    | _ -> ()
            res

        let keys (m : Map<'a, 'b>) = m |> Map.toSeq |> Seq.map fst
        let values (m : Map<'a, 'b>) = m |> Map.toSeq |> Seq.map snd

        let intersect (l : Map<'a, 'b>) (r : Map<'a, 'c>) =
            l |> choose (fun k lv ->
                match Map.tryFind k r with
                    | Some rv -> Some (lv, rv)
                    | None -> None
            )

        let difference (l : Map<'a, 'b>) (r : Map<'a, 'c>) =
            l |> choose (fun k lv ->
                match Map.tryFind k r with
                    | Some rv -> None
                    | None -> Some lv
            )
