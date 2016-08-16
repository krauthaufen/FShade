namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection
open Aardvark.Base
open FShade.Utils
open FShade.Compiler


[<AutoOpen>]
module BuilderCompilation =
    let rec private inlineTopLevelVariables (e : Expr) =
        match e with
            | Let(v,value, b) ->
                let b : Expr = inlineTopLevelVariables b
                b.Substitute(fun vi -> if vi = v then Some value else None)
            | _ -> e
              
    let private tryGetPrimitiveVertexId (prop : PropertyInfo) =
        let att = prop.GetCustomAttribute<PrimitiveIndexAttribute>()
        if isNull att then 
            None
        else
            Some att.Index

    let rec private removeBuilderCallsInternal (e : Expr) =
        transform {
            match e with
                | BuilderCall(b, mi, [Lambda(v,body)]) when mi.Name = "Delay" ->
                    return! removeBuilderCallsInternal body


                | BuilderCall(b, Method("For",_), [ExprOf(FixedArrayType(d,_)) as seq; Lambda(v,Let(vi,Var(vo),body))]) ->     
                    let! body = removeBuilderCallsInternal body
                    let! seq = removeBuilderCallsInternal seq            
                    let result = Expr.ForEach(vi, seq, body)
                    return result

                | BuilderCall(b, Method("For",_), [Call(None, Method("op_Range",_), [s; e]); Lambda(v,Let(vi,Var(vo),body))]) ->
                    let! body = removeBuilderCallsInternal body
                    let! s = removeBuilderCallsInternal s  
                    let! e = removeBuilderCallsInternal e
                    return Expr.ForIntegerRangeLoop(vi, s, e, body)


                | BuilderCall(b, Method("For",_) , [Coerce(primitive, t); Lambda(v,Let(vi,Var(vo),body))]) 
                  when b.Type = typeof<GeometryBuilder> || b.Type = typeof<TessControlBuilder> || b.Type = typeof<TessEvalBuilder> ->

                    let count = primitive.Type.GetProperty("VertexCount", BindingFlags.Static ||| BindingFlags.Public).GetValue(null) |> unbox<int>
                
                    let i = Var("i", typeof<int>)
                    let! body = removeBuilderCallsInternal body
                    let! body = substituteInputs v.Type (Some (Expr.Var(i))) body

                    let e = Expr.ForIntegerRangeLoop(i, Expr.Value(0), Expr.Value(count - 1),  body)
                    
                    return e

                | BuilderCall(b, Method("Combine",_), [l;r]) ->

                    let! l = removeBuilderCallsInternal l
                    let! r = removeBuilderCallsInternal r
                    return Expr.Sequential(l,r)

                | BuilderCall(builder, Method("Delay",_), [Lambda(_,b)]) ->
                    do! setBuilder builder
                    let! b = removeBuilderCallsInternal b
                    return b

                | BuilderCall(b, Method("Zero",_), []) ->
                    do! setBuilder b
                    return Expr.Value(())

                | Let(v, PropertyGet(Some p, pi, []), body)  when p.Type.GetInterface("Primitive`1") <> null ->
                    
                    let index = 
                        match tryGetPrimitiveVertexId pi with
                            | Some id -> id
                            | _ -> -1                

                    if index < 0 then
                        return! error "invalid input property" 
                    else
                        let i = Expr.Value(index)
                        let vi = Var(v.Name, typeof<int>)
                        
                        let! body = substituteInputAccess v vi body
                        let! body = removeBuilderCallsInternal body

                        return Expr.Let(vi, Expr.Value(index), body)




                //PropertyGet (Some (Value (<null>)), TessCoord, [])
                | MemberFieldGet(PropertyGet(Some p, pi, []), m) when p.Type.GetInterface("Primitive`1") <> null ->

                    //p.Value.abc
                    let t = m.Type.MakeArrayType()
                    let n = m.Semantic
                    let! i = getInput t n
                    let i = Expr.Var(i)

                    match tryGetPrimitiveVertexId pi with
                        | Some index -> return Expr.ArrayAccess(i, Expr.Value(index))
                        | _          -> return! error "invalid input property"

                | MemberFieldGet(PropertyGet(Some p, pi, [index]), m) when p.Type.GetInterface("Primitive`1") <> null && pi.Name = "Item"->
                    let t = m.Type.MakeArrayType()
                    let n = m.Semantic
                    let! i = getInput t n
                    let i = Expr.Var(i)
                    return Expr.ArrayAccess(i, index)

                | BuilderCall(b, mi, [Let(v, e, inner)]) when mi.Name = "Yield" ->
                    let inner = inner.Substitute (fun vi -> if vi = v then Some e else None)
                    return! removeBuilderCallsInternal (Expr.Call(b, mi, [inner]))

                | BuilderCall(b, mi, [NewRecord(t, fields)]) when mi.Name = "Yield" ->
                    do! setBuilder b
                    let semantics = FSharpTypeExt.GetRecordFields(t) |> Seq.map (fun m -> m.Semantic, m.AssignedTarget) |> Seq.toList
                    let setters = List.zip semantics fields

                    let! outputs = 
                        setters |> List.collectC (fun ((s,t),v) ->
                            transform {
                                let! o = getOutput v.Type s t
                                
                                if v.Type.IsArray then
                                    
                                    let! v = removeBuilderCallsInternal v
                                    match v with
                                        | NewArray(t, args) ->
                                            let set = getMethodInfo <@ LanguagePrimitives.IntrinsicFunctions.SetArray @>
                                            let set = set.MakeGenericMethod [|t|]
                                            return args |> List.mapi (fun i ei -> 
                                                Expr.Call(set, [Expr.Var o; Expr.Value i; ei])
                                            )
                                        | _ ->
                                            return! error "outputs cannot be nonprimitive arrays"
                                else
                                    let! v = removeBuilderCallsInternal v
                                    let! o = getOutput v.Type s t
                                
                                    return [Expr.VarSet(o,v)] //[o,v]
                            }
                        )

                    let emit = Expr.Call(getMethodInfo <@ emitVertex @>, [])
                    let result = outputs |> List.fold (fun a (e) -> Expr.Sequential(e, a)) emit
                    return result

                | BuilderCall(b, mi, [Let(v, e, inner)]) when mi.Name = "Return" ->
                    let inner = inner.Substitute (fun vi -> if vi = v then Some e else None)
                    return! removeBuilderCallsInternal (Expr.Call(b, mi, [inner]))

                | BuilderCall(b, mi, [value]) when mi.Name = "Return" ->
                    do! setBuilder b
                    
                    let! value = removeBuilderCallsInternal value

                    if FSharpTypeExt.IsRecord(mi.ReturnType) && mi.ReturnType = value.Type then
                        //standard record return
                        return inlineTopLevelVariables value

                    elif mi.ReturnType = value.Type then
                        //single value return
                        if b.Type = typeof<FragmentBuilder> then
                            if value.Type = typeof<V4d> then
                                return Expr.NewRecord(typeof<ColorOnlyFragment>, [value])
                            elif value.Type = typeof<V3d> then
                                let ctor = typeof<V4d>.GetConstructor [|typeof<V3d>; typeof<float> |]
                                return Expr.NewRecord(typeof<ColorOnlyFragment>, [Expr.NewObject(ctor, [value; Expr.Value(1.0)])])
                            else
                                return! error "invalid color type: %A" value.Type

                        elif b.Type = typeof<VertexBuilder> then
                            if value.Type = typeof<V4d> then
                                return Expr.NewRecord(typeof<PositionOnlyVertex>, [value])
                            elif value.Type = typeof<V3d> then
                                let ctor = typeof<V4d>.GetConstructor [|typeof<V3d>; typeof<float> |]
                                return Expr.NewRecord(typeof<PositionOnlyVertex>, [Expr.NewObject(ctor, [value; Expr.Value(1.0)])])
                            else
                                return! error "invalid position type: %A" value.Type

                        else
                            return! error "unknown shader type: %A" b.Type

                    elif FSharpTypeExt.IsRecord(mi.ReturnType) && (FSharpTypeExt.GetRecordFields(mi.ReturnType)).Length = 1 then
                        //old and deprecated ColorOnly-return-style
                        return Expr.NewRecord(mi.ReturnType, [value])

                    else
                        return! error "unknown return overload"

                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapC removeBuilderCallsInternal
                    return RebuildShapeCombination(o, args)

                | ShapeLambda(v,b) -> 
                    let! b = removeBuilderCallsInternal b
                    return Expr.Lambda(v, b)

                | _ -> return e
        }

    let removeBuilderCalls (e : Expr) =
        transform {
            let! e = match e with
                        | Application(Lambda(var,b),value) ->
                            b.Substitute(fun vi -> if vi = var then Some value else None) |> removeBuilderCallsInternal

                        | _ -> removeBuilderCallsInternal e

            return e
        }

