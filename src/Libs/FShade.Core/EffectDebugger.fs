namespace FShade

open System
open FSharp.Data.Adaptive
open Aardvark.Base

#nowarn "44"

[<Obsolete("Use ShaderDebugger instead.")>]
module EffectDebugger =

    let mutable isAttached = false

    // Effect -> IMod<Effect>
    let mutable registerFun : Option<Effect -> obj> = None
    let mutable saveCode : Effect -> string -> unit = fun _ _ -> ()

    let register (e : Effect) =
        match registerFun with
            | Some f -> Some (f e)
            | _ -> None

module ShaderDebugger =

    type IShaderDebugger =
        inherit IDisposable
        abstract member TryRegisterEffect : Effect -> aval<Effect> option
        abstract member TryRegisterRaytracingEffect : RaytracingEffect -> aval<RaytracingEffect> option

    let private lockObj = obj()
    let mutable private instance : Option<IShaderDebugger> = None

    let initialize (f : unit -> IShaderDebugger) : IDisposable =
        lock lockObj (fun _ ->
            match instance with
            | Some _ -> Disposable.empty
            | _ ->
                let inst = f()
                instance <- Some inst

                EffectDebugger.isAttached <- true
                EffectDebugger.registerFun <-
                    Some (
                        fun effect ->
                            match inst.TryRegisterEffect effect with
                            | Some res -> res :> obj
                            | _ -> AVal.constant effect :> obj
                    )

                { new IDisposable with
                    member x.Dispose() =
                        lock lockObj (fun _ ->
                            instance <- None
                            EffectDebugger.isAttached <- false
                            EffectDebugger.registerFun <- None
                            inst.Dispose()
                        )
                }
        )

    let tryRegisterEffect (effect : Effect) =
        lock lockObj (fun _ ->
            match instance with
            | Some inst -> inst.TryRegisterEffect effect
            | _ -> None
        )

    let tryRegisterRaytracingEffect (effect : RaytracingEffect) =
        lock lockObj (fun _ ->
            match instance with
            | Some inst -> inst.TryRegisterRaytracingEffect effect
            | _ -> None
        )

    let isInitialized() =
        lock lockObj (fun _ ->
            instance.IsSome
        )