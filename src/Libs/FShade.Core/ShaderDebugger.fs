namespace FShade

open System
open FSharp.Data.Adaptive
open Aardvark.Base

#nowarn "44"

module ShaderDebugger =

    type IShaderDebugger =
        inherit IDisposable
        abstract member TryRegisterEffect : Effect -> aval<Effect> option
        abstract member TryRegisterRaytracingEffect : RaytracingEffect -> aval<RaytracingEffect> option
        abstract member TryRegisterComputeShader : ComputeShader -> aval<ComputeShader> option

    let private lockObj = obj()
    let mutable private instance : Option<IShaderDebugger> = None

    let initialize (f : unit -> IShaderDebugger) : IDisposable =
        lock lockObj (fun _ ->
            match instance with
            | Some _ -> Disposable.empty
            | _ ->
                let inst = f()
                instance <- Some inst

                { new IDisposable with
                    member x.Dispose() =
                        lock lockObj (fun _ ->
                            instance <- None
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

    let tryRegisterComputeShader (shader : ComputeShader) =
        lock lockObj (fun _ ->
            match instance with
            | Some inst -> inst.TryRegisterComputeShader shader
            | _ -> None
        )

    let isInitialized() =
        lock lockObj (fun _ ->
            instance.IsSome
        )