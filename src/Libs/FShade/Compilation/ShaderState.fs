namespace FShade

open Microsoft.FSharp.Quotations
open Aardvark.Base
open FShade.Utils
open FShade.Compiler


[<AutoOpen>]
module ShaderState =

    [<NoComparison>]
    type ShaderState = { inputs : Map<string, Var>; outputs : Map<string, Option<string> * Var>; uniforms : Map<Unique<Uniform>, Var>; builder : Option<Expr>; counters : Map<string, int> }

    let addInput n i =
        modifyCompilerState(fun (s : ShaderState) -> { s with inputs = Map.add n i s.inputs })

    let addOutput n i =
        modifyCompilerState(fun (s : ShaderState) -> { s with outputs = Map.add n i s.outputs })

    let addUniform n i =
        modifyCompilerState(fun (s : ShaderState) -> { s with  uniforms = Map.add n i s.uniforms })

    
    let setBuilder b =
        modifyCompilerState(fun s -> 
            match s.builder with
                | None -> { s with builder = Some b }
                | Some _ -> s
        )

    let emptyShaderState = { inputs = Map.empty; outputs = Map.empty; uniforms = Map.empty; builder = None; counters = Map.empty }





    let transform = CompilerBuilder()

    let nextCounter (counterName : string) =
        transform {
            let! s = compilerState
            let current = 
                match Map.tryFind counterName s.counters with
                    | Some c -> c
                    | None -> 0

            do! putCompilerState { s with counters = Map.add counterName (current + 1) s.counters }
            return current
        }

    let resetCounter (counterName : string) =
        transform {
            let! s = compilerState
            do! putCompilerState { s with counters = Map.remove counterName s.counters }
        }

    let getInput t sem =
        transform {
            let! s = compilerState
            match Map.tryFind sem s.inputs with
                | None -> let v = Var(sem, t)
                          do! addInput sem v
                          return v
                | Some v -> return v
        }

    let getOutput t sem target =
        transform {
            let! s = compilerState
            match Map.tryFind sem s.outputs with
                | None -> let v = Var(sem + "Out", t)
                          do! addOutput sem (target, v)
                          return v
                | Some (_,v) -> return v
        }

    let rec getUniform uniform =
        transform {
            let! s = compilerState
            

            let u = Unique(uniform)
            match Map.tryFind u s.uniforms with
                | None -> match uniform with
                            | Attribute(_,t,n) -> let v = Var(n, t)
                                                  do! addUniform u v
                                                  return v
                            | UserUniform(t,e) -> 
                                let! res = getUserUniform t e
                                do! addUniform u res
                                return res

                            | SamplerUniform(t,sem, n,sam) ->
                                let v = Var(n, t)
                                do! addUniform u v
                                return v

                | Some v -> return v
        }
