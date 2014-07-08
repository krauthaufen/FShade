namespace FShade

open Microsoft.FSharp.Quotations
open Aardvark.Base
open FShade.Utils
open FShade.Compiler


[<AutoOpen>]
module ShaderState =

    [<NoComparison>]
    type ShaderState = { inputs : Map<string, Var>; outputs : Map<string, Option<string> * Var>; currentUniformIndex : int; uniforms : Map<Unique<Uniform>, Var>; builder : Option<Expr> }

    let addInput n i =
        modifyCompilerState(fun s -> { inputs = Map.add n i s.inputs; outputs = s.outputs; currentUniformIndex = s.currentUniformIndex; uniforms = s.uniforms; builder = s.builder })

    let addOutput n i =
        modifyCompilerState(fun s -> { inputs = s.inputs; outputs = Map.add n i s.outputs; currentUniformIndex = s.currentUniformIndex; uniforms = s.uniforms; builder = s.builder })

    let addUniform n i =
        modifyCompilerState(fun s -> { inputs = s.inputs; outputs = s.outputs; currentUniformIndex = s.currentUniformIndex; uniforms = Map.add n i s.uniforms; builder = s.builder })

    let addAnonymousUniform n i =
        modifyCompilerState(fun s -> { inputs = s.inputs; outputs = s.outputs; currentUniformIndex = s.currentUniformIndex + 1; uniforms = Map.add n i s.uniforms; builder = s.builder })

    let setBuilder b =
        modifyCompilerState(fun s -> 
            match s.builder with
                | None -> { inputs = s.inputs; outputs = s.outputs; currentUniformIndex = s.currentUniformIndex; uniforms = s.uniforms; builder = Some b }
                | Some _ -> s
        )

    let emptyShaderState = { inputs = Map.empty; outputs = Map.empty; currentUniformIndex = 0; uniforms = Map.empty; builder = None }



    let transform = CompilerBuilder()

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
                            | UserUniform(t,e) -> let name = sprintf "userUniform%d" s.currentUniformIndex
                                                  let v = Var(name, t)
                                                  do! addAnonymousUniform u v
                                                  return v
                            | SamplerUniform(t,sem, n,sam) ->
                                let v = Var(n, t)
                                do! addUniform u v
                                return v

                | Some v -> return v
        }
