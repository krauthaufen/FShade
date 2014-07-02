namespace FShade.Compiler

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Aardvark.Base

[<AutoOpen>]
module Renaming =
    type VariableRenameState = { indices : Map<string, int> }

    let private getUniqueName n : Compiled<string, VariableRenameState> = 
        { runCompile = fun s -> 

            let current = match Map.tryFind n s.userState.indices with
                            | Some i -> i
                            | None -> 0

            let userState = { indices = Map.add n (current + 1) s.userState.indices }
            let newState = { s with userState = userState}

            if current <= 0 then
                Success(newState, n)
            else
                Success(newState, sprintf "%s%d" n current)
        }

    let rec private sequentialVariableNamesInternal (e : Expr) =
        compile {
            match e with
                | Let(v,value,b) ->
                    let! n = getUniqueName v.Name
                    if n = v.Name then 
                        let! b = sequentialVariableNamesInternal b
                        return Expr.Let(v, value, b)
                    else
                        let newVar = Var(n, v.Type, v.IsMutable)
                        let b = b.Substitute(fun vi -> if vi = v then newVar |> Expr.Var |> Some else None)
                        let! b = sequentialVariableNamesInternal b
                        return Expr.Let(newVar, value, b)

                | ExprShape.ShapeCombination(o, args) -> 
                    let! args = args |> List.mapC sequentialVariableNamesInternal
                    return ExprShape.RebuildShapeCombination(o, args)
                | ExprShape.ShapeLambda(v,b) ->
                    let! b = sequentialVariableNamesInternal b
                    return Expr.Lambda(v,b)
                | _ -> return e
        }

    let private liftWithUserState (userState : 's0) (c : Compiled<'a, 's0>) : Compiled<'a, 's1> =
        { runCompile = fun s ->
            let inner = { compiler = Unchecked.defaultof<ICompiler<'s0>>
                          types = s.types
                          functions = s.functions
                          constantId = s.constantId
                          constants = s.constants
                          lambdaId = s.lambdaId
                          lambdas = s.lambdas
                          defines = s.defines
                          functionId = s.functionId
                          bound = s.bound
                          userState = userState}
            match c.runCompile inner with
                | Success(inner, v) ->
                    let resultState = { compiler = s.compiler
                                        types = inner.types
                                        functions = inner.functions
                                        constantId = inner.constantId
                                        constants = inner.constants
                                        lambdaId = inner.lambdaId
                                        lambdas = inner.lambdas
                                        defines = inner.defines
                                        functionId = inner.functionId
                                        bound = inner.bound
                                        userState = s.userState}
                    Success(resultState, v)
                | Error err -> Error err
        }

    /// <summary>
    /// F# supports variable-hinding and must therefore be translated for
    /// languages not supporting that. sequentialVariableNames applies sequential names
    /// to all colliding definitions by adding a numbered suffix.
    /// </summary>
    let sequentialVariableNames (e : Expr) =
        let m = sequentialVariableNamesInternal e
        liftWithUserState { indices = Map.empty } m