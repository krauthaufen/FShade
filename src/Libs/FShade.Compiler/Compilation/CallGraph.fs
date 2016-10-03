namespace FShade.Compiler

//TODO: add documentation

[<AutoOpen>]
module CallGraph =
    open System
    open System.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection
    open Aardvark.Base
    open FShade.Utils

    [<NoComparison>]
    type CallGraph = { self : Function; code : string; mutable called : list<CallGraph> }


    let compileFunction (name : string) (args : list<Var>) (body : Expr) =
        compile {
            let! body = sequentialVariableNames args body
            
            

            let! args = args |> List.mapC (fun a -> 
                compile { 
                    let! argumentType = compileType a.Type
                    let passingStyle = if a.IsMutable then ReferenceArgument else ValueArgument

                    return (argumentType,a.Name,None, passingStyle)
                })

            let! b = compileExpression true true body
            let! t = compileType body.Type

            return! compileFunctionDeclaration t name args b
        }

    let rec compileLambdas() =
        compile {
            let! l = lambdas

            if HashMap.isEmpty l then
                return "",""
            else
                let grouped = l |> HashMap.toSeq |> Seq.groupBy (fun (b, id) -> b.Type)
                                |> Seq.map (fun (t,b) ->
                                    let arg,ret = FSharpTypeExt.GetFunctionElements(t)
                                    let bodies = b |> Seq.map (fun (u,id) -> (id,u)) |> Map.ofSeq
                                    (t, { inputType = arg; returnType = ret; bodies = bodies })
                                ) |> HashMap.ofSeq

                let map = { closures = grouped }

                let! closures = grouped |> HashMap.toSeq |> Seq.mapC (fun (_,c) -> compileClosureDefinition c map)
                let closures = closures |> Seq.toList

                let definitions = closures |> List.map (fun (d,_,_) -> d) |> String.concat "\r\n"
                let extractors = closures |> List.map (fun (_,e,_) -> e) |> String.concat "\r\n"
                let creators = closures |> List.map (fun (_,_,c) -> c) |> String.concat "\r\n"
                
                let closures = sprintf "%s\r\n%s\r\n%s" definitions creators extractors

                let! disp = compileDispatcherForest map

                let dispDeclarations = disp |> List.map fst |> String.concat "\r\n"
                let dispImpls = disp |> List.map snd |> String.concat "\r\n"

                let! l' = lambdas

                let declarations = sprintf "%s\r\n%s" closures dispDeclarations

                //TODO: find a better way for this fixpoint search
                //      also used types and functions will not be correct
                if (HashMap.toArray l').Length <> (HashMap.toArray l).Length then
                    return! compileLambdas()
                else
                    do! resetLambdas()
                    return (declarations, dispImpls)
        }

    let compileMethod (mi : MethodBase) (name : Option<string>) =
        compile {
            match mi with
                | :? CustomMethod -> return ""
                | _ -> 
                    let! name = match name with
                                | None -> getMethodName mi
                                | Some n -> compile { return n }

                    let! result = match Expr.TryGetReflectedDefinition mi with
                                    | Some(MethodLambda(args,body)) -> 
                                        compile {
                                            let args, body =
                                                if not mi.IsStatic then
                                                    let this = args |> List.head
                                                    let newThis = Var(this.Name, this.Type, true)
                                                    let body = body.Substitute(fun vi -> if vi = this then Some (Expr.Var newThis) else None)
                                                    newThis::(List.tail args), body
                                                else
                                                    args, body


                                            let! body = processFunctionBody body
                                            return! compileFunction name args body
                                        }
                                    | Some(_) -> error "invalid reflected definition for %A" mi.Name
                                    | None -> error "no reflected definition for %A" mi.Name
                    let! s = usedFunctions

                    return result
        }

    let rec private buildCallGraphInternal (scope : System.Collections.Generic.Dictionary<Function, CallGraph>) (f : list<Option<string> * Function>) =
        compile {
            return! f |> List.mapC(fun (name,f) ->
                compile {
                    match scope.TryGetValue f with
                        | (true, cg) -> return cg
                        | _ ->  do! putUsedFunctions PersistentHashSet.empty
                                let! cg = compile {
                                             match f with
                                                | MethodFunction mi -> 
                                                   let! c = compileMethod mi name
                                                   return { self = f; code = c; called = [] }
                                       
                                                | SpecialFunction(id, args, body) -> 
                                                   let name = match name with | Some n -> n | _ -> getSpecialFunctionName id
                                                   let! body = processFunctionBody body
                                                   let! c = compileFunction name args body
                                                   return { self = f; code = c; called = [] }
                                          }
                                let! called = usedFunctions
                                scope.Add(f, cg)
                                let! calledGraphs = called |> PersistentHashSet.toSeq |> Seq.mapC(fun c -> buildCallGraphInternal scope [None,c])
                                cg.called <- List.concat [calledGraphs |> Seq.concat |> Seq.toList; cg.called]

                                return cg
                    }
                )
            }

    let buildCallGraph (mi : list<Option<string> * Function>) =
        buildCallGraphInternal (System.Collections.Generic.Dictionary()) mi

    let rec private visitCallGraphInternal (visited : System.Collections.Generic.HashSet<Function>) (cg : list<CallGraph>) =
        seq {
            for cg in cg do
                yield! visitCallGraphInternal visited cg.called

                if not <| visited.Contains cg.self then
                    visited.Add cg.self |> ignore
                    yield cg.code
        }

    let visitCallGraph (cg : list<CallGraph>) =
        visitCallGraphInternal (System.Collections.Generic.HashSet()) cg |> Seq.toList