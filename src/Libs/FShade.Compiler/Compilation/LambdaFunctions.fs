namespace FShade.Compiler

open System
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection
open Aardvark.Base
open FShade.Utils


//TODO: add documentation

[<AutoOpen>]
module LambdaFunctions =

    [<NoComparison>]
    type Closure = { inputType : Type; returnType : Type; bodies : Map<int, Expr> }

    [<NoComparison>]
    type ClosureMap = { closures : Map<Unique<Type>, Closure> }


    let subClosure (rem : int) (c : Closure) =
        { c with bodies = Map.remove rem c.bodies }

    let getDispatcherNameForClosure (c : Closure) =
        compile {

            let! argType = compileType c.inputType
            let! retType = compileType c.returnType

            return getDispatcherName argType retType
        }

    let compileTopLevelDispatcher (parentName : string) (c : Closure) (map : ClosureMap) =
        compile {
            let! ownName = getDispatcherNameForClosure c
            let suffix = c.bodies |> Map.toSeq |> Seq.map fst |> Seq.map (sprintf "%d") |> String.concat ""

            let name = if parentName <> "" then sprintf "%s_%s" ownName suffix else ownName
                
                
            let neededDispatchers = List()

            let bodies = c.bodies |> Map.toList
            let arg = Var("arg", c.inputType)
            let replacements = Dictionary<int * Var, Expr>()
            let funType = FSharpType.MakeFunctionType(c.inputType, c.returnType)
            let closure = Expr.Var(Var("closure", funType))

            let rec getExtractor (id : int) (v : Var) =
                match replacements.TryGetValue ((id,v)) with
                    | (true, v) -> v
                    | _ -> 
                        let fieldName = sprintf "lambda%d_%s" id v.Name
                        let r = 
                            if v.Type.Name.StartsWith "FSharpFunc" then
                                let m = CustomMethod(fieldName, [|funType|], v.Type)

                                Expr.Call(m, [closure])
                            else
                                let prop = CustomProperty(closure.Type, fieldName, v.Type)
                                Expr.PropertyGet(closure, prop)

                        replacements.[(id,v)] <- r
                        r

            let functionArgs = Dictionary<int, HashSet<Var>>()

            let addFunctionArg (id : int) (v : Var) =
                match functionArgs.TryGetValue id with
                    | (true, set) -> set.Add v |> ignore
                    | _ -> let set = HashSet()
                           set.Add v |> ignore
                           functionArgs.Add(id,set)

            let getFunctionArgs (id : int) =
                match functionArgs.TryGetValue id with
                    | (true, set) -> set |> Seq.toList
                    | _ -> []

            let! bodies = bodies |> List.mapC (fun (id,f) ->
                                    compile {
                                        match f with
                                            | Lambda(v,b) ->
                                                let! free = getFreeVars b
                                                    
                                                let (argType,retType) = FSharpType.GetFunctionElements(f.Type)
                                                let subClosure = subClosure id c
                                                let! subName = getDispatcherNameForClosure subClosure
                                                let suffix = subClosure.bodies |> Map.toSeq |> Seq.map fst |> Seq.map (sprintf "%d") |> String.concat ""
                                                let subDispatcher = CustomMethod(sprintf "%s_%s" subName suffix, [|f.Type; argType|], retType)
                                                //let subDispatcher = CustomMethod(sprintf "%s_%s_%s" name subName suffix, [|f.Type; argType|], retType)
                                                let rec substituteApplications (e : Expr)  =
                                                    compile {
                                                        match e with
                                                            | Pipe(e) -> 
                                                                return! substituteApplications e
                                                            | Application(l,arg) ->
                                                                let! arg = substituteApplications arg
                                                                if l.Type = f.Type then
                                                                    neededDispatchers.Add(subClosure)
                                                                    return Expr.Call(subDispatcher, [l; arg])
                                                                else
                                                                        
                                                                    match Map.tryFind (Unique l.Type) map.closures with
                                                                        | Some inner ->
                                                                            neededDispatchers.Add(inner)
                                                                            let! subName = getDispatcherNameForClosure inner
                                                                            let suffix = inner.bodies |> Map.toSeq |> Seq.map fst |> Seq.map (sprintf "%d") |> String.concat ""
                                                                            let subDispatcher = CustomMethod(sprintf "%s_%s" subName suffix, [|l.Type; arg.Type|], e.Type)
                                                                            return Expr.Call(subDispatcher, [l; arg])
                                                                        | _ -> 
                                                                            return! error ""
                                                            | ExprShape.ShapeLambda(v,b) -> 
                                                                return e
                                                            | ExprShape.ShapeCombination(o, args) ->
                                                                let! args = args |> List.mapC substituteApplications
                                                                return ExprShape.RebuildShapeCombination(o, args)
                                                            | ExprShape.ShapeVar(v) -> return e
                                                    }

                                                let! b = substituteApplications b

                                                let mutable b = Expr.Let(v, Expr.Var arg, b)

                                                let b = free |> Seq.filter (fun f -> f <> v) |> Seq.fold (fun b f -> Expr.Let(f, getExtractor id f, b)) b

                                                    

                                                return (id,b)
                                            | _ -> return! error "sdsad"
                                    }
                             
                                    )

            let! bodiesCode = bodies |> List.mapC (fun (id, b) ->
                compile {
                    let! code = compileExpression true true b
                    return (id,code,b)
                })


            let! sw =
                compile {
                    if bodiesCode.Length = 1 then
                        let (_,c,_) = bodiesCode |> List.head
                        return c
                    else
                            
                        
                        let first = bodiesCode |> List.head |> (fun (id, s, b) -> 
                                        sprintf "else\r\n{\r\n%s\r\n}" (String.indent 1 s)
                                    )
                        let cases = bodiesCode |> Seq.skip 1 |> Seq.mapi (fun i (id, s, b) -> 
                                        if i = 0 then
                                            sprintf "if(closure.Function == %d)\r\n{\r\n%s\r\n}" id (String.indent 1 s) 
                                        else
                                            sprintf "else if(closure.Function == %d)\r\n{\r\n%s\r\n}" id (String.indent 1 s)
                                        ) |> String.concat "\r\n"

                        let cases = sprintf "%s\r\n%s" cases first

                        return cases
                }

            let! r = compileType c.returnType
            let! i = compileType c.inputType
            let! c = compileType funType
            let disp = sprintf "%s %s(%s closure, %s arg)\r\n{\r\n%s\r\n}\r\n"  r name c i (String.indent 1 sw)

            let forwardDecl = sprintf "%s %s(%s closure, %s arg);" r name c i

            return name, forwardDecl, disp, neededDispatchers |> Seq.toList
        }

    let rec compileDispatcherTree (defined : HashSet<string>) (prefix : string) (c : Closure) (map : ClosureMap)  =
        compile {
            let! (name, forwardDecl, code, needed) = compileTopLevelDispatcher prefix c map
                
            if defined.Contains name then
                return []
            else
                    
                defined.Add name |> ignore
                if needed |> List.isEmpty then
                    return [forwardDecl,code]
                else
                    //let prefix = if prefix <> "" then sprintf "%s_%s" prefix name else name
                    let! inner = needed |> List.mapC (fun inner ->
                                                
                                                compileDispatcherTree defined name inner map
                                            )
                
                    
                    let inner = inner |> List.concat
                    return List.concat [inner ; [forwardDecl,code] ]
        }
        
    let compileDispatcherForest (map : ClosureMap) =
        compile {
            let closures = map.closures |> Map.toList |> List.map snd
            let set = HashSet()

            let! closureCodes = closures |> List.mapC (fun c -> compileDispatcherTree set "" c map)

            return List.concat closureCodes
        }

    let rec getAllFieldNames (visited : Set<int>) (prefix : string) (closure : Closure) (map : ClosureMap) =
        compile {
            let bodies = closure.bodies |> Map.toList |> List.filter (fun (id,_) -> not <| visited.Contains id)

            let! free = bodies |> List.mapC (fun (id,b) -> compile { let! free = getFreeVars b in return (id,free) })

            let! namesAndTypes =
                free |> List.collectC (fun (id,free) ->
                    compile {
                        let visited = Set.add id visited

                        let! namesAndTypes = 
                            free |> List.collectC (fun f ->
                                compile {
                                    let fieldType = f.Type
                                    let fieldName = if prefix = "" then sprintf "lambda%d_%s" id f.Name else sprintf "%s_lambda%d_%s" prefix id f.Name

                                    if fieldType.Name.StartsWith "FSharpFunc" then
                                        match Map.tryFind (Unique fieldType) map.closures with
                                            | Some innerClosure ->
                                                return! getAllFieldNames visited fieldName innerClosure map
                                            | _ -> 
                                                return! error "could not find inner closure-type: %A" fieldType
                                    else
                                        return [(fieldName, fieldType)]
                                }
                            )
                            

                        return namesAndTypes
                    }
                )

            let funName = if prefix = "" then "Function" else sprintf "%s_Function" prefix
            return (funName, typeof<int>)::namesAndTypes

        }

    let compileAllFieldNames (prefix : string) (c : Closure) (map : ClosureMap) =
        compile {
            let! namesAndTypes = getAllFieldNames Set.empty prefix c map
            return namesAndTypes |> List.choose (fun (n,_) -> if n = "Function" then None else Some n)
        }

    let compileFieldDeclarationsForClosure (prefix : string) (c : Closure) (map : ClosureMap) =
        compile {
            let! namesAndTypes = getAllFieldNames Set.empty prefix c map
            return! namesAndTypes |> List.mapC (fun (name,t) -> compile { let! t = compileType t in return sprintf "%s %s;" t name })
        }



    let compileCreators (c : Closure)  (map : ClosureMap) =
        compile {
            let funType = FSharpType.MakeFunctionType(c.inputType, c.returnType)
            let! structName = compileType funType


            let! allFields = compileAllFieldNames "" c map
            let allFields = HashSet(allFields)

            return! c.bodies |> Map.toList |> List.mapC (fun (id,b) ->
                compile {
                    let! free = getFreeVars b
                    let lambdaName = sprintf "lambda%d" id

                    let! args = free |> Seq.mapC (fun v -> 
                                compile {
                                    let! t = compileType v.Type
                                    let n = v.Name
                                    return sprintf "%s %s" t n
                                })

                    let! copyCode = free |> Seq.mapC (fun v -> 
                                    compile {
                                            
                                        if v.Type.Name.StartsWith "FSharpFunc" then
                                            match Map.tryFind (Unique v.Type) map.closures with
                                                | Some targetClosure ->
                                                    let! copyFields = compileAllFieldNames "" targetClosure map
                                                    let copyFields = copyFields |> Seq.filter (fun f -> allFields.Contains(sprintf "%s_%s_%s" lambdaName v.Name f)) |> Seq.toList

                                                    let copy = copyFields |> List.map (fun f ->
                                                                    let argName = f
                                                                    let nestedName = sprintf "%s_%s_%s" lambdaName v.Name f
                                                                    sprintf "result.%s = %s.%s;" nestedName v.Name argName
                                                                )


    //
    //                                                let copyFields = copyFields |> List.mapC (fun f ->
    //                                                 
                                                    let copy = (sprintf "result.%s_%s_Function = %s.Function;" lambdaName v.Name v.Name)::copy

                                                    return copy |> String.concat "\r\n"
                                                | _ -> return! error "asdsadsadasd"
                                        else
                                            let! t = compileType v.Type
                                            let n = v.Name
                                            return sprintf "result.%s_%s = %s;" lambdaName n n
                                    })

                    let assigns = copyCode |> String.concat "\r\n"
                    let argDef = args |> String.concat ", "

                    return sprintf "%s makeLambda%d(%s)\r\n{\r\n    %s result;\r\n    result.Function = %d;\r\n%s\r\n    return result;\r\n}\r\n" structName id argDef structName id (String.indent 1 assigns)

                }
            )

        }

    let compileClosureDefinition (c : Closure) (map : ClosureMap) =
        compile {
            let! free = c.bodies |> Map.toSeq |> Seq.collectC (fun (id,b) -> compile { let! free = getFreeVars b in return free |> Seq.map (fun f -> id,f) })
            let allFree = HashSet(free) |> Seq.toList

            let! free = getAllFieldNames Set.empty "" c map

            let! freeDecls = free |> List.mapC (fun (n,t) -> compile { let! t = compileType t in return! compileFieldDeclaration { name = n; fieldType = t; arraySize = None; info = None }})
            let allFields = free |> List.choose (fun (n,_) -> if n = "Function" then None else Some n)

            let funType = FSharpType.MakeFunctionType(c.inputType, c.returnType)
            let! typeName = compileType funType


            let! defCode = compileTypeDeclaration typeName freeDecls
            //let defCode = sprintf "struct %s\r\n{\r\n%s\r\n};\r\n" typeName (String.indent 1 decls)



            let! extractors = c.bodies |> Seq.mapC (fun (KeyValue(id,b)) -> 
                                    compile {
                                        let! free = getFreeVars b
                                        let free = HashSet(free)
                                            
                                        let! extractors = free |> Seq.collectC (fun f ->
                                                            compile {
                                                                let name = sprintf "lambda%d_%s" id f.Name
                                                                let vt = f.Type
                                                                let! t = compileType vt

                                                                if vt.Name.StartsWith "FSharpFunc" then
                                                                    let copyFields = allFields |> List.filter (fun f -> f.StartsWith name)
                                                                    let! copyInner = copyFields |> List.mapC (fun f ->
                                                                                                        compile {
                                                                                                            let outerName = f.Substring(1 + name.Length)
                                                                                                            let innerName = f

                                                                                                            return sprintf "result.%s = closure.%s;" outerName innerName
                                                                                                        }
                                                                                                    )

                                                                    let copyInner = copyInner |> String.concat "\r\n"
                                                                    return [sprintf "%s %s(%s closure)\r\n{\r\n    %s result;\r\n%s\r\n    return result;\r\n}\r\n" t name typeName t (String.indent 1 copyInner)]
                                                                else
                                                                    return []
                                                            }
                                                            )

                                        return extractors |> String.concat "\r\n"
                                    }
                                )

            let! creators = compileCreators c map

            let extractors = extractors |> String.concat "\r\n"
            let creators = creators |> String.concat "\r\n"

            return (defCode,extractors,creators)
        }
