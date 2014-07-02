namespace FShade.Compiler

[<AutoOpen>]
module Main =
    open System
    open System.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection
    open Aardvark.Base

    [<NoComparison>]
    type TypeGraph = { self : Type; code : string; mutable used : list<TypeGraph> }


    let private allFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance

    let compileTypeDefinition (t : Type) : Compiled<string, 's> =
        compile {
            let! fieldInfo = if FSharpType.IsTuple t then
                                let fieldTypes = FSharpType.GetTupleElements t |> Seq.filter(fun pi -> pi <> typeof<unit>) |> Seq.toList

                                fieldTypes |> List.mapCi (fun i t -> 
                                    compile {
                                        let! t = compileType t
                                        let name = getTupleField i

                                        return { name = name; fieldType = t; arraySize = None; info = None }
                                        //return (t, name, None, ValueArgument)
                                    })

                             elif FSharpType.IsRecord t then
                                let fields = FSharpType.GetRecordFields t |> Seq.filter(fun pi -> pi.PropertyType <> typeof<unit>) |> Seq.toList

                                fields |> List.mapC (fun pi -> 
                                    compile {
                                        let! t = compileType pi.PropertyType
                                        let name = pi.Name
                                        return { name = name; fieldType = t; arraySize = None; info = Some (pi :> MemberInfo) }
                                        //return (t, name, None, ValueArgument)
                                    })

                             elif FSharpType.IsUnion t then
                                let fields = getAllUnionFields t |> Seq.filter(fun (t,_) -> t <> typeof<unit>) |> Seq.toList
                                compile {
                                    let! intType = compileType typeof<int>
                                    let! c = fields |> List.mapC (fun (t,n) ->
                                                        compile {
                                                            let! t = compileType t
                                                            let name = n

                                                            return { name = name; fieldType = t; arraySize = None; info = None }
                                                            //return (t,name, None, ValueArgument)
                                                        })

                                    let tagField = { name = "tag"; fieldType = intType; arraySize = None; info = None}
                                    return List.Cons(tagField,c)
                                }

                             else
                                let fields = t.GetFields allFlags |> Seq.toList
                                let fields = fields |> List.sortBy (fun fi -> System.Runtime.InteropServices.Marshal.OffsetOf(t, fi.Name))
                                fields |> List.mapC (fun fi -> 
                                    compile {
                                        let! t = compileType fi.FieldType
                                        let name = fi.Name

                                        return { name = name; fieldType = t; arraySize = None; info = Some (fi :> MemberInfo) }
                                        //return (t, name, None, ValueArgument)
                                    })


            let! fieldDecls = fieldInfo |> List.mapC (fun f ->  compileFieldDeclaration f)
            let! n = compileType t
            let! def = compileTypeDeclaration n (fieldDecls |> Seq.toList)

            if FSharpType.IsUnion t then
                
                let ctors = FSharpType.GetUnionCases t |> Seq.map (fun c -> (getUnionCtorName n c.Name, c.Tag, getCaseFields c)) |> Seq.toList

                let typeName = n

                let! ctors = ctors |> List.mapC (fun (name, tag, fields) ->
                                    compile {
                                        let setters = fields |> List.map (fun (_,name) -> sprintf "result.%s = %s;" name name)
                                        let body = sprintf "%s result;\r\nresult.tag = %d;\r\n%s\r\nreturn result;\r\n" n tag (String.concat "\r\n" setters)
                                        let! args = fields |> List.mapC (fun (t,n) ->
                                                        compile {
                                                            let! fieldType = compileType t
                                                            return (fieldType, n, None, ValueArgument)
                                                        })
                                        return! compileFunctionDeclaration typeName name args body
                                    }
                             )

     
                return def + "\r\n" + (String.concat "\r\n" ctors)

            else
                let setters = fieldInfo |> List.map (fun { name = name;  } -> sprintf "result.%s = %s;" name name)
                let body = sprintf "%s result;\r\n%s\r\nreturn result;\r\n" n (String.concat "\r\n" setters)
                let ctorName = getCtorName n

                let args = fieldInfo |> List.map (fun f -> (f.fieldType, f.name, f.arraySize, ValueArgument))

                let! ctor = compileFunctionDeclaration n ctorName args body

                return def + "\r\n" + ctor
        }
    
    let rec private buildTypeGraphInternal (scope : System.Collections.Generic.Dictionary<Type, TypeGraph>) (t : Type) =
        compile {
            
            match scope.TryGetValue t with
                | (true, tg) -> return tg
                | _ -> do! putUsedTypes Set.empty
                       let! c = compileTypeDefinition t
                       let! fieldTypes = usedTypes
                       let fieldTypes = Set.remove (Unique(t)) fieldTypes

                       let tg = { self = t; code = c; used = [] }
                       scope.Add(t, tg)

                       let! used = fieldTypes |> Seq.filter (fun t -> not <| t.Value.Name.StartsWith "FSharpFunc") |> Seq.mapC (fun t -> buildTypeGraphInternal scope t.Value)
                       tg.used <- used |> Seq.toList

                       return tg

        }

    let buildTypeGraph (s : Set<Unique<Type>>) =
        compile {
            let scope = System.Collections.Generic.Dictionary()
            let! result = s |> Seq.filter (fun t -> not <| t.Value.Name.StartsWith "FSharpFunc") |> Seq.mapC (fun u -> buildTypeGraphInternal scope u.Value)
            return result |> Seq.toList
        }

    let rec private visitTypeGraphsInternal (visited : System.Collections.Generic.HashSet<Type>) (tgs : list<TypeGraph>) =
        seq {
            for tg in tgs do
                yield! visitTypeGraphsInternal visited tg.used

                if not <| visited.Contains tg.self then
                    visited.Add tg.self |> ignore
                    yield tg.code
        }

    let visitTypeGraphs (tgs : list<TypeGraph>) =
        visitTypeGraphsInternal (System.Collections.Generic.HashSet()) tgs |> Seq.toList