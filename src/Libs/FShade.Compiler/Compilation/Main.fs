namespace FShade.Compiler

//TODO: add documentation

[<AutoOpen>]
module Main =
    open System
    open System.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection
    open Aardvark.Base

    let run (c : ICompiler<'s>) (r : Compiled<'a, 's>) =
        match r.runCompile (emptyCompilerState(c)) with
            | Error e -> Error e
            | Success(_,v) -> Success (v)

    let runState (c : ICompiler<'s>) (r : Compiled<'a, 's>) =
        match r.runCompile (emptyCompilerState(c)) with
            | Error e -> Error e
            | Success(s,v) -> Success (s.userState, v)


    let compileVariableDeclaration (t : Type) (name : string) =
        compile {
            match t with
                | FixedArrayType(d,e) ->
                    let! t = compileType e
                    let d = d.ToString() |> Some
                    return! compileVariableDeclaration t name d
                | _ ->
                    let! t = compileType t
                    return! compileVariableDeclaration t name None
        }

    let compileMain' (args : list<Var>) (body : Expr) (name : string) =
        compile {
            let! cg = buildCallGraph [(Some name), SpecialFunction(-1, args, body)]

            let! types = usedTypes
            let! constants = usedConstants
            let! defines = usedDefines
            let! tgs = buildTypeGraph types
            
            let! preamble = compilePreamble

            let typeCode = String.concat "\r\n" (visitTypeGraphs tgs)
            let funCode = String.concat "\r\n" (visitCallGraph cg)
            

            let! constants = constants |> HashMap.toSeq |> Seq.mapC (fun (u,v) -> compileConstantDeclaration v.Type v.Name u)
            let defines = defines |> Seq.map (fun (KeyValue(n,v)) -> sprintf "#define %s %s" n v) |> String.concat "\r\n"
            
            let! (def,disp) = compileLambdas()

            let c = defines + "\r\n" + typeCode + "\r\n" + def + "\r\n//__AFTERTYPES__\r\n" + (String.concat "\r\n" constants) + "\r\n//__AFTERCONSTANTS__\r\n\r\n" + preamble + "\r\n\r\n" + disp + "\r\n" + funCode
            return! processCode c
        } 

    let compileTypes (types : PersistentHashSet<Type>) =
        compile {
            let! tgs = buildTypeGraph types
            let typeCode = String.concat "\r\n" (visitTypeGraphs tgs)

            return typeCode
        }

    let compileMainWithoutTypes (args : list<Var>) (body : Expr) (name : string) =
        compile {
            let! cg = buildCallGraph [(Some name), SpecialFunction(-1, args, body)]
            let cg = cg |> List.head

            let! types = usedTypes
            let! constants = usedConstants
            let! defines = usedDefines
            let! preamble = compilePreamble

            let funCode = String.concat "\r\n" (visitCallGraph cg.called)
            let self = cg.code

            let! constants = constants |> HashMap.toSeq |> Seq.mapC (fun (u,v) -> compileConstantDeclaration v.Type v.Name u)
            let defines = defines |> Seq.map (fun (KeyValue(n,v)) -> sprintf "#define %s %s" n v) |> String.concat "\r\n"
            
            let c = defines + "\r\n" + (String.concat "\r\n" constants) + "\r\n//__AFTERCONSTANTS__\r\n\r\n" + preamble + "\r\n\r\n" + funCode
            let! c = processCode c
            let! self = processCode self

            return (c, self)
        } 


    let compileEntries (functions : list<string * Function>) =
        compile {
            let functions = functions |> List.map (fun (k,v) -> (Some k),v)
            let! cg = buildCallGraph functions

            let! types = usedTypes
            let! constants = usedConstants
            let! defines = usedDefines
            let! tgs = buildTypeGraph types
            
            let! preamble = compilePreamble

            let typeCode = String.concat "\r\n" (visitTypeGraphs tgs)
            let funCode = String.concat "\r\n" (visitCallGraph cg)
            

            let! constants = constants |> HashMap.toSeq |> Seq.mapC (fun (u,v) -> compileConstantDeclaration v.Type v.Name u)
            let defines = defines |> Seq.map (fun (KeyValue(n,v)) -> sprintf "#define %s %s" n v) |> String.concat "\r\n"
            
            let c = defines + "\r\n" + typeCode + "\r\n//__AFTERTYPES__\r\n" + (String.concat "\r\n" constants) + "\r\n//__AFTERCONSTANTS__\r\n\r\n" + preamble + "\r\n\r\n" + funCode
            return! processCode c
        } 