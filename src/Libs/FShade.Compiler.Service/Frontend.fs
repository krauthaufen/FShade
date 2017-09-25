namespace FShade.Compiler.Service

open System
open System.Text
open System.IO
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

open Aardvark.Base

open FShade

module Parser =
    [<AutoOpen>]
    module private Helpers = 
        type MyFileSystem() = 
            static let defaultFileSystem = Shim.FileSystem
            let files = System.Collections.Concurrent.ConcurrentDictionary<string, string>()

            member x.NewFile (code : string) =
                let id = Guid.NewGuid().ToString() + ".fsx"
                files.[id] <- code
                id

            member x.Delete (file : string) =
                files.TryRemove file |> ignore
                
            member x.SafeExists(fileName) = 
                files.ContainsKey(fileName) || defaultFileSystem.SafeExists(fileName)

            interface IFileSystem with
                // Implement the service to open files for reading and writing
                member x.FileStreamReadShim(fileName) = 
                    match files.TryGetValue(fileName) with
                    | true, text -> new MemoryStream(Encoding.UTF8.GetBytes(text)) :> Stream
                    | _ -> defaultFileSystem.FileStreamReadShim(fileName)

                member x.FileStreamCreateShim(fileName) = 
                    defaultFileSystem.FileStreamCreateShim(fileName)

                member x.FileStreamWriteExistingShim(fileName) = 
                    defaultFileSystem.FileStreamWriteExistingShim(fileName)

                member x.ReadAllBytesShim(fileName) = 
                    match files.TryGetValue(fileName) with
                    | true, text -> Encoding.UTF8.GetBytes(text)
                    | _ -> defaultFileSystem.ReadAllBytesShim(fileName)

                // Implement the service related to temporary paths and file time stamps
                member x.GetTempPathShim() = 
                    defaultFileSystem.GetTempPathShim()
                member x.GetLastWriteTimeShim(fileName) = 
                    defaultFileSystem.GetLastWriteTimeShim(fileName)
                member x.GetFullPathShim(fileName) = 
                    defaultFileSystem.GetFullPathShim(fileName)
                member x.IsInvalidPathShim(fileName) = 
                    defaultFileSystem.IsInvalidPathShim(fileName)
                member x.IsPathRootedShim(fileName) = 
                    defaultFileSystem.IsPathRootedShim(fileName)

                // Implement the service related to file existence and deletion
                member x.SafeExists(fileName) = x.SafeExists(fileName)
                member x.FileDelete(fileName) = 
                    defaultFileSystem.FileDelete(fileName)

                // Implement the service related to assembly loading, used to load type providers
                // and for F# interactive.
                member x.AssemblyLoadFrom(fileName) = 
                    defaultFileSystem.AssemblyLoadFrom fileName
                member x.AssemblyLoad(assemblyName) = 
                    defaultFileSystem.AssemblyLoad assemblyName 
    
        let fakeFS = MyFileSystem()
        do Shim.FileSystem <- fakeFS

        let checker = FSharpChecker.Create(keepAssemblyContents=true)

        let resolve (file : string) =
            let dir = Environment.CurrentDirectory
            let path = Path.Combine(dir, file)
            if File.Exists path then
                Some ("-r: " + Path.GetFullPath path )
            else
                None

        let references =
            Array.choose resolve [|
                "Aardvark.Base.dll"
                "Aardvark.Base.TypeProviders.dll"
                "Aardvark.Base.FSharp.dll"
                "Aardvark.Base.Incremental.dll"
                "FShade.Imperative.dll"
                "FShade.Core.dll"
            |]

    let parseDeclarations (code : string) =
        async {
            let file = fakeFS.NewFile code
            try
                let! fsxOptions = checker.GetProjectOptionsFromScript(file, code)

                let options = 
                    {
                        fsxOptions with
                            OtherOptions = Array.append fsxOptions.OtherOptions references
                    }

                let! result = checker.ParseAndCheckProject options
                
                if result.Errors.Length > 0 then
                    let errors =
                        result.Errors |> Seq.map string |> String.concat "\r\n"
                    return Error errors
                else
                    let content = result.AssemblyContents

                    let all = 
                        content.ImplementationFiles |> List.collect (fun c ->
                            c.Declarations |> List.collect (fun d ->
                                match d with
                                    | Entity(_,children) -> children
                                    | _ -> []
                            )
                        )

                    return Success all
            finally
                fakeFS.Delete file
        }

    let rec findTopLevelBindings (d : FSharpImplementationFileDeclaration) =
        match d with
            | Entity(e, children) ->
                if e.IsFSharpModule then
                    children |> List.collect findTopLevelBindings 
                else
                    []

            | MemberOrFunctionOrValue(binding,args,body) ->
                if binding.IsCompilerGenerated then
                    []
                else
                    [binding,args,body]

            | InitAction _ ->
                []

    let rec toType' (definition : FSharpEntity) =
        //let definition = t.TypeDefinition
        if definition.IsFSharpRecord then
            let fields = definition.FSharpFields |> Seq.toList

            let recordFields =
                fields |> List.map (fun f ->
                    let attributes = f.PropertyAttributes |> Seq.toList
                    {
                        fieldName = f.DisplayName
                        fieldType = toType' f.FieldType.TypeDefinition
                        fieldAttributes =
                            attributes |> List.map (fun a ->
                                let t = toType' a.AttributeType
                                let args = a.ConstructorArguments |> Seq.map snd |> Seq.toList
                                t, args
                            )
                    }
                ) 

            FSharpType.MakeRecord(definition.LogicalName, recordFields)
   
        elif definition.IsFSharpUnion then
            let cases = definition.UnionCases |> Seq.toList

            let unionCases =
                cases |> List.map (fun c ->
                    let fields = 
                        c.UnionCaseFields 
                            |> Seq.toList
                            |> List.map (fun f -> 
                                let t = toType' f.FieldType.TypeDefinition
                                Some f.Name, t
                            )

                    { caseName = c.Name; caseFields = fields }
                )

            FSharpType.MakeUnion(definition.LogicalName, unionCases)

        elif definition.IsFSharpAbbreviation then
            toType' definition.AbbreviatedType.TypeDefinition

        elif definition.HasAssemblyCodeRepresentation then
            failwith "built-in"

        else
            let t = Type.GetType(definition.QualifiedName)
            t
   
    let rec toType (t : FSharpType) =
        if t.TypeDefinition.HasAssemblyCodeRepresentation then
            toType t.AbbreviatedType
        else
            toType' t.TypeDefinition

    let toField (f : FSharpField) =
        let t = toType' f.DeclaringEntity
        if FSharpType.IsRecord t then
            FSharpType.GetRecordFields t |> Array.find (fun fi -> fi.Name = f.Name)
        elif FSharpType.IsUnion t then
            failwith "not implemented"
        else
            failwith "unsupported"
            


    open Microsoft.FSharp.Quotations

    module Expr = 
        open Aardvark.Base.Monads.State

        type TranslationState =
            {
                this : Option<Var>
                variables : Map<string, Var>
            }   

        let this = State.get |> State.map (fun s -> s.this)
        let putVar (v : Var) = State.modify (fun s -> { s with variables = Map.add v.Name v s.variables })
        
        let getVar (name : string) = State.get |> State.map (fun s -> Map.tryFind name s.variables)

        let rec ofValueS (v : FSharpMemberOrFunctionOrValue) =
            state {
                match v.LiteralValue with
                    | Some value -> 
                        return Expr.Value(value, toType v.FullType)
                
                    | None ->
                        let! var = getVar v.LogicalName
                        match var with
                            | Some v -> return Expr.Var v
                            | _ ->
                                return failwith ""
            }

        let rec toMethod (m : FSharpMemberOrFunctionOrValue) (genArgs1 : list<Type>) (genArgs2 : list<Type>) =
            let name = m.LogicalName
            let t = toType' m.EnclosingEntity
            let mi = t.GetMethod(name, BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static)

            if mi.IsGenericMethod then
                mi.MakeGenericMethod (List.toArray genArgs2)
            else
                mi
        let rec ofFSharpExprS (e : FSharpExpr) =
            state {
                match e with
                    | BasicPatterns.AddressOf(e) ->
                        let! e = ofFSharpExprS e
                        return Expr.AddressOf(e)

                    | BasicPatterns.AddressSet(a,v) ->
                        let! a = ofFSharpExprS a
                        let! v = ofFSharpExprS v
                        return Expr.AddressSet(a, v)
                
                    | BasicPatterns.Application(lambda, _, args) ->
                        let! lambda = ofFSharpExprS lambda
                        let! args = args |> List.mapS (ofFSharpExprS >> State.map List.singleton)
                        return Expr.Applications(lambda, args)
                
                    | BasicPatterns.BaseValue(a) ->
                        let t = toType a
                        let! self = this
                        match self with
                            | Some self -> return Expr.Coerce(Expr.Var self, t)
                            | None -> return failwith "[Expr] cannot resolve base-value"

                    | BasicPatterns.Call(target, meth, genArgs1, genArgs2, args) ->
                        
                        let genArgs1 = List.map toType genArgs1
                        let genArgs2 = List.map toType genArgs2
                        let mi = toMethod meth genArgs1 genArgs2
                        let! args = args |> List.mapS ofFSharpExprS
                        match target with
                            | Some t ->
                                let! t = ofFSharpExprS t
                                return Expr.Call(t, mi, args)
                            | None ->
                                return Expr.Call(mi, args)

                    | BasicPatterns.Coerce(t, e) ->
                        let t = toType t
                        let! e = ofFSharpExprS e
                        return Expr.Coerce(e, t)

                    | BasicPatterns.Const(v,t) ->
                        let t = toType t
                        return Expr.Value(v, t)

                    | BasicPatterns.DecisionTree(arg, cases) ->
                        return failwith ""

                    | BasicPatterns.DecisionTreeSuccess(arg, cases) ->
                        return failwith ""

                    | BasicPatterns.DefaultValue(t) ->
                        let t = toType t
                        return Expr.DefaultValue(t)

                    | BasicPatterns.FSharpFieldGet(target, targetType, field) ->
                        let field = toField field
                        match target with
                            | Some t ->
                                let! t = ofFSharpExprS t
                                return Expr.PropertyGet(t, field)
                            | None ->
                                return Expr.PropertyGet(field)
                        
                    | BasicPatterns.FSharpFieldSet(target, targetType, field, value) ->
                        let field = toField field
                        let! value = ofFSharpExprS value
                        match target with
                            | Some t ->
                                let! t = ofFSharpExprS t
                                return Expr.PropertySet(t, field, value)
                            | None ->
                                return Expr.PropertySet(field, value)

                    | BasicPatterns.FastIntegerForLoop(first, last, BasicPatterns.Lambda(v, body), true) ->
                        let! first = ofFSharpExprS first
                        let! last = ofFSharpExprS last
                        let var = Var(v.DisplayName, toType v.FullType)

                        do! putVar var
                        let! body = ofFSharpExprS body

                        return Expr.ForIntegerRangeLoop(var, first, last, body)

                    | BasicPatterns.FastIntegerForLoop _ ->
                        return failwith "not implemented"

                    | BasicPatterns.ILAsm _ ->
                        return failwith "not supported"

                    | BasicPatterns.ILFieldGet(target, targetType, field) ->
                        let targetType = toType targetType
                        let field = targetType.GetField(field, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static)

                        match target with
                            | Some t ->
                                let! t = ofFSharpExprS t
                                return Expr.FieldGet(t, field)
                            | None ->
                                return Expr.FieldGet(field)
                        
                    | BasicPatterns.ILFieldSet(target, targetType, field, value) ->
                        let! value = ofFSharpExprS value
                        let targetType = toType targetType
                        let field = targetType.GetField(field, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static)
                        
                        match target with
                            | Some t ->
                                let! t = ofFSharpExprS t
                                return Expr.FieldSet(t, field, value)
                            | None ->
                                return Expr.FieldSet(field, value)

                    | BasicPatterns.IfThenElse(cond, i, e) ->
                        let! cond = ofFSharpExprS cond
                        let! i = ofFSharpExprS i
                        let! e = ofFSharpExprS e
                        return Expr.IfThenElse(cond, i, e)


                    | BasicPatterns.Lambda(v,body) ->
                        let var = Var(v.LogicalName, toType v.FullType)
                        do! putVar var
                        let! body = ofFSharpExprS body
                        return Expr.Lambda(var, body)

                    | BasicPatterns.Let((v, e), body) ->
                        let! e = ofFSharpExprS e
                        let var = Var(v.DisplayName, toType v.FullType)
                        do! putVar var
                        let! body = ofFSharpExprS body
                        return Expr.Let(var, e, body)

                    | BasicPatterns.LetRec _ ->
                        return failwith "not implemented"

                    | BasicPatterns.NewArray(t, args) ->
                        let t = toType t
                        let! args = args |> List.mapS ofFSharpExprS
                        return Expr.NewArray(t, args)

                    | BasicPatterns.NewDelegate(t, e) ->
                        let t = toType t
                        let! e = ofFSharpExprS e
                        failwith "not implemented"
                        return Expr.NewDelegate(t, [], e)

                    | BasicPatterns.NewObject(ctor, tArgs, args) ->
                        return failwith "not implemented"

                    | BasicPatterns.NewRecord(t, args) ->
                        let t = toType t
                        let! args = args |> List.mapS ofFSharpExprS
                        return Expr.NewRecord(t, args)
                        
                    | BasicPatterns.NewTuple(_, args) ->
                        let! args = args |> List.mapS ofFSharpExprS
                        return Expr.NewTuple(args)

                    | BasicPatterns.NewUnionCase(t, ci, args) ->   
                        return failwith "not implemented"
                        
                    | BasicPatterns.ObjectExpr _ ->
                        return failwith "not supported"
                        
                    | BasicPatterns.Quote e ->
                        let! e = ofFSharpExprS e
                        return Expr.QuoteRaw e

                    | BasicPatterns.Sequential(l,r) ->
                        let! l = ofFSharpExprS l
                        let! r = ofFSharpExprS r
                        return Expr.Sequential(l,r)

                    | BasicPatterns.ThisValue _ ->
                        let! this = this
                        match this with
                            | Some v -> return Expr.Var v
                            | _ -> return failwith "[Expr] cannot resolve this-value"


                    | BasicPatterns.Value(v) ->
                        return! ofValueS v
                    

                    | _ -> 
                        return failwith ""
            }


        let ofFSharpExpr (this : Option<Var>) (vars : Map<string, Var>) (e : FSharpExpr) =
            let mutable state = { this = this; variables = vars }
            ofFSharpExprS(e).Run(&state)


    let test (str : string) =
        let decls = parseDeclarations str |> Async.RunSynchronously
        match decls with
            | Success decls ->
                let bindings = decls |> List.collect findTopLevelBindings
            
                for (binding, args, body) in bindings do
                    let args = 
                        args |> List.concat |> List.map (fun a ->
                            let t = toType a.FullType
                            Var(a.LogicalName, t)
                        )

                    let variables = args |> List.map (fun v -> v.Name, v) |> Map.ofList
                    let body = Expr.ofFSharpExpr None variables body
                    
            
                    printfn "%A %A" args body
            | Error err ->
                Log.warn "%s" err






