module Cecil

open Mono.Cecil
open System.IO
open System.Reflection
open System.Runtime.Loader

let private assemblyDefCache = System.Collections.Generic.Dictionary<string, option<string * AssemblyDefinition>>()

let read (path : string) (par : option<ReaderParameters>) =
    //let data = File.ReadAllBytes path
    //let ms = new MemoryStream(data)
    match par with
    | Some par -> AssemblyDefinition.ReadAssembly(path, par)
    | None -> AssemblyDefinition.ReadAssembly(path)


let resolveAssembly (dirs : list<string>) (par : option<ReaderParameters>) (assName : AssemblyNameReference) =
    let name = assName.Name
    let exts = [".dll"; ".exe"]

    let def =
        dirs |> List.tryPick (fun dir ->
            exts |> List.tryPick (fun ext ->
                let p = Path.Combine(dir, name + ext)
                if File.Exists p then
                    match assemblyDefCache.TryGetValue p with
                    | (true, def) -> def
                    | _ ->
                        let result =
                            try
                                let ass = read p par

                                if assName.Name = ass.Name.Name then
                                    Some (p, ass)
                                else
                                    None
                            with e ->
                                Log.warn "load of %A failed: %A" p e
                                None
                        assemblyDefCache.[p] <- result
                        result
                else
                    None
            )
        )

    match def with
    | Some d ->
        Some d
    | None ->
        let ctx = AssemblyLoadContext("tmp", true)
        try
            try
                let ass = ctx.LoadFromAssemblyName (AssemblyName assName.Name)
                if isNull ass then
                    None
                else
                    let path = ass.Location
                    let def = read path par
                    assemblyDefCache.[path] <- Some (path, def)
                    Some (path, def)
            with _ ->
                None
        finally
            ctx.Unload()

let readAssembly (par : option<ReaderParameters>) (path : string) =
    match assemblyDefCache.TryGetValue path with
    | (true, (Some (_, ass))) -> ass
    | _ ->
        let ass = read path par
        assemblyDefCache.[path] <- Some (path, ass)
        ass

let readerParams (dirs : list<string>) =

    let r = ReaderParameters()

    r.AssemblyResolver <-
        { new IAssemblyResolver with
            member x.Resolve(name) =
                match resolveAssembly dirs None name with
                | Some (_, a) -> a
                | None -> null
            member x.Resolve(name, p) =
                match resolveAssembly dirs (Some p) name with
                | Some (_, a) -> a
                | None -> null
            member x.Dispose() =
                ()
        }

    //if symbols then
    //    r.SymbolReaderProvider <- Mono.Cecil.Pdb.PdbReaderProvider()
    //    r.SymbolStream <- pdbStream
    //    r.ReadSymbols <- symbols
    r