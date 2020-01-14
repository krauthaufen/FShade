namespace FShade

open Aardvark.Base
open FSharp.Data.Adaptive
open FShade.Debug
open System.IO
open System.Collections.Concurrent
open Microsoft.FSharp.Quotations
open FShade.Imperative

module EffectDebugger =
    
    [<AutoOpen>]
    module private Helpers =
        
        module List =
            let rec mapOption (f : 'a -> Option<'b>) (l : list<'a>) =
                match l with
                    | [] -> Some []
                    | h :: t ->
                        match f h with
                            | Some h ->
                                match mapOption f t with
                                    | Some t -> Some (h :: t)
                                    | None -> None
                            | None ->
                                None

        let rec getLeafs (e : Effect) =
            match e.ComposedOf with
                | [] -> [e]
                | l -> l |> List.collect getLeafs

        let tryGetName (e : Effect) =
            let leafs = getLeafs e

            let names = 
                leafs |> List.mapOption (fun e ->
                    match e.SourceDefintion with
                        | Some (e,t) ->
                            match CodeGenerator.Code.TryGetReflectedDefinition e with
                                | Some def -> Some def.name
                                | None -> None
                        | None ->
                            None
                )



            match names with
                | Some names ->
//                    let idHash = md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes e.Id) |> System.Convert.ToBase64String
//                    let idHash = idHash.Replace("=", "").Replace("/", "_").Replace("+", "_")
//
//                    let names = names @ [idHash]
                    let name = String.concat "_" names
                    Some name
                | None ->
                    None

    let private registered = Dict<string, Effect * cval<Effect>>()
    let private lockObj = obj()

    let private sem = new System.Threading.SemaphoreSlim(0)
    let private queue = ConcurrentHashQueue<string>()
    let private compiledContent = Dict<string, string>()

    
    let private md5 = System.Security.Cryptography.MD5.Create()

    let private getCompositionName (e : Effect) =
        md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes e.Id) |> System.Guid |> string


    let private updateCompiled (file : string) (code : string) (f : unit -> unit) =
        lock compiledContent (fun () ->
            match compiledContent.TryGetValue file with 
                | (true, compiled) when code = compiled -> 
                    ()
                | _ ->
                    try 
                        f()
                        compiledContent.[file] <- code
                    with _ ->
                        ()
        )

    let private compileFile (path : string) =
        lock lockObj (fun () ->
            let name = Path.GetFileNameWithoutExtension path
            match registered.TryGetValue name with
                | (true, (_,evt)) ->
                    Log.startTimed "[FShade] compile %s" name
                    match EffectCompiler.tryCompile path with
                        | Choice1Of2 e -> 
                            transact (fun () -> evt.Value <- e)
                        | Choice2Of2 errors -> 
                            for e in errors do Log.warn "%A" e
                    Log.stop()
                | _ ->
                    ()
        )

    let unknownCache = Dict<string, Effect>()
    let compositions = Dict<string, cval<list<string>> * aval<Effect>>()

    let private compChanged (file : string) =
        let name = Path.GetFileNameWithoutExtension file
        match compositions.TryGetValue name with
            | (true, (r,_)) ->
                let lines = File.ReadAllLines(file) |> Array.toList |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                transact (fun () -> r.Value <- lines)
            | _ ->
                ()


    let private run(o : obj) =
        while true do
            sem.Wait()
            // swallow duplicate changes
            System.Threading.Thread.Sleep(30)
            match queue.TryDequeue() with
                | (true, file) ->
                    let code = 
                        try File.ReadAllText file
                        with _ -> 
                            Log.warn "[FShade] could not read %A" (Path.GetFileName file)
                            System.Guid.NewGuid() |> string

                    try
                        if Path.GetExtension file = ".comp" then
                            compChanged file
                        else
                            updateCompiled file code (fun () ->
                                compileFile file
                            )
                    with _ -> 
                        Log.warn "[FShade] updating %A failed" (Path.GetFileName file)
                | _ ->
                    ()

    let private thread = new System.Threading.Thread(System.Threading.ThreadStart(run), IsBackground = true)

    let private changed (f : string) =
        if queue.Enqueue f then
            Log.line "[FShade] changed %A" (Path.GetFileName f)
            sem.Release() |> ignore

    let private tryRegister (e : Effect) =
        lock lockObj (fun () ->
            if EffectDebugger.isAttached then
                match tryGetName e with
                    | Some name ->
                        match registered.TryGetValue name with
                            | (true, (o,evt)) -> 
                                Some (name, evt :> aval<_>)
                            | _ -> 
                                match CodeGenerator.tryGetCode e with
                                    | Some code ->
                                        let evt = cval e
                                        registered.[name] <- (e, evt)

                                        let fileName = name + ".fsx"
                                        let filePath = Path.Combine(EffectCompiler.debugDir, fileName)
                                        updateCompiled filePath code id

                                        File.WriteAllText(filePath, code)
                                        Git.add EffectCompiler.debugDir fileName
                                        Git.amend EffectCompiler.debugDir


                                        changed filePath

                                        Some (name, evt :> aval<_>)
                        
                                    | None ->
                                        None
                    | None ->
                        None
            else
                None
        )

    let private register (e : Effect) =
        match tryRegister e with
            | Some (name, e) -> e
            | None -> AVal.constant e

    let private printConfig = { EffectConfig.empty with outputs = Map.ofList ["Colors", (typeof<V4d>, 0) ] }
    let private glslNames = Dict<string, string>()
    let private registerOutput (name : string) (effect : Effect) =
        glslNames.[effect.Id] <- name
        effect

    let private printOutput (effect : Effect) (code : string) =
        match glslNames.TryGetValue effect.Id with
            | (true, name) ->
                let file = Path.Combine(EffectCompiler.outDir, name + ".glsl")
                File.WriteAllText(file, code)
            | _ ->
                ()



    let private registerPiecewise (e : Effect) =
        match registered.TryGetValue e.Id with
            | (true, (_,m)) -> 
                m :> aval<_>

            | _ -> 
                let mutable names = []
                let leafs = 
                    e |> getLeafs |> List.map (fun c ->
                        match tryRegister c with
                            | Some (n, cc) ->
                                names <- names @ [n]
                                cc
                            | None ->
                                unknownCache.[c.Id] <- c
                                names <- names @ [c.Id]
                                AVal.constant c
                    )

                let mutable isNew = false

                let fileName = getCompositionName e

                let _, result = 
                    compositions.GetOrCreate(fileName, fun _ -> 
                        isNew <- true

                        let names = cval names
                        let result = 
                            names |> AVal.bind (fun names ->
                                let effects = 
                                    names |> List.choose (fun name ->
                                        match registered.TryGetValue name with
                                            | (true, (_,e)) -> Some (e :> aval<_>)
                                            | _ -> 
                                                match unknownCache.TryGetValue name with
                                                    | (true, e) -> Some (AVal.constant e)
                                                    | _ -> None
                                                
                                    )
                                AVal.custom (fun t ->
                                    effects |> List.map (fun e -> e.GetValue t) |> Effect.compose |> registerOutput fileName
                                )
                            )

                        names, result
                    )

                if isNew then
                    let file = Path.Combine(EffectCompiler.compDir, fileName + ".comp")
                    File.WriteAllLines(file, names)

                result


    let attach() =
        lock lockObj (fun () ->
            if not EffectDebugger.isAttached then
                EffectCompiler.init()
                EffectDebugger.isAttached <- true
                EffectDebugger.saveCode <- printOutput

                let compWatch = new FileSystemWatcher(EffectCompiler.compDir, "*.comp")
                compWatch.Changed.Add (fun e -> changed e.FullPath)
                compWatch.Renamed.Add (fun e -> changed e.FullPath)
                compWatch.EnableRaisingEvents <- true

                thread.Start()

                let w = new FileSystemWatcher(EffectCompiler.debugDir, "*.fsx")
                w.Created.Add (fun e -> changed e.FullPath)
                w.Changed.Add (fun e -> changed e.FullPath)
                w.Renamed.Add (fun e -> changed e.FullPath)
                w.EnableRaisingEvents <- true

                EffectDebugger.registerFun <- Some (fun e -> registerPiecewise e :> obj)

        )

