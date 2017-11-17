namespace FShade

open Aardvark.Base
open Aardvark.Base.Incremental
open FShade.Debug
open System.IO
open System.Collections.Concurrent
open Microsoft.FSharp.Quotations

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

        let getLeafs (e : Effect) =
            match e.ComposedOf with
                | [] -> [e]
                | l -> l

        let md5 = System.Security.Cryptography.MD5.Create()

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

    let mutable private active = false
    let private registered = Dict<string, Effect * ModRef<Effect>>()
    let private lockObj = obj()

    let private sem = new System.Threading.SemaphoreSlim(0)
    let private queue = ConcurrentHashQueue<string>()
    let private compiledContent = Dict<string, string>()

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

    let private register (e : Effect) =
        lock lockObj (fun () ->
            if active then
                match tryGetName e with
                    | Some name ->
                        match registered.TryGetValue name with
                            | (true, (o,evt)) -> 
                                evt :> IMod<_>
                            | _ -> 
                                match CodeGenerator.tryGetCode e with
                                    | Some code ->
                                        let evt = Mod.init e
                                        registered.[name] <- (e, evt)

                                        let fileName = name + ".fsx"
                                        let filePath = Path.Combine(EffectCompiler.debugDir, fileName)

                                        File.WriteAllText(filePath, code)
                                        updateCompiled filePath code id
                                        changed filePath

                                        evt :> IMod<_>
                        
                                    | None ->
                                        Mod.constant e
                    | None ->
                        Mod.constant e
            else
                Mod.constant e
        )

    let attach() =
        lock lockObj (fun () ->
            if not active then
                EffectCompiler.init()
                active <- true
            
//
//                for f in Directory.GetFiles(EffectCompiler.debugDir, "*.fsx") do
////                    let repoFile = Path.Combine(EffectCompiler.repoFolder, Path.GetFileName f)
////                    if File.Exists repoFile then
////                        compiledContent.[f] <- File.ReadAllText repoFile
//                    changed f

                thread.Start()

                let w = new FileSystemWatcher(EffectCompiler.debugDir, "*.fsx")
                w.Created.Add (fun e ->
                    changed e.FullPath
                )

                w.Changed.Add (fun e ->
                    changed e.FullPath
                )

                w.Renamed.Add (fun e ->
                    changed e.FullPath
                )

                w.EnableRaisingEvents <- true

                EffectDebugger.registerFun <- Some (fun e -> register e :> obj)

        )

