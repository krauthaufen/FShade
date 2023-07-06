namespace FShade.Debug

open System.IO
open System.Collections.Generic
open Aardvark.Base

module internal FileWatchers =

    let private watchers = Dictionary<string, FileSystemWatcher>()

    let install (callback : string -> unit) (project : ProjectData) =
        let filesToWatch =
            project.Files
            |> List.groupBy Path.GetDirectoryName
            |> List.map (fun (dir, files) -> dir, Set.ofList files)

        for (dir, files) in filesToWatch do
            let watcher =
                match watchers.TryGetValue dir with
                | (true, w) -> w
                | _ ->
                    let w = new FileSystemWatcher(dir)
                    watchers.[dir] <- w
                    w

            watcher.Changed.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)
            watcher.Renamed.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)
            watcher.Created.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)

    // Enables the file watchers for the given project.
    // On Linux there is a limit on how many file watcher can be active so we only activate them on demand.
    // See: https://travisgosselin.com/configured-user-limit-inotify-instances/
    let activate (project : ProjectData) =
        let directories =
            project.Files
            |> List.map Path.GetDirectoryName
            |> List.distinct

        for dir in directories do
            match watchers.TryGetValue dir with
            | (true, w) ->
                try
                    w.EnableRaisingEvents <- true
                with exn ->
                    Log.warn "[FShade] Could not enable file watcher for directory '%s': %s" dir exn.Message
            | _ ->
                Log.warn "[FShade] File watcher for directory '%s' not found" dir

    let dispose() =
        for w in watchers.Values do w.Dispose()
        watchers.Clear()