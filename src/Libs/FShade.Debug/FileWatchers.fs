namespace FShade.Debug

open System.IO
open System.Collections.Generic

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
                    w.EnableRaisingEvents <- true
                    watchers.[dir] <- w
                    w

            watcher.Changed.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)
            watcher.Renamed.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)
            watcher.Created.Add (fun e -> if Set.contains e.FullPath files then callback e.FullPath)

    let dispose() =
        for w in watchers.Values do w.Dispose()
        watchers.Clear()