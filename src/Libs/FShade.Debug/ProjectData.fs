namespace FShade.Debug

open System.IO
open System.Xml
open System.Collections.Generic
open Aardvark.Base

type internal ProjectData =
    {
        // Path to the project file
        Path : string

        // Source files of the project
        Files : string list

        // Target framework
        Framework : string

        // Direct and transitive project references
        References : Set<string>
    }

    member x.Name =
        Path.GetFileNameWithoutExtension x.Path

module internal ProjectData =

    let private cache = Dictionary<string, ProjectData>()

    let rec tryLoad (path : string) =
        match cache.TryGetValue path with
        | (true, result) ->
            Result.Ok result

        | _ ->
            let name = Path.GetFileName path

            if File.Exists path then
                try
                    let xml = new XmlDocument()
                    xml.Load path

                    let rootDir = Path.GetDirectoryName path

                    let getFilePaths (xpath : string) =
                        xml.SelectNodes xpath
                        |> Seq.cast<XmlAttribute>
                        |> Seq.map (fun attr -> Path.GetFullPath <| Path.Combine(rootDir, attr.Value))
                        |> Seq.toList

                    let files = getFilePaths "//Compile/@Include"
                    let references = getFilePaths "//ProjectReference/@Include"

                    let targets =
                        let multi = xml.SelectSingleNode("//TargetFrameworks/text()")

                        if isNull multi then
                            let single = xml.SelectSingleNode("//TargetFramework/text()")

                            if isNull single then
                                []
                            else
                                [ single.Value ]
                        else
                            List.ofArray <| multi.Value.Split(';')

                    let target =
                        targets |> List.choose (fun t ->
                             match Framework.tryParse t with
                             | Some f when Framework.isSupported f ->
                                 Some (t, f)
                             | _ ->
                                 None
                        )
                        |> List.sortByDescending snd
                        |> List.tryHead

                    match target, files, references with
                    | None, [], [] ->
                        Result.Error [ $"{name}: Could not parse project file." ]

                    | None, _, _ ->
                        Result.Error [ $"{name}: Could not determine compatible target framework (candidates: {targets})." ]

                    | Some (t, _ ), _, _ ->
                        let children =
                            references |> List.map tryLoad

                        let nestedErrors =
                            children
                            |> List.choose (function
                                | Result.Error error -> Some error
                                | _ -> None
                            )
                            |> List.concat

                        if nestedErrors.IsEmpty then
                            let indirectReferences =
                                children |> List.map (function
                                    | Result.Ok c -> c.References
                                    | _ -> Set.empty
                                )
                                |> Set.unionMany

                            let result =
                                { Path       = path
                                  Files      = files
                                  Framework  = t
                                  References = Set.union (Set.ofList references) indirectReferences }

                            cache.[path] <- result
                            Result.Ok result
                        else
                            let errors = List.distinct nestedErrors
                            Result.Error errors

                with exn ->
                    Result.Error [ $"{name}: {exn.Message}" ]
            else
                Result.Error [ $"{name}: File does not exist." ]