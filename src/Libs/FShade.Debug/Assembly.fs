namespace FShade.Debug

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Collections.Generic
open Aardvark.Base

// Utilities for probing assemblies
module internal Assembly =

    [<AutoOpen>]
    module private ProjectHelpers =

        let rec private tryFindProject (dir : string) =
            let project =
                Directory.GetFiles dir
                |> Array.tryFind (fun f ->
                    Path.GetExtension(f).ToLower() = ".fsproj"
                )

            if project.IsNone then
                let parent = Path.GetDirectoryName dir
                if not (isNull parent) then
                    tryFindProject parent
                else
                    None
            else
                project

        let tryFindProjectLocation (assemblyPath : string) : option<string> =
            let pdbPath = Path.ChangeExtension(assemblyPath, ".pdb")

            if File.Exists pdbPath then
                try
                    use file = File.OpenRead pdbPath
                    use provider = MetadataReaderProvider.FromPortablePdbStream(file, MetadataStreamOptions.PrefetchMetadata)
                    let reader = provider.GetMetadataReader()

                    reader.Documents |> Seq.tryPick (fun d ->
                        if d.IsNil then None
                        else
                            try
                                let doc = reader.GetDocument d

                                if doc.Name.IsNil then None
                                else
                                    let name = reader.GetString doc.Name

                                    if File.Exists name then
                                        tryFindProject (Path.GetDirectoryName name)
                                    else
                                        None
                            with _ ->
                                None
                    )
                with _ ->
                    None
            else
                None

    let tryGetProjectData =
        let cache = Dictionary<string, ProjectData option>()

        fun (assembly : Assembly) ->
            let location =
                try Path.GetFullPath assembly.Location |> Some
                with _ -> None

            match location with
            | Some location ->
                match cache.TryGetValue location with
                | (true, data) -> data
                | _ ->
                    match tryFindProjectLocation location with
                    | Some projectFile ->
                        let result =
                            match ProjectData.tryLoad projectFile with
                            | Result.Ok data -> Some data
                            | Result.Error errors ->
                                for err in errors do
                                    Log.warn "%s" err
                                None

                        cache.[location] <- result
                        result

                    | None ->
                        None
            | None ->
                None

    let tryGetMethodName (sourceFile : string) (startLine : int) (startColumn : int) (assembly : Assembly) =
        let assemblyPath =
            try Path.GetFullPath assembly.Location |> Some
            with _ -> None

        match assemblyPath with
        | Some assemblyPath ->
            let pdbPath = Path.ChangeExtension(assemblyPath, ".pdb")

            if File.Exists pdbPath then
                try
                    use stream = File.OpenRead pdbPath
                    use provider = MetadataReaderProvider.FromPortablePdbStream(stream, MetadataStreamOptions.PrefetchMetadata)
                    let pdb = provider.GetMetadataReader()

                    use rd = new PEReader(File.OpenRead assemblyPath, PEStreamOptions.PrefetchMetadata)
                    let dll =
                        let m = rd.GetMetadata()
                        MetadataReader(m.Pointer, m.Length)

                    let rec getFullName (t : TypeDefinition) =
                          let name = dll.GetString t.Name

                          if t.IsNested then
                              let p = dll.GetTypeDefinition(t.GetDeclaringType())
                              sprintf "%s+%s" (getFullName p) name
                          else
                              let ns = dll.GetString t.Namespace
                              if String.IsNullOrWhiteSpace ns then name
                              else sprintf "%s.%s" ns name

                    pdb.MethodDebugInformation |> Seq.tryPick (fun m ->
                        try
                            if m.IsNil then
                                None
                            else
                                let info = pdb.GetMethodDebugInformation(m)

                                if info.Document.IsNil then
                                    None
                                else
                                    let doc = pdb.GetDocument(info.Document)

                                    if doc.Name.IsNil then
                                        None
                                    else
                                        if pdb.GetString doc.Name = sourceFile && not info.SequencePointsBlob.IsNil then
                                            info.GetSequencePoints() |> Seq.tryPick (fun p ->
                                                let dy = p.StartLine - startLine
                                                let dx = p.StartColumn - startColumn

                                                if dy = 0 && abs dx <= 3 then
                                                    let def = dll.GetMethodDefinition(m.ToDefinitionHandle())
                                                    let name = dll.GetString def.Name
                                                    let parent = dll.GetTypeDefinition(def.GetDeclaringType())
                                                    Some(getFullName parent, name)
                                                else
                                                    None
                                            )
                                        else
                                            None
                        with _ ->
                            None
                    )
                with _ ->
                    None
            else
                None
        | _ ->
            None

    let tryGetMethod (typeName : string) (methodName : string) (assembly : Assembly) =
        let typ = assembly.GetType typeName
        if isNull typ then None
        else
            let mi = typ.GetMethod(methodName, BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
            if isNull mi then None
            else Some mi