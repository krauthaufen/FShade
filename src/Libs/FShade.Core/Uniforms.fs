namespace FShade

open System

type UniformScope private(parent : Option<UniformScope>, name : string) =
    static let mutable currentId = 0

    static let glob = UniformScope(None, "Global")



    let id = System.Threading.Interlocked.Increment(&currentId)
    let childScopes = System.Collections.Generic.Dictionary<string, UniformScope>()

    static member Global = glob

    member private x.Id = id
    interface IComparable with
        member x.CompareTo o =
            match o with
                | :? UniformScope as o -> id.CompareTo o.Id
                | _ -> failwith "uncomparable"

    override x.GetHashCode() =
        id.GetHashCode()

    override x.Equals(o) =
        match o with
            | :? UniformScope as o -> o.Id = id
            | _ -> false

    member x.Parent = parent
    member x.Name = name
    member x.FullName =
        let rec build (name : string) (s : UniformScope) =
            match s.Parent with
                | None ->
                    if name.Length = 0 then "Global"
                    else name
                | Some p ->
                    build (s.Name + name) p

        build "" x

    member x.GetChildScope(n : string) =
        lock childScopes (fun () ->
            match childScopes.TryGetValue n with
                | (true,s) ->
                    s
                | _ ->
                    let s = UniformScope(Some x, n)
                    childScopes.[n] <- s
                    s
        )

type ISemanticValue =
    abstract member Semantic : string
    abstract member Scope : UniformScope