namespace FShade

open System.Runtime.InteropServices

type Callable private() =
    static member Execute<'T>([<Optional; DefaultParameterValue(Identifier.Default)>] id : string) : 'T = onlyInShaderCode "Callable.Execute"
    static member Execute<'T>(data : 'T, [<Optional; DefaultParameterValue(Identifier.Default)>] id : string) : 'T = onlyInShaderCode "Callable.Execute"

//type Intersection private() =
//    static member Report<'T>(foo : 'T) : bool = onlyInShaderCode ""
//    static member Ignore() : unit = onlyInShaderCode ""

//type Ray private() =
//    static member Terminate() : unit = onlyInShaderCode ""