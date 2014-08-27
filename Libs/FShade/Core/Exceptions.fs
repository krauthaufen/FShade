namespace FShade

[<AutoOpen>]
module Exceptions =
    exception ShaderOnlyAccessException

    let shaderOnlyAccess() = raise <| ShaderOnlyAccessException
