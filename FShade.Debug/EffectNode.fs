namespace FShade.Debug

open FShade.Compiler
open FShade

[<AutoOpen>]
module Core =
    type CompiledEffect = Compiled<Effect, ShaderState>

    [<ReferenceEquality>]
    type EffectNode = { name : string; read : unit -> CompiledEffect; write : CompiledEffect -> unit }





