namespace FShade

open System
open System.Reflection
open FShade.SpirV
open FShade.Imperative

[<AutoOpen>]
module ``SpirV Extensions`` =

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ModuleCompiler =
        let compileSpirV (module_ : Module) =
            module_ 
                |> ModuleCompiler.compile Backend.Instance 
                |> Assembler.assembleModule