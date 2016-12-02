namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Aardvark.Base
open FShade.Compiler
open Aardvark.Base.ReflectionHelpers
open FShade.Utils

[<AutoOpen>]
module Types = 

    type OutputTopology = Points | LineStrip | TriangleStrip
    type InputTopology = Point | Line | LineAdjacency | Triangle | TriangleAdjacency | Patch of int
    type ShaderType = Vertex | Geometry of Option<int> * OutputTopology | Fragment | TessControl | TessEval

    [<NoComparison>]
    type SourceFileInfo = { sourceFilePath : string; startLine : int; startCol : int; endLine : int; endCol : int }

    [<NoComparison>]
    type ShaderDebugInfo = { sourceFileInfo : SourceFileInfo; functionCode : string; opened : list<string>; argumentType : Type; argumentName : string; closure : list<Type * string * obj> }

    [<NoComparison>]
    type Shader = { shaderType : ShaderType; uniforms : list<Uniform * Var>; inputs : Map<string, Var>; outputs : Map<string, Option<string> * Var>; body : Expr; inputTopology : Option<InputTopology>; debugInfo : Option<ShaderDebugInfo> }

    [<NoComparison>]
    type Effect = { vertexShader : Option<Shader>; geometryShader : Option<Shader * OutputTopology>; tessControlShader : Option<Shader>; tessEvalShader : Option<Shader>; fragmentShader : Option<Shader>; originals : list<Shader> }


    [<NoComparison>]
    type UniformGetter =
        | UniformGetter of obj * Type
        | AttributeGetter of string * Type 

        member x.IsSamplerUniform =
            match x with
                | UniformGetter(value, _) when not (isNull value) -> value.GetType() = typeof<string * SamplerState>
                | _ -> false

        member x.IsSamplerArrayUniform =
            match x with
                | UniformGetter(value, _) when not (isNull value) -> value.GetType() = typeof<list<string * SamplerState>>
                | _ -> false


        member x.Value =
            match x with
                | UniformGetter(value,_) -> value
                | _ -> null

//    [<NoComparison>]
//    type UniformGetter(value : obj, t : Type) =
//        member x.Value = value
//        member x.Type = t
//
//        member x.IsSamplerUniform =
//            value.GetType() = typeof<string * SamplerState>

    [<NoComparison>]
    type CompiledShader = { usedTypes : PersistentHashSet<Type>; uniforms : Map<string, UniformGetter>; uniformBuffers : Map<UniformScope, list<Type * string>>; code : string }

    [<NoComparison>]
    type ColorOnlyFragment = { [<Semantic("Colors")>] colorOnly : V4d }

    [<NoComparison>]
    type PositionOnlyVertex = { [<Semantic("Positions")>] positionOnly : V4d }



    let (|Input|_|) (t : Type) (e : Expr) =
        match e with
            | MemberFieldGet(Value(_,valueType), m) when valueType = t ->
                Input(m.Type, m.Semantic) |> Some

            | MemberFieldGet(Var(v), m) when v.Type = t ->
                Input(m.Type, m.Semantic) |> Some

            | _ -> None
 
    let (|Output|_|) (t : Type) (e : Expr) =
        match e with
            | NewRecord(rt, args) when t = rt ->
                let semantics = FSharpTypeExt.GetRecordFields(t) |> Seq.map (fun m -> m.Semantic, m.AssignedTarget) |> Seq.toList

                Output(List.zip semantics args) |> Some

            | _ -> None
