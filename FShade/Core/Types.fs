namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Aardvark.Base
open FShade.Compiler
open Aardvark.Base.ReflectionHelpers

[<AutoOpen>]
module Types = 

    type OutputTopology = Points | LineStrip | TriangleStrip
    type InputTopology = Point | Line | LineAdjacency | Triangle | TriangleAdjacency | Patch of int
    type ShaderType = Vertex | Geometry of OutputTopology | Fragment | TessControl | TessEval

    [<NoComparison>]
    type SourceFileInfo = { sourceFilePath : string; startLine : int; startCol : int; endLine : int; endCol : int }

    [<NoComparison>]
    type ShaderDebugInfo = { sourceFileInfo : SourceFileInfo; functionCode : string; opened : list<string>; argumentType : Type; argumentName : string; closure : list<Type * string * obj> }

    [<NoComparison>]
    type Shader = { shaderType : ShaderType; uniforms : list<Uniform * Var>; inputs : Map<string, Var>; outputs : Map<string, Option<string> * Var>; body : Expr; inputTopology : Option<InputTopology>; debugInfo : Option<ShaderDebugInfo> }

    [<NoComparison>]
    type Effect = { vertexShader : Option<Shader>; geometryShader : Option<Shader * OutputTopology>; tessControlShader : Option<Shader>; tessEvalShader : Option<Shader>; fragmentShader : Option<Shader>; originals : list<Shader> }

    [<NoComparison>]
    type UniformGetter = { value : IMod; valueType : Type}

    [<NoComparison>]
    type CompiledShader = { usedTypes : Set<Unique<Type>>; uniforms : Map<string, UniformGetter>; uniformBuffers : Map<UniformScope, list<Type * string>>; code : string }

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
                let semantics = FSharpType.GetRecordFields t |> Seq.map (fun m -> m.Semantic, m.AssignedTarget) |> Seq.toList

                Output(List.zip semantics args) |> Some

            | _ -> None


    let private lineBreak = System.Text.RegularExpressions.Regex("\r\n")
    let private ws = System.Text.RegularExpressions.Regex("^[ ]*")

    let private tryFindLineStart (str : string) (line : int) =
        let mutable m = lineBreak.Match str 
        let mutable current = 1
        while current < line - 1 && m.Success do
            current <- current + 1
            m <- m.NextMatch()

        if m.Success then
            Some (m.Index + m.Length)
        else
            None

    let private removeIndent (str : string) =
        let lines = lineBreak.Split str
        let leadingIndentSize s = 
            let m = ws.Match s
            if m.Success then 
                if m.Length <> s.Length then m.Length
                else System.Int32.MaxValue
            else 0
            
        let minimalIndent = lines |> Array.map leadingIndentSize |> Array.min
        lines |> Array.map (fun l -> if l.Length > minimalIndent then l.Substring minimalIndent else l) |> String.concat "\r\n"

    let tryReadShaderCode (startLine : int) (str : string) =
        match tryFindLineStart str startLine with
            | Some index ->
                let mutable current = index
                let mutable brLevel = 0
                let mutable started = false

                while not started || brLevel > 0 do
                    let c = str.[current]
                                    
                    if c = '{' then
                        started <- true
                        brLevel <- brLevel + 1
                    elif c = '}' then
                        brLevel <- brLevel - 1

                    current <- current + 1 


                let code = str.Substring(index, current - index)
                removeIndent code |> Some
            | None -> None

    [<AutoOpen>]
    module ShaderDebugInfoExtensions =
        type SourceFileInfo with
            member x.Read() =
                let str = System.IO.File.ReadAllText(x.sourceFilePath)
                tryReadShaderCode x.startLine str

        type ShaderDebugInfo with
            member x.originalCode =
                let closureArgs = x.closure |> List.map (fun (t,n,_) -> sprintf "(%s : %s)" n t.PrettyName) |> String.concat " "
                let closureArgs = if closureArgs.Length > 0 then sprintf "%s " closureArgs else ""
                let code = x.sourceFileInfo.Read()
                match code with
                    | Some code ->
                        sprintf "fun %s(%s : %s) ->\r\n%s" closureArgs x.argumentName x.argumentType.PrettyName (String.indent 1 code)
                    | None ->
                        x.functionCode