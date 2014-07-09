namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Aardvark.Base
open FShade.Compiler
open Aardvark.Base.ReflectionHelpers
open System.IO
open System.Collections.Concurrent

module ShaderDebug = 

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

                while (not started || brLevel > 0) && current < str.Length do
                    let c = str.[current]
                                    
                    if c = '{' then
                        started <- true
                        brLevel <- brLevel + 1
                    elif c = '}' then
                        brLevel <- brLevel - 1

                    current <- current + 1 

                if brLevel = 0  && current < str.Length then
                    let code = str.Substring(index, current - index)
                    removeIndent code |> Some
                else
                    None
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

    type FileTree = { isNamespace : bool; modLevel : int; 
                      mutable modPath : string; mutable modStartLine : int; mutable modEndLine : int; 
                      mutable modOpened : list<int * string>; 
                      mutable modChildren : list<FileTree>; 
                      mutable modParent : Option<FileTree> } with
        member x.modFullName =
            let result = 
                match x.modParent with
                    | Some p -> 
                        let pn = p.modFullName
                        if pn <> "" then sprintf "%s.%s" pn x.modPath
                        else x.modPath

                    | None -> x.modPath
            result.Replace(" ", "")

    let private buildFileTree (lines : string[]) =
        let currentLine = ref 1

        let openRx = System.Text.RegularExpressions.Regex "^(?<indent>[ ]*)open[ ]+(?<name>.+)$"
        let moduleRx = System.Text.RegularExpressions.Regex @"^(?<indent>[ ]*)module[ ]+(?<name>[^\= ]+)[ \=]+"
        let namespaceRx = System.Text.RegularExpressions.Regex "namespace[ ]+(?<name>.+)$"

        let emptyTree() =
            { isNamespace = true; modLevel = -1; modPath = ""; modStartLine = -1; modEndLine = -1; modOpened = []; modChildren = []; modParent = None }

        let root = ref <| emptyTree()

        let namespaces = ref []
        let stack = ref [!root]

        let push (f : FileTree) =
            let c = !stack |> List.head
            c.modChildren <- f::c.modChildren
            f.modParent <- Some c
            stack := f::!stack

        let pop() =
            match !stack with
                | x::xs -> 
                    stack := xs
                    x
                | _ -> failwith "empty stack"

        let peek() =
            !stack |> List.head


        let closeTo(level : int) =
            while stack.Value.Head.modLevel >= level do
                stack.Value.Head.modEndLine <- !currentLine
                stack := stack.Value.Tail

            stack.Value.Head

        let close() = 
            closeTo(0) |> ignore
            if root.Value.modPath.Length <> 0 then
                namespaces := !root::!namespaces
                let newRoot = emptyTree()
                root.Value.modEndLine <- !currentLine
                newRoot.modStartLine <- !currentLine

                root := newRoot


        for l in lines do
            
            let o = openRx.Match l
            let m = moduleRx.Match l
            let n = namespaceRx.Match l

            if n.Success then
                close()
                root.Value.modPath <- n.Groups.["name"].Value
            elif o.Success then
                let indent = o.Groups.["indent"].Value.Length 
                let ns = o.Groups.["name"].Value

                let m = closeTo(indent)
                m.modOpened <- (!currentLine, ns)::m.modOpened
            elif m.Success then
                let name = m.Groups.["name"].Value
                let indent = m.Groups.["indent"].Value.Length
                let n = { isNamespace = false; modLevel = indent; modPath = name; modStartLine = !currentLine; modEndLine = !currentLine;  modOpened = []; modChildren = []; modParent = None }
                closeTo indent |> ignore
                push n
                ()


            currentLine := !currentLine + 1
        close()

        let namespaces = !namespaces
        let result = { isNamespace = true; modLevel = -1; modPath = ""; modStartLine = 0; modEndLine = !currentLine; modOpened = []; modChildren = namespaces; modParent = None }
        namespaces |> List.iter (fun n -> n.modParent <- Some result)
        result

    let rec private tryFindModuleForLine (line : int) (t : FileTree) =
        if t.modStartLine <= line && line < t.modEndLine then
            match t.modChildren with
                | [] -> Some t
                | l -> 
                    match l |> Seq.tryPick (tryFindModuleForLine line) with
                        | None -> Some t
                        | Some c -> Some c
        else
            None

    let rec private tryFindChild (path : list<string>) (t : FileTree) =
        match path with
            | [] -> Some (t.modFullName)
            | x::xs ->
                match t.modChildren |> List.tryPick(fun c -> if c.modPath = x then Some c else None) with
                    | Some c -> tryFindChild xs c
                    | None -> None

    let rec private resolveOpen (name : list<string>) (r : FileTree) =
        match r.modParent with
            | Some p ->
                match tryFindChild name p with
                    | Some c -> c
                    | None ->
                        resolveOpen name p
            | None ->
                name |> String.concat "."

    let rec private getAllOpens (line : int) (r : FileTree) =
        let mine = r.modOpened |> List.filter (fun (l,_) -> l < line) |> List.map (fun (_,o) -> resolveOpen (o.Split '.' |> Array.toList) r) 
        let parent = 
            match r.modParent with
                | Some p -> getAllOpens line p
                | None -> []
        List.concat [[r.modFullName]; mine; parent]

    let private fileTreeCache = ConcurrentDictionary<string, FileTree>()
    let private getFileTree (path : string) =
        fileTreeCache.GetOrAdd(path, fun _ ->
            buildFileTree (File.ReadAllLines path)
        )

    

    let tryGetShaderInfo (f : 'a -> Expr<'b>) (e : Expr) =
        let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
        let invoke = f.GetType().GetMethod("Invoke", flags)
        let param = invoke.GetParameters().[0]

        let closureFields = f.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static) |> Array.toList
        let closure = closureFields |> List.map (fun fi -> fi.FieldType, fi.Name, fi.GetValue(f))

        e.CustomAttributes |> List.tryPick (fun a ->
            match a with
                | NewTuple([Value(:? string as n,_); NewTuple([Value(:? string as file, _); Value(:? int as startLine, _); Value(:? int as startCol, _); Value(:? int as endLine, _); Value(:? int as endCol, _)])]) when n = "DebugRange" ->
                    if System.IO.File.Exists file then
                        
                        let tree = getFileTree file

                        let str = System.IO.File.ReadAllText file
                        match tryReadShaderCode startLine str with
                            | Some code ->

                                match tryFindModuleForLine startLine tree with
                                    | Some tree ->
                                        let fileInfo : SourceFileInfo = { sourceFilePath = file; startLine = startLine; startCol = startCol; endLine = endLine; endCol = endCol }

                                        let opens = getAllOpens startLine tree |> List.filter (fun s -> s.Length <> 0) |> List.rev

                                        let closureArgs = closure |> List.map (fun (t,n,_) -> sprintf "(%s : %s)" n t.PrettyName) |> String.concat " "
                                        let closureArgs = if closureArgs.Length > 0 then sprintf "%s " closureArgs else ""

                                        let code = sprintf "fun %s(%s : %s) ->\r\n%s" closureArgs param.Name param.ParameterType.PrettyName (String.indent 1 code)

                                        let shaderInfo : ShaderDebugInfo = 
                                            {sourceFileInfo = fileInfo
                                             functionCode = code
                                             opened = opens
                                             argumentType = param.ParameterType
                                             argumentName = param.Name
                                             closure = closure }
                                        Some shaderInfo
                                    | None ->
                                        None

                            | None -> None

                        

                    else
                        None
                | _ -> None
        )