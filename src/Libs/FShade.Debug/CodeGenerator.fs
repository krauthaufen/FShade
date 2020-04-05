namespace FShade.Debug

open System.IO
open System.Reflection

open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.ExprShape
     
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Ast
open FSharp.Compiler.Range

open Aardvark.Base
open Aardvark.Base.TypeInfo
open FSharp.Data.Adaptive
open FShade


module CodeGenerator = 

    type Definition =
        {
            startLine       : int
            endLine         : int
            startCol        : int
            endCol          : int
            fileName        : string
            code            : string
            mustInline      : bool
            opened          : list<string>
            binding         : obj
            declaringType   : string
            name            : string
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module private Definition =

        let private rx = System.Text.RegularExpressions.Regex @"^[ \t]*"

        let private leadingSpaces (str : string) =
            let mutable cnt = 0
            while cnt < str.Length && str.[cnt] = ' ' do
                cnt <- cnt + 1

            cnt

        let create (file : string) (lines : string[]) (r : range) (opened : list<string>) (name : string) (declaringType : string) (b : SynBinding) =
            let sl = r.StartLine
            let sc = r.StartColumn 
            let el = r.EndLine
            let ec = r.EndColumn

            let lines = lines.[sl - 1 .. el - 1]
            let code = 
                if lines.Length = 1 then
                    lines.[0].Substring(sc, ec - sc)

                else
                    lines.[0] <- lines.[0].Substring(sc)
                    lines.[lines.Length - 1] <- lines.[lines.Length - 1].Substring(0, ec)

                    let minLeading = 
                        lines.[1..] 
                            |> Seq.filter (fun s -> s.Length <> 0) 
                            |> Seq.map leadingSpaces 
                            |> Seq.append [System.Int32.MaxValue]
                            |> Seq.min

                    if minLeading > 4 then
                        for i in 1 .. lines.Length - 1 do
                            if lines.[i].Length >= minLeading - 4 then
                                lines.[i] <- lines.[i].Substring(minLeading - 4)


                    String.concat "\r\n" lines 

            let code = code.Replace(" private ", " ").Replace(" internal ", " ")

            {
                startLine       = sl
                endLine         = el
                startCol        = sc
                endCol          = ec
                fileName        = file
                code            = code
                binding         = b
                declaringType   = declaringType
                mustInline      = false
                opened          = opened
                name            = name
            }


    let private checker = FSharpChecker.Create()

    module private Helpers = 
        type NamespaceKind =
            | Namespace of string
            | Module of string
        
        let rec fullyQualifiedName l =
            match l with
                | [Namespace a] -> a
                | [Module m] -> m

                | Namespace a :: r -> a + "." + fullyQualifiedName r
                | Module a :: r -> a + "+" + fullyQualifiedName r
                
                | [] -> ""

        let mkDefn scope opened file lines range prefix isMember (Binding(_,kind, _, _, _, _, valData, pat, _, e, r, _) as binding) =
            [
                let (SynValData.SynValData(_,SynValInfo.SynValInfo(args,_),_)) = valData
                match pat with
                    | SynPat.LongIdent(LongIdentWithDots(ids,_),pats,_,_,_,_) ->
                            
                        let name = (if isMember then List.skip 1 ids else ids) |> List.map string |> String.concat "."
                        let id = prefix + name
                        yield Definition.create file lines range opened id (fullyQualifiedName scope) binding

                    | SynPat.Named(_,id,_,_,_) ->
                        let id = prefix + string id
                        yield Definition.create file lines range opened id (fullyQualifiedName scope) binding

                    | _ ->
                        ()
            ]

        let rec traverseType scope opened file lines (SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo(_,_,_,lid,_,_,_,_), repr, mems, _)) =
            let id = lid |> List.map string |> String.concat " "


            let isExt = 
                match repr with
                    | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconAugmentation,_,_) -> true
                    | _ -> false
                    
            let scope, prefix =
                if isExt then scope, id + "."
                else scope @ [Module id], ""

            mems |> List.collect (fun m ->
                match m with
                    | SynMemberDefn.Member(binding, range) ->
                        mkDefn scope opened file lines range prefix true binding
                    | _ ->
                        []
            )


        let rec traverse scope opened file lines d =
            match d with
                | SynModuleDecl.Types(types, _) ->
                    
                    let d = types |> List.collect (traverseType scope opened file lines)
                    

                    opened, d
                | SynModuleDecl.Open(LongIdentWithDots(id, _),_) ->
                    let name = id |> List.map string |> String.concat "."
                    let opened = opened @ [name]
                    opened, []

                | SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(_,_,_,id,_,_,_,_),_,inner,_,_) ->
                    let name = id |> List.map string |> String.concat " "

                    let mutable io = opened
                    let mutable res = []
                    for i in inner do
                        let o, i = traverse (scope @ [Module name]) io file lines i
                        io <- o
                        res <- res @ i

                    opened, res

                | SynModuleDecl.Let(_,bindings, range) ->
                    opened, [
                        for binding in bindings do
                            yield! mkDefn scope opened file lines range "" false binding
                    ]

                | _ ->
                    opened, []

        let parseDefintions (file : string) =
            let lines = File.readAllLines file
            let code = String.concat "\r\n" lines

            //let projOptions, _ = 
            //    checker.GetProjectOptionsFromScript(file, code)
            //        |> Async.RunSynchronously

            let parseFileResults = 
                checker.ParseFile(file, (FSharp.Compiler.Text.SourceText.ofString code), FSharpParsingOptions.Default)  
                    |> Async.RunSynchronously

            if parseFileResults.ParseHadErrors then
                Log.error "parse error"
                []
            else
                match parseFileResults.ParseTree with
                    | Some (ParsedInput.ImplFile (ParsedImplFileInput(fn, script, name, _, _, modules, _))) ->
                        
                        [
                            for SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m) in modules do
                                let scope = lid |> List.map string |> String.concat " "
                                let mutable opened = []
                                for d in decls do 
                                    let o, i = traverse (if isMod.IsModule then [Module scope] else [Namespace scope]) opened file lines d
                                    opened <- o
                                    yield! i
                        ]

                    | _ ->
                        Log.error "no parse tree"
                        []
        
        let createDefinitionTable (file : string) =
            let defns = parseDefintions file

            let map = 
                defns 
                |> List.map (fun d ->
                    (d.startLine, d.startCol), d
                )
                |> MapExt.ofList

            let lookup (sl, sc, el, ec) =
                let l, s, _ = MapExt.neighbours (sl,sc) map

                let l =
                    match s with
                        | Some s -> Some s
                        | None -> l
                
                match l with
                    | Some (_,d) ->
                        if (d.startLine, d.startCol) <= (sl, sc) && (d.endLine, d.endCol) >= (el, ec) then
                            Some d
                        else
                            None
                    | None ->
                        None


            lookup

    let private cache = System.Collections.Concurrent.ConcurrentDictionary<string, int * int * int * int -> Option<Definition>>()


    type Code private() =

        static member TryGetReflectedDefinition((file : string, sl : int, sc : int, el : int, ec : int)) =
            let defn = cache.GetOrAdd(file, System.Func<_,_>(fun file -> Helpers.createDefinitionTable file))
            defn (sl, sc, el, ec)

        static member TryGetReflectedDefinition(file : string, sl : int, sc : int, el : int, ec : int) =
            Code.TryGetReflectedDefinition((file, sl, sc, el, ec))

        static member TryGetReflectedDefinition(e : Expr) =
            match e.DebugRange with
                | Some t -> Code.TryGetReflectedDefinition t
                | None -> None

        static member TryGetReflectedDefinition(mi : MethodBase) =
            match Expr.TryGetReflectedDefinition mi with
                | Some d -> 
                    match Code.TryGetReflectedDefinition d with
                        | Some d ->
                            let isInline = mi.GetCustomAttributes<InlineAttribute>() |> Seq.isEmpty |> not
                            if isInline then Some { d with mustInline = true }
                            else Some d
                        | None ->
                            None
                | None -> 
                    None

    module private List =
        let rec mapOption (f : 'a -> Option<'b>) (l : list<'a>) =
            match l with
                | [] -> Some []
                | h :: t ->
                    match f h with
                        | Some h ->
                            match mapOption f t with
                                | Some t -> Some (h :: t)
                                | None -> None
                        | None ->
                            None

    let private getLeafs (e : Effect) =
        match e.ComposedOf with
            | [] -> [e]
            | l -> l


    let rec private getUsedUniformExtensions (e : Expr) =
        match e with
            
            | Uniform(u) ->
                match e with
                    | PropertyGet(_, prop, _) ->
                        HashSet.ofList [ prop :> MemberInfo, u ]
                        
                    | Call(_,mi,_) ->
                        HashSet.ofList [ mi :> MemberInfo, u ]
                    | _ ->
                        HashSet.empty

            | Call(t, mi, args) ->
                let set = t |> Option.map getUsedUniformExtensions |> Option.defaultValue HashSet.empty
                let set = args |> List.map getUsedUniformExtensions |> List.fold HashSet.union set

                let set = 
                    match ExprWorkardound.TryGetReflectedDefinition mi with
                        | Some e -> 
                            let inner = getUsedUniformExtensions e
                            HashSet.union set inner
                        | None -> set

                set
            

            | ShapeCombination(o, args) ->
                args |> List.map getUsedUniformExtensions |> List.fold HashSet.union HashSet.empty
            | ShapeVar _ ->
                HashSet.empty
            | ShapeLambda(_,b) ->
                getUsedUniformExtensions b

    let tryGetCode (e : Effect) : Option<string> =
        let leafs = getLeafs e

        let definitions =
            leafs |> List.mapOption (fun e ->
               match e.SourceDefintion with
                | Some (r, inputType) ->
                    match Code.TryGetReflectedDefinition r with
                        | Some definition -> 
                            let inputType = 
                                let iface = inputType.GetInterface(typedefof<FShade.Primitives.Primitive<_>>.Name)
                                if isNull iface then inputType
                                else iface.GetGenericArguments().[0]
                            
                            Some (inputType, r, definition)
                        | None -> None
                | None -> None
            ) 

        match definitions with
            | Some definitions ->
                let things = Dict<MethodBase, Option<Definition>>()

                let rec visit (e : Expr) =
                    match e with

                        | Call(t, mi, args) ->
                            t |> Option.iter visit
                            args |> List.iter visit

                            match ExprWorkardound.TryGetReflectedDefinition mi with
                                | Some e -> visit e
                                | None -> ()

                            things.GetOrCreate((mi :> MethodBase), fun mi ->
                                Code.TryGetReflectedDefinition mi 
                            ) |> ignore

                        | PropertyGet(t, p, index) ->
                            let call =
                                match t with
                                    | Some t -> Expr.Call(t, p.GetMethod, index)
                                    | None -> Expr.Call(p.GetMethod, index)
                            visit call

                        | ShapeVar _ -> ()
                        | ShapeLambda(_,b) -> visit b
                        | ShapeCombination(o, args) ->
                            for a in args do visit a
                   
                for (_,r,_) in definitions do             
                    visit r

                let used = things.Values |> Seq.choose id |> Seq.toList

                // TODO: sort used

                let allUniforms =
                    definitions
                        |> List.map (fun (_,e,_) -> getUsedUniformExtensions e) 
                        |> List.fold HashSet.union HashSet.empty
                        |> HashSet.map (fun (mem, u) ->
                            
                            let name = mem.Name

                            let name =
                                let arr = name.Split [|'.'|]
                                arr.[arr.Length - 1]

                            let name = 
                                if name.StartsWith "get_" then name.Substring 4
                                else name

                            (name, u)
                        )
                        |> Map.ofSeq

                let uniformDefs = 
                    allUniforms
                        |> Map.toList
                        |> List.choose (fun (name, u) ->
                            match u.uniformValue with
                                | UniformValue.Attribute(scope, realName) ->
                                    let rec fullName (s : UniformScope) =
                                        match s.Parent with
                                            | None -> 
                                                "uniform"
                                            | Some p ->
                                                fullName p + "?" + s.Name

                                    let bufferName = fullName scope
                                    let typ = Aardvark.Base.ReflectionHelpers.getPrettyName u.uniformType
                                    let str = sprintf "    member x.%s : %s = %s?%s" name typ bufferName realName
                                    Some str
                                | _ ->
                                    None
                        )
                              
                let toCamelCase (str : string) =
                    if str.Length > 0 then
                        if str.[0] >= 'A' || str.[0] <= 'Z' then
                            str.Substring(0, 1).ToLower() + str.Substring(1)
                        else
                            str
                    else
                        str
           
                let toPascalCase (str : string) =
                    if str.Length > 0 then
                        if str.[0] >= 'a' || str.[0] <= 'z' then
                            str.Substring(0, 1).ToUpper() + str.Substring(1)
                        else
                            str
                    else
                        str

                let samplers =
                    allUniforms
                        |> Map.toList
                        |> List.choose (fun (samplerName, u) ->
                            match u.uniformValue, u.uniformType with
                                | UniformValue.Sampler(texName, state), SamplerType(dim, arr, shadow, ms, valueType) ->
                                    let builderName = toCamelCase u.uniformType.Name

                                    let code = 
                                        String.concat "\r\n" [
                                            yield sprintf "let %s = " samplerName
                                            yield sprintf "    %s {" builderName
                                            yield sprintf "        texture uniform?%s" texName

                                            match state.AddressU with
                                                | Some v -> yield sprintf "        addressU WrapMode.%A" v
                                                | _ -> ()
                                            match state.AddressV with
                                                | Some v -> yield sprintf "        addressV WrapMode.%A" v
                                                | _ -> ()
                                            match state.AddressW with
                                                | Some v -> yield sprintf "        addressW WrapMode.%A" v
                                                | _ -> ()
                                            match state.Filter with
                                                | Some v -> yield sprintf "        filter Filter.%A" v
                                                | _ -> ()
                                            match state.Comparison with
                                                | Some v -> yield sprintf "        comparison ComparisonFunction.%A" v
                                                | _ -> ()
                                            match state.BorderColor with
                                                | Some v -> yield sprintf "        borderColor (C4f(%ff, %ff, %ff, %ff))" v.R v.G v.B v.A
                                                | _ -> ()
                                            match state.MaxAnisotropy with
                                                | Some v -> yield sprintf "        maxAnisotropy %d" v
                                                | _ -> ()
                                            match state.MaxLod with
                                                | Some v -> yield sprintf "        maxLod %ff" v
                                                | _ -> ()
                                            match state.MinLod with
                                                | Some v -> yield sprintf "        minLod %ff" v
                                                | _ -> ()
                                            match state.MipLodBias with
                                                | Some v -> yield sprintf "        mipLodBias %ff" v
                                                | _ -> ()


                                            yield sprintf "    }"

                                        ]


                                    Some code


                                | UniformValue.SamplerArray _, _->
                                    failwith ""
                                | _ ->
                                    None
                        )
                                  
                  
                let str = 
                    String.concat  "\r\n" [
                        yield "#load \"Setup.fsx\""
                        yield "open Setup"
                        yield "open Aardvark.Base"
                        yield "open FShade.Imperative"
                        yield "open FShade"
                        yield ""

                        match uniformDefs with
                            | [] -> ()
                            | ud ->
                                yield "type UniformScope with"
                                yield! ud

                        yield ""
                        for s in samplers do
                            yield s
                            yield ""

                        match used with
                            | [] -> ()
                            | _ -> 
                                yield "[<ReflectedDefinition; AutoOpen>]"
                                yield "module Utils ="
                                for u in used do
                                    if u.mustInline then
                                        yield "    [<Inline>]"
                                    yield String.indent 1 u.code
                                    yield ""
                                yield ""
                                        

                        for (inputType, r, definition) in definitions do
                            yield sprintf "module %s = " (toPascalCase definition.name)

                                          
                            let fields = 
                                if FSharpType.IsRecord(inputType, true) then
                                    FSharpType.GetRecordFields(inputType, true)
                                else
                                    failwith ""
                                 
                            let vertexDef =
                                String.concat "\r\n" [
                                    yield sprintf "type %s = " inputType.Name
                                    yield "    {"
                                    for f in fields do
                                        let sem = f.Semantic
                                        let i = f.Interpolation
                                        let typ = Aardvark.Base.ReflectionHelpers.getPrettyName f.PropertyType

                                        let atts = f.GetCustomAttributesData() |> Seq.toList |> List.filter (fun a -> typeof<SemanticAttribute>.IsAssignableFrom a.AttributeType)

                                        let atts =
                                            [   
                                                for a in atts do
                                                    let args = a.ConstructorArguments |> Seq.toList |> List.map (fun a -> match a.Value with | :? string as str -> "\"" + str + "\"" | _ -> string a) |> String.concat ", "
                                                    let name = a.Constructor.DeclaringType.Name

                                                    let name =
                                                        if name.EndsWith "Attribute" then name.Substring(0, name.Length - 9)
                                                        else name

                                                    if a.AttributeType.Assembly.GetName().Name = "FShade.Core" then
                                                        match args with
                                                            | "" -> yield name
                                                            | _ -> yield sprintf "%s(%s)" name args
                                                    else
                                                        yield sprintf "Semantic(\"%s\")" sem

//
//                                                if sem <> f.Name then
//                                                    yield sprintf "Semantic(\"%s\")" sem

                                                if i <> InterpolationMode.Default then
                                                    yield sprintf "Interpolation(InterpolationMode.%A)" i
                                            ]

                                        match atts with
                                            | [] -> 
                                                yield sprintf "        %s : %s" f.Name typ
                                            | atts ->
                                                let atts = String.concat "; " atts
                                                yield sprintf "        [<%s>] %s : %s" atts f.Name typ

                                        ()
                                    yield "    }"
                                ]


                            //for o in definition.opened do 
                            //    yield sprintf "    open %s" o
                            //
                            //yield ""

                            yield String.indent 1 vertexDef

                            yield ""
                            yield String.indent 1 definition.code
                            yield ""

                        yield "compose {"
                        for (_,_m,definition) in definitions do
                            yield sprintf "    do! %s.%s" (toPascalCase definition.name) definition.name
                        yield "}"

                    ]

                Some str
            | None ->
                None
