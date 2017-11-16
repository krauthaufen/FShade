namespace Examples

open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open System.Reflection

module UtiliyFunctions =
    
    type UniformScope with
        member x.A : float = x?Bla?A
        member x.B : float = x?Bla?B
        member x.AB : float = x?Bla?AB

        member x.X : M44d = x?Buffy?X
        member x.Y : M44d = x?Buffy?Y

    type Vertex = 
        { 
            [<SourceVertexIndex>] i : int
            [<Position>] pos : V4d
            [<Semantic("Hugo")>] hugo: V3d 
        }
 
    type Vertex1 = 
        { 
            [<Position>] pos : V4d
            [<Semantic("Hugo")>] hugo: V3d 
        }
       
    [<ReflectedDefinition; AutoOpen>]
    module Helpers =
        open Aardvark.Base.Ag

        [<Inline>]
        let monster (a : float) =
            if a < 10.0 then
                discard()

        [<Inline>]
        let f (a : float) (b : float) =
            let x = 10.0 * FShade.Imperative.ExpressionExtensions.ShaderIO.ReadInput<float>(FShade.Imperative.ParameterKind.Input, "SomeInput")
            a + b * uniform.A + x
  
        //[<Inline>]
        let g (a : float) (b : float) =
            f a a + uniform.A + uniform.B

    let vs (v : Vertex) =
        vertex {
            return { v with pos = (uniform.X * uniform.Y) * v.pos }
        }

    let sammy =
        sampler3d {
            texture uniform?Blau
            filter Filter.Anisotropic
            addressU WrapMode.Clamp
            addressV WrapMode.Wrap
            addressW WrapMode.Mirror
        }

    [<ReflectedDefinition>]
    let funny v =
        sammy.Sample v

    let shader (v : Vertex) =
        fragment {
            // should be removed
            monster 12.0


            let a = 10 // * int v.pos.X
            let b = a * 2
            let a = 100 //* int v.pos.Y
            let b = b + a * 2
            let a1 = 1000 //* int v.pos.Z


            let c = b + a1



            return funny (V3d(a,a,a)) + V4d.IIII * g v.hugo.X v.hugo.Y * float c
        }

    let gs0 (v : Triangle<Vertex>) =
        triangle {

            let a = 3.0 * v.P0.pos * v.P0.hugo.X
            let b = 3.0 * v.P1.pos * v.P1.hugo.X
            let c = 3.0 * v.P2.pos * v.P2.hugo.X

            yield { v.P0 with pos = a + b; hugo = v.P0.pos.XYZ + v.P0.hugo }
            yield { v.P1 with pos = b + c; hugo = v.P1.pos.XYZ + v.P1.hugo }
            yield { v.P2 with pos = c + a; hugo = v.P2.pos.XYZ + v.P2.hugo }
        }


    let gs1 (v : Triangle<Vertex>) =
        triangle {
            yield { v.P0 with pos = 2.0 * v.P0.pos }
            yield { v.P1 with pos = 2.0 * v.P1.pos }
            yield { v.P2 with pos = 2.0 * v.P2.pos }
            restartStrip()
            yield { v.P0 with pos = 3.0 * v.P0.pos }
            yield { v.P1 with pos = 3.0 * v.P1.pos }
            yield { v.P2 with pos = 3.0 * v.P2.pos }

        }

    let print (add : list<string * System.Type>) (effect : Effect) =
        match effect.LastShader with
            | Some shader ->
                let mutable index = 0
                let id () =
                    let i = index
                    index <- i + 1
                    i

                let existing = shader.shaderOutputs |> Map.remove Intrinsics.SourceVertexIndex |> Map.map (fun name desc -> desc.paramType, id())
                let additional = add |> Map.ofList |> Map.map (fun name t -> t, id())

                let config =
                    {
                        depthRange      = Range1d(-1.0, 1.0)
                        flipHandedness  = false
                        lastStage       = shader.shaderStage
                        outputs         = Map.union existing additional 
                    }

                let glsl410 =
                    GLSL.Backend.Create {
                        version                 = System.Version(4,1)
                        enabledExtensions       = Set.ofList [ ]
                        createUniformBuffers    = true
                        createBindings          = true
                        createDescriptorSets    = true
                        stepDescriptorSets      = false
                        createInputLocations    = true
                        createPerStageUniforms  = false
                        reverseMatrixLogic      = true
                    }

                let glsl = 
                    effect
                        // compile the thing
                        |> Effect.toModule config
                        |> ModuleCompiler.compileGLSL glsl410

                printfn "%s" glsl.code

            | None ->
                ()

    [<AutoOpen>]
    module CodeReflection = 
        open Microsoft.FSharp.Compiler.SourceCodeServices
        open Microsoft.FSharp.Compiler.Ast
        open Microsoft.FSharp.Compiler.Range

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

            let mkDefn scope opened file lines range prefix (Binding(_,kind, _, _, _, _, valData, pat, _, e, r, _) as binding) =
                [
                    let (SynValData.SynValData(_,SynValInfo.SynValInfo(args,_),_)) = valData
                    match pat with
                        | SynPat.LongIdent(LongIdentWithDots(ids,_),pats,_,_,_,_) ->
                            let name = List.skip 1 ids |> List.map string |> String.concat "."
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
                            mkDefn scope opened file lines range prefix binding
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
                                yield! mkDefn scope opened file lines range "" binding
                        ]

                    | _ ->
                        opened, []

            let parseDefintions (file : string) =
                let lines = File.readAllLines file
                let code = String.concat "\r\n" lines

                let projOptions = 
                    checker.GetProjectOptionsFromScript(file, code)
                        |> Async.RunSynchronously

                let parseFileResults = 
                    checker.ParseFileInProject(file, code, projOptions)  
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
                                        let o, i = traverse (if isMod then [Module scope] else [Namespace scope]) opened file lines d
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


        open Microsoft.FSharp.Quotations
        open Microsoft.FSharp.Quotations.Patterns
        open Microsoft.FSharp.Quotations.ExprShape
        open Aardvark.Base.TypeInfo
        open Microsoft.FSharp.Reflection

        let tryGetCode (e : Effect) : Option<string> =
            assert (e.ComposedOf = [])

            match e.SourceDefintion with
                | Some(r, inputType) ->
                    match Code.TryGetReflectedDefinition r with
                        | Some definition ->
                            let things = Dict<MethodBase, Option<Definition>>()

                            let rec visit (e : Expr) =
                                match e with

                                    | Call(t, mi, args) ->
                                        t |> Option.iter visit
                                        args |> List.iter visit

                                        match Expr.TryGetReflectedDefinition mi with
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
                                
                            visit r

                            let used = things.Values |> Seq.choose id |> Seq.toList

                            let uniformDefs = 
                                e.Uniforms 
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

                            let samplers =
                                e.Uniforms 
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
                                  
                                
                            let fields = FSharpType.GetRecordFields(inputType, true)
                                 
                            let vertexDef =
                                String.concat "\r\n" [
                                    yield sprintf "type %s = " inputType.Name
                                    yield "    {"
                                    for f in fields do
                                        let sem = f.Semantic
                                        let i = f.Interpolation
                                        let typ = Aardvark.Base.ReflectionHelpers.getPrettyName f.PropertyType

                                        let atts =
                                            [
                                                if sem <> f.Name then
                                                    yield sprintf "Semantic(\"%s\")" sem

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
                                  
//                            let libs =
//                                [
//                                    """#I @"C:\Users\Schorsch\Development\FShade\bin\Debug" """
//                                    """#r @"..\..\bin\Debug\Aardvark.Base.dll" """
//                                    """#r @"..\..\bin\Debug\Aardvark.Base.TypeProviders.dll" """
//                                    """#r @"..\..\bin\Debug\Aardvark.Base.FSharp.dll" """
//                                    """#r @"..\..\bin\Debug\FShade.Imperative.dll" """
//                                    """#r @"..\..\bin\Debug\FShade.Core.dll" """
//                                    """#r @"..\..\bin\Debug\FShade.GLSL.dll" """
//                                ]

                            
                            let str = 
                                String.concat  "\r\n" [

                                    //yield! libs

                                    for o in definition.opened do 
                                        yield sprintf "open %s" o

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

                                        
                                    yield "[<ReflectedDefinition; AutoOpen>]"
                                    yield "module Utils ="
                                    for u in used do
                                        if u.mustInline then
                                            yield "    [<Inline>]"
                                        yield String.indent 1 u.code
                                        yield ""
                                        
                                    yield vertexDef

                                    yield ""
                                    yield definition.code
                                    yield ""

                                    yield sprintf "Effect.ofFunction %s" definition.name

                                ]

                            Some str
                        | _ ->
                            None
                | None ->
                    None



    let run() =
        
        let effect = Effect.ofFunction shader

        match tryGetCode effect with
            | Some code ->

                let path = @"C:\Users\Schorsch\Desktop\hugo.fsx"
                //File.writeAllText path code
                let res = FShade.Debug.EffectCompiler.evalScript path
                printfn "%A" res
            | None ->
                ()
        System.Environment.Exit 0
        
        

//
//        let effect =
//            Effect.compose [
//                Effect.ofFunction gs0
//                //Effect.ofFunction gs1
//            ]
//
//        effect.GeometryShader.Value.shaderBody |> Optimizer.CSE.findIndexedCommonSubExpressions
//
//        //print [ "Heinz", typeof<int> ] effect
//        System.Environment.Exit 0

        let effect = 
            Effect.compose [
//                Effect.ofFunction vs
//                Effect.ofFunction gs0
                Effect.ofFunction shader
            ]

        effect
//            // decompose derived uniforms here
//            |> Effect.substituteUniforms (fun name typ index ->
//                if name = "AB" then
//                    let a = Expr.ReadInput<float>(ParameterKind.Uniform, "A")
//                    let b = Expr.ReadInput<float>(ParameterKind.Uniform, "B")
//                    Some <@@ (%a) * (%b) @@>
//                else
//                    None
//                )
            // one of the uniforms gets layered
            //|> Effect.toLayeredEffect 2 (Map.ofList [ "A", "As" ]) InputTopology.Triangle
                
            // compile the thing
            |> print []
////
//        let e =
//            <@
//                let thing =
//                    if true then
//                        let mutable a = 10
//                        for i in 0 .. 10 do
//                            a <- a + i
//                        a
//                    else
//                        10
//
//                thing
//            @>
//
//        Optimizer.DeExpr.deExpr e |> string |> printfn "%s"


        ()


