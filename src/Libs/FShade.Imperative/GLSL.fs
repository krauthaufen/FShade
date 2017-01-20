namespace FShade.Imperative

open System
open Aardvark.Base
open FShade

module GLSL =

    module private String =
        let private lineBreak = System.Text.RegularExpressions.Regex("\r\n")

        let indent (str : string) =
            let lines = lineBreak.Split str
            let prefix = "    "
            lines |> Seq.map (fun l -> if l.Length > 0 then prefix + l else l) |> String.concat "\r\n"

    type Config =
        {
            version             : Version
            perStageUniforms    : bool
            locations           : bool
            uniformBuffers      : bool
        }

    let version120 = Version(1,2,0)

    type State =
        {
            config          : Config
            stages          : ShaderStageDescription
            inputs          : Set<string>
        }
 

    let private builtInInputs =
        Dictionary.ofList [
            ShaderStage.Vertex, 
                Map.ofList [
                    Intrinsics.VertexId, "gl_VertexID"
                    Intrinsics.InstanceId, "gl_InstanceID"
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.PatchVertices, "gl_PatchVertices"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.InvocationId, "gl_InvocationID"
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.TessCoord, "gl_TessCoord"
                    Intrinsics.PatchVertices, "gl_PatchVertices"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.TessLevelInner, "gl_TessLevelInner"
                    Intrinsics.TessLevelOuter, "gl_TessLevelOuter"
                ]
                
            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.InvocationId, "gl_InvocationID"
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.FragCoord, "gl_FragCoord"
                    Intrinsics.PointCoord, "gl_PointCoord"
                    Intrinsics.FrontFacing, "gl_FronFacing"
                    Intrinsics.SampleId, "gl_SampleID"
                    Intrinsics.SamplePosition, "gl_SamplePosition"
                    Intrinsics.SampleMask, "gl_SampleMask"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.PrimitiveId, "gl_PrimitiveID"
                    Intrinsics.Layer, "gl_Layer"
                    Intrinsics.ViewportIndex, "gl_ViewportIndex"
                ]
            ]

    let private builtInOutputs =
        Dictionary.ofList [
            ShaderStage.Vertex,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.TessLevelInner, "gl_TessLevelInner"
                    Intrinsics.TessLevelOuter, "gl_TessLevelOuter"
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                ]

            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, "gl_PointSize"
                    Intrinsics.ClipDistance, "gl_ClipDistance"
                    Intrinsics.Layer, "gl_Layer"
                    Intrinsics.ViewportIndex, "gl_ViewportIndex"
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.Depth, "gl_FragDepth"
                    Intrinsics.SampleMask, "gl_SampleMask"
                ]

        ]

  
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        let ofConfig (c : Config) =
            {
                config = c
                stages = { prev = None; self = ShaderStage.Vertex; next = None }
                inputs = Set.empty
            }



        let regularName (kind : ParameterKind) (name : string) (s : State) =
            let c = s.config

            if kind = ParameterKind.Output && name = Intrinsics.Position && s.stages.next = Some ShaderStage.Fragment then
                "gl_Position"

            elif c.version > version120 && c.locations then
                match kind with
                    | ParameterKind.Input       -> name
                    | ParameterKind.Output      -> name + "Out"
                    | ParameterKind.Uniform     -> name
                    | ParameterKind.Argument    -> name
                    | kind                      -> failwithf "[GLSL] unknown parameter-kind %A for %s" kind name
            else
                match kind, s.stages with
                    | ParameterKind.Input, { prev = None }              -> name
                    | ParameterKind.Input, { prev = Some _; self = s }  -> name + string s

                    | ParameterKind.Output, { next = Some n }           -> name + string n
                    | ParameterKind.Output, { next = None }             -> name + "Out"
                    | _                                                 -> name


        let parameterName (kind : ParameterKind) (name : string) (s : State) =
            let c = s.config

            match kind with
                | ParameterKind.Input -> 
                    match Map.tryFind name builtInInputs.[s.stages.self] with
                        | Some s -> s
                        | None -> regularName kind name s
                | ParameterKind.Output -> 
                    match Map.tryFind name builtInOutputs.[s.stages.self] with
                        | Some s -> s
                        | None -> regularName kind name s
                | _ ->
                    regularName kind name s

            





    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CType =
        let rec glsl (t : CType) =
            match t with
                | CType.CBool                               -> "bool"
                | CType.CVoid                               -> "void"
                | CType.CInt(true, (8 | 16 | 32 | 64))      -> "int"
                | CType.CInt(false, (8 | 16 | 32 | 64))     -> "uint"
                | CType.CFloat(16)                          -> "half"
                | CType.CFloat(32 | 64)                     -> "float"
                
                | CType.CVector(CType.CInt(true, (8 | 16 | 32 | 64)), d)   -> "ivec" + string d
                | CType.CVector(CType.CFloat(32 | 64), d)   -> "vec" + string d
                | CType.CMatrix(CType.CFloat(32 | 64), r,c) -> "mat" + string c + "x" + string r

                | CType.CArray(t, l)                        -> glsl t + "[" + string l + "]"
                | CType.CStruct(n,_,_)                      -> n

                | _ -> failwithf "[GLSL] cannot compile type %A" t
                    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CParameterModifier =
        let glsl (m : CParameterModifier) =
            match m with
                | CParameterModifier.In -> ""
                | CParameterModifier.ByRef -> "inout"

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CParameter =
        let glsl (p : CParameter) =
            let m = CParameterModifier.glsl p.modifier
            match p.ctype with
                | CArray(et, l) ->
                    let t = CType.glsl et
                    if m = "" then sprintf "%s %s[%d]" t p.name l
                    else sprintf "%s %s %s[%d]" m t p.name l

                | pt ->
                    let t = CType.glsl pt
                    if m = "" then sprintf "%s %s" t p.name
                    else sprintf "%s %s %s" m t p.name

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CFunctionSignature =
        let glsl (s : CFunctionSignature) =
            let ret = s.returnType |> CType.glsl
            let args = s.parameters |> Seq.map CParameter.glsl |> String.concat ", "
            sprintf "%s %s(%s)" ret s.name args
            
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CLiteral =
        let glsl (l : CLiteral) =
            match l with
                | CLiteral.CBool v -> if v then "true" else "false"
                | CLiteral.Null -> "null"
                | CLiteral.CIntegral v -> string v
                | CLiteral.CFractional v -> 
                    let str = v.ToString(System.Globalization.CultureInfo.InvariantCulture)
                    if str.Contains "." then str
                    else str + ".0"

                | CLiteral.CString v -> "\"" + v + "\""            

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CExpr =

        let rec private componentNames =
            LookupTable.lookupTable [
                CVecComponent.X, "x"
                CVecComponent.Y, "y"
                CVecComponent.Z, "z"
                CVecComponent.W, "w"
            ]

        let swizzle (l : list<CVecComponent>) =
            l |> Seq.map componentNames |> String.concat ""

        let rec glsl (c : State) (e : CExpr) =
            let glsl = glsl c
            match e with
                | CVar v -> 
                    v.name

                | CValue(_,v) -> 
                    CLiteral.glsl v

                | CCall(f, args) ->
                    let args = args |> Seq.map glsl |> String.concat ", "
                    sprintf "%s(%s)" f.name args

                | CReadInput(kind,_, name, idx) ->
                    let name = c |> State.parameterName kind name
                    match idx with
                        | Some idx -> sprintf "%s[%s]" name (glsl idx)
                        | None -> name


                | CCallIntrinsic(_, f, args) ->
                    let args = args |> Seq.map glsl |> String.concat ", "
                    sprintf "%s(%s)" f.intrinsicName args

                | CConditional(_, c, i, e) ->
                    sprintf "(%s ? %s : %s)" (glsl c) (glsl i) (glsl e)

                | CNeg(_, v) ->
                    sprintf "(-%s)" (glsl v)

                | CNot(_, v) ->
                    sprintf "(!%s)" (glsl v)

                | CAdd(_, l, r) -> sprintf "(%s + %s)" (glsl l) (glsl r)
                | CSub(_, l, r) -> sprintf "(%s - %s)" (glsl l) (glsl r)
                | CMul(_, l, r) -> sprintf "(%s * %s)" (glsl l) (glsl r)
                | CDiv(_, l, r) -> sprintf "(%s / %s)" (glsl l) (glsl r)
                | CMod(_, l, r) -> sprintf "(%s %% %s)" (glsl l) (glsl r)

                | CTranspose(_,m) -> sprintf "transpose(%s)" (glsl m)
                | CMulMatMat(_, l, r) -> sprintf "(%s * %s)" (glsl l) (glsl r)
                | CMulMatVec(_, l, r) -> sprintf "(%s * %s)" (glsl l) (glsl r)
                | CDot(_, l, r) -> sprintf "dot(%s, %s)" (glsl l) (glsl r)
                | CCross(_, l, r) -> sprintf "cross(%s, %s)" (glsl l) (glsl r)
                | CVecSwizzle(_, v, s) -> sprintf "%s.%s" (glsl v) (swizzle s)
                | CMatrixElement(_, m, r, c) -> sprintf "%s[%d][%d]" (glsl m) c r
                | CNewVector(r, _, args) -> sprintf "%s(%s)" (CType.glsl r) (args |> List.map glsl |> String.concat ", ")
                | CVecLength(_, v) -> sprintf "length(%s)" (glsl v)

                | CMatrixFromRows _ | CMatrixFromCols _ -> string e
                
                | CConvert(t, v) -> sprintf "%s(%s)" (CType.glsl t) (glsl v)
                
                | CAnd(l, r) -> sprintf "(%s && %s)" (glsl l) (glsl r)    
                | COr(l, r) -> sprintf "(%s || %s)" (glsl l) (glsl r)    

                | CBitAnd(_, l, r) -> sprintf "(%s & %s)" (glsl l) (glsl r)
                | CBitOr(_, l, r) -> sprintf "(%s | %s)" (glsl l) (glsl r)
                | CBitXor(_, l, r) -> sprintf "(%s ^ %s)" (glsl l) (glsl r)

                
                | CLess(l, r) -> sprintf "(%s < %s)" (glsl l) (glsl r)
                | CLequal(l, r) -> sprintf "(%s <= %s)" (glsl l) (glsl r)
                | CGreater(l, r) -> sprintf "(%s > %s)" (glsl l) (glsl r)
                | CGequal(l, r) -> sprintf "(%s >= %s)" (glsl l) (glsl r)
                | CEqual(l, r) -> sprintf "(%s == %s)" (glsl l) (glsl r)
                | CNotEqual(l, r) -> sprintf "(%s != %s)" (glsl l) (glsl r)

                | CAddressOf(_, e) -> sprintf "&(%s)" (glsl e)
                | CField(_, e, f) -> sprintf "%s.%s" (glsl e) f
                | CItem(_, e, i) -> sprintf "%s[%s]" (glsl e) (glsl i)
                    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CLExpr = 
        let glsl (c : State) (e : CLExpr) = 
            e |> CLExpr.toExpr |> CExpr.glsl c
                     
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CRExpr =
        let glsl (c : State) (e : CRExpr) =
            match e with
                | CRArray(t, args) -> args |> List.map (CExpr.glsl c) |> String.concat ", " |> sprintf "{ %s }"
                | CRExpr e -> CExpr.glsl c e

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CStatement =
        let rec glsl (c : State) (s : CStatement) =
             match s with
                | CNop -> ""
                | CDo e -> CExpr.glsl c e
                | CDeclare(v, r) ->
                    match v.ctype with
                        | CArray(t, l) ->
                            match r with
                                | Some r -> sprintf "%s %s[%d] = %s" (CType.glsl t) v.name l (CRExpr.glsl c r)
                                | None -> sprintf "%s %s[%d]" (CType.glsl t) v.name l
                        | _ -> 
                            match r with
                                | Some r -> sprintf "%s %s = %s" (CType.glsl v.ctype) v.name (CRExpr.glsl c r)
                                | None -> sprintf "%s %s" (CType.glsl v.ctype) v.name

                | CWriteOutput(name, value) ->
                    let name = c |> State.parameterName ParameterKind.Output name
                    match value with
                        | CRExpr.CRArray(_,values) ->
                            values |> Seq.map (CExpr.glsl c) |> Seq.mapi (sprintf "%s[%d] = %s" name) |> String.concat ";\r\n"

                        | CRExpr e -> 
                            sprintf "%s = %s" name (CExpr.glsl c e)
                            

                | CWrite(l, v) ->
                    sprintf "%s = %s" (CLExpr.glsl c l) (CExpr.glsl c v)

                | CIncrement(pre, l) ->
                    if pre then sprintf "++%s" (CLExpr.glsl c l)
                    else sprintf "%s++" (CLExpr.glsl c l)

                | CDecrement(pre, l) ->
                    if pre then sprintf "--%s" (CLExpr.glsl c l)
                    else sprintf "%s--" (CLExpr.glsl c l)

                | CSequential s ->
                    s |> List.map (glsl c) |> String.concat ";\r\n"

                | CReturn -> "return"
                | CBreak -> "break"
                | CContinue -> "continue"
                | CReturnValue v -> sprintf "return %s" (CExpr.glsl c v)

                | CFor(init, cond, step, body) ->
                    sprintf "for(%s; %s; %s)\r\n{\r\n%s;\r\n}" (glsl c init) (CExpr.glsl c cond) (glsl c step) (body |> glsl c |> String.indent)

                | CWhile(guard, body) ->
                    sprintf "while(%s)\r\n{\r\n%s\r\n}" (CExpr.glsl c guard) (body |> glsl c |> String.indent)

                | CDoWhile(guard, body) ->
                    sprintf "do\r\n{\r\n%s\r\n}\r\nwhile(%s)" (body |> glsl c |> String.indent) (CExpr.glsl c guard)

                | CIfThenElse(cond, i, e) ->
                    sprintf "if(%s)\r\n{\r\n%s;\r\n}\r\nelse\r\n{\r\n%s;\r\n}" (CExpr.glsl c cond) (i |> glsl c |> String.indent) (e |> glsl c |> String.indent)

                | CSwitch _ -> string s
               
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CUniform =
        let glsl (c : Config) (uniforms : list<CUniform>) =
            if c.uniformBuffers then
                let buffers =
                    uniforms 
                        |> List.groupBy (fun u -> u.cUniformBuffer)

                let definitions = 
                    buffers |> List.map (fun (name, fields) ->
                        let fields = fields |> List.map (fun u -> sprintf "%s %s;" (CType.glsl u.cUniformType) u.cUniformName)

                        match name with
                            | None ->
                                fields |> List.map (sprintf "uniform %s") |> String.concat "\r\n"
                            | Some n ->
                                let fields = fields |> String.concat "\r\n"
                                sprintf "uniform %s\r\n{\r\n%s\r\n}" n (String.indent fields)


                    )

                String.concat "\r\n\r\n" definitions
            else
                uniforms
                    |> List.map (fun u -> sprintf "uniform %s %s;" (CType.glsl u.cUniformType) u.cUniformName)
                    |> String.concat "\r\n"


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CEntryParameter =
        let private regularDefinition (state : State) (kind : ParameterKind) (index : int) (p : CEntryParameter) =
            let c = state.config
            let decorations =
                p.cParamDecorations 
                |> Set.toList
                |> List.choose (fun d ->
                    match d with
                        | ParameterDecoration.Const -> Some "const"
                        | ParameterDecoration.Interpolation m ->
                            match m with
                                | InterpolationMode.Centroid -> Some "centroid"
                                | InterpolationMode.Flat -> Some "flat"
                                | InterpolationMode.NoPerspective -> Some "noperspective"
                                | InterpolationMode.Perspective -> Some "perspective"
                                | InterpolationMode.Sample -> Some "sample"
                                | _ -> None
                        | ParameterDecoration.Memory _ ->
                            None

                )

            let decorations =
                match kind with
                    | ParameterKind.Input | ParameterKind.Output when c.locations && c.version > version120 ->
                        sprintf "layout(location = %d)" index :: decorations
                    | _ ->
                        decorations
                
            let decorations = 
                match decorations with
                    | [] -> ""
                    | _ -> String.concat " " decorations + " "

            let name = state |> State.parameterName kind p.cParamName

            let prefix =
                if c.version > version120 then
                    match kind with
                        | ParameterKind.Input -> "in "
                        | ParameterKind.Output -> "out "
                        | _ -> ""
                else
                    match kind with
                        | ParameterKind.Input -> "varying "
                        | ParameterKind.Output -> "varying "
                        | _ -> ""
                    

            sprintf "%s%s%s %s;" decorations prefix (CType.glsl p.cParamType) name

        let glsl (state : State) (kind : ParameterKind) (index : int) (p : CEntryParameter) =
            match kind with
                | ParameterKind.Input -> 
                    match Map.tryFind p.cParamName builtInInputs.[state.stages.self] with
                        | Some s -> None
                        | None -> regularDefinition state kind index p |> Some

                | ParameterKind.Output -> 
                    match state.stages.next, p.cParamName with
                        | Some ShaderStage.Fragment, "Positions" -> None
                        | _ -> 
                            match Map.tryFind p.cParamName builtInOutputs.[state.stages.self] with
                                | Some s -> None
                                | None -> regularDefinition state kind index p |> Some
                | _ ->
                    regularDefinition state kind index p |> Some

    
        let many (state : State) (kind : ParameterKind) (l : list<CEntryParameter>) =
            let definitions, _ = 
                l |> List.fold (fun (r,i) p ->
                    match glsl state kind i p with
                        | Some str ->
                            (r @ [str], i + 1)
                        | None ->
                            (r, i)
                ) ([], 0) 
            definitions
                

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CEntryDef =
        let glsl (c : Config) (e : CEntryDef) =
            let state =
                { 
                    config = c

                    inputs = e.cInputs |> List.map (fun p -> p.cParamName) |> Set.ofList

                    stages =
                        e.cDecorations 
                        |> List.tryPick (function EntryDecoration.Stages t -> Some t | _ -> None) 
                        |> Option.defaultValue {
                            prev = None
                            self = ShaderStage.Vertex
                            next = None
                        }
                }

            let inputs = CEntryParameter.many state ParameterKind.Input e.cInputs
            let outputs = CEntryParameter.many state ParameterKind.Output e.cOutputs
            let args = CEntryParameter.many state ParameterKind.Argument e.cArguments |> String.concat ", " 

            String.concat "\r\n" [
                yield! inputs
                yield! outputs
                yield sprintf "%s %s(%s)\r\n{\r\n%s;\r\n}" (CType.glsl e.cReturnType) e.cEntryName args (e.cBody |> CStatement.glsl state |> String.indent)
            ]

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CValueDef =
        let rec glsl (c : Config) (d : CValueDef) =
            match d with
                | CConditionalDef(d, inner) ->
                    let inner = inner |> List.map (glsl c) |> String.concat "\r\n"
                    sprintf "\r\n#ifdef %s\r\n%s\r\n\r\n#endif\r\n" d inner

                | CEntryDef e -> 
                    CEntryDef.glsl c e

                | CFunctionDef(signature, body) ->
                    sprintf "%s\r\n{\r\n%s;\r\n}\r\n" (CFunctionSignature.glsl signature) (body |> CStatement.glsl (State.ofConfig c) |> String.indent)

                | CConstant(t, n, init) ->
                    match t with
                        | CArray(t,l) -> sprintf "const %s %s[%d] = %s;" (CType.glsl t) n l (CRExpr.glsl (State.ofConfig c) init)
                        | _ -> sprintf "const %s %s = %s;" (CType.glsl t) n (CRExpr.glsl (State.ofConfig c) init)

                | CUniformDef us ->
                    if c.perStageUniforms then
                        CUniform.glsl c us
                    else
                        ""

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CTypeDef =
        let glsl (d : CTypeDef) =
            match d with
                | CStructDef(name, fields) ->
                    let fields = fields |> List.map (fun (t, n) -> sprintf "%s %s;" (CType.glsl t) n) |> String.concat "\r\n"
                    sprintf "struct %s\r\n{\r\n%s\r\n}" name (String.indent fields)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CModule =
        let glsl (c : Config) (m : CModule) =
            let definitions =
                List.concat [
                    yield m.types |> List.map CTypeDef.glsl

                    if not c.perStageUniforms then
                        let uniforms = m.uniforms
                        yield [ CUniform.glsl c uniforms ]

                    yield m.values |> List.map (CValueDef.glsl c)
                ]

            definitions |> String.concat "\r\n\r\n"

        
