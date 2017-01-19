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

    type ParameterKind =
        | In
        | Out
        | Other

    type State =
        {
            config          : Config
            stages          : ShaderStageDescription
            parameters      : Map<string, ParameterKind>
        }
 
  
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        let ofConfig (c : Config) =
            {
                config = c
                stages = { prev = None; self = ShaderType.Compute; next = None }
                parameters = Map.empty
            }

        let parameterName (name : string) (s : State) =
            let c = s.config
            if c.version > version120 && c.locations then
                name
            else
                match Map.tryFind name s.parameters with
                    | Some kind -> 
                        match kind, s.stages with
                            | In, { prev = None }                   -> name
                            | In, { prev = Some _; self = s }       -> name + string s

                            | Out, { next = Some n }                -> name + string n
                            | Out, { next = None }                  -> name
                            | Other, _                              -> name

                    | None -> 
                        name


            





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

                | CReadInput(_, name, idx) ->
                    let name = c |> State.parameterName name
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

                | CWriteOutput(name, idx, value) ->
                    let name = c |> State.parameterName name
                    match idx with
                        | Some idx -> sprintf "%s[%s] = %s" name (CExpr.glsl c idx) (CExpr.glsl c value)
                        | None -> sprintf "%s = %s" name (CExpr.glsl c value)
                            

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
        let glsl (state : State) (c : Config) (kind : ParameterKind) (index : int) (p : CEntryParameter) =
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
                    | Other -> 
                        decorations

                    | _ ->
                        if c.locations && c.version > version120 then 
                            sprintf "layout(location = %d)" index :: decorations
                        else 
                            decorations
                
            let decorations = 
                match decorations with
                    | [] -> ""
                    | _ -> String.concat " " decorations + " "

            let name = state |> State.parameterName p.cParamName

            let prefix =
                if c.version > version120 then
                    match kind with
                        | In -> "in "
                        | Out -> "out "
                        | Other -> ""
                else
                    match kind with
                        | In -> "varying "
                        | Out -> "varying "
                        | Other -> ""
                    

            sprintf "%s%s%s %s;" decorations prefix (CType.glsl p.cParamType) name
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CEntryDef =
        let glsl (c : Config) (e : CEntryDef) =
            let state =
                { 
                    config = c

                    parameters = 
                        Map.ofList [
                            yield! e.cInputs |> List.map (fun p -> p.cParamName, In)
                            yield! e.cOutputs |> List.map (fun p -> p.cParamName, Out)
                        ]

                    stages =
                        e.cDecorations 
                        |> List.tryPick (function EntryDecoration.Stages t -> Some t | _ -> None) 
                        |> Option.defaultValue {
                            prev = None
                            self = ShaderType.Vertex
                            next = None
                        }
                }

            let inputs = e.cInputs |> List.mapi (CEntryParameter.glsl state c In)
            let outputs = e.cOutputs |> List.mapi (CEntryParameter.glsl state c Out)
            let args = e.cArguments |> List.mapi (CEntryParameter.glsl state c Other) |> String.concat ", " 

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

        
