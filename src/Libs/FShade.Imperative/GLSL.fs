namespace FShade.Imperative

open Aardvark.Base

module GLSL =
    
    module private String =
        let private lineBreak = System.Text.RegularExpressions.Regex("\r\n")

        let indent (str : string) =
            let lines = lineBreak.Split str
            let prefix = "    "
            lines |> Seq.map (fun l -> if l.Length > 0 then prefix + l else l) |> String.concat "\r\n"

    type Config =
        {
            perStageUniforms    : bool
            locations           : bool
        }
  

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
            let t = CType.glsl p.ctype
            let m = CParameterModifier.glsl p.modifier
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

        let rec glsl (e : CExpr) =
            match e with
                | CVar v -> 
                    v.name

                | CValue(_,v) -> 
                    CLiteral.glsl v

                | CCall(f, args) ->
                    let args = args |> Seq.map glsl |> String.concat ", "
                    sprintf "%s(%s)" f.name args

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
        let glsl (e : CLExpr) = 
            e |> CLExpr.toExpr |> CExpr.glsl
                     
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CRExpr =
        let glsl (e : CRExpr) =
            match e with
                | CRArray(t, args) -> args |> List.map CExpr.glsl |> String.concat ", " |> sprintf "{ %s }"
                | CRExpr e -> CExpr.glsl e

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CStatement =
        let rec glsl (s : CStatement) =
             match s with
                | CNop -> ""
                | CDo e -> CExpr.glsl e
                | CDeclare(v, r) ->
                    match v.ctype with
                        | CArray(t, l) ->
                            match r with
                                | Some r -> sprintf "%s %s[%d] = %s" (CType.glsl t) v.name l (CRExpr.glsl r)
                                | None -> sprintf "%s %s[%d]" (CType.glsl t) v.name l
                        | _ -> 
                            match r with
                                | Some r -> sprintf "%s %s = %s" (CType.glsl v.ctype) v.name (CRExpr.glsl r)
                                | None -> sprintf "%s %s" (CType.glsl v.ctype) v.name

                | CWrite(l, v) ->
                    sprintf "%s = %s" (CLExpr.glsl l) (CExpr.glsl v)

                | CIncrement(pre, l) ->
                    if pre then sprintf "++%s" (CLExpr.glsl l)
                    else sprintf "%s++" (CLExpr.glsl l)

                | CDecrement(pre, l) ->
                    if pre then sprintf "--%s" (CLExpr.glsl l)
                    else sprintf "%s--" (CLExpr.glsl l)

                | CSequential s ->
                    s |> List.map glsl |> String.concat ";\r\n"

                | CReturn -> "return"
                | CBreak -> "break"
                | CContinue -> "continue"
                | CReturnValue v -> sprintf "return %s" (CExpr.glsl v)

                | CFor(init, cond, step, body) ->
                    sprintf "for(%s; %s; %s)\r\n{\r\n%s;\r\n}" (glsl init) (CExpr.glsl cond) (glsl step) (body |> glsl |> String.indent)

                | CWhile(guard, body) ->
                    sprintf "while(%s)\r\n{\r\n%s\r\n}" (CExpr.glsl guard) (body |> glsl |> String.indent)

                | CDoWhile(guard, body) ->
                    sprintf "do\r\n{\r\n%s\r\n}\r\nwhile(%s)" (body |> glsl |> String.indent) (CExpr.glsl guard)

                | CIfThenElse(c, i, e) ->
                    sprintf "if(%s)\r\n{\r\n%s;\r\n}\r\nelse\r\n{\r\n%s;\r\n}" (CExpr.glsl c) (i |> glsl |> String.indent) (e |> glsl |> String.indent)

                | CSwitch _ -> string s
               
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CUniform =
        let glsl (u : CUniform) =
            match u with
                | CGlobal(t, n) ->
                    sprintf "uniform %s %s;" (CType.glsl t) n
                | CBuffer(n, fields) ->
                    let fields = fields |> List.map (fun (t, n) -> sprintf "%s %s;" (CType.glsl t) n) |> String.concat "\r\n"
                    sprintf "uniform %s\r\n{\r\n%s\r\n}" n (String.indent fields)
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CEntryDef =
        let glsl (c : Config) (e : CEntryDef) =
            let before, after = 
                match e.cConditional with
                    | Some cond -> ([sprintf "#ifdef %s" cond], ["#endif"])
                    | None -> ([], [])

            let inputs = 
                e.cInputs |> List.mapi (fun i v -> 
                    if c.locations then  sprintf "layout(location = %d) in %s %s;" i (CType.glsl v.ctype) v.name
                    else sprintf "in %s %s;" (CType.glsl v.ctype) v.name
                )
            let outputs = 
                e.cOutputs |> List.mapi (fun i v -> 
                    if c.locations then  sprintf "layout(location = %d) out %s %s;" i (CType.glsl v.ctype) v.name
                    else sprintf "out %s %s;" (CType.glsl v.ctype) v.name
                )
            let uniforms = e.cUniforms |> List.map CUniform.glsl
            let args = e.cArguments |> List.map (fun v -> sprintf "%s %s" (CType.glsl v.ctype) v.name) |> String.concat ", " 

            String.concat "\r\n" [
                yield! before
                if c.perStageUniforms then
                    yield! uniforms
                yield! inputs
                yield! outputs
                yield sprintf "%s %s(%s)\r\n{\r\n%s;\r\n}" (CType.glsl e.cReturnType) e.cEntryName args (e.cBody |> CStatement.glsl |> String.indent)
                yield! after
            ]

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CValueDef =
        let glsl (c : Config) (d : CValueDef) =
            match d with
                | CEntryDef e -> 
                    CEntryDef.glsl c e

                | CFunctionDef(signature, body) ->
                    sprintf "%s\r\n{\r\n%s;\r\n}" (CFunctionSignature.glsl signature) (body |> CStatement.glsl |> String.indent)

                | CConstant(t, n, init) ->
                    match t with
                        | CArray(t,l) -> sprintf "const %s %s[%d] = %s;" (CType.glsl t) n l (CRExpr.glsl init)
                        | _ -> sprintf "const %s %s = %s;" (CType.glsl t) n (CRExpr.glsl init)

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
                        yield m.uniforms |> List.map CUniform.glsl

                    yield m.values |> List.map (CValueDef.glsl c)
                ]

            definitions |> String.concat "\r\n\r\n"

        
