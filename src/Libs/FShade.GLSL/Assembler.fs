namespace FShade.GLSL

open System
open System.Reflection

open Aardvark.Base
open Aardvark.Base.Monads.State

open FShade
open FShade.Imperative
open FShade.GLSL.Utilities

type Config =
    {
        version                 : Version
        enabledExtensions       : Set<string>
        createUniformBuffers    : bool
            
        createBindings          : bool
        createDescriptorSets    : bool
        createInputLocations    : bool
        createPerStageUniforms  : bool
        reverseMatrixLogic      : bool
    }

type Backend private(config : Config) =
    inherit Compiler.Backend()
    static let table = System.Collections.Concurrent.ConcurrentDictionary<Config, Backend>()

    member x.Config = config

    static member Create (config : Config) =
        table.GetOrAdd(config, fun config ->
            Backend(config)
        )

    override x.TryGetIntrinsicMethod (c : MethodInfo) =
        match c with
            | IntrinsicFunction f -> Some f
            | TextureLookup fmt -> Some (CIntrinsic.tagged fmt)
            | _ -> c.Intrinsic<GLSLIntrinsicAttribute>()

    override x.TryGetIntrinsicCtor (c : ConstructorInfo) =
        None

    override x.TryGetIntrinsicType (t : Type) =
        match t with
            | SamplerType(dim, arr, shadow, ms, valueType) -> 
                let dimStr =
                    match dim with
                        | SamplerDimension.Sampler1d -> "1D"
                        | SamplerDimension.Sampler2d -> "2D"
                        | SamplerDimension.Sampler3d -> "3D"
                        | SamplerDimension.SamplerCube -> "Cube"
                        | _ -> failwith "unsupported sampler dimension"

                let shadowSuffix = if shadow then "Shadow" else ""
                let msSuffix = if ms then "MS" else ""
                let typePrefix = 
                    match valueType.Name with
                        | "V4i" -> "i"
                        | _ -> ""

                let name = 
                    if arr then sprintf "%ssampler%s%sArray%s" typePrefix dimStr msSuffix shadowSuffix
                    else sprintf "%ssampler%s%s%s" typePrefix dimStr msSuffix shadowSuffix 

                CIntrinsicType.simple name |> Some

            | _ ->
                None

type AssemblerState =
    {
        config                  : Config
        stages                  : ShaderStageDescription
        currentDescriptorSet    : int
        currentBinding          : int
        currentInputLocation    : int
        currentOutputLocation   : int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AssemblerState =
    let ofConfig (c : Config) =
        {
            config = c
            stages = { prev = None; self = ShaderStage.Vertex; next = None }
            currentDescriptorSet = -1
            currentBinding = -1
            currentInputLocation = 0
            currentOutputLocation = 0
        }


    let stages = State.get |> State.map (fun s -> s.stages)
    let stage = State.get |> State.map (fun s -> s.stages.self)
    let prevStage = State.get |> State.map (fun s -> s.stages.prev)
    let nextStage = State.get |> State.map (fun s -> s.stages.next)
    
    let tryGetParameterName (kind : ParameterKind) (name : string) =
        state {
            let! stages = stages
            match kind with
                | ParameterKind.Input ->
                    return Map.tryFind name IntrinsicParameters.builtInInputs.[stages.self]
                | ParameterKind.Output ->
                    if name = Intrinsics.Position && stages.next = Some ShaderStage.Fragment then
                        return Some "gl_Position"
                    else
                        return Map.tryFind name IntrinsicParameters.builtInOutputs.[stages.self]
                | _ ->
                    return None
        }

    let reverseMatrixLogic = State.get |> State.map (fun s -> s.config.reverseMatrixLogic)
    let config = State.get |> State.map (fun s -> s.config)

    let newBinding =
        State.custom (fun s ->
            let c = s.config
            if c.createBindings then
                let b = s.currentBinding
                { s with currentBinding = b + 1 }, b
            else
                s, -1
        )

    let newSet =
        State.custom (fun s ->
            let c = s.config
            if c.createDescriptorSets then
                let set = s.currentDescriptorSet
                { s with currentDescriptorSet = set + 1; currentBinding = -1 }, set
            else
                s, -1
        )

    let rec private neededLocations (t : CType) =
        match t with
            | CMatrix(et, rows, cols) ->
                let l = CVector(et, rows) |> neededLocations
                l * cols
            | CVector(et,_) -> 
                1
            | CArray(et, len) ->
                let inner = neededLocations et
                inner * len

            | CStruct _ ->
                failwith "[GLSL] no struct inputs allowed"

            | _ ->
                1

    let newLocation (kind : ParameterKind) (t : CType) =
        State.custom (fun s ->
            match kind with
                | ParameterKind.Input ->
                    let free = s.currentInputLocation
                    { s with currentInputLocation = free + neededLocations t }, free
                | ParameterKind.Output ->
                    let free = s.currentOutputLocation
                    { s with currentOutputLocation = free + neededLocations t }, free
                | _ ->
                    s, -1
                    
        )

module Assembler =
    let version120 = Version(1,2)

    let parameterNameS (kind : ParameterKind) (name : string) =
        state {
            let! builtIn = AssemblerState.tryGetParameterName kind name
            match builtIn with
                | Some name -> return name
                | None ->
                    let! stages = AssemblerState.stages
                    let name =
                        if name = Intrinsics.FragmentPosition then Intrinsics.Position
                        else name

                    match kind, stages with
                        | ParameterKind.Input, { prev = None }              -> return name
                        | ParameterKind.Input, { self = s }                 -> return prefixes.[s] + name

                        | ParameterKind.Output, { next = Some n }           -> return prefixes.[n] + name
                        | ParameterKind.Output, { next = None }             -> return name + "Out"
                        | _                                                 -> return name
        }

    let rec assembleType (t : CType) =
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

            | CType.CArray(t, l)                        -> assembleType t + "[" + string l + "]"
            | CType.CStruct(n,_,_)                      -> n

            | CType.CIntrinsic it                       -> it.intrinsicTypeName

            | _ -> failwithf "[GLSL] cannot assemble type %A" t 

    let assembleDeclaration (t : CType) (name : string) =
        match t with
            | CArray(et, len) ->
                sprintf "%s %s[%d]" (assembleType et) name len
            | t ->
                sprintf "%s %s" (assembleType t) name
        

    let assembleParameter (p : CParameter) =
        let modifier =
            match p.modifier with
                | CParameterModifier.In -> ""
                | CParameterModifier.ByRef -> "inout "

        let decl = assembleDeclaration p.ctype p.name
        sprintf "%s%s" modifier decl

    let assembleFunctionSignature (s : CFunctionSignature) =
        let ret = s.returnType |> assembleType
        let args = s.parameters |> Seq.map assembleParameter |> String.concat ", "
        sprintf "%s %s(%s)" ret s.name args

    let assembleLiteral (l : CLiteral) =
        match l with
            | CLiteral.CBool v -> if v then "true" else "false"
            | CLiteral.Null -> "null"
            | CLiteral.CIntegral v -> string v
            | CLiteral.CFractional v -> 
                let str = v.ToString(System.Globalization.CultureInfo.InvariantCulture)
                if str.Contains "." || str.Contains "E" || str.Contains "e" then str
                else str + ".0"

            | CLiteral.CString v -> "\"" + v + "\""        

    let rec assembleVecSwizzle (c : list<CVecComponent>) =
        match c with
            | CVecComponent.X :: rest -> "x" + assembleVecSwizzle rest
            | CVecComponent.Y :: rest -> "y" + assembleVecSwizzle rest
            | CVecComponent.Z :: rest -> "z" + assembleVecSwizzle rest
            | CVecComponent.W :: rest -> "w" + assembleVecSwizzle rest
            | _ -> ""

    let rec assembleExprS (e : CExpr) =
        state {
            
            match e with
                | CVar v ->
                    return v.name

                | CValue(_, v) ->
                    return assembleLiteral v

                | CCall(func, args) ->
                    let! args = args |> assembleExprsS ", "
                    return sprintf "%s(%s)" func.name args

                | CReadInput(kind, _, name, index) ->
                    let! name = parameterNameS kind name
                    match index with
                        | Some index ->
                            let! index = assembleExprS index
                            return sprintf "%s[%s]" name index
                        | None ->
                            return name

                | CCallIntrinsic(_, func, args) ->
                    match func.tag with
                        | null ->
                            let! args = args |> assembleExprsS ", "
                            return sprintf "%s(%s)" func.intrinsicName args

                        | :? string as format ->
                            let! args = args |> Array.mapS (assembleExprS >> State.map (fun a -> a :> obj))
                            return String.Format(format, args)
                            
                        | _ ->
                            return failwithf "[FShade] invalid GLSL intrinsic: %A" func

                | CConditional(_, cond, i, e) ->
                    let! cond = assembleExprS cond
                    let! i = assembleExprS i
                    let! e = assembleExprS e
                    return sprintf "(%s ? %s : %s)" cond i e

                | CNeg(_, v) ->
                    let! v = assembleExprS v
                    return sprintf "(-%s)" v

                | CNot(_, v) ->
                    let! v = assembleExprS v
                    return sprintf "(!%s)" v

                | CAdd(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s + %s)" l r

                | CSub(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s - %s)" l r

                | CMul(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s * %s)" l r

                | CDiv(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s / %s)" l r

                | CMod(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s %% %s)" l r

                | CMulMatMat(_, l, r) | CMulMatVec(_, l, r) ->
                    let! reverse = AssemblerState.reverseMatrixLogic
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    if reverse then return sprintf "(%s * %s)" r l
                    else return sprintf "(%s * %s)" l r

                | CMatrixElement(_, m, r, c) ->
                    let! reverse = AssemblerState.reverseMatrixLogic
                    let! m = assembleExprS m
                    if reverse then return sprintf "%s[%d][%d]" m r c
                    else return sprintf "%s[%d][%d]" m c r

                | CTranspose(_, m) ->
                    let! m = assembleExprS m
                    return sprintf "transpose(%s)" m

                | CDot(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "dot(%s, %s)" l r

                | CCross(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "cross(%s, %s)" l r

                | CVecSwizzle(_, v, s) ->
                    let! v = assembleExprS v
                    let s = assembleVecSwizzle s
                    return sprintf "%s.%s" v s

                | CNewVector(r, _, args) ->
                    let! args = assembleExprsS ", " args
                    let t = assembleType r
                    return sprintf "%s(%s)" t args

                | CVecLength(_, v) ->
                    let! v = assembleExprS v
                    return sprintf "length(%s)" v

                | CConvert(t, v) ->
                    let t = assembleType t
                    let! v = assembleExprS v
                    return sprintf "%s(%s)" t v

                | CAnd(l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s && %s)" l r

                | COr(l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s || %s)" l r

                | CBitAnd(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s & %s)" l r

                | CBitOr(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s | %s)" l r

                | CBitXor(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s ^ %s)" l r
                    
                | CLeftShift(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s << %s)" l r

                | CRightShift(_, l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s >> %s)" l r

                | CLess(l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s < %s)" l r

                | CLequal(l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s <= %s)" l r

                | CGreater(l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s > %s)" l r

                | CGequal(l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s >= %s)" l r

                | CEqual(l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s == %s)" l r

                | CNotEqual(l, r) ->
                    let! l = assembleExprS l
                    let! r = assembleExprS r
                    return sprintf "(%s != %s)" l r

                | CAddressOf(_, v) ->
                    let! v = assembleExprS v
                    return sprintf "(&%s)" v

                | CField(_, v, f) ->
                    let! v = assembleExprS v
                    return sprintf "%s.%s" v f

                | CItem(_, v, i) ->
                    let! v = assembleExprS v
                    let! i = assembleExprS i
                    return sprintf "%s[%s]" v i

                | CMatrixFromCols _ | CMatrixFromRows _ ->
                    return failwith "[GLSL] not implemented"


        }
    
    and assembleExprsS (join : string) (args : seq<CExpr>) =
        args |> Seq.mapS assembleExprS |> State.map (String.concat join)
     
    let assembleLExprS (e : CLExpr) =
        e |> CLExpr.toExpr |> assembleExprS
      
    let assembleRExprS (e : CRExpr) =
        state {
            match e with
                | CRExpr e -> 
                    return! assembleExprS e

                | CRArray(t, args) ->
                    let et =
                        match t with
                            | CArray(t,_) -> t
                            | CPointer(_,t) -> t
                            | _ -> t

                    let ct = assembleType et
                    let! args = args |> List.mapS assembleExprS |> State.map (String.concat ", ")
                    return sprintf "%s[]( %s )" ct args
        }

    let rec assembleStatementS (singleLine : bool) (s : CStatement) =
        state {
            let seq (s : seq<string>) =
                if singleLine then s |> String.concat " "
                else s |> String.concat "\r\n"

            match s with
                | CNop ->
                    return ""

                | CDo e ->
                    let! e = assembleExprS e
                    return sprintf "%s;" e

                | CDeclare(v, r) ->
                    let! r = r |> Option.mapS assembleRExprS
                    let decl = assembleDeclaration v.ctype v.name
                    match r with
                        | Some r -> return sprintf "%s = %s;" decl r
                        | None -> return sprintf "%s;" decl

                | CWriteOutput(name, index, value) ->
                    let! name = parameterNameS ParameterKind.Output name
                    let! index = index |> Option.mapS assembleExprS

                    let name =
                        match index with
                            | Some index -> sprintf "%s[%s]" name index
                            | None -> name

                    match value with
                        | CRArray(t, args) ->
                            let! args = args |> List.mapS assembleExprS
                            return args |> Seq.mapi (sprintf "%s[%d] = %s;" name) |> seq
                        | CRExpr e ->
                            let! value = assembleExprS e
                            return sprintf "%s = %s;" name value

                | CWrite(l, r) ->
                    let! l = assembleLExprS l
                    let! r = assembleExprS r
                    return sprintf "%s = %s;" l r

                | CIncrement(pre, v) ->
                    let! v = assembleLExprS v
                    if pre then return sprintf "++%s;" v
                    else return sprintf "%s++;" v

                | CDecrement(pre, v) ->
                    let! v = assembleLExprS v
                    if pre then return sprintf "--%s;" v
                    else return sprintf "%s--;" v

                | CSequential statements ->
                    let! statements = statements |> List.mapS (assembleStatementS false) 
                    return statements |> seq

                | CReturnValue v ->
                    let! v = assembleExprS v
                    return sprintf "return %s;" v

                | CReturn -> 
                    return "return;"

                | CBreak -> 
                    return "break;"

                | CContinue -> 
                    return "continue;"

                | CFor(init, cond, step, body) ->
                    let! init = assembleStatementS true init
                    let! cond = assembleExprS cond
                    let! step = assembleStatementS true step
                    let! body = assembleStatementS false body

                    let init =
                        if init.EndsWith ";" then init
                        else init + ";"

                    let step =
                        if step.EndsWith ";" then step.Substring(0, step.Length - 1)
                        else step

                    return sprintf "for(%s %s; %s)\r\n{\r\n%s\r\n}" init cond step (String.indent body)

                | CWhile(guard, body) ->
                    let! guard = assembleExprS guard
                    let! body = assembleStatementS false body
                    return sprintf "while(%s)\r\n{\r\n%s\r\n}" guard (String.indent body)

                | CDoWhile(guard, body) ->
                    let! guard = assembleExprS guard
                    let! body = assembleStatementS false body
                    return sprintf "do\r\n{\r\n%s\r\n}\r\nwhile(%s);" (String.indent body) guard
                    
                | CIfThenElse(cond, i, CNop) ->
                    let! cond = assembleExprS cond
                    let! i = assembleStatementS false i
                    return sprintf "if(%s)\r\n{\r\n%s\r\n}" cond (String.indent i)

                | CIfThenElse(cond, CNop, e) ->
                    let! cond = assembleExprS cond
                    let! e = assembleStatementS false e
                    return sprintf "if(!(%s))\r\n{\r\n%s\r\n}" cond (String.indent e)

                | CIfThenElse(cond, i, e) ->
                    let! cond = assembleExprS cond
                    let! i = assembleStatementS false i
                    let! e = assembleStatementS false e
                    return sprintf "if(%s)\r\n{\r\n%s\r\n}\r\nelse\r\n{\r\n%s\r\n}" cond (String.indent i) (String.indent e)

                | CSwitch(value, cases) ->
                    return failwith "[GLSL] switch not implemented"

        }

    let private uniformLayout (set : int) (binding : int) =
        if set >= 0 && binding >= 0 then
            sprintf "layout(set = %d, binding = %d)\r\n" set binding
        elif set >= 0 then
            sprintf "layout(set = %d)\r\n" set
        elif binding >= 0 then
            sprintf "layout(binding = %d)\r\n" binding
        else
            ""


    let assembleUniformsS (uniforms : list<CUniform>) =
        state {
            let! config = AssemblerState.config
            let buffers =
                uniforms 
                    |> List.groupBy (fun u -> u.cUniformBuffer)

            let! set = AssemblerState.newSet
            let! definitions =
                buffers |> List.mapS (fun (name, fields) ->
                    state {
                        let fields = 
                            fields |> List.map (fun u -> 
                                let decl = assembleDeclaration u.cUniformType u.cUniformName
                                sprintf "%s;" decl    
                            )
                        match name with
                            | Some bufferName when config.createUniformBuffers ->
                                let! binding = AssemblerState.newBinding
                                    
                                let fields = fields |> String.concat "\r\n"
                                let prefix = uniformLayout set binding
                            
                                return sprintf "%suniform %s\r\n{\r\n%s\r\n};\r\n" prefix bufferName (String.indent fields)

                            | _ ->
                                let! definitions = 
                                    fields |> List.mapS (fun f ->
                                        state {
                                            let! binding = AssemblerState.newBinding
                                            let prefix = uniformLayout set binding
                                            return sprintf "%suniform %s" prefix f
                                        }
                                    )

                                return definitions |> String.concat "\r\n"
                    }
                )
                
            return String.concat "\r\n\r\n" definitions

                
        }

    let assembleEntryParameterS (kind : ParameterKind) (p : CEntryParameter) =
        state {
            let! stages = AssemblerState.stages
            let! builtIn = AssemblerState.tryGetParameterName kind p.cParamSemantic
            match builtIn with
                | Some _ -> 
                    return None

                | None ->
                    let! config = AssemblerState.config
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
                                        | InterpolationMode.PerPatch -> Some "patch"
                                        | _ -> None
                                | ParameterDecoration.Memory _ | ParameterDecoration.Slot _ ->
                                    None

                        )

                    let slot = p.cParamDecorations |> Seq.tryPick (function ParameterDecoration.Slot s -> Some s | _ -> None)

                    let! location = 
                        match slot with
                            | Some slot -> State.value slot
                            | _ -> AssemblerState.newLocation kind p.cParamType

                    let decorations =
                        match kind with
                            | ParameterKind.Input | ParameterKind.Output when config.createInputLocations && config.version > version120 ->
                                sprintf "layout(location = %d)" location :: decorations
                            | _ ->
                                decorations
                
                    let decorations = 
                        match decorations with
                            | [] -> ""
                            | _ -> String.concat " " decorations + " "

                    let! name = parameterNameS kind p.cParamName

                    let prefix =
                        if config.version > version120 then
                            match kind with
                                | ParameterKind.Input -> "in "
                                | ParameterKind.Output -> "out "
                                | _ -> ""
                        else
                            match kind with
                                | ParameterKind.Input -> "varying "
                                | ParameterKind.Output -> "varying "
                                | _ -> ""
                    

                    match p.cParamType with
                        | CPointer(_, t) ->
                            return sprintf "%s%s%s %s[];" decorations prefix (assembleType t) name |> Some
                        | _ -> 
                            return sprintf "%s%s%s %s;" decorations prefix (assembleType p.cParamType) name |> Some
        }
    
    let assembleEntryS (e : CEntryDef) =
        state {
            let stages =
                e.cDecorations 
                    |> List.tryPick (function EntryDecoration.Stages t -> Some t | _ -> None) 
                    |> Option.defaultValue {
                        prev = None
                        self = ShaderStage.Vertex
                        next = None
                    }

            do! State.modify (fun s ->
                { s with
                    AssemblerState.stages = stages
                    AssemblerState.currentInputLocation = 0
                    AssemblerState.currentOutputLocation = 0
                }
            )

            let prefix = 
                match stages.self with
                    | ShaderStage.Geometry -> 
                        let inputTopology = e.cDecorations |> List.tryPick (function EntryDecoration.InputTopology t -> Some t | _ -> None) |> Option.get
                        let outputTopology = e.cDecorations |> List.tryPick (function EntryDecoration.OutputTopology(t) -> Some(t) | _ -> None) |> Option.get
                        let vertexCount = e.cDecorations |> List.tryPick (function EntryDecoration.OutputVertices(t) -> Some(t) | _ -> None) |> Option.get
                        
                        let inputPrimitive =
                            match inputTopology with
                                | InputTopology.Point -> "points"
                                | InputTopology.Line -> "lines"
                                | InputTopology.LineAdjacency -> "lines_adjacency"
                                | InputTopology.Triangle -> "triangles"
                                | InputTopology.TriangleAdjacency -> "triangles_adjacency"
                                | InputTopology.Patch _ -> failwith "[FShade] GeometryShaders cannot use patches"

                        let outputPrimitive =
                            match outputTopology with
                                | OutputTopology.Points -> "points"
                                | OutputTopology.LineStrip -> "line_strip"
                                | OutputTopology.TriangleStrip -> "triangle_strip"

                        [
                            sprintf "layout(%s) in;" inputPrimitive
                            sprintf "layout(%s, max_vertices = %d) out;" outputPrimitive vertexCount
                        ]

                    | ShaderStage.TessControl ->
                        let inputTopology = e.cDecorations |> List.tryPick (function EntryDecoration.InputTopology t -> Some t | _ -> None) |> Option.get
                        
                        let vertices =
                            match inputTopology with
                                | InputTopology.Point -> 1
                                | InputTopology.Line -> 2
                                | InputTopology.LineAdjacency -> 4
                                | InputTopology.Triangle -> 3
                                | InputTopology.TriangleAdjacency -> 6
                                | InputTopology.Patch n -> n
                        
                        [
                            sprintf "layout(vertices = %d) out;" vertices
                        ]

                    | ShaderStage.TessEval ->
                        let inputTopology = e.cDecorations |> List.tryPick (function EntryDecoration.InputTopology t -> Some t | _ -> None) |> Option.get
                        
                        let inputTop =
                            match inputTopology with
                                | InputTopology.Line -> "isolines"
                                | InputTopology.Triangle -> "triangles"
                                | InputTopology.Patch 3 -> "triangles"
                                | InputTopology.Patch 4 -> "quads"
                                | t -> failwithf "[FShade] TessEvalShader cannot have %A inputs" t

                        [
                            sprintf "layout(%s, equal_spacing, ccw) in;" inputTop
                        ]
                    | _ ->
                        []

            let! inputs = e.cInputs |> List.chooseS (assembleEntryParameterS ParameterKind.Input)
            let! outputs = e.cOutputs |> List.chooseS (assembleEntryParameterS ParameterKind.Output)
            let! args = e.cArguments |> List.chooseS (assembleEntryParameterS ParameterKind.Argument) |> State.map (String.concat ", " )
            let! body = assembleStatementS false e.cBody

            return 
                String.concat "\r\n" [
                    yield! prefix
                    yield! inputs
                    yield! outputs
                    yield sprintf "%s %s(%s)\r\n{\r\n%s\r\n}" (assembleType e.cReturnType) e.cEntryName args (String.indent body)
                ]
        }

    let rec assembleValueDefS (d : CValueDef) =
        state {
            match d with
                | CConditionalDef(d, inner) ->
                    let! inner = inner |> List.mapS assembleValueDefS
                    let inner = inner |> String.concat "\r\n"
                    return sprintf "\r\n#ifdef %s\r\n%s\r\n\r\n#endif\r\n" d inner

                | CEntryDef e -> 
                    return! assembleEntryS e

                | CFunctionDef(signature, body) ->
                    let signature = assembleFunctionSignature signature
                    let! body = assembleStatementS false body
                    return sprintf "%s\r\n{\r\n%s\r\n}\r\n" signature (String.indent body)

                | CConstant(t, n, init) ->
                    let! init = assembleRExprS init
                    match t with
                        | CArray(t,l) -> return sprintf "const %s %s[%d] = %s;" (assembleType t) n l init
                        | CPointer(_,t) -> return sprintf "const %s %s[] = %s;" (assembleType t) n init
                        | _ -> return sprintf "const %s %s = %s;" (assembleType t) n init

                | CUniformDef us ->
                    let! config = AssemblerState.config
                    if config.createPerStageUniforms then
                        return! assembleUniformsS us
                    else
                        return ""
        }

    let assembleTypeDef (d : CTypeDef) =
        match d with
            | CStructDef(name, fields) ->
                let fields = fields |> List.map (fun (t, n) -> sprintf "%s %s;" (assembleType t) n) |> String.concat "\r\n"
                sprintf "struct %s\r\n{\r\n%s\r\n};" name (String.indent fields)

    let assemble (backend : Backend) (m : CModule) =
        let c = backend.Config
        let definitions =
            state {
                let types = m.types |> List.map assembleTypeDef

                let! uniforms = 
                    if not c.createPerStageUniforms then assembleUniformsS m.uniforms |> State.map List.singleton
                    else State.value []

                let! values = m.values |> List.mapS assembleValueDefS


                return 
                    List.concat [
                        [sprintf "#version %d%d0" c.version.Major c.version.Minor ]
                        c.enabledExtensions |> Seq.map (sprintf "#extension %s : enable;") |> Seq.toList
                        types
                        uniforms
                        values
                    ]
            }
        let mutable state = AssemblerState.ofConfig c
        definitions.Run(&state) |> String.concat "\r\n\r\n"
