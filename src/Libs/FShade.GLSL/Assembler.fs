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
        stepDescriptorSets      : bool
        createInputLocations    : bool
        createPerStageUniforms  : bool
        reverseMatrixLogic      : bool
    }

type GLSLShader =
    {
        code        : string
        builtIns    : Map<ShaderStage, Map<ParameterKind, Set<string>>>
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

            | ImageType(fmt, dim, arr, ms, valueType) ->
                let dimStr =
                    match dim with
                        | SamplerDimension.Sampler1d -> "1D"
                        | SamplerDimension.Sampler2d -> "2D"
                        | SamplerDimension.Sampler3d -> "3D"
                        | SamplerDimension.SamplerCube -> "Cube"
                        | _ -> failwith "unsupported sampler dimension"

                let msSuffix = if ms then "MS" else ""
                let typePrefix = 
                    match valueType.Name with
                        | "V4i" -> "i"
                        | _ -> ""

                let fmt = fmt.Name


                let name = 
                    if arr then sprintf "%simage%s%sArray" typePrefix dimStr msSuffix
                    else sprintf "%simage%s%s" typePrefix dimStr msSuffix 

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
        builtIn                 : Map<ShaderStage, Map<ParameterKind, Set<string>>>
        requiredExtensions      : Set<string>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AssemblerState =
    let ofConfig (c : Config) =
        {
            config = c
            stages = { prev = None; self = ShaderStage.Vertex; next = None }
            currentDescriptorSet = 0
            currentBinding = 0
            currentInputLocation = 0
            currentOutputLocation = 0
            builtIn = Map.empty
            requiredExtensions = Set.empty
        }


    let stages = State.get |> State.map (fun s -> s.stages)
    let stage = State.get |> State.map (fun s -> s.stages.self)
    let prevStage = State.get |> State.map (fun s -> s.stages.prev)
    let nextStage = State.get |> State.map (fun s -> s.stages.next)
    
    let useBuiltIn (kind : ParameterKind) (name : string) =
        State.modify (fun s ->
            let stage =s.stages.self

            let old = 
                match Map.tryFind stage s.builtIn with
                    | Some b -> b
                    | None -> Map.empty
            
            let oldSet =
                match Map.tryFind kind old with
                    | Some s -> s
                    | None -> Set.empty

            { s with builtIn = Map.add stage (Map.add kind (Set.add name oldSet) old) s.builtIn }
        )

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
                if c.stepDescriptorSets then
                    { s with currentDescriptorSet = set + 1; currentBinding = 0 }, set
                else
                    s, set
                    
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

    [<Struct>]
    type Identifier(str : string) = 
        member x.Name = str

    open System.Text.RegularExpressions

    let reservedNames =
        let str = 
            String.concat "|" [
                "precision|highp|mediump|lowp"
                "break|case|continue|default|discard|do|else|for|if|return|switch|while"
                "void|bool|int|uint|float|double|vec[2|3|4]|dvec[2|3|4]|bvec[2|3|4]|ivec[2|3|4]|uvec[2|3|4]|mat[2|3|4]"
                "mat2x2|mat2x3|mat2x4|mat3x2|mat3x3|mat3x4|mat4x2|mat4x3|mat4x4|dmat2|dmat3|dmat4|dmat2x2|dmat2x3|dmat2x4|dmat3x2|dmat3x3|dmat3x4|dmat4x2|dmat4x3|dmat4x4"
                "sampler[1|2|3]D|image[1|2|3]D|samplerCube|imageCube|sampler2DRect|image2DRect|sampler[1|2]DArray|image[1|2]DArray|samplerBuffer|imageBuffer|sampler2DMS|image2DMS|sampler2DMSArray|image2DMSArray|samplerCubeArray|imageCubeArray|sampler[1|2]DShadow|sampler2DRectShadow|sampler[1|2]DArrayShadow|samplerCubeShadow|samplerCubeArrayShadow|isampler[1|2|3]D|iimage[1|2|3]D|isamplerCube|iimageCube|isampler2DRect|iimage2DRect|isampler[1|2]DArray|iimage[1|2]DArray|isamplerBuffer|iimageBuffer|isampler2DMS|iimage2DMS|isampler2DMSArray|iimage2DMSArray|isamplerCubeArray|iimageCubeArray|atomic_uint|usampler[1|2|3]D|uimage[1|2|3]D|usamplerCube|uimageCube|usampler2DRect|uimage2DRect|usampler[1|2]DArray|uimage[1|2]DArray|usamplerBuffer|uimageBuffer|usampler2DMS|uimage2DMS|usampler2DMSArray|uimage2DMSArray|usamplerCubeArray|uimageCubeArray|struct"
                "layout|attribute|centroid|sampler|patch|const|flat|in|inout|invariant|noperspective|out|smooth|uniform|varying|buffer|shared|coherent|readonly|writeonly"
                "abs|acos|all|any|asin|atan|ceil|clamp|cos|cross|degrees|dFdx|dFdy|distance|dot|equal|exp|exp2|faceforward|floor|fract|ftransform|fwidth|greaterThan|greaterThanEqual|inversesqrt|length|lessThan|lessThanEqual|log|log2|matrixCompMult|max|min|mix|mod|noise[1-4]|normalize|not|notEqual|outerProduct|pow|radians|reflect|refract|shadow1D|shadow1DLod|shadow1DProj|shadow1DProjLod|shadow2D|shadow2DLod|shadow2DProj|shadow2DProjLod|sign|sin|smoothstep|sqrt|step|tan|texture1D|texture1DLod|texture1DProj|texture1DProjLod|texture2D|texture2DLod|texture2DProj|texture2DProjLod|texture3D|texture3DLod|texture3DProj|texture3DProjLod|textureCube|textureCubeLod|transpose"
                "(gl_.*)"
            ]
        Regex("^(" + str + ")$")


    let checkName (name : string) =
        if name.Contains "__" then
            failwithf "[GLSL] %A is not a GLSL compatible name" name

        if reservedNames.IsMatch name then
            failwithf "[GLSL] %A is not a GLSL compatible name" name
            
        Identifier(name)

    let glslName (name : string) =
        let name = name.Replace("__", "_")
        if reservedNames.IsMatch name then
            Identifier("_" + name)
        else
            Identifier(name)

    let parameterNameS (kind : ParameterKind) (name : string) =
        state {
            let! builtIn = AssemblerState.tryGetParameterName kind name
            match builtIn with
                | Some name -> 
                    return Identifier(name)

                | None ->
                    let! stages = AssemblerState.stages
                    let name =
                        if name = Intrinsics.FragmentPosition then Intrinsics.Position
                        else name

                    match kind, stages with
                        | _, { self = ShaderStage.Compute }                 -> return checkName name

                        | ParameterKind.Input, { prev = None }              -> return checkName name

                        | ParameterKind.Input, { self = s }                 -> return prefixes.[s] + name |> glslName
                        | ParameterKind.Output, { next = Some n }           -> return prefixes.[n] + name |> glslName
                        | ParameterKind.Output, { next = None }             -> 
                            let name = name + "Out"
                            return checkName name

                        | _                                                 -> 
                            return checkName name
        }

    let rec assembleType (t : CType) =
        match t with
            | CType.CBool                               -> "bool"  |> Identifier
            | CType.CVoid                               -> "void"  |> Identifier
            | CType.CInt(true, (8 | 16 | 32))           -> "int"   |> Identifier
            | CType.CInt(false, (8 | 16 | 32))          -> "uint"  |> Identifier

            | CType.CInt(true, 64)                      -> "int64_t" |> Identifier
            | CType.CInt(false, 64)                     -> "uint64_t" |> Identifier

            | CType.CFloat(16)                          -> "half"  |> Identifier
            | CType.CFloat(32 | 64)                     -> "float" |> Identifier
                
            | CType.CVector(CType.CInt(true, (8 | 16 | 32 | 64)), d)   -> "ivec" + string d |> Identifier
            | CType.CVector(CType.CFloat(32 | 64), d)   -> "vec" + string d |> Identifier
            | CType.CMatrix(CType.CFloat(32 | 64), r,c) -> "mat" + string c + "x" + string r |> Identifier

            | CType.CArray(t, l)                        -> assembleType(t).Name + "[" + string l + "]" |> Identifier
            | CType.CStruct(n,_,_)                      -> glslName n

            | CType.CIntrinsic it                       -> it.intrinsicTypeName |> Identifier

            | _ -> failwithf "[GLSL] cannot assemble type %A" t 

    let assembleDeclaration (t : CType) (name : Identifier) =
        match t with
            | CArray(et, len) ->
                sprintf "%s %s[%d]" (assembleType et).Name name.Name len
            | t ->
                sprintf "%s %s" (assembleType t).Name name.Name
        

    let assembleParameter (p : CParameter) =
        let modifier =
            match p.modifier with
                | CParameterModifier.In -> ""
                | CParameterModifier.ByRef -> "inout "
                | CParameterModifier.Out -> "out "

        let decl = assembleDeclaration p.ctype (glslName p.name)
        sprintf "%s%s" modifier decl

    let assembleFunctionSignature (s : CFunctionSignature) =
        let ret = s.returnType |> assembleType
        let args = s.parameters |> Seq.map assembleParameter |> String.concat ", "
        let name = glslName s.name
        sprintf "%s %s(%s)" ret.Name name.Name args

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
                    let name = glslName v.name
                    return name.Name

                | CValue(_, v) ->
                    return assembleLiteral v

                | CCall(func, args) ->
                    let name = glslName func.name
                    let! args = args |> assembleExprsS ", "
                    return sprintf "%s(%s)" name.Name args

                | CReadInput(kind, _, name, index) ->
                    let! name = parameterNameS kind name
                    match index with
                        | Some index ->
                            let! index = assembleExprS index
                            return sprintf "%s[%s]" name.Name index
                        | None ->
                            return name.Name

                | CCallIntrinsic(_, func, args) ->

                    match func.additional with
                        | null -> ()
                        | :? Set<string> as exts -> do! State.modify (fun s -> { s with requiredExtensions = Set.union s.requiredExtensions exts})
                        | _ -> ()

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

                | CMod(t, l, r) ->
                    match t with
                        | CInt _ | CVector(CInt _, _) | CMatrix(CInt _, _, _) ->
                            let! l = assembleExprS l
                            let! r = assembleExprS r
                            return sprintf "(%s %% %s)" l r
                        | _ ->
                            let! l = assembleExprS l
                            let! r = assembleExprS r
                            return sprintf "mod(%s, %s)" l r
                            

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
                    return sprintf "%s(%s)" t.Name args

                | CVecLength(_, v) ->
                    let! v = assembleExprS v
                    return sprintf "length(%s)" v

                | CConvert(t, v) ->
                    let t = assembleType t
                    let! v = assembleExprS v
                    return sprintf "%s(%s)" t.Name v

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
                    let f = glslName f
                    return sprintf "%s.%s" v f.Name

                | CItem(_, v, i) ->
                    let! v = assembleExprS v
                    let! i = assembleExprS i
                    return sprintf "%s[%s]" v i

                | CMatrixFromCols(t,cols) ->
                    let! cols = cols |> List.mapS assembleExprS
                    let! rev = AssemblerState.reverseMatrixLogic
                    let t = assembleType t

                    let code = sprintf "%s(%s)" t.Name (String.concat ", " cols)
                    if rev then return sprintf "transpose(%s)" code
                    else return code

                | CMatrixFromRows(t, rows) ->
                    let! rows = rows |> List.mapS assembleExprS
                    let! rev = AssemblerState.reverseMatrixLogic
                    let t = assembleType t

                    let code = sprintf "%s(%s)" t.Name (String.concat ", " rows)
                    if rev then return code
                    else return sprintf "transpose(%s)" code

                | CNewMatrix(t, elems) ->
                    match t with
                        | CMatrix(_,rows,cols) ->
                            let! elems = elems |> List.mapS assembleExprS
                            let! rev = AssemblerState.reverseMatrixLogic
                            let t = assembleType t

                            if rev then 
                                return sprintf "%s(%s)" t.Name (String.concat ", " elems)
                            else
                                // transpose
                                let elems =
                                    let elems = List.toArray elems
                                    [
                                        for ri in 0 .. rows - 1 do
                                            for ci in 0 .. cols - 1 do
                                                yield elems.[rows * ci + ri]
                                    ]

                                return sprintf "%s(%s)" t.Name (String.concat ", " elems)

                        | t ->
                            return failwithf "[GLSL] invalid matrix type %A" t
                
                | CMatrixRow(t, m, row) ->
                    match t with
                        | CVector(_,d) ->
                            let! m = assembleExprS m
                            let! rev = AssemblerState.reverseMatrixLogic
                            if rev then 
                                return sprintf "%s[%d]" m row
                            else 
                                let t = assembleType t
                                let args = List.init d (fun i -> sprintf "%s[%d][%d]" m i row)
                                return sprintf "%s(%s)" t.Name (String.concat ", " args)
                        | _ ->
                            return failwith "sadsadsad"

                | CMatrixCol(t, m, col) ->
                    match t with
                        | CVector(_,d) ->
                            let! m = assembleExprS m
                            let! rev = AssemblerState.reverseMatrixLogic
                            if rev then 
                                let t = assembleType t
                                let args = List.init d (fun i -> sprintf "%s[%d][%d]" m i col)
                                return sprintf "%s(%s)" t.Name (String.concat ", " args)
                            else
                                return sprintf "%s[%d]" m col
                        | _ ->
                            return failwith "sadsadsad"
                    

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
                    return sprintf "%s[]( %s )" ct.Name args
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
                    let name = glslName v.name
                    let! r = r |> Option.mapS assembleRExprS
                    let decl = assembleDeclaration v.ctype name
                    match r with
                        | Some r -> return sprintf "%s = %s;" decl r
                        | None -> return sprintf "%s;" decl

                | CWriteOutput(name, index, value) ->
                    let! name = parameterNameS ParameterKind.Output name
                    let! index = index |> Option.mapS assembleExprS

                    let name =
                        match index with
                            | Some index -> sprintf "%s[%s]" name.Name index
                            | None -> name.Name

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

                | CIsolated statements ->
                    let! statements = statements |> List.mapS (assembleStatementS false) 
                    let inner = statements |> seq
                    return sprintf "{\r\n%s\r\n}" (String.indent inner)

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

    let private uniformLayout (decorations : list<UniformDecoration>) (set : int) (binding : int) =

        let decorations =
            decorations |> List.choose (fun d ->
                match d with
                    | UniformDecoration.Format t -> Some t.Name
            )
            
        let decorations =
            if binding >= 0 then (sprintf "binding = %d" binding) :: decorations
            else decorations

        let decorations =
            if set >= 0 then (sprintf "set = %d" set) :: decorations
            else decorations

               
        match decorations with
            | [] -> ""
            | d -> d |> String.concat ", " |> sprintf "layout(%s)\r\n" 



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
                                let decl = assembleDeclaration u.cUniformType (checkName u.cUniformName)
                                let decl = sprintf "%s;" decl 
                                decl, u.cUniformDecorations
                            )
                        match name with
                            | Some "SharedMemory" ->
                                let fields = fields |> List.map (fun (d,_) -> sprintf "shared %s" d) |> String.concat "\r\n"
                                return fields
                                
                            | Some bufferName when config.createUniformBuffers ->
                                let bufferName = checkName bufferName
                                let! binding = AssemblerState.newBinding
                                    
                                let fields = fields |> List.map fst |> String.concat "\r\n"
                                let prefix = uniformLayout [] set binding
                            
                                return sprintf "%suniform %s\r\n{\r\n%s\r\n};\r\n" prefix bufferName.Name (String.indent fields)

                            | _ ->
                                let! definitions = 
                                    fields |> List.mapS (fun (f, decorations) ->
                                        state {
                                            let! binding = AssemblerState.newBinding
                                            let prefix = uniformLayout decorations set binding
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
                | Some name -> 
                    do! AssemblerState.useBuiltIn kind name
                    return None

                | None ->
                    let! config = AssemblerState.config

                    let! set = 
                        if config.createDescriptorSets then AssemblerState.newSet
                        else State.value -1

                    let! decorations =
                        p.cParamDecorations 
                        |> Set.toList
                        |> List.chooseS (fun d ->
                            state {
                                match d with
                                    | ParameterDecoration.Const -> return Some "const"
                                    | ParameterDecoration.Interpolation m ->
                                        match m with
                                            | InterpolationMode.Centroid -> return Some "centroid"
                                            | InterpolationMode.Flat -> return Some "flat"
                                            | InterpolationMode.NoPerspective -> return Some "noperspective"
                                            | InterpolationMode.Perspective -> return Some "perspective"
                                            | InterpolationMode.Sample -> return Some "sample"
                                            | InterpolationMode.PerPatch -> return Some "patch"
                                            | _ -> return None

                                    | ParameterDecoration.StorageBuffer ->
                                        let! binding =
                                            if config.createBindings then AssemblerState.newBinding
                                            else State.value -1

                                        let args = []

                                        let args =
                                            if set >= 0 then sprintf "set=%d" set :: args
                                            else args

                                        let args =
                                            if binding >= 0 then sprintf "binding=%d" binding :: args
                                            else args

                                        let args = "std430" :: args |> String.concat ","

                                        return Some (sprintf "layout(%s) buffer " args + (p.cParamSemantic + "_ssb"))

                                    | ParameterDecoration.Shared -> 
                                        return Some "shared"

                                    | ParameterDecoration.Memory _ | ParameterDecoration.Slot _ ->
                                        return None
                            }

                        )


                    let isBuffer = p.cParamDecorations |> Seq.exists (function ParameterDecoration.StorageBuffer -> true | _ -> false)

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

                    let prefix, suffix =
                        if config.version > version120 then
                            match kind with
                                | ParameterKind.Input -> "in ", ""
                                | ParameterKind.Output -> "out ", ""
                                | _ -> 
                                    if isBuffer then " { ", " };"
                                    else "", ""
                        else
                            match kind with
                                | ParameterKind.Input -> "varying ", ""
                                | ParameterKind.Output -> "varying ", ""
                                | _ -> "", ""
                    

                    match p.cParamType with
                        | CArray(t,l) ->
                            return sprintf "%s%s%s %s[%d];%s" decorations prefix (assembleType t).Name name.Name l suffix |> Some
                        | CPointer(_, t) ->
                            return sprintf "%s%s%s %s[];%s" decorations prefix (assembleType t).Name name.Name suffix |> Some
                        | _ -> 
                            return sprintf "%s%s%s %s;%s" decorations prefix (assembleType p.cParamType).Name name.Name suffix |> Some
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


                    | ShaderStage.Compute ->
                        let localSize = e.cDecorations |> List.tryPick (function EntryDecoration.LocalSize s when s.AllGreater 0 -> Some s | _ -> None) 
                        [
                            match localSize with
                                | Some s -> yield sprintf "layout (local_size_x = %d, local_size_y = %d, local_size_z = %d) in;" s.X s.Y s.Z
                                | None -> yield "layout( local_size_variable ) in;" 
                        ]

                    | _ ->
                        []

            let! inputs = e.cInputs |> List.chooseS (assembleEntryParameterS ParameterKind.Input)
            let! outputs = e.cOutputs |> List.chooseS (assembleEntryParameterS ParameterKind.Output)
            let! args = e.cArguments |> List.chooseS (assembleEntryParameterS ParameterKind.Argument)
            let! body = assembleStatementS false e.cBody

            let entryName = checkName e.cEntryName

            return 
                String.concat "\r\n" [
                    yield! prefix
                    yield! inputs
                    yield! outputs
                    yield! args
                    yield sprintf "%s %s()\r\n{\r\n%s\r\n}" (assembleType e.cReturnType).Name entryName.Name (String.indent body)
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
                    let n = glslName n
                    match t with
                        | CArray(t,l) -> return sprintf "const %s %s[%d] = %s;" (assembleType t).Name n.Name l init
                        | CPointer(_,t) -> return sprintf "const %s %s[] = %s;" (assembleType t).Name n.Name init
                        | _ -> return sprintf "const %s %s = %s;" (assembleType t).Name n.Name init

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
                let fields = fields |> List.map (fun (t, n) -> sprintf "%s %s;" (assembleType t).Name (glslName n).Name) |> String.concat "\r\n"
                sprintf "struct %s\r\n{\r\n%s\r\n};" (glslName name).Name (String.indent fields)

    let assemble (backend : Backend) (m : CModule) =

        let c = backend.Config
        let definitions =
            state {
                let types = m.types |> List.map assembleTypeDef

                let! uniforms = 
                    if not c.createPerStageUniforms then assembleUniformsS m.uniforms |> State.map List.singleton
                    else State.value []

                let! values = m.values |> List.mapS assembleValueDefS

                let! s = State.get
                let extensions = Set.union c.enabledExtensions s.requiredExtensions

                return 
                    List.concat [
                        [sprintf "#version %d%d0" c.version.Major c.version.Minor ]
                        [ extensions |> Seq.map (sprintf "#extension %s : enable") |> Seq.toList |> String.concat "\r\n" ]
                        types
                        uniforms
                        values
                    ]
            }
        let mutable state = AssemblerState.ofConfig c
        let code = definitions.Run(&state) |> String.concat "\r\n\r\n"
        {
            code        = code
            builtIns    = state.builtIn
        }
