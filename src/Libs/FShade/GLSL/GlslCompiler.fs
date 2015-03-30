namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Aardvark.Base.TypeInfo.Patterns
open Aardvark.Base
open FShade.Utils
open FShade.Compiler


module GLSL =

    // The GLSL-Compiler does not need any additional state so we simply use
    // ShaderState here.
    type CompilerState = ShaderState

    let private version410 = Version(4,1)
    let private version120 = Version(1,2)

    type Compiler(?glslVersion : Version) =
        let glslVersion = defaultArg glslVersion (Version(4,1))
        let funDefCache = MemoCache(false)

        let rec compileZero (t : Type) =
            compile {
                match t with
                    | VectorOf(dim,baseType) ->
                        let! baseZero = compileZero baseType
                        let args = List.replicate dim baseZero |> String.concat ","
                        let! t = compileType t
                        return sprintf "%s(%s)" t args

                    | MatrixOf(dim, baseType) ->
                        let! baseZero = compileZero baseType
                        let args = List.replicate (dim.X * dim.Y) baseZero |> String.concat ","

                        let! t = compileType t
                        return sprintf "%s(%s)" t args

                    | Float32|Float64 ->
                        return "0.0"

                    | Int32 ->
                        return "0"
                    | UInt32 ->
                        return "0u"
                    | Bool ->
                        return "false"
                    | _ ->
                        return! error "cannot create zero for type: %A" t
            }

        member x.Version = glslVersion

        interface ICompiler<CompilerState> with

            member x.CompileIntrinsicType(t : Type) = 
                compile {
                    if t.IsArray then
                        let! t = compileIntrinsicType (t.GetElementType())
                        match t with
                            | Some t -> return sprintf "%s[]" t |> Some
                            | None -> return None
                    else
                        match t with
                            | Bool -> return Some "bool"
                            | Int32 -> return Some "int"
                            | Float32 -> return Some "float"
                            | Float64 -> return Some "float"
                            | Unit -> return Some "void"

                            | VectorOf(d, (Float32|Float64)) -> 
                                return sprintf "vec%d" d |> Some

                            | VectorOf(d, Int32) -> 
                                return sprintf "ivec%d" d |> Some

                            | MatrixOf(d,(Float32|Float64)) -> 
                                if d.X = d.Y then return sprintf "mat%d" d.X |> Some
                                else return sprintf "mat%dx%d" d.Y d.X |> Some

                            | MatrixOf(d,Int32) -> 
                                if d.X = d.Y then return sprintf "imat%d" d.X |> Some
                                else return sprintf "imat%dx%d" d.Y d.X |> Some

                            | Ref(t) -> return! compileIntrinsicType t

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
                                        | "V4d" -> ""
                                        | "V4i" -> "i"
                                        | _ -> failwith "unsupported sampler value-type"

                                if arr then return sprintf "%ssampler%s%sArray%s" typePrefix dimStr shadowSuffix msSuffix |> Some
                                else return sprintf "%ssampler%s%s%s" typePrefix dimStr shadowSuffix msSuffix |> Some

                            | _ -> return None
                }

            member x.CompileIntrinsicFunction(mi : MethodInfo) = 
                compile {
                    match mi with
                        | Method("op_Addition", [Num|Vector|Matrix; Num|Vector|Matrix]) -> return Some "({0} + {1})"
                        | Method("op_Subtraction", [Num|Vector|Matrix; Num|Vector|Matrix]) -> return Some "({0} - {1})"
                        | Method("op_Multiply", [Num|Vector|Matrix; Num|Vector|Matrix]) -> return Some "{0} * {1}"
                        | Method("op_Division", [Num|Vector|Matrix; Num|Vector|Matrix]) -> return Some "{0} / {1}"
                        | Method("op_Modulus", [Num; Num]) -> return Some "{0} % {1}"
                        | Method("op_LessThan", [Num; Num]) -> return Some "{0} < {1}"
                        | Method("op_GreaterThan", [Num; Num]) -> return Some "{0} > {1}"
                        | Method("op_GreaterThanOrEqual", [Num; Num]) -> return Some "{0} >= {1}"
                        | Method("op_LessThanOrEqual", [Num; Num]) -> return Some "{0} <= {1}"
                        | Method("op_Equality", [Num|Vector|Matrix; Num|Vector|Matrix]) -> return Some "{0} == {1}"
                        | Method("op_Dereference", [Ref(_)]) -> return Some "{0}"
                        | Method("op_ColonEquals", [Ref(_);_]) -> return Some "{0} = {1};\r\n"
                        | Method("op_UnaryNegation", [_]) -> return Some "-{0}"

                        | Method("get_Length", [Vector]) -> return Some "length({0})"
                        | Method("get_LengthSquared", [Vector]) -> return Some "dot({0}, {0})"
                        | Method("get_Normalized", [Vector]) -> return Some "normalize({0})"
                        | Method("Dot", [Vector; Vector]) -> return Some "dot({0}, {1})"
                        | Method("Cross", [Vector; Vector]) -> return Some "cross({0}, {1})"

                        | Method("Abs", [Num]) -> return Some "abs({0})"
                        | Method("Min", [Num; Num]) -> return Some "min({0}, {1})"
                        | Method("Max", [Num; Num]) -> return Some "max({0}, {1})"
                        | Method("Pow", [Num; Num]) -> return Some "pow({0}, {1})"
                        | Method("Exp", [Num]) -> return Some "exp({0})"


                        | MethodQuote <@ abs @> _ -> return Some "abs({0})"
                        | MethodQuote <@ acos @> _ -> return Some "acos({0})"
                        | MethodQuote <@ asin @> _ -> return Some "asin({0})"
                        | MethodQuote <@ atan @> _ -> return Some "atan({0})"
                        | MethodQuote <@ atan2 @> _ -> return Some "atan({0}, {1})"
                        | MethodQuote <@ ceil @> _ -> return Some "ceil({0})"
                        | MethodQuote <@ cos @> _ -> return Some "cos({0})"
                        | MethodQuote <@ cosh @> _ -> return Some "cosh({0})"
                        | MethodQuote <@ exp @> _ -> return Some "exp({0})"
                        | MethodQuote <@ floor @> _ -> return Some "floor({0})"
                        | MethodQuote <@ log @> _ -> return Some "log({0})"
                        | MethodQuote <@ not @> _ -> return Some "(!{0})"
                        | MethodQuote <@ round @> _ -> return Some "round({0})"
                        | MethodQuote <@ sign @> _ -> return Some "sign({0})"
                        | MethodQuote <@ sin @> _ -> return Some "sin({0})"
                        | MethodQuote <@ sinh @> _ -> return Some "sinh({0})"
                        | MethodQuote <@ sqrt @> _ -> return Some "sqrt({0})"
                        | MethodQuote <@ tan @> _ -> return Some "tan({0})"
                        | MethodQuote <@ tanh @> _ -> return Some "tanh({0})"
                        | MethodQuote <@ min @> _ -> return Some "min({0}, {1})"
                        | MethodQuote <@ max @> _ -> return Some "max({0}, {1})"
                        | MethodQuote <@ pow : int -> int -> int @> _ -> return Some "pow({0}, {1})"
                        | MethodQuote <@ clamp : int -> int -> int -> int @> _ -> return Some "clamp({2}, {0}, {1})"

                        | MethodQuote <@ GenericValues.zero : int @> [t] -> 
                            let! zero = compileZero t
                            return Some zero


                        | MethodQuote <@ Vec.dot : V3d -> V3d -> float @> _ -> return Some "dot({0},{1})"
                        | MethodQuote <@ Vec.cross : V3d -> V3d -> V3d @> _ -> return Some "cross({0},{1})"
                        | MethodQuote <@ Vec.length : V3d -> float @> _ -> return Some "length({0})"
                        | MethodQuote <@ Vec.lengthSquared : V3d -> float @> _ -> return Some "dot({0}, {0})"
                        | MethodQuote <@ Vec.normalize : V3d -> V3d @> _ -> return Some "normalize({0})"
                        | MethodQuote <@ Vec.lerp : V3d -> V3d -> float -> V3d @> _ -> return Some "mix({0}, {1}, {2})"
                        | MethodQuote <@ Vec.reflect : V3d -> V3d -> V3d @> _ -> return Some "reflect({0}, {1})"
                        | MethodQuote <@ Vec.refract : V3d -> V3d -> float -> V3d @> _ -> return Some "refract({0}, {1}, {2})"

                        | MethodQuote <@ Vec.xy : V3d -> V2d @> _ -> return Some "({0}).xy"
                        | MethodQuote <@ Vec.yz : V3d -> V2d @> _ -> return Some "({0}).yz"
                        | MethodQuote <@ Vec.zw : V4d -> V2d @> _ -> return Some "({0}).zw"
                        | MethodQuote <@ Vec.xyz : V4d -> V3d @> _ -> return Some "({0}).xyz"
                        | MethodQuote <@ Vec.yzw : V4d -> V3d @> _ -> return Some "({0}).yzw"

                        | MethodQuote <@ Vec.anySmaller : V3d -> V3d -> bool @> _ -> return Some "any(lessThan({0},{1}))"
                        | MethodQuote <@ Vec.anyGreater : V3d -> V3d -> bool @> _ -> return Some "any(greaterThan({0},{1}))"
                        | MethodQuote <@ Vec.allSmaller : V3d -> V3d -> bool @> _ -> return Some "all(lessThan({0},{1}))"
                        | MethodQuote <@ Vec.allGreater : V3d -> V3d -> bool @> _ -> return Some "all(greaterThan({0},{1}))"
                        | MethodQuote <@ Vec.anySmallerOrEqual : V3d -> V3d -> bool @> _ -> return Some "any(lessThanEqual({0},{1}))"
                        | MethodQuote <@ Vec.anyGreaterOrEqual : V3d -> V3d -> bool @> _ -> return Some "any(greaterThanEqual({0},{1}))"
                        | MethodQuote <@ Vec.allSmallerOrEqual : V3d -> V3d -> bool @> _ -> return Some "all(lessThanEqual({0},{1}))"
                        | MethodQuote <@ Vec.allGreaterOrEqual : V3d -> V3d -> bool @> _ -> return Some "all(greaterThanEqual({0},{1}))"


                        | MethodQuote <@ Mat.transpose : M33d -> M33d @> _ -> return Some "transpose({0})"
                        | MethodQuote <@ Mat.inverse : M33d -> M33d @> _ -> return Some "inverse({0})"
                        | MethodQuote <@ Mat.det : M33d -> float @> _ -> return Some "determinant({0})"
                        | MethodQuote <@ Mat.transformPos : M44d -> V3d -> V3d @> _ -> return Some "({0} * vec4({1}, 1.0)).xyz"
                        | MethodQuote <@ Mat.transformDir : M44d -> V3d -> V3d @> _ -> return Some "({0} * vec4({1}, 0.0)).xyz"

                        | MethodQuote <@ ref @> [_] -> return Some "{0}"
                        | MethodQuote <@ LanguagePrimitives.IntrinsicFunctions.GetArray @> [_] -> return Some "{0}[{1}]"
                        | MethodQuote <@ LanguagePrimitives.IntrinsicFunctions.SetArray @> [_] -> return Some "{0}[{1}] = {2}"
                        | MethodQuote <@ emitVertex @> [] -> return Some "EmitVertex();\r\n"
                        | MethodQuote <@ discard @> [] -> return Some "discard;\r\n"
                        | MethodQuote <@ ddx<int> @> _ -> return Some "dFdx({0})"
                        | MethodQuote <@ ddy<int> @> _ -> return Some "dFdy({0})"


                        //SamplerType(dim, isArray, isShadow, isMS, valueType) 
                        | Method("Sample", [SamplerType(dim, isArray, isShadow, isMS, valueType); _]) -> 
                            if glslVersion > version120 then
                                return Some "texture({0}, {1})"
                            else
                                let samplerFun =
                                    match dim with
                                        | SamplerDimension.Sampler1d -> "texture1D"
                                        | SamplerDimension.Sampler2d -> "texture2D"
                                        | SamplerDimension.Sampler3d -> "texture3D"
                                        | SamplerDimension.SamplerCube-> "textureCube"
                                        | _ -> failwith "unsupported sampler type"

                                return Some (sprintf "%s({0}, {1})" samplerFun)

                        | Method("Sample", [SamplerType(_,true,_,_,_); _; _]) -> return Some "texture({0}, vec3({1}, {2}))"
                        | Method("SampleLevel", [SamplerType(_); _; _]) -> return Some "textureLod({0}, {1}, {2})"
                        | Method("SampleLevel", [SamplerType(_,true,_,_,_); _; _; _]) -> return Some "textureLod({0}, vec3({1}, {2}), {3})"


                        | Method("get_Length", [FixedArrayType(s,t)]) -> return Some (sprintf "%d" s)
                        | Method("get_Item", [FixedArrayType(s,t);v]) -> return Some "{0}[{1}]"
                        | Method("set_Item", [FixedArrayType(s,t);i;v]) -> return Some "{0}[{1}] = {2}"

                        | MethodQuote <@ float : int -> float @> _ -> return Some "float({0})"
                        | MethodQuote <@ int : float -> int @> _ -> return Some "int({0})"

                        | _ -> return None
                }

            member x.CompileIntrinsicFunctionDefinition(mi : MethodInfo) =
                let e = funDefCache.Memoized1 (fun mi ->
                            match mi with
                                | (MethodQuote <@ M44d().TransformPosProj @> _) | (MethodQuote <@ Mat.transformPosProj : M44d -> V3d -> V3d @> _) ->
                                    Some <@@ fun (m : M44d) (v : V3d) -> 
                                             let pp = m * new V4d(v, 1.0)
                                             pp.XYZ / pp.W 
                                          @@>

                                | MethodQuote <@ log10 @> [t] ->
                                    let v = Var("v", t)
                                    let mi = getMethodInfo <@ log @>
                                    Some <| Expr.Lambda(v, <@@ %%Expr.Call(mi, [Expr.Var v]) / %%Expr.Call(mi, [Expr.Value 10.0]) @@>)


                                | _ -> None
                        ) mi

                compile {return e}

            member x.CompileIntrinsicPropertyGet(p : MemberInfo) = 
                compile {
                    match p with
                        | VectorSwizzle(name) -> return "({0})." + name.ToLower() |> Some
                        | MatrixElement(x,y) -> return sprintf "({0})._%d%d" (x+1) (y+1) |> Some
                        | _ -> return None   
                }

            member x.CompileIntrinsicPropertySet(p : MemberInfo) = 
                compile {
                    match p with
                        | VectorSwizzle(name) -> return "({0})." + name.ToLower() + " = {1}" |> Some
                        | MatrixElement(x,y) -> return sprintf "({0})._%d%d = {1}" (x+1) (y+1) |> Some
                        | _ -> return None   
                }

            member x.CompileIntrinsicConstructor(c : ConstructorInfo) = 
                compile {
                    match c with
                        | Create(Vector as t, args) -> 
                            let! t = compileIntrinsicType t
                            match t with
                                | Some(t) -> let fmt = sprintf "%s(%s)" t (args |> Seq.mapi (fun i _ -> sprintf "{%d}" i) |> String.concat ", ")
                                             return Some fmt
                                | _ -> return None

                        | _ -> return None
                }

            member x.CompileFunctionDeclaration (retType : string) (name : string) (arguments : list<string * string * Option<string> * ParameterPassingStyle>) (body : string) = 
                compile {
                    let args = String.concat ", " (arguments |> Seq.map (fun (t,n,_,s) -> let prefix = if s = ReferenceArgument then "inout " else ""
                                                                                          sprintf "%s%s %s" prefix t n))
                    return sprintf "%s %s(%s)\r\n{\r\n%s}\r\n" retType name args (String.indent 1 body)
                }

            member x.CompileTypeDeclaration(name : string) (fields : list<string>) = 
                compile {
                    let fields = fields |> List.map (fun c -> c + ";")
                    return sprintf "struct %s\r\n{\r\n%s\r\n};\r\n" name (fields |> String.concat "\r\n" |> String.indent 1)
                }

            member x.CompileVariableDeclaration(t : string) (name : string) (arraySize : Option<string>) =
                compile {
                    match arraySize with
                        | None -> return sprintf "%s %s" t name
                        | Some(s) -> return sprintf "%s %s[%s]" t name s
                }

            member x.CompileConstantDeclaration (t : Type) (name : string) (value : obj) =
                compile {
                    match value with
                        | FixedArray(contentType, elements) ->
                            let! content = elements |> Seq.mapC (fun e -> compileValue contentType e)
                            let! t = compileType contentType

                            return sprintf "const %s %s[%d] = %s[](%s);" t name elements.Length t (String.concat ", " content)
                        | v when t = typeof<SamplerState> ->
                            let v = v |> unbox<SamplerState>
                            return GlslSamplers.compileSamplerState name v

                        | v when t = typeof<SamplerComparisonState> ->
                            let v = v |> unbox<SamplerComparisonState>
                            return GlslSamplers.compileSamplerComparisonState name v


                        | _ -> return! error "unsupported constant type: %A" t 
                }


            member x.CompileFieldDeclaration(f : Field) =
                compile {
                    match f.arraySize with
                        | None -> return sprintf "%s %s" f.fieldType f.name
                        | Some(s) -> return sprintf "%s %s[%s]" f.fieldType f.name s
                }

            member x.CompilePreamble() =
                compile { return "" }

            member x.CompileValue (t : Type) (o : obj) =
                compile {
                    match t with
                        | VectorOf(d, b) -> 
                            let fields = t.GetFields(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                            let! fieldValues = fields |> Seq.mapC (fun fi -> compileValue fi.FieldType (fi.GetValue(o)))
                            return sprintf "vec%d(%s)" d (String.concat ", " fieldValues) |> Some
                        | Float32|Float64 ->
                            let d = Convert.ToDouble(o)
                            return d.ToString("0.000E00", System.Globalization.CultureInfo.InvariantCulture) |> Some
                        | Num ->
                            return o.ToString() |> Some
                        | Bool ->
                            let b = o |> unbox<bool>
                            return Some(if b then "true" else "false")
                        | _ -> return None 
                }

            member x.InitialState() = emptyShaderState

            member x.ProcessCode c =
                compile {
                    return c
                }    

            member x.FilterFunctionArguments (args : list<Var>) =
                compile {
                    return args
//                    return args |> List.choose (fun a ->
//                                match a.Type with
//                                    | TextureType(_) -> None
//                                    | _ -> Some a
//                                )
                }

            member x.ProcessFunctionBody (body : Expr) =
                compile {
                    let! r = substituteUniforms body
                    return r
                }

            member x.CompileApplication (f : obj) (retType : Type) (args : list<Expr>) =
                compile { return None }

    let private version : Compiled<_, ShaderState> =
        { runCompile = fun s ->
            match s.compiler with
                | :? Compiler as c ->
                    Success(s,c.Version)
                | _ ->
                    Error "cannot determine version for non GLSL compiler"
        }


//    let (|VertexOpt|GeometryOpt|FragmentOpt|NoneOpt|) (t : Option<ShaderType>) =
//        match t with
//            | Some Vertex -> VertexOpt
//            | Some (Geometry _) -> GeometryOpt
//            | Some Fragment -> FragmentOpt
//            | _ -> NoneOpt

    let private getIntrinsicInputName (v : Version) (t : ShaderType) (last : Option<ShaderType>) (s : string) =
        match t with
            | Vertex ->
                None
            | Geometry(_) ->
                match s with
                    | "Positions" -> Some "gl_Position"
                    | _ -> None

            | Fragment ->
                None

            | TessControl ->
                match s with
                    | "Positions" -> Some "gl_Position"
                    | "InvocationId" -> Some "gl_InvocationID"
                    | _ -> None
            | TessEval ->
                match s with
                    | "Positions" -> Some "gl_Position"
                    | "TessCoord" -> Some "gl_TessCoord"
                    | _ -> None

    let private getIntrinsicOutputName (v : Version) (t : ShaderType) (next : Option<ShaderType>) (s : string) =
        match t with
            | Vertex|Geometry(_)|TessEval ->
                match s with
                    | "Positions" -> Some "gl_Position"
                    | _ -> None
            | Fragment ->
                match s with
                    | "Depth" -> Some "gl_FragDepth"
                    | "Colors" when v <= version120 -> Some "gl_FragColor"
                    | _ -> None
            | TessControl ->
                match s with
                    | "Positions" -> Some "gl_Position"
                    | "TessLevelInner" -> Some "gl_TessLevelInner"
                    | "TessLevelOuter" -> Some "gl_TessLevelOuter"
                    | _ -> None

    //compilation functions
    let private changeIONames (s : Shader) (last : Option<ShaderType>) (next : Option<ShaderType>) =
        let newInputs = 
            match last with
                | Some stage ->
                    s.inputs |> Map.toList |> List.map(fun (k,v) -> k,v.Name,Var(sprintf "%s%A" k stage, v.Type, v.IsMutable))
                | None -> s.inputs |> Map.toList |> List.map (fun (k,v) -> k,v.Name,v)

        let inputRepl = newInputs |> List.map (fun (_,k,v) -> (k,Expr.Var v)) |> Map.ofList
        let newInputs = newInputs |> List.map (fun (k,_,v) -> (k,v)) |> Map.ofList

        let newOutputs = 
            s.outputs |> Map.toList |> List.map (fun (k,(o,v)) -> (k,o,v,Var(sprintf "%s%A" k s.shaderType, v.Type, v.IsMutable)))

        let outputRepl = newOutputs |> List.map (fun (_,_,o,n) -> o.Name, Expr.Var n) |> Map.ofList
        let newOutputs = newOutputs |> List.map (fun (k,o,_,v) -> k,(o,v)) |> Map.ofList

        let body = s.body.Substitute (fun v -> Map.tryFind v.Name inputRepl)
        let body = body.Substitute (fun v -> Map.tryFind v.Name outputRepl)


        { s with body = body; inputs = newInputs; outputs = newOutputs}



    let private liftIntrinsics (glslVersion : Version) (s : Shader)  (last : Option<ShaderType>) (next : Option<ShaderType>)=
        
        let s =
            if glslVersion <= version120 then changeIONames s last next
            else s
        
        let mutable body = s.body
        let mutable inputs = Map.empty
        let mutable outputs = Map.empty

        for KeyValue(sem, v) in s.inputs do
            match getIntrinsicInputName glslVersion s.shaderType last sem with
                | Some name -> 
                    let replacement = Var(name, v.Type)
                    body <- body.Substitute(fun vi -> 
                                if vi = v then 
                                    Some (Expr.Var(replacement))
                                else
                                    None
                            )

                | _ -> 
                    if sem = "Positions" && s.shaderType = ShaderType.Fragment then
                        let sem = "_Positions_"
                        let replacement = Var(sem, v.Type)
                        body <- body.Substitute(fun vi -> 
                                    if vi = v then 
                                        Some (Expr.Var(replacement))
                                    else
                                        None
                                )
                        inputs <- Map.add sem replacement inputs
                    else
                        inputs <- Map.add sem v inputs

        for KeyValue(sem, (t,v)) in s.outputs do
            match getIntrinsicOutputName glslVersion s.shaderType next sem with
                | Some name -> 
                    let replacement = Var(name, v.Type)
                    body <- body.Substitute(fun vi -> 
                                if vi = v then 
                                    Some (Expr.Var(replacement))
                                else
                                    None
                            )

                | _ -> 
                    outputs <- Map.add sem (t,v) outputs
        

        { shaderType = s.shaderType; inputs = inputs; outputs = outputs; body = body; uniforms = s.uniforms; inputTopology = s.inputTopology; debugInfo = s.debugInfo }


    let private compileVariableDeclaration (t : Type) (name : string) =
        compile {
            match t with
                | FixedArrayType(d,e) ->
                    let! t = compileType e
                    let d = d.ToString() |> Some
                    return! compileVariableDeclaration t name d
                | _ ->
                    let! t = compileType t
                    return! compileVariableDeclaration t name None
        }

    let private compileShader (entryName : string) (s : Shader) =
        compile {
            do! resetCompilerState

            

            let! (header, c) = compileMainWithoutTypes [] s.body entryName
            let! state = compilerState : Compiled<ShaderState, ShaderState>
            let! glslVersion = version



            let uniforms' = seq { yield! s.uniforms ; yield! state.uniforms |> Seq.map(fun (KeyValue(u,v)) -> (u.Value, v)) } |> Seq.toList

            let inputs = s.inputs |> Seq.sortBy(fun (KeyValue(_,n)) -> n.Name)
            let outputs = s.outputs |> Seq.sortBy(fun (KeyValue(_,(_,n))) -> n.Name)
            let uniforms = uniforms'  |> Seq.map(fun (u,v) ->
                                            match u with
                                                | UserUniform(t,o) -> (uniform, t, v.Name)
                                                | Attribute(scope, t, n) -> (scope, t, n)
                                                | SamplerUniform(t,sem, n,_) -> (uniform, t, n)
                                        ) 
                                      |> Seq.groupBy(fun (s,_,_) -> s)
                                      |> Seq.map (fun (g,v) -> (g, v |> Seq.map (fun (_,t,n) -> (t,n)) |> Seq.toList))
                                      |> Map.ofSeq


            let! inputs = inputs |> Seq.mapCi (fun i (KeyValue(_,n)) -> 
                compile {
                    
                    let modifier =
                        if glslVersion > version120 then
                            sprintf "layout(location = %d) in" i
                        else
                            if s.shaderType = ShaderType.Vertex then 
                                "attribute" 
                            else 
                                "varying"

                    if n.Type.IsArray then
                        let t = n.Type.GetElementType()
                        let! r = compileVariableDeclaration t n.Name
                        return sprintf "%s %s[];" modifier r

                    else
                        let! r = compileVariableDeclaration n.Type n.Name
                        return sprintf "%s %s;" modifier r

                })

            let! outputs = outputs |> Seq.mapCi (fun i (KeyValue(_,(_,n))) -> 
                compile {
                    let modifier =
                        if glslVersion > version120 then
                            sprintf "layout(location = %d) out" i
                        else
                            "varying"

                    if s.shaderType = ShaderType.TessControl then
                        let t = n.Type
                        let! r = compileVariableDeclaration t n.Name
                        return sprintf "%s %s[];" modifier r
                    else
                        let! r = compileVariableDeclaration n.Type n.Name
                        return sprintf "%s %s;" modifier r
                    })


            
            let inputs = String.concat "\r\n" inputs
            let outputs = String.concat "\r\n" outputs


            let uniformGetters = uniforms' |> Seq.choose(fun (u,v) ->
                                                    match u with
                                                        | UserUniform(t,o) -> Some (v.Name, UniformGetter(o, t))
                                                        | SamplerUniform(t,sem, n,sam) -> Some (n, UniformGetter((sem, sam), t))
                                                        | _ -> None
                                                )
                                           |> Map.ofSeq

            let layout = match s.shaderType with
                            | Geometry top -> 
                                let top = match top with
                                            | TriangleStrip -> "triangle_strip"
                                            | LineStrip -> "line_strip"
                                            | Points -> "points"

                                let itop = match s.inputTopology with
                                            | Some(InputTopology.Point) -> "points"
                                            | Some(InputTopology.Line) -> "lines"
                                            | Some(InputTopology.LineAdjacency) -> "lines_adjacency"
                                            | Some(InputTopology.Triangle) -> "triangles"
                                            | Some(InputTopology.TriangleAdjacency) -> "triangles_adjacency"
                                            | _ -> failwith "geometryshader does not have proper inputTopology"

                                //TODO: find max_vertices
                                sprintf "layout(%s, max_vertices = 12) out;\r\nlayout(%s) in;\r\n" top itop
                            | TessControl ->
                                let top = 
                                    match s.inputTopology with
                                        | Some(InputTopology.Patch n) ->
                                            sprintf "layout(vertices = %d) out;" n
                                        | _ -> 
                                            failwith "TessControl-Shaders must take patches as input"

                                sprintf "%s\r\n" top

                            | TessEval ->
                                let top =
                                    match s.inputTopology with
                                        | Some(Patch n) ->
                                            match n with
                                                | 3 -> "triangles"
                                                | 4 -> "quads"
                                                | _ -> failwith "unknown input-topology for TessEval-Shader"

                                        | _ -> failwith "unknown input-topology for TessEval-Shader"
                                sprintf "layout(%s, equal_spacing, ccw) in;\r\n" top
                            | _ -> ""

            let! types = usedTypes



            let! (def,disp) = compileLambdas()
//            let disp = sprintf "%s\r\n%s" def disp


            let completeCode = sprintf "%s\r\n%s\r\n%s\r\n%s\r\n%s\r\n%s%s" def header disp inputs outputs layout c

            return { usedTypes = types; uniforms = uniformGetters; uniformBuffers = uniforms; code = completeCode }
        }

    let private compileEffectInternal (e : Compiled<Effect, ShaderState>) =
        compile {
            let! e = e
            let! glslVersion = version

            let hasgs = match e.geometryShader with | Some _ -> true | _ -> false
            let topUsed = ["Colors", typeof<V4d>; "Depth", typeof<float>] |> Map.ofList
            let! fsUsed,fsCode = match e.fragmentShader with
                                    | Some(fs) -> compile {
                                                    let unused = fs.outputs |> Map.filter (fun k (t,v) -> 
                                                                    match t with
                                                                        | Some t -> false
                                                                        | None -> not <| Map.containsKey k topUsed
                                                                 ) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq
                                                    let fs = removeOutputs unused fs

                                                    let fs = liftIntrinsics glslVersion fs ((if hasgs then Geometry TriangleStrip else Vertex) |> Some) None

                                                    let! fsc = compileShader "PS" fs

                                                    let used = seq { yield ("Positions",typeof<V4d>); yield! fs.inputs |> Seq.map (fun (KeyValue(k,v)) -> (k,v.Type)) } |> Map.ofSeq
                                                    return used, Some fsc
                                                  }
                                    | None -> compile { return Map.ofList [("Positions",typeof<V4d>)], None }

            do! resetCompilerState
            let! gsUsed,gsCode = match e.geometryShader with
                                    | Some(gs, t) -> compile {
                                                    let additional = fsUsed |> Map.filter (fun k _ -> not <| Map.containsKey k gs.outputs)
                                                    let gs = addOutputs additional gs

                                                    let unused = gs.outputs |> Map.filter (fun k v -> not <| Map.containsKey k fsUsed) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq
                                                    let gs = removeOutputs unused gs

                                                    let gs = liftIntrinsics glslVersion gs (Some Vertex) (Some Fragment)
                                                    let! gsc = compileShader "GS" gs

                                                    let used = gs.inputs |> Seq.map (fun (KeyValue(k,v)) -> (k, if v.Type.IsArray then v.Type.GetElementType() else v.Type)) |> Map.ofSeq
                                                
                                                    let glPos = System.Text.RegularExpressions.Regex("gl_Position\[(?<index>[^\]]+)\]")
                                                    let gsc = { code = glPos.Replace(gsc.code, fun (m : System.Text.RegularExpressions.Match) -> 
                                                                    let index = m.Groups.["index"].Value
                                                                    sprintf "gl_in[%s].gl_Position" index
                                                                )
                                                                usedTypes = gsc.usedTypes
                                                                uniformBuffers = gsc.uniformBuffers
                                                                uniforms = gsc.uniforms }


                                                    return used,Some (gsc,t)
                                                  }
                                    | None -> compile { return fsUsed,None }

            do! resetCompilerState


            
            let uniformBufferUnion (a : Map<UniformScope, list<Type * string>>) (b : Map<UniformScope, list<Type * string>>) =
                let mutable result = a

                for (KeyValue(s,list)) in b do
                    match Map.tryFind s result with
                        | Some l ->
                            let o = System.Collections.Generic.HashSet(List.concat [l |> Seq.toList; list]) |> Seq.toList
                            result <- Map.add s o result
                        | None -> 
                            result <- Map.add s list result
                result


            let! tessUsed, tessCode = match e.tessControlShader, e.tessEvalShader with
                                        | None, None -> compile { return gsUsed, None }
                                        | Some tcs, Some tev ->
                                            compile {
                                                let additional = gsUsed |> Map.filter (fun k _ -> not <| Map.containsKey k tev.outputs)
                                                let unused = tev.outputs |> Map.filter (fun k v -> not <| Map.containsKey k gsUsed) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq

                                                let tev = addOutputs additional tev
                                                let tev = removeOutputs unused tev

                                                let tevUsed = tev.inputs |> Seq.map (fun (KeyValue(k,v)) -> (k, if v.Type.IsArray then v.Type.GetElementType() else v.Type)) |> Map.ofSeq
                                                let tevUsed = Map.remove "TessCoord" tevUsed
                                                let tevUsed = Map.add "TessLevelInner" typeof<float[]> tevUsed
                                                let tevUsed = Map.add "TessLevelOuter" typeof<float[]> tevUsed

                                                let additional = tevUsed |> Map.filter (fun k _ -> not <| Map.containsKey k tcs.outputs)
                                                let unused = tcs.outputs |> Map.filter (fun k v -> not <| Map.containsKey k tevUsed) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq

                                                let tcs = addOutputs additional tcs
                                                let tcs = removeOutputs unused tcs

                                                let tcsUsed = tcs.inputs |> Seq.map (fun (KeyValue(k,v)) -> (k, if v.Type.IsArray then v.Type.GetElementType() else v.Type)) |> Map.ofSeq
                                                


                                                let id = Var("gl_InvocationID", typeof<int>)
                                                let rec substitute e =
                                                    match e with
                                                        | VarSet(v,value) ->
                                                            match Map.tryFindKey (fun _ (_,vi) -> vi = v) tcs.outputs with
                                                                | Some s when s <> "TessLevelInner" && s <> "TessLevelOuter" ->
                                                                    let set = getMethodInfo <@ LanguagePrimitives.IntrinsicFunctions.SetArray @>
                                                                    let set = set.MakeGenericMethod [|value.Type|]

                                                                    let newVar =
                                                                        if s = "Positions" then 
                                                                            Var("gl_OutPosition", v.Type.MakeArrayType())
                                                                        else
                                                                            Var(v.Name, v.Type.MakeArrayType())

                                                                    Expr.Call(set, [Expr.Var newVar; Expr.Var id; value])
       
                                                                | _ -> e

                                                        | ShapeVar(v) -> e
                                                        | ShapeCombination(o,args) -> RebuildShapeCombination(o, args |> List.map substitute)
                                                        | ShapeLambda(v,b) -> Expr.Lambda(v, substitute b)

                                                let newTcs = substitute tcs.body
                                                let tcs = { tcs with body = newTcs }

                                                let tev = liftIntrinsics glslVersion tev (Some Vertex) (Some Fragment)
                                                let tcs = liftIntrinsics glslVersion tcs (Some Vertex) (Some Fragment)


                                                let! tevc = compileShader "TEV" tev
                                                let! tcsc = compileShader "TCS" tcs

                                                
                                                let glPos = System.Text.RegularExpressions.Regex("gl_Position\[(?<index>[^\]]+)\]")
                                                let glOutPos = System.Text.RegularExpressions.Regex("gl_OutPosition\[(?<index>[^\]]+)\]")

                                                let tevCode = glPos.Replace(tevc.code, (fun (m : System.Text.RegularExpressions.Match) -> 
                                                    let id = m.Groups.["index"].Value
                                                    sprintf "gl_in[%s].gl_Position" id
                                                ))

                                                let tcsCode = glPos.Replace(tcsc.code, (fun (m : System.Text.RegularExpressions.Match) -> 
                                                    let id = m.Groups.["index"].Value
                                                    sprintf "gl_in[%s].gl_Position" id
                                                ))

                                                let tcsCode = glOutPos.Replace(tcsCode, (fun (m : System.Text.RegularExpressions.Match) -> 
                                                    let id = m.Groups.["index"].Value
                                                    sprintf "gl_out[%s].gl_Position" id
                                                ))

                                                let agg = { code = sprintf "\r\n#ifdef TessControl\r\n%s\r\n#endif\r\n#ifdef TessEval\r\n%s\r\n#endif\r\n" tcsCode tevCode
                                                            usedTypes = Set.union tevc.usedTypes tcsc.usedTypes
                                                            uniformBuffers = uniformBufferUnion tevc.uniformBuffers tcsc.uniformBuffers
                                                            uniforms = Map.union tevc.uniforms tcsc.uniforms }
               
                                                let tcsUsed = Map.remove "InvocationId" tcsUsed

                                                return tcsUsed,Some agg
                                            }

                                        | _ -> compile { return! error "invalid tessellation setup" }

            do! resetCompilerState

            let! vsCode = compile {
                            let vs = 
                                match e.vertexShader with
                                    | Some(vs) -> vs
                                    | None -> { shaderType = ShaderType.Vertex; uniforms = []; inputs = Map.empty; outputs = Map.empty; body = Expr.Value(()); inputTopology = None; debugInfo = None }

                            let additional = tessUsed |> Map.filter (fun k _ -> not <| Map.containsKey k vs.outputs)
                            let vs = addOutputs additional vs

                            let unused = vs.outputs |> Map.filter (fun k v -> not <| Map.containsKey k tessUsed) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq
                            let vs = removeOutputs unused vs

                            let vs = liftIntrinsics glslVersion vs None ((if hasgs then Geometry TriangleStrip else Fragment) |> Some) 
                            
                            let! vsc = compileShader "VS" vs
                            return Some vsc
                          }

            

            let uniforms = Map.empty
            
            let mapUnion (a : Map<'a, 'b>) (b : Map<'a, 'b>) = 
                let mutable r = a
                for KeyValue(k,v) in b do
                    r <- Map.add k v r
                r

            let types, uniforms, uniformBuffers, vsCode = 
                match vsCode with
                    | Some(compiled) -> (compiled.usedTypes,compiled.uniforms, compiled.uniformBuffers, sprintf "#ifdef Vertex\r\n%s#endif\r\n\r\n" compiled.code)
                    | None -> Set.empty,Map.empty, Map.empty, ""

            let types, uniforms, uniformBuffers, teCode = 
                match tessCode with
                    | Some(compiled) -> (Set.union types compiled.usedTypes, mapUnion uniforms compiled.uniforms, uniformBufferUnion uniformBuffers compiled.uniformBuffers, compiled.code)
                    | None -> types, uniforms, uniformBuffers, ""

            let types, uniforms, uniformBuffers, gsCode = 
                match gsCode with
                    | Some(compiled,t) -> (Set.union types compiled.usedTypes, mapUnion uniforms compiled.uniforms, uniformBufferUnion uniformBuffers compiled.uniformBuffers, sprintf "#ifdef Geometry\r\n%s#endif\r\n\r\n" compiled.code)
                    | None -> types, uniforms, uniformBuffers, ""

            let types, uniforms, uniformBuffers, fsCode =
                match fsCode with
                    | Some(compiled) -> (Set.union types compiled.usedTypes, mapUnion uniforms compiled.uniforms, uniformBufferUnion uniformBuffers compiled.uniformBuffers, sprintf "#ifdef Pixel\r\n%s#endif\r\n\r\n" compiled.code)
                    | None -> types, uniforms, uniformBuffers, ""



            let! bufferDecls = uniformBuffers |> Map.toSeq |> Seq.mapC (fun (buffer,elements) ->
                compile {
                    
                    let elements = System.Collections.Generic.HashSet(elements)

                    let groups = elements |> Seq.groupBy (fun (t,_) -> match t with | SamplerType(_) -> true | _ -> false)
                    let (textures, elements) = (groups |> Seq.tryPick (fun (f,g) -> if f then Some g else None), groups |> Seq.tryPick (fun (f,g) -> if f then None else Some g))

                    let! buffer = compile {
                                    match elements with
                                        | Some elements ->
                                            let! elements = elements |> Seq.mapC (fun (t,n) ->
                                                compile {
                                                    let! r = compileVariableDeclaration t n
                                                    return r + ";"
                                                })

                                            
                                            if glslVersion > version120 then
                                                let elements = String.concat "\r\n" elements

                                                let name = match buffer.Parent with
                                                            | None -> "Global"
                                                            | Some _ -> buffer.Name

                                                return sprintf "uniform %s\r\n{\r\n%s\r\n};\r\n" name (String.indent 1 elements) |> Some
                                            else
                                                let elements = elements |> Seq.map (fun l -> "uniform " + l) |> String.concat "\r\n" 
                                                return Some elements

                                        | None -> return None
                                  }

                    let! textures = compile {
                                      match textures with
                                        | Some textures ->
                                            let! textures = textures |> Seq.mapC (fun (t,n) ->
                                                compile {
                                                    let! r = compileVariableDeclaration t n
                                                    return "uniform " + r + ";"
                                                })


                                            let textures = String.concat "\r\n" textures
                                            return textures |> Some
                                        | None -> return None
                                    }

                    match buffer, textures with 
                        | Some b, Some t -> return sprintf "%s\r\n%s" b t
                        | None, Some t -> return t
                        | Some b, None -> return b
                        | None, None -> return ""
                })
            let bufferDecls = String.concat "\r\n" bufferDecls

            let! typeCode = compileTypes types
            
            
            let versionString = sprintf "%d%d0" glslVersion.Major glslVersion.Minor

            let completeCode = sprintf "#version %s\r\n%s\r\n%s\r\n%s%s%s%s" versionString typeCode bufferDecls vsCode teCode gsCode fsCode

            return uniforms, completeCode
        }


    let glsl410 = Compiler(Version(4,1))
    let glsl120 = Compiler(Version(1,2))

    let run e =
        e |> runCompile glsl410

    let compileEffect (e : Compiled<Effect, ShaderState>) : Error<Map<string, UniformGetter> * string> =
        e |> compileEffectInternal |> runCompile glsl410

    let run120 e =
        e |> runCompile glsl120

    let compileEffect120 (e : Compiled<Effect, ShaderState>) : Error<Map<string, UniformGetter> * string> =
        e |> compileEffectInternal |> runCompile glsl120