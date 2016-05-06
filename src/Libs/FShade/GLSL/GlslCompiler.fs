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

    type CompilerConfiguration =
        {
            languageVersion : Version
            enabledExtensions : Set<string>
            
            createUniformBuffers : bool
            createGlobalUniforms : bool
            createBindings : bool
            createDescriptorSets : bool
            createInputLocations : bool
            createRowMajorMatrices : bool

            createPerStageUniforms : bool

            flipHandedness : bool
            depthRange : Range1d
        }

    let private version410 =
        {
            languageVersion = Version(4,1,0)
            enabledExtensions = Set.empty
            
            createUniformBuffers = true
            createGlobalUniforms = false
            createBindings = false
            createDescriptorSets = false
            createInputLocations = true
            createRowMajorMatrices = false
            createPerStageUniforms = false
            flipHandedness = false
            depthRange = Range1d(-1.0,1.0)
        }

    let private version120 =
        {
            languageVersion = Version(1,2,0)
            enabledExtensions = Set.empty
            
            createUniformBuffers = false
            createGlobalUniforms = true
            createBindings = false
            createDescriptorSets = false
            createInputLocations = false
            createRowMajorMatrices = true
            createPerStageUniforms = false
            flipHandedness = false
            depthRange = Range1d(-1.0,1.0)
        }

    let private descriptorSetCounterName = "DescriptorSetCounter"
    let private bindingCounterName = "BindingCounter"


    let nextDescriptorSetIndex = 
        compile {
            let! c = nextCounter descriptorSetCounterName
            do! resetCounter bindingCounterName
            return c
        }

    let nextBindingIndex = nextCounter bindingCounterName

    
    let (|TextureLookup|_|) (mi : MethodInfo) =
        match mi with
            | Method(name, ((SamplerType(dim, isArray, isShadow, isMS, valueType)::_) as args)) ->
                let coordComponents =
                    match dim with
                        | SamplerDimension.Sampler1d -> 1
                        | SamplerDimension.Sampler2d -> 2
                        | SamplerDimension.Sampler3d -> 3
                        | SamplerDimension.SamplerCube -> 3
                        | _ -> failwithf "unknown sampler dimension: %A" dim


                let sampleArgs() = 
                    let consumedArgs, sampleArgs =
                        match isArray, isShadow with
                            | true, true -> 4, sprintf "{0}, vec%d({1}, {2}, {3})" (coordComponents + 2)
                            | true, false -> 3, sprintf "{0}, vec%d({1}, {2})" (coordComponents + 1)
                            | false, true -> 3, sprintf "{0}, vec%d({1}, {2})" (coordComponents + 1)
                            | false, false -> 2, "{0}, {1}"

                    let args = List.skip consumedArgs args

                    let rest =
                        match args with
                            | [] -> ""
                            | _ ->
                                args |> List.mapi (fun i _ -> sprintf "{%d}" (i + consumedArgs)) |> String.concat ", " |> sprintf ", %s"


                    sampleArgs + rest

                let projArgs() =
                    let consumedArgs, sampleArgs =
                        match isArray, isShadow with
                            | true, true -> 4, sprintf "{0}, vec%d({1}, {2}, {3})" (coordComponents + 3)
                            | true, false -> 3, sprintf "{0}, vec%d({1}, {2})" (coordComponents + 2)
                            | false, true -> 3, sprintf "{0}, vec%d({1}, {2})" (coordComponents + 2)
                            | false, false -> 2, "{0}, {1}"

                    let args = List.skip consumedArgs args

                    let rest =
                        match args with
                            | [] -> ""
                            | _ ->
                                args |> List.mapi (fun i _ -> sprintf "{%d}" (i + consumedArgs)) |> String.concat ", " |> sprintf ", %s"


                    sampleArgs + rest

                let plainArgs() =
                    args |> List.mapi (fun i _ -> sprintf "{%d}" (i + 1)) |> String.concat ", " |> sprintf "{0}, %s"
                        

                let functionName = 
                    match name with
                        | "get_Size" -> "textureSize({0}, 0)"
                        | "get_MipMapLevels" -> "textureQueryLevels({0})"
                        | "GetSize" -> "textureSize({0}, {1})"
                        | "Sample" -> sprintf "texture(%s)" (sampleArgs())
                        | "SampleOffset" -> sprintf "textureOffset(%s)" (sampleArgs())
                        | "SampleProj" -> sprintf "textureProj(%s)" (projArgs())
                        | "SampleLevel" -> sprintf "textureLod(%s)" (sampleArgs())
                        | "SampleGrad" -> sprintf "textureGrad(%s)" (sampleArgs())
                        | "Gather" -> sprintf "textureGather(%s)" (plainArgs())
                        | "GatherOffset" -> sprintf "textureGatherOffset(%s)" (plainArgs())
                        | "Read" -> sprintf "texelFetch(%s)" (plainArgs())
                        | name -> failwithf "unknown sampler function %A" name

                Some functionName
            | _ ->
                None


    type Compiler(config : CompilerConfiguration) =
        let funDefCache = GenericMemoCache()

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

        member x.Config = config

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
                        | Method("op_Modulus", [Integral; Integral]) -> return Some "{0} % {1}"
                        | Method("op_Modulus", [_; _]) -> return Some "mod({0},{1})"

                        | Method("op_LessThan", [Num; Num]) -> return Some "{0} < {1}"
                        | Method("op_GreaterThan", [Num; Num]) -> return Some "{0} > {1}"
                        | Method("op_GreaterThanOrEqual", [Num; Num]) -> return Some "{0} >= {1}"
                        | Method("op_LessThanOrEqual", [Num; Num]) -> return Some "{0} <= {1}"
                        | Method("op_Equality", [_; _]) -> return Some "{0} == {1}"
                        | Method("op_Dereference", [Ref(_)]) -> return Some "{0}"
                        | Method("op_ColonEquals", [Ref(_);_]) -> return Some "{0} = {1};\r\n"
                        | Method("op_UnaryNegation", [_]) -> return Some "-{0}"

                        | Method("op_RightShift", [Num; Num]) -> return Some "{0} >> {1}"
                        | Method("op_LeftShift", [Num; Num]) -> return Some "{0} << {1}"
                        | Method("op_BitwiseAnd", [Num; Num]) -> return Some "{0} & {1}"
                        | Method("op_BitwiseOr", [Num; Num]) -> return Some "{0} | {1}"
                        | Method("op_ExclusiveOr", [Num; Num]) -> return Some "{0} ^ {1}"
                        | Method("op_LogicalNot", [Num]) -> return Some "~{0}"

                        | Method("get_Length", [Vector]) -> return Some "length({0})"
                        | Method("get_LengthSquared", [Vector]) -> return Some "dot({0}, {0})"
                        | Method("get_Normalized", [Vector]) -> return Some "normalize({0})"
                        | Method("Dot", [Vector; Vector]) -> return Some "dot({0}, {1})"
                        | Method("Cross", [Vector; Vector]) -> return Some "cross({0}, {1})"
                        | Method("Lerp", [Num|Vector; Num|Vector; Num]) -> return Some "mix({0}, {1}, {2})"

                        | Method("Abs", [Num]) -> return Some "abs({0})"
                        | Method("Sign", [Num]) -> return Some "sign({0})"
                        | Method("Min", [Num; Num]) -> return Some "min({0}, {1})"
                        | Method("Max", [Num; Num]) -> return Some "max({0}, {1})"
                        | Method("Clamp", [Num; Num; Num]) -> return Some "clamp({0}, {1}, {2})"
                        | Method("Pow", [Num; Num]) -> return Some "pow({0}, {1})"
                        | Method("Sqrt", [Num; Num]) -> return Some "sqrt({0})"
                        | Method("Exp", [Num]) -> return Some "exp({0})"
                        | Method("Log", [Num]) -> return Some "log({0})"
                        | Method("Log2", [Num]) -> return Some "log2({0})"


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
                        
                        | MethodQuote <@ V2d.Distance : _ * _ -> float @> _ -> return Some "distance({0}, {1})"
                        | MethodQuote <@ V3d.Distance : _ * _ -> float @> _ -> return Some "distance({0}, {1})"
                        | MethodQuote <@ V4d.Distance : _ * _ -> float @> _ -> return Some "distance({0}, {1})"

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

                        | MethodQuote <@ M33d.FromCols : _ * _ * _ -> M33d @> _ -> return Some "mat3({0}, {1}, {2})"
                        | MethodQuote <@ M44d.FromCols : _ * _ * _ * _ -> M44d @> _ -> return Some "mat4({0}, {1}, {2}, {3})"
                        | MethodQuote <@ M33d.FromRows : _ * _ * _ -> M33d @> _ -> return Some "transpose(mat3({0}, {1}, {2}))"
                        | MethodQuote <@ M44d.FromRows : _ * _ * _ * _ -> M44d @> _ -> return Some "transpose(mat4({0}, {1}, {2}, {3}))"
                        
                        | MethodQuote <@ M44d.Transpose @> _ -> return Some "transpose({0})"
                        | Method("get_Transposed", [Matrix]) -> return Some "transpose({0})"


                        | MethodQuote <@ ref @> [_] -> return Some "{0}"
                        | MethodQuote <@ LanguagePrimitives.IntrinsicFunctions.GetArray @> [_] -> return Some "{0}[{1}]"
                        | MethodQuote <@ LanguagePrimitives.IntrinsicFunctions.SetArray @> [_] -> return Some "{0}[{1}] = {2}"
                        | MethodQuote <@ emitVertex @> [] -> return Some "EmitVertex();\r\n"
                        | MethodQuote <@ discard @> [] -> return Some "discard;\r\n"
                        | MethodQuote <@ ddx<int> @> _ -> return Some "dFdx({0})"
                        | MethodQuote <@ ddy<int> @> _ -> return Some "dFdy({0})"
                        | MethodQuote <@ endPrimitive @> _ -> return Some "EndPrimitive()"
                        | MethodQuote <@ restartStrip @> _ -> return Some "EndPrimitive()"


                        | TextureLookup(fmt) when config.languageVersion > Version(1,2) -> return Some fmt

                        
                        | Method("Sample", SamplerType(dim, isArray, isShadow, isMS, valueType)::args) -> 
                            let samplerFun =
                                match dim with
                                    | SamplerDimension.Sampler1d -> "texture1D"
                                    | SamplerDimension.Sampler2d -> "texture2D"
                                    | SamplerDimension.Sampler3d -> "texture3D"
                                    | SamplerDimension.SamplerCube-> "textureCube"
                                    | _ -> failwith "unsupported sampler type"

                            return Some (sprintf "%s({0}, {1})" samplerFun)

                        | Method("get_Length", [FixedArrayType(s,t)]) -> return Some (sprintf "%d" s)
                        | Method("get_Item", [FixedArrayType(s,t);v]) -> return Some "{0}[{1}]"
                        | Method("set_Item", [FixedArrayType(s,t);i;v]) -> return Some "{0}[{1}] = {2}"

                        | MethodQuote <@ float : int -> float @> _ -> return Some "float({0})"
                        | MethodQuote <@ int : float -> int @> _ -> return Some "int({0})"

                        | _ -> return None
                }

            member x.CompileIntrinsicFunctionDefinition(mi : MethodInfo) =
                let e = funDefCache.Invoke( (fun mi ->
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
                        ), mi)

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

                        | v when t.IsArray ->
                            let contentType = t.GetElementType()
                            let arr = v |> unbox<Array>
                            let elements = Array.init arr.Length (fun i -> arr.GetValue(i))
                            let! content = elements |> Seq.mapC (fun e -> compileValue contentType e)
                            let! t = compileType contentType

                            return sprintf "const %s %s[%d] = %s[](%s);" t name elements.Length t (String.concat ", " content)

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

                            let! fieldValues = 
                                Meta.VecFields 
                                    |> Seq.take d 
                                    |> Seq.map (fun name -> t.GetField(name, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic))
                                    |> Seq.mapC (fun fi -> compileValue fi.FieldType (fi.GetValue(o)))

                            return sprintf "vec%d(%s)" d (String.concat ", " fieldValues) |> Some
                        | Float32|Float64 ->
                            let d = Convert.ToDouble(o)
                            return d.ToString(System.Globalization.CultureInfo.InvariantCulture) |> Some
                        | Num ->
                            return o.ToString() |> Some
                        | Bool ->
                            let b = o |> unbox<bool>
                            return Some(if b then "true" else "false")
                        | _ -> return None 
                }

            member x.InitialState() = emptyShaderState

            member x.ResetState(state : CompilerState) = 
                { emptyShaderState with counters = state.counters }

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
                    Success(s,c.Config.languageVersion)
                | _ ->
                    Error "cannot determine version for non GLSL compiler"
        }

    let private config : Compiled<_, ShaderState> =
        { runCompile = fun s ->
            match s.compiler with
                | :? Compiler as c ->
                    Success(s,c.Config)
                | _ ->
                    Error "cannot determine version for non GLSL compiler"
        }



    // intrisics are taken from: https://www.opengl.org/wiki/Built-in_Variable_(GLSL)

    let private getIntrinsicInputName (v : CompilerConfiguration) (t : ShaderType) (last : Option<ShaderType>) (s : string) =
        match s, last with
            | "PointSize", Some _ -> 
                Some "gl_PointSize"

            | "ClipDistance", Some _  -> 
                Some "gl_ClipDistance"

            | _ ->
                match t with
                    | Vertex ->
                        match s with
                            | "VertexId" -> Some "gl_VertexID"
                            | "InstanceId" -> Some "gl_InstanceID"
                            | _ -> None

                    | TessControl ->
                        match s with
                            | "PatchVertices" -> Some "gl_PatchVerticesIn"
                            | "PrimitiveId" -> Some "gl_PrimitiveID"
                            | "InvocationId" -> Some "gl_InvocationID"
                            | _ -> None
                    | TessEval ->
                        match s with
                            | "TessCoord" -> Some "gl_TessCoord"
                            | "PatchVertices" -> Some "gl_PatchVerticesIn"
                            | "PrimitiveId" -> Some "gl_PrimitiveID"
                            | "TessLevelInner" -> Some "gl_TessLevelInner"
                            | "TessLevelOuter" -> Some "gl_TessLevelOuter"
                            | _ -> None

                    | Geometry(_) ->
                        match s with
                            | "PrimitiveId" -> Some "gl_PrimitiveIDIn"
                            | "InvocationId" -> Some "gl_InvocationID"
                            | _ -> None

                    | Fragment ->
                        match s with
                            | "FragCoord" -> Some "gl_FragCoord"
                            | "FrontFacing" -> Some "gl_FrontFacing"
                            | "PointCoord" -> Some "gl_PointCoord"
                            | "SampleId" -> Some "gl_SampleID"
                            | "SamplePosition" -> Some "gl_SamplePosition"
                            | "SampleMask" -> Some "gl_SampleMaskIn"
                            | "PrimitiveId" -> Some "gl_PrimitiveID"
                            | "Layer" -> Some "gl_Layer"
                            | "ViewportIndex" -> Some "gl_ViewportIndex"
                            | _ -> None

    let private getIntrinsicOutputName (v : CompilerConfiguration) (t : ShaderType) (next : Option<ShaderType>) (s : string) =
        match s, next with
            | "Positions", Some Fragment -> 
                Some "gl_Position"

            | "PointSize", Some _ -> 
                Some "gl_PointSize"

            | "ClipDistance", Some _  -> 
                Some "gl_ClipDistance"

            | _ ->
                match t with
                    | Vertex ->
                        None

                    | TessControl ->
                        match s with
                            | "TessLevelInner" -> Some "gl_TessLevelInner"
                            | "TessLevelOuter" -> Some "gl_TessLevelOuter"
                            | _ -> None

                    | TessEval ->
                        None

                    | Geometry(_) ->
                        match s with
                            | "Layer" -> Some "gl_Layer"
                            | "ViewportIndex" -> Some "gl_ViewportIndex"
                            | "PrimitiveId" -> Some "gl_PrimitiveID"
                            | _ -> None

                    | Fragment ->
                        match s with
                            | "Depth" -> Some "gl_FragDepth"
                            | "Colors" when v.languageVersion <= Version(1,2) -> Some "gl_FragColor"
                            | _ -> None

    
    let private stageName (s : ShaderType) =
        match s with
            | Vertex -> "Vertex"
            | Geometry(_) -> "Geometry"
            | TessControl -> "TessControl"
            | TessEval -> "TessEval"
            | Fragment -> "Fragment"
            

    //compilation functions
    let private changeIONames (s : Shader) (last : Option<ShaderType>) (next : Option<ShaderType>) =
        let newInputs = 
            match last with
                | Some stage ->
                    s.inputs |> Map.toList |> List.map(fun (k,v) -> k,v.Name,Var(sprintf "%s%s" k (stageName stage), v.Type, v.IsMutable))
                | None -> s.inputs |> Map.toList |> List.map (fun (k,v) -> k,v.Name,v)

        let inputRepl = newInputs |> List.map (fun (_,k,v) -> (k,Expr.Var v)) |> Map.ofList
        let newInputs = newInputs |> List.map (fun (k,_,v) -> (k,v)) |> Map.ofList

        let newOutputs = 
            match next with
                | Some _ ->
                    s.outputs |> Map.toList |> List.map (fun (k,(o,v)) -> (k,o,v,Var(sprintf "%s%s" k (stageName s.shaderType), v.Type, v.IsMutable)))
                | None -> s.outputs |> Map.toList |> List.map (fun (k,(o,v)) -> k,o,v,v)

        let outputRepl = newOutputs |> List.map (fun (_,_,o,n) -> o.Name, Expr.Var n) |> Map.ofList
        let newOutputs = newOutputs |> List.map (fun (k,o,_,v) -> k,(o,v)) |> Map.ofList

        let body = s.body.Substitute (fun v -> Map.tryFind v.Name inputRepl)
        let body = body.Substitute (fun v -> Map.tryFind v.Name outputRepl)


        { s with body = body; inputs = newInputs; outputs = newOutputs}



    let private liftIntrinsics (config : CompilerConfiguration) (s : Shader)  (last : Option<ShaderType>) (next : Option<ShaderType>)=
        
        let s =
            if config.createInputLocations then s
            else changeIONames s last next
        
        let mutable body = s.body
        let mutable inputs = Map.empty
        let mutable outputs = Map.empty

        for KeyValue(sem, v) in s.inputs do
            match getIntrinsicInputName config s.shaderType last sem with
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
            match getIntrinsicOutputName config s.shaderType next sem with
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

    let private adjustPositionWrites (config : CompilerConfiguration) (s : Shader)  (last : Option<ShaderType>) (next : Option<ShaderType>) =
        match next with
            | Some Fragment ->
                if config.flipHandedness || config.depthRange.Min <> -1.0 || config.depthRange.Max <> 1.0 then
                    let rec replacePositionWrites (e : Expr) =
                        match e with
                            | VarSet(v, e) when v.Name = "gl_Position" ->
                                
                                let flip = config.flipHandedness
                                let range = config.depthRange

                                let intermediate = Var("_pos", v.Type, true)
                                let ie = Expr.Var intermediate

                                let newZ =
                                    if range.Min <> -1.0 || range.Max <> 1.0 then
                                       
                                        // newZ = a * z + b * w

                                        // due to projective division:
                                        // max = (a * w + b * w) / w
                                        // min = (a * -w + b * w) / w

                                        // therefore:
                                        // (1) max = a + b
                                        // (2) min = b - a

                             
                                        // (1) + (2) => max + min = 2 * b
                                        // (1) - (2) => max - min = 2 * a

                                        // so finally we get:
                                        // a = (max - min) / 2
                                        // b = (max + min) / 2


                                        let a = range.Size / 2.0
                                        let b = (range.Min + range.Max) / 2.0

                                        if a = b then
                                            <@@ a * ((%%ie : V4d).Z + (%%ie : V4d).W) @@>
                                        else
                                            <@@ a * (%%ie : V4d).Z + b * (%%ie : V4d).W @@>
                                    else
                                        <@@ (%%ie : V4d).Z @@>

                                let ie =
                                    if config.flipHandedness then
                                        <@@ V4d((%%ie : V4d).X, -(%%ie : V4d).Y, %%newZ, (%%ie : V4d).W)  @@>
                                    else
                                        ie


                                Expr.Let(intermediate, e, Expr.VarSet(v, ie))
//
//                                let final = 
//                                    if config.flipHandedness then
//                                        let y = v.Type.GetField "Y"
//                                        Expr.Sequential(
//                                            Expr.FieldSet(Expr.Var intermediate, y, <@@ -(%%ie : V4d).Y @@>),
//                                            final
//                                        )
//                                    else final
//
//                                Expr.Let(intermediate, e, final)

//                                <@@
//                                    let mutable v : V4d = V4d.Zero
//                                    let mutable gl_PositionStandard = (%%e : V4d)
//                                    gl_PositionStandard.Y <- -gl_PositionStandard.Y
//                                    gl_PositionStandard.Z <- (gl_PositionStandard.Z + gl_PositionStandard.W) * 0.5
//                                    v <- gl_PositionStandard
//                                @@>
                            | ShapeCombination(o, args) -> RebuildShapeCombination(o, args |> List.map replacePositionWrites)
                            | ShapeLambda(v,b) -> Expr.Lambda(v, replacePositionWrites b)
                            | ShapeVar _ -> e

                    { s with body = replacePositionWrites s.body }
                else
                    s
            | _ ->
                s

    let private adjustToConfig (config : CompilerConfiguration) (s : Shader)  (last : Option<ShaderType>) (next : Option<ShaderType>) =
        let s = liftIntrinsics config s last next
        let s = adjustPositionWrites config s last next
        s

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

    let private compileUniforms (uniforms : Map<UniformScope, list<Type * string>>) =
        compile {
            let! config = config

            let textures = 
                uniforms 
                    |> Map.toSeq 
                    |> Seq.collect (fun (_,elements) -> elements |> Seq.filter(function ((SamplerType _),_) -> true | _ -> false))
                    |> Seq.toList

            let uniformBuffers =
                uniforms
                    |> Map.map (fun scope elements -> elements |> List.filter (function (SamplerType(_),_) -> false | _ -> true))
                    |> Map.filter (fun _ e -> not <| List.isEmpty e)

            let! ds = 
                if Map.isEmpty uniformBuffers || not config.createDescriptorSets then compile { return -1 }
                else nextDescriptorSetIndex

            let! bufferDecls = uniformBuffers |> Map.toSeq |> Seq.mapC (fun (buffer,elements) ->
                compile {
                    let! uniformLayoutPrefix = 
                        compile {
                            match config.createDescriptorSets, config.createBindings with
                                | true, true -> 
                                    let! b = nextBindingIndex
                                    return sprintf "layout(set = %d, binding = %d)\r\n" ds b

                                | false, true ->
                                    let! b = nextBindingIndex
                                    return sprintf "layout(binding = %d)\r\n" b
                                    
                                | true, false ->
                                    return sprintf "layout(set = %d)\r\n" ds

                                | false, false ->
                                    return ""
                        }

                    let! elements = elements |> Seq.mapC (fun (t,n) ->
                        compile {
                            let prefix =
                                if config.createRowMajorMatrices then
                                    match t with
                                        | MatrixOf(s, bt) -> "layout(row_major) "
                                        | _ -> ""
                                else
                                    ""
                            let! r = compileVariableDeclaration t n
                            return prefix + r + ";"
                        })

                    let declareBuffer (name : string) (elements : seq<string>) =
                        elements 
                            |> String.concat "\r\n" 
                            |> String.indent 1
                            |> sprintf "%suniform %s\r\n{\r\n%s\r\n};\r\n" uniformLayoutPrefix name

                    let declareGlobal (elements : seq<string>) =
                        elements 
                            |> Seq.map (fun l -> "uniform " + l) 
                            |> String.concat "\r\n" 
                            |> sprintf "%s%s" uniformLayoutPrefix

                    match buffer.Parent with
                        | None ->
                            if config.createGlobalUniforms then return elements |> declareGlobal
                            else return elements |> declareBuffer "Global"
                        | Some _ -> 
                            let name = buffer.Name
                            if config.createUniformBuffers then return elements |> declareBuffer name
                            else return elements |> declareGlobal

                })

            let! textureDecls = textures |> Seq.mapCi (fun i (t,n) ->
                compile {
                    let! textureLayoutPrefix =
                        compile {
                            match config.createDescriptorSets, config.createBindings with
                                | true, true -> 
                                    let! ds = nextDescriptorSetIndex
                                    let! b = nextBindingIndex
                                    return sprintf "layout(set = %d, binding = %d) " ds b

                                | false, true ->
                                    let! b = nextBindingIndex
                                    return sprintf "layout(binding = %d) " b

                                | true, false ->
                                    let! ds = nextDescriptorSetIndex
                                    return sprintf "layout(set = %d) " ds

                                | false, false ->
                                    return ""
                        }

                    let! r = compileVariableDeclaration t n
                    return sprintf "%suniform %s;" textureLayoutPrefix r
                })


            return (String.concat "\r\n" bufferDecls) + (String.concat "\r\n" textureDecls)
        }

    let private compileShader (entryName : string) (s : Shader) =
        compile {
            do! resetCompilerState

            

            let! (header, c) = compileMainWithoutTypes [] s.body entryName
            let! state = compilerState : Compiled<ShaderState, ShaderState>
            let! config = config



            let uniforms' = 
                [
                    yield! s.uniforms
                    yield! state.uniforms |> HashMap.toSeq
                ]

            let inputs = s.inputs |> Seq.sortBy(fun (KeyValue(_,n)) -> n.Name)
            let outputs = s.outputs |> Seq.sortBy(fun (KeyValue(_,(_,n))) -> n.Name)
            let uniforms = 
                uniforms'  
                    |> Seq.map(fun (u,v) ->
                        match u with
                            | UserUniform(t,o) -> (uniform, t, v.Name)
                            | Attribute(scope, t, n) -> (scope, t, n)
                            | SamplerUniform(t,sem, n,_) -> (uniform, t, n)
                       ) 
                    |> Seq.groupBy(fun (s,_,_) -> s)
                    |> Seq.map (fun (g,v) -> (g, v |> Seq.map (fun (_,t,n) -> (t,n)) |> Seq.toList))
                    |> Map.ofSeq


            let! inputs = 
                inputs 
                    |> Seq.mapCs 0  (fun i (KeyValue(_,n)) -> 
                        compile {
                    
                            let modifier =
                                if config.createInputLocations then
                                    sprintf "layout(location = %d) in" i
                                else
                                    if config.languageVersion > Version(1,2) then "in"
                                    else
                                        if s.shaderType = ShaderType.Vertex then "attribute" 
                                        else "varying"

                            let size =
                                match n.Type with
                                    | MatrixOf(size, t) ->
                                        if size.X = 4 then size.Y
                                        elif size.X = 3 then size.Y
                                        else 1
                                    | _ -> 1

                            if n.Type.IsArray then
                                let t = n.Type.GetElementType()
                                let! r = compileVariableDeclaration t n.Name
                                return (i+size),sprintf "%s %s[];" modifier r

                            else
                                let! r = compileVariableDeclaration n.Type n.Name
                                return (i+size),sprintf "%s %s;" modifier r

                        }
                    )

            let! outputs = outputs |> Seq.mapCs 0 (fun i (KeyValue(_,(_,n))) -> 
                compile {
                    let modifier =
                        if config.createInputLocations then sprintf "layout(location = %d) out" i
                        else
                            if config.languageVersion > Version(1,2) then "out"
                            else "varying"


                    let size =
                            match n.Type with
                                | MatrixOf(size, t) ->
                                    if size.X = 4 then size.Y
                                    elif size.X = 3 then size.Y
                                    else 1
                                | _ -> 1

                    if s.shaderType = ShaderType.TessControl then
                        let t = n.Type
                        let! r = compileVariableDeclaration t n.Name
                        return (i+size),sprintf "%s %s[];" modifier r
                    else
                        let! r = compileVariableDeclaration n.Type n.Name
                        return (i+size),sprintf "%s %s;" modifier r
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
                            | Geometry(maxVertices, top) -> 
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
                                match maxVertices with
                                    | Some v -> sprintf "layout(%s, max_vertices = %d) out;\r\nlayout(%s) in;\r\n" top v itop
                                    | None -> 
                                        let range = 
                                            s.body |> ExprUtilities.estimateNumberOfCallsTo (getMethodInfo <@ emitVertex @>)

                                        if range.Max >= 0 then
                                            sprintf "layout(%s, max_vertices = %d) out;\r\nlayout(%s) in;\r\n" top range.Max itop
                                        else
                                            Log.warn "could not determine upper bound for emitVertex-calls (%A): using 32, please annotate" range
                                            sprintf "layout(%s, max_vertices = 32) out;\r\nlayout(%s) in;\r\n" top itop

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

            let! uniformDecls = 
                if config.createPerStageUniforms then compileUniforms uniforms
                else compile { return "" }

            let completeCode = sprintf "%s\r\n\r\n%s\r\n%s\r\n%s\r\n%s\r\n%s\r\n%s%s" uniformDecls def header disp inputs outputs layout c

            return { usedTypes = types; uniforms = uniformGetters; uniformBuffers = uniforms; code = completeCode }
        }

    let private compileEffectInternal (neededOutputs : Map<string, Type>) (e : Compiled<Effect, ShaderState>) =
        compile {
            let! e = e
            let! config = config

            let hasgs = match e.geometryShader with | Some _ -> true | _ -> false
            let! fsUsed,fsCode = match e.fragmentShader with
                                    | Some(fs) -> compile {
                                                    let additional =
                                                        neededOutputs 
                                                            |> Map.remove "Depth"
                                                            |> Map.filter (fun k t ->
                                                                match Map.tryFind k fs.outputs with
                                                                    | Some _ -> false
                                                                    | None -> true
                                                            )
                                                    let fs = addOutputs additional fs

                                                    let unused = fs.outputs |> Map.filter (fun k (t,v) -> 
                                                                    match t with
                                                                        | Some t -> false
                                                                        | None -> not <| Map.containsKey k neededOutputs
                                                                 ) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq

                                                    let fs = removeOutputs unused fs

                                                    let fs = adjustToConfig config fs ((if hasgs then Geometry(None, TriangleStrip) else Vertex) |> Some) None

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

                                                    let gs = adjustToConfig config gs (Some Vertex) (Some Fragment)
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

                                                let tev = adjustToConfig config tev (Some Vertex) (Some Fragment)
                                                let tcs = adjustToConfig config tcs (Some Vertex) (Some Fragment)


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
                                                            usedTypes = PersistentHashSet.union tevc.usedTypes tcsc.usedTypes
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

                            let vs = adjustToConfig config vs None ((if hasgs then Geometry(None, TriangleStrip) else Fragment) |> Some) 
                            
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
                    | None -> PersistentHashSet.empty,Map.empty, Map.empty, ""

            let types, uniforms, uniformBuffers, teCode = 
                match tessCode with
                    | Some(compiled) -> (PersistentHashSet.union types compiled.usedTypes, mapUnion uniforms compiled.uniforms, uniformBufferUnion uniformBuffers compiled.uniformBuffers, compiled.code)
                    | None -> types, uniforms, uniformBuffers, ""

            let types, uniforms, uniformBuffers, gsCode = 
                match gsCode with
                    | Some(compiled,t) -> (PersistentHashSet.union types compiled.usedTypes, mapUnion uniforms compiled.uniforms, uniformBufferUnion uniformBuffers compiled.uniformBuffers, sprintf "#ifdef Geometry\r\n%s#endif\r\n\r\n" compiled.code)
                    | None -> types, uniforms, uniformBuffers, ""

            let types, uniforms, uniformBuffers, fsCode =
                match fsCode with
                    | Some(compiled) -> (PersistentHashSet.union types compiled.usedTypes, mapUnion uniforms compiled.uniforms, uniformBufferUnion uniformBuffers compiled.uniformBuffers, sprintf "#ifdef Pixel\r\n%s#endif\r\n\r\n" compiled.code)
                    | None -> types, uniforms, uniformBuffers, ""


            let! uniformDecls = 
                if config.createPerStageUniforms then compile { return "" }
                else compileUniforms uniformBuffers

            let! typeCode = compileTypes types
            
            
            let extensions = config.enabledExtensions |> Seq.map (sprintf "#extension %s : enable") |> String.concat "\r\n"

            let versionString = 
                let v = config.languageVersion
                sprintf "%d%d0" v.Major v.Minor

            let completeCode = sprintf "#version %s\r\n%s\r\n%s\r\n%s\r\n%s%s%s%s" versionString extensions typeCode uniformDecls vsCode teCode gsCode fsCode

            return uniforms, completeCode
        }


    let glsl410 = Compiler(version410)
    let glsl120 = Compiler(version120)

    let private compilers = Dict.ofList [version120, glsl120; version410, glsl410]

    let run (v : CompilerConfiguration) e =
        let c = compilers.GetOrCreate(v, Func<_,_>(fun v -> Compiler(v)))
        e |> runCompile c

    let compileEffect (v : CompilerConfiguration) (neededOutputs : Map<string, Type>) (e : Compiled<Effect, ShaderState>) : Error<Map<string, UniformGetter> * string> =
        let c = compilers.GetOrCreate(v, Func<_,_>(fun v -> Compiler(v)))
        e |> compileEffectInternal neededOutputs |> runCompile c

    let run410 e =
        e |> runCompile glsl410

    let compileEffect410 (neededOutputs : Map<string, Type>) (e : Compiled<Effect, ShaderState>) : Error<Map<string, UniformGetter> * string> =
        e |> compileEffectInternal neededOutputs |> runCompile glsl410

    let run120 e =
        e |> runCompile glsl120

    let compileEffect120 (neededOutputs : Map<string, Type>) (e : Compiled<Effect, ShaderState>) : Error<Map<string, UniformGetter> * string> =
        e |> compileEffectInternal neededOutputs |> runCompile glsl120