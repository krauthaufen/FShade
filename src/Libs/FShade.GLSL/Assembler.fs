namespace FShade.GLSL

open System
open System.Reflection

open Aardvark.Base
open Aardvark.Base.Monads.State

open FShade
open FShade.Imperative
open FShade.GLSL.Utilities
open FSharp.Data.Adaptive

type BindingMode =
    | None = 0
    | Global = 1
    | PerKind = 2

type Config =
    {
        version                 : Version
        enabledExtensions       : Set<string>
        createUniformBuffers    : bool

       
        bindingMode             : BindingMode
        createDescriptorSets    : bool
        stepDescriptorSets      : bool
        createInputLocations    : bool
        createPerStageUniforms  : bool
        reverseMatrixLogic      : bool
    }

type GLSLShader =
    {
        code        : string
        iface       : GLSLProgramInterface
    }

    override x.ToString() =
        String.Join(Environment.NewLine,
            "====================== CODE ======================",
            x.code,
            "====================== CODE ======================",
            "======================= IO =======================",
            x.iface.ToString(),
            "======================= IO =======================")
        

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
                    
                Some {
                    intrinsicTypeName = name
                    tag = 
                        GLSLTextureType.GLSLSampler {
                            original = t
                            dimension = dim
                            isShadow = shadow
                            isArray = arr
                            isMS = ms
                            valueType = GLSLType.ofCType config.reverseMatrixLogic (CType.ofType x valueType)
                        }
                }

            | ImageType(tFmt, dim, arr, ms, valueType) ->


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

                let fmt = tFmt.Name


                let name = 
                    if arr then sprintf "%simage%s%sArray" typePrefix dimStr msSuffix
                    else sprintf "%simage%s%s" typePrefix dimStr msSuffix 

                Some {
                    intrinsicTypeName = name
                    tag = 
                        GLSLTextureType.GLSLImage {
                            original = t
                            dimension = dim
                            format = ImageFormat.ofFormatType tFmt
                            isArray = arr
                            isMS = ms
                            valueType = GLSLType.ofCType config.reverseMatrixLogic (CType.ofType x valueType)
                        }
                }
              
            | AccelerationStructure ->
                Some {
                    intrinsicTypeName = "accelerationStructureNV"
                    tag = t
                }

            | _ ->
                None


type InputKind =
    | Any = 0
    | UniformBuffer = 1
    | StorageBuffer = 2
    | Sampler = 3
    | Image = 4

type AssemblerState =
    {
        config                  : Config
        stages                  : ShaderStageDescription

        currentDescriptorSet    : int
        currentBinding          : Map<InputKind, int>
        currentInputLocation    : int
        currentOutputLocation   : int
        requiredExtensions      : Set<string>
        ifaceNew                : GLSLProgramInterface
        
        currentFunction         : Option<CFunctionSignature>
        functionInfo            : HashMap<CFunctionSignature, GLSLShaderInterface>

        uniformBuffers          : MapExt<string, GLSLUniformBuffer>
        samplers                : MapExt<string, GLSLSampler>
        images                  : MapExt<string, GLSLImage>
        storageBuffers          : MapExt<string, GLSLStorageBuffer>

        textureInfos            : Map<string, list<string * SamplerState>>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AssemblerState =
    let ofConfig (c : Config) =
        {
            config = c
            stages = { prev = None; self = ShaderStage.Vertex; next = None }
            currentDescriptorSet = 0
            currentBinding = Map.empty
            currentInputLocation = 0
            currentOutputLocation = 0
            requiredExtensions = Set.empty
            ifaceNew =
                {
                    inputs          = []
                    outputs         = []
                    samplers        = MapExt.empty
                    images          = MapExt.empty
                    storageBuffers  = MapExt.empty
                    uniformBuffers  = MapExt.empty
                    shaders         = MapExt.empty
                }
                
            currentFunction = None
            functionInfo = HashMap.empty

            uniformBuffers = MapExt.empty
            samplers       = MapExt.empty
            images         = MapExt.empty
            storageBuffers = MapExt.empty


            textureInfos = Map.empty
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

    let newBinding (kind : InputKind) (cnt : int) =
        State.custom (fun s ->
            let c = s.config
            match c.bindingMode with
                | BindingMode.None ->
                    s, -1

                | BindingMode.Global ->
                    let b = Map.tryFind InputKind.Any s.currentBinding |> Option.defaultValue 0
                    { s with currentBinding = Map.add InputKind.Any (b + cnt) s.currentBinding }, b
                | _ -> //BindingMode.PerKind ->
                    
                    let b = Map.tryFind kind s.currentBinding |> Option.defaultValue 0
                    { s with currentBinding = Map.add kind (b + cnt) s.currentBinding }, b

        )

    let newSet =
        State.custom (fun s ->
            let c = s.config
            if c.createDescriptorSets then
                let set = s.currentDescriptorSet
                if c.stepDescriptorSets then
                    { s with currentDescriptorSet = set + 1; currentBinding = Map.empty }, set
                else
                    s, set
                    
            else
                s, -1
        )

    let rec private neededLocations (rev : bool) (t : CType) =
        match t with
            | CPointer(_,et) -> 
                neededLocations rev et

            | CType.CBool _ | CType.CFloat _ | CType.CInt _ ->
                1

            | CMatrix(et, rows, cols) ->
                if rev then
                    let l = CVector(et, cols) |> neededLocations rev
                    l * rows
                else
                    let l = CVector(et, rows) |> neededLocations rev
                    l * cols
            | CVector(et,_) -> 
                1
            | CArray(et, len) ->
                let inner = neededLocations rev et
                inner * len

            | CStruct _ | CIntrinsic _ | CVoid ->
                1 //failwith "[GLSL] no struct inputs allowed"

    let newLocation (kind : ParameterKind) (t : CType) =
        State.custom (fun s ->
            match kind with
                | ParameterKind.Input ->
                    let free = s.currentInputLocation
                    { s with currentInputLocation = free + neededLocations s.config.reverseMatrixLogic t }, free
                | ParameterKind.Output ->
                    let free = s.currentOutputLocation
                    { s with currentOutputLocation = free + neededLocations s.config.reverseMatrixLogic t }, free
                | _ ->
                    s, -1
                    
        )

    
module Interface =
    let private modify (f : AssemblerState -> GLSLProgramInterface -> GLSLProgramInterface) =
        State.modify (fun (s : AssemblerState) ->
            { s with ifaceNew = f s s.ifaceNew }
        )

    let private emptyShader =
        {
            program                 = Unchecked.defaultof<_>
            shaderStage             = ShaderStage.Compute
            shaderEntry             = ""

            shaderInputs            = []
            shaderOutputs           = []
            shaderSamplers          = HashSet.empty
            shaderImages            = HashSet.empty
            shaderStorageBuffers    = HashSet.empty
            shaderUniformBuffers    = HashSet.empty
            shaderBuiltInFunctions  = HashSet.empty
            shaderDecorations       = []
            shaderBuiltIns          = MapExt.empty
        }

    let private updateShaderInterface (action : AssemblerState -> GLSLShaderInterface -> GLSLShaderInterface) =
        State.modify (fun s ->
            match s.currentFunction with
                | Some f ->
                    let info =
                        match HashMap.tryFind f s.functionInfo with
                            | Some i -> i
                            | None -> emptyShader
                    let newInfo  = action s info
                    { s with functionInfo = HashMap.add f newInfo s.functionInfo }
                | None ->
                    { s with
                        ifaceNew =
                            { s.ifaceNew with shaders = MapExt.alter s.stages.self (function Some sh -> Some (action s sh) | None -> None) s.ifaceNew.shaders }
                    }

        )

    let addDecorations (stage : ShaderStage) (dec : list<EntryDecoration>) =
        let dec = 
            dec |> List.choose (fun e ->
                match e with
                    | EntryDecoration.InputTopology t -> GLSLInputTopology t |> Some
                    | EntryDecoration.Invocations 1 -> None
                    | EntryDecoration.Invocations t -> GLSLInvocations t |> Some
                    | EntryDecoration.LocalSize t -> GLSLLocalSize t |> Some
                    | EntryDecoration.OutputTopology t -> GLSLOutputTopology t |> Some
                    | EntryDecoration.OutputVertices t -> GLSLMaxVertices t |> Some
                    | EntryDecoration.Stages _ -> None
            )

        let dec =
            match stage with
                | ShaderStage.TessEval -> [GLSLSpacing GLSLSpacing.Equal; GLSLWinding GLSLWinding.CCW] @ dec
                | _ -> dec

        match dec with
            | [] -> State.value ()
            | _ -> updateShaderInterface (fun _ s -> { s with shaderDecorations = s.shaderDecorations @ dec })



    let newShader (entry : string) =
        modify (fun s iface ->
            let adjust (o : Option<GLSLShaderInterface>) =
                match o with
                    | Some o -> 
                        Some o
                    | None ->
                        match s.stages.self with
                            | ShaderStage.Fragment ->
                                Some {
                                    emptyShader with 
                                        shaderStage             = ShaderStage.Fragment
                                        shaderEntry             = entry
                                        shaderBuiltIns          = MapExt.ofList [ ParameterKind.Input, MapExt.ofList ["gl_Position", GLSLType.Vec(4, GLSLType.Float 32) ] ]
                                }

                            | stage -> 
                                Some {
                                    emptyShader with 
                                        shaderStage             = stage
                                        shaderEntry             = entry
                                }

            { iface with shaders = MapExt.alter s.stages.self adjust iface.shaders }

        )

    let useBuiltIn (kind : ParameterKind) (name : string) (ctype : CType) =
        modify (fun state iface ->
            let stage = state.stages.self

            let shaders = 
                iface.shaders |> MapExt.alter stage (
                    function 
                    | Some o ->
                        Some { 
                            o with 
                                shaderBuiltIns =
                                    o.shaderBuiltIns |> MapExt.alter kind (fun s ->
                                        let s = s |> Option.defaultValue MapExt.empty
                                        MapExt.add name (GLSLType.ofCType state.config.reverseMatrixLogic ctype) s |> Some
                                    )
                        }
                    | None ->
                        None

                )

            { iface with shaders = shaders }
        )

    let addInput (location : int) (name : string) (parameter : CEntryParameter) =
        modify (fun s iface ->
            
            let ip =
                {
                    paramType           = GLSLType.ofCType s.config.reverseMatrixLogic parameter.cParamType
                    paramLocation       = location
                    paramName           = name
                    paramSemantic       = parameter.cParamSemantic
                    paramInterpolation  = parameter.cParamDecorations |> Seq.tryPick (function ParameterDecoration.Interpolation m -> Some m | _ -> None)
                }
                
            let iface = 
                match s.stages.prev with
                | None -> { iface with  inputs = iface.inputs @ [ip] }
                | Some _ -> iface

            { iface with
                shaders = 
                    iface.shaders 
                    |> MapExt.alter s.stages.self (
                        function 
                        | Some o -> Some { o with shaderInputs = o.shaderInputs @ [ip] } 
                        | None -> None
                    )
            }
        )

    let addOutput (location : int) (name : string) (parameter : CEntryParameter) =
        modify (fun s iface ->
            
            let op =
                {
                    paramType           = GLSLType.ofCType s.config.reverseMatrixLogic parameter.cParamType
                    paramLocation       = location
                    paramName           = name
                    paramSemantic       = parameter.cParamSemantic
                    paramInterpolation  = parameter.cParamDecorations |> Seq.tryPick (function ParameterDecoration.Interpolation m -> Some m | _ -> None)
                }
                
            let iface = 
                match s.stages.next with
                | None -> { iface with outputs = iface.outputs @ [op] }
                | Some _ -> iface

            { iface with
                shaders = 
                    iface.shaders 
                    |> MapExt.alter s.stages.self (
                        function 
                        | Some o -> Some { o with shaderOutputs = o.shaderOutputs @ [op] } 
                        | None -> None
                    )
            }
        )

    let addStorageBuffer (ssb : GLSLStorageBuffer) =
        State.modify (fun (s : AssemblerState) ->
            let iface = 
                { s.ifaceNew with
                    storageBuffers = MapExt.add ssb.ssbName ssb s.ifaceNew.storageBuffers
                    shaders = 
                        s.ifaceNew.shaders 
                        |> MapExt.alter s.stages.self (
                            function 
                            | Some s -> Some { s with shaderStorageBuffers = HashSet.add ssb.ssbName s.shaderStorageBuffers } 
                            | None -> None
                        )
                }
                
            { s with ifaceNew = iface; storageBuffers = MapExt.add ssb.ssbName ssb s.storageBuffers }
        )
 
    let addUniformBuffer (ub : GLSLUniformBuffer) =
        State.modify (fun (s : AssemblerState) ->
            let iface = 
                { s.ifaceNew with
                    uniformBuffers = MapExt.add ub.ubName ub s.ifaceNew.uniformBuffers
                    shaders = 
                        s.ifaceNew.shaders 
                        |> MapExt.alter s.stages.self (
                            function 
                            | Some s -> Some { s with shaderUniformBuffers = HashSet.add ub.ubName s.shaderUniformBuffers } 
                            | None -> None
                        )
                }

            let buffers = ub.ubFields |> List.map (fun f -> f.ufName, ub) |> MapExt.ofList
            { s with ifaceNew = iface; uniformBuffers = MapExt.union s.uniformBuffers buffers }
        )
    
    let addSampler (sampler : GLSLSampler) =
        State.modify (fun (s : AssemblerState) ->
            let sampler = { sampler with samplerTextures = Map.tryFind sampler.samplerName s.textureInfos |> Option.defaultValue [] }
            let iface = 
                { s.ifaceNew with
                    samplers = MapExt.add sampler.samplerName sampler s.ifaceNew.samplers
                    shaders = 
                        s.ifaceNew.shaders 
                        |> MapExt.alter s.stages.self (
                            function 
                            | Some s -> Some { s with shaderSamplers = HashSet.add sampler.samplerName s.shaderSamplers } 
                            | None -> None
                        )
                }
                
            { s with ifaceNew = iface; samplers = MapExt.add sampler.samplerName sampler s.samplers }
        )
 
    let addImage (image : GLSLImage) =
        State.modify (fun (s : AssemblerState) ->
            let iface = 
                { s.ifaceNew with
                    images = MapExt.add image.imageName image s.ifaceNew.images
                    shaders = 
                        s.ifaceNew.shaders 
                        |> MapExt.alter s.stages.self (
                            function 
                            | Some s -> Some { s with shaderImages = HashSet.add image.imageName s.shaderImages } 
                            | None -> None
                        )
                }
                
            { s with ifaceNew = iface; images = MapExt.add image.imageName image s.images }
        )
       
    let newFunction (f : CFunctionSignature) =
        State.modify (fun s -> { s with currentFunction = Some f })
        
    let endFunction =
        State.modify (fun s -> { s with currentFunction = None })

    let callBuiltIn (name : string) (args : CType[]) (ret : CType) =
        updateShaderInterface (fun s iface ->
            let f =
                {
                    name = name
                    args = args |> Array.map (GLSLType.ofCType s.config.reverseMatrixLogic)
                    ret = ret |> GLSLType.ofCType s.config.reverseMatrixLogic
                }
            { iface with shaderBuiltInFunctions = HashSet.add f iface.shaderBuiltInFunctions}
        )

    let callFunction (f : CFunctionSignature) =
        updateShaderInterface (fun s iface ->
            match HashMap.tryFind f s.functionInfo with
                | Some info ->
                    { iface with
                        shaderUniformBuffers = HashSet.union iface.shaderUniformBuffers info.shaderUniformBuffers
                        shaderStorageBuffers = HashSet.union iface.shaderStorageBuffers info.shaderStorageBuffers
                        shaderSamplers = HashSet.union iface.shaderSamplers info.shaderSamplers
                        shaderImages = HashSet.union iface.shaderImages info.shaderImages
                        shaderBuiltInFunctions = HashSet.union iface.shaderBuiltInFunctions info.shaderBuiltInFunctions
                    }
                | None ->
                    iface
        )
     

    let useUniform (name : string) =
        updateShaderInterface (fun s shader ->
            let uniform = 
                MapExt.tryFind name s.uniformBuffers 
                |> Option.map (fun v s -> { s with shaderUniformBuffers = HashSet.add v.ubName s.shaderUniformBuffers })

            let storage =
                MapExt.tryFind name s.storageBuffers 
                |> Option.map (fun v s -> { s with shaderStorageBuffers = HashSet.add v.ssbName s.shaderStorageBuffers })

            let sampler = 
                MapExt.tryFind name s.samplers 
                |> Option.map (fun v s -> { s with shaderSamplers = HashSet.add v.samplerName s.shaderSamplers })

            let image =
                MapExt.tryFind name s.images 
                |> Option.map (fun v s -> { s with shaderImages = HashSet.add v.imageName s.shaderImages })
                
            let all =
                [
                    match uniform with | Some u -> yield u | None -> ()
                    match storage with | Some u -> yield u | None -> ()
                    match sampler with | Some u -> yield u | None -> ()
                    match image with | Some u -> yield u | None -> ()
                ]
            all |> List.fold (fun s v -> v s) shader
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
                        
                        | _, { self = ShaderStage.RayHitShader }            -> return checkName name

                        | ParameterKind.Input, { prev = None }              -> return checkName name

                        | ParameterKind.Input, { self = s }                 -> return prefixes.[s] + name |> glslName
                        | ParameterKind.Output, { next = Some n }           -> return prefixes.[n] + name |> glslName
                        | ParameterKind.Output, { next = None }             -> 
                            let name = name + "Out"
                            return checkName name

                        | _                                                 -> 
                            return checkName name
        }

    let rec assembleType (rev : bool) (t : CType) =
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
            | CType.CMatrix(CType.CFloat(32 | 64), r,c) -> 
                if rev then "mat" + string r + "x" + string c |> Identifier
                else "mat" + string c + "x" + string r |> Identifier

            | CType.CArray(t, l)                        -> (assembleType rev t).Name + "[" + string l + "]" |> Identifier
            | CType.CStruct(n,_,_)                      -> glslName n

            | CType.CPointer(_, t)                      -> (assembleType rev t).Name |> sprintf "%s[]" |> Identifier

            | CType.CIntrinsic it                       -> it.intrinsicTypeName |> Identifier

            | _ -> failwithf "[GLSL] cannot assemble type %A" t 

    let assembleDeclaration (rev : bool) (t : CType) (name : Identifier) =
        match t with
            | CArray(et, len) ->
                sprintf "%s %s[%d]" (assembleType rev et).Name name.Name len

            | CPointer(_,et) ->
                sprintf "%s %s[]" (assembleType rev et).Name name.Name

            | t ->
                sprintf "%s %s" (assembleType rev t).Name name.Name
        

    let assembleParameter (rev : bool) (p : CParameter) =
        let modifier =
            match p.modifier with
                | CParameterModifier.In -> ""
                | CParameterModifier.ByRef -> "inout "
                | CParameterModifier.Out -> "out "

        let decl = assembleDeclaration rev p.ctype (glslName p.name)
        sprintf "%s%s" modifier decl

    let assembleFunctionSignature (rev : bool) (s : CFunctionSignature) =
        let ret = s.returnType |> assembleType rev
        let args = s.parameters |> Seq.map (assembleParameter rev) |> String.concat ", "
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
            let! config = AssemblerState.config
            match e with
                | CVar v ->
                    let name = glslName v.name
                    return name.Name

                | CValue(_, v) ->
                    return assembleLiteral v

                | CCall(func, args) ->
                    do! Interface.callFunction func
                    let name = glslName func.name
                    let! args = args |> assembleExprsS ", "
                    return sprintf "%s(%s)" name.Name args

                | CReadInput(kind, _, name, index) ->
                    let! name = parameterNameS kind name
                    match kind with
                        | ParameterKind.Uniform -> do! Interface.useUniform name.Name
                        | _ -> ()

                    match index with
                        | Some index ->
                            let! index = assembleExprS index
                            return sprintf "%s[%s]" name.Name index
                        | None ->
                            return name.Name

                | CCallIntrinsic(ret, func, args) ->
                    match func.additional with
                        | null -> ()
                        | :? Set<string> as exts -> do! State.modify (fun s -> { s with requiredExtensions = Set.union s.requiredExtensions exts})
                        | _ -> ()

                    match func.tag with
                        | null ->
                            do! Interface.callBuiltIn func.intrinsicName (args |> Array.map (fun e -> e.ctype)) ret
                            let! args = args |> assembleExprsS ", "
                            return sprintf "%s(%s)" func.intrinsicName args

                        | :? string as format ->
                            let name =
                                let i = format.IndexOf("(")
                                if i >= 0 then format.Substring(0,i)
                                else format

                            do! Interface.callBuiltIn name (args |> Array.map (fun e -> e.ctype)) ret

                            let! args = args |> Array.mapS (assembleExprS >> State.map (fun a -> a :> obj))
                            return 
                                try String.Format(format, args)
                                with _ -> failwithf "[FShade] invalid string format: %A (%d args)" format args.Length
                            
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
                            

                | CMulMatMat(_, l, r) | CMulMatVec(_, l, r) | CMulVecMat(_, l, r) ->
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

                | CConvertMatrix(t, m) ->
                    let t = assembleType config.reverseMatrixLogic t
                    let! m = assembleExprS m
                    return sprintf "%s(%s)" t.Name m


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

                | CVecItem(_, v, i) ->
                    let! v = assembleExprS v
                    let! i = assembleExprS i
                    return sprintf "%s[%s]" v i
                    

                | CNewVector(r, _, args) ->
                    let! args = assembleExprsS ", " args
                    let t = assembleType config.reverseMatrixLogic r
                    return sprintf "%s(%s)" t.Name args

                | CVecLength(_, v) ->
                    let! v = assembleExprS v
                    return sprintf "length(%s)" v

                | CConvert(t, v) ->
                    let t = assembleType config.reverseMatrixLogic t
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
                    
                | CBitNot(_,v) ->
                    let! v = assembleExprS v
                    return sprintf "(~%s)" v

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

                | CItem(_, CItem(_, (CReadInput _ as v), i), j) ->
                    let! v = assembleExprS v
                    let! i = assembleExprS i
                    let! j = assembleExprS j
                    return sprintf "%s[%s].data[%s]" v i j
                    

                | CItem(_, v, i) ->
                    let! v = assembleExprS v
                    let! i = assembleExprS i
                    return sprintf "%s[%s]" v i

                | CMatrixFromCols(t,cols) ->
                    let! cols = cols |> List.mapS assembleExprS
                    let! rev = AssemblerState.reverseMatrixLogic
                    let t = assembleType rev t

                    let code = sprintf "%s(%s)" t.Name (String.concat ", " cols)
                    if rev then return sprintf "transpose(%s)" code
                    else return code

                | CMatrixFromRows(t, rows) ->
                    let! rows = rows |> List.mapS assembleExprS
                    let! rev = AssemblerState.reverseMatrixLogic
                    let t = assembleType rev t

                    let code = sprintf "%s(%s)" t.Name (String.concat ", " rows)
                    if rev then return code
                    else return sprintf "transpose(%s)" code

                | CNewMatrix(t, elems) ->
                    match t with
                        | CMatrix(_,rows,cols) ->
                            let! elems = elems |> List.mapS assembleExprS
                            let! rev = AssemblerState.reverseMatrixLogic
                            let t = assembleType rev t

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
                                let t = assembleType rev t
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
                                let t = assembleType rev t
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
            let! config = AssemblerState.config
            match e with
                | CRExpr e -> 
                    return! assembleExprS e

                | CRArray(t, args) ->
                    let et =
                        match t with
                            | CArray(t,_) -> t
                            | CPointer(_,t) -> t
                            | _ -> t

                    let ct = assembleType config.reverseMatrixLogic et
                    let! args = args |> List.mapS assembleExprS |> State.map (String.concat ", ")
                    return sprintf "%s[]( %s )" ct.Name args
        }

    let rec assembleStatementS (singleLine : bool) (s : CStatement) =
        state {
            let! config = AssemblerState.config
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
                    let decl = assembleDeclaration config.reverseMatrixLogic v.ctype name
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

    let private uniformLayout (isUniformBuffer : bool) (decorations : list<UniformDecoration>) (set : int) (binding : int) =

        let decorations =
            decorations |> List.choose (fun d ->
                match d with
                    | UniformDecoration.Format t -> Some t.Name
                    | UniformDecoration.BufferBinding _
                    | UniformDecoration.BufferDescriptorSet _ 
                    | UniformDecoration.FieldIndex _ -> None
            )
            
        let decorations =
            if binding >= 0 then (sprintf "binding = %d" binding) :: decorations
            else decorations

        let decorations =
            if set >= 0 then (sprintf "set = %d" set) :: decorations
            else decorations

        let decorations =
            if isUniformBuffer then "std140" :: decorations
            else decorations

        match decorations with
            | [] -> ""
            | d -> d |> String.concat ", " |> sprintf "layout(%s)\r\n" 



    let assembleUniformsS (uniforms : list<CUniform>) =
        state {
            let! s = State.get
            let! config = AssemblerState.config
            let buffers =
                uniforms 
                    |> List.map (fun u -> match u.cUniformType with | CIntrinsic { tag = (:? GLSLTextureType) } -> { u with cUniformBuffer = None } | _ -> u)
                    |> List.groupBy (fun u -> u.cUniformBuffer)
                    |> List.collect (function (Some a, f) -> [Some a, f] | (None, f) -> f |> List.map (fun f -> None, [f]))

            let allHaveSets =
                buffers |> List.forall (fun (name, fields) ->
                    fields |> List.exists (fun u -> 
                        u.cUniformDecorations |> List.exists (function 
                            | UniformDecoration.BufferDescriptorSet s -> true
                            | _ -> false
                        )
                    )
                )
            let! set = 
                if allHaveSets then State.value 0
                else AssemblerState.newSet

            let! definitions =
                buffers |> List.mapS (fun (name, fields) ->
                    state {
                        let set =
                            let userSet = 
                                fields |> List.tryPick (fun u -> 
                                    u.cUniformDecorations |> List.tryPick (function 
                                        | UniformDecoration.BufferDescriptorSet s -> Some s
                                        | _ -> None
                                    )
                                )
                            match userSet with
                            | Some set -> set
                            | None -> set

                        let getBinding (kind : InputKind) (cnt : int) (fields : list<CUniform>) =
                            let userGiven =     
                                fields |> List.tryPick (fun f ->
                                    f.cUniformDecorations |> List.tryPick (function UniformDecoration.BufferBinding b -> Some b | _ -> None)
                                )
                            match userGiven with
                            | Some u -> State.value u
                            | None -> AssemblerState.newBinding kind cnt
                        
                        match name with
                            | Some "SharedMemory" ->
                                let defs = 
                                    fields |> List.map (fun u ->
                                        let def = assembleDeclaration config.reverseMatrixLogic u.cUniformType (checkName u.cUniformName)
                                        sprintf "shared %s;" def
                                    )
                                return String.concat "\r\n" defs

                            | Some "StorageBuffer" ->
                               
                                let! buffers =
                                    fields |> List.mapS (fun field ->
                                        state {
                                            let! binding = getBinding InputKind.StorageBuffer 1 [field]
                                            let prefix = uniformLayout false [] set binding
                                            let name = checkName field.cUniformName

                                            match field.cUniformType with
                                            | CType.CPointer(_,ct) ->
                                                do! Interface.addStorageBuffer {
                                                    ssbSet = set
                                                    ssbBinding = binding
                                                    ssbName = name.Name
                                                    ssbType = GLSLType.ofCType config.reverseMatrixLogic ct
                                                }
                                                match ct with
                                                | CType.CPointer(_,ct) ->
                                                    let typ = assembleType config.reverseMatrixLogic ct
                                                    return sprintf "%sbuffer %sBuffer { %s[] data; } %s[];" prefix name.Name typ.Name name.Name
                                                | ct ->
                                                    let typ = assembleType config.reverseMatrixLogic ct
                                                    return sprintf "%sbuffer %sBuffer { %s[] %s; };" prefix name.Name typ.Name name.Name
                                            
                                            | ct ->
                                                return failwithf "[GLSL] not a storage buffer type: %A" ct

                                        }
                                    )

                                return buffers |> String.concat "\r\n"
                                
                            | Some bufferName when config.createUniformBuffers ->
                                let bufferName = checkName bufferName
                                let! binding = getBinding InputKind.UniformBuffer 1 fields
                                let prefix = uniformLayout true [] set binding
                            
                                let fieldStr = 
                                    fields |> List.map (fun u -> 
                                        let decl = assembleDeclaration config.reverseMatrixLogic u.cUniformType (checkName u.cUniformName)
                                        sprintf "%s;" decl
                                    ) |> String.concat "\r\n"
                                do! Interface.addUniformBuffer {
                                    ubSet = set
                                    ubBinding = binding
                                    ubName = bufferName.Name
                                    ubFields = fields |> List.map (fun u -> { ufName = checkName(u.cUniformName).Name; ufType = GLSLType.ofCType config.reverseMatrixLogic u.cUniformType; ufOffset = -1 })
                                    ubSize = -1
                                }
                             

                                return sprintf "%suniform %s\r\n{\r\n%s\r\n};\r\n" prefix bufferName.Name (String.indent fieldStr)

                            | _ ->
                                let! definitions = 
                                    fields |> List.mapS (fun u ->
                                        state {
                                            let textureType =
                                                match u.cUniformType with   
                                                    | CIntrinsic { tag = (:? GLSLTextureType as t)} -> Some (t, 1)
                                                    | CArray(CIntrinsic { tag = (:? GLSLTextureType as t)}, len) -> Some (t, len)
                                                    | _ -> None

                                            let mutable prefix = ""

                                            match textureType with   
                                                | Some (t, cnt) ->
                                                    match t with
                                                        | GLSLTextureType.GLSLSampler samplerType -> 
                                                            let! binding = getBinding InputKind.Sampler cnt [u]
                                                            prefix <- uniformLayout false u.cUniformDecorations set binding

                                                            do! Interface.addSampler { 
                                                                samplerSet = set
                                                                samplerBinding = binding
                                                                samplerName = checkName(u.cUniformName).Name
                                                                samplerCount = cnt
                                                                samplerTextures = [] // filled in addSampler
                                                                samplerType = samplerType
                                                            }
                                                           
                                                        | GLSLTextureType.GLSLImage imageType ->
                                                            let! binding = getBinding InputKind.Image cnt [u]
                                                            prefix <- uniformLayout false u.cUniformDecorations set binding

                                                            do! Interface.addImage { 
                                                                imageSet = set
                                                                imageBinding = binding
                                                                imageName = checkName(u.cUniformName).Name
                                                                imageType = imageType
                                                            }
                                                | _ ->
                                                    let! binding = getBinding InputKind.UniformBuffer 1 [u]
                                                    prefix <- uniformLayout false u.cUniformDecorations set binding

                                                    ()

                                            let decl = assembleDeclaration config.reverseMatrixLogic u.cUniformType (checkName u.cUniformName)
                                            return sprintf "%suniform %s;" prefix decl
                                        }
                                    )

                                return definitions |> String.concat "\r\n"
                    }
                )
                
            return String.concat "\r\n\r\n" definitions

                
        }

    let assembleDepthWriteMode (mode : DepthWriteMode) =
        match mode with
            | DepthWriteMode.Equal -> "depth_unchanged"
            | DepthWriteMode.OnlyLess -> "depth_less"
            | DepthWriteMode.OnlyGreater -> "depth_greater"
            | _ -> "depth_any"


    let assembleEntryParameterS (kind : ParameterKind) (p : CEntryParameter) =
        state {
            let depthWrite = p.cParamDecorations |> Seq.tryPick (function ParameterDecoration.DepthWrite m -> Some m | _ -> None) |> Option.defaultValue DepthWriteMode.None
            let! stages = AssemblerState.stages
            let! builtIn = AssemblerState.tryGetParameterName kind p.cParamSemantic
            let prevStage = stages.prev
            let selfStage = stages.self
            let nextStage = stages.next
            
            let! config = AssemblerState.config

            match builtIn with
                | Some name -> 
                    do! Interface.useBuiltIn kind name p.cParamType

                    let interpolation = 
                        if kind = ParameterKind.Input && stages.self = ShaderStage.Fragment then
                            p.cParamDecorations |> Seq.tryPick (function ParameterDecoration.Interpolation i -> Some i | _ -> None)
                        else
                            None

                    if name = "gl_FragDepth" && depthWrite <> DepthWriteMode.None then
                        if config.version >= Version(4,3) then
                            let mode = assembleDepthWriteMode depthWrite
                            return Some (sprintf "layout(%s) out float gl_FragDepth;" mode)
                        else 
                            return None
                    else
                        match interpolation with
                        | Some i ->
                            let mode =
                                match i with
                                | InterpolationMode.Centroid -> Some "centroid"
                                | InterpolationMode.Flat -> Some "flat"
                                | InterpolationMode.NoPerspective -> Some "noperspective"
                                | InterpolationMode.Perspective -> Some "perspective"
                                | InterpolationMode.Sample -> Some "sample"
                                | _ -> None
                            match mode with
                            | Some m ->
                                let t = assembleType config.reverseMatrixLogic p.cParamType
                                return Some (sprintf "%s in %s %s;" m t.Name name)
                            | None ->
                                return None
                        | None -> 
                            return None

                | None ->
                    let! set = 
                        if config.createDescriptorSets then AssemblerState.newSet
                        else State.value -1

                    let mutable bSet = -1
                    let mutable bBinding = -1

                    let! decorations =
                        p.cParamDecorations 
                        |> Set.toList
                        |> List.chooseS (fun d ->
                            state {
                                match d with
                                    | ParameterDecoration.DepthWrite _ ->
                                        return None

                                    | ParameterDecoration.Const -> 
                                        return Some "const"

                                    | ParameterDecoration.Interpolation m ->
                                        
                                        let isFragmentInput =
                                            (selfStage = ShaderStage.Fragment && kind = ParameterKind.Input) ||
                                            (nextStage = Some ShaderStage.Fragment && kind = ParameterKind.Output)
                            
                                        let isTessPatch =
                                            (selfStage = ShaderStage.TessEval && kind = ParameterKind.Input) ||
                                            (selfStage = ShaderStage.TessControl && kind = ParameterKind.Output)

                                        match isTessPatch, isFragmentInput, m with
                                            | _, true, InterpolationMode.Centroid -> return Some "centroid"
                                            | _, true, InterpolationMode.Flat -> return Some "flat"
                                            | _, true, InterpolationMode.NoPerspective -> return Some "noperspective"
                                            | _, true, InterpolationMode.Perspective -> return Some "perspective"
                                            | _, true, InterpolationMode.Sample -> return Some "sample"

                                            | true, _, InterpolationMode.PerPatch -> return Some "patch"

                                            | _ -> return None

                                    | ParameterDecoration.StorageBuffer(read, write) ->
                                        let! binding = AssemblerState.newBinding InputKind.StorageBuffer 1

                                        let args = []

                                        let args =
                                            if set >= 0 then sprintf "set=%d" set :: args
                                            else args

                                        let args =
                                            if binding >= 0 then sprintf "binding=%d" binding :: args
                                            else args

                                        bSet <- set
                                        bBinding <- binding

                                        let args = "std430" :: args |> String.concat ","

                                        let rw = ""
//                                            match read, write with
//                                                | false, true -> " writeonly"
//                                                | true, false -> " readonly"
//                                                | _ -> ""

                                        return Some (sprintf "layout(%s) buffer%s " args rw + (p.cParamSemantic + "_ssb"))

                                    | ParameterDecoration.Shared -> 
                                        return Some "shared"

                                    | ParameterDecoration.Memory _ | ParameterDecoration.Slot _ | ParameterDecoration.Raypayload _ ->
                                        return None
                            }

                        )


                    let isBuffer = p.cParamDecorations |> Seq.exists (function ParameterDecoration.StorageBuffer  _-> true | _ -> false)

                    let slot = p.cParamDecorations |> Seq.tryPick (function ParameterDecoration.Slot s -> Some s | _ -> None)

                    let! location = 
                        match slot with
                            | Some slot -> State.value slot
                            | _ -> AssemblerState.newLocation kind p.cParamType

                    let layoutParams = 
                        match kind with
                            | ParameterKind.Input | ParameterKind.Output when config.createInputLocations && config.version > version120 ->
                                [ sprintf "location = %d" location]
                            | _ ->
                                []

                    let decorations =
                        match layoutParams with
                            | [] -> decorations
                            | _ -> sprintf "layout(%s)" (String.concat ", " layoutParams) :: decorations
                
                    let decorations = 
                        match decorations with
                            | [] -> ""
                            | _ -> String.concat " " decorations + " "

                    let! name = parameterNameS kind p.cParamName

                    let payload =
                        p.cParamDecorations |> Seq.tryPick (function 
                            | ParameterDecoration.Raypayload true -> Some "rayPayloadInNV "
                            | ParameterDecoration.Raypayload false -> Some "rayPayloadNV "
                            | _ -> None
                        )

                    let decorations, prefix, suffix =
                        if Option.isSome payload then
                            decorations, payload.Value, ""
                        elif stages.self = ShaderStage.RayHitShader && name.Name = Intrinsics.HitCoord then
                            match kind with
                            | ParameterKind.Input -> "", "hitAttributeNV ", ""
                            | _ -> failwith "bad ray payload"
                        elif stages.self = ShaderStage.RayHitShader && name.Name = Intrinsics.RayPayloadIn then
                            match kind with
                            | ParameterKind.Input -> decorations, "rayPayloadInNV ", ""
                            | _ -> failwith "bad ray payload"
                            
                        elif stages.self = ShaderStage.RayHitShader && name.Name = Intrinsics.RayPayloadOut then
                            match kind with
                            | ParameterKind.Output -> decorations, "rayPayloadNV ", ""
                            | _ -> failwith "bad ray payload"

                        elif config.version > version120 then
                            match kind with
                                | ParameterKind.Input -> decorations, "in ", ""
                                | ParameterKind.Output -> decorations, "out ", ""
                                | _ -> 
                                    if isBuffer then decorations, " { ", " };"
                                    else decorations, "", ""
                        else
                            match kind with
                                | ParameterKind.Input -> decorations, "varying ", ""
                                | ParameterKind.Output -> decorations, "varying ", ""
                                | _ -> decorations, "", ""
                    

                    if isBuffer then
                        match p.cParamType with
                            | CType.CPointer(_,ct) -> 
                                do! Interface.addStorageBuffer {
                                    ssbSet = bSet
                                    ssbBinding = bBinding
                                    ssbName = p.cParamName
                                    ssbType = GLSLType.ofCType config.reverseMatrixLogic ct
                                }
                            | _ ->
                                failwithf "[GLSL] not a storage buffer type: %A" p.cParamType
                       


                    if kind = ParameterKind.Input then
                        do! Interface.addInput location name.Name p


                    if kind = ParameterKind.Output then
                        do! Interface.addOutput location name.Name p


                    match p.cParamType with
                        | CArray(t,l) ->
                            return sprintf "%s%s%s %s[%d];%s" decorations prefix (assembleType config.reverseMatrixLogic t).Name name.Name l suffix |> Some
                        | CPointer(_, t) ->
                            return sprintf "%s%s%s %s[];%s" decorations prefix (assembleType config.reverseMatrixLogic t).Name name.Name suffix |> Some
                        | _ -> 
                            return sprintf "%s%s%s %s;%s" decorations prefix (assembleType config.reverseMatrixLogic p.cParamType).Name name.Name suffix |> Some
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


            let entryName = checkName e.cEntryName

            do! State.modify (fun s ->
                { s with
                    AssemblerState.stages = stages
                    AssemblerState.currentInputLocation = 0
                    AssemblerState.currentOutputLocation = 0
                }
            )
            do! Interface.newShader entryName.Name 
            do! Interface.addDecorations stages.self e.cDecorations

            
            let prefix = 
                match stages.self with
                    | ShaderStage.Geometry -> 
                        let inputTopology = e.cDecorations |> List.tryPick (function EntryDecoration.InputTopology t -> Some t | _ -> None) |> Option.get
                        let outputTopology = e.cDecorations |> List.tryPick (function EntryDecoration.OutputTopology(t) -> Some(t) | _ -> None) |> Option.get
                        let vertexCount = e.cDecorations |> List.tryPick (function EntryDecoration.OutputVertices(t) -> Some(t) | _ -> None) |> Option.get
                        let invocations = e.cDecorations |> List.tryPick (function EntryDecoration.Invocations i -> Some i | _ -> None) |> Option.defaultValue 1

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
                            yield sprintf "layout(%s) in;" inputPrimitive
                            yield sprintf "layout(%s, max_vertices = %d) out;" outputPrimitive vertexCount
                            if invocations > 1 then
                                yield sprintf "layout(invocations = %d) in;" invocations
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
            let! config = AssemblerState.config
            
            return 
                String.concat "\r\n" [
                    yield! prefix
                    yield! inputs
                    yield! outputs
                    yield! args
                    yield sprintf "%s %s()\r\n{\r\n%s\r\n}" (assembleType config.reverseMatrixLogic e.cReturnType).Name entryName.Name (String.indent body)
                ]
        }

    let rec assembleValueDefS (d : CValueDef) =
        state {
            let! config = AssemblerState.config
            match d with
                | CConditionalDef(d, inner) ->
                    let! inner = inner |> List.mapS assembleValueDefS
                    let inner = inner |> String.concat "\r\n"
                    return sprintf "\r\n#ifdef %s\r\n%s\r\n\r\n#endif\r\n" d inner

                | CEntryDef e -> 
                    return! assembleEntryS e

                | CFunctionDef(signature, body) ->
                    do! Interface.newFunction signature
                    let signature = assembleFunctionSignature config.reverseMatrixLogic signature
                    let! body = assembleStatementS false body
                    do! Interface.endFunction
                    return sprintf "%s\r\n{\r\n%s\r\n}\r\n" signature (String.indent body)

                | CConstant(t, n, init) ->
                    let! init = assembleRExprS init
                    let n = glslName n
                    match t with
                        | CArray(t,l) -> return sprintf "const %s %s[%d] = %s;" (assembleType config.reverseMatrixLogic t).Name n.Name l init
                        | CPointer(_,t) -> return sprintf "const %s %s[] = %s;" (assembleType config.reverseMatrixLogic t).Name n.Name init
                        | _ -> return sprintf "const %s %s = %s;" (assembleType config.reverseMatrixLogic t).Name n.Name init

                | CUniformDef us ->
                    let! config = AssemblerState.config
                    if config.createPerStageUniforms then
                        return! assembleUniformsS us
                    else
                        return ""
        }

    let assembleTypeDef (rev : bool) (d : CTypeDef) =
        match d with
            | CStructDef(name, fields) ->
                let fields = fields |> List.map (fun (t, n) -> sprintf "%s %s;" (assembleType rev t).Name (glslName n).Name) |> String.concat "\r\n"
                sprintf "struct %s\r\n{\r\n%s\r\n};" (glslName name).Name (String.indent fields)
    
    module private Reflection =
        open System.Reflection
        open Aardvark.Base.IL

        let setShaderParent : GLSLShaderInterface -> GLSLProgramInterface -> unit =
            let tShader = typeof<GLSLShaderInterface>
            let fParent = tShader.GetField("program@", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
            if isNull fParent then
                failwith "[FShade] internal error: cannot get parent field for GLSLShaderInterface"
            cil {
                do! IL.ldarg 0
                do! IL.ldarg 1
                do! IL.stfld fParent
                do! IL.ret
            }




    let assemble (backend : Backend) (m : CModule) =
        
        let c = backend.Config
        let definitions =
            state {
                
                let types = m.types |> List.map (assembleTypeDef c.reverseMatrixLogic)

                let! uniforms = 
                    if not c.createPerStageUniforms then assembleUniformsS m.uniforms |> State.map List.singleton
                    else State.value []

                let! values = m.values |> List.mapS assembleValueDefS

                let! s = State.get
                let extensions = Set.union c.enabledExtensions s.requiredExtensions

                let extensions =
                    if ShaderStage.isRayTracing s.stages.self then 
                        extensions
                        |> Set.add "GL_NV_ray_tracing"
                        |> Set.add "GL_EXT_nonuniform_qualifier"
                        |> Set.remove "GL_ARB_tessellation_shader"
                        |> Set.remove "GL_ARB_shading_language_420pack"
                        |> Set.remove "GL_ARB_separate_shader_objects"
                    else 
                        extensions

                let version =
                    if ShaderStage.isRayTracing s.stages.self then "#version 460"
                    else sprintf "#version %d%d0" c.version.Major c.version.Minor

                return 
                    List.concat [
                        [ version ]
                        [ extensions |> Seq.map (sprintf "#extension %s : enable") |> Seq.toList |> String.concat "\r\n" ]
                        types
                        uniforms
                        values
                    ]
            }
        
        

        let mutable state = AssemblerState.ofConfig c

        match m.cuserData with  
            | :? Effect as e ->
                state <- 
                    { state with 
                        textureInfos =
                            e.Uniforms |> Map.choose (fun name p ->
                                match p.uniformValue with
                                    | UniformValue.Sampler(name, s) -> Some [name, s]
                                    | UniformValue.SamplerArray arr -> Some (Array.toList arr)
                                    | _ -> None
                            )
                    }

            | :? ComputeShader as c ->
                let res = 
                    c.csSamplerStates |> Map.toList |> List.map (fun ((samName,index), state) ->
                        let texName =
                            match Map.tryFind (samName,index) c.csTextureNames with
                                | Some name -> name
                                | None -> samName
                                
                        (samName, (index, texName, state))
                    )
                    |> List.groupBy fst
                    |> List.map (fun (samName, elements) ->
                        let elems = 
                            elements
                            |> List.sortBy (fun (_,(i,_,_)) -> i)
                            |> List.map (fun (_,(_,n,s)) -> n,s)
                        samName, elems
                    )
                    |> Map.ofList

                state <- { state with textureInfos = res }
                
            | _ ->
                ()

        let code = definitions.Run(&state) |> String.concat "\r\n\r\n"
        let iface = LayoutStd140.apply state.ifaceNew

        // unsafely mutate the shader's parent
        for (_,shader) in MapExt.toSeq iface.shaders do
            Reflection.setShaderParent shader iface
        
        { code = code; iface = iface }
