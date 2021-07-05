﻿namespace FShade.GLSL

open Aardvark.Base
open FShade.Imperative
open FShade
open System
open FSharp.Data.Adaptive

type GLSLType =
    | Bool
    | Void
    | Int of signed : bool * width : int
    | Float of width : int
    | Vec of dim : int * elem : GLSLType
    | Mat of cols : int * rows : int * elem : GLSLType
    | Struct of name : string * fields : list<string * GLSLType * int> * size : int
    | Array of len : int * elem : GLSLType * stride : int
    | Image of GLSLImageType
    | Sampler of GLSLSamplerType
    | DynamicArray of elem : GLSLType * stride : int
    | Intrinsic of string

and GLSLImageType =
    {
        original    : System.Type
        format      : Option<ImageFormat>
        dimension   : SamplerDimension
        isArray     : bool
        isMS        : bool
        valueType   : GLSLType
    }

and GLSLSamplerType =
    {
        original    : System.Type
        dimension   : SamplerDimension
        isShadow    : bool
        isArray     : bool
        isMS        : bool
        valueType   : GLSLType
    }

and GLSLTextureType =
    | GLSLImage of GLSLImageType
    | GLSLSampler of GLSLSamplerType


type GLSLParameter =
    {
        paramType           : GLSLType
        paramName           : string
        paramLocation       : int
        paramSemantic       : string
        paramInterpolation  : Option<InterpolationMode>
    }

type GLSLIntrinsic =
    {
        name    : string
        args    : GLSLType[]
        ret     : GLSLType
    }

type GLSLSampler =
    {
        samplerSet      : int
        samplerBinding  : int
        samplerName     : string
        samplerCount    : int
        samplerTextures : list<string * SamplerState>
        samplerType     : GLSLSamplerType
    }

type GLSLImage =
    {
        imageSet        : int
        imageBinding    : int
        imageName       : string
        imageType       : GLSLImageType
    }

type GLSLStorageBuffer =
    {
        ssbSet          : int
        ssbBinding      : int
        ssbName         : string
        ssbType         : GLSLType
    }

type GLSLUniformBufferField =
    {
        ufName      : string
        ufType      : GLSLType
        ufOffset    : int
    }

type GLSLUniformBuffer =
    {
        ubSet           : int
        ubBinding       : int
        ubName          : string
        ubFields        : list<GLSLUniformBufferField>
        ubSize          : int
    }

type GLSLAccelerationStructure =
    {
        accelSet        : int
        accelBinding    : int
        accelName       : string
    }

type GLSLWinding =
    | CCW
    | CW

type GLSLSpacing =
    | Equal
    | FractionalEven
    | FractionalOdd

type GLSLShaderDecoration =
    | GLSLInvocations of int
    | GLSLInputTopology of InputTopology
    | GLSLOutputTopology of OutputTopology
    | GLSLMaxVertices of int
    | GLSLSpacing of GLSLSpacing
    | GLSLWinding of GLSLWinding
    | GLSLLocalSize of V3i

[<AutoOpen>]
module private Tools =
    let lines (str : string) =
        str.Split([| "\r\n" |], StringSplitOptions.None) :> seq<_>
             

    let many (entries : list<Option<string>>) =
        entries |> Seq.choose id |> String.concat "\r\n"

    let section (name : string) (entries : list<Option<string>>) =
        let entries = entries |> List.choose id
        match entries with
            | [] -> None
            | _ -> name + ":\r\n" + (entries |> Seq.collect lines |> Seq.map (fun v -> "    " + v) |> String.concat "\r\n") |> Some

    module GLSLType =

        let private samplerName (t : GLSLSamplerType) =
            let dimStr =
                match t.dimension with
                    | SamplerDimension.Sampler1d -> "1D"
                    | SamplerDimension.Sampler2d -> "2D"
                    | SamplerDimension.Sampler3d -> "3D"
                    | SamplerDimension.SamplerCube -> "Cube"
                    | _ -> failwith "unsupported sampler dimension"

            let shadowSuffix = if t.isShadow then "Shadow" else ""
            let msSuffix = if t.isMS then "MS" else ""
            let typePrefix = 
                match t.valueType with
                    | Vec(_,Int _) -> "i"
                    | _ -> ""
                    
            if t.isArray then sprintf "%ssampler%s%sArray%s" typePrefix dimStr msSuffix shadowSuffix
            else sprintf "%ssampler%s%s%s" typePrefix dimStr msSuffix shadowSuffix 
                    
        let private imageName (t : GLSLImageType) =
            let dimStr =
                match t.dimension with
                    | SamplerDimension.Sampler1d -> "1D"
                    | SamplerDimension.Sampler2d -> "2D"
                    | SamplerDimension.Sampler3d -> "3D"
                    | SamplerDimension.SamplerCube -> "Cube"
                    | _ -> failwith "unsupported sampler dimension"

            let msSuffix = if t.isMS then "MS" else ""
            let typePrefix = 
                match t.valueType with
                    | Vec(_,Int _) -> "i"
                    | _ -> ""

            let fmt = 
                match t.format with
                    | Some fmt -> "<" + string fmt + ">"
                    | None -> ""

                    
            if t.isArray then sprintf "%simage%s%sArray%s" typePrefix dimStr msSuffix fmt
            else sprintf "%simage%s%s%s" typePrefix dimStr msSuffix fmt

        let rec toString (t : GLSLType) =
            match t with
                | GLSLType.Intrinsic n -> n
                | GLSLType.Bool -> "bool"
                | GLSLType.Void -> "void" 

                | GLSLType.Int(true, 8) -> "sbyte"
                | GLSLType.Int(true, 16) -> "short"
                | GLSLType.Int(true, 32) -> "int"
                | GLSLType.Int(true, 64) -> "long"
                | GLSLType.Int(true, b) -> sprintf "int%d" b
                
                | GLSLType.Int(false, 8) -> "byte"
                | GLSLType.Int(false, 16) -> "ushort"
                | GLSLType.Int(false, 32) -> "uint"
                | GLSLType.Int(false, 64) -> "ulong"
                | GLSLType.Int(false, b) -> sprintf "uint%d" b
                
                | GLSLType.Float 16 -> "half"
                | GLSLType.Float (32 | 64) -> "float"
                | GLSLType.Float b -> sprintf "float%d" b

                | GLSLType.Vec(dim, elem) -> sprintf "%s%d" (toString elem) dim
                | GLSLType.Mat(c, r, elem) -> sprintf "%s%dx%d" (toString elem) c r

                | GLSLType.Struct(name,_,_) -> name
                | GLSLType.Array(len, elem,_) -> sprintf "%s[%d]" (toString elem) len
                | GLSLType.Image img -> imageName img
                | GLSLType.Sampler sam -> samplerName sam
                | GLSLType.DynamicArray(elem,_) -> sprintf "%s[]" (toString elem)

[<CustomEquality; NoComparison>]
type GLSLShaderInterface =
    {
        program                      : GLSLProgramInterface
        shaderStage                  : ShaderStage
        shaderEntry                  : string
        shaderInputs                 : list<GLSLParameter>
        shaderOutputs                : list<GLSLParameter>
        shaderSamplers               : HashSet<string>
        shaderImages                 : HashSet<string>
        shaderStorageBuffers         : HashSet<string>
        shaderUniformBuffers         : HashSet<string>
        shaderAccelerationStructures : HashSet<string>
        shaderBuiltInFunctions       : HashSet<GLSLIntrinsic>
        shaderDecorations            : list<GLSLShaderDecoration>
        shaderBuiltIns               : MapExt<ParameterKind, MapExt<string, GLSLType>>
    }

    override x.GetHashCode() =
        HashCode.Combine(
            x.shaderStage.GetHashCode(),
            x.shaderEntry.GetHashCode(),
            x.shaderInputs.GetHashCode(),
            x.shaderOutputs.GetHashCode(),
            x.shaderSamplers.GetHashCode(),
            x.shaderImages.GetHashCode(),
            x.shaderStorageBuffers.GetHashCode(),
            x.shaderUniformBuffers.GetHashCode(),
            x.shaderAccelerationStructures.GetHashCode(),
            x.shaderBuiltInFunctions.GetHashCode(),
            x.shaderDecorations.GetHashCode(),
            x.shaderBuiltIns.GetHashCode()
        )
    override x.Equals(o) =
        match o with
            | :? GLSLShaderInterface as o ->
                x.shaderStage = o.shaderStage &&
                x.shaderEntry = o.shaderEntry &&
                x.shaderInputs = o.shaderInputs &&
                x.shaderOutputs = o.shaderOutputs &&
                x.shaderSamplers = o.shaderSamplers &&
                x.shaderImages = o.shaderImages &&
                x.shaderStorageBuffers = o.shaderStorageBuffers &&
                x.shaderUniformBuffers = o.shaderUniformBuffers &&
                x.shaderAccelerationStructures = o.shaderAccelerationStructures &&
                x.shaderBuiltInFunctions = o.shaderBuiltInFunctions &&
                x.shaderDecorations = o.shaderDecorations &&
                x.shaderBuiltIns = o.shaderBuiltIns
            | _ ->
                false

    member x.shaderBuiltInInputs = MapExt.tryFind ParameterKind.Input x.shaderBuiltIns |> Option.defaultValue MapExt.empty
    member x.shaderBuiltInOutputs = MapExt.tryFind ParameterKind.Output x.shaderBuiltIns |> Option.defaultValue MapExt.empty

    override x.ToString() =
        many [
            yield section (string x.shaderStage) [
                        
                yield sprintf "entry: \"%s\"" x.shaderEntry |> Some

                match x.shaderDecorations with
                    | [] -> ()
                    | dec ->
                        yield "attributes: {" |> Some
                        for d in dec do
                            match d with
                                | GLSLMaxVertices d -> yield sprintf "    max-vertices: %d" d |> Some
                                | GLSLInputTopology d -> yield sprintf "    input-top: %A" d |> Some
                                | GLSLOutputTopology d -> yield sprintf "    output-top: %A" d |> Some
                                | GLSLInvocations d -> yield sprintf "    invocations: %A" d |> Some
                                | GLSLLocalSize d -> yield sprintf "    local-size: %dx%dx%d" d.X d.Y d.Z |> Some
                                | GLSLSpacing d -> yield sprintf "    spacing: %A" d |> Some
                                | GLSLWinding d -> yield sprintf "    winding: %A" d |> Some
                        yield "}" |> Some


                let usedUniforms =
                    Seq.concat [
                        x.shaderAccelerationStructures |> Seq.map (sprintf "acc::%s")
                        x.shaderUniformBuffers |> Seq.map (sprintf "ub::%s")
                        x.shaderStorageBuffers |> Seq.map (sprintf "ssb::%s")
                        x.shaderSamplers |> Seq.map (sprintf "sam::%s")
                        x.shaderImages |> Seq.map (sprintf "img::%s")
                    ]

                yield sprintf "uniform {%s}" (String.concat ", " usedUniforms) |> Some

                let called = 
                    x.shaderBuiltInFunctions |> HashSet.toList |> List.map (fun f ->
                        let args = 
                            match f.args.Length with
                                | 0 -> [| GLSLType.Void |]
                                | _ -> f.args

                        sprintf "%s : %s -> %s" f.name (args |> Seq.map GLSLType.toString |> String.concat " -> ") (GLSLType.toString f.ret)
                    )

                match called with
                    | [] -> yield "called {}" |> Some
                    | called ->
                        yield "called {" |> Some
                        for c in called do
                            yield "    " + c |> Some
                        yield "}" |> Some
                        
                for i in x.shaderInputs do
                    yield sprintf "in %s : %s // location: %d semantic: %s" i.paramName (GLSLType.toString i.paramType) i.paramLocation i.paramSemantic |> Some
                
                for (name, typ) in MapExt.toSeq x.shaderBuiltInInputs do
                    yield sprintf "in %s : %s " name (GLSLType.toString typ) |> Some
                     
                     
                for i in x.shaderOutputs do
                    yield sprintf "out %s : %s // location: %d semantic: %s" i.paramName (GLSLType.toString i.paramType) i.paramLocation i.paramSemantic |> Some
                
                for (name, typ) in MapExt.toSeq x.shaderBuiltInOutputs do
                    yield sprintf "out %s : %s " name (GLSLType.toString typ) |> Some
            ]
        ]
                       

and [<RequireQualifiedAccess>] GLSLProgramShaders =
    | Compute    of GLSLShaderInterface
    | Graphics   of GLSLGraphicsShaders
    | Raytracing of GLSLRaytracingShaders

    member x.Slots =
        match x with
        | Compute c -> MapExt.ofList [ ShaderSlot.Compute, c]
        | Graphics g -> g.Slots
        | Raytracing r -> r.Slots

    member x.Item(slot : ShaderSlot) =
        x.Slots.[slot]

    override x.ToString() =
        match x with
        | Compute c -> c.ToString()
        | Graphics g -> g.ToString()
        | Raytracing r -> r.ToString()

and GLSLGraphicsShaders =
    {
        stages : MapExt<ShaderStage, GLSLShaderInterface>
    }

    member x.firstShader = x.stages.[MapExt.min x.stages]
    member x.lastShader = x.stages.[MapExt.max x.stages]

    member x.Slots =
        x.stages |> MapExt.mapMonotonic (fun stage shader ->
            let slot =
                match stage with
                | ShaderStage.Vertex ->      ShaderSlot.Vertex
                | ShaderStage.Fragment ->    ShaderSlot.Fragment
                | ShaderStage.Geometry ->    ShaderSlot.Geometry
                | ShaderStage.TessControl -> ShaderSlot.TessControl
                | ShaderStage.TessEval ->    ShaderSlot.TessEval
                | _ -> failwithf "unsupported shader stage: %A" stage

            slot, shader
        )

    override x.ToString() =
        many [
            for (name, typ) in MapExt.toSeq x.firstShader.shaderBuiltInInputs do
                yield sprintf "in %s : %s " name (GLSLType.toString typ) |> Some

            for (name, typ) in MapExt.toSeq x.lastShader.shaderBuiltInOutputs do
                yield sprintf "out %s : %s " name (GLSLType.toString typ) |> Some

            for (_, shader) in MapExt.toSeq x.stages do
                yield Some (shader.ToString())
        ]

and GLSLRaytracingShaders =
    {
        raygenShader    : Option<GLSLShaderInterface>
        missShaders     : MapExt<Symbol, GLSLShaderInterface>
        callableShaders : MapExt<Symbol, GLSLShaderInterface>
        hitgroups       : MapExt<Symbol, MapExt<Symbol, GLSLRayHitGroup>>
    }

    member x.Slots =
        let inline ofOption slot =
            Option.map (fun s -> slot, s) >> Option.toList

        MapExt.ofList [
            yield! x.raygenShader |> ofOption ShaderSlot.RayGeneration

            for (n, s) in MapExt.toList x.missShaders do
                yield ShaderSlot.Miss n, s

            for (n, s) in MapExt.toList x.callableShaders do
                yield ShaderSlot.Callable n, s

            for (n, perRay) in MapExt.toList x.hitgroups do
                for (r, g) in MapExt.toList perRay do
                    yield! g.anyHitShader |> ofOption (ShaderSlot.AnyHit (n, r))
                    yield! g.closestHitShader |> ofOption (ShaderSlot.ClosestHit (n, r))
                    yield! g.intersectionShader |> ofOption (ShaderSlot.Intersection (n, r))
        ]

    override x.ToString() =
        many [
            yield x.raygenShader |> Option.map string

            for (name, shader) in MapExt.toSeq x.missShaders do
                yield section (sprintf "Miss \"%A\"" name) [
                    yield Some (shader.ToString())
                ]

            for (name, shader) in MapExt.toSeq x.callableShaders do
                yield section (sprintf "Callable \"%A\"" name) [
                    yield Some (shader.ToString())
                ]

            for (name, perRay) in MapExt.toSeq x.hitgroups do
                yield section (sprintf "Hitgroup \"%A\"" name) [
                    for (name, group) in MapExt.toSeq perRay do
                        yield section (sprintf "Ray type \"%A\"" name) [
                            yield Some (group.ToString())
                        ]
                ]
        ]

and GLSLRayHitGroup =
    {
        anyHitShader       : Option<GLSLShaderInterface>
        closestHitShader   : Option<GLSLShaderInterface>
        intersectionShader : Option<GLSLShaderInterface>
    }

    override x.ToString() =
        many [
            yield x.anyHitShader |> Option.map string
            yield x.closestHitShader |> Option.map string
            yield x.intersectionShader |> Option.map string
        ]

and GLSLProgramInterface =
    {
        inputs                  : list<GLSLParameter>
        outputs                 : list<GLSLParameter>
        samplers                : MapExt<string, GLSLSampler>
        images                  : MapExt<string, GLSLImage>
        storageBuffers          : MapExt<string, GLSLStorageBuffer>
        uniformBuffers          : MapExt<string, GLSLUniformBuffer>
        accelerationStructures  : MapExt<string, GLSLAccelerationStructure>
        shaders                 : GLSLProgramShaders
    }

    override x.ToString() =
        many [
            for i in x.inputs do
                yield sprintf "in %s : %s // location: %d semantic: %s" i.paramName (GLSLType.toString i.paramType) i.paramLocation i.paramSemantic |> Some

            for i in x.outputs do
                yield sprintf "out %s : %s // location: %d semantic: %s" i.paramName (GLSLType.toString i.paramType) i.paramLocation i.paramSemantic |> Some

            for (_,b) in MapExt.toSeq x.uniformBuffers do
                let name = sprintf "ub %s { // set: %d binding: %d" b.ubName b.ubSet b.ubBinding
                yield Some name
                for f in b.ubFields do
                    yield sprintf "    %s : %s // offset: %d" f.ufName (GLSLType.toString f.ufType) f.ufOffset |> Some
                yield Some "}"

            for (_,s) in MapExt.toSeq x.storageBuffers do
                yield sprintf "ssb %s : %s[] // set: %d binding: %d" s.ssbName (GLSLType.toString s.ssbType) s.ssbSet s.ssbBinding |> Some

            for (_,s) in MapExt.toSeq x.samplers do
                let suffix =
                    if s.samplerCount > 1 then  sprintf "[%d]" s.samplerCount
                    else ""
                yield sprintf "sam %s : %s%s // set: %d binding: %d" s.samplerName (GLSLType.toString (GLSLType.Sampler s.samplerType)) suffix s.samplerSet s.samplerBinding |> Some

            for (_,s) in MapExt.toSeq x.images do
                yield sprintf "img %s : %s // set: %d binding: %d" s.imageName (GLSLType.toString (GLSLType.Image s.imageType)) s.imageSet s.imageBinding |> Some

            for (_,s) in MapExt.toSeq x.accelerationStructures do
                yield sprintf "accel %s // set: %d binding: %d" s.accelName s.accelSet s.accelBinding |> Some

            yield section "shaders" [
                yield Some (x.shaders.ToString())
            ]

        ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GLSLShaderInterface =

    let private discardFun = 
        {
            name = "discard"
            args = [||]
            ret = GLSLType.Void
        }


    let inline program (i : GLSLShaderInterface) = i.program
    let inline stage (i : GLSLShaderInterface) = i.shaderStage
    let inline entry (i : GLSLShaderInterface) = i.shaderEntry
    let inline inputs (i : GLSLShaderInterface) = i.shaderInputs
    let inline outputs (i : GLSLShaderInterface) = i.shaderOutputs
    let inline samplers (i : GLSLShaderInterface) = i.shaderSamplers
    let inline images (i : GLSLShaderInterface) = i.shaderImages
    let inline storageBuffers (i : GLSLShaderInterface) = i.shaderStorageBuffers
    let inline uniformBuffers (i : GLSLShaderInterface) = i.shaderUniformBuffers
    let inline accelerationStructures (i : GLSLShaderInterface) = i.shaderAccelerationStructures
    let inline builtInFunctions (i : GLSLShaderInterface) = i.shaderBuiltInFunctions
    let inline decorations (i : GLSLShaderInterface) = i.shaderDecorations
    let inline builtIns (i : GLSLShaderInterface) = i.shaderBuiltIns
    
    let writesPointSize (iface : GLSLShaderInterface) =
        if iface.shaderStage <> ShaderStage.Fragment then
            MapExt.containsKey "gl_PointSize" iface.shaderBuiltInOutputs
        else
            false


    let usesDiscard (iface : GLSLShaderInterface) =
        if iface.shaderStage = ShaderStage.Fragment then
            HashSet.contains discardFun iface.shaderBuiltInFunctions
        else
            false


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GLSLGraphicsShaders =

    let empty =
        { stages = MapExt.empty }

    let usesDiscard (shaders : GLSLGraphicsShaders) =
        match MapExt.tryFind ShaderStage.Fragment shaders.stages with
        | Some shader -> GLSLShaderInterface.usesDiscard shader
        | None -> false

    let usesPointSize (shaders : GLSLGraphicsShaders) =
        match MapExt.neighboursAt (shaders.stages.Count-1) shaders.stages with
        | Some (_,prev), Some(_, frag), _ when frag.shaderStage = ShaderStage.Fragment ->
            GLSLShaderInterface.writesPointSize prev
        | _ ->
            false

    let alter (mapping : GLSLShaderInterface option -> GLSLShaderInterface option)
              (stage : ShaderStage) (shaders : GLSLGraphicsShaders) =
        { shaders with stages = shaders.stages |> MapExt.alter stage mapping }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GLSLRayHitGroup =

    let empty =
        {
            anyHitShader = None
            closestHitShader = None
            intersectionShader = None
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GLSLRaytracingShaders =

    let empty =
        {
            raygenShader    = None
            missShaders     = MapExt.empty
            callableShaders = MapExt.empty
            hitgroups       = MapExt.empty
        }

    let iter (action : GLSLShaderInterface -> unit) (s : GLSLRaytracingShaders) =
        s.raygenShader |> Option.iter action
        s.missShaders |> MapExt.iter (fun _ x -> action x)
        s.callableShaders |> MapExt.iter (fun _ x -> action x)

        for (_, perRay) in MapExt.toSeq s.hitgroups do
            for (_, group) in MapExt.toSeq perRay do
                group.anyHitShader |> Option.iter action
                group.closestHitShader |> Option.iter action
                group.intersectionShader |> Option.iter action

    let alter (mapping : GLSLShaderInterface option -> GLSLShaderInterface option)
              (stage : RaytracingStageDescription) (s : GLSLRaytracingShaders) =

        let alterHitGroup (mapping : GLSLRayHitGroup -> GLSLRayHitGroup)
                          (name : Symbol) (rayType : Symbol)
                          (groups : MapExt<Symbol, MapExt<Symbol, GLSLRayHitGroup>>) =
            groups |> MapExt.alter name (
                Option.defaultValue MapExt.empty >> MapExt.alter rayType (
                    Option.defaultValue GLSLRayHitGroup.empty >> mapping >> Some
                ) >> Some
            )

        match stage with
        | RaytracingStageDescription.RayGeneration ->
            { s with raygenShader = s.raygenShader |> mapping }

        | RaytracingStageDescription.Miss name ->
            { s with missShaders = s.missShaders |> MapExt.alter name mapping }

        | RaytracingStageDescription.Callable name ->
            { s with callableShaders = s.callableShaders |> MapExt.alter name mapping }

        | RaytracingStageDescription.AnyHit (name, rayType) ->
            let f g = { g with anyHitShader = g.anyHitShader |> mapping }
            { s with hitgroups = s.hitgroups |> alterHitGroup f name rayType }

        | RaytracingStageDescription.ClosestHit (name, rayType) ->
            let f g = { g with closestHitShader = g.closestHitShader |> mapping }
            { s with hitgroups = s.hitgroups |> alterHitGroup f name rayType }

        | RaytracingStageDescription.Intersection (name, rayType) ->
            let f g = { g with intersectionShader = g.intersectionShader |> mapping }
            { s with hitgroups = s.hitgroups |> alterHitGroup f name rayType }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GLSLProgramShaders =

    let getCompute = function
        | GLSLProgramShaders.Compute c -> Some c
        | _ -> None

    let getGraphics = function
        | GLSLProgramShaders.Graphics g -> Some g
        | _ -> None

    let getRaytracing = function
        | GLSLProgramShaders.Raytracing r -> Some r
        | _ -> None

    let iter (action : GLSLShaderInterface -> unit) (shaders : GLSLProgramShaders) =
        match shaders with
        | GLSLProgramShaders.Compute c ->
            action c

        | GLSLProgramShaders.Graphics g ->
            g.stages |> MapExt.iter (fun _ s -> action s)

        | GLSLProgramShaders.Raytracing r ->
            r |> GLSLRaytracingShaders.iter action

    let alter (mapping : GLSLShaderInterface option -> GLSLShaderInterface option)
              (stage : ShaderStageDescription) (shaders : GLSLProgramShaders) =

        match stage with
        | ShaderStageDescription.Compute ->
            getCompute shaders
            |> mapping
            |> Option.get
            |> GLSLProgramShaders.Compute

        | ShaderStageDescription.Graphics s ->
            getGraphics shaders
            |> Option.defaultValue GLSLGraphicsShaders.empty
            |> GLSLGraphicsShaders.alter mapping s.self
            |> GLSLProgramShaders.Graphics

        | ShaderStageDescription.Raytracing s ->
            getRaytracing shaders
            |> Option.defaultValue GLSLRaytracingShaders.empty
            |> GLSLRaytracingShaders.alter mapping s
            |> GLSLProgramShaders.Raytracing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GLSLProgramInterface =

    let inline inputs (i : GLSLProgramInterface) = i.inputs
    let inline outputs (i : GLSLProgramInterface) = i.outputs
    let inline samplers (i : GLSLProgramInterface) = i.samplers
    let inline images (i : GLSLProgramInterface) = i.images
    let inline storageBuffers (i : GLSLProgramInterface) = i.storageBuffers
    let inline uniformBuffers (i : GLSLProgramInterface) = i.uniformBuffers
    let inline accelerationStructures (i : GLSLProgramInterface) = i.accelerationStructures
    let inline shaders (i : GLSLProgramInterface) = i.shaders

    let usesDiscard (iface : GLSLProgramInterface) =
        match iface.shaders with
        | GLSLProgramShaders.Graphics g -> GLSLGraphicsShaders.usesDiscard g
        | _ -> false

    let usesPointSize (iface : GLSLProgramInterface) =
        match iface.shaders with
        | GLSLProgramShaders.Graphics g -> GLSLGraphicsShaders.usesPointSize g
        | _ -> false

    let toString(iface : GLSLProgramInterface) =
        iface.ToString()

    let log (iface : GLSLProgramInterface) =
        let str = iface.ToString()
        for line in lines str do
            Log.line "%s" line

    let print (iface : GLSLProgramInterface) =
        Console.WriteLine("{0}", iface)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GLSLType =
    
    let rec ofCType (rev : bool) (t : CType) =
        match t with    
            | CType.CBool -> GLSLType.Bool
            | CType.CVoid -> GLSLType.Void
            | CType.CInt(signed, width) -> GLSLType.Int(signed, width)

            | CType.CFloat 64 -> GLSLType.Float 32
            | CType.CFloat(width) -> GLSLType.Float(width)

            | CType.CVector(elem, dim) -> GLSLType.Vec(dim, ofCType rev elem)
            | CType.CMatrix(elem, r, c) -> 
                if rev then GLSLType.Mat(r, c, ofCType rev elem)
                else GLSLType.Mat(c, r, ofCType rev elem)

            | CType.CArray(elem, len) -> GLSLType.Array(len, ofCType rev elem, -1)
            | CType.CStruct(name, fields,_) -> GLSLType.Struct(name, fields |> List.map (fun (t, n) -> n, ofCType rev t, -1), -1)

            | CType.CIntrinsic a ->
                match a.tag with
                    | :? GLSLTextureType as t -> 
                        match t with
                            | GLSLImage t -> GLSLType.Image t
                            | GLSLSampler t -> GLSLType.Sampler t
                    | _ ->
                        GLSLType.Intrinsic a.intrinsicTypeName

            | CType.CPointer(_,e) -> GLSLType.DynamicArray (ofCType rev e, -1)


module LayoutStd140 =
    // https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_uniform_buffer_object.txt
    //
    //(1) If the member is a scalar consuming <N> basic machine units, the
    //    base alignment is <N>.
    //
    //(2) If the member is a two- or four-component vector with components
    //    consuming <N> basic machine units, the base alignment is 2<N> or
    //    4<N>, respectively.
    //
    //(3) If the member is a three-component vector with components consuming
    //    <N> basic machine units, the base alignment is 4<N>.
    //
    //(4) If the member is an array of scalars or vectors, the base alignment
    //    and array stride are set to match the base alignment of a single
    //    array element, according to rules (1), (2), and (3), and rounded up
    //    to the base alignment of a vec4. The array may have padding at the
    //    end; the base offset of the member following the array is rounded up
    //    to the next multiple of the base alignment.
    //
    //(5) If the member is a column-major matrix with <C> columns and <R>
    //    rows, the matrix is stored identically to an array of <C> column
    //    vectors with <R> components each, according to rule (4).
    //
    //(6) If the member is an array of <S> column-major matrices with <C>
    //    columns and <R> rows, the matrix is stored identically to a row of
    //    <S>*<C> column vectors with <R> components each, according to rule
    //    (4).
    //
    //(7) If the member is a row-major matrix with <C> columns and <R> rows,
    //    the matrix is stored identically to an array of <R> row vectors
    //    with <C> components each, according to rule (4).
    //
    //(8) If the member is an array of <S> row-major matrices with <C> columns
    //    and <R> rows, the matrix is stored identically to a row of <S>*<R>
    //    row vectors with <C> components each, according to rule (4).
    //
    //(9) If the member is a structure, the base alignment of the structure is
    //    <N>, where <N> is the largest base alignment value of any of its
    //    members, and rounded up to the base alignment of a vec4. The
    //    individual members of this sub-structure are then assigned offsets 
    //    by applying this set of rules recursively, where the base offset of
    //    the first member of the sub-structure is equal to the aligned offset
    //    of the structure. The structure may have padding at the end; the 
    //    base offset of the member following the sub-structure is rounded up
    //    to the next multiple of the base alignment of the structure.
    //
    //(10) If the member is an array of <S> structures, the <S> elements of
    //     the array are laid out in order, according to rule (9).
    let private next (a : int) (v : int) =
        if v % a = 0 then v
        else v + (a - (v % a))

    let rec layout (t : GLSLType) =
        match t with
            | GLSLType.Void ->
                t, 1, 0

            | GLSLType.Bool ->
                t, 4, 4

            | GLSLType.Int(_,w) ->
                let s = w / 8
                t, s, s

            | GLSLType.Float(64) ->
                t, 4, 4
                
            | GLSLType.Float(w) ->
                let s = w / 8 
                t, s, s

            | GLSLType.Vec(3, bt) ->
                let bt, a, s = layout bt
                GLSLType.Vec(3, bt), 4 * a, 3 * s
                
            | GLSLType.Vec(d, bt) ->
                let bt, a, s = layout bt
                GLSLType.Vec(d, bt), d * s, d * s

            | GLSLType.Array(len, bt, _) ->
                let bt, a, s = layout bt

                let s = next 16 s
                let a = next 16 a

                GLSLType.Array(len, bt, s), a, len * s

            | GLSLType.Mat(cols, rows, bt) ->
                let narr, a, s = layout (GLSLType.Array(cols, GLSLType.Vec(rows, bt), -1))
                let bt = 
                    match narr with
                        | GLSLType.Array(_, GLSLType.Vec(_,bt), _) -> bt
                        | _ -> failwith "that was unexpected"
                GLSLType.Mat(cols, rows, bt), a, s

            | GLSLType.Struct(name, fields, _) ->
                let mutable offset = 0
                let mutable largestAlign = 0

                let newFields =
                    fields |> List.map (fun (name, typ,_) ->
                        let (typ, align, size) = layout typ

                        if offset % align <> 0 then
                            offset <- offset + (align - offset % align)

                        largestAlign <- max largestAlign align
                        let res = name, typ, offset
                        offset <- offset + size

                        res

                    )

                let align = next 16 largestAlign
                let size = next 16 offset

                GLSLType.Struct(name, newFields, offset), align, size
                
            | GLSLType.DynamicArray(e,_) ->
                let (e,align,s) = layout e
                let align = next 16 align

                GLSLType.DynamicArray(e, s), align, 0

            | GLSLType.Image i ->
                GLSLType.Image i, 1, 0
                
            | GLSLType.Sampler s ->
                GLSLType.Sampler s, 1, 0
                
            | GLSLType.Intrinsic s ->
                GLSLType.Intrinsic s, 1, 0

    let applyLayout (ub : GLSLUniformBuffer) : GLSLUniformBuffer =
        let mutable offset = 0

        let newFields =
            ub.ubFields |> List.map (fun uf ->
                let nufType, align, size = layout uf.ufType

                if offset % align <> 0 then
                    offset <- offset + (align - offset % align)

                let res = offset
                offset <- offset + size

                { uf with ufOffset = res; ufType = nufType }
            )
            
        let size = next 16 offset
                    
        { ub with ubFields = newFields; ubSize = size }

    let apply (iface : GLSLProgramInterface) =
        // (a,(b,c))
        // ((a,b), (c,d))
        let inline layout a = let (t,_,_) = layout a in t

        let applyToInterface (s : GLSLShaderInterface) =
            { s with
                shaderInputs = s.shaderInputs |> List.map (fun p -> { p with paramType = layout p.paramType})
                shaderOutputs = s.shaderOutputs |> List.map (fun p -> { p with paramType = layout p.paramType}) }

        let applyToGraphics (s : GLSLGraphicsShaders) =
            { s with stages = s.stages |> MapExt.map (fun _ x -> applyToInterface x) }

        let applyToHitgroup (g : GLSLRayHitGroup) =
            { g with
                anyHitShader        = g.anyHitShader |> Option.map applyToInterface
                closestHitShader    = g.closestHitShader |> Option.map applyToInterface
                intersectionShader  = g.intersectionShader |> Option.map applyToInterface }

        let applyToRaytracing (s : GLSLRaytracingShaders) =
            { s with
                raygenShader    = s.raygenShader    |> Option.map applyToInterface
                missShaders     = s.missShaders     |> MapExt.map (fun _ x -> applyToInterface x)
                callableShaders = s.callableShaders |> MapExt.map (fun _ x -> applyToInterface x)
                hitgroups       = s.hitgroups       |> MapExt.map (fun _ pr -> pr |> MapExt.map (fun _ g -> applyToHitgroup g)) }

        { iface with
            storageBuffers = iface.storageBuffers |> MapExt.map (fun _ s -> { s with ssbType = layout s.ssbType })
            uniformBuffers = iface.uniformBuffers |> MapExt.map (fun _ -> applyLayout)
            inputs = iface.inputs |> List.map (fun p -> { p with paramType = layout p.paramType})
            outputs = iface.outputs |> List.map (fun p -> { p with paramType = layout p.paramType})
            shaders =
                match iface.shaders with
                | GLSLProgramShaders.Graphics g   -> GLSLProgramShaders.Graphics   <| applyToGraphics g
                | GLSLProgramShaders.Compute c    -> GLSLProgramShaders.Compute    <| applyToInterface c
                | GLSLProgramShaders.Raytracing r -> GLSLProgramShaders.Raytracing <| applyToRaytracing r
        }




