namespace FShade.GLSL

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
    

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GLSLType =
    open System.IO
    
    [<AutoOpen>]
    module private Helpers =
    
        let imageTypes =
            LookupTable.lookupTable [
                (false, false, false, SamplerDimension.Sampler1d), typedefof<Image1d<_>>
                (false, false, false, SamplerDimension.Sampler2d), typedefof<Image2d<_>>
                (false, false, false, SamplerDimension.Sampler3d), typedefof<Image3d<_>>
                (false, false, false, SamplerDimension.SamplerCube), typedefof<ImageCube<_>>

                (false, true, false, SamplerDimension.Sampler1d), typedefof<Image1dArray<_>>
                (false, true, false, SamplerDimension.Sampler2d), typedefof<Image2dArray<_>>
                (false, true, false, SamplerDimension.SamplerCube), typedefof<ImageCubeArray<_>>

                (false, false, true, SamplerDimension.Sampler2d), typedefof<Image2dMS<_>>
                (false, true, true, SamplerDimension.Sampler2d), typedefof<Image2dArrayMS<_>>

                (true, false, false, SamplerDimension.Sampler1d), typedefof<IntImage1d<_>>
                (true, false, false, SamplerDimension.Sampler2d), typedefof<IntImage2d<_>>
                (true, false, false, SamplerDimension.Sampler3d), typedefof<IntImage3d<_>>
                (true, false, false, SamplerDimension.SamplerCube), typedefof<IntImageCube<_>>

                (true, true, false, SamplerDimension.Sampler1d), typedefof<IntImage1dArray<_>>
                (true, true, false, SamplerDimension.Sampler2d), typedefof<IntImage2dArray<_>>
                (true, true, false, SamplerDimension.SamplerCube), typedefof<IntImageCubeArray<_>>

                (true, false, true, SamplerDimension.Sampler2d), typedefof<IntImage2dMS<_>>
                (true, true, true, SamplerDimension.Sampler2d), typedefof<IntImage2dArrayMS<_>>
            ]

        // integral, isArray, isMS, isShadow
        let samplerTypes =
            LookupTable.lookupTable [
                (false, false, false, false, SamplerDimension.Sampler1d), typeof<Sampler1d>
                (false, false, false, false, SamplerDimension.Sampler2d), typeof<Sampler2d>
                (false, false, false, false, SamplerDimension.Sampler3d), typeof<Sampler3d>
                (false, false, false, false, SamplerDimension.SamplerCube), typeof<SamplerCube>

                (false, false, false, true, SamplerDimension.Sampler1d), typeof<Sampler1dShadow>
                (false, false, false, true, SamplerDimension.Sampler2d), typeof<Sampler2dShadow>
                (false, false, false, true, SamplerDimension.SamplerCube), typeof<SamplerCubeShadow>

                (false, false, true, false, SamplerDimension.Sampler2d), typeof<Sampler2dMS>

                (false, true, false, false, SamplerDimension.Sampler1d), typeof<Sampler1dArray>
                (false, true, false, false, SamplerDimension.Sampler2d), typeof<Sampler2dArray>
                (false, true, false, false, SamplerDimension.SamplerCube), typeof<SamplerCubeArray>

                (false, true, false, true, SamplerDimension.Sampler1d), typeof<Sampler1dArrayShadow>
                (false, true, false, true, SamplerDimension.Sampler2d), typeof<Sampler2dArrayShadow>
                (false, true, false, true, SamplerDimension.SamplerCube), typeof<SamplerCubeArrayShadow>

                (false, true, true, false, SamplerDimension.Sampler2d), typeof<Sampler2dArrayMS>

                (true, false, false, false, SamplerDimension.Sampler1d), typeof<IntSampler1d>
                (true, false, false, false, SamplerDimension.Sampler2d), typeof<IntSampler2d>
                (true, false, false, false, SamplerDimension.Sampler3d), typeof<IntSampler3d>
                (true, false, false, false, SamplerDimension.SamplerCube), typeof<IntSamplerCube>

                (true, false, true, false, SamplerDimension.Sampler2d), typeof<IntSampler2dMS>

                (true, true, false, false, SamplerDimension.Sampler1d), typeof<IntSampler1dArray>
                (true, true, false, false, SamplerDimension.Sampler2d), typeof<IntSampler2dArray>
                (true, true, false, false, SamplerDimension.SamplerCube), typeof<IntSamplerCubeArray>

                (true, true, true, false, SamplerDimension.Sampler2d), typeof<IntSampler2dArrayMS>
            ]
            

        let rec isIntegral (t : GLSLType) =
            match t with
            | GLSLType.Vec(_, v) -> isIntegral v
            | GLSLType.Mat(_,_,v) -> isIntegral v
            | Float _ -> false
            | Int _ -> true
            | _ -> false

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

    let rec internal serializeInternal (dst : BinaryWriter) (t : GLSLType) =
        match t with
        | GLSLType.Bool ->
            dst.Write 0uy
        | GLSLType.Void ->
            dst.Write 1uy
        | GLSLType.Int(signed, bits) ->
            dst.Write 2uy
            dst.Write signed
            dst.Write (byte bits)
        | GLSLType.Float bits ->
            dst.Write 3uy
            dst.Write (byte bits)
        | GLSLType.Vec(dim, elem) ->
            dst.Write 4uy
            dst.Write (byte dim)
            serializeInternal dst elem
        | GLSLType.Mat(c,r,elem) ->
            dst.Write 5uy
            dst.Write (byte c)
            dst.Write (byte r)
            serializeInternal dst elem
        | GLSLType.Struct(name, fields, size) ->
            dst.Write 6uy
            dst.Write name
            dst.Write (List.length fields)
            for (fn, ft, fs) in fields do
                dst.Write(fn)
                serializeInternal dst ft
                dst.Write fs
            dst.Write size
        | GLSLType.Array(len, elem, align) ->
            dst.Write 7uy
            dst.Write len
            serializeInternal dst elem
            dst.Write align
        | GLSLType.Image img ->
            dst.Write 8uy
            dst.Write (int img.dimension)
            match img.format with
            | Some fmt ->
                dst.Write 1uy
                dst.Write (int fmt)
            | None ->
                dst.Write 0uy

            serializeInternal dst img.valueType
            dst.Write img.isArray
            dst.Write img.isMS
        | GLSLType.Sampler sam ->
            dst.Write 9uy
            dst.Write (int sam.dimension)
            serializeInternal dst sam.valueType
            dst.Write sam.isArray
            dst.Write sam.isMS
            dst.Write sam.isShadow
        | GLSLType.DynamicArray(elem, stride) ->
            dst.Write 10uy
            serializeInternal dst elem
            dst.Write stride
        | GLSLType.Intrinsic name ->
            dst.Write 11uy
            dst.Write name

    let rec internal deserializeInternal (src : BinaryReader) =
        match src.ReadByte() with
        | 0uy -> 
            GLSLType.Bool

        | 1uy ->
            GLSLType.Void

        | 2uy ->
            let signed = src.ReadBoolean()
            let bits = src.ReadByte() |> int
            GLSLType.Int(signed, bits)

        | 3uy ->
            let bits = src.ReadByte() |> int
            GLSLType.Float bits

        | 4uy ->
            let dim = src.ReadByte() |> int
            let elem = deserializeInternal src
            GLSLType.Vec(dim, elem)

        | 5uy ->
            let c = src.ReadByte() |> int
            let r = src.ReadByte() |> int
            let elem = deserializeInternal src
            GLSLType.Mat(c, r, elem)

        | 6uy ->
            let name = src.ReadString()
            let cnt = src.ReadInt32()
            let fields =
                List.init cnt (fun _ ->
                    let fn = src.ReadString()
                    let ft = deserializeInternal src
                    let fs = src.ReadInt32()
                    (fn, ft, fs)
                )
            let size = src.ReadInt32()
            GLSLType.Struct(name, fields, size)

        | 7uy ->
            let len = src.ReadInt32()
            let elem = deserializeInternal src
            let align = src.ReadInt32()
            GLSLType.Array(len, elem, align)

        | 8uy ->
            let dim = src.ReadInt32() |> unbox<SamplerDimension>
            let format =
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadInt32() |> unbox<ImageFormat>)

            let valueType = deserializeInternal src
            let isArray = src.ReadBoolean()
            let isMS = src.ReadBoolean()

            let original = 
                let gen = imageTypes (isIntegral valueType, isArray, isMS, dim)
                match format with
                | Some fmt -> gen.MakeGenericType [| ImageFormat.toFormatType fmt |]
                | None -> gen

            GLSLType.Image {
                GLSLImageType.dimension = dim
                GLSLImageType.isArray = isArray
                GLSLImageType.isMS = isMS
                GLSLImageType.format = format
                GLSLImageType.valueType = valueType
                GLSLImageType.original = original
            }
        | 9uy ->
            let dim = src.ReadInt32() |> unbox<SamplerDimension>
            let valueType = deserializeInternal src
            let isArray = src.ReadBoolean()
            let isMS = src.ReadBoolean()
            let isShadow = src.ReadBoolean()

            let original = samplerTypes (isIntegral valueType, isArray, isMS, isShadow, dim)
            
            GLSLType.Sampler {
                GLSLSamplerType.dimension = dim
                GLSLSamplerType.isArray = isArray
                GLSLSamplerType.isMS = isMS
                GLSLSamplerType.isShadow = isShadow
                GLSLSamplerType.valueType = valueType
                GLSLSamplerType.original = original
            }
        | 10uy ->
            let elem = deserializeInternal src
            let stride = src.ReadInt32()
            GLSLType.DynamicArray(elem, stride)
        | 11uy ->
            let name = src.ReadString()
            GLSLType.Intrinsic name
        | id ->
            failwithf "unexpected GLSLType: %A" id

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

and [<StructuredFormatDisplay("{AsString}")>] GLSLProgramInterface =
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

    member private x.AsString = x.ToString()

module GLSLParameter =
    open System.IO

    let internal serializeInternal (dst : BinaryWriter) (i : GLSLParameter) =
        dst.Write i.paramLocation
        dst.Write i.paramName
        dst.Write i.paramSemantic
        GLSLType.serializeInternal dst i.paramType
        match i.paramInterpolation with
        | Some i ->
            dst.Write 1uy
            dst.Write (int i)
        | None ->
            dst.Write 0uy

    let internal deserializeInternal (src : BinaryReader) =
        let paramLocation = src.ReadInt32()
        let paramName = src.ReadString()
        let paramSemantic = src.ReadString()
        let paramType = GLSLType.deserializeInternal src
        let interpolation =
            match src.ReadByte() with
            | 0uy -> None
            | _ -> Some (src.ReadInt32() |> unbox<InterpolationMode>)
        {
            paramLocation = paramLocation
            paramName = paramName
            paramSemantic = paramSemantic
            paramType = paramType
            paramInterpolation = interpolation
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GLSLShaderInterface =

    let private discardFun = 
        {
            name = "discard"
            args = [||]
            ret = GLSLType.Void
        }

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

    open System.IO

    let internal serializeInternal (dst : BinaryWriter) (s : GLSLShaderInterface) =

        dst.Write (int s.shaderStage)
        dst.Write s.shaderEntry

        dst.Write (List.length s.shaderInputs)
        for i in s.shaderInputs do
            GLSLParameter.serializeInternal dst i

        dst.Write (List.length s.shaderOutputs)
        for i in s.shaderOutputs do
            GLSLParameter.serializeInternal dst i

        dst.Write s.shaderSamplers.Count
        for v in s.shaderSamplers do dst.Write v
        
        dst.Write s.shaderImages.Count
        for v in s.shaderImages do dst.Write v
        
        dst.Write s.shaderStorageBuffers.Count
        for v in s.shaderStorageBuffers do dst.Write v
        
        dst.Write s.shaderUniformBuffers.Count
        for v in s.shaderUniformBuffers do dst.Write v

        dst.Write s.shaderAccelerationStructures.Count
        for v in s.shaderAccelerationStructures do dst.Write v
        
        dst.Write s.shaderBuiltInFunctions.Count
        for v in s.shaderBuiltInFunctions do 
            dst.Write v.name
            GLSLType.serializeInternal dst v.ret
            dst.Write v.args.Length
            for a in v.args do GLSLType.serializeInternal dst a

        dst.Write (List.length s.shaderDecorations)
        for v in s.shaderDecorations do
            match v with
            | GLSLInvocations v ->
                dst.Write 0uy; dst.Write v
            | GLSLInputTopology top ->
                dst.Write 1uy
                match top with
                | InputTopology.Point -> dst.Write 0uy
                | InputTopology.Line -> dst.Write 1uy
                | InputTopology.LineAdjacency -> dst.Write 2uy 
                | InputTopology.Triangle -> dst.Write 3uy
                | InputTopology.TriangleAdjacency -> dst.Write 4uy
                | InputTopology.Patch c -> dst.Write 5uy; dst.Write c
            | GLSLOutputTopology top ->
                dst.Write 2uy
                match top with
                | OutputTopology.Points -> dst.Write 0uy
                | OutputTopology.LineStrip -> dst.Write 1uy
                | OutputTopology.TriangleStrip -> dst.Write 2uy
            | GLSLMaxVertices v ->
                dst.Write 3uy
                dst.Write v
            | GLSLSpacing s ->
                dst.Write 4uy
                match s with
                | GLSLSpacing.Equal -> dst.Write 0uy
                | GLSLSpacing.FractionalEven -> dst.Write 1uy
                | GLSLSpacing.FractionalOdd -> dst.Write 2uy
            | GLSLWinding w ->
                dst.Write 5uy
                match w with
                | GLSLWinding.CW -> dst.Write 0uy
                | GLSLWinding.CCW -> dst.Write 1uy
            | GLSLLocalSize s ->
                dst.Write 6uy
                dst.Write s.X
                dst.Write s.Y
                dst.Write s.Z

        dst.Write (s.shaderBuiltIns.Count)
        for KeyValue(kind, m) in s.shaderBuiltIns do
            dst.Write (int kind)
            dst.Write m.Count
            for KeyValue(name, t) in m do
                dst.Write name
                GLSLType.serializeInternal dst t

    let internal deserializeInternal (src : BinaryReader) =
        let stage = src.ReadInt32() |> unbox<ShaderStage>
        let entry = src.ReadString() 

        let input =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ -> GLSLParameter.deserializeInternal src)
     
        let output =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ -> GLSLParameter.deserializeInternal src)

        let shaderSamplers =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->  src.ReadString()) |> HashSet.ofList
            
        let shaderImages =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->  src.ReadString()) |> HashSet.ofList
            
        let shaderStorageBuffers =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->  src.ReadString()) |> HashSet.ofList
            
        let shaderUniformBuffers =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->  src.ReadString()) |> HashSet.ofList
            
        let shaderAccelerationStructures =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->  src.ReadString()) |> HashSet.ofList
            
        let shaderBuiltInFunctions =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->  
                let name = src.ReadString()
                let ret = GLSLType.deserializeInternal src
                let args = Array.init (src.ReadInt32()) (fun _ -> GLSLType.deserializeInternal src)
                { name = name; ret = ret; args = args }
            )
            |> HashSet.ofList

        let shaderDecorations =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->
                match src.ReadByte() with
                | 0uy -> 
                    src.ReadInt32() |> GLSLInvocations
                | 1uy -> 
                    match src.ReadByte() with
                    | 0uy -> GLSLInputTopology InputTopology.Point
                    | 1uy -> GLSLInputTopology InputTopology.Line
                    | 2uy -> GLSLInputTopology InputTopology.LineAdjacency
                    | 3uy -> GLSLInputTopology InputTopology.Triangle
                    | 4uy -> GLSLInputTopology InputTopology.TriangleAdjacency
                    | _ -> GLSLInputTopology (InputTopology.Patch(src.ReadInt32()))
                | 2uy ->
                    match src.ReadByte() with
                    | 0uy -> GLSLOutputTopology OutputTopology.Points
                    | 1uy -> GLSLOutputTopology OutputTopology.LineStrip
                    | _ -> GLSLOutputTopology OutputTopology.TriangleStrip
                | 3uy ->
                    GLSLMaxVertices (src.ReadInt32())
                | 4uy ->
                    match src.ReadByte() with
                    | 0uy -> GLSLSpacing GLSLSpacing.Equal
                    | 1uy -> GLSLSpacing GLSLSpacing.FractionalEven
                    | _ -> GLSLSpacing GLSLSpacing.FractionalOdd
                | 5uy ->
                    match src.ReadByte() with
                    | 0uy -> GLSLWinding GLSLWinding.CW
                    | _ -> GLSLWinding GLSLWinding.CCW
                | _ ->
                    GLSLLocalSize(V3i(src.ReadInt32(), src.ReadInt32(), src.ReadInt32()))
            )
            
        let shaderBuiltIns =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->
                let kind = src.ReadInt32() |> unbox<ParameterKind>
                let cnt = src.ReadInt32()
                kind, List.init cnt (fun _ ->
                    let name = src.ReadString()
                    let typ = GLSLType.deserializeInternal src
                    name, typ
                )
                |> MapExt.ofList
            )
            |> MapExt.ofList

        {
            shaderStage                  = stage
            shaderEntry                  = entry
            shaderInputs                 = input
            shaderOutputs                = output
            shaderSamplers               = shaderSamplers
            shaderImages                 = shaderImages
            shaderStorageBuffers         = shaderStorageBuffers
            shaderUniformBuffers         = shaderUniformBuffers
            shaderAccelerationStructures = shaderAccelerationStructures
            shaderBuiltInFunctions       = shaderBuiltInFunctions
            shaderDecorations            = shaderDecorations
            shaderBuiltIns               = shaderBuiltIns
        }

     

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
              (slot : ShaderSlot) (s : GLSLRaytracingShaders) =

        let alterHitGroup (mapping : GLSLRayHitGroup -> GLSLRayHitGroup)
                          (name : Symbol) (rayType : Symbol)
                          (groups : MapExt<Symbol, MapExt<Symbol, GLSLRayHitGroup>>) =
            groups |> MapExt.alter name (
                Option.defaultValue MapExt.empty >> MapExt.alter rayType (
                    Option.defaultValue GLSLRayHitGroup.empty >> mapping >> Some
                ) >> Some
            )

        match slot with
        | ShaderSlot.RayGeneration ->
            { s with raygenShader = s.raygenShader |> mapping }

        | ShaderSlot.Miss name ->
            { s with missShaders = s.missShaders |> MapExt.alter name mapping }

        | ShaderSlot.Callable name ->
            { s with callableShaders = s.callableShaders |> MapExt.alter name mapping }

        | ShaderSlot.AnyHit (name, rayType) ->
            let f g = { g with anyHitShader = g.anyHitShader |> mapping }
            { s with hitgroups = s.hitgroups |> alterHitGroup f name rayType }

        | ShaderSlot.ClosestHit (name, rayType) ->
            let f g = { g with closestHitShader = g.closestHitShader |> mapping }
            { s with hitgroups = s.hitgroups |> alterHitGroup f name rayType }

        | ShaderSlot.Intersection (name, rayType) ->
            let f g = { g with intersectionShader = g.intersectionShader |> mapping }
            { s with hitgroups = s.hitgroups |> alterHitGroup f name rayType }

        | _ ->
            failwithf "%A is not a valid raytracing shader slot" slot


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


    open System.IO


    module private SamplerState =

        let serialize (dst : BinaryWriter) (s : SamplerState) =
            match s.AddressU with
            | Some v -> dst.Write 1uy; dst.Write (int v)
            | None -> dst.Write 0uy
            match s.AddressV with
            | Some v -> dst.Write 1uy; dst.Write (int v)
            | None -> dst.Write 0uy
            match s.AddressW with
            | Some v -> dst.Write 1uy; dst.Write (int v)
            | None -> dst.Write 0uy
            match s.Filter with
            | Some v -> dst.Write 1uy; dst.Write (int v)
            | None -> dst.Write 0uy
            match s.Comparison with
            | Some v -> dst.Write 1uy; dst.Write (int v)
            | None -> dst.Write 0uy
            match s.BorderColor with
            | Some v -> dst.Write 1uy; dst.Write v.R; dst.Write v.G; dst.Write v.B; dst.Write v.A
            | None -> dst.Write 0uy
            match s.MaxAnisotropy with
            | Some v -> dst.Write 1uy; dst.Write v
            | None -> dst.Write 0uy
            match s.MaxLod with
            | Some v -> dst.Write 1uy; dst.Write v
            | None -> dst.Write 0uy
            match s.MinLod with
            | Some v -> dst.Write 1uy; dst.Write v
            | None -> dst.Write 0uy
            match s.MipLodBias with
            | Some v -> dst.Write 1uy; dst.Write v
            | None -> dst.Write 0uy

        let deserialize (src : BinaryReader) =
            let addressU = 
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadInt32() |> unbox<WrapMode>)
            let addressV = 
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadInt32() |> unbox<WrapMode>)
            let addressW = 
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadInt32() |> unbox<WrapMode>)
            let filter = 
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadInt32() |> unbox<Filter>)
            let comparison = 
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadInt32() |> unbox<ComparisonFunction>)
            let borderColor = 
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (Aardvark.Base.C4f(src.ReadSingle(), src.ReadSingle(), src.ReadSingle(), src.ReadSingle()))
            let maxAnisotropy = 
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadInt32())
            let maxLod = 
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadDouble())
            let minLod = 
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadDouble())
            let mipLodBias = 
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadDouble())

            {
                AddressU = addressU
                AddressV = addressV
                AddressW = addressW
                Filter = filter
                Comparison = comparison
                BorderColor = borderColor
                MaxAnisotropy = maxAnisotropy
                MaxLod = maxLod
                MinLod = minLod
                MipLodBias = mipLodBias
            }

    let internal serializeInternal (dst : BinaryWriter) (program : GLSLProgramInterface) =
        dst.Write (List.length program.inputs)
        for p in program.inputs do GLSLParameter.serializeInternal dst p
        
        dst.Write (List.length program.outputs)
        for p in program.outputs do GLSLParameter.serializeInternal dst p

        dst.Write program.samplers.Count
        for KeyValue(name, sampler) in program.samplers do
            dst.Write name
            dst.Write sampler.samplerBinding
            dst.Write sampler.samplerCount
            dst.Write sampler.samplerName
            dst.Write sampler.samplerSet
            GLSLType.serializeInternal dst (GLSLType.Sampler sampler.samplerType)
            
            dst.Write (List.length sampler.samplerTextures)
            for (name, state) in sampler.samplerTextures do
                dst.Write name
                SamplerState.serialize dst state
                
        dst.Write program.images.Count
        for KeyValue(name, image) in program.images do
            dst.Write name
            dst.Write image.imageBinding
            dst.Write image.imageName
            dst.Write image.imageSet
            GLSLType.serializeInternal dst (GLSLType.Image image.imageType)
            
        dst.Write program.storageBuffers.Count
        for KeyValue(name, ssb) in program.storageBuffers do
            dst.Write name
            dst.Write ssb.ssbBinding
            dst.Write ssb.ssbName
            dst.Write ssb.ssbSet
            GLSLType.serializeInternal dst ssb.ssbType
            
        dst.Write program.uniformBuffers.Count
        for KeyValue(name, ub) in program.uniformBuffers do
            dst.Write name
            dst.Write ub.ubBinding
            dst.Write ub.ubName
            dst.Write ub.ubSet
            dst.Write ub.ubSize
            dst.Write (List.length ub.ubFields)
            for f in ub.ubFields do
                dst.Write f.ufName
                dst.Write f.ufOffset
                GLSLType.serializeInternal dst f.ufType

        dst.Write program.accelerationStructures.Count
        for KeyValue(name, acc) in program.accelerationStructures do
            dst.Write name
            dst.Write acc.accelBinding
            dst.Write acc.accelName
            dst.Write acc.accelSet

        match program.shaders with
        | GLSLProgramShaders.Graphics gr ->
            dst.Write 0uy
            dst.Write gr.stages.Count
            for KeyValue(stage, shader) in gr.stages do
                dst.Write (int stage)
                GLSLShaderInterface.serializeInternal dst shader

        | GLSLProgramShaders.Compute comp ->
            dst.Write 1uy
            GLSLShaderInterface.serializeInternal dst comp
            ()
        | GLSLProgramShaders.Raytracing rt ->
            dst.Write 2uy
            
            match rt.raygenShader with
            | Some r ->
                dst.Write 1uy
                GLSLShaderInterface.serializeInternal dst r
            | None ->
                dst.Write 0uy


            dst.Write rt.callableShaders.Count
            for KeyValue(a, b) in rt.callableShaders do
                dst.Write (string a)
                GLSLShaderInterface.serializeInternal dst b

            dst.Write rt.missShaders.Count
            for KeyValue(a, b) in rt.missShaders do
                dst.Write (string a)
                GLSLShaderInterface.serializeInternal dst b
                
            dst.Write rt.hitgroups.Count
            for KeyValue(name, grp) in rt.hitgroups do
                dst.Write (string name)
                dst.Write grp.Count
                for KeyValue(n, b) in grp do
                    dst.Write (string n)

                    match b.anyHitShader with
                    | None -> dst.Write 0uy
                    | Some s ->
                        dst.Write 1uy
                        GLSLShaderInterface.serializeInternal dst s
                        
                    match b.closestHitShader with
                    | None -> dst.Write 0uy
                    | Some s ->
                        dst.Write 1uy
                        GLSLShaderInterface.serializeInternal dst s
                        
                    match b.intersectionShader with
                    | None -> dst.Write 0uy
                    | Some s ->
                        dst.Write 1uy
                        GLSLShaderInterface.serializeInternal dst s

    let internal deserializeInternal (src : BinaryReader) =

        let allShaders = System.Collections.Generic.List<GLSLShaderInterface>()
        let inline add s = allShaders.Add s; s

        let inputs =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ -> GLSLParameter.deserializeInternal src)
        let outputs =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ -> GLSLParameter.deserializeInternal src)

        let samplers =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->
                let name = src.ReadString()
                let samplerBinding = src.ReadInt32()
                let samplerCount = src.ReadInt32()
                let samplerName = src.ReadString()
                let samplerSet = src.ReadInt32()
                let samplerType =
                    match GLSLType.deserializeInternal src with
                    | GLSLType.Sampler s -> s
                    | t -> failwithf "unexpected SamplerType: %A" t

                let samplerTextures =
                    let cnt = src.ReadInt32()
                    List.init cnt (fun _ ->
                        let texName = src.ReadString()
                        let state = SamplerState.deserialize src
                        texName, state
                    )

                name, {
                    samplerBinding = samplerBinding
                    samplerCount = samplerCount
                    samplerName = samplerName
                    samplerSet = samplerSet
                    samplerType = samplerType
                    samplerTextures = samplerTextures
                }

            )
            |> MapExt.ofList

        let images =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->
                let name = src.ReadString()
                let imageBinding = src.ReadInt32()
                let imageName = src.ReadString()
                let imageSet = src.ReadInt32()
                let imageType =
                    match GLSLType.deserializeInternal src with
                    | GLSLType.Image i -> i
                    | t -> failwithf "bad ImageType: %A" t

                name, {
                    imageBinding = imageBinding
                    imageName = imageName
                    imageSet = imageSet
                    imageType = imageType
                }
            )   
            |> MapExt.ofList
            
        let storageBuffers =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->
                let name = src.ReadString()
                let ssbBinding = src.ReadInt32()
                let ssbName = src.ReadString()
                let ssbSet = src.ReadInt32()
                let ssbType = GLSLType.deserializeInternal src
                name, {
                    ssbBinding = ssbBinding
                    ssbName = ssbName
                    ssbSet = ssbSet
                    ssbType = ssbType
                }
            )   
            |> MapExt.ofList

        let uniformBuffers =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->
                let name = src.ReadString()
                let ubBinding = src.ReadInt32()
                let ubName = src.ReadString()
                let ubSet = src.ReadInt32()
                let ubSize = src.ReadInt32()
                let ubFields =
                    let cnt = src.ReadInt32()
                    List.init cnt (fun _ ->
                        let ufName = src.ReadString()
                        let ufOffset = src.ReadInt32()
                        let ufType = GLSLType.deserializeInternal src
                        { ufName = ufName; ufOffset = ufOffset; ufType = ufType }
                    )
                name, {
                    ubSet = ubSet
                    ubBinding = ubBinding
                    ubName = ubName
                    ubSize = ubSize
                    ubFields = ubFields
                }
            ) |> MapExt.ofList

        let accelerationStructures =
            let cnt = src.ReadInt32()
            List.init cnt (fun _ ->
                let name = src.ReadString()
                let accelBinding = src.ReadInt32()
                let accelName = src.ReadString()
                let accelSet = src.ReadInt32()
                name, { accelSet = accelSet; accelBinding = accelBinding; accelName = accelName }
            )
            |> MapExt.ofList

        let shaders = 
            match src.ReadByte() with
            | 0uy ->
                let shaders =
                    let cnt = src.ReadInt32()
                    List.init cnt (fun _ -> 
                        let stage = src.ReadInt32() |> unbox<ShaderStage>
                        stage, add (GLSLShaderInterface.deserializeInternal src)
                    )
                    |> MapExt.ofList

                GLSLProgramShaders.Graphics { GLSLGraphicsShaders.stages = shaders }
            | 1uy ->
                let comp = GLSLShaderInterface.deserializeInternal src
                GLSLProgramShaders.Compute comp
            | _ ->
                let rayGen =
                    match src.ReadByte() with
                    | 0uy -> None
                    | _ -> Some (add (GLSLShaderInterface.deserializeInternal src))

                let callableShaders =
                    let cnt = src.ReadInt32()
                    List.init cnt (fun _ ->
                        let name = Symbol.Create (src.ReadString())
                        let shader = GLSLShaderInterface.deserializeInternal src
                        name, add shader
                    )
                    |> MapExt.ofList

                let missShaders =
                    let cnt = src.ReadInt32()
                    List.init cnt (fun _ ->
                        let name = Symbol.Create (src.ReadString())
                        let shader = GLSLShaderInterface.deserializeInternal src
                        name, add shader
                    )
                    |> MapExt.ofList


                let hitgroups =
                    let cnt = src.ReadInt32()
                    List.init cnt (fun _ ->
                        let name = src.ReadString() |> Symbol.Create
                        let cnt = src.ReadInt32()
                        let group =
                            List.init cnt (fun _ ->
                                let rayType = src.ReadString() |> Symbol.Create

                                let anyHit =
                                    match src.ReadByte() with
                                    | 0uy -> None
                                    | _ -> GLSLShaderInterface.deserializeInternal src |> add |> Some
                                    
                                let closestHit =
                                    match src.ReadByte() with
                                    | 0uy -> None
                                    | _ -> GLSLShaderInterface.deserializeInternal src |> add |> Some
                                    
                                let intersectionShader =
                                    match src.ReadByte() with
                                    | 0uy -> None
                                    | _ -> GLSLShaderInterface.deserializeInternal src |> add |> Some

                                rayType, {
                                    anyHitShader = anyHit
                                    closestHitShader = closestHit
                                    intersectionShader = intersectionShader
                                }

                            )
                            |> MapExt.ofList

                        name, group
                    )
                    |> MapExt.ofList

                GLSLProgramShaders.Raytracing {
                    raygenShader = rayGen
                    callableShaders = callableShaders
                    missShaders = missShaders
                    hitgroups = hitgroups
                }

        let result = 
            {
                inputs = inputs
                outputs = outputs
                samplers = samplers
                images = images
                storageBuffers = storageBuffers
                uniformBuffers = uniformBuffers
                shaders = shaders
                accelerationStructures = accelerationStructures
            }

        result

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




