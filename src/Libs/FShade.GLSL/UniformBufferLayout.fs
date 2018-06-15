namespace FShade.GLSL

open Aardvark.Base
open FShade.Imperative
open FShade

type GLSLType =
    | Bool
    | Void
    | Int of signed : bool * width : int
    | Float of width : int
    | Vec of dim : int * elem : GLSLType
    | Mat of rows : int * cols : int * elem : GLSLType
    | Struct of name : string * fields : list<string * GLSLType * int> * size : int
    | Array of len : int * elem : GLSLType * stride : int

type GLSLParameter =
    {
        paramType       : GLSLType
        paramName       : string
        paramLocation   : int
    }

type GLSLSampler =
    {
        samplerSet      : int
        samplerBinding  : int
        samplerName     : string
        samplerCount    : int
        samplerTextures : list<string * SamplerState>
    }

type GLSLImage =
    {
        imageSet        : int
        imageBinding    : int
        imageName       : string
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

type GLSLProgramInterface =
    {
        inputs          : list<GLSLParameter>
        outputs         : list<GLSLParameter>
        samplers        : list<GLSLSampler>
        images          : list<GLSLImage>
        storageBuffers  : list<GLSLStorageBuffer>
        uniformBuffers  : list<GLSLUniformBuffer>
        usedBuiltIns    : MapExt<ShaderStage, MapExt<ParameterKind, Set<string>>>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GLSLType =
    let rec ofCType (t : CType) =
        match t with    
            | CType.CBool -> GLSLType.Bool
            | CType.CVoid -> GLSLType.Void
            | CType.CInt(signed, width) -> GLSLType.Int(signed, width)

            | CType.CFloat 64 -> GLSLType.Float 32
            | CType.CFloat(width) -> GLSLType.Float(width)

            | CType.CVector(elem, dim) -> GLSLType.Vec(dim, ofCType elem)
            | CType.CMatrix(elem, r, c) -> GLSLType.Mat(r, c, ofCType elem)

            | CType.CArray(elem, len) -> GLSLType.Array(len, ofCType elem, -1)
            | CType.CStruct(name, fields,_) -> GLSLType.Struct(name, fields |> List.map (fun (t, n) -> n, ofCType t, -1), -1)
            | _ -> failwithf "[GLSL] invalid shader IO type: %A" t


module LayoutStd140 =
    
    let rec layout (t : GLSLType) =
        match t with
            | GLSLType.Void ->
                t, 0, 0

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

                let s =
                    if s % 16 = 0 then s
                    else s + 16 - (s % 16)

                GLSLType.Array(len, bt, s), 16, len * s

            | GLSLType.Mat(rows, cols, bt) ->
                let narr, a, s = layout (GLSLType.Array(cols, GLSLType.Vec(rows, bt), -1))
                let bt = 
                    match narr with
                        | GLSLType.Array(_, GLSLType.Vec(_,bt), _) -> bt
                        | _ -> failwith "that was unexpected"
                GLSLType.Mat(rows, cols, bt), a, s

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

                GLSLType.Struct(name, newFields, offset), largestAlign, offset



    let applyLayout (ub : GLSLUniformBuffer) : GLSLUniformBuffer =
        let mutable offset = 0
        let mutable lastAlign = 16

        let newFields =
            ub.ubFields |> List.map (fun uf ->
                let nufType, align, size = layout uf.ufType

                if offset % align <> 0 then
                    offset <- offset + (align - offset % align)

                let res = offset
                offset <- offset + size

                { uf with ufOffset = res; ufType = nufType }
            )
        { ub with ubFields = newFields; ubSize = offset }

