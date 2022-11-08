module Layout

open System
open System.Threading
open Aardvark.Base
open FShade.Imperative
open FShade
open FShade.GLSL

let mutable structid = 0
let newStructName() =
    let id = Interlocked.Increment(&structid)
    sprintf "struct%d" id

let rand = Random()

let rec randomType() =
    match rand.Next(7) with
        | 0 ->
            Int(true, 32)
        | 1 ->
            Float(32)
        | 2 ->
            Bool
        | 3 ->
            let dim = rand.Next(3) + 2
            Vec(dim, Float 32)
        | 4 ->
            let dim = rand.Next(3) + 2
            Mat(dim, dim, Float 32)
        | 5 ->
            if rand.NextDouble() > 0.7 then
                let fields = rand.Next(10) + 1
                Struct(
                    newStructName(),
                    List.init fields (fun i -> sprintf "field%d" i, randomType(), -1),
                    -1
                )
            else
                randomType()
        | _ ->
            let len = rand.Next(15) + 1

            let mutable t = randomType()
            let isgood (t : GLSLType) =
                match t with
                    | Array _ -> false
                    | _-> true
            while not (isgood t) do
                t <- randomType()
                

            Array(len, t, -1)

let randomBuffer() =
    
    let fields = rand.Next(10) + 1
    
    LayoutStd140.applyLayout {
        ubSet = 0
        ubBinding = 0
        ubFields =
            List.init fields (fun i -> 
                {
                    ufName = sprintf "uf%d" i
                    ufType = randomType()
                    ufOffset = -1
                }
            )
        ubName = "Global"
        ubSize = -1
    }

module private GLSLType =
    open FShade.Reflector

    let offset (dec : list<SpirV.Decoration * int[]>) =
        dec |> List.tryPick (fun (d,a) ->
            if d = SpirV.Decoration.Offset then Some a.[0]
            else None
        )
        |> Option.defaultValue -1

    let rec ofShaderType (t : ShaderType) =
        match t with
            | ShaderType.Void -> GLSLType.Void
            | ShaderType.Bool -> GLSLType.Bool
            | ShaderType.Int(w,s) -> GLSLType.Int(s, w)
            | ShaderType.Float w -> GLSLType.Float w
            | ShaderType.Vector(c,d) -> GLSLType.Vec(d, ofShaderType c)
            | ShaderType.Matrix(c,d) -> GLSLType.Mat(d,d, ofShaderType c)
            | ShaderType.Array(c,d) -> GLSLType.Array(d, ofShaderType c,-1)
            | ShaderType.Struct(name, fields) ->
                GLSLType.Struct(
                    name,
                    fields |> List.map (fun (typ,name,dec) -> name, ofShaderType typ, offset dec),
                    -1
                )
            | ShaderType.Ptr(_,t) -> ofShaderType t

            | ShaderType.RuntimeArray _
            | ShaderType.Image _
            | ShaderType.SampledImage _
            | ShaderType.Function _  
            | ShaderType.Sampler -> failwith ""
    
    let rec ofPrimitiveType (t : PrimitiveType) =
        match t with
            | PrimitiveType.Bool -> GLSLType.Bool
            | PrimitiveType.Int(w,s) -> GLSLType.Int(s, w)
            | PrimitiveType.Float w -> GLSLType.Float w
            | PrimitiveType.Vector(c,d) -> GLSLType.Vec(d, ofPrimitiveType c)
            | PrimitiveType.Matrix(c,d) -> GLSLType.Mat(d,d, ofPrimitiveType c)
           
    
    let rec ofUniformType (t : UniformType) =
        match t with
            | UniformType.Primitive(t,s,a) -> ofPrimitiveType t
            | UniformType.Array(e,len,size,a) -> GLSLType.Array(len, ofUniformType e, a)
            | UniformType.Struct layout -> ofLayout layout
            | _ -> failwith ""
                
    and ofLayout (l : UniformBufferLayout) =
        GLSLType.Struct(
            newStructName(),
            l.fields |> List.map (fun f -> f.name, ofUniformType f.fieldType, f.offset),
            (match l.size with | Fixed s -> s | _ -> failwith "")
        )

let layoutGLSLang (b : GLSLUniformBuffer) =

    let rec toCType (t : GLSLType) =
        match t with
            | Int(s,w) -> CInt(s,w)
            | Float w -> CFloat w
            | Vec(d,b) -> CVector(toCType b, d)
            | Mat(r,c,b) -> CMatrix(toCType b, r, c)
            | Array(len, b,_) -> CArray(toCType b, len)
            | Bool -> CType.CBool
            | Struct(name, fields, _) ->
                CStruct(name, fields |> List.map (fun (name,t,_) -> toCType t, name), None)

            | DynamicArray _ | Image _ | Sampler _ | Void  | Intrinsic _ -> 
                failwith ""

    let rec structs (t : GLSLType) =
        match t with
            | Int(s,w) -> []
            | Float w -> []
            | Vec(d,b) -> []
            | Mat(r,c,b) -> []
            | Array(len, b,_) -> structs b
            | Bool -> []
            | Struct(name, fields, _) ->
                (fields |> List.collect (fun (name,t,_) -> structs t)) @
                [(name, fields)]

            | DynamicArray _ | Image _ | Sampler _ | Void | Intrinsic _ -> 
                []

    let firstField =
        let f = b.ubFields |> List.head
        let rec getFloat (e : CExpr) =
            match e.ctype with
                | CType.CInt _ -> CExpr.CConvert(CType.CFloat 32, e)
                | CType.CFloat _ -> e
                | CType.CVector(b,_) -> CExpr.CVecSwizzle(b, e, [CVecComponent.X]) |> getFloat
                | CType.CMatrix(b,_,_) -> CExpr.CMatrixElement(b, e, 0, 0) |> getFloat
                | CType.CStruct(_,fields,_) ->
                    let (t,n) = fields |> List.head 
                    CExpr.CField(t, e, n) |> getFloat
                | CType.CArray(t,len) ->
                    CExpr.CItem(t, e, CExpr.CValue(CType.CInt(true, 32), CLiteral.CIntegral 0L)) |> getFloat
                | CType.CBool -> CExpr.CConvert(CType.CFloat 32, e)

                | _ -> failwith ""
        let self = CExpr.CReadInput(ParameterKind.Uniform, toCType f.ufType, f.ufName, None)
        getFloat self




    let values =
        [

            yield CUniformDef [
                for f in b.ubFields do
                    yield {
                        cUniformType = toCType f.ufType
                        cUniformName = f.ufName
                        cUniformBuffer = Some b.ubName
                        cUniformDecorations = []
                    }
            ]

            yield CEntryDef {
                cEntryName = "main"
                cInputs = []
                cOutputs = []
                cArguments = []
                cReturnType = CType.CVoid
                cBody =
                    CStatement.CWriteOutput(
                        "Positions", None, 
                        CRExpr.ofExpr (
                            CExpr.CNewVector(
                                CType.CVector(CType.CFloat(32), 4),
                                [
                                    firstField
                                    firstField
                                    firstField
                                    firstField
                                ]
                            )
                        )
                    )
                cDecorations = [
                    EntryDecoration.Stages (
                        ShaderStageDescription.Graphics { prev = None; self = ShaderStage.Vertex; next = Some ShaderStage.Fragment}
                    )
                ]
            }

        ]

    let m =
        {
            cuserData = null
            types = 
                [
                    for (name, fields) in b.ubFields |> List.collect (fun f -> structs f.ufType) do
                        yield CStructDef(name, fields |> List.map (fun (name, typ,_) -> toCType typ, name))
                ]
            values = values
        }

    let glsl = m |> Assembler.assemble glslVulkan
    
    let spirv = 
        match GLSLang.GLSLang.tryCompile GLSLang.ShaderStage.Vertex "main" [] glsl.code with
            | Some spirv,_ -> spirv
            | None, err -> failwithf "%s" err
    let map = FShade.Reflector.ShaderInfo.ofBinary spirv
    let ub = 
        map.[ShaderStage.Vertex].uniformBlocks |> List.head

    
    { b with
        ubSize = (match ub.layout.size with | Reflector.Fixed s -> s | _ -> failwith "")
        ubFields = 
            ub.layout.fields |> List.map (fun f ->
                {
                    ufName = f.name
                    ufType = GLSLType.ofUniformType f.fieldType
                    ufOffset = f.offset
                }
            )
    }

let validateLayout (b : GLSLUniformBuffer) =
    let glsl = layoutGLSLang b

    let fields = List.zip b.ubFields glsl.ubFields

    let fieldsMatch = 
        fields |> List.forall (fun (lf,rf) ->
            lf.ufOffset = rf.ufOffset
        )

    if fieldsMatch then
         Success glsl
    else
        let badFields = 
            fields |> List.filter (fun (lf,rf) ->
                lf.ufOffset <> rf.ufOffset
            )

        let err = 
            String.concat "\r\n" (
                badFields |> List.map (fun (l,r) -> sprintf "%s; %d vs. %d" l.ufName l.ufOffset r.ufOffset) 
            )
            
        Error err

let randomHate() =
    Log.startTimed "testing layout"
    let iter = 100000
    for i in 1 .. iter do  
        let b = randomBuffer()
        match validateLayout b with
            | Success _ -> ()
            | Error e -> 
                Log.warn "==========================================================="
                Log.warn "BAD"
                Log.warn "%A" b
                Log.warn "==========================================================="
                let glsl = layoutGLSLang b
                Log.warn "%A" glsl
                Log.warn "==========================================================="
                Log.warn "%s" e
                Log.warn "==========================================================="

        Report.Progress(float i / float iter)

    Log.stop()