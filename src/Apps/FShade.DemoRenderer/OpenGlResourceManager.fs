namespace FShade.Demo

open System
open System.Text.RegularExpressions
open System.Runtime.InteropServices
open OpenTK.Graphics.OpenGL4
open FShade
open Aardvark.Base

module OpenGlResourceManager =
    open FShade.SamplerStateModule

    type private GLShaderType = OpenTK.Graphics.OpenGL4.ShaderType

    let private translateShaderType (t : ShaderType) =
        match t with
            | Vertex -> GLShaderType.VertexShader
            | Geometry _ -> GLShaderType.GeometryShader
            | Fragment -> GLShaderType.FragmentShader
            | TessControl -> GLShaderType.TessControlShader
            | TessEval -> GLShaderType.TessEvaluationShader

    let private versionRx = Regex @"#version.+\r\n"
    let private withDefine (kind : ShaderType) (code : string) =
        let m = versionRx.Match code

        let def, entry = 
            match kind with
                | Vertex -> "Vertex", "VS("
                | TessControl -> "TessControl", "TCS("
                | TessEval -> "TessEval", "TEV("
                | Geometry _ -> "Geometry", "GS("
                | Fragment -> "Pixel", "PS("


        let body = code.Substring(m.Index + m.Length).Replace(entry, "main(")

        code.Substring(0, m.Index + m.Length) +
        sprintf "#define %s\r\n" def +
        body


    type OpenGlAttribute(index : int, name : string, t : ActiveAttribType, size : int) =
        member x.Index = index
        member x.Name = name
        member x.Type = t
        member x.Size = size

    type OpenGlUniform(index : int, offset : int, name : string, t : ActiveUniformType, arraySize : int) =
        member x.Index = index
        member x.Offset = offset
        member x.Name = name
        member x.Type = t
        member x.ArraySize = arraySize

    type OpenGlUniformBlock(index : int, name : string, elements : seq<OpenGlUniform>, size : int) =
        let elements = elements |> Seq.sortBy (fun e -> e.Offset) |> Seq.toArray

        member x.Size = size
        member x.Index = index
        member x.Name = name
        member x.Elements = elements

    type OpenGlBuffer(handle : int, dimension : int, elementType : VertexAttribPointerType) =
        member x.Handle = handle
        member x.Dimension = dimension
        member x.Type = elementType

    
    [<CustomComparison>]
    [<CustomEquality>]
    type private TypeTuple = TypeTuple of Type * Type with
        interface IComparable with
            member x.CompareTo(o : obj) =
                match o with
                    | :? TypeTuple as o ->
                        match x, o with
                            | TypeTuple(a0, a1), TypeTuple(b0, b1) ->
                                let c0 = a0.GUID.CompareTo(b0.GUID)
                                if c0 = 0 then
                                    a1.GUID.CompareTo(b1.GUID)
                                else
                                    c0
                    | _ -> failwith "uncomparable"

        override x.GetHashCode() =
            match x with
                | TypeTuple(a,b) -> a .GetHashCode() ^^^ b.GetHashCode()

        override x.Equals(o) =
            match o with
                | :? TypeTuple as o ->
                    match x, o with
                        | TypeTuple(a0, a1), TypeTuple(b0, b1) ->
                            a0 = b0 && a1 = b1
                | _ -> false
                    

    let uniformValueType (a : ActiveUniformType) =
        match a with
            | ActiveUniformType.Bool -> typeof<int>
            | ActiveUniformType.Double -> typeof<float>
            | ActiveUniformType.DoubleVec2 -> typeof<V2d>
            | ActiveUniformType.DoubleVec3 -> typeof<V3d>
            | ActiveUniformType.DoubleVec4 -> typeof<V4d>

            | ActiveUniformType.Float -> typeof<float32>
            | ActiveUniformType.FloatVec2 -> typeof<V2f>
            | ActiveUniformType.FloatVec3 -> typeof<V3f>
            | ActiveUniformType.FloatVec4 -> typeof<V4f>
            | ActiveUniformType.Int -> typeof<float32>
            | ActiveUniformType.IntVec2 -> typeof<V2i>
            | ActiveUniformType.IntVec3 -> typeof<V3i>
            | ActiveUniformType.IntVec4 -> typeof<V4i>

            | ActiveUniformType.FloatMat2 -> typeof<M22f>
            | ActiveUniformType.FloatMat3 -> typeof<M34f>
            | ActiveUniformType.FloatMat4 -> typeof<M44f>

            | _ -> failwith "unknown uniform-type"

    type private ConverterBuilder() =
        member x.Yield(_) = Map.empty

        [<CustomOperation("conv")>]
        member x.Conv (map : Map<TypeTuple, obj -> obj>, f : 'a -> 'b) =
            let a = typeof<'a>
            let b = typeof<'b>

            Map.add (TypeTuple(a,b)) (fun (o : obj) -> o |> unbox |> f :> obj) map


        member x.Run(m : Map<TypeTuple, obj -> obj>) =
            fun (i : obj) (target : ActiveUniformType) ->
                let b = uniformValueType target
                let a = i.GetType()

                

                let t = TypeTuple(a,b)
                match Map.tryFind t m with
                    | Some c -> c i
                    | None -> 
                        if a = b then i
                        else failwithf "unsupported uniform-conversion: %A -> %A" a b

    let private convert = ConverterBuilder()
    let convertUniformValue =
        convert {
            conv (fun (a : double) -> a |> float)
            conv (fun (a : V2d) -> a.ToV2f())
            conv (fun (a : V3d) -> a.ToV3f())
            conv (fun (a : V4d) -> a.ToV4f())
            conv (fun (a : bool) -> if a then 1 else 0)

            conv (fun (a : M33d) -> M34f(a.M00 |> float32, a.M10 |> float32, a.M20 |> float32, 0.0f,
                                         a.M01 |> float32, a.M11 |> float32, a.M21 |> float32, 0.0f,
                                         a.M02 |> float32, a.M12 |> float32, a.M22 |> float32, 0.0f))

            conv (fun (a : M44d) -> M44f(a.M00 |> float32, a.M10 |> float32, a.M20 |> float32, a.M30 |> float32,
                                         a.M01 |> float32, a.M11 |> float32, a.M21 |> float32, a.M31 |> float32,
                                         a.M02 |> float32, a.M12 |> float32, a.M22 |> float32, a.M32 |> float32,
                                         a.M03 |> float32, a.M13 |> float32, a.M23 |> float32, a.M33 |> float32))

            conv (fun (a : Trafo3d) -> 
                let a = a.Forward
                M44f(a.M00 |> float32, a.M10 |> float32, a.M20 |> float32, a.M30 |> float32,
                     a.M01 |> float32, a.M11 |> float32, a.M21 |> float32, a.M31 |> float32,
                     a.M02 |> float32, a.M12 |> float32, a.M22 |> float32, a.M32 |> float32,
                     a.M03 |> float32, a.M13 |> float32, a.M23 |> float32, a.M33 |> float32))
        }


    type OpenGlUniformBuffer(handle : int, layout : seq<OpenGlUniform>, size : int) =
        let layout = layout |> Seq.toList
        let map = layout |> List.map (fun u -> (u.Name,u)) |> Map.ofList
        let data = Marshal.AllocHGlobal(size)
        let mutable isDirty = true

        member x.Size = size
        member x.Handle = handle
        member x.Layout = layout

        member x.Set(name : string, value : obj) =
            match Map.tryFind name map with
                | Some u ->
                    let conv = convertUniformValue value u.Type
                    Marshal.StructureToPtr(conv, data + (nativeint u.Offset), false)
                    isDirty <- true
                | None ->
                    ()

        member x.Update() =
            if isDirty then
                isDirty <- false
                GL.BindBuffer(BufferTarget.UniformBuffer, handle)
                GL.BufferSubData(BufferTarget.UniformBuffer, 0n, nativeint size, data)
                GL.BindBuffer(BufferTarget.UniformBuffer, 0)

    type OpenGlProgram(handle : int) =

        let getInputs () =
            let mutable count = 0
            GL.GetProgram(handle, GetProgramParameterName.ActiveAttributes, &count)
            let count = count
            [ for i in 0..count-1 do
                let mutable size = 0
                let mutable t = ActiveAttribType.None
                let mutable len = 0
                let sb = System.Text.StringBuilder()

                GL.GetActiveAttrib(handle, i, 4192, &len, &size, &t, sb)

                let name = sb.ToString()
                yield OpenGlAttribute(i, name, t, size)
            ]

        let getUniformBlocks() =
            let mutable count = 0
            GL.GetProgram(handle, GetProgramParameterName.ActiveUniformBlocks, &count)
            let count = count

            [ for i in 0..count-1 do
                let mutable children = 0
                let mutable dataSize = 0
                GL.GetActiveUniformBlock(handle, i, ActiveUniformBlockParameter.UniformBlockActiveUniforms, &children)
                let indices = Array.zeroCreate children
                let offsets = Array.zeroCreate children
                GL.GetActiveUniformBlock(handle, i, ActiveUniformBlockParameter.UniformBlockActiveUniformIndices, indices)
                GL.GetActiveUniforms(handle, children, indices, ActiveUniformParameter.UniformOffset, offsets)

                GL.GetActiveUniformBlock(handle, i, ActiveUniformBlockParameter.UniformBlockDataSize, &dataSize)

                let elements =
                    [ for (c,o) in Array.zip indices offsets do
                        let mutable size = 0
                        let mutable len = 0
                        let sb = System.Text.StringBuilder()
                        let mutable t = ActiveUniformType.Int
                        GL.GetActiveUniform(handle, c, 4192, &len, &size, &t, sb)
                        let name = sb.ToString()
                        
                        
                        yield OpenGlUniform(c, o, name, t, size)
                    ]

                let mutable len = 0
                let sb = System.Text.StringBuilder()
                GL.GetActiveUniformBlockName(handle, i, 4192, &len, sb)

                yield OpenGlUniformBlock(i, sb.ToString(), elements, dataSize)

                    


                ()    
            ]

        let (|Sampler|_|) (t : ActiveUniformType) =
            match t with
                | ActiveUniformType.IntSampler1D -> Sampler |> Some
                | ActiveUniformType.IntSampler1DArray -> Sampler |> Some
                | ActiveUniformType.IntSampler2D -> Sampler |> Some
                | ActiveUniformType.IntSampler2DArray -> Sampler |> Some
                | ActiveUniformType.IntSampler3D -> Sampler |> Some

                | ActiveUniformType.Sampler1D -> Sampler |> Some
                | ActiveUniformType.Sampler1DArray -> Sampler |> Some
                | ActiveUniformType.Sampler2D -> Sampler |> Some
                | ActiveUniformType.Sampler2DArray -> Sampler |> Some
                | ActiveUniformType.Sampler2DShadow -> Sampler |> Some
                | ActiveUniformType.Sampler3D -> Sampler |> Some

                | _ -> None

        let getSamplers() =
            let mutable count = 0
            GL.GetProgram(handle, GetProgramParameterName.ActiveUniforms, &count)
            let count = count

            [ for i in 0..count-1 do
                let mutable len = 0
                let mutable size = 0
                let mutable t = ActiveUniformType.Float
                let sb = System.Text.StringBuilder()
                GL.GetActiveUniform(handle, i, 4192, &len, &size, &t, sb)
                let name = sb.ToString()

                let loc = GL.GetUniformLocation(handle, name)

                match t with
                    | Sampler -> yield OpenGlUniform(i, loc, name, t, size)
                    | _ -> ()
            ]

        let inputs = getInputs()
        let uniforms = getUniformBlocks()
        let samplers = getSamplers()

        do for u in uniforms do
            GL.UniformBlockBinding(handle, u.Index, u.Index)
           
           
           GL.UseProgram(handle)
           let mutable index = 0
           for u in samplers do
            GL.Uniform1(u.Offset, u.Index)
            index <- index + 1
           GL.UseProgram(0)

        member x.Inputs = inputs
        member x.Uniforms = uniforms
        member x.Samplers = samplers

        member x.Handle = handle

    let private createShader (kind : ShaderType) (code : string) =

        let code = withDefine kind code

        let handle = GL.CreateShader(translateShaderType kind)

        GL.ShaderSource(handle, code)
        GL.CompileShader(handle)
        let mutable status = 0
        GL.GetShader(handle, ShaderParameter.CompileStatus, &status)

        if status = 1 then
            handle
        else
            let mutable len = 0
            let sb = System.Text.StringBuilder(4192)
            GL.GetShaderInfoLog(handle, 4192, &len, sb)

            let err = sb.ToString()
            printfn "Shader Errors:\r\n%s" err
            failwithf "Shader Errors: %s" err

    let createProgram (code : string) =
        let shaders =
            [ if code.Contains "VS(" then yield createShader ShaderType.Vertex code
              if code.Contains "TCS(" then yield createShader ShaderType.TessControl code
              if code.Contains "TEV(" then yield createShader ShaderType.TessEval code
              if code.Contains "GS(" then yield createShader (ShaderType.Geometry OutputTopology.TriangleStrip) code
              if code.Contains "PS(" then yield createShader ShaderType.Fragment code ]
        let handle = GL.CreateProgram()

        for sh in shaders do
            GL.AttachShader(handle, sh)

        GL.LinkProgram(handle)
        let mutable status = 0
        GL.GetProgram(handle, GetProgramParameterName.LinkStatus, &status)

        if status = 1 then
            OpenGlProgram handle
        else
            let sb = System.Text.StringBuilder()
            let mutable len = 0
            GL.GetProgramInfoLog(handle, 4192, &len, sb)

            let err = sb.ToString()

            printfn "Program Errors:\r\n%s" err
            failwithf "Program Errors:\r\n%s" err

    let createBuffer (data : Array) =
        let elementType = data.GetType().GetElementType()

        let elementSize = Marshal.SizeOf(elementType)
        let size = data.Length * elementSize |> nativeint

        let handle = GL.GenBuffer()

        let gc = GCHandle.Alloc(data, GCHandleType.Pinned)
        GL.BindBuffer(BufferTarget.CopyWriteBuffer, handle)
        GL.BufferData(BufferTarget.CopyWriteBuffer, size, gc.AddrOfPinnedObject(), BufferUsageHint.StaticDraw)
        gc.Free()
        GL.BindBuffer(BufferTarget.CopyWriteBuffer, 0)

        let getAttribType (t : Type) =
            match t with
                | Aardvark.Base.TypeInfo.Patterns.Int32 -> VertexAttribPointerType.Int
                | Aardvark.Base.TypeInfo.Patterns.Float32 -> VertexAttribPointerType.Float
                | Aardvark.Base.TypeInfo.Patterns.Byte -> VertexAttribPointerType.UnsignedByte
                | Aardvark.Base.TypeInfo.Patterns.SByte -> VertexAttribPointerType.Byte
                | _ -> failwith "unknown attribute type"

        let dim, baseType = 
            match elementType with
                | Aardvark.Base.TypeInfo.Patterns.VectorOf(d, t) ->
                    let baseType = getAttribType t
                    d,baseType
                | _ ->
                    1, getAttribType elementType

        OpenGlBuffer(handle, dim, baseType)

    let createUniformBuffer (block : OpenGlUniformBlock) (getter : string -> obj) =
        let handle = GL.GenBuffer()
        GL.BindBuffer(BufferTarget.UniformBuffer, handle)
        GL.BufferData(BufferTarget.UniformBuffer, block.Size |> nativeint, 0n, BufferUsageHint.DynamicDraw)
        GL.BindBuffer(BufferTarget.UniformBuffer, 0)
        OpenGlUniformBuffer(handle, block.Elements, block.Size)


    let createVAO (indexBuffer : OpenGlBuffer) (buffers : OpenGlBuffer[]) =
        let vao = GL.GenVertexArray()

        GL.BindVertexArray(vao)

        let mutable index = 0
        for b in buffers do
            GL.BindBuffer(BufferTarget.ArrayBuffer, b.Handle)
            GL.EnableVertexAttribArray(index)

            GL.VertexAttribPointer(index, b.Dimension, b.Type, false, 0, 0n)

            index <- index + 1

        GL.BindBuffer(BufferTarget.ElementArrayBuffer, indexBuffer.Handle)

        GL.BindVertexArray(0)

        GL.BindBuffer(BufferTarget.ElementArrayBuffer, 0)
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
        vao

    let createTexture (bmp : System.Drawing.Bitmap) =
        let handle = GL.GenTexture()
        GL.BindTexture(TextureTarget.Texture2D, handle)

        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, TextureWrapMode.Repeat |> int)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, TextureWrapMode.Repeat |> int)

        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, TextureMinFilter.LinearMipmapLinear |> int)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, TextureMagFilter.Linear |> int)

        let l = bmp.LockBits(System.Drawing.Rectangle(0,0,bmp.Width, bmp.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba8, bmp.Width, bmp.Height, 0, PixelFormat.Bgra, PixelType.UnsignedByte, l.Scan0)
        bmp.UnlockBits(l)

        GL.GenerateMipmap(GenerateMipmapTarget.Texture2D)



        GL.BindTexture(TextureTarget.Texture2D, 0)
        handle




    let createSampler (state : SamplerState) =
        let handle = GL.GenSampler()

        let warpMode (a : Option<WrapMode>) =
            match a with
                | Some a ->
                    match a with 
                        | WrapMode.Wrap -> TextureWrapMode.Repeat
                        | WrapMode.Clamp -> TextureWrapMode.ClampToEdge
                        | WrapMode.Border -> TextureWrapMode.ClampToBorder
                        | WrapMode.Mirror -> TextureWrapMode.MirroredRepeat
                        | WrapMode.MirrorOnce -> TextureWrapMode.MirroredRepeat
                        | _ -> failwith "unknown WrapMode"
                | None ->
                    TextureWrapMode.Repeat

        let minFilter, magFilter, anis =
            match state.Filter with
                | Some filter ->
                    match filter with
                        | Filter.Anisotropic -> TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, 16
                        | Filter.MinMagLinear -> TextureMinFilter.Linear, TextureMagFilter.Linear, 0
                        | Filter.MinMagPoint -> TextureMinFilter.Nearest, TextureMagFilter.Nearest, 0
                        | Filter.MinLinearMagPoint -> TextureMinFilter.Linear, TextureMagFilter.Nearest, 0
                        | Filter.MinPointMagLinear -> TextureMinFilter.Nearest, TextureMagFilter.Linear, 0

                        | Filter.MinMagMipLinear -> TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, 0
                        | Filter.MinMagMipPoint -> TextureMinFilter.NearestMipmapNearest, TextureMagFilter.Nearest, 0

                        | Filter.MinPointMagMipLinear -> TextureMinFilter.NearestMipmapLinear, TextureMagFilter.Linear, 0
                        | Filter.MinMagPointMipLinear -> TextureMinFilter.LinearMipmapNearest, TextureMagFilter.Linear, 0
                        | Filter.MinMagLinearMipPoint -> TextureMinFilter.LinearMipmapNearest, TextureMagFilter.Linear, 0
                        | Filter.MinLinearMagMipPoint -> TextureMinFilter.LinearMipmapNearest, TextureMagFilter.Nearest, 0

                        | Filter.MinPointMagLinearMipPoint -> TextureMinFilter.NearestMipmapNearest, TextureMagFilter.Linear, 0
                        | Filter.MinLinearMagPointMipLinear -> TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Nearest, 0
           

                        | _ -> failwith "not implemented"

                | None -> TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, 0

        let anis =
            match state.MaxAnisotropy with
                | Some a -> a
                | _ -> anis



        GL.SamplerParameter(handle, SamplerParameterName.TextureWrapS, warpMode state.AddressU |> int)
        GL.SamplerParameter(handle, SamplerParameterName.TextureWrapT, warpMode state.AddressV |> int)
        GL.SamplerParameter(handle, SamplerParameterName.TextureWrapR, warpMode state.AddressW |> int)
        GL.SamplerParameter(handle, SamplerParameterName.TextureMinFilter, minFilter |> int)
        GL.SamplerParameter(handle, SamplerParameterName.TextureMagFilter, magFilter |> int)
        if anis > 0 then GL.SamplerParameter(handle, SamplerParameterName.TextureMaxAnisotropyExt, anis)
        
        match state.MipLodBias with
            | Some bias ->
                GL.SamplerParameter(handle, SamplerParameterName.TextureLodBias, bias |> float32)
            | None ->
                ()

        match state.MinLod with
            | Some min ->
                GL.SamplerParameter(handle, SamplerParameterName.TextureMinLod, min |> float32)
            | None ->
                ()

        match state.MaxLod with
            | Some max ->
                GL.SamplerParameter(handle, SamplerParameterName.TextureMaxLod, max |> float32)
            | None ->
                ()

        match state.BorderColor with
            | Some color ->
                GL.SamplerParameter(handle, SamplerParameterName.TextureBorderColor, [|color.R; color.G; color.B; color.A|])
            | None ->
                ()

        handle
