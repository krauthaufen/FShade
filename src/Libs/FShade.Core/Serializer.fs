namespace FShade

#nowarn "1337"

[<AutoOpen>]
module Serializer =
    open System
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open System.IO
    open Aardvark.Base
    open System.Runtime.InteropServices
    open Microsoft.FSharp.Reflection
    open FShade
    open Microsoft.FSharp.Quotations
    open FShade.Imperative

    module Type =

        type private TypeId =
            | Uint8     = 0uy
            | Uint16    = 1uy
            | Uint32    = 2uy
            | Uint64    = 3uy
            | Int8      = 4uy
            | Int16     = 5uy
            | Int32     = 6uy
            | Int64     = 7uy
            | Float32   = 8uy
            | Float64   = 9uy
            | Decimal   = 10uy
            | NativeInt = 11uy
            | UNativeInt = 12uy

            | Bool      = 11uy
            | Unit      = 12uy
            | String    = 13uy
            | Char      = 14uy
            | Void      = 15uy
        
            | V2i       = 32uy
            | V3i       = 33uy
            | V4i       = 34uy
            | V2l       = 35uy
            | V3l       = 36uy
            | V4l       = 37uy
            | V2f       = 38uy
            | V3f       = 39uy
            | V4f       = 40uy
            | V2d       = 41uy
            | V3d       = 42uy
            | V4d       = 43uy

            | M22i      = 44uy
            | M23i      = 45uy
            | M33i      = 46uy
            | M34i      = 47uy
            | M44i      = 48uy
            | M22f      = 49uy
            | M23f      = 50uy
            | M33f      = 51uy
            | M34f      = 52uy
            | M44f      = 53uy
            | M22d      = 54uy
            | M23d      = 55uy
            | M33d      = 56uy
            | M34d      = 57uy
            | M44d      = 58uy
            | M22l      = 59uy
            | M23l      = 60uy
            | M33l      = 61uy
            | M34l      = 62uy
            | M44l      = 63uy
        
            | GenRef    = 245uy
            | Ref       = 246uy
            | Tuple     = 247uy
            | MTPar     = 248uy
            | TPar      = 249uy
            | Generic   = 250uy
            | Pointer   = 251uy
            | ByRef     = 252uy
            | Array     = 253uy
            | Function  = 254uy
            | Other     = 255uy

        let private primitiveIds =
            Dict.ofList [
                typeof<uint8>,      TypeId.Uint8
                typeof<uint16>,     TypeId.Uint16
                typeof<uint32>,     TypeId.Uint32
                typeof<uint64>,     TypeId.Uint64
            
                typeof<int8>,       TypeId.Int8
                typeof<int16>,      TypeId.Int16
                typeof<int32>,      TypeId.Int32
                typeof<int64>,      TypeId.Int64

                typeof<nativeint>,  TypeId.NativeInt
                typeof<unativeint>, TypeId.UNativeInt

                typeof<float32>,    TypeId.Float32
                typeof<float>,      TypeId.Float64
                typeof<decimal>,    TypeId.Decimal

                typeof<bool>,       TypeId.Bool
                typeof<unit>,       TypeId.Unit
                typeof<string>,     TypeId.String
                typeof<char>,       TypeId.Char
                typeof<Void>,       TypeId.Void

                typeof<V2i>,        TypeId.V2i
                typeof<V3i>,        TypeId.V3i
                typeof<V4i>,        TypeId.V4i
                typeof<V2l>,        TypeId.V2l
                typeof<V3l>,        TypeId.V3l
                typeof<V4l>,        TypeId.V4l
                typeof<V2f>,        TypeId.V2f
                typeof<V3f>,        TypeId.V3f
                typeof<V4f>,        TypeId.V4f
                typeof<V2d>,        TypeId.V2d
                typeof<V3d>,        TypeId.V3d
                typeof<V4d>,        TypeId.V4d

                typeof<M22i>,       TypeId.M22i
                typeof<M23i>,       TypeId.M23i
                typeof<M33i>,       TypeId.M33i
                typeof<M34i>,       TypeId.M34i
                typeof<M44i>,       TypeId.M44i
                typeof<M22f>,       TypeId.M22f
                typeof<M23f>,       TypeId.M23f
                typeof<M33f>,       TypeId.M33f
                typeof<M34f>,       TypeId.M34f
                typeof<M44f>,       TypeId.M44f
                typeof<M22d>,       TypeId.M22d
                typeof<M23d>,       TypeId.M23d
                typeof<M33d>,       TypeId.M33d
                typeof<M34d>,       TypeId.M34d
                typeof<M44d>,       TypeId.M44d
                typeof<M22l>,       TypeId.M22l
                typeof<M23l>,       TypeId.M23l
                typeof<M33l>,       TypeId.M33l
                typeof<M34l>,       TypeId.M34l
                typeof<M44l>,       TypeId.M44l


            ]

        let private primitiveTypes =
            let d = Dict()
            for KeyValue(k, v) in primitiveIds do
                d.[v] <- k
            d

        type internal SerializerState =
            val mutable public Table : Dict<Type, int>
            val mutable public CurrentId : int
            new() = { Table = Dict(); CurrentId = 0 }


        let rec internal serializeInternal (state : SerializerState) (dst : BinaryWriter) (e : Type) =
            match primitiveIds.TryGetValue e with
            | (true, id) ->
                dst.Write(byte id)
            | _ ->
                if e.IsArray then
                    dst.Write (byte TypeId.Array)
                    dst.Write (byte (e.GetArrayRank()))
                    serializeInternal state dst (e.GetElementType())
                elif e.IsByRef then
                    dst.Write (byte TypeId.ByRef)
                    serializeInternal state dst (e.GetElementType())
                elif e.IsPointer then
                    dst.Write (byte TypeId.Pointer)
                    serializeInternal state dst (e.GetElementType())
                elif FSharpType.IsFunction e then
                    let (dom, img) = FSharpType.GetFunctionElements e
                    dst.Write (byte TypeId.Function)
                    serializeInternal state dst dom
                    serializeInternal state dst img
                elif FSharpType.IsTuple e then
                    let els = FSharpType.GetTupleElements e
                    dst.Write (byte TypeId.Tuple)
                    dst.Write els.Length
                    for e in els do serializeInternal state dst e
                elif e.IsGenericParameter then
                    dst.Write (byte TypeId.TPar)
                    dst.Write e.GenericParameterPosition
                elif e.IsGenericType then
                    match state.Table.TryGetValue e with
                    | (true, id) ->
                        dst.Write (byte TypeId.GenRef)
                        dst.Write id
                    | _ ->
                        let id = state.CurrentId
                        state.CurrentId <- id + 1

                        dst.Write (byte TypeId.Generic)
                        dst.Write id
                        dst.Write e.AssemblyQualifiedName
                    for tp in e.GetGenericArguments() do
                        serializeInternal state dst tp
                else
                    match state.Table.TryGetValue e with
                    | (true, id) ->
                        dst.Write (byte TypeId.Ref)
                        dst.Write id
                    | _ ->
                        let id = state.CurrentId
                        state.CurrentId <- id + 1

                        dst.Write (byte TypeId.Other)
                        dst.Write id
                        dst.Write e.AssemblyQualifiedName
            ()

        type internal DeserializerState =
            val mutable public Table : Dict<int, Type>
            new() = { Table = Dict() }

        let rec internal deserializeInternal (state : DeserializerState) (src : BinaryReader) =
            let id = src.ReadByte() |> unbox<TypeId>
            match primitiveTypes.TryGetValue id with
            | (true, t) ->
                t
            | _ ->
                match id with
                | TypeId.Array ->
                    let rank = src.ReadByte() |> int
                    let element = deserializeInternal state src
                    element.MakeArrayType(rank)
                | TypeId.ByRef ->
                    let element = deserializeInternal state src
                    element.MakeByRefType()
                | TypeId.Pointer ->
                    let element = deserializeInternal state src
                    element.MakePointerType()
                | TypeId.Function ->
                    let dom = deserializeInternal state src
                    let img = deserializeInternal state src
                    FSharpType.MakeFunctionType(dom, img)
                | TypeId.Tuple ->
                    let cnt = src.ReadInt32()
                    let els = Array.init cnt (fun _ -> deserializeInternal state src)
                    FSharpType.MakeTupleType(els)
                | TypeId.Generic ->
                    let id = src.ReadInt32()
                    let name = src.ReadString()
                
                    // TODO read "`COUNT" from name
                    let t = Type.GetType(name)

                    let t =
                        if t.IsGenericTypeDefinition then t
                        else t.GetGenericTypeDefinition()

                    state.Table.[id] <- t

                    let cnt = t.GetGenericArguments().Length
                    let targs = Array.init cnt (fun _ -> deserializeInternal state src)
                    t.MakeGenericType(targs)
                | TypeId.Other ->
                    let id = src.ReadInt32()
                    let name = src.ReadString()
                    let t = Type.GetType name
                    state.Table.[id] <- t
                    t
                | TypeId.GenRef ->
                    let id = src.ReadInt32()
                    let t = state.Table.[id]
                
                    let cnt = t.GetGenericArguments().Length
                    let targs = Array.init cnt (fun _ -> deserializeInternal state src)
                    t.MakeGenericType(targs)

                | TypeId.Ref ->
                    let id = src.ReadInt32()
                    state.Table.[id]
                

                | id ->
                    failwithf "unexpected TypeId: %A" id
  
        let serialize (dst : Stream) (t : Type) =
            use w = new BinaryWriter(dst, System.Text.Encoding.UTF8, true)
            serializeInternal (SerializerState()) w t
    
        let deserialize (src : Stream) =
            use r = new BinaryReader(src, System.Text.Encoding.UTF8, true)
            deserializeInternal (DeserializerState()) r

        let tryDeserialize (src : Stream) =
            try deserialize src |> Some
            with _ -> None

    module Value =

        let private badTypes = System.Collections.Generic.HashSet<Type>()

        let private blitCache = System.Collections.Concurrent.ConcurrentDictionary<Type, bool>()

        let private isBlittable (t : Type) =
            blitCache.GetOrAdd(t, fun t ->
                let arr = System.Array.CreateInstance(t, 1)
                try
                    let gc = GCHandle.Alloc(arr, GCHandleType.Pinned)
                    gc.Free()
                    true
                with _ ->
                    false
            )


        let rec serialize (dst : BinaryWriter) (typ : Type) (o : obj) : unit =
            match o with
            | :? int8 as o -> dst.Write o
            | :? int16 as o -> dst.Write o
            | :? int32 as o -> dst.Write o
            | :? int64 as o -> dst.Write o
            | :? uint8 as o -> dst.Write o
            | :? uint16 as o -> dst.Write o
            | :? uint32 as o -> dst.Write o
            | :? uint64 as o -> dst.Write o
            | :? nativeint as o -> dst.Write (int64 o)
            | :? unativeint as o -> dst.Write (uint64 o)
            | :? float32 as o -> dst.Write o
            | :? float as o -> dst.Write o
            | :? decimal as o -> dst.Write o
            | :? bool as o -> dst.Write o
            | :? char as o -> dst.Write o
            | :? string as o -> dst.Write o

            | :? V2i as o -> dst.Write o.X; dst.Write o.Y
            | :? V3i as o -> dst.Write o.X; dst.Write o.Y; dst.Write o.Z
            | :? V4i as o -> dst.Write o.X; dst.Write o.Y; dst.Write o.Z; dst.Write o.W
            | :? V2l as o -> dst.Write o.X; dst.Write o.Y
            | :? V3l as o -> dst.Write o.X; dst.Write o.Y; dst.Write o.Z
            | :? V4l as o -> dst.Write o.X; dst.Write o.Y; dst.Write o.Z; dst.Write o.W
            | :? V2f as o -> dst.Write o.X; dst.Write o.Y
            | :? V3f as o -> dst.Write o.X; dst.Write o.Y; dst.Write o.Z
            | :? V4f as o -> dst.Write o.X; dst.Write o.Y; dst.Write o.Z; dst.Write o.W
            | :? V2d as o -> dst.Write o.X; dst.Write o.Y
            | :? V3d as o -> dst.Write o.X; dst.Write o.Y; dst.Write o.Z
            | :? V4d as o -> dst.Write o.X; dst.Write o.Y; dst.Write o.Z; dst.Write o.W

            | :? UniformScope as o ->
                let rec all (acc : list<string>) (u : UniformScope) =
                    match u.Parent with
                    | None -> u.Name :: acc
                    | Some p -> all (u.Name :: acc) p
                let scope = all [] o
                dst.Write (List.length scope)
                for s in scope do dst.Write s

            | _ ->
                if typ = typeof<unit> then
                    ()

                elif typ.IsEnum then 
                    let o = Convert.ToInt32 o
                    dst.Write o

                elif typ.IsArray then
                    if isNull o then
                        dst.Write 0uy
                    else
                        dst.Write 1uy
                        let o = o :?> System.Array
                        let et = typ.GetElementType()
                        dst.Write o.Length
                        for i in 0 .. o.Length - 1 do serialize dst et (o.GetValue i)


                elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<option<_>> then
                    if isNull o then 
                        dst.Write 0uy
                    else 
                        dst.Write 1uy
                        let oo = typ.GetProperty("Value").GetValue(o)
                        serialize dst (typ.GetGenericArguments().[0]) oo

                elif typeof<IShaderBuilder>.IsAssignableFrom typ then
                    let v = o :?> IShaderBuilder
                    dst.Write (int v.ShaderStage)
                    match v.OutputTopology with
                    | Some t ->
                        dst.Write 1uy
                        match t with
                        | OutputTopology.Points -> dst.Write 0uy
                        | OutputTopology.LineStrip -> dst.Write 1uy
                        | OutputTopology.TriangleStrip -> dst.Write 2uy
                    | None ->
                        dst.Write 0uy

                elif typ.IsValueType && isBlittable typ then    
                    let res = Array.zeroCreate<byte>(Marshal.SizeOf typ)
                    let gc = GCHandle.Alloc(res, GCHandleType.Pinned)
                    try 
                        Marshal.StructureToPtr(o, gc.AddrOfPinnedObject(), false)
                        dst.Write(res)
                    finally
                        gc.Free()
                    
                elif FSharpType.IsRecord(typ, true) then
                    if isNull o then
                        dst.Write 0uy
                    else
                        dst.Write 1uy
                        let fields = FSharpType.GetRecordFields(typ, true)
                        for f in fields do
                            serialize dst f.PropertyType (f.GetValue(o))

                elif FSharpType.IsTuple typ then
                    if isNull o then
                        dst.Write 0uy
                    else
                        dst.Write 1uy
                        let ts = FSharpType.GetTupleElements typ
                        for i in 0 .. ts.Length - 1 do
                            let v = FSharpValue.GetTupleField(o, i)
                            serialize dst ts.[i] v

                elif FSharpType.IsUnion(typ, true) then
                    if isNull o then
                        dst.Write 0uy
                    else
                        dst.Write 1uy
                        let c, args = FSharpValue.GetUnionFields(o, typ, true)
                        dst.Write c.Tag
                        let ts = c.GetFields()
                        for i in 0 .. args.Length - 1 do 
                            let v = args.[i]
                            let t = ts.[i].PropertyType
                            serialize dst t v

                elif isNull o then
                    dst.Write 0uy
                else
                    dst.Write 1uy
                    let fields = 
                        typ.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                        |> Array.sortBy (fun f -> f.Name)

                    for f in fields do
                        serialize dst f.FieldType (f.GetValue(o))
        

        let private deserializeTable =
            Dict.ofList [
                typeof<int8>, fun (src : BinaryReader) -> src.ReadSByte() :> obj
                typeof<unit>, fun (src : BinaryReader) -> () :> obj
                typeof<int8>, fun (src : BinaryReader) -> src.ReadSByte() :> obj
                typeof<int16>, fun (src : BinaryReader) -> src.ReadInt16() :> obj
                typeof<int32>, fun (src : BinaryReader) -> src.ReadInt32() :> obj
                typeof<int64>, fun (src : BinaryReader) -> src.ReadInt64() :> obj
                typeof<uint8>, fun (src : BinaryReader) -> src.ReadByte() :> obj
                typeof<uint16>, fun (src : BinaryReader) -> src.ReadUInt16() :> obj
                typeof<uint32>, fun (src : BinaryReader) -> src.ReadUInt32() :> obj
                typeof<uint64>, fun (src : BinaryReader) -> src.ReadUInt64() :> obj
                typeof<nativeint>, fun (src : BinaryReader) -> src.ReadInt64() |> nativeint :> obj
                typeof<unativeint>, fun (src : BinaryReader) -> src.ReadUInt64() |> unativeint :> obj
                typeof<float32>, fun (src : BinaryReader) -> src.ReadSingle() :> obj
                typeof<float>, fun (src : BinaryReader) -> src.ReadDouble() :> obj
                typeof<decimal>, fun (src : BinaryReader) -> src.ReadDecimal() :> obj
                typeof<bool>, fun (src : BinaryReader) -> src.ReadBoolean() :> obj
                typeof<char>, fun (src : BinaryReader) -> src.ReadChar() :> obj
                typeof<string>, fun (src : BinaryReader) -> src.ReadString() :> obj

                typeof<V2i>, fun (src : BinaryReader) -> V2i(src.ReadInt32(), src.ReadInt32())
                typeof<V3i>, fun (src : BinaryReader) -> V3i(src.ReadInt32(), src.ReadInt32(), src.ReadInt32())
                typeof<V4i>, fun (src : BinaryReader) -> V4i(src.ReadInt32(), src.ReadInt32(), src.ReadInt32(), src.ReadInt32())
                typeof<V2l>, fun (src : BinaryReader) -> V2l(src.ReadInt64(), src.ReadInt64())
                typeof<V3l>, fun (src : BinaryReader) -> V3l(src.ReadInt64(), src.ReadInt64(), src.ReadInt64())
                typeof<V4l>, fun (src : BinaryReader) -> V4l(src.ReadInt64(), src.ReadInt64(), src.ReadInt64(), src.ReadInt64())
                typeof<V2f>, fun (src : BinaryReader) -> V2f(src.ReadSingle(), src.ReadSingle())
                typeof<V3f>, fun (src : BinaryReader) -> V3f(src.ReadSingle(), src.ReadSingle(), src.ReadSingle())
                typeof<V4f>, fun (src : BinaryReader) -> V4f(src.ReadSingle(), src.ReadSingle(), src.ReadSingle(), src.ReadSingle())
                typeof<V2d>, fun (src : BinaryReader) -> V2d(src.ReadDouble(), src.ReadDouble())
                typeof<V3d>, fun (src : BinaryReader) -> V3d(src.ReadDouble(), src.ReadDouble(), src.ReadDouble())
                typeof<V4d>, fun (src : BinaryReader) -> V4d(src.ReadDouble(), src.ReadDouble(), src.ReadDouble(), src.ReadDouble())
            ]

        let rec deserialize (src : BinaryReader) (typ : Type) : obj =
            match deserializeTable.TryGetValue typ with
            | (true, read) -> read src
            | _ ->

                if typ.IsEnum then
                    let v = src.ReadInt32()
                    Enum.ToObject(typ, v)
            
                elif typ.IsArray then
                    match src.ReadByte() with
                    | 0uy ->
                        null
                    | _ ->
                        let et = typ.GetElementType()
                        let cnt = src.ReadInt32()
                        let res = System.Array.CreateInstance(et, cnt)
                        for i in 0 .. res.Length - 1 do res.SetValue(deserialize src et, i)
                        res :> obj

           

                elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<option<_>> then   
                    let hasValue = src.ReadByte()
                    if hasValue <> 0uy then
                        deserialize src (typ.GetGenericArguments().[0])
                    else
                        null
                    
                elif typeof<IShaderBuilder>.IsAssignableFrom typ then
                    let stage = src.ReadInt32() |> unbox<ShaderStage>

                    match src.ReadByte() with
                    | 0uy ->
                        match stage with
                        | ShaderStage.Vertex -> vertex :> obj
                        | ShaderStage.TessControl -> tessellation :> obj
                        | ShaderStage.TessEval -> tessellation :> obj
                        | ShaderStage.Fragment -> fragment :> obj
                        | ShaderStage.Compute -> compute :> obj
                        | ShaderStage.RayGeneration -> raygen :> obj
                        | ShaderStage.Intersection -> intersection :> obj
                        | ShaderStage.AnyHit -> anyHit :> obj
                        | ShaderStage.ClosestHit -> closestHit :> obj
                        | ShaderStage.Miss -> miss :> obj
                        | ShaderStage.Callable -> callable :> obj
                        | _ -> failwith "bad shader-builder value"
                    | _ ->
                        match src.ReadByte() with
                        | 0uy -> point :> obj
                        | 1uy -> line :> obj
                        | _ -> triangle :> obj
                    
                elif typ.IsValueType && isBlittable typ then    
                    let data = src.ReadBytes(Marshal.SizeOf typ)
                    let gc = GCHandle.Alloc(data, GCHandleType.Pinned)
                    try Marshal.PtrToStructure(gc.AddrOfPinnedObject(), typ)
                    finally gc.Free()
                
                elif FSharpType.IsRecord(typ, true) then
                    match src.ReadByte() with
                    | 0uy -> 
                        null
                    | _ ->
                        let fields = FSharpType.GetRecordFields(typ, true)
                        let args =
                            fields |> Array.map (fun f ->
                                deserialize src f.PropertyType
                            )
                        FSharpValue.MakeRecord(typ, args, true)

                elif FSharpType.IsTuple typ then
            
                    match src.ReadByte() with
                    | 0uy -> 
                        null
                    | _ ->
                        let ts = FSharpType.GetTupleElements typ

                        FSharpValue.MakeTuple(
                            ts |> Array.map (fun t -> deserialize src t),
                            typ
                        )
                elif FSharpType.IsUnion(typ, true) then
            
                    match src.ReadByte() with
                    | 0uy -> 
                        null
                    | _ ->
                        let tag = src.ReadInt32()
                        let c = FSharpType.GetUnionCases(typ, true) |> Array.find (fun c -> c.Tag = tag)
                        let ts = c.GetFields()
                        let args =
                            ts |> Array.map (fun t ->
                                deserialize src t.PropertyType
                            )
                        FSharpValue.MakeUnion(c, args, true)

                else
                    match src.ReadByte() with
                    | 0uy -> 
                        null
                    | _ ->
                        let first = lock badTypes (fun () -> badTypes.Add typ)
                        if first then Log.warn "[FShade] slow, reflection-based serialization of %A: please report as FShade issue via github" typ
                        let fields = 
                            typ.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                            |> Array.sortBy (fun f -> f.Name)
                        let res = System.Runtime.Serialization.FormatterServices.GetUninitializedObject(typ)
                        for f in fields do
                            let value = deserialize src f.FieldType
                            f.SetValue(res, value)
                        res

    module rec Expr =
        open System.Security.Cryptography

        type private ExprId =   
            | Var = 0
            | VarId = 1
            | AddressOf = 2
            | AddressSet = 3
            | Application = 4
            | InstanceCall = 5
            | StaticCall = 6
            | Coerce = 7
            | DefaultValue = 8
            | InstanceFieldGet = 9
            | StaticFieldGet = 10
            | InstanceFieldSet = 11
            | StaticFieldSet = 12
            | ForInteger = 13
            | IfThenElse = 14
            | Lambda = 15
            | Let = 16
            | LetRec = 17
            | NewArray = 18
            | NewDelegate = 19
            | NewObj = 20
            | NewRecord = 21
            | NewTuple = 22
            | NewUnionCase = 23
            | StaticPropertyGet = 24
            | InstancePropertyGet = 25
            | StaticPropertySet = 26
            | InstancePropertySet = 27
            | QuoteRaw = 28
            | QuoteTyped = 29
            | Sequential = 30
            | TryFinally = 31
            | TryWith = 32
            | TupleGet = 33
            | TypeTest = 34
            | UnionCaseTest = 35
            | ValueWithName = 36
            | Value = 37
            | VarSet = 38
            | WhileLoop = 39

            | ReadInput = 40
            | WriteOutputs = 41
            | ReflectedCall = 42
            | CallFunction = 43

        type internal SerializerState =
            val mutable public IsHash : bool
            val mutable public TypeState : Type.SerializerState
            val mutable public VariableId : int
            val mutable public VariableIds : Map<Var, int>
            new(hash) = { IsHash = hash; TypeState = Type.SerializerState(); VariableId = 0; VariableIds = Map.empty }


        module private UtilityFunction =
            let serialize (state : SerializerState) (dst : BinaryWriter) (u : UtilityFunction) =
                dst.Write u.functionId
                dst.Write u.functionName
            
                dst.Write (List.length u.functionArguments)
                for v in u.functionArguments do
                    let id = state.VariableId
                    dst.Write id
                    dst.Write v.Name
                    Type.serializeInternal state.TypeState dst v.Type
                    dst.Write v.IsMutable
            
                    state.VariableId <- id + 1
                    state.VariableIds <- Map.add v id state.VariableIds
            
                Expr.serializeInternal state dst u.functionBody

                for v in u.functionArguments do state.VariableIds <- Map.remove v state.VariableIds

                match u.functionMethod with
                | Some m ->
                    dst.Write 1uy
                    Type.serializeInternal state.TypeState dst m.DeclaringType
                    dst.Write m.MetadataToken
                    if m.IsGenericMethod then
                        for a in m.GetGenericArguments() do
                            Type.serializeInternal state.TypeState dst a
                | None ->
                    dst.Write 0uy

                // TODO: functionTag???

                dst.Write u.functionIsInline

            let deserialize (state : DeserializerState) (src : BinaryReader) =
                let functionId = src.ReadString()
                let functionName = src.ReadString()

                let functionArguments = 
                    let cnt = src.ReadInt32() 
                    List.init cnt (fun _ ->
                       let id = src.ReadInt32()
                       let name = src.ReadString()
                       let typ = Type.deserializeInternal state.TypeState src
                       let isMutable = src.ReadBoolean()
                       let v = Var(name, typ, isMutable)
                       state.Variables <- Map.add id v state.Variables
                       id, v
                    )

                let functionBody = Expr.deserializeInternal state src

                for id, _ in functionArguments do state.Variables <- Map.remove id state.Variables

                let functionMethod =
                    match src.ReadByte() with
                    | 0uy ->
                        None
                    | _ ->
                        let typ = Type.deserializeInternal state.TypeState src
                        let token = src.ReadInt32()

                        let m = typ.Module.ResolveMethod(token)
                        let method =
                            if m.IsGenericMethod then
                                let m = m :?> MethodInfo
                                let targs = Array.init (m.GetGenericArguments().Length) (fun _ -> Type.deserializeInternal state.TypeState src)
                                if m.IsGenericMethodDefinition then m.MakeGenericMethod(targs) :> MethodBase
                                else m.GetGenericMethodDefinition().MakeGenericMethod(targs) :> MethodBase
                            else
                                m
                        Some method

                let functionIsInline = src.ReadBoolean()
                {
                    functionId = functionId
                    functionName = functionName
                    functionArguments = List.map snd functionArguments
                    functionBody = functionBody
                    functionMethod = functionMethod
                    functionTag = null
                    functionIsInline = functionIsInline
                }




        module private Patterns =
            open Microsoft.FSharp.Quotations.Patterns
            let (|SimpleValue|_|) (e : Expr) =
                try
                    match e with
                        | FieldGet(None, f) -> Some (f.Name, f.GetValue(null))
                        | PropertyGet(None, pi, []) -> 
                            System.Runtime.CompilerServices.RuntimeHelpers.RunClassConstructor(pi.DeclaringType.TypeHandle)

                            Some (pi.Name, pi.GetValue(null))
                        | Call(None, mi, []) -> Some (mi.Name, mi.Invoke(null, [||]))
                        | NewObject(ctor, []) -> Some (ctor.DeclaringType.Name, ctor.Invoke([||]))
                        | _ -> None
                with _ ->
                    None
            let (|ReflectedCall|_|) (e : Expr) =
                match e with
                    | Call(t,mi,args) ->
                        let isInline = mi.GetCustomAttributes<InlineAttribute>() |> Seq.isEmpty |> not
                        match ExprWorkardound.TryGetReflectedDefinition mi with
                            | Some def -> 
                                let args = 
                                    match t with
                                        | Some t -> t :: args
                                        | None -> args
                                Some (isInline, mi, def, args)
                            | None ->
                                None
                    | _ ->
                        None

            let (|IntrinsicCall|_|) (e : Expr) =
                match e with
                    | Call(t,mi,args) ->
                        let att = mi.GetCustomAttributes(typeof<IntrinsicAttribute>, true) |> Seq.map unbox<IntrinsicAttribute> |> Seq.toList
                        match att with
                            | [] ->
                                None
                            | i -> 
                                let args = 
                                    match t with
                                        | Some t -> t :: args
                                        | None -> args
                                let intr = i |> List.map (fun i -> i.Intrinsic)

                                Some (intr, args)
                    | _ ->
                        None


        let rec internal serializeInternal (state : SerializerState) (dst : BinaryWriter) (e : Expr) =
            match e with

        
            | Patterns.Call(None, mi, [ExprValue v]) when (mi.Name = "op_Splice" || mi.Name = "op_SpliceUntyped") && state.IsHash ->
                serializeInternal state dst v

            | Patterns.IntrinsicCall(atts, args) when state.IsHash ->
                for a in atts do
                    if isNull a.intrinsicName then 
                        dst.Write 0uy
                    else
                        dst.Write 1uy
                        dst.Write a.intrinsicName

                    match a.tag with
                    | :? string as tag ->
                        dst.Write 1uy
                        dst.Write tag
                    | _ ->
                        dst.Write 0uy

                for a in args do
                    serializeInternal state dst a

            | Patterns.ReflectedCall(isInline, mi, def, args) ->
                dst.Write (byte ExprId.ReflectedCall)
                Type.serializeInternal state.TypeState dst mi.DeclaringType
                dst.Write mi.MetadataToken
                if mi.IsGenericMethod then
                    for a in mi.GetGenericArguments() do Type.serializeInternal state.TypeState dst a

                dst.Write isInline
                serializeInternal state dst def
                for a in args do serializeInternal state dst a

            | CallFunction(f, args) ->
                dst.Write (byte ExprId.CallFunction)
                UtilityFunction.serialize state dst f
                dst.Write (List.length args)
                for a in args do serializeInternal state dst a

            | Patterns.SimpleValue(name, value)  when state.IsHash ->
                dst.Write name
                Value.serialize dst e.Type value

            | Patterns.WithValue(_, _, e) ->
                serializeInternal state dst e

            | WriteOutputs(op) ->
                dst.Write (byte ExprId.WriteOutputs)
                dst.Write (Map.count op)

                for KeyValue(name, (index, value)) in op do
                    dst.Write name
                    match index with
                    | Some index ->
                        dst.Write 1uy
                        serializeInternal state dst index
                    | None ->
                        dst.Write 0uy

                    serializeInternal state dst value

            | ReadInput(kind, name, index) ->
                dst.Write (byte ExprId.ReadInput)
                Type.serializeInternal state.TypeState dst e.Type
                dst.Write(int kind)
                dst.Write name
                match index with
                | Some index ->
                    dst.Write 1uy
                    serializeInternal state dst index
                | None ->
                    dst.Write 0uy

                
            | ForInteger(v, l, s, h, body) ->
                let id = state.VariableId

                dst.Write (byte ExprId.ForInteger)
                dst.Write id
                dst.Write v.Name
                serializeInternal state dst l
                serializeInternal state dst s
                serializeInternal state dst h

                state.VariableId <- id + 1
                state.VariableIds <- Map.add v id state.VariableIds
                serializeInternal state dst body
                state.VariableIds <- Map.remove v state.VariableIds
               

            | Patterns.Var v ->
                match Map.tryFind v state.VariableIds with
                | Some vid ->
                    dst.Write(byte ExprId.VarId)
                    dst.Write vid
                | None ->
                    dst.Write(byte ExprId.Var)
                    let id = state.VariableId
                    state.VariableId <- id + 1
                    state.VariableIds <- Map.add v id state.VariableIds
                    dst.Write id
                    dst.Write v.Name
                    Type.serializeInternal state.TypeState dst v.Type
                    dst.Write v.IsMutable


            | Patterns.AddressOf e ->
                dst.Write (byte ExprId.AddressOf)
                serializeInternal state dst e

            | Patterns.AddressSet(a, b) ->
                dst.Write (byte ExprId.AddressSet)
                serializeInternal state dst a
                serializeInternal state dst b

            | Patterns.Application(a, b) ->
                dst.Write (byte ExprId.Application)
                serializeInternal state dst a
                serializeInternal state dst b

            | Patterns.Call(target, mi, args) ->
                match target with
                | Some t -> 
                    dst.Write (byte ExprId.InstanceCall)
                    serializeInternal state dst t
                | None ->
                    dst.Write (byte ExprId.StaticCall)
                    Type.serializeInternal state.TypeState dst mi.DeclaringType

                if mi.IsGenericMethod then
                    dst.Write 1uy
                    let ps = mi.GetGenericArguments()
                    dst.Write ps.Length
                    for p in ps do Type.serializeInternal state.TypeState dst p
                else
                    dst.Write 0uy

                dst.Write mi.MetadataToken
                for a in args do serializeInternal state dst a
            
            | Patterns.Coerce(e, t) ->
                dst.Write (byte ExprId.Coerce)
                serializeInternal state dst e
                Type.serializeInternal state.TypeState dst t

            | Patterns.DefaultValue t ->
                dst.Write (byte ExprId.DefaultValue)
                Type.serializeInternal state.TypeState dst t

            | Patterns.FieldGet(target, field) ->
                match target with
                | Some target ->
                    dst.Write(byte ExprId.InstanceFieldGet)
                    serializeInternal state dst target
                | None ->
                    dst.Write(byte ExprId.StaticFieldGet)
                    Type.serializeInternal state.TypeState dst field.DeclaringType
                dst.Write field.MetadataToken

            | Patterns.FieldSet(target, field, value) ->
                match target with
                | Some target ->
                    dst.Write(byte ExprId.InstanceFieldSet)
                    serializeInternal state dst target
                | None ->
                    dst.Write(byte ExprId.StaticFieldSet)
                    Type.serializeInternal state.TypeState dst field.DeclaringType
                dst.Write field.MetadataToken
                serializeInternal state dst value
             
            | Patterns.IfThenElse(c, i, e) ->
                dst.Write (byte ExprId.IfThenElse)
                serializeInternal state dst c
                serializeInternal state dst i
                serializeInternal state dst e

            | Patterns.Lambda(v, b) ->
                let id = state.VariableId
            
                dst.Write (byte ExprId.Lambda)
                dst.Write id
                dst.Write v.Name
                Type.serializeInternal state.TypeState dst v.Type
                dst.Write v.IsMutable
            
                state.VariableId <- id + 1
                state.VariableIds <- Map.add v id state.VariableIds
                serializeInternal state dst b
                state.VariableIds <- Map.remove v state.VariableIds

            | Patterns.Let(v, e, b) ->
            
                dst.Write (byte ExprId.Let)
                let id = state.VariableId
                dst.Write id
                dst.Write v.Name
                Type.serializeInternal state.TypeState dst v.Type
                dst.Write v.IsMutable
            
                serializeInternal state dst e
                state.VariableId <- id + 1
                state.VariableIds <- Map.add v id state.VariableIds
                serializeInternal state dst b
                state.VariableIds <- Map.remove v state.VariableIds
            
            | Patterns.LetRecursive(bindings, body) ->
        
                dst.Write (byte ExprId.LetRec)

                dst.Write(List.length bindings)
                for (v, _) in bindings do
                    let id = state.VariableId
                    dst.Write id
                    dst.Write v.Name
                    Type.serializeInternal state.TypeState dst v.Type
                    dst.Write v.IsMutable
                    state.VariableIds <- Map.add v id state.VariableIds

                for (_, e) in bindings do
                    serializeInternal state dst e
            
                serializeInternal state dst body

            
                for (v, _) in bindings do
                    state.VariableIds <- Map.remove v state.VariableIds

            | Patterns.NewArray(t, es) ->
                dst.Write (byte ExprId.NewArray)
            
                dst.Write(List.length es)
                Type.serializeInternal state.TypeState dst t
                for e in es do serializeInternal state dst e

            | Patterns.NewDelegate(t, vs, b) ->
                dst.Write (byte ExprId.NewDelegate)
                Type.serializeInternal state.TypeState dst t

                dst.Write (List.length vs)
                for v in vs do
                    let id = state.VariableId
                    dst.Write id
                    dst.Write v.Name
                    Type.serializeInternal state.TypeState dst v.Type
                    dst.Write v.IsMutable
                    state.VariableIds <- Map.add v id state.VariableIds
                
                serializeInternal state dst b

                for v in vs do
                    state.VariableIds <- Map.remove v state.VariableIds
                
            | Patterns.NewObject(ctor, args) ->
                dst.Write (byte ExprId.NewObj)
                Type.serializeInternal state.TypeState dst ctor.DeclaringType
                dst.Write ctor.MetadataToken

                for a in args do serializeInternal state dst a

            | Patterns.NewRecord(typ, args) ->
                dst.Write (byte ExprId.NewRecord)
                Type.serializeInternal state.TypeState dst typ
                for a in args do serializeInternal state dst a

            | Patterns.NewTuple args ->
                dst.Write (byte ExprId.NewTuple)
                let cnt = List.length args
                dst.Write cnt
                for a in args do serializeInternal state dst a

            | Patterns.NewUnionCase(case, args) ->
                dst.Write (byte ExprId.NewUnionCase)
                Type.serializeInternal state.TypeState dst case.DeclaringType
                dst.Write case.Tag
                for a in args do serializeInternal state dst a

            | Patterns.PropertyGet(target, prop, indices) ->
                match target with
                | Some target ->
                    dst.Write (byte ExprId.InstancePropertyGet)
                    serializeInternal state dst target
                | None ->
                    dst.Write (byte ExprId.StaticPropertyGet)
                    Type.serializeInternal state.TypeState dst prop.DeclaringType

                dst.Write prop.Name
                for i in indices do serializeInternal state dst i
            
            | Patterns.PropertySet(target, prop, indices, value) ->
                match target with
                | Some target ->
                    dst.Write (byte ExprId.InstancePropertySet)
                    serializeInternal state dst target
                | None ->
                    dst.Write (byte ExprId.StaticPropertySet)
                    Type.serializeInternal state.TypeState dst prop.DeclaringType

                dst.Write prop.Name
                for i in indices do serializeInternal state dst i
                serializeInternal state dst value

            | Patterns.QuoteRaw e ->
                dst.Write (byte ExprId.QuoteRaw)
                serializeInternal state dst e
            
            | Patterns.QuoteTyped e ->
                dst.Write (byte ExprId.QuoteTyped)
                serializeInternal state dst e

            | Patterns.Sequential(a, b) ->
                dst.Write (byte ExprId.Sequential)
                serializeInternal state dst a
                serializeInternal state dst b

            | Patterns.TryFinally(a, b) ->
                dst.Write (byte ExprId.TryFinally)
                serializeInternal state dst a
                serializeInternal state dst b
            
            | Patterns.TryWith(body,filterVar,filterBody,catchVar,catchBody) ->
                dst.Write (byte ExprId.TryWith)

                serializeInternal state dst body

                let id = state.VariableId
                dst.Write id
                dst.Write filterVar.Name
                Type.serializeInternal state.TypeState dst filterVar.Type
                dst.Write filterVar.IsMutable
                state.VariableIds <- Map.add filterVar id state.VariableIds
                serializeInternal state dst filterBody
                state.VariableIds <- Map.remove filterVar state.VariableIds


                let id = state.VariableId
                dst.Write id
                dst.Write catchVar.Name
                Type.serializeInternal state.TypeState dst catchVar.Type
                dst.Write catchVar.IsMutable
                state.VariableIds <- Map.add catchVar id state.VariableIds
                serializeInternal state dst catchBody
                state.VariableIds <- Map.remove catchVar state.VariableIds

            | Patterns.TupleGet(e, i) ->
                dst.Write (byte ExprId.TupleGet)
                serializeInternal state dst e
                dst.Write i

            | Patterns.TypeTest(e, t) ->
                dst.Write (byte ExprId.TypeTest)
                serializeInternal state dst e
                Type.serializeInternal state.TypeState dst t
            
            | Patterns.UnionCaseTest(e, c) ->
                dst.Write (byte ExprId.UnionCaseTest)
                serializeInternal state dst e

                Type.serializeInternal state.TypeState dst c.DeclaringType
                dst.Write c.Tag

            | Patterns.ValueWithName(value, typ, _) | Patterns.Value(value, typ) ->
                match e with
                | Patterns.ValueWithName(_,_,name) ->
                    dst.Write (byte ExprId.ValueWithName)
                    dst.Write name
                | _ ->
                    dst.Write (byte ExprId.Value)
                
                Type.serializeInternal state.TypeState dst typ
                Value.serialize dst typ value
            
            | Patterns.VarSet(v, e) ->
                dst.Write (byte ExprId.VarSet)
                let vid = state.VariableIds.[v]
                dst.Write vid
                serializeInternal state dst e

            | Patterns.WhileLoop(guard, body) ->
                dst.Write (byte ExprId.WhileLoop)
                serializeInternal state dst guard
                serializeInternal state dst body
            
            | _ ->
                failwithf "%A not implemented" e

        type internal DeserializerState =
            val mutable public TypeState : Type.DeserializerState
            val mutable public Variables : Map<int, Var>
            new() = { TypeState = Type.DeserializerState(); Variables = Map.empty }

        let rec internal deserializeInternal (state : DeserializerState) (src : BinaryReader) =
            let id = src.ReadByte() |> int |> unbox<ExprId>
            match id with

            | ExprId.CallFunction ->
                let f = UtilityFunction.deserialize state src
                let args =
                    let c = src.ReadInt32()
                    List.init c (fun _ ->
                        deserializeInternal state src
                    )

                Expr.CallFunction(f, args)
            

            | ExprId.ReflectedCall ->
                let decl = Type.deserializeInternal state.TypeState src
                let method = 
                    let token = src.ReadInt32()
                    let m = decl.Module.ResolveMethod(token) :?> MethodInfo
                    if m.IsGenericMethod then
                        let targs = Array.init (m.GetGenericArguments().Length) (fun _ -> Type.deserializeInternal state.TypeState src)
                        m.MakeGenericMethod targs
                    else
                        m
                let isInline = src.ReadBoolean()
                let def = deserializeInternal state src
                let args = List.init (method.GetParameters().Length) (fun _ -> deserializeInternal state src)
                Expr.Call(method, args)


            | ExprId.WriteOutputs ->
                let cnt = src.ReadInt32()

                let outputs =
                    List.init cnt (fun _ ->
                        let name = src.ReadString()
                        let index = 
                            match src.ReadByte() with
                            | 0uy -> None
                            | _ -> Some (deserializeInternal state src)
                        let value = deserializeInternal state src
                        name, (index, value)
                    )
                Expr.WriteOutputs(Map.ofList outputs)

            | ExprId.ReadInput ->
                let typ = Type.deserializeInternal state.TypeState src
                let kind = src.ReadInt32() |> unbox<ParameterKind>
                let name = src.ReadString()
            
                let index = 
                    match src.ReadByte() with
                    | 0uy -> None
                    | _ -> Some (deserializeInternal state src)

                match index with
                | Some index -> Expr.ReadInput(kind, typ, name, index)
                | None -> Expr.ReadInput(kind, typ, name)

            | ExprId.VarId ->
                let id = src.ReadInt32()
                Expr.Var (Map.find id state.Variables)

            | ExprId.Var ->
                let id = src.ReadInt32()
                let name = src.ReadString()
                let typ = Type.deserializeInternal state.TypeState src
                let isMutable = src.ReadBoolean()
                let v = Var(name, typ, isMutable)
                state.Variables <- Map.add id v state.Variables
                Expr.Var v

            | ExprId.AddressOf ->
                let inner = deserializeInternal state src
                Expr.AddressOf inner

            | ExprId.AddressSet ->
                let a = deserializeInternal state src
                let b = deserializeInternal state src
                Expr.AddressSet(a, b)

            | ExprId.Application ->
                let a = deserializeInternal state src
                let b = deserializeInternal state src
                Expr.Application(a, b)
            
            | ExprId.StaticCall | ExprId.InstanceCall ->
                let target, declaringType = 
                    match id with
                    | ExprId.StaticCall ->
                        let t = Type.deserializeInternal state.TypeState src
                        None, t
                    | _ ->
                        let target = deserializeInternal state src
                        Some target, target.Type

                let m = declaringType.Module
                let targs =
                    match src.ReadByte() with
                    | 0uy -> [||]
                    | _ -> Array.init (src.ReadInt32()) (fun _ -> Type.deserializeInternal state.TypeState src)
                let token = src.ReadInt32()

                let method = 
                    if declaringType.IsGenericType then
                        m.ResolveMethod(token, declaringType.GetGenericArguments(), targs) :?> MethodInfo
                    elif targs.Length > 0 then
                        m.ResolveMethod(token, [||], targs) :?> MethodInfo
                    else
                        m.ResolveMethod(token) :?> MethodInfo

                let method =
                    if targs.Length > 0 then method.MakeGenericMethod targs
                    else method

                let args =
                    List.init (method.GetParameters().Length) (fun _ -> deserializeInternal state src)

                if method.ContainsGenericParameters then failwith "generic method"

                match target with
                | Some target -> Expr.Call(target, method, args)
                | None -> Expr.Call(method, args)
            
            | ExprId.Coerce ->
                let e = deserializeInternal state src
                let t = Type.deserializeInternal state.TypeState src
                Expr.Coerce(e, t)

            | ExprId.DefaultValue ->
                let t = Type.deserializeInternal state.TypeState src
                Expr.DefaultValue t

            | ExprId.StaticFieldGet ->
                let t = Type.deserializeInternal state.TypeState src
                let m = t.Module
                let token = src.ReadInt32()
                let field = 
                    if t.IsGenericType then m.ResolveField(token, t.GetGenericArguments(), [||])
                    else m.ResolveField(token)
                Expr.FieldGet(field)
                
            | ExprId.InstanceFieldGet ->
                let target = deserializeInternal state src 
                let t = target.Type
                let m = t.Module
                let token = src.ReadInt32()
                let field = 
                    if t.IsGenericType then m.ResolveField(token, t.GetGenericArguments(), [||])
                    else m.ResolveField(token)
                Expr.FieldGet(target, field)

            | ExprId.StaticFieldSet ->
                let t = Type.deserializeInternal state.TypeState src
                let m = t.Module
                let token = src.ReadInt32()
                let field = 
                    if t.IsGenericType then m.ResolveField(token, t.GetGenericArguments(), [||])
                    else m.ResolveField(token)
                let value = deserializeInternal state src 
                Expr.FieldSet(field, value)
            
            | ExprId.InstanceFieldSet ->
                let target = deserializeInternal state src 
                let t = target.Type
                let m = t.Module
                let token = src.ReadInt32()
                let field = 
                    if t.IsGenericType then m.ResolveField(token, t.GetGenericArguments(), [||])
                    else m.ResolveField(token)
                let value = deserializeInternal state src 
                Expr.FieldSet(target, field, value)
            
            | ExprId.ForInteger ->
                let vid = src.ReadInt32()
                let name = src.ReadString()
                let v = Var(name, typeof<int>)

                let l = deserializeInternal state src
                let s = deserializeInternal state src
                let h = deserializeInternal state src

                state.Variables <- Map.add vid v state.Variables
                let b = deserializeInternal state src
                state.Variables <- Map.remove vid state.Variables
                Expr.ForInteger(v, l, s, h, b)

            | ExprId.IfThenElse ->
                let c = deserializeInternal state src
                let i = deserializeInternal state src
                let e = deserializeInternal state src
                Expr.IfThenElse(c, i, e)

            | ExprId.Lambda ->
                let vid = src.ReadInt32()
                let name = src.ReadString()
                let typ = Type.deserializeInternal state.TypeState src
                let isMutable = src.ReadBoolean()
                let v = Var(name, typ, isMutable)

                state.Variables <- Map.add vid v state.Variables
                let b = deserializeInternal state src
                state.Variables <- Map.remove vid state.Variables
                Expr.Lambda(v, b)
            
            | ExprId.Let ->
                let vid = src.ReadInt32()
                let name = src.ReadString()
                let typ = Type.deserializeInternal state.TypeState src
                let isMutable = src.ReadBoolean()
                let v = Var(name, typ, isMutable)

                let e = deserializeInternal state src

                state.Variables <- Map.add vid v state.Variables
                let b = deserializeInternal state src
                state.Variables <- Map.remove vid state.Variables
                Expr.Let(v, e, b)

            | ExprId.LetRec ->
                let cnt = src.ReadInt32()
                let vs = 
                    List.init cnt (fun _ ->
                        let vid = src.ReadInt32()
                        let name = src.ReadString()
                        let typ = Type.deserializeInternal state.TypeState src
                        let isMutable = src.ReadBoolean()
                        let v = Var(name, typ, isMutable)
                        vid, v
                    )

                for (i, v) in vs do state.Variables <- Map.add i v state.Variables
                let es = List.init cnt (fun _ -> deserializeInternal state src)
                let body = deserializeInternal state src
                for (i, _) in vs do state.Variables <- Map.remove i state.Variables

                Expr.LetRecursive((vs, es) ||> List.map2 (fun (_, v) e -> v, e), body)

            | ExprId.NewArray ->
                let cnt = src.ReadInt32()
                let t = Type.deserializeInternal state.TypeState src
                let es = List.init cnt (fun _ -> deserializeInternal state src)
                Expr.NewArray(t, es)

            | ExprId.NewDelegate ->
                let t = Type.deserializeInternal state.TypeState src
                let cnt = src.ReadInt32()

                let vs =
                    List.init cnt (fun _ ->
                        let vid = src.ReadInt32()
                        let name = src.ReadString()
                        let typ = Type.deserializeInternal state.TypeState src
                        let isMutable = src.ReadBoolean()
                        let v = Var(name, typ, isMutable)
                        vid, v
                    )

                for (i, v) in vs do state.Variables <- Map.add i v state.Variables
                let body = deserializeInternal state src
                for (i, _) in vs do state.Variables <- Map.remove i state.Variables
                Expr.NewDelegate(t, List.map snd vs, body)

            | ExprId.NewObj ->
                let t = Type.deserializeInternal state.TypeState src
                let token = src.ReadInt32()
                let ctor = t.Module.ResolveMember(token) :?> ConstructorInfo

                let cnt = ctor.GetParameters().Length
                let args = List.init cnt (fun _ -> deserializeInternal state src)
                Expr.NewObject(ctor, args)

            | ExprId.NewRecord ->
                let t = Type.deserializeInternal state.TypeState src
                let cnt = FSharpType.GetRecordFields(t, true).Length
                let args = List.init cnt (fun _ -> deserializeInternal state src)
                Expr.NewRecord(t, args)

            | ExprId.NewTuple ->
                let cnt = src.ReadInt32()
                let args = List.init cnt (fun _ -> deserializeInternal state src)
                Expr.NewTuple(args)

            | ExprId.NewUnionCase ->
                let t = Type.deserializeInternal state.TypeState src
                let tag = src.ReadInt32()
                let case = FSharpType.GetUnionCases(t, true) |> Array.find (fun c -> c.Tag = tag)
                let args = List.init (case.GetFields().Length) (fun _ -> deserializeInternal state src)
                Expr.NewUnionCase(case, args)

            | ExprId.StaticPropertyGet | ExprId.InstancePropertyGet ->
                let t, target =
                    match id with
                    | ExprId.StaticPropertyGet -> 
                        Type.deserializeInternal state.TypeState src, None
                    | _ ->
                        let target = deserializeInternal state src
                        target.Type, Some target
                    
                let name = src.ReadString()
                let prop = t.GetProperty(name, BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance)

                let indices =
                    let c = prop.GetIndexParameters().Length
                    if c > 0 then 
                        List.init c (fun _ ->
                            deserializeInternal state src
                        ) |> Some
                    else None


                match target with
                | Some target -> Expr.PropertyGet(target, prop, ?indexerArgs = indices)
                | None -> Expr.PropertyGet(prop, ?indexerArgs = indices)
            
            | ExprId.StaticPropertySet | ExprId.InstancePropertySet ->
                let t, target =
                    match id with
                    | ExprId.StaticPropertySet -> 
                        Type.deserializeInternal state.TypeState src, None
                    | _ ->
                        let target = deserializeInternal state src
                        target.Type, Some target
                    
                let name = src.ReadString()
                let prop = t.GetProperty(name, BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance)


                let indices =
                    let c = prop.GetIndexParameters().Length
                    if c > 0 then 
                        List.init c (fun _ ->
                            deserializeInternal state src
                        ) |> Some
                    else None

                let value = deserializeInternal state src

                match target with
                | Some target -> Expr.PropertySet(target, prop, value, ?indexerArgs = indices)
                | None -> Expr.PropertySet(prop, value, ?indexerArgs = indices)

            | ExprId.QuoteRaw ->
                let e = deserializeInternal state src
                Expr.QuoteRaw e

            | ExprId.QuoteTyped ->
                let e = deserializeInternal state src
                Expr.QuoteTyped e

            | ExprId.Sequential ->
                let a = deserializeInternal state src
                let b = deserializeInternal state src
                Expr.Sequential(a,b)

            | ExprId.TryFinally ->
                let a = deserializeInternal state src
                let b = deserializeInternal state src
                Expr.TryFinally(a,b)
            

            | ExprId.TryWith ->
                let body = deserializeInternal state src

                let vid = src.ReadInt32()
                let name = src.ReadString()
                let typ = Type.deserializeInternal state.TypeState src
                let isMutable = src.ReadBoolean()
                let filterVar = Var(name, typ, isMutable)

                state.Variables <- Map.add vid filterVar state.Variables
                let filterBody = deserializeInternal state src
                state.Variables <- Map.remove vid state.Variables

            
                let vid = src.ReadInt32()
                let name = src.ReadString()
                let typ = Type.deserializeInternal state.TypeState src
                let isMutable = src.ReadBoolean()
                let catchVar = Var(name, typ, isMutable)
            
                state.Variables <- Map.add vid catchVar state.Variables
                let catchBody = deserializeInternal state src
                state.Variables <- Map.remove vid state.Variables
                Expr.TryWith(body, filterVar, filterBody, catchVar, catchBody)

            | ExprId.TupleGet ->
                let e = deserializeInternal state src
                let i = src.ReadInt32()
                Expr.TupleGet(e, i)

            | ExprId.TypeTest ->
                let e = deserializeInternal state src
                let t = Type.deserializeInternal state.TypeState src
                Expr.TypeTest(e, t)

            | ExprId.UnionCaseTest ->
                let e = deserializeInternal state src
                let t = Type.deserializeInternal state.TypeState src
                let tag = src.ReadInt32()
                let case = FSharpType.GetUnionCases(t, true) |> Array.find (fun c -> c.Tag = tag)
                Expr.UnionCaseTest(e, case)

            | ExprId.ValueWithName | ExprId.Value ->
                let name = 
                    if id = ExprId.ValueWithName then Some (src.ReadString())
                    else None

                let t = Type.deserializeInternal state.TypeState src
                let value = Value.deserialize src t

                match name with
                | Some name -> Expr.ValueWithName(value, t, name)
                | None -> Expr.Value(value, t)
            
            | ExprId.VarSet ->
                let vid = src.ReadInt32()
                let value = deserializeInternal state src
                Expr.VarSet(state.Variables.[vid], value)

            | ExprId.WhileLoop ->
                let guard = deserializeInternal state src
                let body = deserializeInternal state src
                Expr.WhileLoop(guard, body)

            | id ->
                failwithf "unexpected ExprId: %A" id


        let serialize (dst : Stream) (e : Expr) =
            use w = new BinaryWriter(dst, System.Text.Encoding.UTF8, true)
            serializeInternal (SerializerState(false)) w e
        
        let deserialize (src : Stream) =
            use r = new BinaryReader(src, System.Text.Encoding.UTF8, true)
            deserializeInternal (DeserializerState()) r

        let tryDeserialize (src : Stream) =
            try deserialize src |> Some
            with _ -> None

        let computeHash (e : Expr) =
            use hash = System.Security.Cryptography.MD5.Create()
            use ms = new MemoryStream()
            use h = new CryptoStream(ms, hash, CryptoStreamMode.Write)
            use w = new BinaryWriter(h, System.Text.Encoding.UTF8, true)
            serializeInternal (SerializerState(true)) w e
            h.FlushFinalBlock()
            hash.Hash |> Convert.ToBase64String

    module Shader =

        type internal SerializerState() =
            let eState = Expr.SerializerState(false)
            member x.ExprState = eState
            member x.TypeState = eState.TypeState

        type internal DeserializerState() =
            let eState = Expr.DeserializerState()
            member x.ExprState = eState
            member x.TypeState = eState.TypeState


        module private ParameterDescription =
            let serialize (state : SerializerState) (dst : BinaryWriter) (p : ParameterDescription) =
                Type.serializeInternal state.TypeState dst p.paramType
                dst.Write (int p.paramInterpolation)

            let deserialize (state : DeserializerState) (src : BinaryReader) =
                let t = Type.deserializeInternal state.TypeState src
                let interpolation = src.ReadInt32() |> unbox<InterpolationMode>
                { paramType = t; paramInterpolation = interpolation }

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

        module private UniformParameter =
            let serialize (state : SerializerState) (dst : BinaryWriter) (p : UniformParameter) =
                dst.Write p.uniformName
                Type.serializeInternal state.TypeState dst p.uniformType

                match p.uniformValue with
                | UniformValue.Attribute(scope, name) ->
                    dst.Write 0uy
                    let list =
                        let rec toList (s : UniformScope) =
                            match s.Parent with
                            | None -> 
                                if System.String.IsNullOrWhiteSpace s.Name then []
                                else [s.Name]
                            | Some p -> toList p @ [s.Name]
                        toList scope

                    dst.Write (List.length list)
                    for l in list do dst.Write l
                    dst.Write name

                | UniformValue.Sampler(name, state) ->
                    dst.Write 1uy
                    dst.Write name
                    SamplerState.serialize dst state

                | UniformValue.SamplerArray arr ->
                    dst.Write 2uy
                    dst.Write arr.Length
                    for (name, state) in arr do
                        dst.Write name
                        SamplerState.serialize dst state

                | UniformValue.AccelerationStructure name ->
                    dst.Write 3uy
                    dst.Write name

            let deserialize (state : DeserializerState) (src : BinaryReader) =
                let uniformName = src.ReadString()
                let typ = Type.deserializeInternal state.TypeState src

                let value =
                    match src.ReadByte() with
                    | 0uy ->
                        let len = src.ReadInt32()
                        let parts = List.init len (fun _ -> src.ReadString())
                        let name = src.ReadString()

                        let mutable s = uniform
                        for p in parts do s <- s?(p)

                        UniformValue.Attribute(s, name)

                    | 1uy ->
                        let name = src.ReadString()
                        let state = SamplerState.deserialize src
                        UniformValue.Sampler(name, state)

                    | 2uy ->
                        let cnt = src.ReadInt32()
                        let arr = 
                            Array.init cnt (fun _ ->
                                let name = src.ReadString()
                                let state = SamplerState.deserialize src
                                name, state
                            )
                        UniformValue.SamplerArray arr

                    | _ ->
                        let name = src.ReadString()
                        UniformValue.AccelerationStructure name

                {
                    uniformName = uniformName
                    uniformType = typ
                    uniformValue = value
                }



        let internal serializeInternal (state : SerializerState) (dst : BinaryWriter) (shader : Shader) =
            dst.Write (int shader.shaderStage)

            dst.Write (Map.count shader.shaderInputs)
            for KeyValue(name, p) in shader.shaderInputs do
                dst.Write name
                ParameterDescription.serialize state dst p

            dst.Write (Map.count shader.shaderOutputs)
            for KeyValue(name, p) in shader.shaderOutputs do
                dst.Write name
                ParameterDescription.serialize state dst p

            dst.Write (Map.count shader.shaderUniforms)
            for KeyValue(name, u) in shader.shaderUniforms do
                dst.Write name
                UniformParameter.serialize state dst u

            match shader.shaderInputTopology with
            | None -> 
                dst.Write 0uy
            | Some t ->
                dst.Write 1uy
                match t with
                | InputTopology.Point -> dst.Write 0uy
                | InputTopology.Line -> dst.Write 1uy
                | InputTopology.LineAdjacency -> dst.Write 2uy
                | InputTopology.Triangle -> dst.Write 3uy
                | InputTopology.TriangleAdjacency -> dst.Write 4uy
                | InputTopology.Patch v -> dst.Write 5uy; dst.Write v

            match shader.shaderOutputTopology with
            | None ->
                dst.Write 0uy
            | Some t ->
                dst.Write 1uy
                match t with
                | OutputTopology.Points -> dst.Write 0uy
                | OutputTopology.LineStrip -> dst.Write 1uy
                | OutputTopology.TriangleStrip -> dst.Write 2uy
            
            match shader.shaderOutputVertices with
            | ShaderOutputVertices.Unknown -> dst.Write 0uy
            | ShaderOutputVertices.UserGiven v -> dst.Write 1uy; dst.Write v
            | ShaderOutputVertices.Computed v -> dst.Write 2uy; dst.Write v

            match shader.shaderOutputPrimitives with
            | Some p -> dst.Write 1uy; dst.Write p
            | None -> dst.Write 0uy

            dst.Write shader.shaderInvocations

            Expr.serializeInternal state.ExprState dst shader.shaderBody

            match shader.shaderDebugRange with
            | Some range ->
                dst.Write 1uy
                dst.Write range.file
                dst.Write range.startLine
                dst.Write range.endLine
                dst.Write range.startCol
                dst.Write range.endCol
            | None ->
                dst.Write 0uy

            dst.Write (Map.count shader.shaderPayloads)
            for KeyValue(n, (t,i)) in shader.shaderPayloads do
                dst.Write n
                Type.serializeInternal state.TypeState dst t
                dst.Write i
                ()

            match shader.shaderPayloadIn with
            | Some (name, typ) ->
                dst.Write 1uy
                dst.Write name
                Type.serializeInternal state.TypeState dst typ
            | None ->
                dst.Write 0uy

            dst.Write (Map.count shader.shaderCallableData)
            for KeyValue(n, (t,i)) in shader.shaderCallableData do
                dst.Write n
                Type.serializeInternal state.TypeState dst t
                dst.Write i
            
            match shader.shaderCallableDataIn with
            | Some (name, typ) ->
                dst.Write 1uy
                dst.Write name
                Type.serializeInternal state.TypeState dst typ
            | None ->
                dst.Write 0uy
            
            match shader.shaderHitAttribute with
            | Some (name, typ) ->
                dst.Write 1uy
                dst.Write name
                Type.serializeInternal state.TypeState dst typ
            | None ->
                dst.Write 0uy

            dst.Write (Set.count shader.shaderRayTypes)
            for s in shader.shaderRayTypes do dst.Write (string s)
        
            dst.Write (Set.count shader.shaderMissShaders)
            for s in shader.shaderMissShaders do dst.Write (string s)
        
            dst.Write (Set.count shader.shaderCallableShaders)
            for s in shader.shaderCallableShaders do dst.Write (string s)

            dst.Write (int shader.shaderDepthWriteMode)
 
        let internal deserializeInternal (state : DeserializerState) (src : BinaryReader) =
            let stage = src.ReadInt32() |> unbox<ShaderStage>

            let shaderInputs =
                let cnt = src.ReadInt32()
                List.init cnt (fun _ -> src.ReadString(), ParameterDescription.deserialize state src)
                |> Map.ofList
            
            let shaderOutputs =
                let cnt = src.ReadInt32()
                List.init cnt (fun _ -> src.ReadString(), ParameterDescription.deserialize state src)
                |> Map.ofList
            
            let shaderUniforms =
                let cnt = src.ReadInt32()
                List.init cnt (fun _ -> src.ReadString(), UniformParameter.deserialize state src)
                |> Map.ofList

            let inputTopology =
                match src.ReadByte() with
                | 0uy -> 
                    None
                | _ ->
                    match src.ReadByte() with
                    | 0uy -> Some InputTopology.Point
                    | 1uy -> Some InputTopology.Line
                    | 2uy -> Some InputTopology.LineAdjacency
                    | 3uy -> Some InputTopology.Triangle
                    | 4uy -> Some InputTopology.TriangleAdjacency
                    | _ -> Some (InputTopology.Patch (src.ReadInt32()))
                
            let outputTopology =
                match src.ReadByte() with
                | 0uy ->
                    None
                | _ ->
                    match src.ReadByte() with
                    | 0uy -> Some OutputTopology.Points
                    | 1uy -> Some OutputTopology.LineStrip
                    | _ -> Some OutputTopology.TriangleStrip

            let outputVertices =
                match src.ReadByte() with
                | 0uy -> ShaderOutputVertices.Unknown
                | 1uy -> ShaderOutputVertices.UserGiven (src.ReadInt32())
                | _ -> ShaderOutputVertices.Computed (src.ReadInt32())

            let outputPrimitives =
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadInt32())
                
            let invocations = src.ReadInt32()

            let body = Expr.deserializeInternal state.ExprState src

            let debugRange =
                match src.ReadByte() with
                | 0uy ->
                    None
                | _ ->
                    let file = src.ReadString()
                    let startLine = src.ReadInt32()
                    let endLine = src.ReadInt32()
                    let startCol = src.ReadInt32()
                    let endCol = src.ReadInt32()
                    Some { file = file; startLine = startLine; endLine = endLine; startCol = startCol; endCol = endCol }

            let shaderPayloads =
                let cnt = src.ReadInt32()
                List.init cnt (fun _ -> src.ReadString(), (Type.deserializeInternal state.TypeState src, src.ReadInt32()))
                |> Map.ofList

            let shaderPayloadIn =
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadString(), Type.deserializeInternal state.TypeState src)

            let shaderCallableData =
                let cnt = src.ReadInt32()
                List.init cnt (fun _ -> src.ReadString(), (Type.deserializeInternal state.TypeState src, src.ReadInt32()))
                |> Map.ofList
            
            let shaderCallableDataIn =
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadString(), Type.deserializeInternal state.TypeState src)
            
            let shaderHitAttribute =
                match src.ReadByte() with
                | 0uy -> None
                | _ -> Some (src.ReadString(), Type.deserializeInternal state.TypeState src)

            let shaderRayTypes =
                let cnt = src.ReadInt32()
                List.init cnt (fun _ -> src.ReadString() |> Aardvark.Base.Symbol.Create) |> Set.ofList
            
            let shaderMissShaders =
                let cnt = src.ReadInt32()
                List.init cnt (fun _ -> src.ReadString() |> Aardvark.Base.Symbol.Create) |> Set.ofList
            
            let shaderCallableShaders =
                let cnt = src.ReadInt32()
                List.init cnt (fun _ -> src.ReadString() |> Aardvark.Base.Symbol.Create) |> Set.ofList

            let shaderDepthWriteMode =
                src.ReadInt32() |> unbox<DepthWriteMode>

            {
                shaderStage = stage
                shaderInputs = shaderInputs
                shaderOutputs = shaderOutputs
                shaderUniforms = shaderUniforms
                shaderInputTopology = inputTopology
                shaderOutputTopology = outputTopology
                shaderOutputVertices = outputVertices
                shaderOutputPrimitives = outputPrimitives
                shaderInvocations = invocations
                shaderBody = body
                shaderDebugRange = debugRange
                shaderPayloads = shaderPayloads
                shaderPayloadIn = shaderPayloadIn
                shaderCallableData = shaderCallableData
                shaderCallableDataIn = shaderCallableDataIn
                shaderHitAttribute = shaderHitAttribute
                shaderRayTypes = shaderRayTypes
                shaderMissShaders = shaderMissShaders
                shaderCallableShaders = shaderCallableShaders
                shaderDepthWriteMode = shaderDepthWriteMode
            }

[<AutoOpen>]
module ExprHashingExtensions =
    open Microsoft.FSharp.Quotations
    type Expr with
        static member ComputeHash(e : Expr) =
            Expr.computeHash e

type Serializer private() =
    static member Init() =
        FShade.ExprHashExtensions._hash <- Expr.computeHash
        
