namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Aardvark.Base



[<AutoOpen>]
module SamplerStateModule =
    type WrapMode = Wrap = 0
                   | Mirror = 1
                   | Clamp = 2
                   | Border = 3
                   | MirrorOnce = 4
                    
    type Filter = Anisotropic = 0
                | MinLinearMagMipPoint = 1
                | MinLinearMagPointMipLinear = 2
                | MinMagLinearMipPoint = 3
                | MinMagMipLinear = 4
                | MinMagMipPoint = 5
                | MinMagPointMipLinear = 6
                | MinPointMagLinearMipPoint = 7
                | MinPointMagMipLinear = 8
                | MinMagPoint = 9
                | MinMagLinear = 10
                | MinPointMagLinear = 11
                | MinLinearMagPoint = 12
                 
    type ComparisonFunction = Never = 1
                            | Less = 2
                            | Equal = 3
                            | LessOrEqual = 4
                            | Greater = 5
                            | GreaterOrEqual = 6
                            | NotEqual = 7
                            | Always = 8
                
    [<NoComparison>]      
    type SamplerState = {
            AddressU : Option<WrapMode>
            AddressV : Option<WrapMode>
            AddressW : Option<WrapMode>
            Filter : Option<Filter>
            BorderColor : Option<C4f>
            MaxAnisotropy : Option<int>
            MaxLod : Option<float>
            MinLod : Option<float>
            MipLodBias : Option<float>
        } with
        static member Empty = { AddressU = None; AddressV = None; AddressW = None; Filter = None; BorderColor = None; MaxAnisotropy = None; MaxLod = None; MinLod = None; MipLodBias = None }
            
    [<NoComparison>]
    type SamplerComparisonState = {
            AddressU : Option<WrapMode>
            AddressV : Option<WrapMode>
            AddressW : Option<WrapMode>
            Filter : Option<Filter>
            Comparison : Option<ComparisonFunction>
            BorderColor : Option<C4f>
            MaxAnisotropy : Option<int>
            MaxLod : Option<float>
            MinLod : Option<float>
            MipLodBias : Option<float>

        } with
        static member Empty = { AddressU = None; AddressV = None; AddressW = None; Filter = None; Comparison = None; BorderColor = None; MaxAnisotropy = None; MaxLod = None; MinLod = None; MipLodBias = None }
            






module TypeLevelEnums =
    type True = True
    type False = False

    type Sampler1dType = Sampler1dType
    type Sampler2dType = Sampler2dType
    type Sampler3dType = Sampler3dType
    type SamplerCubeType = SamplerCubeType



type ISemanticValue =
    //static member CreateUniform(semantic : string, scope : UniformScope)
    abstract member Semantic : string
    abstract member ScopeUntyped : obj


type SamplerType = Float = 0
                 | Int = 1

type SamplerDimension = Sampler1d = 0
                      | Sampler2d = 1
                      | Sampler3d = 2
                      | SamplerCube = 3


type ISampler =
    abstract member Texture : ISemanticValue
    abstract member State : SamplerState

type SamplerBase<'dimension, 'valueType, 'coordType, 'isArray, 'isShadow, 'isMultisampled>(tex : ISemanticValue, state : SamplerState) =

    interface ISampler with
        member x.Texture = tex
        member x.State = state

    static member Dimension =
        let d = typeof<'dimension>
        if d = typeof<TypeLevelEnums.Sampler1dType> then SamplerDimension.Sampler1d
        elif d = typeof<TypeLevelEnums.Sampler2dType> then SamplerDimension.Sampler2d
        elif d = typeof<TypeLevelEnums.Sampler3dType> then SamplerDimension.Sampler3d
        elif d = typeof<TypeLevelEnums.SamplerCubeType> then SamplerDimension.SamplerCube
        else failwith "unknown sampler-dimension"

    static member ValueType = typeof<'valueType>

    static member CoordType = typeof<'valueType>


    static member IsArray =
        if typeof<'isArray> = typeof<TypeLevelEnums.True> then true
        else false

    static member IsShadow =
        if typeof<'isShadow> = typeof<TypeLevelEnums.True> then true
        else false

    static member IsMultisampled =
        if typeof<'isMultisampled> = typeof<TypeLevelEnums.True> then true
        else false

exception ShaderOnlyMethodException of string

[<AutoOpen>]
module ReflectionHelpers =

    let private getProp<'a> (name : string) (t : Type) =
        t.GetProperty(name).GetValue(null) |> unbox<'a>

    /// <summary>
    /// determines whether a given type is a Sampler and returns its properties if successful.
    /// The properties are given by SamplerType(dim, isArray, isShadow, isMS, valueType)
    /// </summary>
    let (|SamplerType|_|) (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<SamplerBase<_,_,_,_,_,_>> then
            let dim : SamplerDimension = getProp "Dimension" t
            let valueType : Type = getProp "ValueType" t
            let coordType : Type = getProp "CoordType" t
            let isArray : bool = getProp "IsArray" t
            let isShadow : bool = getProp "IsShadow" t
            let isMS : bool = getProp "IsMultisampled" t

            SamplerType(dim, isArray, isShadow, isMS, valueType) |> Some


        else
            None

open System.Runtime.CompilerServices
open TypeLevelEnums

type Sampler =

    static member sample (sampler : SamplerBase<_, 'a, 'c, False, False, False>,coord : 'c) : 'a =
        failwith ""

    static member sampleArray (sampler : SamplerBase<_, 'a, 'c, True, False, False>,coord : 'c,slice : int) : 'a =
        failwith ""


[<Extension>]
type SamplerExtensions =
    [<Extension>]
    static member inline Sample<'s, 'a, 'c, 't when 's :> SamplerBase<'t, 'a, 'c, False, False, False>>(this : 's, coord : 'c) : 'a =
        failwith ""
    
    [<Extension>]
    static member Sample(this : SamplerBase<_, 'a, 'c, True, False, False>, coord : 'c, slice : int) : 'a =
        failwith ""

    [<Extension>]
    static member SampleLevel(this : SamplerBase<_, 'a, 'c, False, False, False>, coord : 'c, level : float) : 'a =
        failwith ""
    
    [<Extension>]
    static member SampleLevel(this : SamplerBase<_, 'a, 'c, True, False, False>, coord : 'c, slice : int, level : float) : 'a =
        failwith ""

// This type defines the type provider. When compiled to a DLL, it can be added
// as a reference to an F# command-line compilation, script, or project.
[<TypeProvider>]
type SamplerTypeProvider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()

    let namespaceName = "FShade"
    let thisAssembly = Assembly.GetExecutingAssembly()


    let types = [ SamplerType.Float; SamplerType.Int ]
    let dims = [ SamplerDimension.Sampler1d; SamplerDimension.Sampler2d; SamplerDimension.Sampler3d; SamplerDimension.SamplerCube]
    let arr = [ true; false ]
    let ms = [ true; false ]
    let shadow = [ true; false ]

    let allCombinations =
        [
            for t in types do
                for d in dims do
                    let arr = if d = SamplerDimension.Sampler3d then [false] else arr
                    for a in arr do
                        for m in ms do
                            for s in shadow do
                                yield (t,d,a,m,s)
        ]


    let buildBaseType t d a m s =
        let dimType, coordType =
            match d with
                | SamplerDimension.Sampler1d -> typeof<TypeLevelEnums.Sampler1dType>, typeof<float>
                | SamplerDimension.Sampler2d -> typeof<TypeLevelEnums.Sampler2dType>, typeof<V2d>
                | SamplerDimension.Sampler3d -> typeof<TypeLevelEnums.Sampler3dType>, typeof<V3d>
                | SamplerDimension.SamplerCube -> typeof<TypeLevelEnums.SamplerCubeType>, typeof<V3d>
                | _ -> failwith ""

        let valueType =
            match t with
                    | SamplerType.Float -> typeof<V4d>
                    | SamplerType.Int -> typeof<V4i>
                    | _ -> failwith ""

        let arrType = if a then typeof<TypeLevelEnums.True> else typeof<TypeLevelEnums.False>
        let msType = if m then typeof<TypeLevelEnums.True> else typeof<TypeLevelEnums.False>
        let shadowType = if s then typeof<TypeLevelEnums.True> else typeof<TypeLevelEnums.False>

        typedefof<SamplerBase<_,_,_,_,_,_>>.MakeGenericType [|dimType; valueType; coordType; arrType; shadowType; msType|]

    let types =
        allCombinations |> List.map (fun (t,d,a,m,s) ->
            let prefix =
                match t with
                    | SamplerType.Float -> ""
                    | SamplerType.Int -> "Int"
                    | _ -> ""

            let dim =
                match d with
                    | SamplerDimension.Sampler1d -> "1d"
                    | SamplerDimension.Sampler2d -> "2d"
                    | SamplerDimension.Sampler3d -> "3d"
                    | SamplerDimension.SamplerCube -> "Cube"
                    | _ -> "2d"

            let ms = if m then "MS" else ""
            let arr = if a then "Array" else ""
            let shadow = if s then "Shadow" else ""


            let name = sprintf "%sSampler%s%s%s%s" prefix dim ms arr shadow

            let returnType =
                match t with
                    | SamplerType.Float -> typeof<V4d>
                    | SamplerType.Int -> typeof<V4i>
                    | _ -> failwith "unknown sampler baseType"

            let coordType, texelCoordType =
                match d with
                    | SamplerDimension.Sampler1d -> typeof<float>, typeof<int>
                    | SamplerDimension.Sampler2d -> typeof<V2d>, typeof<V2i>
                    | SamplerDimension.Sampler3d -> typeof<V3d>, typeof<V3i>
                    | SamplerDimension.SamplerCube -> typeof<V3d>, typeof<V3i>
                    | _ -> failwith "unsupported sampler-kind"

            let baseType = buildBaseType t d a m s
            let ty = ProvidedTypeDefinition(thisAssembly, namespaceName, name, Some baseType)


            let baseCtor = baseType.GetConstructor [|typeof<ISemanticValue>; typeof<SamplerState>|]
            let ctor = ProvidedConstructor([ProvidedParameter("tex", typeof<ISemanticValue>); ProvidedParameter("state", typeof<SamplerState>)])
            ctor.BaseConstructorCall <- fun args -> baseCtor, args
            ctor.InvokeCode <- fun args -> Expr.NewObject(baseCtor, args)
            ty.AddMember(ctor)

//
//            if not s then
//                let sampleParams =
//                    if a then [ProvidedParameter("coord", coordType); ProvidedParameter("slice", typeof<int>)]
//                    else [ProvidedParameter("coord", coordType)]
//
//                let m = ProvidedMethod("Sample", sampleParams, returnType)
//                m.SetMethodAttrs(MethodAttributes.Public)
//                m.InvokeCode <- SamplingMethods.sample returnType
//                ty.AddMember(m)
//


            ty
        )


    // And add them to the namespace
    do this.AddNamespace(namespaceName, types)

[<assembly:TypeProviderAssembly>] 
do()