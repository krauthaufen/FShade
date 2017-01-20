namespace FShade

open System
open Aardvark.Base


type UniformScope(parent : Option<UniformScope>, name : string) = 
    static let mutable currentId = 0

    let id = System.Threading.Interlocked.Increment(&currentId)
    let childScopes = System.Collections.Generic.Dictionary<string, UniformScope>()

    member private x.Id = id
    interface IComparable with
        member x.CompareTo o =
            match o with
                | :? UniformScope as o -> id.CompareTo o.Id
                | _ -> failwith "uncomparable"

    override x.GetHashCode() =
        id.GetHashCode()

    override x.Equals(o) =
        match o with
            | :? UniformScope as o -> o.Id = id
            | _ -> false

    member x.Parent = parent
    member x.Name = name
    member x.FullName = 
        let rec build (name : string) (s : UniformScope) =
            match s.Parent with
                | None -> 
                    if name.Length = 0 then "Global"
                    else name
                | Some p ->
                    build (s.Name + name) p

        build "" x

    member x.GetChildScope(n : string) =
        lock childScopes (fun () ->
            match childScopes.TryGetValue n with
                | (true,s) -> 
                    s
                | _ -> 
                    let s = UniformScope(Some x, n)
                    childScopes.[n] <- s
                    s
        )
   
type ISemanticValue =
    abstract member Semantic : string
    abstract member Scope : UniformScope
     
type SemanticException(scope : UniformScope, sem : string) =
    inherit Exception(sem)
    member x.Scope = scope
    member x.Semantic = sem


[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type SemanticAttribute(s : string) =
    inherit Attribute()
    member x.Semantic = s

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type InterpolationAttribute(qualifier : InterpolationMode) =
    inherit Attribute()
    member x.Qualifier = qualifier

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type PrimitiveIndexAttribute(index : int) =
    inherit Attribute()
    member x.Index = index

type IShaderBuilder =
    abstract member ShaderStage : ShaderStage
    abstract member OutputTopology : Option<OutputTopology>

type ParameterDescription =
    {
        paramType           : Type
        paramInterpolation  : InterpolationMode
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParameterDescription =

    let inline paramInterpolation (p : ParameterDescription) = p.paramInterpolation
    let inline paramType (p : ParameterDescription) = p.paramType

    let ofType (t : Type) =
        {
            paramType = t
            paramInterpolation = InterpolationMode.Default
        }


    


type UniformValue =
    | Attribute of scope : UniformScope * name : string
    | Sampler of textureName : string * SamplerState
    | SamplerArray of array<string * SamplerState>

type UniformParameter =
    {
        uniformName         : string
        uniformType         : Type
        uniformValue        : UniformValue
    }

type ISampler =
    abstract member Texture : ISemanticValue
    abstract member State : SamplerState

type ShaderTextureHandle(semantic : string, scope : UniformScope) =
    static member CreateUniform(semantic : string, scope : UniformScope) = ShaderTextureHandle(semantic, scope)
    interface ISemanticValue with
        member x.Semantic = semantic
        member x.Scope = scope


    new() = ShaderTextureHandle(null, Unchecked.defaultof<UniformScope>)       

type TextureMustBeSpecified = TextureMustBeSpecified

type SamplerBaseBuilder() =
    member x.Yield(_) = TextureMustBeSpecified

    [<CustomOperation("texture")>]
    member x.Texture(b : TextureMustBeSpecified, t : ShaderTextureHandle) =
        (t, SamplerState.empty)

    [<CustomOperation("addressU")>]
    member x.AddressU((t : ShaderTextureHandle, h : SamplerState), w : WrapMode) = t,{ h with AddressU = Some w }
             
    [<CustomOperation("addressV")>]
    member x.AddressV((t : ShaderTextureHandle, h : SamplerState), w : WrapMode) = t,{ h with AddressV = Some w }
             
    [<CustomOperation("addressW")>]
    member x.AddressW((t : ShaderTextureHandle, h : SamplerState), w : WrapMode) = t,{ h with AddressW = Some w }
             
    [<CustomOperation("maxAnisotropy")>]
    member x.MaxAnisotropy((t : ShaderTextureHandle, h : SamplerState), a : int) = t,{ h with MaxAnisotropy = Some a }
             
    [<CustomOperation("borderColor")>]
    member x.BorderColor((t : ShaderTextureHandle, h : SamplerState), c : C4f) = t,{ h with BorderColor = Some c }
             
    [<CustomOperation("maxLod")>]
    member x.MaxLod((t : ShaderTextureHandle, h : SamplerState), c : float) = t,{ h with MaxLod = Some c }
             
    [<CustomOperation("minLod")>]
    member x.MinLod((t : ShaderTextureHandle, h : SamplerState), c : float) = t,{ h with MinLod = Some c }
             
    [<CustomOperation("mipLodBias")>]
    member x.MipLodBias((t : ShaderTextureHandle, h : SamplerState), c : float) = t,{ h with MipLodBias = Some c }
             
    [<CustomOperation("filter")>]
    member x.Filter((t : ShaderTextureHandle, h : SamplerState), f : Filter) = t,{ h with Filter = Some f }

    [<CustomOperation("comparison")>]
    member x.Comparison((t : ShaderTextureHandle, h : SamplerState), f : ComparisonFunction) = t,{ h with Comparison = Some f }

[<AutoOpen>]
module UniformExtensions =

    let uniform = UniformScope(None, "Global")

    let (?) (s : UniformScope) (name : string) : 'a =
        let t = typeof<'a>

        if typeof<ISemanticValue>.IsAssignableFrom t then

            let creator = t.GetMethod("CreateUniform", [|typeof<string>; typeof<UniformScope>|])
            if creator <> null then
                let result = creator.Invoke(null, [| name :> obj ; s :> obj |])
                result |> unbox
            else
                raise <| SemanticException(s, name)
                
        elif t = typeof<UniformScope> then
            s.GetChildScope name |> unbox<'a>
        else
            raise <| SemanticException(s, name)