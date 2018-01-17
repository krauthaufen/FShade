namespace FShade

open System
open FShade.Imperative

[<AbstractClass>]
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Method, AllowMultiple = true)>]
type IntrinsicAttribute() =
    inherit Attribute()
    abstract member Intrinsic : CIntrinsic


type GLSLIntrinsicAttribute private(format : string, requiredExtensions : Set<string>) =
    inherit IntrinsicAttribute()

    override x.Intrinsic =
        {
            intrinsicName = null
            tag = format
            arguments = None
            additional = requiredExtensions
        }

    new(format : string) = GLSLIntrinsicAttribute(format, Set.empty)
    new(format : string, [<ParamArray>] requiredExtensions : string[]) = GLSLIntrinsicAttribute(format, Set.ofArray requiredExtensions)

[<AutoOpen>]
module ``Reflection Helpers`` =

    let inline onlyInShaderCode<'a> (name : string) : 'a =
        let msg = sprintf "[FShade] %s can only be called in shaders" name
        raise <| FShadeOnlyInShaderCodeException msg

    type System.Reflection.MethodBase with
        member x.Intrinsic<'a when 'a :> IntrinsicAttribute>() =
            let att = x.GetCustomAttributes(typeof<'a>, true) |> Seq.map unbox<IntrinsicAttribute> |> Seq.toList
            match att with
                | h :: _ -> Some h.Intrinsic
                | _ -> None
