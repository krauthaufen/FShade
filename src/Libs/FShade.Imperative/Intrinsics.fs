namespace FShade

open System
open FShade.Imperative

[<AbstractClass>]
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Method, AllowMultiple = true)>]
type IntrinsicAttribute() =
    inherit Attribute()
    abstract member Intrinsic : CIntrinsic


type GLSLIntrinsicAttribute(format : string) =
    inherit IntrinsicAttribute()

    override x.Intrinsic =
        {
            intrinsicName = null
            tag = format
            arguments = None
        }

[<AutoOpen>]
module ``Reflection Helpers`` =
    type System.Reflection.MethodBase with
        member x.Intrinsic<'a when 'a :> IntrinsicAttribute>() =
            let att = x.GetCustomAttributes(typeof<'a>, true) |> Seq.map unbox<IntrinsicAttribute> |> Seq.toList
            match att with
                | h :: _ -> Some h.Intrinsic
                | _ -> None