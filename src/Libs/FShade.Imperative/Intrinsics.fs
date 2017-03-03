namespace FShade

open System
open FShade.Imperative

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Method, AllowMultiple = false)>]
type IntrinsicAttribute(format : string) =
    inherit Attribute()
    member x.Format = format

[<AutoOpen>]
module ``Reflection Helpers`` =
    type System.Reflection.MethodBase with
        member x.Intrinsic =
            let att = x.GetCustomAttributes(typeof<IntrinsicAttribute>, true) |> Seq.map unbox<IntrinsicAttribute> |> Seq.toList
            match att with
                | h :: _ ->
                    Some {
                        intrinsicName = null
                        tag = h.Format
                        arguments = None
                    }
                | _ -> None