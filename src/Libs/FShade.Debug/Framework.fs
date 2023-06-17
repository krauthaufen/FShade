namespace FShade.Debug

open System
open System.Runtime.InteropServices
open Aardvark.Base

[<RequireQualifiedAccess>]
type internal Framework =
    | NetStandard  of version: int
    | NetFramework of version: int
    | NetCore      of version: int

module internal Framework =

    [<AutoOpen>]
    module private Patterns =
        open System.Text.RegularExpressions

        let inline private tryParseInt (input : string) =
            match Int32.TryParse input with
            | (true, value) -> Some value
            | _ -> None

        let inline private tryMatch (regex : Regex[]) (input : string) =
            regex |> Array.tryPick (fun r ->
                let m = r.Match input
                if m.Success then
                    let v = m.Groups.["version"].Value
                    tryParseInt <| v.Replace(".", "").PadRight(3, '0')
                else
                    None
            )

        let (|NetCore|_|) =
            tryMatch [|
                Regex "net(?<version>[0-9]\.[0.9])"
                Regex "netcoreapp(?<version>[0-9]\.[0-9])"
                Regex ".NET (?<version>[0-9]\.[0-9])"
                Regex ".NET Core (?<version>[0-9]\.[0-9])"
            |]

        let (|NetFramework|_|) =
            tryMatch [|
                Regex "net(?<version>[0-9][0-9][0-9]?)"
                Regex ".NET Framework (?<version>[0-9]\.[0-9])"
            |]

        let (|NetStandard|_|) =
            tryMatch [|
                Regex "netstandard(?<version>[0-9]\.[0-9])"
            |]


    let tryParse = function
        | NetCore v      -> Some (Framework.NetCore v)
        | NetFramework v -> Some (Framework.NetFramework v)
        | NetStandard v  -> Some (Framework.NetStandard v)
        | _              -> None

    let runtime =
        let desc = RuntimeInformation.FrameworkDescription

        match tryParse desc with
        | Some f -> f
        | _ ->
            Log.warn "Failed to parse .NET framework description '%s'" desc
            Framework.NetCore 600

    // Determines if the current runtime supports the given target.
    let isSupported (target : Framework) =
        match runtime, target with
        | Framework.NetCore r, Framework.NetCore t
        | Framework.NetFramework r, Framework.NetFramework t ->
            r >= t

        | Framework.NetCore r, Framework.NetStandard s ->
            if s <= 200 then r >= 200
            else r >= 300

        | Framework.NetFramework r, Framework.NetStandard s ->
            s <= 200 && r >= 461

        | _ ->
            false