namespace FShade.Compiler

[<AutoOpen>]
module Names =
    open System
    open Microsoft.FSharp.Reflection

    /// <summary>
    /// defines tuple-field names.
    /// </summary>
    let getTupleField (i : int) =
        sprintf "e%d" i

    /// <summary>
    /// defines constructor names for compiled constructors
    /// </summary>
    let getCtorName (typeName : string) =
        sprintf "%s_ctor" typeName

    /// <summary>
    /// defines constructor names for union cases.
    /// </summary>
    let getUnionCtorName (typeName : string) (case : string) =
        sprintf "%s_%s" typeName case

    /// <summary>
    /// defines field-names for union-type fields.
    /// </summary>
    let getUnionField (caseName : string) (i : int) =
        sprintf "%s_e%d" caseName i

    /// <summary>
    /// defines the names for anonymous functions.
    /// </summary>
    let getSpecialFunctionName (n : int) =
        sprintf "_helper%d" n

    /// <summary>
    /// gets a 'sane' type-name without certain characters
    /// </summary>
    let saneTypeName(typeName : string) =
        typeName.Replace('[', '_').Replace(']', '_')


    /// <summary>
    /// defines the names for anonymous-function dispatchers
    /// </summary>
    let getDispatcherName (argType : string) (retType : string) =
        let argType = argType |> saneTypeName
        let retType = retType |> saneTypeName
        sprintf "invoke_%s_%s" argType retType


    /// <summary>
    /// Gets all 'fields' contained in a union-case
    /// </summary>
    let getCaseFields (c : UnionCaseInfo) =
        c.GetFields() |> Seq.filter(fun f -> f.PropertyType <> typeof<unit>)
                      |> Seq.mapi (fun i pi -> 
                            let fieldName = getUnionField c.Name i
                            (pi.PropertyType, fieldName)) |> Seq.toList

    /// <summary>
    /// Gets all 'fields' contained in all the cases of a union-type
    /// </summary>
    let getAllUnionFields (t : Type) =
        let cases = FSharpType.GetUnionCases(t, true) |> Seq.toList
        cases |> List.collect getCaseFields
