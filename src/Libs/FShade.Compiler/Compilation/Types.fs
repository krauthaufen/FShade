namespace FShade.Compiler

[<AutoOpen>]
module Types =
    open System
    open Microsoft.FSharp.Reflection
    open Aardvark.Base
    open FShade.Utils

    /// <summary>
    /// translates a type-name to the target language. Works by determining whether the
    /// type is intrinsic (according to the current ICompiler) and creates a default-name
    /// otherwise. Whenever the type is not instrinsic it is added to the CompilerState's 
    /// list of used types.
    /// </summary>
    let rec compileType (t : Type) =
        compile {
            if t.IsEnum then
                return! compileType typeof<int>
            else
                match t with
                    | FixedArrayType(dim,content) ->
                        let! c = compileType content
                        return sprintf "%s[%d]" c dim
    
                    | t when typeof<INatural>.IsAssignableFrom t ->
                        let dim = getSize t
                        return sprintf "_%d" dim
                    | _ ->
                        let! i = compileIntrinsicType t
                        match i with 
                            | Some i -> return i
                            | None -> 
                                //mark the type used (so the compiler will emit a definition for it)
                                do! addUsedType t

                                if FSharpTypeExt.IsTuple t then
                                    let! argTypes = FSharpTypeExt.GetTupleElements t |> Seq.mapC compileType
                                    let argTypes = argTypes |> Seq.map saneTypeName
                                    return sprintf "tup_%s" (String.concat "_" argTypes)
                                else
                                    //could use FSharpTypeExt.IsFunction here but that seems to be extremely slow.
                                    if t.Name.StartsWith "FSharpFunc" then
                                        let (arg, ret) = FSharpTypeExt.GetFunctionElements t
                                        let! arg = compileType arg
                                        let! ret = compileType ret
                                        let arg = arg |> saneTypeName
                                        let ret = ret |> saneTypeName

                                        return sprintf "Func_%s_%s" arg ret
                                    else
                                        if t.IsGenericType then
                                            let! args = t.GetGenericArguments() |> Seq.mapC compileType
                                            let n = t.Name.Substring(0, t.Name.Length - 2)
                                            let args = args |> Seq.map saneTypeName

                                            return sprintf "%s_%s" n (String.concat "_" args)

                                        else
                                            return t.Name            

        }
