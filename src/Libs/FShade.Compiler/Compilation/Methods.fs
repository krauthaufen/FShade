namespace FShade.Compiler

[<AutoOpen>]
module Methods =
    open System
    open System.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection
    open Aardvark.Base

    /// <summary>
    /// gets the dispatcher-name for a given function-type.
    /// </summary>
    let getDispatcherNameForType (t : Type) = 
        compile {
            if t.Name.StartsWith "FSharpFunc" then
                let (a,r) = FSharpType.GetFunctionElements t
                let! argType = compileType a
                let! retType = compileType r

                return getDispatcherName argType retType
            else
                return! error "a dispatcher for type %A cannot be created since it does not appear to be a function-type." t
        }

    /// <summary>
    /// Gets all free variables for an expression and filters them using the 
    /// compiler-defined method FilterFunctionArguments for filtering.
    /// </summary>
    let getFreeVars (e : Expr) =
        compile {
            let free = e.GetFreeVars() |> Seq.toList
            let! realFree = filterFunctionArguments free
            return realFree
        }

    /// <summary>
    /// gets the compiled-name for a given method.
    /// TODO: check for existence since the current naming-scheme may produce collisions.
    /// </summary>
    let getMethodName (mi : MethodBase) =
        compile {
            match mi with
                | :? CustomMethod as m -> 
                    return m.Name
                | _ ->
                    if mi.Name.Contains "|" then
                        return mi.Name.Replace('|', '_')
                    else
                        let! baseName  =
                            if mi.IsGenericMethod then
                                compile {
                                    let targs = mi.GetGenericArguments()
                                    let! targs = targs |> Seq.mapC compileType
                                    let name = sprintf "%s_%s" mi.Name (targs |> String.concat "_")
                                    return name
                                }
                            else
                                compile { return mi.Name }

                        if mi.IsStatic then 
                            let! used = usedTypes
                            let! t = compileType mi.DeclaringType
                            do! putUsedTypes used
                            return sprintf "%s_%s" t baseName
                        else 
                            let! t = compileType mi.DeclaringType
                            return sprintf "%s_%s" t baseName
        }

    /// <summary>
    /// gets the compiled-name for a given constructor.
    /// </summary>
    let compileCtorName (t : Type) =
        compile {
            let! n = compileType t
            return getCtorName n
        }

    /// <summary>
    /// wraps the given expression in a function, adds this function to the 'used' list and
    /// returns code calling this function. This can be used to encode statements inside expressions.
    /// For example F# allows things like 'let a = let b = 4 in b * b' but C/GLSL/HLSL don't because
    /// statements like 'let b = 4' cannot occur at expression-level. It is therefore useful to
    /// 'outsource' those statements to a function. (e.g. int f() { int b = 4; return b * b } ... let a = f() ...
    /// Note: this is especially useful for loops/ifs/etc.
    /// </summary>
    let asFunction (e : Expr) =
        compile {
            let! free = getFreeVars e
            let! id = addFunction free e
            let name = getSpecialFunctionName id

            //TODO: reference arguments might need special annotation in
            //      the calling code
            let args = free |> Seq.map (fun v -> v.Name) |> String.concat ", "

            return sprintf "%s(%s)" name args
        }

    /// <summary>
    /// compiles an intrinsic function-name when possible.
    /// when no intrinsic function-name could be found None is returned.
    /// </summary>
    let compileIntrinsicFunction (mi : MethodInfo) =
        compile {
            let! i = compileIntrinsicFunction mi
            match i with
                | Some v -> return Some v
                | None ->
                    let! def = compileIntrinsicFunctionDefinition mi

                    let rec getBody (e : Expr) =
                        match e with
                            | Lambda(v,b) -> getBody b
                            | _ -> e

                    match def with
                        | Some d ->
                            let d = getBody d
                            let! f = asFunction d
                            return Some f
                        | None -> return None
        }