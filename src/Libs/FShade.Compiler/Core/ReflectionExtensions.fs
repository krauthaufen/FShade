namespace FShade.Compiler

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Aardvark.Base.TypeInfo.Patterns
open FShade.Utils

/// <summary>
/// This module holds types representing custom methods/parameters and types.
/// These are useful for creating functions/etc. which are only used during the 
/// compilation/translation process in the compiler. Note that all those types
/// will not provide meaningful implementations for direct use in .NET.
/// Since special-cases would extremely pollute the compiler-code we decided to
/// use .NET's internal structures for our purposes.
/// </summary>
[<AutoOpen>]
module ReflectionExtensions =

    /// <summary>
    /// ShaderReflectionException is thrown whenever internal structures produced by the
    /// compilation process are used by "real" code in a unsupported way.
    /// </summary>
    exception ShaderReflectionException of string

    /// <summary>
    /// NoMethodCallException is thrown whenever a method-call was expected but not found.
    /// </summary>
    exception NoMethodCallException of string

    // private utility function for raising a ShaderReflectionException
    let private fail msg = raise <| ShaderReflectionException msg


    /// <summary>
    /// CustomParameterInfo is used to represent custom parameters which do not exist in
    /// the reflected code and are used by compiler-generated functions.
    /// </summary>
    type CustomParameterInfo(parameterName : string, parameterType : Type) =
        inherit ParameterInfo()

        override x.Name = parameterName
        override x.ParameterType = parameterType

    /// <summary>
    /// CustomMethod is used to capture properties of compiler-generated functions and is only
    /// sufficient for internal use by the compiler. Note that most calls to it will result in
    /// a ShaderReflectionException.
    /// </summary>
    type CustomMethod(name : string, argTypes : Type[], returnType : Type, isStatic : bool) =
        inherit MethodInfo()
        let parameters = argTypes |> Array.mapi (fun i t -> CustomParameterInfo(sprintf "arg%d" i, t) :> ParameterInfo)
        let attributes = if isStatic then MethodAttributes.Public ||| MethodAttributes.Static else MethodAttributes.Public
        
        override x.Name = name
        override x.ReturnType = returnType
        override x.GetParameters() = parameters
        override x.DeclaringType = typeof<CustomMethod>

        override x.ReflectedType = fail "cannot access ReflectedType for CustomMethod"
        override x.GetCustomAttributes(inh) = fail "cannot query CustomAttributes for CustomMethod"
        override x.GetCustomAttributes(t, inh) = fail "cannot query CustomAttributes for CustomMethod"
        override x.ReturnTypeCustomAttributes = fail "cannot query ReturnTypeCustomAttributes for CustomMethod"
        override x.IsDefined(a,b) = fail "cannot use IsDefined for CustomMethod"
        override x.GetBaseDefinition() = fail "cannot get BaseDefinition for CustomMethod"
        override x.GetMethodImplementationFlags() = fail "cannot get MethodImplementationFlags for CustomMethod"
        override x.MethodHandle = fail "cannot get MethodHandle for CustomMethod"
        override x.Attributes = attributes
        override x.Invoke(a,b,c,d,e) = fail "cannot invoke CustomMethod"

        new(name : string, argTypes : Type[], returnType : Type) = CustomMethod(name, argTypes, returnType, true)

    /// <summary>
    /// CustomProperty is used to represent compiler-generated properties and is only
    /// sufficient for internal use by the compiler. Note that most calls to it will
    /// result in a ShaderReflectionException.
    /// </summary>
    type CustomProperty(instanceType : Type, name : string, t : Type) =
        inherit PropertyInfo()

        let getter = CustomMethod(sprintf "get_%s" name, [||], t, false) :> MethodInfo

        override x.Name = name
        override x.PropertyType = t
        override x.DeclaringType = instanceType
        override x.ReflectedType = fail "cannot access ReflectedType for CustomProperty"
        override x.GetCustomAttributes(a,b) = fail "cannot query CustomAttributes for CustomProperty"
        override x.GetCustomAttributes(a) = fail "cannot query CustomAttributes for CustomProperty"
        override x.IsDefined(a,i) = fail "cannot use IsDefined for CustomProperty"
        override x.Attributes = PropertyAttributes.None
        override x.CanRead = true
        override x.CanWrite = false
        override x.GetAccessors(n) = [|getter|]
        override x.GetGetMethod(n) = getter
        override x.GetIndexParameters() = [||]
        override x.GetSetMethod(n) = fail "cannot get SetMethod for CustomProperty"
        override x.GetValue(o, invokeAtt, binder, index, cult) = fail "cannot get value of CustomProperty"
        override x.SetValue(o, v, invokeAtt, binder, index, cult) = fail "cannot set value of CustomProperty"

    //extracts the (optional) top-most method call from an expression
    let rec tryGetMethodInfo (e : Expr) =
        match e with
            | Patterns.Call(_,mi,_) -> 
                if mi.IsGenericMethod then mi.GetGenericMethodDefinition() |> Some
                else mi |> Some
            | ExprShape.ShapeCombination(_, args) -> 
                args |> List.tryPick tryGetMethodInfo
            | ExprShape.ShapeLambda(_,b) ->
                tryGetMethodInfo b
            | _ -> None


    /// <summary>
    /// extracts the top-most method-call from an expression.
    /// When no method-call is found the method will raise an exception
    /// </summary>
    /// <param name="e"></param>
    let getMethodInfo (e : Expr) =
        match tryGetMethodInfo e with
            | Some mi -> mi
            | None -> raise <| NoMethodCallException "could not find a method-call in expression"

/// <summary>
/// Defines a number of active patterns for matching expressions. Includes some
/// functionality missing in F#.
/// </summary>
[<AutoOpen>]
module ReflectionPatterns =
    let typePrefixPattern = System.Text.RegularExpressions.Regex @"^.*\.(?<methodName>.*)$"
    let (|Method|_|)  (mi : MethodInfo) =
        let args = mi.GetParameters() |> Seq.map(fun p -> p.ParameterType)
        let parameters = if mi.IsStatic then
                            args
                            else
                            seq { yield mi.DeclaringType; yield! args }

        let m = typePrefixPattern.Match mi.Name
        let name =
            if m.Success then m.Groups.["methodName"].Value
            else mi.Name

        Method (name, parameters |> Seq.toList) |> Some

    let private compareMethods (template : MethodInfo) (m : MethodInfo) =
        if template.IsGenericMethod && m.IsGenericMethod then
            if template.GetGenericMethodDefinition() = m.GetGenericMethodDefinition() then
                let targs = template.GetGenericArguments() |> Array.toList
                let margs = m.GetGenericArguments() |> Array.toList

                let zip = List.zip targs margs

                let args = zip |> List.filter(fun (l,r) -> l.IsGenericParameter) |> List.map (fun (_,a) -> a)

                Some args
            else
                None
        elif template = m then
            Some []
        else
            None
                


    let (|MethodQuote|_|) (e : Expr) (mi : MethodInfo) =
        let m = tryGetMethodInfo e
        match m with
            | Some m -> match compareMethods m mi with
                            | Some a -> MethodQuote(a) |> Some
                            | None -> None
            | _ -> None


    let (|Create|_|) (c : ConstructorInfo) =
        Create(c.DeclaringType, c.GetParameters() |> Seq.toList) |> Some