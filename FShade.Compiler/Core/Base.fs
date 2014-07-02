namespace FShade.Compiler

open System
open System.Reflection
open Microsoft.FSharp.Quotations

type FunctionId = int

/// <summary>
/// Represents a function either being a Method or a user-defined function having
/// a unique id, a list of parameters and a function-body
/// </summary>
[<NoComparison>]
type Function = MethodFunction of MethodBase
              | SpecialFunction of FunctionId * list<Var> * Expr

/// <summary>
/// Represents a parameter passing-style. In C-like languages parameters can normally
/// be passed by value or by reference. Additionaly we want to support out-arguments here
/// since most shader-APIs also make this distinction.
/// </summary>
type ParameterPassingStyle = ValueArgument
                           | ReferenceArgument
                           | OutArgument


/// <summary>
/// Field holds a description of a field by its name, type and array-size. Furthermore
/// the original member is optionally present.
/// </summary>
[<NoComparison>]
type Field = { name : string; fieldType : string; arraySize : Option<string>; info : Option<MemberInfo> } 
