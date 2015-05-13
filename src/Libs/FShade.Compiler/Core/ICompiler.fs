namespace FShade.Compiler

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Aardvark.Base
open FShade.Utils

/// <summary>
/// ICompiler is the base-interface for all compilers. It contains several abstract members
/// serving as "configuration" for the compiler. Please note that the generated language is
/// assumed to be C-like and cannot be configured entirely free. The compilation process assumes
/// that the language does not allow recursion or recursive types, does not allow dynamic allocation
/// and has no direct support for function-pointers or closures. The current implementation also expects
/// the target-language to allow forward declarations of functions.
/// </summary>
type ICompiler<'s> =
    //TODO: comments for all members
    abstract member CompileIntrinsicType : Type -> Compiled<Option<string>, 's>
    abstract member CompileIntrinsicFunction : MethodInfo -> Compiled<Option<string>, 's>
    abstract member CompileIntrinsicFunctionDefinition : MethodInfo -> Compiled<Option<Expr>, 's>
    abstract member CompileIntrinsicPropertyGet : MemberInfo -> Compiled<Option<string>, 's>
    abstract member CompileIntrinsicPropertySet : MemberInfo -> Compiled<Option<string>, 's>
    abstract member CompileIntrinsicConstructor : ConstructorInfo -> Compiled<Option<string>, 's>
    abstract member CompileFunctionDeclaration : string -> string -> list<string*string*Option<string> * ParameterPassingStyle> -> string -> Compiled<string, 's>
    abstract member CompileTypeDeclaration : string -> list<string> -> Compiled<string, 's>
    abstract member CompileVariableDeclaration : string -> string -> Option<string> -> Compiled<string, 's>
    abstract member CompileConstantDeclaration : Type -> string -> obj -> Compiled<string, 's>
    abstract member CompileFieldDeclaration : Field -> Compiled<string, 's>
    abstract member CompileApplication : obj -> Type -> list<Expr> -> Compiled<Option<string * list<Expr>>, 's>

    abstract member CompilePreamble : unit -> Compiled<string, 's>
    abstract member CompileValue : Type -> obj -> Compiled<Option<string>, 's>
    abstract member FilterFunctionArguments : list<Var> -> Compiled<list<Var>, 's>
    abstract member ProcessCode : string -> Compiled<string, 's>
    abstract member ProcessFunctionBody : Expr -> Compiled<Expr, 's>
    abstract member InitialState : unit -> 's

/// <summary>
/// The CompilerState represents the internal state of the compilation process and can
/// be modified using the "compile" computationexpression. There is also support for
/// including a user-defined state of any type. These states are internally used by
/// the compilation process and all stateful computations should be done using the compile-monad
/// </summary>
and [<NoComparison>] [<NoEquality>] CompilerState<'s> = 
    { compiler : ICompiler<'s>
      types : Set<Unique<Type>>
        
      functionId : int
      functions : Set<Unique<Function>>
          
      constantId : int
      constants : Map<Unique<obj>, Var>
          
      lambdaId : int
      lambdas : Map<string, Expr * int>

      uniformId : int
      uniforms : Map<Unique<obj>, Var>

      defines : Map<string, string>
          
      bound : Set<Var>

      userState : 's }
   
/// <summary>
/// Compiled<'a 's> represents a state-monad where 'a is the resulting type
/// and 's defines the state. It can furthermore produce Errors at any point.
/// </summary>
and [<NoComparison>] [<NoEquality>] Compiled<'a, 's> = 
    { runCompile : CompilerState<'s> -> Error<CompilerState<'s> * 'a> }


/// <summary>
/// CompilerFunctions "lifts" all members provided by ICompiler to the 
/// state-monad allowing them to be used by the compilation process without
/// referencing a concrete compiler. (Note that the compiler itself is 
/// part of the CompilerState - Type)
/// </summary>
[<AutoOpen>]
module CompilerFunctions = 
    let compileIntrinsicType (t : Type) =
        { runCompile = fun s -> (s.compiler.CompileIntrinsicType t).runCompile s }

    let compileIntrinsicFunction (m : MethodInfo)  =
        { runCompile = fun s -> (s.compiler.CompileIntrinsicFunction m).runCompile s }

    let compileIntrinsicFunctionDefinition (m : MethodInfo)  =
        { runCompile = fun s -> (s.compiler.CompileIntrinsicFunctionDefinition m).runCompile s }

    let compileIntrinsicPropertyGet (p : MemberInfo)  =
        { runCompile = fun s -> (s.compiler.CompileIntrinsicPropertyGet p).runCompile s }

    let compileIntrinsicPropertySet (p : MemberInfo)  =
        { runCompile = fun s -> (s.compiler.CompileIntrinsicPropertySet p).runCompile s }

    let compileIntrinsicConstructor (c : ConstructorInfo)  =
        { runCompile = fun s -> (s.compiler.CompileIntrinsicConstructor c).runCompile s }

    let compileFunctionDeclaration (retType : string) (name : string) (args : list<string*string*Option<string> * ParameterPassingStyle>) (body : string) =
        { runCompile = fun s -> (s.compiler.CompileFunctionDeclaration retType name args body).runCompile s }

    let compileTypeDeclaration (name : string) (fields : list<string>) =
        { runCompile = fun s -> (s.compiler.CompileTypeDeclaration name fields).runCompile s }

    let compileVariableDeclaration (t : string) (name : string) (arraySize : Option<string>)  =
        { runCompile = fun s -> (s.compiler.CompileVariableDeclaration t name arraySize).runCompile s }

    let compileConstantDeclaration (t : Type) (name : string) (value : obj)  =
        { runCompile = fun s -> (s.compiler.CompileConstantDeclaration t name value).runCompile s }

    let compileApplication (f : obj) (retType : Type) (args : list<Expr>)  =
        { runCompile = fun s -> (s.compiler.CompileApplication f retType args).runCompile s }


    let compilePreamble  =
        { runCompile = fun s -> (s.compiler.CompilePreamble()).runCompile s }


    let compileIntrinsicValue (t : Type) (o : obj)  =
        { runCompile = fun s -> (s.compiler.CompileValue t o).runCompile s }

    let filterFunctionArguments (args : list<Var>)  =
        { runCompile = fun s -> (s.compiler.FilterFunctionArguments args).runCompile s }


    let compileFieldDeclaration (f : Field)  =
        { runCompile = fun s -> (s.compiler.CompileFieldDeclaration f).runCompile s }

    let processCode (code : string)  =
        { runCompile = fun s -> (s.compiler.ProcessCode code).runCompile s }

    let processFunctionBody (body : Expr)  =
        { runCompile = fun s -> (s.compiler.ProcessFunctionBody body).runCompile s }