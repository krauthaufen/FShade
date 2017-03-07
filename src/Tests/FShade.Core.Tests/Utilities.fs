namespace FShade

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open FsUnit
open NUnit.Framework
open NUnit.Framework.Constraints

open Aardvark.Base
open Aardvark.Base.Monads.State

open FShade

[<AutoOpen>]
module Utilities = 
    let keep a = ()

    let exprComparer = 
        { new System.Collections.Generic.IEqualityComparer<Expr> with
            member x.GetHashCode(l : Expr) =
                0

            member x.Equals(l : Expr, r : Expr) =
                l.ToString() = r.ToString()
        }

    let exprEqual (r : Expr) = EqualConstraint(r).Using exprComparer

    module Opt =
        open System.Reflection

        let private keepMeth = getMethodInfo <@ keep @>
        let isSideEffect (mi : MethodInfo) =
            mi.IsGenericMethod && mi.GetGenericMethodDefinition() = keepMeth

        let run (expression : Expr) =
            expression
                |> Optimizer.evaluateConstants' isSideEffect
                |> Optimizer.eliminateDeadCode' isSideEffect