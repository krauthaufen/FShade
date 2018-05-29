namespace FShade

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Xunit
open Xunit.Abstractions
open FsUnit

open Aardvark.Base
open Aardvark.Base.Monads.State

open FShade

[<AutoOpen>]
module Utilities = 
    let keep a = ()

    let exprComparer l = 
        { new NHamcrest.Core.IsEqualMatcher<obj>(l) with
        
            override x.Matches(r : obj) =
                l.ToString() = r.ToString()
        }

    let exprEqual (r : Expr) = 
        exprComparer r :> NHamcrest.IMatcher<_>

    module Opt =
        open System.Reflection

        let private keepMeth = getMethodInfo <@ keep @>
        let isSideEffect (mi : MethodInfo) =
            mi.IsGenericMethod && mi.GetGenericMethodDefinition() = keepMeth

        let run (expression : Expr) =
            expression
                |> Optimizer.inlining
                |> Optimizer.evaluateConstants' isSideEffect
                |> Optimizer.eliminateDeadCode' isSideEffect