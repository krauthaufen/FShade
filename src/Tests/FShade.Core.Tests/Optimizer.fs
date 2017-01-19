module Optimizer

open Microsoft.FSharp.Quotations

open FsUnit
open NUnit.Framework
open NUnit.Framework.Constraints

open FShade

let sink a = ()

type ExpressionEqualConstraint(desired : Expr) =
    inherit Constraint()

    override x.ApplyTo<'a>(real : 'a) : ConstraintResult =
        match real :> obj with
            | :? Expr as real ->
                failwith ""

            | _ ->
                failwith ""

let exprEqual (r : Expr) = ExpressionEqualConstraint(r)

[<Test>]
let bla() =
    let input =
        <@
            // long dependency chain test
            // needs 4 iterations in fixpoint search when a is used
            // a <- b <- c <- d <- a ...
            let mutable a = 0
            let mutable b = 0
            let mutable c = 0
            let mutable d = 0
            for i in 0 .. 2 .. 10 do
                a <- a + b
                b <- b + c
                c <- c + d
                d <- d + a
            sink a
        @>

    let output = 
        Optimizer.eliminateDeadCode input

    output |> should exprEqual input


