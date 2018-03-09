
open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open System.Reflection
open System.Threading
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open System


module Shader =
    open FShade


    type UniformScope with
        member x.Dimension : int = uniform?Arguments?Dimension
        member x.Data : float[] = uniform?StorageBuffer?Data
        member x.Perm : int[] = uniform?StorageBuffer?Perm
        member x.Axis : int[] = uniform?StorageBuffer?Axis
        member x.Radius : float[] = uniform?StorageBuffer?Radius
        member x.Queries : float[] = uniform?StorageBuffer?Queries

    [<ReflectedDefinition>]
    let distance (qi : int) (di : int) =
        let dim = uniform.Dimension
        let mutable qOff = dim * qi
        let mutable dOff = dim * di

        let mutable sum = 0.0
        for i in 0 .. uniform.Dimension - 1 do
            let l = uniform.Queries.[qOff]
            let r = uniform.Data.[dOff]

            let d = r - l
            sum <- sum + d * d
            qOff <- qOff + 1
            dOff <- dOff + 1

        sqrt sum




    [<LocalSize(X = 64)>]
    let closestPointQuery (queryCount : int) (dataCount : int) (eps : float) (result : int[]) =
        compute {
            let stack : int[] = allocateShared 2048
            let distStack : float[] = allocateShared 2048

            let qid = getGlobalId().X
            if qid < queryCount then
                let baseIndex = getLocalId().X * 32

                let dim = uniform.Dimension
                //let stack : Arr<16 N, V2d> = Unchecked.defaultof<_>

                // push 0 to the stack
                let mutable stackId = baseIndex
                stack.[stackId] <- 0
                distStack.[stackId] <- 0.0

                let mutable maxDist = 10000000000.0
                let mutable maxDistEps = 10000000000.0
                let mutable closest = -1

                while stackId >= baseIndex do
                    // take the top-element
                    let top = stack.[stackId]
                    let dimDist = distStack.[stackId]
                    stackId <- stackId - 1

                    if maxDistEps >= dimDist then
                        let index = uniform.Perm.[top]

                        // the two children (if any)
                        let mutable t1 = 2 * top + 1
                        let mutable t2 = t1 + 1

                        let dist = distance qid index
                        let delta = dist - maxDist

                        if delta < 0.0 then
                            maxDist <- dist
                            maxDistEps <- dist + eps
                            closest <- index

                        elif t1 < dataCount && delta > uniform.Radius.[top] then
                            t1 <- dataCount

                            
                        if t1 < dataCount then
                            let d = uniform.Axis.[top]
                            let x = uniform.Queries.[dim * qid + d]
                            let s = uniform.Data.[dim * index + d]

                            let delta = s - x
                            if delta > 0.0 then
                                if t2 < dataCount then
                                    stackId <- stackId + 1
                                    stack.[stackId] <- t2
                                    distStack.[stackId] <- delta

                                stackId <- stackId + 1
                                stack.[stackId] <- t1
                                distStack.[stackId] <- dimDist
                            else
                                stackId <- stackId + 1    
                                stack.[stackId] <- t1 //V2d(float t1, -delta)
                                distStack.[stackId] <- -delta

                                if t2 < dataCount then
                                    stackId <- stackId + 1
                                    stack.[stackId] <- t2 // V2d(float t2, dimDist)
                                distStack.[stackId] <- dimDist

                result.[qid] <- closest

        }






[<EntryPoint>]
let main args =
    let hugo = 100

    let shader = ComputeShader.ofFunction (V3i(128, 128, 128)) Shader.closestPointQuery

    let glsl =
        shader |> ComputeShader.toModule |> ModuleCompiler.compileGLSL410

    printfn "%s" glsl.code


    0

