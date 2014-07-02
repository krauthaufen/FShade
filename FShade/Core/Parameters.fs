namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Aardvark.Base
open FShade.Compiler
open Aardvark.Base.TypeInfo.Patterns

[<AutoOpen>]
module Parameters =

    type SemanticAttribute(s : string, target : Option<string>) =
        inherit Attribute()
        member x.Semantic = s
        member x.Target = target

        new(s : string) = SemanticAttribute(s, None)

    type System.Reflection.MemberInfo with
        member x.Type =
            match x with
                | :? PropertyInfo as p -> p.PropertyType
                | :? FieldInfo as f -> f.FieldType
                | _ -> failwith "no member type could be determined"

        member x.Semantic =
            let att = x.GetCustomAttributes<SemanticAttribute>(true) |> Seq.toList
            match att with
                | x::_ -> x.Semantic
                | _ -> x.Name

        
        member x.AssignedTarget =
            let att = x.GetCustomAttributes<SemanticAttribute>(true) |> Seq.toList
            match att with
                | x::_ -> x.Target
                | _ -> None

    [<AutoOpen>]
    module Uniforms =
        open Operators
        let mutable private id = 0

        type UniformScope(parent : Option<UniformScope>, name : string) = 
            let id = System.Threading.Interlocked.Increment(&id)
            let childScopes = System.Collections.Generic.Dictionary<string, UniformScope>()

            member private x.Id = id
            interface IComparable with
                member x.CompareTo o =
                    match o with
                        | :? UniformScope as o -> id.CompareTo o.Id
                        | _ -> failwith "uncomparable"

            override x.GetHashCode() =
                id.GetHashCode()

            override x.Equals(o) =
                match o with
                    | :? UniformScope as o -> o.Id = id
                    | _ -> false

            member x.Parent = parent
            member x.Name = name
            member x.GetChildScope(n : string) =
                match childScopes.TryGetValue n with
                    | (true,s) -> s
                    | _ -> let s = UniformScope(Some x, n)
                           childScopes.[n] <- s
                           s

                


        [<NoComparison>]
        type Uniform = Attribute of UniformScope * Type * string
                     | UserUniform of Type * IMod

        type SemanticException(scope : UniformScope, sem : string) =
            inherit Exception(sem)
            member x.Scope = scope
            member x.Semantic = sem

        let uniform = UniformScope(None, "Global")

        let private raiseSem (scope : UniformScope) (sem : string) =
            raise <| SemanticException(scope, sem)


        let (?) (u : UniformScope) (name : string) : 'a =
            if typeof<'a> = typeof<UniformScope> then
                u.GetChildScope name |> unbox
            else
                raiseSem u name

        let private (|UniformScopeType|_|) (t : Type) =
            if t = typeof<UniformScope> then UniformScopeType |> Some
            else None


        let mutable uniformDetectors : list<Expr -> Option<Uniform>> = []

        let private detectUniform (e : Expr) =
            uniformDetectors |> List.tryPick (fun f -> f e)

        let (|Uniform|_|) (e : Expr) =
            match detectUniform e with
                | Some u -> Uniform(u) |> Some
                | None -> 
                    match e with

                        | Call(None, Method("op_Dynamic", [UniformScopeType; String]), [scope; Value(s,_)]) ->
                            match Expr.tryEval scope with
                                | Some scope ->
                                    let scope = scope |> unbox
                                    Uniform(Attribute(scope, e.Type, s |> unbox<string>)) |> Some
                                | None ->
                                    None

                        | PropertyGet(Some scope, p, []) when scope.Type = typeof<UniformScope> ->
                            try
                                match Expr.tryEval scope with
                                    | Some scope ->
                                        p.GetValue(scope, [||]) |> ignore
                                        None
                                    | None ->
                                        None
                            with :? TargetInvocationException as ex ->
                                match ex.InnerException with
                                    | :? SemanticException as s -> Uniform(Attribute(s.Scope, p.PropertyType, s.Semantic)) |> Some
                                    | _ -> None

                        | Call(None, m, [scope]) when scope.Type = typeof<UniformScope> ->
                            try
                                match Expr.tryEval scope with
                                    | Some scope ->
                                        m.Invoke(null, [| scope |]) |> ignore
                                        None
                                    | None ->
                                        None
                            with :? TargetInvocationException as ex ->
                                match ex.InnerException with
                                    | :? SemanticException as s -> Uniform(Attribute(s.Scope, m.ReturnType, s.Semantic)) |> Some
                                    | _ -> None

                        | _ -> None