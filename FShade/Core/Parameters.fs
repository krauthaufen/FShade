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
    [<AutoOpen>]
    module Uniforms =
        open Aardvark.Base.TypeInfo.Patterns
        let mutable private id = 0

        [<NoComparison>]
        type Uniform = Attribute of UniformScope * Type * string
                     | UserUniform of Type * IMod

        and UniformScope(parent : Option<UniformScope>, name : string) = 
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

                



        type SemanticException(scope : UniformScope, sem : string) =
            inherit Exception(sem)
            member x.Scope = scope
            member x.Semantic = sem

        let uniform = UniformScope(None, "Global")

        let private raiseSem (scope : UniformScope) (sem : string) =
            raise <| SemanticException(scope, sem)


        type ISemanticValue =
            //static member CreateUniform(semantic : string, scope : UniformScope)
            abstract member Semantic : string
            abstract member Scope : UniformScope

        let createSemanticValueWhenPossible (t : Type) (sem : string) (scope : UniformScope) =
            let creator = t.GetMethod("CreateUniform", [|typeof<string>; typeof<UniformScope>|])
            if creator <> null then
                let result = creator.Invoke(null, [| sem :> obj ; scope :> obj |])
                result |> Some
            else
                None

        let tryExtractUniformFromValue (value : obj) =
            match value with
                | :? ISemanticValue as v ->
                    Attribute(v.Scope, value.GetType(), v.Semantic) |> Some
                | _ -> None

        let (?) (u : UniformScope) (name : string) : 'a =
            if typeof<'a> = typeof<UniformScope> then
                u.GetChildScope name |> unbox
            else
                match createSemanticValueWhenPossible typeof<'a> name u with
                    | Some value -> value |> unbox<'a>
                    | None -> raiseSem u name

        let private (|UniformScopeType|_|) (t : Type) =
            if t = typeof<UniformScope> then UniformScopeType |> Some
            else None


        let mutable uniformDetectors : list<Expr -> Option<Uniform>> = []

        let detectUniform (e : Expr) =
            uniformDetectors |> List.tryPick (fun f -> f e)

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
