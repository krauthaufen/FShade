namespace FShade.Imperative

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.CompilerServices

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade


exception FShadeOnlyInShaderCodeException of string 

[<AutoOpen>]
module FShadeOnlyInShaderCodeExceptionPattern =
    let rec (|ShaderOnlyExn|_|) (e : Exception) =
        match e with
            | FShadeOnlyInShaderCodeException n -> 
                Some n

            | :? TargetInvocationException as e ->
                match e.InnerException with
                    | ShaderOnlyExn n -> Some n 
                    | _ -> None

            | :? AggregateException as a ->
                match Seq.toList a.InnerExceptions with
                    | [ ShaderOnlyExn n ] -> Some n 
                    | _ -> None
                

            | _ ->
                None



[<RequireQualifiedAccess>]
type ParameterDecoration =
    | Interpolation of InterpolationMode
    | Memory of MemoryType
    | Const
    | Slot of int
    | StorageBuffer of read : bool * write : bool
    | Shared
    
[<RequireQualifiedAccess>]
type UniformDecoration =
    | Format of System.Type

type Uniform =
    {
        uniformType         : Type
        uniformName         : string
        uniformDecorations  : list<UniformDecoration>
        uniformBuffer       : Option<string>
        uniformTextureInfo  : list<string * obj>
    }

type ParameterKind =
    | Input         = 0
    | Output        = 1
    | Uniform       = 2
    | Argument      = 3

type EntryParameter =
    {
        paramType           : Type
        paramName           : string
        paramSemantic       : string
        paramDecorations    : Set<ParameterDecoration>
    }

type ShaderStageDescription =
    {
        prev : Option<ShaderStage>
        self : ShaderStage
        next : Option<ShaderStage>
    }

[<RequireQualifiedAccess>]
type EntryDecoration =
    | Stages of ShaderStageDescription
    | InputTopology of InputTopology
    | OutputTopology of OutputTopology
    | OutputVertices of int
    | LocalSize of V3i
    | Invocations of int

type EntryPoint =
    {
        conditional : Option<string>
        entryName   : string
        inputs      : list<EntryParameter>
        outputs     : list<EntryParameter>
        uniforms    : list<Uniform>
        arguments   : list<EntryParameter>
        body        : Expr
        decorations : list<EntryDecoration>
    }

[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property)>]
type InlineAttribute() = inherit System.Attribute()


[<AutoOpen>]
module ExpressionExtensions =
    

    type ShaderIO private() =
        static let allMethods = typeof<ShaderIO>.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)

        static let find (name : string) (types : array<Type>) =
            allMethods |> Array.find (fun mi -> 
                if mi.Name = name then
                    let p = mi.GetParameters()
                    if p.Length = types.Length then
                        Array.forall2 (fun (p : ParameterInfo) (t : Type) -> isNull t || p.ParameterType.IsAssignableFrom t) p types
                    else
                        false
                else
                    false
            )

        static let readInput        = find "ReadInput" [| typeof<ParameterKind>; typeof<string> |]
        static let readInputIndex   = find "ReadInput" [| typeof<ParameterKind>; typeof<string>; typeof<int> |]
        static let writeOutputs     = find "WriteOutputs" [| typeof<array<string * int * obj>> |]

        static member internal ReadInputMeth = readInput
        static member internal ReadInputIndexedMeth = readInputIndex
        static member internal WriteOutputsMeth = writeOutputs

        static member ReadInput<'a>(kind : ParameterKind, name : string) : 'a =
            raise <| FShadeOnlyInShaderCodeException "ReadInput"

        static member ReadInput<'a>(kind : ParameterKind, name : string, index : int) : 'a =
            raise <| FShadeOnlyInShaderCodeException "ReadInput"

        static member WriteOutputs(values : array<string * int * obj>) : unit =
            raise <| FShadeOnlyInShaderCodeException "WriteOutputs"

    type Expr with

        static member ReadInput<'a>(kind : ParameterKind, name : string) : Expr<'a> =
            let mi = ShaderIO.ReadInputMeth.MakeGenericMethod [| typeof<'a> |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name) ]) |> Expr.Cast

        static member ReadInput<'a>(kind : ParameterKind, name : string, index : Expr) : Expr<'a> =
            let mi = ShaderIO.ReadInputIndexedMeth.MakeGenericMethod [| typeof<'a> |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name); index ]) |> Expr.Cast

        static member ReadInput(kind : ParameterKind, t : Type, name : string) =
            let mi = ShaderIO.ReadInputMeth.MakeGenericMethod [| t |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name) ])

        static member ReadInput(kind : ParameterKind, t : Type, name : string, index : Expr) =
            let mi = ShaderIO.ReadInputIndexedMeth.MakeGenericMethod [| t |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name); index ])
            
        static member WriteOutputsRaw(values : list<string * Option<Expr> * Expr>) =
            let values =
                values 
                    |> List.map (fun (name, index, value) -> 
                        let index = index |> Option.defaultValue (Expr.Value -1)
                        if value.Type = typeof<obj> then
                            Expr.NewTuple [ Expr.Value name; index; value ]
                        else 
                            Expr.NewTuple [ Expr.Value name; index; Expr.Coerce(value, typeof<obj>) ]
                    )

            Expr.Call(
                ShaderIO.WriteOutputsMeth,
                [ Expr.NewArray(typeof<string * int * obj>, values) ]
            )

        static member WriteOutputs(values : Map<string, Option<Expr> * Expr>) =
            Expr.WriteOutputsRaw(Map.toList values |> List.map (fun (a,(b,c)) -> a,b,c))

        static member WriteOutputs (outputs : list<string * Option<Expr> * Expr>) =
            let mutable map = Map.empty
            for (name, index, value) in outputs do
                match Map.tryFind name map with
                    | Some old ->
                        failwithf "[FShade] conflicting output-writes for semantic %s (%A vs. %A)" name old value
                    | _ ->
                        map <- Map.add name (index, value) map

            Expr.WriteOutputs map



    let (|ReadInput|_|) (e : Expr) =
        match e with
            | Call(None, mi, [ Value((:? ParameterKind as kind),_); Value((:? string as name),_) ]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = ShaderIO.ReadInputMeth ->
                Some(kind, name, None)

            | Call(None, mi, [ Value((:? ParameterKind as kind),_); Value((:? string as name),_); index ]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = ShaderIO.ReadInputIndexedMeth ->
                Some(kind, name, Some index)

            | _ ->
                None
                
    let (|WriteOutputsRaw|_|) (e : Expr) =
        match e with
            | Call(none, mi, [NewArray(_,args)]) when mi = ShaderIO.WriteOutputsMeth ->
                let args =
                    args |> List.map (fun a ->
                        match a with
                            | NewTuple [String name; index; Coerce(value, _) ] ->
                                match index with
                                    | Int32 -1 -> name, None, value
                                    | _ -> name, (Some index), value
                            | _ ->  
                                failwithf "[FShade] ill-formed WriteOutputs argument: %A" a    
                    )

                Some args
            | _ -> 
                None

    let (|WriteOutputs|_|) (e : Expr) =
        match e with
            | WriteOutputsRaw(args) ->
                let args = args |> List.map (fun (a,b,c) -> a, (b,c)) |> Map.ofList
                Some args
            | _ -> 
                None

    let private unrollMeth = getMethodInfo <@ Preprocessor.unroll : unit -> unit @>

    let private refof = getMethodInfo <@ (~&&) @>
    let private newref = getMethodInfo <@ ref @>
    let private deref = getMethodInfo <@ (!) @>
    let private setref = getMethodInfo <@ (:=) @>


    let (|RefOf|_|) (e : Expr) =
        match e with
            | Call(None, mi, [v]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = refof ->
                Some v
            | _ ->
                None

    let (|NewRef|_|) (e : Expr) =
        match e with
            | Call(None, mi, [v]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = newref ->
                Some v
            | _ ->
                None

    let (|DeRef|_|) (e : Expr) =
        match e with
            | Call(None, mi, [v]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = deref ->
                Some v
            | _ ->
                None

    let (|SetRef|_|) (e : Expr) =
        match e with
            | Call(None, mi, [r;v]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = setref ->
                Some(r,v)
            | _ ->
                None

    type MethodInfo with
        static member WriteOutputs = ShaderIO.WriteOutputsMeth
        static member ReadInput = ShaderIO.ReadInputMeth
        static member ReadInputIndexed = ShaderIO.ReadInputIndexedMeth
        static member Unroll = unrollMeth
        static member NewRef = newref
        static member DeRef = deref

module private Affected =
    open Aardvark.Base.Monads.State
    
    type State = { dependencies : Map<Var, Set<string>>; affected : Map<string, Set<string>> }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        let add (v : Var) (deps : Set<string>) = 
            State.modify (fun s -> 
                match Map.tryFind v s.dependencies with
                    | Some old -> { s with dependencies = Map.add v (Set.union old deps) s.dependencies }
                    | None -> { s with dependencies = Map.add v deps s.dependencies }
            )

        let affects (o : string) (deps : Set<string>) = 
            State.modify (fun s -> 
                match Map.tryFind o s.affected with
                    | Some old -> { s with affected = Map.add o (Set.union old deps) s.affected }
                    | None -> { s with affected = Map.add o deps s.affected }
            )

        let dependencies (v : Var) =
            State.get |> State.map (fun s -> 
                match Map.tryFind v s.dependencies with
                    | Some d -> d
                    | _ -> Set.empty
            )
       
    let rec usedInputsS (e : Expr) : State<State, Set<string>> =
        state {
            match e with
                | ReadInput(ParameterKind.Input, name, idx) ->
                    match idx with
                        | Some idx -> 
                            let! used = usedInputsS idx
                            return Set.add name used
                        | None ->
                            return Set.singleton name

                | WriteOutputs values ->
                    let! values = 
                        values |> Map.toList |> List.mapS (fun (name, (index, value)) ->
                            state {
                                let! vUsed = usedInputsS value
                                do! State.affects name vUsed
                                match index with
                                    | Some index -> 
                                        let! iUsed = usedInputsS index
                                        do! State.affects name iUsed
                                        return Set.union iUsed vUsed
                                    | _ ->
                                        return vUsed
                            }
                        )   
                    return Set.unionMany values

                | VarSet(v, e) ->
                    let! eUsed = usedInputsS e
                    do! State.add v eUsed
                    return eUsed
                    
                | Let(v, e, b) ->
                    let! eUsed = usedInputsS e
                    do! State.add v eUsed
                    let! bUsed = usedInputsS b
                    return Set.union eUsed bUsed

                | ShapeVar v -> 
                    return! State.dependencies v

                | ShapeLambda(v, b) ->
                    return! usedInputsS b

                | ShapeCombination(o, args) ->
                    let! args = args |> List.mapS usedInputsS
                    return Set.unionMany args

        }

    let getAffectedOutputsMap (e : Expr) =
        let s, _ = usedInputsS e |> State.run { dependencies = Map.empty; affected = Map.empty }
        s.affected

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EntryPoint =
    open Aardvark.Base.Monads.State
    open Microsoft.FSharp.Quotations.ExprShape
    
    let rec private substituteVarsWithReads (args : Set<Var>) (e : Expr) =
        state {
            match e with
                | ShapeVar(v) ->
                    if Set.contains v args then
                        let! s = State.get
                        
                        match Map.tryFind v s with
                            | Some p ->
                                return Expr.ReadInput(ParameterKind.Argument, e.Type, p.paramName)
                            | None ->
                                let parameter =
                                    {
                                        paramType = v.Type
                                        paramName = v.Name
                                        paramSemantic = v.Name
                                        paramDecorations = Set.empty
                                    }
                                do! State.put (Map.add v parameter s)
                                return Expr.ReadInput(ParameterKind.Argument, e.Type, parameter.paramName)

                    else
                        return e

                | ShapeLambda(v,b) ->
                    let! b = substituteVarsWithReads args b
                    return Expr.Lambda(v, b)
            
                | ShapeCombination(o, c) ->
                    let! c = c |> List.mapS (substituteVarsWithReads args)
                    return RebuildShapeCombination(o, c)
        }

    let ofLambda (name : string) (e : Expr) =
        let args, body = 
            match e with
                | Lambdas(args, body) ->
                    let args = 
                        match List.concat args with
                            | [v] when v.Type = typeof<unit> -> []
                            | a -> a

                    let parameters, body = substituteVarsWithReads (Set.ofList args) body |> State.run Map.empty
                    let args = 
                        args |> List.map (fun a -> 
                            match Map.tryFind a parameters with
                                | Some p -> p
                                | None -> { paramType = a.Type; paramName = a.Name; paramSemantic = a.Name; paramDecorations = Set.empty }
                        )
                    args, body
                | e ->
                    [], e

        {
            conditional = None
            entryName = name
            inputs = []
            outputs = []
            uniforms = []
            arguments = args
            body = body
            decorations = []
        }
      

                
    let setInputs (inputs : MapExt<string, Type>) (e : EntryPoint) =
        let oldInputs = 
            e.inputs |> List.map (fun input -> input.paramSemantic, input) |> MapExt.ofList

        let createInput (name : string) (i : Option<EntryParameter>) (t : Option<Type>) : EntryParameter =
            match t with
                | Some t ->
                    match i with
                        | Some i ->
                            if i.paramType <> t then
                                failwithf "[FShade] shader input %A has type %A but is required to have %A" name i.paramType t

                            i

                        | None ->
                            {
                                paramType = t
                                paramName = name
                                paramSemantic = name
                                paramDecorations = Set.ofList [ParameterDecoration.Interpolation InterpolationMode.Default]
                            }
                | None -> 
                    failwithf "[FShade] shader uses input %s which is not given in layout" name
              
        let newInputs =
            MapExt.map2 createInput oldInputs inputs

        { e with inputs = newInputs |> MapExt.toList |> List.map snd }

    let setUniforms (decorations : Type -> list<UniformDecoration>) (uniforms : MapExt<string, Type>) (uniformBuffers : MapExt<string, MapExt<string, Type>>) (e : EntryPoint) =
        let oldBufferUniforms =
            e.uniforms 
                |> List.choose (fun u -> match u.uniformBuffer with | Some b -> Some (u.uniformName, u) | _ -> None)
                |> MapExt.ofList

        let oldGlobalUniforms =
            e.uniforms 
                |> List.choose (fun u -> match u.uniformBuffer with | None -> Some(u.uniformName,u) | _ -> None)
                |> MapExt.ofList

        let createGlobalUniform (name : string) (l : Option<Uniform>) (r : Option<Type>) =
            match r with
                | Some r ->
                    match l with
                        | Some l ->
                            if l.uniformType <> r then
                                failwithf "[FShade] uniform %s has invalid type %A (expected: %A)" name l.uniformType r
                            l
                        | None ->
                            {
                                uniformName = name
                                uniformType = r
                                uniformDecorations = decorations r
                                uniformBuffer = None
                                uniformTextureInfo = []
                            }
                | None ->
                    failwithf "[FShade] shader requests uniform %s which is not part of layout" name

        let createBufferUniform (name : string) (l : Option<Uniform>) (r : Option<string * Type>) =
            match r with
                | Some(rBuffer, rType) ->
                    match l with
                        | Some l ->
                            if l.uniformType <> rType then
                                failwithf "[FShade] uniform %s has invalid type %A (expected: %A)" name l.uniformType rType
                            
                            { l with uniformBuffer = Some rBuffer }

                        | None ->
                            {
                                uniformType = rType
                                uniformName = name
                                uniformDecorations = decorations rType
                                uniformBuffer = Some rBuffer
                                uniformTextureInfo = []
                            }
                | None ->
                    failwithf "[FShade] shader requests uniform %s which is not part of layout" name

        let bufferUniforms =
            uniformBuffers 
                |> MapExt.toList 
                |> List.collect (fun (b,us) ->
                    us |> MapExt.toList |> List.map (fun (n,t) -> n,(b, t))
                  )   
                |> MapExt.ofList


        let newBufferUniforms =
            MapExt.map2 createBufferUniform oldBufferUniforms bufferUniforms

        let newUniforms =
            MapExt.map2 createGlobalUniform oldGlobalUniforms uniforms

        let result =
            MapExt.union newBufferUniforms newUniforms
                |> MapExt.toList
                |> List.map snd

        { e with uniforms = result }


type Module = 
    { 
        hash : string
        userData : obj
        entries : list<EntryPoint>
        tryGetOverrideCode : MethodBase -> Option<Expr> 
    }
       
//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module Module =

//    let ofLambda (name : string) (e : Expr) =
//        { hash = Expr.ComputeHash e; userData = null; entries = [EntryPoint.ofLambda name e]; tryGetOverrideCode = constF None }

//    let ofLambdas (l : list<string * Expr>) =
//        { hash = Guid.NewGuid() |> string; userData = null; entries = l |> List.map (uncurry EntryPoint.ofLambda); tryGetOverrideCode = constF None }
