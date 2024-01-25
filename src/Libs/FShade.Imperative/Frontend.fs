namespace FShade.Imperative

open System
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Aardvark.Base
open FShade

[<RequireQualifiedAccess>]
type ParameterDecoration =
    | Interpolation of InterpolationMode
    | Memory of MemoryType
    | Const
    | Slot of int
    | StorageBuffer of read : bool * write : bool
    | Shared
    | DepthWrite of DepthWriteMode

[<RequireQualifiedAccess>]
type UniformDecoration =
    | Format of System.Type
    | FieldIndex of int
    | BufferDescriptorSet of int
    | BufferBinding of int

type Uniform =
    {
        uniformType         : Type
        uniformName         : string
        uniformDecorations  : list<UniformDecoration>
        uniformBuffer       : Option<string>
        uniformTextureInfo  : list<string * obj>
    }

[<RequireQualifiedAccess>]
type RaytracingDataKind =
    | RayPayload of location: int
    | RayPayloadIn
    | CallableData of location : int
    | CallableDataIn
    | HitAttribute

type RaytracingData =
    {
        rtdataType : Type
        rtdataName : string
        rtdataKind : RaytracingDataKind
    }

type ParameterKind =
    | Input          = 0
    | Output         = 1
    | Uniform        = 2
    | Argument       = 3
    | RaytracingData = 4

type EntryParameter =
    {
        paramType           : Type
        paramName           : string
        paramSemantic       : string
        paramDecorations    : Set<ParameterDecoration>
    }

type GraphicsStageDescription =
    {
        prev : Option<ShaderStage>
        self : ShaderStage
        next : Option<ShaderStage>
    }

    member x.Slot =
        match x.self with
        | ShaderStage.Vertex      -> ShaderSlot.Vertex
        | ShaderStage.TessControl -> ShaderSlot.TessControl
        | ShaderStage.TessEval    -> ShaderSlot.TessEval
        | ShaderStage.Geometry    -> ShaderSlot.Geometry
        | ShaderStage.Fragment    -> ShaderSlot.Fragment
        | s -> failwithf "invalid graphics stage %A" s

[<RequireQualifiedAccess>]
type ShaderStageDescription =
    | Compute
    | Graphics   of GraphicsStageDescription
    | Raytracing of ShaderSlot

    member x.Slot =
        match x with
        | Compute -> ShaderSlot.Compute
        | Graphics g -> g.Slot
        | Raytracing r -> r

    member x.Stage =
        x.Slot.Stage

    member x.Previous =
        match x with
        | Graphics g -> g.prev
        | _ -> None

    member x.Next =
        match x with
        | Graphics g -> g.next
        | _ -> None

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
        conditional    : Option<string>
        entryName      : string
        inputs         : list<EntryParameter>
        outputs        : list<EntryParameter>
        uniforms       : list<Uniform>
        arguments      : list<EntryParameter>
        raytracingData : list<RaytracingData>
        body           : Expr
        decorations    : list<EntryDecoration>
    }
    
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property)>]
type InlineAttribute() = inherit System.Attribute()

[<AttributeUsage(AttributeTargets.Method)>]
type KeepCallAttribute() = inherit System.Attribute()


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

        static let readInput        = find "ReadInput" [| typeof<ParameterKind>; typeof<string>; typeof<ShaderSlot option> |]
        static let readInputIndex   = find "ReadInput" [| typeof<ParameterKind>; typeof<string>; typeof<int>; typeof<ShaderSlot option> |]
        static let writeOutputs     = find "WriteOutputs" [| typeof<array<string * int * obj>> |]
        static let unsafeWrite      = allMethods |> Array.find (fun m -> m.Name = "UnsafeWrite")

        static member internal ReadInputMeth = readInput
        static member internal ReadInputIndexedMeth = readInputIndex
        static member internal WriteOutputsMeth = writeOutputs
        static member internal UnsafeWriteMeth = unsafeWrite

        static member ReadInput<'a>(kind : ParameterKind, name : string, slot : Option<ShaderSlot>) : 'a =
            onlyInShaderCode "ReadInput"

        static member ReadInput<'a>(kind : ParameterKind, name : string, index : int, slot : Option<ShaderSlot>) : 'a =
            onlyInShaderCode "ReadInput"

        static member WriteOutputs(values : array<string * int * obj>) : unit =
            onlyInShaderCode "WriteOutputs"

        static member UnsafeWrite(dst : 'a, value : 'a) : unit =
            onlyInShaderCode "UnsafeWrite"

    type Expr with

        static member UnsafePropertySet(target : Expr, prop : PropertyInfo, indices : Expr list, value : Expr) =
            let mi = ShaderIO.UnsafeWriteMeth.MakeGenericMethod [| value.Type |]
            Expr.Call(mi, [Expr.PropertyGet(target, prop, indices); value])

        static member UnsafePropertySet(target : Expr, prop : PropertyInfo, value : Expr) =
            Expr.UnsafePropertySet(target, prop, [], value)

        static member UnsafeWrite(target : Expr, value : Expr) =
            let mi = ShaderIO.UnsafeWriteMeth.MakeGenericMethod [| value.Type |]
            Expr.Call(mi, [target; value])

        static member ReadInput<'a>(kind : ParameterKind, name : string, slot : Option<ShaderSlot>) : Expr<'a> =
            let mi = ShaderIO.ReadInputMeth.MakeGenericMethod [| typeof<'a> |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name); Expr.Value(slot) ]) |> Expr.Cast

        static member ReadInput<'a>(kind : ParameterKind, name : string, index : Expr, slot : Option<ShaderSlot>) : Expr<'a> =
            let mi = ShaderIO.ReadInputIndexedMeth.MakeGenericMethod [| typeof<'a> |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name); index; Expr.Value(slot) ]) |> Expr.Cast

        static member ReadInput(kind : ParameterKind, t : Type, name : string, slot : Option<ShaderSlot>) =
            let mi = ShaderIO.ReadInputMeth.MakeGenericMethod [| t |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name); Expr.Value(slot) ])

        static member ReadInput(kind : ParameterKind, t : Type, name : string, index : Expr, slot : Option<ShaderSlot>) =
            let mi = ShaderIO.ReadInputIndexedMeth.MakeGenericMethod [| t |]
            Expr.Call(mi, [ Expr.Value(kind); Expr.Value(name); index; Expr.Value(slot) ])

        static member ReadInput<'a>(kind : ParameterKind, name : string) =
            Expr.ReadInput<'a>(kind, name, None)

        static member ReadInput<'a>(kind : ParameterKind, name : string, index : Expr) =
            Expr.ReadInput<'a>(kind, name, index, None)

        static member ReadInput(kind : ParameterKind, t : Type, name : string) =
            Expr.ReadInput(kind, t, name, None)

        static member ReadInput(kind : ParameterKind, t : Type, name : string, index : Expr) =
            Expr.ReadInput(kind, t, name, index, None)

        static member ReadRaytracingData<'a>(name : string) =
            Expr.ReadInput<'a>(ParameterKind.RaytracingData, name, None)

        static member ReadRaytracingData<'a>(name : string, slot : ShaderSlot) =
            Expr.ReadInput<'a>(ParameterKind.RaytracingData, name, Some slot)

        static member ReadRaytracingData(t : Type, name : string) =
            Expr.ReadInput(ParameterKind.RaytracingData, t, name, None)

        static member ReadRaytracingData(t : Type, name : string, slot : ShaderSlot) =
            Expr.ReadInput(ParameterKind.RaytracingData, t, name, Some slot)

        static member WriteRaytracingData(name : string, value : Expr) =
            let t = Expr.ReadRaytracingData(value.Type, name)
            Expr.UnsafeWrite(t, value)

        static member WriteRaytracingData(name : string, value : Expr, slot : ShaderSlot) =
            let t = Expr.ReadRaytracingData(value.Type, name, slot)
            Expr.UnsafeWrite(t, value)

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

    let (|UnsafeWrite|_|) (e : Expr) =
        match e with
        | Call(None, mi, [target; value]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = ShaderIO.UnsafeWriteMeth ->
            Some (target, value)
        | _ ->
            None

    let (|ReadInputOrRaytracingData|_|) (e : Expr) =
        match e with
        | Call(None, mi, [ Value((:? ParameterKind as kind),_); Value((:? string as name),_); Constant(:? Option<ShaderSlot> as slot)]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = ShaderIO.ReadInputMeth ->
            Some(kind, name, None, slot)

        | Call(None, mi, [ Value((:? ParameterKind as kind),_); Value((:? string as name),_); index;  Constant(:? Option<ShaderSlot> as slot)]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = ShaderIO.ReadInputIndexedMeth ->
            Some(kind, name, Some index, slot)

        | _ ->
            None

    let (|ReadInput|_|) (e : Expr) =
        match e with
        | ReadInputOrRaytracingData (kind, name, index, _) when kind <> ParameterKind.RaytracingData ->
            Some (kind, name, index)

        | _ ->
            None

    let (|ReadRaytracingData|_|) (e : Expr) =
        match e with
        | ReadInputOrRaytracingData (ParameterKind.RaytracingData, name, None, slot) ->
            Some (name, slot)

        | _ ->
            None

    let (|WriteOutputsRaw|_|) (e : Expr) =
        match e with
            | Call(none, mi, [NewArray(_,args)]) when mi = ShaderIO.WriteOutputsMeth ->
                let args =
                    args |> List.map (fun a ->
                        match a with
                            | NewTuple [String name; index; value ] ->
                                let value =
                                    match value with
                                    | Coerce(value, t) when t = typeof<obj> -> value
                                    | e -> e
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

    let (|RefExpr|_|) (e : Expr) =
        match e.Type with
        | Ref inner -> Some inner
        | _ -> None

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
        | PropertyGet(Some (RefExpr _ as v), pi, []) when pi.Name = "Value" ->
            Some v
        | _ ->
            None

    let (|SetRef|_|) (e : Expr) =
        match e with
        | Call(None, mi, [r;v]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = setref ->
            Some(r, v)
        | PropertySet(Some (RefExpr _ as r), pi, [], v) when pi.Name = "Value" ->
            Some(r, v)
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
                | ReadInputOrRaytracingData(ParameterKind.Input, name, idx, _) ->
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
            raytracingData = []
            body = body
            decorations = []
        }

type Module =
    val private entries : Lazy<EntryPoint list>
    val Hash : string
    val UserData : obj
    val TryGetOverrideCode : MethodBase -> Expr option

    new (hash : string, userData : obj, entries : Lazy<EntryPoint list>, tryGetOverrideCode : MethodBase -> Expr option) =
        { entries = entries
          Hash = hash
          UserData = userData
          TryGetOverrideCode = tryGetOverrideCode }

    member x.Entries = x.entries.Value

//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module Module =

//    let ofLambda (name : string) (e : Expr) =
//        { hash = Expr.ComputeHash e; userData = null; entries = [EntryPoint.ofLambda name e]; tryGetOverrideCode = constF None }

//    let ofLambdas (l : list<string * Expr>) =
//        { hash = Guid.NewGuid() |> string; userData = null; entries = l |> List.map (uncurry EntryPoint.ofLambda); tryGetOverrideCode = constF None }
