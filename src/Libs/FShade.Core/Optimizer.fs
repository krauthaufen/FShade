namespace FShade

open System
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative


[<AutoOpen>]
module Optimizer =
    open Aardvark.Base.Monads.State

    module UtilityFunction =

        let rec (|TrivialOrInput|_|) (e : Expr) =
            match e with
                | Var _ 
                | Value _
                | FieldGet(None, _)
                | PropertyGet(None, _, [])
                | TupleGet(Trivial, _)
                | PropertyGet(Some TrivialOrInput, (FSharpTypeProperty | ArrayLengthProperty), [])
                | FieldGet(Some TrivialOrInput, _) 
                | ReadInput(_, _, None) 
                | ReadInput(_, _, Some TrivialOrInput) -> 
                    Some()
                | _ ->
                    None

        let inlineCode (f : UtilityFunction) (args : list<Expr>) =
            let rec wrap (v : list<Var>) (e : list<Expr>) (b : Expr) =
                match v, e with
                    | [], [] -> 
                        b

                    | v :: vs, (TrivialOrInput as ev) :: es ->
                        let b = b.Substitute(fun vi -> if vi = v then Some ev else None)
                        wrap vs es b

                    | v :: vs, e :: es ->
                        Expr.Let(v,e, wrap vs es b)

                    | _ ->
                        failwithf "[FShade] bad arity for utility function call"

            wrap f.functionArguments args f.functionBody

    [<AutoOpen>]
    module Helpers = 
        module State =
            let withLocalState (m : State<'s, 'a>) =
                State.get |> State.map (fun s ->
                    m |> State.run s
                )

        module Option =
            let mapS (f : 'a -> State<'s, 'b>) (m : Option<'a>) =
                match m with
                    | Some a -> a |> f |> State.map Some
                    | None -> State.value None

        let rec (|LExpr|_|) (e : Expr) =
            // TODO: complete?
            match e with
                | Var v -> Some v
                | FieldGet(Some (LExpr v), fi) -> Some v
                | PropertyGet(Some (LExpr v), _, _) -> Some v
                | GetArray(LExpr a, i) -> Some a
                | _ -> None

        let rec (|MutableArgument|_|) (e : Expr) =
            match e with
                | Var v -> 
                    if v.IsMutable then
                        Some v
                    else
                        match v.Type with
                            | ArrOf _ -> Some v
                            | ArrayOf _ -> Some v
                            | _ -> None

                | AddressOf (MutableArgument v) -> Some v
                | _ -> None

    module DeadCodeElimination = 

        type EliminationState =
            {
                isGlobalSideEffect : MethodInfo -> bool
                usedVariables : Set<Var>
            }


        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module EliminationState =
            let empty = 
                { 
                    isGlobalSideEffect = fun mi -> mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<System.Void>
                    usedVariables = Set.empty
                }
            let useVar (v : Var) = State.modify (fun s -> { s with usedVariables = Set.add v s.usedVariables })
            let remVar (v : Var) = State.modify (fun s -> { s with usedVariables = Set.remove v s.usedVariables })
            let isUsed (v : Var) = State.get |> State.map (fun s -> Set.contains v s.usedVariables)
            let useVars (vars : seq<Var>) = State.modify (fun s -> { s with usedVariables = Set.union (Set.ofSeq vars) s.usedVariables })
       

            let merge (l : EliminationState) (r : EliminationState) =
                {
                    isGlobalSideEffect = l.isGlobalSideEffect
                    usedVariables = Set.union l.usedVariables r.usedVariables
                }

            let rec fix (m : State<EliminationState, 'a>) =
                state {
                    let! initial = State.get
                    let! res = m
                    let! final = State.get
                    if initial.usedVariables = final.usedVariables then return res
                    else return! fix m
                }

        let private staticallyNeededCalls =
            HashSet.ofList [
                MethodInfo.WriteOutputs
                MethodInfo.Unroll
            ]

        let private callNeededS (t : Option<Expr>) (mi : MethodInfo) (args : list<Expr>) =
            state {
                let! state = State.get

                if state.isGlobalSideEffect mi || staticallyNeededCalls.Contains mi then
                    return true
                else
                    let needsMutableParameter (p : ParameterInfo) (v : Expr) =
                        let pt = p.ParameterType
                        if pt.IsByRef || pt.IsArray || pt.IsArr then
                            match v with
                                | MutableArgument v -> Set.contains v state.usedVariables
                                | _ -> false
                        else
                            false

                    let parameters = mi.GetParameters() |> Array.toList
                    match t with
                        | Some t ->
                            match t with
                                | MutableArgument v when Set.contains v state.usedVariables -> 
                                    return true
                                | _ -> 
                                    return List.exists2 needsMutableParameter parameters args
                        | None ->
                            return List.exists2 needsMutableParameter parameters args
            }

        let rec withoutValueS (e : Expr) : State<EliminationState, Expr> =
            state {
                match e with
                    | ExprOf t when t = typeof<unit> ->
                        return! eliminateDeadCodeS e

                    
                    | IfThenElse(cond, i, e) ->
                        let! elseState, e = State.withLocalState (withoutValueS e)
                        let! ifState, i = State.withLocalState (withoutValueS i)
                        do! State.put (EliminationState.merge ifState elseState)
                    
                        match i, e with
                            | Unit, Unit -> 
                                return! withoutValueS cond
                            | _ ->
                                let! cond = eliminateDeadCodeS cond
                                return Expr.IfThenElse(cond, i, e)

                    | AddressOf e ->
                        return! withoutValueS e

                    | AddressSet(e,v) ->
                        let! v = withoutValueS v
                        let! e = withoutValueS e
                        return Expr.Seq [v; e]

                    | Application(l, a) ->
                        let! a = eliminateDeadCodeS a
                        let! l = eliminateDeadCodeS l
                        return Expr.Application(l, a)

                    | CallFunction(utility, args) ->
                        let code = UtilityFunction.inlineCode utility args
                        return! withoutValueS code

                    | Call(t, mi, args) ->
                        let! needed = callNeededS t mi args

                        if needed then
                            let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                            let! t = t |> Option.mapS eliminateDeadCodeS

                            match t with
                                | Some t -> return Expr.Call(t, mi, args) |> Expr.Ignore
                                | None -> return Expr.Call(mi, args) |> Expr.Ignore
                        else
                            let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                            let! t = t |> Option.mapS withoutValueS

                            match t with
                                | Some t -> return Expr.Seq (t :: args)
                                | None -> return Expr.Seq args            


                    | Coerce(e,t) ->
                        return! withoutValueS e

                    | DefaultValue t ->
                        return Expr.Unit

                    | FieldGet(None, _) -> 
                        return Expr.Unit

                    | FieldGet(Some t, _) ->
                        return! withoutValueS t

                    | Lambda(v, b) ->
                        return Expr.Unit

                    | LetRecursive _ ->
                        return failwith "recursive bindings not implemented"
                    
                    | Let(v,e,b) ->
                        let! b = withoutValueS b
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! e = eliminateDeadCodeS e
                            return Expr.Let(v, e, b)

                        else
                            let! e = withoutValueS e
                            match e with
                                | Unit -> return b
                                | _ -> return Expr.Sequential(e, b)

                    | NewArray(t, args) ->
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq args

                    | NewDelegate(t, vars, e) ->
                        return! withoutValueS e
  
                    | NewObject(ctor, args) ->
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq args

                    | NewRecord(t, args) ->
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq args

                    | NewTuple(args) ->
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq args

                    | NewUnionCase(ci, args) ->
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq args


                    | PropertyGet(None, pi, idx) ->
                        let! idx = idx |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        return Expr.Seq idx

                    | PropertyGet(Some t, pi, idx) ->
                        let! idx = idx |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        let! t = withoutValueS t
                        return Expr.Seq (t :: idx)
                    
                    | QuoteRaw _ | QuoteTyped _ -> 
                        return failwith "not implemented"

                    | Sequential(l, r) ->
                        let! r = withoutValueS r
                        let! l = withoutValueS l
                        match l, r with
                            | Unit, r   -> return r
                            | l, Unit   -> return l
                            | l, r      -> return Expr.Sequential(l, r)

                    | TryWith _ | TryFinally _ -> 
                        return failwith "not implemented"

                    | TupleGet(t, i) ->
                        return! withoutValueS t

                    | TypeTest(e, t) ->
                        return! withoutValueS e

                    | UnionCaseTest(e, ci) ->
                        return! withoutValueS e

                    | Value _ ->
                        return Expr.Unit

                    | Var v ->
                        return Expr.Unit

                    | _ ->
                        return failwith ""

        
            }

        and eliminateDeadCodeS (e : Expr) : State<EliminationState, Expr> =
            state {
                match e with

                    // side effects affecting variables
                    | VarSet(v, e) ->
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! e = eliminateDeadCodeS e
                            return Expr.VarSet(v, e)
                        else
                            return! withoutValueS e

                    | FieldSet(Some (LExpr v as target), fi, value) ->
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! value = eliminateDeadCodeS value
                            let! target = eliminateDeadCodeS target
                            return Expr.FieldSet(target, fi, value)
                        else
                            let! v = withoutValueS value
                            let! t = withoutValueS target
                            return Expr.Seq [v;t]

                    | PropertySet(Some (LExpr v as target), pi, idx, value) ->
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! value = eliminateDeadCodeS value
                            let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                            let! target = eliminateDeadCodeS target
                            return Expr.PropertySet(target, pi, value, idx)
                        else 
                            let! value = withoutValueS value
                            let! idx = idx |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                            let! target = withoutValueS target

                            return Expr.Seq [
                                yield target
                                yield! idx
                                yield value
                            ]

                    // global side effects
                    | FieldSet(None, f, value) ->
                        let! value = eliminateDeadCodeS value
                        return Expr.FieldSet(f, value)

                    | PropertySet(None, pi, idx, value) ->
                        let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        let! value = eliminateDeadCodeS value
                        return Expr.PropertySet(pi, value, idx)

                    // unknown side effects
                    | FieldSet(Some t, f, value) ->
                        Log.warn "[FShade] found FieldSet on unknown expression: %A" t
                        let! value = eliminateDeadCodeS value
                        let! t = eliminateDeadCodeS t
                        return Expr.FieldSet(t, f, value)

                    | PropertySet(Some t, pi, idx, value) ->
                        Log.warn "[FShade] found PropertySet on unknown expression: %A" t
                        let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        let! value = eliminateDeadCodeS value
                        let! t = eliminateDeadCodeS t
                        return Expr.PropertySet(t, pi, value, idx)


                    // control-flow
                    | IfThenElse(cond, i, e) ->
                        let! elseState, e = State.withLocalState (eliminateDeadCodeS e)
                        let! ifState, i = State.withLocalState (eliminateDeadCodeS i)
                        do! State.put (EliminationState.merge ifState elseState)

                        match i, e with
                            | Unit, Unit ->
                                return! withoutValueS cond
                            | _ ->
                                let! cond = eliminateDeadCodeS cond
                                return Expr.IfThenElse(cond, i, e)

                    | ForEach(v, seq, body) ->
                        let iterate =
                            state {
                                let! body = eliminateDeadCodeS body
                                match body with
                                    | Unit ->
                                        return! withoutValueS seq
                                    | _ ->
                                        let! seq = eliminateDeadCodeS seq
                                        return Expr.ForEach(v, seq, body)
                                    
                            }
                        return! EliminationState.fix iterate

                    | ForInteger(v, first, step, last, body) ->
                        let iterate =
                            state {
                                let! body = eliminateDeadCodeS body
                                match body with
                                    | Unit ->
                                        let! last = withoutValueS last
                                        let! step = withoutValueS step
                                        let! first = withoutValueS first
                                        return Expr.Seq [first; step; last]
                                    | _ ->
                                        let! last = eliminateDeadCodeS last
                                        let! step = eliminateDeadCodeS step
                                        let! first = eliminateDeadCodeS first
                                        return Expr.ForInteger(v, first, step, last, body)
                                    
                            }

                        return! EliminationState.fix iterate

                    | WhileLoop(guard, body) ->
                        let iterate =
                            state {
                                let! body = eliminateDeadCodeS body
                                match body with
                                    | Unit -> 
                                        let! guardState, guard = State.withLocalState (withoutValueS guard)
                                        match guard with
                                            | Unit ->
                                                do! State.put guardState
                                                return Expr.Unit
                                            | _ -> 
                                                let! guard = eliminateDeadCodeS guard
                                                return Expr.WhileLoop(guard, body)
                                    | _ ->
                                        let! guard = eliminateDeadCodeS guard
                                        return Expr.WhileLoop(guard, body)
                            }

                        return! EliminationState.fix iterate


                    | Ignore e ->
                        return! withoutValueS e

                    | AddressOf e ->
                        let! e = eliminateDeadCodeS e
                        return Expr.AddressOf(e)

                    | AddressSet(e,v) ->
                        let! v = eliminateDeadCodeS v
                        let! e = eliminateDeadCodeS e
                        return Expr.AddressSet(e, v)

                    | Application(l, a) ->
                        let! a = eliminateDeadCodeS a
                        let! l = eliminateDeadCodeS l
                        return Expr.Application(l, a)
           
                    | CallFunction(utility, args) ->
                        if utility.returnType = typeof<unit> then
                            let code = UtilityFunction.inlineCode utility args
                            return! withoutValueS code
                        else
                            // TODO: is the call needed???
                            let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                            let! s = State.get

                            let utility =
                                utility |> UtilityFunction.map (fun b ->
                                    let mutable innerState = { EliminationState.empty with isGlobalSideEffect = s.isGlobalSideEffect }
                                    eliminateDeadCodeS(b).Run(&innerState)
                                )

                            let usedVariables = utility.functionBody.GetFreeVars() |> Set.ofSeq
                            let allArgsUsed = List.forall (fun v -> Set.contains v usedVariables) utility.functionArguments

                            if allArgsUsed then
                                return Expr.CallFunction(utility, args)
                            else
                                let args, values = 
                                    List.zip utility.functionArguments args
                                        |> List.filter (fun (v,_) -> Set.contains v usedVariables)
                                        |> List.unzip

                                let utility =
                                    { utility with
                                        functionArguments = args
                                        functionTag = null   
                                    }

                                return Expr.CallFunction(utility, values)

                    | Call(t, mi, args) ->
                        let! needed = 
                            if e.Type <> typeof<unit> then State.value true
                            else callNeededS t mi args

                        if needed then
                            let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                            let! t = t |> Option.mapS eliminateDeadCodeS

                            match t with
                                | Some t -> return Expr.Call(t, mi, args)
                                | None -> return Expr.Call(mi, args)
                        else
                            return Expr.Unit

                    | Coerce(e,t) ->
                        let! e = eliminateDeadCodeS e
                        return Expr.Coerce(e, t)

                    | DefaultValue t ->
                        return e

                    | FieldGet(None, f) -> 
                        return e

                    | FieldGet(Some t, f) ->
                        let! t = eliminateDeadCodeS t
                        return Expr.FieldGet(t, f)

                    | Lambda(v, b) ->
                        let! b = eliminateDeadCodeS b
                        return Expr.Lambda(v, b)
  
                    | LetRecursive _ ->
                        return failwith "recursive bindings not implemented"
                    
                    | Let(v,e,b) ->
                        let! b = eliminateDeadCodeS b
                        let! vUsed = EliminationState.isUsed v
                        if vUsed then
                            let! e = eliminateDeadCodeS e
                            return Expr.Let(v, e, b)

                        else
                            let! e = withoutValueS e
                            match e with
                                | Unit -> return b
                                | _ -> return Expr.Sequential(e, b)

                    | NewArray(t, args) ->
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.NewArray(t, args)

                    | NewDelegate(t, vars, e) ->
                        let! e = eliminateDeadCodeS e
                        return Expr.NewDelegate(t, vars, e)
  
                    | NewObject(ctor, args) ->
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.NewObject(ctor, args)

                    | NewRecord(t, args) ->
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.NewRecord(t, args)

                    | NewTuple(args) ->
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.NewTuple(args)

                    | NewUnionCase(ci, args) ->
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.NewUnionCase(ci, args)

                    | PropertyGet(None, pi, idx) ->
                        let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        return Expr.PropertyGet(pi, idx)

                    | PropertyGet(Some t, pi, idx) ->
                        let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        let! t = eliminateDeadCodeS t
                        return Expr.PropertyGet(t, pi, idx)
                    
                    | QuoteRaw _ | QuoteTyped _ -> 
                        return failwith "not implemented"

                    | Sequential(l, r) ->
                        let! r = eliminateDeadCodeS r
                        let! l = eliminateDeadCodeS l
                        match l, r with
                            | Unit, r   -> return r
                            | l, Unit   -> return l
                            | l, r      -> return Expr.Sequential(l, r)

                    | TryWith _ | TryFinally _ -> 
                        return failwith "not implemented"

                    | TupleGet(t, i) ->
                        let! t = eliminateDeadCodeS t
                        return Expr.TupleGet(t, i)

                    | TypeTest(e, t) ->
                        let! e = eliminateDeadCodeS e
                        return Expr.TypeTest(e, t)

                    | UnionCaseTest(e, ci) ->
                        let! e = eliminateDeadCodeS e
                        return Expr.UnionCaseTest(e, ci)

                    | Value _ ->
                        return e


                    | Var v ->
                        do! EliminationState.useVar v
                        return e

                    | _ ->
                        return failwithf "[FShade] unexpected expression %A" e
            }

        let withoutValue (e : Expr) =
            let run = withoutValueS e
            let mutable state = EliminationState.empty
            run.Run(&state)

        let eliminateDeadCode (e : Expr) =
            let run = eliminateDeadCodeS e
            let mutable state = EliminationState.empty
            run.Run(&state)

        let withoutValue' (isSideEffect : MethodInfo -> bool) (e : Expr) =
            let run = withoutValueS e
            let mutable state = { EliminationState.empty with isGlobalSideEffect = isSideEffect }
            run.Run(&state)

        let eliminateDeadCode' (isSideEffect : MethodInfo -> bool)  (e : Expr) =
            let run = eliminateDeadCodeS e
            let mutable state = { EliminationState.empty with isGlobalSideEffect = isSideEffect }
            run.Run(&state)

    module ConstantFolding =
        
        type State = 
            {
                variableValues : Map<Var, obj>
                isGlobalSideEffect : MemberInfo -> bool
            }

        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module State =
            let empty = 
                { 
                    variableValues = Map.empty
                    isGlobalSideEffect = fun mi -> 
                        match mi with
                            | :? MethodInfo as mi -> mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<System.Void>
                            | _ -> false
                }

            let needsCall (mi : MethodInfo) : State<State, bool> =
                State.get |> State.map (fun s -> 
                    (mi.IsGenericMethod && mi.GetGenericMethodDefinition() = MethodInfo.ReadInput) ||
                    (mi.IsGenericMethod && mi.GetGenericMethodDefinition() = MethodInfo.ReadInputIndexed) ||
                    (mi = MethodInfo.WriteOutputs) || 
                    (mi = MethodInfo.Unroll) ||
                    (s.isGlobalSideEffect (mi :> MemberInfo))
                )

            let needsField (f : FieldInfo) : State<State, bool> =
                State.get |> State.map (fun s ->
                    s.isGlobalSideEffect (f :> MemberInfo)
                )

            let needsProperty (p : PropertyInfo) : State<State, bool> =
                State.get |> State.map (fun s ->
                    s.isGlobalSideEffect (p :> MemberInfo)
                )

            let setVar (v : Var) (value : obj) =
                State.modify (fun s ->
                    { s with variableValues = Map.add v value s.variableValues }
                )

            let getVar (v : Var) =
                State.get |> State.map (fun s ->
                    Map.tryFind v s.variableValues
                )

        let rec private (|AllConstant|_|) (e : list<Expr>) =
            match e with
                | [] -> Some []
                | h :: t ->
                    match h, t with
                        | Value(h,_), AllConstant t -> Some (h :: t)
                        | _ -> None


        let functionTable (l : list<string * obj>) =
            let table = Dictionary.empty

            let rec getFunctionElements (t : Type) =
                if t.Name.StartsWith "FSharpFunc" || t.BaseType.Name.StartsWith "FSharpFunc" then
                    let a, r = FSharpType.GetFunctionElements t
                    let args, ret = getFunctionElements r
                    a :: args, ret
                else
                    [], t

            for (name, f) in l do
                let fType = f.GetType()
                let invoke : MethodInfo = 
                    fType.GetMethods(BindingFlags.Public ||| BindingFlags.Instance) 
                        |> Array.filter (fun mi -> mi.Name = "Invoke") 
                        |> Array.maxBy (fun mi -> mi.GetParameters().Length)

                let parameters =
                    invoke.GetParameters() |> Array.map (fun p -> p.ParameterType) |> Array.toList

                let run (args : list<obj>) = invoke.Invoke(f, List.toArray args)

                table.[(name, parameters)] <- run

            table

        let operators =
            functionTable [
                "op_UnaryNegation", ((~-) : int8 -> int8) :> obj
                "op_UnaryNegation", ((~-) : int16 -> int16) :> obj
                "op_UnaryNegation", ((~-) : int32 -> int32) :> obj
                "op_UnaryNegation", ((~-) : int64 -> int64) :> obj
                "op_UnaryNegation", ((~-) : float32 -> float32) :> obj
                "op_UnaryNegation", ((~-) : float -> float) :> obj
                "op_UnaryNegation", ((~-) : decimal -> decimal) :> obj

                "op_Addition", ((+) : int8 -> int8 -> int8) :> obj
                "op_Addition", ((+) : int16 -> int16 -> int16) :> obj
                "op_Addition", ((+) : int32 -> int32 -> int32) :> obj
                "op_Addition", ((+) : int64 -> int64 -> int64) :> obj
                "op_Addition", ((+) : uint8 -> uint8 -> uint8) :> obj
                "op_Addition", ((+) : uint16 -> uint16 -> uint16) :> obj
                "op_Addition", ((+) : uint32 -> uint32 -> uint32) :> obj
                "op_Addition", ((+) : uint64 -> uint64 -> uint64) :> obj
                "op_Addition", ((+) : float32 -> float32 -> float32) :> obj
                "op_Addition", ((+) : float -> float -> float) :> obj
                "op_Addition", ((+) : decimal -> decimal -> decimal) :> obj

                
                "op_Subtraction", ((-) : int8 -> int8 -> int8) :> obj
                "op_Subtraction", ((-) : int16 -> int16 -> int16) :> obj
                "op_Subtraction", ((-) : int32 -> int32 -> int32) :> obj
                "op_Subtraction", ((-) : int64 -> int64 -> int64) :> obj
                "op_Subtraction", ((-) : uint8 -> uint8 -> uint8) :> obj
                "op_Subtraction", ((-) : uint16 -> uint16 -> uint16) :> obj
                "op_Subtraction", ((-) : uint32 -> uint32 -> uint32) :> obj
                "op_Subtraction", ((-) : uint64 -> uint64 -> uint64) :> obj
                "op_Subtraction", ((-) : float32 -> float32 -> float32) :> obj
                "op_Subtraction", ((-) : float -> float -> float) :> obj
                "op_Subtraction", ((-) : decimal -> decimal -> decimal) :> obj

                
                "op_Multiply", ((*) : int8 -> int8 -> int8) :> obj
                "op_Multiply", ((*) : int16 -> int16 -> int16) :> obj
                "op_Multiply", ((*) : int32 -> int32 -> int32) :> obj
                "op_Multiply", ((*) : int64 -> int64 -> int64) :> obj
                "op_Multiply", ((*) : uint8 -> uint8 -> uint8) :> obj
                "op_Multiply", ((*) : uint16 -> uint16 -> uint16) :> obj
                "op_Multiply", ((*) : uint32 -> uint32 -> uint32) :> obj
                "op_Multiply", ((*) : uint64 -> uint64 -> uint64) :> obj
                "op_Multiply", ((*) : float32 -> float32 -> float32) :> obj
                "op_Multiply", ((*) : float -> float -> float) :> obj
                "op_Multiply", ((*) : decimal -> decimal -> decimal) :> obj

                
                "op_Division", ((/) : int8 -> int8 -> int8) :> obj
                "op_Division", ((/) : int16 -> int16 -> int16) :> obj
                "op_Division", ((/) : int32 -> int32 -> int32) :> obj
                "op_Division", ((/) : int64 -> int64 -> int64) :> obj
                "op_Division", ((/) : uint8 -> uint8 -> uint8) :> obj
                "op_Division", ((/) : uint16 -> uint16 -> uint16) :> obj
                "op_Division", ((/) : uint32 -> uint32 -> uint32) :> obj
                "op_Division", ((/) : uint64 -> uint64 -> uint64) :> obj
                "op_Division", ((/) : float32 -> float32 -> float32) :> obj
                "op_Division", ((/) : float -> float -> float) :> obj
                "op_Division", ((/) : decimal -> decimal -> decimal) :> obj

                
                "op_Modulus", ((%) : int8 -> int8 -> int8) :> obj
                "op_Modulus", ((%) : int16 -> int16 -> int16) :> obj
                "op_Modulus", ((%) : int32 -> int32 -> int32) :> obj
                "op_Modulus", ((%) : int64 -> int64 -> int64) :> obj
                "op_Modulus", ((%) : uint8 -> uint8 -> uint8) :> obj
                "op_Modulus", ((%) : uint16 -> uint16 -> uint16) :> obj
                "op_Modulus", ((%) : uint32 -> uint32 -> uint32) :> obj
                "op_Modulus", ((%) : uint64 -> uint64 -> uint64) :> obj
                "op_Modulus", ((%) : float32 -> float32 -> float32) :> obj
                "op_Modulus", ((%) : float -> float -> float) :> obj
                "op_Modulus", ((%) : decimal -> decimal -> decimal) :> obj

                
                "op_BitwiseOr", ((|||) : int8 -> int8 -> int8) :> obj
                "op_BitwiseOr", ((|||) : int16 -> int16 -> int16) :> obj
                "op_BitwiseOr", ((|||) : int32 -> int32 -> int32) :> obj
                "op_BitwiseOr", ((|||) : int64 -> int64 -> int64) :> obj
                "op_BitwiseOr", ((|||) : uint8 -> uint8 -> uint8) :> obj
                "op_BitwiseOr", ((|||) : uint16 -> uint16 -> uint16) :> obj
                "op_BitwiseOr", ((|||) : uint32 -> uint32 -> uint32) :> obj
                "op_BitwiseOr", ((|||) : uint64 -> uint64 -> uint64) :> obj

                
                "op_BitwiseAnd", ((&&&) : int8 -> int8 -> int8) :> obj
                "op_BitwiseAnd", ((&&&) : int16 -> int16 -> int16) :> obj
                "op_BitwiseAnd", ((&&&) : int32 -> int32 -> int32) :> obj
                "op_BitwiseAnd", ((&&&) : int64 -> int64 -> int64) :> obj
                "op_BitwiseAnd", ((&&&) : uint8 -> uint8 -> uint8) :> obj
                "op_BitwiseAnd", ((&&&) : uint16 -> uint16 -> uint16) :> obj
                "op_BitwiseAnd", ((&&&) : uint32 -> uint32 -> uint32) :> obj
                "op_BitwiseAnd", ((&&&) : uint64 -> uint64 -> uint64) :> obj

                
                "op_ExclusiveOr", ((^^^) : int8 -> int8 -> int8) :> obj
                "op_ExclusiveOr", ((^^^) : int16 -> int16 -> int16) :> obj
                "op_ExclusiveOr", ((^^^) : int32 -> int32 -> int32) :> obj
                "op_ExclusiveOr", ((^^^) : int64 -> int64 -> int64) :> obj
                "op_ExclusiveOr", ((^^^) : uint8 -> uint8 -> uint8) :> obj
                "op_ExclusiveOr", ((^^^) : uint16 -> uint16 -> uint16) :> obj
                "op_ExclusiveOr", ((^^^) : uint32 -> uint32 -> uint32) :> obj
                "op_ExclusiveOr", ((^^^) : uint64 -> uint64 -> uint64) :> obj

                
                "op_LessThan", ((<) : int8 -> int8 -> bool) :> obj
                "op_LessThan", ((<) : int16 -> int16 -> bool) :> obj
                "op_LessThan", ((<) : int32 -> int32 -> bool) :> obj
                "op_LessThan", ((<) : int64 -> int64 -> bool) :> obj
                "op_LessThan", ((<) : uint8 -> uint8 -> bool) :> obj
                "op_LessThan", ((<) : uint16 -> uint16 -> bool) :> obj
                "op_LessThan", ((<) : uint32 -> uint32 -> bool) :> obj
                "op_LessThan", ((<) : uint64 -> uint64 -> bool) :> obj
                "op_LessThan", ((<) : float32 -> float32 -> bool) :> obj
                "op_LessThan", ((<) : float -> float -> bool) :> obj
                "op_LessThan", ((<) : decimal -> decimal -> bool) :> obj

                
                "op_LessThanOrEqual", ((<=) : int8 -> int8 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : int16 -> int16 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : int32 -> int32 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : int64 -> int64 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : uint8 -> uint8 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : uint16 -> uint16 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : uint32 -> uint32 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : uint64 -> uint64 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : float32 -> float32 -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : float -> float -> bool) :> obj
                "op_LessThanOrEqual", ((<=) : decimal -> decimal -> bool) :> obj

                
                "op_GreaterThan", ((>) : int8 -> int8 -> bool) :> obj
                "op_GreaterThan", ((>) : int16 -> int16 -> bool) :> obj
                "op_GreaterThan", ((>) : int32 -> int32 -> bool) :> obj
                "op_GreaterThan", ((>) : int64 -> int64 -> bool) :> obj
                "op_GreaterThan", ((>) : uint8 -> uint8 -> bool) :> obj
                "op_GreaterThan", ((>) : uint16 -> uint16 -> bool) :> obj
                "op_GreaterThan", ((>) : uint32 -> uint32 -> bool) :> obj
                "op_GreaterThan", ((>) : uint64 -> uint64 -> bool) :> obj
                "op_GreaterThan", ((>) : float32 -> float32 -> bool) :> obj
                "op_GreaterThan", ((>) : float -> float -> bool) :> obj
                "op_GreaterThan", ((>) : decimal -> decimal -> bool) :> obj

                
                "op_GreaterThanOrEqual", ((>=) : int8 -> int8 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : int16 -> int16 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : int32 -> int32 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : int64 -> int64 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : uint8 -> uint8 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : uint16 -> uint16 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : uint32 -> uint32 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : uint64 -> uint64 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : float32 -> float32 -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : float -> float -> bool) :> obj
                "op_GreaterThanOrEqual", ((>=) : decimal -> decimal -> bool) :> obj

                
                "op_Equality", ((=) : int8 -> int8 -> bool) :> obj
                "op_Equality", ((=) : int16 -> int16 -> bool) :> obj
                "op_Equality", ((=) : int32 -> int32 -> bool) :> obj
                "op_Equality", ((=) : int64 -> int64 -> bool) :> obj
                "op_Equality", ((=) : uint8 -> uint8 -> bool) :> obj
                "op_Equality", ((=) : uint16 -> uint16 -> bool) :> obj
                "op_Equality", ((=) : uint32 -> uint32 -> bool) :> obj
                "op_Equality", ((=) : uint64 -> uint64 -> bool) :> obj
                "op_Equality", ((=) : float32 -> float32 -> bool) :> obj
                "op_Equality", ((=) : float -> float -> bool) :> obj
                "op_Equality", ((=) : decimal -> decimal -> bool) :> obj

                
                "op_Inequality", ((<>) : int8 -> int8 -> bool) :> obj
                "op_Inequality", ((<>) : int16 -> int16 -> bool) :> obj
                "op_Inequality", ((<>) : int32 -> int32 -> bool) :> obj
                "op_Inequality", ((<>) : int64 -> int64 -> bool) :> obj
                "op_Inequality", ((<>) : uint8 -> uint8 -> bool) :> obj
                "op_Inequality", ((<>) : uint16 -> uint16 -> bool) :> obj
                "op_Inequality", ((<>) : uint32 -> uint32 -> bool) :> obj
                "op_Inequality", ((<>) : uint64 -> uint64 -> bool) :> obj
                "op_Inequality", ((<>) : float32 -> float32 -> bool) :> obj
                "op_Inequality", ((<>) : float -> float -> bool) :> obj
                "op_Inequality", ((<>) : decimal -> decimal -> bool) :> obj


            ]

//        let rec (|EqualityCondition|_|) (e : Expr) =
//            match e with
//                | Call(None, Method("op_Equality", _), ([Var v; Value(c, _)] | [Value(c, _); Var v])) ->
//                    Some (Map.ofList [v, c])
//
//                | AndAlso(EqualityCondition l, EqualityCondition r) ->
//                    Some (Map.union l r)
//
//                | _ ->
//                    None
//
//        let rec (|InequalityCondition|_|) (e : Expr) =
//            match e with
//                | Call(None, Method("op_Inequality", _), ([Var v; Value(c, _)] | [Value(c, _); Var v])) ->
//                    Some (Map.ofList [v, c])
//
//                | OrElse(InequalityCondition l, InequalityCondition r) ->
//                    Some (Map.union l r)
//
//                | _ ->
//                    None

        let rec evaluateConstantsS (e : Expr) =
            state {
                match e with
                    | WriteOutputs values ->
                        let! values = 
                            values |> Map.mapS (fun _ (i, v) -> 
                                state {
                                    let! v = 
                                        match v with
                                            | Coerce(v, t) -> evaluateConstantsS v |> State.map (fun v -> Expr.Coerce(v, t))
                                            | v -> evaluateConstantsS v

                                    let! i = i |> Option.mapS evaluateConstantsS
                                    return i, v
                                }
                                        
                            )
                        return Expr.WriteOutputs(values)



                    | AddressOf e ->
                        let! e = evaluateConstantsS e
                        return Expr.AddressOf(e)

                    | AddressSet(v, e) ->
                        let! v = evaluateConstantsS v
                        let! e = evaluateConstantsS e
                        return Expr.AddressSet(v, e)

                    | Application(lambda, arg) ->
                        let! lambda = evaluateConstantsS lambda
                        let! arg = evaluateConstantsS arg
                        match lambda, arg with
                            | Value(lambda, lt), Value(arg, at) ->
                                let mi = lt.GetMethod("Invoke", [|at|])
                                return Expr.Value(mi.Invoke(lambda, [|arg|]), e.Type)
                            | _ ->
                                return Expr.Application(lambda, arg)

                    | ForInteger(v, first, step, last, body) ->
                        let! first = evaluateConstantsS first
                        let! step = evaluateConstantsS step
                        let! last = evaluateConstantsS last
                        let! body = evaluateConstantsS body

                        match first, step, last with
                            | Int32 f, Int32 s, Int32 l ->
                                if s > 0 && l < f then return Expr.Unit
                                elif s < 0 && l > f then return Expr.Unit
                                else return Expr.ForInteger(v, first, step, last, body)
                            | _ ->

                                match body with
                                    | Value _ -> return Expr.Unit
                                    | _ -> return Expr.ForInteger(v, first, step, last, body)
                    
                    | ForEach(v, s, b) ->
                        let! s = evaluateConstantsS s
                        let! b = evaluateConstantsS b
                        match b with
                            | Unit -> return Expr.Unit
                            | _ -> return Expr.ForEach(v, s, b)

                    | CallFunction(utility, args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        let! s = State.get
                        let utility =
                            utility |> UtilityFunction.map (fun e ->
                                let mutable innerState = { State.empty with isGlobalSideEffect = s.isGlobalSideEffect }
                                evaluateConstantsS(e).Run(&innerState)
                            )

                        return Expr.CallFunction(utility, args)

       
                    | Call(None, mi, args) ->
                        let! needed = State.needsCall mi
                        let! args = args |> List.mapS evaluateConstantsS
                        
                        if needed then
                            return Expr.Call(mi, args)
                        else
                            match args with
                                | AllConstant values ->
                                    let key = mi.Name, args |> List.map (fun v -> v.Type)
                                    match operators.TryGetValue key with
                                        | (true, f) -> 
                                            return Expr.Value(f values, e.Type)
                                        | _ -> 
                                            return
                                                try Expr.Value(mi.Invoke(null, values |> List.toArray), e.Type)
                                                with e -> 
                                                    Log.warn "could not evaluate %A: %A" mi e
                                                    Expr.Call(mi, args)
                                | _ ->
                                    return Expr.Call(mi, args) 

                    | Call(Some t, mi, args) ->
                        let! needed = State.needsCall mi
                        let! t = evaluateConstantsS t
                        let! args = args |> List.mapS evaluateConstantsS
                        
                        if needed then
                            return Expr.Call(t, mi, args)
                        else
                            match t, args with
                                | Value(tv,_), AllConstant values -> 
                                    try
                                        return Expr.Value(mi.Invoke(tv, List.toArray values), e.Type)
                                    with _ ->
                                        return Expr.Call(t, mi, args)
                                | _ ->
                                    return Expr.Call(t, mi, args)

                    | Coerce(e, t) ->
                        let! e = evaluateConstantsS e
                        match e with
                            | Value(e,_) -> return Expr.Value(e, t)
                            | _ -> return Expr.Coerce(e, t)

                    | DefaultValue t ->
                        if t.IsValueType then
                            return Expr.Value(Activator.CreateInstance t, t)
                        else
                            return Expr.Value(null, t)

                    | FieldGet(None, f) ->
                        let! n = State.needsField f
                        if n then return e
                        else return Expr.Value(f.GetValue null, e.Type)

                    | FieldGet(Some t, f) ->
                        let! t = evaluateConstantsS t
                        let! n = State.needsField f
                        match t with
                            | Value(t,_) when not n && not (isNull t) ->  return Expr.Value(f.GetValue t, e.Type)
                            | _ -> return Expr.FieldGet(t, f)

                    | FieldSet(None, f, value) ->
                        let! value = evaluateConstantsS value
                        return Expr.FieldSet(f, value)

                    | FieldSet(Some t, f, value) ->
                        let! t = evaluateConstantsS t
                        let! value = evaluateConstantsS value
                        return Expr.FieldSet(t, f, value)

                    | IfThenElse(cond, i, e) ->
                        let! cond = evaluateConstantsS cond
                        match cond with
                            | Bool true -> 
                                return! evaluateConstantsS i

                            | Bool false -> 
                                return! evaluateConstantsS e

                            | _ ->
                                let! i = evaluateConstantsS i
                                let! e = evaluateConstantsS e
                                return Expr.IfThenElse(cond, i, e)

                    | Lambda(v, b) ->
                        let! b = evaluateConstantsS b
                        return Expr.Lambda(v, b)

                    | Let(v, e, b) ->
                        let! e = evaluateConstantsS e
                        match e with
                            | Value(value,_) when not v.IsMutable ->
                                do! State.setVar v value
                                return! evaluateConstantsS b
                            | _ ->
                                let! b = evaluateConstantsS b
                                return Expr.Let(v, e, b)

                    | NewArray(t, args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        match args with
                            | AllConstant args ->
                                let value = 
                                    let arr = Array.CreateInstance(t, List.length args)
                                    let mutable i = 0
                                    for a in args do
                                        arr.SetValue(a, i)
                                        i <- i + 1
                                    arr

                                return Expr.Value(value, e.Type)
                            | _ ->
                                return Expr.NewArray(t, args)

                    | NewDelegate(t, vars, body) ->
                        let! body = evaluateConstantsS body
                        return Expr.NewDelegate(t, vars, body)

                    | NewObject(ctor, args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        match args with
                            | AllConstant args ->
                                return Expr.Value(ctor.Invoke(List.toArray args), e.Type)
                            | _ ->
                                return Expr.NewObject(ctor, args)
                        
                    | NewRecord(t, args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        match args with
                            | AllConstant args ->
                                return Expr.Value(FSharpValue.MakeRecord(t, List.toArray args, true), e.Type)
                            | _ ->
                                return Expr.NewRecord(t, args)

                    | NewTuple(args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        match args with
                            | AllConstant args ->
                                return Expr.Value(FSharpValue.MakeTuple(List.toArray args, e.Type), e.Type)
                            | _ ->
                                return Expr.NewTuple(args)
                        
                    | NewUnionCase(ci, args) ->
                        let! args = args |> List.mapS evaluateConstantsS
                        match args with
                            | AllConstant args ->
                                return Expr.Value(FSharpValue.MakeUnion(ci, List.toArray args, true), e.Type)
                            | _ ->
                                return Expr.NewUnionCase(ci, args)
                        
                    | PropertyGet(None, pi, indices) ->
                        let! indices = indices |> List.mapS evaluateConstantsS
                        let! n = State.needsProperty pi
                        match indices with
                            | AllConstant indices when not n ->
                                return Expr.Value(pi.GetValue(null, List.toArray indices), e.Type)
                            | _ ->
                                return Expr.PropertyGet(pi, indices)

                    | PropertyGet(Some t, pi, indices) ->
                        let! t = evaluateConstantsS t
                        let! indices = indices |> List.mapS evaluateConstantsS
                        let! n = State.needsProperty pi
                        match t, indices with
                            | Value(t,_), AllConstant indices when not n && not (isNull t)->
                                return Expr.Value(pi.GetValue(t, List.toArray indices), e.Type)
                            | _ ->
                                return Expr.PropertyGet(t, pi, indices)

                    | PropertySet(None, pi, indices, value) ->
                        let! indices = indices |> List.mapS evaluateConstantsS
                        let! value = evaluateConstantsS value
                        return Expr.PropertySet(pi, value, indices)

                    | PropertySet(Some t, pi, indices, value) ->
                        let! t = evaluateConstantsS t
                        let! indices = indices |> List.mapS evaluateConstantsS
                        let! value = evaluateConstantsS value
                        return Expr.PropertySet(t, pi, value, indices)
                        
                    | QuoteRaw e ->
                        let! e = evaluateConstantsS e
                        return Expr.QuoteRaw e
                        
                    | QuoteTyped e ->
                        let! e = evaluateConstantsS e
                        return Expr.QuoteTyped e

                    | Sequential(l, r) ->
                        let! l = evaluateConstantsS l
                        let! r = evaluateConstantsS r
                        match l with
                            | Unit -> return r
                            | _ -> return Expr.Sequential(l, r)

                    | TryFinally(t,f) ->
                        let! t = evaluateConstantsS t
                        let! f = evaluateConstantsS f
                        return Expr.TryFinally(t, f)

                    | TryWith(a, b, c, d, e) ->
                        let! a = evaluateConstantsS a
                        let! c = evaluateConstantsS c
                        let! e = evaluateConstantsS e
                        return Expr.TryWith(a, b, c, d, e)

                    | TupleGet(t, i) ->
                        let! t = evaluateConstantsS t
                        match t with
                            | Value(t,_) -> return Expr.Value(FSharpValue.GetTupleField(t, i), e.Type)
                            | _ -> return Expr.TupleGet(t, i)

                    | TypeTest(v, t) ->
                        let! v = evaluateConstantsS v
                        match v with
                            | Value(v,_) ->
                                return Expr.Value(t.IsAssignableFrom(v.GetType()))
                            | _ ->
                                return Expr.TypeTest(v, t)

                    | UnionCaseTest(e, ci) ->
                        let! e = evaluateConstantsS e
                        // TODO: could be avoided
                        return Expr.UnionCaseTest(e, ci)

                    | Value _ ->
                        return e

                    | VarSet (v, e) ->
                        let! e = evaluateConstantsS e
                        return Expr.VarSet (v, e)

                    | Var v ->
                        let! value = State.getVar v
                        match value with
                            | Some v -> return Expr.Value(v, e.Type)
                            | _ -> return e

                    | WhileLoop(guard, body) ->
                        let! guard = evaluateConstantsS guard
                        let! body = evaluateConstantsS body
                        match guard, body with
                            | Bool false, _ -> return Expr.Unit
                            | _, Unit -> return Expr.Unit
                            | g, b -> return Expr.WhileLoop(g, b)

                    | _ ->
                        return failwithf "[FShade] unexpected expression %A" e
            }

        let evaluateConstants (e : Expr) =
            let run = evaluateConstantsS e
            let mutable state = State.empty
            run.Run(&state)

        let evaluateConstants' (isSideEffect : MethodInfo -> bool) (e : Expr) =
            let run = evaluateConstantsS e

            let isSideEffect (m : MemberInfo) =
                match m with    
                    | :? MethodInfo as mi -> isSideEffect mi
                    | _ -> false

            let mutable state = { State.empty with isGlobalSideEffect = isSideEffect }
            run.Run(&state)

        let evaluateConstants'' (isSideEffect : MemberInfo -> bool) (e : Expr) =
            let run = evaluateConstantsS e
            let mutable state = { State.empty with isGlobalSideEffect = isSideEffect }
            run.Run(&state)
       
    module InputLifting =
        type State =
            {
                usedInputs : Map<string, ParameterDescription>
            }
                 
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module State =
            let empty = 
                { 
                    usedInputs = Map.empty
                }

        
        let rec liftInputsS (e : Expr) : State<State, Expr> =
            state {
                match e with

                    | ReadInput(ParameterKind.Input, name, idx) ->
                        do! State.modify (fun s ->
                            if Map.containsKey name s.usedInputs then
                                s
                            else
                                let typ =
                                    match idx with
                                        | Some _ -> e.Type.MakeArrayType()
                                        | _ -> e.Type
                                { s with usedInputs = Map.add name (ParameterDescription.ofType typ) s.usedInputs }
                        )
                        return e

                    | ReadInput(kind, name, idx) ->
                        match idx with
                            | Some idx ->
                                let! idx = liftInputsS idx
                                return Expr.ReadInput(kind, e.Type, name, idx)
                            | None ->
                                return Expr.ReadInput(kind, e.Type, name)
                                
                    | WriteOutputs outputs -> 
                        let! outputs = 
                            outputs |> Map.mapS (fun name (idx, value) ->
                                state {
                                    let! idx = Option.mapS liftInputsS idx
                                    let! value = liftInputsS value
                                    return (idx, value)
                                }
                            )
                        return Expr.WriteOutputs outputs

                    | AddressOf e ->
                        let! e = liftInputsS e
                        return Expr.AddressOf e

                    | AddressSet(v, e) ->
                        let! v = liftInputsS v
                        let! e = liftInputsS e
                        return Expr.AddressSet(v,e)

                    | Application(lambda, arg) ->
                        let! lambda = liftInputsS lambda
                        let! arg = liftInputsS arg
                        return Expr.Application(lambda, arg)

                    | ForInteger(v, first, step, last, body) ->
                        let! first = liftInputsS first
                        let! step = liftInputsS step
                        let! last = liftInputsS last
                        let! body = liftInputsS body
                        return Expr.ForInteger(v, first, step, last, body)

                    | ForEach(v, s, b) ->
                        let! s = liftInputsS s
                        let! b = liftInputsS b
                        return Expr.ForEach(v, s, b)

                    | CallFunction(utility, args) ->
                        
                        let mutable usedInputs = Map.empty
                        let newBody = liftFunctionInputsS(utility.functionBody).Run(&usedInputs)

                        let (vars, values) = usedInputs |> Map.toList |> List.map snd |> List.unzip

                        let utility =
                            { utility with
                                functionArguments = utility.functionArguments @ vars
                                functionBody = newBody
                                functionId = Expr.ComputeHash newBody
                                functionTag = null
                            }


                        let! values = values |> List.mapS liftInputsS
                        return Expr.CallFunction(utility, args @ values)
                        
       
                    | Call(None, mi, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.Call(mi, args)

                    | Call(Some t, mi, args) ->
                        let! t = liftInputsS t
                        let! args = args |> List.mapS liftInputsS
                        return Expr.Call(t, mi, args)
                        
                    | Coerce(e, t) ->
                        let! e = liftInputsS e
                        return Expr.Coerce(e, t)

                    | DefaultValue t ->
                        return e

                    | FieldGet(None, f) ->
                        return e

                    | FieldGet(Some t, f) ->
                        let! t = liftInputsS t
                        return Expr.FieldGet(t, f)

                    | FieldSet(None, f, value) ->
                        let! value = liftInputsS value
                        return Expr.FieldSet(f, value)

                    | FieldSet(Some t, f, value) ->
                        let! t = liftInputsS t
                        let! value = liftInputsS value
                        return Expr.FieldSet(t, f, value)

                    | IfThenElse(cond, i, e) ->
                        let! cond = liftInputsS cond
                        let! i = liftInputsS i
                        let! e = liftInputsS e
                        return Expr.IfThenElse(cond, i, e)

                    | Lambda(v,b) ->
                        let! b = liftInputsS b
                        return Expr.Lambda(v,b)

                    | Let(v, e, b) ->
                        let! e = liftInputsS e
                        let! b = liftInputsS b
                        return Expr.Let(v,e,b)

                    | NewArray(t, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.NewArray(t, args)

                    | NewDelegate(t, vars, body) ->
                        let! body = liftInputsS body
                        return Expr.NewDelegate(t, vars, body)

                    | NewObject(ctor, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.NewObject(ctor, args)

                    | NewRecord(t, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.NewRecord(t, args)

                    | NewUnionCase(ci, args) ->
                        let! args = args |> List.mapS liftInputsS
                        return Expr.NewUnionCase(ci, args)

                    | PropertyGet(None, pi, indices) ->
                        let! indices = indices |> List.mapS liftInputsS
                        return Expr.PropertyGet(pi, indices)

                    | PropertyGet(Some t, pi, indices) ->
                        let! t = liftInputsS t
                        let! indices = indices |> List.mapS liftInputsS
                        return Expr.PropertyGet(t, pi, indices)
                        
                    | PropertySet(None, pi, indices, value) ->
                        let! value = liftInputsS value
                        let! indices = indices |> List.mapS liftInputsS
                        return Expr.PropertySet(pi, value, indices)

                    | PropertySet(Some t, pi, indices, value) ->
                        let! t = liftInputsS t
                        let! value = liftInputsS value
                        let! indices = indices |> List.mapS liftInputsS
                        return Expr.PropertySet(t, pi, value, indices)
                        
                    | QuoteRaw e ->
                        let! e = liftInputsS e
                        return Expr.QuoteRaw e

                    | QuoteTyped e ->
                        let! e = liftInputsS e
                        return Expr.QuoteTyped e

                    | Sequential(l,r) ->
                        let! l = liftInputsS l
                        let! r = liftInputsS r
                        return Expr.Sequential(l,r)

                    | TryFinally(t, f) ->
                        let! t = liftInputsS t
                        let! f = liftInputsS f
                        return Expr.TryFinally(t, f)

                    | TryWith(a, b, c, d, e) ->
                        let! a = liftInputsS a
                        let! c = liftInputsS c
                        let! e = liftInputsS e
                        return Expr.TryWith(a, b, c, d, e)

                    | TupleGet(t, i) ->
                        let! t = liftInputsS t
                        return Expr.TupleGet(t, i)

                    | TypeTest(t, i) ->
                        let! t = liftInputsS t
                        return Expr.TypeTest(t,i)

                    | UnionCaseTest(e, ci) ->
                        let! e = liftInputsS e
                        return Expr.UnionCaseTest(e, ci)

                    | Value _ ->
                        return e

                    | VarSet(v, e) ->
                        let! e = liftInputsS e
                        return Expr.VarSet(v,e)

                    | Var v ->
                        return e

                    | WhileLoop(guard, body) ->
                        let! guard = liftInputsS guard
                        let! body = liftInputsS body
                        return Expr.WhileLoop(guard, body)


                    | _ ->
                        return failwithf "[FShade] unexpected expression %A" e
            }

        and liftFunctionInputsS (e : Expr) : State<Map<_,_>, Expr> =
            state {
                match e with
                    | ReadInput(ParameterKind.Input, name, idx) ->
                        let! (s : Map<string, Var * Expr>) = State.get

                        let idxHash = idx |> Option.map Expr.ComputeHash |> Option.defaultValue ""
                        let key = name + idxHash

                        match Map.tryFind key s with
                            | Some (v,_) -> 
                                return Expr.Var v
                            | None ->
                                let v = Var(name, e.Type)
                                do! State.put (Map.add key (v,e) s)
                                return Expr.Var v

                    | ReadInput(kind, name, idx) ->
                        match idx with
                            | Some idx ->
                                let! idx = liftFunctionInputsS idx
                                return Expr.ReadInput(kind, e.Type, name, idx)
                            | None ->
                                return Expr.ReadInput(kind, e.Type, name)

                    | CallFunction(utility, args) ->
                        let mutable usedInputs = Map.empty
                        let newBody = liftFunctionInputsS(utility.functionBody).Run(&usedInputs)

                        let (vars, values) = usedInputs |> Map.toList |> List.map snd |> List.unzip

                        let utility =
                            { utility with
                                functionArguments = utility.functionArguments @ vars
                                functionBody = newBody
                                functionId = Expr.ComputeHash newBody
                                functionTag = null
                            }


                        let! values = values |> List.mapS liftFunctionInputsS
                        return Expr.CallFunction(utility, args @ values)

                        
                    | ShapeVar v -> 
                        return Expr.Var v

                    | ShapeLambda(v, b) ->
                        let! b = liftFunctionInputsS b
                        return Expr.Lambda(v, b)

                    | ShapeCombination(o, args) -> 
                        let! args = args |> List.mapS liftFunctionInputsS
                        return RebuildShapeCombination(o, args)
            }

        let liftInputs (e : Expr) =
            let run = liftInputsS(e)
            let mutable state = State.empty
            let res = run.Run(&state)
            res

    module Inlining =
        type State =
            {
                variables   : bool
                trivial     : bool
                inputs      : bool
                functions   : UtilityFunction -> bool
            }

                 
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module State =
            let empty = 
                { 
                    variables = true
                    trivial = true
                    inputs = true
                    functions = fun _ -> false
                }

            let inlineFunction (f : UtilityFunction) =
                State.get |> State.map (fun s -> s.functions f)

            let variables = State.get |> State.map (fun s -> s.variables)
            let trivial = State.get |> State.map (fun s -> s.trivial)
            let inputs = State.get |> State.map (fun s -> s.inputs)

            let rec (|TrivialOrInput|_|) (e : Expr) =
                match e with
                    | Var _ 
                    | Value _
                    | FieldGet(None, _)
                    | PropertyGet(None, _, [])
                    | TupleGet(Trivial, _)
                    | PropertyGet(Some TrivialOrInput, (FSharpTypeProperty | ArrayLengthProperty), [])
                    | FieldGet(Some TrivialOrInput, _) 
                    | ReadInput(_, _, None) 
                    | ReadInput(_, _, Some TrivialOrInput) -> 
                        Some()
                    | _ ->
                        None

            let rec (|OnlyVar|_|) (e : Expr) =
                match e with
                    | Var _ 
                    | Value _ ->
                        Some ()
                    | _ -> 
                        None
                        
            let rec (|OnlyInputs|_|) (e : Expr) =
                match e with
                    | ReadInput _ ->
                        Some ()
                    | _ -> 
                        None
                        
            let rec (|InputOrVar|_|) (e : Expr) =
                match e with
                    | ReadInput _ 
                    | Var _ ->
                        Some ()
                    | _ -> 
                        None
            let letValuePattern =
                State.get |> State.map (fun s ->
                    match s.variables, s.trivial, s.inputs with
                        | (false | true), true, true ->
                            (|TrivialOrInput|_|)

                        | (false | true), true, false ->
                            (|Trivial|_|)

                        | true, false, false ->
                            (|OnlyVar|_|)

                        | false, false, true ->
                            (|OnlyInputs|_|)

                        | true, false, true ->
                            (|InputOrVar|_|)

                        | false, false, false ->
                            fun _ -> None
                )


        let rec inlineS (e : Expr) =
            state {
                match e with
                    | WriteOutputs map ->
                        let! map = map |> Map.mapS (fun name (idx, value) ->
                            state {
                                let! idx = idx |> Option.mapS inlineS
                                let! value = inlineS value
                                return (idx, value)
                            }
                        )
                        return Expr.WriteOutputs map

                    | ReadInput(kind, name, idx) ->
                        match idx with
                            | Some idx ->
                                let! idx = inlineS idx
                                return Expr.ReadInput(kind, e.Type, name, idx)
                            | None ->
                                return e

                    | AddressOf e ->
                        let! e = inlineS e
                        return Expr.AddressOf e

                    | AddressSet(v,e) ->
                        let! v = inlineS v
                        let! e = inlineS e
                        return Expr.AddressSet(v,e)

                    | Application(lambda, arg) ->
                        let! lambda = inlineS lambda
                        let! arg = inlineS arg
                        return Expr.Application(lambda, arg)
                    
                    | ForInteger(v, first, step, last, body) ->
                        let! first = inlineS first
                        let! step = inlineS step
                        let! last = inlineS last
                        let! body = inlineS body
                        return Expr.ForInteger(v, first, step, last, body)

                    | ForEach(v, s, b) ->
                        let! s = inlineS s
                        let! b = inlineS b
                        return Expr.ForEach(v, s, b)

                    | CallFunction(utility, args) ->
                        let! inl = State.inlineFunction utility
                        if inl || utility.functionIsInline then
                            let code = UtilityFunction.inlineCode utility args
                            return! inlineS code
                        else
                            let! args = args |> List.mapS inlineS
                            let! s = State.get
                            let utility =
                                utility |> UtilityFunction.map (fun b ->
                                    let mutable innerState = s
                                    inlineS(b).Run(&innerState)
                                )
                            let utility = { utility with functionTag = null }
                            return Expr.CallFunction(utility, args)

                    | Call(None, mi, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.Call(mi, args)

                    | Call(Some t, mi, args) ->
                        let! t = inlineS t
                        let! args = args |> List.mapS inlineS
                        return Expr.Call(t, mi, args)

                    | Coerce(e, t) ->
                        let! e = inlineS e
                        return Expr.Coerce(e,t)

                    | DefaultValue _ -> 
                        return e

                    | FieldGet(None, f) ->
                        return e

                    | FieldGet(Some t, f) ->
                        let! t = inlineS t
                        return Expr.FieldGet(t, f)

                    | FieldSet(None, f, value) ->
                        let! value = inlineS value
                        return Expr.FieldSet(f, value)

                    | FieldSet(Some t, f, value) ->
                        let! t = inlineS t
                        let! value = inlineS value
                        return Expr.FieldSet(t, f, value)

                    | IfThenElse(cond, i, e) ->
                        let! cond = inlineS cond
                        let! i = inlineS i
                        let! e = inlineS e
                        return Expr.IfThenElse(cond, i, e)

                    | Lambda(v, b) ->
                        let! b = inlineS b
                        return Expr.Lambda(v, b)

                    | Let(v, e, b) when v.IsMutable ->
                        let! e = inlineS e
                        let! b = inlineS b
                        return Expr.Let(v, e, b)

                    | Let(v, e, b) ->
                        let! pattern = State.letValuePattern
                        let! e = inlineS e

                        match pattern e with
                            | Some () ->
                                let b = b.Substitute(fun vi -> if vi = v then Some e else None)
                                return! inlineS b
                            | None ->
                                let! b = inlineS b
                                return Expr.Let(v, e, b)

                    | NewArray(t, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.NewArray(t, args)
                        
                    | NewDelegate(t, vars, body) ->
                        let! body = inlineS body
                        return Expr.NewDelegate(t, vars, body)

                    | NewObject(ctor, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.NewObject(ctor, args)

                    | NewRecord(t, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.NewRecord(t, args)
                        
                    | NewTuple(args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.NewTuple args
                        
                    | NewUnionCase(ci, args) ->
                        let! args = args |> List.mapS inlineS
                        return Expr.NewUnionCase(ci, args)

                    | PropertyGet(None, pi, indices) ->
                        let! indices = indices |> List.mapS inlineS
                        return Expr.PropertyGet(pi, indices)

                    | PropertyGet(Some t, pi, indices) ->
                        let! t = inlineS t
                        let! indices = indices |> List.mapS inlineS
                        return Expr.PropertyGet(t, pi, indices)
                        
                    | PropertySet(None, pi, indices, value) ->
                        let! value = inlineS value
                        let! indices = indices |> List.mapS inlineS
                        return Expr.PropertySet(pi, value, indices)
                        
                    | PropertySet(Some t, pi, indices, value) ->
                        let! t = inlineS t
                        let! value = inlineS value
                        let! indices = indices |> List.mapS inlineS
                        return Expr.PropertySet(t, pi, value, indices)

                    | QuoteRaw e ->
                        let! e = inlineS e
                        return Expr.QuoteRaw e
                        
                    | QuoteTyped e ->
                        let! e = inlineS e
                        return Expr.QuoteTyped e

                    | Sequential(l, r) ->
                        let! l = inlineS l
                        let! r = inlineS r
                        return Expr.Sequential(l, r)

                    | TryFinally(t,f) ->
                        let! t = inlineS t
                        let! f = inlineS f
                        return Expr.TryFinally(t, f)

                    | TryWith(a, b, c, d, e) ->
                        let! a = inlineS a
                        let! c = inlineS c
                        let! e = inlineS e
                        return Expr.TryWith(a, b, c, d, e)

                    | TupleGet(t, i) ->
                        let! t = inlineS t
                        return Expr.TupleGet(t, i)

                    | TypeTest(v, t) ->
                        let! v = inlineS v
                        return Expr.TypeTest(v, t)

                    | UnionCaseTest(e, ci) ->
                        let! e = inlineS e
                        return Expr.UnionCaseTest(e, ci)
                        
                    | Value _ ->
                        return e
                        
                    | VarSet (v, e) ->
                        let! e = inlineS e
                        return Expr.VarSet (v, e)

                    | Var v ->
                        return e

                    | WhileLoop(guard, body) ->
                        let! guard = inlineS guard
                        let! body = inlineS body
                        return Expr.WhileLoop(guard, body)

                    | _ -> 
                        return failwithf "[FShade] unexpected expression %A" e
            }

        let inlining (e : Expr) =
            let run = inlineS(e)
            let mutable state = State.empty
            run.Run(&state)


        let inlining' (functions : UtilityFunction -> bool) (e : Expr) =
            let run = inlineS(e)
            let mutable state = { State.empty with functions = functions }
            run.Run(&state)

    module DeExpr =

        let rec extractStatements (e : Expr) : Option<Expr -> Expr> * Expr =
            match e with
                | Let(v,e,b) ->
                    let rest, b = extractStatements b
                    match rest with
                        | Some rest -> 
                            Some (fun b -> Expr.Let(v,e,rest b)), b
                        | None ->
                            Some (fun b -> Expr.Let(v,e,b)), b

                | Sequential(l,r) ->
                    let rb, r = extractStatements r
                    match rb with
                        | Some rb ->
                            Some (fun f -> Expr.Seq [l; rb f]), r
                        | None ->
                            Some (fun f -> Expr.Seq [l; f]), r
                            
                | AddressSet _ 
                | FieldSet _ 
                | PropertySet _
                | ForInteger _ 
                | ForEach _ 
                | VarSet _ 
                | WhileLoop _ ->
                    Some (fun f -> Expr.Seq [e; f]), Expr.Unit

                | AddressOf e ->
                    match extractStatements e with
                        | Some rb, e -> Some rb, Expr.AddressOf e
                        | None, e -> None, Expr.AddressOf e
                        
                | Application(l,a) ->
                    match extractList [l;a] with
                        | Some rb, [l;a] -> Some rb, Expr.Application(l, a)
                        | None, [l;a] -> None, Expr.Application(l, a)
                        | _ -> failwith "impossible"

                | CallFunction(f, args) ->
                    let f = f |> UtilityFunction.map deExpr
                    match extractList args with
                        | Some rb, args -> Some rb, Expr.CallFunction(f, args)
                        | None, args -> None, Expr.CallFunction(f, args)

                | Call(None, mi, args) ->
                    match extractList args with
                        | Some rb, args -> Some rb, Expr.Call(mi, args)
                        | None, args -> None, Expr.Call(mi, args)

                | Call(Some t, mi, args) ->
                    match extractList (t :: args) with
                        | Some rb, (t :: args) -> Some rb, Expr.Call(t, mi, args)
                        | None, (t :: args) -> None, Expr.Call(t, mi, args)
                        | _ -> failwith "impossible"
                    
                | Coerce(e, t) ->
                    match extractStatements e with
                        | Some rb, e -> Some rb, Expr.Coerce(e, t)
                        | None, e -> None, Expr.Coerce(e, t)

                | FieldGet(Some t, f) ->
                    match extractStatements t with
                        | Some rb, t -> Some rb, Expr.FieldGet(t, f)
                        | None, t -> None, Expr.FieldGet(t, f)
                        
                | IfThenElse(cond, i, e) ->
                    match extractStatements cond with
                        | Some rb, cond -> Some rb, Expr.IfThenElse(cond, i, e)
                        | None, cond -> None, Expr.IfThenElse(cond, i, e)

                | NewArray(t, args) ->
                    match extractList args with
                        | Some rb, args -> Some rb, Expr.NewArray(t, args) 
                        | None, args -> None, Expr.NewArray(t, args) 

                        
                | NewObject(ctor, args) ->
                    match extractList args with
                        | Some rb, args -> Some rb, Expr.NewObject(ctor, args) 
                        | None, args -> None, Expr.NewObject(ctor, args) 

                | NewRecord(t, args) ->
                    match extractList args with
                        | Some rb, args -> Some rb, Expr.NewRecord(t, args) 
                        | None, args -> None, Expr.NewRecord(t, args) 

                | NewTuple(args) ->
                    match extractList args with
                        | Some rb, args -> Some rb, Expr.NewTuple(args) 
                        | None, args -> None, Expr.NewTuple(args) 

                | NewUnionCase(ci, args) ->
                    match extractList args with
                        | Some rb, args -> Some rb, Expr.NewUnionCase(ci, args) 
                        | None, args -> None, Expr.NewUnionCase(ci, args) 

                | PropertyGet(None, pi, indices) ->
                    match extractList indices with
                        | Some rb, indices -> Some rb, Expr.PropertyGet(pi, indices)
                        | None, indices -> None, Expr.PropertyGet(pi, indices)

                | PropertyGet(Some t, pi, indices) ->
                    match extractList (t :: indices) with
                        | Some rb, t :: indices -> Some rb, Expr.PropertyGet(t, pi, indices)
                        | None, t :: indices -> None, Expr.PropertyGet(t, pi, indices)
                        | _ -> failwith "impossible"

                        
                | TupleGet(t, i) ->
                    match extractStatements t with
                        | Some rb, t -> Some rb, Expr.TupleGet(t, i)
                        | None, t -> None, Expr.TupleGet(t, i)

                | TypeTest(v, t) ->
                    match extractStatements v with
                        | Some rb, v -> Some rb, Expr.TypeTest(v, t)
                        | None, v -> None, Expr.TypeTest(v, t)
                        
                | UnionCaseTest(v, ci) ->
                    match extractStatements v with
                        | Some rb, v -> Some rb, Expr.UnionCaseTest(v, ci)
                        | None, v -> None, Expr.UnionCaseTest(v, ci)

                | e ->
                    None, e
        
        and extractList (l : list<Expr>) =
            match l with
                | [] -> None, []
                | a :: r ->
                    let rebuildA, a = extractStatements a
                    let rebuildRest, r = extractList r
                    match rebuildA, rebuildRest with
                        | Some ra, Some rr ->
                            Some (ra >> rr), a :: r
                        | Some ra, None ->
                            Some ra, a :: r
                        | None, Some rr ->
                            Some rr, a :: r
                        | None, None ->
                            None, a :: r

        and extractMapL (l : list<string * (Option<Expr> * Expr)>) =
            match l with
                | [] -> None, []
                | (name, (ai, av)) :: r ->
                    let rai = ai |> Option.map extractStatements
                    let rav, av = extractStatements av
                    let rr, r = extractMapL r

                    let ra, ai =
                        match rai with
                            | Some(Some rai, ai) -> 
                                match rav with
                                    | Some rav -> Some (rai >> rav), Some ai
                                    | None -> Some rai, Some ai

                            | Some(None, ai) ->
                                rav, Some ai

                            | None -> 
                                rav, None

                    match ra, rr with
                        | Some ra, Some rr ->
                            Some (ra >> rr), (name, (ai, av)) :: r
                        | Some ra, None ->
                            Some ra, (name, (ai, av)) :: r
                        | None, Some rr ->
                            Some rr, (name, (ai, av)) :: r
                        | None, None ->
                            None, (name, (ai, av)) :: r

        and extractMap (l : Map<string, Option<Expr> * Expr>) =
            let rb, e = l |> Map.toList |> extractMapL
            rb, Map.ofList e

        and deExpr (e : Expr) =
            match e with
                | WriteOutputs values ->
                    match extractMap values with
                        | Some rb, values ->
                            Expr.WriteOutputs(values) |> rb |> deExpr
                        | None, values ->
                            Expr.WriteOutputs(values)

                | Let(v, e, b) ->
                    let b = deExpr b
                    match extractStatements e with
                        | Some rb, e ->
                            Expr.Let(v,e,b) |> rb |> deExpr
                        | None, e ->
                            Expr.Let(v,e,b)

                | CallFunction(f, args) ->
                    let f = f |> UtilityFunction.map deExpr
                    match extractList args with  
                        | Some rb, args ->
                            Expr.CallFunction(f, args) |> rb |> deExpr
                        | None, args ->
                            Expr.CallFunction(f, args)

                | Call(None, mi, args) ->
                    match extractList args with  
                        | Some rb, args ->
                            Expr.Call(mi, args) |> rb |> deExpr
                        | None, args ->
                            Expr.Call(mi, args)
                    
                | Call(Some t, mi, args) ->
                    match extractList (t :: args) with
                        | Some rb, t :: args ->
                            Expr.Call(t, mi, args) |> rb |> deExpr
                        | None, t :: args ->
                            Expr.Call(t, mi, args)
                        | _ ->
                            failwith "impossible"

                | AddressOf e ->
                    match extractStatements e with
                        | Some rb, e ->
                            Expr.AddressOf(e) |> rb |> deExpr
                        | None, e ->
                            Expr.AddressOf e

                
                | AddressSet(v, e) ->
                    match extractList [v;e] with
                        | Some rb, [v;e] ->
                            Expr.AddressSet(v, e) |> rb |> deExpr
                        | None, [v;e]->
                            Expr.AddressSet(v, e)
                        | _ ->
                            failwith "impossible"

                | Application(l,a) ->
                    match extractList [l;a] with
                        | Some rb, [l;a] ->
                            Expr.Application(l, a) |> rb |> deExpr
                        | None, [l;a] ->
                            Expr.Application(l, a)
                        | _ ->
                            failwith "impossible"

                | ForInteger(v, first, step, last, body) ->
                    let body = deExpr body
                    match extractList [first; step; last] with
                        | Some rb, [first; step; last] ->
                            Expr.ForInteger(v, first, step, last, body)
                        | None, [first; step; last] ->
                            Expr.ForInteger(v, first, step, last, body)
                        | _ ->
                            failwith "impossible"

                | ForEach(v, s, body) ->
                    let body = deExpr body
                    match extractStatements s with
                        | Some rb, s ->
                            Expr.ForEach(v, s, body) |> rb |> deExpr
                        | None, s ->
                            Expr.ForEach(v, s, body)

                | Coerce(e, t) ->
                    match extractStatements e with
                        | Some rb, e ->
                            Expr.Coerce(e, t) |> rb |> deExpr
                        | None, e ->
                            Expr.Coerce(e,t)

                | DefaultValue t ->
                    e

                | FieldGet(None, f) ->
                    Expr.FieldGet(f)

                | FieldGet(Some t, f) ->
                    match extractStatements t with
                        | Some rb, t -> Expr.FieldGet(t, f) |> rb |> deExpr
                        | None, t -> Expr.FieldGet(t, f)

                | FieldSet(None, f, value) ->
                    match extractStatements value with
                        | Some rb, value -> Expr.FieldSet(f, value) |> rb |> deExpr
                        | None, value -> Expr.FieldSet(f, value)

                | FieldSet(Some t, f, value) ->
                    match extractList [t; value] with
                        | Some rb, [t;value] -> Expr.FieldSet(t, f, value) |> rb |> deExpr
                        | None, [t;value] -> Expr.FieldSet(t, f, value)
                        | _ ->
                            failwith "impossible"

                | IfThenElse(cond, i, e) when e.Type = typeof<unit> ->
                    match extractStatements cond with
                        | Some rb, cond -> Expr.IfThenElse(cond, i, e) |> rb |> deExpr
                        | None, cond -> Expr.IfThenElse(cond, deExpr i, deExpr e)

                | IfThenElse(cond, i, e) ->
                    match extractStatements cond with
                        | Some rb, cond -> Expr.IfThenElse(cond, i, e) |> rb |> deExpr
                        | None, cond -> Expr.IfThenElse(cond, deExpr i, deExpr e)

                | Lambda(v,b) ->
                    Expr.Lambda(v, deExpr b)

                | NewArray(t, args) ->
                    match extractList args with
                        | Some rb, args -> Expr.NewArray(t, args) |> rb |> deExpr
                        | None, args -> Expr.NewArray(t, args)

                | NewDelegate(t, vars, body) ->
                    let body = deExpr body
                    Expr.NewDelegate(t, vars, body)
                    
                | NewObject(ctor, args) ->
                    match extractList args with
                        | Some rb, args -> Expr.NewObject(ctor, args) |> rb |> deExpr
                        | None, args -> Expr.NewObject(ctor, args)

                | NewRecord(t, args) ->
                    match extractList args with
                        | Some rb, args -> Expr.NewRecord(t, args) |> rb |> deExpr
                        | None, args -> Expr.NewRecord(t, args)

                | NewTuple(args) ->
                    match extractList args with
                        | Some rb, args -> Expr.NewTuple(args) |> rb |> deExpr
                        | None, args -> Expr.NewTuple(args)
                        
                | NewUnionCase(ci, args) ->
                    match extractList args with
                        | Some rb, args -> Expr.NewUnionCase(ci, args) |> rb |> deExpr
                        | None, args -> Expr.NewUnionCase(ci, args)
                        
                | PropertyGet(None, pi, indices) ->
                    match extractList indices with
                        | Some rb, indices -> Expr.PropertyGet(pi, indices) |> rb |> deExpr
                        | None, indices -> Expr.PropertyGet(pi, indices)
                        
                | PropertyGet(Some t, pi, indices) ->
                    match extractList (t :: indices) with
                        | Some rb, t :: indices -> Expr.PropertyGet(t, pi, indices) |> rb |> deExpr
                        | None, t :: indices -> Expr.PropertyGet(t, pi, indices)
                        | _ ->
                            failwith "impossible"

                | PropertySet(None, pi, indices, value) ->
                    match extractList (value :: indices) with
                        | Some rb, value :: indices -> Expr.PropertySet(pi, value, indices) |> rb |> deExpr
                        | None, value :: indices -> Expr.PropertySet(pi, value, indices)
                        | _ ->
                            failwith "impossible"
                    
                | PropertySet(Some t, pi, indices, value) ->
                    match extractList (t :: value :: indices) with
                        | Some rb, t :: value :: indices -> Expr.PropertySet(t, pi, value, indices) |> rb |> deExpr
                        | None, t :: value :: indices -> Expr.PropertySet(t, pi, value, indices)
                        | _ ->
                            failwith "impossible"
                        
                | QuoteRaw e -> Expr.QuoteRaw e
                | QuoteTyped e -> Expr.QuoteTyped e

                | Sequential(l,r) ->
                    Expr.Sequential (deExpr l, deExpr r)

                
                | TryFinally(t,f) ->
                    let t = deExpr t
                    let f = deExpr f
                    Expr.TryFinally(t, f)
                    
                | TryWith(a, b, c, d, e) ->
                    let a = deExpr a
                    let c = deExpr c
                    let e = deExpr e
                    Expr.TryWith(a, b, c, d, e)

                    
                | TupleGet(t, i) ->
                    match extractStatements t with
                        | Some rb, t -> Expr.TupleGet(t, i) |> rb |> deExpr
                        | None, t -> Expr.TupleGet(t,i)

                        
                | TypeTest(e, t) ->
                    match extractStatements e with
                        | Some rb, e -> Expr.TypeTest(e, t) |> rb |> deExpr
                        | None, e -> Expr.TypeTest(e, t)

                | UnionCaseTest(e, ci) ->
                    match extractStatements e with
                        | Some rb, e -> Expr.UnionCaseTest(e, ci) |> rb |> deExpr
                        | None, e -> Expr.UnionCaseTest(e, ci)

                | Value _ ->
                    e

                | VarSet(v,e) ->
                    match extractStatements e with
                        | Some rb, e -> Expr.VarSet(v, e) |> rb |> deExpr
                        | None, e -> Expr.VarSet(v, e)

                | Var _ ->
                    e

                | WhileLoop(guard, body) ->
                    Expr.WhileLoop(guard, deExpr body)

                | _ ->
                    failwithf "[FShade] unexpected expression %A" e



    /// creates a new expression only containing e's visible side-effects.
    /// NOTE: all methods are assumed to be pure (except for the ones returning void/unit)
    let withoutValue (e : Expr) =
        DeadCodeElimination.withoutValue e
        
    /// creates a new expression by removing all unused variables/calls.
    /// NOTE: all methods are assumed to be pure (except for the ones returning void/unit)
    let eliminateDeadCode (e : Expr) =
        DeadCodeElimination.eliminateDeadCode e
        
    /// creates a new expression by evaluating all constant-subexpressions where possible.
    /// NOTE: all methods are assumed to be pure (except for the ones returning void/unit)
    let evaluateConstants (e : Expr) =
        ConstantFolding.evaluateConstants e
        
    /// creates a new expression only containing e's visible side-effects.
    /// NOTE: isSideEffect needs to determine whether a Method has non-local side-effects.
    let withoutValue' (isSideEffect : MethodInfo -> bool) (e : Expr) =
        DeadCodeElimination.withoutValue' isSideEffect e
        
    /// creates a new expression by removing all unused variables/calls.
    /// NOTE: isSideEffect needs to determine whether a Method has non-local side-effects.
    let eliminateDeadCode' (isSideEffect : MethodInfo -> bool)  (e : Expr) =
        DeadCodeElimination.eliminateDeadCode' isSideEffect e
        
    /// creates a new expression by evaluating all constant-subexpressions where possible.
    /// NOTE: isSideEffect needs to determine whether a Method has non-local side-effects.
    let evaluateConstants' (isSideEffect : MethodInfo -> bool)  (e : Expr) =
        ConstantFolding.evaluateConstants' isSideEffect e

    let liftInputs (e : Expr) =
        InputLifting.liftInputs e

    let inlining (e : Expr) =
        Inlining.inlining e

    let inlining' (f : UtilityFunction -> bool) (e : Expr) =
        Inlining.inlining' f e