namespace FShade

open System
open Aardvark.Base

//
//module internal HMap =
//    let union (l : HMap<'a, 'b>) (r : HMap<'a, 'b>) =
//        let mutable res = l
//        for (k,v) in HMap.toSeq r do
//            res <- HMap.add k v res
//
//        res


type IFunctionSignature =
    interface end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FunctionSignature =
    open System.Reflection
    open Aardvark.Base.IL
    open Microsoft.FSharp.Reflection

    [<AutoOpen>]
    module Implementation = 
        type ClosureArg =
            | CArgument of int
            | CField of FieldInfo
            | CValue of obj

            
        type Argument =
            | Argument of int
            | Value of Type * obj

        open Microsoft.FSharp.Reflection

        let rec simulateCtor (values : hmap<FieldInfo, Argument>) (args : list<Argument>) (il : list<Instruction>) =
            match il with
                | [] -> Some values
                
                | Ldarg 0 :: Call (:? ConstructorInfo as ctor) :: il ->
                    simulateCtor values args il

                | (Nop | Start | Tail) :: il ->
                    simulateCtor values args il

                | Ldarg 0 :: Ldarg i :: Stfld f :: il ->
                    simulateCtor (HMap.add f args.[i - 1] values) args il

                | Ret :: il ->
                    simulateCtor values args il

                | _ ->
                    None
               
        type Type with
            member x.InvokeMethod =
                let invokes = x.GetMethods() |> Array.filter (fun mi -> mi.Name = "Invoke")
                if invokes.Length = 0 then null
                else invokes |> Array.maxBy (fun mi -> mi.GetParameters().Length)
            
            member x.TryResolveOverride(mi : MethodBase) =
                match mi with
                    | :? MethodInfo as mi ->
                        let parameters = mi.GetParameters()
                        let args = parameters |> Array.map (fun p -> p.ParameterType)
                        let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
                        let impl = x.GetMethod(mi.Name, flags, System.Type.DefaultBinder, args, null)

                        if isNull impl then
                            None
                        elif mi.IsGenericMethod then
                            if not impl.IsGenericMethod then
                                None
                            else
                                impl.MakeGenericMethod(mi.GetGenericArguments()) :> MethodBase |> Some
                        else
                            impl :> MethodBase |> Some
                    | _ ->
                        None


        type SimulationState =
            {
                self            : MethodBase
                target          : Argument
                targetFields    : hmap<FieldInfo, Argument> 
                locals          : hmap<Local, Argument>
                stack           : list<Argument>
            }

        open Aardvark.Base.Monads.State

        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module SimulationState =
            let isStatic =
                State.get |> State.map (fun s -> s.self.IsStatic)

            let tryGetTargetField (f : FieldInfo) =
                State.get |> State.map (fun s ->
                    match HMap.tryFind f s.targetFields with
                        | Some v -> Some v
                        | None ->
                            match s.target with
                                | Value(_,v) -> Value(f.FieldType, f.GetValue(v)) |> Some
                                | _ -> None
                )

            let pop =
                State.custom (fun s ->
                    match s.stack with
                        | h :: stack -> { s with stack = stack }, h
                        | _ -> failwith "empty stack"
                )

            let push (v : Argument) =
                State.modify (fun s -> { s with stack = v :: s.stack })

            let pushTarget =
                State.modify (fun s -> { s with stack = s.target :: s.stack })

            let stloc (l : Local) =
                State.modify (fun s ->
                    match s.stack with
                        | h :: stack ->
                            { s with locals = HMap.add l h s.locals; stack = stack }
                        | _ ->
                            failwith "empty stack"
                )
                
            let ldloc (l : Local) =
                State.modify (fun s ->
                    match HMap.tryFind l s.locals with
                        | Some v ->
                            { s with stack = v :: s.stack }
                        | _ ->
                            failwith "undeclared local"
                )
                
            let take (n : int) =
                State.custom (fun s ->
                    { s with stack = List.skip n s.stack }, List.rev (List.take n s.stack)
                )

        let tryDisassemble (mi : MethodBase) =
            if isNull mi then 
                None
            else
                try Some (Aardvark.Base.IL.Disassembler.disassemble mi)
                with _ -> None

        let conversions =
            LookupTable.lookupTable [
                (IL.ValueType.Int8, typeof<int8>),          fun o -> o |> unbox<int8>               :> obj
                (IL.ValueType.Int8, typeof<int16>),         fun o -> o |> unbox<int16>      |> int8 :> obj
                (IL.ValueType.Int8, typeof<int32>),         fun o -> o |> unbox<int32>      |> int8 :> obj
                (IL.ValueType.Int8, typeof<int64>),         fun o -> o |> unbox<int64>      |> int8 :> obj
                (IL.ValueType.Int8, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> int8 :> obj
                (IL.ValueType.Int8, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> int8 :> obj
                (IL.ValueType.Int8, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> int8 :> obj
                (IL.ValueType.Int8, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> int8 :> obj
                (IL.ValueType.Int8, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> int8 :> obj
                (IL.ValueType.Int8, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> int8 :> obj
                (IL.ValueType.Int8, typeof<float32>),       fun o -> o |> unbox<float32>    |> int8 :> obj
                (IL.ValueType.Int8, typeof<float>),         fun o -> o |> unbox<float>      |> int8 :> obj
                
                (IL.ValueType.UInt8, typeof<int8>),          fun o -> o |> unbox<int8>       |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<int16>),         fun o -> o |> unbox<int16>      |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<int32>),         fun o -> o |> unbox<int32>      |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<int64>),         fun o -> o |> unbox<int64>      |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<float32>),       fun o -> o |> unbox<float32>    |> uint8 :> obj
                (IL.ValueType.UInt8, typeof<float>),         fun o -> o |> unbox<float>      |> uint8 :> obj
                
                (IL.ValueType.Int16, typeof<int8>),          fun o -> o |> unbox<int8>       |> int16 :> obj
                (IL.ValueType.Int16, typeof<int16>),         fun o -> o |> unbox<int16>      |> int16 :> obj
                (IL.ValueType.Int16, typeof<int32>),         fun o -> o |> unbox<int32>      |> int16 :> obj
                (IL.ValueType.Int16, typeof<int64>),         fun o -> o |> unbox<int64>      |> int16 :> obj
                (IL.ValueType.Int16, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> int16 :> obj
                (IL.ValueType.Int16, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> int16 :> obj
                (IL.ValueType.Int16, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> int16 :> obj
                (IL.ValueType.Int16, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> int16 :> obj
                (IL.ValueType.Int16, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> int16 :> obj
                (IL.ValueType.Int16, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> int16 :> obj
                (IL.ValueType.Int16, typeof<float32>),       fun o -> o |> unbox<float32>    |> int16 :> obj
                (IL.ValueType.Int16, typeof<float>),         fun o -> o |> unbox<float>      |> int16 :> obj
                
                (IL.ValueType.UInt16, typeof<int8>),          fun o -> o |> unbox<int8>       |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<int16>),         fun o -> o |> unbox<int16>      |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<int32>),         fun o -> o |> unbox<int32>      |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<int64>),         fun o -> o |> unbox<int64>      |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<float32>),       fun o -> o |> unbox<float32>    |> uint16 :> obj
                (IL.ValueType.UInt16, typeof<float>),         fun o -> o |> unbox<float>      |> uint16 :> obj
                
                (IL.ValueType.Int32, typeof<int8>),          fun o -> o |> unbox<int8>       |> int32 :> obj
                (IL.ValueType.Int32, typeof<int16>),         fun o -> o |> unbox<int16>      |> int32 :> obj
                (IL.ValueType.Int32, typeof<int32>),         fun o -> o |> unbox<int32>      |> int32 :> obj
                (IL.ValueType.Int32, typeof<int64>),         fun o -> o |> unbox<int64>      |> int32 :> obj
                (IL.ValueType.Int32, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> int32 :> obj
                (IL.ValueType.Int32, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> int32 :> obj
                (IL.ValueType.Int32, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> int32 :> obj
                (IL.ValueType.Int32, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> int32 :> obj
                (IL.ValueType.Int32, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> int32 :> obj
                (IL.ValueType.Int32, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> int32 :> obj
                (IL.ValueType.Int32, typeof<float32>),       fun o -> o |> unbox<float32>    |> int32 :> obj
                (IL.ValueType.Int32, typeof<float>),         fun o -> o |> unbox<float>      |> int32 :> obj
                
                (IL.ValueType.UInt32, typeof<int8>),          fun o -> o |> unbox<int8>       |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<int16>),         fun o -> o |> unbox<int16>      |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<int32>),         fun o -> o |> unbox<int32>      |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<int64>),         fun o -> o |> unbox<int64>      |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<float32>),       fun o -> o |> unbox<float32>    |> uint32 :> obj
                (IL.ValueType.UInt32, typeof<float>),         fun o -> o |> unbox<float>      |> uint32 :> obj
                
                (IL.ValueType.Int64, typeof<int8>),          fun o -> o |> unbox<int8>       |> int64 :> obj
                (IL.ValueType.Int64, typeof<int16>),         fun o -> o |> unbox<int16>      |> int64 :> obj
                (IL.ValueType.Int64, typeof<int32>),         fun o -> o |> unbox<int32>      |> int64 :> obj
                (IL.ValueType.Int64, typeof<int64>),         fun o -> o |> unbox<int64>      |> int64 :> obj
                (IL.ValueType.Int64, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> int64 :> obj
                (IL.ValueType.Int64, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> int64 :> obj
                (IL.ValueType.Int64, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> int64 :> obj
                (IL.ValueType.Int64, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> int64 :> obj
                (IL.ValueType.Int64, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> int64 :> obj
                (IL.ValueType.Int64, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> int64 :> obj
                (IL.ValueType.Int64, typeof<float32>),       fun o -> o |> unbox<float32>    |> int64 :> obj
                (IL.ValueType.Int64, typeof<float>),         fun o -> o |> unbox<float>      |> int64 :> obj
                
                (IL.ValueType.UInt64, typeof<int8>),          fun o -> o |> unbox<int8>       |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<int16>),         fun o -> o |> unbox<int16>      |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<int32>),         fun o -> o |> unbox<int32>      |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<int64>),         fun o -> o |> unbox<int64>      |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<float32>),       fun o -> o |> unbox<float32>    |> uint64 :> obj
                (IL.ValueType.UInt64, typeof<float>),         fun o -> o |> unbox<float>      |> uint64 :> obj
                
                (IL.ValueType.NativeInt, typeof<int8>),          fun o -> o |> unbox<int8>       |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<int16>),         fun o -> o |> unbox<int16>      |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<int32>),         fun o -> o |> unbox<int32>      |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<int64>),         fun o -> o |> unbox<int64>      |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<float32>),       fun o -> o |> unbox<float32>    |> nativeint :> obj
                (IL.ValueType.NativeInt, typeof<float>),         fun o -> o |> unbox<float>      |> nativeint :> obj
                
                (IL.ValueType.UNativeInt, typeof<int8>),          fun o -> o |> unbox<int8>       |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<int16>),         fun o -> o |> unbox<int16>      |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<int32>),         fun o -> o |> unbox<int32>      |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<int64>),         fun o -> o |> unbox<int64>      |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<float32>),       fun o -> o |> unbox<float32>    |> unativeint :> obj
                (IL.ValueType.UNativeInt, typeof<float>),         fun o -> o |> unbox<float>      |> unativeint :> obj
                
                (IL.ValueType.Float32, typeof<int8>),          fun o -> o |> unbox<int8>       |> float32 :> obj
                (IL.ValueType.Float32, typeof<int16>),         fun o -> o |> unbox<int16>      |> float32 :> obj
                (IL.ValueType.Float32, typeof<int32>),         fun o -> o |> unbox<int32>      |> float32 :> obj
                (IL.ValueType.Float32, typeof<int64>),         fun o -> o |> unbox<int64>      |> float32 :> obj
                (IL.ValueType.Float32, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> float32 :> obj
                (IL.ValueType.Float32, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> float32 :> obj
                (IL.ValueType.Float32, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> float32 :> obj
                (IL.ValueType.Float32, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> float32 :> obj
                (IL.ValueType.Float32, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> float32 :> obj
                (IL.ValueType.Float32, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> float32 :> obj
                (IL.ValueType.Float32, typeof<float32>),       fun o -> o |> unbox<float32>    |> float32 :> obj
                (IL.ValueType.Float32, typeof<float>),         fun o -> o |> unbox<float>      |> float32 :> obj
                
                (IL.ValueType.Float64, typeof<int8>),          fun o -> o |> unbox<int8>       |> float :> obj
                (IL.ValueType.Float64, typeof<int16>),         fun o -> o |> unbox<int16>      |> float :> obj
                (IL.ValueType.Float64, typeof<int32>),         fun o -> o |> unbox<int32>      |> float :> obj
                (IL.ValueType.Float64, typeof<int64>),         fun o -> o |> unbox<int64>      |> float :> obj
                (IL.ValueType.Float64, typeof<nativeint>),     fun o -> o |> unbox<nativeint>  |> float :> obj
                (IL.ValueType.Float64, typeof<uint8>),         fun o -> o |> unbox<uint8>      |> float :> obj
                (IL.ValueType.Float64, typeof<uint16>),        fun o -> o |> unbox<uint16>     |> float :> obj
                (IL.ValueType.Float64, typeof<uint32>),        fun o -> o |> unbox<uint32>     |> float :> obj
                (IL.ValueType.Float64, typeof<uint64>),        fun o -> o |> unbox<uint64>     |> float :> obj
                (IL.ValueType.Float64, typeof<unativeint>),    fun o -> o |> unbox<unativeint> |> float :> obj
                (IL.ValueType.Float64, typeof<float32>),       fun o -> o |> unbox<float32>    |> float :> obj
                (IL.ValueType.Float64, typeof<float>),         fun o -> o |> unbox<float>      |> float :> obj

            ]

        let rec simulateNew (args : Argument[]) (il : list<Instruction>) =
            state {
                match il with
                    | (Start | Tail | Nop) :: il ->
                        return! simulateNew args il

                    | Ldloc l :: il ->
                        do! SimulationState.ldloc l
                        return! simulateNew args il

                    | Stloc l :: il ->
                        do! SimulationState.stloc l
                        return! simulateNew args il

                    | Ldarg i :: il ->
                        let! isStatic = SimulationState.isStatic

                        if isStatic then
                            do! SimulationState.push args.[i]
                            return! simulateNew args il
                        elif i = 0 then
                            do! SimulationState.pushTarget
                            return! simulateNew args il
                        else
                            do! SimulationState.push args.[i-1]
                            return! simulateNew args il

                    | Ldfld f :: il when f.IsStatic ->
                        let v = f.GetValue(null)
                        do! SimulationState.push (Value(f.FieldType, v))
                        return! simulateNew args il

                    | Call mi :: il when mi.IsStatic && mi.GetParameters().Length = 0 ->
                        let v = mi.Invoke(null, [||])
                        let retType =
                            match mi with
                                | :? MethodInfo as mi -> mi.ReturnType
                                | :? ConstructorInfo as c -> c.DeclaringType
                                | _ -> failwith "unknown methodbase"
                        do! SimulationState.push (Value(retType, v))
                        return! simulateNew args il

                    | Ldfld f :: il ->
                        let! v = SimulationState.pop   
                        match v with
                            | Value(_,null) ->
                                let! res = SimulationState.tryGetTargetField f
                                match res with
                                    | Some v ->   
                                        do! SimulationState.push v
                                        return! simulateNew args il
                                    | None ->
                                        return None

                            | Value(_,v) -> 
                                do! SimulationState.push (Value (f.FieldType, f.GetValue v))
                                return! simulateNew args il

                            | _ ->
                                return None

                    | LdNull :: il ->
                        do! SimulationState.push (Value(typeof<obj>, null))
                        return! simulateNew args il
                
                    | LdConst c :: il  ->
                        let valueType, value = 
                            match c with
                                | Int8 v -> typeof<int8>, v :> obj
                                | UInt8 v -> typeof<uint8>, v :> obj
                                | Int16 v -> typeof<int16>, v :> obj
                                | UInt16 v -> typeof<uint16>, v :> obj
                                | Int32 v -> typeof<int32>, v :> obj
                                | UInt32 v -> typeof<uint32>, v :> obj
                                | Int64 v -> typeof<int64>, v :> obj
                                | UInt64 v -> typeof<uint64>, v :> obj
                                | Float64 v -> typeof<float>, v :> obj
                                | Float32 v -> typeof<float32>, v :> obj
                                | NativeInt v -> typeof<nativeint>, v :> obj
                                | UNativeInt v -> typeof<unativeint>, v :> obj
                                | String v -> typeof<string>, v :> obj
                        do! SimulationState.push (Value(valueType, value))
                        return! simulateNew args il

                    | (Conv target | ConvChecked target) :: il ->
                        let! v = SimulationState.pop
                        match v with
                            | Value(t,v) ->
                                let nv = conversions (target,t) v
                                do! SimulationState.push(Value(nv.GetType(), nv))
                                return! simulateNew args il
                            | _ ->
                                return None


                    | [Call mi; Ret] ->
                        let pars = mi.GetParameters().Length
                        
                        if mi.IsStatic && mi.Name = "InvokeFast" then
                            let! args = SimulationState.take pars

                            match args with
                                | (Value(t,v)) :: rest ->
                                    let invoke = v.GetType().InvokeMethod
                                    let parameters = mi.GetParameters()

                                    match tryDisassemble invoke with
                                        | Some def ->
                                            let res = simulateNew (List.toArray rest) def.Body
                                            let mutable state =
                                                {
                                                    self = invoke
                                                    target = Value(t,v)
                                                    targetFields = HMap.empty
                                                    locals = HMap.empty
                                                    stack = []
                                                }
                                            match res.Run(&state) with
                                                | Some(t,mi,args) -> return Some(t,mi,args)
                                                | None -> return Some(Some (Value(t,v)), invoke :> MethodBase, rest)
                                        | None ->
                                            return Some(Some (Value(t,v)), invoke :> MethodBase, rest)
                                | _ ->
                                    return None

                        elif mi.IsStatic then
                            let! args = SimulationState.take pars
                            return Some(None, mi, args)
//                            match tryDisassemble mi with
//                                | Some def ->
//                                    let res = simulateNew (List.toArray args) def.Body
//                                    let mutable state =
//                                        {
//                                            self = mi
//                                            target = Value(typeof<obj>, null)
//                                            targetFields = HMap.empty
//                                            locals = HMap.empty
//                                            stack = []
//                                        }
//                                    match res.Run(&state) with
//                                        | Some res -> return Some res
//                                        | None -> return Some(None, mi, args)
//                                | None ->
//                                    return Some(None, mi, args)
                            
              
                        else
                            let! args = SimulationState.take pars
                            let! target = SimulationState.pop

                            let impl =
                                if mi.IsAbstract then
                                    match target with
                                        | Value(_,t) -> t.GetType().TryResolveOverride mi
                                        | _ -> None

                                elif mi.IsVirtual then
                                    match target with
                                        | Value(_, t) -> t.GetType().TryResolveOverride mi
                                        | _ -> Some mi

                                else 
                                    Some mi

                            match impl with
                                | Some mi ->
                                    match tryDisassemble mi with
                                        | Some def -> 
                                            let res = simulateNew (List.toArray args) def.Body
                                            let mutable state =
                                                {
                                                    self = mi
                                                    target = target
                                                    targetFields = HMap.empty
                                                    locals = HMap.empty
                                                    stack = []
                                                }

                                            match res.Run(&state) with
                                                | Some res -> return Some res
                                                | None -> return Some(Some target, mi, args)
                                        | None ->
                                            return Some(Some target, mi, args)
                                | None ->
                                    return Some(Some target, mi, args)

                    | [NewObj ctor; Ret] when FSharpType.IsFunction ctor.DeclaringType ->
                        match tryDisassemble ctor with
                            | Some def ->
                                let parameters = ctor.GetParameters().Length
                                let! ctorArgs = SimulationState.take parameters

                                match simulateCtor HMap.empty ctorArgs def.Body with
                                    | Some values ->
                                        let invoke = ctor.DeclaringType.InvokeMethod
                                        match tryDisassemble invoke with
                                            | Some def ->
                                                let maxArg =
                                                    ctorArgs |> List.choose (function Argument i -> Some i | _ -> None) |> List.max

                                                let restArgs = Array.skip (maxArg + 1) args

                                                do! State.put { 
                                                        self = invoke
                                                        target = Value(ctor.DeclaringType, null)
                                                        targetFields = values
                                                        locals = HMap.empty
                                                        stack = []
                                                    }

                                                return! simulateNew restArgs def.Body
                                            | None ->
                                                return None
                                    | None ->
                                        return failwith "invalid ctor"
                            | None ->
                                return None

                    | _ ->
                        return None

            }

        let simulate (target : obj) (self : MethodBase) (il : list<Instruction>) =
            let mutable state =
                {
                    self = self
                    target = Value(self.DeclaringType, target)
                    targetFields = HMap.empty
                    locals = HMap.empty
                    stack = []
                }

            let res = simulateNew (Array.init 128 Argument) il
            res.Run(&state)

        let rec extractCall (target : obj) (mi : MethodBase) (args : list<Argument>) =
            if mi.IsStatic && mi.Name = "InvokeFast" then
                match args with
                    | Value(_,t) :: rest when not (isNull t) ->
                        let targetType = t.GetType()

                        let cnt = List.length rest

                        let invoke =
                            targetType.GetMethods()
                                |> Seq.tryFind (fun mi -> mi.Name = "Invoke" && mi.GetParameters().Length = cnt)
                                   
                        match invoke with
                            | Some mi -> extractCall t (mi :> MethodBase) rest
                            | None -> (None, mi, args)


                    | _ ->
                        (None, mi, args)

            elif mi.IsStatic then
                (None, mi, args)
                        
            else
                if isNull target then
                    failwith "[FShade] target is null for non-static function"

                match target with
                    | :? Delegate as d ->
                        
                        let impl = d.Method :> MethodBase
                        let target = d.Target
                        match tryDisassemble impl with
                            | Some definition -> 
                                match simulate target impl definition.Body with
                                    | Some(target, meth, args) ->
                                        (target, meth, args)
                                    | None ->
                                        (Some (Value(target.GetType(), target)), impl, args)
                            | None ->
                                (Some (Value(target.GetType(), target)), impl, args)
                                
                    | _ ->

                        let targetType = target.GetType()


                        let impl = 
                            if targetType <> mi.DeclaringType then
                                let parameters = mi.GetParameters()
                                let args = parameters |> Array.map (fun p -> p.ParameterType)
                                let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
                                let impl = targetType.GetMethod(mi.Name, flags, System.Type.DefaultBinder, args, null)

                                if isNull impl then
                                    Log.warn "[FShade] could not get override for method %A in type %A" mi targetType
                                    null
                                elif mi.IsGenericMethod then
                                    if not impl.IsGenericMethod then
                                        Log.warn "[FShade] could not get override for method %A in type %A" mi targetType
                                        null
                                    else
                                        impl.MakeGenericMethod(mi.GetGenericArguments()) :> MethodBase
                                else
                                    impl :> MethodBase

                            else
                                mi

                        if isNull impl then
                            (Some (Value(targetType, target)), mi, args)
                        else
                            match tryDisassemble impl with
                                | Some definition -> 
                                    match simulate target impl definition.Body with
                                        | Some(target, meth, args) ->
                                            (target, meth, args)
                                        | None ->
                                            (Some (Value(targetType, target)), impl, args)
                                | None ->
                                    (Some (Value(targetType, target)), impl, args)
                                    

        type Signature =
            {
                target : obj
                mi : MethodBase
                args : list<Argument>
            }
            with interface IFunctionSignature

        type RandomFunctionSignature() =
            let id = Guid.NewGuid()
            interface IFunctionSignature

            member x.Id = id

            override x.GetHashCode() = id.GetHashCode()
            override x.Equals o =
                match o with
                    | :? RandomFunctionSignature as o -> id = o.Id
                    | _ -> false

    let ofFunction (f : 'a -> 'b) =
        try

            let invoke =
                if typeof<'b>.Name.StartsWith "FSharpFunc" then
                    let invokes = f.GetType().GetMethods() |> Array.filter (fun mi -> mi.Name = "Invoke")
                    if invokes.Length = 0 then null
                    else invokes |> Array.maxBy (fun mi -> mi.GetParameters().Length)
                else
                    f.GetType().GetMethod "Invoke"

            if isNull invoke then
                failwithf "[FShade] could not get signature for function %A" f

            let (target, mi, args) = extractCall f invoke (invoke.GetParameters() |> Array.toList |> List.mapi (fun i _ -> Argument (1 + i)))

            let target =
                match target with
                    | Some (Value(t,v)) -> 
                        let targetFields = t.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                        if targetFields.Length = 0 then
                            null
                        else
                            v
                    | Some (Argument index) ->
                        index :> obj
                    | None ->
                        null

            let parameters = mi.GetParameters()

            let convert (o : obj) (t : Type) =
                match o with
                    | :? int as o ->
                        if t = typeof<int8> then int8 o :> obj
                        elif t = typeof<uint8> then uint8 o :> obj
                        elif t = typeof<int16> then int16 o :> obj
                        elif t = typeof<uint16> then uint16 o :> obj
                        elif t = typeof<int32> then o :> obj
                        elif t = typeof<uint32> then uint32 o :> obj
                        elif t = typeof<int64> then int64 o :> obj
                        elif t = typeof<uint64> then uint64 o :> obj
                        else failwith "unconvertible"
                    | :? int64 as o ->
                        if t = typeof<int8> then int8 o :> obj
                        elif t = typeof<uint8> then uint8 o :> obj
                        elif t = typeof<int16> then int16 o :> obj
                        elif t = typeof<uint16> then uint16 o :> obj
                        elif t = typeof<int32> then o :> obj
                        elif t = typeof<uint32> then uint32 o :> obj
                        elif t = typeof<int64> then int64 o :> obj
                        elif t = typeof<uint64> then uint64 o :> obj
                        else failwith "unconvertible"
                    | _ ->
                        failwith "unconvertible"

            let args = 
                args |> List.mapi (fun i a ->
                    match a with
                        | Value(t,v) ->
                            let p = parameters.[i]
                            if t = p.ParameterType then 
                                Value(t,v)
                            else 
                                Value(p.ParameterType, convert v p.ParameterType)
                        | Argument i ->
                            Argument i
                )



            { target = target; mi = mi; args = args } :> IFunctionSignature
        with e ->
            Log.warn "[FShade] could not get function signature for %A" f
            Log.warn "[FShade] said: '%s'" e.Message
            RandomFunctionSignature() :> IFunctionSignature

    let tryGetAttribute<'a when 'a :> System.Attribute>(signature : IFunctionSignature) =
        match signature with
            | :? Signature as s -> 
                match s.mi.GetCustomAttributes(typeof<'a>) |> Seq.tryHead with
                    | Some (:? 'a as a) -> Some a
                    | _ -> None
            | _ ->
                None

type UniformScope(parent : Option<UniformScope>, name : string) = 
    static let mutable currentId = 0

    let id = System.Threading.Interlocked.Increment(&currentId)
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
    member x.FullName = 
        let rec build (name : string) (s : UniformScope) =
            match s.Parent with
                | None -> 
                    if name.Length = 0 then "Global"
                    else name
                | Some p ->
                    build (s.Name + name) p

        build "" x

    member x.GetChildScope(n : string) =
        lock childScopes (fun () ->
            match childScopes.TryGetValue n with
                | (true,s) -> 
                    s
                | _ -> 
                    let s = UniformScope(Some x, n)
                    childScopes.[n] <- s
                    s
        )
   
type ISemanticValue =
    abstract member Semantic : string
    abstract member Scope : UniformScope

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type SemanticAttribute(s : string) =
    inherit Attribute()
    member x.Semantic = s

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type InterpolationAttribute(qualifier : InterpolationMode) =
    inherit Attribute()
    member x.Qualifier = qualifier

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type PrimitiveIndexAttribute(index : int) =
    inherit Attribute()
    member x.Index = index

type IShaderBuilder =
    abstract member ShaderStage : ShaderStage
    abstract member OutputTopology : Option<OutputTopology>

type ParameterDescription =
    {
        paramType           : Type
        paramInterpolation  : InterpolationMode
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParameterDescription =

    let inline paramInterpolation (p : ParameterDescription) = p.paramInterpolation
    let inline paramType (p : ParameterDescription) = p.paramType

    let ofType (t : Type) =
        {
            paramType = t
            paramInterpolation = InterpolationMode.Default
        }


module Formats =
    type IFormat = interface end 
    type IFloatingFormat = inherit IFormat
    type ISignedFormat = inherit IFormat
    type IUnsignedFormat = inherit IFormat

    type rgba32f() = interface IFloatingFormat
    type rgba16f() = interface IFloatingFormat
    type rg32f() = interface IFloatingFormat
    type rg16f() = interface IFloatingFormat
    type r11g11b10f() = interface IFloatingFormat
    type r32f() = interface IFloatingFormat
    type r16f() = interface IFloatingFormat

    type rgba16() = interface IFloatingFormat
    type rgb10a2() = interface IFloatingFormat
    type rgba8() = interface IFloatingFormat
    type rg16() = interface IFloatingFormat
    type rg8() = interface IFloatingFormat
    type r16() = interface IFloatingFormat
    type r8() = interface IFloatingFormat

    type rgba16_snorm() = interface IFloatingFormat
    type rgba8_snorm() = interface IFloatingFormat
    type rg16_snorm() = interface IFloatingFormat
    type rg8_snorm() = interface IFloatingFormat
    type r16_snorm() = interface IFloatingFormat
    type r8_snorm() = interface IFloatingFormat

    type rgba32ui() = interface IUnsignedFormat
    type rgba16ui() = interface IUnsignedFormat
    type rgb10a2ui() = interface IUnsignedFormat
    type rgba8ui() = interface IUnsignedFormat
    type rg32ui() = interface IUnsignedFormat
    type rg16ui() = interface IUnsignedFormat
    type rg8ui() = interface IUnsignedFormat
    type r32ui() = interface IUnsignedFormat
    type r16ui() = interface IUnsignedFormat
    type r8ui() = interface IUnsignedFormat

    type rgba32i() = interface ISignedFormat
    type rgba16i() = interface ISignedFormat
    type rgba8i() = interface ISignedFormat
    type rg32i() = interface ISignedFormat
    type rg16i() = interface ISignedFormat
    type rg8i() = interface ISignedFormat
    type r32i() = interface ISignedFormat
    type r16i() = interface ISignedFormat
    type r8i() = interface ISignedFormat




type UniformValue =
    | Attribute of scope : UniformScope * name : string
    | Sampler of textureName : string * SamplerState
    | SamplerArray of array<string * SamplerState>

type UniformParameter =
    {
        uniformName         : string
        uniformType         : Type
        uniformValue        : UniformValue
    }




type ISampler =
    abstract member Texture : ISemanticValue
    abstract member State : SamplerState

type IImage =
    interface end

type ShaderTextureHandle(semantic : string, scope : UniformScope) =
    static member CreateUniform(semantic : string, scope : UniformScope) = ShaderTextureHandle(semantic, scope)
    interface ISemanticValue with
        member x.Semantic = semantic
        member x.Scope = scope

    member x.WithIndex (i : int) =
        ShaderTextureHandle(semantic + string i, scope)

    new() = ShaderTextureHandle(null, Unchecked.defaultof<UniformScope>)       
    

type TextureMustBeSpecified = TextureMustBeSpecified

type SamplerBaseBuilder() =
    member x.Yield(_) = TextureMustBeSpecified

    [<CustomOperation("texture")>]
    member x.Texture(b : TextureMustBeSpecified, t : ShaderTextureHandle) =
        (t, SamplerState.empty)

    [<CustomOperation("textureArray")>]
    member x.TextureArray(b : TextureMustBeSpecified, t : ShaderTextureHandle, count : int) =
        ((t, count), SamplerState.empty)


    [<CustomOperation("addressU")>]
    member x.AddressU((t, h : SamplerState), w : WrapMode) = t,{ h with AddressU = Some w }
             
    [<CustomOperation("addressV")>]
    member x.AddressV((t, h : SamplerState), w : WrapMode) = t,{ h with AddressV = Some w }
             
    [<CustomOperation("addressW")>]
    member x.AddressW((t, h : SamplerState), w : WrapMode) = t,{ h with AddressW = Some w }
             
    [<CustomOperation("maxAnisotropy")>]
    member x.MaxAnisotropy((t, h : SamplerState), a : int) = t,{ h with MaxAnisotropy = Some a }
             
    [<CustomOperation("borderColor")>]
    member x.BorderColor((t, h : SamplerState), c : C4f) = t,{ h with BorderColor = Some c }
             
    [<CustomOperation("maxLod")>]
    member x.MaxLod((t, h : SamplerState), c : float) = t,{ h with MaxLod = Some c }
             
    [<CustomOperation("minLod")>]
    member x.MinLod((t, h : SamplerState), c : float) = t,{ h with MinLod = Some c }
             
    [<CustomOperation("mipLodBias")>]
    member x.MipLodBias((t, h : SamplerState), c : float) = t,{ h with MipLodBias = Some c }
             
    [<CustomOperation("filter")>]
    member x.Filter((t, h : SamplerState), f : Filter) = t,{ h with Filter = Some f }

    [<CustomOperation("comparison")>]
    member x.Comparison((t, h : SamplerState), f : ComparisonFunction) = t,{ h with Comparison = Some f }

[<AutoOpen>]
module UniformExtensions =

    let uniform = UniformScope(None, "Global")


    type internal UniformStuff private() =

        [<ThreadStatic; DefaultValue>]
        static val mutable private current : Option<UniformScope * string>

        static member Push() =
            let old = UniformStuff.current
            UniformStuff.current <- None
            old

        static member Pop(old : Option<_>) =
            let c = UniformStuff.current
            UniformStuff.current <- old
            c

        static member Set(scope : UniformScope, name : string) =
            UniformStuff.current <- Some(scope, name)

    let (?) (s : UniformScope) (name : string) : 'a =
        let t = typeof<'a>

        if typeof<ISemanticValue>.IsAssignableFrom t then

            let creator = t.GetMethod("CreateUniform", [|typeof<string>; typeof<UniformScope>|])
            if creator <> null then
                let result = creator.Invoke(null, [| name :> obj ; s :> obj |])
                result |> unbox
            else
                UniformStuff.Set(s, name)
                Unchecked.defaultof<'a>
                
        elif t = typeof<UniformScope> then
            s.GetChildScope name |> unbox<'a>
        else
            UniformStuff.Set(s, name)
            Unchecked.defaultof<'a>