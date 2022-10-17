module Interpreter

open System.Reflection
open Mono.Cecil
open Mono.Cecil.Cil
open System.Runtime.Loader



let rec getType (ctx : AssemblyLoadContext) (t : TypeReference) : System.Type =
    let d = t.Resolve()
    let mm = d.Module
    let aName = mm.Assembly.FullName
    let mName = mm.Name
    
    let ass = ctx.LoadFromAssemblyName(AssemblyName aName)
    let mToken = d.MetadataToken.ToInt32()
    let m = ass.GetModule(mName)
    
    let targs = 
        match t with
        | :? GenericInstanceType as t ->
            t.GenericArguments |> Seq.map (getType ctx) |> Seq.toArray
        | _ ->
            [||]

    let r = m.ResolveType(mToken, targs, [||])
    if targs.Length > 0 then r.MakeGenericType targs
    else r
    
let getMethodBase (ctx : AssemblyLoadContext) (m : MethodReference) =
    let d = m.Resolve()
    let mm = d.Module
    let aName = mm.Assembly.FullName
    let mName = mm.Name

    let typ = getType ctx m.DeclaringType
    
    let mtargs =
        match m with
        | :? GenericInstanceMethod as m ->
            m.GenericArguments |> Seq.map (getType ctx) |> Seq.toArray
        | _ ->
            [||]

    let ass = ctx.LoadFromAssemblyName(AssemblyName aName)
    let mToken = d.MetadataToken.ToInt32()
    let m = ass.GetModule(mName)
    
    let meth = 
        typ.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance)
        |> Array.tryFind (fun mi -> mi.MetadataToken = mToken)

    match meth with
    | Some m -> 
        if m.IsGenericMethod then m.MakeGenericMethod mtargs :> MethodBase
        else m :> MethodBase
    | None ->
        typ.GetConstructors(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance)
        |> Array.find (fun mi -> mi.MetadataToken = mToken)
        :> MethodBase
        
let getFieldInfo (ctx : AssemblyLoadContext) (m : FieldReference) =
    let d = m.Resolve()
    let mm = d.Module
    let aName = mm.Assembly.FullName
    let mName = mm.Name
    let ass = ctx.LoadFromAssemblyName(AssemblyName aName)
    let mToken = d.MetadataToken.ToInt32()
    
    let m = ass.GetModule(mName)
    m.ResolveField(mToken)


module Patterns =

    let (|Call|_|) (ctx : AssemblyLoadContext)  (i : Instruction) =
        match i.OpCode.Code with
        | Code.Call | Code.Callvirt -> Some (getMethodBase ctx (i.Operand :?> MethodReference) :?> MethodInfo)
        | _ -> None
        
    let (|NewObj|_|) (ctx : AssemblyLoadContext)  (i : Instruction) =
        match i.OpCode.Code with
        | Code.Newobj -> Some (getMethodBase ctx (i.Operand :?> MethodReference) :?> ConstructorInfo)
        | _ -> None

    let (|Ldc|_|) (ctx : AssemblyLoadContext) (i : Instruction) =
        if i.OpCode = OpCodes.Ldc_I4 then Some (System.Convert.ToInt32 i.Operand :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_S then Some (System.Convert.ToInt32 i.Operand :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_0 then Some (0 :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_1 then Some (1 :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_2 then Some (2 :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_3 then Some (3 :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_4 then Some (4 :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_5 then Some (5 :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_6 then Some (6 :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_7 then Some (7 :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_8 then Some (8 :> obj)
        elif i.OpCode = OpCodes.Ldc_I4_M1 then Some (-1 :> obj)
        elif i.OpCode = OpCodes.Ldc_I8 then Some (System.Convert.ToInt64 i.Operand)
        elif i.OpCode = OpCodes.Ldc_R4 then Some (System.Convert.ToSingle i.Operand)
        elif i.OpCode = OpCodes.Ldc_R8 then Some (System.Convert.ToDouble i.Operand)
        elif i.OpCode = OpCodes.Ldstr then Some (System.Convert.ToString i.Operand)
        elif i.OpCode = OpCodes.Ldtoken then
            let value = 
                match i.Operand with
                | :? MethodReference as m -> getMethodBase ctx m :> obj
                | :? FieldReference as m -> getFieldInfo ctx m :> obj
                | :? TypeReference as m -> getType ctx m :> obj
                | _ -> null
            Some ValueNone
        else
            None
        
    let (|Pop|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Pop then Some ()
        else None
        
    let (|Ldfld|_|) (ctx : AssemblyLoadContext) (i : Instruction) =
        if i.OpCode = OpCodes.Ldsfld || i.OpCode = OpCodes.Ldfld then Some (getFieldInfo ctx (i.Operand :?> FieldReference))
        else None
          
    let (|Stfld|_|) (ctx : AssemblyLoadContext) (i : Instruction) =
        if i.OpCode = OpCodes.Stsfld || i.OpCode = OpCodes.Stfld then Some (getFieldInfo ctx (i.Operand :?> FieldReference))
        else None
       
    let (|Ldloc|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldloc then Some (System.Convert.ToInt32 i.Operand)
        elif i.OpCode = OpCodes.Ldloc_S then Some (System.Convert.ToInt32 i.Operand)
        elif i.OpCode = OpCodes.Ldloc_0 then Some 0
        elif i.OpCode = OpCodes.Ldloc_1 then Some 1
        elif i.OpCode = OpCodes.Ldloc_2 then Some 2
        elif i.OpCode = OpCodes.Ldloc_3 then Some 3
        else None
        
    let (|Stloc|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Stloc then Some (System.Convert.ToInt32 i.Operand)
        elif i.OpCode = OpCodes.Stloc_S then Some (System.Convert.ToInt32 i.Operand)
        elif i.OpCode = OpCodes.Stloc_0 then Some 0
        elif i.OpCode = OpCodes.Stloc_1 then Some 1
        elif i.OpCode = OpCodes.Stloc_2 then Some 2
        elif i.OpCode = OpCodes.Stloc_3 then Some 3
        else None
        
    let (|Conv|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Conv_I || i.OpCode = OpCodes.Conv_Ovf_I then 
            let conv =
                if sizeof<nativeint> = 8 then fun (o : obj) -> System.Convert.ToInt64(o) |> nativeint :> obj
                else fun (o : obj) -> System.Convert.ToInt32(o) |> nativeint :> obj
            Some conv
        elif i.OpCode = OpCodes.Conv_U ||i.OpCode = OpCodes.Conv_Ovf_U then 
            let conv =
                if sizeof<nativeint> = 8 then fun (o : obj) -> System.Convert.ToInt64(o) |> unativeint :> obj
                else fun (o : obj) -> System.Convert.ToInt32(o) |> unativeint :> obj
            Some conv
            
        elif i.OpCode = OpCodes.Conv_I1 || i.OpCode = OpCodes.Conv_Ovf_I1 then 
            Some (fun (o : obj) -> System.Convert.ToSByte o :> obj)
        elif i.OpCode = OpCodes.Conv_I2 || i.OpCode = OpCodes.Conv_Ovf_I2 then 
            Some (fun (o : obj) -> System.Convert.ToInt16 o :> obj)
        elif i.OpCode = OpCodes.Conv_I4 || i.OpCode = OpCodes.Conv_Ovf_I4 then 
            Some (fun (o : obj) -> System.Convert.ToInt32 o :> obj)
        elif i.OpCode = OpCodes.Conv_I8 || i.OpCode = OpCodes.Conv_Ovf_I8 then 
            Some (fun (o : obj) -> System.Convert.ToInt64 o :> obj)
            
        elif i.OpCode = OpCodes.Conv_U1 || i.OpCode = OpCodes.Conv_Ovf_U1 then 
            Some (fun (o : obj) -> System.Convert.ToByte o :> obj)
        elif i.OpCode = OpCodes.Conv_U2 || i.OpCode = OpCodes.Conv_Ovf_U2 then 
            Some (fun (o : obj) -> System.Convert.ToUInt16 o :> obj)
        elif i.OpCode = OpCodes.Conv_U4 || i.OpCode = OpCodes.Conv_Ovf_U4 then 
            Some (fun (o : obj) -> System.Convert.ToUInt32 o :> obj)
        elif i.OpCode = OpCodes.Conv_U8 || i.OpCode = OpCodes.Conv_Ovf_U8 then 
            Some (fun (o : obj) -> System.Convert.ToUInt64 o :> obj)

            
        elif i.OpCode = OpCodes.Conv_R4 then 
            Some (fun (o : obj) -> System.Convert.ToSingle o :> obj)
        elif i.OpCode = OpCodes.Conv_R8 then 
            Some (fun (o : obj) -> System.Convert.ToDouble o :> obj)
            
        elif i.OpCode = OpCodes.Conv_Ovf_I then 
            Some (fun (o : obj) -> System.Convert.ToSingle o :> obj)
        else
            None

    let (|Tail|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Tail then Some ()
        else None
        
    let (|Volatile|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Volatile then Some ()
        else None
        
    let (|Ldarg|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldarg then Some (System.Convert.ToInt32 i.Operand)
        elif i.OpCode = OpCodes.Ldarg_S then Some (System.Convert.ToInt32 i.Operand)
        elif i.OpCode = OpCodes.Ldarg_0 then Some 0
        elif i.OpCode = OpCodes.Ldarg_1 then Some 1
        elif i.OpCode = OpCodes.Ldarg_2 then Some 2
        elif i.OpCode = OpCodes.Ldarg_3 then Some 3
        elif i.OpCode = OpCodes.Ldarga_S then Some (System.Convert.ToInt32 i.Operand)
        else None
        
    let (|Add|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Add then Some ()
        else None

type State(ctx : AssemblyLoadContext) =
    let cache = System.Collections.Generic.Dictionary<int * int, option<obj>>()
    member x.Context = ctx
    
    member x.TryGetLocalValue(instructionIndex : int, localIndex : int) =
        match cache.TryGetValue ((instructionIndex, localIndex)) with
        | (true, v) ->
            Some v
        | _ ->
            None
    
    member x.SetLocal(instructionIndex : int, localIndex : int, value : option<obj>) =
        cache.[(instructionIndex, localIndex)] <- value

let rec private tryGetTopOfStackInternal (state : State) (instructions : Instruction[]) (idx : int) =
    if idx > 0 then
        let i = instructions.[idx - 1]
        
        match i with
        | Patterns.Ldarg _ ->
            // arguments are unknown
            idx-1, None

        | Patterns.Tail | Patterns.Volatile ->
            // JIT helpers
            tryGetTopOfStackInternal state instructions (idx - 1)

        | Patterns.Ldfld state.Context fi ->
            if fi.IsStatic then
                idx - 1, Some (fi.GetValue null)
            else
                let idx, self = tryGetTopOfStackInternal state instructions (idx - 1)
                match self with
                | Some self ->
                    idx, Some (fi.GetValue self)
                | None -> 
                    idx, None
                  
        | Patterns.Stfld state.Context fi ->
            if fi.IsStatic then
                let idx, value = tryGetTopOfStackInternal state instructions (idx - 1)
                match value with
                | Some v -> 
                    try fi.SetValue(null, v)
                    with _ -> ()
                | None ->
                    ()
                tryGetTopOfStackInternal state instructions idx
            else
                let idx, value = tryGetTopOfStackInternal state instructions (idx - 1)
                let idx, self = tryGetTopOfStackInternal state instructions idx
                match self, value with
                | Some self, Some value -> 
                    try fi.SetValue(self, value)
                    with _ -> ()
                | _ ->
                    ()
                tryGetTopOfStackInternal state instructions idx
        
        
        | Patterns.NewObj state.Context ci ->
            let ps = ci.GetParameters()
            
            let mutable idx = idx - 1
            let mutable args = []
            for pi in 0 .. ps.Length - 1 do
                let ni, v = tryGetTopOfStackInternal state instructions idx
                args <- v :: args
                idx <- ni
            
            if args |> List.forall Option.isSome then
                let res = ci.Invoke(args |> List.map Option.get |> List.toArray)
                idx, Some res
            else
                idx, None

        | Patterns.Call state.Context mi ->
            let ps = mi.GetParameters()
            
            if mi.IsStatic then
                let mutable idx = idx - 1
                let mutable args = []
                for pi in 0 .. ps.Length - 1 do
                    let ni, v = tryGetTopOfStackInternal state instructions idx
                    args <- v :: args
                    idx <- ni
                    
                if mi.ReturnType = typeof<System.Void> then
                    let idx, res = tryGetTopOfStackInternal state instructions idx
                    if args |> List.forall Option.isSome then
                        mi.Invoke(null, args |> List.map Option.get |> List.toArray) |> ignore
                        idx, res
                    else
                        idx, None
                else
                    if args |> List.forall Option.isSome then
                        let res = mi.Invoke(null, args |> List.map Option.get |> List.toArray)
                        idx, Some res
                    else
                        idx, None

            else
                let mutable idx = idx - 1
                let mutable args = []
                for pi in 0 .. ps.Length - 1 do
                    let ni, v = tryGetTopOfStackInternal state instructions idx
                    args <- v :: args
                    idx <- ni

                let ni, self = tryGetTopOfStackInternal state instructions idx
                idx <- ni

                if mi.ReturnType = typeof<System.Void> then
                    let idx, res = tryGetTopOfStackInternal state instructions idx
                    
                    idx, res
                else

                    match self with
                    | Some self when args |> List.forall Option.isSome ->
                        let res = mi.Invoke(self, args |> List.map Option.get |> List.toArray)
                        idx, Some res
                    | _ -> 
                        idx, None
        
        | Patterns.Pop ->
            let idx, _ = tryGetTopOfStackInternal state instructions (idx - 1)
            tryGetTopOfStackInternal state instructions idx
        
        | Patterns.Ldc state.Context value ->
            idx - 1, Some value
        
        | Patterns.Conv f ->
            let idx, value = tryGetTopOfStackInternal state instructions (idx - 1)
            match value with
            | Some v -> idx, Some(f v)
            | None -> idx, None

        | Patterns.Ldloc li ->
            let mutable stidx = idx - 1
            let inline isStore (i : int) =
                match instructions.[i] with
                | Patterns.Stloc ii when ii = li -> true
                | _ -> false
                
            while stidx >= 0 && not (isStore stidx) do
                stidx <- stidx - 1

            match instructions.[stidx] with
            | Patterns.Stloc i when i = li ->
                match state.TryGetLocalValue(stidx, li) with
                | Some v ->
                    idx - 1, v
                | None -> 
                    let _, v = tryGetTopOfStackInternal state instructions stidx
                    state.SetLocal(stidx, li, v)
                    idx - 1, v
            | _ ->
                idx - 1, None
                
        | Patterns.Stloc _ ->
            let idx, _ = tryGetTopOfStackInternal state instructions (idx - 1)
            tryGetTopOfStackInternal state instructions idx
            

        | _ ->
            Log.warn "bad instruction: %A" i
            idx - 1, None
           
    else
        idx - 1, None
    
let tryGetTopOfStack (ctx : AssemblyLoadContext) (instructions : Instruction[]) (idx : int) =
    let state = State(ctx)
    let _idx, v = tryGetTopOfStackInternal state instructions idx
    v

let tryFindParameterPushLocation (ctx : AssemblyLoadContext) (after : int) (instructions : Instruction[]) (callIndex : int) =
    let state = State(ctx)
    let mutable idx = callIndex
    let mutable after = after
    while idx >= 0 && after > 0 do
        let ni, _ = tryGetTopOfStackInternal state instructions idx
        idx <- ni
        after <- after - 1

    if after = 0 then   
        Some idx
    else
        None

