namespace Preprocessor
open System
open System.IO
open System.Diagnostics
open System.Reflection
open Microsoft.FSharp.Quotations
open Aardvark.Base

#nowarn "1337"

type Command =
    | FindEffects of assemblies : list<string>
    | ProcessAssemblies of assemblies : list<string>

module Command =
    module List = 
        let rec splitWhile (condition : 'a -> bool) (l : list<'a>) =
            match l with
            | [] -> [], []
            | h :: t ->
                if condition h then
                    let a,b = splitWhile condition t
                    h::a, b
                else
                    [], l
    
    let tryParse (args : list<string>) =
        match args with
        | "-s" :: rest ->
            let ass, rest = rest |> List.splitWhile (fun c -> not (c.StartsWith "-"))
            Some (FindEffects ass)

        | rest ->
            let ass, rest = rest |> List.splitWhile (fun c -> not (c.StartsWith "-"))
            Some (ProcessAssemblies ass)
            






module Search =
    open Microsoft.FSharp.Reflection

    let (?) (t : Type) (name : string) : 'a =
        if FSharpType.IsFunction typeof<'a> then
            let (d, i) = FSharpType.GetFunctionElements typeof<'a>

            let argTypes =
                if d = typeof<unit> then [||]
                elif FSharpType.IsTuple d then FSharpType.GetTupleElements d
                else [| d |]

            let stat = t.GetMethod(name, BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static, Type.DefaultBinder, argTypes, null)

            if not (isNull stat) then
                FSharpValue.MakeFunction(typeof<'a>, fun arg ->
                    let args = 
                        if FSharpType.IsTuple d then FSharpValue.GetTupleFields(arg)
                        elif d = typeof<unit> then [||]
                        else [|arg|]
                    stat.Invoke(null, args)
                ) |> unbox<'a>
            elif argTypes.Length > 0 then
                let t = argTypes.[0]
                let rest = argTypes.[1..]
                let inst = t.GetMethod(name, BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance, Type.DefaultBinder, rest, null)
                if not (isNull inst) then  
                    FSharpValue.MakeFunction(typeof<'a>, fun arg ->
                        let args = 
                            if FSharpType.IsTuple d then FSharpValue.GetTupleFields(arg)
                            else [|arg|]

                        let target = args.[0]
                        let args = args.[1..]
                        stat.Invoke(target, args)
                    ) |> unbox<'a>
                else
                    failwith "bad"
            else
                let all = t.GetMethods(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static)
                let meths = all |> Array.filter (fun mi -> mi.Name = name)
                failwith "bad"
        else
            failwith "bad"

    let findEffects (fshade : Assembly) (file : string) =
        let tEffect = fshade.GetType("FShade.Effect")
        let tEffectModule = fshade.GetType("FShade.EffectModule")

        let r = tEffectModule.GetMethod("read", BindingFlags.Public ||| BindingFlags.Static)
        try r.Invoke(null, [|Array.empty<byte>|]) |> ignore
        with _ -> ()

        try
        
            let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static

            let a = Assembly.LoadFile file

            let rec allTypes (t : Type) =
                let nested = t.GetNestedTypes flags
                Seq.append (Seq.singleton t) (nested |> Seq.collect allTypes)

            a.DefinedTypes
            |> Seq.collect allTypes
            |> Seq.collect (fun t ->
                t.GetMethods(flags)
                |> Seq.choose (fun mi ->
                    if typeof<Expr>.IsAssignableFrom mi.ReturnType && mi.GetParameters().Length = 1 then
                        let pars = mi.GetParameters()
                        let inputType = pars.[0].ParameterType
                        try 
                            let value =
                                if inputType.IsValueType then Activator.CreateInstance(inputType)
                                else null

                            let res = mi.Invoke(null, [| value |]) :?> Expr
                            Log.startTimed "compiling %s.%s" mi.DeclaringType.FullName mi.Name
                            try
                                try 
                                    tEffectModule.GetProperty("UsePrecompiled").SetValue(null, false)
                                    //Effect.UsePrecompiled <- false


                                    let effect = tEffectModule.GetMethod("ofExpr").Invoke(null, [|inputType; res|])
                                    tEffect.GetProperty("Shaders").GetValue(effect) |> ignore
                                    //let effect = Effect.ofExpr inputType res
                                    //effect.Shaders |> ignore
                                    Some(mi, effect)
                                with _ ->
                                    Log.warn "failed"
                                    None
                            finally
                                Log.stop()
                        with _ ->
                            None
                    else
                        None
                )
            )
            |> Seq.toList
        with e ->
            Log.warn "%A" e
            []


module Fix =
    open Mono.Cecil
    open Mono.Cecil.Cil
    

    let withValueMeth =
        match <@ Expr.WithValue(null, typeof<int>, Unchecked.defaultof<_>) @> with
        | Patterns.Call(None, mi, _) -> mi
        | _ -> failwith ""
        
    let castMeth =
        match <@ Expr.Cast<int>(Unchecked.defaultof<_>) @> with
        | Patterns.Call(None, mi, _) -> mi.GetGenericMethodDefinition()
        | _ -> failwith ""

    let typeProp =
        match <@ fun (e : Expr) -> e.Type @> with
        | Patterns.Lambda(_, Patterns.PropertyGet(_, pi, [])) -> pi
        | _ -> failwith "Type property"


    let fromB64 =
        match <@ System.Convert.FromBase64String null @> with
        | Patterns.Call(None, mi,_) -> mi
        | _ -> failwith "FromBase64"

    let writeLine =
        match <@ System.Console.WriteLine("asd") @> with
        | Patterns.Call(_, mi, _) -> mi
        | _ -> failwith "WriteLine"

    let inlineEffectData (fshade : Assembly) (entries : list<string * int * int * byte[]>) =
    
        let tEffect = fshade.GetType("FShade.Effect")
        let tEffectModule = fshade.GetType("FShade.EffectModule")
        let entries =
            entries
            |> List.groupBy (fun (p,_,_,_) -> p)

        for path, entries in entries do
            let pdbPath = Path.ChangeExtension(path, ".pdb")

            let fileStream = new MemoryStream(File.ReadAllBytes path)
            
            let symbols = File.Exists pdbPath

            let pdbStream =
                if symbols then
                    new MemoryStream(File.ReadAllBytes pdbPath)
                else
                    null

            let r = ReaderParameters()
            if symbols then
                r.SymbolReaderProvider <- Mono.Cecil.Pdb.PdbReaderProvider()
                r.SymbolStream <- pdbStream
                r.ReadSymbols <- symbols
            use dll = AssemblyDefinition.ReadAssembly(fileStream,r)


            

            let main = dll.MainModule
            
            let identicalCore = 
                match main.AssemblyReferences |> Seq.tryFind (fun r -> r.Name = "FShade.Core") with
                | Some r ->
                        try
                            let ass = tEffect.Assembly.Location |> File.ReadAllBytes
                            let ref = Path.Combine(Path.GetDirectoryName path, "FShade.Core.dll") |> File.ReadAllBytes
                            ass = ref
                        with _ ->
                            true
                | None ->
                    false
            let readMeth = 
                tEffectModule.GetMethod("read")
                //match <@ fun (e : byte[]) -> Effect.read e @> with
                //| Patterns.Lambda(_, Patterns.Call(None, mi, _)) -> mi
                //| _ -> failwith ""

            let withEffectAtt = 
                fshade.GetType("FShade.WithEffectAttribute").GetConstructor([||])
                //match <@ WithEffectAttribute() @> with
                //| Patterns.NewObject(ctor, _) -> ctor
                //| _ -> failwith "WithEffectAttribute"





            //main.ImportReference(typeof<Effect>)
            let mEffectRead = main.ImportReference readMeth
            let mCast = main.ImportReference castMeth
            let mWithValue = main.ImportReference withValueMeth
            let mGetType = main.ImportReference typeProp.GetMethod
            let mFromBase64 = main.ImportReference fromB64
            let cWithEffect = main.ImportReference withEffectAtt
            let mWriteLine = main.ImportReference writeLine

            let rec allTokens (t : TypeDefinition) =
                Seq.append 
                    (Seq.singleton (t.MetadataToken.ToInt32(), t))
                    (t.NestedTypes |> Seq.collect allTokens)

            let typeTable =
                dll.MainModule.Types
                |> Seq.collect allTokens
                |> Dict.ofSeq

            for (_, typeToken, methToken, data) in entries do
                match typeTable.TryGetValue typeToken with
                | (true, tdef) ->
                    match tdef.Methods |> Seq.tryFind (fun m -> m.MetadataToken.ToInt32() = methToken) with
                    | Some mdef ->
                        Log.line "patching %A" mdef
                        
                        let body = mdef.Body
                        let code = body.GetILProcessor()
                        
                        let t = mdef.ReturnType :?> GenericInstanceType
                        let mc = GenericInstanceMethod(mCast)
                        mc.GenericArguments.Add t.GenericArguments.[0]

                        let isPatched = mdef.CustomAttributes |> Seq.exists (fun c -> c.Constructor.DeclaringType.FullName = cWithEffect.DeclaringType.FullName)

                        if isPatched then
                            let mutable i = 0
                            let mutable fromBase64 = None
                            while Option.isNone fromBase64 && i < body.Instructions.Count do
                                let inst = body.Instructions.[i]
                                if inst.OpCode = OpCodes.Call || inst.OpCode = OpCodes.Callvirt then
                                    match inst.Operand with
                                    | :? MethodReference as r when r.FullName = mFromBase64.FullName ->
                                        fromBase64 <- Some inst
                                    | _ ->
                                        ()

                                i <- i + 1
                            
                            match fromBase64 with
                            | Some from ->
                                let prev = from.Previous
                                if not (isNull prev) && prev.OpCode = OpCodes.Ldstr then
                                    prev.Operand <- System.Convert.ToBase64String data
                            | None ->
                                failwithf "bad patched method: %A" mdef
                                

                        elif body.Instructions.[body.Instructions.Count - 1].OpCode = OpCodes.Ret then
                            mdef.CustomAttributes.Add(CustomAttribute(cWithEffect))
                            code.Remove(body.Instructions.[body.Instructions.Count - 1])

                            
                            code.Remove(body.Instructions.[body.Instructions.Count - 1])

                            let v = VariableDefinition(mdef.ReturnType)
                            mdef.Body.Variables.Add v
                            code.Append (code.Create(OpCodes.Stloc, v))
                        
                            code.Append (code.Create(OpCodes.Ldstr, System.Convert.ToBase64String data))
                            code.Append (code.Create(OpCodes.Call, mFromBase64))
                            code.Append (code.Create(OpCodes.Call, mEffectRead))

                            code.Append (code.Create(OpCodes.Ldloc, v))
                            code.Append (code.Create(OpCodes.Call, mGetType))

                            code.Append (code.Create(OpCodes.Ldloc, v))


                            code.Append (code.Create(OpCodes.Call, mWithValue))
                            code.Append (code.Create(OpCodes.Call, mc))
                            code.Append (code.Create(OpCodes.Ret))
                        else
                            failwithf "bad shader: %A" mdef
                    | None ->
                        ()
                | _ ->
                    ()

            dll.Write(path, WriterParameters(WriteSymbols = symbols))

        
        


        ()
