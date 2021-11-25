open System
open System.IO
open System.Diagnostics
open System.Reflection
open Microsoft.FSharp.Quotations
open Aardvark.Base
open FShade

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
    let findEffects (file : string) =
        try Effect.read [||] |> ignore
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
                                    Effect.UsePrecompiled <- false
                                    let effect = Effect.ofExpr inputType res
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
    
    let readMeth = 
        match <@ fun (e : byte[]) -> Effect.read e @> with
        | Patterns.Lambda(_, Patterns.Call(None, mi, _)) -> mi
        | _ -> failwith ""

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

    let withEffectAtt = 
        match <@ WithEffectAttribute() @> with
        | Patterns.NewObject(ctor, _) -> ctor
        | _ -> failwith "WithEffectAttribute"

    let fromB64 =
        match <@ System.Convert.FromBase64String null @> with
        | Patterns.Call(None, mi,_) -> mi
        | _ -> failwith "FromBase64"

    let writeLine =
        match <@ System.Console.WriteLine("asd") @> with
        | Patterns.Call(_, mi, _) -> mi
        | _ -> failwith "WriteLine"

    let inlineEffectData (entries : list<string * int * int * byte[]>) =
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
                            let ass = typeof<Effect>.Assembly.Location |> File.ReadAllBytes
                            let ref = Path.Combine(Path.GetDirectoryName path, "FShade.Core.dll") |> File.ReadAllBytes
                            ass = ref
                        with _ ->
                            true
                | None ->
                    false





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


[<EntryPoint>]
let main argv =


    match Command.tryParse (Array.toList argv) with
    | Some cmd ->
        match cmd with
        | FindEffects assemblies ->
            use ms = new MemoryStream()
            use w = new BinaryWriter(ms, System.Text.Encoding.UTF8, true)

            let dirs = assemblies |> List.map Path.GetDirectoryName
            let exts = [".dll"; ".exe"]

            AppDomain.CurrentDomain.add_AssemblyResolve(ResolveEventHandler(fun _ e ->
                let assName = AssemblyName(e.Name)
                let name = assName.Name

                let result = 
                    dirs |> List.tryPick (fun dir ->
                        exts |> List.tryPick (fun ext ->
                            let p = Path.Combine(dir, name + ext)
                            if File.Exists p then
                                try 
                                    let ass = Assembly.LoadFile p
                                    if assName.FullName = ass.FullName then
                                        Some ass
                                    else 
                                        None
                                with _ -> 
                                    None
                            else
                                None
                        )
                    )

                match result with
                | Some r -> r
                | None -> printfn "could not load %A" e.Name; null
            ))


            let effects = assemblies |> List.collect Search.findEffects

            for (meth, effect) in effects do
                w.Write meth.DeclaringType.Assembly.Location
                w.Write meth.DeclaringType.MetadataToken
                w.Write meth.MetadataToken
                w.Write 0L 
                let p = ms.Position
                Effect.serialize ms effect
                let l = ms.Position - p

                let o = ms.Position
                ms.Position <- p - int64 sizeof<int64>
                w.Write l
                ms.Position <- o




            let data = ms.ToArray() |> Convert.ToBase64String
            eprintfn "%s" data
            0
        | ProcessAssemblies assemblies ->

            let self = Process.GetCurrentProcess()
            let file = self.MainModule.FileName

            let runSelf (args : list<string>) =
                let pi = 
                    if Path.GetExtension(file) = ".dll" then 
                        let pi = ProcessStartInfo("dotnet")
                        pi.ArgumentList.Add(file)
                        pi
                    else
                        ProcessStartInfo(file)

                for a in args do pi.ArgumentList.Add a
                pi.CreateNoWindow <- true
                pi.UseShellExecute <- false
                pi.RedirectStandardOutput <- true
                pi.RedirectStandardError <- true

                let proc = Process.Start pi


                proc.OutputDataReceived.Add (fun c ->
                    System.Console.WriteLine("{0}", c.Data)
                )

                let l = System.Collections.Generic.List<string>()
                proc.ErrorDataReceived.Add (fun d ->
                    if not (String.IsNullOrEmpty d.Data) then
                        l.Add d.Data
                )

                proc.BeginErrorReadLine()
                proc.BeginOutputReadLine()
                proc.WaitForExit()

                if proc.ExitCode = 0 && l.Count = 1 then
                    Some l.[0]
                else
                    None

            for a in assemblies do
                match runSelf ["-s"; a] with
                | Some res ->
                    let arr = res |> Convert.FromBase64String
                    use ms = new MemoryStream(arr)
                    //use gz = new System.IO.Compression.GZipStream(ms, Compression.CompressionMode.Decompress, true)
                    use r = new BinaryReader(ms, System.Text.Encoding.UTF8, true)

                    let data = System.Collections.Generic.List<string * int * int * byte[]>()
                    while ms.Position < ms.Length do
                        let assPath = r.ReadString()
                        let typeToken = r.ReadInt32()
                        let methToken = r.ReadInt32()
                        let len = r.ReadInt64()
                        let effectData = r.ReadBytes(int len)

                        data.Add(assPath, typeToken, methToken, effectData)

                    Fix.inlineEffectData (Seq.toList data)
                | None ->
                    printfn "failed"


            0


    | None ->
        -1