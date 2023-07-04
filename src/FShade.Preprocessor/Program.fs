open System
open System.IO
open System.Reflection
open System.Runtime.Loader

type Config =
    {
        Entry : string
        Dirs : list<string>
    }

type MethodDefinitionReference =
    {
        AssemblyPath : string
        AssemblyName : string
        DeclaringTypeName : string
        MethodName : string
        Token : int
    }

module MethodDefinitionReference =
    let ofMethodInfo (t : Type) (mi : MethodInfo) =
        {
            AssemblyPath = mi.DeclaringType.Assembly.Location
            AssemblyName = mi.DeclaringType.Assembly.GetName().Name
            DeclaringTypeName = t.FullName
            MethodName = mi.Name
            Token = mi.MetadataToken
        }

let loadAllAssemblies (cfg : Config) =

    let ctx = new AssemblyLoadContext("isolated", true)

    let entry = cfg.Entry
    let dirs = cfg.Dirs
    let exts = [".dll"; ".exe"]
         
    let nested = new System.Threading.ThreadLocal<bool>(fun _ -> false)
    
    let nameCache = System.Collections.Generic.Dictionary<string, option<Assembly>>()

    let tryLoadAssembly (ctx : AssemblyLoadContext) (assName : AssemblyName) =
        if nested.Value then
            None
        else
            let name = assName.Name
            match nameCache.TryGetValue name with
            | (true, ass) -> 
                ass
            | _ ->
                let result =
                    dirs |> List.tryPick (fun dir ->
                        exts |> List.tryPick (fun ext ->
                            let p = Path.Combine(dir, name + ext)
                            if File.Exists p then
                                try 
                                    //let tmp = Path.GetTempFileName() + ".dll"
                                    //File.Copy(p, tmp)
                                    let ass = ctx.LoadFromAssemblyPath p
                                    if assName.Name = ass.GetName().Name then
                                        Some ass
                                    else 
                                        None
                                with e -> 
                                    None
                            else
                                None
                        )
                    )
                let realResult = 
                    match result with
                    | Some r -> Some r
                    | None -> 
                        nested.Value <- true
                        try
                            let a = ctx.LoadFromAssemblyName assName
                            if isNull a then None
                            else Some a
                        finally
                            nested.Value <- false
                nameCache.[name] <- realResult
                realResult
                
    ctx.add_Resolving(fun ctx assName ->
        let result = tryLoadAssembly ctx assName
        match result with
        | Some r -> r
        | None -> null
    )

    let entry = 
        ctx.LoadFromAssemblyPath entry
    
    let allAssemblies =
        let rec run (set : System.Collections.Generic.HashSet<Assembly>) (a : Assembly) =
            if set.Add a then
                let referenced = 
                    a.GetReferencedAssemblies() |> Array.choose (fun name ->
                        if name.Name.StartsWith "System" then
                            None
                        else
                            try tryLoadAssembly ctx name
                            with _ -> None
                    )
                for r in referenced do run set r
        
        let set = System.Collections.Generic.HashSet()
        run set entry
        Seq.toArray set |> Array.sortBy (fun a -> a.GetName().Name)
    ctx
    
let getReplacableShaderCompileMethods (ctx : AssemblyLoadContext) =
    let allAssemblies =
        ctx.Assemblies 
        |> Seq.filter (fun a ->
            a.GetName().Name = "FShade.Core" || 
            a.GetReferencedAssemblies() |> Seq.exists (fun n ->
                n.Name = "FShade.Core"
            )
        )
        |> Seq.toArray
        |> Array.sortBy (fun a -> a.FullName)
        
    let allTypes = 
        let rec getAllTypes (t : Type) =
            if isNull t then
                Seq.empty
            else
                let nested =
                    try
                        t.GetNestedTypes(BindingFlags.NonPublic ||| BindingFlags.Public)
                    with
                    | :? ReflectionTypeLoadException as e -> e.Types
                    | _ -> [||]
                Seq.append 
                    (Seq.singleton t)
                    (nested |> Seq.collect getAllTypes)
        allAssemblies |> Seq.collect (fun a ->
            let types = 
                try a.GetTypes()
                with 
                    | :? ReflectionTypeLoadException as e -> e.Types
                    | _ -> [||]
            types |> Seq.collect getAllTypes   
        )
        |> System.Collections.Generic.HashSet
        |> Seq.toArray
        

    let effectType =
        ctx.LoadFromAssemblyName(AssemblyName("FShade.Core")).GetType("FShade.Effect")

    let shaderCompileMethods =
        allTypes |> Array.collect (fun t ->
            let ms = t.GetMethods(BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
            ms |> Array.choose (fun mi ->
                if not (mi.Name.EndsWith "$W") then
                    let ps = mi.GetParameters()
                    if ps.Length >= 1 && mi.DeclaringType.Assembly.GetName().Name <> "FSharp.Core" then
                        let parameterIndex = 
                            ps |> Array.tryFindIndex (fun p ->
                                let t = p.ParameterType
                                t.Name = "FSharpFunc`2" && t.GetGenericArguments().[1].Name.StartsWith "FSharpExpr"
                            )
                    
                        match parameterIndex with
                        | Some pi -> 
                            if mi.ReturnType = effectType then
                                if ps.Length = 1 then
                                    Some (t, mi, pi, None)
                                else
                                    None
                            else
                                let flags =
                                    if mi.IsStatic then BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic
                                    else BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic
                                    
                                let args =
                                    let c = ps |> Array.map (fun p -> p.ParameterType)
                                    c.[pi] <- effectType
                                    c
                                    
                                let overload = 
                                    t.GetMethod(mi.Name, 0, flags, Type.DefaultBinder, args, null)
                                
                                if not (isNull overload) then
                                    Some (t, mi, pi, Some overload)
                                else
                                    None

                        | None ->
                            None
                    else
                        None
                else
                    None
            )
        )

    
    shaderCompileMethods |> Array.map (fun (t, mi, pi, opt) ->
        let mi = MethodDefinitionReference.ofMethodInfo t mi
        match opt with
        | Some opt ->
            mi, pi, Some (MethodDefinitionReference.ofMethodInfo t opt)
        | None ->
            mi, pi, None
    )

open Mono.Cecil
open Mono.Cecil.Cil

let assemblyDefCache = System.Collections.Generic.Dictionary<string, option<string * AssemblyDefinition>>()

let cecilRead (path : string) (par : option<ReaderParameters>) =
    //let data = File.ReadAllBytes path
    //let ms = new MemoryStream(data)
    match par with
    | Some par -> AssemblyDefinition.ReadAssembly(path, par)
    | None -> AssemblyDefinition.ReadAssembly(path)
    

let resolveAssembly (dirs : list<string>) (par : option<ReaderParameters>) (assName : AssemblyNameReference) =
    let name = assName.Name
    let exts = [".dll"; ".exe"]

    let def = 
        dirs |> List.tryPick (fun dir ->
            exts |> List.tryPick (fun ext ->
                let p = Path.Combine(dir, name + ext)
                if File.Exists p then
                    match assemblyDefCache.TryGetValue p with
                    | (true, def) -> def
                    | _ ->
                        let result = 
                            try 
                                let ass = cecilRead p par
                                    
                                if assName.Name = ass.Name.Name then
                                    Some (p, ass)
                                else 
                                    None
                            with e -> 
                                Log.warn "load of %A failed: %A" p e
                                None
                        assemblyDefCache.[p] <- result
                        result
                else
                    None
            )
        )
        
    match def with
    | Some d ->
        Some d
    | None ->
        let ctx = AssemblyLoadContext("tmp", true)
        try
            try
                let ass = ctx.LoadFromAssemblyName (AssemblyName assName.Name)
                if isNull ass then
                    None
                else
                    let path = ass.Location
                    let def = cecilRead path par
                    assemblyDefCache.[path] <- Some (path, def)
                    Some (path, def)
            with _ ->
                None
        finally
            ctx.Unload()
    
let readAssembly (par : option<ReaderParameters>) (path : string) =
    match assemblyDefCache.TryGetValue path with
    | (true, (Some (_, ass))) -> ass
    | _ ->
        let ass = 
            cecilRead path par
            
        assemblyDefCache.[path] <- Some (path, ass)
        ass

let readerParams (dirs : list<string>) =

    let r = ReaderParameters()
    
    r.AssemblyResolver <- 
        { new IAssemblyResolver with
            member x.Resolve(name) =
                match resolveAssembly dirs None name with
                | Some (_, a) -> a
                | None -> null
            member x.Resolve(name, p) =
                match resolveAssembly dirs (Some p) name with
                | Some (_, a) -> a
                | None -> null
            member x.Dispose() =
                ()
        }
         
    //if symbols then
    //    r.SymbolReaderProvider <- Mono.Cecil.Pdb.PdbReaderProvider()
    //    r.SymbolStream <- pdbStream
    //    r.ReadSymbols <- symbols
    r


type Directory with
    static member Copy(srcPath, dstPath, ?copySubDirs : bool) =
        let copySubDirs = defaultArg copySubDirs true
        
        if not <| Directory.Exists(srcPath) then
            let msg = System.String.Format("Source directory does not exist or could not be found: {0}", srcPath)
            raise (DirectoryNotFoundException(msg))

        if not <| Directory.Exists(dstPath) then
            Directory.CreateDirectory(dstPath) |> ignore

        let srcDir = new DirectoryInfo(srcPath)

        for file in srcDir.GetFiles() do
            let temppath = System.IO.Path.Combine(dstPath, file.Name)
            file.CopyTo(temppath, true) |> ignore

        if copySubDirs then
            for subdir in srcDir.GetDirectories() do
                let dstSubDir = System.IO.Path.Combine(dstPath, subdir.Name)
                Directory.Copy(subdir.FullName, dstSubDir, copySubDirs)
            
[<EntryPoint>]
let main argv =
    
    let entry = 
        argv |> Array.tryFind (fun s ->
            try File.Exists (Path.GetFullPath s)
            with _ -> false
        )

    Log.verbose <-
        argv |> Array.exists (function "-v" | "--verbose" -> true | _ -> false)
        
    match entry with
    | None ->
        Log.error "usage: fshadeaot <entrydllpath> (--verbose)"
        exit -1
    | _ ->
        ()
    let entry = Option.get entry
        
    
    let dir = Path.GetDirectoryName entry
    let tmp = Path.Combine(Path.GetTempPath(), string (Guid.NewGuid()))
    Directory.CreateDirectory tmp |> ignore
    Directory.Copy(dir, tmp, true)
    
    try
        let config =
            {
                Entry = Path.Combine(tmp, Path.GetFileName entry)
                Dirs = [tmp]
            }
        
        let ctx = loadAllAssemblies config
        
        Log.start "searching for patchable methods"
        let shaderCompileMethods = 
            getReplacableShaderCompileMethods ctx
                  
      
                  
        for (mi, _, replacement) in shaderCompileMethods do
            match replacement with
            | Some repl ->
                Log.line "%s.%s -> %s.%s" mi.DeclaringTypeName mi.MethodName repl.DeclaringTypeName repl.MethodName 
            | None ->
                Log.line "%s.%s" mi.DeclaringTypeName mi.MethodName

        Log.stop()
                
        let readerParams = readerParams config.Dirs
        
        let tokenSet = 
            shaderCompileMethods |> Seq.map (fun (mi,_,_) -> mi.AssemblyName, mi.Token) |> Set.ofSeq


        let fshade = ctx.LoadFromAssemblyName(AssemblyName "FShade.Core")
        let tEffect = fshade.GetType("FShade.Effect")
        let tEffectModule = fshade.GetType("FShade.EffectModule")
        let mOfExpr = tEffectModule.GetMethod("ofExpr")
        let mPickle = tEffectModule.GetMethod("pickle")
        let pId = tEffect.GetProperty("Id")
    
        let legacy, allDefs =

            let rec load (set : System.Collections.Generic.Dictionary<string, option<string * AssemblyDefinition>>) (name : AssemblyNameReference) =
                let strName = name.FullName
                if not (set.ContainsKey strName) then
                    let res = resolveAssembly config.Dirs (Some readerParams) name 
                    set.[strName] <- res

                    match res with
                    | Some (_, res) ->
                    
                        let refs = res.Modules |> Seq.collect (fun m -> m.AssemblyReferences)
                        for r in refs do load set r
                    | None ->
                        ()

            let state = System.Collections.Generic.Dictionary()
        
            let entryDef = cecilRead config.Entry (Some readerParams) //AssemblyDefinition.ReadAssembly(config.Entry, readerParams)
            
            let framework = 
                entryDef.CustomAttributes |> Seq.pick (fun a ->
                    let name = typeof<System.Runtime.Versioning.TargetFrameworkAttribute>.FullName
                    if a.AttributeType.FullName = name then
                        a.ConstructorArguments |> Seq.tryPick (fun a ->
                            match a.Value with
                            | :? string as s -> Some s
                            | _ -> None
                        )
                    else
                        None
                )

            let legacy = framework.ToLower().Contains "netframework"
            
            state.[AssemblyNameReference(entryDef.Name.Name, entryDef.Name.Version).ToString()] <- Some (config.Entry, entryDef)
            for m in entryDef.Modules do
                for r in m.AssemblyReferences do load state r

            legacy,
            state.Values 
            |> Seq.choose id 
            |> Seq.toArray
            |> Array.filter (fun (_, ass) ->
                ass.Name.Name <> "FShade.GLSL" &&
                ass.Modules |> Seq.exists (fun m -> m.AssemblyReferences |> Seq.exists (fun r -> r.Name = "FShade.Core"))
            )

        if legacy then 
            Log.error "netframework dlls cannot be patched atm."
            exit -1

        Log.start "processing %d assemblies" allDefs.Length

        let fshadeDef =
            resolveAssembly config.Dirs (Some readerParams) (AssemblyNameReference("FShade.Core", Version(0,0,0,0)))
            |> Option.get
            |> snd
        
        let rtDef =
            let name = AssemblyNameReference("System.Private.CoreLib", Version(0,0,0,0))
            resolveAssembly config.Dirs (Some readerParams) name
            |> Option.get
            |> snd
        
        let aardvarkBase =
            resolveAssembly config.Dirs (Some readerParams) (AssemblyNameReference("Aardvark.Base", Version(0,0,0,0)))
            |> Option.get
            |> snd
            
        let read = 
            fshadeDef.Modules |> Seq.pick (fun m ->
                let r = m.GetType "FShade.EffectModule"
                if not (isNull r) then
                    let d = r.Resolve()
                    
                    let unpickleResource = d.Methods |> Seq.tryFind (fun m -> m.Name = "unpickleResource")
                    match unpickleResource with
                    | Some m -> Some (Choice1Of4 m)
                    | None ->
                        let unpickleInternal = d.Methods |> Seq.tryFind (fun m -> m.Name = "unpickleInternal")
                        match unpickleInternal with
                        | Some m -> Some (Choice2Of4 m)
                        | None ->
                            let unpickleWithId = d.Methods |> Seq.tryFind (fun m -> m.Name = "unpickleWithId")
                            match unpickleWithId with
                            | Some m -> Some (Choice3Of4 m)
                            | None ->
                                let read = d.Methods |> Seq.tryFind (fun m -> m.Name = "read")
                                match read with
                                | Some r -> Some (Choice4Of4 r)
                                | None -> None
                    
                else
                    None
            )
            
        let fromBase64 = 
            rtDef.Modules |> Seq.pick (fun m ->
                let r = m.GetType "System.Convert"
                if not (isNull r) then
                    let d = r.Resolve()
                    d.Methods |> Seq.tryFind (fun m -> m.Name = "FromBase64String")
                else
                    None
            )
            
        //let tObject =
        //    rtDef.Modules |> Seq.pick (fun m ->
        //        let r = m.GetType "System.Object"
        //        if not (isNull r) then
        //            Some r
        //        else
        //            None
        //    )

        //Report.Line(4, "", [||])
        //let reportLine = 
        //    aardvarkBase.Modules |> Seq.pick (fun m ->
        //        let r = m.GetType "Aardvark.Base.Report"
        //        if not (isNull r) then
        //            let d = r.Resolve()
        //            d.Methods |> Seq.tryFind (fun m -> 
        //                m.Name = "Line" && m.Parameters.Count = 3 && 
        //                m.Parameters.[0].ParameterType.Name = "Int32" &&
        //                m.Parameters.[1].ParameterType.Name = "String" &&
        //                m.Parameters.[2].ParameterType.IsArray
        //            )
        //        else
        //            None
        //    )


        

        
        let changedAssemblies = System.Collections.Generic.List<string * AssemblyDefinition>()
        
        for (path, assdef) in allDefs do
            Log.start "%s" assdef.Name.Name
        
            let mutable changed = 0
        
            for mod_ in assdef.Modules do
                let fromBase64 =
                    lazy (mod_.ImportReference(fromBase64))
                
                let read = 
                    lazy (
                        match read with
                        | Choice1Of4 r -> Choice1Of4(mod_.ImportReference r)
                        | Choice2Of4 r -> Choice2Of4(mod_.ImportReference r)
                        | Choice3Of4 r -> Choice3Of4(mod_.ImportReference r)
                        | Choice4Of4 r -> Choice4Of4(mod_.ImportReference r)
                    )
                
                //let reportLine =
                //    lazy (
                //        mod_.ImportReference reportLine
                //    )

                let allTypes =
                    let rec collect (t : TypeDefinition) =
                        Seq.append 
                            (Seq.singleton t)
                            (t.NestedTypes |> Seq.collect collect)
                    mod_.GetTypes() |> Seq.collect collect |> System.Collections.Generic.HashSet
                
                for typ in allTypes do
                    for meth in typ.Methods do
                        try
                            let body = meth.Body.Instructions

                            let mutable bodyChanged = false  
                            let mutable idx = 0
                            let mutable found = false
                            while idx < body.Count do
                                let i = body.[idx]
                                if i.OpCode = Mono.Cecil.Cil.OpCodes.Call || i.OpCode = Mono.Cecil.Cil.OpCodes.Callvirt then
                                    match i.Operand with  
                                    | :? Mono.Cecil.MethodReference as m ->
                                        let def = m.Resolve()
                                        let t = def.MetadataToken.ToInt32()
                                        if Set.contains (def.Module.Assembly.Name.Name, t) tokenSet then
                                            found <- true
                                        
                                            let arr = Seq.toArray body
                                        
                                            let parameterIndex, call =
                                                shaderCompileMethods |> Array.pick (fun (m, pi, r) ->
                                                    let m = 
                                                        let a = readAssembly (Some readerParams) m.AssemblyPath
                                                        a.Modules |> Seq.pick (fun mm ->
                                                            let d = mm.LookupToken(m.Token)
                                                            if isNull d then None
                                                            else Some (d :?> MethodDefinition)
                                                        )
                                                    if def = m then
                                                        match r with
                                                        | Some r ->
                                                            let r = 
                                                                let a = readAssembly (Some readerParams) r.AssemblyPath
                                                                a.Modules |> Seq.pick (fun m ->
                                                                    let d = m.LookupToken(r.Token)
                                                                    if isNull d then None
                                                                    else Some (d :?> MethodDefinition)
                                                                )
                                                            Some (pi, Some r)
                                                        | None ->
                                                            Some (pi, None)
                                                    else
                                                        None
                                                            
                                                )

                                            let after = def.Parameters.Count - parameterIndex - 1 
                                            match Interpreter.tryFindParameterPushLocation ctx after arr idx with
                                            | Some pushIndex ->
                                                let res =
                                                    try 
                                                        Interpreter.tryGetTopOfStack ctx (Seq.toArray body) pushIndex
                                                    with e ->
                                                        Log.error "%A" e
                                                        None
                                        
                                                match res with
                                                | Some res when not (isNull res) ->
                                                    let result = 
                                                        try
                                                            let t = res.GetType()
                                                            let invoke = t.GetMethod("Invoke")
                                                            let quotation = invoke.Invoke(res, [|null|])
                                                            let tVertex = invoke.GetParameters().[0].ParameterType
                                                
                                                            let effect = mOfExpr.Invoke(null, [| tVertex; quotation |])
                                                            let binary = mPickle.Invoke(null, [|effect|]) :?> byte[]
                                                            let id = pId.GetValue(effect) :?> string
                                                            Some (id, binary)
                                                        with _ ->
                                                            None

                                                    match result with
                                                    | Some (id, binary) ->
                                                        Log.line "patching %s.%s: { Id = %A }" meth.DeclaringType.Name meth.Name id

                                                        // cleanup old call: also remove "tail." instruction if one exists
                                                        let isTail = idx > 0 && body.[idx - 1].OpCode = OpCodes.Tail
                                                        if isTail then
                                                            body.[idx - 1] <- Instruction.Create(OpCodes.Nop)

                                                        match call with
                                                        | Some call ->
                                                            let call = mod_.ImportReference call
                                                            body.[idx] <- Instruction.Create(OpCodes.Call, call)
                                                        | None ->
                                                            body.[idx] <- Instruction.Create(OpCodes.Nop)


                                                        // mod_.Resources.Add(new EmbeddedResource(id, ManifestResourceAttributes.Public, binary))

                                                        let replacement =
                                                            [
                                                                match read.Value with
                                                                | Choice1Of4 read ->
                                                                    // unpickleResource
                                                                    mod_.Resources.Add(new EmbeddedResource(id, ManifestResourceAttributes.Public, binary))
                                                                    Instruction.Create(OpCodes.Ldstr, id)
                                                                    Instruction.Create(OpCodes.Ldstr, assdef.FullName)
                                                                    Instruction.Create(OpCodes.Call, read)

                                                                | Choice2Of4 read ->
                                                                    // unpickleInternal
                                                                    Instruction.Create(OpCodes.Ldstr, id)
                                                                    Instruction.Create(OpCodes.Ldstr, System.Convert.ToBase64String binary)
                                                                    Instruction.Create(OpCodes.Call, read)
                                                                    
                                                                | Choice3Of4 read ->
                                                                    // unpickleWithId
                                                                    Instruction.Create(OpCodes.Pop)
                                                                    Instruction.Create(OpCodes.Ldstr, id)
                                                                    Instruction.Create(OpCodes.Ldstr, System.Convert.ToBase64String binary)
                                                                    Instruction.Create(OpCodes.Call, read)
                                                            
                                                                | Choice4Of4 read -> 
                                                                    // read
                                                                    Instruction.Create(OpCodes.Pop)                                                          
                                                                    Instruction.Create(OpCodes.Ldstr, System.Convert.ToBase64String binary)
                                                                    Instruction.Create(OpCodes.Call, fromBase64.Value)
                                                                    Instruction.Create(OpCodes.Call, read)
                                                                    
                                                            ]

                                                        // insert replacement
                                                        let mutable pi = pushIndex
                                                        for inst in replacement do
                                                            body.Insert(pi, inst)
                                                            pi <- pi + 1
                                                            idx <- idx + 1

                                                        bodyChanged <- true
                                                        changed <- changed + 1

                                                    | None ->
                                                        Log.debug "effect-creation failed for: %s.%s" meth.DeclaringType.Name meth.Name
                                                    
                                                | _ ->
                                                    Log.debug "non-constant argument in: %s.%s" meth.DeclaringType.Name meth.Name
                                        
                                            | None ->
                                                Log.debug "found no argument push in: %s.%s" meth.DeclaringType.Name meth.Name
                                            
                                    | _ ->
                                        ()
                            
                                idx <- idx + 1

                            // fix _S jumps that now might be larger than +127/-128
                            if bodyChanged then
                                for i in 0..body.Count-1 do
                                    let op = body.[i].OpCode
                                    if op = OpCodes.Br_S then body[i] <- Instruction.Create(OpCodes.Br, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Beq_S then body[i] <- Instruction.Create(OpCodes.Beq, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Bge_S then body[i] <- Instruction.Create(OpCodes.Bge, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Bge_Un_S then body[i] <- Instruction.Create(OpCodes.Bge_Un, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Bgt_S then body[i] <- Instruction.Create(OpCodes.Bgt, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Bgt_Un_S then body[i] <- Instruction.Create(OpCodes.Bgt_Un, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Ble_S then body[i] <- Instruction.Create(OpCodes.Ble, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Ble_Un_S then body[i] <- Instruction.Create(OpCodes.Ble_Un, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Blt_S then body[i] <- Instruction.Create(OpCodes.Blt, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Blt_Un_S then body[i] <- Instruction.Create(OpCodes.Blt_Un, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Bne_Un_S then body[i] <- Instruction.Create(OpCodes.Bne_Un, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Brfalse_S then body[i] <- Instruction.Create(OpCodes.Brfalse, body[i].Operand :?> Instruction)
                                    elif op = OpCodes.Brtrue_S then body[i] <- Instruction.Create(OpCodes.Brtrue, body[i].Operand :?> Instruction)

                            ()
                        with _ ->
                            ()
                            
            if changed > 0 then
                Log.line "patched %d effect-creations" changed
                changedAssemblies.Add (path, assdef)
            else
                Log.line "no patchable effect-creations found"
            Log.stop()
            
        ctx.Unload()

        if changedAssemblies.Count > 0 then
            Log.start "saving assemblies"
            for (path, c) in changedAssemblies do
                let tmpFile = Path.ChangeExtension(Path.GetTempFileName(), ".dll")
                try
                    let rel =
                        let path = Path.GetFullPath path
                        let rel = path.Substring(tmp.Length)
                        if rel.Length > 0 && (rel.[0] = Path.DirectorySeparatorChar || rel.[0] = Path.AltDirectorySeparatorChar) then rel.Substring 1
                        else rel
                        
                    let dst = Path.Combine(dir, rel)
                    Log.line "%s -> %s" c.Name.Name rel
                    try
                        c.Write(tmpFile)
                        File.Copy(tmpFile, dst, true)
                    with e ->
                        Log.error "%A" e
                finally 
                    try File.Delete tmpFile
                    with _ -> ()
            
            Log.stop()
    finally
        try Directory.Delete(tmp, true)
        with _ -> ()
    0