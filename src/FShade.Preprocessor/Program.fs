open System
open System.IO
open System.Reflection
open System.Runtime.Loader
open Microsoft.FSharp.Quotations
open Mono.Cecil
open Mono.Cecil.Cil

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

    let doubleCheck =
        argv |> Array.exists (function "-d" | "--double-check" -> true | _ -> false)

    let singleAssembly =
        argv |> Array.exists (function "-s" | "--single" -> true | _ -> false)

    Log.verbose <-
        argv |> Array.exists (function "-v" | "--verbose" -> true | _ -> false)

    match entry with
    | None ->
        Log.error "usage: fshadeaot <entrydllpath> [--verbose] [--double-check] [--single]"
        Log.error "  --single   only patch the entry assembly; do not walk references"
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

        if doubleCheck then
            DoubleChecker.run entry
        else
            // ============================================================
            // Marker-based AOT (universal-rewrite, no constant folding).
            // Replaces the legacy try-to-constant-fold-call-sites approach.
            // ============================================================
            Log.start "scanning for shader functions"

            // Load FShade.Core to get Aot helpers at build time.
            // We can't cast across ALC boundaries, so we go through obj+reflection.
            let fshadeCore = ctx.LoadFromAssemblyName(AssemblyName "FShade.Core")
            let aotType = fshadeCore.GetType("FShade.Aot")
            let mNormalizeAndHash = aotType.GetMethod("normalizeAndHash")
            let mPrecomputeShader = aotType.GetMethod("precomputeShader")
            let normalizeAndHash (carriers : (string * Type)[]) (inputType : Type) (rawExpr : obj) =
                mNormalizeAndHash.Invoke(null, [| box carriers; box inputType; rawExpr |]) :?> string
            // returns (id, byte[]) — the precomputed Effect ready to embed as resource.
            let precomputeShader (inputType : Type) (rawExpr : obj) : string * byte[] =
                let result = mPrecomputeShader.Invoke(null, [| box inputType; rawExpr |])
                let t = result.GetType()
                let id = t.GetProperty("Item1").GetValue(result) :?> string
                let bin = t.GetProperty("Item2").GetValue(result) :?> byte[]
                id, bin

            let readerParams = Cecil.readerParams config.Dirs

            let allAssDefs =
                let entryDef = Cecil.read config.Entry (Some readerParams)
                let isPatchable (ad : AssemblyDefinition) =
                    ad.Name.Name <> "FShade.Core"
                    && ad.Name.Name <> "FShade.GLSL"
                    && ad.Name.Name <> "FShade.SpirV"
                    && ad.Name.Name <> "FShade.Imperative"
                    && ad.Modules |> Seq.exists (fun m -> m.AssemblyReferences |> Seq.exists (fun r -> r.Name = "FShade.Core"))
                if singleAssembly then
                    // --single: just the entry assembly. Useful as a postbuild step where
                    // each project's deploy already gets fshadeaot run on it.
                    if isPatchable entryDef then [| (entryDef, config.Entry) |]
                    else
                        Log.warn "%s does not reference FShade.Core; nothing to patch" entryDef.Name.Name
                        [||]
                else
                    // Walk transitive references that mention FShade.Core.
                    let state = System.Collections.Generic.Dictionary<string, AssemblyDefinition * string>()
                    let entryName = AssemblyNameReference(entryDef.Name.Name, entryDef.Name.Version).ToString()
                    state.[entryName] <- (entryDef, config.Entry)
                    let rec load (name : AssemblyNameReference) =
                        let key = name.FullName
                        if not (state.ContainsKey key) then
                            match Cecil.resolveAssembly config.Dirs (Some readerParams) name with
                            | Some (path, ad) ->
                                state.[key] <- (ad, path)
                                for m in ad.Modules do
                                    for r in m.AssemblyReferences do load r
                            | None -> ()
                    for m in entryDef.Modules do
                        for r in m.AssemblyReferences do load r
                    state.Values
                    |> Seq.toArray
                    |> Array.filter (fun (ad, _) -> isPatchable ad)
                    // Dedup by short assembly name. Some assemblies are reachable via multiple
                    // version-qualified names (entry's own name + same name via references).
                    |> Array.distinctBy (fun (ad, _) -> ad.Name.Name)

            let changed = System.Collections.Generic.List<string * AssemblyDefinition>()

            for (assdef, path) in allAssDefs do
                Log.start "%s" assdef.Name.Name

                // Resolve refs once per module.
                let mutable patchedHere = 0
                let cecilResolver : AotRewrite.Rewrite.Resolver =
                    fun name ->
                        match Cecil.resolveAssembly config.Dirs (Some readerParams) (AssemblyNameReference(name, Version(0,0,0,0))) with
                        | Some (_, ad) -> ad
                        | None -> null
                for mod_ in assdef.Modules do
                    let refs =
                        try Some (AotRewrite.Rewrite.resolveRefs cecilResolver mod_)
                        with e ->
                            Log.warn "could not resolve FShade.Aot refs in %s: %s" mod_.Name e.Message
                            None
                    match refs with
                    | None -> ()
                    | Some refs ->
                        // mod_.GetTypes() already enumerates nested types — don't recurse.
                        let allTypes = mod_.GetTypes() |> Seq.toArray

                        let allMethods =
                            allTypes
                            |> Array.collect (fun t -> t.Methods |> Seq.toArray)
                        Log.debug "  scanning %d methods across %d types" allMethods.Length allTypes.Length
                        let candidates =
                            allMethods
                            |> Array.filter AotRewrite.Detect.isShaderFunction
                            |> Array.filter AotRewrite.Rewrite.canRewrite
                        Log.debug "  %d shader functions to patch" candidates.Length

                        // For each candidate, reflectively invoke + hash + rewrite.
                        // Load runtime assembly + find MethodInfo to invoke.
                        let runtimeAss =
                            try Some (ctx.LoadFromAssemblyPath path)
                            with e ->
                                Log.warn "load failed for %s: %s" path e.Message
                                None
                        match runtimeAss with
                        | None -> ()
                        | Some runtimeAss ->
                            for md in candidates do
                                try
                                    let typeFullName = md.DeclaringType.FullName.Replace('/', '+')
                                    let runtimeType = runtimeAss.GetType(typeFullName)
                                    if isNull runtimeType then
                                        Log.debug "skip %s.%s (runtime type not found)" md.DeclaringType.FullName md.Name
                                    else
                                        // Resolve each formal parameter type to its runtime System.Type by
                                        // walking the (already loaded) referenced assemblies. We can't rely
                                        // on Cecil's Resolve() telling us a runtime assembly that's loadable
                                        // (e.g. System.Runtime → System.Private.CoreLib at runtime).
                                        let rec resolveParam (pt : Mono.Cecil.TypeReference) : Type =
                                            match pt with
                                            | :? Mono.Cecil.GenericInstanceType as gt ->
                                                let openT = resolveParam gt.ElementType
                                                if isNull openT then null
                                                else
                                                    let args = gt.GenericArguments |> Seq.map resolveParam |> Seq.toArray
                                                    if args |> Array.exists isNull then null
                                                    else
                                                        try openT.MakeGenericType(args)
                                                        with _ -> null
                                            | _ ->
                                                // For open generics, FullName ends with `1, `2 etc — that's fine,
                                                // BCL's Assembly.GetType accepts it. Replace nested-class '/' with '+'.
                                                let nm = pt.FullName.Replace('/', '+')
                                                let mutable found : Type = null
                                                for ass in ctx.Assemblies do
                                                    if isNull found then
                                                        let t = ass.GetType(nm)
                                                        if not (isNull t) then found <- t
                                                if isNull found then
                                                    try
                                                        let r = pt.Resolve()
                                                        if not (isNull r) then
                                                            let asmName = r.Module.Assembly.Name.Name
                                                            try
                                                                let a = ctx.LoadFromAssemblyName(AssemblyName asmName)
                                                                if not (isNull a) then
                                                                    let t = a.GetType(nm)
                                                                    if not (isNull t) then found <- t
                                                            with _ -> ()
                                                    with _ -> ()
                                                if isNull found then
                                                    try found <- Type.GetType(nm)
                                                    with _ -> ()
                                                found
                                        let paramTypes = md.Parameters |> Seq.map (fun p -> resolveParam p.ParameterType) |> Seq.toArray
                                        let badIdx = paramTypes |> Array.tryFindIndex isNull
                                        match badIdx with
                                        | Some i ->
                                            Log.debug "skip %s.%s (param[%d] type %s not resolvable)" md.DeclaringType.FullName md.Name i md.Parameters.[i].ParameterType.FullName
                                        | None ->
                                            let bindingFlags =
                                                let access = BindingFlags.Public ||| BindingFlags.NonPublic
                                                if md.IsStatic then access ||| BindingFlags.Static
                                                else access ||| BindingFlags.Instance
                                            let mi =
                                                runtimeType.GetMethod(
                                                    md.Name, bindingFlags, null, paramTypes, null)
                                            if isNull mi then
                                                Log.debug "skip %s.%s (MethodInfo not found)" md.DeclaringType.FullName md.Name
                                            else
                                                let inputType = paramTypes.[paramTypes.Length - 1]
                                                let model = AotRewrite.Rewrite.carrierModel md
                                                let totalRuntimeArgs = model.ClosureFields.Length + (paramTypes.Length - 1)
                                                // Resolve closure-field types via the same resolver used for params.
                                                let closureFieldsRuntime =
                                                    model.ClosureFields
                                                    |> Array.map (fun f -> f.Name, resolveParam f.FieldType)
                                                let closureFieldFails =
                                                    closureFieldsRuntime |> Array.tryFindIndex (fun (_, t) -> isNull t)
                                                match closureFieldFails with
                                                | Some i ->
                                                    Log.debug "skip %s.%s (closure field[%d] %s not resolvable)" md.DeclaringType.FullName md.Name i model.ClosureFields.[i].Name
                                                | None ->
                                                    if md.IsStatic && totalRuntimeArgs = 0 then
                                                        // STATIC + ZERO-arg shader: precompute the entire Effect.
                                                        try
                                                            let placeholderInput = AotRewrite.Hashing.placeholder inputType
                                                            let rawExpr = mi.Invoke(null, [| placeholderInput |])
                                                            if isNull rawExpr then
                                                                Log.debug "skip %s.%s: returned null" md.DeclaringType.FullName md.Name
                                                            else
                                                                let id, binary = precomputeShader inputType rawExpr
                                                                AotRewrite.Rewrite.rewriteShaderFunctionPrecomputed refs md id id binary
                                                                Log.line "patched %s.%s : %s (precomputed, %d bytes)" md.DeclaringType.FullName md.Name id binary.Length
                                                                patchedHere <- patchedHere + 1
                                                        with e ->
                                                            let inner =
                                                                match e with
                                                                | :? TargetInvocationException as t when not (isNull t.InnerException) -> t.InnerException
                                                                | _ -> e
                                                            Log.warn "precompute of %s.%s failed: %s: %s" md.DeclaringType.FullName md.Name (inner.GetType().Name) inner.Message
                                                    else
                                                        // Deferred marker. Carriers = closure fields + formal params.
                                                        match AotRewrite.Hashing.tryHashShaderFunction normalizeAndHash mi closureFieldsRuntime inputType with
                                                        | Result.Error e ->
                                                            Log.debug "skip %s.%s: %s" md.DeclaringType.FullName md.Name e
                                                        | Result.Ok (_, bodyHash) ->
                                                            try
                                                                AotRewrite.Rewrite.rewriteShaderFunction cecilResolver refs md bodyHash
                                                                let kind = if md.IsStatic then "deferred" else "deferred-closure"
                                                                Log.line "patched %s.%s : %s (%s)" md.DeclaringType.FullName md.Name bodyHash kind
                                                                patchedHere <- patchedHere + 1
                                                            with e ->
                                                                Log.warn "IL rewrite of %s.%s failed: %s" md.DeclaringType.FullName md.Name e.Message
                                                                Log.warn "  %s" (if isNull e.StackTrace then "<no stack>" else (e.StackTrace.Split('\n') |> Array.head))
                                with e ->
                                    Log.warn "outer failure on %s.%s: %s" md.DeclaringType.FullName md.Name e.Message

                if patchedHere > 0 then
                    Log.line "patched %d shader functions" patchedHere
                    changed.Add (path, assdef)
                else
                    Log.line "no shader functions"
                Log.stop()

            ctx.Unload()

            if changed.Count > 0 then
                Log.start "saving assemblies"
                for (path, c) in changed do
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
                        try File.Delete tmpFile with _ -> ()
                Log.stop()
            Log.stop()
    finally
        try Directory.Delete(tmp, true)
        with _ -> ()
    0
