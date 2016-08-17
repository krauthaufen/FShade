namespace FSCC

open System
open Newtonsoft.Json
open FShade


module CLI =
    type Range = ZeroToInf
               | MinToInf of int
               | ZeroToMax of int
               | Range of int * int with

        member x.Max =
            match x with
                | ZeroToInf -> System.Int32.MaxValue
                | MinToInf min -> System.Int32.MaxValue
                | ZeroToMax max -> max
                | Range(min, max) -> max
        member x.Min =
            match x with
                | ZeroToInf -> 0
                | MinToInf min -> min
                | ZeroToMax max -> 0
                | Range(min, max) -> min

    let inline range (start : int) (e : int) =
        Range(start,e)

    let inline atmost (e : int) =
        ZeroToMax e

    let inline atleast (e : int) =
        MinToInf e

    let inline exactly (e : int) =
        Range(e,e)

    let one = exactly 1
    let optional = atmost 1
    let any = ZeroToInf


    type ICLArg =
        abstract member options : list<string>
        abstract member description : string
        abstract member count : Range
        abstract member argumentType : Type
        abstract member name : string

    type CLArg<'a> = { options : list<string>; description : string; count : Range; name : Option<string> } with
        interface ICLArg with
            member x.options = x.options
            member x.description = x.description
            member x.count = x.count
            member x.argumentType = typeof<'a>

            member x.name =
                match x.name with
                    | Some name -> name
                    | None -> 
                        match x.options with
                            | x::_ -> x
                            | _ -> ""

    type CLFlag = { flagNames : list<string>; flagDescription : string; flagName : Option<string> } with
        interface ICLArg with
            member x.options = x.flagNames
            member x.description = x.flagDescription
            member x.count = Range(0,0)
            member x.argumentType = typeof<unit>
            member x.name = 
                match x.flagName with
                    | Some name -> name
                    | None -> x.flagNames |> List.head

    type CLArgBuilder<'a>() =
        member x.Yield(_) : CLArg<'a> = { options = []; description = ""; count = any; name = None }


        [<CustomOperation("name")>]
        member x.Name(a : CLArg<'a>, name : string) =
            { a with name = Some name }

        [<CustomOperation("option")>]
        member x.Option(a : CLArg<'a>, options : string) =
            let options = options.Split('|') |> Array.toList
            { a with options = [options; a.options] |> List.concat }

        [<CustomOperation("atleast")>]
        member x.AtLeast(a : CLArg<'a>, count : int) =
            let newRange =
                match a.count with
                    | ZeroToInf -> MinToInf count
                    | MinToInf _ -> MinToInf count
                    | ZeroToMax max -> Range(count, max)
                    | Range(_,max) -> Range(count, max)

            { a with count = newRange}

        [<CustomOperation("atmost")>]
        member x.AtMost(a : CLArg<'a>, count : int) =
            let newRange =
                match a.count with
                    | ZeroToInf -> ZeroToMax count
                    | MinToInf min -> Range(min, count)
                    | ZeroToMax _ -> ZeroToMax count
                    | Range(min,_) -> Range(min, count)

            { a with count = newRange}

        [<CustomOperation("range")>]
        member x.Range(a : CLArg<'a>, min : int, max : int) =
            { a with count = Range(min, max) }

        [<CustomOperation("single")>]
        member x.One(a : CLArg<'a>) =
            { a with count = Range(1, 1) }

        [<CustomOperation("optional")>]
        member x.Optional(a : CLArg<'a>) =
            { a with count = Range(0, 1) }

        [<CustomOperation("description")>]
        member x.Description(a : CLArg<'a>, desc : string) =
            { a with description = desc }

        member x.Run(a) = a :> ICLArg

    type CLFlagBuilder() =
        member x.Yield(_) : CLFlag = { flagNames = []; flagDescription = ""; flagName = None }

        [<CustomOperation("name")>]
        member x.Name(a : CLFlag, name : string) =
            { a with flagName = Some name }

        [<CustomOperation("switch")>]
        member x.Switch(a : CLFlag, options : string) =
            let options = options.Split('|') |> Array.toList
            { a with flagNames = [options; a.flagNames] |> List.concat }

        [<CustomOperation("description")>]
        member x.Description(a : CLFlag, desc : string) =
            { a with flagDescription = desc }

        member x.Run(a) = a :> ICLArg

    let arg<'a> = CLArgBuilder<'a>()
    let flag = CLFlagBuilder()


    open System.Collections.Generic

    let compileParser (args : list<ICLArg>) =
        let map = args |> List.collect (fun a -> 
            let opts =
                if List.isEmpty a.options then
                    [""]
                else
                    a.options 
            opts |> List.map (fun o -> o,a)) |> Map.ofList

        let needed = args |> List.filter (fun a -> a.count.Min > 0)
        let checkResult (r : Map<string, list<obj>>) =
            args |> List.forall (fun a ->
                if a.argumentType = typeof<unit> then
                    match Map.tryFind a.name r with
                        | Some res -> res.Length = 1
                        | _ -> true
                else
                    match Map.tryFind a.name r with
                        | Some res ->
                            let l = res.Length
                            l >= a.count.Min && l <= a.count.Max
                        | None ->
                            a.count.Min = 0
            )

        fun (tokens : #seq<string>) ->
            let current = ref <| Map.tryFind "" map
            let currentCount = ref 0
            let result = ref Map.empty

            let add (value : obj) =
                let name = 
                    match !current with
                        | Some c -> c.name
                        | None -> ""
                match Map.tryFind name !result with
                    | Some l -> result := Map.add name ([l; [value]] |> List.concat) !result
                    | None -> result := Map.add name [value] !result

            for t in tokens do
                
                match Map.tryFind t map with
                    | Some n ->
                        
                        if n.argumentType = typeof<unit> then
                            let o = !current
                            current := Some n
                            add ()
                            current := o
                        else
                            current := Some n
                            currentCount := 0
                    | None ->
                        if t.StartsWith "-" then
                            printfn "WARNING: ignored: %A" t
                        else
                            match !current with
                                | Some cur ->
                                    let count = !currentCount
                                    if count + 1 <= cur.count.Max then
                                        currentCount := count + 1
                                        add t
                                    else
                                        current := Map.tryFind "" map
                                        currentCount := 0
                                        add t
                                | None ->
                                    printfn "WARNING: ignored: %A" t
            
            if checkResult !result then
                Some !result
            else
                None

    let compileUsage (toolName : string) (args : list<ICLArg>) =
        
        let options = args |> List.filter (fun a -> a.argumentType = typeof<unit>)
        let args = args |> List.filter (fun a -> a.argumentType <> typeof<unit>)

        let single = args |> List.filter (fun a -> a.count.Min = 1 && a.count.Max = 1)
        let optional = args |> List.filter (fun a -> a.count.Min = 0 && a.count.Max = 1) 
        let lists = args |> List.filter (fun a -> a.count.Max > 1) 

        

        let single = single |> List.map (fun a -> 
                                    if List.isEmpty a.options then
                                        sprintf "<%s>" a.name
                                    else
                                        sprintf "%s %s" (a.options |> List.head) a.name
                               ) 
                            |> String.concat " "

        let optional = optional |> List.map (fun a -> sprintf "[%s %s]" (a.options |> List.head) a.name) |> String.concat " "
        let lists = lists |> List.map (fun a -> 
                                        if a.options |> List.isEmpty then
                                            sprintf "[%s,...]" a.name
                                        else
                                            sprintf "[%s [%s,...]]" (a.options |> List.head) a.name
                             )
                          |> String.concat " "
        let options = options |> List.map (fun a -> a.options |> List.head) |> List.map (fun str -> str.Replace("-", "")) |> String.concat "" |> sprintf "[-%s]"

        let rec concat (l : list<string>) =
            match l with
                | x::xs ->
                    if x.Length > 0 then x + " " + (concat xs)
                    else concat xs
                | _ -> ""

        let args = [options; single; optional; lists] |> concat

        sprintf "%s %s" toolName args 
 
module FSC =
    open System
    open System.IO
    let private folder = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)

    type OSType = Windows
                | MacOSX
                | Linux

    let (|Windows|MacOSX|Linux|) (v : OperatingSystem) =
        match v.Platform with
            | PlatformID.Win32NT | PlatformID.Win32S | PlatformID.Win32Windows | PlatformID.WinCE | PlatformID.Xbox -> Windows
            | PlatformID.MacOSX -> MacOSX
            | PlatformID.Unix -> Linux
            | _ -> failwith "impossible"

    let mutable private initialized = false
    let private initSession() =

        if not initialized then
            initialized <- true

            let refAsmDir, fscDir =
                match Environment.OSVersion with
                    | Windows -> @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\3.0\Runtime\v4.0",
                                 @"C:\Program Files (x86)\Microsoft SDKs\F#\3.0\Framework\v4.0"
                    | Linux -> "/usr/lib/mono/4.0"," /usr/lib/mono/4.0 " 
                    | MacOSX -> failwith "notimp"

            let compilerFiles = [ "FSharp.Build.dll"; "FSharp.Compiler.dll"; "FsiAnyCPU.exe"; "FsiAnyCPU.exe.config"; "Fsc.exe"; "FSharp.Compiler.Interactive.Settings.dll" ]
            let refFiles = ["FSharp.Core.optdata"; "FSharp.Core.sigdata"; "policy.2.3.FSharp.Core.dll"; "pub.config"; "Type Providers\\FSharp.Data.TypeProviders.dll"]
        
            let copyFile (source : string) (target : string) =
                let target = Path.Combine(folder, target)

                if not <| File.Exists target then
                    let d = Path.GetDirectoryName target
                    if d <> "" && not <| Directory.Exists d then
                        Directory.CreateDirectory d |> ignore

                    File.Copy(source, target, true)

            compilerFiles |> List.iter (fun f ->
                let p = Path.Combine(fscDir, f)
                if File.Exists p then
                    copyFile p f
            )

            refFiles |> List.iter (fun f ->
                let p = Path.Combine(refAsmDir, f)
                if File.Exists p then
                    copyFile p f
            )

    let private fsc = Microsoft.FSharp.Compiler.SimpleSourceCodeServices.SimpleSourceCodeServices()

    let private references = [| "Aardvark.Base.TypeProviders";"System.Drawing"; "Aardvark.Preliminary"; "Aardvark.Base"; "Aardvark.Base.FSharp"; "FShade.Compiler"; "FShade"; "System"; "System.Core" |]
    let private assemblyLocations = references |> Array.choose(fun n -> try System.Reflection.Assembly.Load(n).Location |> Some with e -> None)
    
    
    let private r = assemblyLocations |> Array.collect (fun r -> [|"-r"; r |])
    let compile (file : string) =

        initSession()
        
        let tempFile = System.IO.Path.GetTempFileName() + ".dll"
        let args = [[|"-o"; tempFile; "-a"; file;|]; r] |> Array.concat
        //let cmd = sprintf "fsc.exe %s" ([[|"-o"; tempFile; "-a"; file;|]; assemblyLocations |> Array.collect (fun r -> [|"-r"; sprintf "\"%s\"" r |])] |> Array.concat |> String.concat " ")
        //printfn "%A" cmd
        match fsc.CompileToDynamicAssembly(args, None) with
            | (_,_,Some ass) -> Some ass
            | (err,c,_) -> 
                printfn "%A" err
                None      

module FSCC =
    open CLI
    open FShade.Compiler
    open FShade
    open Aardvark.Base

    let mutable repl = false

    let private args =
        [ arg<string> {
            atleast 1
            name "inputFiles"
            description "input files for the compiler (can be any kind of .NET-library)"
          }

          arg<string> {
            optional
            option "-o|--output"
            name "outputFile"
            description "the output file for the compiler (the result will be printed on stdout when no output-file is given)"
          }  

          arg<string> {
            optional
            option "-l|--lang"
            name "language"
            description "specifies the target language to be generated by the compiler"
          }

          arg<string> {
            atleast 1
            option "-c|--compose"
            name "shaders"
            description "the (fully qualified) names of the shaders to be composed"
          }

        ]    

    let private parseArgs = args |> compileParser
    let private usageString = args |> compileUsage "FSCC"
    let private usage() : 'a =
        if repl then
            failwith ""
        else
            printfn "usage: %s" usageString
            System.Environment.Exit(1)
            failwith "impossible"

    type Output = File of string | StreamOut of System.IO.TextWriter | StdOut with
        member x.Write(text : string) =
            match x with
                | File f -> System.IO.File.WriteAllText(f, text)
                | StreamOut s -> s.WriteLine(text)
                | StdOut -> System.Console.WriteLine(text)

    type Language = GLSL


    type Config = { inputFiles : list<string>; output : Output; shaderNames : list<string>; language : Language}

    let parseConfig (args : string[]) =
        match parseArgs args with
            | Some args ->
                let inputs =
                    match Map.tryFind "inputFiles" args with 
                        | Some files -> files |> List.map (unbox<string>)
                        | None -> usage() 

                let output =
                    match Map.tryFind "outputFile" args with 
                        | Some [:? string as file] -> file |> File
                        | _ -> StdOut

                let lang =
                    match Map.tryFind "language" args with
                        | Some [:? string as lang] ->
                            match lang.ToLower() with
                                | "glsl" -> GLSL
                                | _ -> usage()
                        | _ -> GLSL

                let composed =
                    match Map.tryFind "shaders" args with
                        | Some shaders -> shaders |> List.map (unbox<string>)
                        | None -> usage()

                { inputFiles = inputs; output = output; shaderNames = composed; language = lang}

            | None -> 
                usage()

    let run(args : string[]) =
        let c = parseConfig args

        let types = c.inputFiles |> List.collect (fun p ->
                            let ext = System.IO.Path.GetExtension(p)
                            if ext = ".dll" || ext = ".exe" then
                                let ass = System.Reflection.Assembly.LoadFile p
                                try
                                    ass.GetTypes() |> Array.map (fun t -> Success t) |> Array.toList
                                with e ->
                                    []
                            elif ext = ".fs" then
                                let code = System.IO.File.ReadAllText p
                                match Fsi.compileModule code with
                                    | Fsi.FsiSuccess m -> [Success m]
                                    | Fsi.FsiError e -> [Error e.AsString]
                            else 
                                []
                    ) |> List.toArray
  
        let rec flattenNesting (t : Type) =
            let n = t.GetNestedTypes() |> Array.collect flattenNesting
            [[|t|]; n] |> Array.concat

        let allSuccess = types |> Array.forall (fun t -> match t with | Success _ -> true | _ -> false)

        if allSuccess then
            
            let allTypes = types |> Array.map (fun t -> match t with | Success t -> t | _ -> failwith "impossible")
            let allTypes = allTypes |> Array.collect flattenNesting
       

            let toEffectMethod = FShade.Compiler.ReflectionExtensions.getMethodInfo <@ toEffect @>
            let buildFunctionMethod = FShade.Compiler.ReflectionExtensions.getMethodInfo <@ Aardvark.Base.FunctionReflection.FunctionBuilder.BuildFunction<int, int>(null, null) @>

            let allShaders = c.shaderNames |> List.map (fun name ->
                allTypes |> Array.tryPick (fun t ->
                    let mi = t.GetMethod name

                    if mi <> null then
                        let p = mi.GetParameters().[0]
                        let toEffect = toEffectMethod.MakeGenericMethod [|p.ParameterType; mi.ReturnType.GetGenericArguments().[0]|]
                        let build = buildFunctionMethod.MakeGenericMethod [|p.ParameterType; mi.ReturnType|]

                        let f = build.Invoke(null, [|null; mi|])

                        let effect = toEffect.Invoke(null, [|f|]) |> unbox<Compiled<Effect, ShaderState>>

                        Some effect
                    else 
                        None
                )
            )

            if allShaders |> List.forall (fun o -> o.IsSome) then
                let allShaders = allShaders |> List.map (fun o -> o.Value)

                let shader = compose allShaders
                match GLES.compileEffect shader with
                    | Aardvark.Base.Prelude.Success(uniforms, code) ->

                        let samplerStates = uniforms |> Map.toList |> List.filter (fun (s,v) -> v.IsSamplerUniform) |> List.map (fun (s,v) ->
                                                let sem, sam = v.Value |> unbox<string * SamplerState>

                                                let code = GlslSamplers.compileSamplerState s sam

                                                let code = FShade.Utils.String.linePrefix "//" code

                                                sprintf "//string %s = \"%s\"\r\n%s" s sem code
                                            ) |> String.concat "\r\n"

                        c.output.Write(sprintf "%s\r\n//SAMPLERSTATES\r\n%s" code samplerStates)
                    | Aardvark.Base.Prelude.Error e ->
                        c.output.Write("ERROR: " + e)
            else
                ()
        else
            let err = types |> Array.choose (fun e -> match e with | Error e -> Some e | _ -> None) |> String.concat "\r\n"
            c.output.Write("ERROR: " + err)

    type CompilerResult = { code : string; errors : list<Fsi.CompilerError> }

    let compile (config : Config) (code : string) (comp : string) =
        let rec flattenNesting (t : Type) =
            let n = t.GetNestedTypes() |> Array.collect flattenNesting
            [[|t|]; n] |> Array.concat

        match Fsi.compileModule code with
            | Fsi.FsiSuccess m ->
                let allTypes = [|m|] |> Array.collect flattenNesting
       

                let toEffectMethod = FShade.Compiler.ReflectionExtensions.getMethodInfo <@ toEffect @>
                let buildFunctionMethod = FShade.Compiler.ReflectionExtensions.getMethodInfo <@ Aardvark.Base.FunctionReflection.FunctionBuilder.BuildFunction<int, int>(null, null) @>

                let allShaders = config.shaderNames |> List.map (fun name ->
                    allTypes |> Array.tryPick (fun t ->
                        let mi = t.GetMethod name

                        if mi <> null then
                            let p = mi.GetParameters().[0]
                            let toEffect = toEffectMethod.MakeGenericMethod [|p.ParameterType; mi.ReturnType.GetGenericArguments().[0]|]
                            let build = buildFunctionMethod.MakeGenericMethod [|p.ParameterType; mi.ReturnType|]

                            let f = build.Invoke(null, [|null; mi|])

                            let effect = toEffect.Invoke(null, [|f|]) |> unbox<Compiled<Effect, ShaderState>>

                            Some effect
                        else 
                            None
                    )
                )

                if allShaders |> List.forall (fun o -> o.IsSome) then
                    let allShaders = allShaders |> List.map (fun o -> o.Value)

                    let shader = compose allShaders
                    match GLES.compileEffect shader with
                        | Aardvark.Base.Prelude.Success(uniforms, code) ->

                            let samplerStates = uniforms |> Map.toList |> List.filter (fun (s,v) -> v.IsSamplerUniform) |> List.map (fun (s,v) ->
                                                    let sem, sam = v.Value |> unbox<string * SamplerState>

                                                    let code = GlslSamplers.compileSamplerState s sam

                                                    let code = FShade.Utils.String.linePrefix "//" code

                                                    sprintf "//string %s = \"%s\"\r\n%s" s sem code
                                                ) |> String.concat "\r\n"

                            let res = { code = sprintf "%s\r\n//SAMPLERSTATES\r\n%s" code samplerStates; errors = [] }

                            config.output.Write(JsonConvert.SerializeObject(res))
                        | Aardvark.Base.Prelude.Error e ->
                            let e = { Fsi.CompilerError.file = ""; Fsi.CompilerError.line = 0; Fsi.CompilerError.col = 0; Fsi.CompilerError.code = ""; Fsi.CompilerError.errorType = Fsi.CompilerErrorType.Error; Fsi.CompilerError.message = e }
                            config.output.Write(JsonConvert.SerializeObject({ code = null; errors = [e] }))
                else
                    ()
                ()
            | Fsi.FsiError e -> 
                let res = { code = null; errors = e.errors }
                config.output.Write(JsonConvert.SerializeObject(res))
