open System
open System.IO
open System.Diagnostics
open System.Reflection
open Microsoft.FSharp.Quotations
open Aardvark.Base
open Preprocessor


let hook (dirs : list<string>) =


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
            
    Assembly.LoadFile (Path.Combine(List.head dirs, "FShade.Core.dll"))
        
[<EntryPoint>]
let main argv =


    match Command.tryParse (Array.toList argv) with
    | Some cmd ->
        match cmd with
        | FindEffects assemblies ->
            let dirs = assemblies |> List.map Path.GetDirectoryName
            let fshade = hook dirs
            use ms = new MemoryStream()
            use w = new BinaryWriter(ms, System.Text.Encoding.UTF8, true)
            let tEffect = fshade.GetType("FShade.Effect")
            let tEffectModule = fshade.GetType("FShade.EffectModule")


            let effects = assemblies |> List.collect (Search.findEffects fshade)

            for (meth, effect) in effects do
                w.Write meth.DeclaringType.Assembly.Location
                w.Write meth.DeclaringType.MetadataToken
                w.Write meth.MetadataToken
                w.Write 0L 
                let p = ms.Position
                tEffectModule.GetMethod("serialize", BindingFlags.Public ||| BindingFlags.Static).Invoke(null, [|ms; effect|]) |> ignore
                //Effect.serialize ms effect
                let l = ms.Position - p

                let o = ms.Position
                ms.Position <- p - int64 sizeof<int64>
                w.Write l
                ms.Position <- o




            let data = ms.ToArray() |> Convert.ToBase64String
            eprintfn "%s" data
            0
        | ProcessAssemblies assemblies ->
            let dirs = assemblies |> List.map Path.GetDirectoryName
            let fshade = hook dirs

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

                    Fix.inlineEffectData fshade (Seq.toList data)
                | None ->
                    printfn "failed"


            0


    | None ->
        -1