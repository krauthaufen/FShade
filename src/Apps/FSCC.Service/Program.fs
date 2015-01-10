// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module Service =
    open System.Net
    open System.Threading
    open System.Threading.Tasks
    open System.IO
    open System.Text
    open System.Text.RegularExpressions
    open FSCC

    let listener = new HttpListener()

    let mutable workersRunning = 0

    let private bounaryRx = Regex "[\r\n]*---.*Boundary.*[\r\n]+Content-Disposition:[ \t]*(?<type>[^;]+);[ \t]*name=\"(?<name>.+)\"[\r\n]+"
    let private endRx = Regex "[\r\n]*---.*Boundary.*[\r\n]+"

    let parseFormData(msg : string) =
        let mutable m = bounaryRx.Match msg
        let mutable startIndex = -1
        let mutable result = Map.empty
        let mutable currentName = null

        while m.Success do
            if startIndex >= 0 then
                let value = msg.Substring(startIndex, m.Index - startIndex)
                result <- Map.add currentName value result

            currentName <- m.Groups.["name"].Value
            startIndex <- m.Index + m.Length

            m <- m.NextMatch()

        if startIndex >= 0 then
            let m = endRx.Match(msg, startIndex)
            if m.Success then
                let value = msg.Substring(startIndex, m.Index - startIndex)
                result <- Map.add currentName value result
  

        result


    let private compile (code : string) (composition : string) =
        printfn "initializing FSCC"
        FSCC.repl <- true
        let code = code.Replace("\t", "    ")

        let temp = Path.GetTempFileName() + ".fs"
        File.WriteAllText(temp, code)
        let shaderNames = composition.Split(' ')
                
        printfn "starting FSCC"
        let ms = new System.IO.MemoryStream()
        let o = new System.IO.StreamWriter(ms)
        //let savedOut = System.Console.Out
        //System.Console.SetOut(o)

        let c = { FSCC.Config.inputFiles = []; FSCC.Config.language = FSCC.Language.GLSL; FSCC.Config.output = FSCC.StreamOut o; FSCC.Config.shaderNames = shaderNames |> Array.toList }

        FSCC.compile c code composition

        o.Flush()
        ms.ToArray()

    open System
    let appDomains = Array.init 4 (fun i -> DateTime.Now,AppDomain.CreateDomain(sprintf "FSCC%d" i))
    let rand = System.Random()

    let runInRandomDomain (f : unit -> string) =
        let index = rand.Next(appDomains.Length)
        let (t,d) = appDomains.[index]
        let name = d.FriendlyName

        let age = DateTime.Now - t
        let d =
            if age.TotalMinutes > 30.0 then
                printfn "killed AppDomain: %s" name
                AppDomain.Unload(d)
                let d = AppDomain.CreateDomain(name)
                appDomains.[index] <- (DateTime.Now, d)
                d
            else
                d


        let mine = System.AppDomain.CurrentDomain
        d.DoCallBack(System.CrossAppDomainDelegate(fun () ->
            let str = f()
            mine.SetData(System.AppDomain.CurrentDomain.FriendlyName, str)
        ))
        let result = mine.GetData(name) |> unbox<string>
        result

    let worker(ctx : HttpListenerContext) =
        Task.Factory.StartNew(fun () ->
            try
                printfn "%d workers running" (Interlocked.Increment(&workersRunning))
                while true do 
                    use reader = new StreamReader(ctx.Request.InputStream, ctx.Request.ContentEncoding)
                    let msg = reader.ReadToEnd()

                    if msg.Length <> 0 then
                        let data = parseFormData msg

                        match Map.tryFind "code" data, Map.tryFind "comp" data with
                            | Some code, Some comp ->
                                printfn "starting compiler"
                                let b = runInRandomDomain (fun () -> compile code comp |> System.Text.ASCIIEncoding.Default.GetString) |> System.Text.ASCIIEncoding.Default.GetBytes

                                printfn "reply"
                                ctx.Response.StatusCode <- 200
                                ctx.Response.ContentLength64 <- int64 b.Length
                                ctx.Response.OutputStream.Write(b, 0, b.Length)
                                ctx.Response.OutputStream.Close()
                            | _ ->
                                raise <| System.OperationCanceledException()
                    else
                        raise <| System.OperationCanceledException()
            with 
                | :? System.OperationCanceledException as e -> 
                    Interlocked.Decrement(&workersRunning) |> ignore
                | e ->
                    //printfn "ERROR: %A" e
                    Interlocked.Decrement(&workersRunning) |> ignore

        ) |> ignore

    let run() =
        listener.Prefixes.Add("http://*:8080/")
        
        listener.Start()
        while true do
            let ctx = listener.GetContext()
           
            ctx.Response.AddHeader("Access-Control-Allow-Origin", "*")
            worker(ctx)

    

[<EntryPoint>]
let main argv = 
    
    Service.run()
    printfn "%A" argv
    0 // return an integer exit code
