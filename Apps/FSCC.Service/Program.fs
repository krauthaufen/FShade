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
        let savedOut = System.Console.Out
        System.Console.SetOut(o)
        [[|temp; "-c"; |]; shaderNames] |> Array.concat |> FSCC.run
        System.Console.SetOut(savedOut)

        o.Flush()
        ms.ToArray()

    let worker(ctx : HttpListenerContext) =
        Task.Factory.StartNew(fun () ->
            try
                printfn "%d workers running" (Interlocked.Increment(&workersRunning))
                while true do 
                    use reader = new StreamReader(ctx.Request.InputStream, ctx.Request.ContentEncoding)
                    let str = ctx.Request.QueryString
                    let msg = reader.ReadToEnd()

                    if msg.Length <> 0 then
                        let data = parseFormData msg

                        match Map.tryFind "code" data, Map.tryFind "comp" data with
                            | Some code, Some comp ->
                                printfn "starting compiler"
                                let b = compile code comp

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
                    printfn "ERROR: %A" e
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
