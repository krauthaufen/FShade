// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open FSCC

[<EntryPoint>]
let main argv = 

    Aardvark.Base.Fsi.execute "let a = 10" |> ignore
    let code = "a.E"
    let set = Aardvark.Base.Fsi.getCompletions code 1 2 "E"
    set.Items |> Array.iter(fun a -> printfn "%A" a.DescriptionText)
    System.Environment.Exit(0)


    if argv.Length = 0 then
        FSCC.repl <- true

        let tokenize (str : string) =
            [|
                let tokenStart = ref -1
                let inString = ref false
                for i in 0..str.Length-1 do
                    if str.[i] = '"' then
                        if !inString then
                            yield str.Substring(!tokenStart, i - !tokenStart)
                            inString := false
                            tokenStart := i + 1
                        else
                            tokenStart := i + 1
                            inString := true
                    elif str.[i] = ' ' && not !inString then
                        if !tokenStart >= 0 && !tokenStart < i then
                            let value = str.Substring(!tokenStart, i - !tokenStart).Trim()
                            if value.Length > 0 then yield value

                        tokenStart := i
                if !tokenStart >= 0 && !tokenStart < str.Length then
                    let value = str.Substring(!tokenStart, str.Length - !tokenStart).Trim()
                    if value.Length > 0 then yield value
            |]

        while true do
            let args = System.Console.ReadLine() |> tokenize
            try
                FSCC.run args
            with e ->
                eprintfn "Invalid args"

    else
        FSCC.run argv
    0 // return an integer exit code
