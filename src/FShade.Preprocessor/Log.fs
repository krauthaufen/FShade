module Log

open System

let mutable private indent = ""
    
let private colorsSupported =
    try 
        let o = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Red
        Console.ForegroundColor <- o
        true
    with _ -> 
        false

let mutable verbose = true
            
let debug fmt =
    fmt |> Printf.kprintf (fun str ->
        if verbose then
            if colorsSupported then
                let o = Console.ForegroundColor
                Console.ForegroundColor <- ConsoleColor.DarkGray
                try System.Console.WriteLine("{0}{1}", indent, str)
                finally Console.ForegroundColor <- o
            else
                System.Console.WriteLine("{0}{1}", indent, str)
    )
                    
let line fmt =
    fmt |> Printf.kprintf (fun str ->
        System.Console.WriteLine("{0}{1}", indent, str)
    )
        
let warn fmt =
    fmt |> Printf.kprintf (fun str ->
        if colorsSupported then
            let o = Console.ForegroundColor
            Console.ForegroundColor <- ConsoleColor.DarkYellow
            try System.Console.WriteLine("{0}{1}", indent, str)
            finally Console.ForegroundColor <- o
        else
            System.Console.WriteLine("{0}{1}", indent, str)
    )
        
let error fmt =
    fmt |> Printf.kprintf (fun str ->
        if colorsSupported then
            let o = Console.ForegroundColor
            Console.ForegroundColor <- ConsoleColor.Red
            try System.Console.WriteLine("{0}{1}", indent, str)
            finally Console.ForegroundColor <- o
        else
            System.Console.WriteLine("{0}{1}", indent, str)
    )

let stop() =
    indent <- indent.Substring 2 
        
let start fmt =
    fmt |> Printf.kprintf (fun str -> 
        System.Console.WriteLine("{0}{1}", indent, str)
        indent <- indent + "  "
    )
