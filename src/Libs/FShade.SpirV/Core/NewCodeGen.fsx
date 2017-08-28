
#I @"..\..\..\..\Packages\Newtonsoft.Json\lib\net45"
#r @"Newtonsoft.Json.dll"
#r "System.Xml.dll"
#r "System.Xml.Linq.dll"

open System
open System.IO
open System.Text.RegularExpressions
open Newtonsoft
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Serialization

type Kind =
    | AccessQualifier                       = 0
    | AddressingModel                       = 1
    | Capability                            = 2
    | Decoration                            = 3
    | Dim                                   = 4
    | ExecutionMode                         = 5
    | ExecutionModel                        = 6
    | FunctionControl                       = 7
    | GroupOperation                        = 8
    | IdMemorySemantics                     = 9
    | IdRef                                 = 10
    | IdResult                              = 11
    | IdResultType                          = 12
    | IdScope                               = 13
    | ImageFormat                           = 14
    | ImageOperands                         = 15
    | LiteralContextDependentNumber         = 16
    | LiteralExtInstInteger                 = 17
    | LiteralInteger                        = 18
    | LiteralSpecConstantOpInteger          = 19
    | LiteralString                         = 20
    | LoopControl                           = 21
    | MemoryAccess                          = 22
    | MemoryModel                           = 23
    | PairIdRefIdRef                        = 24
    | PairIdRefLiteralInteger               = 25
    | PairLiteralIntegerIdRef               = 26
    | SamplerAddressingMode                 = 27
    | SamplerFilterMode                     = 28
    | SelectionControl                      = 29
    | SourceLanguage                        = 30
    | StorageClass                          = 31

type Type =
    | Simple of Kind
    | Array of Kind
    | Option of Kind

type Instruction =
    {
        code : int
        name : string
        operands : list<string * Type>
    }

type EnumType = { typeName : string; flags : bool; values : list<string * int> }

type Meta =
    {
        magic : uint32
        version : uint32
        revision : uint32
    }

type Spec =
    { 
        meta            : Meta
        types           : list<EnumType>
        instructions    : list<Instruction>
    }


module Console = 
    let withColor (c : ConsoleColor) (f : unit -> 'r) =
        let c = Console.ForegroundColor
        try f()
        finally Console.ForegroundColor <- c
        
    let inline line fmt = 
        Printf.kprintf (fun str -> 
            Console.WriteLine("{0}", str)
        ) fmt

    let inline warn fmt = 
        Printf.kprintf (fun str -> 
            withColor ConsoleColor.Red (fun () ->
                Console.WriteLine("WARNING: {0}", str)
            )
        ) fmt

    let inline error fmt = 
        Printf.kprintf (fun str -> 
            withColor ConsoleColor.Red (fun () ->
                Console.WriteLine("ERROR: {0}", str)
            )
        ) fmt



let specFile = "spirv.json"
let instructionSpecFile = "spirv.core.grammar.json"
let outFile = "SpirVInstructions.fs"

module Parser =

    let reserved =
        Set.ofList [
//            "source"
//            "target"
            "process"
            "type"
            "component"
            "object"
            "function"
            "base"
            "default"
            "class"
            "member"
            "interface"
        ]

    let kinds =
        let names = Enum.GetNames(typeof<Kind>)
        let values = Enum.GetValues(typeof<Kind>) |> unbox<Kind[]>

        Array.zip names values
            |> Map.ofArray

    let idRx = Regex @"[a-zA-Z_][a-zA-Z_0-9]*"

    let getName (index : int) (name : string) =
        let matches = idRx.Matches name

        let result = 
            let all = List.init matches.Count (fun i -> matches.[i].Value)
            match all with
                | h :: r ->
                    if h.[0] >= 'A' && h.[0] <= 'Z' then
                        let start = System.String(h.[0] + char 32, 1)
                        let rest = h.Substring(1)
                        let all = start + rest :: r
                        String.concat "" all
                    else
                        String.concat "" all

                | _ ->
                    sprintf "op%d" index
        if Set.contains result reserved then
            Console.error "bad name %A" result
            "_" + result
        else
            result

    let getKind (kind : string) =
        match Map.tryFind kind kinds with
            | Some k -> k
            | None -> failwithf "invalid kind: %A" kind

    let parseMeta (file : string) =
        let file = Path.Combine(__SOURCE_DIRECTORY__, file)
        let obj = JsonConvert.DeserializeObject(File.ReadAllText file) |> unbox<JObject>
        
        let spv = obj.GetValue "spv" |> unbox<JObject>
        let meta = spv.GetValue "meta" |> unbox<JObject>

        let opCodeMask = meta.GetValue("OpCodeMask").ToObject<int>()
        let wordCountShift = meta.GetValue("WordCountShift").ToObject<int>()
        if opCodeMask <> 0xFFFF || wordCountShift <> 16 then
            failwith "invalid binary layout"

        {
            magic = meta.GetValue("MagicNumber").ToObject<uint32>()
            version = meta.GetValue("Version").ToObject<uint32>()
            revision = meta.GetValue("Revision").ToObject<uint32>()
        }


    let parseTypes (file : string) =
        let file = Path.Combine(__SOURCE_DIRECTORY__, file)
        let obj = JsonConvert.DeserializeObject(File.ReadAllText file) |> unbox<JObject>
        
        let spv = obj.GetValue "spv" |> unbox<JObject>
        let enums = spv.GetValue "enum"
        [
            for e in enums.Children<JObject>() do
                let name = e.GetValue("Name").ToObject<string>()
                let kind = e.GetValue("Type").ToObject<string>()
                let values = e.GetValue("Values") |> unbox<JObject>

                let all = 
                    values 
                        :> seq<System.Collections.Generic.KeyValuePair<_,_>> 
                        |> Seq.map (fun (KeyValue(name, t)) -> name, t.Value<int>()) 
                        |> Seq.toList


                match kind with
                    | "Bit" -> 
                        let values = all |> List.map (fun (n,v) -> n, 1 <<< v)
                        yield { typeName = name; flags = true; values = ("None", 0) :: values }

                    | "Value" ->
                        yield { typeName = name; flags = false; values = all }
                        
                    | _ ->
                        failwithf "unexpected kind %A" kind

        ]

    let parseInstructions (file : string) = 
        let file = Path.Combine(__SOURCE_DIRECTORY__, file)
        let obj = JsonConvert.DeserializeObject(File.ReadAllText file) |> unbox<JObject>
    
        let instructions = obj.GetValue "instructions"
        [
            for i in instructions.Children<JObject>() do
                let name = i.GetValue "opname"
                let code = i.GetValue "opcode"
                let name = name.ToObject<string>()
                let code = code.ToObject<int>()

                let operands = 
                    match i.TryGetValue "operands" with
                        | (true, operands) -> operands.Children<JObject>() |> Seq.toList
                        | _ -> []

                let operands = 
                    operands |> List.mapi (fun i o ->
                        let kind = o.GetValue("kind").ToObject<string>() |> getKind

                        let name =
                            match o.TryGetValue("name") with
                                | (true, name) -> name.ToString() |> getName i
                                | _ -> sprintf "op%d" i

                        let argumentType =
                            match kind with
                                | Kind.LiteralContextDependentNumber ->
                                    Array Kind.LiteralInteger

                                | _ -> 
                                    match o.TryGetValue("quantifier") with
                                        | (true, quantifier) -> 
                                            match quantifier.ToString() with
                                                | "*" -> Array kind
                                                | "?" -> Option kind
                                                | v -> failwithf "unexpected quantifier %A" v
                                        | _ ->
                                            Simple kind

                        name, argumentType
                    )

                let operands =
                    operands |> List.collect (fun (n,t) ->
                        match t with
                            | Option Kind.ImageOperands ->
                                [(n,t); ("parameters", Array Kind.IdRef)]
                            | _ ->
                                [(n,t)]
                    )
                
                let moreThanOnce =
                    operands |> Seq.countBy (fun (n,_) -> n) |> Seq.filter (fun (_,v) -> v > 1) |> Map.ofSeq

                let mutable currentIds = Map.map (fun _ _ -> 1) moreThanOnce

                let operands =
                    operands |> List.map (fun (n,t) ->
                        match Map.tryFind n currentIds with
                            | Some id ->
                                currentIds <- Map.add n (id + 1) currentIds
                                (n + string id, t)
                            | None ->
                                (n,t)
                    )

                yield { name = name; code = code; operands = operands }
            ]

    let parseSpec() =
        let meta = parseMeta specFile
        let types = parseTypes specFile
        let instructions = parseInstructions instructionSpecFile
        { meta = meta; types = types; instructions = instructions }

module Printer =
    open System.Text
    let mutable builder = StringBuilder()

    module List =
        let rec mapOption (f : 'a -> Option<'b>) (l : list<'a>) =
            match l with
                | [] -> Some []
                | h :: r ->
                    match f h with
                        | Some h ->
                            match mapOption f r with
                                | Some r -> Some (h :: r)
                                | _ -> None
                        | _ ->
                            None


    let line fmt = 
        Printf.kprintf (fun str ->
            builder.AppendLine(str) |> ignore
            Console.WriteLine("{0}", str)
        ) fmt

    let printTypes (types : list<EnumType>) =
        for t in types do
            if t.flags then
                line "[<Flags>]"

            line "type %s = " t.typeName
            for (n,v) in t.values do
                line "    | %s = %d" n v

    let kindName (k : Kind) =
        match k with
            | Kind.LiteralString -> "string"
            | Kind.LiteralInteger -> "int"
            | Kind.LiteralContextDependentNumber -> failwith "unexpected"
            | Kind.IdScope -> "Scope"
            | Kind.IdMemorySemantics -> "MemorySemantics"
            | _ -> string k

    let typeName (t : Type) =
        match t with
            | Array k -> sprintf "array<%s>" (kindName k)
            | Option k -> sprintf "Option<%s>" (kindName k)
            | Simple k -> kindName k

    let sizeInIntegers (k : Kind) =
        match k with
            | Kind.AccessQualifier                       -> Some 1
            | Kind.AddressingModel                       -> Some 1
            | Kind.Capability                            -> Some 1
            | Kind.Decoration                            -> Some 1
            | Kind.Dim                                   -> Some 1
            | Kind.ExecutionMode                         -> Some 1
            | Kind.ExecutionModel                        -> Some 1
            | Kind.FunctionControl                       -> Some 1
            | Kind.GroupOperation                        -> Some 1
            | Kind.IdMemorySemantics                     -> Some 1
            | Kind.IdRef                                 -> Some 1
            | Kind.IdResult                              -> Some 1
            | Kind.IdResultType                          -> Some 1
            | Kind.IdScope                               -> Some 1 
            | Kind.ImageFormat                           -> Some 1
            | Kind.ImageOperands                         -> Some 1
            | Kind.LiteralContextDependentNumber         -> failwith "unexpected"
            | Kind.LiteralExtInstInteger                 -> Some 1
            | Kind.LiteralInteger                        -> Some 1
            | Kind.LiteralSpecConstantOpInteger          -> Some 1
            | Kind.LiteralString                         -> None
            | Kind.LoopControl                           -> Some 1
            | Kind.MemoryAccess                          -> Some 1
            | Kind.MemoryModel                           -> Some 1
            | Kind.PairIdRefIdRef                        -> Some 2
            | Kind.PairIdRefLiteralInteger               -> Some 2
            | Kind.PairLiteralIntegerIdRef               -> Some 2
            | Kind.SamplerAddressingMode                 -> Some 1
            | Kind.SamplerFilterMode                     -> Some 1
            | Kind.SelectionControl                      -> Some 1
            | Kind.SourceLanguage                        -> Some 1
            | Kind.StorageClass                          -> Some 1
            | _ -> failwith "unsupported kind"

    let typeSizeInIntegers (t : Type) =
        match t with
            | Simple k -> sizeInIntegers k
            | _ -> None

    let printPreamble() =
        line ""
        line "type IdResult = uint32"
        line "type IdResultType = uint32"
        line "type IdRef = uint32"
        line "type LiteralExtInstInteger = int"
        line "type LiteralSpecConstantOpInteger = int"


        line "type PairIdRefIdRef ="
        line "    struct"
        line "        val mutable public E0 : IdRef"
        line "        val mutable public E1 : IdRef"
        line "        new(e0, e1) = { E0 = e0; E1 = e1 }"
        line "    end"

        line "type PairIdRefLiteralInteger ="
        line "    struct"
        line "        val mutable public IdRef : IdRef"
        line "        val mutable public Value : int"
        line "        new(id, value) = { IdRef = id; Value = value }"
        line "    end"

        line "type PairLiteralIntegerIdRef ="
        line "    struct"
        line "        val mutable public Value : int"
        line "        val mutable public IdRef : IdRef"
        line "        new(value, id) = { Value = value; IdRef = id }"
        line "    end"
        line ""
    
    let printInstructionType (instructions : list<Instruction>) =
        line "type Instruction = "

        for i in instructions do
            match i.operands with
                | [] -> 
                    line "    | %s" i.name

                | operands ->
                    let args = operands |> List.map (fun (n,t) -> sprintf "%s : %s" n (typeName t)) |> String.concat " * "
                    line "    | %s of %s" i.name args

    type SizeComponent =
        | Dynamic of string
        | Static of int

    let rec sizeComponents (i : list<string * Type>) =
        match i with
            | [] -> []
            | (n,t) :: rest ->
                let rest = sizeComponents rest
                match t with
                    | Simple (Kind.LiteralString) ->
                        Dynamic(sprintf "div4 (1 + %s.Length)" n) :: rest

                    | Simple kind ->
                        let s = Option.get (sizeInIntegers kind)
                        Static(s) :: rest

                    | Option Kind.LiteralString ->
                        Dynamic (sprintf "(match %s with | Some str -> div4 (1 + str.Length) | None -> 0)" n) :: rest

                    | Option kind ->
                        let s = Option.get (sizeInIntegers kind)
                        Dynamic(sprintf "(if Option.isSome %s then %d else 0)" n s) :: rest

                    | Array kind ->
                        let s = Option.get (sizeInIntegers kind)
                        Dynamic(sprintf "(%s.Length * %d)" n s) :: rest

    let sizeExpression (i : list<string * Type>) =
        let comp = Static 1 :: sizeComponents i    
        
        let rec print (l : list<SizeComponent>) =
            match l with
                | [] -> "0"

                | [Static s] -> string s
                | [Dynamic d] -> d

                | Static a :: Static b :: rest -> print (Static (a + b) :: rest)
                | Static a :: rest -> sprintf "%d + %s" a (print rest)
                | Dynamic d :: rest -> sprintf "%s + %s" d (print rest)
               
        print comp
                

    let printMeta (m : Meta) =
        line "    [<Literal>]"
        line "    let Magic = 0x%Xu" m.magic
        line "    [<Literal>]"
        line "    let Version = %du" m.version
        line "    [<Literal>]"
        line "    let Revision = %du" m.revision
        line ""


    let printWriter (instructions : list<Instruction>) =
        line "    let private div4 (v : int) ="
        line "        if v %% 4 = 0 then v / 4"
        line "        else 1 + v / 4"
        line ""
        line "    type BinaryWriter with"
        line "        member inline x.Write<'a when 'a : enum<int> and 'a : (static member op_Explicit : 'a -> int)>(value : 'a) ="
        line "            x.Write(int value)"
        line "        member inline x.Write(o :PairIdRefLiteralInteger) ="
        line "            x.Write(o.IdRef)"
        line "            x.Write(o.Value)"
        line "        member inline x.Write(o :PairLiteralIntegerIdRef) ="
        line "            x.Write(o.Value)"
        line "            x.Write(o.IdRef)"
        line "        member inline x.Write(o :PairIdRefIdRef) ="
        line "            x.Write(o.E0)"
        line "            x.Write(o.E1)"
        line ""
        line "        member inline x.Write(o : Option<int>) ="
        line "            match o with | Some o -> x.Write o | _ -> ()"
        line "        member inline x.Write(o : Option<uint32>) ="
        line "            match o with | Some o -> x.Write o | _ -> ()"
        line "        member inline x.Write<'a when 'a : enum<int> and 'a : (static member op_Explicit : 'a -> int)>(value : Option<'a>) ="
        line "            match value with | Some value -> x.Write(int value) | None -> ()"
        line ""
        line "        member inline x.WriteString(str : string) ="
        line "            let len = str.Length + 1"
        line "            x.Write(str.ToCharArray())"
        line "            x.Write(char 0)"
        line "            if len %% 4 <> 0 then"
        line "                for m in 1 .. (4 - len %% 4) do x.Write(0uy)"
        line "        member inline x.WriteString(o : Option<string>) ="
        line "            match o with | Some o -> x.WriteString o | _ -> ()"
        line ""
        line "        member inline x.WriteArray(arr : int[]) ="
        line "            for a in arr do x.Write(a)"
        line "        member inline x.WriteArray(arr : uint32[]) ="
        line "            for a in arr do x.Write(a)"
        line ""
        line "        member inline x.WriteArray(arr : PairIdRefLiteralInteger[]) ="
        line "            for a in arr do x.Write(a)"
        line "        member inline x.WriteArray(arr : PairLiteralIntegerIdRef[]) ="
        line "            for a in arr do x.Write(a)"
        line "        member inline x.WriteArray(arr : PairIdRefIdRef[]) ="
        line "            for a in arr do x.Write(a)"



        line ""
        line "    let writeTo (writer : BinaryWriter) (i : Instruction) = "


        line "        match i with"
        for i in instructions do
            let pattern =
                match i.operands with
                    | [] -> i.name
                    | operands -> 
                        let args = operands |> List.map fst |> String.concat ", "
                        sprintf "%s(%s)" i.name args

            let fixedSize =
                i.operands 
                    |> List.mapOption (fun (_,t) -> typeSizeInIntegers t)
                    |> Option.map List.sum
                    |> Option.map (fun s -> 1 + s)



            line "            | %s ->" pattern
            match fixedSize with
                | Some s ->
                    let encoded = ((uint32 s &&& 0xFFFFu) <<< 16) ||| (uint32 i.code &&& 0xFFFFu)
                    line "                // size: %d, opcode: %d" s (int i.code)
                    line "                writer.Write(0x%Xu)" encoded
                | None ->
                    let code = (uint32 i.code &&& 0xFFFFu)
                    line "                // opcode: %d" (int i.code)
                    line "                let _size = %s" (sizeExpression i.operands)
                    line "                writer.Write(((uint32 _size &&& 0xFFFFu) <<< 16) ||| %du)" code
 
            for (n,t) in i.operands do
                match t with
                    | Simple Kind.LiteralString | Option Kind.LiteralString ->
                        line "                writer.WriteString(%s)" n
                    | Array Kind.LiteralString ->
                        failwith "unexpected"

                    | Array kind ->
                        line "                writer.WriteArray(%s)" n

                    | _ -> 
                        line "                writer.Write(%s)" n 

    let printReader (instructions : list<Instruction>) =
        line ""
        line "    let inline private tryDecrement (r : ref<int>) (v : int) (f : unit -> 'a) = if !r >= v then r := !r - v; Some(f()) else None"
        line "    "
        line "    type BinaryReader with"
        line "        member inline x.ReadAccessQualifier(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<AccessQualifier>(x.ReadInt32()))"
        line "        member inline x.ReadAddressingModel(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<AddressingModel>(x.ReadInt32()))"
        line "        member inline x.ReadCapability(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<Capability>(x.ReadInt32()))"
        line "        member inline x.ReadDecoration(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<Decoration>(x.ReadInt32()))"
        line "        member inline x.ReadDim(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<Dim>(x.ReadInt32()))"
        line "        member inline x.ReadExecutionMode(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<ExecutionMode>(x.ReadInt32()))"
        line "        member inline x.ReadExecutionModel(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<ExecutionModel>(x.ReadInt32()))"
        line "        member inline x.ReadFunctionControl(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<FunctionControl>(x.ReadInt32()))"
        line "        member inline x.ReadGroupOperation(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<GroupOperation>(x.ReadInt32()))"
        line "        member inline x.ReadIdMemorySemantics(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<MemorySemantics>(x.ReadInt32()))"
        line "        member inline x.ReadIdRef(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadUInt32())"
        line "        member inline x.ReadIdResult(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadUInt32())"
        line "        member inline x.ReadIdResultType(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadUInt32())"
        line "        member inline x.ReadIdScope(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<Scope>(x.ReadInt32()))"
        line "        member inline x.ReadImageFormat(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<ImageFormat>(x.ReadInt32()))"
        line "        member inline x.ReadImageOperands(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<ImageOperands>(x.ReadInt32()))"
        line "        member inline x.ReadLiteralExtInstInteger(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadInt32())"
        line "        member inline x.ReadLiteralInteger(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadInt32())"
        line "        member inline x.ReadLiteralSpecConstantOpInteger(s : ref<int>) = tryDecrement s 1 (fun () -> x.ReadInt32())"
        line "        member inline x.ReadLoopControl(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<LoopControl>(x.ReadInt32()))"
        line "        member inline x.ReadMemoryAccess(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<MemoryAccess>(x.ReadInt32()))"
        line "        member inline x.ReadMemoryModel(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<MemoryModel>(x.ReadInt32()))"
        line "        member inline x.ReadSamplerAddressingMode(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<SamplerAddressingMode>(x.ReadInt32()))"
        line "        member inline x.ReadSamplerFilterMode(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<SamplerFilterMode>(x.ReadInt32()))"
        line "        member inline x.ReadSelectionControl(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<SelectionControl>(x.ReadInt32()))"
        line "        member inline x.ReadSourceLanguage(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<SourceLanguage>(x.ReadInt32()))"
        line "        member inline x.ReadStorageClass(s : ref<int>) = tryDecrement s 1 (fun () -> unbox<StorageClass>(x.ReadInt32()))"
        line "        member inline x.ReadPairIdRefIdRef(s : ref<int>) = tryDecrement s 2 (fun () -> PairIdRefIdRef(x.ReadUInt32(), x.ReadUInt32()))"
        line "        member inline x.ReadPairIdRefLiteralInteger(s : ref<int>) = tryDecrement s 2 (fun () -> PairIdRefLiteralInteger(x.ReadUInt32(), x.ReadInt32()))"
        line "        member inline x.ReadPairLiteralIntegerIdRef(s : ref<int>) = tryDecrement s 2 (fun () -> PairLiteralIntegerIdRef(x.ReadInt32(), x.ReadUInt32()))"
        line "        member inline x.ReadLiteralString(s : ref<int>) = "
        line "            if !s > 0 then"
        line "                let mutable cont = true"
        line "                let mutable bytes = !s * 4"
        line "                let result = System.Collections.Generic.List<char>(!s * 4)"
        line "                while cont && bytes > 0 do"
        line "                    let c = x.ReadChar()"
        line "                    bytes <- bytes - 1"
        line "                    if c <> char 0 then"
        line "                        result.Add c"
        line "                    else"
        line "                        cont <- false"
        line "            "
        line "                let rest = bytes %% 4"
        line "                bytes <- bytes - rest"
        line "                if rest > 0 then x.BaseStream.Seek(int64 rest, SeekOrigin.Current) |> ignore"
        line "                s := bytes / 4"
        line "                String(Seq.toArray result) |> Some"
        line "            else"
        line "                None"
        line ""
        line "        member inline x.ReadArray(reader : ref<int> -> Option<'a>, remaining : ref<int>) ="
        line "            let rec run() ="
        line "                match reader(remaining) with"
        line "                    | Some v -> v :: run()"
        line "                    | None -> []"
        line "            List.toArray (run())"
        line ""
        line "    let tryReadFrom (reader : BinaryReader) = "
        line "        option {"
        line "            let encoded = reader.ReadUInt32()"
        line "            let code = int (encoded &&& 0xFFFFu)"
        line "            let remaining = ref (int (encoded >>> 16) - 1)"
        line "            "
        line "            match code with"
   
        let read (s : ref<int>) (reader : BinaryReader) =
            if !s > 0 then
                let mutable cont = true
                let mutable bytes = !s * 4
                let result = System.Collections.Generic.List<char>(!s * 4)
                while cont && bytes > 0 do
                    let c = reader.ReadChar()
                    if c <> char 0 then
                        result.Add c
                    else
                        cont <- false

                let rest = bytes % 4
                bytes <- bytes - rest
                if rest > 0 then reader.BaseStream.Seek(int64 rest, SeekOrigin.Current) |> ignore
                s := bytes / 4
                String(Seq.toArray result) |> Some
            else
                None
                    

        for i in instructions do
            line "                | %d -> // %s" i.code i.name
            for (n,t) in i.operands do
                let reader = 
                    match t with
                        | Simple kind -> sprintf "let! %s = reader.Read%A(remaining)" n kind
                        | Array kind -> sprintf "let %s = reader.ReadArray(reader.Read%A, remaining)" n kind
                        | Option kind -> sprintf "let %s = reader.Read%A(remaining)" n kind
                    
                line "                    %s" reader

            let result =
                match i.operands with
                    | [] -> 
                        sprintf "%s" i.name
                    | operands -> 
                        let args = operands |> List.map fst |> String.concat ", "
                        sprintf "(%s(%s))" i.name args

            line "                    if !remaining = 0 then return %s" result
            line "                    else return! None"

        line "                | _ -> return! None"
        line "        }"
        line ""
        line "    let readFrom (reader : BinaryReader) = tryReadFrom reader |> Option.get"

    let printResults (instructions : list<Instruction>) =
        line ""
        line ""
        line "    let resultType (i : Instruction) = "
        line "        match i with"
        for i in instructions do
            let pattern =
                match i.operands with
                    | [] -> i.name
                    | operands -> 
                        let args = operands |> List.map fst |> String.concat ", "
                        sprintf "%s(%s)" i.name args

            let resultTypeField = 
                i.operands |> List.tryPick (fun (n,t) ->
                    match t with
                        | Simple Kind.IdResultType -> Some n
                        | _ -> None
                )
            match resultTypeField with
                | Some f -> 
                    line "        | %s -> Some %s" pattern f
                | None ->
                    ()

        line "        | _ -> None"
                    

        line ""
        line "    let resultId (i : Instruction) = "
        line "        match i with"
        for i in instructions do
            let pattern =
                match i.operands with
                    | [] -> i.name
                    | operands -> 
                        let args = operands |> List.map fst |> String.concat ", "
                        sprintf "%s(%s)" i.name args

            let resultTypeField = 
                i.operands |> List.tryPick (fun (n,t) ->
                    match t with
                        | Simple Kind.IdResult -> Some n
                        | _ -> None
                )
            match resultTypeField with
                | Some f -> 
                    line "        | %s -> Some %s" pattern f
                | None ->
                    ()

        line "        | _ -> None"

        line ""
        line "    let name (i : Instruction) = "
        line "        match i with"
        for i in instructions do
            let pattern =
                match i.operands with
                    | [] -> i.name
                    | operands -> sprintf "%s _" i.name

            line "        | %s -> \"%s\"" pattern i.name


        line ""
        line "    let operands (i : Instruction) = "
        line "        match i with"
        for i in instructions do
            let pattern =
                match i.operands with
                    | [] -> i.name
                    | operands -> 
                        let args = operands |> List.map fst |> String.concat ", "
                        sprintf "%s(%s)" i.name args

            let values = 
                match i.operands with
                    | [] -> []
                    | (_, Simple Kind.IdResultType) :: (_, Simple Kind.IdResult) :: rest -> rest
                    | (_, Simple Kind.IdResultType) :: rest -> rest
                    | ops -> ops

            line "        | %s -> [| %s |]" pattern (values |> List.map (fst >> sprintf "%s :> obj") |> String.concat "; ")



    let printSpec (s : Spec) =
        builder <- StringBuilder()
//        line "#I @\"E:\\Development\\fshade\\bin\\Debug\""
//        line "#r \"Aardvark.Base.dll\""
//        line "#r \"Aardvark.Base.TypeProviders.dll\""
//        line "#r \"Aardvark.Base.FSharp.dll\""

        line "namespace FShade.SpirV"

        line "open System"
        line "open System.IO"
        line "open Aardvark.Base.Monads.Option"

        printTypes s.types

        printPreamble()

        line ""
        printInstructionType s.instructions
        
        line ""
        line "[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]"
        line "[<RequireQualifiedAccess>]"
        line "module Instruction ="
        printMeta s.meta
        printWriter s.instructions
        printReader s.instructions
        printResults s.instructions


        let outFile = Path.Combine(__SOURCE_DIRECTORY__, outFile)
        File.WriteAllText(outFile, builder.ToString())

let run() = Printer.printSpec (Parser.parseSpec())