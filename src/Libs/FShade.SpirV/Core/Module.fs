namespace FShade.SpirV

open System
open System.IO
open Aardvark.Base


[<AutoOpen>]
module InstructionExtensions =
    
    type Instruction with
        member inline x.ResultType = Instruction.resultType x
        member inline x.ResultId = Instruction.resultId x
        member inline x.Name = Instruction.name x
        member inline x.Operands = Instruction.operands x




type Module =
    {
        magic : uint32
        version : Version
        generatorMagic : uint32
        bound : uint32
        reserved : uint32
        instructions : list<Instruction>
    }




[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Module =

    let private knownGenerators =
        Map.ofList [
            0,  "Khronos"
            1,  "LunarG"
            2,  "Valve"
            3,  "Codeplay"
            4,  "NVIDIA"
            5,  "ARM"
            6,  "Khronos"
            7,  "Khronos"
            8,  "Khronos"
            9,  "Qualcomm"
            10, "AMD"
            11, "Intel"

            0XFADE, "FShade"
        ]


    let private versionToUInt32 (v : Version) =
        (uint32 v.Major &&& 0xFFFFu) <<< 16 |||
        (uint32 v.Minor &&& 0xFFFFu)

    let private versionOfUInt32 (v : uint32) =
        let major = int (v >>> 16)
        let minor = int (v &&& 0xFFFFu)
        Version(major, minor)


    let print (m : Module) =
        Log.start "SpirV module"

        Log.line "// magic:     0x%08X" m.magic
        Log.line "// version:   %A" m.version

        let genId = 
            let genId = m.generatorMagic >>> 16 |> int
            let genVersion = m.generatorMagic &&& 0xFFFFu |> int
            match Map.tryFind genId knownGenerators with
                | Some generator -> generator + sprintf "(v%d)" genVersion
                | None -> sprintf "unknown%d(v%d)" genId genVersion

        Log.line "// generator: %s" genId
        Log.line "// bound:     %d" m.bound
        Log.line ""

        let maxIdWidth = Fun.Log10 m.bound |> ceil |> int
        let noId = System.String(' ', maxIdWidth)

        let idString (id : uint32) =
            let str = string id
            if str.Length < maxIdWidth then System.String(' ', maxIdWidth - str.Length) + str
            else str

        let mutable indent = ""

        for i in m.instructions do
            let name = i.Name
            let operands = i.Operands |> Seq.filter (not << isNull) |> Seq.map (sprintf "%A") |> String.concat " "

            match i with
                | OpFunctionEnd -> indent <- indent.Substring 4
                | _ -> ()

            let line = 
                match i.ResultType, i.ResultId with
                    | Some rt, None -> sprintf "%s <-%s %s %s" (idString rt) indent name operands
                    | Some rt, Some ri -> sprintf "%s <-%s %s t%s %s" (idString ri) indent name (string rt) operands
                    | None, Some ri -> sprintf "%s <-%s %s %s" (idString ri) indent name operands
                    | _, None -> sprintf "%s%s    %s %s" noId indent name operands

            Log.line "%s" line

            match i with
                | OpFunction _ -> indent <- indent + "    "
                | _ -> ()


        Log.stop()

    let tryReadFrom (stream : Stream) =
        use reader = new BinaryReader(stream, System.Text.Encoding.UTF8, true)
        let magic = reader.ReadUInt32()
        let version = reader.ReadUInt32()
        let generatorMagic = reader.ReadUInt32()
        let bound = reader.ReadUInt32()
        let reserved = reader.ReadUInt32()

        if magic <> Instruction.Magic || version > Instruction.Version then 
            None
        else
            let rec read() =
                if reader.BaseStream.Position < reader.BaseStream.Length then
                    match Instruction.tryReadFrom reader with
                        | Some i ->
                            match read() with
                                | Some rest -> Some (i :: rest)
                                | None -> None
                        | None ->
                            None
                else
                    Some []
            
            match read() with
                | Some instructions ->
                    Some {
                        magic = magic
                        version = versionOfUInt32 version
                        generatorMagic = generatorMagic
                        bound = bound
                        reserved = reserved
                        instructions = instructions
                    }
                | None ->
                    None

    let readFrom (stream : Stream) =
        match tryReadFrom stream with
            | Some m -> m
            | None -> failwith "could not read SpirV module"

    let writeTo (stream : Stream) (m : Module) =
        use writer = new BinaryWriter(stream, System.Text.Encoding.UTF8, true)

        writer.Write(m.magic)
        writer.Write(versionToUInt32 m.version)
        writer.Write(m.generatorMagic)
        writer.Write(m.bound)
        writer.Write(m.reserved)

        for i in m.instructions do
            Instruction.writeTo writer i

        writer.Flush()
    
    let toByteArray (m : Module) =
        use ms = new MemoryStream()
        writeTo ms m
        ms.ToArray()

    let ofByteArray (binary : byte[]) =
        use ms = new MemoryStream(binary)
        readFrom ms
