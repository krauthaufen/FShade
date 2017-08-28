namespace FShade.SpirV.New

open System.IO
open Aardvark.Base

type Module =
    {
        magic : uint32
        version : uint32
        generatorMagic : uint32
        bound : uint32
        reserved : uint32
        instructions : list<Instruction>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Module =
    
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
                        version = version
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
        writer.Write(m.version)
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
