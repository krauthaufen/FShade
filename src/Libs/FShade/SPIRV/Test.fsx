#I @"..\..\..\..\Bin\Debug"
#r "FShade.dll"


open System.IO
open SpirV

let test() =

    let inputData = File.ReadAllBytes @"C:\VulkanSDK\0.9.2\Bin\cube-frag.spv"

    use stream = new MemoryStream(inputData)
    let reader = new BinaryReader(stream)
    let m = Serializer.read reader

    use testStream = new MemoryStream()
    let writer = new BinaryWriter(testStream)
    Serializer.write m writer

    let outputData = testStream.ToArray()
    printfn "input = output: %A" (inputData = outputData)
    let m = Serializer.read (new BinaryReader(new MemoryStream(outputData)))

    for i in m.instructions do
        printfn "%A" i


