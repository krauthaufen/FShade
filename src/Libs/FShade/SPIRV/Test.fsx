#I @"..\..\..\..\Bin\Debug"
#r "FShade.dll"


open System.IO
open SpirV

let test() =

    let inputData = File.ReadAllBytes @"C:\VulkanSDK\0.9.2\Bin\cube-vert.spv"

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


    let headerLength = 4*4

    let inputInstructions = Array.sub inputData headerLength (inputData.Length-headerLength)
    let inputStream = new MemoryStream(Array.append inputInstructions (Array.zeroCreate 10000))
    for i in m.instructions do
        use testStream = new MemoryStream()
        let writer = new BinaryWriter(testStream)
        Serializer.writeInstructions [i] writer
        let icode = testStream.ToArray()
        let real = Array.zeroCreate icode.Length
        inputStream.Read(real, 0, real.Length) |> ignore


        if real <> icode then
            printfn "bad instruction: %A" i






    inputData, outputData


