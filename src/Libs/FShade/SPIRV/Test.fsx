#I @"..\..\..\..\Bin\Debug"
#r "FShade.dll"


open System.IO
open SpirV

let test() =
    use stream = new FileStream(@"C:\VulkanSDK\0.9.2\Bin\cube-vert.spv", FileMode.Open)
    let reader = new BinaryReader(stream)
    let m = Serializer.read reader


    for i in m.instructions do
        printfn "%A" i


