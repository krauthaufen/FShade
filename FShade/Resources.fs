namespace FShade

module Resources =
    let private executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    let private resources = new System.Resources.ResourceManager("Resources", executingAssembly)

    let MainIcon : System.Drawing.Icon = unbox <| resources.GetObject("MainIcon")

    let syncing : System.Drawing.Image = unbox <| resources.GetObject("syncing")
    let unsyncable : System.Drawing.Image = unbox <| resources.GetObject("unsyncable")
    let uptodate : System.Drawing.Image = unbox <| resources.GetObject("uptodate")
    let warning : System.Drawing.Image = unbox <| resources.GetObject("warning")
