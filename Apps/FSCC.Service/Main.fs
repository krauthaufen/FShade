namespace FSCC.Service

open IntelliFactory.Html
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets
open FSCC
open System.IO

type Action =
    | Home
    | About

module Skin =
    open System.Web

    type Page =
        {
            Title : string
            Body : list<Content.HtmlElement>
        }

    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    let WithTemplate title body : Content<Action> =
        Content.WithTemplate MainTemplate <| fun context ->
            {
                Title = title
                Body = body context
            }


module Site =

    let ( => ) text url =
        A [HRef url] -< [Text text]

    let compile (ctx: Context<Action>) =
        let code = ctx.Request.Post.["code"]
        let composition = ctx.Request.Post.["comp"]

        match code, composition with
            | Some code, Some composition ->
                FSCC.repl <- true
                let code = code.Replace("\t", "    ")

                let temp = Path.GetTempFileName() + ".fs"
                File.WriteAllText(temp, code)
                let shaderNames = composition.Split(' ')
                
                
                let ms = new System.IO.MemoryStream()
                let o = new System.IO.StreamWriter(ms)
                let savedOut = System.Console.Out
                System.Console.SetOut(o)
                [[|temp; "-c"; |]; shaderNames] |> Array.concat |> FSCC.run
                System.Console.SetOut(savedOut)

                o.Flush()


                let glsl = System.Text.ASCIIEncoding.Default.GetString(ms.ToArray())

                Some glsl

            | _ -> None        

    let Code (ctx: Context<Action>) =
        
        let code = ctx.Request.Post.["code"]
        let composition = ctx.Request.Post.["comp"]

        match code, composition with
            | Some code, Some composition ->
                FSCC.repl <- true
                let temp = Path.GetTempFileName() + ".fs"
                File.WriteAllText(temp, code)
                let shaderNames = composition.Split(' ')
                
                let ms = new System.IO.MemoryStream()
                let o = new System.IO.StreamWriter(ms)
                let savedOut = System.Console.Out
                System.Console.SetOut(o)
                [[|temp; "-c"; |]; shaderNames] |> Array.concat |> FSCC.run
                System.Console.SetOut(savedOut)

                o.Flush()


                let glsl = System.Text.ASCIIEncoding.Default.GetString(ms.ToArray())

                Text (glsl)

            | _ -> Text "NONE"

    open IntelliFactory.WebSharper.Sitelets.Http

    let HomePage =
        Content.CustomContent (fun ctx ->
            { Status = Status.Ok; Headers = []; WriteBody = fun s ->
                match compile ctx with
                    | Some code ->
                        let sw = new StreamWriter(s)
                        sw.WriteLine code
                        sw.Flush()
                    | None ->
                        ()

            }
        )
        //Skin.WithTemplate "FShade WebService" <| fun ctx -> [ Code ctx ]

    let Main =
        Sitelet.Sum [ Sitelet.Content "/" Home HomePage ]

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Home]

type Global() =
    inherit System.Web.HttpApplication()

    member g.Application_Start(sender: obj, args: System.EventArgs) =
        ()

[<assembly: Website(typeof<Website>)>]
do ()
