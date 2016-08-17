namespace FShade.Debug

module EffectEditor =
    open System.Collections.Generic
    open System.Collections.Concurrent
    open Aardvark.Base
    open Aardvark.Base.Arrays
    open System
    open System.Collections.Concurrent
    open System.Windows.Forms
    open FShade.Compiler
    open FShade
    open FShade.ShaderDebug
    

    type Delta<'a> = Add of 'a | Remove of 'a

    let private lineBreak = System.Text.RegularExpressions.Regex "\r\n"
    let private ws = System.Text.RegularExpressions.Regex "^[ ]*"
    let private removeIndent (str : string) =
        let lines = lineBreak.Split str |> Array.map (fun l -> l.Replace("\r", ""))
        let leadingIndentSize s = 
            let m = ws.Match s
            if m.Success then 
                if m.Length <> s.Length then m.Length
                else System.Int32.MaxValue
            else 0
            
        let minimalIndent = lines |> Array.map leadingIndentSize |> Array.min
        lines |> Array.map (fun l -> if l.Length > minimalIndent then l.Substring minimalIndent else l) |> String.concat "\r\n"

    let upToDate = Resources.uptodate
    let outOfDate = Resources.unsyncable
    let warning = Resources.warning


    type EditorEnvironment = { currentItem : ref<Option<ListViewItem>>; editor : ScintillaNET.Scintilla; list : ListView; codeCache : System.Collections.Generic.Dictionary<ListViewItem, string>; errorView : DataGridView }

    type ErrorView(error : Fsi.CompilerError) =
        member x.line = error.line
        member x.col = error.col
        member x.message = error.message
        member x.errorType =
            match error.errorType with
                | Fsi.CompilerErrorType.Warning -> warning
                | Fsi.CompilerErrorType.Error -> outOfDate

    let private compileEffect (env : EditorEnvironment) (m : EffectNode) (code : list<string>) (commit : bool) =
        let effect = m.read() |> GLSL.run410
        match effect with
            | Success (effect) ->
                let debugInfos =
                    effect.originals |> List.choose (fun s ->
                        match s.debugInfo with
                            | Some d -> Some d
                            | None -> None
                    )

                if debugInfos.Length = code.Length then
                        
                    let newDebugInfos =
                        List.zip debugInfos code |> List.map (fun (s,c) ->
                            let code = c |> removeIndent

                            { s with functionCode = code }
                        )

                    let newShaders =
                        List.zip newDebugInfos effect.originals |> List.map (fun (d, o) ->
                            { o with debugInfo = Some d }
                        )

                    System.Threading.Tasks.Task.Factory.StartNew(fun () ->
                        Report.BeginTimed "saving debug-shader"

                        Report.BeginTimed "compiling shader"

                        let shaders = newDebugInfos |> List.map DebugInfo.toEffect |> List.map (GLSL.run410)
                        let shaders = List.zip shaders newDebugInfos

                        let countLines (str : string) = (lineBreak.Split str |> Array.length)

                        let shaders = shaders |> List.fold (fun (o,se) (e,d) ->
                                                    let newOffset = o + countLines d.functionCode
                                                    match se, e with 
                                                        | Success list, Success sh ->
                                                            (newOffset, Success (sh::list))
                                                        | Error e, Error sh ->
                                                            let sh = Fsi.parseErrors sh
                                                            let sh = sh |> List.map (fun e -> { e with line = e.line + o })
                                                            let sh = { Fsi.errors = sh }
                                                            (newOffset, Error (sprintf "%s\r\n%A" e sh))
                                                        | Error e, Success _ ->
                                                            (newOffset, Error e)
                                                        | Success l, Error e ->
                                                            let sh = Fsi.parseErrors e
                                                            let sh = sh |> List.map (fun e -> { e with line = e.line + o })
                                                            let sh = { Fsi.errors = sh }
                                                            (newOffset, Error (sprintf "%A" sh))
                                                 ) (0, Success [])

                        let _,shaders = shaders

                        let shaders =
                            compile {
                                match shaders with
                                    | Success sh -> return sh |> List.rev
                                    | Error e -> return! error "%s" e
                            }

                        Report.End() |> ignore //compiling

                        let compiled =
                            compile {
                                let! shaders = shaders
                                Report.BeginTimed "composing shader"
                                let! composed = shaders |> List.map(fun s -> compile { return s }) |> compose

                                return composed
                            }

                        
                        match GLSL.run410 compiled with
                            | Success e ->
                                Report.End() |> ignore //composing
                                if commit then
                                    Report.BeginTimed "updating shader"

                                    let e = { e with originals = newShaders }
                                    m.write (compile { return e })

                                    Report.End() |> ignore //updating
                                
                                    let b = new BindingSource ()
                                    let a =  new System.Action(fun () -> 
                                        env.editor.GetRange(0, env.editor.TextLength-1).ClearIndicator(2)
                                        env.editor.GetRange(0, env.editor.TextLength-1).ClearIndicator(3)

                                        env.errorView.DataSource <- b
                                    ) 
                                    env.errorView.Invoke(a) |> ignore

                            | Error e ->
                                let errors = Fsi.parseErrors e
                                let b = new BindingSource ()
                                errors |> List.iter (fun e -> b.Add(ErrorView(e)) |> ignore)

     
                                let a =  new System.Action(fun () ->
                                    env.editor.GetRange(0, env.editor.TextLength-1).ClearIndicator(2)
                                    env.editor.GetRange(0, env.editor.TextLength-1).ClearIndicator(3)

                                    for err in errors do
                                        let line = env.editor.Lines.[err.line-1]
                                        let start = line.StartPosition + err.col - 1
                                        let e = line.EndPosition


                                        if err.errorType = Fsi.CompilerErrorType.Error then
                                            env.editor.GetRange(start, e).SetIndicator(2)
                                        else
                                            env.editor.GetRange(start, e).SetIndicator(3)
                                        //env.editor.Lines.[e.line-1].AddMarker(2) |> ignore

                                    env.errorView.DataSource <- b
                                ) 
                                env.errorView.Invoke(a) |> ignore




                                Log.error "%A" errors

                            
                        Report.End() |> ignore //saving
                    )
                else
                    System.Threading.Tasks.Task.Factory.StartNew(fun () -> ())
            | Error e ->
                System.Threading.Tasks.Task.Factory.StartNew(fun () -> failwith e; ())

    let private toListViewItem (s : EffectNode) =
        let item = ListViewItem(sprintf "  %s" s.name)
        item.Tag <- s
        item.ImageIndex <- 0
                
                
        item

    let private installSaveAction (t : ScintillaNET.Scintilla) (save : unit -> unit) =
        let controlDown = ref false

       
//
//        t.PreviewKeyDown.Add(fun e ->
//                    
//            if e.KeyCode = Keys.ControlKey then
//                controlDown := true
//                    
//        )
        t.KeyDown.Add(fun e ->
            if e.Control && e.KeyCode = Keys.S then
                save()
                e.Handled <- true
        )
//        t.KeyPress.Add(fun e ->
//            if int e.KeyChar = 19 && !controlDown then
//                save()
//                e.Handled <- true
//                        
//        )
//
//        t.KeyUp.Add(fun e->
//            if e.KeyCode = Keys.ControlKey then
//                controlDown := false
//        )       


    let private installContextMenu (env : EditorEnvironment) =
        let glslCode = new MenuItem("GLSL code")
        let revert = new MenuItem("Revert")


        glslCode.Click.Add(fun _ ->
            if env.list.SelectedItems.Count > 0 then
                let node = env.list.SelectedItems.[0].Tag |> unbox<EffectNode>
                let s = node.read()
                match GLSL.compileEffect410 (Map.ofList ["Colors", typeof<V4d>; "Depth", typeof<float>]) s with
                    | Success(_, code) ->
                        let f = new Form()
                        f.Text <- "GLSL"
                        let code = code.Replace("#ifdef", "//#ifdef").Replace("#endif", "//#endif").Replace("#ifndef", "//#ifndef")

                        let t = new ScintillaNET.Scintilla()
                  
                        t.Folding.IsEnabled <- true
                        t.Margins.[0].Width <- 30
                        t.ConfigurationManager.CustomLocation <- @"glsl.xml"
                        t.Lexing.LexerLanguageMap.Add("glsl", "cpp")
                        t.ConfigurationManager.Language <- "glsl"
                        t.Text <- code
                        t.Dock <- DockStyle.Fill
                        t.UndoRedo.EmptyUndoBuffer()



                        f.Width <- 800
                        f.Height <- 1000
                        f.Controls.Add(t)
                        f.Show()

                        ()
                    | Error e ->
                        MessageBox.Show(e) |> ignore
            ()
        )

        revert.Click.Add(fun _ ->
            if env.list.SelectedItems.Count > 0 then
                let item = env.list.SelectedItems.[0]
                let s = item.Tag |> unbox<EffectNode>
                let effect = s.read() |> GLSL.run410

                match effect with
                    | Success e ->
                        let codes =
                            e.originals |> List.choose (fun s ->
                                match s.debugInfo with
                                    | Some d -> Some d.originalCode
                                    | None -> None
                            )

                        let code = codes |> String.concat "\r\n"
                        env.editor.Text <- code
                        env.codeCache.[item] <- code
                        env.editor.UndoRedo.EmptyUndoBuffer()
                        item.ImageIndex <- 0
                        compileEffect env s codes true |> ignore
                    | Error e ->
                        ()


        )

        let ctx = new ContextMenu [| glslCode; revert |]
                
        env.list.ContextMenu <- ctx


    let mutable private registration = fun _ -> ()
    let mutable private pending = []

    let private createListView (t : ScintillaNET.Scintilla) =
        let l = new ListView()
        let images = new ImageList()
        images.Images.Add upToDate
        images.Images.Add outOfDate

        l.SmallImageList <- images
        l.Dock <- DockStyle.Fill
        l.View <- View.SmallIcon

        let dict = Dictionary()


        registration <- 
            (fun (d : list<Delta<EffectNode>>) ->
                let a = new Action(fun () -> 
                    for d in d do
                        match d with
                            | Add (e) ->
                                let item = toListViewItem e
                                item |> l.Items.Add |> ignore
                                dict.Add(e, item)
                            | Remove (e) ->
                                match dict.TryGetValue e with
                                    | (true,item) -> item.Remove()
                                    | _ -> ()
                )
                if l.IsHandleCreated then
                    l.Invoke(a) |> ignore
                else
                    a.Invoke()
            )

        registration (pending |> List.map Add)


        l


    let register (e : EffectNode) =
        registration [Add e]
        pending <- e::pending

    open System.Drawing

    let showUI () =
        let form = new Form()
        let splitter = new SplitContainer()
        splitter.Orientation <- Orientation.Vertical
        splitter.SplitterDistance <- 150
        splitter.Dock <- DockStyle.Fill
        splitter.FixedPanel <- FixedPanel.Panel1

        

        form.Controls.Add splitter

        let t = new ScintillaNET.Scintilla()
        t.ConfigurationManager.CustomLocation <- @"fsharp.xml"
        t.Lexing.LexerLanguageMap.Add("fs", "cpp")
        t.ConfigurationManager.Language <- "fs"
        t.Dock <- DockStyle.Fill
        t.Margins.[0].Width <- 30
        let errorIndicator = t.Indicators.[2]
        errorIndicator.DrawMode <- ScintillaNET.IndicatorDrawMode.Underlay
        errorIndicator.Style <- ScintillaNET.IndicatorStyle.Squiggle
        errorIndicator.Color <- Drawing.Color.Red
        let warnIndicator = t.Indicators.[3]
        warnIndicator.DrawMode <- ScintillaNET.IndicatorDrawMode.Underlay
        warnIndicator.Style <- ScintillaNET.IndicatorStyle.Squiggle
        warnIndicator.Color <- Drawing.Color.Blue

        let err = new DataGridView()

        let typeCol = new DataGridViewImageColumn(DataPropertyName = "errorType", Name = "")
        typeCol.AutoSizeMode <- DataGridViewAutoSizeColumnMode.None
        typeCol.Width <- 32
        typeCol.ImageLayout <- DataGridViewImageCellLayout.Zoom
        err.Columns.Add(typeCol) |> ignore

        let msgCol = new DataGridViewTextBoxColumn(DataPropertyName = "message", Name = "Description")
        msgCol.AutoSizeMode <- DataGridViewAutoSizeColumnMode.Fill
        err.Columns.Add(msgCol) |> ignore
        
        let lineCol = new DataGridViewTextBoxColumn(DataPropertyName = "line", Name = "Line")
        lineCol.AutoSizeMode <- DataGridViewAutoSizeColumnMode.None
        err.Columns.Add(lineCol) |> ignore

        let colCol = new DataGridViewTextBoxColumn(DataPropertyName = "col", Name = "Column")
        colCol.AutoSizeMode <- DataGridViewAutoSizeColumnMode.None
        err.Columns.Add(colCol) |> ignore

        err.RowHeadersVisible <- false
        err.ColumnHeadersVisible <- true
        err.AutoSizeColumnsMode <- DataGridViewAutoSizeColumnsMode.Fill
        err.Dock <- DockStyle.Fill
        err.AutoGenerateColumns <- false
        err.SelectionMode <- DataGridViewSelectionMode.FullRowSelect
        err.BackColor <- Color.White
        err.AllowUserToAddRows <- false
        err.AllowUserToResizeRows <- false
        err.AllowUserToDeleteRows <- false
        err.Sort(lineCol, ComponentModel.ListSortDirection.Ascending)
        err.BackgroundColor <- Drawing.Color.White
        err.BorderStyle <- BorderStyle.None
        err.DataSource <- null

        err.CellDoubleClick.Add(fun e ->
            let data = err.DataSource |> unbox<BindingSource>
            let element = data.[e.RowIndex] |> unbox<ErrorView>

            t.GoTo.Line(element.line - 1)
            let l = t.Lines.[element.line - 1]
            l.Select()
            ()
        )

        let rightSplitter = new SplitContainer()
        rightSplitter.Orientation <- Orientation.Horizontal
        
        rightSplitter.Dock <- DockStyle.Fill
        rightSplitter.FixedPanel <- FixedPanel.Panel2
        

        let l = createListView t

        let codeCache = System.Collections.Generic.Dictionary<ListViewItem, string>()
        let env = { currentItem = ref None; list = l; editor = t; codeCache = codeCache; errorView = err }

        installContextMenu env


        
        l.ItemSelectionChanged.Add(fun e ->
            if e.Item <> null then
                match !env.currentItem with
                    | Some item -> codeCache.[item] <- t.Text
                    | _ -> ()

                env.currentItem := Some e.Item
                match codeCache.TryGetValue e.Item with
                    | (true, c) ->
                        t.Text <- c
                        t.UndoRedo.EmptyUndoBuffer()
                    | _ ->
                        let tag = e.Item.Tag |> unbox<EffectNode>
                        let effect = tag.read() |> GLSL.run410
                        match effect with
                            | Success effect ->
                                let codes =
                                    effect.originals |> List.choose (fun s ->
                                        match s.debugInfo with
                                            | Some d -> Some d
                                            | None -> None
                                    )

                                //current := codes
                                let c = codes |> List.map (fun c -> c.functionCode) |> String.concat "\r\n"
                                t.Text <- c
                                codeCache.[e.Item] <- c
                                t.UndoRedo.EmptyUndoBuffer()


                            | _ -> ()
            else
                t.Text <- ""
                t.UndoRedo.EmptyUndoBuffer()
                env.currentItem := None
        )

        let save() = 
            let item = !env.currentItem
            match item with
                | Some item ->  
                    let current = item.Tag |> unbox<EffectNode>

                    let code = t.Text
                    codeCache.[item] <- code
                    let code = code.Replace("\t", "    ")
                    
                    let shaderStartRx = System.Text.RegularExpressions.Regex "(^|\n)(?<start>[^ \t\r\n]+)"
                    let indentRx = System.Text.RegularExpressions.Regex "^[ \t]*"
                    let splitShaderCode (c : string) =  
                        let lines = lineBreak.Split c
                        let result = System.Collections.Generic.List()
                        let mutable startLine = -1
                        let mutable currentIndent = 1
                        for i in 0..lines.Length-1 do
                            let l = lines.[i]
                            let m = indentRx.Match l
                            let indent = m.Length 

                            if l.Trim().Length <> 0 then
                                if currentIndent > 0 && indent = 0 then
                                    if startLine >= 0 then
                                        let sh = Array.sub lines startLine (i - startLine) |> String.concat "\r\n"
                                        result.Add sh
                                    startLine <- i
                                currentIndent <- indent

                        if startLine >= 0 then
                            let sh = Array.sub lines startLine (lines.Length - startLine) |> String.concat "\r\n"
                            result.Add sh

//                        let result = System.Collections.Generic.List()
//                        let mutable m = shaderStartRx.Match c
//                        let mutable start = -1
//                        while m.Success do
//                            if start >= 0 then
//                                let sh = c.Substring(start, m.Index - start)
//                                result.Add sh
//                            start <- m.Groups.["start"].Index
//                            m <- m.NextMatch()
//
//                        if start >= 0 then
//                            c.Substring(start, c.Length - start) |> result.Add

                        result |> Seq.toList
                    
                    let codes = splitShaderCode code
                    
                    item.ImageIndex <- 1
                    compileEffect env current codes true |> ignore
                | _ -> ()

        installSaveAction t save

  


        rightSplitter.Panel1.Controls.Add t
        rightSplitter.Panel2.Controls.Add err

        splitter.Panel1.Controls.Add l
        splitter.Panel2.Controls.Add rightSplitter

                
        form.Text <- "FShade Runtime Editor"
        form.Width <- 600
        form.Height <- 900
        rightSplitter.SplitterDistance <- form.ClientSize.Height - 150
        form.Show()
        form

                

    let show() = showUI() |> ignore

    let runTray() =
        #if DEPOLY
        ()
        #else

        let currentUI = ref None
        let openEditor _ =
            match !currentUI with
                | Some (ui : Form) when not ui.IsDisposed ->
                    if ui.Visible then ui.BringToFront()
                    else ui.Show()
                | _ ->
                    let ui = showUI()
                    currentUI := Some ui

        let contextMenu = new ContextMenuStrip()
        let showShaderEditor = new ToolStripButton("Show editor")
        contextMenu.Items.Add(showShaderEditor) |> ignore
        showShaderEditor.Click.Add openEditor


        let i = new NotifyIcon(
                    ContextMenuStrip = contextMenu,
                    Icon = FShade.Resources.MainIcon,
                    Text = "double click to show the shader editor",
                    Visible = true
                )

        i.DoubleClick.Add openEditor

        Application.ApplicationExit.Add(fun _ ->
            i.Visible <- false
            i.Icon <- null
            i.Text <- null
            i.Dispose()
        )

        
        #endif
