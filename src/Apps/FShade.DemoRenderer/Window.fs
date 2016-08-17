namespace FShade.Demo

open OpenTK
open OpenTK.Input
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open System.Diagnostics
open FShade.Demo.SceneGraph
open Aardvark.Base

module Camera =

    let projection (l : float) (r : float) (b : float) (t : float) (n : float) (f : float) =
        Trafo3d(
                M44d(
                    (2.0 * n) / (r - l),                     0.0,     (r + l) / (r - l),                   0.0,
                                    0.0,     (2.0 * n) / (t - b),     (t + b) / (t - b),                   0.0,
                                    0.0,                     0.0,           f / (n - f),     (f * n) / (n - f),
                                    0.0,                     0.0,                  -1.0,                   0.0
                ),                                                     
                                                                       
                M44d(                                      
                    (r - l) / (2.0 * n),                     0.0,                     0.0,     (r + l) / (2.0 * n),
                                    0.0,     (t - b) / (2.0 * n),                     0.0,     (t + b) / (2.0 * n),
                                    0.0,                     0.0,                     0.0,                    -1.0,
                                    0.0,                     0.0,       (n - f) / (f * n),                 1.0 / n
                )
            )

    let perspective (fov : float) (aspect : float) (near : float) (far : float) =
        let d = System.Math.Tan(Conversion.RadiansFromDegrees(fov) * 0.5) * near

        projection -d d (-d / aspect) (d / aspect) near far

    let lookAt (eye : V3d) (center : V3d) (up : V3d) =
        let f = center - eye |> Vec.normalize
        let r = Vec.cross f up |> Vec.normalize
        let u = Vec.cross r f |> Vec.normalize

        Trafo3d.ViewTrafo(eye, r, u, -f)


type Window() =
    inherit GameWindow(1024, 768, GraphicsMode(ColorFormat(32)), "FShade Demo Renderer", GameWindowFlags.Default, DisplayDevice.Default, 4, 0, GraphicsContextFlags.Default)
    let w = Stopwatch()
    let mutable sg : Option<ISg> = None

    let mutable location = V3d.III * 6.0
    let mutable forward = -V3d.III.Normalized
    let up = V3d.OOI
    let mutable viewTrafo : Trafo3d = Camera.lookAt location (location + forward) up
    
    let setCamera (loc : V3d) (fw : V3d) =
        location <- loc
        forward <- fw
        viewTrafo <- Camera.lookAt location (location + 10.0 * forward) up
    
    
    let mutable projTrafo : Trafo3d = Camera.perspective 60.0 1.0 0.1 100.0

    let scrollSpeed = 3.0
    let maxScrollSpeed = 8.0
    let scrollDamping = 0.02
    let moveSpeed = 2.0
    let lookSpeed = 0.006
    let zoomFactor = 0.03
    let panSpeed = 0.02

    let mutable leftDown = false
    let mutable rightDown = false
    let mutable middleDown = false
    let mutable zoomSpeed = 0.0
    let mutable moveForward = false
    let mutable moveBack = false
    let mutable moveRight = false
    let mutable moveLeft = false
    
    let mouseMove (e : MouseMoveEventArgs) =
        let delta = V2d(e.XDelta, e.YDelta)
        let r = Vec.cross forward up |> Vec.normalize
        let up = Vec.cross r forward |> Vec.normalize

        if leftDown then    
            let fw = (M44d.RotationZ(delta.X * -lookSpeed) * M44d.Rotation(r, delta.Y * -lookSpeed)).TransformDir(forward) |> Vec.normalize
            setCamera location fw
        elif rightDown then
            setCamera (location + forward * delta.Y * -zoomFactor) forward
        elif middleDown then
            setCamera (location + (up * -delta.Y + r * delta.X) * panSpeed) forward
        ()

    let mouseDown (e : MouseButtonEventArgs) =
        if e.Button = MouseButton.Left then leftDown <- true
        elif e.Button = MouseButton.Right then rightDown <- true
        elif e.Button = MouseButton.Middle then middleDown <- true

    let mouseUp (e : MouseButtonEventArgs) =
        if e.Button = MouseButton.Left then leftDown <- false
        elif e.Button = MouseButton.Right then rightDown <- false
        elif e.Button = MouseButton.Middle then middleDown <- false


    let mouseScroll (e : MouseWheelEventArgs) =
        let newSpeed = zoomSpeed + (e.DeltaPrecise.Sign() |> float) * scrollSpeed
        zoomSpeed <- Fun.Clamp(newSpeed, -maxScrollSpeed, maxScrollSpeed)

    let keyDown (e : KeyboardKeyEventArgs) =
        if e.Key = Key.W then moveForward <- true
        elif e.Key = Key.S then moveBack <- true
        elif e.Key = Key.A then moveLeft <- true
        elif e.Key = Key.D then moveRight <- true

    let keyUp (e : KeyboardKeyEventArgs) =
        if e.Key = Key.W then moveForward <- false
        elif e.Key = Key.S then moveBack <- false
        elif e.Key = Key.A then moveLeft <- false
        elif e.Key = Key.D then moveRight <- false

    let idle(dt : double) =
        if zoomSpeed.Abs() > 0.01 then
            let dz = zoomSpeed * dt
            setCamera (location + forward * dz) forward
            zoomSpeed <- zoomSpeed * Fun.Pow(scrollDamping, dt)

        let r = Vec.cross forward up |> Vec.normalize
        let xMove = 
            if moveLeft && moveRight then V3d.Zero
            elif moveLeft then -r
            elif moveRight then r
            else V3d.Zero

        let zMove = 
            if moveForward && moveBack then V3d.Zero
            elif moveForward then forward
            elif moveBack then -forward
            else V3d.Zero

        let move = (xMove + zMove) * moveSpeed * dt
        setCamera (location + move) forward

        ()

    do base.Mouse.Move.Add mouseMove
       base.Mouse.ButtonDown.Add mouseDown
       base.Mouse.ButtonUp.Add mouseUp
       base.Mouse.WheelChanged.Add mouseScroll
       base.Keyboard.KeyDown.Add keyDown
       base.Keyboard.KeyUp.Add keyUp
       base.Icon <- FShade.Resources.MainIcon

    member x.Scene
        with get() = sg.Value
        and set v = sg <- Some v

    override x.OnRenderFrame e =
        System.Windows.Forms.Application.DoEvents()

        idle w.Elapsed.TotalSeconds
        w.Restart()
        GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
        GL.Viewport(0,0,base.Width,base.Height)
        projTrafo <- Camera.perspective 60.0 (float base.Width / float base.Height) 0.1 100.0
        let uniforms = ["ModelTrafo", Trafo3d.Identity :> obj
                        "ViewTrafo", viewTrafo :> obj
                        "ProjTrafo", projTrafo :> obj
                        "ViewProjTrafo", (viewTrafo * projTrafo ) :> obj
                        "ViewportSize", V2i(base.Width, base.Height) :> obj
                        "CameraLocation", location :> obj
                        "LightLocation", (location + V3d.OOI) :> obj
                       ] |> Map.ofSeq

        let state = TraversalState.Empty
        let state = { state with uniforms = uniforms }

        match sg with
            | Some sg -> sg.Render(state)
            | _ -> ()

        base.SwapBuffers()