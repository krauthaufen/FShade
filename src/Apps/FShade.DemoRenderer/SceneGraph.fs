namespace FShade.Demo


open System
open System.Drawing
open Aardvark.Base
open FShade.Compiler
open FShade
open FShade.Debug

module SceneGraph =
    open FShade.Demo.OpenGlResourceManager
    open OpenTK.Graphics.OpenGL4
    open System.Drawing

    type TraversalState =
        { path : list<int>
          uniforms : Map<string, obj>
          textures : Map<string, int>
          samplerStates : Map<string, string * SamplerState>
          shader : OpenGlProgram
        } with
            member x.GetUniform(name : string) =
                x.uniforms.[name]

            static member Empty =
                { path = []; uniforms = Map.empty; textures = Map.empty; samplerStates = Map.empty; shader = Unchecked.defaultof<OpenGlProgram> }

    let mutable private currentNodeId = 0
    let getId() =
        System.Threading.Interlocked.Increment(&currentNodeId)

    type ISg =
        abstract member Render : TraversalState -> unit

    type UniformNode(values : list<string * obj>, child : ISg) =
        interface ISg with
            member x.Render (state : TraversalState) =
                let m = values |> Map.ofSeq
                let s = { state with uniforms = Map.union m state.uniforms }
                child.Render(s)

    type GeometryNode(attributes : Map<string, Array>, ?indices : Array) =
        let id = getId()
        let mutable vao = -1
        let mutable uniformBuffers = []
        let mutable samplers = []

        let isIndexed,faceVertexCount =
            match indices with
                | Some i -> true, i.Length
                | None ->
                    match Map.tryFind "Positions" attributes with
                        | Some p -> false, p.Length
                        | _ -> failwith "could not find positions for geometry"
                         
        member x.Prepare(state : TraversalState) =
            let optAttributes = state.shader.Inputs |> List.sortBy(fun a -> a.Index) |> List.map (fun a -> Map.tryFind a.Name attributes)
            if optAttributes |> List.forall (fun o -> o.IsSome) then
                let buffers = optAttributes |> List.map (fun o -> createBuffer o.Value) |> List.toArray
                let indexBuffer = match indices with | Some i -> createBuffer i | _ -> OpenGlBuffer(0, 0, VertexAttribPointerType.UnsignedInt)

                vao <- createVAO indexBuffer buffers

                uniformBuffers <- state.shader.Uniforms |> List.sortBy(fun b -> b.Index) |> List.map (fun b -> b.Index, createUniformBuffer b state.GetUniform)

                samplers <- state.shader.Samplers |> List.choose (fun s ->
                    match Map.tryFind s.Name state.samplerStates with
                        | Some (name, sam) -> 
                            match Map.tryFind name state.textures with
                                | Some tex -> 
                                    let sampler = createSampler sam
                                    (s.Offset, tex, sampler) |> Some


                                | None -> None
                        | None -> None
                )


            else
                failwith "could not find attributes"

            ()

        member x.Render(state : TraversalState) = 
            if vao < 0 then
                x.Prepare(state)

            for (i, b) in uniformBuffers do
                b.Layout |> List.iter (fun u -> b.Set(u.Name, state.GetUniform(u.Name)))
                b.Update()


            GL.BindVertexArray(vao)
            GL.UseProgram(state.shader.Handle)
            
            for (i,b) in uniformBuffers do
                GL.BindBufferRange(BufferRangeTarget.UniformBuffer, i, b.Handle, 0n, b.Size |> nativeint)

            for (i,t,s) in samplers do
                GL.ActiveTexture((int TextureUnit.Texture0 + i) |> unbox)
                GL.BindTexture(TextureTarget.Texture2D, t)
                GL.BindSampler(i, s)
           
            
            if isIndexed then
                GL.DrawElements(BeginMode.Points, faceVertexCount, DrawElementsType.UnsignedInt, 0)
            else
                GL.DrawArrays(PrimitiveType.Points, 0, faceVertexCount)

            for (i,_,_) in samplers do
                GL.ActiveTexture((int TextureUnit.Texture0 + i) |> unbox)
                GL.BindTexture(TextureTarget.Texture2D, 0)
                GL.BindSampler(i, 0)

            for (i, _) in uniformBuffers do
                GL.BindBufferRange(BufferRangeTarget.UniformBuffer, i, 0, 0n, 0n)


        interface ISg with
            member x.Render(state : TraversalState) = x.Render state
    
    type ShaderNode(shaderCode : string, uniforms : Map<string, UniformGetter>, child : ISg) =

        let mutable program = None
        let samplerStates =
            uniforms |> Map.toList 
                     |> List.filter (fun (k,u) -> u.IsSamplerUniform)
                     |> List.map (fun (k,v) -> k,v.Value) 
                     |> List.map (fun (k,v) -> k,v |> unbox<string * SamplerState>)
                     |> Map.ofList
        member x.Render (state : TraversalState) =
            let p =
                match program with
                    | None -> 
                        let p = createProgram shaderCode
                        program <- Some p
                        p
                    | Some p -> p

            child.Render({state with shader = p; samplerStates = samplerStates })

        


        interface ISg with
            member x.Render s = x.Render s

    type TextureNode(name : string, image : Bitmap, child : ISg) =
        let mutable tex = -1

        member x.Render(state : TraversalState) =
            if tex < 0 then
                tex <- createTexture image

            let s = { state with textures = Map.add name tex state.textures}
            child.Render(s)

        interface ISg with
            member x.Render s = x.Render s

    type Group(elements : seq<ISg>) =
        let l = obj()
        let elements = System.Collections.Generic.HashSet<ISg>(elements)

        member x.Add (s : ISg) =
            lock l (fun () -> elements.Add s |> ignore)

        member x.Remove (s : ISg) =
            lock l (fun () -> elements.Remove s |> ignore)

        member x.Render (state : TraversalState) =
            lock l (fun () -> elements |> Seq.iter (fun e -> e.Render(state)))

        interface ISg with
            member x.Render s = x.Render s


module Sg =
    open SceneGraph

    let shader (name : string) (e : Compiled<Effect, ShaderState>) (sg : ISg) =
        
        
        match GLSL.compileEffect e with
            | Success (uniforms, code) ->
                printfn "%s" code

                let g = Group []
                let node = ShaderNode(code, uniforms, sg)
                let currentNode = ref node
                let currentEffect = ref e
                g.Add node

                FShade.Debug.EffectEditor.register { name = name; read = (fun () -> !currentEffect); write = 
                    (fun e -> 
                        
                        match GLSL.compileEffect e with
                            | Success (uniforms, code) ->
                                currentEffect := e
                                g.Remove !currentNode
                                let n = ShaderNode(code, uniforms, sg)
                                currentNode := n
                                g.Add n

                            | _ -> ()
                    
                    ) }


                g :> ISg

            | Error e ->
                failwith e

    let texture (name : string) (bmp : Bitmap) (sg : ISg) =
        TextureNode(name, bmp, sg) :> ISg

    let fileTexture (name : string) (path : string) (sg : ISg) =
        let bmp = System.Drawing.Bitmap.FromFile path |> unbox<Bitmap>
        texture name bmp sg

    let geometry (indices : Option<Array>) (attributes : Map<string, Array>) =
        match indices with
            | Some indices -> GeometryNode(attributes, indices) :> ISg
            | None -> GeometryNode(attributes) :> ISg

    let group (elements : #seq<ISg>) =
        Group(elements) :> ISg

    let uniform (name : string) (value : 'a) (sg : ISg) =
        UniformNode([name, value :> obj], sg) :> ISg