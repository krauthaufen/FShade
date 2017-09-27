namespace FShade

open System
open FShade.Imperative
open Aardvark.Base


type EffectInputLayout =
    {
        eInputs          : MapExt<string, Type>
        eUniformBuffers  : MapExt<string, MapExt<string, Type>>
        eUniforms        : MapExt<string, Type>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EffectInputLayout =
    open Aardvark.Base.TypeInfo.Patterns

    let empty =
        { 
            eInputs = MapExt.empty
            eUniformBuffers = MapExt.empty
            eUniforms = MapExt.empty
        }

    let unify (l : EffectInputLayout) (r : EffectInputLayout) =
        let unifyTypes (l : Type) (r : Type) =
            if l = r then
                l
            else 
                failwithf "[FShade] cannot unify %A and %A" l r

        let unifyBuffers (l : MapExt<string, Type>) (r : MapExt<string, Type>) =
            MapExt.unionWith unifyTypes l r

        {
            eInputs = MapExt.unionWith unifyTypes l.eInputs r.eInputs
            eUniformBuffers = MapExt.unionWith unifyBuffers l.eUniformBuffers r.eUniformBuffers
            eUniforms = MapExt.unionWith unifyTypes l.eUniforms r.eUniforms
        }

    let ofModule (m : Module) =
        let mutable inputs          = MapExt.empty
        let mutable uniformBuffers  = MapExt.empty
        let mutable uniforms        = MapExt.empty
        
        for e in m.entries do
            let prev = e.decorations |> List.tryPick (function EntryDecoration.Stages { prev = prev } -> Some prev | _ -> None)

            match prev with
                | Some None ->
                    inputs <-
                        e.inputs
                            |> List.map (fun i -> i.paramSemantic, i.paramType)
                            |> MapExt.ofList
                | None ->
                    Log.warn "[FShade] cannot determine stage for EntryPoint"
                | _ ->
                    ()

            // process all uniforms
            for u in e.uniforms do
                match u.uniformBuffer with
                    | Some b ->
                        uniformBuffers <-
                            uniformBuffers |> MapExt.alter b (fun fields ->
                                defaultArg fields MapExt.empty
                                    |> MapExt.alter u.uniformName (fun oldType ->
                                        match oldType with
                                            | None -> Some u.uniformType
                                            | Some o ->
                                                if o <> u.uniformType then
                                                    failwithf "[FShade] shader uses uniform %s with conflicing types %A/%A" u.uniformName o u.uniformType
                                                Some u.uniformType
                                       )
                                    |> Some
                            )
                    | None ->
                        uniforms <-
                            uniforms |> MapExt.alter u.uniformName (fun oldType ->
                                match oldType with
                                    | None -> 
                                        Some u.uniformType
                                    | Some o -> 
                                        if o <> u.uniformType then
                                            failwithf "[FShade] shader uses uniform %s with conflicing types %A/%A" u.uniformName o u.uniformType
                                        Some u.uniformType
                            )

        {   
            eInputs = inputs
            eUniforms = uniforms
            eUniformBuffers = uniformBuffers
        }

    let ofModules (m : seq<Module>) =
        m |> Seq.map ofModule |> Seq.fold unify empty

    let apply (layout : EffectInputLayout) (m : Module) =
        let decorations (t : Type) =            
            match t with
                | ImageType(fmt,_,_,_,_) -> [Imperative.UniformDecoration.Format fmt]
                | _ -> []

        { m with
            entries =
                m.entries |> List.map (fun e ->
                    let prev = e.decorations |> List.tryPick (function EntryDecoration.Stages { prev = prev } -> Some prev | _ -> None)
            
                    let e = EntryPoint.setUniforms decorations layout.eUniforms layout.eUniformBuffers e

                    match prev with
                        | Some None -> 
                            EntryPoint.setInputs layout.eInputs e
                        | None ->
                            Log.warn "[FShade] cannot determine stage for EntryPoint"
                            e
                        | Some _ ->
                            e
                )
        }
