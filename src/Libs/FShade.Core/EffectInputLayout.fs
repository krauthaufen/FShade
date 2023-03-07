namespace FShade

open System
open FShade.Imperative
open Aardvark.Base
open System.IO
open System.Security.Cryptography
open System.Runtime.CompilerServices

type UniformLayout =
    {
        /// Type of the uniform.
        Type     : Type

        /// Name of containing uniform buffer.
        Buffer   : Option<string>

        /// Texture information.
        Textures : List<string * SamplerState>

        /// Stages in which the uniform is accessed.
        Stages   : Set<ShaderStage>
    }

type EffectInputLayout =
    {
        Inputs   : MapExt<string, Type>
        Uniforms : MapExt<string, UniformLayout>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EffectInputLayout =
    open Aardvark.Base.TypeInfo.Patterns

    module private UniformLayout =

        let ofUniform (stage : ShaderStage) (uniform : Uniform) =
            { Type     = uniform.uniformType
              Buffer   = uniform.uniformBuffer
              Textures = uniform.uniformTextureInfo |> List.map (fun (n, s) -> n, unbox<SamplerState> s)
              Stages   = Set.singleton stage }

    module private Unify =

        let types (l : Type) (r : Type) =
            match l, r with
            | _ when l = r -> l

            | VectorOf(_, fl), VectorOf(_, fr) when fl = fr ->
                raise <| NotImplementedException("Vector type unification not implemented.")

            | MatrixOf(_, fl), MatrixOf(_, fr) when fl = fr ->
                raise <| NotImplementedException("Matrix type unification not implemented.")

            | _ ->
                failwithf "[FShade] cannot unify types %A and %A" l r

        let private values (name : string) (l : 'T) (r : 'T) =
            if l = r then l
            else
                failwithf "[FShade] conflicting %s (%A and %A)" name l r

        let private textures (l : List<string * SamplerState>) (r : List<string * SamplerState>) =
            match l, r with
            | [], x | x, [] -> x
            | _ -> values "textures" l r

        let uniformLayouts (l : UniformLayout) (r : UniformLayout) =
            { Type = types l.Type r.Type
              Buffer = values "uniform buffer" l.Buffer r.Buffer
              Textures = textures l.Textures r.Textures
              Stages = Set.union l.Stages r.Stages }

    module private EntryPoint =

        let setInputs (inputs : MapExt<string, Type>) (e : EntryPoint) =
            let oldInputs =
                e.inputs |> List.map (fun input -> input.paramSemantic, input) |> MapExt.ofList

            let newInputs =
                (oldInputs, inputs) ||> MapExt.map2 (fun name i t ->
                    match t with
                    | Some t ->
                        match i with
                        | Some i ->
                            if i.paramType <> t then
                                failwithf "[FShade] shader input %A has type %A but is required to have %A" name i.paramType t
                            i
                        | None ->
                            { paramType = t
                              paramName = name
                              paramSemantic = name
                              paramDecorations = Set.ofList [ParameterDecoration.Interpolation InterpolationMode.Default] }
                    | None ->
                        failwithf "[FShade] shader uses input %s which is not given in layout" name
                )
                |> MapExt.toList |> List.map snd

            { e with inputs = newInputs }

        let private decorations (t : Type) =
            match t with
            | ImageType(fmt,_,_,_,_) -> [Imperative.UniformDecoration.Format fmt]
            | _ -> []

        let setUniforms (uniforms : MapExt<string, UniformLayout>) (e : EntryPoint) =
            let oldUniforms =
                e.uniforms |> List.map (fun uniform -> uniform.uniformName, uniform) |> MapExt.ofList

            let newUniforms =
                (oldUniforms, uniforms) ||> MapExt.map2 (fun name l r ->
                    match r with
                    | Some r ->
                        match l with
                        | Some l ->
                            if l.uniformType <> r.Type then
                                failwithf "[FShade] uniform %s has invalid type %A (expected: %A)" name l.uniformType r
                            l
                        | None ->
                            { uniformName = name
                              uniformType = r.Type
                              uniformDecorations = decorations r.Type
                              uniformBuffer = r.Buffer
                              uniformTextureInfo = r.Textures |> List.map (fun (n, s) -> n, box s) }
                    | None ->
                        failwithf "[FShade] shader requests uniform %s which is not part of layout" name
                )
                |> MapExt.toList |> List.map snd

            { e with uniforms = newUniforms }


    let empty : EffectInputLayout =
        { Inputs   = MapExt.empty
          Uniforms = MapExt.empty }

    /// Unifies two input layouts.
    /// Throws an exception if the layouts are not compatible.
    let unify (l : EffectInputLayout) (r : EffectInputLayout) =
        { Inputs   = (l.Inputs, r.Inputs) ||> MapExt.unionWith Unify.types
          Uniforms = (l.Uniforms, r.Uniforms) ||> MapExt.unionWith Unify.uniformLayouts }

    /// Returns the input layout of the given module.
    let ofModule (module_ : Module) =
        match module_.userData with
        | :? (obj * EffectInputLayout) as (_, layout) -> layout
        | _ ->
            let getStageDescription (e : EntryPoint) =
                e.decorations |> List.pick (function EntryDecoration.Stages s -> Some s | _ -> None)

            let first =
                module_.entries.Value |> List.find (fun e ->
                    let desc = getStageDescription e
                    desc.Previous = None
                )

            let inputs =
                first.inputs
                |> List.map (fun i -> i.paramSemantic, i.paramType)
                |> MapExt.ofList

            let uniforms =
                module_.entries.Value |> List.map (fun e ->
                    let desc = getStageDescription e

                    e.uniforms |> List.map (fun u ->
                        u.uniformName, UniformLayout.ofUniform desc.Stage u
                    )
                    |> MapExt.ofList
                )
                |> List.reduce (MapExt.unionWith Unify.uniformLayouts)

            { Inputs   = inputs
              Uniforms = uniforms }

    /// Returns the unified input layout of the given sequence of modules.
    /// Throws an exception if the modules are not compatible.
    let ofModules (modules : seq<Module>) =
        modules |> Seq.map ofModule |> Seq.fold unify empty

    /// Applies the given input layout to the given module.
    let apply (layout : EffectInputLayout) (module_ : Module) =
        match module_.userData with
        | :? (obj * EffectInputLayout) as (_, curr) when curr = layout ->
            module_

        | _ ->
            let entries =
                lazy (
                    module_.entries.Value |> List.map (fun e ->
                        let prev = e.decorations |> List.tryPick (function EntryDecoration.Stages s -> Some s.Previous | _ -> None)

                        let e = EntryPoint.setUniforms layout.Uniforms e

                        match prev with
                        | Some None ->  EntryPoint.setInputs layout.Inputs e
                        | None ->
                            Log.warn "[FShade] cannot determine stage for EntryPoint"
                            e
                        | _ ->
                            e
                    )
                )

            let userData =
                let original =
                    match module_.userData with
                    | :? (obj * EffectInputLayout) as (original, _)
                    | original -> original

                original, layout

            { module_ with
                entries = entries
                userData = userData }


[<Extension; Sealed; AbstractClass>]
type ModuleHashingExtensions() =

    static let serializeLayout (dst : BinaryWriter) (layout : EffectInputLayout) =
        let state = Serializer.Type.SerializerState()

        dst.Write layout.Inputs.Count
        for KeyValue(name, input) in layout.Inputs do
            dst.Write name
            Serializer.Type.serializeInternal state dst input

        dst.Write layout.Uniforms.Count
        for KeyValue(name, uniform) in layout.Uniforms do
            dst.Write name
            Serializer.Type.serializeInternal state dst uniform.Type

            match uniform.Buffer with
            | Some b ->
                dst.Write 1uy
                dst.Write b
            | _ ->
                dst.Write 0uy

            dst.Write uniform.Textures.Length
            for (n, s) in uniform.Textures do
                dst.Write n
                Serializer.Value.serialize dst typeof<SamplerState> s

            dst.Write uniform.Stages.Count
            for s in uniform.Stages do
                dst.Write (int8 s)

    /// Computes the hash of the input layout.
    [<Extension>]
    static member ComputeHash(this : EffectInputLayout) =
        use hash = SHA1.Create()
        use ms = new MemoryStream()
        use h = new CryptoStream(ms, hash, CryptoStreamMode.Write)
        use w = new BinaryWriter(h, System.Text.Encoding.UTF8, true)

        serializeLayout w this

        h.FlushFinalBlock()
        hash.Hash |> Convert.ToBase64String

    /// Computes the hash of the module, taking applied input layouts into consideration.
    [<Extension>]
    static member ComputeHash(this : Module) =
        match this.userData with
        | :? (obj * EffectInputLayout) as (_, layout) ->
            this.hash + layout.ComputeHash()
        | _ ->
            this.hash