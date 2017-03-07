namespace FShade

open System
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Reflection
open Aardvark.Base

[<AutoOpen>]
module FSharpTypeExt = 
    open System.Threading
    open System.Diagnostics
    open System.Collections.Concurrent

    type FSharpRecordField =
        {
            fieldName           : string
            fieldType           : Type
            fieldAttributes     : list<Type * list<obj>>
        }

    type FSharpUnionCase =
        {
            caseName            : string
            caseFields          : list<Option<string> * Type> 
        }

    type FSharpRecordDefinition =
        {
            recordName : string
            recordFields : list<FSharpRecordField>
        }

    type FSharpUnionDefinition =
        {
            unionName : string
            unionCases : list<FSharpUnionCase>
        }

    [<AutoOpen>]
    module private TypeBuilderExtensions = 
        let private ctorCompMapping = typeof<CompilationMappingAttribute>.GetConstructor [| typeof<SourceConstructFlags> |]
        let private ctorCompMapping2 = typeof<CompilationMappingAttribute>.GetConstructor [| typeof<SourceConstructFlags>; typeof<int> |]
        let private ctorCompMapping3 = typeof<CompilationMappingAttribute>.GetConstructor [| typeof<SourceConstructFlags>; typeof<int>; typeof<int> |]
        let private ctorDebuggerBrowse = typeof<System.Diagnostics.DebuggerBrowsableAttribute>.GetConstructor [| typeof<System.Diagnostics.DebuggerBrowsableState> |]
        let private ctorSerialize = typeof<System.SerializableAttribute>.GetConstructor [||]
        let private ctorNoComparison = typeof<NoComparisonAttribute>.GetConstructor [||]
        let private ctorNoEquality = typeof<NoEqualityAttribute>.GetConstructor [||]
        let private ctorCompilerGen = typeof<System.Runtime.CompilerServices.CompilerGeneratedAttribute>.GetConstructor [||]
        let private ctorNonUser = typeof<System.Diagnostics.DebuggerNonUserCodeAttribute>.GetConstructor [||]

        type TypeBuilder with
            member x.SetCompilationMapping(f : SourceConstructFlags) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping, [| f :> obj |]))

            member x.SetCompilationMapping(f : SourceConstructFlags, arg : int) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping2, [| f :> obj; arg :> obj |]))
                
            member x.SetCompilationMapping(f : SourceConstructFlags, a0 : int, a1 : int) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping3, [| f :> obj; a0 :> obj; a1 :> obj |]))

            member x.SetSerializable() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorSerialize, [||]))
                
            member x.SetCompilerGenerated() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompilerGen, [||]))
                
            member x.SetDebuggerNonUserCode() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorNonUser, [||]))


            member x.SetNoComparison() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorNoComparison, [||]))

            member x.SetNoEquality() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorNoEquality, [||]))

        type FieldBuilder with
            member x.SetCompilerGenerated() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompilerGen, [||]))

            member x.SetDebuggerBrowsable (state : DebuggerBrowsableState) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorDebuggerBrowse, [| state :> obj |]))
                
            member x.SetDebuggerNonUserCode() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorNonUser, [||]))

            member x.SetCompilationMapping(f : SourceConstructFlags, arg : int) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping2, [| f :> obj; arg :> obj |]))
                
            member x.SetCompilationMapping(f : SourceConstructFlags, a0 : int, a1 : int) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping3, [| f :> obj; a0 :> obj; a1 :> obj |]))

        type PropertyBuilder with
            member x.SetCompilerGenerated() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompilerGen, [||]))

            member x.SetDebuggerBrowsable (state : DebuggerBrowsableState) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorDebuggerBrowse, [| state :> obj |]))
                
            member x.SetDebuggerNonUserCode() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorNonUser, [||]))

            member x.SetCompilationMapping(f : SourceConstructFlags, arg : int) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping2, [| f :> obj; arg :> obj |]))
                
            member x.SetCompilationMapping(f : SourceConstructFlags, a0 : int, a1 : int) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping3, [| f :> obj; a0 :> obj; a1 :> obj |]))

        type MethodBuilder with
            member x.SetCompilerGenerated() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompilerGen, [||]))

            member x.SetDebuggerBrowsable (state : DebuggerBrowsableState) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorDebuggerBrowse, [| state :> obj |]))
                
            member x.SetDebuggerNonUserCode() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorNonUser, [||]))

            member x.SetCompilationMapping(f : SourceConstructFlags, arg : int) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping2, [| f :> obj; arg :> obj |]))
                
            member x.SetCompilationMapping(f : SourceConstructFlags, a0 : int, a1 : int) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping3, [| f :> obj; a0 :> obj; a1 :> obj |]))

        type ConstructorBuilder with
            member x.SetCompilerGenerated() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompilerGen, [||]))

            member x.SetDebuggerBrowsable (state : DebuggerBrowsableState) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorDebuggerBrowse, [| state :> obj |]))
                
            member x.SetDebuggerNonUserCode() = 
                x.SetCustomAttribute(CustomAttributeBuilder(ctorNonUser, [||]))

            member x.SetCompilationMapping(f : SourceConstructFlags, arg : int) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping2, [| f :> obj; arg :> obj |]))
                
            member x.SetCompilationMapping(f : SourceConstructFlags, a0 : int, a1 : int) =
                x.SetCustomAttribute(CustomAttributeBuilder(ctorCompMapping3, [| f :> obj; a0 :> obj; a1 :> obj |]))

    [<AutoOpen>]
    module private Dynamic = 
        let dAss = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName "FShade.Reflected", AssemblyBuilderAccess.RunAndSave)
        let dMod = dAss.DefineDynamicModule("MainModule")

        type SelfType = class end
        let selfType = typeof<SelfType>
        let recordCache = ConcurrentDictionary<FSharpRecordDefinition, Type>()
        let unionCache = ConcurrentDictionary<FSharpUnionDefinition, Type>()

        let mutable generation = -1

        let uniqueName(name : string) =
            let id = Interlocked.Increment(&generation)
            "Aardvark.Generated" + string id + "." + name

    type FSharpType with
        static member SelfType = selfType

        static member MakeRecord(def : FSharpRecordDefinition) : Type =
            recordCache.GetOrAdd(def, fun def ->
                let fields = def.recordFields
                let name = uniqueName def.recordName
                let dType = dMod.DefineType(name, TypeAttributes.Sealed ||| TypeAttributes.Class ||| TypeAttributes.Public)      

                let fieldType (f : FSharpRecordField) =
                    let t = f.fieldType
                    if t = selfType then dType :> Type
                    else t

                dType.SetCompilationMapping SourceConstructFlags.RecordType
                dType.SetSerializable()
                dType.SetNoComparison()
                dType.SetNoEquality()

                let dFields =
                    fields |> List.map (fun field ->                  
                        let dField = dType.DefineField(field.fieldName + "@", fieldType field, FieldAttributes.Assembly)
                        dField.SetDebuggerBrowsable DebuggerBrowsableState.Never
                        field, dField
                    )

                let mutable index = 0
                for (field, dField) in dFields do
                    let dProp = dType.DefineProperty(field.fieldName, PropertyAttributes.None, dField.FieldType, [||])
                
                    dProp.SetCompilationMapping(SourceConstructFlags.Field, index)

                    for (tAtt, args) in field.fieldAttributes do
                        let args = args |> List.toArray
                        let types = args |> Array.map (fun a -> a.GetType())
                        let ctor = tAtt.GetConstructor types

                        if isNull ctor then
                            failwithf "[Reflection] cannot set custom attriubte %A(%A)" tAtt args

                        dProp.SetCustomAttribute(
                            CustomAttributeBuilder(ctor, args)
                        )


                    let dGet = dType.DefineMethod("get_" + field.fieldName, MethodAttributes.Public ||| MethodAttributes.SpecialName, CallingConventions.HasThis, dField.FieldType, [||])
                    let il = dGet.GetILGenerator()
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldfld, dField)
                    il.Emit(OpCodes.Ret)

                    dProp.SetGetMethod dGet
                    index <- index + 1

                let dFieldTypes = fields |> List.map fieldType |> List.toArray
                let dCtor = dType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, dFieldTypes)

                let il = dCtor.GetILGenerator()
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
                let mutable arg = 1
                for (_,dField) in dFields do
                    il.Emit(OpCodes.Ldarg_0)
                    match arg with
                        | 0 -> il.Emit(OpCodes.Ldarg_0)
                        | 1 -> il.Emit(OpCodes.Ldarg_1)
                        | 2 -> il.Emit(OpCodes.Ldarg_2)
                        | 3 -> il.Emit(OpCodes.Ldarg_3)
                        | _ -> il.Emit(OpCodes.Ldarg_S, int16 arg)

                    il.Emit(OpCodes.Stfld, dField)
                    arg <- arg + 1

                il.Emit(OpCodes.Ret)

                dType.CreateType()
            )

        static member MakeUnion(def : FSharpUnionDefinition) =
            unionCache.GetOrAdd(def, fun def -> 
                let name = uniqueName def.unionName
                let cases = def.unionCases |> List.indexed
                let hasEmptyCases = cases |> List.exists (fun (_,c) -> c.caseFields |> List.isEmpty)

                let dTypeAtt =
                    if hasEmptyCases then TypeAttributes.Class
                    else TypeAttributes.Abstract ||| TypeAttributes.Class

                let dType = dMod.DefineType(name, dTypeAtt)      

                let realType (t : Type) =
                    if t = selfType then dType :> Type
                    else t

                dType.SetCompilationMapping SourceConstructFlags.SumType
                dType.SetSerializable()
                dType.SetNoComparison()
                dType.SetNoEquality()
                // [StructLayout(LayoutKind.Auto, CharSet = CharSet.Auto)]

                let dTagField = dType.DefineField("_tag", typeof<int>, FieldAttributes.Assembly ||| FieldAttributes.InitOnly)

                let dBaseCtor = dType.DefineConstructor(MethodAttributes.Assembly, CallingConventions.Standard, [| typeof<int> |])
                dBaseCtor.SetCompilerGenerated()
                dBaseCtor.SetDebuggerNonUserCode()
                let il = dBaseCtor.GetILGenerator()
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldarg_1)
                il.Emit(OpCodes.Stfld, dTagField)
                il.Emit(OpCodes.Ret)


                // define the nested 'Tags' type
                let dTags = dType.DefineNestedType("Tags", TypeAttributes.Class ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.NestedPublic)
                for (tag, case) in cases do
                    let dField = dTags.DefineField(case.caseName, typeof<int>, FieldAttributes.Literal ||| FieldAttributes.Public ||| FieldAttributes.Static)
                    dField.SetConstant(tag)

            

                let withNames (fields : list<Option<string> * Type>) =
                    let rec withNames (index : int) (l : list<Option<string> * Type>) =
                        match l with
                            | [] -> []
                            | (n,t) :: rest ->
                                match n with
                                    | Some n ->
                                        (n,t) :: withNames index rest
                                    | None ->
                                        let n = sprintf "Item%d" index
                                        (n,t) :: withNames (index + 1) rest

                    match fields with
                        | [] -> []
                        | [(n,t)] ->
                            [Option.defaultValue "Item" n, t]
                        | many ->
                            withNames 0 many

                let dCases =
                    cases |> List.map (fun (tag, case) ->
                        match case.caseFields with
                            | [] ->
                                tag, case, None
                            | _ -> 
                                let dCase = dType.DefineNestedType(case.caseName, TypeAttributes.NestedPublic ||| TypeAttributes.Class, dType)
                                dCase.SetSerializable()


                                let fields = withNames case.caseFields

                                let dFields =
                                    fields |> List.map (fun (name, t) ->
                                        let t = realType t
                                        let fName =
                                            let fName = name.ToLower()
                                            if fName = name then "_" + name
                                            else fName

                                        let dField = dCase.DefineField(fName, t, FieldAttributes.Assembly ||| FieldAttributes.InitOnly)

                                        dField.SetDebuggerBrowsable DebuggerBrowsableState.Never
                                        dField.SetCompilerGenerated()
                                        dField.SetDebuggerNonUserCode()

                                        name, dField
                                    )


                                let mutable index = 0
                                for (name, dField) in dFields do
                                    let dProp = dCase.DefineProperty(name, PropertyAttributes.None, dField.FieldType, [||])

                                    dProp.SetCompilationMapping(SourceConstructFlags.Field, tag, index)
                                    dProp.SetDebuggerNonUserCode()
                                    dProp.SetCompilerGenerated()

                                    let dGet = dCase.DefineMethod("get_" + name, MethodAttributes.Public ||| MethodAttributes.SpecialName, CallingConventions.HasThis, dField.FieldType, [||])
                                    let il = dGet.GetILGenerator()

                                    dGet.SetCompilerGenerated()
                                    dGet.SetDebuggerNonUserCode()

                                    il.Emit(OpCodes.Ldarg_0)
                                    il.Emit(OpCodes.Ldfld, dField)
                                    il.Emit(OpCodes.Ret)

                                    dProp.SetGetMethod dGet

                                    index <- index + 1


                                let fieldTypes = case.caseFields |> List.map snd |> List.toArray
                                let dCtor = dCase.DefineConstructor(MethodAttributes.Assembly, CallingConventions.Standard, fieldTypes)

                                dCtor.SetDebuggerNonUserCode()
                                dCtor.SetCompilerGenerated()

                                let il = dCtor.GetILGenerator()
                
                                il.Emit(OpCodes.Ldarg_0)
                                il.Emit(OpCodes.Ldc_I4, tag)
                                il.Emit(OpCodes.Call, dBaseCtor)

                                let mutable arg = 1
                                for (_, dField) in dFields do
                                    il.Emit(OpCodes.Ldarg_0)
                                    match arg with
                                        | 0 -> il.Emit(OpCodes.Ldarg_0)
                                        | 1 -> il.Emit(OpCodes.Ldarg_1)
                                        | 2 -> il.Emit(OpCodes.Ldarg_2)
                                        | 3 -> il.Emit(OpCodes.Ldarg_3)
                                        | _ -> il.Emit(OpCodes.Ldarg_S, int16 arg)

                                    il.Emit(OpCodes.Stfld, dField)

                                    arg <- arg + 1

                                il.Emit(OpCodes.Ret)
                            
    //                            let dCase = dCase.CreateType()
    //                            let dCtor = dCase.GetConstructor fieldTypes
                                tag, case, Some (dCase, dCtor)
                    )


                // int Tag { get; }
                let dTag = dType.DefineProperty("Tag", PropertyAttributes.None, typeof<int>, [||])
                dTag.SetDebuggerBrowsable DebuggerBrowsableState.Never
                dTag.SetDebuggerNonUserCode()
                dTag.SetCompilerGenerated()
                let dTagGet = dType.DefineMethod("get_Tag", MethodAttributes.Public ||| MethodAttributes.SpecialName, CallingConventions.HasThis, typeof<int>, [||])
                dTagGet.SetDebuggerNonUserCode()
                dTagGet.SetCompilerGenerated()
                let il = dTagGet.GetILGenerator()
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldfld, dTagField)
                il.Emit(OpCodes.Ret)
                dTag.SetGetMethod dTagGet

                // Is* { get; }
                for (tag, case, _) in dCases do
                    let dName = "Is" + case.caseName
                    let dIs = dType.DefineProperty(dName, PropertyAttributes.None, typeof<bool>, [||])
                    dIs.SetDebuggerBrowsable DebuggerBrowsableState.Never
                    dIs.SetDebuggerNonUserCode()
                    dIs.SetCompilerGenerated()

                    let dGet = dType.DefineMethod("get_" + dName, MethodAttributes.Public ||| MethodAttributes.SpecialName, CallingConventions.HasThis, typeof<bool>, [||])
                    dGet.SetDebuggerNonUserCode()
                    dGet.SetCompilerGenerated()
                    let il = dGet.GetILGenerator()
                    il.Emit(OpCodes.Ldarg_0)
                    il.EmitCall(OpCodes.Call, dTag.GetMethod, null)
                    il.Emit(OpCodes.Ldc_I4, tag)
                    il.Emit(OpCodes.Ceq)
                    il.Emit(OpCodes.Ret)

                    dIs.SetGetMethod dGet

                // New*(args)

                let dInitializers = 
                    dCases |> List.choose (fun (tag, case, info) ->
                        match info with
                            | Some (dCase, dCtor) -> 
                                let args = case.caseFields |> List.map (snd >> realType) |> List.toArray
                                let dNew = dType.DefineMethod("New" + case.caseName, MethodAttributes.Public ||| MethodAttributes.Static, dType, args)
                                dNew.SetCompilationMapping(SourceConstructFlags.UnionCase, tag)
                
                                let il = dNew.GetILGenerator()

                                for i in 0 .. args.Length - 1 do
                                    match i with
                                        | 0 -> il.Emit(OpCodes.Ldarg_0)
                                        | 1 -> il.Emit(OpCodes.Ldarg_1)
                                        | 2 -> il.Emit(OpCodes.Ldarg_2)
                                        | 3 -> il.Emit(OpCodes.Ldarg_3)
                                        | i -> il.Emit(OpCodes.Ldarg_S, int16 i)

                                il.Emit(OpCodes.Newobj, dCtor)
                                il.Emit(OpCodes.Ret)
                                None
                            | None ->
                                let dField = dType.DefineField("_unique_" + case.caseName, dType, FieldAttributes.InitOnly ||| FieldAttributes.Static ||| FieldAttributes.Assembly)
                                dField.SetDebuggerBrowsable DebuggerBrowsableState.Never
                                dField.SetDebuggerNonUserCode()
                                dField.SetCompilerGenerated()

                                let dProp = dType.DefineProperty(case.caseName, PropertyAttributes.None, dType, [||])
                                dProp.SetDebuggerBrowsable DebuggerBrowsableState.Never
                                dProp.SetDebuggerNonUserCode()
                                dProp.SetCompilerGenerated()
                                dProp.SetCompilationMapping(SourceConstructFlags.UnionCase, tag)
                
                                let dGet = dType.DefineMethod("get_" + case.caseName, MethodAttributes.Static |||  MethodAttributes.Public, dType, [||])
                                let il = dGet.GetILGenerator()
                                il.Emit(OpCodes.Ldsfld, dField)
                                il.Emit(OpCodes.Ret)
                        
                                dProp.SetGetMethod dGet
                                Some (dField, tag)
                        )

                match dInitializers with
                    | [] -> ()
                    | _ -> 
                        let dCtor = dType.DefineTypeInitializer()
                        let il = dCtor.GetILGenerator()
                        for (dField, tag) in dInitializers do
                            il.Emit(OpCodes.Ldc_I4, tag)
                            il.Emit(OpCodes.Newobj, dBaseCtor)
                            il.Emit(OpCodes.Stsfld, dField)


                let result = dType.CreateType()
                dTags.CreateType() |> ignore
                for (_,_,info) in dCases do
                    match info with
                        | Some(t,_) -> t.CreateType() |> ignore
                        | None -> ()
                result
            )

        static member MakeRecord(name : string, fields : list<FSharpRecordField>) =
            FSharpType.MakeRecord { recordName = name; recordFields = fields }

        static member MakeUnion(name : string, cases : list<FSharpUnionCase>) =
            FSharpType.MakeUnion { unionName = name; unionCases = cases }

        static member MakeRecord(name : string, fields : list<string * Type>) =
            let fields = fields |> List.map (fun (n,t) -> { fieldName = n; fieldType = t; fieldAttributes = [] })
            FSharpType.MakeRecord { recordName = name; recordFields = fields }
