namespace FShade.SpirV.New

#nowarn "9"
#nowarn "51"

open System
open System.Reflection
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Aardvark.Base
open Aardvark.Base.TypeInfo.Patterns
open SpirV
open FShade.Compiler.ExpressionExtensions.Patterns
open FShade
open FShade.Compiler


type Label(name : string) =
    let id = System.Guid.NewGuid()
    let name = if System.String.IsNullOrEmpty name then id.ToString() else name

    member x.Id = id
    member x.Name = name

    interface System.IComparable with
        member x.CompareTo(o) =
            match o with
                | :? Label as o -> compare id o.Id
                | _ -> failwith "uncomparable"

    override x.GetHashCode() = id.GetHashCode()

    override x.Equals o =
        match o with
            | :? Label as o -> id = o.Id
            | _ -> false

    new() = Label(System.Guid.NewGuid().ToString())



module InstructionPrinter =
    

    [<StructuralComparison; StructuralEquality>]
    type SpirVType =
        | Void
        | Bool
        | Sampler
        | Function of args : list<SpirVType> * retType : SpirVType
        | Int of width : int * signed : bool
        | Float of width : int
        | Vector of compType : SpirVType * dim : int
        | Matrix of colType : SpirVType * dim : int
        | Array of elementType : SpirVType * length : int
        | Struct of name : string * fields : list<SpirVType * string>
        | Image of sampledType : SpirVType * dim : Dim * depth : int * arrayed : bool * ms : int * sampled : bool * format : int
        | SampledImage of SpirVType
        | Ptr of StorageClass * SpirVType

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SpirVType =

        let Bool = Bool
        let Void = Void
        let Unit = Void

        let Int8 = Int(8, true)
        let UInt8 = Int(8, false)

        let Int16 = Int(16, true)
        let UInt16 = Int(16, false)

        let Int32 = Int(32, true)
        let UInt32 = Int(32, false)
    
        let Float32 = Float(32)
        let Float64 = Float(64)

        let V2f = Vector(Float32, 2)
        let V3f = Vector(Float32, 3)
        let V4f = Vector(Float32, 4)

        let V2d = Vector(Float64, 2)
        let V3d = Vector(Float64, 3)
        let V4d = Vector(Float64, 4)


        let M22f = Matrix(V2f, 2)
        let M33f = Matrix(V3f, 3)
        let M44f = Matrix(V4f, 4)
        let M23f = Matrix(V2f, 3)
        let M34f = Matrix(V3f, 4)
    
        let M22d = Matrix(V2d, 2)
        let M33d = Matrix(V3d, 3)
        let M44d = Matrix(V4d, 4)
        let M23d = Matrix(V2d, 3)
        let M34d = Matrix(V3d, 4)


    let rec private buildNameMaps (current : Map<uint32, string>) (currentMembers : Map<uint32 * uint32, string>) (s : list<Instruction>) =
        match s with
            | [] -> current, currentMembers
            | OpName(id, name) :: rest -> buildNameMaps (Map.add id name current) currentMembers rest
            | OpMemberName(id, idx, name) :: rest -> buildNameMaps current (Map.add (id,idx) name currentMembers) rest
            | _ :: rest -> buildNameMaps current currentMembers rest

    let rec private buildTypeMap (names : Map<uint32, string>) (memberNames : Map<uint32 * uint32, string>) (current : Map<uint32, SpirVType>) (s : list<Instruction>) =
        let buildTypeMap c s = buildTypeMap names memberNames c s

        match s with
            | [] -> current
            | OpTypeVoid id :: s -> buildTypeMap (Map.add id Void current) s
            | OpTypeBool id :: s -> buildTypeMap (Map.add id Bool current) s
            | OpTypeInt(id,w,s) :: rest -> buildTypeMap (Map.add id (Int(int w, if s = 1u then true else false)) current) rest
            | OpTypeFloat(id,w) :: rest -> buildTypeMap (Map.add id (Float(int w)) current) rest
            | OpTypeVector(id,inner, dim) :: rest -> 
                let inner = current.[inner]
                buildTypeMap (Map.add id (Vector(inner, int dim)) current) rest
            | OpTypeMatrix(id,inner, dim) :: rest -> 
                let inner = current.[inner]
                buildTypeMap (Map.add id (Matrix(inner, int dim)) current) rest

            | OpTypeArray(id,inner, dim) :: rest -> 
                let inner = current.[inner]
                buildTypeMap (Map.add id (Array(inner, int dim)) current) rest

            | OpTypePointer(id,c, inner) :: rest -> 
                let inner = current.[inner]
                buildTypeMap (Map.add id (Ptr(c, inner)) current) rest

            | OpTypeStruct(id, fields) :: rest ->
                let getMemberName (i : int) =
                    match Map.tryFind (id, uint32 i) memberNames with
                        | Some n -> n
                        | None -> sprintf "f%d" i

                let name =
                    match Map.tryFind id names with
                        | Some n -> n
                        | _ -> sprintf "struct%d" (int id)

                let fieldTypes = fields |> Array.mapi (fun i id -> (match Map.tryFind id current with | Some a -> a | None -> SpirVType.Unit), getMemberName i) |> Array.toList
                let t = Struct(name, fieldTypes)
                buildTypeMap (Map.add id t current) rest

            | OpTypeFunction(id, ret, args) :: rest ->
                let args = args |> Array.map (fun a -> current.[a]) |> Array.toList
                let ret = current.[ret]

                buildTypeMap (Map.add id (Function(args, ret)) current) rest

            | OpTypeSampler(id) :: rest ->
                buildTypeMap (Map.add id Sampler current) rest
                

            | OpTypeImage(id,a,b,c,d,e,f,g,h) :: rest ->
                let t = Image(current.[a], b, int c, (if d = 1u then true else false), int e, (if f = 1u then true else false), int g)
                buildTypeMap (Map.add id t current) rest
                
            | OpTypeSampledImage(id, t) :: rest ->
                buildTypeMap (Map.add id (SampledImage(current.[t])) current) rest

            | i::rest -> buildTypeMap current rest


    let rec private shortTypeName (t : SpirVType) =
        match t with
            
            | Bool -> "bool"
            | Void -> "void"
            | Sampler -> "sam"
            | Image(_) -> "img"

            | Int(8, false) -> "byte"
            | Int(8, true) -> "sbyte"
            | Int(32, true) -> "int"
            | Float(16) -> "half"
            | Float(32) -> "float"
            | Float(64) -> "double"

            | Int(w,true) -> sprintf "int%d" w
            | Int(w,false) -> sprintf "uint%d" w


            | Vector(Float(32), d) -> sprintf "fvec%d" d
            | Vector(Float(64), d) -> sprintf "dvec%d" d
            | Vector(Int(32,true), d) -> sprintf "ivec%d" d
            | Vector(Int(32,false), d) -> sprintf "uvec%d" d
            | Vector(Bool, d) -> sprintf "bvec%d" d

            | Struct(name,_) -> sprintf "str %s" name


            | Matrix(Vector(Float(32), r), c) -> 
                if r = c then sprintf "fmat%d" r
                else sprintf "fmat%d%d" r c

            | Matrix(Vector(Float(64), r), c) -> 
                if r = c then sprintf "dmat%d" r
                else sprintf "dmat%d%d" r c

            | Array(t, l) ->
                sprintf "%s[%d]" (shortTypeName t) l

            | Ptr(_,t) -> sprintf "%s*" (shortTypeName t)
            | _ -> sprintf "%A" t


    let toString (s : seq<Instruction>) =
        let s = Seq.toList s


        let names, memberNames = buildNameMaps Map.empty Map.empty s

        let typeMap = buildTypeMap names memberNames Map.empty s

        let mutable names = names
        for (id,t) in Map.toList typeMap do
            names <- Map.remove id names



        let printOperand (o : obj) =
            match o with
                | null -> "None"
                | :? uint32 as o ->
                    match Map.tryFind o names with
                        | Some n -> sprintf "%s(%s)" (string o) n
                        | None -> string o

                | _ ->
                    sprintf "%A" o

        let data = 
            s |> List.map (fun i ->
                let id = i.ResultId |> Option.map string
                let tid = i.ResultType |> Option.map (fun i -> sprintf "%d(%s)" i (shortTypeName typeMap.[i]))
                let name = i.Name
                let args = i.Operands |> List.map printOperand |> String.concat " "

                (i, id,tid,name,args)
            )

        let longestId = data |> List.choose (fun (_,i,_,_,_) -> i) |> List.map String.length |> List.max
        let longestType = data |> List.choose (fun (_,_,i,_,_) -> i) |> List.map String.length |> List.max


        let padLeft (l : int) (s : string) =
            if s.Length < l then System.String(' ', l - s.Length) + s
            else s

        let padRight (l : int) (s : string) =
            if s.Length < l then s + System.String(' ', l - s.Length)
            else s

        let mutable indent = ""

        let lines =
            data |> List.map (fun (o,i,t,n,a) ->
                let i = padLeft (longestId + 1) (match i with | Some i -> i + ":" | None -> " ")
                let t = padLeft longestType (match t with | Some i -> i | None -> "")

                match o with
                    | OpFunctionEnd -> indent <- indent.Substring(2)
                    | _ -> ()

                let res = sprintf "%s   %s %s%s %s" i t indent n a

                match o with
                    | OpFunction(_) -> indent <- indent + "  "
                    | _ -> ()

                res
            )

        String.concat "\r\n" lines

[<AutoOpen>]
module SpirVBuilders =
    
    let (|ExitInstruction|_|) (i : Instruction) =
        match i with
            | OpReturn -> Some()
            | OpReturnValue _ -> Some()
            | OpBranch _ -> Some()
            | OpBranchConditional _ -> Some()
            | OpKill -> Some ()
            | OpUnreachable -> Some ()
            | OpSwitch(_,_,_) -> Some ()
            | _ -> None


    type RevList<'a> =
        | Snoc of RevList<'a> * 'a
        | Nil

    module RevList =
        let empty<'a> : RevList<'a> = Nil

        let toList (l : RevList<'a>) =
            let rec acc (c : list<'a>) (l : RevList<'a>) =
                match l with
                    | Nil -> c
                    | Snoc(l,v) ->
                        acc (v::c) l
                         
            acc [] l

        let snoc (l : RevList<'a>) (v : 'a) =
            Snoc(l,v)


    type Scope = { startLabel : uint32; endLabel : uint32 }

    type VariableId =
        | LocationId of uint32
        | ValueId of uint32

    type SpirVState = 
        {
            currentId : uint32
            glslExtId : uint32
            typeInstructions : RevList<Instruction>
            instructions : RevList<Instruction>
            typeCache : HashMap<Type, uint32>
            labelIds : Map<Label, uint32>
            variableIds : Map<Var, VariableId>
            variableDeclarationLabels : Map<Var, uint32>
            currentScope : list<Scope>
      
        } with
            
            static member Empty = 
                {
                    currentId = 1u
                    glslExtId = 0u
                    typeInstructions = Nil
                    instructions = Nil
                    typeCache = HashMap.empty
                    labelIds = Map.empty
                    variableIds = Map.empty
                    variableDeclarationLabels = Map.empty
                    currentScope = []
                }


    type SpirV<'a>= { build : SpirVState -> SpirVState * 'a }


    let newId = { build = fun s -> { s with currentId = s.currentId + 1u}, s.currentId}

    let setGlslId (id : uint32) =
        { build = fun s ->
            { s with glslExtId = id }, ()
        }

    let glslId =
        { build = fun s ->
            s, s.glslExtId
        }

    let lastInstruction =
        { build = fun s ->
            s, (match s.instructions with | Snoc(_,i) -> Some i | Nil -> None)
        }

    let tryFindTypeId (t : Type) =
        { build = fun s ->
            match HashMap.tryFind t s.typeCache with
                | Some id -> s, Some id
                | _ -> s, None
        }

    let setTypeId (t : Type) (id : uint32) =
        { build = fun s ->
            { s with typeCache = HashMap.add t id s.typeCache }, ()
        }

    let pushScope newScope =
        { build = fun s ->
            { s with currentScope = newScope::s.currentScope }, ()
        }

    let popScope =
        { build = fun s ->
            match s.currentScope with
                | _::scope -> { s with currentScope = scope }, ()
                | _ -> s,()
        }

    let currentScope =
        { build = fun s ->
            match s.currentScope with
                | scope::_ -> s, Some scope
                | _ -> s, None
        }

    let getLabelId (l : Label) =
        { build = fun s ->
            match Map.tryFind l s.labelIds with
                | Some id -> 
                    s,id
                | None ->
                    let (s, id) = newId.build s
                    { s with labelIds = Map.add l id s.labelIds }, id
        }

    let setLabelId (l : Label) (id : uint32) =
        { build = fun s ->
            { s with labelIds = Map.add l id s.labelIds }, ()
        }

    let newMutableVarId (v : Var) =
        { build = fun s ->
            let (s, id) = newId.build s

            { s with 
                variableIds = Map.add v (LocationId id) s.variableIds
            }, id
        }

    let getVarId (v : Var) =
        { build = fun s ->
            match Map.tryFind v s.variableIds with
                | Some id -> 
                    s,id
                | _ ->
                    failwith "cannot get location for immutable variable"
        }


    let setVarId (v : Var) (id : uint32) =
        { build = fun s ->
            { s with 
                variableIds = Map.add v (ValueId id) s.variableIds
            }, ()
        }

    let fail str : SpirV<'a> =
        failwith str

    type SpirVBuilder() =

        member x.Yield(i : Instruction) =
            { build = fun s ->
                { s with instructions = Snoc(s.instructions, i) }, ()
            }

        member x.YieldFrom(i : seq<Instruction>) =
            { build = fun s ->
                let mutable c = s.instructions
                for e in i do c <- Snoc(c, e)
                { s with instructions = c }, ()
            }


        member x.Bind(m : SpirV<'a>, f : 'a -> SpirV<'b>) =
            { build = fun s ->
                let (s,v) = m.build s
                (f v).build s
            }

        member x.Zero() =
            { build = fun s -> s, () }


        member x.Combine(l : SpirV<unit>, r : SpirV<'a>) =
            { build = fun s -> 
                let (s, ()) = l.build s
                r.build s
            }

        member x.Delay(f : unit -> SpirV<'a>) =
            { build = fun s ->
                (f ()).build s
            }

        member x.For(seq : seq<'a>, f : 'a -> SpirV<unit>) =
            { build = fun s ->
                let mutable c = s
                for e in seq do
                    let (s,()) = (f e).build c
                    c <- s
                c, ()
            } 


        member x.Return(v : 'a) =
            { build = fun s -> s, v }

        member x.ReturnFrom(v : SpirV<'a>) =
            v

    let spirv = SpirVBuilder()


    type SpirVTypeBuilder() =

        member x.Yield(i : Instruction) =
            { build = fun s ->
                { s with typeInstructions = Snoc(s.typeInstructions, i) }, ()
            }

        member x.YieldFrom(i : seq<Instruction>) =
            { build = fun s ->
                let mutable c = s.typeInstructions
                for e in i do c <- Snoc(c, e)
                { s with typeInstructions = c }, ()
            }


        member x.Bind(m : SpirV<'a>, f : 'a -> SpirV<'b>) =
            { build = fun s ->
                let (s,v) = m.build s
                (f v).build s
            }

        member x.Zero() =
            { build = fun s -> s, () }


        member x.Combine(l : SpirV<unit>, r : SpirV<'a>) =
            { build = fun s -> 
                let (s, ()) = l.build s
                r.build s
            }

        member x.Delay(f : unit -> SpirV<'a>) =
            { build = fun s ->
                (f ()).build s
            }

        member x.For(seq : seq<'a>, f : 'a -> SpirV<unit>) =
            { build = fun s ->
                let mutable c = s
                for e in seq do
                    let (s,()) = (f e).build c
                    c <- s
                c, ()
            } 


        member x.Return(v : 'a) =
            { build = fun s -> s, v }

        member x.ReturnFrom(v : SpirV<'a>) =
            v

    let spirvType = SpirVTypeBuilder()



    module List =
        let mapSpv (f : 'a -> SpirV<'b>) (l : list<'a>) =
            { build = fun s ->
                let mutable c = s
                let mutable res = Nil
                for e in l do
                    let (s,e) = (f e).build c
                    c <- s
                    res <- Snoc(res, e)
                c, RevList.toList res
            }

        let mapSpvi (f : int -> 'a -> SpirV<'b>) (l : list<'a>) =
            { build = fun s ->
                let mutable c = s
                let mutable i = 0
                let mutable res = Nil
                for e in l do
                    let (s,e) = (f i e).build c
                    c <- s
                    i <- i + 1
                    res <- Snoc(res, e)
                c, RevList.toList res
            }

    module Seq =
        let mapSpv (f : 'a -> SpirV<'b>) (l : seq<'a>) =
            { build = fun s ->
                let mutable c = s
                let mutable res = Nil
                for e in l do
                    let (s,e) = (f e).build c
                    c <- s
                    res <- Snoc(res, e)
                c, RevList.toList res
            }

module SpirVCompiler =

    let doubleAsFloat = true

    [<AutoOpen>]
    module private Utils =
        type ptr<'a> = Ptr
        type parameter<'a> = Input
        type input<'a> = Input
        type output<'a> = Output
        type uniform<'a> = Unif

        let makePtrType (t : Type) =
            typedefof<ptr<_>>.MakeGenericType [|t|]

        let makeParameterType (t : Type) =
            typedefof<parameter<_>>.MakeGenericType [|t|]

        let makeInputType (t : Type) =
            typedefof<input<_>>.MakeGenericType [|t|]

        let makeOutputType (t : Type) =
            typedefof<input<_>>.MakeGenericType [|t|]

        let makeUniformType (t : Type) =
            typedefof<uniform<_>>.MakeGenericType [|t|]


        let (|Ptr|_|) (t : Type) =
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<ptr<_>> then
                Some (t.GetGenericArguments().[0], StorageClass.Function)
            elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<parameter<_>> then
                Some (t.GetGenericArguments().[0], StorageClass.Function)
            elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<input<_>> then
                Some (t.GetGenericArguments().[0], StorageClass.Input)
            elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<output<_>> then
                Some (t.GetGenericArguments().[0], StorageClass.Output)
            elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<uniform<_>> then
                Some (t.GetGenericArguments().[0], StorageClass.Uniform)
            else 
                None

        let getDwords (v : obj) =
            let t = v.GetType()
            let size = (Marshal.SizeOf t + 3) &&& ~~~3
            let cnt = size / 4
            let ptr : nativeptr<uint32> = NativePtr.stackalloc cnt
            Marshal.StructureToPtr(v, NativePtr.toNativeInt ptr, false)
            ptr |> NativePtr.toArray cnt


        type BinaryOperation =
            | Add
            | Sub
            | Mul
            | Div
            | Mod

            | GreaterOrEqual
            | Greater
            | SmallerOrEqual
            | Smaller
            | Equal
            | NotEqual

            | BitAnd | BitOr | BitXor | LeftShift | RightShift
        
        type UnaryOperation =
            | Negate
            | Not


        let (|BinaryOperator|_|) (mi : MethodInfo) =
            if mi.IsStatic then
                match mi with
                    | MethodQuote <@ (+) : int -> int -> int @> _ -> Some Add
                    | MethodQuote <@ (-) : int -> int -> int @> _ -> Some Sub
                    | MethodQuote <@ (*) : int -> int -> int @> _ -> Some Mul
                    | MethodQuote <@ (/) : int -> int -> int @> _ -> Some Div
                    | MethodQuote <@ (%) : int -> int -> int @> _ -> Some Mod

                    | MethodQuote <@ (>=) : int -> int -> bool @> _ -> Some GreaterOrEqual
                    | MethodQuote <@ (>) : int -> int -> bool @> _ -> Some Greater
                    | MethodQuote <@ (<=) : int -> int -> bool @> _ -> Some SmallerOrEqual
                    | MethodQuote <@ (<) : int -> int -> bool @> _ -> Some Smaller
                    | MethodQuote <@ (=) : int -> int -> bool @> _ -> Some Equal
                    | MethodQuote <@ (<>) : int -> int -> bool @> _ -> Some NotEqual

                    | MethodQuote <@ (&&&) : int -> int -> int @> _ -> Some BitAnd
                    | MethodQuote <@ (|||) : int -> int -> int @> _ -> Some BitOr
                    | MethodQuote <@ (^^^) : int -> int -> int @> _ -> Some BitXor
                    | MethodQuote <@ (<<<) : int -> int -> int @> _ -> Some LeftShift
                    | MethodQuote <@ (>>>) : int -> int -> int @> _ -> Some RightShift
                    | _ -> None
            else
                None

        let (|UnaryOperator|_|) (mi : MethodInfo) =
            if mi.IsStatic then
                match mi with
                    | MethodQuote <@ (~-) : int -> int @> _ -> Some(Negate)
                    | MethodQuote <@ not : bool -> bool @> _ -> Some(Not)
                    | _ -> None
            else
                None

        let (|SignedIntegral|_|) (t : Type) =
            match t with
                | SByte | Int16 | Int32 | Int64 -> Some()
                | _ -> None

        let (|UnsignedIntegral|_|) (t : Type) =
            match t with
                | Byte | UInt16 | UInt32 | UInt64 -> Some()
                | _ -> None


        type FakeCompiler() =
            interface ICompiler<ShaderState> with
                member x.CompileIntrinsicType(_) = failwith ""
                member x.CompileIntrinsicFunction(_) = failwith ""
                member x.CompileIntrinsicFunctionDefinition(_) = failwith ""
                member x.CompileIntrinsicPropertyGet(_) = failwith ""
                member x.CompileIntrinsicPropertySet(_) = failwith ""
                member x.CompileIntrinsicConstructor(_) = failwith ""
                member x.CompileFunctionDeclaration _ _ _ _ = failwith ""
                member x.CompileTypeDeclaration _ _ = failwith ""
                member x.CompileVariableDeclaration _ _ _ = failwith ""
                member x.CompileConstantDeclaration _ _ _ = failwith ""
                member x.CompileFieldDeclaration _ = failwith ""
                member x.CompileApplication _ _ _ = failwith ""
                member x.CompilePreamble() = failwith ""

                member x.CompileValue _ _ = failwith ""
                member x.FilterFunctionArguments _ = failwith ""
                member x.ProcessCode  _ = failwith ""
                member x.ProcessFunctionBody e = compile { return e }
                member x.InitialState() = ShaderState.emptyShaderState
                member x.ResetState s = { emptyShaderState with counters = s.counters }

    type CustomField(parent : Type, index : int, name : string, fieldType : Type) =
        inherit FieldInfo()

        member x.Index = index

        override x.Name = name
        override x.FieldType = fieldType
        override x.DeclaringType = parent
        override x.Attributes = FieldAttributes.Public

        override x.FieldHandle = failwith "not supported"
        override x.GetValue(_) = failwith "not supported"
        override x.SetValue(_,_,_,_,_) = failwith "not supported"
        override x.ReflectedType = failwith "not supported"
        override x.GetCustomAttributes(_) = failwith "not supported"
        override x.GetCustomAttributes(_,_) = failwith "not supported"
        override x.IsDefined(_,_) = failwith "not supported"

    type CustomStruct(name : string, fields : list<string * Type>) as this =
        inherit Type()
        let guid = Guid.NewGuid()

        let fieldMap = fields |> List.mapi (fun i (n,t) -> n, CustomField(this, i, n, t)) |> Map.ofList


        override x.GUID = guid
        override x.Name = name
        override x.FullName = name
        override x.IsArrayImpl() = false
        override x.IsByRefImpl() = false
        override x.IsPointerImpl() = false
        override x.IsPrimitiveImpl() = false
        override x.IsCOMObjectImpl() = false
        override x.HasElementTypeImpl() = false
        override x.Assembly = typeof<CustomStruct>.Assembly
        override x.Module = typeof<CustomStruct>.Module
        override x.UnderlyingSystemType = x :> Type
        override x.BaseType = typeof<obj>

        override x.GetField(n,_) = 
            match Map.tryFind n fieldMap with
                | Some f -> f :> FieldInfo
                | _ -> null

        override x.GetFields(_) =
            fieldMap |> Map.toArray |> Array.map (fun (_,f) -> f :> FieldInfo)

        override x.GetCustomAttributes(_,_) = [||]
        override x.GetCustomAttributes(_) = [||]
        override x.IsDefined(_,_) = false

        override x.Namespace = failwith "not supported"
        override x.AssemblyQualifiedName = failwith "not supported"
        override x.InvokeMember(_,_,_,_,_,_,_,_) = failwith "not supported"
        override x.GetConstructorImpl(_,_,_,_,_) = failwith "not supported"
        override x.GetConstructors(_) = failwith "not supported"
        override x.GetMethodImpl(_,_,_,_,_,_) = failwith "not supported"
        override x.GetMethods(_) = failwith "not supported"
        override x.GetInterface(_,_) = failwith "not supported"
        override x.GetInterfaces() = failwith "not supported"
        override x.GetEvent(_,_) = failwith "not supported"
        override x.GetEvents(_) = failwith "not supported"
        override x.GetPropertyImpl(_,_,_,_,_,_) = failwith "not supported"
        override x.GetProperties(_) = failwith "not supported"
        override x.GetNestedType(_,_) = failwith "not supported"
        override x.GetNestedTypes(_) = failwith "not supported"
        override x.GetMembers(_) = failwith "not supported"
        override x.GetAttributeFlagsImpl() = failwith "not supported"
        override x.GetElementType() = failwith "not supported"



        member x.Fields = fields


    let rec compileType (t : Type) =
        spirvType {
            let! cache = tryFindTypeId t
            match cache with
                | Some id -> return id
                | None ->
                    let! id = newId
                    do! setTypeId t id

                    match t with
                        | :? CustomStruct as s ->
                            let! fieldTypes = s.Fields |> List.mapSpv (fun (n,t) -> compileType t)
                            yield OpTypeStruct(id, fieldTypes |> List.toArray)
                                    
                            yield OpName(id, s.Name)
                            for (i,(n,_)) in s.Fields |> List.mapi (fun i f -> (i,f)) do
                                yield OpMemberName(id, uint32 i, n)
                                            
                            return id
                                                                
                        | _ ->

                            let rec argsAndRet (t : Type) =
                                try
                                    if FSharpType.IsFunction t then
                                        let arg, rest = FSharpType.GetFunctionElements t
                                        let args, ret = argsAndRet rest
                                        arg::args, ret
                                    else
                                        [], t
                                with e ->
                                    [], t

                            let args, ret = argsAndRet t

                            match args with
                                | [] ->
                                    match t with
                                        | Ptr(t, c) ->
                                            let! i = compileType t
                                            yield OpTypePointer(id, c, i)
                                        | Bool -> yield OpTypeBool(id)
                                        | SByte -> yield OpTypeInt(id, 8u, 1u)
                                        | Int16 -> yield OpTypeInt(id, 16u, 1u)
                                        | Int32 -> yield OpTypeInt(id, 32u, 1u)
                                        | Int64 -> yield OpTypeInt(id, 64u, 1u)
                                        | Byte -> yield OpTypeInt(id, 8u, 0u)
                                        | UInt16 -> yield OpTypeInt(id, 16u, 0u)
                                        | UInt32 -> yield OpTypeInt(id, 32u, 0u)
                                        | UInt64 -> yield OpTypeInt(id, 64u, 0u)
                                        | IntPtr -> yield OpTypeInt(id, 8u * uint32 IntPtr.Size, 1u)

                                        | Float32 -> yield OpTypeFloat(id, 32u)
                                        | Float64 -> 
                                            if doubleAsFloat then yield OpTypeFloat(id, 32u)
                                            else yield OpTypeFloat(id, 64u)
                                        | Decimal ->
                                            if doubleAsFloat then yield OpTypeFloat(id, 32u)
                                            else yield OpTypeFloat(id, 64u)

                                        | Unit -> yield OpTypeVoid(id)

                                        | Enum -> 
                                            // TODO: enums don't have to be int32
                                            //let f = t.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Public) |> Seq.head
                                            yield OpTypeInt(id, 32u, 1u)

                                        | VectorOf(d,ct) ->
                                            let! comp = compileType ct
                                            yield OpTypeVector(id, comp, uint32 d)

                                        | MatrixOf(d, ct) ->
                                            let vecType = t.GetProperty("C0").PropertyType
                                            let! colType = compileType vecType
                                            yield OpTypeMatrix(id, colType, uint32 d.X)

                                        | FixedArrayType(l, et) ->
                                            let! e = compileType et
                                            yield OpTypeArray(id, e, uint32 l)

                                        | _ ->

                                            if FSharpType.IsRecord t then
                                                let fields = FSharpType.GetRecordFields t |> Array.toList
                                                let! fieldTypes = fields |> List.mapSpv (fun f -> compileType f.PropertyType)

                                                yield OpTypeStruct(id, List.toArray fieldTypes)
                                                yield OpName(id, t.Name)

                                                let fieldNames = fields |> List.mapi (fun i f -> i, f.Name)
                                                for (i,n) in fieldNames do
                                                    yield OpMemberName(id, uint32 i, n)

                                            elif FSharpType.IsUnion t then
                                                let cases = FSharpType.GetUnionCases t |> Array.toList

                                                let allFields =
                                                    cases 
                                                        |> List.collect (fun c -> 
                                                            c.GetFields()
                                                                |> Array.toList
                                                                |> List.map(fun f ->
                                                                    (c.Tag, c.Name, f)
                                                                )
                                                        )
                                                        |> Seq.groupBy (fun (_,_,f) -> f.PropertyType, f.Name)
                                                        |> Seq.map (fun ((fType, fName), cases) ->
                                    
                                                            ()
                                                        )

                                                failwithf "union-types not supported atm."

                                            elif FSharpType.IsFunction t then
                                                failwithf "functions not supported atm."

                                            elif t.IsArray then
                                                failwith "arrays not supported atm."

                                            else
                                                let fields = t.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) |> Array.toList
                                                let! fieldTypes = fields |> List.mapSpv (fun f -> compileType f.FieldType)

                                                yield OpTypeStruct(id, List.toArray fieldTypes)
                                                yield OpName(id, t.Name)

                                                let fieldNames = fields |> List.mapi (fun i f -> i, f.Name)
                                                for (i,n) in fieldNames do
                                                    yield OpMemberName(id, uint32 i, n)

                                    return id
                                | args ->
                                    let args = args |> List.filter (fun t -> t <> typeof<unit>)
                                    let! argTypes = args |> List.mapSpv compileType
                                    let! retType = ret |> compileType

                                    yield OpTypeFunction(id, retType, List.toArray argTypes)

                                    return id

        }

    let rec compileConstant (t : Type) (value : obj) =
        spirv {
            if t = typeof<unit> then
                return 0u
            else
                let! tid = compileType t
                let! id = newId
                match t with
               
                    | Num -> 
                        match value with
                            | :? float as v when doubleAsFloat ->
                                yield OpConstant(tid, id, v |> float32 |> getDwords)
                            | _ -> 
                                yield OpConstant(tid, id, value |> getDwords)

                    | Bool ->
                        if unbox value then yield OpConstantTrue(tid, id)
                        else yield OpConstantFalse(tid, id)

                    | _ -> failwithf "constants of type %A not implemented" t

                return id
        }

    let rec compileIntrinsicCall (retType : Type) (m : MethodInfo) (args : list<Expr>) =
        spirv {
            match m, args with
                | BinaryOperator op, [a;b] ->
                    let! ia = compileExpression false a
                    let! ib = compileExpression false b
                    let! tid = compileType retType

                    let! id = newId
                    match op with
                        | Add -> 
                            match a.Type with
                                | Integral | VectorOf(_,Integral) -> yield OpIAdd(tid, id, ia, ib)
                                | Fractional | VectorOf(_,Fractional) -> yield OpFAdd(tid, id, ia, ib)
                                | _ -> failwithf "cannot add types: %A and %A" a.Type b.Type

                        | Sub ->
                            match a.Type with
                                | Integral | VectorOf(_,Integral) -> yield OpISub(tid, id, ia, ib)
                                | Fractional | VectorOf(_,Fractional) -> yield OpFSub(tid, id, ia, ib)
                                | _ -> failwithf "cannot subtract types: %A and %A" a.Type b.Type

                        | Mul ->
                            if a.Type = b.Type then
                                match a.Type with
                                    | Integral | VectorOf(_,Integral) -> yield OpIMul(tid, id, ia, ib)
                                    | Fractional | VectorOf(_,Fractional) -> yield OpFMul(tid, id, ia, ib)
                                    | MatrixOf(size,bt) when size.X = size.Y -> yield OpMatrixTimesMatrix(tid, id, ia, ib)

                                    | _ -> failwithf "cannot multiply types: %A and %A" a.Type b.Type
                            else
                                match a.Type, b.Type with
                                    | MatrixOf(ms, mt), VectorOf(vd, vt) when vt = mt && ms.X = vd ->
                                        let! resType = compileType (a.Type.GetProperty("C0").PropertyType)
                                        yield OpMatrixTimesVector(resType, id, ia, ib)
                                    | _ ->
                                        failwithf "cannot multiply types: %A and %A" a.Type b.Type 
                        | Div ->
                            match a.Type with
                                | SignedIntegral | VectorOf(_,SignedIntegral) -> yield OpSDiv(tid, id, ia, ib)
                                | UnsignedIntegral | VectorOf(_,UnsignedIntegral) -> yield OpUDiv(tid, id, ia, ib)
                                | Fractional | VectorOf(_,Fractional) -> yield OpFDiv(tid, id, ia, ib)
                                | _ -> failwithf "cannot divide types: %A and %A" a.Type b.Type

                        | Mod ->
                            match a.Type with
                                | SignedIntegral | VectorOf(_,SignedIntegral) -> yield OpSMod(tid, id, ia, ib)
                                | UnsignedIntegral | VectorOf(_,UnsignedIntegral) -> yield OpUMod(tid, id, ia, ib)
                                | Fractional | VectorOf(_,Fractional) -> yield OpFMod(tid, id, ia, ib)
                                | _ -> failwithf "cannot divide types: %A and %A" a.Type b.Type

                        | Greater ->
                            match a.Type with
                                | SignedIntegral -> yield OpSGreaterThan(tid, id, ia, ib)
                                | UnsignedIntegral-> yield OpUGreaterThan(tid, id, ia, ib)
                                | Fractional -> yield OpFOrdGreaterThan(tid, id, ia, ib)
                                | _ -> failwithf "cannot compare types: %A and %A" a.Type b.Type

                        | GreaterOrEqual ->
                            match a.Type with
                                | SignedIntegral -> yield OpSGreaterThanEqual(tid, id, ia, ib)
                                | UnsignedIntegral -> yield OpUGreaterThanEqual(tid, id, ia, ib)
                                | Fractional -> yield OpFOrdGreaterThanEqual(tid, id, ia, ib)
                                | _ -> failwithf "cannot compare types: %A and %A" a.Type b.Type


                        | Smaller ->
                            match a.Type with
                                | SignedIntegral -> yield OpSLessThan(tid, id, ia, ib)
                                | UnsignedIntegral -> yield OpULessThan(tid, id, ia, ib)
                                | Fractional -> yield OpFOrdLessThan(tid, id, ia, ib)
                                | _ -> failwithf "cannot compare types: %A and %A" a.Type b.Type

                        | SmallerOrEqual ->
                            match a.Type with
                                | SignedIntegral -> yield OpSLessThanEqual(tid, id, ia, ib)
                                | UnsignedIntegral -> yield OpULessThanEqual(tid, id, ia, ib)
                                | Fractional -> yield OpFOrdLessThanEqual(tid, id, ia, ib)
                                | _ -> failwithf "cannot compare types: %A and %A" a.Type b.Type

                        | Equal ->
                            match a.Type with
                                | Integral -> yield OpIEqual(tid, id, ia, ib)
                                | Fractional -> yield OpFOrdEqual(tid, id, ia, ib)
                                | Bool -> yield OpLogicalEqual(tid, id, ia, ib)
                                | _ -> failwithf "cannot equal types: %A and %A" a.Type b.Type

                        | NotEqual ->
                           match a.Type with
                                | Integral -> yield OpINotEqual(tid, id, ia, ib)
                                | Fractional -> yield OpFOrdNotEqual(tid, id, ia, ib)
                                | Bool -> yield OpLogicalNotEqual(tid, id, ia, ib)
                                | _ -> failwithf "cannot equal types: %A and %A" a.Type b.Type


                        | BitAnd -> yield OpBitwiseAnd(tid, id, ia, ib)
                        | BitOr -> yield OpBitwiseOr(tid, id, ia, ib)
                        | BitXor -> yield OpBitwiseXor(tid, id, ia, ib)
                        | LeftShift -> yield OpShiftLeftLogical(tid, id, ia, ib)
                        | RightShift -> yield OpShiftRightLogical(tid, id, ia, ib)

                    return Some id
                    
                | UnaryOperator op, [a] ->
                    let! ia = compileExpression false a
                    let! tid = compileType retType
                    let! id = newId

                    match op with
                        | Negate ->
                            match a.Type with
                                | SignedIntegral | VectorOf(_,SignedIntegral) -> yield OpSNegate(tid, id, ia)
                                | Fractional | VectorOf(_,Fractional) -> yield OpFNegate(tid, id, ia)
                                | _ -> failwithf "cannot negate type: %A" a.Type

                        | Not ->
                            match a.Type with
                                | Bool | VectorOf(_,Bool) -> yield OpNot(tid, id, ia)
                                | _ -> failwithf "cannot negate type: %A" a.Type

                    return Some id

                | (MethodQuote <@ Vec.xyz : V4d -> V3d @> _ | Method("get_XYZ",[Vector])), [a] ->
                    let! ia = compileExpression false a
                    let! tid = compileType retType
                    let! id = newId

                    yield OpVectorShuffle(tid, id, ia, ia, [|0u;1u;2u|])


                    return Some id

                | (MethodQuote <@ sqrt @> _ | MethodQuote <@ Fun.Sqrt : float -> float @> _), [a] ->
                    let! glsl = glslId
                    if glsl > 0u then
                        let! ia = compileExpression false a
                        let! tid = compileType retType
                        let! id = newId
                        yield OpExtInst(tid, id, glsl, uint32 GLSLExtInstruction.GLSLstd450Sqrt, [|ia|])
                        return Some id
                    else
                        return None

                | (MethodQuote <@ exp @> _ | MethodQuote <@ Fun.Exp : float -> float @> _), [a] ->
                    let! glsl = glslId
                    if glsl > 0u then
                        let! ia = compileExpression false a
                        let! tid = compileType retType
                        let! id = newId
                        yield OpExtInst(tid, id, glsl, uint32 GLSLExtInstruction.GLSLstd450Exp, [|ia|])
                        return Some id
                    else
                        return None

                | (MethodQuote <@ pow : float -> float -> float @> _ | MethodQuote <@ Fun.Pow : float * float -> float @> _), [x;e] ->
                    let! glsl = glslId
                    if glsl > 0u then
                        let! ix = compileExpression false x
                        let! ie = compileExpression false e
                        let! tid = compileType retType
                        let! id = newId
                        yield OpExtInst(tid, id, glsl, uint32 GLSLExtInstruction.GLSLstd450Pow, [|ix; ie|])
                        return Some id
                     else
                        return None

                | _ ->
                    return None
        }

    and compileExpression (last : bool) (e : Expr) =
        let ret (id : uint32) =
            spirv {
                if last then 
                    if id > 0u then yield OpReturnValue(id)
                    else yield OpReturn
                    return 0u
                else 
                    return id
            }

        spirv {
            match e with

                | Call(None, Method("op_LessAmpGreater", _), [a;b]) ->
                    let! lb = getLabelId <| Label()
                    let! lf = getLabelId <| Label()
                    let! le = getLabelId <| Label()
                    let! t = compileType typeof<bool>

                    let! a = compileExpression false a
                    yield OpBranchConditional(a, lb, lf, [||])
                    
                    yield OpLabel(lb)
                    let! b = compileExpression false b
                    yield OpBranch(le)

                    yield OpLabel(lf)
                    let! fid = newId
                    yield OpConstantFalse(t, fid)

                    yield OpLabel(le)

                    let! id = newId
                    yield OpPhi(t, id, [|b;lb; fid;lf|])
                    return! ret id


                | Call(None, Method("op_LessBarGreater", _), [a;b]) ->
                    let! lb = getLabelId <| Label()
                    let! lt = getLabelId <| Label()
                    let! le = getLabelId <| Label()
                    let! t = compileType typeof<bool>

                    let! a = compileExpression false a
                    yield OpBranchConditional(a, lt, lb, [||])
                    
                    yield OpLabel(lb)
                    let! b = compileExpression false b
                    yield OpBranch(le)

                    yield OpLabel(lt)
                    let! tid = newId
                    yield OpConstantTrue(t, tid)

                    yield OpLabel(le)

                    let! id = newId
                    yield OpPhi(t, id, [|b;lb; tid;lt|])
                    return! ret id

                

                // foreach-loops have to be matched as first pattern since they consist of a number of expressions
                // and may be 'destroyed' otherwise.
                | ForEach(var,seq,body) ->
                    let seqType = seq.Type
                    match seqType with
                        | FixedArrayType(d, baseType) ->
                            let i = Var("_i", typeof<int>)
                            let e = 
                                Expr.ForIntegerRangeLoop(i, Expr.Value(0), Expr.Value(d-1), 
                                    Expr.Let(var, Expr.PropertyGet(seq, seqType.GetProperty("Item"), [Expr.Var i]),
                                        body
                                    )
                                )
                            return! compileExpression last e
                        | _ ->
                            return failwithf "unexpected foreach loop seq: %A" seq

                // empty array-creations cannot be expressed inline (in most C-like languages). Therefore we create a function
                // creating a local variable having the desired type and returning it. 
                | NewObject(c, []) when c.DeclaringType.IsGenericType && c.DeclaringType.GetGenericTypeDefinition() = typedefof<Arr<_,_>> ->
                    let! tid = compileType c.DeclaringType
                    let! id = newId
                    let targs = c.DeclaringType.GetGenericArguments()
                    let l = getSize targs.[0]
                    let et = targs.[1]

                    let bytes = (Marshal.SizeOf et * l + 3) &&& ~~~3 
                    let words = bytes / 4

                    yield OpCompositeConstruct(tid, id, Array.zeroCreate words)
                    return! ret id


                // lets become variable-declarations whenever the variable is mutable. 
                // when the variable is immutable we don't need to introduce a variable in SpirV
                // but instead simply use the expression's id.
                | Let(v,e,b) ->
                    // when sequentially accessing properties on structs F# often introduces copies 
                    // to ensure that the original struct cannot be modified by the property-access.
                    // since in C every variable is mutable anyways we do not want to have those copies
                    // in our code. 
                    // TODO: find a better way of identifying those copies (user cannot use the name atm.)
                    if v.Name.StartsWith "copyOfStruct" then
                        let b = b.Substitute(fun vi -> if vi = v then Some e else None)
                        return! compileExpression last b
                    else
                        match v.Type, e with
                            // When a new FixedArray is created and directly assigned to a 
                            // variable we skip its initialization
                            | FixedArrayType(et,d), NewObject(_) ->
                                let! tid = compileType (makePtrType v.Type)
                                let! vid = newMutableVarId v
                                yield OpVariable(tid, vid, StorageClass.Function, None)

                            | _ ->
                                if v.IsMutable then
                                    let! tid = compileType (makePtrType v.Type)
                                    let! vid = newMutableVarId v
                                    let! e = compileExpression false e
                                    yield OpVariable(tid, vid, StorageClass.Function, Some e)
                                else
                                    let! e = compileExpression false e
                                    do! setVarId v e

                        return! compileExpression last b


                // F#'s for-loops sadly treat their upper bound inclusively. This often causes code to contain upper-bounds
                // like 'count-1'. Since C-Style programmers mostly use 'i < count' (and compilers may be optimized therefore)
                // we emit 'i < count' when easily possible (meaning that the upper bound is something like count-1)
                | ForIntegerRangeLoop(v, s, e, b) ->

                    let compareOp, e = 
                        match e with
                            | Call(None, mi, [e; Value(value,t)]) when mi.Name = "op_Subtraction" && value = (1 :> obj) ->
                                OpSLessThan, e
                            | _ ->
                                OpSLessThanEqual, e

                    let! tid = compileType v.Type
                    let! ptid = compileType <| (makePtrType v.Type)
                    let! vid = newMutableVarId v
                    let! bid = compileType typeof<bool>

                    let! one = compileExpression false (Microsoft.FSharp.Quotations.Expr.Value(1 :> obj, typeof<int>))
                    let! l = compileExpression false s
                    let! u = compileExpression false e
                    yield OpVariable(ptid, vid, StorageClass.Function, Some l)

                    let! startLabel = getLabelId <| Label()
                    let! contLabel = getLabelId <| Label()
                    let! bodyLabel = getLabelId <| Label()
                    let! endLabel = getLabelId <| Label()

                    let! iv = newId
                    let! cid = newId
                    let! incId = newId

                    // start:
                    yield OpLabel(startLabel)
                                    
                    // if i < upper then jmp body else jmp body
                    yield OpLoad(tid, iv, vid, None)
                    yield compareOp(bid, cid, iv, u)
                    yield OpLoopMerge(endLabel, startLabel, LoopControl.None)


                                    
                    yield OpBranchConditional(cid, bodyLabel, endLabel, [||])

                    yield OpLabel(bodyLabel)
                    do! pushScope { startLabel = startLabel; endLabel = contLabel }
                    let! _ = compileExpression false b
                    do! popScope


                    yield OpLabel(contLabel)

                    // i <- i + 1
                    yield OpIAdd(tid, incId, iv, one)
                    yield OpStore(vid, incId, None)

                    // jmp start
                    yield OpBranch(startLabel)

                    //end:
                    yield OpLabel(endLabel)


                    return! ret 0u


                // While loops simply work like in C but F# doesn't come with support for breaks.
                // TODO: think about integrating break just for the shader-language.
                | WhileLoopFlat(guard,body) ->

                    let! startLabel = getLabelId <| Label()
                    let! bodyLabel = getLabelId <| Label()
                    let! endLabel = getLabelId <| Label()

                    yield OpLabel startLabel
                    let! gid = compileExpression false guard
                       
                    yield OpLoopMerge(endLabel, startLabel, LoopControl.None)
                    yield OpBranchConditional(gid, bodyLabel, endLabel, [||])

                    do! pushScope { startLabel = startLabel; endLabel = endLabel }
                    let! _ = compileExpression false body
                    do! popScope

                    yield OpBranch(startLabel)
                    yield OpLabel(endLabel)

                    return! ret 0u

                // Sequentials in C are simply represented by concatenation of the statements.
                | Sequential(l,r) ->
                    let! _ = compileExpression false l
                    return! compileExpression last r

                // Since pipes are very common in F#-code w want to support them whenever possible
                // Most pipes can however simply be 'inlined'. This active pattern determines
                // whether a pipe can be 'inlined' and returns a reorganized expression without the pipe.
                | Pipe(e) ->
                    return! compileExpression last e
                    

                // static calls (without a target) are compiled as they are.
                | Call(None, mi, args) ->
                    let! res = compileIntrinsicCall e.Type mi args
                    match res with
                        | Some id ->
                            return! ret id
                        | None ->
                            // TODO: user-given methods shall be compiled too (when possible)
                            return failwithf "user-methods not implemented: %A" mi


                // member calls are translated to functions taking the this-parameter at the first position.
                | Call(Some v, mi, args) -> 
                    let! res = compileIntrinsicCall e.Type mi (v::args)
                    match res with
                        | Some id ->
                            return! ret id
                        | None ->
                            // TODO: user-given methods shall be compiled too (when possible)
                            return failwithf "user-methods not implemented: %A" mi


                // coercions to object are simply ignored. (some F# constructs make use of those)
                | Coerce(a,t) when t = typeof<obj> ->  
                    return! compileExpression last a  

                | VarSet(v,e) ->
                    let! vid = getVarId v
                    match vid with
                        | LocationId vid ->
                            let! eid = compileExpression false e

                            yield OpStore(vid, eid, None)

                            return! ret 0u
                        | _ ->
                            return failwithf "cannot mutabte immutable binding %A" v



                | IfThenElse(guard, ifTrue, ifFalse) ->
                    if ifTrue.Type = typeof<unit> then
                        let! lt = getLabelId <| Label()
                        let! lf = getLabelId <| Label()

                        let! gid = compileExpression false guard
                        yield OpBranchConditional(gid, lt, lf, [||])

                        if last then
                            yield OpLabel(lt)
                            let! _ = compileExpression true ifTrue
                            yield OpLabel(lf)
                            let! _ = compileExpression true ifFalse
                            
                            return 0u

                        else
                            let! le = getLabelId <| Label()

                            yield OpLabel(lt)
                            let! _ = compileExpression false ifTrue
                            yield OpBranch(le)

                            yield OpLabel(lf)
                            let! _ = compileExpression false ifFalse
                            yield OpLabel(le)

                            return! ret 0u
                    else
                        let! t = compileType ifTrue.Type
                        let! lt = getLabelId <| Label()
                        let! lf = getLabelId <| Label()

                        let! gid = compileExpression false guard
                        yield OpBranchConditional(gid, lt, lf, [||])

                        if last then
                            yield OpLabel(lt)
                            let! _ = compileExpression true ifTrue

                            yield OpLabel(lf)
                            let! _ = compileExpression true ifFalse
                            
                            return 0u

                        else
                            let! le = getLabelId <| Label()
                            yield OpLabel(lt)
                            let! tv = compileExpression false ifTrue
                            yield OpBranch(le)

                            yield OpLabel(lf)
                            let! fv = compileExpression false ifFalse
                            yield OpLabel(le)


                            let! id = newId
                            yield OpPhi(t, id, [|tv;lt; fv;lf|])

                            return! ret id

                    



                | Value(_, Unit) ->
                    return! ret 0u

                | Value(v,t) ->
                    let! c = compileConstant t v
                    return! ret c
                | Var(v) ->
                    let! id = getVarId v
                    match id with
                        | LocationId id ->
                            let! rid = newId
                            let! tid = compileType v.Type
                            yield OpLoad(tid, rid, id, None)
                            return! ret rid
                        | ValueId id ->
                            return! ret id
                
                | FieldGet(Some (Var v), field) ->
                    let! vid = getVarId v
                    match vid with
                        | LocationId vid ->
                            let index = 
                                match field with
                                    | :? CustomField as c -> c.Index
                                    | _ ->
                                        field.DeclaringType.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) |> Array.findIndex (fun fi -> fi = field)

                            let! ci = compileConstant typeof<uint32> (uint32 index)
                            let! pt = compileType (makePtrType field.FieldType)
                            let! pid = newId
                            yield OpInBoundsAccessChain(pt, pid, vid, [|ci|])

                            let! t = compileType field.FieldType
                            let! id = newId
                            yield OpLoad(t, id, pid, None)

                            return! ret id

                        | ValueId target ->
                            let! t = compileType field.FieldType
                            let! id = newId
                            match field with
                                | :? CustomField as c -> 
                                    yield OpCompositeExtract(t, id, target, [|uint32 c.Index|])

                                | _ ->
                                    let index = field.DeclaringType.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) |> Array.findIndex (fun fi -> fi = field)
                                    yield OpCompositeExtract(t, id, target, [|uint32 index|])

                            return! ret id

                | FieldGet(Some target, field) ->
                    let! t = compileType field.FieldType
                    let! target = compileExpression false target
                    let! id = newId
                    match field with
                        | :? CustomField as c -> 
                            yield OpCompositeExtract(t, id, target, [|uint32 c.Index|])

                        | _ ->
                            let index = field.DeclaringType.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) |> Array.findIndex (fun fi -> fi = field)
                            yield OpCompositeExtract(t, id, target, [|uint32 index|])

                    return! ret id

                | NewObject(ctor, args) ->
                    match ctor.DeclaringType with
                        | VectorOf(d,t) ->
                            let! args = args |> List.mapSpv (compileExpression false)
                            
                            let! tid = compileType ctor.DeclaringType
                            let! id = newId

                            yield OpCompositeConstruct(tid, id, List.toArray args)
                            return! ret id
                        | _ ->
                            return failwithf "construction of %A not implemented" ctor.DeclaringType

                // property-getters and setters having indices are compiled using their respective
                // Get-/SetMethod.
                | PropertySet(t, pi, indices, value) ->
                    match t with
                        | None -> return! Expr.Call(pi.SetMethod, List.concat [indices; [value]]) |> compileExpression last 
                        | Some t -> return! Expr.Call(t, pi.SetMethod, List.concat [indices; [value]]) |> compileExpression last 

                | PropertyGet(t, pi, indices) ->
                    match t with
                        | None -> return! Expr.Call(pi.GetMethod, indices) |> compileExpression last 
                        | Some t -> return! Expr.Call(t, pi.GetMethod, indices) |> compileExpression last
                            
                | _ ->
                    return failwithf "unsupported expr: %A" e

        }

    let compileFunction (e : Expr) =
        spirv {
            match e with
                | MethodLambda(args, body) ->
                    let! funcType = compileType e.Type
                    let! resType = compileType body.Type

                    let! id = newId
                    yield OpFunction(resType, id, FunctionControl.None, funcType)
                    for a in args do
                        let! vid = newMutableVarId a
                        let! at = compileType (makeParameterType a.Type)
                        yield OpFunctionParameter(at, vid)

                    let! _ = compileExpression true body

                    yield OpFunctionEnd
                    
                | _ ->
                    ()

            
        }


    let getBuiltInInput (t : ShaderType) (last : Option<ShaderType>) (s : string) =
        match s, last with
            | "PointSize", Some _ -> 
                Some ("gl_PointSize", BuiltIn.PointSize)

            | "ClipDistance", Some _  -> 
                Some ("gl_ClipDistance", BuiltIn.ClipDistance)

            | _ ->
                match t with
                    | Vertex ->
                        match s with
                            | "VertexId" -> Some ("gl_VertexID", BuiltIn.VertexId)
                            | "InstanceId" -> Some ("gl_InstanceID", BuiltIn.InstanceId)
                            | _ -> None

                    | TessControl ->
                        match s with
                            | "PatchVertices" -> Some ("gl_PatchVerticesIn", BuiltIn.PatchVertices)
                            | "PrimitiveId" -> Some ("gl_PrimitiveID", BuiltIn.PrimitiveId)
                            | "InvocationId" -> Some ("gl_InvocationID", BuiltIn.InvocationId)
                            | _ -> None
                    | TessEval ->
                        match s with
                            | "TessCoord" -> Some ("gl_TessCoord", BuiltIn.TessCoord)
                            | "PatchVertices" -> Some ("gl_PatchVerticesIn", BuiltIn.PatchVertices)
                            | "PrimitiveId" -> Some ("gl_PrimitiveID", BuiltIn.PrimitiveId)
                            | "TessLevelInner" -> Some ("gl_TessLevelInner", BuiltIn.TessLevelInner)
                            | "TessLevelOuter" -> Some ("gl_TessLevelOuter", BuiltIn.TessLevelOuter)
                            | _ -> None

                    | Geometry(_) ->
                        match s with
                            | "PrimitiveId" -> Some ("gl_PrimitiveIDIn", BuiltIn.PrimitiveId)
                            | "InvocationId" -> Some ("gl_InvocationID", BuiltIn.InvocationId)
                            | _ -> None

                    | Fragment ->
                        match s with
                            | "FragCoord" -> Some ("gl_FragCoord", BuiltIn.FragCoord)
                            | "FrontFacing" -> Some ("gl_FrontFacing", BuiltIn.FrontFacing)
                            | "PointCoord" -> Some ("gl_PointCoord", BuiltIn.PointCoord)
                            | "SampleId" -> Some ("gl_SampleID", BuiltIn.SampleId)
                            | "SamplePosition" -> Some ("gl_SamplePosition", BuiltIn.SamplePosition)
                            | "SampleMask" -> Some ("gl_SampleMaskIn", BuiltIn.SampleMask)
                            | "PrimitiveId" -> Some ("gl_PrimitiveID", BuiltIn.PrimitiveId)
                            | "Layer" -> Some ("gl_Layer", BuiltIn.Layer)
                            | "ViewportIndex" -> Some ("gl_ViewportIndex", BuiltIn.ViewportIndex)
                            | _ -> None

    let getBuiltInOutput (t : ShaderType) (next : Option<ShaderType>) (s : string) =
        match s, next with
            | "Positions", Some Fragment -> 
                Some ("gl_Position", BuiltIn.Position)

            | "PointSize", Some _ -> 
                Some ("gl_PointSize", BuiltIn.PointSize)

            | "ClipDistance", Some _  -> 
                Some ("gl_ClipDistance", BuiltIn.ClipDistance)

            | _ ->
                match t with
                    | Vertex ->
                        None

                    | TessControl ->
                        match s with
                            | "TessLevelInner" -> Some ("gl_TessLevelInner", BuiltIn.TessLevelInner)
                            | "TessLevelOuter" -> Some ("gl_TessLevelOuter", BuiltIn.TessLevelOuter)
                            | _ -> None

                    | TessEval ->
                        None

                    | Geometry(_) ->
                        match s with
                            | "Layer" -> Some ("gl_Layer", BuiltIn.Layer)
                            | "ViewportIndex" -> Some ("gl_ViewportIndex", BuiltIn.ViewportIndex)
                            | "PrimitiveId" -> Some ("gl_PrimitiveID", BuiltIn.PrimitiveId)
                            | _ -> None

                    | Fragment ->
                        match s with
                            | "Depth" -> Some ("gl_FragDepth", BuiltIn.FragDepth)
                            | _ -> None

    let compileShaderFunction (last : Option<ShaderType>) (shaderType : ShaderType) (next : Option<ShaderType>) (uniforms : list<int * int * Var>) (inputs : list<Either<int, BuiltIn> * Var>) (outputs : list<Either<int, BuiltIn> * Var>) (body : Expr) =
        spirv {

            let model =
                match shaderType with
                    | ShaderType.Vertex -> ExecutionModel.Vertex
                    | ShaderType.TessControl -> ExecutionModel.TessellationControl
                    | ShaderType.TessEval -> ExecutionModel.TessellationEvaluation
                    | ShaderType.Geometry _ -> ExecutionModel.Geometry
                    | ShaderType.Fragment -> ExecutionModel.Fragment


            yield OpCapability Capability.Shader

            let! glslExt = newId
            yield OpExtInstImport(glslExt, "GLSL.std.450")
            do! setGlslId glslExt

            yield OpMemoryModel(AddressingModel.Logical, MemoryModel.GLSL450)

            let! entry = newId
            yield OpEntryPoint(model, entry, "main")

            match shaderType with
                | Fragment ->
                    yield OpExecutionMode(entry, ExecutionMode.OriginLowerLeft, None)

                | _ ->
                    // TODO: special annotations per stage
                    ()


            //yield OpSource(SourceLanguage.Unknown, 100u, "FShade")
            yield OpSourceExtension("GL_ARB_separate_shader_objects")
            yield OpSourceExtension("GL_ARB_shading_language_420pack")

            yield OpName(entry, "main")

            for (_,i) in inputs do
                let! id = newMutableVarId i
                yield OpName(id, i.Name)

            for (_,o) in outputs do
                let! id = newMutableVarId o
                yield OpName(id, o.Name)


            for (loc, i) in inputs do
                let! tid = compileType (makeInputType i.Type)
                let! vid = getVarId i
                match vid with
                    | LocationId vid ->
                        yield OpVariable(tid, vid, StorageClass.Input, None)

                        match loc with
                            | Left loc -> yield OpDecorate(vid, Decoration.Location, [|uint32 loc|])
                            | Right b -> yield OpDecorate(vid, Decoration.BuiltIn, [|uint32 (int b)|])

                    | _ -> 
                        failwith "impossible"

            for (loc, o) in outputs do
                let! tid = compileType (makeOutputType o.Type)
                let! vid = getVarId o
                match vid with
                    | LocationId vid ->
                        yield OpVariable(tid, vid, StorageClass.Output, None)

                        match loc with
                            | Left loc -> yield OpDecorate(vid, Decoration.Location, [|uint32 loc|])
                            | Right b -> yield OpDecorate(vid, Decoration.BuiltIn, [|uint32 (int b)|])
                    | _ -> 
                        failwith "impossible"


            let mutable binding = 0
            for (ds, b, u) in uniforms do
                let! tid = compileType (makeUniformType u.Type)
                let! vid = newMutableVarId u
                yield OpVariable(tid, vid, StorageClass.Uniform, None)

                yield OpName(vid, u.Name)
                yield OpDecorate(vid, Decoration.DescriptorSet, [| uint32 ds |])
                yield OpDecorate(vid, Decoration.Binding, [| uint32 b |])
                binding <- binding + 1
                // TODO: descriptorsets and bindings here
                //yield OpDecorate(vid, Decoration.Location, [|uint32 loc|])




            let! tVoid = compileType typeof<unit>
            let! entryType = compileType typeof<unit -> unit>
            yield OpFunction(tVoid, entry, FunctionControl.None, entryType)

            let! _ = compileExpression true body

            yield OpFunctionEnd



        }


    let compileShader (last : Option<ShaderType>) (next : Option<ShaderType>) (s : Shader) : Compiled<Module, ShaderState> =
        compile {

            // uniform grouping
            let uniformGroups = 
                s.uniforms  
                    |> Seq.choose(fun (u,v) ->
                        match u with
                            | UserUniform(t,o) -> Some (uniform, t, v.Name,v)
                            | Attribute(scope, t, n) -> Some (scope, t, n,v)
                            | SamplerUniform(t,sem, n,_) -> None
                       ) 
                    |> Seq.groupBy(fun (s,_,_,_) -> s)
                    |> Seq.map (fun (g,v) -> (g, v |> Seq.map (fun (_,t,n,v) -> (t,n,v)) |> Seq.toList))
                    |> Map.ofSeq

            let bufferVariables = 
                uniformGroups |> Map.map (fun scope fields ->
                    let t = CustomStruct(scope.Name, fields |> List.map (fun (t,n,_) -> n,t))   
                    Var(sprintf "_%s" scope.Name, t)
                )

            let variableParents =
                uniformGroups 
                    |> Map.toList
                    |> List.collect (fun (scope, fields) ->
                        fields |> List.map (fun (_,_,v) ->
                            v, Map.find scope bufferVariables
                        )
                       )
                    |> Map.ofList

            let body =
                s.body.Substitute (fun vi ->
                    match Map.tryFind vi variableParents with
                        | Some buffer ->
                            Expr.FieldGet(Expr.Var buffer, buffer.Type.GetField(vi.Name)) |> Some
                        | None ->
                            None
                )

            let uniforms =
                List.concat [
                    bufferVariables 
                        |> Map.toList |> List.map snd

                    s.uniforms 
                        |> List.choose (fun (u,v) -> 
                            match u with
                                | SamplerUniform(_) -> Some v
                                | _ -> None
                            )
                ]

            let! uniforms = 
                uniforms |> List.mapC (fun u ->
                    compile {
                        let! ds = nextCounter "DS"
                        return (ds, 0, u)
                    }
                )

            // IO builtIns
            let inputs = s.inputs |> Map.toSeq |> Seq.sortBy(fun (_,n) -> n.Name) |> Seq.map snd |> Seq.toList
            let outputs = s.outputs |> Map.toSeq |> Seq.sortBy(fun (_,(_,n)) -> n.Name) |> Seq.map snd |> Seq.toList

            let inputRemap = 
                s.inputs
                    |> Map.toList 
                    |> List.choose (fun (sem,i) ->
                        match getBuiltInInput s.shaderType last sem with
                            | Some (name, builtin) ->
                                Some (i, (Var(name,i.Type), builtin))
                            | _ ->
                                None
                       )
                    |> Map.ofList

            let outputRemap = 
                s.outputs
                    |> Map.toList 
                    |> List.choose (fun (sem,(_,o)) ->
                        match getBuiltInOutput s.shaderType next sem with
                            | Some (name, builtin) ->
                                Some (o, (Var(name,o.Type), builtin))
                            | _ ->
                                None
                       )
                    |> Map.ofList

            let remap = Map.union inputRemap outputRemap

            let body =
                body.Substitute(fun vi ->
                    match Map.tryFind vi remap with
                        | Some (v, builtin) ->
                            Some (Expr.Var v)
                        | None ->
                            None
                )

            let mutable location = 0
            let inputs = 
                inputs |> List.map (fun i ->
                    match Map.tryFind i inputRemap with
                        | Some (ni, b) -> (Right b, ni)
                        | None -> 
                            let l = location
                            location <- location + 1
                            (Left l, i)
                )


            let mutable location = 0
            let outputs = 
                outputs |> List.map (fun (loc, i) ->
                    match Map.tryFind i outputRemap with
                        | Some (ni, b) -> (Right b, ni)
                        | None -> 
                            let l = location
                            location <- location + 1
                            (Left l, i)
                )



            // compile to instructions

            let res = compileShaderFunction last s.shaderType next uniforms inputs outputs body
            let (state,_) = res.build SpirVState.Empty



            return { 
                magic = 0x07230203u
                version = 0x00010000u
                generatorMagic = (0xFADEu <<< 16) ||| 1u
                bound = state.currentId
                reserved = 0u
                instructions = (RevList.toList state.typeInstructions) @ (RevList.toList state.instructions)
            }
        }


    type CompiledEffect =
        {
            modules : Map<ShaderType, Module>
        }

    let private compileEffectInternal (neededOutputs : Map<string, Type>) (e : Compiled<Effect, ShaderState>) =
        compile {
            let! e = e

            let hasgs = match e.geometryShader with | Some _ -> true | _ -> false
            let! fsUsed,fsCode = match e.fragmentShader with
                                    | Some(fs) -> compile {
                                                    let additional =
                                                        neededOutputs 
                                                            |> Map.remove "Depth"
                                                            |> Map.filter (fun k t ->
                                                                match Map.tryFind k fs.outputs with
                                                                    | Some _ -> false
                                                                    | None -> true
                                                            )
                                                    let fs = addOutputs additional fs

                                                    let unused = fs.outputs |> Map.filter (fun k (t,v) -> 
                                                                    match t with
                                                                        | Some t -> false
                                                                        | None -> not <| Map.containsKey k neededOutputs
                                                                 ) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq

                                                    let fs = removeOutputs unused fs

                                                    // TODO: needed here?
                                                    //let fs = adjustToConfig config fs ((if hasgs then Geometry(None, TriangleStrip) else Vertex) |> Some) None

                                                    let last = ((if hasgs then Geometry(None, TriangleStrip) else Vertex) |> Some)
                                                   
                                                   
                                                    let! fsc = compileShader last None fs

                                                    let used = seq { yield ("Positions",typeof<V4d>); yield! fs.inputs |> Seq.map (fun (KeyValue(k,v)) -> (k,v.Type)) } |> Map.ofSeq
                                                    return used, Some fsc
                                                  }
                                    | None -> compile { return Map.ofList [("Positions",typeof<V4d>)], None }

            do! resetCompilerState
            let! gsUsed,gsCode = match e.geometryShader with
                                    | Some(gs, t) -> compile {
                                                    let additional = fsUsed |> Map.filter (fun k _ -> not <| Map.containsKey k gs.outputs)
                                                    let gs = addOutputs additional gs

                                                    let unused = gs.outputs |> Map.filter (fun k v -> not <| Map.containsKey k fsUsed) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq
                                                    let gs = removeOutputs unused gs

                                                    //let gs = adjustToConfig config gs (Some Vertex) (Some Fragment)
                                                    let! gsc = compileShader (Some Vertex) (Some Fragment) gs

                                                    let used = gs.inputs |> Seq.map (fun (KeyValue(k,v)) -> (k, if v.Type.IsArray then v.Type.GetElementType() else v.Type)) |> Map.ofSeq
                                                
     
                                                    return used,Some (gsc,t)
                                                  }
                                    | None -> compile { return fsUsed,None }

            do! resetCompilerState


            
            let uniformBufferUnion (a : Map<UniformScope, list<Type * string>>) (b : Map<UniformScope, list<Type * string>>) =
                let mutable result = a

                for (KeyValue(s,list)) in b do
                    match Map.tryFind s result with
                        | Some l ->
                            let o = System.Collections.Generic.HashSet(List.concat [l |> Seq.toList; list]) |> Seq.toList
                            result <- Map.add s o result
                        | None -> 
                            result <- Map.add s list result
                result


            let! tessUsed, tessCode = match e.tessControlShader, e.tessEvalShader with
                                        | None, None -> compile { return gsUsed, None }
                                        | Some tcs, Some tev ->
                                            compile {
                                                let additional = gsUsed |> Map.filter (fun k _ -> not <| Map.containsKey k tev.outputs)
                                                let unused = tev.outputs |> Map.filter (fun k v -> not <| Map.containsKey k gsUsed) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq

                                                let tev = addOutputs additional tev
                                                let tev = removeOutputs unused tev

                                                let tevUsed = tev.inputs |> Seq.map (fun (KeyValue(k,v)) -> (k, if v.Type.IsArray then v.Type.GetElementType() else v.Type)) |> Map.ofSeq
                                                let tevUsed = Map.remove "TessCoord" tevUsed
                                                let tevUsed = Map.add "TessLevelInner" typeof<float[]> tevUsed
                                                let tevUsed = Map.add "TessLevelOuter" typeof<float[]> tevUsed

                                                let additional = tevUsed |> Map.filter (fun k _ -> not <| Map.containsKey k tcs.outputs)
                                                let unused = tcs.outputs |> Map.filter (fun k v -> not <| Map.containsKey k tevUsed) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq

                                                let tcs = addOutputs additional tcs
                                                let tcs = removeOutputs unused tcs

                                                let tcsUsed = tcs.inputs |> Seq.map (fun (KeyValue(k,v)) -> (k, if v.Type.IsArray then v.Type.GetElementType() else v.Type)) |> Map.ofSeq
                                                


                                                let id = Var("gl_InvocationID", typeof<int>)
                                                let rec substitute e =
                                                    match e with
                                                        | VarSet(v,value) ->
                                                            match Map.tryFindKey (fun _ (_,vi) -> vi = v) tcs.outputs with
                                                                | Some s when s <> "TessLevelInner" && s <> "TessLevelOuter" ->
                                                                    let set = getMethodInfo <@ LanguagePrimitives.IntrinsicFunctions.SetArray @>
                                                                    let set = set.MakeGenericMethod [|value.Type|]

                                                                    let newVar =
                                                                        if s = "Positions" then 
                                                                            Var("gl_OutPosition", v.Type.MakeArrayType())
                                                                        else
                                                                            Var(v.Name, v.Type.MakeArrayType())

                                                                    Expr.Call(set, [Expr.Var newVar; Expr.Var id; value])
       
                                                                | _ -> e

                                                        | ShapeVar(v) -> e
                                                        | ShapeCombination(o,args) -> RebuildShapeCombination(o, args |> List.map substitute)
                                                        | ShapeLambda(v,b) -> Expr.Lambda(v, substitute b)

                                                let newTcs = substitute tcs.body
                                                let tcs = { tcs with body = newTcs }

                                                //let tev = adjustToConfig config tev (Some Vertex) (Some Fragment)
                                                //let tcs = adjustToConfig config tcs (Some Vertex) (Some Fragment)


                                                let! tevc = compileShader (Some Vertex) (Some Fragment) tev
                                                let! tcsc = compileShader (Some Vertex) (Some Fragment) tcs

                                                
                                                let glPos = System.Text.RegularExpressions.Regex("gl_Position\[(?<index>[^\]]+)\]")
                                                let glOutPos = System.Text.RegularExpressions.Regex("gl_OutPosition\[(?<index>[^\]]+)\]")

//                                                let tevCode = glPos.Replace(tevc.code, (fun (m : System.Text.RegularExpressions.Match) -> 
//                                                    let id = m.Groups.["index"].Value
//                                                    sprintf "gl_in[%s].gl_Position" id
//                                                ))
//
//                                                let tcsCode = glPos.Replace(tcsc.code, (fun (m : System.Text.RegularExpressions.Match) -> 
//                                                    let id = m.Groups.["index"].Value
//                                                    sprintf "gl_in[%s].gl_Position" id
//                                                ))
//
//                                                let tcsCode = glOutPos.Replace(tcsCode, (fun (m : System.Text.RegularExpressions.Match) -> 
//                                                    let id = m.Groups.["index"].Value
//                                                    sprintf "gl_out[%s].gl_Position" id
//                                                ))
//
//                                                let agg = { code = sprintf "\r\n#ifdef TessControl\r\n%s\r\n#endif\r\n#ifdef TessEval\r\n%s\r\n#endif\r\n" tcsCode tevCode
//                                                            usedTypes = PersistentHashSet.union tevc.usedTypes tcsc.usedTypes
//                                                            uniformBuffers = uniformBufferUnion tevc.uniformBuffers tcsc.uniformBuffers
//                                                            uniforms = Map.union tevc.uniforms tcsc.uniforms }
//               
                                                let tcsUsed = Map.remove "InvocationId" tcsUsed

                                                return tcsUsed,Some (tcsc, tevc)
                                            }

                                        | _ -> compile { return! error "invalid tessellation setup" }

            do! resetCompilerState

            let! vsCode = compile {
                            let vs = 
                                match e.vertexShader with
                                    | Some(vs) -> vs
                                    | None -> { shaderType = ShaderType.Vertex; uniforms = []; inputs = Map.empty; outputs = Map.empty; body = Expr.Value(()); inputTopology = None; debugInfo = None }

                            let additional = tessUsed |> Map.filter (fun k _ -> not <| Map.containsKey k vs.outputs)
                            let vs = addOutputs additional vs

                            let unused = vs.outputs |> Map.filter (fun k v -> not <| Map.containsKey k tessUsed) |> Seq.map(fun (KeyValue(k,(_,v))) -> v) |> Set.ofSeq
                            let vs = removeOutputs unused vs

                            //let vs = adjustToConfig config vs None ((if hasgs then Geometry(None, TriangleStrip) else Fragment) |> Some) 
                            
                            let! vsc = compileShader None (Some (if hasgs then Geometry(None, TriangleStrip) else Fragment)) vs
                            return Some vsc
                          }

            let map = Map.empty

            let map =
                match fsCode with
                    | Some fs -> 
                        map |> Map.add ShaderType.Fragment fs
                    | _ -> map

            let map =
                match gsCode with
                    | Some(gs,t) ->
                        map |> Map.add (ShaderType.Geometry(None, t)) gs
                    | _ -> map

            let map =
                match tessCode with
                    | Some(tcs,tev) ->
                        map |> Map.add ShaderType.TessControl tcs
                            |> Map.add ShaderType.TessEval tev
                    | _ -> map

            let map =
                match vsCode with
                    | Some vs -> 
                        map |> Map.add ShaderType.Vertex vs
                    | _ -> map

            return { modules = map }
        }


    let compileEffect (neededOutputs : Map<string, Type>) (e : Compiled<Effect, ShaderState>) : Error<CompiledEffect> =
        e |> compileEffectInternal neededOutputs |> runCompile (FakeCompiler() :> ICompiler<_>)


    let vs (e : Compiled<Effect,_>) =
        compile {
            let! e = e
            return e.vertexShader.Value
        }

module SpirVCompilerTest =
    let testCode = 
        <@ 
            let ipos = V4d.Zero
            let mutable op = V4d.Zero

            op <- ipos
        @>


    type Vertex =
        {
            [<Semantic("Positions")>] pos : V4d
            [<Semantic("WorldPosition")>] wp : V4d
            [<Semantic("Normals")>] n : V3d
            [<Semantic("Colors")>] c : V4d
        }

    let trafo (v : Vertex) =
        vertex {
            let m : M44d = uniform?PerModel?ModelTrafo
            let vp : M44d = uniform?PerView?ViewProjTrafo
            let nm : M33d = uniform?PerModel?NormalMatrix

            let wp = m * v.pos

            return { v with pos = vp * wp; wp = wp; n = nm * v.n }
        }

    let vertexColor (v : Vertex) =
        fragment {
            return v.c
        }

    let constructedColor (v : Vertex) =
        fragment {
            return V4d(v.c.XYZ, 1.0)
        }

    let runTest() =

        let effect =
            SequentialComposition.compose [
                trafo |> toEffect
                constructedColor |> toEffect
            ]

        let res = SpirVCompiler.compileEffect (Map.ofList ["Colors", typeof<V4d>]) effect

        match res with
            | Success(compiled) ->
                for (stage, m) in Map.toSeq compiled.modules do
                    printfn "%A:" stage
                    printfn "    // Id's are bound by: %A" m.bound
                    let str = InstructionPrinter.toString m.instructions
                    printfn "%s" (String.indent 1 str)

                    let file =
                        match stage with
                            | Vertex -> "vert.spv"
                            | Fragment -> "frag.spv"
                            | _ -> "other.spv"

                    let path = System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop)
                    let file = System.IO.Path.Combine(path, file)
                    if System.IO.File.Exists file then
                        System.IO.File.Delete file

                    use s = System.IO.File.OpenWrite(file)
                    let writer = new System.IO.BinaryWriter(s)
                    SpirV.Serializer.write m writer

            | Error e ->
                failwith e

