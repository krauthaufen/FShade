namespace FShade.SpirV

open SpirV


type Type =
    | Void
    | Function of args : list<Type> * retType : Type
    | Int of width : int * signed : bool
    | Float of width : int
    | Vector of compType : Type * dim : int
    | Matrix of colType : Type * dim : int
    | Array of elementType : Type * length : int
    | Struct of name : string * fields : list<Type * string>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Type =

    let Int8 = Int(8, true)
    let UInt8 = Int(8, false)

    let Int16 = Int(16, true)
    let UInt16 = Int(16, false)

    let Int32 = Int(32, true)
    let UInt32 = Int(32, false)

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

type Expr =
    | Label of Label
    | Jump of Label
    | ConditionalJump of Expr * Label * Label
    | Ret
    | Block of list<Expr>
    | VarDef of Type * string
    | Var of string
    | IntValue of int
    | FloatValue of float32




[<AutoOpen>]
module SpirVBuilders =
    
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


    type SpirVState = 
        {
            currentId : uint32
            instructions : RevList<Instruction>
            typeCache : Map<Type, uint32>
            labelIds : Map<Label, uint32>
        }


    type SpirV<'a>= { build : SpirVState -> SpirVState * 'a }


    let newId = { build = fun s -> { s with currentId = s.currentId + 1u}, s.currentId}

    let tryFindTypeId (t : Type) =
        { build = fun s ->
            s, Map.tryFind t s.typeCache
        }

    let setTypeId (t : Type) (id : uint32) =
        { build = fun s ->
            { s with typeCache = Map.add t id s.typeCache }, ()
        }


    let tryFindLabelId (l : Label) =
        { build = fun s ->
            s, Map.tryFind l s.labelIds
        }

    let setLabelId (l : Label) (id : uint32) =
        { build = fun s ->
            { s with labelIds = Map.add l id s.labelIds }, ()
        }


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


    module List =
        let mapSpv (f : 'a -> SpirV<'b>) (l : list<'a>) =
            { build = fun s ->
                l |> List.fold (fun (s,l) v -> 
                    let (s,e) = (f v).build s
                    (s, e::l)
                ) (s, [])
            }

        let mapSpvi (f : int -> 'a -> SpirV<'b>) (l : list<'a>) =
            { build = fun s ->
                let (_,s,res) =
                    l |> List.fold (fun (i, s,l) v -> 
                        let (s,e) = (f i v).build s
                        (i + 1, s, e::l)
                    ) (0, s, [])
                (s, res)
            }
    module Seq =
        let mapSpv (f : 'a -> SpirV<'b>) (l : seq<'a>) =
            { build = fun s ->
                l |> Seq.fold (fun (s,l) v -> 
                    let (s,e) = (f v).build s
                    (s, e::l)
                ) (s, [])
            }


module SpirV =
    
    let rec getTypeId (t : Type) =
        spirv {
            let! existing = tryFindTypeId t
            match existing with
                | Some id -> return id
                | None ->
                    let! id = newId
                    match t with
                        | Void -> 
                            yield OpTypeVoid id
                        | Int(w,s) ->
                            yield OpTypeInt(id, uint32 w, if s then 1u else 0u)
                        | Float(w) ->
                            yield OpTypeFloat(id, uint32 w)

                        | Function(args, ret) ->
                            let! args = List.mapSpv getTypeId args
                            let! ret = getTypeId ret

                            yield OpTypeFunction(id, ret, args |> List.map uint32 |> List.toArray)

                        | Vector(bt, dim) ->
                            let! bt = getTypeId bt
                            yield OpTypeVector(id, bt, uint32 dim)

                        | Matrix(bt, dim) ->
                            let! bt = getTypeId bt
                            yield OpTypeMatrix(id, bt, uint32 dim)

                        | Array(et, len) ->
                            let! et = getTypeId et
                            yield OpTypeArray(id, et, uint32 len)

                        | Struct(name, fields) ->
                            
                            let! fields = 
                                fields |> List.mapSpvi (fun i (t,n) -> spirv { let! tid = getTypeId t in return (tid, i, n)})

                            for (_,i,n) in fields do
                                yield OpMemberName(id, uint32 i, n)

                            yield OpName(id, name)
                            yield OpTypeStruct(id, fields |> List.map (fun (t,_,_) -> t) |> List.toArray)


                    do! setTypeId t id
                    return id
        }

    let getLabelId (l : Label) =
        spirv {
            let! id = tryFindLabelId l
            match id with
                | Some id ->
                    return id
                | None ->
                    let! id = newId
                    do! setLabelId l id
                    return id
        }
    

    let rec compile (e : Expr) =
        spirv {
            match e with
                | Ret -> 
                    yield OpReturn
                    return None

                | IntValue v ->
                    let! t = getTypeId Type.Int32
                    let! id = newId
                    yield OpConstant(t, id, [|uint32 v|])
                    return Some id

                | _ ->  
                    return None
        }

