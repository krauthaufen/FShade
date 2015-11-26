namespace FShade.SpirV

open Aardvark.Base
open SpirV


type Type =
    | Void
    | Bool
    | Function of args : list<Type> * retType : Type
    | Int of width : int * signed : bool
    | Float of width : int
    | Vector of compType : Type * dim : int
    | Matrix of colType : Type * dim : int
    | Array of elementType : Type * length : int
    | Struct of name : string * fields : list<Type * string>
    | Image of sampledType : Type * dim : Dim * depth : int * arrayed : bool * ms : int * sampled : bool * format : int
    | SampledImage of Type

    //sampledType : uint32 * dim : Dim * depth : uint32 * arrayed : uint32 * ms : uint32 * sampled : uint32 * format : int * access : Option<AccessQualifier>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Type =

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


    let rec (|Integral|_|) (t : Type) =
        match t with
            | Int(_,_) -> Some()
            | Vector(Integral,_) -> Some ()
            | Matrix(Integral,_) -> Some ()
            | _ -> None

    let rec (|Fractional|_|) (t : Type) =
        match t with
            | Float(_) -> Some()
            | Vector(Fractional,_) -> Some ()
            | Matrix(Fractional,_) -> Some ()
            | _ -> None

    let rec (|Signed|_|) (t : Type) =
        match t with
            | Int(_,true) -> Some ()
            | Vector(Signed,_) -> Some ()
            | Matrix(Signed,_) -> Some ()
            | _ -> None

    let rec (|Unsigned|_|) (t : Type) =
        match t with
            | Int(_,false) -> Some ()
            | Vector(Unsigned,_) -> Some ()
            | Matrix(Unsigned,_) -> Some ()
            | _ -> None

    let private addTypes =
        Dictionary.ofList [
            (Int8, Int8), Int8
        ]


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

type Var(name : string, t : Type, isMutable : bool) =
    let id = System.Guid.NewGuid()

    member x.Id = id
    member x.Name = name
    member x.Type = t
    member x.Mutable = isMutable

    interface System.IComparable with
        member x.CompareTo(o) =
            match o with
                | :? Var as o -> compare id o.Id
                | _ -> failwith "uncomparable"

    override x.Equals o =
        System.Object.ReferenceEquals(x,o)

    override x.GetHashCode() =
        System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x)
      
    new(name : string, t : Type) = Var(name, t, false)         
    new(t : Type) = Var(System.Guid.NewGuid().ToString(), t, false)          

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


type UnaryOperation =
    | Negate


type ExpressionType =
    | Custom = 0
    | Binary = 1
    | Unary = 2
    | NewObject = 3
    | Var = 4
    | Value = 5
    | Return = 6
    | Unit = 7
    | Block = 8
    | Label = 9
    | VectorSwizzle = 10
    | Let = 11
    | IfThenElse = 12

type Expr =
    abstract member Type : Type
    abstract member Kind : ExpressionType
    abstract member Substitute : (Var -> Option<Var>) -> Expr


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Expr =

    [<AutoOpen>]
    module private Types =
        let rec private multiplicationType (l : Type) (r : Type) =
            match l, r with
                // vec * vec 
                | Vector(lt,ld), Vector(rt,rd) when ld = rd -> 
                    match multiplicationType lt rt with
                        | Some res -> Vector(res, ld) |> Some
                        | None -> None

                | Vector(_,_), Vector(_,_) ->
                    None

                // mat * mat
                | Matrix(Vector(lt, lr), lc), Matrix(Vector(rt, rr), rc) when rr = lc ->
                    match multiplicationType lt rt with
                        | Some res ->
                            Matrix(Vector(res, lr), rc) |> Some
                        | None ->
                            None

                // mat * vec -> vec
                | Matrix(Vector(mt, rows), cols), Vector(vt, dim) when dim = cols && mt = vt ->
                    Some <| Vector(vt, rows)

                // mat * scalar | scalar * mat
                | Matrix(Vector(mt,r), c), s | s, Matrix(Vector(mt,r), c) ->
                    match multiplicationType mt s with
                        | Some res ->
                            Matrix(Vector(res, r), c) |> Some
                        | None ->
                            None

                // scalar * vec | vec * scalar -> vec
                | Vector(vt, dim), s | s, Vector(vt, dim) ->
                    match multiplicationType vt s with
                        | Some res ->
                            Some <| Vector(res, dim)
                        | None -> 
                            None

                // float * float
                | Float(lw), Float(rw) when lw = rw ->
                    Some (Float(lw))

                // int * int
                | Int(lw, ls), Int(rw, rs) when lw = rw && ls = rs ->
                    Some (Int(lw, ls))

                // int * float | float * int
                | Int(iw, _), Float(fw) | Float(fw), Int(iw, _) when fw >= iw ->
                    Float(fw) |> Some

                | _ -> None

        let rec private additionType (l : Type) (r : Type) =
            match l,r with
                // int + int
                | Int(lw, ls), Int(rw, rs) when ls = rs ->
                    Some (Int(max lw rw, ls))

                // float + float
                | Float(lw), Float(rw) ->
                    Some (Float(max lw rw))

                // vec + vec
                | Vector(lt, ld), Vector(rt, rd) ->
                    match additionType lt rt with
                        | Some res -> Vector(res, max ld rd) |> Some
                        | None -> None

                // mat + mat
                | Matrix(lv, lc), Matrix(rv, rc) ->
                    match additionType lv rv with
                        | Some res -> Matrix(res, max lc rc) |> Some
                        | None -> None

                | _ -> None

        let rec private divisionType (l : Type) (r : Type) =
           match l, r with
                // vec / vec 
                | Vector(lt,ld), Vector(rt,rd) when ld = rd -> 
                    match multiplicationType lt rt with
                        | Some res -> Vector(res, ld) |> Some
                        | None -> None

                | Vector(_,_), Vector(_,_) ->
                    None

                // mat / mat
                | Matrix(Vector(lt, lr), lc), Matrix(Vector(rt, rr), rc) when rr = lc ->
                    match multiplicationType lt rt with
                        | Some res ->
                            Matrix(Vector(res, lr), rc) |> Some
                        | None ->
                            None

                // mat / scalar 
                | Matrix(Vector(mt,r), c), s ->
                    match multiplicationType mt s with
                        | Some res ->
                            Matrix(Vector(res, r), c) |> Some
                        | None ->
                            None

                // scalar / vec | vec / scalar -> vec
                | Vector(vt, dim), s | s, Vector(vt, dim) ->
                    match multiplicationType vt s with
                        | Some res ->
                            Some <| Vector(res, dim)
                        | None -> 
                            None

                // float / float
                | Float(lw), Float(rw) when lw = rw ->
                    Some (Float(lw))

                // int / int
                | Int(lw, ls), Int(rw, rs) when lw = rw && ls = rs ->
                    Some (Int(lw, ls))

                // int / float | float / int
                | Int(iw, _), Float(fw) | Float(fw), Int(iw, _) when fw >= iw ->
                    Float(fw) |> Some

                | _ -> None


        let private binaryOpResultType (op : BinaryOperation) (l : Type) (r : Type) =
            let res = 
                match op with
                    | Add | Sub -> additionType l r
                    | Mul  -> multiplicationType l r
                    | Div | Mod -> divisionType l r
                    | _ -> Some Type.Bool

            match res with
                | Some res -> res
                | None -> failwithf "could not determine type for %A(%A, %A)" op l r

        [<AbstractClass>]
        type AbstractExpression(kind : ExpressionType, t : Type) =
            member x.Type = t
            member x.Kind = kind

            abstract member Substitute : (Var -> Option<Var>) -> Expr

            interface Expr with
                member x.Type = t
                member x.Kind = kind
                member x.Substitute f = x.Substitute f
       
        [<AbstractClass>] 
        type DerivedExpression(kind : ExpressionType, t : Type, args : list<Expr>) =
            inherit AbstractExpression(kind, t)

            abstract member Rebuild : list<Expr> -> Expr

            override x.Substitute f =
                let args = args |> List.map (fun e -> e.Substitute f)
                x.Rebuild args


            member x.Args = args


        type ValueExpression (t : Type, v : obj) =
            inherit AbstractExpression(ExpressionType.Value, t)
            override x.Substitute f = x :> Expr

            member x.Value = v

        type VariableExpression (v : Var) =
            inherit AbstractExpression(ExpressionType.Var, v.Type)

            override x.Substitute f =
                match f v with
                    | Some v -> VariableExpression(v) :> Expr
                    | None -> x :> Expr

            member x.Var = v

        type UnaryExpression(op : UnaryOperation, e : Expr) =
            inherit DerivedExpression(ExpressionType.Unary, e.Type, [e])
            member x.Operation = op
            member x.Operand = e
        
            override x.Rebuild args =
                UnaryExpression(op, List.head args) :> Expr

        type BinaryExpression(op : BinaryOperation, l : Expr, r : Expr) =
            inherit DerivedExpression(ExpressionType.Binary, binaryOpResultType op l.Type r.Type, [l;r])

            member x.Operation = op
            member x.Left = l
            member x.Right = r

            override x.Rebuild args =
                match args with
                    | [l;r] -> BinaryExpression(op, l, r) :> Expr
                    | _ -> failwith "impossible"

        type NewObjectExpression(t : Type, args : list<Expr>) =
            inherit DerivedExpression(ExpressionType.NewObject, t, args)

            member x.Args = args

            override x.Rebuild args =
                NewObjectExpression(t, args) :> Expr

        type ReturnExpression(e : Expr) =
            inherit DerivedExpression(ExpressionType.Return, Type.Unit, [e])

            member x.Value = e

            override x.Rebuild args =
                ReturnExpression(List.head args) :> Expr

        type UnitExpression private () =
            inherit AbstractExpression(ExpressionType.Unit, Type.Unit)
            static let single = UnitExpression()

            static member Instance = single

            override x.Substitute _ = x :> Expr
            override x.GetHashCode() = 0
            override x.Equals o =
                match o with
                    | :? UnitExpression -> true
                    | :? ValueExpression as o -> o.Type = Type.Unit && o.Value = null
                    | _ -> false

        type LabelExpression(l : Label) =
            inherit AbstractExpression(ExpressionType.Label, Type.Unit)

            member x.Label = l

            override x.Substitute _ = x :> Expr

        type BlockExpression(statements : list<Expr>) =
            inherit DerivedExpression(ExpressionType.Block, Type.Unit, statements)
            do assert (match statements with | [] -> true | (:? LabelExpression)::_ -> true | _ -> false )
               assert (statements |> List.forall (fun s -> s.Type = Type.Unit))

            member x.Statements = statements

            override x.Rebuild args =
                BlockExpression(args) :> Expr

        type VectorComponentExpression(comp : int, v : Expr) =
            inherit DerivedExpression(ExpressionType.VectorSwizzle, (match v.Type with | Vector(bt,dim) -> (if comp < dim then bt else failwith "vector-component out of bounds") | _ -> failwith "not a vector type"), [v])

            member x.Component = comp
            member x.Vector = v

            override x.Rebuild args =
                VectorComponentExpression(comp, List.head args) :> Expr

        type MatrixElementExpression(row : int, col : int, m : Expr) =
            inherit DerivedExpression(
                ExpressionType.VectorSwizzle, 
                (match m.Type with | Matrix(Vector(bt,rows), cols) -> (if row < rows && col < cols then bt else failwith "matrix-component out of bounds") | _ -> failwith "not a matrix type"), 
                [m]
            )

            member x.Row = row
            member x.Col = col
            member x.Matrix = m

            override x.Rebuild args =
                MatrixElementExpression(row, col, List.head args) :> Expr

        type LetExpression(v : Var, e : Expr, body : Expr) =
            inherit AbstractExpression(ExpressionType.Let, body.Type)

            override x.Substitute f =
                let b = body.Substitute f
                let e = e.Substitute f

                match f v with
                    | Some v -> LetExpression(v, e, b) :> Expr
                    | None -> LetExpression(v,e,b) :> Expr

            member x.Var = v
            member x.Expr = e
            member x.Body = body

        type IfThenElseExpression(cond : Expr, ifTrue : Expr, ifFalse : Expr) =
            inherit DerivedExpression(ExpressionType.IfThenElse, ifTrue.Type, [cond; ifTrue; ifFalse])

            member x.Condition = cond
            member x.IfTrue = ifTrue
            member x.IfFalse = ifFalse

            override x.Rebuild args =
                match args with
                    | [cond; i; e] -> IfThenElseExpression(cond, i, e) :> Expr
                    | _ -> failwith "impossible"


    let Value(t : Type, v : obj) = ValueExpression(t, v) :> Expr
    let Var(v : Var) = VariableExpression(v) :> Expr
    
    let Unary(op : UnaryOperation, e : Expr) = UnaryExpression(op, e) :> Expr
    let Negate(e : Expr) = UnaryExpression(Negate, e) :> Expr

    let Binary(op : BinaryOperation, l : Expr, r : Expr) = BinaryExpression(op, l, r) :> Expr
    let Add(l : Expr, r : Expr) = BinaryExpression(Add, l, r) :> Expr
    let Sub(l : Expr, r : Expr) = BinaryExpression(Sub, l, r) :> Expr
    let Mul(l : Expr, r : Expr) = BinaryExpression(Mul, l, r) :> Expr
    let Div(l : Expr, r : Expr) = BinaryExpression(Div, l, r) :> Expr
    let Mod(l : Expr, r : Expr) = BinaryExpression(Mod, l, r) :> Expr

    let Greater(l : Expr, r : Expr) = BinaryExpression(Greater, l, r) :> Expr
    let GreaterOrEqual(l : Expr, r : Expr) = BinaryExpression(GreaterOrEqual, l, r) :> Expr
    let Smaller(l : Expr, r : Expr) = BinaryExpression(Smaller, l, r) :> Expr
    let SmallerOrEqual(l : Expr, r : Expr) = BinaryExpression(SmallerOrEqual, l, r) :> Expr
    let Equal(l : Expr, r : Expr) = BinaryExpression(Equal, l, r) :> Expr
    let NotEqual(l : Expr, r : Expr) = BinaryExpression(NotEqual, l, r) :> Expr


    let NewObject(t : Type, args : list<Expr>) = NewObjectExpression(t, args) :> Expr
    let Unit = UnitExpression.Instance :> Expr

    let Return(v : Expr) = ReturnExpression(v) :> Expr
    let Label (l : Label) = LabelExpression(l) :> Expr
    let Block (body : list<Expr>) = BlockExpression(body) :> Expr

    let VectorComponent(vec : Expr, comp : int) = VectorComponentExpression(comp, vec) :> Expr
    let MatrixElement(mat : Expr, row : int, col : int) = MatrixElementExpression(row, col, mat) :> Expr
    let Let(v : Var, e : Expr, body : Expr) = LetExpression(v, e, body) :> Expr
    let IfThenElse(c : Expr, ifTrue : Expr, ifFalse : Expr) = IfThenElseExpression(c, ifTrue, ifFalse) :> Expr


    module Patterns =
        let (|Value|_|) (e : Expr) =
            match e with
                | :? ValueExpression as e -> Some(e.Type, e.Value)
                | _ -> None

        let (|Var|_|) (e : Expr) =
            match e with
                | :? VariableExpression as v -> Some v.Var
                | _ -> None

        let (|Unary|_|) (e : Expr) =
            match e with
                | :? UnaryExpression as e -> Some(e.Operation, e.Operand)
                | _ -> None

        let (|Binary|_|) (e : Expr) =
            match e with
                | :? BinaryExpression as e -> Some(e.Operation, e.Left, e.Right)
                | _ -> None

        let (|NewObject|_|) (e : Expr) =
            match e with
                | :? NewObjectExpression as e -> Some(e.Type, e.Args)
                | _ -> None

        let (|UnitVal|_|) (e : Expr) =
            if UnitExpression.Instance.Equals e then Some ()
            else None

        let (|Return|_|) (e : Expr) =
            match e with
                | :? ReturnExpression as e -> Some(e.Value)
                | _ -> None

        let (|Label|_|) (e : Expr) =
            match e with
                | :? LabelExpression as e -> Some(e.Label)
                | _ -> None

        let (|Block|_|) (e : Expr) =
            match e with
                | :? BlockExpression as e -> Some e.Statements
                | _ -> None

        let (|VectorComponent|_|) (e : Expr) =
            match e with
                | :? VectorComponentExpression as e -> Some(e.Vector, e.Component)
                | _ -> None

        let (|MatrixElement|_|) (e : Expr) =
            match e with
                | :? MatrixElementExpression as e -> Some(e.Matrix, e.Row, e.Col)
                | _ -> None

        let (|Let|_|) (e : Expr) =
            match e with
                | :? LetExpression as e -> Some(e.Var, e.Expr, e.Body)
                | _ -> None

        let (|IfThenElse|_|) (e : Expr) =
            match e with
                | :? IfThenElseExpression as e -> Some(e.Condition, e.IfTrue, e.IfFalse)
                | _ -> None

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
            variableIds : Map<Var, uint32>
        } with
            
            static member Empty = 
                {
                    currentId = 0u
                    instructions = Nil
                    typeCache = Map.empty
                    labelIds = Map.empty
                    variableIds = Map.empty
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

    let getVarId (v : Var) =
        { build = fun s ->
            match Map.tryFind v s.variableIds with
                | Some id -> s,id
                | None ->
                    let (s, id) = newId.build s
                    { s with variableIds = Map.add v id s.variableIds }, id
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
    open Expr.Patterns

    let rec getTypeId (t : Type) =
        spirv {
            let! existing = tryFindTypeId t
            match existing with
                | Some id -> return id
                | None ->
                    let! id = newId
                    match t with
                        | Bool ->
                            yield OpTypeBool id

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

                        | Image(sampledType, dim, depth, arrayed, ms, sampled, format) ->
                            let! sampledType = getTypeId sampledType
                            yield OpTypeImage(id, sampledType, dim, uint32 depth, (if arrayed then 1u else 0u), uint32 ms, (if sampled then 1u else 0u), format, None)

                        | SampledImage(imageType) ->
                            let! imageType = getTypeId imageType
                            yield OpTypeSampledImage(id, imageType)

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
    

    let rec compile (self : Expr) =
        spirv {
            match self with
                | Return(UnitVal) -> 
                    yield OpReturn
                    return None

                | Return e ->
                    let! e = compile e
                    match e with
                        | Some e ->
                            yield OpReturnValue(e)
                            return None
                        | None ->
                            yield OpReturn
                            return None

                | Binary(op, l, r) ->
                    let! tid = getTypeId self.Type
                    let! lid = compile l
                    let! rid = compile r
                    let! id = newId

                    match lid, rid with
                        | Some lid, Some rid ->
                            match l.Type with
                                | Type.Integral ->
                                    match op with
                                        | Add -> yield OpIAdd(tid, id, lid, rid)
                                        | Sub -> yield OpISub(tid, id, lid, rid)
                                        | Mul -> yield OpIMul(tid, id, lid, rid)
                                        | Div -> 
                                            match self.Type with
                                                | Type.Signed -> yield OpSDiv(tid, id, lid, rid)
                                                | _ -> yield OpUDiv(tid, id, lid, rid)
                                        | Mod ->
                                            match self.Type with
                                                | Type.Signed -> yield OpSMod(tid, id, lid, rid)
                                                | _ -> yield OpUMod(tid, id, lid, rid)

                                        | Greater ->
                                            match self.Type with
                                                | Type.Signed -> yield OpSGreaterThan(tid, id, lid, rid)
                                                | _ -> yield OpUGreaterThan(tid, id, lid, rid)

                                        | GreaterOrEqual ->
                                            match self.Type with
                                                | Type.Signed -> yield OpSGreaterThanEqual(tid, id, lid, rid)
                                                | _ -> yield OpUGreaterThanEqual(tid, id, lid, rid)

                                        | Smaller ->
                                            match self.Type with
                                                | Type.Signed -> yield OpSLessThan(tid, id, lid, rid)
                                                | _ -> yield OpULessThan(tid, id, lid, rid)

                                        | SmallerOrEqual ->
                                            match self.Type with
                                                | Type.Signed -> yield OpSLessThanEqual(tid, id, lid, rid)
                                                | _ -> yield OpULessThanEqual(tid, id, lid, rid)
                                        | Equal ->
                                            yield OpIEqual(tid, id, lid, rid)

                                        | NotEqual ->
                                            yield OpINotEqual(tid, id, lid, rid)
                                        
                                | Type.Fractional ->
                                    match op with
                                        | Add -> yield OpFAdd(tid, id, lid, rid)
                                        | Sub -> yield OpFSub(tid, id, lid, rid)
                                        | Mul -> yield OpFMul(tid, id, lid, rid)
                                        | Div -> yield OpFDiv(tid, id, lid, rid)
                                        | Mod -> yield OpFMod(tid, id, lid, rid)

                                        | Greater -> yield OpFOrdGreaterThan(tid, id, lid, rid)
                                        | GreaterOrEqual -> yield OpFOrdGreaterThanEqual(tid, id, lid, rid)
                                        | Smaller -> yield OpFOrdLessThan(tid, id, lid, rid)
                                        | SmallerOrEqual -> yield OpFOrdLessThanEqual(tid, id, lid, rid)
                                        | Equal -> yield OpFOrdEqual(tid, id, lid, rid)
                                        | NotEqual -> yield OpFOrdNotEqual(tid, id, lid, rid)

                                | _ ->
                                    failwithf "cannot add type: %A" self.Type

                            return Some id
                        | _ ->
                            return failwithf "cannot add non-value expressions: %A %A" l r

                | Unary(op, e) ->
                    let! e = compile e

                    match e with
                        | Some e ->
                            let! id = newId
                            let! tid = getTypeId self.Type
                            match self.Type with
                                | Type.Signed -> yield OpSNegate(tid, id, e)
                                | _ -> yield OpFNegate(tid, id, e)

                            return Some id
                        | None ->
                            return failwithf "non-value expression found %A" e

                | Block(statements) ->
                    for s in statements do
                        let! r = compile s
                        match r with
                            | None -> ()
                            | Some v -> Log.warn "unused value: %A" v

                    return None

                | NewObject(t, values) ->
                    
                    let! values = values |> List.mapSpv compile

                    let values =
                        if values |> List.forall Option.isSome then
                            values |> List.map (fun v -> v.Value) |> List.toArray
                        else
                            failwith "cannot create composite type from non-value expressions"

                    let! id = newId
                    let! tid = getTypeId t
                    yield OpCompositeConstruct(tid, id, values)

                    return Some id

                | Var(v) ->
                    let! id = getVarId v
                    return Some id

                | Value(t, v) ->
                    let! t = getTypeId t
                    let! id = newId

                    let bytes =
                        match v with
                            | :? int32 as v -> System.BitConverter.GetBytes(v)
                            | :? uint32 as v -> System.BitConverter.GetBytes(v)
                            | :? int64 as v -> System.BitConverter.GetBytes(v)
                            | :? uint64 as v -> System.BitConverter.GetBytes(v)
                            | :? float32 as v -> System.BitConverter.GetBytes(v)
                            | :? float as v -> System.BitConverter.GetBytes(v)
                            | _ -> failwithf "unsupported constant-value: %A" v

                    let words =
                        Array.init ((bytes.Length + 3 &&& ~~~3) / 4) (fun i ->
                            System.BitConverter.ToUInt32(bytes, 4 * i)
                        )

                    yield OpConstant(t, id, words)
                    return Some id

                | Label(l) ->
                    let! id = getLabelId l
                    yield OpLabel(id)
                    return None

                | Let(v,e,b) ->
                    let! e = compile e
                    match e with
                        | Some e ->
                            let! tid = getTypeId v.Type
                            let! vid = getVarId v
                            
                            yield OpVariable(tid, vid, StorageClass.Private, Some e)
                            yield OpName(vid, v.Name)

                            let! b = compile b
                            return b

                        | None ->
                            return failwith "cannot let non-value expression"

                | IfThenElse(c,i,e) ->
                    let! c = compile c

                    match c with
                        | Some c ->
                            
                            let! trueLabel = getLabelId <| Label()
                            let! falseLabel = getLabelId <| Label()
                            let! endLabel = getLabelId <| Label()

                            yield OpBranchConditional(c, trueLabel, falseLabel, [||])

                            yield OpLabel(trueLabel)
                            let! _ = compile i
                            yield OpBranch(endLabel)

                            yield OpLabel(falseLabel)
                            let! _ = compile e
                            yield OpLabel(endLabel)

                            return None
                        | None ->
                            return failwith "non-value condition in ifthenelse"


                | _ ->  
                    return None
        }


    let test2() =
        let v = new Var("v", Type.M34f)
        let a = new Var("a", Type.V4f)

        let v = Expr.Binary(BinaryOperation.Mul, Expr.Var(v), Expr.Var(a))

        printfn "%A" v.Type

    let test() =
        test2()

        let l = new Label()
        let l1 = new Label()
        let l2 = new Label()
        let v = new Var("a", Type.Float64)
        let v2 = new Var("b", Type.Float64)
        let ex = 
            Expr.Block [
                Expr.Label l
                Expr.Let(v, Expr.Value(Type.Float64, 1.0), 
                    Expr.IfThenElse(
                        Expr.Smaller(Expr.Var v, Expr.Value(Type.Float64, 2.0)),
                        Expr.Block [
                            Expr.Label l2
                            Expr.Let(v2, Expr.Div(Expr.NewObject(Type.V2d, [Expr.Var v; Expr.Var v]), Expr.Var v), 
                                Expr.Return (Expr.Var v2)
                            )
                        ],
                        Expr.Return(Expr.NewObject(Type.V2d, [Expr.Value(Type.Float64, 0.0); Expr.Value(Type.Float64, 0.0)]))
                    )
                )
            ]

        let s = compile ex
        let (s, e) = s.build SpirVState.Empty

        let instructions = s.instructions |> RevList.toList

        printfn "bound: %A" s.currentId
        for i in instructions do
            printfn "%A" i
