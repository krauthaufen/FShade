namespace FShade.SpirV

open Aardvark.Base
open SpirV

#nowarn "9"
#nowarn "51"

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

    | And | Or
    | BitAnd | BitOr | BitXor | LeftShift | RightShiftArithmetic | RightShiftLogic


type UnaryOperation =
    | Negate
    | All
    | Any

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


type Function = FunctionDefinition of arguments : list<Var> * body : Expr


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
                    | Greater | Smaller | GreaterOrEqual | SmallerOrEqual ->
                        if l = r then
                            match l with
                                | Int(_,_) | Float(_) -> Some Type.Bool
                                | Vector(_,d) -> Vector(Type.Bool, d) |> Some
                                | _ -> None
                        else
                            None

                    | And | Or ->
                        if l = r then
                            match l with
                                | Bool -> Some Type.Bool
                                | Vector(Bool,d) -> Vector(Type.Bool, d) |> Some
                                | _ -> None
                        else
                            None

                    | BitAnd | BitOr | BitXor | LeftShift | RightShiftArithmetic | RightShiftLogic ->
                        match r with
                            | Int(_,_) ->
                                match l with
                                    | Int(_,_) -> Some l
                                    | _ -> None
                            | _ -> None

                    | Equal | NotEqual ->
                        if l = r then
                            match l with
                                | Int(_,_) | Float(_) | Bool -> Some Type.Bool
                                | Vector(_,d) -> Vector(Type.Bool, d) |> Some
                                | _ -> None
                        else
                            None

            match res with
                | Some res -> res
                | None -> failwithf "could not determine type for %A(%A, %A)" op l r

        let private unaryOpResultType (op : UnaryOperation) (i : Type) =
            match op with
                | Negate -> i
                | All -> Bool
                | Any -> Bool

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
            inherit DerivedExpression(ExpressionType.Unary, unaryOpResultType op e.Type, [e])
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
            inherit DerivedExpression(ExpressionType.Block, statements |> Seq.last |> (fun e -> e.Type), statements)

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
            do assert(cond.Type = Bool)
               assert(ifTrue.Type = ifFalse.Type)

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

    let And(l : Expr, r : Expr) = BinaryExpression(And, l, r) :> Expr
    let Or(l : Expr, r : Expr) = BinaryExpression(Or, l, r) :> Expr
    
    let BitAnd(l : Expr, r : Expr) = BinaryExpression(BitAnd, l, r) :> Expr
    let BitOr(l : Expr, r : Expr) = BinaryExpression(BitOr, l, r) :> Expr
    let BitXor(l : Expr, r : Expr) = BinaryExpression(BitXor, l, r) :> Expr

    let LeftShift(l : Expr, r : Expr) = BinaryExpression(LeftShift, l, r) :> Expr
    let RightShiftLogic(l : Expr, r : Expr) = BinaryExpression(RightShiftLogic, l, r) :> Expr
    let RightShiftArithmetic(l : Expr, r : Expr) = BinaryExpression(RightShiftArithmetic, l, r) :> Expr



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


    type SpirVState = 
        {
            currentId : uint32
            typeInstructions : RevList<Instruction>
            instructions : RevList<Instruction>
            typeCache : Map<Type, uint32>
            labelIds : Map<Label, uint32>
            variableIds : Map<Var, uint32>
            variableDeclarationLabels : Map<Var, uint32>
            expressionValueCache : HashMap<Expr, uint32>
      
        } with
            
            static member Empty = 
                {
                    currentId = 0u
                    typeInstructions = Nil
                    instructions = Nil
                    typeCache = Map.empty
                    labelIds = Map.empty
                    variableIds = Map.empty
                    variableDeclarationLabels = Map.empty
                    expressionValueCache = HashMap.empty
                }


    type SpirV<'a>= { build : SpirVState -> SpirVState * 'a }


    let newId = { build = fun s -> { s with currentId = s.currentId + 1u}, s.currentId}

    let lastInstruction =
        { build = fun s ->
            s, (match s.instructions with | Snoc(_,i) -> Some i | Nil -> None)
        }

    let tryFindTypeId (t : Type) =
        { build = fun s ->
            s, Map.tryFind t s.typeCache
        }

    let setTypeId (t : Type) (id : uint32) =
        { build = fun s ->
            { s with typeCache = Map.add t id s.typeCache }, ()
        }

    let tryFindExprCache (e : Expr) =
        { build = fun s ->
            s, HashMap.tryFind e s.expressionValueCache
        }

    let setExprValue (e : Expr) (value : uint32) =
         { build = fun s -> {s with expressionValueCache = HashMap.add e value s.expressionValueCache },() }


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

    let getVarId (v : Var) =
        { build = fun s ->
            match Map.tryFind v s.variableIds with
                | Some id -> 
                    s,id
                | None ->
                    let (s, id) = newId.build s
                    let (s, lid) = getLabelId(Label()).build s

                    { s with 
                        variableIds = Map.add v id s.variableIds
                        instructions = Snoc(s.instructions, OpLabel(lid)) 
                        variableDeclarationLabels = Map.add v lid s.variableDeclarationLabels
                    }, id
        }

    let getVarLabelId (v : Var) =
        { build = fun s ->
            match Map.tryFind v s.variableDeclarationLabels with
                | Some id -> 
                    s,id
                | None ->
                    let (s, id) = newId.build s
                    let (s, lid) = getLabelId(Label()).build s

                    { s with 
                        variableIds = Map.add v id s.variableIds
                        instructions = Snoc(s.instructions, OpLabel(lid)) 
                        variableDeclarationLabels = Map.add v lid s.variableDeclarationLabels
                    }, lid
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
    open System.Runtime.InteropServices

    let rec getTypeId (t : Type) =
        spirvType {
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

    let getLabelId (l : Label) = getLabelId l
    


    let byteArray (v : obj) =
        let s = Marshal.SizeOf(v)
        let arr : byte[] = Array.zeroCreate s

        let gc = GCHandle.Alloc(arr, GCHandleType.Pinned)
        Marshal.StructureToPtr(v, gc.AddrOfPinnedObject(), false)
        gc.Free()


        arr
            

    let rec private compileConstant (t : Type) (data : byte[]) =
        spirv {
            let! tid = getTypeId t
            let! id = newId

            let skip (n : int) (arr : byte[]) =
                if n = 0 then arr
                else Array.sub arr n (arr.Length - n)

            let take (n : int) (arr : byte[]) =
                if arr.Length = n then arr
                elif arr.Length > n then Array.sub arr 0 n
                else Array.append (Array.zeroCreate (n - arr.Length)) arr

            let toWordArray (arr : byte[]) : uint32[] =
                let wordCount = ((arr.Length + 3) &&& ~~~3) / 4
                let real = Array.zeroCreate wordCount
                real.UnsafeCoercedApply<byte>(fun real -> arr.CopyTo(real, 0))
                real

            match t with
                | Int(w,_) | Float(w) ->
                    let byteSize = w / 8
                    yield OpConstant(tid, id, data |> take byteSize |> toWordArray)
                    return id, byteSize


                | Bool ->
                    yield OpConstant(tid, id, data |> take 4 |> toWordArray)
                    return id, 4

                | Vector(bt, d) | Matrix(bt, d) | Array(bt, d) ->
                    
                    let ids = Array.zeroCreate d
                    let mutable offset = 0

                    for i in 0..d-1 do
                        let! (iid, size) = compileConstant bt (skip offset data)
                        offset <- offset + size
                        ids.[i] <- iid

                    yield OpConstantComposite(tid, id, ids)
                    return (id, offset)

                | Struct(_, fields) ->
                    
                    let fields = List.toArray fields
                    let ids = Array.zeroCreate fields.Length
                    let mutable offset = 0

                    for i in 0..fields.Length-1 do
                        let (t,_) = fields.[i]
                        let! (iid, size) = compileConstant t (skip offset data)
                        offset <- offset + size
                        ids.[i] <- iid

                    yield OpConstantComposite(tid, id, ids)
                    return (id, offset)

                | _ ->
                    return failwithf "unsupported constant-type: %A" t
                    
        }

    let rec compileBinaryOperator (resultType : Type) (op : BinaryOperation) (l : Expr) (r : Expr) =
        spirv {
            let! lid = compileExpr l
            let! rid = compileExpr r
            match lid, rid with
                | Some lid, Some rid ->
                    let! tid = getTypeId resultType
                    let! id = newId

                    match op with
                        | Add ->
                            assert(resultType = l.Type)
                            match l.Type,r.Type with
                                | Int(_,_), Int(_,_) when l.Type = r.Type ->
                                    yield OpIAdd(tid, id, lid, rid)

                                | Float(_), Float(_) when l.Type = r.Type ->
                                    yield OpFAdd(tid, id, lid, rid)
                            
                                | Vector(bt, _), Vector(_, _) when l.Type = r.Type ->
                                    match bt with
                                        | Int(_,_) -> yield OpISub(tid, id, lid, rid)
                                        | Float(_) -> yield OpFSub(tid, id, lid, rid)
                                        | _ ->
                                            failwithf "cannot sub vectors with non-scalar components: %A" bt
                                            


                                | _ -> 
                                    failwithf "cannot add %A and %A" l.Type r.Type

                        | Sub ->
                            assert(resultType = l.Type)
                            match l.Type,r.Type with
                                | Int(_,_), Int(_,_) when l.Type = r.Type ->
                                    yield OpISub(tid, id, lid, rid)

                                | Float(_), Float(_) when l.Type = r.Type ->
                                    yield OpFSub(tid, id, lid, rid)
                            
                                | Vector(bt, _), Vector(_, _) when l.Type = r.Type ->
                                    match bt with
                                        | Int(_,_) -> yield OpISub(tid, id, lid, rid)
                                        | Float(_) -> yield OpFSub(tid, id, lid, rid)
                                        | _ ->
                                            failwithf "cannot sub vectors with non-scalar components: %A" bt
                                            

                                | _ -> 
                                    failwithf "cannot sub %A and %A" l.Type r.Type

                        | Mul ->
                            match l.Type,r.Type with
                                | Int(_,_), Int(_,_) when l.Type = r.Type ->
                                    assert(resultType = l.Type)
                                    yield OpIMul(tid, id, lid, rid)

                                | Float(_), Float(_) when l.Type = r.Type ->
                                    assert(resultType = l.Type)
                                    yield OpFMul(tid, id, lid, rid)
                            
                                | Vector(bt, _), Vector(_, _) when l.Type = r.Type ->
                                    assert(resultType = l.Type)
                                    match bt with
                                        | Int(_,_) -> yield OpIMul(tid, id, lid, rid)
                                        | Float(_) -> yield OpFMul(tid, id, lid, rid)
                                        | _ ->
                                            failwithf "cannot multiply vectors with non-scalar components: %A" bt
                                    
                                | Vector(Float(_), d), Float(_) ->
                                    assert(resultType = l.Type)
                                    yield OpVectorTimesScalar(tid, id, lid, rid)

                                | Float(_), Vector(Float(_), d) ->
                                    assert(resultType = r.Type)
                                    yield OpVectorTimesScalar(tid, id, rid, lid)


                                | Vector(vt, d), Int(_,_) when vt = r.Type->
                                    assert(resultType = l.Type)
                                    let! vecId = newId
                                    yield OpCompositeConstruct(tid, vecId, Array.create d rid)
                                    yield OpIMul(tid, id, lid, vecId)

                                | Int(_,_), Vector(vt, d) when vt = l.Type->
                                    assert(resultType = r.Type)
                                    let! vecId = newId
                                    yield OpCompositeConstruct(tid, vecId, Array.create d lid)
                                    yield OpIMul(tid, id, vecId, rid)



                                | Matrix(Vector(Float(_), _),_), Float(_) ->
                                    assert(resultType = l.Type)
                                    yield OpMatrixTimesScalar(tid, id, lid, rid)

                                | Float(_), Matrix(Vector(Float(_), _),_) ->
                                    assert(resultType = r.Type)
                                    yield OpMatrixTimesScalar(tid, id, rid, lid)

                                | Matrix(Vector(mb, rows), cols), Vector(vb, d) when mb = vb && d = cols ->
                                    assert(resultType = Vector(mb, rows))
                                    yield OpMatrixTimesVector(tid, id, lid, rid)

                                | Vector(vb, d), Matrix(Vector(mb, rows), cols) when mb = vb && d = cols ->
                                    assert(resultType = Vector(mb, rows))
                                    yield OpVectorTimesMatrix(tid, id, lid, rid)

                                | _ -> 
                                    failwithf "cannot multiply %A and %A" l.Type r.Type

                        | Div ->
                            match l.Type,r.Type with
                                | Int(_,true), Int(_,true) when l.Type = r.Type ->
                                    assert(resultType = l.Type)
                                    yield OpSDiv(tid, id, lid, rid)

                                | Int(_,false), Int(_,false) when l.Type = r.Type ->
                                    assert(resultType = l.Type)
                                    yield OpUDiv(tid, id, lid, rid)


                                | Float(_), Float(_) when l.Type = r.Type ->
                                    assert(resultType = l.Type)
                                    yield OpFDiv(tid, id, lid, rid)
                            
                                | Vector(bt, _), Vector(_, _) when l.Type = r.Type ->
                                    assert(resultType = l.Type)
                                    match bt with
                                        | Int(_,true) -> yield OpSDiv(tid, id, lid, rid)
                                        | Int(_,false) -> yield OpUDiv(tid, id, lid, rid)
                                        | Float(_) -> yield OpFDiv(tid, id, lid, rid)
                                        | _ ->
                                            failwithf "cannot multiply vectors with non-scalar components: %A" bt
                                    
                                | Vector(Float(w), d), Float(_) ->
                                    assert(resultType = l.Type)
                                    let! vecId = newId
                                    yield OpCompositeConstruct(tid, vecId, Array.create d rid)
                                    yield OpFDiv(tid, id, lid, rid)

                                | Float(_), Vector(Float(_), d) ->
                                    assert(resultType = r.Type)
                                    let! vecId = newId
                                    yield OpCompositeConstruct(tid, vecId, Array.create d lid)
                                    yield OpFDiv(tid, id, vecId, rid)

                                | Vector(vt, d), Int(_,signed) when vt = r.Type ->
                                    assert(resultType = l.Type)
                                    let! vecId = newId
                                    yield OpCompositeConstruct(tid, vecId, Array.create d rid)
                                    if signed then 
                                        yield OpSDiv(tid, id, lid, vecId)
                                    else
                                        yield OpUDiv(tid, id, lid, vecId)


                                | Int(_,signed), Vector(vt, d) when vt = l.Type ->
                                    assert(resultType = r.Type)
                                    let! vecId = newId
                                    yield OpCompositeConstruct(tid, vecId, Array.create d lid)
                                    if signed then 
                                        yield OpSDiv(tid, id, vecId, rid)
                                    else
                                        yield OpUDiv(tid, id, vecId, rid)

                                | _ -> 
                                    failwithf "cannot divide %A and %A" l.Type r.Type
                                
                        | Mod ->
                            failwith "Mod not implemented"



                        | Smaller ->
                            if l.Type = r.Type then
                                match l.Type with
                                    | Int(_,true) | Vector(Int(_, true),_) -> 
                                        yield OpSLessThan(tid, id, lid, rid)

                                    | Int(_,false) | Vector(Int(_, false),_) -> 
                                        yield OpULessThan(tid, id, lid, rid)

                                    | Float(_) | Vector(Float(_),_) -> 
                                        yield OpFOrdLessThan(tid, id, lid, rid)

                                    | _ -> failwith "comparison for %A not supported" l.Type
                            else
                                failwithf "cannot compare %A to %A" l.Type r.Type

                        | SmallerOrEqual ->
                            if l.Type = r.Type then
                                match l.Type with
                                    | Int(_,true) | Vector(Int(_, true),_) -> 
                                        yield OpSLessThanEqual(tid, id, lid, rid)

                                    | Int(_,false) | Vector(Int(_, false),_) -> 
                                        yield OpULessThanEqual(tid, id, lid, rid)

                                    | Float(_) | Vector(Float(_),_) -> 
                                        yield OpFOrdLessThanEqual(tid, id, lid, rid)

                                    | _ -> failwith "comparison for %A not supported" l.Type
                            else
                                failwithf "cannot compare %A to %A" l.Type r.Type
                                
                        | Greater ->
                            if l.Type = r.Type then
                                match l.Type with
                                    | Int(_,true) | Vector(Int(_, true),_) -> 
                                        yield OpSGreaterThan(tid, id, lid, rid)

                                    | Int(_,false) | Vector(Int(_, false),_) -> 
                                        yield OpUGreaterThan(tid, id, lid, rid)

                                    | Float(_) | Vector(Float(_),_) -> 
                                        yield OpFOrdGreaterThan(tid, id, lid, rid)

                                    | _ -> failwith "comparison for %A not supported" l.Type
                            else
                                failwithf "cannot compare %A to %A" l.Type r.Type
                                
                        | GreaterOrEqual ->
                            if l.Type = r.Type then
                                match l.Type with
                                    | Int(_,true) | Vector(Int(_, true),_) -> 
                                        yield OpSGreaterThanEqual(tid, id, lid, rid)

                                    | Int(_,false) | Vector(Int(_, false),_) -> 
                                        yield OpUGreaterThanEqual(tid, id, lid, rid)

                                    | Float(_) | Vector(Float(_),_) -> 
                                        yield OpFOrdGreaterThanEqual(tid, id, lid, rid)

                                    | _ -> failwith "comparison for %A not supported" l.Type
                            else
                                failwithf "cannot compare %A to %A" l.Type r.Type
                            
                        | Equal ->
                            if l.Type = r.Type then
                                match l.Type with
                                    | Int(_,_) | Vector(Int(_, _),_) -> 
                                        yield OpIEqual(tid, id, lid, rid)

                                    | Float(_) | Vector(Float(_),_) -> 
                                        yield OpFOrdEqual(tid, id, lid, rid)

                                    | Bool | Vector(Bool, _) ->
                                        yield OpLogicalEqual(tid, id, lid, rid)

                                    | _ -> failwith "equality for %A not supported" l.Type
                            else
                                failwithf "cannot equal %A to %A" l.Type r.Type

                        | NotEqual ->
                            if l.Type = r.Type then
                                match l.Type with
                                    | Int(_,_) | Vector(Int(_, _),_) -> 
                                        yield OpINotEqual(tid, id, lid, rid)

                                    | Float(_) | Vector(Float(_),_) -> 
                                        yield OpFOrdNotEqual(tid, id, lid, rid)

                                    | Bool | Vector(Bool, _) ->
                                        yield OpLogicalNotEqual(tid, id, lid, rid)

                                    | _ -> failwith "equality for %A not supported" l.Type
                            else
                                failwithf "cannot equal %A to %A" l.Type r.Type

 

                        | And ->
                            if l.Type = r.Type then
                                match l.Type with
                                    | Bool -> yield OpLogicalAnd(tid, id, lid, rid)
                                    | Vector(Bool,_) -> yield OpLogicalAnd(tid, id, lid, rid)
                                    | _ -> failwith "logics for %A not supported" l.Type
                            else
                                failwithf "cannot and %A and %A" l.Type r.Type

                        | Or ->
                            if l.Type = r.Type then
                                match l.Type with
                                    | Bool -> yield OpLogicalOr(tid, id, lid, rid)
                                    | Vector(Bool,_) -> yield OpLogicalOr(tid, id, lid, rid)
                                    | _ -> failwith "logics for %A not supported" l.Type
                            else
                                failwithf "cannot and %A and %A" l.Type r.Type


                        | BitAnd ->
                            yield OpBitwiseAnd(tid, id, lid, rid)

                        | BitOr ->
                            yield OpBitwiseOr(tid, id, lid, rid)

                        | BitXor ->
                            yield OpBitwiseXor(tid, id, lid, rid)

                        | LeftShift ->
                            yield OpShiftLeftLogical(tid, id, lid, rid)

                        | RightShiftArithmetic ->
                            yield OpShiftRightArithmetic(tid, id, lid, rid)

                        | RightShiftLogic ->
                            yield OpShiftRightLogical(tid, id, lid, rid)

                    return id
                | _ ->
                    return failwith "non-value operation"
        }


    and compileExpr (self : Expr) : SpirV<Option<uint32>> =
        spirv {
            let! c = tryFindExprCache self
            match c with
                | Some v -> return Some v
                | None -> 
                    match self with
                        | Return(UnitVal) -> 
                            yield OpReturn
                            return None

                        | Return e ->
                            let! e = compileExpr e
                            match e with
                                | Some e ->
                                    yield OpReturnValue(e)
                                    return Some e
                                | None ->
                                    yield OpReturn
                                    return None

                        | Binary(op, l, r) ->
                            let! id = compileBinaryOperator self.Type op l r
                            do! setExprValue self id
                            return Some id

                        | Unary(op, e) ->
                            let! e = compileExpr e

                            match e with
                                | Some e ->
                                    let! id = newId
                                    let! tid = getTypeId self.Type
                                    match op with
                                        | Negate ->
                                            match self.Type with
                                                | Type.Signed -> yield OpSNegate(tid, id, e)
                                                | _ -> yield OpFNegate(tid, id, e)

                                        | All -> yield OpAll(tid, id, e)
                                        | Any -> yield OpAny(tid, id, e)

                                    do! setExprValue self id
                                    return Some id
                                | None ->
                                    return failwithf "non-value expression found %A" e

                        | Block(statements) ->
                            let mutable lastId = -1

                            let arr = List.toArray statements

                            for i in 0..arr.Length-2 do
                                let! r = compileExpr arr.[i]
                                match r with
                                    | None -> ()
                                    | Some v -> Log.warn "unused value: %A" v

                            let! r = compileExpr arr.[arr.Length-1]
                            return r

                        | NewObject(t, values) ->
                    
                            let! values = values |> List.mapSpv compileExpr

                            let values =
                                if values |> List.forall Option.isSome then
                                    values |> List.map (fun v -> v.Value) |> List.toArray
                                else
                                    failwith "cannot create composite type from non-value expressions"

                            let! id = newId
                            let! tid = getTypeId t
                            yield OpCompositeConstruct(tid, id, values)

                            do! setExprValue self id
                            return Some id

                        | Var(v) ->
                            let! id = getVarId v

                            do! setExprValue self id
                            return Some id


                        | Value(t, v) ->
                            let byteArray = byteArray v
                            let! (id, size) = compileConstant t byteArray
                            assert(size = byteArray.Length)

                            do! setExprValue self id
                            return Some id

                        | Label(l) ->
                            let! id = getLabelId l
                            yield OpLabel(id)
                            return None

                        | Let(v,e,b) ->
                            let! e = compileExpr e
                            match e with
                                | Some e ->
                                    let! tid = getTypeId v.Type
                                    let! vid = getVarId v
                            
                                    yield OpVariable(tid, vid, StorageClass.Private, Some e)
                                    yield OpName(vid, v.Name)

                                    let! b = compileExpr b
                                    return b

                                | None ->
                                    return failwith "cannot let non-value expression"


                        | IfThenElse(c, UnitVal, UnitVal) ->
                            return None

                        | IfThenElse(c, Var(i), e) when self.Type <> Type.Unit ->
                            let! c = compileExpr c

                            match c with
                                | Some c ->
                                    let! iv = getVarId i
                                    let! declId = getVarLabelId i

                                    let! falseLabel = getLabelId <| Label()
                                    let! endLabel = getLabelId <| Label()

                                    yield OpBranchConditional(c, endLabel, falseLabel, [||])
                                    yield OpLabel(falseLabel)
                                    let! ev = compileExpr e
                                    yield OpLabel(endLabel)

                                    match ev with
                                        | Some ev ->
                                            let! resType = getTypeId self.Type
                                            let! id = newId
                                            
                                            yield OpPhi(resType, id, [|iv; declId; ev; falseLabel|])
                                            return Some id
                                        | _ ->
                                            return failwith "inconsistent types in if/else"
                                | None ->
                                    return failwith "non-value condition in ifthenelse"

                        | IfThenElse(c, i, Var(e)) when self.Type <> Type.Unit ->
                            let! c = compileExpr c

                            match c with
                                | Some c ->
                                    let! ev = getVarId e
                                    let! declId = getVarLabelId e

                                    let! trueLabel = getLabelId <| Label()
                                    let! endLabel = getLabelId <| Label()

                                    yield OpBranchConditional(c, trueLabel, endLabel, [||])
                                    yield OpLabel(trueLabel)
                                    let! iv = compileExpr i
                                    yield OpLabel(endLabel)


                                    match iv with
                                        | Some iv ->
                                            let! resType = getTypeId self.Type
                                            let! id = newId
                                            
                                            yield OpPhi(resType, id, [|iv; trueLabel; ev; declId|])
                                            return Some id
                                        | _ ->
                                            return failwith "inconsistent types in if/else"
                                | None ->
                                    return failwith "non-value condition in ifthenelse"

                        | IfThenElse(c, i, UnitVal) ->
                            let! c = compileExpr c

                            match c with
                                | Some c ->
                                    let! trueLabel = getLabelId <| Label()
                                    let! falseLabel = getLabelId <| Label()
                            
                                    yield OpBranchConditional(c, trueLabel, falseLabel, [||])

                                    yield OpLabel(trueLabel)
                                    let! _ = compileExpr i
                                    yield OpLabel(falseLabel)


                                    return None

                                | None ->
                                    return failwith "non-value condition in ifthenelse"


                        | IfThenElse(c, UnitVal, e) ->
                            let! c = compileExpr c

                            match c with
                                | Some c ->
                                    let! trueLabel = getLabelId <| Label()
                                    let! falseLabel = getLabelId <| Label()
                            
                                    yield OpBranchConditional(c, trueLabel, falseLabel, [||])

                                    yield OpLabel(falseLabel)
                                    let! _ = compileExpr e
                                    yield OpLabel(trueLabel)


                                    return None

                                | None ->
                                    return failwith "non-value condition in ifthenelse"



                        | IfThenElse(c,i,e) ->
                            let! c = compileExpr c

                            match c with
                                | Some c ->
                            
                                    let! trueLabel = getLabelId <| Label()
                                    let! falseLabel = getLabelId <| Label()
                                    let endLabel = Label()

                                    yield OpBranchConditional(c, trueLabel, falseLabel, [||])

                                    yield OpLabel(trueLabel)
                                    let! iv = compileExpr i

                                    let mutable needsEndLabel = false
                                    let! last = lastInstruction
                                    match last with
                                        | Some ExitInstruction -> 
                                            ()
                                        | _ -> 
                                            needsEndLabel <- true
                                            let! endLabel = getLabelId endLabel
                                            yield OpBranch(endLabel)

                                    yield OpLabel(falseLabel)
                                    let! ev = compileExpr e

                                    let! last = lastInstruction
                                    match last with
                                        | Some ExitInstruction when not needsEndLabel -> ()
                                        | _ -> 
                                            let! endLabel = getLabelId endLabel
                                            yield OpLabel(endLabel)

                                    match iv, ev with
                                        | Some iv, Some ev when self.Type <> Type.Unit ->
                                            let! resType = getTypeId self.Type
                                            let! id = newId
                                            yield OpPhi(resType, id, [|iv; trueLabel; ev; falseLabel|])
                                            return Some id
                                        | _ ->
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

        let v = new Var("a", Type.UInt8)
        let v2 = new Var("b", Type.UInt8)

        let zeroVec = Expr.Value(Type.V2d, V2d.II)

        let ex = 
            Expr.Let(v, Expr.Value(Type.UInt8, 1uy), 
                Expr.IfThenElse(
                    Expr.Smaller(Expr.Var v, Expr.Value(Type.UInt8, 2uy)),
                    Expr.Var v,
                    Expr.Value(Type.UInt8, 0uy)
                )
            )

        let s = compileExpr ex
        let (s, _) = s.build SpirVState.Empty

        let typeInstructions = s.typeInstructions |> RevList.toList
        let instructions = s.instructions |> RevList.toList

        printfn "bound: %A" s.currentId

        for i in typeInstructions do
            printfn "%A" i

        for i in instructions do
            printfn "%A" i
