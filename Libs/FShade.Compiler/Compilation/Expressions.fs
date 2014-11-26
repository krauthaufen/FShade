namespace FShade.Compiler

[<AutoOpen>]
module Expressions =
    open System
    open System.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection
    open Aardvark.Base
    open FShade.Utils

    let rec private traverseUsedTypesAndFunctions (e : Expr) =
        compile {
            let! _ = compileType e.Type
            match e with
                | Pipe(e) -> return! traverseUsedTypesAndFunctions e

                | Call(target,m,args) ->
                    let! _ = args |> List.mapC traverseUsedTypesAndFunctions

                    do! match target with
                            | Some t -> traverseUsedTypesAndFunctions t
                            | _ -> compile { return () }

                    let! i = compileIntrinsicFunction m
                    do! match i with
                            | Some _ -> compile { return () }
                            | _ -> addMethod m

                    return ()

                | ExprShape.ShapeLambda(v,b) -> 
                    let! id = addLambda e
                    return! traverseUsedTypesAndFunctions b
                | ExprShape.ShapeVar(v) ->
                    return ()
                | ExprShape.ShapeCombination(o, args) ->
                    let! _ = args |> List.mapC traverseUsedTypesAndFunctions
                    return ()
        }

    /// <summary>
    /// compileValue translates a value having a given type.
    /// </summary>
    let rec compileValue (t : Type) (o : obj) =
        compile {
            if t.IsEnum then
                let name = Enum.GetName(t, o)
                let v = Convert.ToInt32 o
                let prefix = t.Name
                let! str = asDefine (prefix + "_" + name) (v.ToString())
                return str
            else
                let! i = compileIntrinsicValue t o
                match i with
                    | Some i -> return i
                    | None -> 
                        let! e = asConstant o
                        return! compileExpression false false e
        }

    /// <summary>
    /// compileExpression is the main-tool for compilation and can translate any supported
    /// expression to code for the target-language. While doing that it also maintains the 
    /// appropriate CompilerState containing all the information needed to defined needed
    /// types / function / lambdas / etc.
    /// </summary>
    and compileExpression (lastExpression : bool) (isStatement: bool) (e : Expr) =
        let ret s = 
            if lastExpression && e.Type <> typeof<unit> then 
                sprintf "return %s;\r\n" s 
            elif e.Type = typeof<unit> then 
                sprintf "%s;\r\n" s 
            else s

        compile {
            match e with

                | Call(None, Method("op_LessAmpGreater", _), [a;b]) ->
                    let! a = compileExpression false false a
                    let! b = compileExpression false false b
                    return sprintf "(%s && %s)" a b |> ret

                | Call(None, Method("op_LessBarGreater", _), [a;b]) ->
                    let! a = compileExpression false false a
                    let! b = compileExpression false false b
                    return sprintf "(%s || %s)" a b |> ret
//                | Let(v, Coerce(e,_), b) ->
//                    return! compileExpression lastExpression isStatement (b.Substitute(fun vi -> if vi = v then Some e else None))
//                | Coerce(e,_) ->
//                    return! compileExpression lastExpression isStatement e
                // foreach-loops have to be matched as first pattern since they consist of a number of expressions
                // and may be 'destroyed' otherwise.
                | ForEach(var,seq,body) ->
                    let seqType = seq.Type
                    match seqType with
                        | FixedArrayType(d, baseType) ->
                            let i = Var("_i", typeof<int>)
                            let e = Expr.ForIntegerRangeLoop(i, Expr.Value(0), Expr.Value(d-1), 
                                        Expr.Let(var, Expr.PropertyGet(seq, seqType.GetProperty("Item"), [Expr.Var i]),
                                            body
                                        )
                                    )

                            return! compileExpression lastExpression isStatement e

                        | _ -> return! error "cannot loop over %A" seq.Type

                // empty array-creations cannot be expressed inline (in most C-like languages). Therefore we create a function
                // creating a local variable having the desired type and returning it. 
                | NewObject(c, []) when c.DeclaringType.IsGenericType && c.DeclaringType.GetGenericTypeDefinition() = typedefof<Arr<_,_>> ->
                    let v = Var("temp", c.DeclaringType)
                    let! f = asFunction (Expr.Let(v, e, Expr.Var v))
                    return f

                // lets simply become variable-declarations. Since F# supports
                // hiding of variables but C doesn't there is a global renaming-pass
                // sequentially numbering variables when they collide.
                | Let(var,value,body) ->

                    //variable declarations may occur on statement-level
                    if isStatement then
                        match var.Type,value with
                            // When a new FixedArray is created and directly assigned to a 
                            // variable there is no possibility to create its 'value'. therefore
                            // we're having a special-case here.
                            | FixedArrayType(dim,baseType),NewObject(_) ->
                                do! addBound var
                                let! b = compileExpression lastExpression true body
                                do! removeBound var

                                let! t = compileType baseType
                                let! d = compileVariableDeclaration t var.Name (Some (dim.ToString()))

                                // Note that the variable-definition does not contain an assignment.
                                return sprintf "%s;\r\n%s" d b
                            | _ ->
                                let! e = compileExpression false false value

                                do! addBound var
                                let! b = compileExpression lastExpression true body
                                do! removeBound var

                                let! t = compileType var.Type
                                let! d = compileVariableDeclaration t var.Name None

                                return sprintf "%s = %s;\r\n%s" d e b

                    // when sequentially accessing properties on structs F# often introduces copies 
                    // to ensure that the original struct cannot be modified by the property-access.
                    // since in C every variable is mutable anyways we do not want to have those copies
                    // in our code. 
                    // TODO: find a better way of identifying those copies (user cannot use the name atm.)
                    elif var.Name.StartsWith "copyOfStruct" then
                        let body = body.Substitute(fun vi -> if vi = var then Some value else None)
                        return! compileExpression lastExpression true body
                    
                    // whenever a let occurs on expression-level we generate a function here.
                    // TODO: maybe estimate the complexity of the expression and inline it?
                    else
                        return! asFunction e

                // F#'s for-loops sadly treat their upper bound inclusively. This often causes code to contain upper-bounds
                // like 'count-1'. Since C-Style programmers mostly use 'i < count' (and compilers may be optimized therefore)
                // we emit 'i < count' when easily possible (meaning that the upper bound is something like count-1)
                | ForIntegerRangeLoop(v,s,e,b) ->
                    
                    let op, e = 
                        match e with
                            | Call(None, mi, [e; Value(value,t)]) when mi.Name = "op_Subtraction" && value = (1 :> obj) ->
                                "<", e
                            | _ ->
                                "<=", e
                    
                    let! s = compileExpression false false s
                    let! e = compileExpression false false e

                    do! addBound v
                    let! b = compileExpression false true b
                    do! removeBound v

                    let! i = compileType typeof<int>
                    let! d = compileVariableDeclaration i v.Name None
                    return sprintf "for(%s = %s; %s %s %s; %s++)\r\n{\r\n%s}\r\n" d s v.Name op e v.Name (String.indent 1 b)

                
                // While loops simply work like in C but F# doesn't come with support for breaks.
                // TODO: think about integrating break just for the shader-language.
                | WhileLoopFlat(c,b) ->
                    let! c = compileExpression false false c
                    let! b = compileExpression false true b

                    return sprintf "while(%s)\r\n{\r\n%s}\r\n" c (String.indent 1 b)

                // Sequentials in C are simply represented by concatenation of the statements.
                | Sequential(l,r) ->
                    let! l = compileExpression false true l
                    let! r = compileExpression lastExpression true r

                    return sprintf "%s%s" l r

                // writing variables is always possible in C (at least on a statement-level)
                // TODO: think about varsets in expressions.
                | VarSet(v,e) ->
                    if isStatement then
                        let! e = compileExpression false false e
                        return sprintf "%s = %s;\r\n" v.Name e
                    else
                        return! error "cannot write to variables on expression-level"

                // when a long (> 4) sequence of elif-expressions occur and the conditions
                // are trivial (a == 3) then the compiler generates switch code instead of
                // else ifs.
                | Switch(value, cases) when cases.Length > 4 && isStatement ->
                    //TODO: test switch creation a bit since it is very unsafe atm.
                    //      think about return-switches not needing breaks
                    let! cases = cases |> List.mapC (fun (l,b) -> 
                        compile {
                            let! label = if l = null then compile { return "default" }
                                         else compile {
                                            let! label = compileValue (l.GetType()) l  
                                            return sprintf "case %s" label
                                         }

                            let! body = compileExpression true true b

                            return sprintf "%s:\r\n%s    break;" label (String.indent 1 body)
                        })
                    let! v = compileExpression false false value

                    return sprintf "switch(%s)\r\n{\r\n%s\r\n}\r\n" v (String.indent 1 (String.concat "\r\n" cases))

                | Alternatives(cases, elseCase) when isStatement ->
                    let! alts = cases |> List.mapC (fun (c,b) ->
                                    compile {
                                        let! c = compileExpression false false c
                                        let! i = compileExpression lastExpression true b
                                        return sprintf "if(%s)\r\n{\r\n%s}\r\n" c (String.indent 1 i)
                                    }
                                )

                    let! e = compileExpression lastExpression true elseCase
                    let ec = sprintf "else\r\n{\r\n%s}\r\n" (String.indent 1 e)

                    let cascade = (alts |> String.concat "else ") + ec

                    return cascade

                // a simple if then expression (without the else branch)
                | IfThenFlat(c,i) ->
                    if isStatement then
                        let t = i.Type
                        let! c = compileExpression false false c
                        let! i = compileExpression lastExpression true i

                        if String.lineCount i <= 1 then
                            return sprintf "if(%s)\r\n%s" c (String.indent 1 i)
                        else
                            return sprintf "if(%s)\r\n{\r\n%s}\r\n" c (String.indent 1 i)
                    else
                        return! error "cannot express partial conditional branches on expression level (would require unit-values)"

                // a conditional-expression having an if and an else-branch
                // Note that conditional expressions in C come with several restrictions
                // which might not be fully implemented here
                | IfThenElseFlat(c,ifTrue,ifFalse) ->
                    let! c = compileExpression false false c
                    if not isStatement then
                        // if the conditional's type is unit it can't be expressed
                        // on expression-level and will be translated into a function.
                        if ifTrue.Type = typeof<unit> then
                            return! asFunction e
                        else
                            let! i = compileExpression lastExpression false ifTrue
                            let! e = compileExpression lastExpression false ifFalse

                            return sprintf "%s ? %s : %s" c i e
                    else
                        let! i = compileExpression lastExpression true ifTrue
                        let! e = compileExpression lastExpression true ifFalse

                        let ic = String.lineCount i
                        let ec = String.lineCount e

                        // some special-cases making the code a bit shorter
                        match ic,ec with
                            | 0,0 -> return ""
                            | 1,0 -> return sprintf "if(%s)\r\n%s" c (String.indent 1 i)
                            | 0,1 -> return sprintf "if(%s) { }\r\nelse%s" c (String.indent 1 i)
                            | 1,1 -> return sprintf "if(%s)\r\n%selse\r\n%s" c (String.indent 1 i) (String.indent 1 e)
                            | 1,n -> return sprintf "if(%s)\r\n%selse\r\n{\r\n%s}\r\n" c (String.indent 1 i) (String.indent 1 e)
                            | n,1 -> return sprintf "if(%s)\r\n{\r\n%s}\r\nelse\r\n%s" c (String.indent 1 i) (String.indent 1 e)
                            | m,n -> return sprintf "if(%s)\r\n{\r\n%s}\r\nelse\r\n{\r\n%s}\r\n" c (String.indent 1 i) (String.indent 1 e)

                // MemberFieldGet matches FieldGet and PropertyGet since we don't want to distinguish 
                // between those cases here.
                | MemberFieldGet(t, m) ->
                    // since union-type fields have non-standard names we use some special logic here
                    if FSharpType.IsUnion t.Type then
                        let case = FSharpType.GetUnionCases t.Type |> Seq.tryFind (fun c -> (c.GetFields() |> Seq.tryFind(fun fi -> fi :> MemberInfo = m)).IsSome)
                        
                        match case with
                            | Some(c) -> 
                                let! t = compileExpression false false t
                                let fieldIndex = c.GetFields() |> Array.tryFindIndex (fun fi -> fi :> MemberInfo = m)
                                match fieldIndex with
                                    | Some i -> let fieldName = getUnionField c.Name i
                                                return sprintf "%s.%s" t fieldName |> ret
                                    | None -> return! error "not a union-case field %A" m

                            | None ->
                                return! error "not a union-case field %A" m
                        
                    // all other fields are simply resolved using the intrinsic property-definition provided
                    // by the compiler and get a default-name if they're not intrinsic.
                    else
                        let! i = compileIntrinsicProperty m
                        let! t = compileExpression false false t
                        match i with
                            | Some(str) -> return sprintf "%s.%s" t str |> ret
                            | None -> 
                                match m with
                                    | :? CustomProperty as m -> return sprintf "%s.%s" t m.Name
                                    | _ ->
                                        match m with
                                            | :? PropertyInfo as p -> 
                                                let! i = compileIntrinsicFunction p.GetMethod
                                                match i with
                                                    | Some fmt -> return System.String.Format(fmt, t) |> ret
                                                    | None ->  return sprintf "%s.%s" t m.Name |> ret
                                            | _ ->
                                                 return sprintf "%s.%s" t m.Name |> ret

                // matches FieldSet and PropertySet since we make no distinction between them here.
                // since the current implementation does not allow union-type fields to be mutable we
                // don't have to check for these here.
                | MemberFieldSet(t, m, v) ->
                    let! i = compileIntrinsicProperty m
                    let! t = compileExpression false false t
                    let! v = compileExpression false false v
                    match i with
                        | Some(str) -> return sprintf "%s.%s = %s;\r\n" t str v
                        | None ->  
                            match m with
                                    | :? PropertyInfo as p -> 
                                        let! i = compileIntrinsicFunction p.SetMethod
                                        match i with
                                            | Some fmt -> return System.String.Format(fmt, t, v) |> ret
                                            | None ->  return sprintf "%s.%s = %s;\r\n" t m.Name v
                                    | _ ->
                                         return sprintf "%s.%s = %s;\r\n" t m.Name v

                // static properties and fields are simply inlined atm. 
                // TODO: this might not be sufficient since changes will not be reflected in the shader-code.
                //       Is this even possible without completely altering the compiler?
                | PropertyGet(None, p ,[]) ->
                    return! compileExpression lastExpression isStatement (Expr.Value(p.GetValue(null), p.PropertyType))

                | FieldGet(None, f) ->
                    return! compileExpression lastExpression isStatement (Expr.Value(f.GetValue(null), f.FieldType))


                // record / union / tuple creators use the self-defined constructor-function. This function always accompanies type-definitions.
                | NewRecord(t, args) ->
                    let! ctor = compileCtorName t
                    let! args = args |> List.mapC (compileExpression false false)

                    return sprintf "%s(%s)" ctor (String.concat ", " args) |> ret

                | NewTuple(args) ->
                    let t = FSharpType.MakeTupleType(args |> Seq.map (fun e -> e.Type) |> Seq.toArray)
                    let! ctor = compileCtorName t
                    let! args = args |> List.mapC (compileExpression false false)

                    return sprintf "%s(%s)" ctor (String.concat ", " args) |> ret

                | NewUnionCase(c,args) ->
                    let! t = compileType c.DeclaringType
                    let ctor = getUnionCtorName t c.Name
                    let! args = args |> List.mapC (compileExpression false false)

                    return sprintf "%s(%s)" ctor (String.concat ", " args) |> ret

                // testing for union-cases is simply done by their 'tag'.
                // TODO: this field's name cannot easily be changed atm. since it's hard-coded
                //       throughout the compiler.
                | UnionCaseTest(e, c) ->
                    let! e = compileExpression false false e
                    return sprintf "%s.tag == %d" e c.Tag |> ret

                // when an object is created the compiler currenlty assumes that the constructor
                // takes all arguments in the exact order of its fields and does simply assign the
                // fields to the given values.
                // TODO: Since types may use completely custom-constructors it's very important to find
                //       a way of dealing with those. (they don't have reflected-definitions when defined in C#)
                | NewObject(c,args) ->
                    let! args = args |> List.mapC (compileExpression false false)
                    let! i = compileIntrinsicConstructor c
                    match i with
                        | Some i -> return System.String.Format(i, args |> Seq.map unbox |> Seq.toArray)  |> ret
                        | None -> 
                            let! n = compileType c.DeclaringType
                            return sprintf "%s_ctor(%s)" n (String.concat ", " args) |> ret

                // Inline array-creations are supported by most target-languages.
                // TODO: the current syntax is for GLSL only and should be "moved" to the compiler-implementation.
                | NewArray(t,args) ->
                    let! t = compileType t
                    let! args = args |> List.mapC (compileExpression false false)
                    return sprintf "%s[](%s)" t (String.concat ", " args)

                // getting tuple-fields is different from all the other fields since
                // they don't have an explicit name in F#. 
                | TupleGet(t,i) ->
                    let fieldName = getTupleField i
                    let! t = compileExpression false false t
                    return sprintf "%s.%s" t fieldName |> ret

                // Since pipes are very common in F#-code w want to support them whenever possible
                // Most pipes can however simply be 'inlined'. This active pattern determines
                // whether a pipe can be 'inlined' and returns a reorganized expression without the pipe.
                | Pipe(e) ->
                    return! compileExpression lastExpression isStatement e


                // lambda expressions are simply added to the CompilerState. The (not yet defined) creator
                // for it will be used to create its value.
                | Lambda(_,b) ->
                    let! id = addLambda e

                    let! free = getFreeVars e
                    let argTypes = free |> Seq.map (fun v -> v.Type) |> Seq.toArray
                    let args = free |> Seq.map (Expr.Var) |> Seq.toList

                    let m = CustomMethod(sprintf "makeLambda%d" id, argTypes, e.Type)

                    // Note that lambdas could internally use types / functions / other lambdas / etc. which
                    // do not yet occur in the CompilerState. Since their compilation must be deferred we simply
                    // traverse them here testing for used types/etc. 
                    // Note that this was previously done by compiling their body and the current implementation
                    // might not respect all corner-cases.
                    do! traverseUsedTypesAndFunctions e

                    return! compileExpression lastExpression isStatement (Expr.Call(m, args))

                // applications of lambdas normally get translated to dispatcher-calls
                // Since 'printfn' was very helpful for a C99-compiler those format-functions are treated specially here
                // That case however is irrelevant for Shaders and can be ignored.
                | Application(f,arg) ->
                    let rec flattenApplications ei =
                        match ei with
                            | Application(f, arg) -> 
                                let args,f = flattenApplications f
                                (arg::args, f)
                            | _ -> ([],ei)
                    let args,f' = flattenApplications e
                    let args = List.rev args

                    let f' = Expr.tryEval f'
                    match f' with
                        | Some f' ->
                            let! fmtAndArgs = compileApplication f' e.Type args

                            match fmtAndArgs with
                                | Some (fmt,args) -> 
                                    let! args = args |> List.mapC (compileExpression false false)
                                    return System.String.Format(fmt, args |> List.map (fun a -> a :> obj) |> List.toArray) |> ret

                                | None ->
                                    let! dispatcherName = getDispatcherNameForType f.Type
                                    let subDispatcher = CustomMethod(dispatcherName, [|f.Type; arg.Type|], e.Type)

                                    return! compileExpression lastExpression isStatement (Expr.Call(subDispatcher, [f; arg]))
                        
                        // whenever the function cannot be evaluated to a constant-value the default behaviour
                        // is used. TODO: maybe this is not entirely sufficient.
                        | None -> 
                           
                            let! dispatcherName = getDispatcherNameForType f.Type
                            let subDispatcher = CustomMethod(dispatcherName, [|f.Type; arg.Type|], e.Type)

                            return! compileExpression lastExpression isStatement (Expr.Call(subDispatcher, [f; arg]))

                // coercions to object are simply ignored. (some F# constructs make use of those)
                | Coerce(a,t) when t = typeof<obj> ->
                    return! compileExpression lastExpression isStatement a

                // static calls (without a target) are compiled as they are.
                | Call(None, mi, args) ->
                    let! i = compileIntrinsicFunction mi
                    let! args = args |> List.filter(fun a -> a.Type <> typeof<unit>) |> List.mapC (compileExpression false false)

                    match i with
                        | Some fmt -> return System.String.Format(fmt, args |> List.map unbox |> List.toArray) |> ret
                        | None -> 
                            do! addMethod mi
                            let! name = getMethodName mi
                            return sprintf "%s(%s)" name (String.concat ", " args) |> ret

                // member calls are translated to functions taking the this-parameter at the first position.
                | Call(Some v, mi, args) -> 
                    let! i = compileIntrinsicFunction mi
                    let! args = args |> List.filter(fun a -> a.Type <> typeof<unit>) |> List.mapC (compileExpression false false)
                    let! this = compileExpression false false v

                    let args = List.Cons(this, args)

                    match i with
                        | Some fmt -> return System.String.Format(fmt, args |> List.map unbox |> List.toArray) |> ret
                        | None -> 
                            do! addMethod mi
                            let! name = getMethodName mi
                            return sprintf "%s(%s)" name (String.concat ", " args) |> ret

                // variables are simply represented using their name.
                | Var(v) -> 
                    return v.Name |> ret

                // all values not being of type unit are compiled using the compiler.
                | Value(v,t) when t <> typeof<unit> ->
                    let! v = compileValue t v
                    return ret v

                // values of type unit can only appear on statement-level and can therefore always be omitted
                | Value(_,t) when t = typeof<unit> -> return ""

                // property-getters and setters having indices are compiled using their respective
                // Get-/SetMethod.
                | PropertySet(t, pi, indices, value) ->
                    match t with
                        | None -> return! Expr.Call(pi.SetMethod, List.concat [indices; [value]]) |> compileExpression lastExpression isStatement
                        | Some t -> return! Expr.Call(t, pi.SetMethod, List.concat [indices; [value]]) |> compileExpression lastExpression isStatement

                | PropertyGet(t, pi, indices) ->
                    match t with
                        | None -> return! Expr.Call(pi.GetMethod, indices) |> compileExpression lastExpression isStatement
                        | Some t -> return! Expr.Call(t, pi.GetMethod, indices) |> compileExpression lastExpression isStatement


                | Coerce(e, _) ->
                    return! compileExpression lastExpression isStatement e

                | _ -> return! error "unknown expression: %A" e
        }
