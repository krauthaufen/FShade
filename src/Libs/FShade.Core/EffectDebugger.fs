namespace FShade


module EffectDebugger =
    
    let mutable isAttached = false

    // Effect -> IMod<Effect>
    let mutable registerFun : Option<Effect -> obj> = None
    let mutable saveCode : Effect -> string -> unit = fun _ _ -> ()

    let register (e : Effect) = 
        match registerFun with
            | Some f -> Some (f e)
            | _ -> None