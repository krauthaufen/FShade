namespace FShade


module EffectDebugger =
    
    // Effect -> IMod<Effect>
    let mutable registerFun : Option<Effect -> obj> = None

    let register (e : Effect) = 
        match registerFun with
            | Some f -> Some (f e)
            | _ -> None