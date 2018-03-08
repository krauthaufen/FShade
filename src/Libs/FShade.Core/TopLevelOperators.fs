namespace FShade

open Microsoft.FSharp.Quotations

[<AutoOpen>]
module TopLevelOperators =
    
    let (~%) (e : Expr<'a>) : 'a =
        failwith "splices cannot be evaluated"

    let (~%%) (e : Expr) : 'a =
        failwith "splices cannot be evaluated"