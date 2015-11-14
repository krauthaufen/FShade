namespace FShade

open System.Runtime.CompilerServices

type private EmptyRef<'a>() =
    let mutable hasValue = false
    let mutable content = Unchecked.defaultof<'a>

    member x.HasValue =
        hasValue

    member x.Value
        with get() = content
        and set v = content <- v

type MemoCache<'a, 'b when 'a : not struct>(f : 'a -> 'b) =
    let cache = ConditionalWeakTable<'a, EmptyRef<'b>>()

    member x.Invoke(v : 'a) =
        let r = cache.GetOrCreateValue(v)
        lock r (fun () ->
            if r.HasValue then r.Value
            else
                let res = f v
                r.Value <- res
                res
        )

type GenericMemoCache() =
    let cache = ConditionalWeakTable<obj, EmptyRef<obj>>()

    member x.Invoke(f : 'a -> 'b, v : 'a) =
        let r = cache.GetOrCreateValue(v)
        lock r (fun () ->
            if r.HasValue then r.Value |> unbox<'b>
            else
                let res = f v
                r.Value <- res
                res
        )

