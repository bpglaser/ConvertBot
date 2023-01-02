module Maybe

type MaybeBuilder() =
    member this.Bind(m, f) = Option.bind f m
    member this.Return(x) = Some x
    member this.ReturnFrom(x) = x
    member this.Zero() = None
    member this.Delay(f) = f
    member this.Run(f) = f ()

    member this.Combine(a, b) =
        match a with
        | Some _ -> a
        | None -> b ()

    member this.TryWith(m, f) =
        try
            this.ReturnFrom(m ())
        with
        | e -> f e

let maybe = MaybeBuilder()
