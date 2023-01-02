module AsyncResult

let result a = a |> Ok |> Async.result

let map (f: 'a -> 'b) (a: Async<Result<'a, 'c>>) : Async<Result<'b, 'c>> = a |> Async.map (Result.map f)

let bind (f: 'a -> Async<Result<'b, 'c>>) (a: Async<Result<'a, 'c>>) : Async<Result<'b, 'c>> =
    async {
        let! r = a

        match r with
        | Ok value ->
            let next: Async<Result<'b, 'c>> = f value
            return! next
        | Error err -> return (Error err)
    }

let bindError (f: 'a -> Async<'b>) (a: Async<Result<'c, 'a>>) : Async<Result<'c, 'b>> =
    async {
        let! r = a

        match r with
        | Ok value -> return Ok value
        | Error err ->
            let! err = f err
            return Error err
    }

let compose (f: 'a -> Async<Result<'b, 'e>>) (g: 'b -> Async<Result<'c, 'e>>) : 'a -> Async<Result<'c, 'e>> =
    fun x -> bind g (f x)

let tap (f: 'a -> Async<Result<unit, 'b>>) (a: Async<Result<'a, 'b>>) : Async<Result<'a, 'b>> =
    async {
        let! a = a

        match a with
        | Ok a ->
            let! res = f a

            match res with
            | Ok _ -> return Ok a
            | Error e -> return Error e
        | Error e -> return Error e
    }

let (>>=) a f = bind f a
let (>=>) f g = compose f g
