[<EntryPoint>]
let main argv =
    let settings = Settings.loadSettings ()
    SimpleLog.logf "Loaded settings: %A" settings

    let result =
        [ settings |> ConvertBot.run
          StatusServer.run () ]
        |> Async.Parallel
        |> Async.Catch
        |> Async.RunSynchronously

    match result with
    | Choice2Of2 err -> SimpleLog.logf "application error: %A" err
    | _ -> ()

    0
