[<EntryPoint>]
let main argv =
    let settings = Settings.loadSettings()
    SimpleLog.logf "Loaded settings: %A" settings
    [ settings |> ConvertBot.run
      StatusServer.run() ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    0
