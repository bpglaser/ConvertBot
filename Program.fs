[<EntryPoint>]
let main argv =
    let settings = Settings.loadSettings()
    SimpleLog.logf "Loaded settings: %A" settings
    [ settings |> ConvertBot.run
      StatusServer.run()
      Watcher.run @"C:\Users\Brad\Downloads" "*.webm" ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    0
