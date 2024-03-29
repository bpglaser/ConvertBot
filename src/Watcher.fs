module Watcher

open SimpleLog

open System.IO
open System.Threading

let watchForCreation path filter f =
    let watcher = new FileSystemWatcher(path, filter)

    watcher.NotifyFilter <-
        NotifyFilters.Attributes
        ||| NotifyFilters.CreationTime
        ||| NotifyFilters.DirectoryName
        ||| NotifyFilters.FileName
        ||| NotifyFilters.LastAccess
        ||| NotifyFilters.LastWrite
        ||| NotifyFilters.Security
        ||| NotifyFilters.Size

    watcher.IncludeSubdirectories <- true
    watcher.EnableRaisingEvents <- true

    let rec inner () =
        async {
            let! e = Async.AwaitEvent watcher.Created
            do! Async.Sleep 5000
            let! result = f e.FullPath

            match result with
            | Ok _ -> log "conversion done!"
            | Error e -> logf "%A" e

            return! inner ()
        }

    let cts = new CancellationTokenSource()

    let rec modified () =
        async {
            let! e = Async.AwaitEvent watcher.Changed
            logf "%A" e.ChangeType
            return! modified ()
        }

    [ inner (); modified () ]
    |> Async.Parallel
    |> Async.Ignore

let run path filter =
    watchForCreation path filter Convert.ffmpegConvertMP4
