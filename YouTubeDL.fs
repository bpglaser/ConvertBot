module YouTubeDL

open System
open System.Diagnostics
open System.IO

open SimpleLog

let download (uri: Uri) =
    async {
        use proc = new Process()
        proc.StartInfo.FileName <- "cmd.exe"
        proc.StartInfo.Arguments <- sprintf "/C python -m youtube_dl %s" (uri.ToString())
        proc.StartInfo.CreateNoWindow <- true
        proc.StartInfo.UseShellExecute <- false
        proc.EnableRaisingEvents <- true

        logf "Downloading from Reddit [%s]..." (uri.ToString())
        proc.Start() |> ignore
        let! _ = proc.Exited |> Async.AwaitEvent
        log "Download finished."

        if proc.ExitCode <> 0 then
            return Error "youtube-dl failed"
        else
            let fileName = Directory.EnumerateFiles(".", "*.mp4") |> Seq.last
            return Ok fileName
    }
