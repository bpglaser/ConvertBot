module Convert

open System
open System.Diagnostics

let ffmpegConvertMP4 (fileName: string) =
    async {
        let baseName = fileName.Substring(0, fileName.LastIndexOf('.'))
        let outputFileName = sprintf "%s.mp4" baseName

        if IO.File.Exists outputFileName then
            IO.File.Delete outputFileName

        use proc = new Process()
        proc.StartInfo.FileName <- "cmd.exe"
        proc.StartInfo.Arguments <- sprintf "/C ffmpeg -i %s %s" fileName outputFileName
        proc.StartInfo.CreateNoWindow <- true
        proc.StartInfo.UseShellExecute <- false
        proc.EnableRaisingEvents <- true

        proc.Start() |> ignore
        let! _ = proc.Exited |> Async.AwaitEvent

        if proc.ExitCode <> 0 then
            return Error "ffmpeg failed"
        else
            return Ok outputFileName
    }
