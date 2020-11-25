module SimpleLog

open System
open System.IO

type SimpleLog() =
    let output = new StreamWriter("log.txt", true, AutoFlush = true)

    member _.Log(message: string) =
        let now = DateTime.Now
        let date = now.ToShortDateString()
        let time = now.ToShortTimeString()
        let message = sprintf "[%s | %s] - %s" date time message
        printfn "%s" message
        output.WriteLine(message)

    interface IDisposable with
        member _.Dispose() =
            output.Close()
            output.Dispose()
