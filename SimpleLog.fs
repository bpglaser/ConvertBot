module SimpleLog

open System
open System.IO

module List =
    let rec safeTake n list =
        match list with
        | x :: xs when n > 0 -> x :: safeTake (n - 1) xs
        | _ -> []

type LogMsg =
    | Send of string
    | Get of AsyncReplyChannel<string list>

type SimpleLog() =
    static let output = new StreamWriter("log.txt", true, AutoFlush = true)

    static let agent = MailboxProcessor.Start(fun inbox ->
        let rec loop log = async {
            let! msg = inbox.Receive()
            match msg with
            | Send msg ->
                let log = msg :: log
                return! loop (log |> List.safeTake 20)
            | Get channel ->
                channel.Reply(log |> List.rev)
                return! loop log
        }
        loop [])

    static member GetTail() = agent.PostAndAsyncReply(fun channel -> Get channel)

    static member Log(message: string) =
        let now = DateTime.Now
        let date = now.ToShortDateString()
        let time = now.ToShortTimeString()
        let message = sprintf "[%s | %s] - %s" date time message
        agent.Post <| Send message
        printfn "%s" message
        output.WriteLine(message)

    interface IDisposable with
        member _.Dispose() =
            output.Close()
            output.Dispose()
