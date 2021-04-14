open System
open System.Diagnostics
open System.Net

open FunogramHelpers
open Maybe
open Settings
open SimpleLog
open Whitelist

open Funogram.Api
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Funogram.Telegram.Types
open Funogram.Types

open FSharp.Data
open FSharp.Data.JsonExtensions

type FileState =
    { Downloaded: string
      Converted: string }

let isRedditUri (uri: Uri) = uri.Host = "www.reddit.com" || uri.Host = "v.redd.it"

let getRedditPath (client: WebClient) (uri: Uri) =
    let result = client.DownloadString(sprintf "%s.json" (uri.ToString()))
    let result = JsonValue.Parse(result)
    let fallback = result.[0]?data?children.[0]?data?media?reddit_video?fallback_url
    fallback.AsString() |> Uri

let download (client: WebClient) uri =
    async {
        if isRedditUri uri then
            use proc = new Process()
            proc.StartInfo.FileName <- "cmd.exe"
            proc.StartInfo.Arguments <- sprintf "/C python -m youtube_dl %s" (uri.ToString())
            proc.StartInfo.CreateNoWindow <- true
            proc.StartInfo.UseShellExecute <- false


            printfn "Downloading [%s]..." (uri.ToString())
            proc.Start() |> ignore
            proc.WaitForExit()
            printfn "Download finished."

            let fileName = System.IO.Directory.EnumerateFiles(".", "*.mp4") |> Seq.last

            return Ok fileName
        else
            let parts = uri.AbsolutePath.Split('/')
            let fileName = Array.last parts
            printfn "Downloading [%s] to [%s]..." (uri.ToString()) fileName
            client.DownloadFile(uri, fileName)
            printfn "Download finished."
            return Ok fileName
    }

let convert (fileName: string) =
    async {
        if fileName.EndsWith(".mp4") then
            printfn "No conversion needed."
            return Ok
                       { Downloaded = fileName
                         Converted = fileName }
        else
            let! result = Convert.ffmpegConvertMP4 fileName
            match result with
            | Ok outputFileName ->
                printfn "Converting done."
                return Ok
                           { Downloaded = fileName
                             Converted = outputFileName }
            | Error e -> return Error e
    }

let reply config chatID fileState =
    async {
        printfn "Uploading %s to %i" fileState.Converted chatID
        use stream = IO.File.Open(fileState.Converted, IO.FileMode.Open)
        let! response = sendVideo chatID (FileToSend.File(fileState.Converted, stream)) ""
                        |> api config
                        |> Async.map convertFunogramResult
        match response with
        | Ok _ -> return Ok fileState
        | Error e -> return Error e
    }

let cleanup fileState =
    let safeDelete path =
        try
            IO.File.Delete path
        with
        | :? IO.DirectoryNotFoundException
        | :? IO.IOException -> ()
    safeDelete fileState.Downloaded
    safeDelete fileState.Converted

let extractMessage context =
    maybe {
        let! message = context.Update.Message
        let! user = message.Chat.Username
        let! text = message.Text
        let chatId = message.Chat.Id
        return { User = user
                 Message = text
                 ChatId = chatId }
    }

let doDownload config client chatMessage =
    let chatId = chatMessage.ChatId

    let sendMessage s a =
        a
        |> sprintf s
        |> (sendMessageAsync config chatId)

    chatMessage.Message
    |> Uri
    |> AsyncResult.result
    |> AsyncResult.tap (sendMessage "Downloading %A")
    |> AsyncResult.bind (download client)
    |> AsyncResult.tap (sendMessage "Converting %s")
    |> AsyncResult.bind convert
    |> AsyncResult.tap (sendMessage "Uploading...%A")
    |> AsyncResult.bind (reply config chatId)
    |> AsyncResult.map cleanup

let reject config chatMessage = sendMessageAsync config chatMessage.ChatId "You are not whitelisted. :^)"

let (|Command|_|) (prefix: string) (chatMessage: ChatMessage) =
    if chatMessage.Message.StartsWith(prefix) then Some chatMessage else None

let onUpdate config client settings (context: UpdateContext) =
    async {
        let isAdmin user = Set.contains user settings.Admins
        let isWhitelist user = Set.contains user settings.Whitelist

        match extractMessage context with
        | None -> return Ok()
        | Some chatMessage ->
            log <| sprintf "Message from %s: %s" chatMessage.User chatMessage.Message
            match chatMessage with
            | Command "/whitelist add" msg when msg.User |> isAdmin -> return! addToWhitelist config settings msg
            | Command "/whitelist remove" msg when msg.User |> isAdmin ->
                return! removeFromWhitelist config settings msg
            | Command "/whitelist" msg when msg.User |> isAdmin -> return! sendWhitelist config settings msg
            | msg when msg.User |> isWhitelist -> return! doDownload config client msg
            | msg -> return! reject config msg
    }

let runClient (settings: Settings) =
    let config = { defaultConfig with Token = settings.Token }
    use client = new WebClient()

    let updatesArrived =
        onUpdate config client settings
        >> Async.RunSynchronously
        >> ignore
    startBot config updatesArrived None

[<EntryPoint>]
let main argv =
    let settings = loadSettings()
    log <| sprintf "Loaded settings: %A" settings
    [ settings |> runClient
      StatusServer.run()
      Watcher.run @"C:\Users\Brad\Downloads" "*.webm" ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    0
