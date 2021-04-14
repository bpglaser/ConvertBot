module ConvertBot

open System
open System.IO
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

type FileState =
    { Downloaded: string
      Converted: string }

let download (client: WebClient) uri =
    async {
        if Reddit.isRedditUri uri then
            return! YouTubeDL.download uri
        else
            let parts = uri.AbsolutePath.Split('/')
            let fileName = Array.last parts
            log <| sprintf "Downloading [%s] to [%s]..." (uri.ToString()) fileName
            client.DownloadFile(uri, fileName)
            log "Download finished."
            return Ok fileName
    }

let convert (fileName: string) =
    async {
        if fileName.EndsWith(".mp4") then
            log "No conversion needed."
            return Ok
                       { Downloaded = fileName
                         Converted = fileName }
        else
            let! result = Convert.ffmpegConvertMP4 fileName
            match result with
            | Ok outputFileName ->
                log "Converting done."
                return Ok
                           { Downloaded = fileName
                             Converted = outputFileName }
            | Error e -> return Error e
    }

let reply config chatID fileState =
    async {
        log <| sprintf "Uploading %s to %i" fileState.Converted chatID
        use stream = File.Open(fileState.Converted, FileMode.Open)
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
            File.Delete path
        with
        | :? DirectoryNotFoundException
        | :? IOException -> ()
    safeDelete fileState.Downloaded
    safeDelete fileState.Converted

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

let onUpdate config client settings (chatMessage: ChatMessage) =
    async {
        let isAdmin user = Set.contains user settings.Admins
        let isWhitelist user = Set.contains user settings.Whitelist
        log <| sprintf "Message from %s: %s" chatMessage.User chatMessage.Message
        match chatMessage with
        | Command "/whitelist add" msg when msg.User |> isAdmin -> return! addToWhitelist config settings msg
        | Command "/whitelist remove" msg when msg.User |> isAdmin ->
            return! removeFromWhitelist config settings msg
        | Command "/whitelist" msg when msg.User |> isAdmin -> return! sendWhitelist config settings msg
        | msg when msg.User |> isWhitelist -> return! doDownload config client msg
        | msg -> return! reject config msg
    }

let sendError config chatMessage s =
    async {
        logf "Error encountered %s" s
        let! result = sendMessageAsync config chatMessage.ChatId s
        match result with
        | Ok _ -> ()
        | Error e -> logf "Failed to send error! %s" e
    }

let run (settings: Settings) =
    let config = { defaultConfig with Token = settings.Token }
    use client = new WebClient()

    let updatesArrived context =
        maybe {
            let! message =
                context
                |> extractMessage
            onUpdate config client settings message
            |> AsyncResult.mapError (sendError config message)
            |> Async.RunSynchronously
            |> ignore
        } |> ignore
    startBot config updatesArrived None
