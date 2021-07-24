module ConvertBot

open System
open System.IO
open System.Net.Http

open FunogramHelpers
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

let download (client: HttpClient) uri =
    async {
        if Reddit.isRedditUri uri then
            return! YouTubeDL.download uri
        else
            let parts = uri.AbsolutePath.Split('/')
            let fileName = Array.last parts
            log <| sprintf "Downloading [%s] to [%s]..." (uri.ToString()) fileName
            use! source =
                uri.ToString()
                |> client.GetStreamAsync
                |> Async.AwaitTask
            use fileStream = File.OpenWrite fileName
            do! source.CopyToAsync(fileStream) |> Async.AwaitTask
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

let parseUri s =
    match s with
    | None -> Error "No chat message provided"
    | Some s ->
        match System.Uri.TryCreate(s, UriKind.Absolute) with
        | true, uri -> Ok uri
        | _ -> Error "Invalid uri"

let convertReplyCleanup config chatId =
    let sendMessage s a =
        sprintf s a
        |> (sendMessageAsync config chatId)
    AsyncResult.tap (sendMessage "Converting %s")
    >> AsyncResult.bind convert
    >> AsyncResult.tap (sendMessage "Uploading...%A")
    >> AsyncResult.bind (reply config chatId)
    >> AsyncResult.map cleanup

let convertWebm config client (chatMessage: ChatMessage) : Async<Result<unit, string>> =
    let chatId = chatMessage.ChatId
    let sendMessage s a =
        sprintf s a
        |> (sendMessageAsync config chatId)
    let doc =
        chatMessage.Context.Update.Message
        |> Option.bind (fun msg -> msg.Document)
    match doc with
    | None ->
        Error "No file provided" |> Async.result
    | Some doc ->
        doc
        |> AsyncResult.result
        |> AsyncResult.tap (sendMessage "Downloading %A")
        |> AsyncResult.bind (downloadDocument config client)
        |> convertReplyCleanup config chatId

let doDownload config client chatMessage =
    let chatId = chatMessage.ChatId
    let sendMessage s a =
        sprintf s a
        |> (sendMessageAsync config chatId)
    chatMessage.Message
    |> parseUri
    |> Async.result
    |> AsyncResult.tap (sendMessage "Downloading %A")
    |> AsyncResult.bind (download client)
    |> convertReplyCleanup config chatId

let reject config chatMessage = sendMessageAsync config chatMessage.ChatId "You are not whitelisted. :^)"

let onUpdate config client settings (chatMessage: ChatMessage) =
    chatMessage |> isVideoMessage |> printfn "%A"
    async {
        let isAdmin = Set.contains chatMessage.User settings.Admins
        let isWhitelist = Set.contains chatMessage.User settings.Whitelist
        log <| sprintf "Message from %s: %A" chatMessage.User chatMessage.Message
        match chatMessage with
        | Command "/whitelist add" msg when isAdmin ->
            return! addToWhitelist config settings msg
        | Command "/whitelist remove" msg when isAdmin ->
            return! removeFromWhitelist config settings msg
        | Command "/whitelist" msg when isAdmin ->
            return! sendWhitelist config settings msg
        | msg when isWhitelist && isVideoMessage msg ->
            return! convertWebm config client msg
        | msg when isWhitelist ->
            return! doDownload config client msg
        | msg ->
            return! reject config msg
    }

let sendError config chatMessage s =
    async {
        logf "Error encountered %s" s
        let! result = sendMessageAsync config chatMessage.ChatId s
        match result with
        | Ok _ -> ()
        | Error e -> logf "Failed to send error! %s" e
    }

let rec run (settings: Settings) =
    try
        let client = new HttpClient()
        let config =
            { defaultConfig with
                Token = settings.Token
                Client = client
                OnError = fun e -> logf "Encountered error %A" e }

        let updatesArrived context =
            match context |> extractMessage with
            | None ->
                log <| sprintf "Unable to extract message: %A" context.Update
                async.Zero()
            | Some message ->
                onUpdate config client settings message
                |> AsyncResult.bindError (sendError config message)
                |> Async.Ignore
        startBotAsync config updatesArrived None
    with
        | e ->
            logf "Run encountered exception: %A" e
            run settings
