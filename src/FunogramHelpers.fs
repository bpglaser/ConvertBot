module FunogramHelpers

open System.IO
open System.Net.Http

open Maybe
open SimpleLog

open Funogram.Api
open Funogram.Types
open Funogram.Telegram
open Funogram.Telegram.Bot
open System

type ChatMessage =
    { User: string
      ChatId: int64
      Message: string option
      Context: UpdateContext }

let (|Command|_|) (prefix: string) (chatMessage: ChatMessage) =
    match chatMessage.Message with
    | Some message when message.StartsWith(prefix) -> Some chatMessage
    | _ -> None

let convertFunogramResult (result: Result<'a, ApiResponseError>) =
    match result with
    | Ok _ -> Ok()
    | Error e -> Error e.Description

let sendMessageAsync config chatId message =
    sprintf "Sending %A => %s" chatId message
    |> SimpleLog.Log

    Api.sendMessage chatId message
    |> api config
    |> Async.map convertFunogramResult

let extractMessage (context: UpdateContext) =
    maybe {
        let! message = context.Update.Message
        let! user = message.Chat.Username
        let chatId = message.Chat.Id

        return
            { User = user
              Message = message.Text
              ChatId = chatId
              Context = context }
    }

let isVideoMessage (chatMessage: ChatMessage) =
    let context = chatMessage.Context

    context.Update.Message
    |> Option.bind (fun msg -> msg.Document)
    |> Option.bind (fun doc -> doc.MimeType)
    |> (=) (Some "video/webm")

let downloadDocument config (client: HttpClient) (doc: Types.Document) =
    async {
        let! file = doc.FileId |> Api.getFile |> api config

        match file with
        | Error e -> return Error e.Description
        | Ok file ->
            match file.FilePath with
            | None -> return Error "No file path for file"
            | Some filePath ->
                let uri = $"https://api.telegram.org/file/bot{config.Token}/{filePath}"
                use! inStream = client.GetStreamAsync(uri) |> Async.AwaitTask

                let tempFilename =
                    Path.GetTempPath()
                    + Guid.NewGuid().ToString()
                    + ".webm"

                printfn "%A" tempFilename
                use outStream = File.Create tempFilename
                do! inStream.CopyToAsync outStream |> Async.AwaitTask
                return Ok tempFilename
    }
