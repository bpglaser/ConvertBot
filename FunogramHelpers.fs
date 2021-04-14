module FunogramHelpers

open Maybe
open SimpleLog

open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot

type ChatMessage =
    { User: string
      Message: string
      ChatId: int64 }

let (|Command|_|) (prefix: string) (chatMessage: ChatMessage) =
    if chatMessage.Message.StartsWith(prefix) then Some chatMessage else None

let convertFunogramResult (result: Result<'a, ApiResponseError>) =
    match result with
    | Ok _ -> Ok()
    | Error e -> Error e.Description

let sendMessageAsync config chatId message =
    sprintf "Sending %A => %s" chatId message |> SimpleLog.Log
    sendMessage chatId message
    |> api config
    |> Async.map convertFunogramResult

let extractMessage (context: UpdateContext) =
    maybe {
        let! message = context.Update.Message
        let! user = message.Chat.Username
        let! text = message.Text
        let chatId = message.Chat.Id
        return { User = user
                 Message = text
                 ChatId = chatId }
    }
