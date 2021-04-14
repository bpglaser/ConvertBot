module FunogramHelpers

open SimpleLog
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api

type ChatMessage =
    { User: string
      Message: string
      ChatId: int64 }

let convertFunogramResult (result: Result<'a, ApiResponseError>) =
    match result with
    | Ok _ -> Ok()
    | Error e -> Error e.Description

let sendMessageAsync config chatId message =
    sprintf "Sending %A => %s" chatId message |> SimpleLog.Log
    sendMessage chatId message
    |> api config
    |> Async.map convertFunogramResult
