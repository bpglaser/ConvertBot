module Whitelist

open System
open FunogramHelpers
open Settings

let save path names =
    IO.File.WriteAllLinesAsync(path, Array.ofSeq names)
    |> Async.AwaitTask

let modifyWhitelist action responseMessage config (settings: Settings) chatMessage =
    async {
        match chatMessage.Message with
        | None -> return Error "No chat message provided"
        | Some msg ->
            let words = msg.Split(' ')

            if words.Length = 3 then
                let user = words.[2]
                settings.Whitelist <- action user settings.Whitelist
                do! save "whitelist" settings.Whitelist
                return! sendMessageAsync config chatMessage.ChatId (sprintf responseMessage user)
            else
                return Error "Invalid command"
    }

let sendWhitelist config (settings: Settings) chatMessage =
    let message =
        sprintf "Whitelist:\n%s" (String.Join("\n", settings.Whitelist |> Array.ofSeq))

    sendMessageAsync config chatMessage.ChatId message

let addToWhitelist config settings chatMessage =
    async {
        let! result = modifyWhitelist Set.add "Added \"%s\" to whitelist" config settings chatMessage

        match result with
        | Ok _ -> return! sendWhitelist config settings chatMessage
        | _ -> return result
    }

let removeFromWhitelist config settings chatMessage =
    async {
        let! result = modifyWhitelist Set.remove "Removed \"%s\" from whitelist" config settings chatMessage

        match result with
        | Ok _ -> return! sendWhitelist config settings chatMessage
        | _ -> return result
    }
