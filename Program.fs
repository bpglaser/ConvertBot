open System
open System.Diagnostics
open System.Net

open Maybe
open SimpleLog

open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Funogram.Telegram.Types

open FSharp.Data
open FSharp.Data.JsonExtensions


let log msg = SimpleLog.Log(msg)


type Settings =
    { Token: string
      mutable Admins: string Set
      mutable Whitelist: string Set }


type FileState =
    { Downloaded: string option
      Converted: string }

type ChatMessage =
    { User: string
      Message: string
      ChatId: int64 }


let convertFunogramResult (result: Result<'a, ApiResponseError>) =
    match result with
    | Ok _ -> Ok()
    | Error e -> Error e.Description


let sendMessageAsync config chatId message =
    sprintf "Sending %A => %s" chatId message |> log
    sendMessage chatId message
    |> api config
    |> Async.map convertFunogramResult


let isRedditUri (uri: Uri) = uri.Host = "www.reddit.com" || uri.Host = "v.redd.it"


let getRedditPath (client: WebClient) (uri: Uri) =
    let result = client.DownloadString(sprintf "%s.json" (uri.ToString()))
    let result = JsonValue.Parse(result)
    let fallback = result.[0]?data?children.[0]?data?media?reddit_video?fallback_url
    fallback.AsString() |> Uri


let download config chatId (client: WebClient) convert uri =
    async {
        let! result = sendMessageAsync config chatId "Downloading..."
        match result with
        | Error e -> return Error e
        | Ok _ ->
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

                return Ok
                           { Downloaded = None
                             Converted = fileName }
            else
                let parts = uri.AbsolutePath.Split('/')
                let fileName = Array.last parts
                printfn "Downloading [%s] to [%s]..." (uri.ToString()) fileName
                client.DownloadFile(uri, fileName)
                printfn "Download finished."
                return! convert fileName
    }

let convert config chatId (fileName: string) =
    async {
        if fileName.EndsWith(".mp4") then
            printfn "No conversion needed."
            return Ok
                       { Downloaded = Some fileName
                         Converted = fileName }
        else
            let! result = sendMessageAsync config chatId "Converting..."
            match result with
            | Error e -> return Error e
            | Ok _ ->
                let! result = Convert.ffmpegConvertMP4 fileName
                match result with
                | Ok outputFileName ->
                    printfn "Converting done."
                    return Ok
                               { Downloaded = Some fileName
                                 Converted = outputFileName }
                | Error e -> return Error e
    }


let reply config chatID fileState =
    async {
        let! result = sendMessageAsync config chatID "Uploading..."
        match result with
        | Error e -> return Error e
        | Ok _ ->
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
    if fileState.Downloaded.IsSome && IO.File.Exists(fileState.Downloaded.Value) then
        IO.File.Delete(fileState.Downloaded.Value)
    if IO.File.Exists(fileState.Converted) then IO.File.Delete(fileState.Converted)


let reject config chatMessage = sendMessageAsync config chatMessage.ChatId "You are not whitelisted. :^)"


let save path names = IO.File.WriteAllLinesAsync(path, Array.ofSeq names) |> Async.AwaitTask


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
    async {
        let! result = chatMessage.Message
                      |> Uri
                      |> download config chatMessage.ChatId client (convert config chatMessage.ChatId)
        match result with
        | Ok fileState ->
            let! result = reply config chatMessage.ChatId fileState
            return result |> Result.map cleanup
        | Error e -> return Error e
    }


let modifyWhitelist action responseMessage config settings chatMessage =
    async {
        let words = chatMessage.Message.Split(' ')
        if words.Length = 3 then
            let user = words.[2]
            settings.Whitelist <- action user settings.Whitelist
            do! save "whitelist" settings.Whitelist
            return! sendMessageAsync config chatMessage.ChatId (sprintf responseMessage user)
        else
            return Error ""
    }


let sendWhitelist config settings chatMessage =
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


let (|Command|_|) (prefix: string) chatMessage =
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


let loadSettings() =
    let token =
        IO.File.ReadLines("token")
        |> seq
        |> Seq.head

    let admins = IO.File.ReadLines("admins") |> Set.ofSeq
    let whitelist = IO.File.ReadLines("whitelist") |> Set.ofSeq
    // Ensure that admins are in the whitelist
    let whitelist = Set.union admins whitelist
    { Token = token
      Admins = admins
      Whitelist = whitelist }


let runClient settings =
    let config = { defaultConfig with Token = settings.Token }
    use client = new WebClient()

    let updatesArrived =
        (onUpdate config client settings
         >> Async.RunSynchronously
         >> ignore)
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
