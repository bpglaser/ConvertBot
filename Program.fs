open System
open System.Diagnostics
open System.Net

open Maybe
open SimpleLog

open Funogram.Api
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


let sendMessageSync config chatId message =
    log <| sprintf "Sending %A => %s" chatId message
    let result = 
        sendMessage chatId message
        |> api config
        |> Async.RunSynchronously
    match result with
    | Ok _ -> ()
    | Error e -> raise (e.AsException())


let isRedditUri (uri: Uri) =
    uri.Host = "www.reddit.com" || uri.Host = "v.redd.it"


let getRedditPath (client: WebClient) (uri: Uri) =
    let result = client.DownloadString(sprintf "%s.json" (uri.ToString()))
    let result = JsonValue.Parse(result)
    let fallback = result.[0]?data?children.[0]?data?media?reddit_video?fallback_url
    fallback.AsString() |> Uri


let download config chatId (client: WebClient) (convert: string -> FileState) (uri: Uri) =
    sendMessageSync config chatId "Downloading..."

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

        { Downloaded = None 
          Converted = fileName }
    else
        let parts = uri.AbsolutePath.Split('/')
        let fileName = Array.last parts
        printfn "Downloading [%s] to [%s]..." (uri.ToString()) fileName
        client.DownloadFile(uri, fileName)
        printfn "Download finished."
        convert fileName


let convert config chatId (fileName: string) =
    if fileName.EndsWith(".mp4") then
        printfn "No conversion needed."
        { Downloaded = Some fileName
          Converted = fileName }
    else 
        sendMessageSync config chatId "Converting..."
        let baseName = fileName.Substring(0, fileName.LastIndexOf('.'))
        let outputFileName = sprintf "%s.mp4" baseName

        if IO.File.Exists outputFileName then
            IO.File.Delete outputFileName

        printfn "Converting [%s]..." fileName

        use proc = new Process()
        proc.StartInfo.FileName <- "cmd.exe"
        proc.StartInfo.Arguments <- sprintf "/C ffmpeg -i %s %s" fileName outputFileName
        proc.StartInfo.CreateNoWindow <- true
        proc.StartInfo.UseShellExecute <- false

        proc.Start() |> ignore
        proc.WaitForExit()

        if proc.ExitCode <> 0 then
            raise (Exception("ffmpeg failed"))

        printfn "Converting done."
        { Downloaded = Some fileName
          Converted = outputFileName }


let reply config chatID fileState =
    sendMessageSync config chatID "Uploading..."
    printfn "Uploading %s to %i" fileState.Converted chatID
    use stream = IO.File.Open(fileState.Converted, IO.FileMode.Open)
    let response = 
        sendVideo chatID (FileToSend.File(fileState.Converted, stream)) ""
        |> api config
        |> Async.RunSynchronously
    match response with
    | Ok _ -> ()
    | Error e -> raise (e.AsException())
    fileState


let cleanup fileState =
    if fileState.Downloaded.IsSome && IO.File.Exists(fileState.Downloaded.Value) then
        IO.File.Delete(fileState.Downloaded.Value)
    if IO.File.Exists(fileState.Converted) then
        IO.File.Delete(fileState.Converted)


let reject config chatMessage =
    sendMessageSync config chatMessage.ChatId "You are not whitelisted. :^)"


let save path names =
    IO.File.WriteAllLines(path, Array.ofSeq names)


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
    try
        chatMessage.Message
        |> Uri
        |> download config chatMessage.ChatId client (convert config chatMessage.ChatId)
        |> reply config chatMessage.ChatId
        |> cleanup
    with
        | :? UriFormatException ->
            sendMessageSync config chatMessage.ChatId "Invalid URL"
        | e ->
            log ($"Bot failed: {e.ToString()}")
            sendMessageSync config chatMessage.ChatId "Bot failed :("


let modifyWhitelist action responseMessage config settings chatMessage =
    let words = chatMessage.Message.Split(' ')
    if words.Length = 3 then
        let user = words.[2]
        settings.Whitelist <- action user settings.Whitelist
        save "whitelist" settings.Whitelist
        sendMessageSync config chatMessage.ChatId (sprintf responseMessage user)
        true
    else
        false


let sendWhitelist config settings chatMessage =
    let message = sprintf "Whitelist:\n%s" (String.Join("\n", settings.Whitelist |> Array.ofSeq))
    sendMessageSync config chatMessage.ChatId message


let addToWhitelist config settings chatMessage =
    let result = modifyWhitelist Set.add "Added \"%s\" to whitelist" config settings chatMessage
    if result then sendWhitelist config settings chatMessage


let removeFromWhitelist config settings chatMessage =
    let result = modifyWhitelist Set.remove "Removed \"%s\" from whitelist" config settings chatMessage
    if result then sendWhitelist config settings chatMessage


let (|Command|_|) (prefix: string) chatMessage =
    if chatMessage.Message.StartsWith(prefix) then Some chatMessage
    else None


let onUpdate config client settings (context: UpdateContext) =
    let isAdmin user = Set.contains user settings.Admins
    let isWhitelist user = Set.contains user settings.Whitelist

    match extractMessage context with
    | None -> ()
    | Some chatMessage ->
        log <| sprintf "Message from %s: %s" chatMessage.User chatMessage.Message
        match chatMessage with
        | Command "/whitelist add" msg when msg.User |> isAdmin ->
            addToWhitelist config settings msg

        | Command "/whitelist remove" msg when msg.User |> isAdmin ->
            removeFromWhitelist config settings msg

        | Command "/whitelist" msg when msg.User |> isAdmin ->
            sendWhitelist config settings msg

        | msg when msg.User |> isWhitelist ->
            doDownload config client msg

        | msg ->
            reject config msg


let loadSettings() =
    let token = IO.File.ReadLines("token") |> seq |> Seq.head
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
    startBot config (onUpdate config client settings) None


[<EntryPoint>]
let main argv =
    let settings = loadSettings()
    log <| sprintf "Loaded settings: %A" settings
    [ settings |> runClient
      StatusServer.run() ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    0

