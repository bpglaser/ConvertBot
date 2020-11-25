open System
open System.Diagnostics
open System.Net

open Maybe

open Funogram.Api
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Funogram.Telegram.Types

open FSharp.Data
open FSharp.Data.JsonExtensions


type Settings =
    { Token: string
      mutable Admins: string Set
      mutable Whitelist: string Set }


type FileState =
    { Downloaded: string
      Converted: string }

type ChatMessage =
    { User: string
      Message: string
      ChatId: int64 }


let sendMessageSync config chatId message =
    let result = 
        sendMessage chatId message
        |> api config
        |> Async.RunSynchronously
    match result with
    | Ok _ -> ()
    | Error e -> raise (e.AsException())


let isRedditUri (uri: Uri) =
    uri.Host = "www.reddit.com"


let getRedditPath (client: WebClient) (uri: Uri) =
    let result = client.DownloadString(sprintf "%s.json" (uri.ToString()))
    let result = JsonValue.Parse(result)
    let fallback = result.[0]?data?children.[0]?data?media?reddit_video?fallback_url
    fallback.AsString() |> Uri


let download config chatId (client: WebClient) (uri: Uri) =
    sendMessageSync config chatId "Downloading..."
    let uri = if isRedditUri uri then getRedditPath client uri else uri
    let parts = uri.AbsolutePath.Split('/')
    let fileName = Array.last parts
    printfn "Downloading [%s] to [%s]..." (uri.ToString()) fileName
    client.DownloadFile(uri, fileName)
    printfn "Download finished."
    fileName


let convert config chatId (fileName: string) =
    if fileName.EndsWith(".mp4") then
        printfn "No conversion needed."
        { Downloaded = fileName
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
        // proc.StartInfo.RedirectStandardOutput <- true
        // proc.StartInfo.RedirectStandardError <- true
        // proc.OutputDataReceived.Add(fun args -> printfn "%s" args.Data)
        // proc.ErrorDataReceived.Add(fun args -> eprintfn "%s" args.Data)

        proc.Start() |> ignore
        // proc.BeginOutputReadLine()
        // proc.BeginErrorReadLine()
        proc.WaitForExit()

        if proc.ExitCode <> 0 then
            raise (Exception("ffmpeg failed"))

        printfn "Converting done."
        { Downloaded = fileName
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
    if IO.File.Exists(fileState.Downloaded) then
        IO.File.Delete(fileState.Downloaded)
    if IO.File.Exists(fileState.Converted) then
        IO.File.Delete(fileState.Converted)


let reject config chatMessage =
    sendMessageSync config chatMessage.ChatId "You are not whitelisted. :^)"
    true


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


let dispatch (context: UpdateContext) handlers =
    match extractMessage context with
    | None -> false
    | Some chatMessage ->
        printfn "Request from: %A => %A" chatMessage.User chatMessage.Message
        let rec inner handlers =
            if Seq.isEmpty handlers then false
            else
                let handler = Seq.head handlers
                if handler chatMessage then true
                else handlers |> Seq.tail |> inner
        inner handlers


let admin settings handler chatMessage =
    if Set.contains chatMessage.User settings.Admins then
        handler chatMessage
    else
        false


let whitelist settings handler chatMessage =
    if Set.contains chatMessage.User settings.Whitelist then
        handler chatMessage
    else
        false


let command (prefix: string) handler (chatMessage: ChatMessage) =
    if chatMessage.Message.StartsWith(prefix) then
        handler chatMessage
    else
        false


let doDownload config client chatMessage =
    try
        chatMessage.Message
        |> Uri
        |> download config chatMessage.ChatId client
        |> convert config chatMessage.ChatId
        |> reply config chatMessage.ChatId
        |> cleanup
    with
        | :? UriFormatException ->
            sendMessageSync config chatMessage.ChatId "Invalid URL"
        | _ ->
            sendMessageSync config chatMessage.ChatId "Bot failed :("
    true


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
    true


let addToWhitelist config settings chatMessage =
    let result = modifyWhitelist Set.add "Added \"%s\" to whitelist" config settings chatMessage
    if result then sendWhitelist config settings chatMessage
    else false


let removeFromWhitelist config settings chatMessage =
    let result = modifyWhitelist Set.remove "Removed \"%s\" from whitelist" config settings chatMessage
    if result then sendWhitelist config settings chatMessage
    else false


let onUpdate config client settings (context: UpdateContext) =
    let admin = admin settings
    let whitelist = whitelist settings
    let addToWhitelist = addToWhitelist config settings
    let removeFromWhitelist = removeFromWhitelist config settings
    let sendWhitelist = sendWhitelist config settings
    dispatch context [
        admin <| command "/whitelist add" addToWhitelist
        admin <| command "/whitelist remove" removeFromWhitelist
        admin <| command "/whitelist" sendWhitelist
        whitelist (doDownload config client)
        reject config
    ] |> ignore


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
    printfn "Loaded settings: %A" settings
    settings
    |> runClient
    |> Async.RunSynchronously
    0
