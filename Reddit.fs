module Reddit

open System
open System.Net

open FSharp.Data
open FSharp.Data.JsonExtensions

let isRedditUri (uri: Uri) = uri.Host = "www.reddit.com" || uri.Host = "v.redd.it"

let getRedditPath (client: WebClient) (uri: Uri) =
    let result = client.DownloadString(sprintf "%s.json" (uri.ToString()))
    let result = JsonValue.Parse(result)
    let fallback = result.[0]?data?children.[0]?data?media?reddit_video?fallback_url
    fallback.AsString() |> Uri
