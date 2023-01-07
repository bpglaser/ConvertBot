module HttpRetry

open System
open System.Net.Http
open System.Threading.Tasks

open SimpleLog

open Polly

let sanitizeUri (uri: Uri) = 
    if uri.Host = "api.telegram.org" && uri.AbsolutePath.StartsWith "/bot" then
        let pathChunks = uri.AbsolutePath[1..].Split '/'
        pathChunks[0] <- "bot{{TOKEN}}"
        let b = UriBuilder(uri)
        b.Path <- String.Join('/', pathChunks)
        b.Uri.ToString()
    else
        uri.ToString()

type HttpRetryMessageHandler(handler: HttpClientHandler) =
    inherit DelegatingHandler(handler)

    member internal self.SendAsyncBase(request: HttpRequestMessage, cancellationToken) =
        sprintf "%s %s" (request.Method.ToString()) (sanitizeUri request.RequestUri)
        |> log

        ``base``.SendAsync(request, cancellationToken)

    override self.SendAsync(request, cancellationToken) =
        Policy
            .Handle<HttpRequestException>()
            .Or<TaskCanceledException>()
            .WaitAndRetryAsync(3, (fun retryAttempt -> TimeSpan.FromSeconds(Math.Pow(3, retryAttempt))))
            .ExecuteAsync(fun () -> self.SendAsyncBase(request, cancellationToken))
