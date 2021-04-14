module StatusServer

open SimpleLog

open Suave
open Suave.Filters
open Suave.Operators

let status() =
    fun ctx ->
        async {
            let! log = SimpleLog.GetTail()
            let lines =
                log
                |> List.map (sprintf "<p>%s</p>")
                |> List.fold (+) ""

            let logSection = sprintf "<section>%s</section>" lines
            let response = sprintf "<!DOCTYPE html><html><body>%s</body></html>" logSection
            return! Successful.OK response ctx
        }

let app = choose [ GET >=> choose [ path "/status" >=> status() ] ]

let run() =
    async {
        let _, server = startWebServerAsync defaultConfig app
        do! server
    }
