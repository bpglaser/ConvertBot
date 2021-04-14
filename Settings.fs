module Settings

open System.IO

type Settings =
    { Token: string
      mutable Admins: string Set
      mutable Whitelist: string Set }

let loadSettings() =
    let token =
        File.ReadLines("token")
        |> seq
        |> Seq.head

    let admins = File.ReadLines("admins") |> Set.ofSeq
    let whitelist = File.ReadLines("whitelist") |> Set.ofSeq
    // Ensure that admins are in the whitelist
    let whitelist = Set.union admins whitelist
    { Token = token
      Admins = admins
      Whitelist = whitelist }
