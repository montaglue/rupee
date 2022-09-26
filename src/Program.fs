// For more information see https://aka.ms/fsharp-console-apps
open Parser
open FParsec

[<EntryPoint>]
let main args = 
    let program = System.IO.File.ReadAllText args[0]
    printfn "%A" (run astParser program)
    0