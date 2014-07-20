namespace FSharp.Patterns.Examples

module Main =

    open FSharp.Patterns.Operators
    module G = FSharp.Patterns.Generators
    module P = FSharp.Patterns.PrettyPrinting

    [<EntryPoint>]
    let main argv = 
        Sentences.printBookSummaries ()
        0
