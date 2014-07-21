namespace FSharp.Patterns.Examples

module Main =

    open FSharp.Patterns.Operators
    module G = FSharp.Patterns.Generators
    module P = FSharp.Patterns.PrettyPrinting

    [<EntryPoint>]
    let main argv = 
        printfn "Dates:"
        Dates.printAllVarations()

        printfn "\nBook Summaries:"
        BookSummary.printBookSummaries ()
        0
