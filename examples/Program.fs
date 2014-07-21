namespace FSharp.FinitePatterns.Examples

module Main =

    open FSharp.FinitePatterns.Operators
    module G = FSharp.FinitePatterns.Generators
    module P = FSharp.FinitePatterns.PrettyPrinting

    [<EntryPoint>]
    let main argv = 
        printfn "Dates:"
        Dates.printAllVarations()

        printfn "\nBook Summaries:"
        BookSummary.printBookSummaries ()
        0
