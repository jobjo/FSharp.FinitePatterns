namespace FSharp.FinitePatterns.Examples

module Dates =
    open System
    open System.Globalization
    open FSharp.FinitePatterns.Operators
    module G = FSharp.FinitePatterns.Generators
    module P = FSharp.FinitePatterns.PrettyPrinting

    (*
        This is an example of using Patterns for generating different
        variations of dates.
    *)

    /// Pattern for choosing a month.
    let months = [1..12] |> List.map (!) |> choose

    /// Given a year and a month returns a pattern choosing one of the days.
    let days y m = [1 .. DateTime.DaysInMonth(y,m)] |> List.map (!) |> choose

    /// Helper function to check if a given date is a Friday.
    let isFriday y m d = DateTime(y,m,d).DayOfWeek = DayOfWeek.Friday

    /// Helper function to add an initial zero for numbers below 10.
    let toTwoDigits x = if x > 9 then string x else (sprintf "0%A" x)
    
    /// Given a year, month and a day returns a pattern representing variations
    /// of date formats.
    let dateVariations y m d =
        let sy = string y
        let sm = toTwoDigits m
        let sd = toTwoDigits d
        let weekDay = DateTime(y,m,d).DayOfWeek.ToString()
        let monthName = CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName(m);

        // Choose one separator. Either slash or hyphen.
        let sep = !?<[ !"-"; !"/"]

        // Choose one of the following patterns
        [
            !<[ !sy; sep; !sm; sep; !sd ]
            !<[ !sm; sep; !sd ; sep; !sy ]
            !<[ !weekDay; !" "; !sd; !" "; !monthName ; !" "; !sy ]
        ]
        |> choose

    /// Variations for all Fridays 2013
    let fridays =
        pattern {
            let! y = !2013 <|> !2014 <|> !2015 <|> !2016
            let! m = months
            let! d = days y m
            if isFriday y m d then
                return! P.withRenderNoSpace <| dateVariations y m d
        }

    /// Prints the variations of dates for Fridays 2013.
    let printAllVarations () =
        let dates = G.generateAll fridays
        for x in List.collect id dates do
            printfn "%s" x
        printfn "%A" (Seq.length dates)



