namespace FSharp.Patterns.Examples

module Sentences =

    open FSharp.Patterns.Operators
    module G = FSharp.Patterns.Generators
    module P = FSharp.Patterns.PrettyPrinting

    type Book =
        {
            Title : string
            IsDebut : bool
            Year : option<int>
            Authors : list<string>
            Publisher : option<string>
            SalesTrendLastYear : option<float>
            BestSellerRank : option<int>
            Country : option<string>
            Category : option<string>
        }
    
    /// Attach a percent sign postfix to a string.
    let private withPercent x = 
        P.renderNoSpace !<[ !(string x) ; !"%" ]

    /// Helper for attachign a trend prefix to a number.
    let private withPosNeg x =
        !<[
            !(if x > 0. then "increased" else "decreased")
            !"by"
            !(string x)
        ]
        |> P.renderSpace

    let private withParens x = 
        P.renderNoSpace !<[ !"("; !x ; !")"]

    // Description
    let summary book =

        // Authors seperated by commma.
        let authors = 
            !"by" <? P.commaAnd (List.map (!) book.Authors)
        
        // Debug and category pattern. This pattern fails if category if
        // category is none.
        let category =
            if book.IsDebut && book.Authors.Length = 1 then
                !"is a debut" <+> !~book.Category
            else
                !"is a" <? !~book.Category

        let basics =
            let pubCountry =
                [ book.Publisher; book.Country] 
                |> List.map (!?!~)
                |> P.commaOnly
                |> P.renderSpace
            let catAuths =
                (category ?> authors ?> !"and") 
                <|> 
                authors
            !<[
                !book.Title
                catAuths
                !"was first published" <? (map string !?!~book.Year)
                pubCountry >>= withParens
            ]

        let bestSeller =
            !"It is currently number" 
            <? (map string !?!~book.BestSellerRank) 
            ?> !"on the Amazon best sellers rank"

        let trend =
            let trend = !?!~book.SalesTrendLastYear >>= withPosNeg >>= withPercent
            !"Sales" <? trend ?> !"since last year"

        !<[
            P.toSentence basics
            P.toSentence bestSeller
            P.toSentence trend
        ]


    let book1 =
        {
            Title = "The Catcher in the Rye"
            Year = Some 1951
            IsDebut = true
            Authors = ["J.D. Salinger"]
            BestSellerRank = Some 32
            Publisher = Some "Littlem, Brown and Company"
            SalesTrendLastYear = Some 7.
            Country = Some "USA"
            Category = Some "Novel"
        }

    let book2 =
        {
            Title = "Real-World Functional Programming"
            Year = Some 2010
            IsDebut = false
            Authors = ["Tomas Petricek"; "John Skeet"]
            BestSellerRank = None
            Publisher = Some "Manning Publications"
            SalesTrendLastYear = None
            Country = None
            Category = None
        }

    let book3 =
        {
            Title = "How to Make a Tree out of your Finger"
            Year = Some 2012
            IsDebut = false
            Authors = [] // ["David Wayne"; "Laura Gregory"; "Shelly Johnson"]
            BestSellerRank = None
            Publisher = Some "Wayny Day's Publication"
            SalesTrendLastYear = None
            Country = Some "Canada"
            Category = Some "Science and Nature book"
        }

    let printBookSummaries() =

        /// Helper for rendering a pattern.
        let print p = 
            match P.prettyPrintFirst p with
            | Some s    -> printfn "%s\n" s
            | _         -> ()
        
        printfn "%A" (!"A" <? (P.commaAnd []))
        print <| summary book1
        print <| summary book2
        print <| summary book3