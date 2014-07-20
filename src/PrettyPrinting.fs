namespace FSharp.Patterns

/// Utilities for Pretty Printing.
module PrettyPrinting =
    open Utils
    open Operators
    module G = Generators
    
    /// Renders a pattern using a given deliminator.
    let private render del specSyms ss =
        match ss with
        | []        -> 
            ""
        | [s]       -> 
            s
        | s :: ss   ->
            let sb = System.Text.StringBuilder()
            ignore <| sb.Append s
            for s in ss do
                let s : string = 
                    if List.exists ((=) s) specSyms then s else del + s
                ignore <| sb.Append s
            sb.ToString()

    let private space = " "
    let private specSymbols = [","; ";"; "."]

    /// Prints the first matching pattern.
    let printFirst = G.generateFirst >> maybe [] id >> render "" []
    
    /// Prints all generated patterns.
    let printAll = G.generateAll  >> List.map (render "" [])

    /// Pretty prints the first pattern with spaces inserted between words.
    let prettyPrintFirst = 
        G.generateFirst >> Option.map (render space specSymbols)

    /// Pretty prints all patterns with spaces inserted between words.
    let prettyPrintAll = G.generateAll >> List.map (render space specSymbols)

    /// Given two separators and a list of blocks, returns 
    /// a block with inserted separators.
    /// Ex: many !"," !"and" [!"foo"; !"bar"; !"baz"] => "foo, bar and baz"
    let many sep1 sep2 =
        let rec go ps =
            match ps with
            | []            -> Empty
            | [p]           -> p
            | [p1;p2]       -> p1 <+> sep2 <+> p2
            | p :: ps       -> p <+> sep1 <+> go ps
        List.filter (isEmpty >> not) >> go

    /// Seperates a list of patterns with `commas` and `and`.
    let commaAnd xs = many !"," !"and" (Seq.toList xs)

    let commaOnly xs = many !"," !"," (Seq.toList xs)

    /// Seperates a list of patterns with `commas` and `or`.
    let commaOr xs = many !"," !"or" (Seq.toList xs)

    /// Transforms a pattern to upper case.
    let toUpper : Pattern<string> -> Pattern<string> = map (fun s -> s.ToUpper())

    /// Transforms a pattern to lower case.
    let toLower : Pattern<string> -> Pattern<string> = map (fun s -> s.ToLower())

    /// Renders all patterns with inserted spaces.
    let renderSpace = prettyPrintAll >> List.map literal >> choose

    /// Render all patterns without any inserted spaces.
    let renderNoSpace = 
        G.generateAll >> List.map (render "" []) >> List.map literal >> choose

    /// Transforms a string pattern to a sentance. First letter upper case and end with dot.
    let toSentence : Pattern<string> -> Pattern<string> =
        let firstLetterUpper = function
            | ""    -> 
                ""
            | str    ->
                let f = str.Substring(0,1)
                let r = str.Substring(1)
                f.ToUpper() + r
        mapFirst firstLetterUpper >> mapLast (sprintf "%s.")
        





    
