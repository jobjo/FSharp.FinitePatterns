namespace FSharp.FinitePatterns

/// Pattern generators.
module Generators =

    /// Generate all patterns.
    let generateAll =
        let rec go b k =
            match b with
            | Fail              ->
                k []
            | Empty             ->
                k [[]]
            | Literal x         ->
                k [[x]]
            | Choice (b1,b2)    ->
                go b1 <| fun xs -> 
                    go b2 <| fun ys -> k (xs @ ys)
            | Sequence (b1, b2) ->
                go b1 <| fun xs ->
                    go b2 <| fun ys -> 
                        k [ for x in xs do for y in ys do yield x @ y ]
        fun p -> go p id

    /// Generate first pattern if exists.
    let generateFirst patt =
        match generateAll patt with
        | []        -> None
        | x :: _    -> Some x


