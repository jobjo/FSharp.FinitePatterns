namespace FSharp.FinitePatterns

[<AutoOpen>]
module Interfaces =
    open Utils
    module TP = Utils.TreePrinter

    let private (!<) = TP.(!<)

    /// A pattern encodes a finite set of matching
    /// sequences of type 'T.
    type Pattern<'T> =
        internal
        | Fail
        | Empty
        | Literal of 'T
        | Choice of Pattern<'T> * Pattern<'T>
        | Sequence of Pattern<'T> * Pattern<'T>
        override this.ToString() =
            let showBin name p1 p2=
                !< [
                    TP.print (sprintf "%A[" name)
                    TP.nest p1
                    TP.nest p2
                    TP.print "]"
                ]
            let rec go p=
                match p with
                | Fail              -> TP.print "Fail"
                | Empty             -> TP.print "Empty"
                | Literal x         -> box x |> string |> TP.print
                | Choice (p1,p2)    -> showBin "Choice" (go p1) (go p2)
                | Sequence (p1,p2)  -> showBin "Sequence" (go p1) (go p2)
            TP.run (go this)
            

