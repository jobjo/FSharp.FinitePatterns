namespace FSharp.Patterns

[<AutoOpen>]
module Interfaces =

    /// A pattern encodes a finite set of matching
    /// sequences of type 'T.
    type Pattern<'T> =
        | Fail
        | Empty
        | Literal of 'T
        | Choice of Pattern<'T> * Pattern<'T>
        | Sequence of Pattern<'T> * Pattern<'T>

