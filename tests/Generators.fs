namespace FSharp.Patterns.Tests

module Generators =
    open FSharp.Patterns
    open FsCheck

    let patternGen =
        let leafPatternGen =
            [
                4, Gen.map Literal (Gen.choose (1,100))
                1, Gen.constant Fail
            ]
            |> Gen.frequency

        let rec pattern n =
            if n <= 0 then
                leafPatternGen
            else
                gen {
                    let! c1 = pattern (n / 2)
                    let! c2 = pattern (n / 2)
                    let! op =
                        Gen.oneof [
                            Gen.constant Pattern.Choice
                            Gen.constant Pattern.Sequence
                        ]
                    return op (c1,c2)
                }
        Gen.sized pattern

    let seed = Random.newSeed ()
    
    /// Custom generators.
    type CustomGenerators =
        static member Pattern() = Arb.fromGen patternGen
