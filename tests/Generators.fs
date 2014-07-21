namespace FSharp.FinitePatterns.Tests

module Generators =
    open FSharp.FinitePatterns.Operators
    open FsCheck

    let patternGen =
        let leafPatternGen =
            [
                10, Gen.map literal (Gen.choose (1,100))
                2, Gen.constant empty
                1, Gen.constant fail
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
                        [
                            4, Gen.constant (<+>)
                            1, Gen.constant (<|>)
                        ]
                        |> Gen.frequency
                    return op c1 c2
                }

        Gen.sized pattern
        |> Gen.resize 20
        

    let seed = Random.newSeed ()
    
    /// Custom generators.
    type CustomGenerators =
        static member Pattern() = Arb.fromGen patternGen
