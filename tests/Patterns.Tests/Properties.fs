namespace Dealogic.Patterns.Tests

module Properties =
    open Dealogic.Patterns
    open Dealogic.Patterns.Operators
    open Dealogic.Patterns.Generators
    open Xunit
    open Xunit.Sdk
    open System.Net
    open System
    open FsCheck
    open FsCheck.Xunit


    let private (==) x y = 
        let r1 = generateAll x
        let r2 = generateAll y
        [
            r1  = r2
            generateFirst x = generateFirst y
        ]
        |> List.forall id


    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Left identity for sequence`` (p: Pattern<int>) =
        (empty <+> p) == p

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Right identity for sequence`` (p: Pattern<int>) =
        (p <+> empty) == p

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Left identity for choice`` (p: Pattern<int>) =
        (fail <|> p) == p

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Right identity for choice`` (p: Pattern<int>) =
        (p <|> fail) == p


