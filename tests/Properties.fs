namespace FSharp.FinitePatterns.Tests

module Properties =
    open FSharp.FinitePatterns
    open FSharp.FinitePatterns.Operators
    open FSharp.FinitePatterns.Generators
    open Xunit
    open Xunit.Sdk
    open System.Net
    open System
    open FsCheck
    open FsCheck.Xunit


    let private (==) x y = 
        let r1 = generateAll x
        let r2 = generateAll y
        // printfn "r1: %A" r1
        // printfn "r2: %A" r2
        [
            r1  = r2
            generateFirst x = generateFirst y
        ]
        |> List.forall id


    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Left identity for sequence`` (p: Pattern<int>) =
        (empty <+> p) == p

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Empty is right identity for sequence`` (p: Pattern<int>) =
        (p <+> empty) == p

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Left identity for choice`` (p: Pattern<int>) =
        (fail <|> p) == p

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Right identity for choice`` (p: Pattern<int>) =
        (p <|> fail) == p

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Fail in sequence fails`` (p: Pattern<int>) =
        (p <+> fail <+> p) == fail

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``literal id for monadic bind`` (p: Pattern<int>) =
        printfn "%s" (string p)
        p >>= literal == p

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``bind is associative`` (m: Pattern<int>) =
        let f x = 
            match x % 5 with
            | 0 -> !(x + x)
            | 1 -> empty
            | 2 -> !x <+> !(-x)
            | 3 -> !x <|> !(-x)
            | _ -> fail

        let g x =
            match x % 5 with
            | 0 -> !(-x)
            | 1 -> fail
            | 2 -> !(-x) <+> !(x)
            | 3 -> !(2*x) <|> !(3*x)
            | _ -> fail
        let p1 =
            pattern { 
                let! y =
                    pattern { 
                        let! x = m
                        return! f x
                    }
                return! g y
            }
        let p2 =
            pattern {
                let! x = m
                return! 
                    pattern {
                        let! y = f x
                        return! g y
                    }
            }
        let p3 =
            pattern {
                let! x = m
                let! y = f x
                return! g y
            }
        p1 == p2 && p2 == p3


