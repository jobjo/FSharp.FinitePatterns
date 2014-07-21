namespace FSharp.FinitePatterns

/// Defines basic pattern operators.
module Operators =

    /// Maps the literals of a pattern.
    let rec map f p =
        match p with
        | Fail              -> Fail
        | Empty             -> Empty
        | Literal l         -> Literal (f l)
        | Choice (b1,b2)    -> Choice (map f b1, map f b2)
        | Sequence (b1,b2)  -> Sequence (map f b1, map f b2)

    /// Monadic join. Flattens a pattern of patterns.
    let rec join = function
        | Fail              -> Fail
        | Empty             -> Empty
        | Literal p         -> p
        | Choice (p1,p2)    -> Choice (join p1, join p2)
        | Sequence (p1,p2)  -> Sequence (join p1, join p2)

    /// Monadic bind.
    let bind p f = join <| map f p

    /// Applicative composition operator.
    let apply pf p = bind pf <| fun f -> bind p (fun x -> f x)

    let rec internal mapFirst f p =
        match p with
        | Fail              -> Fail
        | Empty             -> Empty
        | Literal l         -> Literal (f l)
        | Choice (b1,b2)    -> Choice (mapFirst f b1, mapFirst f b2)
        | Sequence (b1, b2) -> Sequence (mapFirst f b1, b2)

    let rec internal mapLast f p =
        match p with
        | Fail              -> Fail
        | Empty             -> Empty
        | Literal l         -> Literal (f l)
        | Choice (b1,b2)    -> Choice (mapLast f b1, mapLast f b2)
        | Sequence (b1, b2) -> Sequence (b1, mapLast f b2)

    /// Checks if a pattern is equivalent to the empty pattern.
    let rec isEmpty = function
        | Fail              -> false
        | Empty             -> true
        | Literal _         -> false
        | Choice (b1,b2)    -> false
        | Sequence (b1,b2)  -> isEmpty b1 && isEmpty b2

    /// Checks whether pattern is failing.
    let rec isFail p =
        let rec go p k = 
            match p with
            | Fail              -> 
                k true
            | Empty             -> 
                k false
            | Literal _         -> 
                k false
            | Choice (b1,b2)    -> 
                go b1 (fun x ->  go b2 (fun y -> k (x && y)))
            | Sequence (b1,b2)  -> 
                go b1 (fun x -> go b2 (fun y -> k (x || y)))
        go p id

    /// The failing pattern. Identity for Choice.
    let fail = Fail
        
    /// Empty pattern. Identity for Sequence.
    let empty = Empty

    /// Pattern representing a literal value.
    let literal = Literal

    /// Sequences two patterns.
    let add b1 b2 =
        if isFail b1 || isFail b2 then Fail
        elif isEmpty b1 then b2
        elif isEmpty b2 then b1
        else Sequence (b1,b2)

    /// Choose between two patterns.
    let either b1 b2 =  
        if isFail b1 then b2
        elif isFail b2 then b1
        else Choice (b1,b2)

    /// Choose between a sequence of patterns.
    let choose xs = Seq.fold either Fail xs

    /// Sequences a sequence of patterns.
    let sequence xs = Seq.fold add Empty xs

    /// Sequences a sequence of patterns with a separator.
    let rec separateBy s = Utils.joinWith s >> sequence

    /// Create a pattern from an optional value.
    /// Maps None to the pattern Fail.
    let fromOption = function
        | Some x    -> Literal x
        | None      -> Fail

    /// Adds a prefix to a non-empty pattern.
    let rec prefix b1 b2 =
        if isFail b2 then
            Fail
        elif isEmpty b2 then
            Empty
        else
            match b2 with
            | Literal _             -> add b1 b2
            | Choice (b1',b2')      -> Choice (prefix b1 b1', prefix b1 b2')
            | Sequence (b1',b2')    -> Sequence (prefix b1  b1', b2')
            | _                     -> Fail

    /// Adds a postfix to a non-empty pattern.
    let rec postfix b1 b2 =
        if isFail b1 then
            Fail
        elif isEmpty b1 then
            Empty
        else
            match b1 with
            | Literal _             -> add b1 b2
            | Choice (b1',b2')      -> Choice (postfix b1' b2, postfix b2' b2)
            | Sequence (b1',b2')    -> Sequence (b1', postfix b2' b2)
            | _                     -> Fail

    /// Default to Empty for failing patterns.
    let maybeEmpty x = either x empty

    /// Unary operators
    let (!)         = literal
    let (!<)        = sequence
    let (!?<)       = choose
    let (!?)        = maybeEmpty
    let (!~)        = fromOption
    
    /// Binary operators
    let (<|>)       = either
    let (<+>)       = add
    let (<?)        = prefix
    let (?>)        = postfix
    let (>>=)       = bind
    let (<*>)       = apply

    /// Composed unary operators
    let (!?!) x     = !?(!x)
    let (!?!?<) xs  = !?(!?<xs)
    let (!?!<) xs   = !?(!<xs)
    let (!?!~) x    = !?(!~x)

    /// Computation expression builder.
    type PatternBuilder() =
        member this.Bind (p,f) = p >>= f
        member this.Return(x) = Literal x
        member this.Yield(x) = Literal x
        member this.YieldFrom(x) = x
        member this.ReturnFrom(p) = p
        member this.Zero() = Fail
        member this.For (ps,f) = sequence <| Seq.map f ps
        member this.Combine(p1,p2) = p1 <+> p2
        member this.Delay(f) = f ()

    /// Builds a pattern work flow
    let pattern  = PatternBuilder()



    


