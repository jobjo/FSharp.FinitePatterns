namespace FSharp.Patterns

module internal Utils =
    let maybe d f = function
        | None      -> d
        | Some x    -> f x
    
    let joinWith d = function
        | []        -> 
            []
        | y :: ys   -> 
            y :: (List.foldBack (fun x xs -> d :: x :: xs) ys [] )
