namespace FSharp.FinitePatterns

module internal Utils =

    let maybe d f = function
        | None      -> d
        | Some x    -> f x

    let joinWith d xs = 
        match List.ofSeq xs with
        | []        -> []
        | y :: ys   ->  y :: (List.foldBack (fun x xs -> d :: x :: xs) ys [] )

    module TreePrinter = 
        type TreePrinter = private { Run : int -> list<string>}
    
        let private treePrinter f = {Run = f}
        let private space n = 
            ("", List.replicate (n * 2) " ")
            ||> Seq.fold (+)
    
        let print s = treePrinter <| fun n -> [sprintf "%s%s" (space n) s]

        let nest p = treePrinter <| fun n -> p.Run (n+1)

        let run (p: TreePrinter) = 
            let sb = new System.Text.StringBuilder()
            for s in p.Run 0 do
                ignore <| sb.Append s
                ignore <| sb.Append "\n"
            sb.ToString()
        
        let empty = treePrinter <| fun _ -> []
        let (<+>) tp1 tp2 = treePrinter <| fun n -> tp1.Run n @ tp2.Run n
        let (!<) ps = Seq.fold (<+>) empty ps