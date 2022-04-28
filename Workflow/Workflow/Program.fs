open System


type RoundBuilder(i) =
    let debug (x : float) (i : int) = Math.Round(x, i)
    member this.Bind(x, f) =
        f (debug x i)
    member this.Return(x : float) = Math.Round(x, i)
    
type CalculateBuilder() =     
    member this.Bind(x : string, f) =
        try
            match Int32.TryParse x with
            | true, x -> f x
            | false, _ -> raise(ArgumentException())
        with
            | :? ArgumentException -> None
    member this.Return(x) = Some x