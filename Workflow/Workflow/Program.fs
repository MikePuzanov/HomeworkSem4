module Workflows
open System

type RoundBuilder(i : int) =
    static member rounding = RoundBuilder
    member this.Bind(x: float, f) = f (Math.Round(x, i))
    member this.Return(x: float) = Math.Round(x, i)
    
type CalculateBuilder() =     
    member this.Bind(x : string, f) =
            match Int32.TryParse x with
            | true, x -> f x
            | false, _ -> None
    member this.Return(x) = Some x