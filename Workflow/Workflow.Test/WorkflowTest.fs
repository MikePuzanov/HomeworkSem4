module Workflow.Test

open System
open NUnit.Framework
open FsUnit
open Workflows

[<Test>]
let RoundTest () =
    let rounding i = new RoundBuilder(i)
    let result =
        rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    result |> should equal 0.048
    
[<Test>]
let RoundWithTest () =
    (fun () ->
        RoundBuilder.rounding -3 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        } |> ignore) |> should throw typeof<System.ArgumentOutOfRangeException>
    
[<Test>]
let CalculateTest () =
    let calculate = new CalculateBuilder()
    let result =
        calculate {
        let! a = "1"
        let! b = "3"
        let z = a + b
        return z
    }
    result |> should equal (Some 4)
    
[<Test>]
let CalculateTestIncorrect () =
    let calculate = new CalculateBuilder()
    let result =
        calculate {
        let! a = "1"
        let! b = "."
        let z = a + b
        return z
    }
    result |> should equal None