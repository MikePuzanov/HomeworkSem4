module SequenceOfStaples.Test

open NUnit.Framework
open FsUnit
open Program

[<Test>]
let TestExpTrue () =
    let result = checkStaples "(({}))"
    result |> should equal true
    
[<Test>]
let TestExpWrongWithOneMoreStaple () =
    let result = checkStaples "(({})))"
    result |> should equal false
    
[<Test>]
let TestExpWrongWithDifferentSides () =
    let result = checkStaples ")("
    result |> should equal false
    
[<Test>]
let TestExpWrongWithDifferentStaple () =
    let result = checkStaples "(]"
    result |> should equal false