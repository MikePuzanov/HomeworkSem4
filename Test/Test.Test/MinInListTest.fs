module Test.Test.MinInLisTest

open NUnit.Framework
open FsUnit
open Test.MinInList


[<Test>]
let minInListTest () =
    let value = minInList [5; 2; 4; 5; 7; 3; 6; 2]
    let value2 = minInList [5; 2; 4; 2; 7; 3; 6; 2]
    let value3 = minInList [5; -2; 4; 5; 7; 3; 6; 2]
    
    value |> should equal 2
    value2 |> should equal 2
    value3 |> should equal -2