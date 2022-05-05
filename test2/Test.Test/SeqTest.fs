module Test.SquareTest

open NUnit.Framework
open FsUnit
open SeqTest


[<Test>]
let SeqTest () =
    let first15Nums = [ for i in 0 .. 14 -> Seq.item i (seqMake()) ]

    first15Nums |> should equal [ 1; 2; 2; 3; 3; 3; 4; 4; 4; 4; 5; 5; 5; 5; 5]