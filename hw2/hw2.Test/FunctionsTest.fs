module hw2.Test

open NUnit.Framework
open FsCheck
open FsUnit
open Program

[<Test>]
let countEvenNumbersCheck () =
    let check x =
        let mapFilter = countEvenNumbersMap x = countEvenNumbersFilter x
        let mapFold = countEvenNumbersMap x = countEvenNumbersFold x
        let foldFilter = countEvenNumbersFold x = countEvenNumbersFilter x
        match mapFilter, mapFold, foldFilter with
        | true, true, true -> true
        | _ -> false
    
    Check.QuickThrowOnFailure check

[<Test>]
let funcToTreeTest () =
    let tree1 = Tree.Tree(2, Tree.Tip(4), Tree.Tip(6))
    
    let newTree = funcToTree tree1 (fun x -> x * 2)
    
    newTree |> should equal (Tree(4, Tree.Tip(8), Tree.Tip(12)))
    
[<Test>]
let countTreeTest () =
    let tree1 = TreeArithmetic.Node(Multiplication, Leaf 2, Node(Summation, Leaf 1, Leaf 1))
    
    let result = count tree1
    
    result |> should equal 4
    
[<Test>]
let primeNumbersTest () =
    let first7PrimeNums = [ for i in 0 .. 7 -> Seq.item i (primeNumbers()) ]

    first7PrimeNums |> should equal [ 2; 3; 5; 7; 11; 13; 17; 19]
    