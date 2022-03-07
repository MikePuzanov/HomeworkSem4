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
    let tree1 = Tree.Tree(2, Tree.Tree(4, Tree.Empty, Tree.Empty), Tree.Tree(6, Tree.Empty, Tree.Empty))
    
    let newTree = funcToTree tree1 (fun x -> x * 2)
    
    newTree |> should equal (Tree.Tree(4, Tree.Tree(8, Tree.Empty, Tree.Empty), Tree.Tree(12, Tree.Empty, Tree.Empty)))
    
[<Test>]
let countTreeTest () =
    let tree1 = TreeArithmetic.Node(Multi, Leaf 2, Node(Sum, Leaf 1, Leaf 1))
    
    let result = count tree1
    
    result |> should equal 4
    
[<Test>]
let primeNumbersTest () =
   
    let number13 = primeNumbers |> Seq.item 5

    number13 |> should equal 13
    