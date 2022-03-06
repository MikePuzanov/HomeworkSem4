module hw2.Test

open NUnit.Framework
open FsCheck
open FsUnit
open Program
open Program

[<Test>]
let funcToTreeTest () =
    let tree1 = Tree.Tree(2, Tree.Tree(4, Tree.Empty, Tree.Empty), Tree.Tree(6, Tree.Empty, Tree.Empty))
    
    let newTree = funcToTree tree1 (fun x -> x * 2)
    
    newTree |> should equal (Tree.Tree(4, Tree.Tree(8, Tree.Empty, Tree.Empty), Tree.Tree(12, Tree.Empty, Tree.Empty)))
    
[<Test>]
let countTreeTest () =
    let tree1 = ArithmeticTree.Node(Multi, Leaf 2, Node(Sum, Leaf 1, Leaf 1))
    
    let result = count tree1
    
    result |> should equal 4
    
[<Test>]
let primeNumbersTest () =
    let primeNums10 = [ for i in 0 .. 9 -> Seq.item i primeNumbers ]

    primeNums10 |> should equal [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29 ]