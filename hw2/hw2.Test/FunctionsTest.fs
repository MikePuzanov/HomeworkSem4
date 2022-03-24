module hw2.Test

open NUnit.Framework
open FsCheck
open FsUnit
open Program

[<Test>]
let countEvenNumbersCheckWithNumbers () =
    let result = countEvenNumbersFilter [1; 2; 3; 4; 5; 6]
    
    result |> should equal 3

[<Test>]
let countEvenNumbersCheckEqual () =
    let check x =
        let mapFilter = countEvenNumbersMap x = countEvenNumbersFilter x
        let mapFold = countEvenNumbersMap x = countEvenNumbersFold x
        let foldFilter = countEvenNumbersFold x = countEvenNumbersFilter x
        match mapFilter, mapFold, foldFilter with
        | true, true, true -> true
        | _ -> false
    
    Check.QuickThrowOnFailure check
    
[<Test>]
let funcToTreeWhenRightSubtreeIsNotEmpty () =
    let tree = Tree.Tree(2, Tree.Tip(4), Tree.Tree(2, Tree.Tree(4, Tree.Tip(4), Tree.Tip(4)), Tree.Tip(4)))
    
    let newTree = funcToTree tree (fun x -> x * 2)
    
    newTree |> should equal (Tree.Tree(4, Tree.Tip(8), Tree.Tree(4, Tree.Tree(8, Tree.Tip(8), Tree.Tip(8)), Tree.Tip(8))))

[<Test>]
let funcToTreeTest () =
    let tree = Tree.Tree(2, Tree.Tip(4), Tree.Tip(6))
    
    let newTree = funcToTree tree (fun x -> x * 2)
    
    newTree |> should equal (Tree(4, Tree.Tip(8), Tree.Tip(12)))
    
[<Test>]
let countTreeTestSum () =
    let tree1 = TreeArithmetic.Node(Summation, Leaf 2, Node(Summation, Leaf 1, Leaf -1))
    
    let result = count tree1
    
    result |> should equal 2
    
[<Test>]
let countTreeTestSub () =
    let tree1 = TreeArithmetic.Node(Subtracting, Leaf 15, Node(Subtracting, Leaf 10, Leaf -5))
    
    let result = count tree1
    
    result |> should equal 0
    
[<Test>]
let countTreeTestMult () =
    let tree1 = TreeArithmetic.Node(Multiplication, Leaf -2, Node(Multiplication, Leaf 2, Leaf 4))
    
    let result = count tree1
    
    result |> should equal -16
    
[<Test>]
let countTreeTestDiv () =
    let tree1 = TreeArithmetic.Node(Division, Leaf -10, Node(Division, Leaf 10, Leaf 5))
    
    let result = count tree1
    
    result |> should equal -5

[<Test>]
let countTreeTestWithAll () =
    let tree1 = TreeArithmetic.Node(Division,
                                    Node(Summation, Node(Subtracting, Leaf 35, Leaf -15), Leaf 50),
                                    Node(Multiplication, Leaf 10, Leaf 5))
    
    let result = count tree1
    
    result |> should equal 2

    
[<Test>]
let primeNumbersTest () =
    let first7PrimeNums = [ for i in 0 .. 7 -> Seq.item i (primeNumbers()) ]

    first7PrimeNums |> should equal [ 2; 3; 5; 7; 11; 13; 17; 19]
    