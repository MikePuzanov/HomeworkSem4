module Test.Test.MinInListTest

open NUnit.Framework
open FsUnit
open Test.Tree

[<Test>]
let funcToTreeTest () =
    let tree1 = Tree(3, Tip(4), Tip(61))
    let tree2 = Tree(3, Tip(1), Tip(61))
    let tree3 = Tree(2, Tip(4), Tip(61))
    
    let list1 = funcToTree tree1 (fun x -> x % 2 = 0)
    let list2 = funcToTree tree2 (fun x -> x % 2 = 0)
    let list3 = funcToTree tree3 (fun x -> x % 2 = 0)
    
    list1 |> should equal [4]
    list2 |> should be Empty
    list3 |> should equal [2; 4]