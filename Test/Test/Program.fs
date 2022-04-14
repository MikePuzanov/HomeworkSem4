module Program

open Test.Tree


let tree = Tree(2, Tip(4), Tree(3, Tree(6, Tip(5), Tip(8)), Tip(1)))

let newTree = funcToTree tree (fun x -> x % 2 = 0)

printf $"{funcToTree tree (fun x -> x % 2 = 0)}"