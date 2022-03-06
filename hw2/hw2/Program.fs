open System

let countEvenNumbersFilter list =
    (List.filter (fun x -> x % 2 = 0) list).Length

let countEvenNumbersMap list =
    list |> List.map (fun x -> (x + 1) % 2) |> List.sum

let countEvenNumbersFold list =
    List.fold (fun x acc -> (x + 1) % 2 + acc) 0 list
    
printfn "%d" (countEvenNumbersFilter [1; 3; 5; 7; 9])
printfn "%A" (countEvenNumbersFilter [1; 3; 5; 7; 9])
printfn "%A" (countEvenNumbersFilter [1; 3; 5; 7; 9])

type Tree<'a> =
| Tree of 'a * Tree<'a> * Tree<'a>
| Empty 

let rec funcToTree tree func =
    match tree with
    | Empty -> Empty
    | Tree (value, left, right) -> Tree(func value, funcToTree left func, funcToTree right func)

type Operation =
    | Sum
    | Sub
    | Multi
    | Div
    
type ArithmeticTree<'t> = 
    | Leaf of 't
    | Node of Operation * ArithmeticTree<'t> * ArithmeticTree<'t>

let reс count tree =
    match tree with
    | Leaf x -> x
    | Node(operation, left, right) ->
        let left = count left
        let right = count right
        match operation with
        | Sum -> left + right
        | Sub -> left - right
        | Multi ->  left * right
        | Div when right <> 0 -> left / right
        | Div when right = 0 -> raise(ArgumentNullException "На ноль делить нельзя")

let primeNumbers =
    let rec test element list =
        match list with
        | [] -> true
        | h :: tail when (float h <= sqrt(element |> float) && element % h = 0) -> false
        | h :: tail when element % h <> 0 -> test element tail
        | _ -> false
           
    let rec sequence element list =
        seq {
            if test element list then
                yield element
                yield! sequence (element + 1) (element :: list)
            else
                yield! sequence (element + 1) list
        }
    sequence 2 []
    
printfn "%A" primeNumbers