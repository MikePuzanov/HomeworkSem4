open System

//  подсчитывает количество чётных чисел в списке c filter
let countEvenNumbersFilter list =
    list |> List.filter (fun x -> x % 2 = 0) |> List.length

// подсчитывает количество чётных чисел в списке c map
let countEvenNumbersMap list =
    list |> List.map (fun x -> (abs x + 1) % 2) |> List.sum

// подсчитывает количество чётных чисел в списке c fold
let countEvenNumbersFold list =
    (0, list) ||> List.fold (fun acc x -> acc + (abs x + 1) % 2)
    
// бинарное дерево
type Tree<'a> =
| Tree of 'a * Tree<'a> * Tree<'a>
| Empty 

// функция, которая применяет функцию к каждому элементу дерева
let rec funcToTree tree func =
    match tree with
    | Empty -> Empty
    | Tree (value, left, right) -> Tree(func value, funcToTree left func, funcToTree right func)

// все операции
type Operation =
    | Sum
    | Sub
    | Multi
    | Div
  
//  арифметическое дерево
type TreeArithmetic<'t> = 
    | Leaf of 't
    | Node of Operation * TreeArithmetic<'t> * TreeArithmetic<'t>

// функция, реализующая подсчет арифмитического дерева
let rec count tree =
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
        | _ -> raise(InvalidOperationException "Такой операции не существует!")

// выводит бесконечную последовательность простых чисел
let primeNumbers =
    let rec test element list =
        match list with
        | [] -> true
        | h :: tail when element % h = 0 -> false
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
    
printfn "%d"  (Seq.item 5 primeNumbers)