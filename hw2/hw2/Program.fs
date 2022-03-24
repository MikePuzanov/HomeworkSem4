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
| Tip of 'a

// функция, которая применяет функцию к каждому элементу дерева
let rec funcToTree tree func =
    match tree with
    | Tip (value) -> Tip(func value)
    | Tree (value, left, right) -> Tree(func value, funcToTree left func, funcToTree right func)

// все операции
type Operation =
    | Summation
    | Subtracting
    | Multiplication
    | Division
  
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
        | Summation -> left + right
        | Subtracting -> left - right
        | Multiplication ->  left * right
        | Division -> left / right

// выводит бесконечную последовательность простых чисел
let primeNumbers () =
    let rec testOnPrime element list =
        match list with
        | [] -> true
        | h :: tail ->
            if element % h = 0 then false else testOnPrime element tail
    let rec sequence element list =
        seq {
            if testOnPrime element list then
                yield element
                yield! sequence (element + 1) (element :: list)
            else
                yield! sequence (element + 1) list
        }
    sequence 2 []