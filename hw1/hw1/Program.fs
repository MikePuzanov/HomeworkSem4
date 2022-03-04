open System

let factorial x =
    if x < 0 then raise (ArgumentException"n должно быть неотрицательным")
    if x = 0 then 1 else
        let rec factorialCount x i acc =
            if x = i then i * acc
            else factorialCount x (i + 1) (acc * i)
        factorialCount x 1 1 
    
let fibonacci n =
    if n < 0 then raise (ArgumentException"n должно быть неотрицательным") else
        let rec fibonacciCount number acc1 acc2 =
            if number = n then acc1
            else
                fibonacciCount (number + 1) acc2 (acc1 + acc2)
        fibonacciCount 0 0 1
        
let reverseList list =
    if List.length list = 0 then [] else
        let rec reverseListMaker list listNew  =
            if list = [] then listNew
            else
                reverseListMaker (List.tail list) ((List.head list) :: listNew)
        reverseListMaker list []
        
let makeList n m =
    let rec countPower n m list =
        if n = m then list
        else
            countPower n (m - 1) (pown 2 m :: list)
    countPower n (n + m) []

let findFirst list number =
    if list = [] then raise(ArgumentException("")) else
        let rec findElement list number i =
            if list = [] then raise(ArgumentException("Нет такого элемента в списке"))
            if List.head list = number then i
            else
                findElement (List.tail list) number (i + 1)
        findElement list number 0