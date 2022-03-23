//Реализовать функцию, которая по произвольной строке проверяет корректность скобочной
//последовательности в этой строке. Скобки бывают трёх видов.

let checkStaples (exp : string) =
    let arrayChar = Seq.toList exp
    let rec check (listOfExp : char List) (array : char array) =
        match listOfExp with
        | h :: listOfExp when h = '[' || h = '(' || h = '{' -> (check listOfExp.Tail (array |> Array.insertAt 0 h)) 
        | h :: listOfExp when h = ']' -> if '[' = (array |> Array.item 0) then (check listOfExp.Tail (array |> Array.removeAt 0)) else false
        | h :: listOfExp when h = ')'-> if '(' = (array |> Array.item 0) then (check listOfExp.Tail (array |> Array.removeAt 0)) else false
        | h :: listOfExp when h = '}' -> if '{' = (array |> Array.item 0) then (check listOfExp.Tail (array |> Array.removeAt 0)) else false
        | [] -> array.Length = 0
        | _ -> false
    check arrayChar [||]

if (checkStaples "(()){}") then printf "true" else printf "false"  