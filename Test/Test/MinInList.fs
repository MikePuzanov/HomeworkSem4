module Test.MinInList

// находит минимальный элемент списка
let minInList list =
    (list |> List.map (fun x -> x * -1) |> List.max ) * -1