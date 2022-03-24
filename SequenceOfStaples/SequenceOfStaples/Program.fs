let checkStaples (exp : string) =
    let ListOfChar = Seq.toList exp
    let rec check (listOfExp : char List) (listOfStaples : char list) =
        let charFunc x = if x = listOfStaples.Head then (check listOfExp.Tail listOfStaples.Tail) else false
        match listOfExp with
        | h :: tail when h = '[' || h = '(' || h = '{' -> (check tail (h :: listOfStaples)) 
        | h :: listOfExp when h = ']' && listOfStaples.Length > 0 -> charFunc '['
        | h :: listOfExp when h = ')' && listOfStaples.Length > 0 -> charFunc '('
        | h :: listOfExp when h = '}' && listOfStaples.Length > 0 -> charFunc '{'
        | [] -> listOfStaples.Length = 0
        | _ -> false
    check ListOfChar []