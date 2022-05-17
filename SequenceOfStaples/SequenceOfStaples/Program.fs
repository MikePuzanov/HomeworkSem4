let checkStaples (exp : string) =
    let listOfChar = Seq.toList exp
    let reverseStaple staple =
        match staple with
        | '}' -> '{'
        | ']' -> '['
        | ')' -> '('
    let rec checkExpressionOnStaples (listOfExp : char List) (listOfStaples : char list) =
        match listOfExp with
        | h :: tail when h = '[' || h = '(' || h = '{' -> (checkExpressionOnStaples tail (h :: listOfStaples))
        | h :: tail when (h = ']' || h = ')' || h = '}') && listOfStaples.Length > 0 ->
            if reverseStaple h = listOfStaples.Head then
                checkExpressionOnStaples tail listOfStaples.Tail
            else false
        | [] -> listOfStaples.Length = 0
        | _ -> false
    checkExpressionOnStaples listOfChar []