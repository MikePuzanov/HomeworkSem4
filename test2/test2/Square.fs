module test2.Square

open System

let buildSquare n =
    let rec buildFirstAndLastLine i n =
        if i = n then
            printf"*\n"
        else
            printf"*"
            buildFirstAndLastLine (i + 1) n
    let rec buildOtherLine i n =
        if i = n then
            printf"*\n"
        else if i = 1 then
            printf"*"
            buildOtherLine (i + 1) n
        else
            printf" "
            buildOtherLine (i + 1) n
    let rec square i =
        if i < 1 then raise(ArgumentException())
        else if i = n then
            buildFirstAndLastLine 1 n
        else if i = 1 then
            buildFirstAndLastLine 1 n
            square (i + 1)
        else
            buildOtherLine 1 n
            square (i + 1)
 
    square 1

buildSquare 2