module Test.Tree

type Tree<'a> =
| Tree of 'a * Tree<'a> * Tree<'a>
| Tip of 'a

// функция, которая применяет функцию к каждому элементу дерева
let funcToTree tree func =
    let check value func list =
        if func value then value :: list else
            list
    let rec findElement tree func list =
        match tree with
        | Tip value -> check value func list
        | Tree (value, left, right) ->
            
            (check value func list) @ findElement left func [] @ findElement right func []
    findElement tree func []