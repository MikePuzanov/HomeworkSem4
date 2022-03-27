module PhoneBookFunc

open System.IO

type BookOfNumber =
    {Name : string; Phone : string}
    
let addPhone name phone book =
    {Name = name; Phone = phone} :: book
    
let rec findNameByPhone phone (book : BookOfNumber list) =
    let getFirst ({Name = a; Phone =  b} : BookOfNumber) = a
    book |> List.filter (fun {Name = _; Phone = x} -> x = phone) |> List.map(getFirst) 
    
let rec findPhoneByName name (book : BookOfNumber list) =
    let getSecond ({Name = a; Phone =  b} : BookOfNumber) = b
    book |> List.filter (fun {Name = x; Phone = _} -> x = name) |> List.map(getSecond)
    
let rec printAll book =
    let person (book : BookOfNumber list) = book.Head
    if (not(book |> List.isEmpty)) then
        printf "%s  %s\n" (person book).Name (person book).Phone
        printAll book.Tail
        
let writeToFile (path : string) book =
    use writer = new StreamWriter(path)
    book |> List.iter (fun {Name = x; Phone = y} -> (writer.WriteLine $"{x} {y}"))
    
let readFromFile path =
    let lines path = File.ReadAllLines(path) |> Array.toList
    let split (line : string) = line.Split ' '
    let getFirst (arr : string array) = arr |> Array.item 0
    let getSecond arr = arr |> Array.item 1
    if (File.Exists(path)) then 
        let rec read list book =
           match list with
           | h :: tail ->
               let line = split h
               let book = addPhone (getFirst line) (getSecond line) book 
               read tail book
           | [] -> book
        read (lines path) []
    else  raise (FileNotFoundException "Файл не найден.")
       
       
    