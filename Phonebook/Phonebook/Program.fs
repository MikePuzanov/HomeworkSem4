open System
open PhoneBookFunc

let rule () =
    printfn
        "%s"
        "0 - выйти;\n\
        1 - добавить запись (имя и телефон);\n\
        2 - найти телефон по имени;\n\
        3 - найти имя по телефону;\n\
        4 - вывести всё текущее содержимое базы;\n\
        5 - сохранить текущие данные в файл;\n\
        6 - считать данные из файла."

let getCommand () =
    printf"%s" "\nВведите команду - "
    Console.ReadLine()

let getName () =
    printf "%s" "Введите имя - "
    Console.ReadLine()
    
let getPhone () =
    printf "%s" "Введите номер - "
    Console.ReadLine()
    
let getPath () =
    printf "%s" "Введите путь - "
    Console.ReadLine()
    
let tryAgain () =
    printf "%s" "Попробуйте еще раз."  
  
let rec program book =
    let command = getCommand ()
    match command with
    | "0" -> ()  
    | "1" -> addPhone (getName ()) (getPhone ()) book |> program
    | "2" ->
        (findPhoneByName (getName()) book) |> List.iter(fun x -> printf "%s" x)
        program book
    | "3" ->
        (findNameByPhone (getPhone ()) book) |> List.iter(fun x -> printf "%s" x)
        program book
    | "4" ->
        printAll book
        program book
    | "5" ->
        writeToFile (getPath ()) book
        program book
    | "6" ->
        program (readFromFile (getPath()))
    | _ ->
        tryAgain ()
        program book
rule ()
program []