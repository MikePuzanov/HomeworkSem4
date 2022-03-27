module Phonebook.Test

open System.IO
open NUnit.Framework
open FsUnit
open PhoneBookFunc

let book = [{Name = "Ulu"; Phone = "123"}
            {Name = "Ele"; Phone = "456"}
            {Name = "Yly"; Phone = "789"}]

[<Test>]
let addTest () =
    let book = addPhone "Oleg" "12345" book
    
    book |> should contain {Name = "Oleg"; Phone = "12345"}
    
[<Test>]
let findNameTest () =
    let name = findNameByPhone "456" book
    let name1 = findNameByPhone "1" book
    
    name |> should equal ["Ele"]
    name1 |> should be Empty
    
[<Test>]
let findPhoneTest () =
    let phone = findPhoneByName "Ele" book
    let phone1 = findPhoneByName "Ele1" book
    
    phone |> should equal ["456"]
    phone1 |> should be Empty
    
[<Test>]
let fileTest () =
    writeToFile "Test.txt" book
    
    readFromFile "Test.txt" |> should equal [{Name = "Yly"; Phone = "789"}; {Name = "Ele"; Phone = "456"}; {Name = "Ulu"; Phone = "123"}]
    (*readFromFile "123.txt" |> should raise (FileNotFoundException "Файл не найден.")*)
    (fun () -> readFromFile "123.txt" |> ignore) |> should throw typeof<System.IO.FileNotFoundException>