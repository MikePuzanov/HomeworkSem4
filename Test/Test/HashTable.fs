module Test.HashTable

open System
open Test

type public MyHashTable (func : Func<string, int>) =
    let func element = func 
    let hash (element : string) = element |> func
    
  
        