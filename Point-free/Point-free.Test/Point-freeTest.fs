module Point_free.Test

open NUnit.Framework
open FsCheck
open Program
    
[<Test>]
let checkEqualTest () =
    let check x l =
        funcToList x l = funcToList'3 () x l
        
    
    Check.QuickThrowOnFailure check 