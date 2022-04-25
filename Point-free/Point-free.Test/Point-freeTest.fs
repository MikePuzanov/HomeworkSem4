module Point_free.Test

open NUnit.Framework
open FsCheck
open PointFree
    
[<Test>]
let checkEqualTest () =
    let check x list =
        funcToList x list = funcToList3 x list
        
    Check.QuickThrowOnFailure check