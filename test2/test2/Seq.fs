module SeqTest
let seqMake () =
    let rec seqCheck element i = i <> element
    let rec sequence element list i =
        seq {  
            if seqCheck element i then
                yield element
                yield! sequence element (element :: list) (i + 1)
            else
                yield! sequence (element + 1) list 0
            
        }
    sequence 1 [] 0