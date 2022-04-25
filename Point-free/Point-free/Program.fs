module PointFree
// изначальная функция
let funcToList x l = List.map (fun y -> y * x) l
 
 // убрали список
let funcToList1 x : int list -> int list = List.map (fun y -> y * x)

// убрали функцию
let funcToList2 x : int list -> int list = List.map ((*)x)

// убрали переменную 
let funcToList3 = (*) >> List.map
