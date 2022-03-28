// изначальная функция
let funcToList x l = List.map (fun y -> y * x) l
 
 // убрали список
let funcToList'1 x : int list -> int list = List.map (fun y -> y * x)

// убрали функцию
let funcToList'2 x : int list -> int list = List.map ((*)x)

// убрали переменную 
let funcToList'3() : int -> int list -> int list = List.map << (*) 

