open System
open System.Collections.Generic
open System.Security.Principal

type Term =
    | Variable of int
    | Application of Term * Term
    | LambdaAbstraction of int * Term

let getVar term =
    let rec execute term list =
            match term with
            | Variable(v) -> v :: list
            | Application(term1, term2) -> (execute term1 list) @ (execute term2 list)
            | LambdaAbstraction(v, term1) -> execute term1 (List.filter(fun x -> x <> v) list)
    execute term []
let checkFreeVar var term = List.exists(fun x -> x = var) (getVar term)
       
let rec substitution var termOut termIn = 
    match termOut with
    | Variable(v) -> if v = var then termOut else Variable(var)
    | Application(termApp1, termApp2) -> Application(substitution var termApp1 termIn, substitution var termApp2 termIn)
    | LambdaAbstraction(var1, term1) ->
        match var1 with
        | v when var = v -> LambdaAbstraction(var1, term1)
        | v when (checkFreeVar var1 termIn) || (checkFreeVar var term1) -> LambdaAbstraction(var1, (substitution var term1 termIn))
        | _ ->
            //
            //
            //
            

let rec betaReduction term =
    match term with
    | Variable(v) -> Variable(v)
    | Application(LambdaAbstraction(v, term), termForSubstitution) -> betaReduction (substitution v term termForSubstitution)
    | Application(term1, term2) -> Application(betaReduction term1, betaReduction term2)
    | LambdaAbstraction(v, term) -> LambdaAbstraction(v, betaReduction term) 
