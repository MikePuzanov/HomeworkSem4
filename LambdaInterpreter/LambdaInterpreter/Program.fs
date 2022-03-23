open System
open System.Collections.Generic
open System.Security.Principal

type Term =
    | Variable of string
    | Application of Term * Term
    | LambdaAbstraction of string * Term

let getVar term =
    let rec execute term list =
            match term with
            | Variable(v) -> v :: list
            | Application(term1, term2) -> (execute term1 list) @ (execute term2 list)
            | LambdaAbstraction(v, term1) -> execute term1 (List.filter(fun x -> x <> v) list)
    execute term []
let checkFreeVar var term = term |> getVar |> List.exists(fun x -> x = var)
       
let rec substitution var termOut termIn = 
    match termOut with
    | Variable(v) -> if v = var then termIn else Variable(var)
    | Application(termApp1, termApp2) -> Application(substitution var termApp1 termIn, substitution var termApp2 termIn)
    | LambdaAbstraction(varInAbs, termInAbs) ->
        match varInAbs with
        | v when var = v -> LambdaAbstraction(varInAbs, termInAbs)
        | v when (checkFreeVar varInAbs termIn) || (checkFreeVar var termInAbs) -> LambdaAbstraction(varInAbs, (substitution var termInAbs termIn))
        | _ ->
            let newVar = (((getVar termInAbs) @ (getVar termIn)) |> List.toArray |> Array.max) + "1"
            let newTerm = newVar |> Variable
            let termWithNewVar = substitution var termInAbs newTerm 
            LambdaAbstraction(newVar, substitution var termWithNewVar termIn )
            

let rec betaReduction term =
    match term with
    | Variable(v) -> Variable(v)
    | Application(LambdaAbstraction(v, term), termForSubstitution) -> betaReduction (substitution v term termForSubstitution)
    | Application(term1, term2) -> Application(betaReduction term1, betaReduction term2)
    | LambdaAbstraction(v, term) -> LambdaAbstraction(v, betaReduction term) 
