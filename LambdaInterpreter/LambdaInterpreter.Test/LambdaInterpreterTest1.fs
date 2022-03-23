module LambdaInterpreter.Test

open NUnit.Framework
open FsUnit
open Program

[<Test>]
let TestAppWithVarTerm () =
    let term = Term.Application(LambdaAbstraction("y", Variable("y")), Variable("z"))
    
    let result = betaReduction term
    
    result |> should equal (Variable("z"))
    
[<Test>]
let TestAppWithTwoTerm () =
    let term = Term.Application(LambdaAbstraction("y", Variable("y")), LambdaAbstraction("z", Variable("z")))
    
    let result = term |> betaReduction
    
    result |> should equal (LambdaAbstraction("z", Variable("z")))
    
[<Test>]
let TestLambdaAbs () =
    let term = Term.LambdaAbstraction("x", LambdaAbstraction("z", Variable("z")))
    
    let result = term |> betaReduction
    
    result |> should equal (LambdaAbstraction("x", LambdaAbstraction("z", Variable("z"))))