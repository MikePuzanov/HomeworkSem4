module LocalNetwotk.Test

open System
open LocalNetwork.LocalNetwork
open NUnit.Framework
open FsUnit
open LocalNetwork

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestWithProbability1 () =
    let comp1 = Computer(0, OperationSystem.Linux, false, 1.0)
    let comp2 = Computer(0, OperationSystem.Windows, true, 1.0)
    
    let comp3 = Computer(0, OperationSystem.MacOS, false, 1.0)
    let matrix = [ [1; 1; 0]; [1; 1; 1]; [0; 1; 0] ]
    let network = Network(array2D matrix, [| comp1; comp2; comp3|], [1])
    network.DoAll()
    for comp in network.Computers do
        comp.Virus |> should equal true
        
[<Test>]
let TestWithProbability0 () =
    let comp1 = Computer(0, OperationSystem.Linux, false, 0.0)
    let comp2 = Computer(0, OperationSystem.Windows, false, 0.0)
    
    let comp3 = Computer(0, OperationSystem.MacOS, false, 0.0)
    let matrix = [ [1; 1; 0]; [1; 1; 1]; [0; 1; 0] ]
    let network = Network(array2D matrix, [| comp1; comp2; comp3|], [1])
    network.DoAll()
    for comp in network.Computers do
        comp.Virus |> should equal false
        
[<Test>]

let TestWithDifferentProbability () =
    let comp1 = Computer(0, OperationSystem.Linux, true, 0.3)
    let comp2 = Computer(0, OperationSystem.Windows, false, 0.3)
    
    let comp3 = Computer(0, OperationSystem.MacOS, false, 0.7)
    let matrix = [ [1; 1; 0]; [1; 1; 1]; [0; 1; 0] ]
    let network = Network(array2D matrix, [| comp1; comp2; comp3|], [1])
    network.DoAll()
    for comp in network.Computers do
        comp.Virus |> should equal true
        
[<Test>]
let TestWithWhenNotAllBecomeVirus () =
    let comp1 = Computer(0, OperationSystem.Linux, true, 0.5)
    let comp2 = Computer(0, OperationSystem.Windows, false, 0.8)
    
    let comp3 = Computer(0, OperationSystem.MacOS, false, 0.7)
    let matrix = [ [1; 1; 0]; [1; 1; 0]; [0; 0; 0] ]
    let network = Network(array2D matrix, [| comp1; comp2; comp3|], [1])
    network.DoAll()
    comp1.Virus |> should equal true
    comp2.Virus |> should equal true
    comp3.Virus |> should equal false