module LocalNetwork.LocalNetwork

open System
open System.Collections.Generic

// виды систем
type OperationSystem =
    | Windows
    | Linux
    | MacOS

// тип компьютера
type Computer(id : int,name : OperationSystem, virus : bool, probability : float) =
    member val Id = id
    member val Name =
        match name with
        | Windows -> "Windows"
        | Linux -> "Linux"
        | MacOS -> "MacOS"
    member val Virus = virus with get, set
    member val Probability = probability
    
// локальная сеть
type public Network(matrix : int [,], computers : Computer[], virus : int list) =
    let matrix = matrix
    let computers = computers
    let mutable IsAnyInfected = true
    let mutable countOfSteps = 0
    let virusNetwork() =
        IsAnyInfected <- false
        countOfSteps <- countOfSteps + 1 
        computers |> Array.iteri(fun i computer ->
            if computer.Virus then
                computers |> Array.iteri(fun j computer ->
                    let comp = Array.get computers j
                    if (Array2D.get matrix i j = 1 && not comp.Virus) then
                        IsAnyInfected <- true
                        if comp.Probability >= Random().NextDouble() then
                           comp.Virus <- true))
            
    let print =
         printfn "\n\nStep: %i" countOfSteps
         computers |> Array.iteri(fun i computer -> printfn $"\nName: %i{i + 1}, Operation system: %s{computer.Name}, is infected: %b{computer.Virus}")

    member val Computers = computers with get
    member this.DoOneStep() =
        virusNetwork()
    member this.DoAll() =
        while IsAnyInfected  do
           virusNetwork()
           print