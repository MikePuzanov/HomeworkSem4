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
    let mutable state = true
    let mutable countOfSteps = 0
    let virusNetwork() =
        state <- false
        countOfSteps <- countOfSteps + 1
        for i in 0 .. computers.Length - 1 do
            if (Array.get computers i).Virus then 
                for j in 0 .. computers.Length - 1 do
                    let comp = Array.get computers j
                    if (Array2D.get matrix i j = 1 && not comp.Virus) then
                        state <- true
                        let random = Random().NextDouble()
                        if comp.Probability >= random then
                           comp.Virus <- true
            
    let print =
        printfn "\n\nStep: %i" countOfSteps
        for i in 0 .. computers.Length - 1 do
            let computer = Array.get computers i
            printfn "\nName: %i, Operation system: %s, is infected: %b" (i + 1) computer.Name computer.Virus

    new (matrix: int[,], computers: Computer[]) = Network(matrix, computers)
    member val Computers = computers with get
    member this.DoOneStep() =
        virusNetwork()
    member this.DoAll() =
        while state  do
           virusNetwork()
           print