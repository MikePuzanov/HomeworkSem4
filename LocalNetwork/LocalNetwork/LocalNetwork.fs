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
type public Network(graph : int [,], computers : List<Computer>, virus : List<int>) =
    let check =
        virus.Count = computers.Count
     
    let random =
         let randomDouble = new Random()
         randomDouble.NextDouble()
        
    let virusNetwork (graph : int [,]) (computers : List<Computer>) (virus : List<int>) =
        for i in 0 .. computers.Count - 1 do
            if (computers.Item i).Virus then 
                for j in 0 .. computers.Count - 1 do
                    if (graph[i, j] = 1 && not (computers.Item j).Virus) then
                        if (computers.Item j).Probability <= random then
                           (computers.Item j).Virus <- true            
            
    let print() =
        for i in 0 .. computers.Count - 1 do
            let computer = computers.Item i
            printfn "Name: %i, OS: %s, is infected: %b" i computer.Name computer.Virus
        printfn "\n"
            
    member val Graph = graph with get, set
    member val Computers = computers with get
    member val Virus = virus with get,set
    
     member this.DoAll() =
         while not check do
            virusNetwork graph computers virus
            print()