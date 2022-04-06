module LocalNetwork.LocalNetwork

open System

type Computer(id, name, virus, probability : float) =
    member val Id = id
    member val Name = name
    member val Virus = virus with get, set
    member val Probability = probability

type Network (graph : int[,] , virus : int List) =
    let goVirus graph virus =
        let rec doVirus (graph : int[,]) virus : int List =                              // обход в ширину для каждого компа с вирусом
            match virus with                                     // обход в ширину для вируса с проверкой на вероятность      
                | h :: tail ->
                    let (neighbour : Computer List) = (graph |> Array.filter (fun [virus, x] -> x != 0 )) |> Array.toList
                    let neighbourIndex = graph |> Array.filter 
                    let rec tryVirus  (graph : Computer[,]) (neighbour : Computer List) (virus : int List)=
                        match neighbour with
                            | h :: tail ->
                                let random = new Random(float)
                                let probability = random.NextDouble
                                if (probability <= h.Probability) then
                                    h.Virus = true
                                    virus |> Array.append 
        
    member this.Graph = graph
    member this.Virus = virus