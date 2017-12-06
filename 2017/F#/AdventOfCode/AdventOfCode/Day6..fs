module Day6


open NUnit.Framework

module Day6App =

    let SolvePart1 (input:string) = 

        let rec redistribute (memory: Map<int,int>) freeBlocks index = 
            if freeBlocks = 0 
            then memory
            else
                let key = index % Map.count memory
                let currentBlock = memory.[key]
                let newConfig = Map.add key (currentBlock + 1) memory
                redistribute newConfig (freeBlocks - 1) (key + 1)
        
        let values = input |> Array.map (string >> int)
        let max = Array.max values
        let indexes = values |> Array.filter (fun x -> x = max)
        let memoryBlockIndex = Array.min indexes
        let memoryBlock = values.[memoryBlockIndex]
        let result = Map.ofArray(Array.mapi (fun i v -> if i = memoryBlockIndex then i,0 else i,v ) values )

        let rec solve 
        ()

module Day6Tests = 
    
    let ``Given a test input then the correct output is returned`` () = 
        Assert.That(Day6App.SolvePart1 "0270", Is.EqualTo(5))
