module Day6

open NUnit.Framework

module Day6App =

    type State = { Seen: string Set; Bank: Map<int,int> }

    let SolvePart1Exp = 
        let input =[0; 2; 7; 0]        

        let initial = { Seen = Set.empty; Bank=Map.ofSeq(List.indexed input) }

        let rec redistributeN i n bank =
            let nextI = (i + 1) % Map.count bank

            if n = 0
            then bank
            else
                let next = Map.add nextI (Map.find nextI bank + 1) bank
                redistributeN nextI (n-1) next

        let redistribute {Seen = seen; Bank = bank } =
            let maxIdx, max = Seq.maxBy snd (Map.toSeq bank)

            let emptied = Map.add maxIdx 0 bank
            let redistributed = redistributeN maxIdx max emptied

            let key = String.concat "," (List.map (string << snd) (Map.toList bank))

            if Set.contains key seen
            then None
            else Some (key, { Seen= Set.add key seen; Bank = redistributed } )

        (Seq.length (Seq.unfold redistribute initial))


    let SolvePart1 (input:string) = 

        let rec redistribute (memory: Map<int,int>) freeBlocks index = 
            if freeBlocks = 0 
            then memory
            else
                let key = index % Map.count memory
                let currentBlock = memory.[key]
                let newConfig = Map.add key (currentBlock + 1) memory
                redistribute newConfig (freeBlocks - 1) (key + 1)
        
        let values = input.ToCharArray() |> Array.map( string >> int)
        let max = Array.max values
        let indexes = values |> Array.filter (fun x -> x = max)
        let memoryBlockIndex = Array.min indexes
        let memoryBlock = values.[memoryBlockIndex]
        let result = Map.ofArray(Array.mapi (fun i v -> if i = memoryBlockIndex then i,0 else i,v ) values )
        ()

module Day6Tests = 
    
    let ``Given a test input then the correct output is returned`` () = 
        Assert.That(Day6App.SolvePart1 "0270", Is.EqualTo(5))
