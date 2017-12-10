namespace Day10

open NUnit.Framework

module Day10App =
   
    type State = { CurrentIndex : int; SkipSize : int }
    
    let init = { CurrentIndex = 0; SkipSize = 0}

    let split length (xs: seq<'T>) =
        let rec loop xs =
            [
                yield Seq.truncate length xs |> Seq.toList
                match Seq.length xs <= length with
                | false -> yield! loop (Seq.skip length xs)
                | true -> ()
            ]
        loop xs

    let adjust (memory: Map<int,int>) length state = 
        let count = Map.count memory     
        let endIndex = state.CurrentIndex + length - 1
        let indexes = [state.CurrentIndex .. endIndex] |> List.map(fun x -> (x % count))
        let subList = 
            indexes
            |> List.map(fun x -> memory.Item x)
            
        let reversed = List.rev subList
        let resultMemory = 
            reversed
            |> List.zip indexes
            |> Seq.fold(fun (mem : Map<int,int>) (i, v) -> Map.add i v mem ) memory

        let newIndex = state.CurrentIndex + ((length + state.SkipSize) % count)
        (resultMemory, { state with CurrentIndex = newIndex; SkipSize = state.SkipSize + 1 })

    let answer (input:seq<int>) memory startingState = 
        input
        |> Seq.fold(fun (mem,state) x -> adjust mem x state) (memory, startingState)

    let convertToHex (number:int) = 
        System.String.Format("{0:X2}",number).ToLower()

    let convertToAscii (input: char) = 
            System.Text.Encoding.ASCII.GetBytes(string input).[0].ToString()

    let SolvePart1 (rawMemory:int list) (input:string) =
        let memory = Map.ofList(List.mapi(fun i v -> i, v) rawMemory)       
        
        let result = answer (input.Split(',') |> Seq.map (string >> int)) memory init

        (fst(result).Item 0) * (fst(result).Item 1)

    let SolvePart2 (rawMemory:int list) (input:string) =

        let ascii = 
            input
            |> Seq.map convertToAscii 
            |> Seq.map (fun x ->  int x)

        let suffix = seq { yield 17; yield 31; yield 73; yield 47; yield 23}

        let formattedInput = Seq.append ascii suffix
        let memory = Map.ofList(List.mapi(fun i v -> i, v) rawMemory) 

        let sparseHash = 
            seq {0..63}
            |> Seq.fold(fun (mem, state) _ -> answer formattedInput mem state)  (memory , init)
            |> fst
            
        let desnseHash = 
            sparseHash
            |> Map.toSeq
            |> Seq.map snd
            |> split 16
            |> Seq.map(fun x -> x |> Seq.fold(fun (acc:int) (y:int) -> acc ^^^ y ) 0)

        let knotHash =
            desnseHash
            |> Seq.map convertToHex    
            |> String.concat ""

        knotHash

    
module Day10Tests =

    [<Test>]
    let ``Given a test input then the correct output is returned`` () = 
        Assert.That(Day10App.SolvePart1 [0..4] "3,4,1,5", Is.EqualTo(12))

    [<Test>]
    let ``Given a input then the correct output is returned`` () = 
        Assert.That(Day10App.SolvePart1 [0..255] "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24", Is.EqualTo(4480))

    [<TestCase("1,2,3","3efbe78a8d82f29979031a4aa0b16a9d")>]
    [<TestCase("1,2,4","63960835bcdc130f0b66d7ff4f6a5a8e")>]
    let ``Solve part 2 test `` input expected =
        Assert.That(Day10App.SolvePart2 [0..255] input, Is.EqualTo(expected))

    [<Test>]
    let ``Solve part 2`` () =
        Assert.That(Day10App.SolvePart2 [0..255] "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24", Is.EqualTo("c500ffe015c83b60fad2e4b7d59dabc4"))

    