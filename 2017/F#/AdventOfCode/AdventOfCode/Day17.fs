namespace Day17

open NUnit.Framework
open System

module Day17App =

    let SolvePart1 steps search iterations =

        let insert value index (array : int array) = 
            Array.concat (seq[array.[0 .. index - 1]; [|value|]; array.[index .. Array.length array - 1]])

        let buffer = [|0|] 

        let rec spinlock currentPosition count buffer = 
            let index = ((currentPosition + steps) %  (Array.length buffer)) + 1
            if count = iterations + 1 
            then buffer
            else spinlock index (count + 1) (insert count index buffer)

        let result = spinlock 0 1 buffer 
        let index = 
            result
            |> Array.findIndex(fun x -> x = search)

        Array.item (index + 1) result

    let SolvePart2 steps =
        let rec solve index count resultAfterZero =
            let newIndex = ((index + steps) % count) + 1
            if count = 50000001
            then resultAfterZero
            else solve newIndex (count + 1) (if newIndex = 1 then count else resultAfterZero) 

        solve 0 1 0
    
    


    
module Day17Tests =

    [<Test>]
    let ``Solve part 1 test `` () =
        Assert.That(Day17App.SolvePart1 3 2017 2017,  Is.EqualTo(638))

    [<Test>]
    let ``Solve part 1 `` () =
        Assert.That(Day17App.SolvePart1 348 2017 2017, Is.EqualTo(417))  
        
    [<Test>]
    let ``Solve part 2`` () =
        Assert.That(Day17App.SolvePart2 348, Is.EqualTo(34334221)) 
        

