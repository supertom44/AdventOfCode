namespace Day14

open NUnit.Framework
open System

module Day14App =                
    open Day10
    
    let SolvePart1 (input:string)  = 
            let x = 
                seq{0..127}
                |> Seq.map(fun x -> input + "-" + x.ToString())
                |> Seq.map(fun x-> Day10App.SolvePart2 [0..255] x)
            
            let y = 
                x
                |> Seq.map(fun x -> x |> Seq.map(fun y -> System.Convert.ToString(Convert.ToInt32(y.ToString(), 16), 2).PadLeft(4, '0')) |> String.Concat )

            let rowTotals = y|> Seq.map(fun x -> x |> Seq.sumBy(fun c -> if c = '1' then 1 else 0 ))
            rowTotals|> Seq.sum

    

    
module Day14Tests =

    [<Test>]
    let ``Solve part 1 test `` () =
        Assert.That(Day14App.SolvePart1 "flqrgnkx", Is.EqualTo(8108))

    [<Test>]
    let ``Solve part 1 `` () =
        Assert.That(Day14App.SolvePart1 "hxtvlmkl", Is.EqualTo(8214))

    