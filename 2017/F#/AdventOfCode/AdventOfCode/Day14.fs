namespace Day14

open NUnit.Framework
open System

module Day14App =                
    open Day10
    
    let SolvePart1 (input:string)  =             
        seq{0..127}
        |> Seq.map(fun x -> input + "-" + x.ToString())
        |> Seq.map(fun x-> Day10App.SolvePart2 [0..255] x)
        |> Seq.map(fun x -> x |> Seq.map(fun y -> System.Convert.ToString(Convert.ToInt32(y.ToString(), 16), 2).PadLeft(4, '0')) |> String.Concat )
        |> Seq.map(fun x -> x |> Seq.sumBy(fun c -> if c = '1' then 1 else 0 ))|> Seq.sum

    let SolvePart2 (input : string) =
        let data = 
            seq{0..127}
            |> Seq.map(fun x -> input + "-" + x.ToString())
            |> Seq.map(fun x-> Day10App.SolvePart2 [0..255] x)
            |> Seq.map(fun x -> x |> Seq.map(fun y -> System.Convert.ToString(Convert.ToInt32(y.ToString(), 16), 2).PadLeft(4, '0')) |> String.Concat )
            |> Seq.toArray
            |> Array.map (fun x -> x |> Seq.toArray |> Array.map(fun y -> int (y.ToString())))

        let rec remove i j =
            if i >= 0 && i < 128 && j >= 0 && j < 128 && data.[i].[j] = 1 then
                data.[i].[j] <- 0
                1 + remove (i + 1) j * remove (i - 1) j * remove i (j + 1) * remove i (j - 1)
            else
                0

        [ for i in 0 .. 127 do for j in 0 .. 127 -> remove i j ] |> List.sum

    

    
module Day14Tests =

    [<Test>]
    let ``Solve part 1 test `` () =
        Assert.That(Day14App.SolvePart1 "flqrgnkx", Is.EqualTo(8108))

    [<Test>]
    let ``Solve part 2 test `` () =
        Assert.That(Day14App.SolvePart2 "flqrgnkx", Is.EqualTo(1242))

    [<Test>]
    let ``Solve part 1 `` () =
        Assert.That(Day14App.SolvePart1 "hxtvlmkl", Is.EqualTo(8214))

    [<Test>]
    let ``Solve part 2`` () =
        Assert.That(Day14App.SolvePart2 "hxtvlmkl", Is.EqualTo(1093))

    