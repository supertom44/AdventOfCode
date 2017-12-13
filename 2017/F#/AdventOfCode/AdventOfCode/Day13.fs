namespace Day13

open NUnit.Framework
open System

module Day13App =

    let parseInput (input:string) = 
        input.Split('\n')
        |> Seq.map(fun x -> x.Split([|": "|], StringSplitOptions.None))
        |> Seq.map(fun x ->(int (Array.head x), int (Array.last x)))
                
    
    let SolvePart1 (input:string) offset =         
        let data = parseInput input
        let x = data |> Seq.choose(fun (i, x) -> if (i + offset) % ((x - 1) * 2) = 0 then Some (i,x) else None)
        x |> Seq.sumBy(fun (i,d) -> i * d)


    let SolvePart2 (input:string) = 
        Seq.initInfinite (id)
        |> Seq.takeWhile(fun x -> (SolvePart1 input x) > 0)
        |> Seq.last


    

    
module Day13Tests =

    let input = @"0: 3
1: 2
4: 4
6: 4"

    [<Test>]
    let ``Solve part 1 test `` () =
        Assert.That(Day13App.SolvePart1 input 0, Is.EqualTo(24))

    [<Test>]
    let ``Solve part 2 test `` () =
        Assert.That(Day13App.SolvePart2 input, Is.EqualTo(10))


    let problem = @"0: 5
1: 2
2: 3
4: 4
6: 6
8: 4
10: 6
12: 10
14: 6
16: 8
18: 6
20: 9
22: 8
24: 8
26: 8
28: 12
30: 12
32: 8
34: 8
36: 12
38: 14
40: 12
42: 10
44: 14
46: 12
48: 12
50: 24
52: 14
54: 12
56: 12
58: 14
60: 12
62: 14
64: 12
66: 14
68: 14
72: 14
74: 14
80: 14
82: 14
86: 14
90: 18
92: 17"

    [<Test>]
    let ``Solve part 1 `` () =
        Assert.That(Day13App.SolvePart1 problem, Is.EqualTo(788))