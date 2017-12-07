namespace Day7

open NUnit.Framework

module Day7App =
    open System

    let splitLine (line:string) = 
        let parts = line.Split([|"->"|], StringSplitOptions.None)
        let key = parts.[0].Split(' ').[0]
        if parts.Length > 1
        then
            let values = parts.[1].Split(',') |> Array.map(fun (x:string) -> x.Trim())
            (key, values)
        else
            (key, [|""|])

    let SolvePart1 (input:string) = 
        let structure = 
            input.Split('\n')
                |> Array.map (fun line ->  (splitLine line))

        let nodes = structure
                    |> Array.map snd
                    |> Array.concat
                    |> Set.ofArray

        let bases = structure
                    |> Array.map fst
                    |> Set.ofArray
                    
        fst ((bases - nodes) |> Set.toList)

module Day7Tests =

    let input = @"pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"

    [<Test>]
    let ``Given a test input then the correct output is returned`` () = 
        Assert.That(Day7App.SolvePart1 input, Is.EqualTo("tknk"))