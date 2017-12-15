namespace Day15

open NUnit.Framework

module Day15App =

    let rec generator factor x = 
        seq{
            yield (x * factor) % 2147483647L
            yield!  generator factor (x * factor)
        }
    
    let compare (x, y) = 
        (x &&& 0xFFFFL) = (y &&& 0xFFFFL)

    let SolvePart1 a b runs =    
        let gena = generator 16807L a
        let genb = generator 48271L b

        gena 
        |> Seq.zip genb
        |> Seq.take runs
        |> Seq.filter compare
        |> Seq.length

    

    

    
module Day15Tests =

    [<Test>]
    let ``Solve part quick 1 test `` () =
        Assert.That(Day15App.SolvePart1 65L 8921L 5, Is.EqualTo(1))

    [<Test>]
    let ``Solve part 1 test `` () =
        Assert.That(Day15App.SolvePart1 65L 8921L 40000000, Is.EqualTo(588))

    [<Test>]
    let ``Solve part 1 `` () =
        Assert.That(Day15App.SolvePart1 591L 393L 40000000, Is.EqualTo(588))

    