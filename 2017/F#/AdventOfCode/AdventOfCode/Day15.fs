namespace Day15

open NUnit.Framework

module Day15App =

    let rec generator factor x = 
        let value = (x * factor) % 2147483647L
        seq{
            yield value
            yield!  generator factor value
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
    
    let SolvePart2 a b runs = 
        let gena = generator 16807L a |> Seq.filter(fun x -> x % 4L = 0L)
        let genb = generator 48271L b |> Seq.filter(fun x -> x % 8L = 0L)

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
        Assert.That(Day15App.SolvePart1 65L 8921L 40_000_000, Is.EqualTo(588))

    [<Test>]
    let ``Solve part 1 `` () =
        Assert.That(Day15App.SolvePart1 591L 393L 40_000_000, Is.EqualTo(619))

    [<Test>]
    let ``Solve part 2 `` () =
        Assert.That(Day15App.SolvePart2 591L 393L 5_000_000, Is.EqualTo(619))

    