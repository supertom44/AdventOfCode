namespace Day3

open NUnit.Framework

module Day3App =

    type Position = { Horizontal: int; Vertical: int }

    type Direction = Left | Right | Up | Down

    type State = { At: Position; Facing: Direction } 
       
    let turn s = 
        { s with
            Facing = 
                match s.Facing with
                | Up -> Left
                | Left -> Down
                | Down -> Right
                | Right -> Up}

    let forward s = 
        match s.Facing with
        | Up -> { s with At = { s.At with Vertical = s.At.Vertical + 1} }
        | Left -> { s with At = { s.At with Horizontal = s.At.Horizontal - 1} }
        | Down -> { s with At = { s.At with Vertical = s.At.Vertical - 1} }
        | Right -> { s with At = { s.At with Horizontal = s.At.Horizontal + 1} }

    let (|TurningCorner|NonTurningCorner|) { Horizontal=h; Vertical=v; } =
        match h, v with
        | 0,0 -> NonTurningCorner
        | _ when h = -v && v < 0 -> NonTurningCorner
        | _ when -h = v - 1 && v < 0 -> TurningCorner
        | _ when abs h = abs v -> TurningCorner
        | _ -> NonTurningCorner

    let step s = 
        match s.At with
        | TurningCorner -> s |> turn |> forward
        | NonTurningCorner -> s |> forward

    let initial = { At = { Horizontal = 0; Vertical = 0;}; Facing = Right }

    let neighbouringCells { Horizontal =h; Vertical = v} =
        [
            { Horizontal = h + 1; Vertical = v + 1}
            { Horizontal = h + 1; Vertical = v }
            { Horizontal = h + 1; Vertical = v - 1}

            { Horizontal = h; Vertical = v + 1}
            { Horizontal = h; Vertical = v - 1}

            { Horizontal = h - 1; Vertical = v + 1}
            { Horizontal = h - 1; Vertical = v }
            { Horizontal = h - 1; Vertical = v - 1}
        ]

    let valueOr other = function 
        | Some x -> x
        | _ -> other

    let firstSumGreaterThan n =
        let rec aux sums s = 
            let sum = s.At 
                    |> neighbouringCells 
                    |> Seq.sumBy (fun pos -> Map.tryFind pos sums |> valueOr 0)
                        
            if sum > n
            then sum
            else aux (Map.add s.At sum sums) (step s)

        if 1 > n
        then 1 
        else aux (Map.ofSeq [initial.At, 1]) (turn (forward initial))

    let elementAt n =
        let rec aux n s = 
            match n with
            | 1 -> s
            | _ -> aux(n - 1) (step s)

        match n with 
        | 1 -> initial
        | _ -> aux (n - 1) (turn (forward initial))
    
    let SolvePart1 input = 
        let result = elementAt input
        (abs result.At.Horizontal + abs result.At.Vertical)

    let SolvePart2 input = 
        firstSumGreaterThan input      
        
    

module Day3Tests = 

    [<TestCase(1,0)>]
    [<TestCase(12,3)>]
    [<TestCase(23,2)>]
    [<TestCase(1024,31)>]
    [<TestCase(347991,480)>]
    let ``then the correct number of steps is taken`` input expected =
        Assert.That(Day3App.SolvePart1 input, Is.EqualTo expected)

    [<Test>]
    let ``Solve part 2`` () = 
        Assert.That(Day3App.SolvePart2 347991, Is.EqualTo 349975)
