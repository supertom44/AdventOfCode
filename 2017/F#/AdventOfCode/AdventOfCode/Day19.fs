namespace Day19

open NUnit.Framework

module Day19App =
    
    type Direction = 
    | Up
    | Down
    | Left
    | Right

    type State = { Letters : char array; CurrentPosition: int * int; Direction: Direction }   

    let SolvePart1 (input:string) = 
              
        let maze = 
            input.Split('\n')
            |> Array.map(fun x -> Array.map(fun y -> y) (x.ToCharArray()))

        let getMoveFrom (x , y) = 
            maze.[x].[y]
            
        let getMove state =
            getMoveFrom(fst state.CurrentPosition, snd state.CurrentPosition)        

        let start = Array.findIndex(fun x -> x = '|') maze.[0]

        let moveDown (x, y)  = 
            (x , y + 1) 

        let moveUp (x, y)  = 
            (x , y - 1) 

        let moveLeft (x, y)  = 
            (x - 1 , y) 

        let moveRight (x, y)  = 
            (x + 1, y)

        let findNextMove position dir = 
            let nextPosition = 
                match dir with
                | Up -> moveUp position
                | Down -> moveDown position
                | Left -> moveLeft position
                | Right -> moveRight position

            let nextPotentialMove = getMoveFrom nextPosition

            match dir with
            | Up | Down when nextPotentialMove = '|' -> nextPotentialMove
            | Up when nextPotentialMove = '-' -> moveUp nextPotentialMove
            | Down when nextPotentialMove = '-' -> movedown nextPotentialMove
            | Left | Right when nextPotentialMove = '-' -> nextPotentialMove
            | Left when nextPotentialMove = '|' -> moveLeft nextPotentialMove
            | Right when nextPotentialMove = '|' -> moveRight nextPotentialMove
            | Up | Down -> 

            
        let move state = 
            
            let nextMove = getMove state

            match nextMove with
            | '|' when state.Direction = Up -> { state with CurrentPosition = moveUp state.CurrentPosition }
            | '|' when state.Direction = Down -> { state with CurrentPosition = moveDown state.CurrentPosition }
            | '-' when state.Direction = Right -> { state with CurrentPosition = moveRight state.CurrentPosition }
            | '-' when state.Direction = Left -> { state with CurrentPosition = moveLeft state.CurrentPosition }
            | '+' when state.Direction = Left -> { state with CurrentPosition = moveLeft state.CurrentPosition }

        ()       

   

    
module Day19Tests =

    let input = @"     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 
"

    [<Test>]
    let ``Solve part 1 test `` () =
        Assert.That(Day19App.SolvePart1 input, Is.EqualTo("ABCDEF"))
