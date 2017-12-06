open System;

let input = "R3, L5, R1, R2, L5, R2, R3, L2, L5, R5, L4, L3, R5, L1, R3, R4, R1, L3, R3, L2, L5, L2, R4, R5, R5, L4, L3, L3, R4, R4, R5, L5, L3, R2, R2, L3, L4, L5, R1, R3, L3, R2, L3, R5, L194, L2, L5, R2, R1, R1, L1, L5, L4, R4, R2, R2, L4, L1, R2, R53, R3, L5, R72, R2, L5, R3, L4, R187, L4, L5, L2, R1, R3, R5, L4, L4, R2, R5, L5, L4, L3, R5, L2, R1, R1, R4, L1, R2, L3, R5, L4, R2, L3, R1, L4, R4, L1, L2, R3, L1, L1, R4, R3, L4, R2, R5, L2, L3, L3, L1, R3, R5, R2, R3, R1, R2, L1, L4, L5, L2, R4, R5, L2, R4, R4, L3, R2, R1, L4, R3, L3, L4, L3, L1, R3, L2, R2, L4, L4, L5, R3, R5, R3, L2, R5, L2, L1, L5, L1, R2, R4, L5, R2, L4, L5, L4, L5, L2, L5, L4, R5, R3, R2, R2, L3, R3, L2, L5"

type direction= Right | Left
type move = direction * int
type orientation = North | East | South | West

type thing = {
    Position : int * int
    Orientation : orientation
    Visits : (int * int) list
}

let transform move (thingy : thing) =
    match thingy.Orientation with 
    | North -> 
        match fst move with 
        | Right -> 
                    let newPosition = (fst thingy.Position + snd move, snd thingy.Position)
                    let paths = [ for x in [(fst thingy.Position) + 1 .. (fst newPosition)] -> (x, snd thingy.Position)]
                    { Position = newPosition; Orientation = East; Visits = List.append thingy.Visits paths }
        | Left -> 
                    let newPosition = (fst thingy.Position - snd move, snd thingy.Position)
                    let paths = [ for x in [(fst newPosition) .. (fst thingy.Position) - 1 ] -> (x, snd thingy.Position)]
                    { Position = newPosition; Orientation = West; Visits = List.append thingy.Visits (List.rev paths) }
    | East -> 
        match fst move with 
        | Right -> 
                    let newPosition = (fst thingy.Position, snd thingy.Position - snd move)
                    let paths = [ for y in [(snd newPosition) .. (snd thingy.Position) - 1 ] -> (fst thingy.Position, y)]
                    { Position = newPosition; Orientation = South; Visits = List.append thingy.Visits (List.rev paths) }
        | Left -> 
                    let newPosition = (fst thingy.Position, snd thingy.Position + snd move)
                    let paths = [ for y in [ (snd thingy.Position) + 1 .. (snd newPosition) ] -> (fst thingy.Position, y)]
                    { Position = newPosition; Orientation = North; Visits = List.append thingy.Visits paths }
    | South -> 
        match fst move with 
        | Right -> 
                    let newPosition = (fst thingy.Position - snd move, snd thingy.Position)
                    let paths = [ for x in [(fst newPosition) .. (fst thingy.Position) - 1 ] -> (x, snd thingy.Position)]
                    { Position = newPosition; Orientation = West; Visits = List.append thingy.Visits (List.rev paths) }
        | Left -> 
                    let newPosition = (fst thingy.Position + snd move, snd thingy.Position)
                    let paths = [ for x in [(fst thingy.Position) + 1 .. (fst newPosition)] -> (x, snd thingy.Position)]
                    { Position = newPosition; Orientation = East; Visits = List.append thingy.Visits paths }
    | West -> 
        match fst move with 
        | Right -> 
                    let newPosition = (fst thingy.Position, snd thingy.Position + snd move)
                    let paths = [ for y in [ (snd thingy.Position) + 1 .. (snd newPosition) ] -> (fst thingy.Position, y)]
                    { Position = newPosition; Orientation = North; Visits = List.append thingy.Visits paths }
        | Left -> 
                    let newPosition = (fst thingy.Position, snd thingy.Position - snd move)
                    let paths = [ for y in [(snd newPosition) .. (snd thingy.Position) - 1 ] -> (fst thingy.Position, y)]
                    { Position = newPosition; Orientation = South; Visits = List.append thingy.Visits (List.rev paths) }

let mapToMove (x : string) =
     match x.[0] with
     | 'R' -> (Right, Int32.Parse(x.[1..x.Length-1].ToString()))
     | 'L' -> (Left, Int32.Parse(x.[1..x.Length-1].ToString()))
     | _ -> failwith (sprintf "'erorr %c'" x.[0])

let moves (raw : string) = 
    raw.Split(',')
    |> Seq.map mapToMove       

let endLocation moves = 
    moves
    |> Seq.fold ( fun acc mov -> transform mov acc) { Position = (0,0); Orientation = North; Visits = [(0,0)]}

let distance location =
    abs(fst location) + abs(snd location)

let routeData = 
    input.Replace(" ", String.Empty)
    |> moves
    |> endLocation

let distanceToLocation = 
    distance routeData.Position

let firstDuplicateVisit = 
    routeData.Visits
    |> Seq.countBy id
    |> Seq.filter (fun (k,n) -> n > 1)
    |> Seq.map fst
    |> Seq.head

sprintf "End Location: %O, Distance: %i, First Duplicate: %O, Distance from first Duplicate: %i" routeData.Position distanceToLocation firstDuplicateVisit (distance firstDuplicateVisit)
