namespace Day18

open NUnit.Framework
open System

module Day18App =

    let (|Int|_|) str =
        match System.Int64.TryParse(str) with
        | (true,int) -> Some(int)
        | _ -> None

    type Instruction =
    | Set of char * string
    | Add of char * string
    | Mulitply of char * string
    | Modulos of char * string
    | Recover of char
    | Jump of string * string
    | Send of char

    let getValueOfRegistar (input:string) (registars : Map<char,int64>) = 
        match input with
        | Int i -> i
        | _ -> registars.[input.ToCharArray().[0]]

    let getValueOrEmpty (registar:char) (registars:Map<char,int64>) = 
        match Map.tryFind registar registars with 
        | Some x -> x
        | None -> 0L

    let toChar (input:string) = 
        input.ToCharArray().[0]

    let set registar value = 
        Map.add registar value

    let add (registar:char) value (registars: Map<char,int64>) =
        Map.add registar ((getValueOrEmpty registar registars) + value) registars

    let multiply (registar:char) value (registars: Map<char,int64>) =
        Map.add registar ((getValueOrEmpty registar registars) * value) registars

    let modulos (registar:char) value (registars: Map<char,int64>) =
        Map.add registar ((getValueOrEmpty registar registars) % value) registars

    let recover (registar:char) (registars: Map<char,int64>) =
        let value = (getValueOrEmpty registar registars)       
        if value > 0L
        then
            printfn "RECOVER: %c" registar
            Some value
        else None

    let jump condition (offset:int) =
        if condition > 0
        then offset
        else 1

    let instructionFrom (input:string) = 
        let parts = input.Split(' ')
        match parts with 
        | [|"set"; x; y|] -> Set((toChar x), y)
        | [|"add"; x; y|] -> Add((toChar x), y)
        | [|"mul"; x; y|] -> Mulitply((toChar x), y)
        | [|"mod"; x; y|] -> Modulos((toChar x), y)
        | [|"rcv"; x |] -> Recover(toChar x)
        | [|"jgz"; x; y|] -> Jump(x,y)
        | [|"snd"; x|] -> Send (toChar x)
        | _ -> failwith "Unknown Part"

    type State = { Registars : Map<char,int64>; CurrentIndex : int; Recovered : int64 option; LastPlayed : int64}

    let playInstruction (instruction: Instruction) (state:State) = 
        match instruction with 
        | Set (x,y) -> { state with Registars = set x (getValueOfRegistar y state.Registars) state.Registars; CurrentIndex = state.CurrentIndex + 1 }
        | Add (x,y) -> { state with Registars = add x (getValueOfRegistar y state.Registars) state.Registars; CurrentIndex = state.CurrentIndex + 1 }
        | Mulitply (x,y) -> { state with Registars = multiply x (getValueOfRegistar y state.Registars) state.Registars; CurrentIndex = state.CurrentIndex + 1 }
        | Modulos (x,y) -> { state with Registars = modulos x (getValueOfRegistar y state.Registars) state.Registars; CurrentIndex = state.CurrentIndex + 1 }
        | Jump (x,y) -> { state with CurrentIndex = state.CurrentIndex + (jump (int(getValueOfRegistar x state.Registars)) (int(getValueOfRegistar y state.Registars)))}
        | Recover (x) -> { state with Recovered = recover x state.Registars; CurrentIndex = state.CurrentIndex + 1 }
        | Send x -> printfn "Send: %i" state.Registars.[x]; { state with LastPlayed = state.Registars.[x]; CurrentIndex = state.CurrentIndex + 1 }

    let SolvePart1 (rawInstructions: string) =
        let instructions = 
            rawInstructions.Split('\n')
            |> Array.map instructionFrom

        let rec play (state:State) = 
            let instruction = Array.item state.CurrentIndex instructions
            let newState = playInstruction instruction state 
            match newState.Recovered with 
            | Some _ -> newState.LastPlayed
            | None -> play newState

        play { Registars = Map.empty; CurrentIndex = 0; Recovered = None; LastPlayed = 0L }
        

    
module Day18Tests =

    let testInput = @"set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"

    [<Test>]
    let ``Solve part 1 test `` () =
        Assert.That(Day18App.SolvePart1 testInput, Is.EqualTo(4))
        

    let puzzle = @"set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 680
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19"

    [<Test>]
    let ``Solve part 1 `` () =
        //3675 too high
        Assert.That(Day18App.SolvePart1 puzzle, Is.EqualTo(0)) 

