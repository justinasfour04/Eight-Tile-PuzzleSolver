// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.



open System.Collections

type Position = int * int
type Cell = int
type Move = U | D | R | L

type ValidMoves = Move list

type Board = (Cell * Position) list // State of board 

type SolutionTree = Node of Board * SolutionTree * SolutionTree

// Given position of zero get list of next possible moves
let getNextMoveSet zero : ValidMoves = 
    match zero with
    | (0,0) -> [R; D]
    | (1,0) -> [R; L; D]
    | (2,0) -> [L; D]
    | (0,1) -> [U; D; R]
    | (1,1) -> [U; D; R; L]
    | (2,1) -> [U; D; L]
    | (0,2) -> [U; R]
    | (1,2) -> [U; R; L]
    | (2,2) -> [U; L]
    | (_, _) -> failwith "Zero out of bounds"

// Find the position of the blank/zero tile
let rec findZero (board:Board):Position = 
    if fst (List.head board) = 0 then snd (List.head board)
    else findZero (List.skip 1 board) 

let up = fun board -> fun elem -> (snd (snd elem) = snd (findZero board) - 1) && (fst (snd elem) = fst (findZero board))
let down = fun board -> fun elem -> (snd (snd elem) = snd (findZero board) + 1) && (fst (snd elem) = fst (findZero board))
let right = fun board -> fun elem -> (fst (snd elem) = fst (findZero board) + 1) && (snd (snd elem) = snd (findZero board))
let left = fun board -> fun elem -> (fst (snd elem) = fst (findZero board) - 1) && (snd (snd elem) = snd (findZero board))

let swap board direction =
    let tileToSwap = (List.find (direction board) board)
    let zero = findZero board
    let rec helper currentBoard = 
        match currentBoard with 
        | [] -> []
        | x::xs -> 
            if x = tileToSwap then
                (0, snd tileToSwap)::(helper xs)
            elif snd x = zero then
                (fst tileToSwap, zero)::(helper xs)           
            else
                x::(helper xs)
    helper board

let nextBoard currentBoard = 
    let zeroPosition = findZero currentBoard
    let validMoves = getNextMoveSet (zeroPosition)
    let rec helper moves =
        match moves with
        | [] -> []
        | m::ms ->
            match m with
            | U -> (swap currentBoard up)::(helper ms)
            | D -> (swap currentBoard down)::(helper ms)
            | L -> (swap currentBoard left)::(helper ms)
            | R -> (swap currentBoard right)::(helper ms)
    helper validMoves
     
let rec manhattanDistance (board:Board) = 
    match board with
    | [] -> 0
    | x::xs -> 
        let value = fst x // Value of the tile in current state 
        if value <> 0 then
            let xPos = fst (snd x) // X coordinate of tile in current state
            let yPos = snd (snd x) // Y coordinate of tile in current state
            let targetx = (value - 1) % 3 // X coordinate of tile in goal state  
            let targety = (value - 1) / 3 // Y coordinate of tile in goal state
            (manhattanDistance xs) + abs (xPos - targetx) + abs (yPos - targety)
        else 
            manhattanDistance xs

//let solve initialBoard finalBoard = 
//    let openlist = Queue<Board>()
//    let closedlist = Queue<Board>()
//    let totalMoves = ref 0
//    let movesSoFar = ref []
//    openlist.Enqueue(initialBoard)
//
//    while openlist.Count > 0 do
//        let minf = manhattanDistance (openlist.Dequeue()) + !totalMoves
//        if initialBoard = finalBoard then !movesSoFar
//        let nextBoards = nextBoard initialBoard
//        for b in nextBoard do
//            let cost =   
        

(*
    | 1 | 2 | 3 |
    | 4 | 5 | 6 |
    | 7 | 8 | 0 |
*)

let (initialBoard:Board) = 
    [ (1, (0,0)); (2, (1,0)); (3, (2,0));
      (4, (0,1)); (5, (1,1)); (6, (2,1));
      (7, (0,2)); (8, (1,2)); (0, (2,2)) ]

let print board =
    let rec helper b i =
        match b with
        | [] -> printfn "\n"
        | x::xs -> 
            if i < 3 then 
                (printf "%i" (fst x))
                let i = i + 1
                helper xs i
            else 
                printfn ""
                helper b 0 
    helper board 0

//TODO: keep track of where it came from and don't go back to that state 
let solve board =
    let finalBoard = 
        [ (1, (0,0)); (2, (1,0)); (3, (2,0));
          (4, (0,1)); (5, (1,1)); (6, (2,1));
          (7, (0,2)); (8, (1,2)); (0, (2,2)) ]
    let rec helper (currentBoard:Board) (previousBoard:Board) (finalBoard:Board) totalMoves heuristic = 
        let next = nextBoard currentBoard
        let fList = List.map (fun elem -> (heuristic elem) + totalMoves) next
        let minf = List.min fList // minimum f
        let nextNodesPossible = List.collect (fun elem -> )
        let q = List.item (List.findIndex (fun elem -> elem = minf) fList) next // next node to check
        print q
        if q = finalBoard then printfn "done"; q
        elif q = previousBoard then 
            let nextQ = List.item (List.findIndex (fun elem -> elem = minf) fList) next
        else helper q finalBoard (totalMoves + 1) heuristic
    helper board finalBoard 0 manhattanDistance

[<EntryPoint>]
let main argv = 
    let c = solve initialBoard
    0
         
