open Core

(* let filename = "lib/day6/example.txt" *)
let filename = "lib/day6/input.txt"
let day6Input = In_channel.read_lines filename
let width = String.length (List.hd_exn day6Input)
let height = List.length day6Input

type pos = { col : int; row : int }
type direction = North | East | South | West
type stateOfPos = Empty | Obstacle | OffGrid | Illegal

let string_of_pos p = string_of_int p.col ^ ":" ^ string_of_int p.row

let string_of_dir = function
  | North -> "N"
  | East -> "E"
  | West -> "W"
  | South -> "S"

let string_of_pos_and_dir pos dir = string_of_pos pos ^ "|" ^ string_of_dir dir
let coordToIdx pos = pos.col + (width * pos.row)
let idxToCoord idx = { col = idx mod width; row = idx / width }

let grid =
  List.map ~f:String.to_list day6Input
  |> List.fold ~init:[] ~f:( @ )
  |> Array.of_list

let turn = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let getStateOfPos pos =
  if pos.col < 0 || pos.col >= width || pos.row < 0 || pos.row >= height then
    OffGrid
  else
    let idx = coordToIdx pos in
    let c = Array.get grid idx in
    match c with '.' | '^' -> Empty | '#' -> Obstacle | _ -> Illegal

let rec moveOneStep pos direction =
  let nextPos =
    match direction with
    | North -> { pos with row = pos.row - 1 }
    | South -> { pos with row = pos.row + 1 }
    | East -> { pos with col = pos.col + 1 }
    | West -> { pos with col = pos.col - 1 }
  in
  let state = getStateOfPos nextPos in
  match state with
  | Empty -> (nextPos, direction, state)
  | Obstacle -> moveOneStep pos (turn direction)
  | OffGrid -> (nextPos, direction, state)
  | Illegal -> raise (Failure "Illegal char in grid")

let visitedPosSet = Set.empty (module String)

let startPos =
  fst (Array.findi_exn grid ~f:(fun _idx c -> Char.equal c '^')) |> idxToCoord

let rec doMove accum pos direction =
  let nextPos, nextDir, nextState = moveOneStep pos direction in
  (* Printf.printf "%s\n" (string_of_pos_and_dir nextPos nextDir); *)
  match nextState with
  | OffGrid -> accum
  | Empty | Obstacle ->
      doMove (Set.add accum (string_of_pos nextPos)) nextPos nextDir
  | _ -> raise (Failure "Illegal state")

let resultPath = doMove visitedPosSet startPos North
let resultP1 = Set.length resultPath
let posEqual p1 p2 = p1.col = p2.col && p1.row = p2.row

let posOfString s =
  let lst = String.split ~on:':' s in
  match List.map ~f:int_of_string lst with
  | c :: r :: _ -> { col = c; row = r }
  | _ -> raise (Failure "Error in pos set")

let obstaclePositions = List.map ~f:posOfString (Set.to_list resultPath)

let rec moveOneStepWithObstacle pos direction obstaclePos =
  let nextPos =
    match direction with
    | North -> { pos with row = pos.row - 1 }
    | South -> { pos with row = pos.row + 1 }
    | East -> { pos with col = pos.col + 1 }
    | West -> { pos with col = pos.col - 1 }
  in

  let state =
    if posEqual nextPos obstaclePos then Obstacle else getStateOfPos nextPos
  in

  match state with
  | Empty -> (nextPos, direction, state)
  | Obstacle -> moveOneStepWithObstacle pos (turn direction) obstaclePos
  | OffGrid -> (nextPos, direction, state)
  | Illegal -> raise (Failure "Illegal char in grid")

(* Returns true if we are looping forever and false otherwise *)
let rec doMoveWithObstacle accum pos direction obstaclePos =
  let nextPos, nextDir, nextState =
    moveOneStepWithObstacle pos direction obstaclePos
  in
  (* Printf.printf "%s\n" (string_of_pos_and_dir nextPos nextDir); *)
  if Set.mem accum (string_of_pos_and_dir nextPos nextDir) then true
  else
    match nextState with
    | OffGrid -> false
    | Empty | Obstacle ->
        doMoveWithObstacle
          (Set.add accum (string_of_pos_and_dir nextPos nextDir))
          nextPos nextDir obstaclePos
    | _ -> raise (Failure "Illegal state")

let r2 =
  List.map
    ~f:(fun oPos -> doMoveWithObstacle visitedPosSet startPos North oPos)
    obstaclePositions

let resultP2 = List.count ~f:Fn.id r2
