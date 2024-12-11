open Core 

(* let filename = "lib/day10/example.txt" *)
let filename = "lib/day10/input.txt"
let day10Input = In_channel.read_lines filename

let width = String.length (List.hd_exn day10Input)
let height = List.length day10Input

type pos = { col : int; row : int } 
type direction = North | East | South | West

let string_of_pos p = string_of_int p.col ^ ":" ^ string_of_int p.row

let string_of_dir  = function
  | North -> "N"
  | East -> "E"
  | West -> "W"
  | South -> "S"

let string_of_pos_and_dir pos dir =
  string_of_pos pos ^ "|" ^ string_of_dir dir

let coordToIdx pos = pos.col + width * pos.row

let idxToCoord idx = {col = idx mod width; row = idx / width}


let l1 = List.map ~f:String.to_list day10Input
let grid = (List.map ~f:String.to_list day10Input) 
  |> (List.fold ~init:[] ~f:(@)) 
  |> ( List.map ~f:(fun c -> int_of_char c - int_of_char '0') )
  |> Array.of_list


let findStartPoints grid = 
  
  let rec loop acc idx =
    if idx >= width * height then 
      List.rev acc
    else
      if grid.(idx) = 0 then
        loop (idx::acc) (idx + 1)
      else
        loop acc (idx + 1)
      in
  loop [] 0

let moveOneStep direction coord =
  match direction with 
  | North -> {coord with row = coord.row - 1}
  | South -> {coord with row = coord.row + 1}
  | West -> {coord with col = coord.col - 1}
  | East -> {coord with col = coord.col + 1}

let isOnGrid coord = 
  coord.col >= 0 && coord.col < width && coord.row >=0 && coord.row < height

let find9 startPoint =

  let rec loop acc point visited = 

    let currH = grid.(coordToIdx point) in
    if currH = 9 then 
      [point :: acc] 
    else
      let possibleMoves = [North;East;South;West]
        |> List.map ~f:(fun dir -> moveOneStep dir point) 
        |> List.filter ~f:(isOnGrid) 
        |> List.filter ~f:(fun p -> grid.(coordToIdx p) = currH +1)
        |> List.filter ~f:(fun p -> not ( Set.mem visited (string_of_pos p)))
      in
      List.concat_map possibleMoves ~f:(fun move ->
        let newVisited = Set.add visited (string_of_pos move) in
         loop (point::acc) move newVisited)
    in
      
  let visited = Set.empty (module  String) in
    
  let paths = loop [] startPoint visited in

  let noOfTrailHeads = 
    List.map ~f:(List.hd_exn) paths 
    |> List.map ~f:(string_of_pos) 
    |> Set.of_list (module String) 
    |> Set.length in

  noOfTrailHeads,paths



let startPoints = findStartPoints grid |> List.map ~f:idxToCoord

let trailHeads = List.map startPoints ~f:(fun p -> fst (find9 p))



let resultP1 = List.fold trailHeads ~init:0 ~f:(+)

let noOfDistPaths = List.map ~f:(fun sp -> List.length (snd (find9 sp))) startPoints

let resultP2 = List.fold noOfDistPaths ~init:0 ~f:(+)