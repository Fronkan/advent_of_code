open Core 

let isExample = false
let filename = if isExample then
   "lib/day15/example2.txt" 
  else
    "lib/day15/input.txt"

let day15Input = In_channel.read_all filename

type gridType = { arr:char array; width:int; height:int }
type pos = { col : int; row : int } 
type direction = North | East | South | West

let idx_of_pos grid pos = pos.col + grid.width * pos.row

let pos_of_idx grid idx = {col = idx mod grid.width; row = idx / grid.width}

let parseInput day15Input =
  let emptyLinePos =   String.substr_index_exn day15Input ~pattern:"\n\n" in
  
  let gridPart = String.sub day15Input ~pos:0 ~len:emptyLinePos in
  let gridLines = String.split_lines gridPart in
  let height = List.length gridLines in
  let width = String.length (List.hd_exn gridLines) in
  let gridStr = String.concat gridLines in
  let arr = String.to_array gridStr in
  let grid = { arr; width; height} in
  
  let dirPart = String.sub day15Input ~pos:(emptyLinePos+2) ~len:((String.length day15Input) - emptyLinePos - 2) in
  let moves = List.filter (String.to_list dirPart) ~f:(fun c -> not (Char.equal c '\n')) in
  grid,moves

let grid,moves = parseInput day15Input

let robotPos = pos_of_idx grid (fst (Array.findi_exn grid.arr ~f:(fun _ c -> Char.equal c '@')))

let dir_of_move move = 
  match move with 
  | '^' -> North
  | '>' -> East
  | 'v' -> South
  | '<' -> West
  | _ -> failwith "Err in directions"
  
let moveOneStep direction coord =
  match direction with 
  | North -> {coord with row = coord.row - 1}
  | South -> {coord with row = coord.row + 1}
  | West -> {coord with col = coord.col - 1}
  | East -> {coord with col = coord.col + 1}

let isWall grid pos =
  Char.equal grid.arr.(idx_of_pos grid pos) '#'

let isFree grid pos =
  Char.equal grid.arr.(idx_of_pos grid pos) '.'


let moveRobot grid fromPos toPos =
  grid.arr.(idx_of_pos grid fromPos) <- '.' ;
  grid.arr.(idx_of_pos grid toPos) <- '@';
  toPos

let tryPushBox grid direction pos =
  let rec pushAttempt pos =
    let nextPos = moveOneStep direction pos in
    if isFree grid nextPos then 
      (grid.arr.(idx_of_pos grid nextPos) <- 'O';
      grid.arr.(idx_of_pos grid pos) <- '.';
      true)
    else
      if isWall grid nextPos then 
        false
      else
        (* nextpos has a box. try push *)
        if pushAttempt nextPos then
          (* Push succeded. We move this box also *)
          (grid.arr.(idx_of_pos grid nextPos) <- 'O';
          grid.arr.(idx_of_pos grid pos) <- '.';
          true)
        else
          false
  in
  pushAttempt pos

let tryMove grid move robotPos = 
  let nextPos = moveOneStep (dir_of_move move) (robotPos) in
  if isWall grid nextPos then 
    (* Found wall. Dont move *)
    robotPos
  else 
    if isFree grid nextPos then 
      (* This spot is free. Just move *)
      moveRobot grid robotPos nextPos
    else
      (* Next pos has a box. Try to push it. *)
      let couldPush = tryPushBox grid (dir_of_move move) nextPos in
      if couldPush then 
        moveRobot grid robotPos nextPos
      else 
        (* Robot could not push boxes *)
        robotPos

let printGrid grid =
  let rec getRow acc rowNo =
    if rowNo = grid.height then List.rev acc
    else 
      let rec getCol acc colNo =
        if colNo = grid.width then String.of_char_list (List.rev acc)
        else
          let c = grid.arr.(idx_of_pos grid {col=colNo; row = rowNo}) in
          
          getCol (c::acc) (colNo + 1)

      in
      getRow ((getCol [] 0)::acc) (rowNo + 1)

  in
  List.iter (getRow [] 0) ~f:(fun l ->
    Printf.printf "%s\n" l)

let doMoves grid moves robotPos = 
  let rec moveLoop moves robotPos=
    match moves with 
    | [] -> robotPos
    | move::remainingMoves  -> 
        let robotPos = tryMove grid move robotPos in
        moveLoop remainingMoves robotPos
    in

  moveLoop moves robotPos

let gpsCoordOfPos pos = pos.col + 100 * pos.row

let res grid = List.foldi (Array.to_list grid.arr) ~init:0 ~f:(fun idx acc c ->
    acc + if Char.equal c 'O' then gpsCoordOfPos (pos_of_idx grid idx)
    else 0
  )

let _ = doMoves grid moves robotPos
let resultP1 = res grid
