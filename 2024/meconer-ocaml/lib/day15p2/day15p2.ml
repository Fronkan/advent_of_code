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

let convertToP2 line = 
  let lst = String.to_list line in
  (List.map lst ~f:(fun c -> 
    match c with 
    | '#' -> "##"
    | '.' -> ".."
    | 'O' -> "[]"
    | '@' -> "@."
    | _ -> "Unknown char in grid"
    )) 
  |> List.fold ~init:"" ~f:(fun acc s -> acc ^ s )


let parseInput day15Input =
  let emptyLinePos =   String.substr_index_exn day15Input ~pattern:"\n\n" in
  
  let gridPart = String.sub day15Input ~pos:0 ~len:emptyLinePos in
  let gridLines = List.map ~f:(convertToP2) (String.split_lines gridPart) in
  
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

let isWall grid pos =
  Char.equal grid.arr.(idx_of_pos grid pos) '#'

let isFree grid pos =
  Char.equal grid.arr.(idx_of_pos grid pos) '.'


let moveRobot grid fromPos toPos =
  grid.arr.(idx_of_pos grid fromPos) <- '.' ;
  grid.arr.(idx_of_pos grid toPos) <- '@';
  toPos

let tryPushEWBox grid direction pos =

  let rec pushAttempt pos =

    (* Check the position two steps away *)
    let posOneStepAhead =  moveOneStep direction pos in
    let posTwoStepsAhead = moveOneStep direction posOneStepAhead in
    let boxCharOneStep, boxCharTwoSteps = 
        match direction with 
        | East -> '[', ']'
        | West -> ']', '[' 
        | _ -> failwith "Only east and west directions here" in

    if isFree grid posTwoStepsAhead then 
      (* Free position. We can move one step *)
        (grid.arr.(idx_of_pos grid posOneStepAhead) <- boxCharOneStep;
        grid.arr.(idx_of_pos grid posTwoStepsAhead) <- boxCharTwoSteps;
        grid.arr.(idx_of_pos grid pos) <- '.';
        true)
    else
      (* Pos on other side of box is not free *)

      if isWall grid posTwoStepsAhead then 
        (* There is a wall. We cannot move *)
        false
      else

        (* There is a box on the other side. Try to push it *)
        if pushAttempt posTwoStepsAhead then
          (* Push succeded. We move this box also *)
          (grid.arr.(idx_of_pos grid posOneStepAhead) <- boxCharOneStep;
          grid.arr.(idx_of_pos grid posTwoStepsAhead) <- boxCharTwoSteps;
          grid.arr.(idx_of_pos grid pos) <- '.';
          true)
        else
          false
  in
  pushAttempt pos

(* Try to push the box north or south. Return false if not possible *)
let rec canPushNSBox grid direction pos =
  (* Set the correct box location to the left char of the box *)

    let posL = 
    if Char.equal grid.arr.( idx_of_pos grid pos) ']' then
      {col = pos.col - 1; row = pos.row}
    else pos
    in

    let posOneStepL = moveOneStep direction posL in
    let posOneStepR = moveOneStep East posOneStepL in
    
    if isWall grid posOneStepL || isWall grid posOneStepR then 
      false
    else
      (* There is no wall above. Check if any other boxes stop us from pushing *)
      let leftChar = grid.arr.(idx_of_pos grid posOneStepL) in
      let rightChar = grid.arr.(idx_of_pos grid posOneStepR) in
      (* Printf.printf "%c %c" leftChar rightChar; *)
      match leftChar, rightChar with 
      | ']','[' -> (* Two boxes above: We need to be able to push both *) 
              canPushNSBox grid direction posOneStepL && canPushNSBox grid direction posOneStepR
      | ']','.' -> (* One box above to the left *)
              canPushNSBox grid direction posOneStepL
      | '[',']' -> (* One box exactly on top *)
              canPushNSBox grid direction posOneStepL
      | '.','[' -> (* One box above to the right *)
              canPushNSBox grid direction posOneStepR
      | '.','.' -> (* Above is free *)
              true
      | _, _ -> failwith "Impossible combo in canPushNSBox"

let rec pushNSBox grid direction pos =

  (* Set the correct box location to the left char of the box *)
  let posL = 
  if Char.equal grid.arr.( idx_of_pos grid pos) ']' then
    {col = pos.col - 1; row = pos.row}
  else pos in
  let posR = moveOneStep East posL in
  
  (* Printf.printf "Row: %d\n" posL.row;
  Printf.printf "L,R: %d,%d\n" posL.col posR.col;
  let l = grid.arr.(idx_of_pos grid posL) in
  let r = grid.arr.(idx_of_pos grid posR) in
  Printf.printf "L,R: %c,%c\n" l r; *)
  let posOneStepL = moveOneStep direction posL in
  let posOneStepR = moveOneStep East posOneStepL in

  let leftChar = grid.arr.(idx_of_pos grid posOneStepL) in
  let rightChar = grid.arr.(idx_of_pos grid posOneStepR) in

  let res = 
  match leftChar, rightChar with 
  | ']','[' -> (* Two boxes above. Push both *) 
          pushNSBox grid direction posOneStepL && pushNSBox grid direction posOneStepR
  | ']','.' -> (* One box above to the left *)
          pushNSBox grid direction posOneStepL
  | '[',']' -> (* One box exactly on top *)
          pushNSBox grid direction posOneStepL
  | '.','[' -> (* One box above to the right *)
          pushNSBox grid direction posOneStepR
  | '.','.' -> (* No boxes above. Nothing to push *)
  
          true
  | _, _ -> failwith "Impossible combo"
  in
  (* Now, after the push above we are able to move this box *)
  (* Printf.printf "Moving box\n"; *)
  grid.arr.(idx_of_pos grid posOneStepL) <- '[';
  grid.arr.(idx_of_pos grid posOneStepR) <- ']';
  grid.arr.(idx_of_pos grid posL) <- '.';
  grid.arr.(idx_of_pos grid posR) <- '.';
  res


let tryMoveNS grid direction pos =
    (* Check the position one step forward away *)
    (* Printf.printf "In tryMoveNS\n"; *)
    let posOneStepAhead =  pos in
    
    if isFree grid posOneStepAhead then 
      true
    else
      (* Pos on other side of box is not free *)

      if isWall grid posOneStepAhead then 
        (* There is a wall. We cannot move *)
        false
      else

        (* There is a box where we want to move. Try to push it *)
        if canPushNSBox grid direction posOneStepAhead then
          (* Push is possible. Now do the move*)
            (
              (* Printf.printf "Before push\n";
              printGrid grid; *)
              let res = pushNSBox grid direction posOneStepAhead in
              (* Printf.printf "After push\n";
              printGrid grid; *)
              res
            )
            
          else
            false

let tryPushBox grid direction pos =
  match direction with 
  | North | South -> tryMoveNS grid direction pos
  | East | West -> tryPushEWBox grid direction pos

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

let waitForEnter s =
  Printf.printf "%s" s;
  Out_channel.flush stdout;
  let _  = In_channel.(input_line stdin) in
  false

let doMoves grid moves robotPos = 
  let rec moveLoop moves robotPos=
    match moves with 
    | [] -> robotPos
    | move::remainingMoves  -> 
        (* Printf.printf "%c\n" move;
        printGrid grid; *)
        (* let _ = waitForEnter "Press enter\n" in *)
        let robotPos = tryMove grid move robotPos in
        moveLoop remainingMoves robotPos
    in

  moveLoop moves robotPos

let gpsCoordOfPos pos = pos.col + 100 * pos.row

let res grid = List.foldi (Array.to_list grid.arr) ~init:0 ~f:(fun idx acc c ->
    acc + if Char.equal c '[' then gpsCoordOfPos (pos_of_idx grid idx)
    else 0
  )

let _ = doMoves grid moves robotPos


let resultP2 = res grid
