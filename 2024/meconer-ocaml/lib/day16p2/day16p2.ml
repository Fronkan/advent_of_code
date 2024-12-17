open Core  

  let rec sorted_insert ~cmp elem lst =
  match lst with
  | [] -> [elem] (* Insert into an empty list *)
  | head :: tail ->
    if cmp elem head <= 0 then
      elem :: lst (* Insert before the current head *)
    else
      head :: sorted_insert ~cmp elem tail (* Recurse on the rest of the list *)
  (* Insert an element while maintaining sorted order *)


  (* Extract the smallest element *)
  let pop = function
    | [] -> None
    | x :: xs -> Some (x, xs)

  (* Peek at the smallest element *)
  let peek = function
    | [] -> None
    | x :: _ -> Some x

  (* Check if empty *)
  let is_empty queue = List.is_empty queue


let isExample = false
let filename = if isExample then
   "lib/day16p2/example2.txt" 
  else
    "lib/day16p2/input.txt"

let day16Input = In_channel.read_lines filename

type gridType = { arr:char array; width:int; height:int }
type pos = { col : int; row : int } 
type direction = North | East | South | West

type reindeer = {pos:pos; dir:direction; cost:int}

type moveType = TurnLeft | TurnRight | MoveForward

let allMoves = [TurnLeft; TurnRight; MoveForward]

let idx_of_pos grid pos = pos.col + grid.width * pos.row

let pos_of_idx grid idx = {col = idx mod grid.width; row = idx / grid.width}

let string_of_dir dir = 
  match dir with 
  | North -> "N"
  | West -> "W"
  | South -> "S"
  | East -> "E" 

let char_of_dir dir = 
  match dir with 
  | North -> '^'
  | West -> '<'
  | South -> 'v'
  | East -> '>' 

let str_of_reindeer reindeer = 
    string_of_int reindeer.pos.row ^ ":"
    ^ string_of_int reindeer.pos.col 
    ^ ":" ^ string_of_dir reindeer.dir

let parseInput lines =
  let height = List.length lines in
  let width = String.length (List.hd_exn lines) in
  let gridStr = String.concat lines in
  let arr = String.to_array gridStr in
  let grid = { arr; width; height} in
  grid

let grid = parseInput day16Input

let turnLeft reindeer  =
  let newDir = match reindeer.dir with 
  | North -> West
  | West -> South
  | South -> East
  | East -> North in
  let newCost = reindeer.cost + 1000 in
  {reindeer with dir= newDir;cost = newCost}

let turnRight reindeer =
  let newDir = match reindeer.dir with 
  | North -> East
  | West -> North
  | South -> West
  | East -> South in
  let newCost = reindeer.cost + 1000 in
  {reindeer with dir= newDir;cost = newCost}

let moveOneStep direction coord =
  match direction with 
  | North -> {coord with row = coord.row - 1}
  | South -> {coord with row = coord.row + 1}
  | West -> {coord with col = coord.col - 1}
  | East -> {coord with col = coord.col + 1}

let moveForward reindeer =
  let newPos = moveOneStep reindeer.dir reindeer.pos in
  let newCost = reindeer.cost + 1 in
  {reindeer with pos = newPos; cost = newCost}


let getCharOfPath path pos c =
  let lastDirOfPos = List.find path ~f:(fun reindeer ->
      reindeer.pos.col = pos.col && reindeer.pos.row = pos.row
    ) in
    match lastDirOfPos with 
    | None -> c
    | Some reindeer -> char_of_dir reindeer.dir

let printGrid grid path =
  let rec getRow acc rowNo =
    if rowNo = grid.height then List.rev acc
    else 
      let rec getCol acc colNo =
        if colNo = grid.width then String.of_char_list (List.rev acc)
        else
          let c = grid.arr.(idx_of_pos grid {col=colNo; row = rowNo}) in
          let c = getCharOfPath path  {col=colNo; row = rowNo} c in
          getCol (c::acc) (colNo + 1)

      in
      getRow ((getCol [] 0)::acc) (rowNo + 1)

  in
  List.iter (getRow [] 0) ~f:(fun l ->
    Printf.printf "%s\n" l)

let printGridWithSet grid visitedSet =
  let rec getRow acc rowNo =
    if rowNo = grid.height then List.rev acc
    else 
      let rec getCol acc colNo =
        if colNo = grid.width then String.of_char_list (List.rev acc)
        else
          let idxOfPos = idx_of_pos grid {col=colNo; row = rowNo} in
          let c = grid.arr.(idxOfPos) in
          let c = if Set.mem visitedSet (idxOfPos) then 'O' else c
          in
          getCol (c::acc) (colNo + 1)

      in
      getRow ((getCol [] 0)::acc) (rowNo + 1)

  in
  List.iter (getRow [] 0) ~f:(fun l ->
    Printf.printf "%s\n" l)

let doMove  move reindeer =
  match move with 
  | TurnLeft -> turnLeft reindeer
  | TurnRight -> turnRight reindeer
  | MoveForward -> moveForward reindeer
            
let isOnWall grid reindeer =
  let gridChar = grid.arr.(idx_of_pos grid reindeer.pos) in
  Char.equal gridChar '#'

let isOnTarget grid reindeer =
  let gridChar = grid.arr.(idx_of_pos grid reindeer.pos) in
  Char.equal gridChar 'E'


let checkMoves grid moves reindeer = 
  List.filter moves ~f:(fun move ->
    let movedReindeer = doMove  move reindeer in
    match move with 
    | TurnLeft | TurnRight -> let posIfTurnAndMove = moveForward movedReindeer in
                    not (isOnWall grid posIfTurnAndMove)
    | MoveForward -> not (isOnWall grid movedReindeer)
     
    )

let rComp (r1,_) (r2,_) =
  Int.compare r1.cost r2.cost

let waitForEnter s =
  Printf.printf "%s" s;
  Out_channel.flush stdout;
  let _  = In_channel.(input_line stdin) in
  ()

let printPath path = 
  List.iter path ~f:(fun reindeer ->
    Printf.printf "%s Cost:%d\n" (str_of_reindeer reindeer) reindeer.cost;
    )
  (* ;
  waitForEnter "\n" *)



let bfs reindeerStart = 
  let visited = ref (Map.empty (module String)) in 
  let queue = ref [(reindeerStart, [])] in
  let result_paths = ref [] in
  let min_cost = ref Int.max_value in

  let rec moveLoop () =
    if is_empty !queue then
       !result_paths
    else
      
      let popped = Option.value_exn (pop !queue) in
      queue := snd popped ;
      let currReindeer, path = fst popped in
      (* printGrid grid path;
      Printf.printf "Cost: %d " currReindeer.cost; *)
      let already_visited = Map.find !visited (str_of_reindeer currReindeer) in

      let isAlreadyVisitedWithLowerCost =  Option.is_some already_visited && (Option.value_exn already_visited < currReindeer.cost) in
      if isAlreadyVisitedWithLowerCost then
        begin
          (* Printf.printf "Throwing %d" (Option.value_exn already_visited); *)
          (* waitForEnter "\n"; *)
          moveLoop () 
        end
      else
        begin
          (* waitForEnter "\n"; *)
          visited := Map.set !visited ~key:(str_of_reindeer currReindeer) ~data:currReindeer.cost;
          if isOnTarget grid currReindeer then
            if currReindeer.cost < !min_cost then 
              begin
                min_cost := currReindeer.cost;
                result_paths := [(currReindeer::path)]; (* Smaller cost is found. We replace all earlier paths with this one  *)
              end
            else
              
              if currReindeer.cost = !min_cost then 
                (* Same cost. Add to paths*)
                result_paths := (currReindeer::path) :: !result_paths ;

              let moves = checkMoves grid allMoves currReindeer in
              let possibleReindeers = List.map moves ~f:(fun move ->
                  doMove move currReindeer
                ) in
              List.iter possibleReindeers ~f:(fun reindeer ->
                let el = (reindeer, reindeer::path ) in
                queue := sorted_insert el !queue ~cmp:rComp ;
              );
              moveLoop ()
        end
    in
    moveLoop ()



let reindeer = {pos = {col=1;row=grid.height-2}; dir= East; cost=0}

let shortestPaths = bfs reindeer

let setsOfPositionsVisited = List.map shortestPaths ~f:(fun reindeerLst -> 
    List.fold reindeerLst ~init:(Set.empty (module  Int)) ~f:(fun acc reindeer ->
      Set.add acc (idx_of_pos grid reindeer.pos)
      )
  )

let startSet = Set.of_list (module Int) [idx_of_pos grid reindeer.pos]

let combinedSet = List.fold setsOfPositionsVisited ~init:startSet ~f:(fun acc set ->
     Set.union acc set
  )

let _ = printGridWithSet grid combinedSet
let resultP2 = Set.length combinedSet

(* 562 too low. Should be 563. I did not count the start position which only was stored if the first
  reindeer move was a turn *)