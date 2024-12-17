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
   "lib/day16/example.txt" 
  else
    "lib/day16/input.txt"

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

let str_of_reindeer reindeer = 
    string_of_int reindeer.pos.row ^ ":"
    ^ string_of_int reindeer.pos.col ^ ":"
    ^ string_of_dir reindeer.dir

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
      let onWall = isOnWall grid movedReindeer in
      not onWall 
    )

let rComp (r1,_) (r2,_) =
  Int.compare r1.cost r2.cost


let bfs reindeerStart = 
  (* visited is a ref so it is mutable *)
  let visited = ref (Set.empty (module String)) in 
  let queue = ref [(reindeerStart, [])] in

  let rec moveLoop () =
    if is_empty !queue then None
    else
      
      let popped = Option.value_exn (pop !queue) in
      queue := snd popped ;
      let currReindeer, path = fst popped in
      if Set.mem !visited (str_of_reindeer currReindeer) then 
        (
        moveLoop ()
        )
      else 
        if isOnTarget grid currReindeer then
          Some (List.rev path)
        else (
          visited := Set.add !visited (str_of_reindeer currReindeer);
          let moves = checkMoves grid allMoves currReindeer in
          let possibleReindeers = List.map moves ~f:(fun move ->
              doMove move currReindeer
            ) in
          List.iter possibleReindeers ~f:(fun reindeer ->
            let el = (reindeer, reindeer::path ) in
            queue := sorted_insert el !queue ~cmp:rComp ;
          );
          moveLoop ()
        )
      
      in
    moveLoop ()



let reindeer = {pos = {col=1;row=grid.height-2}; dir= East; cost=0}

let shortestPath = bfs reindeer

let resultP1 = match shortestPath with 
      | None -> 0
      | Some path -> (List.last_exn path).cost
let resultP2 = 0
