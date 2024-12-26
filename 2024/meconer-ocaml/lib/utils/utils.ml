type gridType = { arr : char array; width : int; height : int }
type pos = { col : int; row : int }
type direction = North | East | South | West
type reindeer = { pos : pos; dir : direction; cost : int }
type moveType = TurnLeft | TurnRight | MoveForward

let allMoves = [ TurnLeft; TurnRight; MoveForward ]
let allDirections = [ North; East; South; West ]

let moveOneStep direction coord =
  match direction with
  | North -> { coord with row = coord.row - 1 }
  | South -> { coord with row = coord.row + 1 }
  | West -> { coord with col = coord.col - 1 }
  | East -> { coord with col = coord.col + 1 }

let idx_of_pos_in_grid grid pos = pos.col + (grid.width * pos.row)
let idx_of_pos width pos = pos.col + (width * pos.row)

let pos_of_idx_in_grid grid idx =
  { col = idx mod grid.width; row = idx / grid.width }

let pos_of_idx width idx = { col = idx mod width; row = idx / width }

let string_of_dir dir =
  match dir with North -> "N" | West -> "W" | South -> "S" | East -> "E"

let isOnGrid ~width ~height coord =
  coord.col >= 0 && coord.col < width && coord.row >= 0 && coord.row < height

let is_on_grid grid coord =
  coord.col >= 0 && coord.col < grid.width && coord.row >= 0
  && coord.row < grid.height

(* Code for a priority queue sorted by cmp *)

(* Insert an element while maintaining sorted order *)
let rec sorted_insert ~cmp elem lst =
  match lst with
  | [] -> [ elem ] (* Insert into an empty list *)
  | head :: tail ->
      (* Check if the element should be inserted here *)
      if cmp elem head <= 0 then elem :: lst
        (* Insert before the current head *)
      else head :: sorted_insert ~cmp elem tail
(* or else recurse on the rest of the list *)

(* Returns the first element in the queue and the rest of the queue*)
(* as an option. None if the queue is empty *)
let pop = function [] -> None | x :: xs -> Some (x, xs)

(* Peek at the smallest element *)
let peek = function [] -> None | x :: _ -> Some x

(* Check if empty *)
let is_empty queue = List.is_empty queue
