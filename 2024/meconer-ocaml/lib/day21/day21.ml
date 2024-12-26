open Core

let isExample = false

let filename =
  if isExample then "lib/day21/example.txt" else "lib/day21/input.txt"

let input = In_channel.read_lines filename

let num_robot_keypad =
  [| '7'; '8'; '9'; '4'; '5'; '6'; '1'; '2'; '3'; ' '; '0'; 'A' |]

let num_robot_keypad_width = 3
let dir_robot_keypad = [| ' '; '^'; 'A'; '<'; 'v'; '>' |]
let dir_robot_keypad_width = 3

type posT = { col : int; row : int }
type moveT = { l : int; r : int; u : int; d : int; a : int }

let pos_of_idx_num idx =
  { col = idx mod num_robot_keypad_width; row = idx / num_robot_keypad_width }

let pos_of_idx_dir idx =
  { col = idx mod dir_robot_keypad_width; row = idx / dir_robot_keypad_width }

let numpad_pos_of key =
  let idx =
    fst (Array.findi_exn num_robot_keypad ~f:(fun _ c -> Char.( = ) c key))
  in
  pos_of_idx_num idx

let dirpad_pos_of key =
  let idx =
    fst (Array.findi_exn dir_robot_keypad ~f:(fun _ c -> Char.( = ) c key))
  in
  pos_of_idx_dir idx

let numpad_dist start target =
  let start_pos = numpad_pos_of start in
  let target_pos = numpad_pos_of target in
  (target_pos.col - start_pos.col, start_pos.row - target_pos.row)

let dirpad_dist start target =
  let start_pos = dirpad_pos_of start in
  let target_pos = dirpad_pos_of target in
  (target_pos.col - start_pos.col, start_pos.row - target_pos.row)

type directionT = Up | Down | Left | Right | None

(*
   Best directions according to reddit post
    https://www.reddit.com/r/adventofcode/comments/1hjgyps/2024_day_21_part_2_i_got_greedyish/
*)
let direction_of_numpad_move start_pos end_pos =
  if
    start_pos.row = 3 && end_pos.row < 3 && start_pos.col > 0 && end_pos.col = 0
  then (Up, Left)
  else if
    end_pos.row = 3 && start_pos.row < 3 && start_pos.col = 0 && end_pos.col > 0
  then (Right, Down)
  else
    let isUp = end_pos.row < start_pos.row in
    let isDown = end_pos.row > start_pos.row in
    let isLeft = end_pos.col < start_pos.col in
    let isRight = end_pos.col > start_pos.col in
    match (isUp, isDown, isLeft, isRight) with
    | true, false, false, false -> (Up, None)
    | false, true, false, false -> (Down, None)
    | false, false, true, false -> (Left, None)
    | false, false, false, true -> (Right, None)
    | true, false, true, false -> (Left, Up)
    | false, true, true, false -> (Left, Down)
    | false, true, false, true -> (Down, Right)
    | true, false, false, true -> (Up, Right)
    | _, _, _, _ -> failwith "Illegal move in direction_of_numpad_move"

let move_str_of_numpad_moves moves start_pos end_pos =
  let dirs = direction_of_numpad_move start_pos end_pos in
  let s =
    match dirs with
    | Up, Left -> String.make moves.u '^' ^ String.make moves.l '<'
    | Right, Down -> String.make moves.r '>' ^ String.make moves.d 'v'
    | Up, None -> String.make moves.u '^'
    | Down, None -> String.make moves.d 'v'
    | Left, None -> String.make moves.l '<'
    | Right, None -> String.make moves.r '>'
    | Left, Up -> String.make moves.l '<' ^ String.make moves.u '^'
    | Left, Down -> String.make moves.l '<' ^ String.make moves.d 'v'
    | Down, Right -> String.make moves.d 'v' ^ String.make moves.r '>'
    | Up, Right -> String.make moves.u '^' ^ String.make moves.r '>'
    | _, _ -> failwith "Illegal move combo in move_str_of_numpad_moves"
  in
  s ^ "A"

let direction_of_dirpad_move start_pos end_pos =
  if start_pos.col = end_pos.col && start_pos.row = end_pos.row then (None, None)
  else if start_pos.row = 1 && start_pos.col = 0 && end_pos.row = 0 then
    (Right, Up)
  else if end_pos.row = 1 && end_pos.col = 0 && start_pos.row = 0 then
    (Down, Left)
  else
    let isUp = end_pos.row < start_pos.row in
    let isDown = end_pos.row > start_pos.row in
    let isLeft = end_pos.col < start_pos.col in
    let isRight = end_pos.col > start_pos.col in
    match (isUp, isDown, isLeft, isRight) with
    | true, false, false, false -> (Up, None)
    | false, true, false, false -> (Down, None)
    | false, false, true, false -> (Left, None)
    | false, false, false, true -> (Right, None)
    | true, false, true, false -> (Left, Up)
    | false, true, true, false -> (Left, Down)
    | false, true, false, true -> (Down, Right)
    | true, false, false, true -> (Up, Right)
    | _, _, _, _ -> failwith "Illegal move in direction_of_dirpad_move"

let move_str_of_dirpad_moves moves start_pos end_pos =
  let dirs = direction_of_dirpad_move start_pos end_pos in
  let s =
    match dirs with
    | Right, Up -> String.make moves.r '>' ^ String.make moves.u '^'
    | Down, Right -> String.make moves.d 'v' ^ String.make moves.r '>'
    | Up, None -> String.make moves.u '^'
    | Down, None -> String.make moves.d 'v'
    | Left, None -> String.make moves.l '<'
    | Right, None -> String.make moves.r '>'
    | Left, Up -> String.make moves.l '<' ^ String.make moves.u '^'
    | Left, Down -> String.make moves.l '<' ^ String.make moves.d 'v'
    | Up, Right -> String.make moves.u '^' ^ String.make moves.r '>'
    | Down, Left -> String.make moves.d 'v' ^ String.make moves.l '<'
    | None, None -> ""
    | _, _ -> failwith "Illegal move combo in move_str_of_dirpad_moves"
  in
  s ^ "A"

let num_pad_moves (x, y) start_pos end_pos =
  let moves =
    if x < 0 then { l = -x; r = 0; u = 0; d = 0; a = 0 }
    else { l = 0; r = x; u = 0; d = 0; a = 0 }
  in
  let moves =
    if y < 0 then { moves with d = moves.d - y }
    else { moves with u = moves.u + y }
  in
  (* And last the a move for the push *)
  let moves = { moves with a = 1 } in
  move_str_of_numpad_moves moves start_pos end_pos

let dir_pad_moves (x, y) start_pos end_pos =
  let moves =
    if x < 0 then { l = -x; r = 0; u = 0; d = 0; a = 0 }
    else { l = 0; r = x; u = 0; d = 0; a = 0 }
  in
  let moves =
    if y < 0 then { moves with d = moves.d - y }
    else { moves with u = moves.u + y }
  in
  (* And last the a move for the push *)
  let moves = { moves with a = 1 } in
  move_str_of_dirpad_moves moves start_pos end_pos

let press_key_on_numpad_pair curr_key next_key =
  let dist = numpad_dist curr_key next_key in
  let start_pos = numpad_pos_of curr_key in
  let new_pos = numpad_pos_of next_key in
  let move_str = num_pad_moves dist start_pos new_pos in
  (next_key, move_str)

let press_key_on_dirpad_pair curr_key next_key =
  let dist = dirpad_dist curr_key next_key in
  let start_pos = dirpad_pos_of curr_key in
  let new_pos = dirpad_pos_of next_key in
  let move_str = dir_pad_moves dist start_pos new_pos in
  (next_key, move_str)

let press_key_on_dirpad curr_key next_key =
  let dist = dirpad_dist curr_key next_key in
  let start_pos = dirpad_pos_of curr_key in
  let new_pos = dirpad_pos_of next_key in
  let move_str = dir_pad_moves dist start_pos new_pos in
  move_str

let numpad_moves_of_seq seqstr =
  let seq = String.to_list seqstr in
  let rec loop curr_key acc seq =
    match seq with
    | [] -> acc
    | next_key :: rest_of_seq ->
        let new_curr_key, moves = press_key_on_numpad_pair curr_key next_key in
        loop new_curr_key (acc ^ moves) rest_of_seq
  in
  loop 'A' "" seq

let dirpad_moves_of_numpad_seq seqstr =
  let numpad_moves = numpad_moves_of_seq seqstr in
  let seq = String.to_list numpad_moves in
  let rec loop curr_key acc seq =
    match seq with
    | [] -> acc
    | next_key :: rest_of_seq ->
        let new_curr_key, moves = press_key_on_dirpad_pair curr_key next_key in
        loop new_curr_key (acc ^ moves) rest_of_seq
  in
  loop 'A' "" seq

let memo = ref (Map.empty (module String))

let memo_key_of depth c1 c2 =
  string_of_int depth ^ String.of_list [ ':'; c1; c2 ]

let rec len_of_dirpad_pair depth c1 c2 =
  if depth = 1 then 1
  else
    let memo_key = memo_key_of depth c1 c2 in
    if Map.mem !memo memo_key then Map.find_exn !memo memo_key
    else
      let presses = press_key_on_dirpad c1 c2 in
      let list = 'A' :: String.to_list presses in
      let rec loop acc list =
        match list with
        | _ :: [] -> acc
        | [] -> failwith "List is empty in seq_of"
        | c1 :: c2 :: tail ->
            let l = len_of_dirpad_pair (depth - 1) c1 c2 in
            loop (acc + l) (c2 :: tail)
      in
      let l = loop 0 list in
      memo := Map.set !memo ~key:memo_key ~data:l;
      l

let seq_length_of_last_key_presses_p2 seqstr no_of_robots =
  let s = dirpad_moves_of_numpad_seq seqstr in

  let list = 'A' :: String.to_list s in
  let rec loop lacc list =
    match list with
    | _ :: [] -> lacc
    | [] -> failwith "List empty in seq_length_of_last_key_presses_p2"
    | c1 :: c2 :: tail ->
        let l = len_of_dirpad_pair no_of_robots c1 c2 in
        loop (lacc + l) (c2 :: tail)
  in
  let length = loop 0 list in
  length

let lengths_p1 =
  List.map input ~f:(fun line -> seq_length_of_last_key_presses_p2 line 2)

let num_values =
  List.map input ~f:(fun s ->
      int_of_string (String.sub s ~pos:0 ~len:(String.length s - 1)))

let resultP1 =
  List.fold2_exn lengths_p1 num_values ~init:0 ~f:(fun acc l v -> acc + (l * v))

let lengths_p2 =
  List.map input ~f:(fun line -> seq_length_of_last_key_presses_p2 line 25)

let resultP2 =
  List.fold2_exn lengths_p2 num_values ~init:0 ~f:(fun acc l v -> acc + (l * v))
