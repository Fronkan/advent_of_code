open Core
open Utils

let isExample = false

let filename =
  if isExample then "lib/day20/example.txt" else "lib/day20/input.txt"

let day20Input = In_channel.read_lines filename

let parseInput lines =
  let height = List.length lines in
  let width = String.length (List.hd_exn lines) in
  let gridStr = String.concat lines in
  let arr = String.to_array gridStr in
  let grid = { arr; width; height } in
  grid

let find_char grid c =
  let rec loop i =
    if i = Array.length grid.arr then -1
    else if Char.equal c grid.arr.(i) then i
    else loop (i + 1)
  in
  loop 0

let printGrid grid =
  let rec getRow acc rowNo =
    if rowNo = grid.height then List.rev acc
    else
      let rec getCol acc colNo =
        if colNo = grid.width then String.of_char_list (List.rev acc)
        else
          let c =
            grid.arr.(idx_of_pos_in_grid grid { col = colNo; row = rowNo })
          in

          getCol (c :: acc) (colNo + 1)
      in

      getRow (getCol [] 0 :: acc) (rowNo + 1)
  in
  List.iter (getRow [] 0) ~f:(fun l -> Printf.printf "%s\n" l)

let grid = parseInput day20Input
let dist_arr = Array.(create ~len:(length grid.arr) (-1))

let get_next idx =
  let neighbours =
    List.map allDirections ~f:(fun dir ->
        let nPos = moveOneStep dir (pos_of_idx_in_grid grid idx) in
        idx_of_pos_in_grid grid nPos)
    |> List.filter ~f:(fun pi ->
           dist_arr.(pi) < 0
           && (Char.equal grid.arr.(pi) '.' || Char.equal grid.arr.(pi) 'E'))
  in
  if List.length neighbours = 0 then None
  else
    let next_pi = List.hd_exn neighbours in
    Some next_pi

let make_dist_arr grid =
  let start_i = find_char grid 'S' in
  let end_i = find_char grid 'E' in
  dist_arr.(start_i) <- 0;

  let rec loop curr_dist idx =
    if idx = end_i then dist_arr.(end_i) <- curr_dist
    else (
      dist_arr.(idx) <- curr_dist;
      let next = get_next idx in
      loop (curr_dist + 1) (Option.value_exn next))
  in

  loop 0 start_i;
  (start_i, end_i)

let start_pos_idx, end_pos_idx = make_dist_arr grid

let path_without_cheat =
  let rec loop acc posi curr_dist =
    if posi = end_pos_idx then List.rev acc
    else
      let pos = pos_of_idx_in_grid grid posi in
      let neighbours =
        allDirections
        |> List.map ~f:(fun dir -> moveOneStep dir pos)
        |> List.filter ~f:(fun pos -> is_on_grid grid pos)
      in
      let next =
        List.find neighbours ~f:(fun pos ->
            let pi = idx_of_pos_in_grid grid pos in
            dist_arr.(pi) = curr_dist + 1)
      in
      let next_pi =
        match next with
        | None -> failwith "Err in make_path"
        | Some next -> idx_of_pos_in_grid grid next
      in
      loop (next_pi :: acc) next_pi (curr_dist + 1)
  in
  loop [ start_pos_idx ] start_pos_idx 0

let cheats =
  let rec loop acc path =
    match path with
    | [] -> acc
    | posi :: rest_of_path ->
        let curr_pos = pos_of_idx_in_grid grid posi in
        let curr_dist = dist_arr.(posi) in
        (* Move two steps in every direction and see if it is a valid move *)
        let neighbours =
          allDirections
          |> List.map ~f:(fun dir ->
                 moveOneStep dir curr_pos |> moveOneStep dir)
          |> List.filter ~f:(fun pos ->
                 is_on_grid grid pos
                 && List.exists [ 'E'; '.' ]
                      ~f:(Char.equal grid.arr.(idx_of_pos_in_grid grid pos)))
        in
        let saves =
          List.fold ~init:[] neighbours ~f:(fun acc npos ->
              let nposi = idx_of_pos_in_grid grid npos in
              let ndist = dist_arr.(nposi) in
              if ndist > curr_dist + 2 then acc @ [ ndist - curr_dist - 2 ]
              else acc)
        in
        loop (acc @ saves) rest_of_path
  in

  loop [] path_without_cheat

let resultP1 = List.filter cheats ~f:(fun cheat -> cheat >= 100) |> List.length
let min_time_to_save = if isExample then 50 else 100
let max_cheat_time = 20

let is_free pos =
  List.exists [ '.'; 'E' ] ~f:(fun c ->
      Char.equal grid.arr.(idx_of_pos_in_grid grid pos) c)

let is_wall pos = Char.equal grid.arr.(idx_of_pos_in_grid grid pos) '#'

let manhattan_dist pos1 pos2 =
  abs (pos1.col - pos2.col) + abs (pos1.row - pos2.row)

let search_diamant =
  List.init ((2 * max_cheat_time) + 1) ~f:(fun c -> c - max_cheat_time)
  |> List.fold ~init:[] ~f:(fun acc col ->
         List.init
           ((2 * (max_cheat_time - abs col)) + 1)
           ~f:(fun r ->
             let row = r - (max_cheat_time - abs col) in
             let pos = { col; row } in
             pos)
         @ acc)

let get_reachable start =
  let curr_dist = dist_arr.(start) in
  let start_pos = pos_of_idx_in_grid grid start in
  let reachable_in_max_time =
    List.map search_diamant ~f:(fun pos ->
        { col = pos.col + start_pos.col; row = pos.row + start_pos.row })
  in
  let possible_cheats =
    List.filter reachable_in_max_time ~f:(fun pos ->
        is_on_grid grid pos && is_free pos)
    |> List.filter ~f:(fun pos ->
           dist_arr.(idx_of_pos_in_grid grid pos) - curr_dist
           >= min_time_to_save)
  in
  let savings =
    List.fold possible_cheats ~init:[] ~f:(fun acc pos ->
        let posi = idx_of_pos_in_grid grid pos in
        (* Calculate the path length if travelled through the grid *)
        let norm_dist = dist_arr.(posi) - curr_dist in
        (* The distance if we are cheating *)
        let cheat_dist = manhattan_dist pos start_pos in
        let saving = norm_dist - cheat_dist in
        if saving >= min_time_to_save then saving :: acc else acc)
  in
  savings

let waitForEnter s =
  Printf.printf "%s" s;
  Out_channel.flush stdout;
  let _ = In_channel.(input_line stdin) in
  ()

let cheats_p2_rev2 pos_idx fwd_path =
  let curr_dist_from_startpt = dist_arr.(pos_idx) in
  if curr_dist_from_startpt mod 1000 = 0 then (
    Printf.printf "cdfs %d\n" curr_dist_from_startpt;
    Out_channel.flush stdout);

  let rec loop acc curr_pos_idx path2 =
    match path2 with
    | [] -> acc
    | cheat_pos_idx :: rest_of_path ->
        let dist =
          manhattan_dist
            (pos_of_idx_in_grid grid pos_idx)
            (pos_of_idx_in_grid grid cheat_pos_idx)
        in
        let saving = dist_arr.(cheat_pos_idx) - curr_dist_from_startpt - dist in

        if dist < max_cheat_time && saving >= min_time_to_save then
          loop (saving :: acc) curr_pos_idx rest_of_path
        else loop acc curr_pos_idx rest_of_path
  in
  loop [] pos_idx fwd_path

let cheats_p2 path_without_cheat =
  List.fold ~init:[] path_without_cheat ~f:(fun acc posi ->
      acc @ get_reachable posi)

(* Alternative solution that gives wrong answer. Also slow though *)
let cheats_p2_r2 path_without_cheat =
  let rec loop acc path =
    let fwd_path = List.drop path (min_time_to_save - 1) in
    Printf.printf "LL %d\n" (List.length fwd_path);
    match path with
    | [] -> acc
    | curr_pos :: rest_of_path ->
        let savings = cheats_p2_rev2 curr_pos fwd_path in
        loop (acc @ savings) rest_of_path
  in
  loop [] path_without_cheat

let resultP2 = List.length (cheats_p2 path_without_cheat)

(* let resultP2_2 = List.length (cheats_p2_r2 path_without_cheat) *)
