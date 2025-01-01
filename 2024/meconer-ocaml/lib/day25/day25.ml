open Core

let isExample = false

let filename =
  if isExample then "lib/day25/example.txt" else "lib/day25/input.txt"

let aoc_input = In_channel.read_lines filename

let group_input lines =
  let rec loop groups acc lines =
    match lines with
    | [] -> List.rev (List.rev acc :: groups)
    | line :: tail when String.is_empty line ->
        let section = List.rev acc in
        loop (section :: groups) [] tail
    | line :: tail -> loop groups (line :: acc) tail
  in

  loop [] [] lines

let is_lock group = String.equal "#####" (List.hd_exn group)

let key_or_lock_of_group group =
  let rec loop acc n =
    if n = 5 then List.rev acc
    else
      let count =
        List.fold group ~init:0 ~f:(fun acc line ->
            if Char.equal (String.get line n) '#' then acc + 1 else acc)
      in
      loop ((count - 1) :: acc) (n + 1)
  in
  loop [] 0

let parse_input aoc_input =
  let groups = group_input aoc_input in
  let rec loop accKeys accLocks groups =
    match groups with
    | [] -> (accKeys, accLocks)
    | group :: rest ->
        if is_lock group then
          loop accKeys (key_or_lock_of_group group :: accLocks) rest
        else loop (key_or_lock_of_group group :: accKeys) accLocks rest
  in

  loop [] [] groups

let keys, locks = parse_input aoc_input

let does_key_fit_in_lock key lock =
  not
    (List.exists2_exn key lock ~f:(fun k_height l_pin -> k_height + l_pin > 5))

let count_fits keys locks =
  let rec key_loop acc lock keys =
    match keys with
    | [] -> acc
    | key :: tail ->
        if does_key_fit_in_lock key lock then key_loop (acc + 1) lock tail
        else key_loop acc lock tail
  in

  let rec lock_loop acc locks =
    match locks with
    | [] -> acc
    | lock :: tail ->
        let no_of_fitting_keys = key_loop 0 lock keys in
        lock_loop (no_of_fitting_keys + acc) tail
  in
  lock_loop 0 locks

let resultP1 = count_fits keys locks
let resultP2 = 0
