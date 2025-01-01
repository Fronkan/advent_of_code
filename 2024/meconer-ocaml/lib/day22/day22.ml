open Core

let isExample = false

let filename =
  if isExample then "lib/day22/example.txt" else "lib/day22/input.txt"

let input = In_channel.read_lines filename |> List.map ~f:int_of_string
let mix n1 n2 = n1 lxor n2
let prune n = n mod 0x1000000

let next_number n =
  let step1 = prune (mix n (n * 64)) in
  let step2 = prune (mix step1 (step1 / 32)) in
  prune (mix step2 (step2 * 2048))

let next no_of_numbers n =
  let rec loop count n =
    if count = no_of_numbers then n else loop (count + 1) (next_number n)
  in
  loop 0 n

let resultP1 =
  input |> List.map ~f:(fun n -> next 2000 n) |> List.fold ~init:0 ~f:( + )

let seq_of_last_digit total n =
  let dig1 = n mod 10 in
  let rec loop acc_digits_and_changes count n =
    if count = total then List.rev acc_digits_and_changes
    else
      let next = next_number n in
      let prev_digit = n mod 10 in
      let last_digit = next mod 10 in
      let change = last_digit - prev_digit in
      loop ((last_digit, change) :: acc_digits_and_changes) (count + 1) next
  in
  let full_list = loop [ (dig1, 0) ] 0 n in
  List.drop full_list 1
(* We need to drop the first element here since the change sequences start at the second*)

(* Calculates an integer key from a list of 4 elements from -9 to 9 to use in the accumulator map *)
let calc_key_of_seq lst =
  let ofslst = List.map lst ~f:(fun el -> el + 9) in
  let rec loop acc lst =
    match lst with [] -> acc | hd :: tl -> loop ((acc * 100) + hd) tl
  in
  loop 0 ofslst

(* Inverse of the above. Calculates the sequence from an int key. Only used when debugging *)
let calc_seq_of_key key =
  let rec loop acc divisor key_rest =
    if divisor = 0 then List.rev acc
    else
      let ofsn = key_rest / divisor in
      let n = ofsn - 9 in
      loop (n :: acc) (divisor / 100) (key_rest - (ofsn * divisor))
  in
  loop [] 1000000 key

let digit_and_change_seqs =
  List.map input ~f:(fun n -> seq_of_last_digit 2000 n)

let calc_bananas list_of_list_of_d_and_c =
  let seen = Map.empty (module Int) in

  (* Inner loop used to calc bananas for one start value. The idx is used to keep track of which start value
     was used since we stop searching for the same sequence again for the same starting number
  *)
  let rec inner_loop acc_map lst_of_d_and_c idx =
    match lst_of_d_and_c with
    | [] | _ :: [] | [ _; _ ] | [ _; _; _ ] -> acc_map
    | p1 :: p2 :: p3 :: p4 :: tl ->
        let seq_key = calc_key_of_seq [ snd p1; snd p2; snd p3; snd p4 ] in
        let digit_after_seq = fst p4 in
        let new_map =
          Map.update acc_map seq_key ~f:(function
            | None -> (digit_after_seq, idx)
            | Some data ->
                let last_idx_of_entry = snd data in
                if idx <> last_idx_of_entry then
                  (digit_after_seq + fst data, idx)
                else data)
        in
        inner_loop new_map (p2 :: p3 :: p4 :: tl) idx
  in

  let rec outer_loop accMap list_of_list_of_d_and_c idx =
    match list_of_list_of_d_and_c with
    | [] -> accMap
    | list_of_d_and_c :: tail ->
        let new_map = inner_loop accMap list_of_d_and_c idx in
        outer_loop new_map tail (idx + 1)
  in
  outer_loop seen list_of_list_of_d_and_c 0

let banana_map = calc_bananas digit_and_change_seqs

let resultP2 =
  snd
    (Map.fold banana_map ~init:(-1, 0) ~f:(fun ~key:k ~data:v acc ->
         if fst v > snd acc then (k, fst v) else acc))
