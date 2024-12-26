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

let resultP2 = 0
