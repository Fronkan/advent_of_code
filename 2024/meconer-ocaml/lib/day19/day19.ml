open Core

let isExample = false

let filename =
  if isExample then "lib/day19/example.txt" else "lib/day19/input.txt"

let day19Input = In_channel.read_lines filename

let split_at_empty_line lines =
  let rec inner before after = function
    | [] ->
        (List.rev before, List.rev after) (* Reverse both lists at the end *)
    | "" :: rest -> (List.rev before, rest) (* Empty line found: split *)
    | line :: rest -> inner (line :: before) after rest
  in
  inner [] [] lines

let parseTowels towelStr =
  let towels = String.substr_replace_all towelStr ~pattern:" " ~with_:"" in
  String.split towels ~on:','

let parseInput lines =
  let avTowelStr, wantedPatterns = split_at_empty_line lines in
  let towels = parseTowels (List.hd_exn avTowelStr) in
  (towels, wantedPatterns)

let towels, wantedPatterns = parseInput day19Input

(* Get a list of towels that fits the start of the wanted pattern *)
let getTowelsForStartOfPattern pattern towels =
  let rec loop accTowels list =
    match list with
    | [] -> accTowels
    | towel :: rest ->
        let lenOfT = String.length towel in
        if lenOfT > String.length pattern then loop accTowels rest
        else
          let startOfP = String.sub pattern ~pos:0 ~len:lenOfT in
          if String.equal startOfP towel then loop (towel :: accTowels) rest
          else loop accTowels rest
  in
  loop [] towels

(* Drop n characters from start of string *)
let string_drop n s =
  let len = String.length s in
  if len < n then None else Some (String.sub s ~pos:n ~len:(len - n))

let isPossible towels wantedPattern =
  let rec tryLoop patternSoFar restOfStr =
    if String.equal wantedPattern patternSoFar then true
    else
      let possibleTowelsForRest = getTowelsForStartOfPattern restOfStr towels in
      if List.is_empty possibleTowelsForRest then false
      else
        List.exists possibleTowelsForRest ~f:(fun towel ->
            let rest =
              Option.value_exn (string_drop (String.length towel) restOfStr)
            in
            tryLoop (patternSoFar ^ towel) rest)
  in

  tryLoop "" wantedPattern

let memo = ref (Map.empty (module String))

let countPossibleCombos towels wantedPattern =
  let rec tryLoop count patternSoFar restOfStr =
    (* Printf.printf "%d %s %s\n" count patternSoFar restOfStr; *)
    match Map.find !memo restOfStr with
    | Some n -> n (* Found in memo *)
    | None ->
        if String.equal wantedPattern patternSoFar then 1
          (* Ready. We found 1 possible combo *)
        else
          let possibleTowelsForRest =
            getTowelsForStartOfPattern restOfStr towels
          in
          if List.is_empty possibleTowelsForRest then count
          else
            count
            + List.fold ~init:0 possibleTowelsForRest ~f:(fun acc towel ->
                  let rest =
                    Option.value_exn
                      (string_drop (String.length towel) restOfStr)
                  in
                  let newCount = tryLoop 0 (patternSoFar ^ towel) rest in
                  if newCount > 0 && not (String.is_empty rest) then
                    memo := Map.set !memo ~key:rest ~data:newCount;
                  acc + newCount)
  in

  tryLoop 0 "" wantedPattern

let possible = List.map wantedPatterns ~f:(isPossible towels)

let resultP1 =
  List.fold ~init:0 possible ~f:(fun acc b -> acc + if b then 1 else 0)

let counts = List.map wantedPatterns ~f:(countPossibleCombos towels)
let resultP2 = List.fold ~init:0 counts ~f:(fun acc c -> acc + c)
