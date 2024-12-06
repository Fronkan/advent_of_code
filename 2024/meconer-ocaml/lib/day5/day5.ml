open Core 

(* let filename = "lib/day5/example.txt" *)
let filename = "lib/day5/input.txt"
let day5Input = In_channel.read_lines filename

let split_at_empty_line lines =
  let rec inner before after = function
    | [] -> (List.rev before, List.rev after)  (* Reverse both lists at the end *)
    | "" :: rest -> (List.rev before, rest)   (* Empty line found: split *)
    | line :: rest -> inner (line :: before) after rest
  in
  inner [] [] lines

let rules, updates = split_at_empty_line day5Input

(* Get a tuple with the first two elements of a list*)
let getPair lst = 
  let a = List.hd_exn lst in
  let b = List.hd_exn (List.tl_exn lst) in
  a,b
  

(* Converts a list of rules to a list of int tuples  *)
let convertRules rules = 
  let pairsAsStrs = List.map ~f:(fun line -> String.split ~on:'|' line) rules in
  let pairsAsInts = List.map ~f:(fun lst -> List.map ~f:(int_of_string) lst) pairsAsStrs in
  List.map ~f:getPair pairsAsInts 

let rules = convertRules rules

(*Convert a list of updates in string form f.ex "1,2,3,4" to a list of list of strings*)
let convertUpdates updates = 
  let updatesAsStrLists = List.map ~f:(fun line -> String.split ~on:',' line) updates in
  let updatesAsIntLists = List.map ~f:(fun lst -> List.map ~f:(int_of_string) lst) updatesAsStrLists in
  updatesAsIntLists
  
let updates = convertUpdates updates

(* Checks two numbers to see if they are in order*)
let rec isInOrder num1 num2 rules =
    match rules with 
    | [] -> true
    | hd::tl -> if (num1 = snd hd && num2 = fst hd) then false else isInOrder num1 num2 tl

let checkNum num lst  =
  let res = List.exists ~f:(fun el -> not (isInOrder num el rules)) lst in
  not res


let rec checkOrder rules update = 
  match update with
  | [] -> true
  | h::t ->   if checkNum h t then checkOrder rules t else false

let updatesOk = List.map ~f:(fun update -> checkOrder rules update) updates

let getMiddleElement lst = 
  let middle = (List.length lst) / 2 in
  let rec inner l = 
    match l with 
    | h::t when middle = List.length t -> h
    | _::t -> inner t
    | [] -> -1
  in inner lst

let middles = List.map ~f:(fun lst -> getMiddleElement lst) updates

let middleElements = List.map2_exn ~f:( fun num isOk -> if isOk then num else 0) middles updatesOk

let resultP1 = List.fold ~init:0 ~f:( + ) middleElements

let getNotOkUpdates updates updatesOk= 
  let rec inner accum updates updatesOk= 
    match updates, updatesOk with 
    | [],_ -> accum
    | _,[] -> accum
    | h::t , hO::tO -> if hO then (inner accum t tO ) else inner (h::accum) t tO
  in
  inner [] updates updatesOk

let notOkUpdates = getNotOkUpdates updates updatesOk

let sortFunc num1 num2 =
  if  isInOrder num1 num2 rules then -1 else 1

let corrected = List.map ~f:(fun lst -> List.sort ~compare:sortFunc lst) notOkUpdates

let correctedMiddles = List.map ~f:(fun lst -> getMiddleElement lst) corrected

let resultP2 = List.fold ~init:0 ~f:(+) correctedMiddles

