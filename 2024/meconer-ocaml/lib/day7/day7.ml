open Core 

(* let filename = "lib/day7/example.txt" *)
let filename = "lib/day7/input.txt"
let day7Input = In_channel.read_lines filename

let getResultsAndValues line = 
  let parts = String.split ~on:':' line in
  match parts with 
  | res::valuesPart::[] -> let values = String.split ~on:' ' (String.strip valuesPart) in
                          int_of_string res, (List.map ~f:int_of_string values)
  | _ -> raise (Failure "Illegal format")

let resAndValues = List.map ~f:getResultsAndValues day7Input


let concAB a b = int_of_string (string_of_int a ^ string_of_int b)

let getAllResultsP2 a b = [ a + b ; a * b ; concAB a b]
let getAllResultsP1 a b = [ a + b ; a * b ]

let getAllPossibleResults lst resFun= 
  let lst = List.rev lst in
  let rec inner lst =
  match lst with 
  | a::b::[] -> resFun b a
  | a::tl -> let opsRes = inner tl in
             let combined = List.map ~f:(fun b -> resFun b a) opsRes in
             List.concat combined
  | [] -> raise (Failure "At least two elements")
  in
  inner lst

let getResultIfOk resultAndValues resFun = 
  let result = fst resultAndValues in
  let values = snd resultAndValues in
  let allPossibleResults =  getAllPossibleResults values resFun in
  let isOk = List.exists ~f:(fun r -> r = result) allPossibleResults in
  if isOk then result else 0

let okSumsP1 = List.map ~f:(fun rav -> getResultIfOk rav getAllResultsP1) resAndValues 
let resultP1 =  List.fold ~f:(+) ~init:0 okSumsP1
let okSumsP2 = List.map ~f:(fun rav -> getResultIfOk rav getAllResultsP2) resAndValues 
let resultP2 =  List.fold ~f:(+) ~init:0 okSumsP2