open Core 

(* let filename = "lib/day13/example.txt" *)
let filename = "lib/day13/input.txt"
let day13Input = In_channel.read_lines filename

let parseLineGroup lines =
  let rec loop groupAcc lines =
    match lines with 
    | [] -> List.rev groupAcc
    | buttonALine::buttonBLine::prizeLine::(""::tl | tl) -> 
      loop ((buttonALine,buttonBLine,prizeLine)::groupAcc) tl
    | _::_::[] | _::[]-> failwith "Wrong no of lines"
  in
  loop [] lines

type crdType = {x:int; y:int}
type clawMachineType = { btnA:crdType; btnB:crdType; prize:crdType}

let parseLine buttonLine =
  let valsRegex = Re2.create_exn "X.(\\d+), Y.(\\d+)" in
  let matchArr = Re2.find_submatches_exn  valsRegex buttonLine in
  let x = match matchArr.(1) with
  | None -> failwith "No X"
  | Some xs -> int_of_string xs in
  let y = match matchArr.(2) with
  | None -> failwith "No X"
  | Some ys -> int_of_string ys in
  {x;y}

let groups = parseLineGroup day13Input
  

let makeClawMachine lineGroup =
  let buttonALine, buttonBLine, prizeLine = lineGroup in
  let btnA = parseLine buttonALine in 
  let btnB = parseLine buttonBLine in 
  let prize = parseLine prizeLine in
  {btnA; btnB; prize}

let clawMachines = List.map groups ~f:(makeClawMachine)

let tryGame r a b =
  (* r = n1 * a + n2 * b *)
  let n2 = min 100 (r / b) in

  let rec loop acc n2 =
    if n2 = 0 then acc
    else ( 
      let n1 = (r - n2 * b) / a in
      if (n1 * a + n2 * b = r) then
         loop ((n1,n2)::acc) (n2-1)
      else
         loop acc (n2-1)
    )
    in

  loop [] n2


let testClawMachine clawMachine =
  let px = clawMachine.prize.x in
  let py = clawMachine.prize.y in
  let ax = clawMachine.btnA.x in
  let ay = clawMachine.btnA.y in
  let bx = clawMachine.btnB.x in
  let by = clawMachine.btnB.y in
  let xResList = tryGame px ax bx in
  let resList = List.filter ~f:(fun (n1,n2) -> n1*ay + n2*by = py) xResList in
  resList

let possibleListForAllMachines = List.map clawMachines ~f:(testClawMachine)

let cost res = 3 * (fst res) + (snd res)

let costList lst = List.map lst ~f:(cost)

let minCostList = List.map possibleListForAllMachines ~f:(fun possibleList -> 
   if List.length possibleList = 0 then 0 
   else List.fold possibleList ~init:999999 ~f:(fun acc possible -> min acc (cost possible))
  ) 

let resultP1 = List.fold minCostList ~init:0 ~f:(+)

let p2Extra = 10000000000000

let p2ClawMachines = List.map clawMachines ~f:(fun clawMachine ->
  let px = clawMachine.prize.x + p2Extra in
  let py = clawMachine.prize.y + p2Extra in
  let newPrize = {x=px;y=py} in
  {clawMachine with prize=newPrize}
  )

(* equation system:
  n1 * ax + n2 * bx = px
  n1 * ay + n2 * by = py

  Two equations, two unknown
  n1 = (px - n2 * bx) / ax
  ay / ax * (px - n2 * bx) + n2 * by = py
  px * ay / ax - n2 * bx * ay / ax + n2 * by = py
  n2 *( by - bx * ay / ax ) = py -  px * ay / ax
  n2 = (py -  px * ay / ax) / ( by - bx * ay / ax )

  try with part 1 example 
  n2 = (5400 - 8400 * 34/94) / ( 67 - 22 * 94 / 34) = 2361.702 /  
*)

let findN1AndN2 clawMachine = 
  let px = float_of_int clawMachine.prize.x in
  let py = float_of_int clawMachine.prize.y in
  let ax = float_of_int clawMachine.btnA.x in
  let ay = float_of_int clawMachine.btnA.y in
  let bx = float_of_int clawMachine.btnB.x in
  let by = float_of_int clawMachine.btnB.y in
  let n2 = (py -. px *. (ay /. ax)) /. (by -. bx *. ay /. ax ) in
  let n1 = (px -. n2 *. bx) /. ax in
  n1,n2


let tryMachine clawMachine n1 n2 =
  let n1i = int_of_float (Float.round_nearest n1) in
  let n2i = int_of_float (Float.round_nearest n2) in
  let px = clawMachine.prize.x in
  let py = clawMachine.prize.y in
  let ax = clawMachine.btnA.x in
  let ay = clawMachine.btnA.y in
  let bx = clawMachine.btnB.x in
  let by = clawMachine.btnB.y in
  ( n1i * ax + n2i * bx = px ) && ( n1i * ay + n2i * by = py )


let testClawMachineP2 clawMachine =
  let n1,n2 = findN1AndN2 clawMachine in
  if tryMachine clawMachine n1 n2 then 
    3 * (int_of_float (Float.round_nearest n1)) + (int_of_float (Float.round_nearest n2) )
  else
    0

let costs = List.map ~f:(testClawMachineP2) p2ClawMachines

let resultP2 = List.fold costs ~init:0 ~f:(+)
