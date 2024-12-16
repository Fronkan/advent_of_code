open Core 


let isExample = false
let filename = if isExample then
   "lib/day14/example.txt" 
  else
    "lib/day14/input.txt"

let width = if isExample then 11 else 101
let height = if isExample then 7 else 103

let day14Input = In_channel.read_lines filename

type pos = { x : int; y : int } 

let idx_of_pos pos = pos.x + width * pos.y

let pos_of_idx idx = {x = idx mod width; y = idx / width}

type robotType = {pos:pos; vel:pos}

let getXYFromStr s = 
  let cPart = String.split ~on:',' (String.sub s ~pos:2 ~len:(String.length s - 2) ) in
  let x = int_of_string (List.hd_exn cPart) in
  let y = int_of_string (List.last_exn cPart) in
  x,y

let getRobotFromLine line = 
  let parts = String.split line ~on:' ' in
  let posPart = List.hd_exn parts in
  let pos = getXYFromStr posPart in
  let velPart = List.last_exn parts in
  let vel = getXYFromStr velPart in
  let robot = {pos = {x = fst pos; y = snd pos}; vel = {x = fst vel; y = snd vel}} in
  robot

let robots = List.rev 
  (List.fold day14Input ~init:[] ~f:(fun acc line -> 
    (getRobotFromLine line)::acc))

let wrap x y =  let res = x mod y in if res < 0 then res + y else res;;

let doOneMove robots = 
  List.fold robots ~init:[] ~f:(fun acc robot ->
    let newPos = 
      {x = wrap (robot.pos.x + robot.vel.x)  width;
       y = wrap (robot.pos.y + robot.vel.y) height} in
      {pos=newPos; vel= robot.vel}::acc
    )


let getNoOfRobotsHere robots pos = 
  List.count robots ~f:(fun robot -> 
      robot.pos.x = pos.x && robot.pos.y = pos.y
    )
let getMap robots =
  let rec getRow acc rowNo =
    if rowNo = height then List.rev acc
    else 
      let rec getCol acc colNo =
        if colNo = width then List.rev acc
        else
          let n = getNoOfRobotsHere robots {x=colNo;y=rowNo} in
          
          getCol (n :: acc) (colNo + 1)

      in
      getRow ((getCol [] 0)::acc) (rowNo + 1)

  in
  getRow [] 0

let doNMoves noOfIterations robots =
  let rec loop robots n =
    if n  = noOfIterations then robots
    else 
      let robots = doOneMove robots in
      loop robots (n+1)
  in
  loop robots 0

let getPositionsOfQuadrant quadNo =
  let rec loopY acc y =
    if y = height / 2 then List.rev acc
    else 
      let rec loopX acc x y =
        if x = width / 2 then acc
        else loopX ({x;y}::acc) (x + 1) y
      in
    loopY ((loopX [] 0 y)@acc) (y+1)
  in
  let lstOfQuad0 = loopY [] 0 in
  let ofsX = if quadNo = 0 || quadNo = 2 then 0 else width / 2 + 1 in
  let ofsY = if quadNo = 0 || quadNo = 1 then 0 else height / 2 + 1 in

  List.map lstOfQuad0 ~f:(fun p -> { x = p.x + ofsX; y = p.y + ofsY } )

  
let countRobotsInQuadrants robots = 
  [0;1;2;3] 
  |> List.map ~f:(getPositionsOfQuadrant)
  |> List.map ~f:(fun quadrant -> 
    List.fold quadrant ~init:0 ~f:(fun acc pos ->
    acc + (getNoOfRobotsHere robots pos)))

let robotsAfter100Moves = doNMoves 100 robots


let printMap robots = 
  let map = getMap robots in
  List.iter map ~f:(fun robotList ->
      let line = List.fold robotList ~init:"" ~f:(fun acc rCount ->
        acc ^ (if rCount = 0 then "." else "#")
        ) in
      Out_channel.output_line stdout line;
    )

let isCandidate robots = 
  let y = height / 2 in
  let x = width / 2 - 5 in
  let startIdx = idx_of_pos {x;y} in
  let lengthToCheck = 10 in
  
  let rec check robots idx =
    if idx = startIdx + lengthToCheck then true
    else 
      let count = getNoOfRobotsHere robots (pos_of_idx idx) in
      if count = 0 then false 
      else check robots (idx+1)

  in
  check robots startIdx



let findTree startMoveNo robots = 
  let rec tryFind robots n =

    (* if isCandidate robots then  *)
      let _ = printMap robots in
      Printf.printf "After step: %d\n" n;
      Out_channel.flush stdout;
      tryFind (doOneMove robots) (n+1)
    (* else tryFind (doOneMove robots) (n+1) *)
  in
  tryFind robots startMoveNo


let rec showMap robots n =
  let nRobots = doOneMove robots in
  printMap nRobots;
  Printf.printf "Stepno: %d\n" n;
  Out_channel.flush stdout;
  
  let s = In_channel.(input_line stdin) in
  match s with 
  | None -> 1
  | Some s -> if String.length s > 0  then 
      2
    else
      showMap nRobots (n+1)
  

let resultP1 = List.fold (countRobotsInQuadrants robotsAfter100Moves) ~init:1 ~f:( * )

(* 
Steps 1942 1959 2043 2062 2144
Fount interesting steps by looking. Cycle found at 
1942 + n * 101
*)

let start = 1942
let cycleLength = 101

let doCycles start cycleLength robots =
  let rec loop n robots =
    let nRobots = doNMoves cycleLength robots in
    if isCandidate nRobots then 
     ( printMap nRobots;
      Printf.printf "Stepno: %d\n" (n+cycleLength);
      (n+cycleLength))
    else
      loop (n+cycleLength) nRobots 
  in

  let robots = doNMoves start robots in
  loop start robots

let resultP2 = doCycles start cycleLength robots

