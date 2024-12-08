open Core 

(* let filename = "lib/day8/example.txt" *)
let filename = "lib/day8/input.txt"
let day8Input = In_channel.read_lines filename

let width = String.length (List.hd_exn day8Input)
let height = List.length day8Input

type pos = { col : int; row : int }
let string_of_pos p = string_of_int p.col ^ ":" ^ string_of_int p.row

let isOnGrid pos = 
  pos.col >= 0 && pos.col < width && pos.row >= 0 && pos.row < height

type antAndPos = {pos:pos; antenna:char}

let findAntennaInLine lineNo line =
  let cols = String.to_list line in

  let rec innerCol accCol colNo cols = 
  match cols with 
  | [] -> List.rev accCol
  | c::t -> if Char.equal c '.' then 
              innerCol accCol (colNo+1) t 
             else 
              let pos = {col=colNo; row = lineNo} in
              let antennaAndPos = {pos;antenna=c} in
              innerCol (antennaAndPos::accCol) (colNo+1) t
  in
  innerCol [] 0 cols

let findAntennas inputLines = 
  let rec innerLine accLine lineNo lines =
    match lines with 
    | [] -> List.rev accLine
    | l::t -> innerLine ((findAntennaInLine lineNo l) @ accLine) (lineNo+1) t
  in
  innerLine [] 0 inputLines

let antennas = findAntennas day8Input

let findAntennaNames antennas = 
  let rec inner acc antennas =
    match antennas with 
    | [] -> acc
    | antenna::t -> if List.exists ~f:(fun a -> Char.equal a antenna.antenna) acc then
                      inner acc t
                    else
                      inner (antenna.antenna::acc) t
    in
    inner [] antennas

let antennaNames = findAntennaNames antennas

let antinodePositions = Set.empty (module String)

let findAntinodePositions p1 p2 =
  let dCol = p2.col - p1.col in
  let dRow = p2.row - p1.row in
  let anPos1 = {col = p1.col + 2 * dCol ; row = p1.row + 2 * dRow} in
  let anPos2 = {col = p2.col - 2 * dCol ; row = p2.row - 2 * dRow} in
  [anPos1;anPos2]

let findAntinodePositionsP2 p1 p2 =
  let dCol = p2.col - p1.col in
  let dRow = p2.row - p1.row in
  let rec negatives acc pos =
    if (isOnGrid pos) then 
      negatives (pos::acc) { col = pos.col - dCol ; row = pos.row - dRow}
    else 
      acc
    in

  let rec positives acc pos =
    if (isOnGrid pos) then 
      positives (pos::acc) { col = pos.col + dCol ; row = pos.row + dRow}
    else 
      acc
    in
  
  let res = (negatives [] p2) @ (positives [] p1) in
    (* List.iter ~f:(fun r -> Printf.printf "POS: %s\n" (string_of_pos r)) res; *)
  res 

let findAntinodesInList p1 posList findFun = 
  let rec inner acc lst =
    match lst with 
    | [] -> acc
    | p::tl -> let anPoss = findFun p1 p in
                   inner (anPoss @ acc) tl 
  in
  inner [] posList

let findAntinodes antennaList findFun =
  let rec inner acc lst =
    match lst with 
    | [] | _::[] -> acc
    | p1::t -> let anPoss = findAntinodesInList p1 t findFun in
                   inner (anPoss @ acc) t
  in inner [] antennaList

let findAllAntinodes findFun =
  let rec inner acc antennaNames =
    match antennaNames with 
    | [] -> acc
    | name::tl -> let antennasWithSameName = List.filter ~f:(fun antenna -> Char.equal antenna.antenna name) antennas in
                  let posList = List.map ~f:(fun antenna -> antenna.pos ) antennasWithSameName in
                  let antinodePositions = findAntinodes posList findFun in
                  inner (antinodePositions@acc) tl
  in inner [] antennaNames

let lst = List.filter ~f:(fun a -> Char.(=) a.antenna 'A' ) antennas
let posList = List.map ~f:(fun a -> a.pos) lst

let antiNodes = findAllAntinodes findAntinodePositions

let antiNodesOnGrid = List.filter ~f:(isOnGrid) antiNodes

let antiNodesOnGridAsStrings = List.map ~f:(string_of_pos) antiNodesOnGrid

let nodeSet = Set.of_list (module String) antiNodesOnGridAsStrings

let resultP1 =  Set.length nodeSet

let antiNodes = findAllAntinodes findAntinodePositionsP2

let antiNodesOnGrid = List.filter ~f:(isOnGrid) antiNodes

let antiNodesOnGridAsStrings = List.map ~f:(string_of_pos) antiNodesOnGrid

let nodeSet = Set.of_list (module String) antiNodesOnGridAsStrings
let resultP2 =  Set.length nodeSet