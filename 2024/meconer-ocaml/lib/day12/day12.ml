open Core 

(* let filename = "lib/day12/example.txt" *)
let filename = "lib/day12/input.txt"
let day12Input = In_channel.read_lines filename

let width = String.length (List.hd_exn day12Input)
let height = List.length day12Input

type pos = { col : int; row : int } 

let idx_of_pos pos = pos.col + width * pos.row

let pos_of_idx idx = {col = idx mod width; row = idx / width}

let grid = (List.map ~f:String.to_list day12Input) 
  |> (List.fold ~init:[] ~f:(@)) 
  |> Array.of_list

type direction = North | East | South | West

let string_of_pos p = string_of_int p.col ^ ":" ^ string_of_int p.row

let string_of_dir  = function
  | North -> "N"
  | East -> "E"
  | West -> "W"
  | South -> "S"

let allDirections=[North;East;South;West]

let moveOneStep direction coord =
  match direction with 
  | North -> {coord with row = coord.row - 1}
  | South -> {coord with row = coord.row + 1}
  | West -> {coord with col = coord.col - 1}
  | East -> {coord with col = coord.col + 1}

let isOnGrid coord = 
  coord.col >= 0 && coord.col < width && coord.row >=0 && coord.row < height

let getPlantAtPos pos = 
  if isOnGrid pos then Some grid.((idx_of_pos pos))
  else None

let findPerimsOfPos pos = 
  let plantHere = match getPlantAtPos pos with 
    | None -> raise (Failure "OffGrid!")
    | Some plant -> plant
  in
  let perims = List.fold allDirections ~init:[] ~f:(fun acc direction ->
      let neighbourPos = moveOneStep direction pos in
      let neighbourPlant = getPlantAtPos neighbourPos in
      match neighbourPlant with
      | None -> (* Offgrid, no plant here. Add fence*) 
              direction::acc
      | Some plant -> (* Add a fence in this direction if not the same plant*)
              if Char.equal plant plantHere then acc
              else direction::acc
    )
  in perims
  

(* Builds a map with pos idx as key and the value is a list of fences for this pos*)
let perims =
  List.init (width * height) ~f:(Fn.id) 
  |> List.fold ~init:(Map.empty (module Int)) ~f:(fun map posIdx -> 
    Map.set map ~key:posIdx ~data:(findPerimsOfPos (pos_of_idx posIdx))
    )

let findRegionOfPos pos =
  let regionHere = match getPlantAtPos pos with
    | None -> raise (Failure "Offgrid in findRegionOfPos")
    | Some plant -> plant
  in

  
  let rec sameRegion visited acc pos = 
    if Set.mem visited (idx_of_pos pos) then acc, visited
    else
      match getPlantAtPos pos with 
      | None -> (*OffGrid *) acc, visited
      | Some plant -> 
          if Char.equal plant regionHere then
            let visited = Set.add visited (idx_of_pos pos) in
            let acc = pos::acc in
            List.fold allDirections ~init:(acc,visited) ~f:(fun (acc,visited) dir ->
              sameRegion 
                visited acc (moveOneStep dir pos)
            ) 
          else acc, visited
  in
  let visited =Set.empty (module  Int) in
    
  List.rev (fst (sameRegion visited [] pos))

let isInRegion regionList posToTest =
  List.exists regionList ~f:(fun region ->
      List.exists region ~f:(fun pos -> idx_of_pos pos = idx_of_pos posToTest)
    )

let regions =
  let allPositions = List.init (width * height) ~f:(Fn.id) in

  let rec addRegion acc posList =
    match posList with 
    | [] -> List.rev acc
    | posIdx::tl ->
        if isInRegion acc (pos_of_idx posIdx) then
          addRegion acc tl
        else 
          let newRegion = findRegionOfPos (pos_of_idx posIdx) in
          addRegion (newRegion::acc) tl
  in
  addRegion [] allPositions
  
let regionArea region = 
  List.length region

let regionAreas = List.map regions ~f:(regionArea)

let perimList = List.map regions ~f:(fun region ->
    List.fold region ~init:0 ~f:(fun acc pos ->
      let noOfPerimsAtPos = match Map.find perims (idx_of_pos pos) with
        | None -> 0
        | Some dirList -> List.length dirList
      in
        acc + noOfPerimsAtPos
      )
  )

let resultP1 = List.fold2_exn regionAreas perimList ~init:0 ~f:(fun acc ra p -> acc + ra * p)


let string_of_idx_and_edge posIdx edge =
  string_of_int posIdx ^ string_of_dir edge


let isDirEqual dir1 dir2 = 
  match dir1, dir2 with
    | North, North
    | East, East
    | South, South
    | West, West -> true
    | _ -> false

let contains_direction edgeLst direction =
  List.exists ~f:(fun x -> isDirEqual x direction) edgeLst

let contains_pos region posToCheck =
  List.exists region ~f:(fun p -> p.col = posToCheck.col && p.row = posToCheck.row)

let regionNoOfPos pos = 
  List.findi regions  ~f:(fun _ region -> contains_pos region pos)
  

let findSidesInCol accRegSidesMap col =

  let rec scanCol accRegSideMap lastPosAndEdgeList row col =
    let currPos = {col;row} in
    if row = height then accRegSideMap
    else 
      let pos, edgeLst = match Map.find perims (idx_of_pos currPos) with 
                          | None -> currPos, []
                          | Some eList -> currPos, eList in

      let currRegionNo = match (regionNoOfPos pos) with 
      | None -> failwith "Cannot find region no"
      | Some (n,_) -> n in

      let lastPos,lastEdgeLst = lastPosAndEdgeList in
      let lastRegionNo = match (regionNoOfPos lastPos) with 
      | None -> failwith "Cannot find region no of lastpos"
      | Some (n,_) -> n in
     
      let leftSidesToAdd = 
        if (not (currRegionNo = lastRegionNo) && (contains_direction edgeLst West))  (* new region with left side. *)
          || (contains_direction edgeLst West && not (contains_direction lastEdgeLst West)) (* Same region with new left side*)
        then 1 
        else 0 in
      let rightSidesToAdd = 
        if (not ( currRegionNo = lastRegionNo) && contains_direction edgeLst East)  (* new region with right side. *)
          || (contains_direction edgeLst East && not (contains_direction lastEdgeLst East)) (* Same region with new right side*)
        then 1 
        else 0 in
      let newAcc = Map.update accRegSideMap currRegionNo ~f:(function
      | None -> leftSidesToAdd + rightSidesToAdd
      | Some sides -> sides + leftSidesToAdd + rightSidesToAdd
      ) in
      scanCol newAcc (pos,edgeLst) (row + 1) col
      in
      
  scanCol accRegSidesMap ({col=0;row=0}, []) 0 col

let scanAllCols =
  let rec loop regionSidesMap col =
    if col = width then regionSidesMap
    else 
      let updatedRSMap = findSidesInCol regionSidesMap col in
      loop updatedRSMap (col + 1)
  in

  loop (Map.empty (module Int)) 0

let findSidesInRow accRegSidesMap row =

  let rec scanRow accRegSideMap lastPosAndEdgeList row col =
    let currPos = {col;row} in
    if col = width then accRegSideMap
    else 
      let pos, edgeLst = match Map.find perims (idx_of_pos currPos) with 
                          | None -> currPos, []
                          | Some eList -> currPos, eList in

      let currRegionNo = match (regionNoOfPos pos) with 
      | None -> failwith "Cannot find region no"
      | Some (n,_) -> n in

      let lastPos,lastEdgeLst = lastPosAndEdgeList in
      let lastRegionNo = match (regionNoOfPos lastPos) with 
      | None -> failwith "Cannot find region no of lastpos"
      | Some (n,_) -> n in
     
      let topSidesToAdd = 
        if (not (currRegionNo = lastRegionNo) && (contains_direction edgeLst North))  (* new region with top side. *)
          || (contains_direction edgeLst North && not (contains_direction lastEdgeLst North)) (* Same region with new top side*)
        then 1 
        else 0 in
      let bottomSidesToAdd = 
        if (not ( currRegionNo = lastRegionNo) && contains_direction edgeLst South)  (* new region with right side. *)
          || (contains_direction edgeLst South && not (contains_direction lastEdgeLst South)) (* Same region with new right side*)
        then 1 
        else 0 in
      let newAcc = Map.update accRegSideMap currRegionNo ~f:(function
      | None -> topSidesToAdd + bottomSidesToAdd
      | Some sides -> sides + topSidesToAdd + bottomSidesToAdd
      ) in
      scanRow newAcc (pos,edgeLst) row (col + 1)
      in
      
  scanRow accRegSidesMap ({col=0;row=0}, []) row 0

let scanAllRows =
  let rec loop regionSidesMap row =
    if row = height then regionSidesMap
    else 
      let updatedRSMap = findSidesInRow regionSidesMap row in
      loop updatedRSMap (row + 1)
  in

  loop scanAllCols 0

let sidesList = Map.to_alist scanAllRows |> List.map ~f:(snd)
let resultP2 = List.fold2_exn regionAreas sidesList ~init:0 ~f:(fun acc ra s -> acc + ra * s)
