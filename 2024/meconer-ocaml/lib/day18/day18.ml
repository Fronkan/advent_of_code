open Core
open Utils

let isExample = false

let filename =
  if isExample then "lib/day18/example.txt" else "lib/day18/input.txt"

let day18Input = In_channel.read_lines filename
let width = if isExample then 7 else 71
let height = width
let noOfBytesToSimulate = if isExample then 12 else 1024

(* Using my gridType from Utils just for the idx calculations, not for storing  *)
let grid = { arr = [||]; width; height }

let pos_of_line line =
  let coordList = String.split line ~on:',' |> List.map ~f:int_of_string in
  { col = List.hd_exn coordList; row = List.last_exn coordList }

let incomings = List.map day18Input ~f:pos_of_line

let buildCorrupteds noOfBytesToSimulate =
  List.map (List.take incomings noOfBytesToSimulate) ~f:(idx_of_pos width)
  |> Set.of_list (module Int)

let corrupteds = buildCorrupteds noOfBytesToSimulate

let waitForEnter s =
  Printf.printf "%s" s;
  Out_channel.flush stdout;
  let _ = In_channel.(input_line stdin) in
  ()

let printGridWithSet grid corrupteds =
  let rec getRow acc rowNo =
    if rowNo = grid.height then List.rev acc
    else
      let rec getCol acc colNo =
        if colNo = grid.width then String.of_char_list (List.rev acc)
        else
          let idxOfPos = idx_of_pos width { col = colNo; row = rowNo } in
          let c = if Set.mem corrupteds idxOfPos then '#' else '.' in
          getCol (c :: acc) (colNo + 1)
      in

      getRow (getCol [] 0 :: acc) (rowNo + 1)
  in

  List.iter (getRow [] 0) ~f:(fun l -> Printf.printf "%s\n" l)

let () = printGridWithSet grid corrupteds

let printGrid width height dist corrupteds queue =
  let rec getRow acc rowNo =
    if rowNo = height then List.rev acc
    else
      let rec getCol acc colNo =
        if colNo = width then String.of_char_list (List.rev acc)
        else
          let idxOfPos = idx_of_pos width { col = colNo; row = rowNo } in
          let c = if Set.mem corrupteds idxOfPos then '#' else '.' in
          let c =
            match dist.(idxOfPos) with
            | dist when dist = 0 -> 'S'
            | dist when dist < Int.max_value ->
                if List.exists queue ~f:(fun el -> el = idxOfPos) then '+'
                else 'O'
            | _ -> c
          in

          getCol (c :: acc) (colNo + 1)
      in

      getRow (getCol [] 0 :: acc) (rowNo + 1)
  in
  List.iter (getRow [] 0) ~f:(fun l -> Printf.printf "%s\n" l);
  waitForEnter "\n"

let printGridVisited width height corrupteds visitedSet =
  let rec getRow acc rowNo =
    if rowNo = height then List.rev acc
    else
      let rec getCol acc colNo =
        if colNo = width then String.of_char_list (List.rev acc)
        else
          let idxOfPos = idx_of_pos width { col = colNo; row = rowNo } in
          let c = if Set.mem corrupteds idxOfPos then '#' else '.' in
          let c = if Set.mem visitedSet idxOfPos then 'O' else c in

          getCol (c :: acc) (colNo + 1)
      in

      getRow (getCol [] 0 :: acc) (rowNo + 1)
  in
  List.iter (getRow [] 0) ~f:(fun l -> Printf.printf "%s\n" l)

let printRevPath width height prev corrupteds =
  let targetIdx = (width * height) - 1 in
  let rec buildPath acc idx =
    match idx with 0 -> acc | n -> buildPath (n :: acc) prev.(idx)
  in

  let revPath = buildPath [] targetIdx in
  let revPathSet = Set.of_list (module Int) revPath in

  let rec getRow acc rowNo =
    if rowNo = height then List.rev acc
    else
      let rec getCol acc colNo =
        if colNo = width then String.of_char_list (List.rev acc)
        else
          let idxOfPos = idx_of_pos width { col = colNo; row = rowNo } in
          let c = if Set.mem corrupteds idxOfPos then '#' else '.' in
          let c = if Set.mem revPathSet idxOfPos then 'O' else c in

          getCol (c :: acc) (colNo + 1)
      in

      getRow (getCol [] 0 :: acc) (rowNo + 1)
  in
  List.iter (getRow [] 0) ~f:(fun l -> Printf.printf "%s\n" l);
  waitForEnter "\n"

let manhattanDist pos1 pos2 =
  abs (pos1.col - pos2.col) + abs (pos1.row - pos2.row)

let straightDist pos1 pos2 =
  let p1c = float_of_int pos1.col in
  let p1r = float_of_int pos1.row in
  let p2c = float_of_int pos2.col in
  let p2r = float_of_int pos2.row in
  let dist =
    sqrt (((p1c -. p2c) *. (p1c -. p2c)) +. ((p1r -. p2r) *. (p1r -. p2r)))
  in
  int_of_float (Float.round_nearest dist)

let isOnTarget pos targetPos =
  pos.col = targetPos.col && pos.row = targetPos.row

let getNeighbours pos corrupteds =
  allDirections
  |> List.map ~f:(fun dir -> moveOneStep dir pos)
  |> List.filter ~f:(fun pos ->
         isOnGrid ~width ~height pos
         && not (Set.mem corrupteds (idx_of_pos width pos)))

let removeNode lst p =
  let rec loop acc lst =
    match lst with
    | [] -> acc
    | h :: t -> if h = p then acc @ t else loop (acc @ [ h ]) t
  in
  loop [] lst

let getPosWithMinCost lst dist =
  let pos = ref 0 in
  let _ =
    List.fold ~init:Int.max_value lst ~f:(fun acc posI ->
        let score = dist.(posI) in
        if score < acc then (
          pos := posI;
          score)
        else acc)
  in
  let queueWithNodeRemoved = removeNode lst !pos in
  (!pos, queueWithNodeRemoved)

let str_of_pos pos = string_of_int pos.row ^ ":" ^ string_of_int pos.col

let bfs startPos targetPos corrupteds =
  (* visited is a ref so it is mutable *)
  let visited = ref (Set.empty (module Int)) in
  let queue = ref [ (startPos, 0) ] in

  let rec moveLoop () =
    if is_empty !queue then None
    else
      let popped = Option.value_exn (pop !queue) in
      queue := snd popped;
      let currPos, dist = fst popped in
      if Set.mem !visited (idx_of_pos width currPos) then moveLoop ()
      else if isOnTarget currPos targetPos then Some dist
      else
        let currDist = dist + 1 in
        visited := Set.add !visited (idx_of_pos width currPos);
        let neighbours = getNeighbours currPos corrupteds in
        List.iter neighbours ~f:(fun nPos ->
            let el = (nPos, currDist) in
            queue := !queue @ [ el ]);
        moveLoop ()
  in

  moveLoop ()

let printQueue queue distArr =
  let rec loop acc q =
    match q with
    | [] ->
        Printf.printf "%s\n" acc;
        ()
    | h :: t ->
        loop
          (acc
          ^ str_of_pos (pos_of_idx width h)
          ^ " / "
          ^ string_of_int distArr.(h)
          ^ "\n")
          t
  in
  loop "" queue;
  ()

let dijkstra startPos targetPos corrupteds =
  let dist = ref (Array.create ~len:(width * height) Int.max_value) in
  !dist.(idx_of_pos width startPos) <- 0;
  let prev = ref (Array.create ~len:(width * height) (-1)) in
  let queue = ref [ idx_of_pos width startPos ] in

  let rec loop () =
    if is_empty !queue then (!dist, !prev)
    else
      let currIdx, nQueue = getPosWithMinCost !queue !dist in

      queue := nQueue;
      let currDist = !dist.(currIdx) in
      let currPos = pos_of_idx width currIdx in
      if isOnTarget currPos targetPos then (!dist, !prev)
      else
        let neighbours = getNeighbours currPos corrupteds in
        List.iter neighbours ~f:(fun nPos ->
            let nDist = currDist + 1 in
            if nDist < !dist.(idx_of_pos width nPos) then (
              !dist.(idx_of_pos width nPos) <- nDist;
              !prev.(idx_of_pos width nPos) <- currIdx;
              queue := !queue @ [ idx_of_pos width nPos ]));
        loop ()
  in
  loop ()

let findNodeWithLowestFScore openSet fScore =
  fst
    (Set.fold ~init:(-1, Int.max_value) openSet ~f:(fun acc posI ->
         let score = fScore.(posI) in
         if score < snd acc then (posI, score) else acc))

let aStar startPos targetPos corrupteds heur =
  (* openSet is the set of examined nodes *)
  let openSet =
    ref (Set.add (Set.empty (module Int)) (idx_of_pos width startPos))
  in

  (* Used only to print out what positions were visited. Not need for the algorithm *)
  let visited =
    ref (Set.add (Set.empty (module Int)) (idx_of_pos width startPos))
  in

  (* gScore is the currently known cost from start to the node *)
  let gScore = ref (Array.init (width * height) ~f:(fun _ -> Int.max_value)) in
  !gScore.(idx_of_pos width startPos) <- 0;

  (* For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
     how cheap a path could be from start to finish if it goes through n. *)
  let fScore = ref (Array.create ~len:(width * height) Int.max_value) in
  !fScore.(idx_of_pos width startPos) <- heur startPos targetPos;

  let prev = ref (Array.create ~len:(width * height) (-1)) in

  let rec loop () =
    if Set.is_empty !openSet then None
      (* If the open set is empty we did not reach the target*)
    else
      let current = findNodeWithLowestFScore !openSet !fScore in
      visited := Set.add !visited current;
      let currPos = pos_of_idx width current in
      if isOnTarget currPos targetPos then Some (!fScore.(current), !visited)
      else (
        openSet := Set.remove !openSet current;
        let neighbours = getNeighbours currPos corrupteds in
        List.iter neighbours ~f:(fun nPos ->
            (* tentative_gScore is the distance from start to the neighbor through current *)
            let tentative_gScore = !gScore.(idx_of_pos width currPos) + 1 in
            if tentative_gScore < !gScore.(idx_of_pos width nPos) then (
              (* This path to neighbor is better than any previous one. Record it! *)
              !gScore.(idx_of_pos width nPos) <- tentative_gScore;
              !prev.(idx_of_pos width nPos) <- idx_of_pos width currPos;
              !fScore.(idx_of_pos width nPos) <-
                tentative_gScore + heur nPos targetPos;
              openSet := Set.add !openSet (idx_of_pos width nPos)));
        loop ())
  in

  loop ()

let dijkstraResult =
  dijkstra { col = 0; row = 0 } { col = width - 1; row = width - 1 } corrupteds

let dijkstraDist = (fst dijkstraResult).((width * height) - 1)

let bfsDist =
  bfs { col = 0; row = 0 } { col = width - 1; row = width - 1 } corrupteds

let aStarDist =
  aStar { col = 0; row = 0 }
    { col = width - 1; row = width - 1 }
    corrupteds straightDist

let resultP1 = Option.value_exn bfsDist

let rec getNoOfBytesBeforeBlocked n =
  let corrupteds = buildCorrupteds n in
  let bfsDist =
    bfs { col = 0; row = 0 } { col = width - 1; row = width - 1 } corrupteds
  in
  match bfsDist with None -> n | Some _ -> getNoOfBytesBeforeBlocked (n + 1)

let noOfBytesWhenBlocked = getNoOfBytesBeforeBlocked 2880
let resultP2 = List.nth_exn day18Input (noOfBytesWhenBlocked - 1)
