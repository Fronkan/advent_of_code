open Core 

(* let filename = "lib/day11/example.txt" *)
let filename = "lib/day11/input.txt"
let day11Input = In_channel.read_all filename

let stones = String.split ~on:' ' day11Input |> List.map ~f:(int_of_string)
let hasEvenNoOfDigits n =
  let noOfDigits = String.length (string_of_int n) in
  noOfDigits mod 2 = 0

let splitNo n =
  let s = string_of_int n in
  let lenOfEach = (String.length s) / 2 in
  [ int_of_string (String.sub ~pos:0 ~len:lenOfEach s);
    int_of_string (String.sub ~pos:lenOfEach ~len:lenOfEach s)
  ]

let handleStone engrNo =
  match engrNo with
  | n when n=0 -> [1]
  | n when (hasEvenNoOfDigits n) -> splitNo n
  | n -> [n * 2024]


let initList = List.map stones ~f:(fun k -> (k,1))

let stoneMap = Map.of_alist_reduce (module Int) initList ~f:(+) 


let printMap map = 
  Map.iteri map ~f:(fun ~key ~data -> 
    Printf.printf "%d : %d\n" key data)

let doOneBlink  map =
   Map.fold 
    ~init:(Map.empty (module Int)) 
    ~f:(fun ~key ~data aMap -> 
      (*Do the blink for this stone*)
      let newStones = handleStone key in
      (* Update the map with the new stones.*)
      List.fold newStones ~init:aMap ~f:( fun map stone ->
        Map.update map stone ~f:(function
          | None -> data
          | Some count -> data + count
        ))
      ) 
      map
        

let blinkNTimesP2 n stoneMap =
  let max = n in

  let rec loop n stoneMap=
    if n = max then stoneMap
      
    else 
      let stoneMap =doOneBlink stoneMap in
      loop (n+1) stoneMap
  in
  loop 0 stoneMap

let after25Blinks = blinkNTimesP2 25 stoneMap



let countStones map = 
  Map.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data) map

let resultP1 = countStones (blinkNTimesP2 25 stoneMap)
let resultP2 = countStones (blinkNTimesP2 75 stoneMap)
