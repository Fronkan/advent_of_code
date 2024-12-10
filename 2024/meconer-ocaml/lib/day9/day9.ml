open Core 

(* let filename = "lib/day9/example.txt" *)
let filename = "lib/day9/input.txt"
let day8Input = In_channel.read_all filename
(* let day8Input = "12345" *)

type fileType = {id:int; start:int; length:int}

let buildFileSystem input =
  let charList = String.to_list input in
  let rec inner accFileSys id pos lst =
    match lst with 
    | [] -> raise (Failure "Should be odd number of chars")
    | fileSize::[] -> let len = int_of_char fileSize - int_of_char '0' in
                      let nFile = {id; start=pos; length=len } in
            (List.rev accFileSys) @ [nFile]
    | fileSize::emptySize::tl -> let len=int_of_char fileSize  - int_of_char '0'in
                      let nFile = {id; start=pos; length = len} in
                      let lenEmpty = int_of_char emptySize - int_of_char '0' in
                      let emptySpace = { id= -1; start=pos+len; length = lenEmpty} in
                      if lenEmpty>0 then
                        inner (emptySpace::nFile::accFileSys) (id+1) (pos+len+lenEmpty) tl
                      else
                        inner (nFile::accFileSys) (id+1) (pos+len+lenEmpty) tl
  in
  inner [] 0 0 charList

let fileSys  = buildFileSystem day8Input

let getSizeOfFiles files = 
  let rec inner acc files =
  match files with 
  | [] -> acc
  | file::tl -> let {id; start = _; length} = file in
      let length = if id>= 0 then length else 0 in
        inner (acc + length) tl in
  inner 0 files

let getSizeOfFileSystem fileSys =
  let rec loop acc sys =
    match sys with 
    | [] -> acc
    | block::tl -> loop (acc + block.length) tl
  in
  loop 0 fileSys

let sizeOfSys = getSizeOfFileSystem fileSys
let sizeOfFiles = getSizeOfFiles fileSys

let buildFileSystemAsArray size fileSys =
  let sysArr = Array.create ~len:size (-1) in
  let rec loop pos sys =
    match sys with 
    | [] -> sysArr
    | block::tl -> 
          for i = 1 to block.length do 
            Array.set sysArr (pos + i - 1) block.id
          done;
          loop (pos + block.length) tl
  in
  loop 0 fileSys

let fileSysAsArray = buildFileSystemAsArray sizeOfSys fileSys

let revFileSysAsIdList = Array.rev fileSysAsArray |> Array.to_list

let compactFileSysArrayP1 sizeOfFiles sysArr revSysList =
  let newArr = Array.create ~len:sizeOfFiles (-1) in

  let getFirstNonEmptyFromList lst = 
    let rec loop2 lst =
      match lst with
      | [] -> -2,[]
      | h::t -> if h<0 then loop2 t else h,t
    in
    loop2 lst
  in

  let rec loop pos revSysList =
    if pos = sizeOfFiles then newArr
    else
      let currId = Array.get sysArr pos in
        if currId >= 0 then
          (* A file block. Copy it*)
          (
            newArr.(pos) <- currId;
            loop (pos+1) revSysList 
          )
        else
          (* Empty space. Move block from end*)
          let id, newRevSysList = getFirstNonEmptyFromList revSysList in
          newArr.(pos) <- id;
          loop (pos+1) newRevSysList
  in

  loop 0 revSysList

let compactedFileSysAsArr = compactFileSysArrayP1 sizeOfFiles fileSysAsArray revFileSysAsIdList

let calcCheckSumFromArr fileSysAsArray =
  Array.foldi fileSysAsArray ~init:0 ~f:(
    fun idx acc id -> acc + idx * (max id 0))

let resultP1 = calcCheckSumFromArr compactedFileSysAsArr


let findLastFileBlock pos sysArr =
  
  let rec lastFileBlockPos p  =
    if p <= 0 then None
    else
    let id = sysArr.(p) in
    if id < 0 then 
      lastFileBlockPos (p-1) 
    else
      Some (p,id)
    in

  let rec startPosOfBlock p id =
    if p < 0 then 0 else
    if id = sysArr.(p) then
      startPosOfBlock (p-1) id
    else 
      p+1
    in  

  match lastFileBlockPos pos with 
  | None -> None
  | Some (endPos, id) ->

  let startPos = startPosOfBlock (endPos - 1) id in
  let sizeOfBlock = endPos - startPos + 1 in
  Some (startPos,sizeOfBlock)


let findFirstEmptyBlockWithSize reqSize sysArr =

  let rec firstEmptyBlockPos p  =
  if p >= Array.length sysArr then None
  else 
  let id = sysArr.(p) in
  if id >= 0 then 
    firstEmptyBlockPos (p+1) 
  else
    Some p
  in

  let rec endPosOfEmptyBlock p =
  if p >= Array.length sysArr then 
    p-1
  else
    if sysArr.(p) = (-1) then
      endPosOfEmptyBlock (p+1) 
    else 
      p-1
    in

  let rec findBlockWithMinSize reqSize pos =
  if pos >= Array.length sysArr then None
  else 
    match firstEmptyBlockPos pos  with 
    | None -> None
    | Some stPos -> 
          let endPos = endPosOfEmptyBlock (stPos + 1)  in
          let sizeOfBlock = endPos - stPos + 1 in
          if sizeOfBlock >= reqSize then Some (stPos,sizeOfBlock)
          else
            findBlockWithMinSize reqSize (endPos + 1)
          in
  
  findBlockWithMinSize reqSize 0 
    
let printArr arr =
  Array.iter arr ~f:(fun v -> if v < 0 then Printf.printf "." else Printf.printf "%d" v)

let compactFileSysArrayP2 sysArr =

  let rec findLastBlock lastPos  = 
    (* printArr sysArr;
    Printf.printf "\n"; *)
    if lastPos <= 0 then true
    else
    match findLastFileBlock lastPos sysArr with 
    | None -> true
    | Some (stPos,size) -> 
      match  findFirstEmptyBlockWithSize size sysArr with 
      | None -> findLastBlock (stPos - 1)
      | Some (mPos, _) ->
        if (stPos > mPos) then
        let id = sysArr.(stPos) in
        for i=0 to (size - 1) do 
          sysArr.(mPos+i) <- id;
          sysArr.(stPos+i) <- (-1)
        done;
        findLastBlock (stPos-1)
      else findLastBlock (stPos-1)
  in

  findLastBlock (Array.length sysArr - 1)

let _ =  compactFileSysArrayP2 fileSysAsArray

let resultP2 =  calcCheckSumFromArr fileSysAsArray