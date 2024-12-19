open Core


let isExample = false
let filename = if isExample then
   "lib/day17/example2.txt" 
  else
    "lib/day17/input.txt"

let day17Input = In_channel.read_lines filename

let regA = ref 0
let regB = ref 0
let regC = ref 0

let ip = ref 0

let output = ref []

let getStringOf lst = 
  List.map lst ~f:(string_of_int) |> String.concat ~sep:","


let int_to_bin i =
  let rec int_to_bit acc i =
    if i=0 then acc
    else int_to_bit (i land 1::acc) (i lsr 1)
  in
  let l=int_to_bit [] i in
  String.concat (List.map l ~f:(string_of_int))


let printRegs () = 
  Printf.printf "A: %d\nB: %d %s\nC: %d %s\n" !regA !regB (int_to_bin !regB) !regC (int_to_bin !regC);
  Printf.printf "Ip: %d Output: %s\n" !ip (getStringOf !output)

let extractReg line =
  int_of_string (String.sub line ~pos:12 ~len:(String.length line - 12))

let extractProgram progLine = 
  let progPart = List.last_exn (String.split ~on:' ' progLine) in
  let programAsList = String.split ~on:',' progPart |> List.map ~f:int_of_string in
  programAsList

let parseInput lines = 
  match lines with 
  | regALine::regBLine::regCLine::_::progLine::[] ->
    regA := extractReg regALine;
    regB := extractReg regBLine;
    regC := extractReg regCLine;
    let program = extractProgram progLine in program
  | _ -> failwith "Err in input"

let programAsList = parseInput day17Input
let progArr = Array.of_list programAsList

type instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv

let instr_of opCode = 
  match opCode with 
  | 0 -> Adv
  | 1 -> Bxl
  | 2 -> Bst
  | 3 -> Jnz
  | 4 -> Bxc
  | 5 -> Out
  | 6 -> Bdv
  | 7 -> Cdv
  | _ -> failwith "Wrong opcode"

let string_of_opCode opCode = 
  match opCode with 
  | 0 -> "Adv"
  | 1 -> "Bxl"
  | 2 -> "Bst"
  | 3 -> "Jnz"
  | 4 -> "Bxc"
  | 5 -> "Out"
  | 6 -> "Bdv"
  | 7 -> "Cdv"
  | _ -> failwith "Wrong opcode"

let getComboOperand operand = 
  match operand with 
  | n when n <= 3 -> n
  | n when n = 4 -> !regA
  | n when n = 5 -> !regB
  | n when n = 6 -> !regC
  | _ -> failwith "Illegal operand"

let powerOfTwo n = 1 lsl n

let compareWithList output programAsList =
  let rec loop idx outputList programAsList = 
    match outputList , programAsList with 
    | [], _  -> true
    | hO::tO, hP::tP -> if hO = hP then loop (idx + 1) tO tP else false
    | _, [] -> failwith "output longer than program"

  in
  let isOkSoFar = loop 0 output programAsList in
  let isComplete = isOkSoFar && (List.length output) = List.length programAsList in
  (isOkSoFar, isComplete)



let doInstr instr operand isP2 listToMatch = 
  match instr with 
  | Adv -> let num = !regA in 
           let den = powerOfTwo (getComboOperand operand) in
           let res = num / den in
           regA := res;
           ip := !ip + 2;
           (true,false)
  | Bxl -> let n1 = !regB in
           let n2 = operand in
           let res = n1 lxor n2 in
           regB := res;
           ip := !ip + 2;
           (true,false)
  | Bst -> let n1 = getComboOperand operand in
           let res = n1 % 8 in
           regB := res;
           ip := !ip + 2;
           (true,false)
  | Jnz -> ip := if !regA = 0 then !ip + 2 else operand;
           (true,false)
  | Bxc -> let res = !regB lxor !regC in
           regB := res;
           ip := !ip + 2;
           (true,false)
  | Out -> let value = (getComboOperand operand) % 8 in
           output := if List.is_empty !output then [value] else !output @ [value];
           ip := !ip + 2;
           if isP2 then 
            compareWithList !output listToMatch
            else (true,true)

  | Bdv -> let num = !regA in 
           let den = powerOfTwo (getComboOperand operand) in
           let res = num / den in
           regB := res;
           ip := !ip + 2;
           (true,false)
  | Cdv -> let num = !regA in 
           let den = powerOfTwo (getComboOperand operand) in
           let res = num / den in
           regC := res;
           ip := !ip + 2;
           (true,false)

let rec execute progArr isP2 listToMatchInP2 =
  let instr = instr_of progArr.(!ip) in
  let operand = progArr.(!ip+1) in
  let (isOkSoFar, isComplete) = doInstr instr operand isP2 listToMatchInP2 in

  if not isP2 then 
    if !ip >= Array.length progArr then 
        begin
        Printf.printf "Program halted. Output = %s\n" (getStringOf !output); 
        true
        end
      else 
        execute progArr isP2 listToMatchInP2
  else
    if not isOkSoFar then
      false
    else begin
      if isComplete then true
      else
        if !ip >= Array.length progArr then begin

          let (_, complete) = compareWithList !output programAsList in
            complete
          end
        else 
          execute progArr isP2 listToMatchInP2
    end
(* Part 1 *)
let res = execute progArr false []
let resultP1 = getStringOf !output

(* Reset registers *)
let programAsList = parseInput day17Input
let progArr = Array.of_list programAsList

let () = ip := 0
let () = output := []


let resetRegs () = 
  regA := 0;
  regB := 0;
  regC := 0;
  ip := 0;
  output := [];
  ()

let waitForEnter s =
  Printf.printf "%s" s;
  Out_channel.flush stdout;
  let _  = In_channel.(input_line stdin) in
  ()


let rec findAForP2 regAAttempt listToMatch = 
  resetRegs ();
  regA := regAAttempt;
  (* Printf.printf "A: %d %s\n" regAAttempt (int_to_bin regAAttempt); *)
  let res = execute progArr true listToMatch in
  if res then 
    begin
      (* printRegs (); *)
      (* Printf.printf "Output %s\n" (getStringOf !output); *)
      regAAttempt
    end
  else begin
    (* printRegs (); *)
    (* waitForEnter "\n"; *)
    findAForP2 (regAAttempt + 1) listToMatch
  end



let getComboOperandStr operand =
    match operand with 
  | n when n <= 3 -> string_of_int n
  | n when n = 4 -> "A"
  | n when n = 5 -> "B"
  | n when n = 6 -> "C"
  | _ -> failwith "Illegal operand"

let getStrOfInstrLine instr operand = 
  match instr with 
  | Adv -> getComboOperandStr operand ^ " -> A"
  | Bxl -> string_of_int operand ^ " -> B"
  | Bst -> getComboOperandStr operand ^ " -> B"
  | Jnz -> string_of_int operand
  | Bxc -> "B C -> B"
  | Out -> getComboOperandStr operand ^ " -> Output"
  | Bdv -> getComboOperandStr operand ^ " -> B"
  | Cdv -> getComboOperandStr operand ^ " -> C"


let disAsm programList = 
  let rec loop acc list =
  match list with 
  | [] -> List.rev acc
  | opCode::operand::tl -> 
              let instrStr = string_of_opCode opCode in
              let opStr = getStrOfInstrLine (instr_of opCode) operand in
                loop ((instrStr, opStr)::acc) tl
  | _ -> failwith "Incorrect program"
  in
  loop [] programList
    


(* let dis = disAsm programAsList

let () = List.iter dis ~f:(
  fun el -> 
    Printf.printf "%s   %s\n" (fst el) (snd el);
) *)

(* the disassembler gives this for the input:
Bst   A -> B           B = A % 8    (takes three lowest bits)
Bxl   5 -> B           B = B XOR 5  (B XOR 101) (Toggles bit 0 and 2)
Cdv   B -> C           C = A / 2^B  (Shifts A B bits to the right )
Bxl   6 -> B           B = B XOR 6  (B XOR 110) (Toggles bit 1 and 2)
Bxc   B C -> B         B = B XOR C
Out   B -> Output      Output (B % 8)
Adv   3 -> A           A = A / 2^3 = A / 8 or shift A right 3 bits (!)
Jnz   0                Jumps to start if A non zero

Program line is 2,4,1,5,7,5,1,6,4,1,5,5,0,3,3,0

100032 : 11 000 011 011 000 000 -> [5;5;0;3;3;0]
800294 : 11 000 011 011 000 100 110 -> [1;5;5;0;3;3;0]
 *)

let findAnswer start listToMatch =
  let revList = List.rev listToMatch in
  let lenList = List.length listToMatch in

  let rec loop noOfElToTest n  =
    if noOfElToTest = lenList then n 
    else
      let listToTest  = List.rev (List.take revList (noOfElToTest)) in
      let res = findAForP2 n listToTest in
      loop (noOfElToTest + 1) (res lsl 3) 

  in
  loop 1 start 


let resultP2 = findAnswer 0 programAsList
