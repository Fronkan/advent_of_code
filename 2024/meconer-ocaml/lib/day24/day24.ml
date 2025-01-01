open Core

let isExample = false

let filename =
  if isExample then "lib/day24/example.txt" else "lib/day24/input.txt"

(* Problem input *)
let input = In_channel.read_lines filename

type opT = AND | OR | XOR

type exprT = {
  op : opT;
  input_gate_1 : string;
  input_gate_2 : string;
  res_gate : string;
}

let op_of_str str =
  match str with
  | "OR" -> OR
  | "AND" -> AND
  | "XOR" -> XOR
  | _ -> failwith "illegal operator"

let str_of_op op = match op with OR -> "OR" | AND -> "AND" | XOR -> "XOR"

let parse_input input =
  let rec loop in_first_section acc_expr acc_gates list =
    match list with
    | [] -> (acc_gates, List.rev acc_expr)
    | line :: tail when String.equal line "" -> loop false [] acc_gates tail
    | line :: tail ->
        if in_first_section then
          let gate_name, v =
            (String.slice line 0 3, int_of_string (String.slice line 5 6))
          in
          let next_gates = Map.set acc_gates ~key:gate_name ~data:v in
          loop true [] next_gates tail
        else
          let expr_list = String.split line ~on:' ' in
          let expr =
            {
              op = op_of_str (List.nth_exn expr_list 1);
              input_gate_1 = List.hd_exn expr_list;
              input_gate_2 = List.nth_exn expr_list 2;
              res_gate = List.last_exn expr_list;
            }
          in
          loop false (expr :: acc_expr) acc_gates tail
  in

  loop true [] (Map.empty (module String)) input

let gate_map, expr_list = parse_input input

let do_expr gate_map expr =
  let in1 = Map.find gate_map expr.input_gate_1 in
  let in2 = Map.find gate_map expr.input_gate_2 in
  let res_gate_name = expr.res_gate in
  match (in1, in2) with
  | None, None | Some _, None | None, Some _ -> gate_map
  | Some in1, Some in2 ->
      let operator = expr.op in
      let res =
        match operator with
        | OR -> in1 lor in2
        | AND -> in1 land in2
        | XOR -> in1 lxor in2
      in
      Map.set gate_map ~key:res_gate_name ~data:res

let do_all_expressions gate_map expr_list =
  let rec loop gate_map expr_list =
    match expr_list with
    | [] -> gate_map
    | expr :: tail ->
        let new_gate_map = do_expr gate_map expr in
        loop new_gate_map tail
  in
  loop gate_map expr_list

let calc_all_gates gate_map expr_list =
  let rec loop gate_map expr_list =
    let new_gate_map = do_all_expressions gate_map expr_list in
    let is_ready = Map.length new_gate_map = Map.length gate_map in
    if not is_ready then loop new_gate_map expr_list else gate_map
  in
  loop gate_map expr_list

let get_z_number gate_map =
  let rec z_loop acc z_no =
    let gate_name =
      "z" ^ String.pad_left ~char:'0' ~len:2 (string_of_int z_no)
    in
    let v = Map.find gate_map gate_name in
    match v with
    | None -> List.rev (List.sort acc ~compare:String.compare)
    | Some v ->
        let str = gate_name ^ ":" ^ string_of_int v in
        z_loop (str :: acc) (z_no + 1)
  in
  let z_numbers = z_loop [] 0 in

  let rec z_int_loop acc list =
    match list with
    | [] -> acc
    | hd :: tl ->
        let num = (acc * 2) + int_of_string (String.slice hd 4 5) in
        z_int_loop num tl
  in
  z_int_loop 0 z_numbers

let resultP1 = get_z_number (calc_all_gates gate_map expr_list)

let find_expression_for res_name expr_list =
  let rec loop depth res_name =
    let expr =
      List.find_exn expr_list ~f:(fun expr ->
          String.equal expr.res_gate res_name)
    in
    let inp1 = expr.input_gate_1 in
    let inp2 = expr.input_gate_2 in
    let c1 = String.get inp1 0 in
    let c2 = String.get inp2 0 in
    let op = expr.op in
    let op_str = str_of_op op in
    (if depth < 3 then
       let indent = String.make depth ' ' in
       Printf.printf "%s%s <= %s %s %s\n" indent res_name inp1 op_str inp2);
    if (not (Char.equal c1 'x')) && not (Char.equal c1 'y') then
      loop (depth + 1) inp1;
    if (not (Char.equal c2 'x')) && not (Char.equal c2 'y') then
      loop (depth + 1) inp2
  in
  loop 0 res_name

let gen_input x_number y_number =
  let rec loop acc number =
    if number = 0 then acc else loop ((number land 1) :: acc) (number lsr 1)
  in
  let x_inp_list = List.rev (loop [] x_number) in
  let y_inp_list = List.rev (loop [] y_number) in
  let inp_strs =
    List.init 45 ~f:(fun n ->
        let ns = string_of_int n in
        let xs = "x" ^ String.pad_left ns ~len:2 ~char:'0' in
        let ys = "y" ^ String.pad_left ns ~len:2 ~char:'0' in
        (xs, ys))
  in
  let rec loop_params idx gate_acc inp_strs =
    match inp_strs with
    | [] -> gate_acc
    | (x_gate, y_gate) :: tail ->
        let gate_acc_with_x =
          match List.nth x_inp_list idx with
          | None -> Map.set gate_acc ~key:x_gate ~data:0
          | Some v -> Map.set gate_acc ~key:x_gate ~data:v
        in
        let gate_acc_with_x_and_y =
          match List.nth y_inp_list idx with
          | None -> Map.set gate_acc_with_x ~key:y_gate ~data:0
          | Some v -> Map.set gate_acc_with_x ~key:y_gate ~data:v
        in
        loop_params (idx + 1) gate_acc_with_x_and_y tail
  in
  loop_params 0 (Map.empty (module String)) inp_strs

let rec try_loop x y max =
  let gate_map = gen_input x y in
  let z = get_z_number (calc_all_gates gate_map expr_list) in
  if x + y = z then Printf.printf "%d + %d = %d\n" x y z
  else Printf.printf "!!!!  =>  %d + %d != %d\n" x y z;
  if x <= max then try_loop (x * 2) (y * 2) max

(* Switches needed hnd and z09. This solves calc at bit 9 *)
(* tdv and z16 solves calc at bit 16 *)
(* bks and z23 solves calc at bit 23 *)
(* tjp and nrn solves calc at bit 37 *)

let switches = [ "hnd"; "z09"; "tdv"; "z16"; "bks"; "z23"; "tjp"; "nrn" ]

let resultP2 =
  String.concat ~sep:"," (List.sort ~compare:String.compare switches)
