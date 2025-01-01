open Core

let isExample = false

let filename =
  if isExample then "lib/day23/example.txt" else "lib/day23/input.txt"

(* Problem input *)
let input = In_channel.read_lines filename

(* Contains all the connection one computer has as a map of comp-name to set of computers*)
let connections = ref (Map.empty (module String))

(* A set of all the computers  *)
let computers = ref (Set.empty (module String))

(* Iterate over the input lines and build the set of computers and
   the map of connections
*)
let parse_input =
  List.iter input ~f:(fun line ->
      let comps = String.split ~on:'-' line in
      match comps with
      | [ comp1; comp2 ] ->
          computers := Set.add !computers comp1;
          computers := Set.add !computers comp2;
          connections :=
            Map.update !connections comp1 ~f:(fun comp ->
                match comp with
                | None -> Set.(add (Set.empty (module String)) comp2)
                | Some comp_set -> Set.add comp_set comp2);
          connections :=
            Map.update !connections comp2 ~f:(fun comp ->
                match comp with
                | None -> Set.(add (Set.empty (module String)) comp1)
                | Some comp_set -> Set.add comp_set comp1)
      | _ -> failwith "Wrong line format")

(* A set of all groups of three interconnected computers as a set of three two char strings in sorted order *)
let groups_of_three = ref (Set.empty (module String))

(* Make a string of three computer names sorted alphabetically and
   add them to the group_of_three set *)
let add_to_groups_of_three comp1 comp2 comp3 =
  let string_list = List.sort ~compare:String.compare [ comp1; comp2; comp3 ] in
  let s = List.reduce string_list ~f:(fun a b -> a ^ "," ^ b) in
  match s with
  | None -> failwith "Wtf?"
  | Some s -> groups_of_three := Set.add !groups_of_three s

(* Find computers that are connected both to computer comp AND one of the computers in the connected set *)
let find_common_connections comp =
  let set_of_connected = Map.find_exn !connections comp in
  (* Loop through the computers that are connected to comp *)
  Set.iter set_of_connected ~f:(fun conn_comp ->
      (* Get the computers connected to this except for the current*)
      let connected_to_this =
        Set.remove (Map.find_exn !connections conn_comp) comp
      in
      (* Get the computers connected to both. Might be more than one *)
      let common = Set.inter set_of_connected connected_to_this in
      (* Iterate through them and add to groups of three *)
      Set.iter common ~f:(fun comp3 ->
          add_to_groups_of_three comp conn_comp comp3))

(* Build the groups of three *)
let _ = Set.iter !computers ~f:(fun comp -> find_common_connections comp)

(* Count the groups that has a computer name that starts with a t.
   It can be any of the three in a group
*)
let resultP1 =
  Set.count !groups_of_three ~f:(fun group ->
      Char.equal (String.get group 0) 't'
      || Char.equal (String.get group 3) 't'
      || Char.equal (String.get group 6) 't')

(* Stringify a set of groups in order  *)
let string_of_group_set group_set =
  let group_list = Set.to_list group_set in
  let sorted = List.sort group_list ~compare:String.compare in
  String.concat ~sep:"," sorted

(* Check if two computers has a direct connection *)
let has_direct_connection comp1 comp2 =
  let connections_to_comp1 = Map.find_exn !connections comp1 in
  Set.exists connections_to_comp1 ~f:(fun conn_comp ->
      String.equal conn_comp comp2)

let find_groups computers =
  let rec loop acc comp_list =
    match comp_list with
    | [] -> acc
    | comp :: tl ->
        (* Take the first two computers and add them to a group since we know they have a direct connection *)
        let group = Set.add (Set.empty (module String)) comp in
        let list_of_connected = Set.to_list (Map.find_exn !connections comp) in
        let group = Set.add group (List.hd_exn list_of_connected) in
        let list_of_connected = List.drop list_of_connected 1 in

        (* Loop through the rest of the computers and add all that has a direct connection with
           all of the computers already added in the group *)
        let ngroup =
          List.fold list_of_connected ~init:group ~f:(fun gacc comp_to_check ->
              if
                Set.exists gacc ~f:(fun comp_in_group ->
                    not (has_direct_connection comp_in_group comp_to_check))
              then gacc
              else Set.add gacc comp_to_check)
        in

        (* Add them to current group *)
        let group_str = string_of_group_set ngroup in
        loop (Set.add acc group_str) tl
  in
  loop (Set.empty (module String)) (Set.to_list computers)

let groups = Set.to_list (find_groups !computers)

let groups_in_length_order =
  List.sort groups ~compare:(fun s1 s2 ->
      Int.compare (String.length s2) (String.length s1))

let resultP2 = List.hd_exn groups_in_length_order
