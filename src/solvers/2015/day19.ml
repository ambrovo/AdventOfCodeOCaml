open Solvers.Signature

(* only_top_k is an option to limit the number of results *)
let possibleTransforms already_seen transforms molecule only_top_k = 
  let n = String.length molecule in
  (* fold over the list of transforms *)
  let all_changes = List.concat_map (fun (pattern, change) ->
    let d = String.length pattern in
    (* loop over the molecule *)
    let rec aux i acc =
      if i > (n - d) then acc
      else
        let here = String.sub molecule i d in
        if here = pattern then
          let before = String.sub molecule 0 i in 
          let after = String.sub molecule (i + d) (n - (d+i)) in
          let new_mol = before ^ change ^ after in
          if not (Hashtbl.mem already_seen new_mol) then (
            Hashtbl.add already_seen new_mol ();
            aux (i+1) (new_mol :: acc)
          ) else
            aux (i+1) acc
        else
          aux (i+1) acc
    in
    aux 0 []
    ) transforms
  in
  (* if only_top_k is Some k then return the k shortest strings only *)
  match only_top_k with
  | Some k -> 
      let sorted = List.sort (fun a b -> compare (String.length a) (String.length b)) all_changes in
      let rec take_first n lst =
        if n <= 0 then []
        else
          match lst with
          | [] -> []
          | x :: xs -> x :: take_first (n - 1) xs
      in
      take_first k sorted
  | None -> all_changes

  
(* PART 2 you need greedy approach and only take the shortest possible new word *)
let part2 reductions molecule =
  let already_seen = Hashtbl.create 16 in
  (* do reductions on each step, do at most 500 [we know solution is 207] *)
  let rec aux steps prev_step = 
    if steps = 500 then 500
    else
      if Hashtbl.mem already_seen "e" then steps
      else
        (* you only need to check the shortest one !!! *)
        let after_reductions = List.concat_map (fun word -> possibleTransforms already_seen reductions word (Some 1)) prev_step in
        aux (steps + 1) after_reductions
  in
  aux 0 [molecule]


(* Parse into string * string list *)
let parse_line lines = 
  let parse_single_line line =
    let parts = String.split_on_char ' ' line in
    match parts with
    | [_from; "=>"; _to] -> (_from, _to)
    | _ -> failwith "Invalid line format"
  in
  let rec changes acc1 acc2 lines =
    match lines with
    | [] -> failwith "No blank line found"
    | "" :: molecule :: _ -> acc1, acc2, molecule
    | line :: rest -> let (_from, _to) = parse_single_line line in changes (( _from, _to) :: acc1) ((_to, _from) :: acc2) rest
  in
  changes [] [] lines

module Solver : Solver = struct

  let part1 lines = 
    let already_seen = Hashtbl.create 16 in
    let transforms, _, molecule = parse_line lines in
    let expansions = possibleTransforms already_seen transforms molecule None in
    string_of_int (List.length expansions) 
  
  let part2 lines =
    let _, reductions, molecule = parse_line lines in
    part2 reductions molecule |> string_of_int

end