open Solvers.Signature


let rec first_subset_of_size packets target size =
  if size = 0 then
    if target = 0 then [[]] else []
  else if target <= 0 || packets = [] then []
  else
    match packets with
    | [] -> []
    | p :: rest ->
        let with_p = List.map (fun subset -> p :: subset) (* p is in the subset *)
          (first_subset_of_size rest (target - p) (size - 1)) in
        let without_p = first_subset_of_size rest target size in (* p is not *)
        with_p @ without_p


let product lst = List.fold_left ( * ) 1 lst

let find_min_qe packets num_groups =
  let total = List.fold_left (+) 0 packets in
  let target = total / num_groups in
  
  (* check each size from 1 up*)
  let rec try_size size =
    let subsets = first_subset_of_size packets target size in
    (* assume each time, the remainder can be split ok*)
    
    if subsets = [] then
      try_size (size + 1)
    else
      List.map product subsets
      |> List.fold_left min max_int
  in
  try_size 1

module Solver : Solver = struct

  let part1 lines = 
    let packets = List.map int_of_string lines in
    find_min_qe packets 3 |> string_of_int

  let part2 lines = 
    let packets = List.map int_of_string lines in
    find_min_qe packets 4 |> string_of_int

end