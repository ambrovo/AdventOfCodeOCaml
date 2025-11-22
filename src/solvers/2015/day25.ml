open Solvers.Signature

let next_value x = (x * 252533) mod 33554393

let next_loc (col_index, row_index) =
  if row_index = 1
  then (1, col_index+row_index)
  else (col_index+1, row_index-1) 

let moving start_val target =
  let rec move value pos =
    if pos = target then value
    else move (next_value value) (next_loc pos)
  in
  move start_val (1,1)

module Solver : Solver = struct

  let part1 _ =
    (* manually read from the input file this time *)
    let target_pos = (3075, 2981) in
    let start_value = 20151125 in
    moving start_value target_pos |> string_of_int

  let part2 _ =
    "Year 2025 successfully completed!"

end