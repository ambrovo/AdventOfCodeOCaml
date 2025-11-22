open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let part1 lines =
    string_of_int (int_list lines |> sum)
  
  let part2 _ =
    "42"
end