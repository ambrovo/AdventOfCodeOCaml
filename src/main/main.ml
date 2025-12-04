open Solvers.Signature

let choose_solver year day : (module Solver) =
  match year with
  | 2025 -> (
      let open Solvers2025 in
      match day with
      | 0 -> (module Day00.Solver)
      | 1 -> (module Day01.Solver)
      | 2 -> (module Day02.Solver)
      | 3 -> (module Day03.Solver)
      | 4 -> (module Day04.Solver)
      | _ -> failwith "Ni se rešeno")
  | 2024 -> (
      let open Solvers2024 in
      match day with
      | 0 -> (module Day00.Solver)
      | _ -> failwith "Ni se rešeno")
 
  | _ -> failwith "Neveljavno leto"

let run_solver year day part =
  let input_file = Printf.sprintf "data/%d/day_%02d.in" year day in
  let lines = Utils.Files_utils.read_lines input_file in
  
  let solver_module = choose_solver year day in
  
  let module S = (val solver_module) in
  match part with
  | 1 -> S.part1 lines
  | 2 -> S.part2 lines
  | _ -> failwith "Invalid part number"

let () =
  if Array.length Sys.argv < 4 then begin
    Printf.printf "Usage: %s <year> <day> <part>\n" Sys.argv.(0);
    exit 1
  end;
  
  let year = int_of_string Sys.argv.(1) in
  let day = int_of_string Sys.argv.(2) in
  let part = int_of_string Sys.argv.(3) in
  
  let start_time = Sys.time () in
  let result = run_solver year day part in
  let time_spent = Sys.time () -. start_time in
  let output_file = Printf.sprintf "out/%d/day_%02d_part%d.out" year day part in
  Utils.Files_utils.print_in_file output_file result;
  Printf.printf "Result: %s\n" result;
  Printf.printf "Time spent: %.6f seconds\n" time_spent