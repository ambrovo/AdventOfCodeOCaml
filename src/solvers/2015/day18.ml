open Solvers.Signature


let count_neighbors grid y x =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let deltas = [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1)] in
  
  List.fold_left (fun acc (dy, dx) ->
    let ny, nx = y + dy, x + dx in
    if ny >= 0 && ny < height && nx >= 0 && nx < width && grid.(ny).(nx) = '#'
    then acc + 1
    else acc
  ) 0 deltas

let step grid part =
  Array.mapi (fun y row ->
    Array.mapi (fun x cell ->
      let neighbors = count_neighbors grid y x in
      if part = 2 then
        (* In part 2, corners are always on *)
        if (x mod (Array.length row - 1)) + (y mod (Array.length grid - 1)) = 0
        then '#'
        else
          match cell, neighbors with
          | '#', 2 | '#', 3 -> '#'  (* on -> on *)
          | '#', _ -> '.'            (* on -> off *)
          | '.', 3 -> '#'            (* off -> on *)
          | '.', _ -> '.'            (* off -> off *)
          | _, _ -> failwith "wrong cell type"
      else
      match cell, neighbors with
      | '#', 2 | '#', 3 -> '#'  (* on -> on *)
      | '#', _ -> '.'            (* on -> off *)
      | '.', 3 -> '#'            (* off -> on *)
      | '.', _ -> '.'            (* off -> off *)
      | _, _ -> failwith "wrong cell type"
    ) row
  ) grid

let simulate n grid part =
  let rec loop steps c_grid =
    if steps = 0 then c_grid
    else loop (steps - 1) (step c_grid part)
  in
  loop n grid

(* Parse grid into array of char arrays*)
let parse_line lines = 
  lines
  |> List.map (fun line -> String.to_seq line |> Array.of_seq)
  |> Array.of_list

module Solver : Solver = struct

  let part1 lines =
    let start_grid = parse_line lines in
    let end_grid = simulate 100 start_grid 1 in
    Array.fold_left (fun acc row ->
      acc + Array.fold_left(fun acc' cell -> 
        if cell = '#' then acc' + 1 else acc'
        ) 0 row
      ) 0 end_grid |> string_of_int

  let part2 lines = 
        let start_grid = parse_line lines in
    let end_grid = simulate 100 start_grid 2 in
    Array.fold_left (fun acc row ->
      acc + Array.fold_left(fun acc' cell -> 
        if cell = '#' then acc' + 1 else acc'
        ) 0 row
      ) 0 end_grid |> string_of_int
end