open Solvers.Signature

let sum_of_divisors n =
  let rec aux d acc =
    if d * d > n then acc
    else if n mod d = 0 then
      let other = n / d in
      if d = other then aux (d + 1) (acc + d) (* sqrt case *)
      else aux (d + 1) (acc + d + other)
    else
      aux (d + 1) acc
  in
  aux 1 0

let check_to_target target =
  let rec aux house_number =
    let total_presents = sum_of_divisors house_number in
    if total_presents >= target then house_number
    else aux (house_number + 1)
  in
  aux 1

let naive_distribution target =
  (* try target/10 houses and elves to check*)
  let houses_array = Array.make (target / 10) 0 in
  List.iter (fun elf ->
    List.iter (fun k ->
      if (k * elf) < (target / 10) then
        houses_array.(k * elf) <- houses_array.(k* elf) + (11 * elf)
      else ()
      ) (List.init 50 (fun x -> x + 1))
  ) (List.init (target / 10) (fun x -> x + 1));
  let rec aux i =
    if houses_array.(i) >= target then i
    else aux (i + 1)
  in
  aux 1

module Solver : Solver = struct

  let part1 _ =
    let target = 29000000 / 10 in
    string_of_int (check_to_target target) (* ~3.2 seconds*)
  
  let part2 _ =
    let target = 29000000 in 
    string_of_int (naive_distribution target) (* ~1 second*)

end