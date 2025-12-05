open Solvers.Signature

let razdeli_input lines = 
  let rec prva l = match l with
  | [] -> []
  | x :: xs -> match x with 
  | "" -> []
  | x -> (List.map (fun a -> int_of_string a) (List.filter (fun a -> a <> "") (String.split_on_char '-' x))) :: prva xs
in
  let rec druga l dodaj = match l with
  [] -> []
  | x :: xs -> match x with 
  | "" -> druga xs true
  | x -> match dodaj with
  | false -> druga xs false
  | true -> int_of_string x :: druga xs true
in
(prva lines, druga lines false)

let rec prestej_razdalje r = match r with
| [] -> 0
| x :: xs ->  (List.nth x 1) - List.hd x + 1 + prestej_razdalje xs


let rec printaj r = match r with
| [] -> ()
| x::xs  ->let ()=Printf.printf "prvi: %d drugi: %d \n" (List.hd x) (List.nth x 1) in printaj xs
module Solver : Solver = struct
  let part1 lines =
    let (razdalje, stevila) = razdeli_input lines in
    let rec skozi_razdalje r x = match r with
    | [] -> false
    | t :: ts -> if List.hd t <= x && x <= List.hd (List.tl t) then true else skozi_razdalje ts x in
    let rec skozi_stevila s = match s with
    | [] -> 0
    | x :: xs -> let tocka = match skozi_razdalje razdalje x with
    | true -> 1
    | false -> 0 in
    tocka + skozi_stevila xs in
    string_of_int (skozi_stevila stevila)
  
  let part2 lines =
    let primerjaj l1 l2 = match compare (List.hd l1) (List.hd l2) with 
    | 0 -> compare (List.hd (List. tl l1)) (List.hd (List. tl l2))
    | x -> x in
    let (razdalje, _) = razdeli_input lines in 
    let razdalje = List.sort primerjaj razdalje in
    
    let rec skozi_razdalje r = match r with
    | x::y ::xs -> (if List.nth x 1>= List.hd y then ([List.hd x; max (List.nth y 1) (List.nth x 1)] :: (skozi_razdalje xs)) else x :: skozi_razdalje (y ::xs))
    | x -> x
  in
    let rec veckrat r d= let nove_razdalje = skozi_razdalje r in 
    let nd =  List.length nove_razdalje in
    
    if nd = d then nove_razdalje else
      veckrat nove_razdalje nd
    
    in
    let razdalje = veckrat razdalje (List.length razdalje) in

 string_of_int (prestej_razdalje razdalje)
    
end