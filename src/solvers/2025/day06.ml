open Solvers.Signature
let rec naredi_list znaki= match znaki with
| [] -> []
| x :: xs -> match x with 
| "*" -> 1 :: naredi_list xs
| "+" -> 0 :: naredi_list xs
| _   -> failwith "narobe"

let rec pristej vrsta rezultat znaki = match (vrsta, rezultat, znaki) with
| x::xs,y::ys,z::zs -> (match z with 
| "+" -> x+y :: pristej xs ys zs
| "*" -> x*y :: pristej xs ys zs
| _ -> failwith "napacen znak")
| _,_,_ -> []
let rec sum l = match l with
  | [] -> 0
  | h::t -> h + (sum t)

  let rec printaj r = match r with
| [] -> ()
| x::xs  ->let ()=Printf.printf " .%s. " x in printaj xs


let rec skozi_vrsto x prvi= match x with
| [] -> []
| x :: y :: xs -> (match  prvi with
| true -> skozi_vrsto (y::xs) false
| false -> x :: skozi_vrsto (y::xs) false)
| x:: xs -> (x^" ") :: skozi_vrsto xs false

let rec popravi_zacetek_konec l = 
    
    match l with
| [] -> []
| x  ::xs -> skozi_vrsto x true :: xs


let split_by_indexes s indexes =
  let len = String.length s in
  let sorted = List.sort compare indexes in
  let rec aux start idxs =
    match idxs with
    | [] ->
        [String.sub s start (len - start)]
    | i :: rest ->
        let part = String.sub s start (i - start) in
        part :: aux i rest
  in
  aux 0 sorted


let rec sum2 l z= match l with
  | [] -> (match z with
  | "+" -> 0
  | _ -> 1)
  | h::t ->  match z with
  |"+" ->  h + (sum2 t z)
  | _ -> h* (sum2 t z)

let razdeli_string s =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (if s.[i] = ' ' then ("" :: acc) else (String.make 1 s.[i] :: acc))
  in
  aux (String.length s - 1) []

let rec sestej_vrsti a b = match a, b with
| x :: xs, y::ys -> (x^y) :: sestej_vrsti xs ys
| _,_ -> []

let rec pristej2 vrsta rezultat  = match (vrsta, rezultat) with
| x::xs,y::ys -> sestej_vrsti y  x :: pristej2 xs ys
| _,_ -> []

module Solver : Solver = struct
  let part1 lines =
    let znaki = lines |> List.rev |> List.hd |> String.split_on_char ' ' |> List.filter (fun x -> x <> "") in
    let r = naredi_list znaki in
    
    let rec resi l r= match l with
    | [] -> r
    | x :: xs ->  if x.[0] = '*' then r else let x = x |> String.split_on_char ' ' |> List.filter (fun x -> x <> "") |> List.map (fun a -> int_of_string a) in
      resi xs (pristej x r znaki) 
    
    in let r =  (resi lines r ) 
  in 
  sum r |> string_of_int

  let part2 lines =
    let znaki = lines |> List.rev |> List.hd |> String.split_on_char ' ' in 
    let znaki2 = lines |> List.rev |> List.hd |> String.split_on_char ' ' |> List.filter (fun x -> x <> "") in
    let r = naredi_list znaki2 in
    let rec skozi_znake z i= match z with
    | [] -> []
    | "" :: xs -> skozi_znake xs (i+1)
    | _ :: xs -> i :: skozi_znake xs (i+2) 
  in

  let mesta_znakov = skozi_znake znaki 0 in
  let  rec spremeni_input l = match l with
  | [] -> []
  | x :: xs ->if x.[0] == '*' then [] else let r = split_by_indexes x mesta_znakov  in r:: spremeni_input xs
   in 

   let novi_lines =spremeni_input lines in 
   let novi_lines = popravi_zacetek_konec novi_lines in 
   let rec vrsta x = match x with
   | [] -> []
   | x :: xs -> if x = "" then vrsta xs else  razdeli_string (String.sub x 0 (String.length x -1)) :: vrsta xs in


   let rec skozi_input l = match l with
   | [] -> []
   | x:: xs -> vrsta x :: skozi_input xs in
   let rec pripni_input l r= match l with
   | [] -> r
   | x:: xs -> (match r with 
   | [[]] -> pripni_input xs x
   | d ->  pripni_input xs (pristej2 x d)
   )  in
  let rec sestej l znaki = match (l,znaki) with
  | s::xs , z ::zs -> sum2 (List.map (fun a -> int_of_string a) s) z :: sestej xs zs
  | _,_ -> [] in
  let print_string_list_list lst =
  lst
  |> List.iter (fun sublist ->
         let line = String.concat ", " sublist in
         Printf.printf "[%s]\n" line)
  in
   let input = pripni_input (skozi_input novi_lines) [[]] in
   let () = print_string_list_list input in
   sestej input znaki2|> sum  |> string_of_int


    
end