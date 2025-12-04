open Solvers.Signature
let sosedi = [(0,1);(0,-1);(1,1);(1,-1);(1,0);(-1,1);(-1,-1);(-1,0)]
let add_tuple (a,b) (c,d) =(a+c,b+d)
let obstaja tocka minx miny maksx maksy = match tocka with
| (a,b) -> minx<= a && a < maksx && miny <= b && b < maksy

let naredi_graf lines = 
  let rec skozi_stolpce x i= match i< String.length x with
  | false -> []
  | true -> x.[i] :: skozi_stolpce x (i+1) in

  let rec skozi_vrstice l = match l with
  |  [] -> []
  | x :: xs -> Array.of_list (skozi_stolpce x 0) :: skozi_vrstice xs
in
Array.of_list (skozi_vrstice lines)

let lokacije maksx maksy = 
  let rec stej i j = match i< maksx with
  | false -> stej 0 (j+1)
  | true -> match  j<maksy with
  | false -> []
  | true -> (i,j) ::  (stej (i+1) j)
in
stej 0 0


module Solver : Solver = struct
  let part1 lines =
    let minx,miny,maksx,maksy = 0,0, String.length (List.nth lines 0), List.length lines in
    let  poglej_sosede graf lokacija = 
    if match lokacija with (a,b) -> graf.(a).(b) != '@' then 0 else
      let rec skozi l = match l with 
      | [] -> 0
      | x::xs -> let nova_tocka = add_tuple x lokacija in 
      match obstaja nova_tocka minx miny maksx maksy with
      | false -> skozi xs
      | true -> match nova_tocka with (x,y) ->
        match graf.(x).(y) with
        | '@' -> 1 + skozi xs
        | _ -> skozi xs
    in
     if skozi sosedi<4 then 1 else 0

  in
  let g = naredi_graf lines in
  let l = lokacije maksx maksy in 
  let rec r l = match l with
  | [] -> 0
  | x :: xs -> (poglej_sosede g x) + r xs


    in
    string_of_int (r l)
  
  let part2 lines =
    let minx,miny,maksx,maksy = 0,0, String.length (List.nth lines 0), List.length lines in
    let  poglej_sosede graf lokacija = 
    if match lokacija with (a,b) -> graf.(a).(b) != '@' then false else
      let rec skozi l = match l with 
      | [] -> 0
      | x::xs -> let nova_tocka = add_tuple x lokacija in 
      match obstaja nova_tocka minx miny maksx maksy with
      | false -> skozi xs
      | true -> match nova_tocka with (x,y) ->
        match graf.(x).(y) with
        | '@' -> 1 + skozi xs
        | _ -> skozi xs
    in
     if skozi sosedi<4 then true else false

  in
  let g = naredi_graf lines in
  let l = lokacije maksx maksy in 
  let rec r l = match l with
  | [] -> []
  | x :: xs -> if (poglej_sosede g x )= true then (x :: r xs) else  r xs


  in
    let tocke =  (r l) in
  let rec spremeni_g t = match t with
  | [] -> ()                          
  | (x, y) :: xs ->
      let () = g.(x).(y) <- '.' in
      spremeni_g xs
  in
  let rec veckrat tocke tf= match List.length tocke with
  | 0 -> if tf != true then veckrat (r l) true else 0
  | x -> let () = spremeni_g tocke in x + veckrat (r l) tf

  in string_of_int (veckrat tocke false)
end