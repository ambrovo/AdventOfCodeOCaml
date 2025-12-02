open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let part1 lines =
  let napol x = (
    let prva = if String.length x =1 then 1 else int_of_string (String.sub x 0 ((String.length x)/2))
    in 
    ( prva)
    )
  in
    let rec skozi p = match p with
    | [] ->0
    | x :: xs -> 
      let l = ( x
      |> String.split_on_char '-' 
      |> List.filter (fun a -> a<> "")) in
      let prvi,drugi = List.nth l 0,List.nth  l 1 in
      let rec prestej start maks = match int_of_string (string_of_int start ^ string_of_int start) <= maks with 
      | false -> 0
      | true -> int_of_string (string_of_int start ^ string_of_int start) + prestej (start+1) maks in
      let start = napol prvi in
      let start = if int_of_string (string_of_int start ^ string_of_int start) < int_of_string prvi then (start +1) else start in
      let start = if (String.length (string_of_int start))*2 < (String.length prvi) then int_of_string (String.make (String.length (string_of_int start)+1) '1') -  int_of_string (String.make (String.length (string_of_int start)) '1') else start
    in
      let maks = int_of_string drugi in
      prestej start maks + skozi xs

  in
    let rec c l = match l with
    | [] -> 0
    | x ::xs-> 
      let primeri =( x
      |> String.split_on_char ',' 
      |> List.filter (fun a -> a<> "")) in
      (skozi primeri) + (c xs) in
    string_of_int (c lines)






(* paart 2  bi probal tako, da generiram vse mozne ponavljajoca stevila in jih preverjam podobno kot sem za dve*)
    let part2 lines =
    let deljitelji x = 
      let rec p x i = 
        if i > x/2 then [] else (if x mod i =0 then i:: (p x (i+1)) else (p x (i+1)) )
      in p x 1 in
    let  ponovi_string s n =(
      let rec aux acc i =
      if i = 0 then acc
      else aux (acc ^ s) (i - 1)
    in
      aux "" n) in
    let ponavljanje zacetek dolzina minimum maksimum= (
      let d = deljitelji dolzina in 

      let obiskani = ref [] in

      let rec generiraj_in_preveri st dolzina_niza   = (
        let stevilo_ponovitev = dolzina/dolzina_niza in 
        
        let niz =  string_of_int st   in
        

        if String.length niz < dolzina_niza then 
          generiraj_in_preveri (int_of_string (niz ^ String.make (dolzina_niza - String.length niz) '0')) dolzina_niza
        else
        let stevilo = int_of_string (ponovi_string niz stevilo_ponovitev) in
        let nadaljuje,stejemo = if stevilo < minimum then true,false else( if stevilo > maksimum then false,false else true,true) in
        if nadaljuje then (
          if stejemo then 
            if not (List.mem stevilo !obiskani) then
              let () = obiskani := stevilo :: !obiskani in
              stevilo + generiraj_in_preveri (st+1) dolzina_niza
            else
              generiraj_in_preveri (st+1) dolzina_niza
             else  (generiraj_in_preveri (st+1) dolzina_niza)
        )
          else 0
      )

        in
        
      let rec p deli zacetek = match deli with
      | [] -> 0 
      | x :: xs -> generiraj_in_preveri zacetek x  + p xs zacetek in
      p d zacetek
      
    )
  in
    let rec skozi p = match p with
    | [] -> 0
    | x :: xs -> 
         
      let l = ( x
      |> String.split_on_char '-' 
      |> List.filter (fun a -> a<> "")) in
      let prvi,drugi = List.nth l 0,List.nth  l 1 in
      let l1,l2 = String.length prvi, String.length drugi in
      let dolzine = if l1 = l2 then [l1] else [l1;l2] in
      let zacetne = if l1 =l2 then [String.make 1 prvi.[0]] else [String.make 1 prvi.[0];String.make 1 '1'] in
   

      let rec p d z = match z,d with
      | ([],[]) -> 0
      | (x::xs,y::ys) -> ponavljanje (int_of_string x) y (int_of_string prvi) (int_of_string drugi)+ p ys xs
      | (_,_) -> 0 
    in
      p dolzine zacetne + skozi xs
  in
    let rec c l = match l with
    | [] -> 0
    | x ::xs-> 
      let primeri =( x
      |> String.split_on_char ',' 
      |> List.filter (fun a -> a<> "")) in
      (skozi primeri) + (c xs) in
    string_of_int (c lines)
end