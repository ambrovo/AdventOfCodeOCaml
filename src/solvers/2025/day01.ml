open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let part1 lines =
    let rec premakni smer pozicija = (match pozicija < 100 && pozicija >= 0 with
    | true -> if pozicija = 0 then 1 else 0
    | false -> premakni smer (pozicija + smer*100)
  )
  in
    let rec c l position =
      match l with
      | [] -> 0
      | x :: xs ->
          let smer = x.[0] in
          let stevilo = int_of_string (String.sub x 1 (String.length x -1)) in
        
          let (position,stevilo) =
            match smer with
            | 'R' ->
                let p = (position + stevilo) mod 100 in
                let s = premakni (-1) (position+stevilo)  in p,s
            | 'L' ->
                let p = position - stevilo in
                let s = premakni 1 p in
                if p < 0 then  (((p mod 100) + 100) mod 100,s) else (p,s)
            | _ -> (position, 0)
         
            in
       
          c xs position + stevilo

    in string_of_int (c lines 50)
  

  let part2 lines =
    let rec premakni smer pozicija = (match pozicija < 100 && pozicija >= 0 with
    | true -> if pozicija = 0 then 1 else 0
    | false -> 1 + premakni smer (pozicija + smer*100)
  )
  in
    let rec c l position =
      match l with
      | [] -> 0
      | x :: xs ->
          let smer = x.[0] in
          let stevilo = int_of_string (String.sub x 1 (String.length x -1)) in
        
          let (pos,stevilo) =
            match smer with
            | 'R' ->
                let p = (position + stevilo) mod 100 in
                let s = premakni (-1) (position+stevilo)+ if p = 0 then -1 else 0  in p,s
            | 'L' ->
                let p = position - stevilo in
                let s = premakni 1 p + if position = 0 then -1 else 0 in
                if p < 0 then  (((p mod 100) + 100) mod 100,s) else (p,s)
            | _ -> (position, 0)
         
            in
         (*  Printf.printf "rotiran: %s do mesta : %d, zaznano: %d\n" x pos stevilo;
      flush stdout;*)
          c xs pos + stevilo 

    in string_of_int (c lines 50)
end