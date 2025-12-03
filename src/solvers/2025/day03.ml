open Solvers.Signature


module Solver : Solver = struct
  let rec najvisja prva druga mesto dolzina banka=
      match mesto <dolzina with 
      | false -> int_of_string (String.make 1 prva ^String.make 1 druga)
      | true -> let nova = banka.[mesto] in
      match int_of_char druga >int_of_char prva with
      | true ->  najvisja druga nova (mesto+1) dolzina banka
      | false -> match int_of_char nova> int_of_char druga with
      | true -> najvisja prva nova (mesto+1) dolzina banka
      | false -> najvisja prva druga (mesto+1) dolzina banka 

  let rec sestej vrsta = match vrsta with
  | [] -> ""
  | x::xs -> String.make 1 x ^ (sestej xs)
  
  let rec preuredi vrsta =
    
    match vrsta with
  | [] -> []
  | x :: y:: xs ->  (match int_of_char y > int_of_char x with 
  | true -> y:: xs
  | false -> x:: preuredi (y::xs))
  | _ ->  preuredi [] 


  let rec najvisja_vrsta vrsta mesto dolzina banka=
      match mesto <dolzina with 
      | false ->  int_of_string (sestej vrsta)
      | true -> let vrsta =  List.rev (( banka.[mesto]) ::(List.rev vrsta)) in 
      najvisja_vrsta (preuredi vrsta) (mesto+1) dolzina banka
  let part1 lines =
    
      
      
    let rec skozi l = match l with
    | [] -> 0
    | x:: xs -> 
      najvisja '0' '0' 0 (String.length x) x + skozi xs

      

  in string_of_int (skozi lines)
  
  let part2 lines =
    let rec skozi l = match l with
    | [] -> 0
    | x:: xs ->  najvisja_vrsta ['0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0'] 0 (String.length x) x+  skozi xs

      

  in string_of_int (skozi lines)
end