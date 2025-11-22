open Solvers.Signature

type instruction =
  | Hlf of char
  | Tpl of char
  | Inc of char
  | Jmp of int
  | Jie of char * int
  | Jio of char * int

type registers = {
  a: int;
  b: int;
}


let execute_instructions instr (regs: registers) pc =
  let rec move regs pc =
    if pc < 0 || pc >= Array.length instr then regs
    else
    match instr.(pc) with
    | Hlf r ->
        let new_regs = 
          if r = 'a' then { regs with a = regs.a / 2 }
          else { regs with b = regs.b / 2 }
        in
        move new_regs (pc + 1)
    | Tpl r ->
        let new_regs = 
          if r = 'a' then { regs with a = regs.a * 3 }
          else { regs with b = regs.b * 3 }
        in
        move new_regs (pc + 1)
    | Inc r ->
        let new_regs = 
          if r = 'a' then { regs with a = regs.a + 1 }
          else { regs with b = regs.b + 1 }
        in
        move new_regs (pc + 1)
    | Jmp offset ->
        move regs (pc + offset) 
    | Jie (r, offset) ->
        let is_even = 
          if r = 'a' then regs.a mod 2 = 0
          else regs.b mod 2 = 0
        in
        if is_even then move regs (pc + offset)
        else move regs (pc + 1)
    | Jio (r, offset) ->
        let is_one = 
          if r = 'a' then regs.a = 1
          else regs.b = 1
        in
        if is_one then move regs (pc + offset)
        else move regs (pc + 1)
  in
  move regs pc





(* parse input to instruction array *)
let parse_instructions lines =
  let parse_line line =
    let parts = String.split_on_char ' ' line in
    match parts with
    | ["hlf"; r] -> Hlf r.[0]
    | ["tpl"; r] -> Tpl r.[0]
    | ["inc"; r] -> Inc r.[0]
    | ["jmp"; offset] -> Jmp (int_of_string offset)
    | ["jie"; r_comma; offset] -> 
        let r = r_comma.[0] in
        let off = int_of_string offset in
        Jie (r, off)
    | ["jio"; r_comma; offset] ->
        let r = r_comma.[0] in
        let off = int_of_string offset in
        Jio (r, off)
    | _ -> failwith ("Invalid instruction: " ^ line)
  in
  Array.of_list (List.map parse_line lines)

module Solver : Solver = struct

  let part1 lines = 
    let instructions = parse_instructions lines in
    let final_regs = execute_instructions instructions { a = 0; b = 0 } 0 in
    string_of_int final_regs.b
  
  let part2 lines = 
    let instructions = parse_instructions lines in
    let final_regs = execute_instructions instructions { a = 1; b = 0 } 0 in
    string_of_int final_regs.b

end