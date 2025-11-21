let sum = List.fold_left ( + ) 0

let int_list str_list =
  List.map int_of_string str_list

let first_element lst =
  match lst with
  | [] -> failwith "Empty list has no first element"
  | x :: _ -> x

let rec print_list_of_str = function
  | [] -> ()
  | s :: ss -> (print_endline s; print_list_of_str ss)

let rec print_list_of_int = function
  | [] -> ()
  | i :: is -> (print_endline (string_of_int i); print_list_of_int is)

  let rec print_list_of_int_int = function
  | [] -> ()
  | (i, j) :: is -> (print_endline ((string_of_int i) ^ " , " ^ (string_of_int j)); print_list_of_int_int is)

let rec print_list_of_str_str = function
  | [] -> ()
  | (s1, s2) :: ss -> (
    print_endline ("( " ^ s1 ^ " , " ^ s2 ^ " )");
    print_list_of_str_str ss
  )

let rec print_list_str_int = function
  | [] -> ()
  | (s, i) :: ss -> (
    print_endline ("( " ^ s ^ " , " ^ (string_of_int i) ^ " )");
    print_list_str_int ss
  )

let rec print_list_int_int_int = function
  | [] -> ()
  | ((a, b), c) :: ss -> (
    print_endline ("( " ^ (string_of_int a) ^ " , " ^ (string_of_int b)^ " , " ^ (string_of_int c) ^ " )");
    print_list_int_int_int ss
  )

let rec interleave x lst =
  match lst with
  | [] -> [[x]]
  | y :: ys -> (x :: lst) :: List.map (fun l -> y :: l) (interleave x ys)

let rec permutations lst =
  match lst with
  | [] -> [[]]
  | x :: xs ->
      List.concat (List.map (interleave x) (permutations xs))

let rec list_drop n = function
  | l when n <= 0 -> l
  | [] -> []
  | _ :: xs -> list_drop (n - 1) xs
