(*Learning Ocaml list and pattern matching*)


(*
let numbers = [10; 20; 30; 40; 50];;
let new_numbers = 5 :: numbers

let remaining = List.tl new_numbers;;
List.iter (fun x -> print_endline (string_of_int x)) remaining

let result = List.hd new_numbers;;
print_endline (string_of_int result) 

*)

(* pattern matching *)

(*
let numbers = [1; 2; 3]

let result =
  match numbers with
  | [] -> "Empty"
  | first :: rest -> "Not empty";; 
  
 (*If the list isn't empty, take the first item and call it first, and call everthing else rest*)

print_endline result

*)

let example = [1; 2; 3]
let first_int = 0
let get_first =
  match example with
  | [] -> 0 (* notice this can't be the string empty because you can't sometimes return strings and others ints*)
  | first :: rest -> first;;

print_endline (string_of_int get_first)

(*
let second_int =
  match example with
  | [] -> 0
  | first :: second :: rest -> second;;

print_endline (string_of_int second_int)
*)