let numbers = [10; 20; 30]

let rec list_length numbers = 
  match numbers with
  | [] -> 0
  | first :: rest -> 1 + list_length rest;;

  print_int (list_length numbers);;
  print_newline()

let l1 = [1; 2; 3]
let l2 = [4; 5; 6]
let rec add_lists (l1 : int list) (l2 : int list) : int list = (* Defines a recursive function taking two integer lists *)
  match l1 with (* Looks at the first list*)
  | [] -> l2 (* if first list is empty return second list*)
  | first :: rest -> first :: add_lists rest l2;; (*Puts first item in ront and recursively combine the rest*)
  
let result = add_lists l1 l2;; (*Call function and stores list*)

List.iter(fun x -> print_int x) result;; (*prints each number in combined list*)
print_newline();;
