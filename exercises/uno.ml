let (%) = (mod);;

let is_even x = 
  if x % 2 = 0 then
    "Even"
  else
    "Odd";;

print_endline (is_even 5);;
print_endline (is_even 8);;



(*
let is_even = 5

let result = 
  if is_even mod 2 = 0 then
    "Even"
  else
    "Odd";;
print_endline result
*)
(*
let age = 20
let result = 
    if age >= 18 then 
      "Adult"
    else 
      "Minor";;

print_endline result
*)