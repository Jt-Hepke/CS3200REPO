let square x = x * x
let () = print_int (square 2); print_newline()

let add3 x y z = x + y + z
let () = print_int(add3 2 2 2); print_newline()

let grade x =
  if x >= 90 then "A"
  else if x >= 80 then "B"
  else "C"
let () = print_endline(grade 85)

let twice f x = f (f x)
let () = print_int(twice square 2); print_newline()