
let rec count l =
  match l with
  | [] -> 0
  | h :: t -> 1 + count t

  let () = Printf.printf "%d\n" (count [7; 8; 9])


  (*Tail recursive*)
let count l = 
  let rec go l r = 
    match l with
    | [] -> r
    | h :: t -> go t (r + 1)
  in
  go l 0

let () = Printf.printf "%d\n" (count [7; 8; 9; 10; 11])


let sum l =
  let rec sum1 l r =
    match l with
    | [] -> r
    | h :: t -> sum1 t (r + h)
  in
  sum1 l 0

let () = Printf.printf "%d\n" (sum [1; 2; 3])

let sum_squares l = 
  let rec go l r =
    match l with
    | [] -> r
    | h :: t -> go t (h * h + r)
  in
  go l 0

let () = Printf.printf "%d\n" (sum_squares [1; 2; 3])

let count_evens l =
  let rec go l r =
    match l with
    | [] -> r
    | h :: t -> go t (if h mod 2 = 0 then r + 1 else r)
  in
  go l 0

let () = Printf.printf "%d\n" (count_evens [1; 2; 3; 4])


let double_all l =
  let rec go l r =
    match l with
    | [] -> List.rev r
    | h :: t -> go t (h*2 :: r)
  in
  go l []

let () = List.iter (Printf.printf "%d") (double_all [1; 2; 4])