(************************************************************************
0. Fill your name and OHIO ID on the line below:
*)

let name ="Jt Hepke";;
let id = "jh639423@ohio.edu";;
let _ = Printf.printf "\n-----NAME: %s ID:%s -----\n" name id;;


open Alcotest
open Util

(** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(** The natural numbers 1, 2, 3, 4, ..., start briskly at 1 and then
   go on forever, although how we might explain what it means for
   anything to go on forever without in turn using the natural numbers
   is something of a mystery. In almost every respect, they are, those
   numbers, simply given to us, and they express a primitive and
   intimate part of our experience. ~ David Berlinski, A Tour of the Calculus *)
(** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(** PART I (Natural numbers):

   Recall that the natural numbers can be given by the following
   inductive definition:

   1) O (zero) is a natural number (despite what the above quote says).

   2) If n is a natural number, then S n (the successor of n) is a
      natural number.

   3) The only natural numbers are those generated from 1) and 2).

   We can encode the natural numbers in OCaml with the following
   datatype (note that the zero constructor 'O' is a capital 'o'). *)

type nat =
  | O
  | S of nat

(** Testing voodoo (feel free to ignore). *)
let rec string_of_nat : nat -> string = function
  | O -> "O"
  | S n' -> "S " ^ string_of_nat n'
let rec nat_of_int : int -> nat = function
  | n when n <= 0 -> O
  | n -> S (nat_of_int (n - 1))
let nat : nat testable =
  let pp_nat ppf x = Fmt.pf ppf "%s" (string_of_nat x) in
  testable pp_nat ( = )
let nat_gen = QCheck.Gen.(map nat_of_int (int_bound 7))
let arbitrary_nat =
  let open QCheck.Iter in
  let shrink_nat = function
    | O -> empty
    | S n' -> return n'
  in
  QCheck.make nat_gen ~print:string_of_nat ~shrink:shrink_nat

(** Some example nats for testing. *)
let one   = S O
let two   = S one
let three = S two
let four  = S three
let five  = S four
let six   = S five

(** 1. As a warmup, define a function 'pred' that computes the
   predecessor of a nat. Define O as the predecessor of O. *)

let pred (n : nat) : nat =
  match n with
  |O -> O
  |S x -> x

let () = add_test "pred" @@
           fun _ ->
           (check nat) "pred 0 = 0" O (pred O);
           (check nat) "pred 1 = 0" O (pred one);
           (check nat) "pred 2 = 1" one (pred two);
           (check nat) "pred 3 = 2" two (pred three)

(** 2. (10 pts) Define a function 'plus' that takes two nats and
   produces their sum as a nat. HINT: structural recursion on n. *)

(* n + m *)
let rec plus (n : nat) (m : nat) : nat =
  match n with
  |O -> m
  |S x -> S(plus x m)

let () = add_test "plus" @@
           fun _ ->
           (check nat) "0 + 0 = 0" O (plus O O);
           (check nat) "0 + 1 = 1" one (plus O one);
           (check nat) "1 + 0 = 1" one (plus one O);
           (check nat) "1 + 1 = 2" two (plus one one);
           (check nat) "2 + 1 = 3" three (plus two one);
           (check nat) "2 + 2 = 4" four (plus two two);
           (check nat) "2 + 3 = 5" five (plus two three)

(** 3. (10 pts) Using 'plus' from #2, define a function 'mult' that
   computes the product of two nats. *)

(* n · m *)
let rec mult (n : nat) (m : nat) : nat =
  match n with
  |O -> O
  |S x -> plus m (mult x m)

let () = add_test "mult" @@
           fun _ ->
           (check nat) "0 * 3 = 0" O (mult O three);
           (check nat) "2 * 0 = 0" O (mult two O);
           (check nat) "1 * 3 = 3" three (mult one three);
           (check nat) "2 * 2 = 4" four (mult two two);
           (check nat) "2 * 3 = 6" six (mult two three)

(** 4. (10 pts) Using 'mult' from #3, define a function 'fact' that
   computes the factorial of a nat. *)

(* n! *)
let rec fact (n : nat) : nat =
  match n with
  | O -> S O
  | S x -> mult (S x) (fact x)

let () = add_test "fact" @@
           fun _ ->
           (check nat) "0! = 1" one (fact O);
           (check nat) "1! = 1" one (fact one);
           (check nat) "2! = 2" two (fact two);
           (check nat) "3! = 6" six (fact three);
           (check nat) "4! = 6 * 4" (mult six four) (fact four);
           (check nat) "5! = 6 * 4 * 5" (mult (mult six four) five) (fact five)

(** 5. (10 pts) Using 'mult' from #3, define a function 'pow' that
   raises a nat n to a nat m power. *)

(* nᵐ *)
let rec pow (n : nat) (m : nat) : nat =
  match m with
  |O -> S O
  |S x -> mult n (pow n x)

let () = add_test "pow" @@
           fun _ ->
           (check nat) "0⁰ = 1" one (pow O O);
           (check nat) "0¹ = 0" O (pow O one);
           (check nat) "1⁰ = 1" one (pow one O);
           (check nat) "1¹ = 1" one (pow one one);
           (check nat) "2⁰ = 1" one (pow two O);
           (check nat) "2¹ = 2" two (pow two one);
           (check nat) "2² = 4" four (pow two two);
           (check nat) "5³ = 5·5·5" (mult (mult five five) five) (pow five three)

(** 6. (10 pts) Define a function 'minus' that takes two nats and
   produces their difference as a nat, or 0 when the subtrahend (m) is
   less than the minuend (n). *)

(* n - m *)
let rec minus (n : nat) (m : nat) : nat =
  match m with
  |O -> n
  |S x -> pred(minus n x)

let () = add_test "minus" @@
           fun _ ->
           (check nat) "0 - 0 = 0" O (minus O O);
           (check nat) "0 - 1 = 0" O (minus O one);
           (check nat) "1 - 0 = 1" one (minus one O);
           (check nat) "1 - 1 = 0" O (minus one one);
           (check nat) "2 - 1 = 1" one (minus two one);
           (check nat) "2 - 2 = 0" O (minus two two);
           (check nat) "2 - 3 = 0" O (minus two three);
           (check nat) "3 - 1 = 2" two (minus three one)

(** [∀ a b, a - b + b = a] does *NOT* hold. Counterexample: *)
let () = add_test "minus_bad" @@
           fun _ ->
           (check nat) "1 - 2 + 2 = 2" two (plus (minus one two) two)

(** 7. (10 pts) Define a pair of mutually recursive functions 'even'
   and 'odd' that evaluate to true when the input nat is even or odd,
   respectively. *)

let rec even (n : nat) : bool =
  match n with
  |O -> true
  |S O -> false
  |S (S x) -> even x

and odd (n : nat) : bool =
  match n with
  |O -> false
  |S O -> true
  |S (S x) -> odd x

let () = add_test "even_odd" @@
           fun _ ->
           (check bool) "even 0 = true" true (even O);
           (check bool) "odd 0 = false" false (odd O);
           (check bool) "even 1 = false" false (even one);
           (check bool) "odd 1 = true" true (odd one);
           (check bool) "even 2 = true" true (even two);
           (check bool) "odd 2 = false" false (odd two)

(** 8. (10 pts) Define a "less-than-or-equal-to" function that takes
   two nats n and m as arguments and evaluates to true when n is less
   than or equal to m. *)

(* n ≤ m *)
let rec leq (n : nat) (m : nat) : bool =
  match n, m with (*we want to compare n < m. cases for n (n = 0, n != 0, m = 0, m =! 0)*)
  |O, O -> true
  |O, S _ -> true
  |S _, O -> false
  |S x, S y -> leq x y (*go down tell hits one of the test cases*)


let () = add_test "leq" @@
           fun _ ->
           (check bool) "0 ≤ 0" true (leq O O);
           (check bool) "0 ≤ 1" true (leq O one);
           (check bool) "~ 1 ≤ 0" false (leq one O);
           (check bool) "0 ≤ 2" true (leq O two);
           (check bool) "1 ≤ 2" true (leq one two);
           (check bool) "~ 2 ≤ 1" false (leq two one);
           (check bool) "~ 2 ≤ 0" false (leq two O);
           (check bool) "3 ≤ 5" true (leq three five)

(** PART II (Binary-encoded numbers): *)

(** The usual inductive definition of the natural numbers induces a
   *unary* encoding. While convenient for its simplicity (especially
   for doing proofs by induction), a unary encoding of numbers is
   woefully inadequate for large-scale computation.

   The problem, for one, is that the size (and thus memory usage) of a
   nat is linearly proportional to the value of the nat itself. I.e.,
   the nat representation of 2 requires literally twice as much memory
   as the representation of 1, 3 requires three times as much as 1,
   and so on.

   Moreover, the running time of common operations can scale even
   worse than linear (e.g., your implementation of 'mult' above
   probably runs in time proportional to the product of n and m).  *)

(** You may be wondering: "Computer hardware uses a *binary* encoding
   of numbers. Why can't we do that"? The answer is that we
   can. Consider the following type 'pos' of binary-encoded positive
   integers: *)

type pos =
  | H
  | O of pos
  | I of pos

(** The type 'pos' has three constructors, 'H', 'O', and 'I'. They can
   be understood as follows:

   * 'H' stands for 1.
   * 'O n', the 'O' constructor applied to a pos n, stands for 2 * n.
   * 'I n', the 'I' constructor applied to a pos n, stands for 2 * n + 1.

   This interpretation of pos is codified by the function 'int_of_pos': *)

let rec int_of_pos = function
  | H -> 1
  | O n -> 2 * int_of_pos n
  | I n -> 2 * int_of_pos n + 1

(** Testing voodoo (feel free to ignore). *)
let rec string_of_pos : pos -> string = function
  | H -> "H"
  | O n' -> "O " ^ string_of_pos n'
  | I n' -> "I " ^ string_of_pos n'
let rec pos_of_int : int -> pos = function
  | n when n <= 1 -> H
  | n -> if n mod 2 = 0 then O (pos_of_int (n / 2)) else I (pos_of_int (n / 2))
let pos : pos testable =
  let pp_pos ppf x = Fmt.pf ppf "%s" (string_of_pos x) in
  testable pp_pos ( = )
let pos_gen = QCheck.Gen.(map pos_of_int (int_bound 20))
let arbitrary_pos =
  let open QCheck.Iter in
  let shrink_pos = function
    | H -> empty
    | O n' -> return n'
    | I n' -> return n'
  in
  QCheck.make pos_gen ~print:string_of_pos ~shrink:shrink_pos

(** Some example pos values for testing. *)
let one   = H
let two   = O one
let three = I one
let four  = O two
let five  = I two
let six   = O three

(** 9. (10 pts) Define a function 'psucc' that computes the successor
   of a pos. HINT: structural recursion on n. *)

let rec psucc (n : pos) : pos =
  match n with
  | H -> O H
  | O n -> I n
  | I n -> O (psucc n)

let () = add_test "psucc" @@
           fun _ ->
           (check pos) "psucc 1 = 2" two (psucc one);
           (check pos) "psucc 2 = 3" three (psucc two);
           (check pos) "psucc 3 = 4" four (psucc three);
           (check pos) "psucc 4 = 5" five (psucc four)

(** 10. (10 pts) Using 'psucc', define a function 'pplus' that computes
   the sum of two numbers. HINT: structural recursion on n. *)

(* n + m *)
let rec pplus (n : pos) (m : pos) : pos =
  match n with
  | H -> psucc m
  | O n -> pplus n (pplus n m)
  | I n -> psucc (pplus n (pplus n m))

let () = add_test "pplus" @@
           fun _ ->
           (check pos) "1 + 1 = 2" two (pplus one one);
           (check pos) "1 + 2 = 3" three (pplus one two);
           (check pos) "2 + 1 = 3" three (pplus two one);
           (check pos) "2 + 2 = 4" four (pplus two two);
           (check pos) "2 + 3 = 5" five (pplus two three);
           (check pos) "3 + 2 = 5" five (pplus three two)

(** 11. (10 pts) Using 'pplus', define a function 'pmult' that computes
   the product of two numbers. HINT: structural recursion on n. *)

(* n · m *)
let rec pmult (n : pos) (m : pos) : pos =
  match n with
  |H -> m
  |O n -> pplus (pmult n m) (pmult n m)
  |I n -> pplus m (pplus (pmult n m) (pmult n m))

let () = add_test "pmult" @@
           fun _ ->
           (check pos) "1 * 3 = 3" three (pmult one three);
           (check pos) "2 * 2 = 4" four (pmult two two);
           (check pos) "2 * 3 = 6" six (pmult two three)

(** For those curious, we can make use of 'pos' to represent the
   integers (including 0 and negative integers): *)

(* Integers *)
type z =
  | Z0
  | Zpos of pos
  | Zneg of pos

(** and then rational numbers as integer/positive pairs
   (numerator/denominator). *)

(* Rationals *)
type q =
  { num : z
  ; den : pos }

(** You could imagine developing an entire library for symbolic
   computation with rational numbers, with all the inner pieces
   thoroughly verified by QCheck. That could be a lot of fun, but we
   must move on to other things. As always, you're free to experiment
   on your own! *)
let () = ()
