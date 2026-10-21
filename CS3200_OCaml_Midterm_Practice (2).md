# CS3200 OCaml Midterm — Coding Question Bank and Solutions

Based on the 16 PDFs in the uploaded lecture archive. These are plausible practice questions, **not known exam questions**. The slides also cover testing and the design of solutions; use the practice prompts to solve problems independently before looking at the answers.

## 1. Basics, recursion, and pattern matching

**Q1.** Implement factorial recursively and with a tail-recursive helper.
```ocaml
let rec fact n =
  if n < 0 then invalid_arg "fact" else
  if n = 0 then 1 else n * fact (n - 1)

let fact_tail n =
  if n < 0 then invalid_arg "fact_tail" else
  let rec loop acc n =
    if n = 0 then acc else loop (acc * n) (n - 1)
  in loop 1 n
```
**Q2.** Implement Fibonacci, a sum from 1 to n, and Euclidean GCD.
```ocaml
let rec fib n =
  if n < 0 then invalid_arg "fib" else
  if n < 2 then n else fib (n - 1) + fib (n - 2)

let sum_to n =
  if n < 0 then invalid_arg "sum_to" else
  let rec loop acc k = if k = 0 then acc else loop (acc + k) (k - 1) in
  loop 0 n

let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)
```
**Q3.** Implement list length, sum, reversal, membership, and maximum without `List` helpers.
```ocaml
let length xs =
  let rec go acc = function [] -> acc | _ :: tl -> go (acc + 1) tl in
  go 0 xs
let sum xs = List.fold_left ( + ) 0 xs
let reverse xs =
  let rec go acc = function [] -> acc | h :: t -> go (h :: acc) t in
  go [] xs
let rec contains x = function [] -> false | h :: t -> h = x || contains x t
let maximum = function
  | [] -> None
  | h :: t -> Some (List.fold_left max h t)
```
**Q4.** Remove adjacent duplicates and all duplicate occurrences while retaining first appearances.
```ocaml
let rec dedup_adjacent = function
  | [] -> []
  | [x] -> [x]
  | x :: (y :: _ as tl) -> if x = y then dedup_adjacent tl else x :: dedup_adjacent tl

let dedup xs =
  let rec go seen = function
    | [] -> List.rev seen
    | x :: tl -> if List.mem x seen then go seen tl else go (x :: seen) tl
  in go [] xs
```
**Q5.** Implement take, drop, and zip.
```ocaml
let rec take n xs = match n, xs with
  | n, _ when n <= 0 -> []
  | _, [] -> []
  | n, x :: tl -> x :: take (n - 1) tl
let rec drop n xs = match n, xs with
  | n, _ when n <= 0 -> xs
  | _, [] -> []
  | n, _ :: tl -> drop (n - 1) tl
let rec zip xs ys = match xs, ys with
  | x :: xt, y :: yt -> (x, y) :: zip xt yt
  | _ -> []
```

## 2. Higher-order programming

**Q6.** Square every even number using a pipeline.
```ocaml
let even_squares xs =
  xs |> List.filter (fun x -> x mod 2 = 0) |> List.map (fun x -> x * x)
(* even_squares [1;2;3;4] = [4;16] *)
```
**Q7.** Reimplement map, filter, and fold_left recursively.
```ocaml
let rec my_map f = function [] -> [] | x :: xs -> f x :: my_map f xs
let rec my_filter p = function
  | [] -> []
  | x :: xs -> if p x then x :: my_filter p xs else my_filter p xs
let rec my_fold_left f acc = function
  | [] -> acc
  | x :: xs -> my_fold_left f (f acc x) xs
```
**Q8.** Implement compose, twice, and a curried add.
```ocaml
let compose f g x = f (g x)
let twice f x = f (f x)
let add x y = x + y
(* twice (( + ) 3) 4 = 10; compose (( * ) 2) (( + ) 1) 4 = 10 *)
```
**Q9.** Implement count_if, group counts, and flatten using folds.
```ocaml
let count_if p xs = List.fold_left (fun acc x -> if p x then acc + 1 else acc) 0 xs
let frequencies xs =
  List.fold_left
    (fun counts x ->
      let previous = match List.assoc_opt x counts with None -> 0 | Some n -> n in
      (x, previous + 1) :: List.remove_assoc x counts)
    [] xs
let flatten xss = List.fold_right ( @ ) xss []
```
**Q10.** Explain the difference between left and right folds by implementing both.
```ocaml
let rec fold_right f xs acc = match xs with
  | [] -> acc
  | x :: tl -> f x (fold_right f tl acc)
(* fold_left (-) 0 [1;2;3] = -6
   fold_right (-) [1;2;3] 0 = 2 *)
```

## 3. Variants, options, and natural numbers

**Q11.** Implement safe division and option mapping.
```ocaml
let safe_div x y = if y = 0 then None else Some (x / y)
let option_map f = function None -> None | Some x -> Some (f x)
```
**Q12.** Create Peano natural numbers with conversion, addition, multiplication.
```ocaml
type nat = Z | S of nat
let rec int_of_nat = function Z -> 0 | S n -> 1 + int_of_nat n
let rec nat_of_int n =
  if n < 0 then invalid_arg "nat_of_int"
  else if n = 0 then Z else S (nat_of_int (n - 1))
let rec nat_plus a b = match a with Z -> b | S a' -> S (nat_plus a' b)
let rec nat_mult a b = match a with Z -> Z | S a' -> nat_plus b (nat_mult a' b)
let nat_pred = function Z -> Z | S n -> n
```
**Q13.** Use a variant to calculate areas.
```ocaml
type shape = Circle of float | Rectangle of float * float
let area = function
  | Circle r -> Float.pi *. r *. r
  | Rectangle (w, h) -> w *. h
```
**Q14.** Use a record to update a player without mutating the old value.
```ocaml
type player = { name : string; score : int }
let add_points n p = { p with score = p.score + n }
```

## 4. Binary trees — especially important

These definitions follow the shape used in the Trees Lab slides. Height counts nodes: `Leaf` has height 0.
```ocaml
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
let example =
  Node (4, Node (2, Leaf, Leaf),
        Node (6, Node (5, Leaf, Leaf), Node (9, Leaf, Leaf)))
```
**Q15.** Implement tree size, sum, height, leaf count, and count of nodes with one child.
```ocaml
let rec tree_size = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + tree_size l + tree_size r
let rec tree_sum = function
  | Leaf -> 0
  | Node (x, l, r) -> x + tree_sum l + tree_sum r
let rec tree_height = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (tree_height l) (tree_height r)
let rec leaf_count = function
  | Leaf -> 0
  | Node (_, Leaf, Leaf) -> 1
  | Node (_, l, r) -> leaf_count l + leaf_count r
let rec one_child_count = function
  | Leaf -> 0
  | Node (_, l, r) ->
      let one = match l, r with
        | Leaf, Node _ | Node _, Leaf -> 1
        | _ -> 0 in
      one + one_child_count l + one_child_count r
```
**Q16.** Implement inorder, preorder, postorder traversals.
```ocaml
let rec inorder = function
  | Leaf -> []
  | Node (x, l, r) -> inorder l @ [x] @ inorder r
let rec preorder = function
  | Leaf -> []
  | Node (x, l, r) -> [x] @ preorder l @ preorder r
let rec postorder = function
  | Leaf -> []
  | Node (x, l, r) -> postorder l @ postorder r @ [x]
(* example: inorder [2;4;5;6;9]; preorder [4;2;6;5;9];
   postorder [2;5;9;6;4] *)
```
**Q17.** Implement tree map, mirror, and a general fold.
```ocaml
let rec tree_map f = function
  | Leaf -> Leaf
  | Node (x, l, r) -> Node (f x, tree_map f l, tree_map f r)
let rec mirror = function
  | Leaf -> Leaf
  | Node (x, l, r) -> Node (x, mirror r, mirror l)
let rec tree_fold op acc = function
  | Leaf -> acc
  | Node (x, l, r) -> op x (tree_fold op acc l) (tree_fold op acc r)
let size_fold t = tree_fold (fun _ a b -> 1 + a + b) 0 t
let height_fold t = tree_fold (fun _ a b -> 1 + max a b) 0 t
let sum_fold t = tree_fold (fun x a b -> x + a + b) 0 t
let inorder_fold t = tree_fold (fun x a b -> a @ [x] @ b) [] t
```
**Q18.** Compute the maximum root-to-leaf path sum and minimum root-to-leaf path sum.
```ocaml
let rec max_path = function
  | Leaf -> None
  | Node (x, Leaf, Leaf) -> Some x
  | Node (x, l, r) ->
      let options = List.filter_map (fun x -> x) [max_path l; max_path r] in
      Some (x + List.fold_left max min_int options)
let rec min_path = function
  | Leaf -> None
  | Node (x, Leaf, Leaf) -> Some x
  | Node (x, l, r) ->
      let options = List.filter_map (fun x -> x) [min_path l; min_path r] in
      Some (x + List.fold_left min max_int options)
(* Do not use 0 as a fake missing branch for minimum paths. *)
```
**Q19.** Implement both meanings of filtering a tree: prune entire failing subtrees, or keep passing values in inorder.
```ocaml
let rec filter_prune p = function
  | Leaf -> Leaf
  | Node (x, l, r) ->
      if p x then Node (x, filter_prune p l, filter_prune p r) else Leaf
let rec filter_keep p = function
  | Leaf -> []
  | Node (x, l, r) ->
      filter_keep p l @ (if p x then [x] else []) @ filter_keep p r
```
**Q20.** Implement level lists and the width of a tree.
```ocaml
let levels t =
  let rec go current =
    match current with
    | [] -> []
    | _ ->
        let values = List.filter_map
          (function Leaf -> None | Node (x, _, _) -> Some x) current in
        if values = [] then []
        else
          let next = List.concat_map (function
            | Leaf -> []
            | Node (_, l, r) -> [l; r]) current in
          values :: go next
  in go [t]
let width t = List.fold_left (fun m xs -> max m (List.length xs)) 0 (levels t)
```
**Q21.** Implement BST membership and insertion (no duplicates).
```ocaml
let rec bst_mem x = function
  | Leaf -> false
  | Node (v, l, r) ->
      if x = v then true else if x < v then bst_mem x l else bst_mem x r
let rec bst_insert x = function
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (v, l, r) as t ->
      if x = v then t
      else if x < v then Node (v, bst_insert x l, r)
      else Node (v, l, bst_insert x r)
let bst_of_list xs = List.fold_left (fun t x -> bst_insert x t) Leaf xs
```
**Q22.** Implement BST removal, including the two-child case using the minimum of the right subtree.
```ocaml
let rec bst_min = function
  | Leaf -> None
  | Node (x, Leaf, _) -> Some x
  | Node (_, l, _) -> bst_min l
let rec bst_remove x = function
  | Leaf -> Leaf
  | Node (v, l, r) ->
      if x < v then Node (v, bst_remove x l, r)
      else if x > v then Node (v, l, bst_remove x r)
      else match l, r with
        | Leaf, _ -> r
        | _, Leaf -> l
        | _, _ -> match bst_min r with
          | None -> assert false
          | Some successor -> Node (successor, l, bst_remove successor r)
```
**Q23.** Check the BST ordering invariant using lower/upper bounds.
```ocaml
let bst_valid t =
  let rec go lo hi = function
    | Leaf -> true
    | Node (x, l, r) ->
        (match lo with None -> true | Some v -> v < x) &&
        (match hi with None -> true | Some v -> x < v) &&
        go lo (Some x) l && go (Some x) hi r
  in go None None t
```
**Q24.** Implement AVL rotations and insertion using ordinary binary trees. This is correct but recomputes heights and is not optimized.
```ocaml
let rotate_left = function
  | Node (x, a, Node (y, b, c)) -> Node (y, Node (x, a, b), c)
  | t -> t
let rotate_right = function
  | Node (y, Node (x, a, b), c) -> Node (x, a, Node (y, b, c))
  | t -> t
let balance_factor = function
  | Leaf -> 0
  | Node (_, l, r) -> tree_height l - tree_height r
let rebalance = function
  | Leaf -> Leaf
  | Node (x, l, r) as t ->
      let b = balance_factor t in
      if b > 1 then
        let l' = if balance_factor l < 0 then rotate_left l else l in
        rotate_right (Node (x, l', r))
      else if b < -1 then
        let r' = if balance_factor r > 0 then rotate_right r else r in
        rotate_left (Node (x, l, r'))
      else t
let rec avl_insert x = function
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (v, l, r) as t ->
      if x = v then t
      else if x < v then rebalance (Node (v, avl_insert x l, r))
      else rebalance (Node (v, l, avl_insert x r))
```
**Q25.** Verify AVL balance recursively.
```ocaml
let rec avl_balanced = function
  | Leaf -> true
  | Node (_, l, r) ->
      abs (tree_height l - tree_height r) <= 1 &&
      avl_balanced l && avl_balanced r
```

## 5. Modules, sets, queues, and mutation

**Q26.** Define a module with a signature that hides a counter's implementation.
```ocaml
module type COUNTER = sig
  type t
  val create : unit -> t
  val increment : t -> unit
  val value : t -> int
end
module Counter : COUNTER = struct
  type t = int ref
  let create () = ref 0
  let increment c = incr c
  let value c = !c
end
```
**Q27.** Implement list-based set insertion, union, intersection, and difference.
```ocaml
let set_insert x xs = if List.mem x xs then xs else x :: xs
let set_union xs ys = List.fold_left (fun acc x -> set_insert x acc) ys xs
let set_intersection xs ys = List.filter (fun x -> List.mem x ys) xs
let set_difference xs ys = List.filter (fun x -> not (List.mem x ys)) xs
(* Assume inputs are sets, i.e. contain no duplicates. *)
```
**Q28.** Implement a persistent queue with two lists.
```ocaml
type 'a queue = { front : 'a list; back : 'a list }
let empty_queue = { front = []; back = [] }
let normalize q =
  match q.front with [] -> { front = List.rev q.back; back = [] } | _ -> q
let enqueue x q = normalize { q with back = x :: q.back }
let dequeue q =
  match (normalize q).front with
  | [] -> None
  | x :: xs -> Some (x, normalize { front = xs; back = q.back })
```
**Q29.** Implement a mutable counter and an in-place array reversal.
```ocaml
let make_counter () =
  let count = ref 0 in
  fun () -> incr count; !count
let reverse_array a =
  let n = Array.length a in
  for i = 0 to (n / 2) - 1 do
    let j = n - 1 - i in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done
```
**Q30.** Memoize an integer-to-integer function using a hash table.
```ocaml
let memoize f =
  let cache = Hashtbl.create 32 in
  fun x ->
    match Hashtbl.find_opt cache x with
    | Some result -> result
    | None -> let result = f x in Hashtbl.add cache x result; result
```

## 6. Interpreters, testing, and correctness

**Q31.** Define a small expression language and write its evaluator.
```ocaml
type expr =
  | Int of int
  | Add of expr * expr
  | Mul of expr * expr
  | Neg of expr
  | Var of string
  | Let of string * expr * expr
let rec eval env = function
  | Int n -> n
  | Add (a, b) -> eval env a + eval env b
  | Mul (a, b) -> eval env a * eval env b
  | Neg x -> -(eval env x)
  | Var x -> (match List.assoc_opt x env with
      | Some n -> n | None -> failwith ("Unbound variable: " ^ x))
  | Let (x, value, body) ->
      let n = eval env value in
      eval ((x, n) :: env) body
(* eval [] (Let ("x", Int 3, Add (Var "x", Int 4))) = 7 *)
```
**Q32.** Implement a simple type checker for arithmetic expressions.
```ocaml
let rec valid_arithmetic = function
  | Int _ -> true
  | Add (a, b) | Mul (a, b) -> valid_arithmetic a && valid_arithmetic b
  | Neg x -> valid_arithmetic x
  | Var _ | Let _ -> false
(* This is a deliberately restricted syntax checker, not a full type inference engine. *)
```
**Q33.** Write assertions for core list and tree functions.
```ocaml
let () =
  assert (fact_tail 5 = 120);
  assert (dedup [1; 2; 1] = [1; 2]);
  assert (tree_size example = 5);
  assert (tree_height example = 3);
  assert (inorder example = [2; 4; 5; 6; 9]);
  assert (bst_valid example);
  assert (avl_balanced (avl_insert 3 (avl_insert 2 (avl_insert 1 Leaf))))
```
**Q34.** Write property-based invariants that can be checked with QCheck or manual randomized tests.
```ocaml
let reverse_involution xs = reverse (reverse xs) = xs
let map_identity xs = my_map (fun x -> x) xs = xs
let insert_membership x xs = bst_mem x (bst_insert x (bst_of_list xs))
let insert_preserves_bst x xs = bst_valid (bst_insert x (bst_of_list xs))
```

## 7. Five-question mock midterm

Try these WITHOUT viewing the answers above, then check your implementations with the assertions below.

1. **Tail recursion:** Write `range : int -> int -> int list` that returns all integers from `lo` through `hi`, inclusive, using a tail-recursive helper. Return `[]` if `lo > hi`.
2. **Higher-order functions:** Write `sum_odd_squares : int list -> int` using `List.filter`, `List.map`, and `List.fold_left` (no explicit recursion).
3. **Types and options:** Write `mean : int list -> float option` returning `None` on the empty list.
4. **Trees:** Define `'a tree`, then implement `tree_fold`, `tree_height`, `inorder`, and `bst_insert`.
5. **Testing and data structures:** Implement `dedup : 'a list -> 'a list` retaining the first occurrence of each value, and provide at least three edge-case tests.

### Mock midterm reference answers
```ocaml
let range lo hi =
  let rec loop i acc =
    if i < lo then acc else loop (i - 1) (i :: acc)
  in if lo > hi then [] else loop hi []

let sum_odd_squares xs =
  xs
  |> List.filter (fun x -> x mod 2 <> 0)
  |> List.map (fun x -> x * x)
  |> List.fold_left ( + ) 0

let mean xs = match xs with
  | [] -> None
  | _ ->
      let total, count =
        List.fold_left (fun (s, n) x -> (s + x, n + 1)) (0, 0) xs in
      Some (float_of_int total /. float_of_int count)

(* For mock Q4 use the definitions of tree_fold, tree_height,
   inorder, and bst_insert from Q15-Q21 above. *)
(* For mock Q5 use dedup from Q4 above. *)

let () =
  assert (range 2 5 = [2;3;4;5]);
  assert (range 5 2 = []);
  assert (sum_odd_squares [1;2;3;4] = 10);
  assert (mean [] = None);
  assert (mean [2;4;6] = Some 4.);
  assert (dedup [] = []);
  assert (dedup [3;3;3] = [3]);
  assert (dedup [1;2;1] = [1;2])
```

## 8. GitHub submission rehearsal

A typical Dune project separates code and tests. Follow the actual midterm repository's file names and instructions instead of assuming the same layout as an assignment.

```bash
opam switch cs3200
eval $(opam env)
ocaml -version
dune build
dune test
git status
git add .
git commit -m "Complete OCaml midterm solutions"
git push origin main
```

Check the assignment's policy on permitted resources and AI assistance during the real exam. Practice writing these implementations yourself so you can explain their pattern matches, recursive calls, invariants, and edge cases.

## 9. Priority and common pitfalls

- **Trees:** `Leaf` versus `Node`; correct base cases; traversal order; BST insertion/deletion; AVL rotation direction; fold accumulator traps.
- **Recursion:** A function is tail-recursive only if the recursive call is the last operation; `x :: recurse xs` is not tail-recursive.
- **Folds:** `fold_left` calls `f accumulator element`; `fold_right` calls `f element accumulated_right`.
- **Options:** Handle both `None` and `Some` and avoid unsafe `Option.get`.
- **Modules:** The signature limits which definitions callers can see.
- **Mutation:** `!r` reads a ref; `r := x` writes it; `a.(i) <- x` mutates an array.
- **Tests:** Include empty inputs, singleton inputs, duplicates, boundary values, and structurally different trees.

---

## Additional questions from an actual class quiz (provided by student)

These three prompts appeared on a previous quiz, so they are particularly useful practice. They are not a guarantee of midterm content.

### Quiz Q1 — Inclusive range (no `List.init`)

**Prompt:** Write `range : int -> int -> int list` returning all integers from the first argument through the second, inclusive, in increasing order. If the first argument is larger, return `[]`. Do not use `List.init`.

```ocaml
let rec range first last =
  if first > last then []
  else first :: range (first + 1) last
```

**How it works:** When `first > last`, recursion stops. Otherwise, `first` becomes the head of the list, and the function generates the remaining integers.

```ocaml
assert (range 2 5 = [2; 3; 4; 5]);;
assert (range 3 3 = [3]);;
assert (range 5 2 = []);;
```

**Related variations:** Write a tail-recursive `range` with an accumulator; write a descending range; implement an infix `( -- )` operator using `let ( -- ) a b = range a b`.

### Quiz Q2 — Count elements satisfying a predicate

**Prompt:** Write `count_if : ('a -> bool) -> 'a list -> int` using pattern matching or a fold. Do not use `List.filter` followed by `List.length`.

```ocaml
let rec count_if predicate lst =
  match lst with
  | [] -> 0
  | x :: xs ->
      if predicate x then 1 + count_if predicate xs
      else count_if predicate xs
```

**How it works:** An empty list contains zero matches. For each nonempty list, test the head and add one only when the predicate is true.

**Alternative using a fold:**

```ocaml
let count_if predicate xs =
  List.fold_left
    (fun count x -> if predicate x then count + 1 else count)
    0 xs
```

```ocaml
assert (count_if (fun x -> x mod 2 = 0) [1; 2; 3; 4; 6] = 3);;
assert (count_if (fun x -> x > 10) [1; 2; 3] = 0);;
```

**Related variations:** Implement `exists`, `for_all`, or `sum_if` using recursion or a fold.

### Quiz Q3 — Squares of even numbers using `filter` and `map`

**Prompt:** Write `squares_of_evens : int list -> int list` that keeps only even numbers, squares them, and preserves order. Use `List.filter` and `List.map`.

```ocaml
let squares_of_evens xs =
  List.map (fun x -> x * x)
    (List.filter (fun x -> x mod 2 = 0) xs)
```

**How it works:** `List.filter` keeps only even inputs; `List.map` squares each retained value. Both preserve relative order.

**Pipeline variation:**

```ocaml
let squares_of_evens xs =
  xs
  |> List.filter (fun x -> x mod 2 = 0)
  |> List.map (fun x -> x * x)
```

```ocaml
assert (squares_of_evens [1; 2; 3; 4; 5; 6] = [4; 16; 36]);;
assert (squares_of_evens [1; 3; 5] = []);;
```

**Related variations:** Cube the odd numbers; filter multiples of three and double them; implement the same function with `List.fold_right`.

### What these quiz questions suggest studying

The quiz directly tested three different solution styles: ordinary recursion that builds a list (`range`), predicate-based recursion or folding (`count_if`), and higher-order list processing (`squares_of_evens`). Practice writing each solution from scratch, identifying the base case, and tracing the result on a short example.

---

## Actual Unit-Testing Activity — OUnit tests for `Stats`

**Activity prompt:** Paste the five tests from `test/test_stats.ml` (first TODO through last), then identify in one sentence which test found the bug. Stretch: add a QCheck property for `dedup` or `clamp`.

**Your five tests (formatted as list entries inside the existing OUnit test list):**

```ocaml
(* TODO 1: clamp *)
("clamp above maximum" >:: fun _ ->
  assert_equal 10 (Stats.clamp 0 10 15) ~printer:string_of_int);

(* TODO 2: mean of a non-empty list *)
("mean of non-empty list" >:: fun _ ->
  assert_equal (Some 3.0) (Stats.mean [1; 3; 5]));

(* TODO 3: median of an even-length list *)
("median of even-length list" >:: fun _ ->
  assert_equal 2.5 (Stats.median [4; 1; 3; 2])
    ~printer:string_of_float);

(* TODO 4: median of [] raises Stats.Empty *)
("median of empty raises exception" >:: fun _ ->
  assert_raises Stats.Empty (fun () -> Stats.median []));

(* TODO 5: dedup *)
("dedup removes duplicates" >:: fun _ ->
  assert_equal [1; 2; 3] (Stats.dedup [1; 2; 2; 3; 1]));
```

**Which test found the bug?** The even-length median test found the bug because the function returned `3.0` instead of the expected `2.5`.

### What each test checks

1. **Clamp:** A value above the upper bound should be capped at the maximum (`15` becomes `10`).
2. **Mean:** The arithmetic mean of `[1; 3; 5]` is `3.0`, returned as `Some 3.0`.
3. **Median:** Sort `[4; 1; 3; 2]` to `[1; 2; 3; 4]`; average the middle two values: `(2.0 +. 3.0) /. 2.0 = 2.5`.
4. **Exception:** Calling `Stats.median []` must raise `Stats.Empty`.
5. **Dedup:** Repeated values are removed, preserving the order of their first occurrence in this example.

**OUnit syntax:** `>::` names a test, `assert_equal expected actual` compares results, `~printer` formats failures, and `assert_raises` verifies exceptions. Put these entries in the test list already provided by the starter file; don't add an extra semicolon after the last entry if the enclosing list syntax doesn't need it.

### Stretch: QCheck property for `clamp`

```ocaml
let clamp_stays_in_bounds =
  QCheck.Test.make
    ~name:"clamp stays within ordered bounds"
    QCheck.(triple small_int small_int small_int)
    (fun (a, b, x) ->
      let lo = min a b in
      let hi = max a b in
      let y = Stats.clamp lo hi x in
      lo <= y && y <= hi)
```

To run this with OUnit, add `QCheck_ounit.to_ounit2_test clamp_stays_in_bounds` to your suite **if** your project includes `qcheck-ounit`. Alternatively, run the property directly using `QCheck.Test.check_exn clamp_stays_in_bounds` in a suitable test executable.

**Other possible midterm variations:** Test the lower clamp boundary, test `mean []`, test odd-length median, test `dedup []`, write a failing test before fixing a function, or add a QCheck property that `dedup` never produces repeated elements.

# Homework Addendum — PA1, PA2, PA3, and RTS-Arena

These are the actual uploaded homework implementations and assignment comments, not invented exam questions. Each section preserves the source function signatures and surrounding instructions. Study the patterns and reimplement them without looking. The source code is included for reference; some implementations may still have issues, so use `dune test` in the original repository to verify them.


## PA1 — Recursive Lists

**Practice prompts:** Implement `len`, `append`, `snoc`, `rev` using `snoc`, `flatten` using `append`, and insertion sort (`isort`). Pay attention to list pattern matching and base cases.

### Source: `lib/lib.ml`

```ocaml
(************************************************************************
0. Fill your name and OU ID or PID on the line below:
 *)
let _ = Printf.printf "\n-----NAME: %s ID:%s -----\n" name id;;
open Alcotest
open Util

(** PART I (Lists):

   1. (10 pts) As a warmup, define a function 'len' that computes the
   length of a list, producing an int result.

   NOTE: Don't use any library functions (like 'List.length') in this
   exercise. Instead, use pattern matching and recursion on the list
   to calculate the length. *)

let rec len (l : 'a list) : int =
   match l with (*pattern matching examnes what the list looks like*)
  | [] -> 0 (*base case: an empty list that has zero elements*)
  | _ :: rest -> 1 + len rest(*recursive case: _ takes place of the first element and the rest is everything after the first element*)

(** The following tests check that your 'len' function behaves as
   expected on a few specific inputs. *)
let () = add_test "len" @@
           fun _ ->
           (check int) "len [0] = 1" 1 (len [0]);
           (check int) "len [] = 0" 0 (len []);
           (check int) "len [1; 2; 3; 4; 5] = 5" 5 (len [1; 2; 3; 4; 5]);
           (check int) "len [1; 2] = 2" 2 (len [1; 2]);
           (check int) "len [false] = 1" 1 (len [false]);
           (check int) "the cosine of u root divided by.." 1 (len [["heimerdinger"]])

(** 2. (10 pts) Define a function 'append' that takes two 'a list
   arguments l1 and l2, for any type 'a, and returns the concatenation
   of the two lists. That is, your function should return a single
   list that contains, first, all the elements of l1, then all the
   elements of l2. *)

let rec append (l1 : 'a list) (l2 : 'a list) : 'a list =
 match l1 with (*looks at the first list*)
  | [] -> l2 (*base case: if l1 is empty, return l2*)
(*recursive case: first = first element of l1 and the rest is the remaining elements of l1*)
  | first :: rest -> first :: append rest l2(*keep the first at the front then recursively go through l2*)
let () = add_test "append" @@
           fun _ ->
           (check @@ list int) "[] ++ [] = []" [] (append [] []);
           (check @@ list char) "['a'] ++ [] = ['a']" ['a'] (append ['a'] []);
           (check @@ list (float epsilon_float)) "[1.0] ++ [] = [1.0]" [1.0] (append [1.0] []);
           (check @@ list int) "[1; 2] ++ [3] = [1; 2; 3]" [1; 2; 3] (append [1; 2] [3]);
           (check @@ list (list int)) "[[]] ++ [[]] = [[]; []]" [[]; []] (append [[]] [[]])

(** In addition to handwritten test cases like those above, we will
   sometimes use QCheck to test general properties that are expected
   to hold of your code. For example, the following test checks that
   the empty list is the left identity wrt. 'append': *)

(* [∀ l, [] ++ l = l] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"append_left_identity" ~count:200
                     (small_list int)
                     (fun l -> append [] l = l))

(** QCheck works by randomly generating values (lists of integers in
   this case) and checking that a given predicate evaluates to true
   for all of them. Here (above) we told QCheck to generate 200 random
   lists using the generator 'small_list int' (specifying how random
   lists should be generated), and to check that the predicate 'fun l
   -> append [] l = l' always evaluates to true when applied to them.

   You're free to experiment with QCheck, but you aren't required to
   write any QCheck tests. We provide tests to help guide your
   implementation, and to help us with grading. Your implementation
   should pass all of the tests that we provide.

   The file '/test/pa1.ml' contains more QCheck tests that your
   'append' and other functions are expected to pass. *)

(** 3. (10 pts) Define a function 'snoc' that links a value of type 'a
   onto the *end* of a 'a list. For example: snoc(1, [3; 2]) should
   result in [3; 2; 1]. 'snoc' is the reverse of 'cons'.

   HINT: Structure your program, as you presumably did in exercises 1
   and 2, as a case analysis on the input list l. Think recursively in
   the case when l is a cons(f, r), with head or front element f and
   tail or rest r. What's a recursive way in which to move the new
   element "a" toward the end of the list, while maintaining the
   correct structure of the list up to that point? *)

let rec snoc (a : 'a) (l : 'a list) : 'a list =
  match l with(*look at the list*)
  | [] -> [a](*base case: if list is empty create a new list with a in it*)
  | first :: rest -> first :: snoc a rest(*recursive case: keep the first element where it is and recursively move toward the end of the list*)


let () = add_test "snoc" @@
           fun _ ->
           (check @@ list int) "snoc 1 [3; 2] = [3; 2; 1]" [3; 2; 1] (snoc 1 [3; 2]);
           (check @@ list char) "snoc 'z' [] = ['z']" ['z'] (snoc 'z' []);
           (check @@ list bool) "snoc false [true; false] = [true; false; false]"
             [true; false; false] (snoc false [true; false])

(** 4. (20 pts) Using your implementation of snoc in #3, define a
   function 'rev' that reverses a list. For example, rev [1; 2; 3]
   should result in [3; 2; 1]. *)

let rec rev (l : 'a list) : 'a list =
    match l with(*look at the list*)
  | [] -> [](*Base case the reverse of an empty list is still empty*)
  | first :: rest -> snoc first (rev rest)(*Recursive case: recursively reverse the rest of the list*)
(*then use snoc to put the original first element at the end of that reversed list*)
let () = add_test "rev" @@
           fun _ ->
           (check @@ list int) "rev [1; 2; 3] = [3; 2; 1]" [3; 2; 1] (rev [1; 2; 3]);
           (check @@ list char) "rev [] = []" [] (rev []);
           (check @@ list int) "rev [1] = [1]" [1] (rev [1]);
           (check @@ list bool) "rev [false; true] = [true; false]"
             [true; false] (rev [false; true])

(** 5. (20 pts) Using your append function from #2, define a function
   'flatten' that takes a list of lists and flattens it to a single
   list. For example, flatten [[1]; []; [2; 3]] should result in the
   list [1; 2; 3]. *)

let rec flatten (l : 'a list list) : 'a list =
  match l with(*look at the outer list*)
  | [] -> [](*Base case:there ar eno lists left to combine so return an empty list*)
  | first :: rest -> append first (flatten rest)(*recursive case: first is the first inner list*)
(*rest is the remaining inner lists then append the first list to the flattened version of the remaining lists*)
let () = add_test "flatten" @@
           fun _ ->
           (check @@ list int) "flatten [[1]; []; [2; 3]] = [1; 2; 3]"
             [1; 2; 3] (flatten [[1]; []; [2; 3]]);
           (check @@ list char) "flatten [[]] = []" [] (flatten [[]]);
           (check @@ list (list int)) "flatten [[[]]] = [[]]" [[]] (flatten [[[]]]);
           (check @@ list int) "flatten [[1; 2; 3]] = [1; 2; 3]"
             [1; 2; 3] (flatten [[1; 2; 3]]);
           (check @@ list bool) "flatten [[false]] = [false]"
             [false] (flatten [[false]]);
           (check @@ list bool) "flatten [[false]; []] = [false]"
             [false] (flatten [[false]; []])

(** 6. (30 pts) Define a function 'isort' that uses the insertion sort
   algorithm to sort a list of integers. *)

let rec isort (l : int list) : int list =
  let rec insert (a : int) (xs : int list) : int list =(*inserts a xs this takes an integer [a] and inserts it into an
  already sorted list[xs] in the correct position*)
     match xs with(*looks at the sorted list*)
    | [] -> [a](*if the list is empty then a becomes the only element*)
    | first :: rest ->(*first is first element and rest is the remaining elements*)
        if a <= first then(*if a is smaller than or equal to first put a before first*)
          (*since xs is already sorted the rest of xs is already in the correct position*)
          a :: xs
        else(*else first belongs before a and keep first and recursively search for the correct position of a in rest*)
          first :: insert a rest
      (*in means insert is a local function, it can only be used inside isort*)
  in
   match l with(*go through the original list*)
  | [] -> [](*base case: an empty list is already sorted*)
  | first :: rest -> insert first (isort rest)(*recursive case: first is first number and rest is the remaining numbers
  first sort rest recursively and then insert first into the sorted rest*)

let () = add_test "isort" @@
           fun _ ->
           (check @@ list int) "isort [3; 2; 1] = [1; 2; 3]" [1; 2; 3] (isort [3; 2; 1]);
           (check @@ list int) "isort [] = []" [] (isort []);
           (check @@ list int) "isort [1] = [1]" [1] (isort [1]);
           (check @@ list int) "isort [3; 5; -7; 1; 0; 200] = [-7; 0; 1; 3; 5; 200]"
             [-7; 0; 1; 3; 5; 200] (isort [3; 5; -7; 1; 0; 200])
```


## PA2 — Inductive Natural Numbers

**Practice prompts:** Implement `pred`, `plus`, `mult`, `fact`, `pow`, saturating `minus`, mutually recursive `even`/`odd`, `leq`, and binary-positive `psucc`, `pplus`, `pmult`. Identify base and successor cases.

### Source: `lib/lib.ml`

```ocaml
(************************************************************************
0. Fill your name and OHIO ID on the line below:
*)

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
  | O -> O
  | S n' -> n'(*returns the predecessor of a natural number*)
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
   match n with(*if n is 0 then the answer is m*)
  | O -> m
  | S n' -> S (plus n' m)(*if not then recursively add m to the predecessor of n*)

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
  | O -> O
  | S n' -> plus m (mult n' m)(*for every successor in n we add another m*)

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
  | O -> one
  | S n' -> mult n (fact n')(*calculates the factorial of a natural number*)

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
  | O -> one
  | S m' -> mult n (pow n m')(*raises n to the power of m*)

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
   match n, m with
  | O, _ -> O
  | n, O -> n
  | S n', S m' -> minus n' m'(*subtracts m from n if m is larger then n the result is 0*)
(*the function recursively removes one successor from each number*)
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
  | O -> true(*checks if the natural number is even. 0 is even.*)
  | S n' -> odd n'(*checks the previous number of the successor if it is odd*)

and odd (n : nat) : bool =
  match n with
  | O -> false(*checks if the natural number is odd. 0 is odd*)
  | S n' -> even n'(*check if the previous number of the successor is even*)

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
  match n, m with(*checks if n is less than or equal to m*)
  | O, _ -> true (*0 is always less than or equal to another natural number*)
  | S _, O -> false
  | S n', S m' -> leq n' m'(*if m reaches 0 first, n must be greater than m*)

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
   match n with(*finds the successor of a positive binary number*)
  | H -> O H (*H reps 1 so it's successor is 0, H which is 2*)
  | O n' -> I n'(*if the number ends in 0, then changing it to 1 gives the successor*)
  | I n' -> O (psucc n')(*if it ends in 1 we move the addition to the next digit*)

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
  | H -> psucc m(*H represents 1 so add 1 to m using psucc*)
  | O n' ->(*n begins with a binary 0.*)
      (match m with
       | H -> psucc n(*2n plus 1 when m is 1*)
       | O m' -> O (pplus n' m')(*both numbers have 0 in this position so keep 
       the 0 and recursively add the remaining digits*)
       | I m' -> I (pplus n' m'))(*the digits are 0 and 1. keep the 1 and recursively add the digits*)
  | I n' ->(*n begins with a binary 1.*)
      (match m with
       | H -> psucc n(*add 1 to me*)
       | O m' -> I (pplus n' m')(*the digits are 1 and 0. keep the 1 and recursively add the remaining digits*)
       | I m' -> O (psucc (pplus n' m')))(*both digits are one so carry is needed. the result has a 0 in this position and the carry is added to the remaining digits*)



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
  | H -> m(*1 times m is equal to n*)
  | O n' -> pplus (pmult n' m) (pmult n' m)(*0 n' reps 2 times n so multiply the recursive result by 2 by adding it to itself*)
  | I n' -> pplus m (pplus (pmult n' m) (pmult n' m))(*I n' reps 2 times n plus 1 so we calculate 2 times n times m and then add another additonal m*)

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
```


## PA3 — Sets, BSTs, and Red-Black Trees

**Practice prompts:** Implement `dedup`, list-set insert/member/union/intersection, BST insert/member/union/intersection, and red-black invariants and set operations. Explain the BST ordering and red-black invariants.

### Source: `lib/lib.ml`

```ocaml
(************************************************************************
0. Fill your name and OU ID or PID on the line below:
 *)

open QCheck
open Alcotest

open Set
open Util

(** [Sets]

   In this assignment you will develop a collection of Set modules
   that can be used for working with sets in future assignments. The
   file 'set.ml' contains the Set interface. Take a look at it now. It
   specifies the operations that every Set module must define. The Set
   interface is given as a module type (or "signature"). See section
   5.2 of the book to learn about modules and module types:
   https://cs3110.github.io/textbook/chapters/modules/modules.html.

   The file '/test/pa3.ml' contains a test module for sets. The test
   module is instantiated with three different implementations: one
   based on lists, one based on binary search trees, and one based on
   balanced binary search trees. All three are defined in the current
   file below, but they are incomplete. It's your job to fill in the
   missing pieces.  *)

(** Part 1 (List sets)

   The first implementation of the Set signature uses lists to store
   the elements. We enforce a simple invariant: lists that represent
   sets do not contain any duplicate elements. *)

(* A checker for the SetList invariant. Evaluates to true iff the
   given list contains no duplicates. *)
let rec nodup (l : 'a list) : bool =
  match l with
  | [] -> true
  | x :: xs -> List.for_all (fun y -> x <> y) xs && nodup xs

(** 1. (10 pts) Write a function 'dedup' that removes duplicate
   elements from a list. That is, the output should be a list
   containing all the same elements as the input except for
   duplicates. Hint: structural recursion and List.filter. *)

let rec dedup (l : 'a list) : 'a list =
  match l with(*checks list*)
  | [] -> [](*returns empty list if nothing is left*)
  | x :: xs ->
      x :: dedup (List.filter (fun y -> y <> x) xs)(*keeps x and removes any copies of it*)

let () = add_test "dedup" @@
           fun _ ->
           (check @@ list int) "" [] (dedup []);
           (check @@ list int) "" [1] (dedup [1]);
           (check @@ list int) "" [1] (dedup [1; 1]);
           (check @@ list int) "" [1; 2; 3] (dedup [1; 2; 3]);
           (check @@ list int) "" [1; 2; 3] (dedup [1; 2; 1; 3]);
           (check @@ list int) "" [1; 2; 3] (dedup [1; 2; 2; 1; 3]);
           (check @@ list char) "" ['m'; 'o'] (dedup ['m'; 'o'; 'o'])

(* [∀ l, nodup (dedup l))] *)
let () = add_qcheck @@
           QCheck.Test.make ~name:"nodup_dedup" ~count:100
             (small_list small_int)
             (fun l -> nodup (dedup l))

(* [∀ l, l ⊆ dedup l] *)
let () = add_qcheck @@
           QCheck.Test.make ~name:"dedup_sound" ~count:100
             (small_list small_int)
             (fun l -> let l' = dedup l in
                       List.for_all (fun x -> List.mem x l') l)

(* List-backed sets. *)
module ListSet : functor (T : Type) ->
                 Set with type elt = T.t with type t = T.t list =
  functor
    (T : Type) -> struct
    type elt = T.t
    type t = T.t list

    (* QCheck generator for lists with no duplicates. *)
    let gen = QCheck.make (Gen.map dedup (Gen.small_list T.gen.gen))

    (* Render list to string. *)
    let show l = string_of_list T.show l

    (* The empty set is represented by the empty list. *)
    let empty = []

    (* A singleton set is represented by a list containing a single element. *)
    let single x = [x]

    (** 2. (10 pts) Write the 'insert' function for ListSets. Remember
       to preserve the ListSet invariant: no duplicates! Hint:
       'List.mem' could be useful. https://v2.ocaml.org/api/List.html *)
    let insert (x : elt) (l : elt list) : elt list =
      if List.mem x l then l(*returns list if item is already there *)
  else x :: l(*otherwise adds item to the front *)

    (** 3. (10 pts) Write the 'elem' function for ListSets. This should
       be easy if you attended class on Sep 7 :) *)
    let rec elem (x : elt) (l : elt list) : bool =
      match l with(*checks the list *)
  | [] -> false(*returns false if list is empty *)
  | y :: ys ->
      x = y || elem x ys(*checks current item or the rest *)

    (* 4. (10 pts) Write the 'union' function for ListSets. Hint:
       'insert' could be useful. *)
    let rec union (a : elt list) (b : elt list) : elt list =
      match a with (*goes through the first set*)
  | [] -> b (*returns second set when first is empty*)
  | x :: xs ->
      insert x (union xs b)(*inserts each item into the result*)

    (* 5. (10 pts) Write the 'intersection' function for
       ListSets. Hint: 'List.filter' could be useful. *)
    let intersection (a : elt list) (b : elt list) : elt list =
       List.filter (fun x -> elem x b) a(*only keeps items from a that are also in b *)
    (* Use dedup to establish the ListSet invariant. *)
    let of_list = dedup

    (* Trivial since already a list. *)
    let to_list l = l

    (* a ⊆ b iff ∀ x, x ∈ a → x ∈ b *)
    let subset a b = List.for_all (fun x -> List.mem x b) a

    (* First check that lengths are equal to avoid the more expensive
       subset operations when the lists are obviously not
       equal. Otherwise, compute the symmetric closure of the subset relation. *)
    let eq a b = List.length a = List.length b && subset a b && subset b a

    let inv = nodup
  end

(** Part 2 (BST sets)

   Now you will implement the set interface using the following
   datatype, 'a bst, which defines polymorphic binary search trees
   parameterized by the type 'a of data contained at the leaves.  *)

type 'a bst =
  | Leaf
  | Node of 'a * 'a bst * 'a bst

(* Curried form of node constructor for convenience. *)
let node x l r = Node (x, l, r)

(* Render bst to string. *)
let rec string_of_bst (show : 'a -> string) : 'a bst -> string = function
  | Leaf -> "Leaf"
  | Node (x, l, r) -> "Node (" ^ show x ^ ", " ^
                        string_of_bst show l ^ ", " ^
                          string_of_bst show r ^ ")"

(* Testing voodoo. *)
let bst (show : 'a -> string) : ('a bst) testable =
  let pp_btree ppf t = Fmt.pf ppf "%s" (string_of_bst show t) in
  testable pp_btree ( = )

(** For BSTs we enforce an even stronger invariant: BSTs contain no
   duplicates, and are in sorted order.

   That is:
     for any [Node (x, l, r)], we have
       y < x for all y ∈ l, and
       x < z for all z ∈ r.

   In English: At every node, all elements appearing in the left
   subtree are *less than* the current node's element, and all
   elements appearing in the right subtree are *greater than* the
   current node's element. *)

(* Check that a given predicate holds on all elements in a BST. *)
let rec bst_forall (pred : 'a -> bool) (t : 'a bst) : bool =
  match t with
  | Leaf -> true
  | Node (x, l, r) -> pred x && bst_forall pred l && bst_forall pred r

(* Check the BST invariant. *)
let rec bst_inv (lt : 'a -> 'a -> bool) (t : 'a bst) : bool =
  match t with
  | Leaf -> true
  | Node (x, l, r) -> bst_forall (fun y -> lt y x) l &&
                        bst_forall (lt x) r &&
                          bst_inv lt l && bst_inv lt r

module BstSet : functor (Ord : OType) ->
                Set with type elt = Ord.t with type t = Ord.t bst =
  functor
    (Ord : OType) -> struct
    type elt = Ord.t
    type t = Ord.t bst

    (* Render a BST to string. *)
    let show t = string_of_bst Ord.show t

    (* The empty set is a represented by a leaf. *)
    let empty = Leaf

    (* A singleton set is represented by a node with two leaf children. *)
    let single x = Node (x, Leaf, Leaf)

    (* Check for membership of an element in a BST. Exploits the BST
       invariant for efficiency. *)  
    let rec elem x = function
      | Leaf -> false
      | Node (y, l, r) ->
         x = y || elem x (if Ord.le x y then l else r)

    (** 6. (10 pts) Write an 'insert' function for inserting a single
       element into a BST. Make sure to preserve the BST invariant! *)
    let rec insert (x : elt) (t : elt bst) : elt bst =
     match t with(*checks the current tree*)
  | Leaf ->
      Node (x, Leaf, Leaf)(*creates a new node if empty*)
  | Node (y, l, r) ->
      if x = y then
        t(*keeps the tree the same if the value already exists*)
      else if Ord.le x y then
        Node (y, insert x l, r)(*puts smaller values on the left*)
      else
        Node (y, l, insert x r)(*puts larger values on the right*)

    (** 7. (10 pts) Write a 'union' function for taking the union of
       two BSTs. Hint: structural recursion and 'insert'. *)
    let rec union (a : elt bst) (b : elt bst) : elt bst =
       match a with(*checks first tree *)
  | Leaf -> b(*returns second tree if first is empty *)
  | Node (x, l, r) ->
      insert x (union l (union r b))(*combines both subtrees and inserts current value*)

    (** 8. (10 pts) Write an 'intersection' function for taking the
       intersection of two BSTs. Hint: you may find it useful to use
       'union' from above. *)
    let rec intersection (a : elt bst) (b : elt bst) : elt bst =
       match a with(*checks first tree *)
  | Leaf -> Leaf(*no values if the first tree is empty*)
  | Node (x, l, r) ->
      let rest = union (intersection l b) (intersection r b) in
      (*finds common values from both subtrees *)
      if elem x b then insert x rest(*adds current value if it exists in both *)
      else rest(*leaves value out if not shared*)

    (* Convert a list to a BST. *)
    let rec of_list = function
      | [] -> Leaf
      | x :: xs -> insert x (of_list xs)

    (* Convert a BST to a list *)
    let rec to_list = function
      | Leaf -> []
      | Node (x, l, r) -> x :: to_list l @ to_list r

    let subset a b = bst_forall (fun x -> elem x b) a                

    let eq a b = subset a b && subset b a

    let bst_gen : ('a bst) Gen.t =
      Gen.map of_list (Gen.small_list Ord.gen.gen)

    let gen : ('a bst) arbitrary =
      let open QCheck.Iter in
      let rec shrink_bst = function
        | Leaf -> empty
        | Node (x, l, r) ->
           of_list [l; r]
           <+> (shrink_bst l >|= fun l' -> node x l' r)
           <+> (shrink_bst r >|= fun r' -> node x l r')
      in
      QCheck.make bst_gen ~print:(string_of_bst Ord.show) ~shrink:shrink_bst

    let inv = bst_inv Ord.le
  end

(** Part 3 (Red-black tree sets)

   The following data structure, RBTree, defines red-black binary
   trees (essentially just BTrees augmented with color information --
   red or black). If you haven't seen such trees before, or just
   forget, you can refresh your memory by reading the article here:
   https://en.wikipedia.org/wiki/Red%E2%80%93black_tree. *)

type color =
  | Red
  | Black

type 'a rbtree =
  | RBLeaf
  | RBNode of (color * 'a * 'a rbtree * 'a rbtree)

let color_of (t : 'a rbtree) : color =
  match t with
  | RBLeaf -> Black
  | RBNode (c, _, _, _) -> c

let string_of_color = function
  | Red -> "Red"
  | Black -> "Black"

let rec string_of_rbt (show : 'a -> string) : 'a rbtree -> string = function
  | RBLeaf -> "RBLeaf"
  | RBNode (c, x, l, r) ->
     "RBNode (" ^ string_of_color c ^ ", " ^ show x ^ ", " ^
       string_of_rbt show l ^ ", " ^ string_of_rbt show r ^ ")"

(* Curried constructors for convenience. *)
let leaf : 'a rbtree = RBLeaf
let node c x l r = RBNode (c, x, l, r)

(* Some rbtrees for testing: *)
let r1 = node Black 2 (node Red 1 leaf leaf) (node Red 3 leaf leaf)
let r2 = node Red 1 leaf leaf
let r3 = node Black 4
           (node Red 2
              (node Black 1 leaf leaf)
              (node Red 3 leaf leaf))
           (node Red 6
              (node Black 5 leaf leaf)
              (node Black 7 leaf leaf))
let r4 = node Black 4
           (node Red 2
              (node Black 1 leaf leaf)
              (node Black 3 leaf leaf))
           (node Black 6 leaf leaf)
let r5 = node Black 4
           (node Red 2
              (node Black 1 leaf leaf)
              (node Black 3 leaf leaf))
           (node Red 6
              (node Red 5 leaf leaf)
              (node Black 7 leaf leaf))
let r6 = node Black 4
           (node Red 2
              (node Black 1 leaf leaf)
              (node Black 3 leaf leaf))
           (node Red 8
              (node Black 6 leaf leaf)
              (node Black 10 leaf leaf))
let r7 = node Black 4
           (node Red 2
              (node Black 1 leaf leaf)
              (node Black 3 leaf leaf))
           (node Black 6
              (node Black 5 leaf leaf)
              (node Black 7 leaf leaf))
let r8 = node Black 1
           (node Red 3 leaf leaf)
           (node Red 2 leaf leaf)
(* END RBTrees used for testing *)

(** In order for an RBTree to be a valid red-black tree, all of the
   following properties must hold:

   (o) All nodes are either red or black.

   (i) The root node is black.

   (ii) All leaves are black.

   (iii) If a node is red, then both its children are black.

   (iv) Any path from any node in the tree to any of its descendent
   black leaf nodes has the same number of black nodes (not including
   the node itself but including the leaf node) as any other such path
   from that node.

   (v) The tree additionally satisfies the binary search tree
   invariant. These properties together imply that the height of an
   RBTree is pretty much balanced -- no left subtree of a node can be
   more than 2x deeper than the right subtree of that node, and vice
   versa. *)

(** 9. (10 pts) Define a function 'rbt_inv' that determines whether a
   given RBTree satisfies the RBT invariant as described above.

   HINT 1: You probably don't need to directly encode properties (o)
   and (ii) -- they are automatically true of every RBTree by
   construction.

   HINT 2: You may find it helpful to define one function for each
   property (i--iv) above. Then your overall rbt_inv function can just
   string these individual invariant-checking functions together. *)

(* Check that a given predicate holds on all elements in a BST. *)
let rec rbt_forall (pred : 'a -> bool) (t : 'a rbtree) : bool =
  match t with
  | RBLeaf -> true
  | RBNode (_, x, l, r) -> pred x && rbt_forall pred l && rbt_forall pred r

let rbt_inv (le : 'a -> 'a -> bool) (t : 'a rbtree) : bool =
  let root_black t = (* Checks if the root is black *)
    match t with
    | RBLeaf -> true (*empty leaf is valid*)
    | RBNode (c, _, _, _) -> c = Black(*checks the node color*)
  in

  let rec red_children_black t =(*Checks that red nodes have black children*)
    match t with
    | RBLeaf -> true (*empty leaf is fine*)
    | RBNode (c, _, l, r) ->
        (c <> Red ||
         (color_of l = Black && color_of r = Black))(*checks both children if the node is red*)
        && red_children_black l(*checks left side*)
        && red_children_black r(*checks right side*)
  in

  let rec black_height t = (*checks if the black heights are the same *)
    match t with
    | RBLeaf -> Some 1 (*counts the leaf as black*)
    | RBNode (c, _, l, r) ->
        match black_height l, black_height r with
        | Some lh, Some rh when lh = rh ->
            Some (lh + if c = Black then 1 else 0)(*adds one if this node is black*)
        | _ -> None(*heights do not match*)
  in

  let rec bst_order t =(*checks that the tree is in BST order*)
    match t with
    | RBLeaf -> true(*empty tree is valid*)
    | RBNode (_, x, l, r) ->
        rbt_forall (fun y -> le y x && y <> x) l(*makes sure left values are smaller*)
        && rbt_forall (fun y -> le x y && y <> x) r(*makes sure right values are larger*)
        && bst_order l(*checks left subtree*)
        && bst_order r(*checks right subtree*)
  in
(*will return true if all the rules pass*)
  root_black t(* checks root color*)
  && red_children_black t(*checks red node children*)
  && (match black_height t with
      | Some _ -> true(*black heights match*)
      | None -> false)(*black heights don't match*)
  && bst_order t(*checks BST order*)

let () = add_test "rbt_inv" @@
           fun _ ->
           (check Alcotest.bool) "r1 satisfies rbt_inv" true (rbt_inv (<=) r1);
           (check Alcotest.bool) "r2 violates rbt_inv" false (rbt_inv (<=) r2);
           (check Alcotest.bool) "r3 violates rbt_inv" false (rbt_inv (<=) r3);
           (check Alcotest.bool) "r4 satisfies rbt_inv" true (rbt_inv (<=) r4);
           (check Alcotest.bool) "r5 violates rbt_inv" false (rbt_inv (<=) r5);
           (check Alcotest.bool) "r6 satisfies rbt_inv" true (rbt_inv (<=) r6);
           (check Alcotest.bool) "r7 violates rbt_inv" false (rbt_inv (<=) r7);
           (check Alcotest.bool) "r8 violates rbt_inv" false (rbt_inv (<=) r8)

module RbtSet : functor (Ord : OType) ->
                Set with type elt = Ord.t with type t = Ord.t rbtree =
  functor (Ord : OType) -> struct
    type elt = Ord.t
    type t = Ord.t rbtree

    let empty = leaf

    let single x = node Red x leaf leaf

    (* Check for membership of an element in an RBT. Exploits the
       BST invariant for efficiency. *)  
    let rec elem x t =
      match t with
      | RBLeaf -> false
      | RBNode (_, y, l, r) ->
         x = y || if Ord.le x y then elem x l else elem x r

    (* https://www.cs.tufts.edu/comp/150FP/archive/chris-okasaki/redblack99.pdf *)        
    let balance (c : color) (z : 'a) (l : 'a rbtree) (r : 'a rbtree) : 'a rbtree =
      match (c, l, r) with
      | (Black, RBNode (Red, y, RBNode (Red, x, a, b), c), d) ->
         node Red y (node Black x a b) (node Black z c d)
      | (Black, RBNode (Red, x, a, RBNode (Red, y, b, c)), d) ->
         node Red y (node Black x a b) (node Black z c d)
      | (Black, a, RBNode (Red, x, RBNode (Red, y, b, c), d)) ->
         node Red y (node Black z a b) (node Black x c d)
      | (Black, a, RBNode (Red, y, b, RBNode (Red, x, c, d))) ->
         node Red y (node Black z a b) (node Black x c d)
      | _ -> node c z l r

    (* Insert an element in an RBT while preserving rbt_inv. *)
    let insert (x : 'a) (t : 'a rbtree) : 'a rbtree =
      let make_black (b : 'a rbtree) : 'a rbtree =
        match b with
        | RBLeaf -> RBLeaf
        | RBNode (_, y, l, r) -> RBNode(Black, y, l, r)
      in
      let rec ins (b : 'a rbtree) : 'a rbtree =
        match b with
        | RBLeaf -> node Red x leaf leaf
        | RBNode (c, y, l, r) ->
           if x = y then node c y l r
           else if Ord.le x y then balance c y (ins l) r
           else balance c y l (ins r)
      in
      make_black (ins t)

    (** 10. (5 pts) Write a 'union' function for taking the union of
       two RBTs. Hint: similar to #6. *)
    let rec union (a : elt rbtree) (b : elt rbtree) : elt rbtree =
       match a with(*checks first tree*)
  | RBLeaf -> b(*returns second tree if first is empty*)
  | RBNode (_, x, l, r) ->
      insert x (union l (union r b))(*combines both subtrees and inserts current value*)

    (** 11. (5 pts) Write an 'intersection' function for taking the
       intersection of two RBTs. Hint: similar to #7. *)
    let rec intersection (a : elt rbtree) (b : elt rbtree) : elt rbtree =
      match a with(*checks first tree*)
  | RBLeaf -> leaf (*no values if first tree is empty*)
  | RBNode (_, x, l, r) ->
      let rest = union (intersection l b) (intersection r b) in (*finds common values from both subtrees*)
      if elem x b then insert x rest(*adds current value if it exists in both *)
      else rest(*leaves value out if not shared*)

    let rec of_list l =(*converts a list into a red-black tree*)
      match l with(*checks list*)
      | [] -> leaf(*empty list becomes empty tree*)
      | x :: xs -> insert x (of_list xs)(*inserts each value into the tree *)

    let rec to_list t =(*converts a red-black tree into a list*)
      match t with(*checks tree*)
      | RBLeaf -> [](*empty tree returns empty list*)
      | RBNode (_, x, l, r) -> x :: to_list l @ to_list r(*adds current value and both subtrees*)

    let subset a b = rbt_forall (fun x -> elem x b) a(*checks if all values from a are in b*)

    let eq a b = subset a b && subset b a(*checks if two sets have the same values*)

    let gen = QCheck.make (Gen.map of_list (Gen.small_list Ord.gen.gen))
                ~print:(string_of_rbt Ord.show)(*generates random red-black trees for testing*)

    let show = string_of_rbt Ord.show(*converts the red-black tree into a string*)

    let inv = rbt_inv Ord.le (*checks that the tree follows red-black tree rules*)
  end
```


## RTS-Arena — Higher-Order Pipelines and Rules

**Practice prompts:** Implement counting by kind, army value, nearest enemy, nearby threats, idle workers, income; evaluate a game state using record differences; implement rule combinators (`always`, `when_`, `guard`, `first_of`, `all_of`, `to_bot`). Note the original no-recursion and no-`List.length` restriction for queries.

### Source: `student/bot.ml`

```ocaml
(************************************************************************
   RTS-1, Part C2 — Your bot (25 pts)

   Fill in your name and Ohio ID: *)

(* Build [my_bot] out of the combinators in rules.ml and the queries in
   queries.ml. Two rules for this file:

   - No [if] and no [match] anywhere in it (the tests check). Decisions are
     made by [when_], [guard] and [first_of], with predicates like the ones
     below.
   - [my_bot] stays a function: keep the [fun v -> ... v] around it, so that
     nothing runs until the game asks.

   The starter below gathers, builds a barracks, and then never trains a
   single soldier. It loses to every built-in bot. Graded matches are on the default 16x16 map,
   500 turns, and you must win on both sides. Try things with:

     dune exec bin/main.exe -- play my-bot light-rush --show
     dune exec test/test_rts1.exe -- test bot
 ************************************************************************)

open Rtsarena
open Queries
open Rules

(* ---- Given: predicates and board-dependent rules ------------------- *)

(** How many Light, Heavy and Ranged units you have. *)
let army (v : view) : int = count_of v Light + count_of v Heavy + count_of v Ranged

(** Enemy soldiers within [r] of your base. *)
let under_attack (r : int) (v : view) : bool = threats_near v ~radius:r <> []

(** Attack the enemy base (or, once it is gone, whatever is left). *)
let attack_enemy : rule = fun v ->
  Option.map (fun (x, y) -> [ Attack (x, y) ]) (enemy_target v)

(* ---- Your bot ------------------------------------------------------- *)

let economy : rule = (*controls the bot's workers and resource gathering*)
   first_of [(*selects the first rule whose condition is satisfied*)
    when_ (fun v -> v.v_turn < 60)(*before turn sixty the bot sends 4 workers to harvest and trains 2 workers
    after turn 60 the bot continues harvesting with three workers*)
      [ Harvest_with 4; Train (Worker, 2) ];
    always [ Harvest_with 3 ]
  ]

let production : rule =
   first_of [(*ensures first production rule is selected*)
    when_ (fun v -> not (has_barracks v))(*if it doesn't have a barracks build one*)
      [ Build_barracks ];
    when_ (fun v -> army v < 3)(*if fewer than 3 military units train 3 light units*)
      [ Train (Light, 3) ];
    when_ (fun v -> army v < 6)(*if army has fewer than 6 military units, train 2 heavy units*)
      [ Train (Heavy, 2) ];
    always [ Train (Ranged, 1) ](*else train 1 ranged*)
  ]

let defense : rule =
  when_ (under_attack 4) [ Defend ](*protects the bot's base when enemy threats are detected within a radius of 4 spaces*)

let attack : rule =   guard
    (fun v -> army v >= 3)
    attack_enemy(*allows bot to attack once it has at least 3 military units*)

let my_bot : bot = fun v ->(*combines all the bot's individual strategies into one bot*)
    to_bot
      (all_of [(*combines individual rules into one rule set*)
        economy;(*executed in this order*)
        production;
        defense;
        attack
      ])
      v
```

### Source: `student/eval.ml`

```ocaml
(************************************************************************
   RTS-1, Part B — An evaluation function (20 pts)

   [eval v] answers "how well am I doing?" as one number: positive means
   winning, negative means losing, 0.0 means even. RTS-3 will call it
   thousands of times to compare futures, so it has to mean something.

   Same rules as queries.ml: no rec, no List.length (comments are skipped).

   Build it in three steps:
     1. [side] measures ONE side: a record of plain numbers.
     2. [features] is my side minus their side, field by field.
     3. [eval] is the weighted sum of those differences.

   Built that way, eval is antisymmetric for free: whatever the position,
   what player 0 sees is exactly the negative of what player 1 sees. The
   tests check that on positions from real games.
 ************************************************************************)

open Rtsarena


(** What we measure about one side. Keep these fields; you may add more,
    but then add a matching weight below. *)
type side = {
  army : float;       (** total cost of combat units *)
  economy : float;    (** resources in the stockpile, plus what workers carry *)
  workers : float;    (** number of workers *)
  hp : float;         (** total hit points of everything *)
  base : float;       (** 1.0 if the base is standing, 0.0 if not *)
  barracks : float;   (** 1.0 if at least one barracks is standing *)
}

(** One weight per field of [side]. *)
type weights = {
  w_army : float;
  w_economy : float;
  w_workers : float;
  w_hp : float;
  w_base : float;
  w_barracks : float;
}

(** 1. (6 pts) [side units resources] measures one side, given all of its
    entities and its stockpile. Use [Queries.is_combat]. *)
let side (units : entity list) (resources : int) : side =(*this finds all the features for one side of the board*)
   { army =(*list of entities owned by the player and resources is the player's current stockpile of resources*)
      units(*each feature is calculated seperately *)
      |> List.filter Queries.is_combat(*keeep only combat units in Queries.is_combat*)
      |> List.map (fun e -> float_of_int (stats_of_kind e.kind).cost)(*then convert each combat unit into it's resource cost using stats_of_kind*)
      |> List.fold_left ( +. ) 0.0;(*then add them all together*)
    economy =
      units
      |> List.map (fun e -> e.carrying)(*gets the amount of each entity is carrying*)
      |> List.fold_left ( + ) resources(*adds the resources to the stockpile then converts it to float *)
      |> float_of_int;
    workers =
      units
      |> List.filter is_worker(*identifies the worker units*)
      |> List.map (fun _ -> 1.0)(*each worker is converted into 1.0 and all the values are added together*)
      |> List.fold_left ( +. ) 0.0;
    hp =
  units(*get the hp of every entity owned by the player then convert each integer HP value into a float*)
  |> List.map (fun (e : entity) -> float_of_int e.hp)(*then add all the HP values together*)
  |> List.fold_left ( +. ) 0.0;(*this finds the total amount of health remaining across the player's entities*)
    base =(*checks if one entitiy is a base. 1.0 means there is a base *)
      if List.exists (fun e -> e.kind = B Base) units then 1.0 else 0.0;
    barracks =(*checks to see if the player has at least one barracks. 1.0 means there is at least one barracks*)
      if List.exists (fun e -> e.kind = B Barracks) units then 1.0 else 0.0 }

(** 2. (6 pts) [features v] is (my side) minus (their side), field by field.
    Your units are [v.my_units] with [v.my_resources]; theirs are
    [v.enemy_units] with [v.enemy_resources]. *)
let features (v : view) : side =
  let mine = side v.my_units v.my_resources in(*my value minus opponent's value*)
  let theirs = side v.enemy_units v.enemy_resources in
  { army = mine.army -. theirs.army;(*my army - opponents army and continue through all features*)
    economy = mine.economy -. theirs.economy;
    workers = mine.workers -. theirs.workers;
    hp = mine.hp -. theirs.hp;
    base = mine.base -. theirs.base;
    barracks = mine.barracks -. theirs.barracks }

(** 3. (4 pts) Your weights. The ones given here are a starting point that
    predicts some games; tune them. Say in README.md what you changed and
    why. *)
let weights : weights =(*determines how strong each feature contributes to the final score*)
  { w_army = 4.0; w_economy = 0.5; w_workers = 0.25; w_hp = 0.75;
    w_base = 15.0; w_barracks = 2.0 }

(** 4. (4 pts) [eval_with w v] is the weighted sum of [features v]. *)
let eval_with (w : weights) (v : view) : float =(*calculates the final evaluation score for a particular game position using the supplied weights*)
   let f = features v in(*use features and multiply each feature difference by it's weight*)
  w.w_army *. f.army(*add all resulting values together to get final score*)
  +. w.w_economy *. f.economy
  +. w.w_workers *. f.workers
  +. w.w_hp *. f.hp
  +. w.w_base *. f.base
  +. w.w_barracks *. f.barracks


(** Given. *)
let eval (v : view) : float = eval_with weights v
```

### Source: `student/queries.ml`

```ocaml
(************************************************************************
   RTS-1, Part A — Queries (30 pts)

   Fill in your name and Ohio ID in bot.ml, not here.

   Every function in this file takes what a player can see (a [view]) and
   answers one question about it. The rules for this file:

   - No [let rec]. No [List.length].
   - Every answer is a pipeline of List.map / List.filter / List.fold_left /
     List.sort / List.find_opt / ... joined with [|>].

   The autograder reads this file and rejects the keyword rec and the name
   List.length anywhere in the code. (Comments are skipped, so this one is
   fine.)

   Useful things from the engine (engine/rtsarena.ml):
     v.my_units, v.enemy_units : entity list   (both include buildings)
     is_building e, is_worker e : bool
     dist a b : int                            Manhattan distance
     unit_stats u, building_stats b, stats_of_kind k : stats   (.cost, .hp, ...)
 ************************************************************************)

open Rtsarena


(** A combat unit is anything that is neither a building nor a worker:
    Light, Heavy, or Ranged. Given — use it. *)
let is_combat (e : entity) : bool = (not (is_building e)) && not (is_worker e)

(** 1. (5 pts) [count_by_kind v] counts your own entities (units AND
    buildings) by kind. One pair per kind you actually have, sorted by kind
    with [compare]. Kinds you have none of do not appear.

    At the start of a game: [[(U Worker, 2); (B Base, 1)]]. *)
let count_by_kind (v : view) : (kind * int) list =(*counts the player's entities by their kind*)
  v.my_units(*units and buildings are both included into this function*)
  |> List.fold_left(*uses list.fold_left to process each entity*)
       (fun counts e ->
          let old = List.assoc_opt e.kind counts |> Option.value ~default:0 in
          (e.kind, old + 1) :: List.remove_assoc e.kind counts)
       []
  |> List.sort (fun (k1, _) (k2, _) -> compare k1 k2)(*sort the final list by entity kind*)

(** 2. (5 pts) [army_value v] is the total [cost] of your combat units.
    Workers and buildings do not count. *)
let army_value (v : view) : int =
  v.my_units
  |> List.filter is_combat(*keeps only combat units*)
  |> List.map (fun e -> (stats_of_kind e.kind).cost)(*converts each combat unit into it's resource cost using stats_of_kind*)
  |> List.fold_left ( + ) 0(*all costs together*)

(** 3. (5 pts) [nearest_enemy v u] is the enemy entity (unit or building)
    closest to [u] by Manhattan distance, or [None] if there are no enemies.
    Ties go to the smaller [id]. *)
let nearest_enemy (v : view) (u : entity) : entity option =
   v.enemy_units(*enemy buildings and enemy units that can be selected*)
  |> List.fold_left(*keeps track of the best enemy found*)
       (fun best e ->
          match best with
          | None -> Some e
          | Some b ->(*compare the current enemy with the best enemy found so far*)
              let de = dist u e and db = dist u b in(*calculate the distance from [u] to both the current enemy and the best current enemy*)
              if de < db || (de = db && e.id < b.id) then Some e else best)(*then choose the closer enemy and if they are the same then choose the one with the smaller id*)
       None

(** 4. (5 pts) [threats_near v ~radius] is every enemy COMBAT unit within
    [radius] (inclusive) of your base, nearest to the base first, ties by
    smaller [id]. If you have no base, the answer is [[]]. *)
let threats_near (v : view) ~(radius : int) : entity list =
   match List.find_opt (fun e -> e.kind = B Base) v.my_units with(*search the player's entities for a Base. *)
  | None -> [](*if no base was found then there is no threats near the base*)
  | Some base ->(*if a base is found then check all visible enemy entities*)
      v.enemy_units
      |> List.filter (fun e -> is_combat e && dist base e <= radius)(*keep only enemy combat units that are a distance away from the base that is equal to or less than the specified radius*)
      |> List.sort (fun a b ->(*sort threats by distance from the base. If two threats have the same distance then the smaller entity id is placed first*)
           let da = dist base a and db = dist base b in
           if da <> db then compare da db else compare a.id b.id)

(** 5. (5 pts) [idle_workers v] is your workers whose [action] is [None],
    in the order they appear in [v.my_units]. *)
let idle_workers (v : view) : entity list =
   v.my_units(*uses List.filter to find a worker with no current action*)
  |> List.filter (fun e -> is_worker e && e.action = None)(*a worker is considered idle if e.action = None*)

(** 6. (5 pts) [income evs ~player] is the total amount [player] deposited
    at their base, over a list of events (what [step] returns). Only
    [Deposited] events count. *)
let income (evs : event list) ~(player : int) : int =
  evs(*keep only the Deposited events for the requested player and extract the amount from each event*)
  |> List.filter_map (function
       | Deposited { player = p; amount } when p = player -> Some amount
       | _ -> None)
  |> List.fold_left ( + ) 0(*add all deposited amounts together*)
```

### Source: `student/rules.ml`

```ocaml
(************************************************************************
   RTS-1, Part C1 — Rule combinators (15 pts)

   A bot is a function [view -> order list]. Writing one as a long chain of
   if/else gets unreadable fast, so instead we build bots out of small
   pieces called rules, and functions that combine rules.

   A [rule] looks at the board and either has an opinion (Some orders) or
   says "not my situation" (None).

   These are higher-order functions: most of them take functions and return
   a function. Keep each one short. Recursion IS allowed in this file, but
   none of them needs it.
 ************************************************************************)

open Rtsarena

type rule = view -> order list option

(** The orders [to_bot] falls back on when no rule fires. Given. *)
let default_orders : order list = [ Harvest_with 2; Defend ]

(** 1. (2 pts) [always orders] is a rule that always fires with [orders]. *)
let always (orders : order list) : rule =
   fun _ -> Some orders(*_ means that supplied view is ignored*)

(** 2. (2 pts) [when_ p orders] fires with [orders] when [p v] holds, and is
    [None] otherwise. *)
let when_ (p : view -> bool) (orders : order list) : rule =
  fun v -> if p v then Some orders else None(*if [p v] is false then None. 
    This is useful when orders are fixed but only should happen during a specfic period*)

(** 3. (2 pts) [guard p r] is [r] when [p v] holds, and [None] otherwise.
    [when_] is for fixed orders; [guard] is for orders that depend on the
    board, like where to attack. *)
let guard (p : view -> bool) (r : rule) : rule =
  fun v -> if p v then r v else None(*if [p v] is true then r v if [p v] is false. None*)

(** 4. (3 pts) [first_of rules] tries the rules in order and gives the first
    answer that is not [None]. [first_of []] never fires.
    Hint: List.find_map. *)
let first_of (rules : rule list) : rule =
   fun v -> List.find_map (fun r -> r v) rules(*evalutes rules from left to right and returns the first value that is [None]*)

(** 5. (3 pts) [all_of rules] runs EVERY rule and concatenates the orders of
    those that fire, in order. It fires if at least one of them did, and is
    [None] if none did. This is how you run an economy rule and an army
    rule side by side. *)
let all_of (rules : rule list) : rule =
  fun v ->(*Run every rule and keep only the rules that fired*)
    let fired = List.filter_map (fun r -> r v) rules in
    match fired with
    | [] -> None(*if no rules fired then return [None]*)
    | _ -> Some (List.flatten fired)(*At least one rule fired*)

(** 6. (3 pts) [to_bot r] is a bot: the orders of [r] when it fires,
    [default_orders] when it does not. *)
let to_bot (r : rule) : bot =
    fun v ->
    match r v with(*the rule fired, so use the orders it produced.*)
    | Some orders -> orders
    | None -> default_orders(* No rule fired, so use the orders it produced*)
```


## Five-question mock midterm based on the uploaded homework

1. **Lists (PA1):** Write `flatten` and `isort` recursively. Test empty, singleton, already-sorted and reverse-sorted inputs.
2. **Algebraic data types (PA2):** Implement Peano `plus` and `mult` and test `S (S O)` multiplied by `S (S (S O))`.
3. **Higher-order functions (RTS):** Given a list of `(kind, cost, is_combat)` triples, use `List.filter` and `List.fold_left` to compute total combat cost without recursion or `List.length`.
4. **Trees and sets (PA3):** Implement BST `elem` and `insert`, then write an invariant checker that verifies strict ordering with lower and upper bounds.
5. **Testing and rule combinators (unit testing + RTS):** Write OUnit tests for empty and nonempty inputs, then implement `first_of` over rules of type `'a -> 'b option`.

**GitHub rehearsal:** From the repository root, run `eval $(opam env)`, `dune build`, and `dune test`; inspect failures, commit your work, push to the required private repository, and follow the instructor's exact tag/ZIP submission directions.
