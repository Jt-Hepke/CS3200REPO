open QCheck_alcotest

(** Testing voodoo`. Feel free to ignore. *)

let todo (type t) (x : t) : 'a =
  let module M = struct exception Todo of t end in
  raise @@ M.Todo x

let tests = ref []
let qcheck_tests = ref []

let add_test nm t = tests := !tests @ [Alcotest.test_case nm `Quick t]
let add_qcheck_ t = tests := !tests @ [to_alcotest t]

let add_qcheck t = qcheck_tests := !qcheck_tests @ [to_alcotest t]
