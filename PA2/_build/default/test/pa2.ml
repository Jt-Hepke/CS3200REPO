open Pa2__Lib
open Pa2__Util

(* 0 is the additive identity on the left. *)
(* [∀ n, 0 + n = n] *)
let () = add_qcheck @@
           QCheck.Test.make ~name:"plus_left_identity" ~count:200
             arbitrary_nat (fun n -> plus O n = n)

(* 0 is the additive identity on the right. *)
(* [∀ n, n + 0 = n] *)
let () = add_qcheck @@
           QCheck.Test.make ~name:"plus_right_identity" ~count:200
             arbitrary_nat (fun n -> plus n O = n)

(* Addition is commutative. *)
(* [∀ n m, n + m = m + n] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"plus_comm" ~count:200
                     (pair arbitrary_nat arbitrary_nat)
                     (fun (n, m) -> plus n m = plus m n))

(* Addition is associative. *)
(* [∀ a b c, (a + b) + c = a + (b + c)] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"plus_assoc" ~count:200
            (triple arbitrary_nat arbitrary_nat arbitrary_nat)
            (fun (a, b, c) -> plus (plus a b) c = plus a (plus b c)))

(* [∀ n m, S (n + m) = S n + m] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"plus_distr_l" ~count:200
            (pair arbitrary_nat arbitrary_nat)
            (fun (n, m) -> S (plus n m) = plus (S n) m))

(* [∀ n m, S (n + m) = n + S m] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"plus_distr_r" ~count:200
            (pair arbitrary_nat arbitrary_nat)
            (fun (n, m) -> S (plus n m) = plus n (S m)))

(* 1 is the multiplicative identity on the left. *)
(* [∀ n, 1 * n = n] *)
let () = add_qcheck @@
           QCheck.Test.make ~name:"mult_left_identity" ~count:200
             arbitrary_nat (fun n -> mult (S O) n = n)

(* 1 is the multiplicative identity on the right. *)
(* [∀ n, n * 1 = n] *)
let () = add_qcheck @@
  QCheck.Test.make ~name:"mult_right_identity" ~count:200
    arbitrary_nat (fun n -> mult n (S O) = n)

(* 0 is the multiplicative annihilator on the left. *)
(* [∀ n, 0 * n = 0] *)
let () = add_qcheck @@
  QCheck.Test.make ~name:"mult_left_annihilator" ~count:200
    arbitrary_nat (fun n -> mult O n = O)

(* 0 is the multiplicative annihilator on the right. *)
(* [∀ n, n * 0 = 0] *)
let () = add_qcheck @@
  QCheck.Test.make ~name:"mult_right_annihilator" ~count:200
    arbitrary_nat (fun n -> mult n O = O)

(* Multiplication is commutative. *)
(* [∀ n m, n * m = m * n] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"mult_comm" ~count:200
            (pair arbitrary_nat arbitrary_nat)
            (fun (a, b) -> mult a b = mult b a))

(* Multiplication is associative. *)
(* [∀ a b c, (a * b) * c = a * (b * c)] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"mult_assoc" ~count:400
            (triple arbitrary_nat arbitrary_nat arbitrary_nat)
            (fun (a, b, c) -> mult a (mult b c) = mult (mult a b) c))

(* Multiplication distributes over addition on the left. *)
(* [∀ a b c, a * (b + c) = a * b + a * c] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"mult_distr_l" ~count:400
            (triple arbitrary_nat arbitrary_nat arbitrary_nat)
            (fun (a, b, c) -> mult a (plus b c) = plus (mult a b) (mult a c)))

(* Multiplication distributes over addition on the right. *)
(* [∀ a b c, (a + b) * c = a * c + b * c] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"mult_distr_r" ~count:400
            (triple arbitrary_nat arbitrary_nat arbitrary_nat)
            (fun (a, b, c) -> mult (plus a b) c = plus (mult a c) (mult b c)))

(* [∀ n, n ≠ 0 → n! = n * (n-1)!] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"fact_mult" ~count:300
            arbitrary_nat (fun n -> assume (n <> O);
                                    fact n = mult n (fact (pred n))))

let small_nat_gen = QCheck.Gen.(map nat_of_int (int_bound 3))
let arbitrary_small_nat = QCheck.make small_nat_gen ~print:string_of_nat

(* [∀ a n m, a^(n+m) = a^n * a^m] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"pow_distr_plus" ~count:200
            (triple arbitrary_small_nat arbitrary_small_nat arbitrary_small_nat)
            (fun (a, n, m) -> pow a (plus n m) = mult (pow a n) (pow a m)))

(* [∀ a b n, (a*b)^n = a^n * b^n *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"pow_distr_mult" ~count:200
            (triple arbitrary_small_nat arbitrary_small_nat arbitrary_small_nat)
            (fun (a, b, n) -> pow (mult a b) n = mult (pow a n) (pow b n)))

(* [∀ a n m, (a^n)^m = a^(n*m) *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"curry" ~count:200
            (triple arbitrary_small_nat arbitrary_small_nat arbitrary_small_nat)
            (fun (a, n, m) -> pow (pow a n) m = pow a (mult n m)))

(* [∀ n, even n ∨ odd n] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"even_or_odd" ~count:100
                     arbitrary_nat
                     (fun n -> even n || odd n))

(* [∀ n, ¬ (even n ∧ odd n)] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"not_even_and_odd" ~count:100
                     arbitrary_nat
                     (fun n -> not (even n && odd n)))

(* [∀ n, even n → odd (S n)] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"even_S_odd" ~count:200
                     arbitrary_nat
                     (fun n -> assume (even n); odd (S n)))

(* [∀ n, odd n → even (S n)] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"odd_S_even" ~count:200
                     arbitrary_nat
                     (fun n -> assume (odd n); even (S n)))

(* [∀ n m, even n → even m → even (n + m)] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"even_even_plus" ~count:400
                     (pair arbitrary_nat arbitrary_nat)
                     (fun (n, m) -> assume (even n && even m);
                                    even (plus n m)))

(* [∀ n m, odd n → even m → odd (n + m)] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"odd_even_plus" ~count:400
                     (pair arbitrary_nat arbitrary_nat)
                     (fun (n, m) -> assume (odd n && even m);
                                    odd (plus n m)))

(* [∀ n m, even n → odd m → odd (n + m)] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"even_odd_plus" ~count:400
                     (pair arbitrary_nat arbitrary_nat)
                     (fun (n, m) -> assume (even n && odd m);
                                    odd (plus n m)))

(* [∀ n m, odd n → odd m → even (n + m)] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"odd_odd_plus" ~count:400
                     (pair arbitrary_nat arbitrary_nat)
                     (fun (n, m) -> assume (odd n && odd m);
                                    even (plus n m)))

(* [∀ n m, even n → even (n · m)] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"even_mult_l" ~count:400
                     (pair arbitrary_nat arbitrary_nat)
                     (fun (n, m) -> assume (even n);
                                    even (mult n m)))

(* [∀ n m, even m → even (n · m)] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"even_mult_r" ~count:400
                     (pair arbitrary_nat arbitrary_nat)
                     (fun (n, m) -> assume (even m);
                                    even (mult n m)))

(* [∀ n m, odd n → odd m → odd (n · m)] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"odd_mult" ~count:400
                     (pair arbitrary_nat arbitrary_nat)
                     (fun (n, m) -> assume (odd n && odd m);
                                    odd (mult n m)))

(* [∀ n, n ≤ n] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"leq_refl" ~count:100
                     arbitrary_nat
                     (fun n -> leq n n))

(* [∀ a b c, a ≤ b → b ≤ c → a ≤ c] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"leq_trans" ~count:400
                     (triple arbitrary_nat arbitrary_nat arbitrary_nat)
                     (fun (a, b, c) -> assume (leq a b);
                                       assume (leq b c);
                                       leq a c))

(* [∀ n, n ≤ S n] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"leq_S" ~count:100
                     arbitrary_nat
                     (fun n -> leq n (S n)))

(* [∀ a b, b ≤ a → a - b + b = a] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"leq_plus_minus" ~count:200
                     (pair arbitrary_nat arbitrary_nat)
                     (fun (a, b) -> assume (leq b a);
                                    plus (minus a b) b = a))
       
(* Addition is commutative. *)
(* [∀ n m, n + m = m + n] *)
let () = add_qcheck @@
           QCheck.(Test.make ~name:"pplus_comm" ~count:200
                     (pair arbitrary_pos arbitrary_pos)
                     (fun (n, m) -> pplus n m = pplus m n))

(* Addition is associative. *)
(* [∀ a b c, (a + b) + c = a + (b + c)] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"pplus_assoc" ~count:200
            (triple arbitrary_pos arbitrary_pos arbitrary_pos)
            (fun (a, b, c) -> pplus (pplus a b) c = pplus a (pplus b c)))

(* [∀ n m, S (n + m) = S n + m] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"pplus_distr_l" ~count:200
            (pair arbitrary_pos arbitrary_pos)
            (fun (n, m) -> psucc (pplus n m) = pplus (psucc n) m))

(* [∀ n m, S (n + m) = n + S m] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"pplus_distr_r" ~count:200
            (pair arbitrary_pos arbitrary_pos)
            (fun (n, m) -> psucc (pplus n m) = pplus n (psucc m)))

(* 1 is the multiplicative identity on the left. *)
(* [∀ n, 1 * n = n] *)
let () = add_qcheck @@
           QCheck.Test.make ~name:"pmult_left_identity" ~count:200
             arbitrary_pos (fun n -> pmult one n = n)

(* 1 is the multiplicative identity on the right. *)
(* [∀ n, n * 1 = n] *)
let () = add_qcheck @@
  QCheck.Test.make ~name:"pmult_right_identity" ~count:200
    arbitrary_pos (fun n -> pmult n one = n)

(* Multiplication is commutative. *)
(* [∀ n m, n * m = m * n] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"pmult_comm" ~count:200
            (pair arbitrary_pos arbitrary_pos)
            (fun (a, b) -> pmult a b = pmult b a))

(* Multiplication is associative. *)
(* [∀ a b c, (a * b) * c = a * (b * c)] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"pmult_assoc" ~count:400
            (triple arbitrary_pos arbitrary_pos arbitrary_pos)
            (fun (a, b, c) -> pmult a (pmult b c) = pmult (pmult a b) c))

(* Multiplication distributes over addition on the left. *)
(* [∀ a b c, a * (b + c) = a * b + a * c] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"pmult_distr_l" ~count:400
            (triple arbitrary_pos arbitrary_pos arbitrary_pos)
            (fun (a, b, c) -> pmult a (pplus b c) = pplus (pmult a b) (pmult a c)))

(* Multiplication distributes over addition on the right. *)
(* [∀ a b c, (a + b) * c = a * c + b * c] *)
let () = add_qcheck @@
  QCheck.(Test.make ~name:"pmult_distr_r" ~count:400
            (triple arbitrary_pos arbitrary_pos arbitrary_pos)
            (fun (a, b, c) -> pmult (pplus a b) c = pplus (pmult a c) (pmult b c)))


(* ============================================================ *)
(* LLM TEST GENERATION (Gemini + OpenAI) — Generic & Fixed JSON *)
(* ============================================================ *)

module LLM_Tests = struct
  type test_case = { name : string; input : Yojson.Basic.t; expected : Yojson.Basic.t }

  let parse_json_test json =
    let open Yojson.Basic.Util in
    {
      name = json |> member "name" |> to_string;
      input = json |> member "input";
      expected = json |> member "expected";
    }

  (* Sanitize malformed JSON (fix OCaml syntax like [[1;2];[3;4]]) *)
  let sanitize_ocaml_syntax s =
    let open Str in
    s
    |> global_replace (regexp ";") ","
    |> global_replace (regexp "\\]\\[") "],[" 
    |> global_replace (regexp "([ \t]*") "["
    |> global_replace (regexp ")[ \t]*") "]"
    |> String.trim

  let clean_content_text text =
    let text = String.trim text in
    let text =
      if String.starts_with ~prefix:"```json" text then
        String.sub text 7 (String.length text - 7)
      else text
    in
    let text =
      if String.ends_with ~suffix:"```" text then
        String.sub text 0 (String.length text - 3)
      else text
    in
    sanitize_ocaml_syntax text

  (* --- Gemini API --- *)
  let call_gemini_api api_key prompt =
    let body =
      Printf.sprintf {|{ "contents": [ { "parts": [ { "text": %S } ] } ] }|} prompt
    in
    let cmd =
      Printf.sprintf
        "curl -s -X POST \
         'https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-pro:generateContent?key=%s' \
         -H 'Content-Type: application/json' -d '%s'"
        api_key body
    in
    let ic = Unix.open_process_in cmd in
    let rec read_all acc =
      try input_line ic |> fun l -> read_all (acc ^ l ^ "\n")
      with End_of_file -> acc
    in
    let result = read_all "" in
    ignore (Unix.close_process_in ic); result

  (* --- OpenAI API --- *)
  let call_openai_api api_key prompt =
    let body =
      Printf.sprintf {|{
        "model": "gpt-4o-mini",
        "messages": [{"role":"user","content":%S}]
      }|} prompt
    in
    let cmd =
      Printf.sprintf
        "curl -s -X POST https://api.openai.com/v1/chat/completions \
         -H 'Content-Type: application/json' \
         -H 'Authorization: Bearer %s' -d '%s'"
        api_key body
    in
    let ic = Unix.open_process_in cmd in
    let rec read_all acc =
      try input_line ic |> fun l -> read_all (acc ^ l ^ "\n")
      with End_of_file -> acc
    in
    let result = read_all "" in
    ignore (Unix.close_process_in ic); result

  (* --- Extract Tests --- *)
    let extract_tests provider response =
    try
      let open Yojson.Basic.Util in
      let get_safe_member key json =
        match json |> member key with
        | `Null -> None
        | x -> Some x
      in
      let text_opt =
        match provider with
        | "gemini" -> (
            match Yojson.Basic.from_string response |> get_safe_member "candidates" with
            | Some (`List (c::_)) ->
                (match get_safe_member "content" c with
                 | Some (`Assoc parts) ->
                     let parts_val = List.assoc_opt "parts" parts in
                     (match parts_val with
                      | Some (`List (p::_)) ->
                          (match get_safe_member "text" p with
                           | Some (`String s) -> Some (clean_content_text s)
                           | _ -> None)
                      | _ -> None)
                 | _ -> None)
            | _ -> None)
        | "openai" -> (
            match Yojson.Basic.from_string response |> get_safe_member "choices" with
            | Some (`List (c::_)) ->
                (match get_safe_member "message" c with
                 | Some (`Assoc msg) ->
                     (match List.assoc_opt "content" msg with
                      | Some (`String s) -> Some (clean_content_text s)
                      | _ -> None)
                 | _ -> None)
            | _ -> None)
        | _ -> None
      in
      match text_opt with
      | Some text when String.length text > 5 ->
          let text = sanitize_ocaml_syntax text in
          let s = String.index_opt text '['
          and e = String.rindex_opt text ']' in
          (match (s, e) with
           | Some s, Some e ->
               let j = String.sub text s (e - s + 1) in
               (try
                  Yojson.Basic.from_string j |> to_list |> List.map parse_json_test
                with _ ->
                  Printf.eprintf "[WARN] Gemini returned non-JSON body.\n"; [])
           | _ -> [])
      | _ ->
          Printf.eprintf "[WARN] No usable text found in %s response.\n" provider;
          []
    with e ->
      Printf.eprintf "[ERROR] parsing %s LLM response: %s\n"
        provider (Printexc.to_string e);
      []


  (* --- Prompts Generation --- *)
  let fetch_tests_for func_name =
    let prompt =
      match func_name with
      | "plus" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for OCaml function `%s` that takes two natural numbers and returns their sum. \
           Respond ONLY with valid JSON array, no extra text or explanation. \
          Format strictly as: [{\"name\":\"...\",\"input\":...,\"expected\":...}]."
          func_name
      | "mult" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for OCaml function `%s` that multiplies two natural numbers. \
           Respond ONLY with valid JSON array, no extra text or explanation. \
           Format strictly as: [{\"name\":\"...\",\"input\":...,\"expected\":...}]."
          func_name
      | "fact" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for OCaml function `%s` that calculates the factorial of a natural number no greater than 7. \
           Respond ONLY with valid JSON array, no extra text or explanation. \
           Format strictly as: [{\"name\":\"...\",\"input\":...,\"expected\":...}]."
          func_name
      | "pow" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for OCaml function `%s` that takes a natural number and raises it to a natural number power. \
           Respond ONLY with valid JSON array, no extra text or explanation. \
           Format strictly as: [{\"name\":\"...\",\"input\":...,\"expected\":...}]."
          func_name
      | "minus" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for OCaml function `%s` takes two naturals and produces their difference as a natural, or 0 when the subtrahend is less the n the minuend. \
           Respond ONLY with valid JSON array, no extra text or explanation. \
           Format strictly as: [{\"name\":\"...\",\"input\":...,\"expected\":...}]."
          func_name
      | "even" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for OCaml function `%s` that takes a natural number and returns true if it is even and false if it is odd. \
           Respond ONLY with valid JSON array, no extra text or explanation. \
           Format strictly as: [{\"name\":\"...\",\"input\":...,\"expected\":...}].\
           Example: [{\"name\":\"two \",\"input\":{\"S\": {\"S\": \"O\"}},\"expected\":true}]."
          func_name
      | "odd" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for OCaml function `%s` that takes a natural number and returns true if it is odd and false if it is even. \
           Respond ONLY with valid JSON array, no extra text or explanation. \
           Format strictly as: [{\"name\":\"...\",\"input\":...,\"expected\":...}].\
           Example: [{\"name\":\"three \",\"input\":{\"S\": {\"S\": {\"S\": \"O\"}}},\"expected\":true}]."
          func_name
      | "leq" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for OCaml function `%s` that takes two natural numbers n and m and returns true if n is less than or equal to m. \
           Respond ONLY with valid JSON array, no extra text or explanation. \
           Format strictly as: [{\"name\":\"...\",\"input\":...,\"expected\":...}].\
           Example: [{\"one_and_three\": (1,3),\"input\":\"[{\"S\": \"O\"}, {\"S\":{\"S\":{\"S\": \"O\"}}}]\",\"expected\":true}]."
          func_name
      | "psucc" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for the OCaml function  `%s` ` pos -> pos`, where `pos` is a binary-encoded positive integer defined as: type pos = H | O of pos | I of pos. `H` represents 1, `O p` doubles, `I p` doubles and adds 1.
           Return only a JSON array in the following format:
          [{'name': '...', 'input': ..., 'expected': ...}]

          Example: {'name': 'three', 'input': [{'I': 'H'}], 'expected': {'O': {'O' :'H'}}}

          DO NOT include explanations or code, just the JSON array."
          func_name
      | "pplus" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for the OCaml function `%s` ` pos x pos -> pos`, where `pos` is a binary-encoded positive integer defined as: type pos = H | O of pos | I of pos. `H` represents 1, `O p` doubles, `I p` doubles and adds 1.
          Return only a JSON array in the following format:
          [{'name': '...', 'input': ..., 'expected': ...}]

          Example: {'name': 'one_plus_three', 'input': ['H', {'I': 'H'}], 'expected': {'O': {'O' :'H'}}}

          DO NOT include explanations or code, just the JSON array."
          func_name
      | "pmult" -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for the OCaml function `%s` `pos x pos -> pos`, where `pos` is a binary-encoded positive integer defined as: type pos = H | O of pos | I of pos. `H` represents 1, `O p` doubles, `I p` doubles and adds 1.
          Return only a JSON array in the following format:
          [{'name': '...', 'input': ..., 'expected': ...}]

          Example: {'name': 'two_times_three', 'input': [{'O': 'H'}, {'I': 'H'}], 'expected': {'O': {'I': 'H'}}}
          
          DO NOT include explanations or code, just the JSON array."
          func_name
      | _ -> Printf.sprintf
          "Generate 3 JSON test cases ONLY for OCaml function `%s`. \
           Respond ONLY with valid JSON array, no extra text or explanation. \
           Format strictly as: [{\"name\":\"...\",\"input\":...,\"expected\":...}]."
          func_name
  in



    match (Sys.getenv_opt "GEMINI_API_KEY", Sys.getenv_opt "OPENAI_API_KEY") with
    | Some gkey, None ->
        Printf.printf "Fetching LLM tests for %s from Gemini...\n" func_name;
        extract_tests "gemini" (call_gemini_api gkey prompt)
    | None, Some okey ->
        Printf.printf "Fetching LLM tests for %s from OpenAI...\n" func_name;
        extract_tests "openai" (call_openai_api okey prompt)
    | Some gkey, Some _ ->
        Printf.printf "Both keys found — using Gemini for %s...\n" func_name;
        extract_tests "gemini" (call_gemini_api gkey prompt)
    | None, None ->
        Printf.printf "No API key found. Skipping %s tests.\n" func_name; []
end

(* ============================================================ *)
(* Combined Run — final fix for flatten type                     *)
(* ============================================================ *)

let () =
 (* let open Yojson.Basic.Util in *)
  let sum_nat_tests = LLM_Tests.fetch_tests_for "plus" in
  let mult_nat_tests = LLM_Tests.fetch_tests_for "mult" in
  let fact_tests = LLM_Tests.fetch_tests_for "fact" in
  let pow_tests = LLM_Tests.fetch_tests_for "pow" in
  let minus_tests = LLM_Tests.fetch_tests_for "minus" in
  let even_tests = LLM_Tests.fetch_tests_for "even" in
  let odd_tests = LLM_Tests.fetch_tests_for "odd" in
  let leq_tests = LLM_Tests.fetch_tests_for "leq" in
  let succ_pos_tests = LLM_Tests.fetch_tests_for "psucc" in
  let sum_pos_tests = LLM_Tests.fetch_tests_for "pplus" in
  let mult_pos_tests = LLM_Tests.fetch_tests_for "pmult" in

  (* === JSON ↔ OCaml Converters === *)
  let rec json_to_nat (j : Yojson.Basic.t) : nat =
    match j with
    | `String "O" -> O
    | `Assoc [("S", inner)] -> S (json_to_nat inner)
    | `Int n when n <= 0 -> O
    | `Int n -> nat_of_int n
    | _ -> failwith "Invalid nat JSON"
  in


  let rec json_to_pos (j : Yojson.Basic.t) : pos =
    match j with
    | `String "H" -> H
    | `Assoc [("O", inner)] -> O (json_to_pos inner)
    | `Assoc [("I", inner)] -> I (json_to_pos inner)
    | `Int n when n >= 1 -> pos_of_int n
    | _ -> failwith "Invalid pos JSON"
  in

  (* nat → nat *)
  let make_llm_cases_nat
      (name : string)
      (func : nat -> nat)
      (tests : LLM_Tests.test_case list) =
    List.map
      (fun (t : LLM_Tests.test_case) ->
         Alcotest.test_case (name ^ " - " ^ t.name) `Quick (fun () ->
             let input = json_to_nat t.input in
             let expected = json_to_nat t.expected in
             let actual = func input in
             Alcotest.(check nat) t.name expected actual))
      tests
  in

  (* nat × nat → nat *)
  let make_llm_cases_nat2
      (name : string)
      (func : nat -> nat -> nat)
      (tests : LLM_Tests.test_case list) =
    List.map
      (fun (t : LLM_Tests.test_case) ->
         Alcotest.test_case (name ^ " - " ^ t.name) `Quick (fun () ->
            (* let open Yojson.Basic.Util in *)
             let input =
               match t.input with
               | `List [a; b] -> (json_to_nat a, json_to_nat b)
               | _ -> failwith "Expected nat pair input"
             in
             let expected = json_to_nat t.expected in
             let actual = func (fst input) (snd input) in
             Alcotest.(check nat) t.name expected actual))
      tests
  in

  (* nat → bool *)
  let make_llm_cases_nat_bool
      (name : string)
      (func : nat -> bool)
      (tests : LLM_Tests.test_case list) =
    List.map
      (fun (t : LLM_Tests.test_case) ->
         Alcotest.test_case (name ^ " - " ^ t.name) `Quick (fun () ->
             let input = json_to_nat t.input in
             let expected =
               match t.expected with
               | `Bool b -> b
               | _ -> failwith "Expected bool output"
             in
             let actual = func input in
             Alcotest.(check bool) t.name expected actual))
      tests
  in


  (* nat × nat → bool *)
  let make_llm_cases_nat2_bool
      (name : string)
      (func : nat -> nat -> bool)
      (tests : LLM_Tests.test_case list) =
    List.map
      (fun (t : LLM_Tests.test_case) ->
         Alcotest.test_case (name ^ " - " ^ t.name) `Quick (fun () ->
            (* let open Yojson.Basic.Util in *)
             let input =
               match t.input with
               | `List [a; b] -> (json_to_nat a, json_to_nat b)
               | _ -> failwith "Expected nat pair input"
             in
             let expected =
               match t.expected with
               | `Bool b -> b
               | _ -> failwith "Expected bool output"
             in
             let actual = func (fst input) (snd input) in
             Alcotest.(check bool) t.name expected actual))
      tests
  in

  (* pos → pos *)
  let make_llm_cases_pos
      (name : string)
      (func : pos -> pos)
      (tests : LLM_Tests.test_case list) =
    List.map
      (fun (t : LLM_Tests.test_case) ->
         Alcotest.test_case (name ^ " - " ^ t.name) `Quick (fun () ->
             let input = json_to_pos t.input in
             let expected = json_to_pos t.expected in
             let actual = func input in
             Alcotest.(check pos) t.name expected actual))
      tests
  in

  (* pos × pos → pos *)
  let make_llm_cases_pos2
      (name : string)
      (func : pos -> pos -> pos)
      (tests : LLM_Tests.test_case list) =
    List.map
      (fun (t : LLM_Tests.test_case) ->
         Alcotest.test_case (name ^ " - " ^ t.name) `Quick (fun () ->
            (* let open Yojson.Basic.Util in *)
             let input =
               match t.input with
               | `List [a; b] -> (json_to_pos a, json_to_pos b)
               | _ -> failwith "Expected pos pair input"
             in
             let expected = json_to_pos t.expected in
             let actual = func (fst input) (snd input) in
             Alcotest.(check pos) t.name expected actual))
      tests
  in


  (* Create groups *)
  let llm_sum_nat_cases =
    make_llm_cases_nat2 "plus" plus sum_nat_tests
  in
  let llm_mult_nat_cases =
    make_llm_cases_nat2 "mult" mult mult_nat_tests
  in 
  let llm_fact_cases =
    make_llm_cases_nat "fact" fact fact_tests
  in 
  let llm_pow_cases =
    make_llm_cases_nat2 "pow" pow pow_tests
  in
  let llm_minus_cases =
    make_llm_cases_nat2 "minus" minus minus_tests
  in
  let llm_even_cases =
    make_llm_cases_nat_bool "even" even even_tests
  in
  let llm_odd_cases =
    make_llm_cases_nat_bool "odd" odd odd_tests
  in
  let llm_leq_cases =
    make_llm_cases_nat2_bool "leq" leq leq_tests
  in
  let llm_psucc_cases =
    make_llm_cases_pos "psucc" psucc succ_pos_tests
  in
  let llm_sum_pos_cases =
    make_llm_cases_pos2 "pplus" pplus sum_pos_tests
  in
  let llm_mult_pos_cases =
    make_llm_cases_pos2 "pmult" pmult mult_pos_tests
  in

  (* ============================================================ *)
  (* Pretty-print the LLM-generated test cases                    *)
  (* ============================================================ *)
  let print_llm_tests name tests to_str_input to_str_expected =
    if tests <> [] then (
      Printf.printf "\n===== LLM-GENERATED TEST CASES for %s =====\n" name;
      List.iteri
        (fun i (t : LLM_Tests.test_case) ->
           Printf.printf "Test %d: %s\n" (i + 1) t.name;
           Printf.printf "  Input: %s\n" (to_str_input t.input);
           Printf.printf "  Expected: %s\n" (to_str_expected t.expected);
           print_endline "------------------------------------")
        tests;
      print_endline "==========================================\n"
    )
  in

  (* Converters for display *)
  let json_to_string j = Yojson.Basic.pretty_to_string j in

  (* Print all fetched LLM test sets nicely *)
  print_llm_tests "plus" sum_nat_tests json_to_string json_to_string;
  print_llm_tests "mult" mult_nat_tests json_to_string json_to_string;
  print_llm_tests "pow" pow_tests json_to_string json_to_string;
  print_llm_tests "minus" minus_tests json_to_string json_to_string;
  print_llm_tests "even" even_tests json_to_string json_to_string;
  print_llm_tests "odd" odd_tests json_to_string json_to_string;
  print_llm_tests "leq" leq_tests json_to_string json_to_string;
  print_llm_tests "psucc" succ_pos_tests json_to_string json_to_string;
  print_llm_tests "pplus" sum_pos_tests json_to_string json_to_string;
  print_llm_tests "pmult" mult_pos_tests json_to_string json_to_string;
  print_llm_tests "fact" fact_tests json_to_string json_to_string;

(* Run the tests. *)
  Alcotest.run "PA2"
    [ ("test", !tests);
      ("qcheck", !qcheck_tests);
      ("plus", llm_sum_nat_cases);
      ("mult", llm_mult_nat_cases);
      ("pow", llm_pow_cases);
      ("minus", llm_minus_cases);
      ("even", llm_even_cases);
      ("odd", llm_odd_cases);
      ("leq", llm_leq_cases);
      ("psucc", llm_psucc_cases);
      ("pplus", llm_sum_pos_cases);
      ("pmult", llm_mult_pos_cases);
      ("fact", llm_fact_cases) ]