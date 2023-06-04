module Q = QCheck

(* test utils *)
let pp_res f = function
  | Ok x -> f x
  | Error e -> e

let pp_res_query = Q.Print.(pp_res (list (pair string string)))

let err_map f = function
  | Ok x -> Ok (f x)
  | Error e -> Error e

let sort_l l = List.sort compare l
let eq_sorted a b = err_map sort_l a = err_map sort_l b
let is_ascii_char c = Char.code c < 128

let assert_eq ?to_string ?(cmp = ( = )) a b =
  let ok = cmp a b in
  if not ok then (
    (match to_string with
    | Some f -> Printf.eprintf "failed: %s != %s\n%!" (f a) (f b)
    | None -> ());
    failwith "test failed"
  )

let qchecks = ref []
let add_qcheck f = qchecks := f :: !qchecks

let run_qcheck_and_exit () : 'a =
  exit @@ QCheck_base_runner.run_tests ~colors:false !qchecks
