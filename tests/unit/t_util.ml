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
let assert_eq ?(cmp = ( = )) a b = assert (cmp a b)

open Tiny_httpd_util

let qchecks = ref []
let add_qcheck f = qchecks := f :: !qchecks
let () = assert_eq "hello%20world" (percent_encode "hello world")
let () = assert_eq "%23%25^%24%40^%40" (percent_encode "#%^$@^@")

let () =
  assert_eq "a%20ohm%2B5235%25%26%40%23%20---%20_"
    (percent_encode "a ohm+5235%&@# --- _")

let () = assert_eq (Some "?") (percent_decode @@ percent_encode "?")

let () =
  add_qcheck
  @@ QCheck.Test.make ~count:1_000 ~long_factor:20 Q.string (fun s ->
         String.iter (fun c -> Q.assume @@ is_ascii_char c) s;
         match percent_decode (percent_encode s) with
         | Some s' -> s = s'
         | None -> Q.Test.fail_report "invalid percent encoding")

let () = assert_eq [ "a"; "b" ] (split_on_slash "/a/b")
let () = assert_eq [ "coucou"; "lol" ] (split_on_slash "/coucou/lol")
let () = assert_eq [ "a"; "b"; "c" ] (split_on_slash "/a/b//c/")
let () = assert_eq [ "a"; "b" ] (split_on_slash "//a/b/")
let () = assert_eq [ "a" ] (split_on_slash "/a//")
let () = assert_eq [] (split_on_slash "/")
let () = assert_eq [] (split_on_slash "//")

let () =
  assert_eq ~cmp:eq_sorted (Ok [ "a", "b"; "c", "d" ]) (parse_query "a=b&c=d")

let () =
  add_qcheck
  @@ QCheck.Test.make ~long_factor:20 ~count:1_000
       Q.(small_list (pair string string))
       (fun l ->
         List.iter
           (fun (a, b) ->
             Q.assume (a <> "" && b <> "");
             String.iter (fun c -> Q.assume @@ is_ascii_char c) a;
             String.iter (fun c -> Q.assume @@ is_ascii_char c) b)
           l;
         let s =
           String.concat "&"
             (List.map
                (fun (x, y) -> percent_encode x ^ "=" ^ percent_encode y)
                l)
         in
         eq_sorted (Ok l) (parse_query s))

let () = exit @@ QCheck_base_runner.run_tests ~colors:false !qchecks
