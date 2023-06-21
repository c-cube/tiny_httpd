open Test_util
open Tiny_httpd_util

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

let () = run_qcheck_and_exit ()
