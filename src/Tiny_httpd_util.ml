
(* test utils *)
(*$inject
  let pp_res f = function Ok x -> f x | Error e -> e
  let pp_res_query = (Q.Print.(pp_res (list (pair string string))))
  let err_map f = function Ok x-> Ok (f x) | Error e -> Error e
  let sort_l l = List.sort compare l
  let eq_sorted a b = (=) (err_map sort_l a)(err_map sort_l b)
  let is_ascii_char c = Char.code c < 128
*)

let percent_encode ?(skip=fun _->false) s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | c when skip c -> Buffer.add_char buf c
      | (' ' | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+'
        | ',' | '/' | ':' | ';' | '=' | '?' | '@' | '[' | ']' | '~')
        as c ->
        Printf.bprintf buf "%%%X" (Char.code c)
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(*$= & ~printer:(fun s->s)
  "hello%20world" (percent_encode "hello world")
  "%23%25^%24%40^%40" (percent_encode "#%^$@^@")
  "a%20ohm%2B5235%25%26%40%23%20---%20_" (percent_encode "a ohm+5235%&@# --- _")
*)

(*$= & ~printer:Q.(Print.(option string))
  (Some "?") (percent_decode @@ percent_encode "?")
*)

let hex_int (s:string) : int = Scanf.sscanf s "%x" (fun x->x)

let percent_decode (s:string) : _ option =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  try
    while !i < String.length s do
      match String.get s !i with
      | '%' ->
        if !i+2 < String.length s then (
          begin match hex_int @@ String.sub s (!i+1) 2 with
            | n -> Buffer.add_char buf (Char.chr n)
            | exception _ -> raise Exit
          end;
          i := !i + 3;
        ) else (
          raise Exit (* truncated *)
        )
      | '+' -> Buffer.add_char buf ' '; incr i (* for query strings *)
      | c -> Buffer.add_char buf c; incr i
    done;
    Some (Buffer.contents buf)
  with Exit -> None

(*$QR & ~count:1_000 ~long_factor:20
    Q.string (fun s ->
        String.iter (fun c -> Q.assume @@ is_ascii_char c) s;
        match percent_decode (percent_encode s) with
        | Some s' -> s=s'
        | None -> Q.Test.fail_report "invalid percent encoding")
*)

exception Invalid_query

let get_query s : string =
  match String.index s '?' with
  | i -> String.sub s (i+1) (String.length s-i-1)
  | exception Not_found -> ""

let parse_query s : (_ list, string) result=
  let pairs = ref [] in
  let is_sep_ = function '&' | ';' -> true | _ -> false in
  let i = ref 0 in
  let j = ref 0 in
  try
    let percent_decode s =
      match percent_decode s with Some x -> x | None -> raise Invalid_query
    in
    let parse_pair () =
      let eq = String.index_from s !i '=' in
      let k = percent_decode @@ String.sub s !i (eq- !i) in
      let v = percent_decode @@ String.sub s (eq+1) (!j-eq-1) in
      pairs := (k,v) :: !pairs;
    in
    while !i < String.length s do
      while !j < String.length s && not (is_sep_ (String.get s !j)) do incr j done;
      if !j < String.length s then (
        assert (is_sep_ (String.get s !j));
        parse_pair();
        i := !j+1;
        j := !i;
      ) else (
        parse_pair();
        i := String.length s; (* done *)
      )
    done;
    Ok !pairs
  with
  | Invalid_argument _ | Not_found | Failure _ ->
    Error (Printf.sprintf "error in parse_query for %S: i=%d,j=%d" s !i !j)
  | Invalid_query -> Error ("invalid query string: " ^ s)

(*$= & ~printer:pp_res_query ~cmp:eq_sorted
  (Ok ["a", "b"; "c", "d"]) (parse_query "a=b&c=d")
*)

(*$QR & ~long_factor:20 ~count:1_000
    Q.(small_list (pair string string))
      (fun l ->
        List.iter (fun (a,b) ->
            Q.assume (a<>"" && b<>"" );
            String.iter (fun c -> Q.assume @@ is_ascii_char c) a;
            String.iter (fun c -> Q.assume @@ is_ascii_char c) b;
          ) l;
        let s = String.concat "&"
            (List.map (fun (x,y) -> percent_encode x ^"="^percent_encode y) l) in
        eq_sorted (Ok l) (parse_query s))
*)
