
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
      | c when Char.code c > 127 ->
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

let find_q_index_ s = String.index s '?'

let get_non_query_path s =
  match find_q_index_ s with
  | i -> String.sub s 0 i
  | exception Not_found -> s

let get_query s : string =
  match find_q_index_ s with
  | i -> String.sub s (i+1) (String.length s-i-1)
  | exception Not_found -> ""

let split_query s = get_non_query_path s, get_query s

let split_on_slash s : _ list =
  let l = ref [] in
  let i = ref 0 in
  let n = String.length s in
  while !i < n do
    match String.index_from s !i '/' with
    | exception Not_found ->
      if !i < n then (
        (* last component *)
        l := String.sub s !i (n - !i) :: !l;
      );
      i := n (* done *)
    | j ->
      if j > !i then  (
        l := String.sub s !i (j - !i) :: !l;
      );
      i := j+1;
  done;
  List.rev !l

(*$= & ~printer:Q.Print.(list string)
  ["a"; "b"] (split_on_slash "/a/b")
  ["coucou"; "lol"] (split_on_slash "/coucou/lol")
  ["a"; "b"; "c"] (split_on_slash "/a/b//c/")
  ["a"; "b"] (split_on_slash "//a/b/")
  ["a"] (split_on_slash "/a//")
  [] (split_on_slash "/")
  [] (split_on_slash "//")
*)

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

let str_mutex = Mutex.create ()

type (_,_) to_read =
  | Nothing : ('a, 'a) to_read
  | Begin   : ('a, 'b) to_read -> (int -> 'a, 'b) to_read
  | End     : ('a, 'b) to_read -> (int -> 'a, 'b) to_read
  | Grp     : int * ('a, 'b) to_read -> (string -> 'a, 'b) to_read
  | OptGrp  : int * ('a, 'b) to_read -> (string option -> 'a, 'b) to_read

let read_groups str groups cont =
  let open Str in
  let rec fn : type a b. (a,b) to_read -> a -> b =
    fun groups cont ->
    match groups,cont with
    | Nothing , cont -> cont
    | Begin r , cont -> fn r (cont (match_beginning ()))
    | End   r , cont -> fn r (cont (match_end ()))
    | Grp(i,r), cont -> fn r (cont (matched_group i str))
    | OptGrp(i,r), cont ->
       let str = try Some(matched_group i str)
                 with Not_found -> None
       in
       fn r (cont str)
  in
  fn groups cont

let search_forward regexp_ str ?(from=0) groups cont =
  let open Str in
  Mutex.lock str_mutex;
  let _ = search_forward regexp_ str from in
  let cont = read_groups str groups cont  in
  Mutex.unlock str_mutex;
  cont

let string_match regexp_ str ?(from=0) groups cont =
  let open Str in
  Mutex.lock str_mutex;
  let _ = string_match regexp_ str from in
  let cont = read_groups str groups cont  in
  Mutex.unlock str_mutex;
  cont

let first_line str =
  let pos = String.index str '\n' in
  let pos = if str.[pos] = '\r' then pos-1 else pos in
  String.sub str 0 (pos-1)


(* Decoding of multipart encoded post forms according to rfc7578 *)
type multipart_part =
  { disposition : string
  ; mime_type   : string option
  ; charset     : string option
  ; filename    : string option
  ; content     : string }

let content_disposition_regexp =
  Str.regexp "Content-Disposition: \\([^;\r\n]*\\)\\(;[ ]*\\)?\\([^\r\n]*\\)"

let content_type_regexp =
  Str.regexp "Content-Type: \\([^\r\n]*\\)\\(;[ ]*charset=\\([^ \n\r]+\\)\\)"

let empty_line_re =
  Str.regexp "\r?\n\r?\n"

let decode_parts str =
  try
    let groups = Begin (End Nothing) in
    let (header_end, content_begin) =
      search_forward empty_line_re str groups (fun b e -> (b,e))
    in
    let header = String.sub str 0 header_end in
    let len = String.length str in
    let rm = if str.[len-1] = '\n' then
               if str.[len-2] = '\r' then 2 else 1 else 0
    in
    let len = len - content_begin - rm in
    let content = String.sub str content_begin len in
    let (disposition, values) =
      search_forward content_disposition_regexp header
        (Grp(1,Grp (3,Nothing))) (fun x y -> (x,y))
    in
    let values =
      match parse_query values with
      | Ok l -> List.map (fun (k,v) ->
                    let open String in
                    let k = trim k in
                    let v = trim v in
                    let len = String.length v in
                    let v =
                      if v.[0] = '"' && v.[len-1] = '"' && len > 1 then
                        String.sub v 1 (len-2)
                      else v
                    in
                    (k,v)) l
      | _ -> []
    in
    let mime_type, charset =
      try
        search_forward content_type_regexp header (OptGrp (1,OptGrp (3,Nothing)))
          (fun x y -> (x,y))
      with
        Not_found -> (None, None)
    in
    let name = List.assoc "name" values in
    let filename = try percent_decode (List.assoc "filename" values)
                   with Not_found -> None
    in
    Some(name,{disposition; mime_type; charset; filename; content})
  with Not_found -> None

let decode_multipart str =
  let sep = first_line str in
  let parts = Str.(split (regexp (sep ^ "\\(--\\)?\r?\n")) str) in
  let res = List.filter_map decode_parts parts in
  let default_charset, res =
    List.partition (fun (name,_) -> name = "_charset_") res
  in
  match default_charset with
  | (_,{content=charset; _})::_ ->
     List.map (function
           (name, part as c) ->
           if part.charset = None then
             (name, {part with charset = Some charset})
           else c) res
  | [] -> res
