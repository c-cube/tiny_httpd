type path = string list (* split on '/' *)

type (_, _) comp =
  | Exact : string -> ('a, 'a) comp
  | Int : (int -> 'a, 'a) comp
  | String : (string -> 'a, 'a) comp
  | String_urlencoded : (string -> 'a, 'a) comp

type (_, _) t =
  | Fire : ('b, 'b) t
  | Rest : { url_encoded: bool } -> (string -> 'b, 'b) t
  | Compose : ('a, 'b) comp * ('b, 'c) t -> ('a, 'c) t

let return = Fire
let rest_of_path = Rest { url_encoded = false }
let rest_of_path_urlencoded = Rest { url_encoded = true }
let ( @/ ) a b = Compose (a, b)
let string = String
let string_urlencoded = String_urlencoded
let int = Int
let exact (s : string) = Exact s

let exact_path (s : string) tail =
  let rec fn = function
    | [] -> tail
    | "" :: ls -> fn ls
    | s :: ls -> exact s @/ fn ls
  in
  fn (String.split_on_char '/' s)

let rec eval : type a b. path -> (a, b) t -> a -> b option =
 fun path route f ->
  match path, route with
  | [], Fire -> Some f
  | _, Fire -> None
  | _, Rest { url_encoded } ->
    let whole_path = String.concat "/" path in
    (match
       if url_encoded then (
         match Util.percent_decode whole_path with
         | Some s -> s
         | None -> raise_notrace Exit
       ) else
         whole_path
     with
    | whole_path -> Some (f whole_path)
    | exception Exit -> None)
  | c1 :: path', Compose (comp, route') ->
    (match comp with
    | Int ->
      (match int_of_string c1 with
      | i -> eval path' route' (f i)
      | exception _ -> None)
    | String -> eval path' route' (f c1)
    | String_urlencoded ->
      (match Util.percent_decode c1 with
      | None -> None
      | Some s -> eval path' route' (f s))
    | Exact s ->
      if s = c1 then
        eval path' route' f
      else
        None)
  | [], Compose (String, Fire) -> Some (f "") (* trailing *)
  | [], Compose (String_urlencoded, Fire) -> Some (f "") (* trailing *)
  | [], Compose _ -> None

let bpf = Printf.bprintf

let rec pp_ : type a b. Buffer.t -> (a, b) t -> unit =
 fun out -> function
  | Fire -> bpf out "/"
  | Rest { url_encoded } ->
    bpf out "<rest_of_url%s>"
      (if url_encoded then
        "_urlencoded"
      else
        "")
  | Compose (Exact s, tl) -> bpf out "%s/%a" s pp_ tl
  | Compose (Int, tl) -> bpf out "<int>/%a" pp_ tl
  | Compose (String, tl) -> bpf out "<str>/%a" pp_ tl
  | Compose (String_urlencoded, tl) -> bpf out "<enc_str>/%a" pp_ tl

let to_string x =
  let b = Buffer.create 16 in
  pp_ b x;
  Buffer.contents b

module Private_ = struct
  let eval = eval
end

let pp out x = Format.pp_print_string out (to_string x)
