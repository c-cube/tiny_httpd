(* ported from https://github.com/cryptosense/multipart-form-data .
   License: BSD-2 *)

open Utils_

(* TODO: redo some light form of lwt stream for porting purposes? *)
module Stream_ = struct
  type 'a t = { next: unit -> 'a option } [@@unboxed]

  let from next : _ t = { next }
  let[@inline] get (self : _ t) : _ option = self.next ()

  let filter_map f (self : _ t) : _ t =
    let rec next () =
      match self.next () with
      | None -> None
      | Some x ->
        (match f x with
        | None -> next ()
        | Some _ as r -> r)
    in
    { next }
end

let split s boundary =
  let r = ref None in
  let push v =
    match !r with
    | None -> r := Some v
    | Some _ -> assert false
  in
  let pop () =
    let res = !r in
    r := None;
    res
  in
  let go c0 =
    let c =
      match pop () with
      | Some x -> x ^ c0
      | None -> c0
    in
    let string_to_process =
      match find_common_idx c boundary with
      | None -> c
      | Some idx ->
        let prefix = String.sub c 0 idx in
        let suffix = String.sub c idx (String.length c - idx) in
        push suffix;
        prefix
    in
    split_and_process_string ~boundary string_to_process
  in
  let initial = List.map go s in
  let final =
    Stream_.flatten
    @@ Stream_.from (fun () ->
           option_map (split_and_process_string ~boundary) @@ pop ())
  in
  Stream_.append initial final

let until_next_delim s =
  Stream_.from @@ fun () ->
  let res = Stream_.get s in
  match res with
  | None | Some `Delim -> None
  | Some (`Word w) -> Some w

let join s =
  Stream_.filter_map
    (function
      (* | `Delim -> Some (until_next_delim @@ Lwt_stream.clone s) *)
      | `Delim -> Some (until_next_delim s)
      | `Word _ -> None)
    s

let align stream boundary = join @@ split stream boundary

type header = string * string

let extract_boundary content_type =
  Stringext.chop_prefix ~prefix:"multipart/form-data; boundary=" content_type

let unquote s = Scanf.sscanf s "%S" @@ fun x -> x

let parse_name s =
  option_map unquote @@ Stringext.chop_prefix ~prefix:"form-data; name=" s

let parse_header s =
  match Stringext.cut ~on:": " s with
  | Some (key, value) -> key, value
  | None -> invalid_arg "parse_header"

let non_empty st =
  let%lwt r = Lwt_stream.to_list @@ Lwt_stream.clone st in
  Lwt.return (String.concat "" r <> "")

let get_headers : string Lwt_stream.t Lwt_stream.t -> header list Lwt.t =
 fun lines ->
  let%lwt header_lines = Lwt_stream.get_while_s non_empty lines in
  Lwt_list.map_s
    (fun header_line_stream ->
      let%lwt parts = Lwt_stream.to_list header_line_stream in
      Lwt.return @@ parse_header @@ String.concat "" parts)
    header_lines

type stream_part = { headers: header list; body: string Lwt_stream.t }

let parse_part chunk_stream =
  let lines = align chunk_stream "\r\n" in
  match%lwt get_headers lines with
  | [] -> Lwt.return_none
  | headers ->
    let body = Lwt_stream.concat @@ Lwt_stream.clone lines in
    Lwt.return_some { headers; body }

let parse_stream ~stream ~content_type =
  match extract_boundary content_type with
  | None -> Lwt.fail_with "Cannot parse content-type"
  | Some boundary ->
    let actual_boundary = "--" ^ boundary in
    Lwt.return
    @@ Lwt_stream.filter_map_s parse_part
    @@ align stream actual_boundary

let s_part_body { body; _ } = body

let s_part_name { headers; _ } =
  match parse_name @@ List.assoc "Content-Disposition" headers with
  | Some x -> x
  | None -> invalid_arg "s_part_name"

let parse_filename s =
  let parts = split_on_string s ~pattern:"; " in
  let f = function
    | None -> None
    | Some part ->
      (match Stringext.cut part ~on:"=" with
      | Some ("filename", quoted_string) -> Some (unquote quoted_string)
      | _ -> None)
  in
  first_matching f parts

let s_part_filename { headers; _ } =
  parse_filename @@ List.assoc "Content-Disposition" headers

type file = stream_part

let file_stream = s_part_body
let file_name = s_part_name
let file_content_type { headers; _ } = List.assoc "Content-Type" headers

let as_part part =
  match s_part_filename part with
  | Some _filename -> Lwt.return (`File part)
  | None ->
    let%lwt chunks = Lwt_stream.to_list part.body in
    let body = String.concat "" chunks in
    Lwt.return (`String body)

let get_parts s =
  let go part m =
    let name = s_part_name part in
    let%lwt parsed_part = as_part part in
    Lwt.return @@ StringMap.add name parsed_part m
  in
  Lwt_stream.fold_s go s StringMap.empty

let concat a b =
  match a, b with
  | _, "" -> a
  | "", _ -> b
  | _ -> a ^ b

module Reader = struct
  type t = { mutable buffer: string; source: string Lwt_stream.t }

  let make stream = { buffer = ""; source = stream }
  let unread r s = r.buffer <- concat s r.buffer

  let empty r =
    if r.buffer = "" then
      Lwt_stream.is_empty r.source
    else
      Lwt.return false

  let read_next r =
    let%lwt next_chunk = Lwt_stream.next r.source in
    r.buffer <- concat r.buffer next_chunk;
    Lwt.return_unit

  let read_chunk r =
    try%lwt
      let%lwt () =
        if r.buffer = "" then
          read_next r
        else
          Lwt.return_unit
      in
      let res = r.buffer in
      r.buffer <- "";
      Lwt.return (Some res)
    with Lwt_stream.Empty -> Lwt.return None

  let buffer_contains r s =
    match Stringext.cut r.buffer ~on:s with
    | Some _ -> true
    | None -> false

  let rec read_until r cond =
    if cond () then
      Lwt.return_unit
    else (
      let%lwt () = read_next r in
      read_until r cond
    )

  let read_line r =
    let delim = "\r\n" in
    let%lwt () = read_until r (fun () -> buffer_contains r delim) in
    match Stringext.cut r.buffer ~on:delim with
    | None -> assert false
    | Some (line, next) ->
      r.buffer <- next;
      Lwt.return (line ^ delim)
end

let read_headers reader =
  let rec go headers =
    let%lwt line = Reader.read_line reader in
    if line = "\r\n" then
      Lwt.return headers
    else (
      let header = parse_header line in
      go (header :: headers)
    )
  in
  go []

let rec compute_case reader boundary =
  match%lwt Reader.read_chunk reader with
  | None -> Lwt.return `Empty
  | Some line ->
    (match Stringext.cut line ~on:(boundary ^ "\r\n") with
    | Some (pre, post) -> Lwt.return @@ `Boundary (pre, post)
    | None ->
      (match Stringext.cut line ~on:(boundary ^ "--\r\n") with
      | Some (pre, post) -> Lwt.return @@ `Boundary (pre, post)
      | None ->
        (match find_common_idx line boundary with
        | Some 0 ->
          Reader.unread reader line;
          let%lwt () = Reader.read_next reader in
          compute_case reader boundary
        | Some amb_idx ->
          let unambiguous = String.sub line 0 amb_idx in
          let ambiguous =
            String.sub line amb_idx (String.length line - amb_idx)
          in
          Lwt.return @@ `May_end_with_boundary (unambiguous, ambiguous)
        | None -> Lwt.return @@ `App_data line)))

let iter_part reader boundary callback =
  let fin = ref false in
  let last () =
    fin := true;
    Lwt.return_unit
  in
  let handle ~send ~unread ~finish =
    let%lwt () = callback send in
    Reader.unread reader unread;
    if finish then
      last ()
    else
      Lwt.return_unit
  in
  while%lwt not !fin do
    let%lwt res = compute_case reader boundary in
    match res with
    | `Empty -> last ()
    | `Boundary (pre, post) -> handle ~send:pre ~unread:post ~finish:true
    | `May_end_with_boundary (unambiguous, ambiguous) ->
      handle ~send:unambiguous ~unread:ambiguous ~finish:false
    | `App_data line -> callback line
  done

let read_file_part reader boundary callback = iter_part reader boundary callback

let strip_crlf s =
  if ends_with ~suffix:"\r\n" ~suffix_length:2 s then
    String.sub s 0 (String.length s - 2)
  else
    s

let read_string_part reader boundary =
  let value = Buffer.create 0 in
  let append_to_value line = Lwt.return (Buffer.add_string value line) in
  let%lwt () = iter_part reader boundary append_to_value in
  Lwt.return @@ strip_crlf (Buffer.contents value)

let read_part reader boundary callback fields =
  let%lwt headers = read_headers reader in
  let content_disposition = List.assoc "Content-Disposition" headers in
  let name =
    match parse_name content_disposition with
    | Some x -> x
    | None -> invalid_arg "handle_multipart"
  in
  match parse_filename content_disposition with
  | Some filename -> read_file_part reader boundary (callback ~name ~filename)
  | None ->
    let%lwt value = read_string_part reader boundary in
    fields := (name, value) :: !fields;
    Lwt.return_unit

let handle_multipart reader boundary callback =
  let fields = (ref [] : (string * string) list ref) in
  let%lwt () =
    let%lwt _dummyline = Reader.read_line reader in
    let fin = ref false in
    while%lwt not !fin do
      if%lwt Reader.empty reader then
        Lwt.return (fin := true)
      else
        read_part reader boundary callback fields
    done
  in
  Lwt.return !fields

let parse ~stream ~content_type ~callback =
  let reader = Reader.make stream in
  let boundary =
    match extract_boundary content_type with
    | Some s -> "--" ^ s
    | None -> invalid_arg "iter_multipart"
  in
  handle_multipart reader boundary callback
