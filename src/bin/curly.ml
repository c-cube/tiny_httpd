module Result = struct
  include Result
  let (>>=)
    : type a b e. (a, e) result -> (a -> (b, e) result) -> (b, e) result
    = fun r f ->
      match r with
      | Ok x -> f x
      | (Error _) as e -> e
end

open Result

module Meth = struct
  type t =
    [ `GET
    | `POST
    | `HEAD
    | `PUT
    | `DELETE
    | `OPTIONS
    | `TRACE
    | `CONNECT
    | `PATCH
    | `Other of string
    ]

  let to_string = function
    | `GET -> "GET"
    | `POST -> "POST"
    | `HEAD -> "HEAD"
    | `PUT -> "PUT"
    | `DELETE -> "DELETE"
    | `OPTIONS -> "OPTIONS"
    | `TRACE -> "TRACE"
    | `CONNECT -> "CONNECT"
    | `PATCH -> "PATCH"
    | `Other s -> s

  let pp fmt t = Format.fprintf fmt "%s" (to_string t)
end

module Header = struct
  type t = (string * string) list

  let empty = []

  let to_cmd t =
    t
    |> List.map (fun (k, v) -> ["-H"; Printf.sprintf "%s: %s" k v])
    |> List.concat

  let pp fmt t =
    Format.pp_print_list
      ~pp_sep:Format.pp_print_newline
      (fun fmt (k ,v) -> Format.fprintf fmt "%s: %s\n" k v)
      fmt t
end

module Response = struct
  type t = Http.response =
    { code: int
    ; headers: Header.t
    ; body: string
    }

  let default =
    { code = 0
    ; headers = []
    ; body = "" }

  let of_stdout s =
    let lexbuf = Lexing.from_string s in
    try Ok (Http.response default lexbuf)
    with e -> Error e

  let pp fmt t =
    Format.fprintf fmt "{code=%d;@ headers=%a;@ body=\"%s\"}"
      t.code Header.pp t.headers t.body
end

module Process_result = struct
  type t =
    { status: Unix.process_status
    ; stderr: string
    ; stdout: string
    }

  let pp_process_status fmt = function
    | Unix.WEXITED n -> Format.fprintf fmt "Exit code %d" n
    | Unix.WSIGNALED n -> Format.fprintf fmt "Signal %d" n
    | Unix.WSTOPPED n -> Format.fprintf fmt "Stopped %d" n

  let pp fmt t =
    Format.fprintf fmt "{status=%a;@ stderr=\"%s\";@ stdout=\"%s\"}"
      pp_process_status t.status t.stderr t.stdout
end

module Error = struct
  type t =
    | Invalid_request of string
    | Bad_exit of Process_result.t
    | Failed_to_read_response of exn * Process_result.t
    | Exn of exn

  let pp fmt = function
    | Bad_exit p ->
      Format.fprintf fmt "Non 0 exit code %a@.%a"
        Process_result.pp_process_status p.Process_result.status
        Process_result.pp p
    | Failed_to_read_response (e, _)  ->
      Format.fprintf fmt "Couldn't read response:@ %s" (Printexc.to_string e)
    | Invalid_request r -> Format.fprintf fmt "Invalid request: %s" r
    | Exn e -> Format.fprintf fmt "Exception: %s" (Printexc.to_string e)
end

module Request = struct
  type t =
    { meth: Meth.t
    ; url: string
    ; headers: Header.t
    ; body: string
    }

  let make ?(headers=Header.empty) ?(body="") ~url ~meth () =
    { meth
    ; url
    ; headers
    ; body }

  let has_body t = String.length t.body > 0

  let validate t =
    if has_body t && List.mem t.meth [`GET; `HEAD] then
      Error (Error.Invalid_request "No body is allowed with GET/HEAD methods")
    else
      Ok t

  let to_cmd_args t =
    List.concat
      [ ["-X"; Meth.to_string t.meth]
      ; Header.to_cmd t.headers
      ; [t.url]
      ; (if has_body t then
           ["--data-binary"; "@-"]
         else
           [])
      ]

  let pp fmt t =
    Format.fprintf fmt
      "{@ meth=%a;@ url=\"%s\";@ headers=\"%a\";@ body=\"%s\"@ }"
      Meth.pp t.meth t.url Header.pp t.headers t.body
end

let result_of_process_result t =
  match t.Process_result.status with
  | Unix.WEXITED 0 -> Ok t
  | _ -> Error (Error.Bad_exit  t)

let run prog args stdin_str =
  let (stdout, stdin, stderr) =
    let prog =
      prog :: (List.map Filename.quote args)
      |> String.concat " " in
    Unix.open_process_full prog [||] in
  if String.length stdin_str > 0 then (
    output_string stdin stdin_str
  );
  begin
    try close_out stdin;
    with _ -> ()
  end;
  let stdout_fd = Unix.descr_of_in_channel stdout in
  let stderr_fd = Unix.descr_of_in_channel stderr in
  let (in_buf, err_buf) = Buffer.(create 128, create 128) in
  let read_buf_len = 512 in
  let read_buf = Bytes.create read_buf_len in
  let input ch =
    match input ch read_buf 0 read_buf_len with
    | 0 -> Error `Eof
    | s -> Ok s in
  let rec loop = function
    | [] -> ()
    | read_list ->
      let can_read, _, _ = Unix.select read_list [] [] 1.0 in
      let to_remove =
        List.fold_left (fun to_remove fh ->
            let (rr, buf) =
              if fh = stderr_fd then (
                (input stderr, err_buf)
              ) else (
                (input stdout, in_buf)
              ) in
            begin match rr with
              | Ok len ->
                Buffer.add_subbytes buf read_buf 0 len;
                to_remove
              | Error `Eof ->
                fh :: to_remove
            end
          ) [] can_read in
      read_list
      |> List.filter (fun fh -> not (List.mem fh to_remove))
      |> loop
  in
  ignore (loop [ stdout_fd ; stderr_fd ]);
  let status = Unix.close_process_full (stdout, stdin, stderr) in
  { Process_result.
    status
  ; stdout = Buffer.contents in_buf
  ; stderr = Buffer.contents err_buf
  }

let run ?(exe="curl") ?(args=[]) req =
  Request.validate req >>= fun req ->
  let args = "-si" :: (Request.to_cmd_args req) @ args in
  let res =
    try
      result_of_process_result (run exe args req.Request.body)
    with e ->
      Error (Error.Exn e)
  in
  res >>= fun res ->
  match Response.of_stdout res.Process_result.stdout with
  | Ok r -> Ok r
  | Error e -> Error (Error.Failed_to_read_response (e, res))

let get ?exe ?args ?headers url =
  run ?exe ?args (Request.make ?headers ~url ~meth:`GET ())
let head ?exe ?args ?headers url =
  run ?exe ?args (Request.make ?headers ~url ~meth:`HEAD ())
let delete ?exe ?args ?headers url =
  run ?exe ?args (Request.make ?headers ~url ~meth:`DELETE ())
let post ?exe ?args ?headers ?body url =
  run ?exe ?args (Request.make ?body ?headers ~url ~meth:`POST ())
let put ?exe ?args ?headers ?body url =
  run ?exe ?args (Request.make ?body ?headers ~url ~meth:`PUT ())
