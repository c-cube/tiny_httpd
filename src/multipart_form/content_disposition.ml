open Utils_

type t = { kind: string; name: string option; filename: string option }

(** Simple display *)
let to_string (self : t) =
  let stropt = function
    | None -> "None"
    | Some s -> spf "%S" s
  in
  spf "{kind=%S; name=%s; filename=%s}" self.kind (stropt self.name)
    (stropt self.filename)

let parse (hs : Tiny_httpd.Headers.t) : t option =
  match Tiny_httpd.Headers.get "content-disposition" hs with
  | None -> None
  | Some s ->
    (match String.split_on_char ';' s with
    | [] ->
      failwith (Printf.sprintf "multipart: invalid content-disposition %S" s)
    | kind :: tl ->
      let name = ref None in
      let filename = ref None in
      List.iter
        (fun s ->
          match Utils_.split1_on ~c:'=' @@ String.trim s with
          | Some ("name", v) -> name := Some (Utils_.remove_quotes v)
          | Some ("filename", v) -> filename := Some (Utils_.remove_quotes v)
          | _ -> ())
        tl;
      Some { kind; name = !name; filename = !filename })
