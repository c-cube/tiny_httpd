
(** behavior of static directory *)
type dir_behavior =
  | Index
  (** Redirect to index.html if present *)
  | Lists
  (** Lists content of directory *)
  | Index_or_lists
  (** Redirect to index.html if present and Lists content otherwise *)
  | Forbidden
  (** Forbid access to directory *)

(** configuration for static file handlers *)
type config = {
  mutable download: bool;
  mutable dir_behavior: dir_behavior;
  mutable delete: bool;
  mutable upload: bool;
  mutable max_upload_size: int;
}

(** default configuration: [
  { download=true
  ; dir_behavior=Forbidden
  ; delete=false
  ; upload=false
  ; max_upload_size = 10 * 1024 * 1024
  }] *)
val default_config : unit -> config

(** [add_dirpath ~config ~dir ~prefix server] adds route handle to the
    [server] to serve static files in [dir] when url starts with [prefix],
    using the given configuration [config]. *)
val add_dir_path : config:config ->
                   dir:string ->
                   prefix:string ->
                   Tiny_httpd.t -> unit
