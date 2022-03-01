
(** Serving static content from directories

    This module provides the same functionality as the "http_of_dir" tool.
    It exposes a directory (and its subdirectories), with the optional ability
    to delete or upload files.

    @since 0.11 *)

(** behavior of static directory.

    This controls what happens when the user requests the path to
    a directory rather than a file. *)
type dir_behavior =
  | Index
  (** Redirect to index.html if present, else fails. *)
  | Lists
  (** Lists content of directory. Be careful of security implications. *)
  | Index_or_lists
  (** Redirect to index.html if present and lists content otherwise.
      This is useful for tilde ("~") directories and other per-user behavior,
      but be mindful of security implications *)
  | Forbidden
  (** Forbid access to directory. This is suited for serving assets, for example. *)

type hidden
(** Type used to prevent users from building a config directly.
    Use {!default_config} or {!config} instead. *)

(** configuration for static file handlers. This might get
    more fields over time. *)
type config = {
  mutable download: bool;
  (** Is downloading files allowed? *)

  mutable dir_behavior: dir_behavior;
  (** Behavior when serving a directory and not a file *)

  mutable delete: bool;
  (** Is deleting a file allowed? (with method DELETE) *)

  mutable upload: bool;
  (** Is uploading a file allowed? (with method PUT) *)

  mutable max_upload_size: int;
  (** If {!upload} is true, this is the maximum size in bytes for
      uploaded files. *)

  _rest: hidden;
  (** Just ignore this field. *)
}

(** default configuration: [
  { download=true
  ; dir_behavior=Forbidden
  ; delete=false
  ; upload=false
  ; max_upload_size = 10 * 1024 * 1024
  }] *)
val default_config : unit -> config

val config :
  ?download:bool ->
  ?dir_behavior:dir_behavior ->
  ?delete:bool ->
  ?upload:bool ->
  ?max_upload_size:int ->
  unit ->
  config
(** Build a config from {!default_config}.
    @since NEXT_RELEASE *)

(** [add_dirpath ~config ~dir ~prefix server] adds route handle to the
    [server] to serve static files in [dir] when url starts with [prefix],
    using the given configuration [config]. *)
val add_dir_path :
  config:config ->
  dir:string ->
  prefix:string ->
  Tiny_httpd.t -> unit
