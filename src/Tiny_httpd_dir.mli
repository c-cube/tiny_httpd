
type dir_behavior =
  Index | Forbidden | Lists

type config = {
  mutable download: bool;
  mutable mem_cache: bool;
  mutable dir_behavior: dir_behavior;
  mutable delete: bool;
  mutable upload: bool;
  mutable max_upload_size: int;
}

val default_config : unit -> config

val add_dir_path : config:config ->
                   dir:string ->
                   prefix:string ->
                   Tiny_httpd.t -> unit
