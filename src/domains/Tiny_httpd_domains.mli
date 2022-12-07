(** {1 Tiny Http With OCaml's domain}

    This module allows for using OCaml 5 domains instead of system thread.
    This enable sharing memory between task serving requests, for instance
    for caching answers.

    Here is how to use:

    {[
let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
let new_thread = Tiny_httpd_domains.new_thread pool in
let server = Tiny_httpd.create ~new_thread ()
....
match Tiny_httpd_domains.run pool server with
| Ok () -> ()
| Error e -> raise e
*)

val new_thread : Domainslib.Task.pool -> 'a Domainslib.Task.task  -> unit

val run : Domainslib.Task.pool -> Tiny_httpd_server.t -> (unit, exn) result
