

let new_thread pool f =
   ignore (Domainslib.Task.async pool f)

let run pool server =
  Domainslib.Task.run pool (fun () -> Tiny_httpd.run server)
