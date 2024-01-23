(* Use Logs *)

module Log = (val Logs.(src_log @@ Src.create "tiny_httpd"))

let info k = Log.info (fun fmt -> k (fun x -> fmt ?header:None ?tags:None x))
let debug k = Log.debug (fun fmt -> k (fun x -> fmt ?header:None ?tags:None x))
let error k = Log.err (fun fmt -> k (fun x -> fmt ?header:None ?tags:None x))

let setup ~debug () =
  Logs.set_reporter @@ Logs.format_reporter ();
  Logs.set_level ~all:true
    (Some
       (if debug then
         Logs.Debug
       else
         Logs.Info))
