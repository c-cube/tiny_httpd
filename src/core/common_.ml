exception Bad_req of int * string

let spf = Printf.sprintf
let bad_reqf c fmt = Printf.ksprintf (fun s -> raise (Bad_req (c, s))) fmt

type 'a resp_result = ('a, int * string) result

let unwrap_resp_result = function
  | Ok x -> x
  | Error (c, s) -> raise (Bad_req (c, s))
