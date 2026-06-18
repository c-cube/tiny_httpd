open Common_

type t = [ `GET | `PUT | `POST | `HEAD | `DELETE | `OPTIONS | `QUERY ]

let to_string = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `POST -> "POST"
  | `DELETE -> "DELETE"
  | `OPTIONS -> "OPTIONS"
  | `QUERY -> "QUERY"

let pp out s = Format.pp_print_string out (to_string s)

let of_string = function
  | "GET" -> `GET
  | "PUT" -> `PUT
  | "POST" -> `POST
  | "HEAD" -> `HEAD
  | "DELETE" -> `DELETE
  | "OPTIONS" -> `OPTIONS
  | "QUERY" -> `QUERY
  | s -> bad_reqf 400 "unknown method %S" s
