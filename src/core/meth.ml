open Common_

type t = [ `GET | `PUT | `POST | `HEAD | `DELETE | `OPTIONS ]

let to_string = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `POST -> "POST"
  | `DELETE -> "DELETE"
  | `OPTIONS -> "OPTIONS"

let pp out s = Format.pp_print_string out (to_string s)

let of_string = function
  | "GET" -> `GET
  | "PUT" -> `PUT
  | "POST" -> `POST
  | "HEAD" -> `HEAD
  | "DELETE" -> `DELETE
  | "OPTIONS" -> `OPTIONS
  | s -> bad_reqf 400 "unknown method %S" s
