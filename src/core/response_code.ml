type t = int

let ok = 200
let not_found = 404

let descr = function
  | 100 -> "Continue"
  | 200 -> "OK"
  | 201 -> "Created"
  | 202 -> "Accepted"
  | 204 -> "No content"
  | 300 -> "Multiple choices"
  | 301 -> "Moved permanently"
  | 302 -> "Found"
  | 304 -> "Not Modified"
  | 400 -> "Bad request"
  | 401 -> "Unauthorized"
  | 403 -> "Forbidden"
  | 404 -> "Not found"
  | 405 -> "Method not allowed"
  | 408 -> "Request timeout"
  | 409 -> "Conflict"
  | 410 -> "Gone"
  | 411 -> "Length required"
  | 413 -> "Payload too large"
  | 417 -> "Expectation failed"
  | 500 -> "Internal server error"
  | 501 -> "Not implemented"
  | 503 -> "Service unavailable"
  | n -> "Unknown response code " ^ string_of_int n (* TODO *)

let[@inline] is_success n = n < 400
