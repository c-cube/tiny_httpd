
(** @inline *)
include Tiny_httpd_html_

(** Convert a HTML element to a string.
    @param top if true, add DOCTYPE at the beginning. The top element should then
    be a "html" tag. *)
let to_string ?(top=false) (self:elt) : string =
  let out = Out.create () in
  if top then Out.add_string out "<!DOCTYPE html>\n";
  self out;
  Out.to_string out

let to_string_top = to_string ~top:true

(** Convert a HTML element to a stream. This might just convert
    it to a string first, do not assume it to be more efficient. *)
let to_stream (self:elt) : Tiny_httpd_stream.t =
  Tiny_httpd_stream.of_string @@ to_string self
