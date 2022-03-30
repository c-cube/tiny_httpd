
(** HTML combinators.

    This module provides combinators to produce html. It doesn't enforce
    the well-formedness of the html, unlike Tyxml, but it's simple and should
    be reasonably efficient.
    @since 0.12
*)

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

(** Convert a list of HTML elements to a string.
    This is designed for fragments of HTML that are to be injected inside
    a bigger context, as it's invalid to have multiple elements at the toplevel
    of a HTML document. *)
let to_string_l (l:elt list) =
  let out = Out.create () in
  List.iter (fun f -> f out; Out.add_format_nl out) l;
  Out.to_string out

let to_string_top = to_string ~top:true

(** Convert a HTML element to a stream. This might just convert
    it to a string first, do not assume it to be more efficient. *)
let to_stream (self:elt) : Tiny_httpd_stream.t =
  Tiny_httpd_stream.of_string @@ to_string self
