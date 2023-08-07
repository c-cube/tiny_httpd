(** HTML combinators.

    This module provides combinators to produce html. It doesn't enforce
    the well-formedness of the html, unlike Tyxml, but it's simple and should
    be reasonably efficient.
    @since 0.12
*)

module IO = Tiny_httpd_io

include Tiny_httpd_html_
(** @inline *)

(** Write an HTML element to this out channel.
    @param top if true, add DOCTYPE at the beginning. The top element should then
    be a "html" tag.
    @since NEXT_RELEASE
    *)
let to_out_channel ?(top = false) (self : elt) (out : IO.Out_channel.t) : unit =
  let out = Out.create_of_out out in
  if top then Out.add_string out "<!DOCTYPE html>\n";
  self out;
  Out.add_format_nl out;
  Out.flush out

(** Convert a HTML element to a string.
    @param top if true, add DOCTYPE at the beginning. The top element should then
    be a "html" tag. *)
let to_string ?top (self : elt) : string =
  let buf = Buffer.create 64 in
  let out = IO.Out_channel.of_buffer buf in
  to_out_channel ?top self out;
  Buffer.contents buf

(** Convert a list of HTML elements to a string.
    This is designed for fragments of HTML that are to be injected inside
    a bigger context, as it's invalid to have multiple elements at the toplevel
    of a HTML document. *)
let to_string_l (l : elt list) =
  let buf = Buffer.create 64 in
  let out = Out.create_of_buffer buf in
  List.iter
    (fun f ->
      f out;
      Out.add_format_nl out)
    l;
  Buffer.contents buf

let to_string_top = to_string ~top:true

(** Write a toplevel element to an output channel.
    @since NEXT_RELEASE *)
let to_out_channel_top = to_out_channel ~top:true

(** Produce a streaming writer from this HTML element.
    @param top if true, add a DOCTYPE. See {!to_out_channel}.
    @since NEXT_RELEASE *)
let to_writer ?top (self : elt) : IO.Writer.t =
  let write oc = to_out_channel ?top self oc in
  IO.Writer.make ~write ()

(** Convert a HTML element to a stream. This might just convert
    it to a string first, do not assume it to be more efficient. *)
let to_stream (self : elt) : Tiny_httpd_stream.t =
  Tiny_httpd_stream.of_string @@ to_string self
