(* adapted from https://github.com/sindresorhus/html-tags (MIT licensed) *)

let pf = Printf.printf
let spf = Printf.sprintf

let void =
  [
    "area";
    "base";
    "br";
    "col";
    "embed";
    "hr";
    "img";
    "input";
    "link";
    "menuitem";
    "meta";
    "param";
    "source";
    "track";
    "wbr";
  ]

let normal =
  [
    "a";
    "abbr";
    "address";
    "area";
    "article";
    "aside";
    "audio";
    "b";
    "base";
    "bdi";
    "bdo";
    "blockquote";
    "body";
    "br";
    "button";
    "canvas";
    "caption";
    "cite";
    "code";
    "col";
    "colgroup";
    "data";
    "datalist";
    "dd";
    "del";
    "details";
    "dfn";
    "dialog";
    "div";
    "dl";
    "dt";
    "em";
    "embed";
    "fieldset";
    "figcaption";
    "figure";
    "footer";
    "form";
    "h1";
    "h2";
    "h3";
    "h4";
    "h5";
    "h6";
    "head";
    "header";
    "hgroup";
    "hr";
    "html";
    "i";
    "iframe";
    "img";
    "input";
    "ins";
    "kbd";
    "label";
    "legend";
    "li";
    "link";
    "main";
    "map";
    "mark";
    "math";
    "menu";
    "menuitem";
    "meta";
    "meter";
    "nav";
    "noscript";
    "object";
    "ol";
    "optgroup";
    "option";
    "output";
    "p";
    "param";
    "picture";
    "pre";
    "progress";
    "q";
    "rb";
    "rp";
    "rt";
    "rtc";
    "ruby";
    "s";
    "samp";
    "script";
    "section";
    "select";
    "slot";
    "small";
    "source";
    "span";
    "strong";
    "style";
    "sub";
    "summary";
    "sup";
    "svg";
    "table";
    "tbody";
    "td";
    "template";
    "textarea";
    "tfoot";
    "th";
    "thead";
    "time";
    "title";
    "tr";
    "track";
    "u";
    "ul";
    "var";
    "video";
    "wbr";
  ]
  |> List.filter (fun s -> not (List.mem s void))

(* obtained via:
   {[
     l = Array(...document.querySelectorAll('div tbody td code a')).map(
       x => x.firstChild.textContent);
     JSON.stringify(l)
   ]}
   on https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes
*)
let attrs =
  [
    "accept";
    "accept-charset";
    "accesskey";
    "action";
    "align";
    "allow";
    "alt";
    "async";
    "autocapitalize";
    "autocomplete";
    "autofocus";
    "autoplay";
    "buffered";
    "capture";
    "challenge";
    "charset";
    "checked";
    "cite";
    "class";
    "code";
    "codebase";
    "cols";
    "colspan";
    "content";
    "contenteditable";
    "contextmenu";
    "controls";
    "coords";
    "crossorigin";
    "csp";
    "data";
    "data-*";
    "datetime";
    "decoding";
    "default";
    "defer";
    "dir";
    "dirname";
    "disabled";
    "download";
    "draggable";
    "enctype";
    "enterkeyhint";
    "for";
    "form";
    "formaction";
    "formenctype";
    "formmethod";
    "formnovalidate";
    "formtarget";
    "headers";
    "hidden";
    "high";
    "href";
    "hreflang";
    "http-equiv";
    "icon";
    "id";
    "importance";
    "integrity";
    "ismap";
    "itemprop";
    "keytype";
    "kind";
    "label";
    "lang";
    "language";
    "list";
    "loop";
    "low";
    "manifest";
    "max";
    "maxlength";
    "minlength";
    "media";
    "method";
    "min";
    "multiple";
    "muted";
    "name";
    "novalidate";
    "open";
    "optimum";
    "pattern";
    "ping";
    "placeholder";
    "poster";
    "preload";
    "radiogroup";
    "readonly";
    "referrerpolicy";
    "rel";
    "required";
    "reversed";
    "rows";
    "rowspan";
    "sandbox";
    "scope";
    "scoped";
    "selected";
    "shape";
    "size";
    "sizes";
    "slot";
    "span";
    "spellcheck";
    "src";
    "srcdoc";
    "srclang";
    "srcset";
    "start";
    "step";
    "style";
    "summary";
    "tabindex";
    "target";
    "title";
    "translate";
    "Text";
    "type";
    "usemap";
    "value";
    "width";
    "wrap";
  ]

let prelude =
  {|
(** Output for HTML combinators.

    This output type is used to produce a string reasonably efficiently from
    a tree of combinators.

    {b NOTE}: this is experimental and an unstable API.

    @since 0.12
    @open *)
module Out : sig
  type t
  val create_of_buffer : Buffer.t -> t
  val create_of_out: Tiny_httpd_io.Out_channel.t -> t
  val flush : t -> unit
  val add_char : t -> char -> unit
  val add_string : t -> string -> unit
  val add_format_nl : t -> unit
  val with_no_format_nl : t -> (unit -> 'a) -> 'a
end = struct
  module IO = Tiny_httpd_io
  type t = {
    out: IO.Out_channel.t;
    mutable fmt_nl: bool; (* if true, we print [\n] around tags to format the html *)
  }
  let create_of_out out = {out; fmt_nl=true}
  let create_of_buffer buf : t = create_of_out (IO.Out_channel.of_buffer buf)
  let[@inline] flush self : unit = IO.Out_channel.flush self.out
  let[@inline] add_char self c = IO.Out_channel.output_char self.out c
  let[@inline] add_string self s = IO.Out_channel.output_string self.out s
  let[@inline] add_format_nl self = if self.fmt_nl then add_char self '\n'
  let with_no_format_nl self f =
    if self.fmt_nl then (
      self.fmt_nl <- false;
      try let x=f() in self.fmt_nl <- true; x with e -> self.fmt_nl <- true; raise e
    ) else f()
end

type attribute = string * string
(** An attribute, i.e. a key/value pair *)

type elt = Out.t -> unit
(** A html element. It is represented by its output function, so we
    can directly print it. *)

type void = ?if_:bool -> attribute list -> elt
(** Element without children. *)

type nary = ?if_:bool -> attribute list -> elt list -> elt
(** Element with children, represented as a list.
    @param if_ if false, do not print anything (default true) *)

(** A chunk of sub-elements, possibly empty.
    @inline *)
type sub_elt = [ `E of elt | `L of elt list | `S of elt Seq.t | `Nil]

type nary' = ?if_:bool -> attribute list -> sub_elt list -> elt
(** Element with children, represented as a list of {!sub_elt} to be flattened
    @param if_ if false, do not print anything (default true) *)

(**/**)
module Helpers_ = struct

(** Escape string so it can be safely embedded in HTML text. *)
let _str_escape (out:Out.t) (s:string) : unit =
  String.iter (function
    | '<' -> Out.add_string out "&lt;"
    | '>' -> Out.add_string out "&gt;"
    | '&' -> Out.add_string out "&amp;"
    | '"' -> Out.add_string out "&quot;"
    | '\'' -> Out.add_string out "&apos;"
    | c -> Out.add_char out c)
  s

(** Print the value part of an attribute *)
let _attr_escape (out:Out.t) (s:string) =
  Out.add_char out '"';
  _str_escape out s;
  Out.add_char out '"'

(** Output a list of attributes. *)
let _write_attrs (out:Out.t) (l:attribute list) : unit =
  List.iter
    (fun (k,v) ->
      Out.add_char out ' ';
      Out.add_string out k;
      Out.add_char out '=';
      _attr_escape out v)
    l

(** Write sub-elements of a {!nary'} element, returns [true] iff
    at least one sub-element was written. *)
let _write_subs (out:Out.t) (l:sub_elt list) : bool =
  let has_sub = ref false in
  let prepend_white () = has_sub := true; Out.add_format_nl out; in
  let emit1 = function
    | `E x -> prepend_white(); x out
    | `L l -> List.iter (fun e -> prepend_white(); e out) l
    | `S l -> Seq.iter (fun e -> prepend_white(); e out) l
    | `Nil -> ()
  in
  List.iter emit1 l;
  !has_sub

(** Write a tag, with its attributes.
    @param void if true, end with "/>", otherwise end with ">" *)
let _write_tag_attrs ~void (out:Out.t) (tag:string) (attrs:attribute list) : unit =
  Out.add_string out "<";
  Out.add_string out tag;
  _write_attrs out attrs;
  if void then Out.add_string out "/>" else Out.add_string out ">"

end
open Helpers_
(**/**)

(** Sub-element with a single element inside. *)
let[@inline] sub_e (elt:elt) : sub_elt = `E elt

(** Sub-element with a list of items inside. *)
let[@inline] sub_l (l:elt list) : sub_elt = `L l

(** Sub-element with a sequence ({!Seq.t}) of items inside. *)
let[@inline] sub_seq (l:elt Seq.t) : sub_elt = `S l

(** Helper to build a {!Seq.t} from an array. *)
let seq_of_array (a:_ array) : _ Seq.t =
  let rec loop i () =
    if i=Array.length a then Seq.Nil
    else Seq.Cons (a.(i), loop (i+1))
  in loop 0

(** Sub-element with nothing inside. Useful in conditionals, when one
    decides not to emit a sub-element at all. *)
let sub_empty : sub_elt = `Nil

(** Emit a string value, which will be escaped. *)
let txt (txt:string) : elt = fun out -> _str_escape out txt

(** Formatted version of {!txt} *)
let txtf fmt = Format.kasprintf (fun s -> fun out -> _str_escape out s) fmt

(** Emit raw HTML. Caution, this can lead to injection vulnerabilities,
  never use with text that comes from untrusted users. *)
let raw_html (s:string) : elt = fun out -> Out.add_string out s
|}

let oname = function
  | "object" -> "object_"
  | "class" -> "class_"
  | "method" -> "method_"
  | "data-*" -> "data_star"
  | "for" -> "for_"
  | "open" -> "open_"
  | "Text" -> "text"
  | "type" -> "type_"
  | name ->
    String.map
      (function
        | '-' -> '_'
        | c -> c)
      name

let emit_void name =
  let oname = oname name in
  pf
    "(** tag %S, see \
     {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/%s} mdn} *)\n"
    name name;
  pf "let %s : void = fun ?(if_=true) attrs out ->\n" oname;
  pf "  if if_ then (\n";
  pf "    _write_tag_attrs ~void:true out %S attrs;\n" name;
  pf "  )";
  pf "\n\n";
  ()

let emit_normal name =
  let oname = oname name in

  pf
    "(** tag %S, see \
     {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/%s} mdn} *)\n"
    name name;
  pf "let %s : nary = fun ?(if_=true) attrs sub out ->\n" oname;
  pf "  if if_ then (\n";
  (* for <pre>, newlines actually matter *)
  if name = "pre" then pf "  Out.with_no_format_nl out @@ fun () ->\n";
  pf "    _write_tag_attrs ~void:false out %S attrs;\n" name;
  pf "    List.iter (fun sub -> Out.add_format_nl out; sub out) sub;\n";
  pf "    if sub <> [] then Out.add_format_nl out;\n";
  pf "    Out.add_string out \"</%s>\")" name;
  pf "\n\n";

  (* block version *)
  let oname = oname ^ "'" in
  pf
    "(** tag %S, see \
     {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/%s} mdn} *)\n"
    name name;
  pf "let %s : nary' = fun ?(if_=true) attrs l out ->\n" oname;
  pf "  if if_ then (\n";
  if name = "pre" then pf "  Out.with_no_format_nl out @@ fun () ->\n";
  pf "    _write_tag_attrs ~void:false out %S attrs;\n" name;
  pf "    let has_sub = _write_subs out l in\n";
  pf "    if has_sub then Out.add_format_nl out;\n";
  pf "    Out.add_string out \"</%s>\")" name;
  pf "\n\n";

  ()

let doc_attrs =
  {|Attributes.

This module contains combinator for the standard attributes.
One can also just use a pair of strings. |}

let emit_attr name =
  let oname = oname name in
  pf "  (** Attribute %S. *)\n" name;
  pf "  let %s : t = fun v -> %S, v\n" oname name;
  pf "\n"

let () =
  pf "%s\n" prelude;
  List.iter emit_void void;
  List.iter emit_normal normal;
  pf "(** %s *)\n" doc_attrs;
  pf "module A = struct\n";
  pf "  type t = string -> attribute\n";
  pf "  (** Attribute builder *)\n";
  pf "\n";
  List.iter emit_attr attrs;
  pf "end\n";
  ()
