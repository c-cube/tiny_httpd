
(* adapted from https://github.com/sindresorhus/html-tags (MIT licensed) *)

let pf = Printf.printf
let spf = Printf.sprintf

let void = [
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

let normal = [
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
] |> List.filter (fun s -> not (List.mem s void))

(* obtained via:
   {[
     l = Array(...document.querySelectorAll('div tbody td code a')).map(
       x => x.firstChild.textContent);
     JSON.stringify(l)
   ]}
   on https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes
*)
let attrs = [
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

let prelude = {|
(** Output for HTML combinators.

    This output type is used to produce a string reasonably efficiently from
    a tree of combinators. *)
module Out : sig
  type t
  val create : unit -> t
  val clear : t -> unit
  val add_char : t -> char -> unit
  val add_string : t -> string -> unit
  val to_string : t -> string
end = struct
  include Buffer
  let create () = Buffer.create 256
  let to_string = contents
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

type nary' = ?if_:bool -> attribute list -> (Out.t -> unit) -> elt
(** Element with children, represented as a continuation.
    @param if_ if false, do not print anything (default true) *)

(** Escape string so it can be safely embedded in HTML text. *)
let str_escape (out:Out.t) (s:string) : unit =
  String.iter (function
    | '<' -> Out.add_string out "&lt;"
    | '>' -> Out.add_string out "&gt;"
    | '&' -> Out.add_string out "&amp;"
    | '"' -> Out.add_string out "&quot;"
    | '\'' -> Out.add_string out "&apos;"
    | c -> Out.add_char out c)
  s

(** Print the value part of an attribute *)
let attr_escape (out:Out.t) (s:string) =
  Out.add_char out '"';
  str_escape out s;
  Out.add_char out '"'

(** Output a list of attributes. *)
let write_attrs (out:Out.t ) (l:attribute list) : unit =
  List.iter
    (fun (k,v) ->
      Out.add_char out ' ';
      Out.add_string out k;
      Out.add_char out '=';
      attr_escape out v)
    l

(** Emit a string value, which will be escaped. *)
let txt (txt:string) : elt = fun out -> str_escape out txt

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
    String.map (function '-' -> '_' | c -> c) name

let emit_void name =
  let oname = oname name in
  pf "(** tag %S, see {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/%s} mdn} *)\n"
    name name;
  pf "let %s : void = fun ?(if_=true) attrs out ->\n" oname;
  pf "  if if_ then (\n";
  pf "    Out.add_string out \"<%s\";\n" name;
  pf "    write_attrs out attrs;\n";
  pf "    Out.add_string out \"/>\")";
  pf "\n\n";
  ()

let emit_normal name =
  let oname = oname name in

  pf "(** tag %S, see {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/%s} mdn} *)\n"
    name name;
  pf "let %s : nary = fun ?(if_=true) attrs sub out ->\n" oname;
  pf "  if if_ then (\n";
  pf "    Out.add_string out \"<%s\";\n" name;
  pf "    write_attrs out attrs;\n";
  pf "    Out.add_string out \">\\n\";\n";
  pf "    List.iter (fun sub -> sub out; Out.add_char out '\\n') sub;\n";
  pf "    Out.add_string out \"</%s>\\n\")" name;
  pf "\n\n";

  (* block version *)
  let oname = oname ^ "'" in
  pf "(** tag %S, see {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/%s} mdn} *)\n"
    name name;
  pf "let %s : nary' = fun ?(if_=true) attrs f out ->\n" oname;
  pf "  if if_ then (\n";
  pf "    Out.add_string out \"<%s\";\n" name;
  pf "    write_attrs out attrs;\n";
  pf "    Out.add_string out \">\\n\";\n";
  pf "    f out;\n";
  pf "    Out.add_string out \"</%s>\\n\")" name;
  pf "\n\n";


  ()

let emit_attr name =
  let oname = oname name in
  pf "  (** Attribute %S. *)\n" name;
  pf "  let %s : t = fun v -> %S, v\n" oname name;
  pf "\n"

let () =
  pf "%s\n" prelude;
  List.iter emit_void void;
  List.iter emit_normal normal;
  pf "(** Attributes *)\n";
  pf "module A = struct\n";
  pf "  type t = string -> attribute\n";
  pf "  (** Attribute builder *)\n";
  pf "\n";
  List.iter emit_attr attrs;
  pf "end\n";
  ()

