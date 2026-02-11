(** Routing

    Basic type-safe routing of handlers based on URL paths. This is optional, it
    is possible to only define the root handler with something like
    {{:https://github.com/anuragsoni/routes/} Routes}.
    @since 0.6 *)

type ('a, 'b) comp
(** An atomic component of a path *)

type ('a, 'b) t
(** A route, composed of path components *)

val int : (int -> 'a, 'a) comp
(** Matches an integer. *)

val string : (string -> 'a, 'a) comp
(** Matches a string not containing ['/'] and binds it as is. *)

val string_urlencoded : (string -> 'a, 'a) comp
(** Matches a URL-encoded string, and decodes it. *)

val exact : string -> ('a, 'a) comp
(** [exact "s"] matches ["s"] and nothing else. *)

val return : ('a, 'a) t
(** Matches the empty path. *)

val rest_of_path : (string -> 'a, 'a) t
(** Matches a string, even containing ['/']. This will match the entirety of the
    remaining route.
    @since 0.7 *)

val rest_of_path_urlencoded : (string -> 'a, 'a) t
(** Matches a string, even containing ['/'], and URL-decode it (piecewise). This
    will match the entirety of the remaining route.
    @since 0.7 *)

val ( @/ ) : ('a, 'b) comp -> ('b, 'c) t -> ('a, 'c) t
(** [comp / route] matches ["foo/bar/…"] iff [comp] matches ["foo"], and [route]
    matches ["bar/…"]. *)

val exact_path : string -> ('a, 'b) t -> ('a, 'b) t
(** [exact_path "foo/bar/..." r] is equivalent to
    [exact "foo" @/ exact "bar" @/ ... @/ r]
    @since 0.11 **)

val pp : Format.formatter -> _ t -> unit
(** Print the route.
    @since 0.7 *)

val to_string : _ t -> string
(** Print the route.
    @since 0.7 *)

val to_url : ('a, string) t -> 'a

(** [to_url route args] takes a route, and turns it into a URL path.
    @since NEXT_RELEASE *)

module Private_ : sig
  val eval : string list -> ('a, 'b) t -> 'a -> 'b option
end
