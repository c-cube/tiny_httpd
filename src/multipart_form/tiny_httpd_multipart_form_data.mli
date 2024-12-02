val align : string Lwt_stream.t -> string -> string Lwt_stream.t Lwt_stream.t
(**
   Align a stream on a particular sequence and remove these boundaries.
 *)

type stream_part

val s_part_name : stream_part -> string
val s_part_body : stream_part -> string Lwt_stream.t
val s_part_filename : stream_part -> string option

val parse_stream :
  stream:string Lwt_stream.t ->
  content_type:string ->
  stream_part Lwt_stream.t Lwt.t

type file

val file_name : file -> string
val file_content_type : file -> string
val file_stream : file -> string Lwt_stream.t

module StringMap : Map.S with type key = string

val get_parts :
  stream_part Lwt_stream.t ->
  [ `String of string | `File of file ] StringMap.t Lwt.t

val parse :
  stream:string Lwt_stream.t ->
  content_type:string ->
  callback:(name:string -> filename:string -> string -> unit Lwt.t) ->
  (string * string) list Lwt.t
