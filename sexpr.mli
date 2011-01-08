
type t =
  | Sstring of string
  | Sident of string
  | Sint of int
  | Sfloat of float
  | Schar of char
  | Sexpr of t list

val read: char Stream.t -> t
val write: Format.formatter -> t -> unit
val equal: t -> t -> bool
val to_string: t -> string
val from_string: string -> t

exception Unreadable_object
exception Undefined_external_form

val ident: string -> t
val string: string -> t
val tagged_sexpr: string -> t list -> t
val parse_tagged_sexpr: t -> string * t list
