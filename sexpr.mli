
type t =
  | Sstring of string
  | Sident of string
  | Sint of int
  | Sfloat of float
  | Schar of char
  | Sexpr of t list

val read: char Stream.t -> t
val write: Format.formatter -> t -> unit
val eq_Sexpr: t -> t -> bool
val of_string: t -> string
