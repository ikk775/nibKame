val write_number : int -> string
val read_number : char Stream.t -> int
val write_id : string -> string
val read_id : char Stream.t -> string
val write_seq : ('a -> string) -> 'a list -> string
val read_seq : char Stream.t -> (char Stream.t -> 'a) -> 'a list

val escape : string -> string
val escapex : string -> string -> string
