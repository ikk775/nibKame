module Array :
  sig
    val swap : 'a array -> int -> int -> unit
    val shuffle : 'a array -> unit
  end

module Format :
  sig
    val call_with_output_string : (Format.formatter -> 'a) -> string
  end

module List :
  sig
    val iota : ?step:int -> int -> int -> int list
    val mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a list -> bool
    val setDiff : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
    val shuffle : 'a list -> 'a list
    val unique : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list
    val select : 'a list list -> 'a list list
    val iter_list : int -> (unit -> 'a) -> 'a list
  end

module String :
  sig
    val explode : string -> char list
    val implode : char list -> string
  end
